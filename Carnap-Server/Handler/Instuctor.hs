{-#LANGUAGE DeriveGeneric #-}
module Handler.Instuctor where

import Import
import Util.Data
import Util.Database
import Yesod.Form.Bootstrap3
import Yesod.Form.Jquery
import Handler.User (scoreByIdAndClassPerProblem)
import Text.Blaze.Html (toMarkup)
import Text.Read (readMaybe)
import Data.Time
import Data.Time.Zones
import Data.Time.Zones.DB
import Data.Time.Zones.All
import Data.Aeson (decode,encode)
import qualified Data.IntMap (insert,fromList,toList,delete)
import qualified Data.Text as T
import qualified Data.List as L
import System.FilePath
import System.Directory (getDirectoryContents,removeFile, doesFileExist, createDirectoryIfMissing)

putInstructorR :: Text -> Handler Value
putInstructorR ident = do
        ((assignmentrslt,_),_) <- runFormPost (identifyForm "updateAssignment" $ updateAssignmentForm)
        ((courserslt,_),_)     <- runFormPost (identifyForm "updateCourse" $ updateCourseForm)
        ((documentrslt,_),_)   <- runFormPost (identifyForm "updateDocument" $ updateDocumentForm)
        case (assignmentrslt,courserslt,documentrslt) of 
            (FormSuccess (filename, coursename, mdue, mduetime,mfrom,mfromtime,muntil,muntiltime, mdesc),_,_) -> do
                             massignEnt <- runDB $ do Just (Entity uid _) <- getBy (UniqueUser ident)
                                                      Just (Entity _ umd) <- getBy (UniqueUserData uid)
                                                      let Just imdid = userDataInstructorId umd
                                                      Just (Entity cid _) <- getBy (UniqueCourse coursename imdid)
                                                      Just (Entity docid _) <- getBy $ UniqueDocument filename uid
                                                      getBy $ UniqueAssignment docid cid
                             case massignEnt of 
                                   Nothing -> returnJson ("Could not find assignment!"::Text)
                                   Just (Entity k v) -> 
                                        do let cid = assignmentMetadataCourse v
                                           runDB $ do (Just course) <- get cid
                                                      let (Just tz) = tzByName . courseTimeZone $ course
                                                      let mtimeUpdate mdate mtime field = maybeDo mdate (\date-> 
                                                             do let localtime = case mtime of
                                                                        (Just time) -> LocalTime date time
                                                                        _ -> LocalTime date (TimeOfDay 23 59 59)
                                                                update k [ field =. (Just $ localTimeToUTCTZ tz localtime) ])
                                                      mtimeUpdate mdue mduetime AssignmentMetadataDuedate
                                                      mtimeUpdate mfrom mfromtime AssignmentMetadataVisibleFrom
                                                      mtimeUpdate muntil muntiltime AssignmentMetadataVisibleTill
                                                      maybeDo mdesc (\desc -> update k
                                                         [ AssignmentMetadataDescription =. (Just $ unTextarea desc) ])
                                           returnJson ("updated!"::Text)
            (_,FormSuccess (coursetitle,mdesc,mstart,mend,mpoints),_) -> do
                             Just instructor <- instructorIdByIdent ident
                             mcourseEnt <- runDB . getBy . UniqueCourse coursetitle $ instructor
                             case entityKey <$> mcourseEnt of
                                 Just k -> do runDB $ do maybeDo mdesc (\desc -> update k
                                                           [ CourseDescription =. (Just $ unTextarea desc) ])
                                                         maybeDo mstart (\start -> update k
                                                           [ CourseStartDate =. UTCTime start 0 ])
                                                         maybeDo mend (\end-> update k
                                                           [ CourseEndDate =. UTCTime end 0 ])
                                                         maybeDo mpoints (\points-> update k
                                                           [ CourseTotalPoints =. points ])
                                              returnJson ("updated!"::Text)
                                 Nothing -> returnJson ("could not find course!"::Text)
            (_,_,FormSuccess (filename, mscope, mdesc,mfile)) -> do
                             musr <- runDB $ getBy $ UniqueUser ident
                             case entityKey <$> musr of
                                 Just k -> do
                                     mdocId <- runDB $ getBy $ UniqueDocument filename k
                                     case mdocId of
                                         Just (Entity k' _) -> 
                                            do runDB $ do maybeDo mdesc (\desc -> update k'
                                                           [ DocumentDescription =. (Just $ unTextarea desc) ])
                                                          maybeDo mscope (\scope -> update k'
                                                           [ DocumentScope =. scope ])
                                               maybeDo mfile (saveTo ("documents" </> unpack ident) $ unpack filename)
                                               returnJson ("updated!"::Text)
                                         Nothing -> returnJson ("could not find document!"::Text)
                                 Nothing -> returnJson ("could not find user id!"::Text)

            (FormMissing,FormMissing,FormMissing) -> returnJson ("no form" :: Text)
            (form1,form2,form3) -> returnJson ("errors: " <> errorsOf form1 <> errorsOf form2 <> errorsOf form3)
                where errorsOf (FormFailure s) = concat s <> ", " 
                      errorsOf _ = "" 
    where maybeDo mv f = case mv of Just v -> f v; _ -> return ()

deleteInstructorR :: Text -> Handler Value
deleteInstructorR ident = do
    msg <- requireJsonBody :: Handler InstructorDelete
    case msg of 
      DeleteAssignment id ->
        do datadir <- appDataRoot <$> (appSettings <$> getYesod) 
           deleted <- runDB $ deleteCascade id
           returnJson ("Assignment deleted" :: Text)
      DeleteProblems coursename setnum -> 
        do miid <- instructorIdByIdent ident
           case miid of
               Just iid -> 
                    do mclass <- runDB $ getBy $ UniqueCourse coursename iid
                       case mclass of 
                            Just (Entity classkey theclass)->
                                do case readAssignmentTable <$> courseTextbookProblems theclass  of
                                       Just assign -> do runDB $ update classkey
                                                                        [CourseTextbookProblems =. (Just $ BookAssignmentTable $ Data.IntMap.delete setnum assign)]
                                                         returnJson ("Deleted Assignment"::Text)
                                       Nothing -> returnJson ("Assignment table Missing, can't delete."::Text)
                            Nothing -> returnJson ("Something went wrong with retriving the course."::Text)

               Nothing -> returnJson ("You do not appear to be an instructor."::Text)
      DeleteCourse coursename -> 
        do miid <- instructorIdByIdent ident
           case miid of
               Nothing -> returnJson ("You do not appear to be an instructor."::Text)
               Just iid -> 
                    do mclass <- runDB $ getBy $ UniqueCourse coursename iid
                       case mclass of 
                            Just (Entity classkey theclass)-> 
                                do runDB $ do studentsOf <- selectList [UserDataEnrolledIn ==. Just classkey] []
                                              mapM (\s -> update (entityKey s) [UserDataEnrolledIn =. Nothing]) studentsOf
                                              deleteCascade classkey
                                   returnJson ("Class Deleted"::Text)
                            Nothing -> returnJson ("No class to delete, for some reason"::Text)
      DeleteDocument fn ->
        do datadir <- appDataRoot <$> (appSettings <$> getYesod) 
           musr <- runDB $ getBy $ UniqueUser ident
           case musr of
               Nothing -> returnJson ("Could not get user id."::Text)
               Just usr -> do
                   deleted <- runDB $ do mk <- getBy $ UniqueDocument fn (entityKey usr)
                                         case mk of
                                             Just (Entity k v) -> 
                                                do deleteCascade k
                                                   liftIO $ do fe <- doesFileExist (datadir </> "documents" </> unpack ident </> unpack fn) 
                                                               if fe then removeFile (datadir </> "documents" </> unpack ident </> unpack fn)
                                                                     else return ()
                                                   return True
                                             Nothing -> return False
                   if deleted 
                       then returnJson (fn ++ " deleted")
                       else returnJson ("unable to retrieve metadata for " ++ fn)
      DropStudent sident ->
        do sid <- fromIdent sident
           dropped <- runDB $ do msd <- getBy (UniqueUserData sid)
                                 case msd of
                                     Nothing -> return False
                                     Just (Entity k _) -> 
                                        do update k [UserDataEnrolledIn =. Nothing]
                                           return True
           if dropped then returnJson (sident ++ " dropped")
                      else returnJson ("couldn't drop " ++ sident)

postInstructorR :: Text -> Handler Html
postInstructorR ident = do
    classes <- classesByInstructorIdent ident
    docs <- documentsByInstructorIdent ident
    ((assignmentrslt,_),_) <- runFormPost (identifyForm "uploadAssignment" $ uploadAssignmentForm classes docs)
    ((documentrslt,_),_) <- runFormPost (identifyForm "uploadDocument" $ uploadDocumentForm)
    ((newclassrslt,_),_) <- runFormPost (identifyForm "createCourse" createCourseForm)
    ((frombookrslt,_),_) <- runFormPost (identifyForm "setBookAssignment" $ setBookAssignmentForm classes)
    case assignmentrslt of 
        (FormSuccess (doc, Entity classkey theclass, mdue,mduetime,mfrom,mfromtime,mtill,mtilltime, massignmentdesc, subtime)) ->
            do let (Just tz) = tzByName . courseTimeZone $ theclass
                   localize (mdate,mtime) = case (mdate,mtime) of
                              (Just date, Just time) -> Just $ LocalTime date time
                              (Just date,_)  -> Just $ LocalTime date (TimeOfDay 23 59 59)
                              _ -> Nothing
                   localdue = localize (mdue,mduetime)
                   localfrom = localize (mfrom,mfromtime)
                   localtill = localize (mtill,mtilltime)
                   info = unTextarea <$> massignmentdesc
               success <- tryInsert $ AssignmentMetadata 
                                        { assignmentMetadataDocument = (entityKey doc)
                                        , assignmentMetadataDescription = info 
                                        , assignmentMetadataDuedate = (localTimeToUTCTZ tz <$> localdue) 
                                        , assignmentMetadataVisibleFrom = (localTimeToUTCTZ tz <$> localfrom)
                                        , assignmentMetadataVisibleTill = (localTimeToUTCTZ tz <$> localtill)
                                        , assignmentMetadataDate = subtime
                                        , assignmentMetadataCourse = classkey
                                        }
               if success then return ()
                          else setMessage "This file has already been assigned for this course"
        (FormFailure s) -> setMessage $ "Something went wrong: " ++ toMarkup (show s)
        FormMissing -> return ()
    case documentrslt of 
        (FormSuccess (file, sharescope, docdesc, subtime)) ->
            do musr <- runDB $ getBy $ UniqueUser ident
               let fn = fileName file
                   info = unTextarea <$> docdesc
                   (Just uid) = musr -- FIXME: catch Nothing here
               success <- tryInsert $ Document fn subtime (entityKey uid) info sharescope
               if success then saveTo ("documents" </> unpack ident) (unpack fn) file 
                          else setMessage "You already have a shared document with this name."
        (FormFailure s) -> setMessage $ "Something went wrong: " ++ toMarkup (show s)
        FormMissing -> return ()
    case newclassrslt of
        (FormSuccess (title, coursedesc, startdate, enddate, tzlabel)) -> do
            miid <- instructorIdByIdent ident
            case miid of
                Just iid -> 
                    do let localize x = localTimeToUTCTZ (tzByLabel tzlabel) (LocalTime x midnight)
                       success <- tryInsert $ Course title (unTextarea <$> coursedesc) iid Nothing (localize startdate) (localize enddate) 0 (toTZName tzlabel)
                       if success then setMessage "Course Created" 
                                  else setMessage "Could not save---this course already exists"
                Nothing -> setMessage "you're not an instructor!"
        (FormFailure s) -> setMessage $ "Something went wrong: " ++ toMarkup (show s)
        FormMissing -> return ()
    case frombookrslt of
        (FormSuccess (Entity classkey theclass, theassignment, duedate, mduetime)) -> runDB $ do
            let (Just tz) = tzByName . courseTimeZone $ theclass
                localdue = case mduetime of
                              Just time -> LocalTime duedate time
                              _ -> LocalTime duedate (TimeOfDay 23 59 59)
                due = localTimeToUTCTZ tz localdue
            case readAssignmentTable <$> courseTextbookProblems theclass of
                Just assign -> update classkey [CourseTextbookProblems =. (Just $ BookAssignmentTable $ Data.IntMap.insert theassignment due assign)]
                Nothing -> update classkey [CourseTextbookProblems =. (Just $ BookAssignmentTable $ Data.IntMap.fromList [(theassignment, due)])]
        (FormFailure s) -> setMessage $ "Something went wrong: " ++ toMarkup (show s)
        FormMissing -> return ()
    redirect $ InstructorR ident

getInstructorR :: Text -> Handler Html
getInstructorR ident = do
    musr <- runDB $ getBy $ UniqueUser ident
    case musr of 
        Nothing -> defaultLayout nopage
        (Just (Entity uid _))  -> do
            UserData firstname lastname enrolledin _ _ <- checkUserData uid 
            classes <- classesByInstructorIdent ident 
            docs <- documentsByInstructorIdent ident 
            let tags = map tagOf classes
            classWidgets <- mapM (classWidget ident) classes
            instructorCourses <- classesByInstructorIdent ident
            assignmentMetadata <- concat <$> mapM (listAssignmentMetadata . entityKey) classes
            assignmentDocs <- mapM (runDB . get) (map (assignmentMetadataDocument . entityVal) assignmentMetadata)
            documents <- runDB $ selectList [DocumentCreator ==. uid] []
            assignmentCourses <- forM assignmentMetadata $ \c -> do 
                                    Just e <- runDB $ get (assignmentMetadataCourse . entityVal $ c)
                                    return e
            (createAssignmentWidget,enctypeCreateAssignment) <- generateFormPost (identifyForm "uploadAssignment" $ uploadAssignmentForm classes docs)
            (uploadDocumentWidget,enctypeShareDocument) <- generateFormPost (identifyForm "uploadDocument" $ uploadDocumentForm)
            (setBookAssignmentWidget,enctypeSetBookAssignment) <- generateFormPost (identifyForm "setBookAssignment" $ setBookAssignmentForm classes)
            (updateAssignmentWidget,enctypeUpdateAssignment) <- generateFormPost (identifyForm "updateAssignment" $ updateAssignmentForm)
            (updateDocumentWidget,enctypeUpdateDocument) <- generateFormPost (identifyForm "updateDocument" $ updateDocumentForm)
            (createCourseWidget,enctypeCreateCourse) <- generateFormPost (identifyForm "createCourse" createCourseForm)
            (updateCourseWidget,enctypeUpdateCourse) <- generateFormPost (identifyForm "updateCourse" $ updateCourseForm)
            defaultLayout $ do
                 addScript $ StaticR js_bootstrap_bundle_min_js
                 addScript $ StaticR js_bootstrap_min_js
                 setTitle $ "Instructor Page for " ++ toMarkup firstname ++ " " ++ toMarkup lastname
                 $(widgetFile "instructor")
    where tagOf = T.append "course-" . T.map (\c -> if c `elem` [' ',':'] then '_' else c) . courseTitle . entityVal
          mprobsOf course = readAssignmentTable <$> courseTextbookProblems course
          nopage = [whamlet|
                    <div.container>
                        <p> Instructor not found.
                   |]

getInstructorDownloadR :: Text -> Text -> Handler TypedContent
getInstructorDownloadR ident coursetitle = do
    musr <- runDB $ getBy $ UniqueUser ident
    case musr of 
        Nothing -> notFound
        (Just (Entity uid usr))  -> do
            mud <- runDB $ getBy (UniqueUserData uid)
            case (entityVal <$> mud) >>= userDataInstructorId of
                Nothing -> notFound
                Just iid -> do
                    mcourse <- runDB $ getBy $ UniqueCourse coursetitle iid
                    case mcourse of 
                        Nothing -> notFound
                        Just course -> do
                            csv <- classCSV course
                            addHeader "Content-Disposition" $ concat
                              [ "attachment;"
                              , "filename=\""
                              , "export.csv"
                              , "\""
                              ]
                            sendResponse (typeOctet, csv)

---------------------
--  Message Types  --
---------------------

data InstructorDelete = DeleteAssignment AssignmentMetadataId
                      | DeleteProblems Text Int
                      | DeleteCourse Text
                      | DeleteDocument Text
                      | DropStudent Text
    deriving Generic

instance ToJSON InstructorDelete

instance FromJSON InstructorDelete

------------------
--  Components  --
------------------

uploadAssignmentForm classes docs extra = do
            (fileRes, fileView) <- mreq (selectFieldList docnames) (bfs ("Document" :: Text)) Nothing
            (classRes, classView) <- mreq (selectFieldList classnames) (bfs ("Class" :: Text)) Nothing
            (dueRes,dueView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Due Date"::Text)) Nothing
            (duetimeRes, duetimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Due Time"::Text)) Nothing
            (fromRes,fromView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Visible From Date"::Text)) Nothing
            (fromtimeRes, fromtimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Visible From Time"::Text)) Nothing
            (tillRes, tillView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Visible Until Date"::Text)) Nothing
            (tilltimeRes,tilltimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Visible Until Time"::Text)) Nothing
            (descRes,descView) <- mopt textareaField (bfs ("Assignment Description"::Text)) Nothing
            currentTime <- lift (liftIO getCurrentTime)
            let theRes = (,,,,,,,,,) <$> fileRes <*> classRes 
                                     <*> dueRes  <*> duetimeRes 
                                     <*> fromRes <*> fromtimeRes
                                     <*> tillRes <*> tilltimeRes
                                     <*> descRes <*> pure currentTime
            let widget = do
                [whamlet|
                #{extra}
                <h6>File to Assign
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput fileView}
                <h6>Assign to
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput classView}
                <h6> Due Date
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput dueView}
                    <div.form-group.col-md-6>
                        ^{fvInput duetimeView}
                <h6> Visible From
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput fromView}
                    <div.form-group.col-md-6>
                        ^{fvInput fromtimeView}
                <h6> Visible To
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput tillView}
                    <div.form-group.col-md-6>
                        ^{fvInput tilltimeView}
                <h6> Description
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput descView}
                |]
            return (theRes,widget)
    where classnames = map (\theclass -> (courseTitle . entityVal $ theclass, theclass)) classes
          docnames = map (\thedoc -> (documentFilename . entityVal $ thedoc, thedoc)) docs

updateAssignmentForm extra = do 
            (fileRes,fileView) <- mreq fileName "" Nothing
            (courseRes, courseView) <- mreq courseName "" Nothing
            (dueRes,dueView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Due Date"::Text)) Nothing
            (duetimeRes, duetimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Due Time"::Text)) Nothing
            (fromRes,fromView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Visible From Date"::Text)) Nothing
            (fromtimeRes, fromtimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Visible From Time"::Text)) Nothing
            (tillRes, tillView) <- mopt (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Visible Until Date"::Text)) Nothing
            (tilltimeRes,tilltimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Visible Until Time"::Text)) Nothing
            (descRes,descView) <- mopt textareaField (bfs ("Assignment Description"::Text)) Nothing
            let theRes = (,,,,,,,,) <$> fileRes <*> courseRes 
                                    <*> dueRes <*> duetimeRes 
                                    <*> fromRes <*> fromtimeRes
                                    <*> tillRes <*> tilltimeRes
                                    <*> descRes
            let widget = do
                [whamlet|
                #{extra}
                ^{fvInput fileView}
                ^{fvInput courseView}
                <h6> Due Date
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput dueView}
                    <div.form-group.col-md-6>
                        ^{fvInput duetimeView}
                <h6> Visible From
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput fromView}
                    <div.form-group.col-md-6>
                        ^{fvInput fromtimeView}
                <h6> Visible To
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput tillView}
                    <div.form-group.col-md-6>
                        ^{fvInput tilltimeView}
                <h6> Description
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput descView}
                |]
            return (theRes,widget)

    where fileName :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Text 
          fileName = hiddenField
          courseName :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Text 
          courseName = hiddenField

updateAssignmentModal form enc = [whamlet|
                    <div class="modal fade" id="updateAssignmentData" tabindex="-1" role="dialog" aria-labelledby="updateAssignmentDataLabel" aria-hidden="true">
                        <div class="modal-dialog" role="document">
                            <div class="modal-content">
                                <div class="modal-header">
                                    <h5 class="modal-title" id="updateAssignmentDataLabel">Update Assignment Data</h5>
                                    <button type="button" class="close" data-dismiss="modal" aria-label="Close">
                                      <span aria-hidden="true">&times;</span>
                                <div class="modal-body">
                                    <form#updateAssignment enctype=#{enc}>
                                        ^{form}
                                        <div.form-group>
                                            <input.btn.btn-primary type=submit value="update">
                    |]    

uploadDocumentForm = renderBootstrap3 BootstrapBasicForm $ (,,,)
            <$> fileAFormReq (bfs ("Document" :: Text))
            <*> areq (selectFieldList scopes) (bfs ("Share With " :: Text)) Nothing
            <*> aopt textareaField (bfs ("Description"::Text)) Nothing
            <*> lift (liftIO getCurrentTime)
    where scopes :: [(Text,SharingScope)]
          scopes = [("Everyone (Visible to everyone)", Public)
                   ,("Instructors (Visible to all instructors)", InstructorsOnly)
                   ,("Link Only (Available, but visible to no one)", LinkOnly)
                   ,("Private (Unavailable)", Private)
                   ]

updateDocumentForm = renderBootstrap3 BootstrapBasicForm $ (,,,)
            <$> areq fileName "" Nothing
            <*> aopt (selectFieldList scopes) (bfs ("Share With " :: Text)) Nothing
            <*> aopt textareaField (bfs ("Description"::Text)) Nothing
            <*> fileAFormOpt (bfs ("Replacement File" :: Text)) 
    where fileName :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Text 
          fileName = hiddenField

          scopes :: [(Text,SharingScope)]
          scopes = [("Everyone (Visible to everyone)", Public)
                   ,("Instructors (Visible to all instructors)", InstructorsOnly)
                   ,("Link Only (Available, but visible to no one)", LinkOnly)
                   ,("Private (Unavailable)", Private)
                   ]

updateDocumentModal form enc = [whamlet|
                    <div class="modal fade" id="updateDocumentData" tabindex="-1" role="dialog" aria-labelledby="updateDocumentLabel" aria-hidden="true">
                        <div class="modal-dialog" role="document">
                            <div class="modal-content">
                                <div class="modal-header">
                                    <h5 class="modal-title" id="updateDocumentLabel">Update Shared Document</h5>
                                    <button type="button" class="close" data-dismiss="modal" aria-label="Close">
                                      <span aria-hidden="true">&times;</span>
                                <div class="modal-body">
                                    <form#updateDocument enctype=#{enc}>
                                        ^{form}
                                        <div.form-group>
                                            <input.btn.btn-primary type=submit value="update">
                    |]    

setBookAssignmentForm classes extra = do 
            (classRes, classView) <- mreq (selectFieldList classnames) (bfs ("Class" :: Text)) Nothing
            (probRes, probView) <- mreq (selectFieldList chapters) (bfs ("Problem Set" :: Text))  Nothing
            (dueRes, dueView) <- mreq (jqueryDayField def) (withPlaceholder "Date" $ bfs ("Due Date"::Text)) Nothing
            (duetimeRes, duetimeView) <- mopt timeFieldTypeTime (withPlaceholder "Time" $ bfs ("Due Time"::Text)) Nothing
            let theRes = (,,,) <$> classRes <*> probRes <*> dueRes <*> duetimeRes
            let widget = do
                [whamlet|
                #{extra}
                <h6>Assign to
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput classView}
                <h6> Problem Set
                <div.row>
                    <div.form-group.col-md-12>
                        ^{fvInput probView}
                <h6> Due Date
                <div.row>
                    <div.form-group.col-md-6>
                        ^{fvInput dueView}
                    <div.form-group.col-md-6>
                        ^{fvInput duetimeView}
                |]
            return (theRes, widget)
    where chapters = map (\x -> ("Problem Set " ++ pack (show x),x)) [1..17] :: [(Text,Int)]
          classnames = map (\theclass -> (courseTitle . entityVal $ theclass, theclass)) classes

createCourseForm = renderBootstrap3 BootstrapBasicForm $ (,,,,)
            <$> areq textField (bfs ("Title" :: Text)) Nothing
            <*> aopt textareaField (bfs ("Course Description"::Text)) Nothing
            <*> areq (jqueryDayField def) (bfs ("Start Date"::Text)) Nothing
            <*> areq (jqueryDayField def) (bfs ("End Date"::Text)) Nothing
            <*> areq (selectFieldList zones)    (bfs ("TimeZone"::Text)) Nothing
    where zones = map (\(x,y,_) -> (decodeUtf8 x,y)) (rights tzDescriptions)

updateCourseModal form enc = [whamlet|
                    <div class="modal fade" id="updateCourseData" tabindex="-1" role="dialog" aria-labelledby="updateCourseDataLabel" aria-hidden="true">
                        <div class="modal-dialog" role="document">
                            <div class="modal-content">
                                <div class="modal-header">
                                    <h5 class="modal-title" id="updateCourseDataLabel">Update Course Data</h5>
                                    <button type="button" class="close" data-dismiss="modal" aria-label="Close">
                                      <span aria-hidden="true">&times;</span>
                                <div class="modal-body">
                                    <form#updateCourse enctype=#{enc}>
                                        ^{form}
                                        <div.form-group>
                                            <input.btn.btn-primary type=submit value="update">
                    |]

updateCourseForm = renderBootstrap3 BootstrapBasicForm $ (,,,,)
            <$> areq courseName "" Nothing
            <*> aopt textareaField (bfs ("Course Description"::Text)) Nothing
            <*> aopt (jqueryDayField def) (bfs ("Start Date"::Text)) Nothing
            <*> aopt (jqueryDayField def) (bfs ("End Date"::Text)) Nothing
            <*> aopt intField (bfs ("Total Points for Course"::Text)) Nothing
    where courseName :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Text 
          courseName = hiddenField

saveTo thedir fn file = do
        datadir <- appDataRoot <$> (appSettings <$> getYesod)
        let path = datadir </> thedir
        liftIO $ 
            do createDirectoryIfMissing True path
               e <- doesFileExist (path </> fn)
               if e then removeFile (path </> fn) else return ()
               fileMove file (path </> fn)

classWidget :: Text -> Entity Course -> HandlerT App IO Widget
classWidget ident classent = do
       let cid = entityKey classent
           course = entityVal classent
           mprobs = readAssignmentTable <$> courseTextbookProblems course :: Maybe (IntMap UTCTime)
       allUserData <- map entityVal <$> (runDB $ selectList [UserDataEnrolledIn ==. Just cid] [])
       asmd <- runDB $ selectList [AssignmentMetadataCourse ==. cid] []
       asDocs <- mapM (runDB . get) (map (assignmentMetadataDocument . entityVal) asmd)
       let allUids = (map userDataUserId  allUserData)
       musers <- mapM (\x -> runDB (get x)) allUids
       let users = catMaybes musers
       allScores <- zip (map userIdent users) <$> mapM (scoreByIdAndClassPerProblem cid) allUids 
       let usersAndData = zip users allUserData
       (Just course) <- runDB $ get cid
       return [whamlet|
                    <h2>Assignments
                    <table.table.table-striped>
                        <thead>
                            <th> Assignment
                            <th> Due Date
                            <th> Submissions
                            <th> High Score
                            <th> Low Score
                            <th> Submission Average
                        <tbody>
                            $maybe probs <- mprobs
                                $forall (set,due) <- Data.IntMap.toList probs
                                    <tr>
                                        <td>Problem Set #{show set}
                                        <td>#{dateDisplay due course}
                                        ^{analyticsFor (Right (pack (show set))) allScores}
                        $forall (Entity k a, Just d) <- zip asmd asDocs
                            <tr>
                                <td>
                                    <a href=@{AssignmentR $ documentFilename d}>
                                        #{documentFilename d}
                                $maybe due <- assignmentMetadataDuedate a
                                    <td>#{dateDisplay due course}
                                $nothing
                                    <td>No Due Date
                                ^{analyticsFor (Left k) allScores}
                    <h2>Students
                    <table.table.table-striped>
                        <thead>
                            <th> Registered Student
                            <th> Student Name
                            <th> Total Score
                            <th> Action
                        <tbody>
                            $forall (u,UserData fn ln _ _ _) <- usersAndData
                                <tr#student-#{userIdent u}>
                                    <td>
                                        <a href=@{UserR (userIdent u)}>#{userIdent u}
                                    <td>
                                        #{ln}, #{fn}
                                    <td>
                                        #{totalByUser (userIdent u) allScores}/#{show $ courseTotalPoints course}
                                    <td>
                                        <button.btn.btn-sm.btn-secondary type="button" onclick="tryDropStudent('#{decodeUtf8 $ encode $ DropStudent $ userIdent u}')">
                                            <i.fa.fa-trash-o>
                    <h2>Course Data
                    <dl.row>
                        <dt.col-sm-3>Course Title
                        <dd.col-sm-9>#{courseTitle course}
                        $maybe desc <- courseDescription course
                            <dd.col-sm-9.offset-sm-3>#{desc}
                        <dt.col-sm-3>Points Available
                        <dd.col-sm-9>#{courseTotalPoints course}
                        <dt.col-sm-3>Start Date
                        <dd.col-sm-9>#{dateDisplay (courseStartDate course) course}
                        <dt.col-sm-3>End Date
                        <dd.col-sm-9>#{dateDisplay (courseEndDate course) course}
                        <dt.col-sm-3>Time Zone
                        <dd.col-sm-9>#{decodeUtf8 $ courseTimeZone course}
                    <button.btn.btn-sm.btn-secondary type="button"  onclick="modalEditCourse('#{courseTitle course}')">
                        Edit Course Information
                    <button.btn.btn-sm.btn-secondary type="button" onclick="location.href='@{InstructorDownloadR ident (courseTitle course)}';">
                        Export Grades as .csv
                    <button.btn.btn-sm.btn-danger type="button" onclick="tryDeleteCourse('#{decodeUtf8 $ encode $ DeleteCourse (courseTitle course)}')">
                        Delete Course
              |]
    where totalByUser uident scores = case lookup uident scores of
                                Just xs -> show $ foldr (+) 0 (map snd xs)
                                Nothing -> "can't find scores"
          analyticsFor assignment scores = 
                do --list the per-problem scores of each user for this assignment
                   let thescores = map (\(x,y) -> map snd $ filter (\x -> fst x == assignment) y) scores
                       --extract data
                       submissions = filter (/= []) thescores
                       thereareany = length submissions > 0
                       totals = map sum submissions
                       highscore = if thereareany then show (L.maximum totals) else "N/A"
                       lowscore = if thereareany then show (L.minimum totals) else "N/A"
                       average = if thereareany then  show $ sum totals `div` length submissions else "N/A"
                   [whamlet|
                          <td>
                              #{length submissions}/#{length thescores}
                          <td>
                              #{highscore}
                          <td>
                              #{lowscore}
                          <td>
                              #{average}
                          |]
classCSV :: Entity Course -> HandlerT App IO Content
classCSV classent = do
       let cid = entityKey classent
           course = entityVal classent
           mprobs = readAssignmentTable <$> courseTextbookProblems course :: Maybe (IntMap UTCTime)
       allUserData <- map entityVal <$> (runDB $ selectList [UserDataEnrolledIn ==. Just cid] [])
       let allUids = (map userDataUserId  allUserData)
       musers <- mapM (\x -> runDB (get x)) allUids
       let users = catMaybes musers
       rawScores <- mapM (scoreByIdAndClassPerProblem cid) allUids >>= mapM (filterM (forClass classent . fst)) >>= mapM (mapM fixLabel)
       let allScores = zip (map userIdent users) rawScores
           usersAndData = zip users allUserData
           scoreHeaders = (L.nub . map fst . concat $ rawScores)
           header = commaSep $ ["Registered Student", "Last Name", "First Name"] ++ scoreHeaders ++ ["Total Score"]
           body = concat $ map (\x -> toRow allScores scoreHeaders x) usersAndData 
       return $ toContent $ header ++ body
    where toRow scores headers (u,UserData fn ln _ _ _) = commaSep $ [userIdent u, ln, fn] 
                                                                ++ byAssignment (userIdent u) scores headers 
                                                                ++ [pack (totalByUser (userIdent u) scores)]
          totalByUser uident scores = case lookup uident scores of
                                Just xs -> show $ foldr (+) 0 (map snd xs)
                                Nothing -> "can't find scores"
          byAssignment uident scores headers = case lookup uident scores of
                                Nothing -> map (const "-") headers
                                Just xs -> map (\h -> pack . show . foldr (+) 0 . map snd . filter (\x -> fst x == h) $ xs) headers
          commaSep l = "\"" ++ intercalate "\",\"" l ++ "\",\n"

          forClass classent (Left amid) = runDB $ do masgn <- get amid
                                                     case assignmentMetadataCourse <$> masgn of
                                                         Just cid -> return (cid == entityKey classent)
                                                         Nothing -> return False
          forClass classent (Right psn) = case courseTextbookProblems (entityVal classent) of
                                              Nothing -> return False
                                              Just (BookAssignmentTable probs) -> 
                                                case readMaybe (unpack psn) of
                                                    Just n -> return $ n `member` probs
                                                    Nothing -> return False

          fixLabel (Left amid,x) = runDB $ do masgn <- get amid
                                              case masgn of 
                                                    Nothing -> return $ (pack (show amid),x)
                                                    Just asgn -> do 
                                                        mdoc <- get $ assignmentMetadataDocument asgn
                                                        case mdoc of 
                                                            Nothing -> return $ (pack (show amid),x)
                                                            Just doc -> return $ (documentFilename doc,x)
          fixLabel (Right psn,x) = return ("Problem Set " ++ psn,x)

dateDisplay utc course = case tzByName $ courseTimeZone course of
                             Just tz  -> formatTime defaultTimeLocale "%F %R %Z" $ utcToZonedTime (timeZoneForUTCTime tz utc) utc
                             Nothing -> formatTime defaultTimeLocale "%F %R UTC" $ utc

-- TODO compare directory contents with database results
listAssignmentMetadata theclass = do asmd <- runDB $ selectList [AssignmentMetadataCourse ==. theclass] []
                                     return asmd
