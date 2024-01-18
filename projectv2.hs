type Name = String
type Content = String

data FileSystem = File Name Content | Directory Name [FileSystem]
    deriving (Show)


rootDirectory :: FileSystem
--rootDirectory = Directory "/" [Directory "dir1" [Directory "dir2" [File "file1" "sydyrjanie na fail nomer 1" , File "file2" "nigga nomer 2", File "file3" "sszz"], File "niggafile" "n content man"],Directory "shitty" [], File "datedrazni" "drazneshot e nali"]
rootDirectory= Directory "/" []
-- ################################################################################################################################################################################################################################################################

pureFromJust :: Maybe a -> a
pureFromJust value = case value of
                    Just result -> result

getDirName :: FileSystem -> String
getDirName (File name content)    = "{error}"
getDirName (Directory name files) = name

getFileName :: FileSystem -> String
getFileName (Directory name files) = "{error}"
getFileName (File name content)    = name

mytailHelper :: [a] -> Maybe [a]
mytailHelper [] = Nothing
mytailHelper (x:xs) = Just xs

myheadHelper :: [a] -> Maybe a
myheadHelper [] = Nothing
myheadHelper (x:xs) = Just x

myhead :: [a] -> a
myhead xs = pureFromJust (myheadHelper xs)

mytail :: [a] -> [a]
mytail xs = pureFromJust (mytailHelper xs)



--конвертира стринг от пър към лист от стрингове
getSomethingToList:: String -> Char -> [String]
getSomethingToList path sym = filter (not . null) $ split sym path
    where split _ [] = [""]
          split sep (x:xs)
              | x == sep = "" : rest
              | otherwise = (x : myhead rest) : mytail rest
              where rest = split sep xs


getElementsToList::String -> [String]
getElementsToList files = getSomethingToList files ' '


getPathToList :: String -> [String]
getPathToList path = getSomethingToList path '/'



-- тва за ся е с абсолютен път  на тва направи някъв експепшън!!!!!!                            
cd :: FileSystem -> [String] ->  Maybe FileSystem 
cd (Directory "/" files ) ["/"] = Just (Directory "/" files)
cd (Directory name []) ps = if (ps == []) then Just (Directory name []) else Nothing
cd dir [] = Just dir
cd (Directory name (file:files)) (p:ps) = if ((getDirName file) == p) 
                                        then (cd file ps)
                                        else (cd (Directory name files) (p:ps))


checkPathExists :: FileSystem -> [String] -> Bool
checkPathExists dir path = case (cd dir path) of
                            Just result -> True
                            Nothing -> False  

-- za file system podavash tva (cd path dir)
checkFileExists :: String -> Maybe FileSystem -> Bool
checkFileExists fname (Just (Directory dname [])) = False
checkFileExists fname (Just (Directory dname (d:ds))) =  (fname == (getFileName d) ) || (checkFileExists fname (Just(Directory dname ds)))


checkAllFilesExist :: [String] -> FileSystem -> FileSystem -> Bool
checkAllFilesExist [] curDir rootDir = True
checkAllFilesExist (f:fs) curDir rootDir = if((take 1 f) == "/") 
                                            then 
                                                let wholePath = getPathToList f
                                                    file = myhead (reverse wholePath)
                                                    dirsPath = reverse (mytail (reverse wholePath))
                                                in
                                            (checkFileExists file (cd rootDir dirsPath)) && (checkAllFilesExist fs curDir rootDir) 
                                            else if((take 1 f) == ".")
                                                then 
                                                    let wholePath = getPathToList (drop 1 f)
                                                        file = myhead (reverse wholePath)
                                                        dirsPath = reverse (mytail (reverse wholePath))
                                                    in
                                                    (checkFileExists file (cd curDir dirsPath)) && (checkAllFilesExist fs curDir rootDir) 
                                                else
                                                    let wholePath = getPathToList f
                                                        file = myhead (reverse wholePath)
                                                        dirsPath = reverse (mytail (reverse wholePath))
                                                    in
                                                    (checkFileExists file (cd curDir dirsPath)) && (checkAllFilesExist fs curDir rootDir)


checkDirectoryExists :: String -> Maybe FileSystem -> Bool
checkDirectoryExists fname (Just (Directory dname [])) = False
checkDirectoryExists fname (Just (Directory dname (d:ds))) =  (fname == (getDirName d) ) || (checkDirectoryExists fname (Just(Directory dname ds)))


checkAllDirectoriesExist :: [String] -> FileSystem -> FileSystem -> Bool
checkAllDirectoriesExist [] curDir rootDir = True
checkAllDirectoriesExist (f:fs) curDir rootDir = if((take 1 f) == "/") 
                                            then 
                                                let wholePath = getPathToList f
                                                    file = myhead (reverse wholePath)
                                                    dirsPath = reverse (mytail (reverse wholePath))
                                                in
                                            (checkDirectoryExists file (cd rootDir dirsPath)) && (checkAllDirectoriesExist fs curDir rootDir) 
                                            else if((take 1 f) == ".")
                                                then 
                                                    let wholePath = getPathToList (drop 1 f)
                                                        file = myhead (reverse wholePath)
                                                        dirsPath = reverse (mytail (reverse wholePath))
                                                    in
                                                    (checkDirectoryExists file (cd curDir dirsPath)) && (checkAllDirectoriesExist fs curDir rootDir) 
                                                else
                                                    let wholePath = getPathToList f
                                                        file = myhead (reverse wholePath)
                                                        dirsPath = reverse (mytail (reverse wholePath))
                                                    in
                                                    (checkDirectoryExists file (cd curDir dirsPath)) && (checkAllDirectoriesExist fs curDir rootDir)


-- Връща с една директория назад
revertPath :: String -> String
revertPath "/" = "/"
revertPath path = iter (reverse path)
    where iter (r:rs) = if (r == '/') then reverse rs else iter rs  

-- Има от типа ../dir1/../dir1/dir2
revertPathMultiple :: [String] -> String -> String
revertPathMultiple [] "" = "/"
revertPathMultiple [] path = if (take 2 path) == "//" then drop 1 path else path
revertPathMultiple (p:ps) path = if (p=="..") then revertPathMultiple ps (revertPath path) else  revertPathMultiple ps ( path ++ "/" ++ p)

ls :: FileSystem -> String
ls (Directory name []) = ""
ls (Directory name (file:files)) = (if((getDirName file)== "{error}") then (getFileName file) else (getDirName file) ) ++ " " ++ (ls (Directory name files))

 
-- HELPERS

readCharacters :: IO String
readCharacters = do
    input <- getLine
    if input == "."
        then return ""
        else do
            rest <- readCharacters
            return (input ++ rest)

contains :: Char -> String -> Bool
contains sym [] = False 
contains sym (x:xs) = (sym == x) || (contains sym xs)

containsInList :: String -> [String] -> Bool
containsInList el [] = False
containsInList el (x:xs) = (el == x) || (containsInList el xs)

getFileContent :: FileSystem -> Maybe String
getFileContent (Directory _  _) = Nothing
getFileContent (File name content) = Just content

openFile :: String -> Maybe FileSystem -> String
openFile file (Just (Directory name [])) = "{error}"
openFile file (Just (Directory name (f:fs))) = if ( file == (getFileName f) ) 
                                                then 
                                                    case (getFileContent f) of
                                                    Nothing -> "{error}"
                                                    Just content -> content
                                                else (openFile file ( Just (Directory name fs)))

openAllFiles :: [String] -> FileSystem -> FileSystem -> String
openAllFiles [] curDir rootDir = ""
openAllFiles (f:fs) curDir rootDir = if((take 1 f) == "/") 
                                   then 
                                    let wholePath = getPathToList f
                                        file = myhead (reverse wholePath)
                                        dirsPath = reverse (mytail (reverse wholePath))
                                    in
                                   (openFile file (cd rootDir dirsPath)) ++ " " ++  (openAllFiles fs curDir rootDir) 
                                   else if((take 1 f) == ".")
                                    then 
                                        let wholePath = getPathToList (drop 1 f)
                                            file = myhead (reverse wholePath)
                                            dirsPath = reverse (mytail (reverse wholePath))
                                        in
                                        (openFile file (cd curDir dirsPath)) ++ " " ++ (openAllFiles fs curDir rootDir) 
                                    else
                                        let wholePath = getPathToList f
                                            file = myhead (reverse wholePath)
                                            dirsPath = reverse (mytail (reverse wholePath))
                                        in
                                        (openFile file (cd curDir dirsPath)) ++ " " ++ (openAllFiles fs curDir rootDir)



addFileHelper :: String -> String -> [String] -> [FileSystem] -> FileSystem -> FileSystem
addFileHelper fname fcontent [] [] (Directory dname ds ) = (Directory dname ((File fname fcontent):ds) )
addFileHelper fname fcontent (p:ps) files (Directory dname (d:ds)) = if (p == (getDirName d)) 
                                                                        then (Directory dname ((reverse files) ++ ((addFileHelper fname fcontent ps [] d ):ds)))
                                                                        else (addFileHelper fname fcontent (p:ps) (d:files) (Directory dname ds) )

addFile :: String -> String -> [String] -> FileSystem -> FileSystem
addFile fname fcontent path directory = addFileHelper fname fcontent path [] directory


addDirectoryHelper :: String -> [String] -> [FileSystem] -> FileSystem -> FileSystem
addDirectoryHelper newdir [] [] (Directory dname ds ) = (Directory dname ((Directory newdir []):ds) )
addDirectoryHelper newdir (p:ps) files (Directory dname (d:ds)) = if (p == (getDirName d)) 
                                                                        then (Directory dname ((reverse files) ++ ((addDirectoryHelper newdir ps [] d ):ds)))
                                                                        else (addDirectoryHelper newdir (p:ps) (d:files) (Directory dname ds) )

addDirectory :: String -> [String] -> FileSystem -> FileSystem
addDirectory newdir path directory = addDirectoryHelper newdir path [] directory


-- само файлове трием , но не и директории
deleteFileHelper :: String -> [String] -> [FileSystem] -> FileSystem -> FileSystem
deleteFileHelper fname [] [] (Directory dname ds ) = (Directory dname (filter (\d -> fname /= (getFileName d) ) ds))
deleteFileHelper fname (p:ps) files (Directory dname (d:ds)) = if (p == (getDirName d)) 
                                                                        then (Directory dname ((reverse files) ++ ((deleteFileHelper fname ps [] d ):ds)) )
                                                                        else (deleteFileHelper fname (p:ps) (d:files) (Directory dname ds) )

deleteFiles :: [String] -> FileSystem -> FileSystem -> String -> (FileSystem,FileSystem)
deleteFiles [] rootDir curDir curpath = (curDir,rootDir)
deleteFiles (f:fs) rootDir curDir curpath = if((take 1 f) == "/") 
                                            then 
                                            let wholePath = getPathToList f
                                                file = myhead (reverse wholePath)
                                                dirsPath = reverse (mytail (reverse wholePath))
                                            in
                                            deleteFiles fs (deleteFileHelper file dirsPath [] rootDir)  (pureFromJust( cd (deleteFileHelper file dirsPath [] rootDir) (getPathToList curpath) )) curpath
                                            else if((take 1 f) == ".")
                                            then 
                                                let wholePath = getPathToList (drop 1 f)
                                                    file = myhead (reverse wholePath)
                                                    dirsPath = reverse (mytail (reverse wholePath))
                                                in
                                            deleteFiles fs (deleteFileHelper file ((getPathToList curpath) ++ dirsPath) [] rootDir)  (deleteFileHelper file dirsPath [] curDir) curpath
                                            else
                                                let wholePath = getPathToList f
                                                    file = myhead (reverse wholePath)
                                                    dirsPath = reverse (mytail (reverse wholePath))
                                                in
                                            deleteFiles fs (deleteFileHelper file ((getPathToList curpath) ++ dirsPath) [] rootDir)  (deleteFileHelper file dirsPath [] curDir) curpath

deleteDirectoriesHelper :: String -> [String] -> [FileSystem] -> FileSystem -> FileSystem
deleteDirectoriesHelper fname [] [] (Directory dname ds ) = (Directory dname (filter (\d -> fname /= (getDirName d) ) ds))
deleteDirectoriesHelper fname (p:ps) files (Directory dname (d:ds)) = if (p == (getDirName d)) 
                                                                        then (Directory dname ((reverse files) ++ ((deleteDirectoriesHelper fname ps [] d ):ds)) )
                                                                        else (deleteDirectoriesHelper fname (p:ps) (d:files) (Directory dname ds) )

deleteDirectories :: [String] -> FileSystem -> FileSystem -> String -> (FileSystem,FileSystem)
deleteDirectories [] rootDir curDir curpath = (curDir,rootDir)
deleteDirectories (f:fs) rootDir curDir curpath = if((take 1 f) == "/") 
                                            then 
                                            let wholePath = getPathToList f
                                                file = myhead (reverse wholePath)
                                                dirsPath = reverse (mytail (reverse wholePath))
                                            in
                                            deleteDirectories fs (deleteDirectoriesHelper file dirsPath [] rootDir)  (pureFromJust( cd (deleteDirectoriesHelper file dirsPath [] rootDir) (getPathToList curpath) )) curpath
                                            else if((take 1 f) == ".")
                                            then 
                                                let wholePath = getPathToList (drop 1 f)
                                                    file = myhead (reverse wholePath)
                                                    dirsPath = reverse (mytail (reverse wholePath))
                                                in
                                            deleteDirectories fs (deleteDirectoriesHelper file ((getPathToList curpath) ++ dirsPath) [] rootDir)  (deleteDirectoriesHelper file dirsPath [] curDir) curpath
                                            else
                                                let wholePath = getPathToList f
                                                    file = myhead (reverse wholePath)
                                                    dirsPath = reverse (mytail (reverse wholePath))
                                                in
                                            deleteDirectories fs (deleteDirectoriesHelper file ((getPathToList curpath) ++ dirsPath) [] rootDir)  (deleteDirectoriesHelper file dirsPath [] curDir) curpath



showFileSystem :: FileSystem -> IO ()
showFileSystem fs = showFileSystem' fs 0
  where
    showFileSystem' :: FileSystem -> Int -> IO ()
    showFileSystem' (File name content) spaceing =
        putStrLn $ replicate spaceing '\t' ++ "File " ++ name 
    showFileSystem' (Directory name files) spaceing = do
        putStrLn $ replicate spaceing '\t' ++ "Directory " ++ name
        mapM_ (\child -> showFileSystem' child (spaceing + 1)) files --monad map hahahaha

-- ################################################################################################################################################################################################################################################################


runFileSystem :: FileSystem -> FileSystem -> String -> IO()
runFileSystem oldrootDir currootDir path = do
    putStr "$ "
    userInput <- getLine
    
    case getElementsToList userInput of
        [] -> runFileSystem oldrootDir currootDir path
        otherwise -> let 
                        elements = getElementsToList userInput
                        function = myhead elements
                        arguments = mytail elements
                     in
                     case function of
                        "exit" ->  putStrLn "Successfully exited the file system" 
                        "cd" -> if (arguments == [] ) 
                                then runFileSystem oldrootDir oldrootDir "/" 
                                else 
                                    if (length arguments) > 1
                                    then 
                                    do
                                        putStrLn $ "$ " ++ "Error.Path does not exists!"
                                        runFileSystem oldrootDir currootDir path 
                                    else 
                                        let inputPath = myhead arguments -- should be only one in the arguments
                                        in
                                        if (containsInList ".." (getPathToList inputPath)) 
                                        then 
                                            let pathResult = revertPathMultiple (getPathToList inputPath) path
                                                dirResult = cd oldrootDir (getPathToList pathResult)
                                            in
                                            case dirResult of 
                                                        Just result -> runFileSystem oldrootDir result pathResult
                                                        Nothing ->  do
                                                                    putStrLn $ "$ " ++ "Error.Path does not exists!"
                                                                    runFileSystem oldrootDir currootDir path 
                                        else
                                            if ((take 1 inputPath)==".")
                                            then 
                                                let dirResult = cd currootDir (getPathToList (drop 1 inputPath))
                                                    pathResult = if take 2 (path ++ (drop 1 inputPath)) == "//" then drop 1 (path ++  (drop 1 inputPath)) else (path ++  (drop 1 inputPath)) 
                                                in
                                                case dirResult of 
                                                            Just result -> runFileSystem oldrootDir result pathResult
                                                            Nothing ->  do
                                                                        putStrLn $ "$ " ++ "Error.Path does not exists!"
                                                                        runFileSystem oldrootDir currootDir path 
                                            else 
                                                if ((take 1 inputPath)=="/")
                                                then                                                         
                                                    let dirResult = cd oldrootDir (getPathToList inputPath)
                                                    in
                                                    case dirResult of 
                                                                Just result -> runFileSystem oldrootDir result inputPath
                                                                Nothing ->  do
                                                                            putStrLn $ "$ " ++ "Error.Path does not exists!"
                                                                            runFileSystem oldrootDir currootDir path 
                                                else 
                                                    let dirResult = cd currootDir (getPathToList inputPath)
                                                        pathResult = if take 2 (path ++ "/" ++ inputPath) == "//" then drop 1 (path ++ "/" ++ inputPath) else (path ++ "/" ++ inputPath)
                                                    in
                                                    case dirResult of 
                                                                Just result -> runFileSystem oldrootDir result pathResult
                                                                Nothing ->  do
                                                                            putStrLn $ "$ " ++ "Error.Path does not exists!"
                                                                            runFileSystem oldrootDir currootDir path 

                        "pwd" -> do 
                                putStrLn $ "$ " ++ path
                                runFileSystem oldrootDir currootDir path

                        "ls" ->  if (arguments == []) 
                                then
                                    do
                                        putStrLn $ "$ " ++ (ls currootDir)
                                        runFileSystem oldrootDir currootDir path 
                                else 
                                    if (length arguments) > 1
                                    then 
                                        do
                                        putStrLn $ "$ " ++ "Error.Path does not exists!"
                                        runFileSystem oldrootDir currootDir path 
                                    else
                                        let 
                                            input = myhead arguments 
                                        in       
                                        if((take 1 input) == "." )
                                        then
                                            let inputPath = (drop 1 input)
                                                dirResult = cd currootDir (getPathToList inputPath)
                                            in
                                            case dirResult of 
                                                        Just result -> do 
                                                                        putStrLn $ "$" ++ (ls result)
                                                                        runFileSystem oldrootDir currootDir path 
                                                        Nothing ->  do
                                                                    putStrLn $ "$ " ++ "Error.Path does not exists!"
                                                                    runFileSystem oldrootDir currootDir path 
                                        else
                                            if((take 1 input) == "/")
                                            then 
                                                let inputPath = input
                                                    dirResult = cd oldrootDir (getPathToList inputPath)
                                                in
                                                case dirResult of 
                                                        Just result -> do 
                                                                        putStrLn $ "$" ++ (ls result)
                                                                        runFileSystem oldrootDir currootDir path 
                                                        Nothing ->  do
                                                                    putStrLn $ "$ " ++ "Error.Path does not exists!"
                                                                    runFileSystem oldrootDir currootDir path 
                                            else
                                                let inputPath = input
                                                    dirResult = cd currootDir (getPathToList inputPath)
                                                in
                                                case dirResult of 
                                                        Just result -> do 
                                                                        putStrLn $ "$" ++ (ls result)
                                                                        runFileSystem oldrootDir currootDir path 
                                                        Nothing ->  do
                                                                    putStrLn $ "$ " ++ "Error.Path does not exists!"
                                                                    runFileSystem oldrootDir currootDir path 
                        
                        "cat" ->  case arguments of
                                        [] -> do
                                            putStrLn $ "$ " ++ "Error.No arguments given!"
                                            runFileSystem oldrootDir currootDir path
                                        otherwise ->  if (not (contains '>' userInput ))  
                                                        then
                                                            let 
                                                                filesList = arguments
                                                                output = (openAllFiles filesList currootDir oldrootDir )
                                                            in 
                                                            if (containsInList "{error}" (getElementsToList output) )
                                                            then 
                                                                do
                                                                putStrLn $ "$ " ++ "Error.Given non-existing files!"
                                                                runFileSystem oldrootDir currootDir path
                                                            else 
                                                                do 
                                                                putStrLn $ "$ " ++ output
                                                                runFileSystem oldrootDir currootDir path
                                                        else --contains '>' 
                                                            let                                    --filma tuka e kyde da se zapomni noviq fail
                                                            filePath = myhead (reverse arguments) --faila koito shte se zapomnq
                                                            in 
                                                            if(checkAllFilesExist [filePath] currootDir oldrootDir) --here
                                                            then do  
                                                                    putStrLn $ "$ " ++ "Error.File already exist!"
                                                                    runFileSystem oldrootDir currootDir path 

                                                            else 
                                                            if ((take 1 filePath) == "/")
                                                            then 
                                                                let wholePath = getPathToList filePath
                                                                    file = myhead (reverse wholePath)
                                                                    dirsPath = reverse (mytail (reverse wholePath))
                                                                in               
                                                                if not(checkPathExists oldrootDir dirsPath) 
                                                                then
                                                                    do 
                                                                    putStrLn $ "$ " ++ "Error.Path to new file does not exist!"
                                                                    runFileSystem oldrootDir currootDir path  
                                                                else                         
                                                                    if ((myhead arguments) == ">")
                                                                    then
                                                                        do
                                                                        content <- readCharacters
                                                                        runFileSystem  (addFile file content dirsPath oldrootDir)  (pureFromJust(cd (addFile file content dirsPath oldrootDir) (getPathToList path) )) path
                                                                    else  
                                                                        let filesContentList = mytail (mytail (reverse arguments)) 
                                                                            content = (openAllFiles filesContentList currootDir oldrootDir)
                                                                        in
                                                                        if (containsInList "{error}" (getElementsToList content))
                                                                        then
                                                                            do
                                                                            putStrLn $ "$ " ++ "Error.Given non-existing files!"
                                                                            runFileSystem oldrootDir currootDir path    
                                                                        else
                                                                            do                                               
                                                                            runFileSystem  (addFile file content dirsPath oldrootDir)  (pureFromJust(cd (addFile file content dirsPath oldrootDir) (getPathToList path))) path -- do tuk e perfektno
                                                            else 
                                                                if((take 1 filePath) == ".")
                                                                then 
                                                                    let wholePath = getPathToList (drop 1 filePath)
                                                                        file = myhead (reverse wholePath)
                                                                        dirsPath = reverse (mytail (reverse wholePath))
                                                                    in 
                                                                    if not(checkPathExists currootDir dirsPath) 
                                                                    then
                                                                    do 
                                                                    putStrLn $ "$ " ++ "Error.Path to new file does not exist!"
                                                                    runFileSystem oldrootDir currootDir path      
                                                                    else                                        
                                                                        if ((myhead arguments) == ">")
                                                                        then
                                                                            do 
                                                                            content <- readCharacters
                                                                            runFileSystem  (addFile file content ((getPathToList path) ++ (dirsPath)) oldrootDir)  (addFile file content dirsPath currootDir) path
                                                                        else 
                                                                        let filesContentList = mytail (mytail (reverse arguments)) 
                                                                            content = (openAllFiles filesContentList currootDir oldrootDir)
                                                                        in
                                                                        do
                                                                        runFileSystem  (addFile file content ((getPathToList path) ++ (dirsPath)) oldrootDir)  (addFile file content dirsPath currootDir) path
                                                                else 
                                                                    let wholePath = getPathToList filePath
                                                                        file = myhead (reverse wholePath)
                                                                        dirsPath = reverse (mytail (reverse wholePath))
                                                                    in 
                                                                    if not(checkPathExists currootDir dirsPath) 
                                                                    then
                                                                        do 
                                                                        putStrLn $ "$ " ++ "Error.Path to new file does not exist!"
                                                                        runFileSystem oldrootDir currootDir path      
                                                                    else                                        
                                                                        if ((myhead arguments) == ">")
                                                                        then
                                                                            do 
                                                                            content <- readCharacters
                                                                            runFileSystem  (addFile file content ((getPathToList path) ++ (dirsPath)) oldrootDir)  (addFile file content dirsPath currootDir) path
                                                                        else 
                                                                            let filesContentList = mytail (mytail (reverse arguments)) 
                                                                                content = (openAllFiles filesContentList currootDir oldrootDir)
                                                                            in
                                                                            do
                                                                            runFileSystem  (addFile file content ((getPathToList path) ++ (dirsPath)) oldrootDir)  (addFile file content dirsPath currootDir) path
                                  
                        
                        "rm" -> case arguments of
                                    [] -> do
                                            putStrLn $ "$ " ++ "Error.No arguments given!"
                                            runFileSystem oldrootDir currootDir path
                                    otherwise -> 
                                                if (myhead arguments ) == "-d" 
                                    -- BONU$$$$$$$$$$$
                                                then 
                                                    if (mytail arguments) == []
                                                      then 
                                                        do
                                                            putStrLn $ "$ " ++ "Error.No directories are given!"
                                                            runFileSystem oldrootDir currootDir path
                                                      else
                                                        let
                                                            dirs = mytail arguments
                                                        in
                                                        if (checkAllDirectoriesExist dirs currootDir oldrootDir)
                                                            then  
                                                                let
                                                                    result = deleteDirectories dirs oldrootDir currootDir path --here
                                                                in
                                                                do
                                                                runFileSystem (fst result) (snd result) path
                                                            else
                                                               do
                                                                putStrLn $ "$ " ++ "Error.Given non-existing directories!"
                                                                runFileSystem oldrootDir currootDir path 
                                                else 
                                                if (checkAllFilesExist arguments currootDir oldrootDir)
                                                 then
                                                    let
                                                        result = deleteFiles arguments oldrootDir currootDir path
                                                    in
                                                    do
                                                    runFileSystem (fst result) (snd result) path
                                                 else 
                                                    do
                                                    putStrLn $ "$ " ++ "Error.Given non-existing files!"
                                                    runFileSystem oldrootDir currootDir path
                                
                        -- BONU$$$$$$$$$$$
                        "mkdir" -> case arguments of
                                    [] -> do
                                            putStrLn $ "$ " ++ "Error.No arguments given!"
                                            runFileSystem oldrootDir currootDir path
                                    otherwise ->  let                                    
                                                    filePath = myhead arguments       
                                                    in 
                                                if (length arguments) > 1
                                                then 
                                                do
                                                putStrLn $ "$ " ++ "Error.Too many arguments given!"
                                                runFileSystem oldrootDir currootDir path 
                                                else
                                                if(checkAllDirectoriesExist [filePath] currootDir oldrootDir) --here
                                                    then do  
                                                        putStrLn $ "$ " ++ "Error.Directory already exist!"
                                                        runFileSystem oldrootDir currootDir path 

                                                else 
                                                if ((take 1 filePath) == "/")
                                                then 
                                                    let wholePath = getPathToList filePath
                                                        newdir = myhead (reverse wholePath)
                                                        dirsPath = reverse (mytail (reverse wholePath))
                                                    in               
                                                    if not(checkPathExists oldrootDir dirsPath) 
                                                    then
                                                        do 
                                                        putStrLn $ "$ " ++ "Error.Path to new directory does not exist!"
                                                        runFileSystem oldrootDir currootDir path  
                                                    else                           
                                                        do                                               
                                                        runFileSystem  (addDirectory newdir dirsPath oldrootDir)  (pureFromJust(cd (addDirectory newdir dirsPath oldrootDir) (getPathToList path))) path -- do tuk e perfektno
                                                else 
                                                    if((take 1 filePath) == ".")
                                                    then 
                                                        let wholePath = getPathToList (drop 1 filePath)
                                                            newdir = myhead (reverse wholePath)
                                                            dirsPath = reverse (mytail (reverse wholePath))
                                                        in 
                                                        if not(checkPathExists currootDir dirsPath) 
                                                        then
                                                        do 
                                                        putStrLn $ "$ " ++ "Error.Path to new directory does not exist!"
                                                        runFileSystem oldrootDir currootDir path      
                                                        else                                        
                                                            do
                                                            runFileSystem  (addDirectory newdir ((getPathToList path) ++ (dirsPath)) oldrootDir)  (addDirectory newdir dirsPath currootDir) path
                                                    else 
                                                        let wholePath = getPathToList filePath
                                                            newdir = myhead (reverse wholePath)
                                                            dirsPath = reverse (mytail (reverse wholePath))
                                                        in 
                                                        if not(checkPathExists currootDir dirsPath) 
                                                        then
                                                            do 
                                                            putStrLn $ "$ " ++ "Error.Path to new file does not exist!"
                                                            runFileSystem oldrootDir currootDir path      
                                                        else                                        
                                                            do
                                                            runFileSystem  (addDirectory newdir ((getPathToList path) ++ (dirsPath)) oldrootDir)  (addDirectory newdir dirsPath currootDir) path
                        


                        -- BONU$$$$$$$$$$$
                        "tree" ->  if (arguments == [] ) 
                                    then 
                                        do
                                        showFileSystem currootDir
                                        runFileSystem oldrootDir currootDir path 
                                    else 
                                        if (length arguments) > 1
                                        then 
                                            do
                                            putStrLn $ "$ " ++ "Error.Path does not exists!"
                                            runFileSystem oldrootDir currootDir path 
                                        else                             
                                            let inputPath = myhead arguments -- should be only one in the arguments
                                            in
                                                if ((take 1 inputPath)==".")
                                                then 
                                                    let dirResult = cd currootDir (getPathToList (drop 1 inputPath))
                                                    in
                                                    case dirResult of 
                                                                Just result -> do 
                                                                                showFileSystem result
                                                                                runFileSystem oldrootDir currootDir path
                                                                Nothing ->  do
                                                                            putStrLn $ "$ " ++ "Error.Path does not exists!"
                                                                            runFileSystem oldrootDir currootDir path 
                                                else 
                                                    if ((take 1 inputPath)=="/")
                                                    then                                                         
                                                        let dirResult = cd oldrootDir (getPathToList inputPath)
                                                        in
                                                        case dirResult of 
                                                                    Just result -> do 
                                                                                    showFileSystem result
                                                                                    runFileSystem oldrootDir currootDir path
                                                                    Nothing ->  do
                                                                                putStrLn $ "$ " ++ "Error.Path does not exists!"
                                                                                runFileSystem oldrootDir currootDir path 
                                                    else 
                                                        let dirResult = cd currootDir (getPathToList inputPath)
                                                        in
                                                        case dirResult of 
                                                                    Just result -> do 
                                                                                    showFileSystem result
                                                                                    runFileSystem oldrootDir currootDir path
                                                                    Nothing ->  do
                                                                                putStrLn $ "$ " ++ "Error.Path does not exists!"
                                                                                runFileSystem oldrootDir currootDir path 



                        otherwise -> do
                                    putStrLn $ "$ " ++ "Error.Unrecognized function!"
                                    runFileSystem oldrootDir currootDir path

                



main :: IO()

main = do
    putStrLn "Welcome to the file system"
    runFileSystem rootDirectory rootDirectory "/"



