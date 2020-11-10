## Author : Shaurya Jauhari
## Last Reviewed: September 11th, 2020.
## Description: This function allows a user to create a directory structure (in the present working
## directory) with a folder and several nested sub-folders. As arguments, the name of the parent 
## folder, a list of names of sub-folders, and a list of names of sub-sub-folders are to be supplied.

createFolderTree <- function(folderName, subFolderList, subsubFolderList) {
  # Parent directory
  dir.create(folderName)
  
  # Create sub-folders by the name of cell-types. 
  for (i in 1:length(subFolderList))
  {
    dir.create(paste0("./", folderName, "/", subFolderList[i]))
  }
  
  
  # Create sub-sub-folders by the name of features.
  for (i in 1:length(subFolderList))
  {
    for(j in 1:length(subsubFolderList))
    {
      dir.create(paste0(paste0(getwd(), "/", folderName, "/", subFolderList[i]),paste0("/",subsubFolderList[j])))
    }
  }
}

