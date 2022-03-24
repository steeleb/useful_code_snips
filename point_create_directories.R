# Create folders if those folders do not exist

dir_x = 'folderPathThatDoesntExist1'
dir_y = 'folderPathThatDoesntExist2'
dir_z = 'folderPathThatDoesntExist3'

folder_paths <- c(dir_x, dir_y, dir_z)

for(i in 1:length(folder_paths)){
  path_sel <- folder_paths[i]
  if(!dir.exists(path_sel)){
    dir.create(path_sel)}
}