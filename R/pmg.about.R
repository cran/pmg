pmg.about = function(container=NULL) {

## image is group pmg via www.geom.uiuc.edu/~dpvc
  
  group = ggroup(horizontal=FALSE,container=container)
  size(group) <-  c(500,500)
  theFactsMam = read.dcf(system.file("DESCRIPTION",package="pmg"))
  glabel(Paste(
               "<b> P M G</b>\n",
               "<i>",
               theFactsMam[1,'Title'],
               "</i>\n",
               "Version ",
               theFactsMam[1,"Version"],
               "\n\n",
               theFactsMam[1,'URL'],
               "\n",
               "Comments to pmgRgui@gmail.com\n",
               "\n\n",
               theFactsMam[1,"Description"],
               "\n"
               ), markup=TRUE, container=group)

  return(group)
}

pmg.about.R = function(container=NULL) {

## image is group pmg via www.geom.uiuc.edu/~dpvc
  
  group = ggroup(horizontal=FALSE,container=container)
  gimage(system.file("images","Rlogo.jpg",package="pmg"),  container=group)
  glabel(paste(
               "<b> R </b>",
               "is a free software environment for statistical\n",
               "computing and graphics.\n\n",
               "<i>http://www.r-project.org</i>\n\n",
               R.version.string,
#               "Version ",
#               paste(R.version$major,R.version$minor,sep="."),
               "\n\n",
               sep=" ", collapse=" "
               ), markup=TRUE, container=group)

  return(group)
}
