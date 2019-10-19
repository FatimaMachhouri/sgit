# sgit

## Description
The "sgit" project is a version-control tool written in Scala language programming. It is largely inspired by the git tool.
This project contains also tests.

## How to install sgit

*Pre-requisite : Make sure you have sbt installed.*

1. Clone this repository : `git clone https://github.com/FatimaMachhouri/sgit.git`
2. Go to the cloned repository : `cd sgit`
3. Run the following command : `sbt assembly`
4. Add this line in your bash file : `export PATH="path/sgit/target/scala-2.13/:$PATH"`. Replace path by the path to the cloned repository. 
5. Refresh your bash file to take into account the changes.


## Documentation
*Find below the list of commands you can run.*

### sgit init
Permits to initialize a repository as a sgit repository.
### sgit add [list of files]
Permits to add to the stage the list of files parameter.
### sgit commit
Permits to commit the content of the stage.
### sgit status
Permits to show the state of the working tree (=working directory) : The untracked files, the modified files, the files to commit...
### sgit diff
Permits to show differences between what was added and the working tree.
### sgit log
Permits to list all commits.
### sgit log -p
Permits to show differences between commits.
### sgit log --stat
Permits to show stats about commits.
### sgit branch [branchName]
Permits to create a branch with the branchName parameter. The branch will be associated to the last commit.
### sgit tag [tagName]
Permits to create a tag with the tagName parameter. The tag will be associated to the last commit.
### sgit branch
Permits to list all branch names and tag names.
### sgit branch -a
Permits to list all branch names and tag names.
### sgit branch -v
Permits to list all branch names and tag names and the last commit associated to each of them.
### sgit branch -av
Permits to list all branch names and tag names and the last commit associated to each of them.

