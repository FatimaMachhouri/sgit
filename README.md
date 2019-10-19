# sgit

**Description** 
The "sgit" project is a version-control tool (as git) written in Scala language programming.

**How to install sgit**

*Pre-requisite : Make sure you have sbt installed.*

1. Clone this repository : `git clone https://github.com/FatimaMachhouri/sgit.git`
2. `cd sgit`
3. `sbt assembly`
4. Add this line in your bash file : `export PATH="path/sgit/target/scala-2.13/:$PATH"`. Replace path by the path to the cloned repository. 
5. Refresh your bash file to take into account the changes


**Documentation**
*Find below the list of commands you can run*

***sgit init***
***sgit add [list of files]***
***sgit commit***
***sgit status***
***sgit diff***
***sgit log***
***sgit log -p***
***sgit log --stat***
***sgit branch [branchName]***
***sgit tag [tagName]***
***sgit branch***
***sgit branch -a***
***sgit branch -v***
***sgit branch -av***
