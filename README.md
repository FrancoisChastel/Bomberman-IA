# Prolog project
### Presentation
Project made during INSA Lyon's studies for AI. The project aims to develop an IA on Bomberman.

## Getting started
### Requirements
You need to have a recent version of SWIPL installed. Have a look on the [official website](http://www.swi-prolog.org).
### Execution 
First, you need to clone this repository.
```shell
git clone https://github.com/FrancoisChastel/Bomberman-IA.git
```
After this, you can have execute the tests by going in the dir test and executing the script test.sh 
```shell
cd Bomberman-IA
bash test/test.sh
```
If everything is ok, you must launch an Apache Server and a MySQL Server
Then, you have to create a Virtual Host referencing Bomberman-IA/IHM/web/ with DocumentIndex 'app.php'

Launch your browser and have a look at 'yourHost'

Then, launch your prolog server by typing

```shell
bash core/run.sh
```

To finish, write 'http://localhost:8080/' in Server Adress field and choose the number of players you want (max 4)

Now you can play ;) 

## Authors
* Christopher Aparicio - christopher.aparicio@insa-lyon.fr
* Antoine Breton - antoine.breton@insa-lyon.fr
* François Chastel - francois.chastel@insa-lyon.fr
* Nathan Haim - nathan.haim@insa-lyon.fr
* Antoine Payan - antoine.payan@insa-lyon.fr
* Théophile Sayer - theophile.sayer@insa-lyon.fr
