

***********************I- Primo etape data: importation  création  transformation des données*******************************************************;

*Definition de la librairie*  ;
libname projet'C:\Users\Hounkponou\Desktop\PROJET SAS';

*Importation de la base de donnée brute*;
proc import datafile='C:\Users\Hounkponou\Desktop\PROJET SAS\base.xlsx'
out=base dbms=xlsx replace ;
run;

*Deplacement de la base dans le dossier projet*;
data projet.base;
set base;
run;

/*trie des données par individu (pays) et par période (années)*/
proc sort data=projet.base;
by id annee;
run;

/* calcul du taux d'ouverture*/
data base;
set projet.base;
touv = ((export+import)/(import+pib))*100;
run;

*********************************II- Analyse descriptive********************************************************;

*Test d'autocorrélation des variable*;
proc corr data= base;
Var TPIB tdebt tinf tinv Touv TSCO SPO ;
RUN;

*statistiques descriptives du modèle*;
proc means data=base;
Var TPIB tdebt tinf tinv Touv TSCO TPOP SPO ;
run;
data BZ1;
set base;
run;

************************III-Analyse econometrique prealable: test de spécification...*******************************;

			*Estimation MCO*;
 proc reg data=bz1 ; 
by id pays Annee ;
model TPIB=  tdebt tinf tinv Touv TSCO TPOP SPO ;
run;*l'hypothèse nulle d'homogénéité a été rejeté suite à ce test* Ce qui nous pousse donc à déterminer la nature de l'hétérogéneité en faisant
un test de Hausman;

*Estimation POOLED=MCO;
 proc panel data=bz1 ; 
id pays Annee ;
model TPIB=tdebt tinf tinv Touv TSCO TPOP SPO /POOLED; 
run;

*Choix effets fixe effet aléatoire*;
		*Random Effects*;
 proc panel data=bz1 ; 
id pays Annee ;
model tpib = tdebt tinf tinv Touv TSCO TPOP SPO /RANONE; 
run;*Suite au test,la statistique de hausman est : m=14,5 la Khi-deux(6)=2,16 (alpha=0.05)  p-value=0.04 
							on rejete donc Ho pour présence d'effet aleatoire*/; 

		*Fixed Effects*;
proc panel;
id pays Annee ;
model TPIB= tdebt tinf tinv Touv TSCO TPOP SPO  /FIXONE; 
run;*Rejet de Ho d'absence d'effet fixe à l'issue du test*;

		*Effet fixe procédure proc reg*;
proc reg data=bz1;
id pays Annee;
model TPIB=tdebt tinf tinv Touv TSCO TPOP SPO ;
run;*Estimation biaisé*;

		*Test d'hétéroscédasticité Breusch-Pagan*;
proc model data=BZ1;
parms b0 b1 b2 b3 b4 b5 b6 b7 ;
TPIB=b0+b1*tdebt+b2*tpop+b3*tinv+b4*Touv+b5*tinf +b6*TSCO +b7*SPO;
fit TPIB/Pagan =(1 tdebt tpop tinv Touv tinf TSCO SPO);
run;

	/*TEST DE NORMALITE DES RESIDUS*/
proc reg data=BZ1;
model TPIB= tdebt tinf tinv Touv TSCO TPOP SPO ;
output out=resid1 residual=uhat;
run;
proc univariate data=resid1 NORMAL PLOT;
var uhat;
QQPLOT uhat/NORMAL (MU=EST SIGMA=EST COLOR=RED L=1);
run;
proc capability data=resid1 NORMAL;
var uhat;
QQPLOT uhat /NORMAL(MU=EST SIGMA=EST COLOR=RED L=1);
PPPLOT uhat /NORMAL(MU=EST SIGMA=EST COLOR=RED L=1);
HISTOGRAM /NORMAL (COLOR=MAROON W=4) CFILL=BLUE CFRAME=LIGR;
INSET MEAN STD /CFILL=BLANK FORMAT=5.2;
run;
PROC AUTOREG data=BZ1;
model TPIB= tdebt tinf tinv Touv TSCO TPOP SPO / NORMAL;
RUN; 

/* Test d'autocorrélation des résidus Godfrey */
proc model data= bz1;
TPIB=b0+b1*tdebt+b2*tpop+b3*tinv+b4*Touv+b5*tinf +b6*TSCO +b7*SPO;
fit Tpib / godfrey=2;
run;
/*TEST AUCORRELATION: en recuperant les residus*/
proc reg data=bz1;
model TPIB= tdebt tinf tinv Touv TSCO TPOP SPO ;
output out=resid1 residual=uhat;
title ' Estimation du Modele initial';
run;
/*Procédure de Tri*/
proc sort data=BZ1;
by id pays Annee;
run;
/*Prise des valeurs retardées du résidus*/
proc panel data=resid1;
id pays Annee;
lag uhat(1 2 3 4)/out=resid2;
run;

/*Regression du résidu sur les résidus retardés et les variables du modèle
initial*/
proc reg data=resid2;
model uhat=tdebt tinf tinv Touv TSCO TPOP SPO uhat_1 uhat_2/noint;
test uhat_1=0, uhat_2=0;
run;

					*Mise en place de la variable Taux PIB retardée*;
 proc panel data=bZ1;
 id pays Annee;
 clag TPIB(1)/out=BZ2;
 run;
 data BZ2;
 set BZ2;
 label Tpibr = 'Tpib_lag1';
 run;

 						*Test de Stationarité Levin Lin Chu(2002)*;
PROC PANEL DATA=BZ2;
ID PAYS Annee;
MODEL TPIB= TPIB_1 tdebt tinf tinv Touv TSCO TPOP SPO/stationarity=(llc=(lag=1));
run;

								*Test de stabilité de Chow*;
proc autoreg data=bz2;
model tpib=TPIB_1 tdebt tinf tinv Touv TSCO TPOP SPO/chow=(13);
RUN;
*****************************III-Analyse econometrique du modele retenu: estimation du modèle************************************ 

*Estimation du modèle par la méthode des moments généralisés en système GMM*;
proc panel plots = none data = bz2 ;
id pays annee ;
instruments depvar ;	
MODEL TPIB= tdebt tinf tinv Touv TSCO TPOP SPO /dynsys maxband=23 ;
run ;
proc panel plots = none data = bz2 ;
id pays annee ;
MODEL TPIB= tdebt tinf tinv Touv TSCO TPOP SPO /dynsys maxband=23 ;
run ;

*Estimation du modèle par les Doubles et Triples moindres carrés *;
						* 2SLS *;
proc syslin data = BZ2 2sls;
endogenous  Tpib_1;
instruments tinf tinv Touv TSCO TPOP SPO;
model TPIB= tpib_1 tdebt tinf tinv Touv TSCO TPOP SPO  ;
run;

	/* 3SLS */
proc syslin data = BZ2 3sls;
endogenous  Tpib_1;
instruments  tinf tinv Touv TSCO TPOP SPO;
model TPIB= tpib_1 tdebt tinf tinv Touv TSCO TPOP SPO  ;
run;
*Apreès l'estimation du modèle avec les deux derniers estimateurs pour verifier la convergence de nos résultats, 
    nous avons reçu un message d'erreur nous informant que cet estimateur est biaisé pour notre modèle*;
ods excel close;



