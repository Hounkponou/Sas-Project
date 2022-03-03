
*Workfile Projet econometrie Emmanuel&Modeste: Programme sous SAS*;

*Th�me: Effets de la dette Publique sur la croissance Economique.Cas des Pays de l'UEMOA;

*Membres du groupes:
- Comlan Alexis Modeste HOOUNGBADJI
-Anselme Emmanuel HOUNKPONOU ;

*Plan du programme sous SAS
*I- Primo etape data: importation  cr�ation de donn�e

II- Analyse descriptive

III-Analyse econometrique prealable: test de sp�cification...

III-Analyse econometrique du modele retenu: estimation du mod�le ;



***********************I- Primo etape data: importation  cr�ation  transformation des donn�es*******************************************************;

*Definition de la librairie*  ;
libname projet'C:\Users\Hounkponou\Desktop\PROJET SAS';

*Importation de la base de donn�e brute*;
proc import datafile='C:\Users\Hounkponou\Desktop\PROJET SAS\base.xlsx'
out=base dbms=xlsx replace ;
run;

*Deplacement de la base dans le dossier projet*;
data projet.base;
set base;
run;

/*trie des donn�es par individu (pays) et par p�riode (ann�es)*/
proc sort data=projet.base;
by id annee;
run;

/* calcul du taux d'ouverture*/
data base;
set projet.base;
touv = ((export+import)/(import+pib))*100;
run;

*********************************II- Analyse descriptive********************************************************;

*Test d'autocorr�lation des variable*;
proc corr data= base;
Var TPIB tdebt tinf tinv Touv TSCO SPO ;
RUN;

*statistiques descriptives du mod�le*;
proc means data=base;
Var TPIB tdebt tinf tinv Touv TSCO TPOP SPO ;
run;
data BZ1;
set base;
run;

************************III-Analyse econometrique prealable: test de sp�cification...*******************************;

			*Estimation MCO*;
 proc reg data=bz1 ; 
by id pays Annee ;
model TPIB=  tdebt tinf tinv Touv TSCO TPOP SPO ;
run;*l'hypoth�se nulle d'homog�n�it� a �t� rejet� suite � ce test* Ce qui nous pousse donc � d�terminer la nature de l'h�t�rog�neit� en faisant
un test de Hausman;

*Estimation POOLED=MCO;
 proc panel data=bz1 ; 
id pays Annee ;
model TPIB=tdebt tinf tinv Touv TSCO TPOP SPO /POOLED; 
run;

*Choix effets fixe effet al�atoire*;
		*Random Effects*;
 proc panel data=bz1 ; 
id pays Annee ;
model tpib = tdebt tinf tinv Touv TSCO TPOP SPO /RANONE; 
run;*Suite au test,la statistique de hausman est : m=14,5 la Khi-deux(6)=2,16 (alpha=0.05)  p-value=0.04 
							on rejete donc Ho pour pr�sence d'effet aleatoire*/; 

		*Fixed Effects*;
proc panel;
id pays Annee ;
model TPIB= tdebt tinf tinv Touv TSCO TPOP SPO  /FIXONE; 
run;*Rejet de Ho d'absence d'effet fixe � l'issue du test*;

		*Effet fixe proc�dure proc reg*;
proc reg data=bz1;
id pays Annee;
model TPIB=tdebt tinf tinv Touv TSCO TPOP SPO ;
run;*Estimation biais�*;

		*Test d'h�t�rosc�dasticit� Breusch-Pagan*;
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

/* Test d'autocorr�lation des r�sidus Godfrey */
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
/*Proc�dure de Tri*/
proc sort data=BZ1;
by id pays Annee;
run;
/*Prise des valeurs retard�es du r�sidus*/
proc panel data=resid1;
id pays Annee;
lag uhat(1 2 3 4)/out=resid2;
run;

/*Regression du r�sidu sur les r�sidus retard�s et les variables du mod�le
initial*/
proc reg data=resid2;
model uhat=tdebt tinf tinv Touv TSCO TPOP SPO uhat_1 uhat_2/noint;
test uhat_1=0, uhat_2=0;
run;

					*Mise en place de la variable Taux PIB retard�e*;
 proc panel data=bZ1;
 id pays Annee;
 clag TPIB(1)/out=BZ2;
 run;
 data BZ2;
 set BZ2;
 label Tpibr = 'Tpib_lag1';
 run;

 						*Test de Stationarit� Levin Lin Chu(2002)*;
PROC PANEL DATA=BZ2;
ID PAYS Annee;
MODEL TPIB= TPIB_1 tdebt tinf tinv Touv TSCO TPOP SPO/stationarity=(llc=(lag=1));
run;

								*Test de stabilit� de Chow*;
proc autoreg data=bz2;
model tpib=TPIB_1 tdebt tinf tinv Touv TSCO TPOP SPO/chow=(13);
RUN;
*****************************III-Analyse econometrique du modele retenu: estimation du mod�le************************************ 

*Estimation du mod�le par la m�thode des moments g�n�ralis�s en syst�me GMM*;
proc panel plots = none data = bz2 ;
id pays annee ;
instruments depvar ;	
MODEL TPIB= tdebt tinf tinv Touv TSCO TPOP SPO /dynsys maxband=23 ;
run ;
proc panel plots = none data = bz2 ;
id pays annee ;
MODEL TPIB= tdebt tinf tinv Touv TSCO TPOP SPO /dynsys maxband=23 ;
run ;

*Estimation du mod�le par les Doubles et Triples moindres carr�s *;
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
*Apre�s l'estimation du mod�le avec les deux derniers estimateurs pour verifier la convergence de nos r�sultats, 
    nous avons re�u un message d'erreur nous informant que cet estimateur est biais� pour notre mod�le*;
ods excel close;



