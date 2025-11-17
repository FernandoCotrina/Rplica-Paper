clear all
set more off, perm
capture log close
log using "02_resultados.log", replace text

display "`c(username)'"
	if "`c(username)'"== "josec" {
			global work1	"C:/Users/josec/Desktop/Blas_Cotrina"
	}

* Data original
	glob rawdata	"${work1}/rawdata"
* Data limpia
	glob cleandata	"${work1}/cleandata"
* Resultados
	glob output		"${work1}/output" 
	* Tablas:
		glob tables		"${output}/tables"
	* Imagénes:
		glob images		"${output}/images"


******* Parte 3 ************************************
****************************************************
*  Archivo Original: file1.do
****************************************************
* Este archivo genera los resultados del paper.
****************************************************

* Carpeta a usar
cd "${cleandata}"
u data_jp_aej, clear			/*Importamos Data Final*/

* Nivel de Wizorización
scalar def winsorize=1			/*	0=no wins, 1=0.5% wins, 2=1% wins, 3=2.5%	*/

* Otras definiciones
gen logncy=log(yl+yt+ym)				/**/
gen privempl=qualp10>=1 & qualp10<=4	/*Variable dummy de empleo privado*/
sort nquest year

* Variables en diferencias
foreach x in lprec lc age age2 educ ncomp married centre south lylav logncy unem public self retired privempl	 {
	qui by nq: gen D`x'=`x'-`x'[_n-1]
}

* Estimación de residuos para le modelo GMM (Du = Dy - Dx*beta)
reg Dlprec Dage Dage2 Deduc Dncomp Dmarried Dcentre Dsouth i.year 	/*Regresión en primeras diferencias para obtener residuales de wealth target*/
predict duw, res		/*Residuales de la ecuación de wealth*/
reg Dlogncy Dage Dage2 Deduc Dncomp Dmarried Dcentre Dsouth i.year 	/*Regresión en primeras diferencias para obtener residuales de ingreso*/
predict duy, res		/*Residual	es de la ecuación de ingreso*/

* Variable dummy para año 2010
gen d2010 = year==2010	/*Debido a un posible cambio estructural*/

* Tratamiento de valores extremos
if winsorize==1	{
	winsor duw, p(0.005) gen(duww)	/*0.5% de cada cola*/
	replace duw = duww
	winsor duy, p(0.005) gen(duyy)
	replace duy = duyy
}
else if winsorize==2	{
	winsor duw, p(0.01) gen(duww)	/*1% de cada cola*/
	replace duw = duww
	winsor duy, p(0.01) gen(duyy)
	replace duy = duyy
}
else if winsorize==3	{
	winsor duw, p(0.025) gen(duww)	/*2.5% de cada cola*/
	replace duw = duww
	winsor duy, p(0.025) gen(duyy)
	replace duy = duyy
}

* Construcción de momentos para estimación por GMM

* Cuadrados y productos cruzados de residuos
gen duw2 = duw^2		/*Varianza de residuos de Wealth*/
reg duw2 if year==2004

sort nq year
qby nq: g duyduylag = duy*duy[_n-1]		/*Productos cruzados rezagados y adelantados para test de sobreidentificación*/
qby nq: g duwduwlag = duw*duw[_n-1]	
qby nq: g duwduylag = duw*duy[_n-1]
qby nq: g duyduwlag = duy*duw[_n-1]
qby nq: g duwduyled = duw*duy[_n+1]

qby nq: g duwduylag2 = duw*duy[_n-2]	/*Adicionales*/
qby nq: g duwduyled2 = duw*duy[_n+2]
qby nq: g duwduyled3 = duw*duy[_n+3]
qby nq: g duwduyled4 = duw*duy[_n+4]

* Tests de Sobreindentificación
preserve 	/*Test 1: Simetría de covarianzas cruzadas*/
	rename duwduylag duwduylag1 
	rename duwduyled duwduyled1
	keep nq year  duwduylag1 duwduyled1 duwduylag2 duwduyled2
	drop if duwduylag1==. & duwduyled1==. & duwduylag2==. & duwduyled2==.
	gen d1 = duwduylag1 - duwduyled1
	gen d2 = duwduylag2 - duwduyled2
	gen id = 1
	replace id = sum(id)
	reshape long d, i(id) j(order)
	ttest d=0						/*Test 1, footnote 12*/
restore

preserve 	/*Test 2: Covarianzas cruzadas futuras iguales a cero*/
	rename duwduyled duwduyled1
	keep nq year  duwduyled*
	drop if duwduyled1==. & duwduyled2==. & duwduyled3==. & duwduyled4==.
	gen id = 1
	replace id = sum(id)
	reshape long duwduyled, i(id) j(order)
	ttest d=0						/*Test 2, footnote 12*/
restore

* Construcción de matrices de pesos del modelo GMM
gen duy2 = duy^2			/*Varianza de residuos del Ingreso*/
gen duyduw = duy*duw		/*Covarianza entre ingreso y wealth*/

	/*Matriz para modelo básico*/
matrix Omega=I(5)
qui su duy2
matrix Omega[1,1]=r(sd)^2
qui su duw2
matrix Omega[2,2]=r(sd)^2
qui su duwduwlag
matrix Omega[3,3]=r(sd)^2
qui su duyduylag
matrix Omega[4,4]=r(sd)^2
qui su duyduw
matrix Omega[5,5]=r(sd)^2
matrix Omega=syminv(Omega)		/*Weighting matrix DWMD(1)*/


	/*Matriz para modelo con shocks transitorios*/
matrix Omega7=I(7)
qui su duy2
matrix Omega7[1,1]=r(sd)^2
qui su duw2
matrix Omega7[2,2]=r(sd)^2
qui su duwduwlag
matrix Omega7[3,3]=r(sd)^2
qui su duyduylag
matrix Omega7[4,4]=r(sd)^2
qui su duyduw
matrix Omega7[5,5]=r(sd)^2
qui su duwduylag
matrix Omega7[6,6]=r(sd)^2
qui su duyduwlag
matrix Omega7[7,7]=r(sd)^2
matrix Omega7=syminv(Omega7)	/*Weighting matrix DWMD(2)*/

*********************************************************
************************ Tabla 2 ************************
*********************************************************

* Columna 1: Baseline
gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010), from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
	test /beta=1
	local p : display %4.3f `r(p)'
	
******************************
su d2010 if duy2!=.				/*Test de Identificación del modelo GMM basado en Newey (1985)*/
scalar def m10_c1=r(mean)
su d2010 if duw2!=.
scalar def m10_c2=r(mean)
su d2010 if duyduw!=.
scalar def m10_c3=r(mean)

matrix def b=e(b)
matrix def mom=J(5,1,0)
matrix def m=J(5,1,0)
matrix m[1,1]=2*b[1,1]+2*b[1,2]+4*b[1,1]*m10_c1
matrix m[2,1]=2*b[1,3]^2*b[1,1]+2*b[1,4]+4*b[1,3]^2*b[1,1]*m10_c2
matrix m[3,1]=-b[1,4]
matrix m[4,1]=-b[1,2]
matrix m[5,1]=2*b[1,3]*b[1,1]+4*b[1,3]*b[1,1]*m10_c3
qui su duy2
matrix mom[1,1]=r(mean)
qui su duw2
matrix mom[2,1]=r(mean)
qui su duwduwlag
matrix mom[3,1]=r(mean)
qui su duyduylag
matrix mom[4,1]=r(mean)
qui su duyduw
matrix mom[5,1]=r(mean)
matrix diff=m-mom
matrix list diff

matrix def G=J(5,5,0)
matrix G[1,1]=2+4*m10_c1
matrix G[1,2]=2
matrix G[2,1]=2*b[1,3]^2+4*b[1,3]^2*m10_c2
matrix G[2,3]=4*b[1,3]*b[1,1]+8*b[1,3]*b[1,1]*m10_c2
matrix G[2,4]=2
matrix G[3,4]=-1
matrix G[4,2]=-1
matrix G[5,1]=2*b[1,3]+4*b[1,3]*m10_c3
matrix G[5,3]=2*b[1,1]+4*b[1,1]*m10_c3

g x1= duy2
g x2= duw2
g x3= duwduwlag 
g x4= duyduylag
g x5= duyduw 
g x1x1=x1*x1
g x1x2=x1*x2
g x1x3=x1*x3
g x1x4=x1*x4
g x1x5=x1*x5
g x2x2=x2*x2
g x2x3=x2*x3
g x2x4=x2*x4
g x2x5=x2*x5
g x3x3=x3*x3
g x3x4=x3*x4
g x3x5=x3*x5
g x4x4=x4*x4
g x4x5=x4*x5
g x5x5=x5*x5
foreach x of varlist x1x1-x5x5	{
	qui reg `x'
	scalar def v`x'=_b[_cons]
}
forvalues j=1(1)5			{
	forvalues i=1(1)5 	{
	scalar def vx`i'x`j'=vx`j'x`i'
						}
							}
matrix V=J(5,5,0)
forvalues j=1(1)5	{
	matrix V[1,`j']=vx1x`j'
	matrix V[2,`j']=vx2x`j'
	matrix V[3,`j']=vx3x`j'
	matrix V[4,`j']=vx4x`j'
	matrix V[5,`j']=vx5x`j'
	}

matrix A=G*syminv(G'*Omega*G)*G'*Omega
matrix W=I(5)-A
matrix Z=W*V*W'
matginv Z, ginv(ZZ)
matrix trial=diff'*ZZ*diff
matrix list trial
di chi2tail(1,trial[1,1])		/*Test footnote 13*/
******************************

* Columna 1: Baseline
eststo clear
eststo:gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010), from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
	test /beta=1
	local p : display %4.3f `r(p)'
estadd scalar pvalue = r(p)

* Columna 2: Con shocks trarnsitorios
eststo:gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} + 2*{sigma_eta} + 2*{kappa}^2*{sigma_e}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta} + {kappa}^2*{sigma_e}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -2*{kappa}*{sigma_e} -4*{beta}*{sigma_zi}*d2010) ///
		(duwduylag + {kappa}*{sigma_e}) ///
		(duyduwlag + {kappa}*{sigma_e}) , from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75 kappa 0 ) ///
		winitial(Omega7) onestep nocommonesample  
	test /beta=1
	local p : display %4.3f `r(p)'
estadd scalar pvalue = r(p)

* Columna 3: Con shocks de preferencias
eststo:gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - 2*{sigma_psi}-(2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{sigma_psi}+4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag  + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010), from(sigma_psi 0.1 beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
	test /beta=1
	local p : display %4.3f `r(p)'
estadd scalar pvalue = r(p) 

esttab using "${tables}/tab2.doc",cells(b(fmt(%9.3f)) se(par))  stats(N pvalue, labels(Observations "P-val. beta=1")) ///
		se noeqlines nostar nonumbers mtitles("Baseline" "Add trans.sh." "Add pref.sh.") replace varlabels(_cons " ") compress varwidth(20) modelwidth(15)


*********************************************************
************************ Tabla 3 ************************
*********************************************************

* Columna 1: Age <= 45
eststo clear
eststo: gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010) if age<=45, from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
test /beta=1
local p : display %4.3f `r(p)'
estadd scalar pvalue = r(p)

* Columna 2: Age > 45
eststo: gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010) if age>45 & age!=., from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
test /beta=1
local p : display %4.3f `r(p)'
estadd scalar pvalue = r(p)

* Columna 3: Poor
eststo: gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010) if poor==1, from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
test /beta=1
local p : display %4.3f `r(p)'
estadd scalar pvalue = r(p)

* Columna 4: Rich
eststo: gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010) if poor==0, from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
test /beta=1
local p : display %4.3f `r(p)'
estadd scalar pvalue = r(p)

* Columna 5: Low Education
eststo: gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010) if educ<=13 & educ!=., from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
test /beta=1
local p : display %4.3f `r(p)'	
estadd scalar pvalue = r(p)

* Columna 6: High Education
eststo: gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010) if educ>=18 & educ!=., from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
test /beta=1
local p : display %4.3f `r(p)'
estadd scalar pvalue = r(p)

* Columna 7: Non-self-employed	
eststo: gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010) if self==0, from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
test /beta=1
local p : display %4.3f `r(p)'
estadd scalar pvalue = r(p)

* Columna 8: Self-employed	
eststo: gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010) if self==1, from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
test /beta=1
local p : display %4.3f `r(p)'
estadd scalar pvalue = r(p)

* Exportar Tabla 3
esttab using "${tables}/tab3.doc", cells(b(fmt(%9.3f)) se(par)) stats(N pvalue, labels(Observations "P-val. beta=1")) ///
		se noeqlines nostar nonumbers mtitles("Age<=45" "Age>45" "Poor" "Rich" "Low Educ." "High Educ." "Non self-empl." "Self-empl.") replace varlabels(_cons " ") compress varwidth(20) modelwidth(15)

*********************************************************
************************ Tabla 4 ************************
*********************************************************
* Columna 1: Year >= 2010
eststo clear
eststo: gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010) if year>=2012, from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
test /beta=1
local p : display %4.3f `r(p)'
estadd scalar pvalue = r(p)

* Columna 2: Year <= 2012
eststo: gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010) if year<=2012, from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
test /beta=1
local p : display %4.3f `r(p)'	
estadd scalar pvalue = r(p)

	
* Columna 3: No winsorization

******************************
* Nivel de Wizorización
scalar def winsorize=0			/*	0=no wins, 1=0.5% wins, 2=1% wins, 3=2.5%	*/

u data_jp_aej,clear

	/*Se repite el mismo proceso que antes sin winsorizar*/
gen logncy=log(yl+yt+ym)
gen privempl=qualp10>=1 & qualp10<=4
replace lprec = log(prec)

sort nquest year
foreach x in lprec lc age age2 educ ncomp married centre south lylav logncy unem public self retired privempl	 {
	qui by nq: gen D`x'=`x'-`x'[_n-1]
}

reg Dlprec Dage Dage2 Deduc Dncomp Dmarried Dcentre Dsouth i.year 
predict duw,res
reg Dlogncy Dage Dage2 Deduc Dncomp Dmarried Dcentre Dsouth i.year 
predict duy,res

gen d2010=year==2010

g duw2=duw^2
reg duw2 if year==2004

sort nq year
qby nq: g duyduylag=duy*duy[_n-1]
qby nq: g duwduwlag=duw*duw[_n-1]
qby nq: g duwduylag=duw*duy[_n-1]
qby nq: g duyduwlag=duy*duw[_n-1]
qby nq: g duwduyled=duw*duy[_n+1]

qby nq: g duwduylag2=duw*duy[_n-2]
qby nq: g duwduyled2=duw*duy[_n+2]
qby nq: g duwduyled3=duw*duy[_n+3]
qby nq: g duwduyled4=duw*duy[_n+4]

g duy2=duy^2
g duyduw=duy*duw

matrix Omega=I(5)
qui su duy2
matrix Omega[1,1]=r(sd)^2
qui su duw2
matrix Omega[2,2]=r(sd)^2
qui su duwduwlag
matrix Omega[3,3]=r(sd)^2
qui su duyduylag
matrix Omega[4,4]=r(sd)^2
qui su duyduw
matrix Omega[5,5]=r(sd)^2
matrix Omega=syminv(Omega)
******************************

	/*Estimamos el modelo sin winsorizar*/
eststo: gmm 	(duy2 - (2*{sigma_zi} + 2*{sigma_e}) - (4*{sigma_zi})*d2010) ///
		(duw2 - (2*{beta}^2*{sigma_zi} +2*{sigma_eta}) - (4*{beta}^2*{sigma_zi})*d2010 ) ///
		(duwduwlag + {sigma_eta}) ///
		(duyduylag + {sigma_e}) ///
		(duyduw - 2*{beta}*{sigma_zi} -4*{beta}*{sigma_zi}*d2010), from(beta 1 sigma_zi 0.05 sigma_e 0.05 sigma_eta 0.75) ///
		winitial(Omega) onestep nocommonesample
test /beta=1
local p : display %4.3f `r(p)'
estadd scalar pvalue = r(p)

* Exportar tabla
esttab using "${tables}/tab4.doc", cells(b(fmt(%9.3f)) se(par)) stats(N pvalue, labels(Observations "P-val. beta=1")) ///
		se noeqlines nostar nonumbers mtitles("Year>=2010" "Year<=2012" "No winsorization") replace varlabels(_cons " ") compress varwidth(20) modelwidth(15)

****************************************************
* Fin del archivo
****************************************************

******* Parte 4 ************************************
****************************************************
*  Archivo Original: file2.do
****************************************************

* Cargamos datos y ordenamos la estructura panel
use "${cleandata}/data_jp_aej.dta", clear
sort nquest year

* Creación de ratios e indicadores
gen py = prec / ylav
gen gap = cash - prec
gen gap_neg = gap < 0 & gap != .
gen cash_rev  = cash + ar3 - (rentpaid + yta)/12
gen lcash_rev = log(cash_rev)

sort nq year

* lags y diferencias panel
quietly by nq: gen lcash_rev1 = lcash_rev[_n-1]
quietly by nq: gen dlcash_rev = lcash_rev - lcash_rev1
quietly by nq: gen dlgap_rev1 = lcash_rev1 - lprec[_n-1]

* Etiquetas de variables
label variable gap      "Wealth gap: (w - w*)"
label variable gap_neg  "(w - w*) < 0"
label variable prec     "Target wealth (w*)"
label variable ylav     "Labor income"
label variable cash     "Cash-on-hand (w)"

* Diferencias de controles
sort nquest year
foreach x in lprec age age2 educ ncomp married centre south lylav {
    quietly by nq: gen D`x' = `x' - `x'[_n-1]
}

*********************************************************
************************ Tabla 1 ************************
*********************************************************
tab year nobs

* Tabla 1 - Estadísticas descriptivas
eststo clear

* Total sample
eststo: estpost sum prec c ylav cash gap gap_neg educ age male ncomp married, detail
estadd local Sample "Total sample"

* Panel sample  
eststo: estpost sum prec c ylav cash gap gap_neg educ age male ncomp married if panel==1, detail
estadd local Sample "Panel"

* Exportar Tabla 1
esttab using "${tables}/tab1.doc", ///
    cells("mean(fmt(2)) p50(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
    noobs nonumber label replace ///
    title("Table 1 - Descriptive Statistics") ///
    mtitle("Total sample" "Panel")

* Table OA1 - Online Appendix - descriptive statistics 
eststo clear

* <=2012 - Total sample
eststo: estpost sum prec c ylav cash gap gap_neg educ age male ncomp married if year<=2012, detail
estadd local Period "<=2012"
estadd local Sample "Total"

* <=2012 - Panel sample
eststo: estpost sum prec c ylav cash gap gap_neg educ age male ncomp married if panel==1 & year<=2012, detail
estadd local Period "<=2012" 
estadd local Sample "Panel"

* >=2014 - Total sample
eststo: estpost sum prec c ylav cash gap gap_neg educ age male ncomp married if year>=2014, detail
estadd local Period ">=2014"
estadd local Sample "Total"

* >=2014 - Panel sample
eststo: estpost sum prec c ylav cash gap gap_neg educ age male ncomp married if panel==1 & year>=2014, detail
estadd local Period ">=2014"
estadd local Sample "Panel"

* Exportar Tabla OA1
esttab using "${tables}/taboa1.doc", ///
    cells("mean(fmt(2)) p50(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
    noobs nonumber label replace ///
    title("Table OA1 - Descriptive Statistics by Period") ///
    mtitle("Total <=2012" "Panel <=2012" "Total >=2014" "Panel >=2014")


*********************************************************
sort nq year
qui by nq: gen dlcash  = lcash-lcash1
qui by nq: gen dlgap1  = lcash1-lprec[_n-1]
qui by nq: gen dlgap2  = dlgap1[_n-1]

gen dlgap1p = dlgap1*(dlgap1>0 & dlgap1!=.)
gen dlgap1n = dlgap1*(dlgap1<=0)
gen dlgap1p1 = dlgap1p[_n-1]
gen dlgap1n1 = dlgap1n[_n-1]


*********************************************************
************************ Tabla 5 ************************
*********************************************************

* Regresión principal con bootstrap
eststo clear
eststo baseline: bootstrap, cluster(nq) seed(1000) reps(100): ///
    reg dlcash dlgap1 Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd*

* Guardar muestra usada
gen baseline_sample = e(sample)

* Cálculo del tiempo de ajuste T (paper)
scalar T_adjust       = 2/abs(_b[dlgap1])
scalar T_adjust_upper = T_adjust + 1.96*(2/_b[dlgap1]^2)*_se[dlgap1]
scalar T_adjust_lower = T_adjust - 1.96*(2/_b[dlgap1]^2)*_se[dlgap1]
scalar list T_adjust T_adjust_upper T_adjust_lower

*********************************************************
* Splits de la muestra
*********************************************************

* --- Age <=45 ---
eststo age_le45: bootstrap, cluster(nq) seed(1000) reps(100): ///
    reg dlcash dlgap1 Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd* if age<=45

* --- Age >45 ---
eststo age_gt45: bootstrap, cluster(nq) seed(1000) reps(100): ///
    reg dlcash dlgap1 Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd* if age>45

* --- Low education ---
eststo low_educ: bootstrap, cluster(nq) seed(1000) reps(100): ///
    reg dlcash dlgap1 Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd* if educ<=13 & educ!=.

* --- High education ---
eststo high_educ: bootstrap, cluster(nq) seed(1000) reps(100): ///
    reg dlcash dlgap1 Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd* if educ>=18 & educ!=.

* --- Non-self employed ---
eststo non_self: bootstrap, cluster(nq) seed(1000) reps(100): ///
    reg dlcash dlgap1 Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd* if self==0 & self!=.

* --- Self employed ---
eststo self_empl: bootstrap, cluster(nq) seed(1000) reps(100): ///
    reg dlcash dlgap1 Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd* if self==1 & self!=.

* --- Poor ---
eststo poor: bootstrap, cluster(nq) seed(1000) reps(100): ///
    reg dlcash dlgap1 Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd* if poor==1

* --- Rich ---
eststo rich: bootstrap, cluster(nq) seed(1000) reps(100): ///
    reg dlcash dlgap1 Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd* if poor==0

* Exportar Tabla 5
esttab baseline age_le45 age_gt45 low_educ high_educ non_self self_empl poor rich ///
    using "${tables}/tab5.doc", ///
    se replace label ///
    mtitle("Baseline" "Age<=45" "Age>45" "Low Educ." "High Educ." "Non-self Empl." "Self-Empl." "Poor" "Rich") ///
    keep(dlgap1 Dage Dage2 Dncomp Dmarried) ///
    coeflabels(dlgap1 "Lagged ln(w)-ln(w*)" Dage "Age" Dage2 "Age sq./100" Dncomp "Family size" Dmarried "Married") ///
    stats(N, labels("Observations") fmt(%9.0f)) ///
    title("Table 5 - Adjustment to Target Wealth") ///
    note("All regressions include year dummies and bootstrap standard errors clustered at household level (100 replications).") ///
    varwidth(20)

*********************************************************
************************ Tabla 6 ************************
*********************************************************

* Limpiamos la memoria para iniciar nueva tabla
eststo clear

* Bloque 1: Estimación principal - 2SLS usando dlgap2 como instrumento de dlgap1
eststo iv_main: bootstrap, cluster(nq) seed(1000) reps(100): ivregress 2sls dlcash (dlgap1=dlgap2) Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd*
* Guardamos la muestra usada por la estimación IV
gen ivsample = e(sample)

* Bloque 2: Primera etapa del 2SLS - regresión de dlgap1 sobre el instrumento dlgap2
reg dlgap1 dlgap2 Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd* if ivsample==1	 
testparm dlgap2

* Bloque 3: Estimación usando cash-on-hand revisado (dlcash_rev)
eststo revised_cash: bootstrap, cluster(nq) seed(1000) reps(100): reg dlcash_rev dlgap_rev1 Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd* if baseline_sample==1

* Bloque 4: Estimación con asimetría (gap positivo y negativo)
eststo asymmetric: bootstrap, cluster(nq) seed(1000) reps(100): reg dlcash dlgap1p dlgap1n Dage Dage2 Dncomp Dmarried Deduc Dcentre Dsouth yrd*

* Exportar Tabla 6
esttab iv_main revised_cash asymmetric ///
    using "${tables}/tab6.doc", ///
    se replace label ///
    mtitle("IV with controls" "Revised cash-on-hand" "Gap below/above target") ///
    keep(dlgap1 dlgap_rev1 dlgap1p dlgap1n Dage Dage2 Dncomp Dmarried) ///
    coeflabels(dlgap1 "Lagged ln(w)-ln(w*)" ///
               dlgap_rev1 "Lagged ln(w)-ln(w*)" ///
               dlgap1p "Lagged ln(w)-ln(w*)>0" ///
               dlgap1n "Lagged ln(w)-ln(w*)<=0" ///
               Dage "Age" ///
               Dage2 "Age sq./100" ///
               Dncomp "Family size" ///
               Dmarried "Married") ///
    stats(N, labels("Observations") fmt(%9.0f)) ///
    title("Table 6 - Adjustment to Target Wealth: IV, Revised Cash-on-hand, and Asymmetric Behavior ") ///
    note("All regressions include year dummies and bootstrap standard errors clustered at household level (100 replications). Column 1 uses IV estimation with dlgap2 as instrument for dlgap1.") ///
    varwidth(25)

*********************************************************
************************ FIGURAS ************************
*********************************************************

* Figura 1: binscatter del ajuste
binscatter dlcash dlgap1 if dlgap1, nq(50) msymbol(oh) rd(0) ///
    xtitle(Lagged ln(W)-ln(W*)) ytitle(Change in ln(W)) ///
    saving("${images}/fig1", replace)
graph export "${images}/Figure1.pdf", as(pdf) replace

* Figura OA1: histograma de W* por año
lab var lprec "log(W*)"
hist lprec, by(year, note("") graphr(c(white))) bin(20) ///
    saving("${images}/figoa1", replace)
graph export "${images}/FigureOA1.pdf", as(pdf) replace

* Figura OA2: media y mediana del W*
replace prec = prec/1000
gcollapse (mean) a=prec al=lprec (p50) m=prec ml=lprec, by(year)

lab var a "Mean W*"
lab var m "Median W*"
lab var year "Year"

scatter a m year, c(l l) clp(solid dash) s(o oh) ///
    xlabel(2002(2)2016) ylabel(0(20)60) graphr(c(white)) ///
    saving("${images}/figoa2", replace)
graph export "${images}/FigureOA2.pdf", as(pdf) replace

* Limpiamos archivos temporales
erase "${images}/fig1.gph"
erase "${images}/figoa1.gph"
erase "${images}/figoa2.gph"

****************************************************
* Fin del archivo
****************************************************
log close