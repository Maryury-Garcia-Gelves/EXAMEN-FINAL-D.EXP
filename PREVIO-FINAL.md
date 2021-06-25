ANÁLISIS FACTORIAL
================
Maryury Johana Garcia Gelves 1950186

# **EXAMEN FINAL DE DISEÑO DE EXPERIMENTOS**

La siguiente base de datos fue una encuesta realizada a **30 jovenes de
la Universidad Francisco de Paula Santarder** en la cual se les pregunta
la edad y a que se dedican durante las 24 horas del día.

# **Importar base de datos en formato Excel**

``` r
library(readxl)
datos<- read_excel("C:/Users/USUARIO/Documents/B.D.xlsx")
```

# **Tipificación o estandarización de variables**

La tipificación permite que todas las variables métricas gocen de una
misma unidad de medida estadistica.

``` r
datosm<- datos # nueva base de datos o data frame
datosm<-scale(datosm,center = T, scale = T)
datosm<- as.data.frame(datosm)
```

# **Normalidad Multivariante**

H0: Normalida Multivariante

H1: No Normalidad Multivariante

confianza= 95%

Alfa= 5% = 0,05%

P value &gt; alfa: no se rechaza la H0 (Normalidad)

P value &lt; alfa: se rechaza la H0 (No normalidad)

``` r
library(MVN)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## sROC 0.1-2 loaded

``` r
mvn(datosm[2:7])
```

    ## $multivariateNormality
    ##              Test         Statistic             p value Result
    ## 1 Mardia Skewness  88.3227734012552 0.00379931165630201     NO
    ## 2 Mardia Kurtosis 0.717064623019103   0.473334223170721    YES
    ## 3             MVN              <NA>                <NA>     NO
    ## 
    ## $univariateNormality
    ##           Test        Variable Statistic   p value Normality
    ## 1 Shapiro-Wilk      edad          0.9208  0.0281      NO    
    ## 2 Shapiro-Wilk hrs en  familia    0.8113   1e-04      NO    
    ## 3 Shapiro-Wilk   hrs-deporte      0.7666  <0.001      NO    
    ## 4 Shapiro-Wilk    hrs_redes       0.8725  0.0019      NO    
    ## 5 Shapiro-Wilk   hrs_trabajo      0.6318  <0.001      NO    
    ## 6 Shapiro-Wilk hrs_de estudio     0.8869  0.0041      NO    
    ## 
    ## $Descriptives
    ##                  n          Mean Std.Dev      Median        Min      Max
    ## edad            30 -5.777750e-16       1 -0.46157777 -1.3008101 2.475735
    ## hrs en  familia 30  1.815473e-16       1  0.04357922 -1.2637975 1.350956
    ## hrs-deporte     30 -3.329946e-17       1 -0.25720529 -0.8083595 2.498566
    ## hrs_redes       30 -2.035752e-16       1 -0.41666490 -1.3781993 1.506404
    ## hrs_trabajo     30  4.440169e-17       1 -0.85977653 -0.8597765 1.124323
    ## hrs_de estudio  30  4.163336e-17       1 -0.02101433 -1.9123038 1.239845
    ##                       25th      75th        Skew   Kurtosis
    ## edad            -0.8811939 0.6923667  0.63925349 -0.6031394
    ## hrs en  familia -1.2637975 1.0241117  0.05197543 -1.3464531
    ## hrs-deporte     -0.8083595 0.2939489  1.05643704  0.1942479
    ## hrs_redes       -0.4166649 0.5448695  0.17134363 -1.2169119
    ## hrs_trabajo     -0.8597765 1.1243232  0.25572840 -1.9979035
    ## hrs_de estudio  -0.6514441 1.2398453 -0.05259886 -1.3500914

En el test Mardia Skewness P value &lt; alfa,se rechaza la H0,es decir
no existe normalidad multivariante, en cambio en el test Mardia Kurtosis
P value &gt; alfa, no se rechaza H0, es decir existe normalidad
multivariable. por tanto, MVN nos da un resultado de NO.

# **Matriz de correlaciones**

H0: Correlación = 0 (no hay correlación)

H1: Correlación diferente de 0 (si hay correlación)

Cuando no se rechaza H0, no se aplica AFE.

se rechaza H0, sí se aplica AFE.

``` r
library(psych)
corr.test(datosm[,2:7])
```

    ## Call:corr.test(x = datosm[, 2:7])
    ## Correlation matrix 
    ##                  edad hrs en  familia hrs-deporte hrs_redes hrs_trabajo
    ## edad             1.00            0.19       -0.26     -0.06        0.11
    ## hrs en  familia  0.19            1.00        0.24      0.24        0.40
    ## hrs-deporte     -0.26            0.24        1.00     -0.13        0.11
    ## hrs_redes       -0.06            0.24       -0.13      1.00        0.29
    ## hrs_trabajo      0.11            0.40        0.11      0.29        1.00
    ## hrs_de estudio   0.18           -0.20       -0.14     -0.49       -0.41
    ##                 hrs_de estudio
    ## edad                      0.18
    ## hrs en  familia          -0.20
    ## hrs-deporte              -0.14
    ## hrs_redes                -0.49
    ## hrs_trabajo              -0.41
    ## hrs_de estudio            1.00
    ## Sample Size 
    ## [1] 30
    ## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
    ##                 edad hrs en  familia hrs-deporte hrs_redes hrs_trabajo
    ## edad            0.00            1.00        1.00      1.00        1.00
    ## hrs en  familia 0.31            0.00        1.00      1.00        0.39
    ## hrs-deporte     0.17            0.21        0.00      1.00        1.00
    ## hrs_redes       0.75            0.21        0.50      0.00        1.00
    ## hrs_trabajo     0.58            0.03        0.56      0.12        0.00
    ## hrs_de estudio  0.34            0.29        0.47      0.01        0.03
    ##                 hrs_de estudio
    ## edad                      1.00
    ## hrs en  familia           1.00
    ## hrs-deporte               1.00
    ## hrs_redes                 0.09
    ## hrs_trabajo               0.36
    ## hrs_de estudio            0.00
    ## 
    ##  To see confidence intervals of the correlations, print with the short=FALSE option

``` r
correlaciones<- corr.test(datosm[,2:7]) #se crea la matriz de correlaciones 
correlaciones$r # matriz de correlaciones
```

    ##                        edad hrs en  familia hrs-deporte   hrs_redes hrs_trabajo
    ## edad             1.00000000       0.1910629  -0.2583880 -0.05982566   0.1062232
    ## hrs en  familia  0.19106288       1.0000000   0.2352193  0.23552331   0.3965488
    ## hrs-deporte     -0.25838799       0.2352193   1.0000000 -0.12913814   0.1106114
    ## hrs_redes       -0.05982566       0.2355233  -0.1291381  1.00000000   0.2872634
    ## hrs_trabajo      0.10622321       0.3965488   0.1106114  0.28726343   1.0000000
    ## hrs_de estudio   0.18152817      -0.1979997  -0.1373881 -0.48982126  -0.4068810
    ##                 hrs_de estudio
    ## edad                 0.1815282
    ## hrs en  familia     -0.1979997
    ## hrs-deporte         -0.1373881
    ## hrs_redes           -0.4898213
    ## hrs_trabajo         -0.4068810
    ## hrs_de estudio       1.0000000

``` r
j <- as.matrix(correlaciones$r)
```

# **Indicadores de aplicabilidad**

## **Contraste de esfericidad de Bartlett**

H0: Las correlaciones teóricas entre cada par de variables es nulo.

H1: Las correlaciones teóricas entre cada par de variables no es nulo.

P value &gt; alfa: no se aplica el AFE (no se rechaza H0)

P value &lt; alfa: sí se aplica AFE (se rechaza H0)

``` r
dim(datosm) # tamaño de la muestra= 30 personas
```

    ## [1] 30  7

``` r
cortest.bartlett(j,n=30)
```

    ## $chisq
    ## [1] 26.63197
    ## 
    ## $p.value
    ## [1] 0.03188197
    ## 
    ## $df
    ## [1] 15

P value &lt; alfa, se rechaza H0, por tanto las correlaciones teóricas
entre cada par de variables es nulo,es decir, el análisis factorial
exploratorio (AFE) es aplicable.

# **Medida de adecuación muestral de Kaiser, Meyer y Oklin (KMO)**

Estudia variable por variable, si son o no aceptadas en el modelo para
hacer AFE.(Qué variables elimino o mantengo)

Se mantiene una variable,si el KMO es igual o mayor a 0,7.

Se elimina una variable, si el KMO &lt; a 0,7.

``` r
KMO(j)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = j)
    ## Overall MSA =  0.54
    ## MSA for each item = 
    ##            edad hrs en  familia     hrs-deporte       hrs_redes     hrs_trabajo 
    ##            0.40            0.54            0.36            0.55            0.68 
    ##  hrs_de estudio 
    ##            0.60

KMO= 0,54, el modelo es Miserable

Edad=0,40 (inaceptable)

hrs\_familia=0,54 (miserable)

hrs\_deporte=0,36 (inaceptable)

hrs\_redes=0,55 (miserable)

hrs\_trabajo= 0,68 (mediocre)

hrs\_estudio=0,60 (mediocre)

Todas las variables estan sujetas a ser eliminadas porque los KMO&lt;
0,7

KMO global = 0,54 no se puede hacer AFE.

# **Determinación del número de factores a extraer**

## **Método de las componentes principales iteradas (Ejes principales )**

Este método de las Ejes principales se ocupa cuando no hay normalidad
multivariante; pero, tambien es válido para modelos normalidad
multivariante.

``` r
fa.parallel(j, fm= "pa", n.obs = 30, ylabel = "Eigenvalues")
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

![](PREVIO-FINAL_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  1

Con el método de los ejes principales se extraería solo 1 factor.

## **Método de las componentes principales**

Sirve solo para modelos con normalidad multivariable.

``` r
fa.parallel(j, fm= "pc", n.obs = 30, ylabel = "Eigenvalues")
```

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

![](PREVIO-FINAL_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

Con el método de las componentes principales se recomienda extraer 1
factor.

## **Método de la máxima verosimilitud**

Sirve solo para modelos con normalidad multivariante.

``` r
fa.parallel(j, fm= "ml", n.obs = 30, ylabel = "Eigenvalues")
```

![](PREVIO-FINAL_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

Con el método de la máxima verosimilitud se recomienda extraer 1 factor.

## **Método paralelo con iteraciones**

Sirve solo para modelos con normalida multivariante.

``` r
library(paran)
```

    ## Loading required package: MASS

``` r
paran(j, iterations= 1000, graph = T)
```

    ## 
    ## Using eigendecomposition of correlation matrix.
    ## Computing: 10%  20%  30%  40%  50%  60%  70%  80%  90%  100%
    ## 
    ## 
    ## Results of Horn's Parallel Analysis for component retention
    ## 1000 iterations, using the mean estimate
    ## 
    ## -------------------------------------------------- 
    ## Component   Adjusted    Unadjusted    Estimated 
    ##             Eigenvalue  Eigenvalue    Bias 
    ## -------------------------------------------------- 
    ## 1           1.261939    3.029250      1.767310
    ## -------------------------------------------------- 
    ## 
    ## Adjusted eigenvalues > 1 indicate dimensions to retain.
    ## (1 components retained)

![](PREVIO-FINAL_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Con el método de Horn’s (método paralelo con iteraciones)se recomienda
extraer 1 factor.

Resumen:

Ejes principales= 1 factor

Componentes principales= 1 factor

Máxima verosimilitud= 1 factor

Método paralelo con iteraciones (Horn’s)= 1 factor

En conclusió se extraera 1 factor.

# **Método de extracción de factores**

## **Método de análisis de los componentes principales (ACP)**

``` r
library(psych)
acp<-principal(j, nfactores=1, rotate= "none")
acp
```

    ## Principal Components Analysis
    ## Call: principal(r = j, rotate = "none", nfactores = 1)
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                   PC1     h2   u2 com
    ## edad            -0.04 0.0018 1.00   1
    ## hrs en  familia  0.63 0.3922 0.61   1
    ## hrs-deporte      0.25 0.0610 0.94   1
    ## hrs_redes        0.68 0.4557 0.54   1
    ## hrs_trabajo      0.74 0.5510 0.45   1
    ## hrs_de estudio  -0.76 0.5834 0.42   1
    ## 
    ##                 PC1
    ## SS loadings    2.05
    ## Proportion Var 0.34
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 component is sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.17 
    ## 
    ## Fit based upon off diagonal values = 0.55

PC1: cargas factoriales de cada variable.

h2: comunalidad (varianza común explicada).Mientras más alta sea h2 es
mejor el modelo.Límite entre 0 y 1, es mejor mientras mas cercano este a
1.

u2: Especificidad (varianza residual o varianza no explicada). Mientras
más pequeño sea u2 es mejor el modelo. Límite entre 0 y 1.

h2 + u2= 1

Comunalidad + Especificidad =1

Varianza explicada + varianza no explicada = 1

SS loadings= 2.05 (Es la varianza explicada en valores absolutos,o la
sumas de los h2).

Proportion Var= 0.34 = 34% (El % que la varianza explicada representa
del total).

Lo ideal es que proportion var sea lo más cercano a 1.

RMSR= 0.17 (Raíz cuadrada media de los residuos ).

## **Método de los ejes principales o componentes principales iteradas (CPI)**

``` r
cpi<-fa(j, nfactores=1, fm="pa", rotate = "none", n.obs = 30)
cpi
```

    ## Factor Analysis using method =  pa
    ## Call: fa(r = j, n.obs = 30, rotate = "none", fm = "pa", nfactores = 1)
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                   PA1      h2   u2 com
    ## edad            -0.03 0.00098 1.00   1
    ## hrs en  familia  0.45 0.20672 0.79   1
    ## hrs-deporte      0.15 0.02259 0.98   1
    ## hrs_redes        0.55 0.30605 0.69   1
    ## hrs_trabajo      0.63 0.39095 0.61   1
    ## hrs_de estudio  -0.69 0.47723 0.52   1
    ## 
    ##                 PA1
    ## SS loadings    1.40
    ## Proportion Var 0.23
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  1.02 with Chi Square of  26.63
    ## The degrees of freedom for the model are 9  and the objective function was  0.44 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.13 
    ## The df corrected root mean square of the residuals is  0.17 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  15.96  with prob <  0.068 
    ## The total number of observations was  30  with Likelihood Chi Square =  11.21  with prob <  0.26 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.664
    ## RMSEA index =  0.084  and the 90 % confidence intervals are  0 0.24
    ## BIC =  -19.4
    ## Fit based upon off diagonal values = 0.73
    ## Measures of factor score adequacy             
    ##                                                    PA1
    ## Correlation of (regression) scores with factors   0.83
    ## Multiple R square of scores with factors          0.70
    ## Minimum correlation of possible factor scores     0.39

Proportion Var 0.23= 23%

(RMSR)= 0.13

## **Método de máxima verosimilitud (MVE)**

``` r
mve<-fa(j, nfactores=1, fm="ml",rotate="none", n.obs = 30)
mve
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = j, n.obs = 30, rotate = "none", fm = "ml", nfactores = 1)
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                   ML1     h2   u2 com
    ## edad            -0.09 0.0083 0.99   1
    ## hrs en  familia  0.39 0.1491 0.85   1
    ## hrs-deporte      0.13 0.0181 0.98   1
    ## hrs_redes        0.60 0.3609 0.64   1
    ## hrs_trabajo      0.56 0.3163 0.68   1
    ## hrs_de estudio  -0.75 0.5607 0.44   1
    ## 
    ##                 ML1
    ## SS loadings    1.41
    ## Proportion Var 0.24
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  1.02 with Chi Square of  26.63
    ## The degrees of freedom for the model are 9  and the objective function was  0.43 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.14 
    ## The df corrected root mean square of the residuals is  0.17 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  16.47  with prob <  0.058 
    ## The total number of observations was  30  with Likelihood Chi Square =  11.06  with prob <  0.27 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.686
    ## RMSEA index =  0.081  and the 90 % confidence intervals are  0 0.238
    ## BIC =  -19.55
    ## Fit based upon off diagonal values = 0.72
    ## Measures of factor score adequacy             
    ##                                                    ML1
    ## Correlation of (regression) scores with factors   0.85
    ## Multiple R square of scores with factors          0.71
    ## Minimum correlation of possible factor scores     0.43

Proportion Var = 0.24 = 24%

(RMSR) = 0.14

### *Resumen*

ACP: Var= 34% RMSR= 0.17

CPI: var= 23% RMSR= 0.13

MVE: var= 24% RMSR= 0.14

Nos quedamos con aquel modelo que tenga la proportion var más alta y
RMSR más pequeño.

# **Obtención de las puntuaciones factoriales**

## *Método de análisis de las componentes principales iteradas (ACP)*

``` r
acp1<- principal(datosm[,2:7], nfactores=1, rotate="none", scores = T)
acp1$scores
```

    ##               PC1
    ##  [1,] -0.17965322
    ##  [2,]  1.36924969
    ##  [3,] -0.98734618
    ##  [4,]  0.95826992
    ##  [5,]  0.56357822
    ##  [6,] -0.08589320
    ##  [7,] -0.28557364
    ##  [8,]  0.46120897
    ##  [9,]  0.81231895
    ## [10,] -0.81747725
    ## [11,] -1.38770026
    ## [12,] -1.69635815
    ## [13,] -0.85421960
    ## [14,] -0.26194481
    ## [15,] -0.50497580
    ## [16,] -1.00475728
    ## [17,] -0.63426208
    ## [18,]  0.08345544
    ## [19,] -1.72247480
    ## [20,]  1.23008413
    ## [21,]  1.60736473
    ## [22,] -0.68327501
    ## [23,]  1.13380123
    ## [24,]  1.26448736
    ## [25,] -0.69876596
    ## [26,] -0.90809777
    ## [27,]  0.47754441
    ## [28,]  0.14337260
    ## [29,]  1.85151874
    ## [30,]  0.75652063

``` r
puntuacionesfactoriales_acp<-acp1$scores
puntuacionesfactoriales_acp<-as.data.frame(puntuacionesfactoriales_acp)
```

## *Método de las componentes principales iteradas (CPI)*

``` r
cpi1<-fa(datosm[,2:7],nfactores=1, fm="pa", rotate="none", n.obs = 30, scores = "regression")
cpi1$scores
```

    ##               PA1
    ##  [1,] -0.01216284
    ##  [2,]  1.28275435
    ##  [3,] -0.89974535
    ##  [4,]  0.85841478
    ##  [5,]  0.55007011
    ##  [6,] -0.19897343
    ##  [7,] -0.37071219
    ##  [8,]  0.52572891
    ##  [9,]  0.67244927
    ## [10,] -0.66775786
    ## [11,] -1.14218035
    ## [12,] -1.35674343
    ## [13,] -0.85126009
    ## [14,] -0.35322611
    ## [15,] -0.72695021
    ## [16,] -0.90081046
    ## [17,] -0.41269144
    ## [18,]  0.04483920
    ## [19,] -1.35834109
    ## [20,]  0.91594938
    ## [21,]  1.22322894
    ## [22,] -0.54876712
    ## [23,]  1.02449260
    ## [24,]  0.90343099
    ## [25,] -0.36846021
    ## [26,] -0.62512430
    ## [27,]  0.45628852
    ## [28,]  0.13702313
    ## [29,]  1.48202324
    ## [30,]  0.71721305

``` r
puntfact_cpi<-cpi1$scores
puntfact_cpi<-as.data.frame(puntfact_cpi)
```

## *Método de la máxima verosimilitud (MVE)*

``` r
mve1<-fa(datosm[,2:7],nfactores=1, fm="ml", rotate="none", n.obs = 30, scores = "regression")
mve1$scores
```

    ##              ML1
    ##  [1,]  0.2127349
    ##  [2,]  1.3798671
    ##  [3,] -0.9301580
    ##  [4,]  0.8695312
    ##  [5,]  0.4870742
    ##  [6,] -0.5539572
    ##  [7,] -0.5266273
    ##  [8,]  0.2896580
    ##  [9,]  0.6312262
    ## [10,] -0.5131309
    ## [11,] -1.0994057
    ## [12,] -1.3462578
    ## [13,] -0.8871237
    ## [14,] -0.4661992
    ## [15,] -0.8896279
    ## [16,] -0.9520944
    ## [17,] -0.2724222
    ## [18,]  0.1546458
    ## [19,] -1.3791625
    ## [20,]  0.8224103
    ## [21,]  1.1829308
    ## [22,] -0.5099715
    ## [23,]  1.0733491
    ## [24,]  1.0778392
    ## [25,] -0.2017881
    ## [26,] -0.4754014
    ## [27,]  0.3514694
    ## [28,]  0.2573460
    ## [29,]  1.5004171
    ## [30,]  0.7128286

``` r
puntfact_mve<-mve1$scores
puntfact_mve<-as.data.frame(puntfact_mve)
```

# **Obtención de los factores extraídos**

Se trabaja ya sea con el método ACP,CPI o MVE.

``` r
factor.scores(j, acp, method = "Thurstone")
```

    ## $scores
    ## NULL
    ## 
    ## $weights
    ##                         PC1
    ## edad            -0.02074646
    ## hrs en  familia  0.30622703
    ## hrs-deporte      0.12077073
    ## hrs_redes        0.33005938
    ## hrs_trabajo      0.36296461
    ## hrs_de estudio  -0.37347292
    ## 
    ## $r.scores
    ##     PC1
    ## PC1   1
    ## 
    ## $R2
    ## [1] 1

Z1= -0.021Edad + 0.31Hrs\_familia + 0.12Hrs\_deporte + 0.33Hrs\_redes +
0.36Hrs\_trabajo - 0.37Hrs\_estudio.

# **Agregar el factor extraído (puntuaciones factoriales) en el data frame original**

``` r
datos_puntuaciones<-c(datos, puntuacionesfactoriales_acp)
datos_puntuaciones<- as.data.frame(datos_puntuaciones)
```
