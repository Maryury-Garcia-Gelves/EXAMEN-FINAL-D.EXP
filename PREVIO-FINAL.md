ANÁLISIS FACTORIAL
================
Maryury Johana Garcia Gelves 1950186

# **EXAMEN FINAL DE DISEÑO DE EXPERIMENTOS**

La siguiente base de datos fue una encuesta realizada a **30 jovenes de
la Universidad Francisco de Paula Santarder** en la cual se les pregunta
la edad y a que se dedican durante las 24 horas del día.

\#**Importar base de datos en formato Excel**

``` r
library(readxl)
datos<- read_excel("C:/Users/USUARIO/Documents/B.D.xlsx")
```

\#**Tipificación o estandarización de variables**

La tipificación permite que todas las variables métricas gocen de una
misma unidad de medida estadistica.

``` r
datosm<- datos # nueva base de datos o data frame
datosm<-scale(datosm,center = T, scale = T)
datosm<- as.data.frame(datosm)
```

\#**Normalidad Multivariante**

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

\#**Matriz de correlaciones**

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

\#**Indicadores de aplicabilidad**

\#\#**Contraste de esfericidad de Bartlett**

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

\#**Medida de adecuación muestral de Kaiser, Meyer y Oklin (KMO)**

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

\#**Determinación del número de factores a extraer**

\#\#**Método de las componentes principales iteradas (Ejes principales
)**

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

\#\#**Método de las componentes principales**

Sirve solo para modelos con normalidad multivariable.

``` r
fa.parallel(j, fm= "pc", n.obs = 30, ylabel = "Eigenvalues")
```

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

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

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

![](PREVIO-FINAL_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  1

Con el método de las componentes principales se recomienda extraer 1
factor.

\#\#**Método de la máxima verosimilitud**

Sirve solo para modelos con normalidad multivariante.

``` r
fa.parallel(j, fm= "ml", n.obs = 30, ylabel = "Eigenvalues")
```

![](PREVIO-FINAL_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  1

Con el método de la máxima verosimilitud se recomienda extraer 1 factor.

\#\#**Método paralelo con iteraciones**

Sirve solo para modelos con normalida multivariante.
