# print.summary.cer_design produces stable output across lifecycle

    A CER Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
     H1  H2  H3  H4 
    0.5 0.5 0.0 0.0 
    
    Graph Transition Matrix
        H1  H2  H3  H4
    H1 0.0 0.5 0.5 0.0
    H2 0.5 0.0 0.0 0.5
    H3 0.0 1.0 0.0 0.0
    H4 1.0 0.0 0.0 0.0
    
    Correlation for parametric test
        H1  H2  H3  H4
    H1 1.0 0.5  NA  NA
    H2 0.5 1.0  NA  NA
    H3  NA  NA 1.0 0.5
    H4  NA  NA 0.5 1.0
    
    Interim test is planned at time fraction 0.5
    

---

    A CER Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
     H1  H2  H3  H4 
    0.5 0.5 0.0 0.0 
    
    Graph Transition Matrix
        H1  H2  H3  H4
    H1 0.0 0.5 0.5 0.0
    H2 0.5 0.0 0.0 0.5
    H3 0.0 1.0 0.0 0.0
    H4 1.0 0.0 0.0 0.0
    
    Correlation for parametric test
        H1  H2  H3  H4
    H1 1.0 0.5  NA  NA
    H2 0.5 1.0  NA  NA
    H3  NA  NA 1.0 0.5
    H4  NA  NA 0.5 1.0
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: H1
    

---

    A CER Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
     H1  H2  H3  H4 
    0.5 0.5 0.0 0.0 
    
    Graph Transition Matrix
        H1  H2  H3  H4
    H1 0.0 0.5 0.5 0.0
    H2 0.5 0.0 0.0 0.5
    H3 0.0 1.0 0.0 0.0
    H4 1.0 0.0 0.0 0.0
    
    Correlation for parametric test
        H1  H2  H3  H4
    H1 1.0 0.5  NA  NA
    H2 0.5 1.0  NA  NA
    H3  NA  NA 1.0 0.5
    H4  NA  NA 0.5 1.0
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: H1
    
    -- Adaptions from inital specification -----------------------------------------
    
    New hypotheses weights
           H1        H2        H3        H4 
    0.3333333 0.6666667 0.0000000 0.0000000 
    

---

    A CER Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
     H1  H2  H3  H4 
    0.5 0.5 0.0 0.0 
    
    Graph Transition Matrix
        H1  H2  H3  H4
    H1 0.0 0.5 0.5 0.0
    H2 0.5 0.0 0.0 0.5
    H3 0.0 1.0 0.0 0.0
    H4 1.0 0.0 0.0 0.0
    
    Correlation for parametric test
        H1  H2  H3  H4
    H1 1.0 0.5  NA  NA
    H2 0.5 1.0  NA  NA
    H3  NA  NA 1.0 0.5
    H4  NA  NA 0.5 1.0
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: H1
    
    -- Adaptions from inital specification -----------------------------------------
    
    New hypotheses weights
           H1        H2        H3        H4 
    0.3333333 0.6666667 0.0000000 0.0000000 
    
    -- Final test result -----------------------------------------------------------
    
    Overall p-values of the hypotheses are:
    [1] 0.200 0.011 0.500 0.030
    Hypotheses rejected: H1 and H2

# print.summary.multiarm_cer_design produces stable output across lifecycle

    A Multi-arm Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
     H1  H2  H3  H4 
    0.5 0.5 0.0 0.0 
    
    Graph Transition Matrix
        H1  H2  H3  H4
    H1 0.0 0.5 0.5 0.0
    H2 0.5 0.0 0.0 0.5
    H3 0.0 1.0 0.0 0.0
    H4 1.0 0.0 0.0 0.0
    
    Correlation for parametric test
        H1  H2  H3  H4
    H1 1.0 0.5  NA  NA
    H2 0.5 1.0  NA  NA
    H3  NA  NA 1.0 0.5
    H4  NA  NA 0.5 1.0
    
    Number of control groups:
    [1] 2
    
    Treatment-to-control assignments (per treatment arm):
    [1] 1 1 2 2
    
    Planned sample sizes per control group:
    [1] 70 70
    
    Planned sample sizes per treatment group:
    [1] 70 70 70 70
    
    Interim test is planned at time fraction 0.5
    

---

    A Multi-arm Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
     H1  H2  H3  H4 
    0.5 0.5 0.0 0.0 
    
    Graph Transition Matrix
        H1  H2  H3  H4
    H1 0.0 0.5 0.5 0.0
    H2 0.5 0.0 0.0 0.5
    H3 0.0 1.0 0.0 0.0
    H4 1.0 0.0 0.0 0.0
    
    Correlation for parametric test
        H1  H2  H3  H4
    H1 1.0 0.5  NA  NA
    H2 0.5 1.0  NA  NA
    H3  NA  NA 1.0 0.5
    H4  NA  NA 0.5 1.0
    
    Number of control groups:
    [1] 2
    
    Treatment-to-control assignments (per treatment arm):
    [1] 1 1 2 2
    
    Planned sample sizes per control group:
    [1] 70 70
    
    Planned sample sizes per treatment group:
    [1] 70 70 70 70
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: H1
    

---

    A Multi-arm Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
     H1  H2  H3  H4 
    0.5 0.5 0.0 0.0 
    
    Graph Transition Matrix
        H1  H2  H3  H4
    H1 0.0 0.5 0.5 0.0
    H2 0.5 0.0 0.0 0.5
    H3 0.0 1.0 0.0 0.0
    H4 1.0 0.0 0.0 0.0
    
    Correlation for parametric test
        H1  H2  H3  H4
    H1 1.0 0.5  NA  NA
    H2 0.5 1.0  NA  NA
    H3  NA  NA 1.0 0.5
    H4  NA  NA 0.5 1.0
    
    Number of control groups:
    [1] 2
    
    Treatment-to-control assignments (per treatment arm):
    [1] 1 1 2 2
    
    Planned sample sizes per control group:
    [1] 70 70
    
    Planned sample sizes per treatment group:
    [1] 70 70 70 70
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: H1
    
    -- Adaptions from inital specification -----------------------------------------
    
    New hypotheses weights
      H1   H2   H3   H4 
    0.00 0.75 0.25 0.00 
    
    New graph Transition Matrix
       H1  H2        H3        H4
    H1  0 0.0 0.0000000 0.0000000
    H2  0 0.0 0.3333333 0.6666667
    H3  0 1.0 0.0000000 0.0000000
    H4  0 0.5 0.5000000 0.0000000
    
    New correlation for parametric test
       H1 H2  H3  H4
    H1  1  0  NA  NA
    H2  0  1  NA  NA
    H3 NA NA 1.0 0.5
    H4 NA NA 0.5 1.0
    
    New time fractions for the hypotheses
    [1] 1.0000000 0.3684211 0.3684211 0.3684211
    
    Second-stage sample sizes (controls):
    [1] 60 60
    
    Second-stage sample sizes (treatments):
    [1]  0 60 60 60
    
    Total planned sample sizes after adaptation (controls):
    [1] 95 95
    
    Total planned sample sizes after adaptation (treatments):
    [1] 35 95 95 95
    

---

    A Multi-arm Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
     H1  H2  H3  H4 
    0.5 0.5 0.0 0.0 
    
    Graph Transition Matrix
        H1  H2  H3  H4
    H1 0.0 0.5 0.5 0.0
    H2 0.5 0.0 0.0 0.5
    H3 0.0 1.0 0.0 0.0
    H4 1.0 0.0 0.0 0.0
    
    Correlation for parametric test
        H1  H2  H3  H4
    H1 1.0 0.5  NA  NA
    H2 0.5 1.0  NA  NA
    H3  NA  NA 1.0 0.5
    H4  NA  NA 0.5 1.0
    
    Number of control groups:
    [1] 2
    
    Treatment-to-control assignments (per treatment arm):
    [1] 1 1 2 2
    
    Planned sample sizes per control group:
    [1] 70 70
    
    Planned sample sizes per treatment group:
    [1] 70 70 70 70
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: H1
    
    -- Adaptions from inital specification -----------------------------------------
    
    New hypotheses weights
      H1   H2   H3   H4 
    0.00 0.75 0.25 0.00 
    
    New graph Transition Matrix
       H1  H2        H3        H4
    H1  0 0.0 0.0000000 0.0000000
    H2  0 0.0 0.3333333 0.6666667
    H3  0 1.0 0.0000000 0.0000000
    H4  0 0.5 0.5000000 0.0000000
    
    New correlation for parametric test
       H1 H2  H3  H4
    H1  1  0  NA  NA
    H2  0  1  NA  NA
    H3 NA NA 1.0 0.5
    H4 NA NA 0.5 1.0
    
    New time fractions for the hypotheses
    [1] 1.0000000 0.3684211 0.3684211 0.3684211
    
    Second-stage sample sizes (controls):
    [1] 60 60
    
    Second-stage sample sizes (treatments):
    [1]  0 60 60 60
    
    Total planned sample sizes after adaptation (controls):
    [1] 95 95
    
    Total planned sample sizes after adaptation (treatments):
    [1] 35 95 95 95
    
    -- Final test result -----------------------------------------------------------
    
    Overall p-values of the hypotheses are:
    [1] 1.000 0.012 0.400 0.025
    Hypotheses rejected: H1 and H2

# print.summary.mame_design produces stable output across lifecycle

    A Multi-arm Design object, for testing the 8 hypotheses prim_arm1, prim_arm2, sec_arm1, sec_arm2, HPV+_prim_arm1, HPV+_prim_arm2, HPV+_sec_arm1, and HPV+_sec_arm2 at FWER 0.025.
    
    There are 2 arms (arm1 and arm2), 2 endpoints (prim and sec) and 1 subgroup (HPV+).
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
         prim_arm1      prim_arm2       sec_arm1       sec_arm2 HPV+_prim_arm1 
              0.35           0.35           0.00           0.00           0.15 
    HPV+_prim_arm2  HPV+_sec_arm1  HPV+_sec_arm2 
              0.15           0.00           0.00 
    
    Graph Transition Matrix
                   prim_arm1 prim_arm2 sec_arm1 sec_arm2 HPV+_prim_arm1
    prim_arm1      0.0000000 0.2000000      0.4      0.0      0.2000000
    prim_arm2      0.2000000 0.0000000      0.0      0.4      0.2000000
    sec_arm1       0.0000000 0.3333333      0.0      0.0      0.3333333
    sec_arm2       0.3333333 0.0000000      0.0      0.0      0.3333333
    HPV+_prim_arm1 0.2000000 0.2000000      0.0      0.0      0.0000000
    HPV+_prim_arm2 0.2000000 0.2000000      0.0      0.0      0.2000000
    HPV+_sec_arm1  0.3333333 0.3333333      0.0      0.0      0.0000000
    HPV+_sec_arm2  0.3333333 0.3333333      0.0      0.0      0.3333333
                   HPV+_prim_arm2 HPV+_sec_arm1 HPV+_sec_arm2
    prim_arm1           0.2000000           0.0           0.0
    prim_arm2           0.2000000           0.0           0.0
    sec_arm1            0.3333333           0.0           0.0
    sec_arm2            0.3333333           0.0           0.0
    HPV+_prim_arm1      0.2000000           0.4           0.0
    HPV+_prim_arm2      0.0000000           0.0           0.4
    HPV+_sec_arm1       0.3333333           0.0           0.0
    HPV+_sec_arm2       0.0000000           0.0           0.0
    
    Correlation for parametric test
                   prim_arm1 prim_arm2  sec_arm1  sec_arm2 HPV+_prim_arm1
    prim_arm1      1.0000000 0.5000000        NA        NA      0.6324555
    prim_arm2      0.5000000 1.0000000        NA        NA      0.3162278
    sec_arm1              NA        NA 1.0000000 0.5000000             NA
    sec_arm2              NA        NA 0.5000000 1.0000000             NA
    HPV+_prim_arm1 0.6324555 0.3162278        NA        NA      1.0000000
    HPV+_prim_arm2 0.3162278 0.6324555        NA        NA      0.5000000
    HPV+_sec_arm1         NA        NA 0.6324555 0.3162278             NA
    HPV+_sec_arm2         NA        NA 0.3162278 0.6324555             NA
                   HPV+_prim_arm2 HPV+_sec_arm1 HPV+_sec_arm2
    prim_arm1           0.3162278            NA            NA
    prim_arm2           0.6324555            NA            NA
    sec_arm1                   NA     0.6324555     0.3162278
    sec_arm2                   NA     0.3162278     0.6324555
    HPV+_prim_arm1      0.5000000            NA            NA
    HPV+_prim_arm2      1.0000000            NA            NA
    HPV+_sec_arm1              NA     1.0000000     0.5000000
    HPV+_sec_arm2              NA     0.5000000     1.0000000
    
    Association between hypotheses and arms/endpoints/subgroups:
          hypothesis group endpoint  arm
    1      prim_arm1 Total     prim arm1
    2      prim_arm2 Total     prim arm2
    3       sec_arm1 Total      sec arm1
    4       sec_arm2 Total      sec arm2
    5 HPV+_prim_arm1  HPV+     prim arm1
    6 HPV+_prim_arm2  HPV+     prim arm2
    7  HPV+_sec_arm1  HPV+      sec arm1
    8  HPV+_sec_arm2  HPV+      sec arm2
    
    First stage sample size per arm/group
         arm  HPV+   n
     control FALSE 120
     control  TRUE  80
        arm1 FALSE 120
        arm1  TRUE  80
        arm2 FALSE 120
        arm2  TRUE  80
    
    Interim test is planned at time fraction 0.5
    

---

    A Multi-arm Design object, for testing the 8 hypotheses prim_arm1, prim_arm2, sec_arm1, sec_arm2, HPV+_prim_arm1, HPV+_prim_arm2, HPV+_sec_arm1, and HPV+_sec_arm2 at FWER 0.025.
    
    There are 2 arms (arm1 and arm2), 2 endpoints (prim and sec) and 1 subgroup (HPV+).
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
         prim_arm1      prim_arm2       sec_arm1       sec_arm2 HPV+_prim_arm1 
              0.35           0.35           0.00           0.00           0.15 
    HPV+_prim_arm2  HPV+_sec_arm1  HPV+_sec_arm2 
              0.15           0.00           0.00 
    
    Graph Transition Matrix
                   prim_arm1 prim_arm2 sec_arm1 sec_arm2 HPV+_prim_arm1
    prim_arm1      0.0000000 0.2000000      0.4      0.0      0.2000000
    prim_arm2      0.2000000 0.0000000      0.0      0.4      0.2000000
    sec_arm1       0.0000000 0.3333333      0.0      0.0      0.3333333
    sec_arm2       0.3333333 0.0000000      0.0      0.0      0.3333333
    HPV+_prim_arm1 0.2000000 0.2000000      0.0      0.0      0.0000000
    HPV+_prim_arm2 0.2000000 0.2000000      0.0      0.0      0.2000000
    HPV+_sec_arm1  0.3333333 0.3333333      0.0      0.0      0.0000000
    HPV+_sec_arm2  0.3333333 0.3333333      0.0      0.0      0.3333333
                   HPV+_prim_arm2 HPV+_sec_arm1 HPV+_sec_arm2
    prim_arm1           0.2000000           0.0           0.0
    prim_arm2           0.2000000           0.0           0.0
    sec_arm1            0.3333333           0.0           0.0
    sec_arm2            0.3333333           0.0           0.0
    HPV+_prim_arm1      0.2000000           0.4           0.0
    HPV+_prim_arm2      0.0000000           0.0           0.4
    HPV+_sec_arm1       0.3333333           0.0           0.0
    HPV+_sec_arm2       0.0000000           0.0           0.0
    
    Correlation for parametric test
                   prim_arm1 prim_arm2  sec_arm1  sec_arm2 HPV+_prim_arm1
    prim_arm1      1.0000000 0.5000000        NA        NA      0.6324555
    prim_arm2      0.5000000 1.0000000        NA        NA      0.3162278
    sec_arm1              NA        NA 1.0000000 0.5000000             NA
    sec_arm2              NA        NA 0.5000000 1.0000000             NA
    HPV+_prim_arm1 0.6324555 0.3162278        NA        NA      1.0000000
    HPV+_prim_arm2 0.3162278 0.6324555        NA        NA      0.5000000
    HPV+_sec_arm1         NA        NA 0.6324555 0.3162278             NA
    HPV+_sec_arm2         NA        NA 0.3162278 0.6324555             NA
                   HPV+_prim_arm2 HPV+_sec_arm1 HPV+_sec_arm2
    prim_arm1           0.3162278            NA            NA
    prim_arm2           0.6324555            NA            NA
    sec_arm1                   NA     0.6324555     0.3162278
    sec_arm2                   NA     0.3162278     0.6324555
    HPV+_prim_arm1      0.5000000            NA            NA
    HPV+_prim_arm2      1.0000000            NA            NA
    HPV+_sec_arm1              NA     1.0000000     0.5000000
    HPV+_sec_arm2              NA     0.5000000     1.0000000
    
    Association between hypotheses and arms/endpoints/subgroups:
          hypothesis group endpoint  arm
    1      prim_arm1 Total     prim arm1
    2      prim_arm2 Total     prim arm2
    3       sec_arm1 Total      sec arm1
    4       sec_arm2 Total      sec arm2
    5 HPV+_prim_arm1  HPV+     prim arm1
    6 HPV+_prim_arm2  HPV+     prim arm2
    7  HPV+_sec_arm1  HPV+      sec arm1
    8  HPV+_sec_arm2  HPV+      sec arm2
    
    First stage sample size per arm/group
         arm  HPV+   n
     control FALSE 120
     control  TRUE  80
        arm1 FALSE 120
        arm1  TRUE  80
        arm2 FALSE 120
        arm2  TRUE  80
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: prim_arm1
    

---

    A Multi-arm Design object, for testing the 8 hypotheses prim_arm1, prim_arm2, sec_arm1, sec_arm2, HPV+_prim_arm1, HPV+_prim_arm2, HPV+_sec_arm1, and HPV+_sec_arm2 at FWER 0.025.
    
    There are 2 arms (arm1 and arm2), 2 endpoints (prim and sec) and 1 subgroup (HPV+).
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
         prim_arm1      prim_arm2       sec_arm1       sec_arm2 HPV+_prim_arm1 
              0.35           0.35           0.00           0.00           0.15 
    HPV+_prim_arm2  HPV+_sec_arm1  HPV+_sec_arm2 
              0.15           0.00           0.00 
    
    Graph Transition Matrix
                   prim_arm1 prim_arm2 sec_arm1 sec_arm2 HPV+_prim_arm1
    prim_arm1      0.0000000 0.2000000      0.4      0.0      0.2000000
    prim_arm2      0.2000000 0.0000000      0.0      0.4      0.2000000
    sec_arm1       0.0000000 0.3333333      0.0      0.0      0.3333333
    sec_arm2       0.3333333 0.0000000      0.0      0.0      0.3333333
    HPV+_prim_arm1 0.2000000 0.2000000      0.0      0.0      0.0000000
    HPV+_prim_arm2 0.2000000 0.2000000      0.0      0.0      0.2000000
    HPV+_sec_arm1  0.3333333 0.3333333      0.0      0.0      0.0000000
    HPV+_sec_arm2  0.3333333 0.3333333      0.0      0.0      0.3333333
                   HPV+_prim_arm2 HPV+_sec_arm1 HPV+_sec_arm2
    prim_arm1           0.2000000           0.0           0.0
    prim_arm2           0.2000000           0.0           0.0
    sec_arm1            0.3333333           0.0           0.0
    sec_arm2            0.3333333           0.0           0.0
    HPV+_prim_arm1      0.2000000           0.4           0.0
    HPV+_prim_arm2      0.0000000           0.0           0.4
    HPV+_sec_arm1       0.3333333           0.0           0.0
    HPV+_sec_arm2       0.0000000           0.0           0.0
    
    Correlation for parametric test
                   prim_arm1 prim_arm2  sec_arm1  sec_arm2 HPV+_prim_arm1
    prim_arm1      1.0000000 0.5000000        NA        NA      0.6324555
    prim_arm2      0.5000000 1.0000000        NA        NA      0.3162278
    sec_arm1              NA        NA 1.0000000 0.5000000             NA
    sec_arm2              NA        NA 0.5000000 1.0000000             NA
    HPV+_prim_arm1 0.6324555 0.3162278        NA        NA      1.0000000
    HPV+_prim_arm2 0.3162278 0.6324555        NA        NA      0.5000000
    HPV+_sec_arm1         NA        NA 0.6324555 0.3162278             NA
    HPV+_sec_arm2         NA        NA 0.3162278 0.6324555             NA
                   HPV+_prim_arm2 HPV+_sec_arm1 HPV+_sec_arm2
    prim_arm1           0.3162278            NA            NA
    prim_arm2           0.6324555            NA            NA
    sec_arm1                   NA     0.6324555     0.3162278
    sec_arm2                   NA     0.3162278     0.6324555
    HPV+_prim_arm1      0.5000000            NA            NA
    HPV+_prim_arm2      1.0000000            NA            NA
    HPV+_sec_arm1              NA     1.0000000     0.5000000
    HPV+_sec_arm2              NA     0.5000000     1.0000000
    
    Association between hypotheses and arms/endpoints/subgroups:
          hypothesis group endpoint  arm
    1      prim_arm1 Total     prim arm1
    2      prim_arm2 Total     prim arm2
    3       sec_arm1 Total      sec arm1
    4       sec_arm2 Total      sec arm2
    5 HPV+_prim_arm1  HPV+     prim arm1
    6 HPV+_prim_arm2  HPV+     prim arm2
    7  HPV+_sec_arm1  HPV+      sec arm1
    8  HPV+_sec_arm2  HPV+      sec arm2
    
    First stage sample size per arm/group
         arm  HPV+   n
     control FALSE 120
     control  TRUE  80
        arm1 FALSE 120
        arm1  TRUE  80
        arm2 FALSE 120
        arm2  TRUE  80
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: prim_arm1
    
    -- Adaptions from inital specification -----------------------------------------
    
    New hypotheses weights
         prim_arm1      prim_arm2       sec_arm1       sec_arm2 HPV+_prim_arm1 
               0.6            0.0            0.0            0.0            0.4 
    HPV+_prim_arm2  HPV+_sec_arm1  HPV+_sec_arm2 
               0.0            0.0            0.0 
    
    New graph Transition Matrix
                   prim_arm1 prim_arm2 sec_arm1 sec_arm2 HPV+_prim_arm1
    prim_arm1      0.0000000         0      0.5        0      0.5000000
    prim_arm2      0.0000000         0      0.0        0      0.0000000
    sec_arm1       0.3333333         0      0.0        0      0.6666667
    sec_arm2       0.0000000         0      0.0        0      0.0000000
    HPV+_prim_arm1 0.5000000         0      0.0        0      0.0000000
    HPV+_prim_arm2 0.0000000         0      0.0        0      0.0000000
    HPV+_sec_arm1  0.6666667         0      0.0        0      0.3333333
    HPV+_sec_arm2  0.0000000         0      0.0        0      0.0000000
                   HPV+_prim_arm2 HPV+_sec_arm1 HPV+_sec_arm2
    prim_arm1                   0           0.0             0
    prim_arm2                   0           0.0             0
    sec_arm1                    0           0.0             0
    sec_arm2                    0           0.0             0
    HPV+_prim_arm1              0           0.5             0
    HPV+_prim_arm2              0           0.0             0
    HPV+_sec_arm1               0           0.0             0
    HPV+_sec_arm2               0           0.0             0
    
    New correlation for parametric test
                   prim_arm1 prim_arm2  sec_arm1 sec_arm2 HPV+_prim_arm1
    prim_arm1      1.0000000       NaN        NA       NA      0.6770032
    prim_arm2            NaN         1        NA       NA            NaN
    sec_arm1              NA        NA 1.0000000      NaN             NA
    sec_arm2              NA        NA       NaN        1             NA
    HPV+_prim_arm1 0.6770032       NaN        NA       NA      1.0000000
    HPV+_prim_arm2       NaN       NaN        NA       NA            NaN
    HPV+_sec_arm1         NA        NA 0.6770032      NaN             NA
    HPV+_sec_arm2         NA        NA       NaN      NaN             NA
                   HPV+_prim_arm2 HPV+_sec_arm1 HPV+_sec_arm2
    prim_arm1                 NaN            NA            NA
    prim_arm2                 NaN            NA            NA
    sec_arm1                   NA     0.6770032           NaN
    sec_arm2                   NA           NaN           NaN
    HPV+_prim_arm1            NaN            NA            NA
    HPV+_prim_arm2              1            NA            NA
    HPV+_sec_arm1              NA     1.0000000           NaN
    HPV+_sec_arm2              NA           NaN             1
    
    New time fractions for the hypotheses
    [1] 0.4782609 1.0000000 0.4782609 1.0000000 0.4444444 1.0000000 0.4444444
    [8] 1.0000000
    
    The second stage sample size per arm/group is:
         arm  HPV+   n
     control FALSE 100
     control  TRUE 100
        arm1 FALSE 140
        arm1  TRUE 100
        arm2 FALSE   0
        arm2  TRUE   0

---

    A Multi-arm Design object, for testing the 8 hypotheses prim_arm1, prim_arm2, sec_arm1, sec_arm2, HPV+_prim_arm1, HPV+_prim_arm2, HPV+_sec_arm1, and HPV+_sec_arm2 at FWER 0.025.
    
    There are 2 arms (arm1 and arm2), 2 endpoints (prim and sec) and 1 subgroup (HPV+).
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
         prim_arm1      prim_arm2       sec_arm1       sec_arm2 HPV+_prim_arm1 
              0.35           0.35           0.00           0.00           0.15 
    HPV+_prim_arm2  HPV+_sec_arm1  HPV+_sec_arm2 
              0.15           0.00           0.00 
    
    Graph Transition Matrix
                   prim_arm1 prim_arm2 sec_arm1 sec_arm2 HPV+_prim_arm1
    prim_arm1      0.0000000 0.2000000      0.4      0.0      0.2000000
    prim_arm2      0.2000000 0.0000000      0.0      0.4      0.2000000
    sec_arm1       0.0000000 0.3333333      0.0      0.0      0.3333333
    sec_arm2       0.3333333 0.0000000      0.0      0.0      0.3333333
    HPV+_prim_arm1 0.2000000 0.2000000      0.0      0.0      0.0000000
    HPV+_prim_arm2 0.2000000 0.2000000      0.0      0.0      0.2000000
    HPV+_sec_arm1  0.3333333 0.3333333      0.0      0.0      0.0000000
    HPV+_sec_arm2  0.3333333 0.3333333      0.0      0.0      0.3333333
                   HPV+_prim_arm2 HPV+_sec_arm1 HPV+_sec_arm2
    prim_arm1           0.2000000           0.0           0.0
    prim_arm2           0.2000000           0.0           0.0
    sec_arm1            0.3333333           0.0           0.0
    sec_arm2            0.3333333           0.0           0.0
    HPV+_prim_arm1      0.2000000           0.4           0.0
    HPV+_prim_arm2      0.0000000           0.0           0.4
    HPV+_sec_arm1       0.3333333           0.0           0.0
    HPV+_sec_arm2       0.0000000           0.0           0.0
    
    Correlation for parametric test
                   prim_arm1 prim_arm2  sec_arm1  sec_arm2 HPV+_prim_arm1
    prim_arm1      1.0000000 0.5000000        NA        NA      0.6324555
    prim_arm2      0.5000000 1.0000000        NA        NA      0.3162278
    sec_arm1              NA        NA 1.0000000 0.5000000             NA
    sec_arm2              NA        NA 0.5000000 1.0000000             NA
    HPV+_prim_arm1 0.6324555 0.3162278        NA        NA      1.0000000
    HPV+_prim_arm2 0.3162278 0.6324555        NA        NA      0.5000000
    HPV+_sec_arm1         NA        NA 0.6324555 0.3162278             NA
    HPV+_sec_arm2         NA        NA 0.3162278 0.6324555             NA
                   HPV+_prim_arm2 HPV+_sec_arm1 HPV+_sec_arm2
    prim_arm1           0.3162278            NA            NA
    prim_arm2           0.6324555            NA            NA
    sec_arm1                   NA     0.6324555     0.3162278
    sec_arm2                   NA     0.3162278     0.6324555
    HPV+_prim_arm1      0.5000000            NA            NA
    HPV+_prim_arm2      1.0000000            NA            NA
    HPV+_sec_arm1              NA     1.0000000     0.5000000
    HPV+_sec_arm2              NA     0.5000000     1.0000000
    
    Association between hypotheses and arms/endpoints/subgroups:
          hypothesis group endpoint  arm
    1      prim_arm1 Total     prim arm1
    2      prim_arm2 Total     prim arm2
    3       sec_arm1 Total      sec arm1
    4       sec_arm2 Total      sec arm2
    5 HPV+_prim_arm1  HPV+     prim arm1
    6 HPV+_prim_arm2  HPV+     prim arm2
    7  HPV+_sec_arm1  HPV+      sec arm1
    8  HPV+_sec_arm2  HPV+      sec arm2
    
    First stage sample size per arm/group
         arm  HPV+   n
     control FALSE 120
     control  TRUE  80
        arm1 FALSE 120
        arm1  TRUE  80
        arm2 FALSE 120
        arm2  TRUE  80
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: prim_arm1
    
    -- Adaptions from inital specification -----------------------------------------
    
    New hypotheses weights
         prim_arm1      prim_arm2       sec_arm1       sec_arm2 HPV+_prim_arm1 
               0.6            0.0            0.0            0.0            0.4 
    HPV+_prim_arm2  HPV+_sec_arm1  HPV+_sec_arm2 
               0.0            0.0            0.0 
    
    New graph Transition Matrix
                   prim_arm1 prim_arm2 sec_arm1 sec_arm2 HPV+_prim_arm1
    prim_arm1      0.0000000         0      0.5        0      0.5000000
    prim_arm2      0.0000000         0      0.0        0      0.0000000
    sec_arm1       0.3333333         0      0.0        0      0.6666667
    sec_arm2       0.0000000         0      0.0        0      0.0000000
    HPV+_prim_arm1 0.5000000         0      0.0        0      0.0000000
    HPV+_prim_arm2 0.0000000         0      0.0        0      0.0000000
    HPV+_sec_arm1  0.6666667         0      0.0        0      0.3333333
    HPV+_sec_arm2  0.0000000         0      0.0        0      0.0000000
                   HPV+_prim_arm2 HPV+_sec_arm1 HPV+_sec_arm2
    prim_arm1                   0           0.0             0
    prim_arm2                   0           0.0             0
    sec_arm1                    0           0.0             0
    sec_arm2                    0           0.0             0
    HPV+_prim_arm1              0           0.5             0
    HPV+_prim_arm2              0           0.0             0
    HPV+_sec_arm1               0           0.0             0
    HPV+_sec_arm2               0           0.0             0
    
    New correlation for parametric test
                   prim_arm1 prim_arm2  sec_arm1 sec_arm2 HPV+_prim_arm1
    prim_arm1      1.0000000       NaN        NA       NA      0.6770032
    prim_arm2            NaN         1        NA       NA            NaN
    sec_arm1              NA        NA 1.0000000      NaN             NA
    sec_arm2              NA        NA       NaN        1             NA
    HPV+_prim_arm1 0.6770032       NaN        NA       NA      1.0000000
    HPV+_prim_arm2       NaN       NaN        NA       NA            NaN
    HPV+_sec_arm1         NA        NA 0.6770032      NaN             NA
    HPV+_sec_arm2         NA        NA       NaN      NaN             NA
                   HPV+_prim_arm2 HPV+_sec_arm1 HPV+_sec_arm2
    prim_arm1                 NaN            NA            NA
    prim_arm2                 NaN            NA            NA
    sec_arm1                   NA     0.6770032           NaN
    sec_arm2                   NA           NaN           NaN
    HPV+_prim_arm1            NaN            NA            NA
    HPV+_prim_arm2              1            NA            NA
    HPV+_sec_arm1              NA     1.0000000           NaN
    HPV+_sec_arm2              NA           NaN             1
    
    New time fractions for the hypotheses
    [1] 0.4782609 1.0000000 0.4782609 1.0000000 0.4444444 1.0000000 0.4444444
    [8] 1.0000000
    
    The second stage sample size per arm/group is:
         arm  HPV+   n
     control FALSE 100
     control  TRUE 100
        arm1 FALSE 140
        arm1  TRUE 100
        arm2 FALSE   0
        arm2  TRUE   0
    -- Final test result -----------------------------------------------------------
    
    Overall p-values of the hypotheses are:
    [1] 0.00105 1.00000 0.04250 1.00000 0.00145 1.00000 0.01250 1.00000
    Hypotheses rejected: prim_arm1 and HPV+_prim_arm1

