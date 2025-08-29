# print.adagraph_design produces stable output

    A Adagraph Design object, for testing 2 hypotheses at FWER 0.05.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
    [1] 0.5 0.5
    
    Graph Transition Matrix
         [,1] [,2]
    [1,]    0    1
    [2,]    1    0
    
    Correlation for parametric test
         [,1] [,2]
    [1,]    1   NA
    [2,]   NA    1
    

# print.cer_design produces stable output across lifecycle

    A CER Design object, for testing 4 hypotheses at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
    [1] 0.5 0.5 0.0 0.0
    
    Graph Transition Matrix
       [,1] [,2] [,3] [,4]
    H1  0.0  0.5  0.5  0.0
    H2  0.5  0.0  0.0  0.5
    H3  0.0  1.0  0.0  0.0
    H4  1.0  0.0  0.0  0.0
    
    Correlation for parametric test
         [,1] [,2] [,3] [,4]
    [1,]  1.0  0.5   NA   NA
    [2,]  0.5  1.0   NA   NA
    [3,]   NA   NA  1.0  0.5
    [4,]   NA   NA  0.5  1.0
    
    Interim test is planned at time fraction 0.5
    

---

    A CER Design object, for testing 4 hypotheses at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
    [1] 0.5 0.5 0.0 0.0
    
    Graph Transition Matrix
       [,1] [,2] [,3] [,4]
    H1  0.0  0.5  0.5  0.0
    H2  0.5  0.0  0.0  0.5
    H3  0.0  1.0  0.0  0.0
    H4  1.0  0.0  0.0  0.0
    
    Correlation for parametric test
         [,1] [,2] [,3] [,4]
    [1,]  1.0  0.5   NA   NA
    [2,]  0.5  1.0   NA   NA
    [3,]   NA   NA  1.0  0.5
    [4,]   NA   NA  0.5  1.0
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: 1
    

---

    A CER Design object, for testing 4 hypotheses at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
    [1] 0.5 0.5 0.0 0.0
    
    Graph Transition Matrix
       [,1] [,2] [,3] [,4]
    H1  0.0  0.5  0.5  0.0
    H2  0.5  0.0  0.0  0.5
    H3  0.0  1.0  0.0  0.0
    H4  1.0  0.0  0.0  0.0
    
    Correlation for parametric test
         [,1] [,2] [,3] [,4]
    [1,]  1.0  0.5   NA   NA
    [2,]  0.5  1.0   NA   NA
    [3,]   NA   NA  1.0  0.5
    [4,]   NA   NA  0.5  1.0
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: 1
    
    -- Adaptions from inital specification -----------------------------------------
    
    New hypotheses weights
    [1] 0.3333333 0.6666667 0.0000000 0.0000000
    

---

    A CER Design object, for testing 4 hypotheses at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
    [1] 0.5 0.5 0.0 0.0
    
    Graph Transition Matrix
       [,1] [,2] [,3] [,4]
    H1  0.0  0.5  0.5  0.0
    H2  0.5  0.0  0.0  0.5
    H3  0.0  1.0  0.0  0.0
    H4  1.0  0.0  0.0  0.0
    
    Correlation for parametric test
         [,1] [,2] [,3] [,4]
    [1,]  1.0  0.5   NA   NA
    [2,]  0.5  1.0   NA   NA
    [3,]   NA   NA  1.0  0.5
    [4,]   NA   NA  0.5  1.0
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: 1
    
    -- Adaptions from inital specification -----------------------------------------
    
    New hypotheses weights
    [1] 0.3333333 0.6666667 0.0000000 0.0000000
    
    -- Final test result -----------------------------------------------------------
    
    Overall p-values of the hypotheses are:
    [1] 0.200 0.011 0.500 0.030
    Hypotheses rejected: 
    [1] 1 2 4

# print.multiarm_cer_design produces stable output across lifecycle

    A Multi-arm Design object, for testing 4 hypotheses at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
    [1] 0.5 0.5 0.0 0.0
    
    Graph Transition Matrix
       [,1] [,2] [,3] [,4]
    H1  0.0  0.5  0.5  0.0
    H2  0.5  0.0  0.0  0.5
    H3  0.0  1.0  0.0  0.0
    H4  1.0  0.0  0.0  0.0
    
    Correlation for parametric test
         [,1] [,2] [,3] [,4]
    [1,]  1.0   NA  0.5   NA
    [2,]   NA  1.0   NA  0.5
    [3,]  0.5   NA  1.0   NA
    [4,]   NA  0.5   NA  1.0
    
    Number of control groups:
    [1] 2
    
    Treatment-to-control assignments (per treatment arm):
    [1] 1 2 1 2
    
    Planned sample sizes per control group:
    [1] 70 70
    
    Planned sample sizes per treatment group:
    [1] 70 70 70 70
    
    Interim test is planned at time fraction 0.5
    

---

    A Multi-arm Design object, for testing 4 hypotheses at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
    [1] 0.5 0.5 0.0 0.0
    
    Graph Transition Matrix
       [,1] [,2] [,3] [,4]
    H1  0.0  0.5  0.5  0.0
    H2  0.5  0.0  0.0  0.5
    H3  0.0  1.0  0.0  0.0
    H4  1.0  0.0  0.0  0.0
    
    Correlation for parametric test
         [,1] [,2] [,3] [,4]
    [1,]  1.0   NA  0.5   NA
    [2,]   NA  1.0   NA  0.5
    [3,]  0.5   NA  1.0   NA
    [4,]   NA  0.5   NA  1.0
    
    Number of control groups:
    [1] 2
    
    Treatment-to-control assignments (per treatment arm):
    [1] 1 2 1 2
    
    Planned sample sizes per control group:
    [1] 70 70
    
    Planned sample sizes per treatment group:
    [1] 70 70 70 70
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: 1
    

---

    A Multi-arm Design object, for testing 4 hypotheses at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
    [1] 0.5 0.5 0.0 0.0
    
    Graph Transition Matrix
       [,1] [,2] [,3] [,4]
    H1  0.0  0.5  0.5  0.0
    H2  0.5  0.0  0.0  0.5
    H3  0.0  1.0  0.0  0.0
    H4  1.0  0.0  0.0  0.0
    
    Correlation for parametric test
         [,1] [,2] [,3] [,4]
    [1,]  1.0   NA  0.5   NA
    [2,]   NA  1.0   NA  0.5
    [3,]  0.5   NA  1.0   NA
    [4,]   NA  0.5   NA  1.0
    
    Number of control groups:
    [1] 2
    
    Treatment-to-control assignments (per treatment arm):
    [1] 1 2 1 2
    
    Planned sample sizes per control group:
    [1] 70 70
    
    Planned sample sizes per treatment group:
    [1] 70 70 70 70
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: 1
    
    -- Adaptions from inital specification -----------------------------------------
    
    New hypotheses weights
    [1] 0.00 0.75 0.25 0.00
    
    New graph Transition Matrix
       [,1] [,2]      [,3]      [,4]
    H1    0  0.0 0.0000000 0.0000000
    H2    0  0.0 0.3333333 0.6666667
    H3    0  1.0 0.0000000 0.0000000
    H4    0  0.5 0.5000000 0.0000000
    
    New correlation for parametric test
         [,1] [,2] [,3] [,4]
    [1,]    1   NA    0   NA
    [2,]   NA  1.0   NA  0.5
    [3,]    0   NA    1   NA
    [4,]   NA  0.5   NA  1.0
    
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

    A Multi-arm Design object, for testing 4 hypotheses at FWER 0.025.
    
    -- Inital design specification -------------------------------------------------
    
    Hypotheses weights
    [1] 0.5 0.5 0.0 0.0
    
    Graph Transition Matrix
       [,1] [,2] [,3] [,4]
    H1  0.0  0.5  0.5  0.0
    H2  0.5  0.0  0.0  0.5
    H3  0.0  1.0  0.0  0.0
    H4  1.0  0.0  0.0  0.0
    
    Correlation for parametric test
         [,1] [,2] [,3] [,4]
    [1,]  1.0   NA  0.5   NA
    [2,]   NA  1.0   NA  0.5
    [3,]  0.5   NA  1.0   NA
    [4,]   NA  0.5   NA  1.0
    
    Number of control groups:
    [1] 2
    
    Treatment-to-control assignments (per treatment arm):
    [1] 1 2 1 2
    
    Planned sample sizes per control group:
    [1] 70 70
    
    Planned sample sizes per treatment group:
    [1] 70 70 70 70
    
    Interim test is planned at time fraction 0.5
    
    -- Interim test result ---------------------------------------------------------
    
    P-values of interim test are:
    [1] 0.00045 0.09520 0.02250 0.11040
    Hypotheses rejected at the interim: 1
    
    -- Adaptions from inital specification -----------------------------------------
    
    New hypotheses weights
    [1] 0.00 0.75 0.25 0.00
    
    New graph Transition Matrix
       [,1] [,2]      [,3]      [,4]
    H1    0  0.0 0.0000000 0.0000000
    H2    0  0.0 0.3333333 0.6666667
    H3    0  1.0 0.0000000 0.0000000
    H4    0  0.5 0.5000000 0.0000000
    
    New correlation for parametric test
         [,1] [,2] [,3] [,4]
    [1,]    1   NA    0   NA
    [2,]   NA  1.0   NA  0.5
    [3,]    0   NA    1   NA
    [4,]   NA  0.5   NA  1.0
    
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
    Hypotheses rejected: 
    [1] 1 2

