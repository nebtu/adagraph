# print.summary.cer_design produces stable output across lifecycle

    A CER Design object, for testing 4 hypotheses at FWER 0.025.
    
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

    A CER Design object, for testing 4 hypotheses at FWER 0.025.
    
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
    Hypotheses rejected at the interim: 1
    

---

    A CER Design object, for testing 4 hypotheses at FWER 0.025.
    
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
    Hypotheses rejected at the interim: 1
    
    -- Adaptions from inital specification -----------------------------------------
    
    New hypotheses weights
           H1        H2        H3        H4 
    0.3333333 0.6666667 0.0000000 0.0000000 
    

---

    A CER Design object, for testing 4 hypotheses at FWER 0.025.
    
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
    Hypotheses rejected at the interim: 1
    
    -- Adaptions from inital specification -----------------------------------------
    
    New hypotheses weights
           H1        H2        H3        H4 
    0.3333333 0.6666667 0.0000000 0.0000000 
    
    -- Final test result -----------------------------------------------------------
    
    Overall p-values of the hypotheses are:
    [1] 0.200 0.011 0.500 0.030
    Hypotheses rejected: 
    [1] 1 2

# print.summary.multiarm_cer_design produces stable output across lifecycle

    A Multi-arm Design object, for testing 4 hypotheses at FWER 0.025.
    
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

    A Multi-arm Design object, for testing 4 hypotheses at FWER 0.025.
    
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
    Hypotheses rejected at the interim: 1
    

---

    A Multi-arm Design object, for testing 4 hypotheses at FWER 0.025.
    
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
    Hypotheses rejected at the interim: 1
    
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

    A Multi-arm Design object, for testing 4 hypotheses at FWER 0.025.
    
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
    Hypotheses rejected at the interim: 1
    
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
    Hypotheses rejected: 
    [1] 1 2

