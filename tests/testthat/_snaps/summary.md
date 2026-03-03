# print.summary.cer_design produces stable output across lifecycle

    A CER Design object, for testing  hypotheses at FWER 0.025.
    
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
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: 1
    -- No adaptions have been performed yet ----------------------------------------
    -- No final test has been performed yet ----------------------------------------

---

    A CER Design object, for testing  hypotheses at FWER 0.025.
    
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

    A CER Design object, for testing  hypotheses at FWER 0.025.
    
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
    [1] 1 2

# print.summary.multiarm_cer_design produces stable output across lifecycle

    A Multi-arm Design object, for testing 4 hypotheses at FWER 0.025.
    
    There are 2 control groups for a total of 4 hypotheses.
    
    -- No interim test has been performed yet. -------------------------------------
    -- No adaptions have been performed yet ----------------------------------------
    -- No final test has been performed yet ----------------------------------------

---

    A Multi-arm Design object, for testing 4 hypotheses at FWER 0.025.
    
    There are 2 control groups for a total of 4 hypotheses.
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: 1
    -- No adaptions have been performed yet ----------------------------------------
    -- No final test has been performed yet ----------------------------------------

---

    A Multi-arm Design object, for testing 4 hypotheses at FWER 0.025.
    
    There are 2 control groups for a total of 4 hypotheses.
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: 1
    -- The following characteristics have been adapted: ----------------------------
    * Hypotheses weights
    * Graph Transition Matrix
    * Correlation for parametric test
    * Time fractions for the hypotheses
    -- No final test has been performed yet ----------------------------------------

---

    A Multi-arm Design object, for testing 4 hypotheses at FWER 0.025.
    
    There are 2 control groups for a total of 4 hypotheses.
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: 1
    -- The following characteristics have been adapted: ----------------------------
    * Hypotheses weights
    * Graph Transition Matrix
    * Correlation for parametric test
    * Time fractions for the hypotheses
    -- Final test result -----------------------------------------------------------
    Hypotheses rejected: 1 and 2

