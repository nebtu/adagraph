# print.adagraph_design produces stable output

    A Adagraph Design object, for testing the 2 hypotheses H1 and H2 at FWER 0.05.
    
    -- No interim test has been performed yet. -------------------------------------
    -- No adaptations have been performed yet --------------------------------------
    -- No final test has been performed yet ----------------------------------------

# print.cer_design produces stable output across lifecycle

    A CER Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    -- No interim test has been performed yet. -------------------------------------
    -- No adaptations have been performed yet --------------------------------------
    -- No final test has been performed yet ----------------------------------------

---

    A CER Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: H1
    -- No adaptations have been performed yet --------------------------------------
    -- No final test has been performed yet ----------------------------------------

---

    A CER Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: H1
    -- The following characteristics have been adapted: ----------------------------
    * Hypotheses weights
    -- No final test has been performed yet ----------------------------------------

---

    A CER Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: H1
    -- The following characteristics have been adapted: ----------------------------
    * Hypotheses weights
    -- Final test result -----------------------------------------------------------
    Hypotheses rejected: H1 and H2

# print.multiarm_cer_design produces stable output across lifecycle

    A Multi-arm Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    There are 2 control groups for a total of 4 hypotheses.
    
    -- No interim test has been performed yet. -------------------------------------
    -- No adaptations have been performed yet --------------------------------------
    -- No final test has been performed yet ----------------------------------------

---

    A Multi-arm Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    There are 2 control groups for a total of 4 hypotheses.
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: H1
    -- No adaptations have been performed yet --------------------------------------
    -- No final test has been performed yet ----------------------------------------

---

    A Multi-arm Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    There are 2 control groups for a total of 4 hypotheses.
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: H1
    -- The following characteristics have been adapted: ----------------------------
    * Hypotheses weights
    * Graph Transition Matrix
    * Correlation for parametric test
    * Time fractions for the hypotheses
    -- No final test has been performed yet ----------------------------------------

---

    A Multi-arm Design object, for testing the 4 hypotheses H1, H2, H3, and H4 at FWER 0.025.
    
    There are 2 control groups for a total of 4 hypotheses.
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: H1
    -- The following characteristics have been adapted: ----------------------------
    * Hypotheses weights
    * Graph Transition Matrix
    * Correlation for parametric test
    * Time fractions for the hypotheses
    -- Final test result -----------------------------------------------------------
    Hypotheses rejected: H1 and H2

# print.trial_design produces stable output across lifecycle

    A trial Design object, for testing the 8 hypotheses prim_arm1, prim_arm2, sec_arm1, sec_arm2, HPV+_prim_arm1, HPV+_prim_arm2, HPV+_sec_arm1, and HPV+_sec_arm2 at FWER 0.025.
    
    There are 2 arms (arm1 and arm2), 2 endpoints (prim and sec) and 1 subgroup (HPV+).
    The first stage sample size per arm/group is:
         arm  HPV+   n
     control FALSE 120
     control  TRUE  80
        arm1 FALSE 120
        arm1  TRUE  80
        arm2 FALSE 120
        arm2  TRUE  80
    
    -- No interim test has been performed yet. -------------------------------------
    -- No adaptations have been performed yet --------------------------------------
    -- No final test has been performed yet ----------------------------------------

---

    A trial Design object, for testing the 8 hypotheses prim_arm1, prim_arm2, sec_arm1, sec_arm2, HPV+_prim_arm1, HPV+_prim_arm2, HPV+_sec_arm1, and HPV+_sec_arm2 at FWER 0.025.
    
    There are 2 arms (arm1 and arm2), 2 endpoints (prim and sec) and 1 subgroup (HPV+).
    The first stage sample size per arm/group is:
         arm  HPV+   n
     control FALSE 120
     control  TRUE  80
        arm1 FALSE 120
        arm1  TRUE  80
        arm2 FALSE 120
        arm2  TRUE  80
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: prim_arm1
    -- No adaptations have been performed yet --------------------------------------
    -- No final test has been performed yet ----------------------------------------

---

    A trial Design object, for testing the 8 hypotheses prim_arm1, prim_arm2, sec_arm1, sec_arm2, HPV+_prim_arm1, HPV+_prim_arm2, HPV+_sec_arm1, and HPV+_sec_arm2 at FWER 0.025.
    
    There are 2 arms (arm1 and arm2), 2 endpoints (prim and sec) and 1 subgroup (HPV+).
    The first stage sample size per arm/group is:
         arm  HPV+   n
     control FALSE 120
     control  TRUE  80
        arm1 FALSE 120
        arm1  TRUE  80
        arm2 FALSE 120
        arm2  TRUE  80
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: prim_arm1
    -- The following characteristics have been adapted: ----------------------------
    * Hypotheses weights
    * Graph Transition Matrix
    * Correlation for parametric test
    * Time fractions for the hypotheses
    The second stage sample size per arm/group is:
         arm  HPV+   n
     control FALSE 100
     control  TRUE 100
        arm1 FALSE 140
        arm1  TRUE 100
        arm2 FALSE   0
        arm2  TRUE   0
    -- No final test has been performed yet ----------------------------------------

---

    A trial Design object, for testing the 8 hypotheses prim_arm1, prim_arm2, sec_arm1, sec_arm2, HPV+_prim_arm1, HPV+_prim_arm2, HPV+_sec_arm1, and HPV+_sec_arm2 at FWER 0.025.
    
    There are 2 arms (arm1 and arm2), 2 endpoints (prim and sec) and 1 subgroup (HPV+).
    The first stage sample size per arm/group is:
         arm  HPV+   n
     control FALSE 120
     control  TRUE  80
        arm1 FALSE 120
        arm1  TRUE  80
        arm2 FALSE 120
        arm2  TRUE  80
    
    -- An interim test has been performed. -----------------------------------------
    Hypotheses rejected at the interim: prim_arm1
    -- The following characteristics have been adapted: ----------------------------
    * Hypotheses weights
    * Graph Transition Matrix
    * Correlation for parametric test
    * Time fractions for the hypotheses
    The second stage sample size per arm/group is:
         arm  HPV+   n
     control FALSE 100
     control  TRUE 100
        arm1 FALSE 140
        arm1  TRUE 100
        arm2 FALSE   0
        arm2  TRUE   0
    -- Final test result -----------------------------------------------------------
    Hypotheses rejected: prim_arm1 and HPV+_prim_arm1

