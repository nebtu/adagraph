# intersection_hypotheses works as expected

    structure(list(hyp_names = c("H1∩H2∩H3∩H4", "H1∩H2∩H3", 
    "H1∩H2∩H4", "H1∩H3∩H4", "H2∩H3∩H4", "H1∩H2", "H1∩H3", 
    "H1∩H4", "H2∩H3", "H2∩H4", "H3∩H4", "H1", "H2", "H3", 
    "H4"), H1 = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, 
    FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), H2 = c(TRUE, 
    TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, 
    FALSE, TRUE, FALSE, FALSE), H3 = c(TRUE, TRUE, FALSE, TRUE, TRUE, 
    FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE
    ), H4 = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, 
    FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE), weights_H1 = c(0.5, 
    0.5, 0.5, 0.75, NA, 0.5, 1, 0.75, NA, NA, NA, 1, NA, NA, NA), 
        weights_H2 = c(0.5, 0.5, 0.5, NA, 0.75, 0.5, NA, NA, 0.75, 
        1, NA, NA, 1, NA, NA), weights_H3 = c(0, 0, NA, 0, 0.25, 
        NA, 0, NA, 0.25, NA, 0.5, NA, NA, 1, NA), weights_H4 = c(0, 
        NA, 0, 0.25, 0, NA, NA, 0.25, NA, 0, 0.5, NA, NA, NA, 1), 
        bound_interim_H1 = c(0.000782138006841732, 0.000782138006841732, 
        0.000782138006841732, 0.00114399206849175, NA, 0.000782138006841732, 
        0.001525322757989, 0.00114399206849175, NA, NA, NA, 0.001525322757989, 
        NA, NA, NA), bound_interim_H2 = c(0.000782138006841732, 0.000782138006841732, 
        0.000782138006841732, NA, 0.00114399206849175, 0.000782138006841732, 
        NA, NA, 0.00114399206849175, 0.001525322757989, NA, NA, 0.001525322757989, 
        NA, NA), bound_interim_H3 = c(0, 0, NA, 0, 0.00038133068949725, 
        NA, 0, NA, 0.00038133068949725, NA, 0.000782138006841732, 
        NA, NA, 0.001525322757989, NA), bound_interim_H4 = c(0, NA, 
        0, 0.00038133068949725, 0, NA, NA, 0.00038133068949725, NA, 
        0, 0.000782138006841732, NA, NA, NA, 0.001525322757989), 
        bound_final_H1 = c(0.0131652663097635, 0.0131652663097635, 
        0.0131652663097635, 0.0183066569770051, NA, 0.0131652663097635, 
        0.0244996124264288, 0.0183066569770051, NA, NA, NA, 0.0244996124264288, 
        NA, NA, NA), bound_final_H2 = c(0.0131652663097635, 0.0131652663097635, 
        0.0131652663097635, NA, 0.0183066569770051, 0.0131652663097635, 
        NA, NA, 0.0183066569770051, 0.0244996124264288, NA, NA, 0.0244996124264288, 
        NA, NA), bound_final_H3 = c(0, 0, NA, 0, 0.00610221899233503, 
        NA, 0, NA, 0.00610221899233503, NA, 0.0131652663097635, NA, 
        NA, 0.0244996124264288, NA), bound_final_H4 = c(0, NA, 0, 
        0.00610221899233503, 0, NA, NA, 0.00610221899233503, NA, 
        0, 0.0131652663097635, NA, NA, NA, 0.0244996124264288)), row.names = c(15L, 
    14L, 13L, 11L, 7L, 12L, 10L, 9L, 6L, 5L, 3L, 8L, 4L, 2L, 1L), class = c("intersection_hypotheses", 
    "data.frame"))

---

    Initial specification
      Hypotheses        Weights           Interim Bounds         Final Bounds
     H1∩H2∩H3∩H4 0.5, 0.5, 0, 0 0.000782, 0.000782, 0, 0 0.0132, 0.0132, 0, 0
        H1∩H2∩H3    0.5, 0.5, 0    0.000782, 0.000782, 0    0.0132, 0.0132, 0
        H1∩H2∩H4    0.5, 0.5, 0    0.000782, 0.000782, 0    0.0132, 0.0132, 0
        H1∩H3∩H4  0.75, 0, 0.25     0.00114, 0, 0.000381    0.0183, 0, 0.0061
        H2∩H3∩H4  0.75, 0.25, 0     0.00114, 0.000381, 0    0.0183, 0.0061, 0
           H1∩H2       0.5, 0.5       0.000782, 0.000782       0.0132, 0.0132
           H1∩H3           1, 0               0.00153, 0            0.0245, 0
           H1∩H4     0.75, 0.25        0.00114, 0.000381       0.0183, 0.0061
           H2∩H3     0.75, 0.25        0.00114, 0.000381       0.0183, 0.0061
           H2∩H4           1, 0               0.00153, 0            0.0245, 0
           H3∩H4       0.5, 0.5       0.000782, 0.000782       0.0132, 0.0132
              H1              1                  0.00153               0.0245
              H2              1                  0.00153               0.0245
              H3              1                  0.00153               0.0245
              H4              1                  0.00153               0.0245
    

---

    structure(list(hyp_names = c("H1∩H2∩H3∩H4", "H1∩H2∩H3", 
    "H1∩H2∩H4", "H1∩H3∩H4", "H2∩H3∩H4", "H1∩H2", "H1∩H3", 
    "H1∩H4", "H2∩H3", "H2∩H4", "H3∩H4", "H1", "H2", "H3", 
    "H4"), H1 = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, 
    FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), H2 = c(TRUE, 
    TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, 
    FALSE, TRUE, FALSE, FALSE), H3 = c(TRUE, TRUE, FALSE, TRUE, TRUE, 
    FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE
    ), H4 = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, 
    FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE), weights_H1 = c(0.5, 
    0.5, 0.5, 0.75, NA, 0.5, 1, 0.75, NA, NA, NA, 1, NA, NA, NA), 
        weights_H2 = c(0.5, 0.5, 0.5, NA, 0.75, 0.5, NA, NA, 0.75, 
        1, NA, NA, 1, NA, NA), weights_H3 = c(0, 0, NA, 0, 0.25, 
        NA, 0, NA, 0.25, NA, 0.5, NA, NA, 1, NA), weights_H4 = c(0, 
        NA, 0, 0.25, 0, NA, NA, 0.25, NA, 0, 0.5, NA, NA, NA, 1), 
        bound_interim_H1 = c(0.000782138006841732, 0.000782138006841732, 
        0.000782138006841732, 0.00114399206849175, NA, 0.000782138006841732, 
        0.001525322757989, 0.00114399206849175, NA, NA, NA, 0.001525322757989, 
        NA, NA, NA), bound_interim_H2 = c(0.000782138006841732, 0.000782138006841732, 
        0.000782138006841732, NA, 0.00114399206849175, 0.000782138006841732, 
        NA, NA, 0.00114399206849175, 0.001525322757989, NA, NA, 0.001525322757989, 
        NA, NA), bound_interim_H3 = c(0, 0, NA, 0, 0.00038133068949725, 
        NA, 0, NA, 0.00038133068949725, NA, 0.000782138006841732, 
        NA, NA, 0.001525322757989, NA), bound_interim_H4 = c(0, NA, 
        0, 0.00038133068949725, 0, NA, NA, 0.00038133068949725, NA, 
        0, 0.000782138006841732, NA, NA, NA, 0.001525322757989), 
        bound_final_H1 = c(0.0131652663097635, 0.0131652663097635, 
        0.0131652663097635, 0.0183066569770051, NA, 0.0131652663097635, 
        0.0244996124264288, 0.0183066569770051, NA, NA, NA, 0.0244996124264288, 
        NA, NA, NA), bound_final_H2 = c(0.0131652663097635, 0.0131652663097635, 
        0.0131652663097635, NA, 0.0183066569770051, 0.0131652663097635, 
        NA, NA, 0.0183066569770051, 0.0244996124264288, NA, NA, 0.0244996124264288, 
        NA, NA), bound_final_H3 = c(0, 0, NA, 0, 0.00610221899233503, 
        NA, 0, NA, 0.00610221899233503, NA, 0.0131652663097635, NA, 
        NA, 0.0244996124264288, NA), bound_final_H4 = c(0, NA, 0, 
        0.00610221899233503, 0, NA, NA, 0.00610221899233503, NA, 
        0, 0.0131652663097635, NA, NA, NA, 0.0244996124264288), interim_rej = c(TRUE, 
        TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, 
        FALSE, TRUE, FALSE, FALSE, FALSE), ad_weights_H1 = c(0, 0, 
        0, 0, NA, 0, 0, 0, NA, NA, NA, 0, NA, NA, NA), ad_weights_H2 = c(0.75, 
        0.75, 1, NA, 0.75, 1, NA, NA, 0.75, 1, NA, NA, 1, NA, NA), 
        ad_weights_H3 = c(0.25, 0.25, NA, 0.5, 0.25, NA, 1, NA, 0.25, 
        NA, 0.5, NA, NA, 1, NA), ad_weights_H4 = c(0, NA, 0, 0.5, 
        0, NA, NA, 1, NA, 0, 0.5, NA, NA, NA, 1), ad_bound_final_H1 = c(0, 
        0, 0, 0, NA, 0, 0, 0, NA, NA, NA, 0, NA, NA, NA), ad_bound_final_H2 = c(0, 
        0, 0, NA, 0.0183066585800663, 0, NA, NA, 0.0183066585800663, 
        0.0244996122950263, NA, NA, 0.0244996122950263, NA, NA), 
        ad_bound_final_H3 = c(0, 0, NA, 0, 0.00610221952668877, NA, 
        0, NA, 0.00610221952668877, NA, 0.0131652664394825, NA, NA, 
        0.0244996305142452, NA), ad_bound_final_H4 = c(0, NA, 0, 
        0, 0, NA, NA, 0, NA, 0, 0.0131652664394825, NA, NA, NA, 0.0244996105227225
        ), rej = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
        TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)), row.names = c(15L, 
    14L, 13L, 11L, 7L, 12L, 10L, 9L, 6L, 5L, 3L, 8L, 4L, 2L, 1L), class = c("intersection_hypotheses", 
    "data.frame"))

---

    Initial specification and interim result
      Hypotheses        Weights           Interim Bounds         Final Bounds
     H1∩H2∩H3∩H4 0.5, 0.5, 0, 0 0.000782, 0.000782, 0, 0 0.0132, 0.0132, 0, 0
        H1∩H2∩H3    0.5, 0.5, 0    0.000782, 0.000782, 0    0.0132, 0.0132, 0
        H1∩H2∩H4    0.5, 0.5, 0    0.000782, 0.000782, 0    0.0132, 0.0132, 0
        H1∩H3∩H4  0.75, 0, 0.25     0.00114, 0, 0.000381    0.0183, 0, 0.0061
        H2∩H3∩H4  0.75, 0.25, 0     0.00114, 0.000381, 0    0.0183, 0.0061, 0
           H1∩H2       0.5, 0.5       0.000782, 0.000782       0.0132, 0.0132
           H1∩H3           1, 0               0.00153, 0            0.0245, 0
           H1∩H4     0.75, 0.25        0.00114, 0.000381       0.0183, 0.0061
           H2∩H3     0.75, 0.25        0.00114, 0.000381       0.0183, 0.0061
           H2∩H4           1, 0               0.00153, 0            0.0245, 0
           H3∩H4       0.5, 0.5       0.000782, 0.000782       0.0132, 0.0132
              H1              1                  0.00153               0.0245
              H2              1                  0.00153               0.0245
              H3              1                  0.00153               0.0245
              H4              1                  0.00153               0.0245
     Rejected at Interim
                    TRUE
                    TRUE
                    TRUE
                    TRUE
                   FALSE
                    TRUE
                    TRUE
                    TRUE
                   FALSE
                   FALSE
                   FALSE
                    TRUE
                   FALSE
                   FALSE
                   FALSE
    
    After adaptation and final result
      Hypotheses  Adapted Weights Adapted Final Bounds Rejected
     H1∩H2∩H3∩H4 0, 0.75, 0.25, 0           0, 0, 0, 0     TRUE
        H1∩H2∩H3    0, 0.75, 0.25              0, 0, 0     TRUE
        H1∩H2∩H4          0, 1, 0              0, 0, 0     TRUE
        H1∩H3∩H4      0, 0.5, 0.5              0, 0, 0     TRUE
        H2∩H3∩H4    0.75, 0.25, 0    0.0183, 0.0061, 0     TRUE
           H1∩H2             0, 1                 0, 0     TRUE
           H1∩H3             0, 1                 0, 0     TRUE
           H1∩H4             0, 1                 0, 0     TRUE
           H2∩H3       0.75, 0.25       0.0183, 0.0061     TRUE
           H2∩H4             1, 0            0.0245, 0     TRUE
           H3∩H4         0.5, 0.5       0.0132, 0.0132    FALSE
              H1                0                    0     TRUE
              H2                1               0.0245     TRUE
              H3                1               0.0245    FALSE
              H4                1               0.0245     TRUE

