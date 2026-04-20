get_total_subgroup <- function(n_table, names_arms, names_subgroups) {
  #number of people in each (subgroup, arm) combination
  n_total_subgroups <- do.call(
    rbind,
    lapply(c(names_arms, "control"), \(arm_name) {
      do.call(
        rbind,
        lapply(names_subgroups, \(name) {
          n = sum(n_table[
            n_table[, name] == TRUE & n_table[, "arm"] == arm_name,
            "n"
          ])
          data.frame(arm = arm_name, subgroup = name, n = n)
        })
      )
    })
  )

  # add total n per arm
  n_total_subgroups <- rbind(
    n_total_subgroups,
    do.call(
      rbind,
      lapply(c(names_arms, "control"), \(arm_name) {
        n = sum(n_table[n_table[, "arm"] == arm_name, "n"])
        data.frame(arm = arm_name, subgroup = "Total", n = n)
      })
    )
  )
}
