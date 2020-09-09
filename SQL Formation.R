# --- SQL exercices --- #

library("sqldf")

# Exploration of Orange DB
sqldf("SELECT *
      FROM Orange")
sqldf("SELECT DISTINCT age
      FROM Orange")

sqldf("SELECT *
      FROM Orange
      WHERE Tree = 1")

sqldf("SELECT *
      FROM Orange
      WHERE Tree = 1
      AND age = 484")
sqldf("SELECT *
      FROM Orange
      WHERE Tree = 1
      OR Tree = 2")

sqldf("SELECT *
      FROM Orange
      ORDER BY age ASC")
sqldf("SELECT *
      FROM Orange
      ORDER BY age")
# sqldf("SELECT * 
#       FROM iris
#       WHERE Species LIKE's%[ar]'")
