# Basic fetch

    # A tibble: 100 x 3
       date       city      activeUsers
       <date>     <chr>           <dbl>
     1 2022-04-21 (not set)         186
     2 2022-04-05 (not set)         174
     3 2022-03-31 (not set)         173
     4 2022-04-07 (not set)         173
     5 2022-04-25 (not set)         171
     6 2022-04-26 (not set)         170
     7 2022-04-06 (not set)         166
     8 2022-04-19 (not set)         163
     9 2022-04-13 (not set)         160
    10 2022-04-11 (not set)         150
    # ... with 90 more rows

# Pagination

    Code
      all_results
    Output
      # A tibble: 1,276 x 5
         date       city      dayOfWeek activeUsers sessions
         <date>     <chr>     <chr>           <dbl>    <dbl>
       1 2022-04-21 (not set) 4                 186      210
       2 2022-04-05 (not set) 2                 174      193
       3 2022-03-31 (not set) 4                 173      196
       4 2022-04-07 (not set) 4                 173      192
       5 2022-04-25 (not set) 1                 171      206
       6 2022-04-26 (not set) 2                 170      191
       7 2022-04-06 (not set) 3                 166      187
       8 2022-04-19 (not set) 2                 163      184
       9 2022-04-13 (not set) 3                 160      176
      10 2022-04-11 (not set) 1                 150      173
      # ... with 1,266 more rows

---

    Code
      all_results_paged
    Output
      # A tibble: 1,276 x 5
         date       city      dayOfWeek activeUsers sessions
         <date>     <chr>     <chr>           <dbl>    <dbl>
       1 2022-04-21 (not set) 4                 186      210
       2 2022-04-05 (not set) 2                 174      193
       3 2022-03-31 (not set) 4                 173      196
       4 2022-04-07 (not set) 4                 173      192
       5 2022-04-25 (not set) 1                 171      206
       6 2022-04-26 (not set) 2                 170      191
       7 2022-04-06 (not set) 3                 166      187
       8 2022-04-19 (not set) 2                 163      184
       9 2022-04-13 (not set) 3                 160      176
      10 2022-04-11 (not set) 1                 150      173
      # ... with 1,266 more rows

---

    Code
      top_510
    Output
      # A tibble: 510 x 5
         date       city      dayOfWeek activeUsers sessions
         <date>     <chr>     <chr>           <dbl>    <dbl>
       1 2022-04-21 (not set) 4                 186      210
       2 2022-04-05 (not set) 2                 174      193
       3 2022-03-31 (not set) 4                 173      196
       4 2022-04-07 (not set) 4                 173      192
       5 2022-04-25 (not set) 1                 171      206
       6 2022-04-26 (not set) 2                 170      191
       7 2022-04-06 (not set) 3                 166      187
       8 2022-04-19 (not set) 2                 163      184
       9 2022-04-13 (not set) 3                 160      176
      10 2022-04-11 (not set) 1                 150      173
      # ... with 500 more rows

---

    Code
      top510_paged500
    Output
      # A tibble: 510 x 5
         date       city      dayOfWeek activeUsers sessions
         <date>     <chr>     <chr>           <dbl>    <dbl>
       1 2022-04-21 (not set) 4                 186      210
       2 2022-04-05 (not set) 2                 174      193
       3 2022-03-31 (not set) 4                 173      196
       4 2022-04-07 (not set) 4                 173      192
       5 2022-04-25 (not set) 1                 171      206
       6 2022-04-26 (not set) 2                 170      191
       7 2022-04-06 (not set) 3                 166      187
       8 2022-04-19 (not set) 2                 163      184
       9 2022-04-13 (not set) 3                 160      176
      10 2022-04-11 (not set) 1                 150      173
      # ... with 500 more rows

# Raw Data fetch

    # A tibble: 7 x 2
      date       sessions
      <date>        <dbl>
    1 2021-01-01        0
    2 2021-01-02        0
    3 2021-01-03        0
    4 2021-01-04        0
    5 2021-01-05        0
    6 2021-01-06        0
    7 2021-01-07        0

---

    # A tibble: 7 x 2
      date       sessions
      <date>        <dbl>
    1 2021-01-01        0
    2 2021-01-02        0
    3 2021-01-03        0
    4 2021-01-04        0
    5 2021-01-05        0
    6 2021-01-06        0
    7 2021-01-07        0

# Filter objects

    --| city 
    ----stringFilter:  
    value:  Copenhagen | matchType:  EXACT | caseSensitive:  FALSE

---

    --| city 
    ----inListFilter:  
    values:  Copenhagen London 
    caseSensitive:  TRUE 

---

    --| activeUsers 
    ----numericFilter:  
    operation:  GREATER_THAN | value:      1

---

    --| activeUsers 
    ----betweenFilter:  
    from:  1  to:  3

# Custom data

    # A tibble: 100 x 6
       date       city      dayOfWeek activeUsers sessions sessionsPerUser
       <date>     <chr>     <chr>           <dbl>    <dbl>           <dbl>
     1 2022-04-21 (not set) 4                 186      210            1.13
     2 2022-04-05 (not set) 2                 174      193            1.11
     3 2022-03-31 (not set) 4                 173      196            1.13
     4 2022-04-07 (not set) 4                 173      192            1.11
     5 2022-04-25 (not set) 1                 171      206            1.20
     6 2022-04-26 (not set) 2                 170      191            1.12
     7 2022-04-06 (not set) 3                 166      187            1.13
     8 2022-04-19 (not set) 2                 163      184            1.13
     9 2022-04-13 (not set) 3                 160      176            1.1 
    10 2022-04-11 (not set) 1                 150      173            1.15
    # ... with 90 more rows

---

    # A tibble: 100 x 6
       date       city      dayOfWeek cdow        activeUsers sessions
       <date>     <chr>     <chr>     <chr>             <dbl>    <dbl>
     1 2022-04-21 (not set) 4         (not set)/4         186      210
     2 2022-04-05 (not set) 2         (not set)/2         174      193
     3 2022-03-31 (not set) 4         (not set)/4         173      196
     4 2022-04-07 (not set) 4         (not set)/4         173      192
     5 2022-04-25 (not set) 1         (not set)/1         171      206
     6 2022-04-26 (not set) 2         (not set)/2         170      191
     7 2022-04-06 (not set) 3         (not set)/3         166      187
     8 2022-04-19 (not set) 2         (not set)/2         163      184
     9 2022-04-13 (not set) 3         (not set)/3         160      176
    10 2022-04-11 (not set) 1         (not set)/1         150      173
    # ... with 90 more rows

# Ordering DSL objects

    [[1]]
    ==GA4 OrderBy==
    Metric:        sessions 
    Descending:    TRUE 
    

---

    [[1]]
    ==GA4 OrderBy==
    Dimension:     city 
    OrderType:     ALPHANUMERIC 
    Descending:    FALSE 
    

---

    [[1]]
    ==GA4 OrderBy==
    Dimension:     city 
    OrderType:     ALPHANUMERIC 
    Descending:    FALSE 
    
    [[2]]
    ==GA4 OrderBy==
    Metric:        sessions 
    Descending:    TRUE 
    

---

    [[1]]
    ==GA4 OrderBy==
    Dimension:     city 
    OrderType:     ALPHANUMERIC 
    Descending:    FALSE 
    
    [[2]]
    ==GA4 OrderBy==
    Metric:        sessions 
    Descending:    TRUE 
    
    [[3]]
    ==GA4 OrderBy==
    Metric:        activeUsers 
    Descending:    FALSE 
    

---

    [[1]]
    ==GA4 OrderBy==
    Dimension:     dayOfWeek 
    OrderType:     NUMERIC 
    Descending:    FALSE 
    

---

    [[1]]
    ==GA4 OrderBy==
    Metric:        sessions 
    Descending:    TRUE 
    
    [[2]]
    ==GA4 OrderBy==
    Dimension:     city 
    OrderType:     ALPHANUMERIC 
    Descending:    FALSE 
    

# Order API fetch

    # A tibble: 100 x 5
       date       city      dayOfWeek activeUsers sessions
       <date>     <chr>     <chr>           <dbl>    <dbl>
     1 2022-04-21 (not set) 4                 186      210
     2 2022-04-25 (not set) 1                 171      206
     3 2022-03-31 (not set) 4                 173      196
     4 2022-04-05 (not set) 2                 174      193
     5 2022-04-07 (not set) 4                 173      192
     6 2022-04-26 (not set) 2                 170      191
     7 2022-04-06 (not set) 3                 166      187
     8 2022-04-19 (not set) 2                 163      184
     9 2022-04-13 (not set) 3                 160      176
    10 2022-04-11 (not set) 1                 150      173
    # ... with 90 more rows

# Filter fetch types

    # A tibble: 1 x 4
      date       city       dayOfWeek activeUsers
      <date>     <chr>      <chr>           <dbl>
    1 2022-04-03 Copenhagen 0                   1

---

    # A tibble: 23 x 4
       date       city   dayOfWeek activeUsers
       <date>     <chr>  <chr>           <dbl>
     1 2022-03-31 London 4                   6
     2 2022-04-05 London 2                   6
     3 2022-04-06 London 3                   6
     4 2022-04-25 London 1                   6
     5 2022-04-13 London 3                   4
     6 2022-04-22 London 5                   4
     7 2022-04-20 London 3                   3
     8 2022-04-21 London 4                   3
     9 2022-04-27 London 3                   3
    10 2022-04-04 London 1                   2
    # ... with 13 more rows

---

    # A tibble: 64 x 4
       date       city      dayOfWeek activeUsers
       <date>     <chr>     <chr>           <dbl>
     1 2022-04-21 (not set) 4                 186
     2 2022-04-05 (not set) 2                 174
     3 2022-03-31 (not set) 4                 173
     4 2022-04-07 (not set) 4                 173
     5 2022-04-25 (not set) 1                 171
     6 2022-04-26 (not set) 2                 170
     7 2022-04-06 (not set) 3                 166
     8 2022-04-19 (not set) 2                 163
     9 2022-04-13 (not set) 3                 160
    10 2022-04-11 (not set) 1                 150
    # ... with 54 more rows

---

    # A tibble: 100 x 4
       date       city      dayOfWeek activeUsers
       <date>     <chr>     <chr>           <dbl>
     1 2022-03-31 London    4                   6
     2 2022-04-04 Melbourne 1                   6
     3 2022-04-05 London    2                   6
     4 2022-04-06 London    3                   6
     5 2022-04-25 London    1                   6
     6 2022-03-31 Sao Paulo 4                   5
     7 2022-04-15 Rochester 5                   5
     8 2022-03-31 Istanbul  4                   4
     9 2022-04-01 Lugano    5                   4
    10 2022-04-05 Singapore 2                   4
    # ... with 90 more rows

