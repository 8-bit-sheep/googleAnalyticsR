# Basic fetch

    # A tibble: 100 x 3
       date       city      activeUsers
       <date>     <chr>           <dbl>
     1 2020-04-08 (not set)          18
     2 2020-04-08 Rome               12
     3 2020-04-15 (not set)           9
     4 2020-04-27 (not set)           9
     5 2020-04-09 (not set)           8
     6 2020-04-14 (not set)           8
     7 2020-04-22 (not set)           8
     8 2020-03-31 (not set)           7
     9 2020-04-08 Bologna             7
    10 2020-04-07 (not set)           6
    # ... with 90 more rows

# Raw Data fetch

    # A tibble: 340 x 2
       date       sessions
       <date>        <dbl>
     1 2020-01-08      134
     2 2020-01-09      131
     3 2020-01-10      124
     4 2020-01-11       34
     5 2020-01-12       62
     6 2020-01-13      127
     7 2020-01-14      134
     8 2020-01-15      106
     9 2020-01-16      121
    10 2020-01-17       86
    # ... with 330 more rows

---

    # A tibble: 340 x 2
       date       sessions
       <date>        <dbl>
     1 2020-01-08      134
     2 2020-01-09      131
     3 2020-01-10      124
     4 2020-01-11       34
     5 2020-01-12       62
     6 2020-01-13      127
     7 2020-01-14      134
     8 2020-01-15      106
     9 2020-01-16      121
    10 2020-01-17       86
    # ... with 330 more rows

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

---

    --| city 
    ----nullFilter:  TRUE 

# Custom data

    # A tibble: 100 x 6
       date       city      dayOfWeek activeUsers sessions sessionsPerUser
       <date>     <chr>     <chr>           <dbl>    <dbl>           <dbl>
     1 2020-04-08 (not set) 4                  18       21            1.17
     2 2020-04-08 Rome      4                  12       14            1.17
     3 2020-04-15 (not set) 4                   9       11            1.22
     4 2020-04-27 (not set) 2                   9       11            1.22
     5 2020-04-09 (not set) 5                   8       10            1.25
     6 2020-04-14 (not set) 3                   8        8            1   
     7 2020-04-22 (not set) 4                   8        9            1.12
     8 2020-03-31 (not set) 3                   7       10            1.43
     9 2020-04-08 Bologna   4                   7        7            1   
    10 2020-04-07 (not set) 3                   6        7            1.17
    # ... with 90 more rows

---

    # A tibble: 100 x 6
       date       city      dayOfWeek cdow        activeUsers sessions
       <date>     <chr>     <chr>     <chr>             <dbl>    <dbl>
     1 2020-04-08 (not set) 4         (not set)/4          18       21
     2 2020-04-08 Rome      4         Rome/4               12       14
     3 2020-04-15 (not set) 4         (not set)/4           9       11
     4 2020-04-27 (not set) 2         (not set)/2           9       11
     5 2020-04-09 (not set) 5         (not set)/5           8       10
     6 2020-04-14 (not set) 3         (not set)/3           8        8
     7 2020-04-22 (not set) 4         (not set)/4           8        9
     8 2020-03-31 (not set) 3         (not set)/3           7       10
     9 2020-04-08 Bologna   4         Bologna/4             7        7
    10 2020-04-07 (not set) 3         (not set)/3           6        7
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
       date       city           dayOfWeek activeUsers sessions
       <date>     <chr>          <chr>           <dbl>    <dbl>
     1 2020-04-08 (not set)      4                  18       21
     2 2020-04-08 Rome           4                  12       14
     3 2020-04-20 London         2                   6       14
     4 2020-04-09 Warsaw         5                   5       11
     5 2020-04-15 (not set)      4                   9       11
     6 2020-04-21 Amsterdam      3                   3       11
     7 2020-04-27 (not set)      2                   9       11
     8 2020-04-09 (not set)      5                   8       10
     9 2020-03-31 (not set)      3                   7       10
    10 2020-04-24 Rio de Janeiro 6                   2        9
    # ... with 90 more rows

# Filter fetch types

    # A tibble: 17 x 4
       date       city       dayOfWeek activeUsers
       <date>     <chr>      <chr>           <dbl>
     1 2020-04-16 Copenhagen 5                   3
     2 2020-04-10 Copenhagen 6                   2
     3 2020-04-15 Copenhagen 4                   2
     4 2020-04-17 Copenhagen 6                   2
     5 2020-04-22 Copenhagen 4                   2
     6 2020-04-23 Copenhagen 5                   2
     7 2020-04-25 Copenhagen 7                   2
     8 2020-04-01 Copenhagen 4                   1
     9 2020-04-04 Copenhagen 7                   1
    10 2020-04-06 Copenhagen 2                   1
    11 2020-04-12 Copenhagen 1                   1
    12 2020-04-18 Copenhagen 7                   1
    13 2020-04-20 Copenhagen 2                   1
    14 2020-04-21 Copenhagen 3                   1
    15 2020-04-24 Copenhagen 6                   1
    16 2020-04-26 Copenhagen 1                   1
    17 2020-04-27 Copenhagen 2                   1

---

    # A tibble: 45 x 4
       date       city   dayOfWeek activeUsers
       <date>     <chr>  <chr>           <dbl>
     1 2020-04-09 London 5                   6
     2 2020-04-20 London 2                   6
     3 2020-04-27 London 2                   6
     4 2020-04-14 London 3                   5
     5 2020-04-16 London 5                   5
     6 2020-04-17 London 6                   5
     7 2020-04-24 London 6                   5
     8 2020-04-26 London 1                   5
     9 2020-03-31 London 3                   4
    10 2020-04-13 London 2                   4
    # ... with 35 more rows

---

    # A tibble: 91 x 4
       date       city      dayOfWeek activeUsers
       <date>     <chr>     <chr>           <dbl>
     1 2020-04-08 (not set) 4                  18
     2 2020-04-08 Rome      4                  12
     3 2020-04-15 (not set) 4                   9
     4 2020-04-27 (not set) 2                   9
     5 2020-04-09 (not set) 5                   8
     6 2020-04-14 (not set) 3                   8
     7 2020-04-22 (not set) 4                   8
     8 2020-03-31 (not set) 3                   7
     9 2020-04-08 Bologna   4                   7
    10 2020-04-07 (not set) 3                   6
    # ... with 81 more rows

---

    # A tibble: 100 x 4
       date       city      dayOfWeek activeUsers
       <date>     <chr>     <chr>           <dbl>
     1 2020-04-07 (not set) 3                   6
     2 2020-04-08 Milan     4                   6
     3 2020-04-09 London    5                   6
     4 2020-04-20 London    2                   6
     5 2020-04-23 Sao Paulo 5                   6
     6 2020-04-24 (not set) 6                   6
     7 2020-04-27 Bengaluru 2                   6
     8 2020-04-27 London    2                   6
     9 2020-04-02 (not set) 5                   5
    10 2020-04-03 (not set) 6                   5
    # ... with 90 more rows

