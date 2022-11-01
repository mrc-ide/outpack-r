Measuring time to get the index
-------------------------------

### Opening an outpack root for the first time

    root <- outpack_root_open("montagu-reports")
    system.time(root$index(skip_cache=TRUE))

    ##    user  system elapsed 
    ##   3.077   0.000   3.077

### Adding a packet and then opening the root again

    root <- outpack_root_open("montagu-reports")
    system.time(create_random_packet(root))

    ##    user  system elapsed 
    ##   0.857   0.000   0.836

    system.time(root$index())

    ##    user  system elapsed 
    ##   0.027   0.000   0.027
