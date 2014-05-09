use "hw1.sml";

val dateList = [(2000, 1, 1), (2000, 1, 31), (2000, 2, 1), (2000, 2, 29), (2000, 2, 28), (1999, 1, 1), (1999, 2, 2), (1999, 3, 3)]

val monthStringList = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

val dayNumberList = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun runTest () =
    if (is_older((1987,9,27), (1989,3,14)) = true
	andalso number_in_month(dateList, 1) = 3
	andalso number_in_months(dateList, [1,3]) = 4
	andalso dates_in_month(dateList, 2) = [(2000, 2, 1), (2000, 2, 29), (2000, 2, 28), (1999, 2, 2)]
	andalso dates_in_months(dateList, [1,3]) = [(2000, 1, 1), (2000, 1, 31), (1999, 1, 1), (1999, 3, 3)]
	andalso get_nth(monthStringList, 9) = "September"
	andalso date_to_string((1987,9,27)) = "September 27, 1987"
	andalso number_before_reaching_sum(59, dayNumberList) = 1
	andalso what_month(59) = 2
	andalso month_range(58, 61) = [2, 2, 3, 3]
	andalso oldest(dateList) = SOME((1999, 1, 1))
	andalso number_in_months_challenge(dateList, [1,3,3,1,1,3,3,3]) = 4
	andalso dates_in_months_challenge(dateList, [3,1,1,3,3,1,1,3]) = [(2000, 1, 1), (2000, 1, 31), (1999, 1, 1), (1999, 3, 3)]
	andalso reasonable_date((1900, 2, 29)) = false
       ) then print "All Passed!\n"
    else print "Somewhere Failed!\n"
