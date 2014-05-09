fun is_older (x : int * int * int, y : int * int * int) =
    if #1 x < #1 y then true
    else if #1 x > #1 y then false
    else
	if #2 x < #2 y then true
	else if #2 x > #2 y then false
	else
	    if #3 x < #3 y then true
	    else false

fun number_in_month (dateList : (int * int * int) list, month : int) =
    if null dateList then 0
    else (if #2 (hd dateList) = month then 1 else 0) + number_in_month(tl dateList, month)

fun number_in_months (dateList : (int * int * int) list, monthList : int list) =
    if null monthList then 0
    else number_in_month(dateList, hd monthList) + number_in_months(dateList, tl monthList)
 
fun dates_in_month (dateList : (int * int * int) list, month : int) =
    if null dateList then []
    else 
	if #2 (hd dateList) = month 
	then hd dateList :: dates_in_month(tl dateList, month) 
	else dates_in_month(tl dateList, month)

fun dates_in_months (dateList : (int * int * int) list, monthList : int list) =
    if null monthList then []
    else dates_in_month(dateList, hd monthList) @ dates_in_months(dateList, tl monthList)

fun get_nth (someList : 'a list, n : int) =
    if n = 1 then hd someList
    else get_nth(tl someList, n - 1)

fun date_to_string (date : int * int * int) =
    let
	val dateStringList = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(dateStringList, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, intList : int list) =
    if hd intList >= sum then 0
    else 1 + number_before_reaching_sum(sum - hd intList, tl intList)

fun what_month (n : int) =
    let
	val dayNumberList = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(n, dayNumberList) + 1
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2 then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dateList : (int * int * int) list) =
    if null dateList then NONE
    else
	let
	    val tl_oldest = oldest(tl dateList)
	in
	    if isSome tl_oldest andalso is_older(valOf tl_oldest, hd dateList) 
	    then tl_oldest 
	    else SOME(hd dateList)
	end

(*===============Challenge Problem======================*)

(*Remove the duplicate elements from the list*)
fun remove_duplicate (iList : int list) =
    if null iList then []
    else
	let
	    fun isIn (e : int, mList : int list) =
		if null mList then false
		else if e = hd mList then true
		else isIn(e, tl mList)
	in
	    if isIn(hd iList, tl iList) then remove_duplicate(tl iList)
	    else hd iList :: remove_duplicate(tl iList)
	end

fun number_in_months_challenge (dateList : (int * int * int) list, monthList : int list) =
    number_in_months(dateList, remove_duplicate(monthList))

fun dates_in_months_challenge (dateList : (int * int * int) list, monthList : int list) =
    dates_in_months(dateList, remove_duplicate(monthList))

(*Return whether the year is leap year or not*)
fun isLeapYear (iYear : int) =
    if iYear mod 400 = 0 orelse (iYear mod 4 = 0 andalso iYear mod 100 <> 0) then true
    else false

fun reasonable_date (date : int * int * int) =
    if #1 date <= 0 then false
    else if #2 date < 1 orelse #2 date > 12 then false
    else if #3 date < 1 then false
    else
	let
	    val dayNumberList = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
	    if #2 date <> 2 then #3 date <= get_nth(dayNumberList, #2 date)
	    else if isLeapYear(#1 date) then #3 date <= 29
	    else #3 date <= 28
	end
