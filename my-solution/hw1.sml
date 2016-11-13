fun is_older (a:int*int*int, b:int*int*int) =
  if #1 a < #1 b
  then true
  else
      if #1 a = #1 b
      then
	  if #2 a < #2 b
	  then true
	  else
	      if #2 a = #2 b
	      then
		  if #3 a < #3 b
		  then true
		  else false
	      else false
      else false

fun number_in_month (dates : (int*int*int) list, month : int) = 
  if null dates
  then 0
  else 
      if #2(hd(dates)) = month
      then 1 + number_in_month(tl(dates),month)
      else number_in_month(tl(dates),month)

fun number_in_months (dates : (int*int*int) list, months : int list) = 
  if null months
  then 0
  else number_in_month(dates,hd(months)) + number_in_months(dates,tl(months))

fun dates_in_month (dates : (int*int*int) list, month : int) = 
  if null dates
  then []
  else
      if #2(hd(dates)) = month
      then hd(dates) :: dates_in_month(tl dates, month)
      else dates_in_month(tl dates, month)

fun append (a : (int*int*int) list, b : (int*int*int) list) = 
  if null a 
  then b
  else
      hd(a) :: append(tl a, b)

fun dates_in_months (dates : (int*int*int) list, months : int list) = 
  if null months
  then []
  else append(dates_in_month(dates, hd(months)),dates_in_months(dates,tl(months)))		

fun get_nth (li : string list, n : int) = 
  if n = 1
  then hd(li)
  else get_nth(tl li, n-1)

fun date_to_string (date : int*int*int) = 
  let val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
  in get_nth(months, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
  end

fun number_before_reaching_sum (sum : int, li : int list) = 
  if hd(li) >= sum
  then 0
  else 1 + number_before_reaching_sum(sum - hd(li), tl li)

fun what_month (day : int) = 
  let val days = [31,28,31,30,31,30,31,31,30,31,30,31]
  in 1 + number_before_reaching_sum(day, days)
  end

fun month_range (day1 : int, day2 : int) = 
  if day1 > day2
  then []
  else
      what_month(day1) :: month_range(day1+1, day2)

fun oldest (dates : (int*int*int) list) = 
  if null dates
  then NONE
  else
      let val tl_ans = oldest(tl dates)
      in
	  if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
	  then tl_ans
	  else SOME(hd dates)
      end
