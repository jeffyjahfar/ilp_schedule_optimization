Imports System.IO
Imports System.Math

Module Module1
    Function findrowmin(ByVal rowarray As Single(), ByVal arraysize As Integer) As Single
        ' local variable declaration */
        Dim result As Single
        result = rowarray(0)
        Dim i As Integer = 0
        For i = 1 To arraysize - 1
            If (result > rowarray(i)) Then
                result = rowarray(i)
            End If
        Next
        findrowmin = result
    End Function

    Function GetRow(ByVal matrix As Single(,), ByVal row_number As Integer, ByVal matrixSize As Integer) As Single()
        'get the number of columns of your matrix
        'define empty array, at the end of the 'for' cycle it will contain requested row's values  
        Dim values As Single() = Nothing
        For i As Integer = 0 To matrixSize - 1
            'Resize array
            ReDim Preserve values(i)
            'Populate array element
            values(i) = matrix(row_number, i)
        Next
        Return values
    End Function

    Function FindNextPos(ByRef ExpectedDurations As Single(), ByVal NoOfLocations As Integer, ByVal CurrentDuration As Single, ByRef LocationsVisited As Boolean(), ByVal CurrentLocation As Integer, ByRef TravelTimes(,) As Single) As Integer
        Dim result As Single
        Dim pos As Integer = -1
        Dim i As Integer = 1
        For i = 0 To NoOfLocations - 1
            If (Not LocationsVisited(i)) Then
                result = ExpectedDurations(i) + TravelTimes(CurrentLocation, i)
                pos = i
                Exit For
            End If
        Next

        For i = 1 To NoOfLocations - 1
            If (Not LocationsVisited(i)) Then
                If (result >= ExpectedDurations(i) + TravelTimes(CurrentLocation, i)) Then
                    'If (ExpectedDurations(i) >= CurrentDuration) Then
                    result = ExpectedDurations(i) + TravelTimes(CurrentLocation, i)
                    pos = i
                    'End If
                End If
            End If
        Next
        'Console.Write("Next location selected: ")
        'Console.WriteLine(pos)
        FindNextPos = pos
    End Function

    Function CheckIfAllVisited(ByVal NoOfLocations As Integer, ByRef LocationsVisited As Boolean()) As Boolean
        Dim i As Integer = 0
        Dim flag As Boolean = True
        For i = 1 To NoOfLocations - 1
            flag = flag And LocationsVisited(i)
        Next
        CheckIfAllVisited = flag
    End Function
    Function UnusedTimeCalculator(ByVal NoOfLocations As Integer, ByRef WorkedHoursSoFar As Single(), ByVal DaysSoFar As Integer) As Single
        Dim i As Integer = 0
        Dim total As Single = 0
        For i = 0 To DaysSoFar - 2
            total += WorkedHoursSoFar(i)
        Next
        UnusedTimeCalculator = (DaysSoFar - 1) * 360 - total
    End Function
    Function MinimumDaysRequired(ByVal NoOfLocations As Integer, ByRef ExpectedDurations As Single()) As Integer
        Dim i As Integer = 0
        Dim total As Single = 0
        For i = 0 To NoOfLocations - 1
            total += ExpectedDurations(i)
        Next
        MinimumDaysRequired = total / 360
    End Function
    Sub Main()
        Console.WriteLine("Integer Programming Formulation for Drainage Operations Schedule Optimization")
        Console.WriteLine()

        ''''''''''''''''''''''''''''''''
        'Initiation Part of the Algorithm
        ''''''''''''''''''''''''''''''''

        Dim TravelTimes(100, 100) As Single
        Dim ExpectedDurations(100) As Single
        Dim NoOfLocations As Integer
        Dim LocationIDs(100) As Integer
        Dim Latitudes(100) As Single
        Dim Longitudes(100) As Single
        'Dim RowSum(100), ColumnSum(100) As Integer
        'Dim RowFlag(100), ColumnFlag(100) As Boolean
        Dim WorkedHoursSoFar(30) As Single
        'Dim RowMinimum As Single
        Dim lines As String
        'Dim DayWiseMatrix(100, 100, 30) As Integer
        Dim FinalOptimumSchedule(100, 30) As Integer
        Dim WorkEnded As Boolean
        Dim NextLocation As Integer
        Dim CurrentLocation As Integer
        Dim LocationsCovered As Integer = 0
        Dim LocationsCoveredOnEachDay(30) As Integer
        Dim LocationVisted(100) As Boolean
        Dim DaysSoFar As Integer = 0
        Dim CurrentMin As Single
        Dim InputFile As StreamReader = New StreamReader("sample_data.txt")
        Dim data(101) As String
        Dim j As Integer = 1
        Dim i As Integer
        Dim CurrentWorkedHours As Single
        Dim TotalTravelTimeSoFar As Single = 0

        Const MAXTIME As Single = 1000
        Const ShiftHourLength As Single = 360

        NoOfLocations = 100
        'RowMinimum = MAXTIME
        WorkEnded = False


        For i = 0 To 100
            'RowSum(i) = 0
            'ColumnSum(i) = 0
            'RowFlag(i) = True
            'ColumnFlag(i) = True
            'LocationIDs(i) = -100
            LocationVisted(i) = False
        Next

        For i = 0 To 30
            WorkedHoursSoFar(i) = 0
            LocationsCoveredOnEachDay(i) = -1
        Next

        'Read from the input file


        For i = 0 To 3
            lines = InputFile.ReadLine()
            data = lines.Split(" ")
            NoOfLocations = data.Length - 1
            If (i = 0) Then
                For j = 1 To data.Length - 1
                    LocationIDs(j - 1) = Convert.ToInt16(data(j))
                Next
            End If
            If (i = 1) Then
                For j = 1 To data.Length - 1
                    Latitudes(j - 1) = Convert.ToSingle(data(j))
                Next
            End If
            If (i = 2) Then
                For j = 1 To data.Length - 1
                    Longitudes(j - 1) = Convert.ToSingle(data(j))
                Next
            End If
            If (i = 3) Then
                For j = 1 To data.Length - 1
                    ExpectedDurations(j - 1) = Convert.ToSingle(data(j))
                Next
            End If
        Next

        'Output the arrays

        'Console.WriteLine()
        'Console.WriteLine("Location IDs")
        'For j = 0 To NoOfLocations - 1
        '    Console.WriteLine(LocationIDs(j))
        'Next
        'Console.WriteLine()
        'Console.WriteLine("Latitudes")

        'For j = 0 To NoOfLocations - 1
        '    Console.WriteLine(Latitudes(j))
        'Next
        'Console.WriteLine()
        'Console.WriteLine("Longitudes")

        'For j = 0 To NoOfLocations - 1
        '    Console.WriteLine(Longitudes(j))
        'Next
        'Console.WriteLine()
        'Console.WriteLine("Expected Durations")

        'For j = 0 To NoOfLocations - 1
        '    Console.WriteLine(ExpectedDurations(j))
        'Next
        'Console.WriteLine()

        'Making the Travel time matrix
        For i = 0 To NoOfLocations - 1
            For j = 0 To NoOfLocations - 1
                If (i = j) Then
                    TravelTimes(i, j) = MAXTIME
                Else
                    TravelTimes(i, j) = Sqrt(Pow(Longitudes(i) - Longitudes(j), 2) + Pow(Latitudes(i) - Latitudes(j), 2))
                End If
            Next
        Next

        'Print the travel times matrix
        'For i = 0 To NoOfLocations - 1
        'For j = 0 To NoOfLocations - 1
        'Console.Write(TravelTimes(i, j))
        'Console.Write(" ")
        'Next
        'Console.WriteLine()
        'Next

        'Check FindRowMin Function
        'For i = 0 To NoOfLocations - 1
        'Console.WriteLine(findrowmin(GetRow(TravelTimes, i, NoOfLocations), NoOfLocations))
        'Next

        ''''''''''''''''''''''''''''''''
        'Iteration Part of the Algorithm
        ''''''''''''''''''''''''''''''''
        Console.WriteLine("Iteration Part of the Algorithm")
        Console.Write("Minimum number of days required: ")
        Console.WriteLine(MinimumDaysRequired(NoOfLocations, ExpectedDurations))

        Do While ((Not CheckIfAllVisited(NoOfLocations, LocationVisted)) And (DaysSoFar < 30))
            'Console.WriteLine("Daywise Loop started")
            'Dim LocationsCoveredToday As Integer = -1
            'Console.WriteLine(DaysSoFar)
            Do While ((WorkedHoursSoFar(DaysSoFar) < ShiftHourLength) And (Not CheckIfAllVisited(NoOfLocations, LocationVisted)))
                'Console.WriteLine("Hourwise Loop started")
                If (LocationsCoveredOnEachDay(DaysSoFar) = -1) Then
                    If (DaysSoFar = 0 And LocationsCoveredOnEachDay(DaysSoFar) = -1) Then
                        LocationsCovered += 1

                        'Else


                        'Console.WriteLine("-1 case worked")
                    End If
                    LocationsCoveredOnEachDay(DaysSoFar) = 0
                    NextLocation = LocationIDs(0)
                    FinalOptimumSchedule(DaysSoFar, LocationsCoveredOnEachDay(DaysSoFar)) = NextLocation
                    LocationVisted(NextLocation) = True


                Else
                    Console.WriteLine("other case worked")
                    'Dim CurrentRowMin As Single = FindRowMin(GetRow(TravelTimes, NextLocation, NoOfLocations), NoOfLocations)

                    Console.Write("Locations Covered Today: ")
                    Console.WriteLine(LocationsCoveredOnEachDay(DaysSoFar))
                    'NextLocation = FindNextPos1(ExpectedDurations, NoOfLocations, CurrentMin, LocationVisted, TravelTimes, CurrentLocation)
                    CurrentLocation = NextLocation
                    'Finding Next Location
                    CurrentMin = ExpectedDurations(CurrentLocation)
                    NextLocation = LocationIDs(FindNextPos(ExpectedDurations, NoOfLocations, CurrentMin, LocationVisted, CurrentLocation, TravelTimes))

                    CurrentWorkedHours = WorkedHoursSoFar(DaysSoFar)
                    'WorkedHoursSoFar(DaysSoFar) += (ExpectedDurations(NextLocation) + TravelTimes(CurrentLocation, NextLocation))
                    If ((CurrentWorkedHours + ExpectedDurations(NextLocation) + TravelTimes(CurrentLocation, NextLocation)) <= ShiftHourLength) Then
                        WorkedHoursSoFar(DaysSoFar) += (ExpectedDurations(NextLocation) + TravelTimes(CurrentLocation, NextLocation))
                        LocationsCovered += 1
                        LocationsCoveredOnEachDay(DaysSoFar) += 1
                        FinalOptimumSchedule(DaysSoFar, LocationsCoveredOnEachDay(DaysSoFar)) = NextLocation
                        Console.Write("Visited Location:")
                        Console.WriteLine(FinalOptimumSchedule(DaysSoFar, LocationsCoveredOnEachDay(DaysSoFar)))
                        LocationVisted(NextLocation) = True


                    Else
                        '    FinalOptimumSchedule(DaysSoFar, LocationsCoveredOnEachDay(DaysSoFar)) = LocationIDs(0)
                        LocationVisted(NextLocation) = False
                        NextLocation = CurrentLocation
                        LocationsCoveredOnEachDay(DaysSoFar) += 1
                        FinalOptimumSchedule(DaysSoFar, LocationsCoveredOnEachDay(DaysSoFar)) = LocationIDs(0)
                        DaysSoFar += 1

                        LocationVisted(LocationIDs(0)) = False
                        '    WorkedHoursSoFar(DaysSoFar) = CurrentWorkedHours
                        Exit Do
                    End If

                End If
            Loop
            'LocationsCoveredOnEachDay(DaysSoFar) += 1
            'FinalOptimumSchedule(DaysSoFar, LocationsCoveredOnEachDay(DaysSoFar)) = LocationIDs(0)
            'DaysSoFar += 1
            'LocationVisted(LocationIDs(0)) = False
            Console.WriteLine("Day over")
        Loop

        ''''''''''''''''''''''''''''''''
        'Output the Final Sequence
        ''''''''''''''''''''''''''''''''
        Console.WriteLine("Output the Final Sequence")
        Console.Write("Days so far :")
        Console.WriteLine(DaysSoFar)

        For i = 0 To DaysSoFar - 1
            Console.Write("Day ")
            Console.WriteLine(i + 1)
            For j = 0 To LocationsCoveredOnEachDay(i)
                If (j <> 0) Then
                    TotalTravelTimeSoFar += TravelTimes(j - 1, j)
                End If

                Console.Write(FinalOptimumSchedule(i, j))
                Console.Write(" ")
            Next
            Console.WriteLine()
        Next

        Console.Write("Total Unused Time: ")
        'Console.WriteLine(UnusedTimeCalculator(NoOfLocations, WorkedHoursSoFar, DaysSoFar))
        Console.Write(UnusedTimeCalculator(NoOfLocations, WorkedHoursSoFar, DaysSoFar) / 60)
        Console.WriteLine(" Hours")

        Console.Write("Total Travel Time: ")
        'Console.WriteLine(TotalTravelTimeSoFar )
        Console.Write(TotalTravelTimeSoFar / 60)
        Console.WriteLine(" Hours")

        Console.ReadKey()
    End Sub

End Module
