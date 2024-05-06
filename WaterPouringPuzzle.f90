module WaterPouringPuzzle
    use iso_fortran_env
    implicit none
    
contains

subroutine fillThreeLitersBucket(threeLiterBucket)
    integer, intent(out) :: threeLiterBucket(3)
    print *, "Filling three-liter bucket..."
    threeLiterBucket = 1
end subroutine fillThreeLitersBucket

subroutine fillFourLitersBucket(fourLiterBucket)
    integer, intent(out) :: fourLiterBucket(4)
    print *, "Filling four-liter bucket..."
    fourLiterBucket = 1
end subroutine fillFourLitersBucket

subroutine pourWaterFromThreeLitersBucketToFourLitersBucket(threeLiterBucket, fourLiterBucket, message)
    integer, intent(inout) :: threeLiterBucket(3)
    integer, intent(inout) :: fourLiterBucket(4)
    character(len=100), intent(out) :: message
    
    integer :: waterToTransfer, availableSpaceInFourLiterBucket
    integer :: i, j

    print *, "Pouring water from three-liter bucket to four-liter bucket..."

    waterToTransfer = sum(threeLiterBucket)
    availableSpaceInFourLiterBucket = 4 - sum(fourLiterBucket)

    do while (waterToTransfer > 0 .and. availableSpaceInFourLiterBucket > 0)
        i = 1
        do while (fourLiterBucket(i) == 1 .and. i <= 4)
            i = i + 1
        end do
        fourLiterBucket(i) = 1
        availableSpaceInFourLiterBucket = availableSpaceInFourLiterBucket - 1
        j = 3
        do while (threeLiterBucket(j) == 0 .and. j <= 3)
            j = j - 1
        end do
        threeLiterBucket(j) = 0
        waterToTransfer = waterToTransfer - 1
    end do

    if (waterToTransfer == 0) then
        message = "Water poured from three-liter bucket to four-liter bucket"
    else
        message = "Four-liter bucket is full"
    end if
end subroutine pourWaterFromThreeLitersBucketToFourLitersBucket

subroutine pourWaterFromFourLitersBucketToThreeLitersBucket(threeLiterBucket, fourLiterBucket, message)
    integer, intent(inout) :: threeLiterBucket(3)
    integer, intent(inout) :: fourLiterBucket(4)
    character(len=100), intent(out) :: message
    
    integer :: waterToTransfer, availableSpaceInThreeLiterBucket
    integer :: i, j

    print *, "Pouring water from four-liter bucket to three-liter bucket..."

    waterToTransfer = sum(fourLiterBucket)
    availableSpaceInThreeLiterBucket = 3 - sum(threeLiterBucket)

    do while (waterToTransfer > 0 .and. availableSpaceInThreeLiterBucket > 0)
        i = 1
        do while (threeLiterBucket(i) == 1 .and. i <= 3)
            i = i + 1
        end do
        threeLiterBucket(i) = 1
        availableSpaceInThreeLiterBucket = availableSpaceInThreeLiterBucket - 1
        j = 4
        do while (fourLiterBucket(j) == 0 .and. j <= 4)
            j = j - 1
        end do
        fourLiterBucket(j) = 0
        waterToTransfer = waterToTransfer - 1
    end do

    if (waterToTransfer == 0) then
        message = "Water poured from four-liter bucket to three-liter bucket"
    else
        message = "Three-liter bucket is full"
    end if
end subroutine pourWaterFromFourLitersBucketToThreeLitersBucket


subroutine emptyThreeLitersBucket(threeLiterBucket, message)
    integer, intent(inout) :: threeLiterBucket(3)
    character(len=100), intent(out) :: message
    print *, "Emptying three-liter bucket..."
    threeLiterBucket = 0
    message = "Three-liter bucket emptied"
end subroutine emptyThreeLitersBucket

subroutine emptyFourLitersBucket(fourLiterBucket, message)
    integer, intent(inout) :: fourLiterBucket(4)
    character(len=100), intent(out) :: message
    print *, "Emptying four-liter bucket..."
    fourLiterBucket = 0
    message = "Four-liter bucket emptied"
end subroutine emptyFourLitersBucket

subroutine printBuckets(threeLiterBucket, fourLiterBucket, size1, size2)
    integer, intent(in) :: threeLiterBucket(:), fourLiterBucket(:)
    integer, intent(in) :: size1, size2

    integer :: i
    integer :: max_height

    
    print *, "        "
    print *, "        "

    ! Determine the maximum height of the buckets
    max_height = max(size1, size2)

    do i = max_height, 1, -1
        ! Print contents of fourLiterBucket
        if (i <= size2) then
            if (fourLiterBucket(i) == 1) then
                write(*, "('|######|      ')", advance='no')
            else
                write(*, "('|      |      ')", advance='no')
            end if
        else
            write(*, "('|      |      ')", advance='no')
        end if

        ! Print contents of threeLiterBucket
        if (i <= size1) then
            if (threeLiterBucket(i) == 1) then
                write(*, "('|######|')", advance='yes')
            else
                write(*, "('|      |')", advance='yes')
            end if
        else
            write(*, "('      ')", advance='yes')
        end if
    end do

    ! Print the bottom line to separate the buckets
    write(*, "('|______|      |______|')", advance='yes')
   
    write(*, "('____________________________________')", advance='yes')
    write(*, "('   4L            3L   ')", advance='yes')


    print *, "        "
    print *, "        "
    print *, "        "
    print *, "        "

end subroutine printBuckets






end module WaterPouringPuzzle


program main
    use WaterPouringPuzzle
    implicit none
    integer :: threeLiterBucket(3) = 0
    integer :: fourLiterBucket(4) = 0
    character(len=100) :: message
    integer :: choice
    logical :: won = .false.

    print *, "_________________________________________________________________________________________"

    print *, "  "
    print *, "  "
    print *, "  "
    print *, "  "

    do while (.not. won)
        print *, "Water Pouring Puzzle"
        print *, "1. Fill three-liter bucket"
        print *, "2. Fill four-liter bucket"
        print *, "3. Pour water from three-liter bucket to four-liter bucket"
        print *, "4. Pour water from four-liter bucket to three-liter bucket"
        print *, "5. Empty three-liter bucket"
        print *, "6. Empty four-liter bucket"
        print *, "7. Show solution"
        write(*, "('Enter your choice (1-7): ', I0)", advance='no')
    read(*, *) choice

        select case (choice)
        case (1)
            call fillThreeLitersBucket(threeLiterBucket)
            call printBuckets(threeLiterBucket, fourLiterBucket, 3, 4)
        case (2)
            call fillFourLitersBucket(fourLiterBucket)
            call printBuckets(threeLiterBucket, fourLiterBucket, 3, 4)
        case (3)
            call pourWaterFromThreeLitersBucketToFourLitersBucket(threeLiterBucket, fourLiterBucket, message)
            call printBuckets(threeLiterBucket, fourLiterBucket, 3, 4)
            print *, message
        case (4)
            call pourWaterFromFourLitersBucketToThreeLitersBucket(threeLiterBucket, fourLiterBucket, message)
            call printBuckets(threeLiterBucket, fourLiterBucket, 3, 4)
            print *, message
        case (5)
            call emptyThreeLitersBucket(threeLiterBucket, message)
            call printBuckets(threeLiterBucket, fourLiterBucket, 3, 4)
            print *, message
        case (6)
            call emptyFourLitersBucket(fourLiterBucket, message)
            call printBuckets(threeLiterBucket, fourLiterBucket, 3, 4)
            print *, message
        case (7)
            print *, "Step-by-step solution:"

            ! Step 1
            call fillThreeLitersBucket(threeLiterBucket)
            call printBuckets(threeLiterBucket, fourLiterBucket, 3, 4)

            ! Step 2
            call pourWaterFromThreeLitersBucketToFourLitersBucket(threeLiterBucket, fourLiterBucket, message)
            call printBuckets(threeLiterBucket, fourLiterBucket, 3, 4)

            ! Step 3
            call fillThreeLitersBucket(threeLiterBucket)
            call printBuckets(threeLiterBucket, fourLiterBucket, 3, 4)

            ! Step 4
            call pourWaterFromThreeLitersBucketToFourLitersBucket(threeLiterBucket, fourLiterBucket, message)
            call printBuckets(threeLiterBucket, fourLiterBucket, 3, 4)

            ! Step 5
            call emptyFourLitersBucket(fourLiterBucket, message)
            call printBuckets(threeLiterBucket, fourLiterBucket, 3, 4)

            ! Step 6
            call pourWaterFromThreeLitersBucketToFourLitersBucket(threeLiterBucket, fourLiterBucket, message)
            call printBuckets(threeLiterBucket, fourLiterBucket, 3, 4)

        case default
            print *, "Invalid choice. Please try again."
        end select

        ! Check if the goal is achieved
        if (sum(fourLiterBucket) == 2 .and. sum(threeLiterBucket) == 0) then
            print *, "You won!"
            won = .true.
        end if
    end do

end program main