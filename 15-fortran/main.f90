program aoc_2021_day_15
  implicit none
  type coord
    integer :: x, y
  end type
  integer :: nargs
  character (256) :: filename
  integer, allocatable, dimension(:,:) :: board
  integer, allocatable, dimension(:,:) :: bigger_board

  nargs = iargc()
  if (nargs < 1) then
    print '(a)',"Usage: ./main <file>"
  else
    call getarg(1,filename)
    call read_file(filename,board)
    print '(A,i5)',"Part 1: ",dijkstra(board)
    call generate_bigger_board(board,bigger_board)
    print '(A,i5)',"Part 2: ",dijkstra(bigger_board)
  end if
contains
  subroutine read_file(filename,board)
    implicit none
    character(len=*), intent(in) :: filename
    integer, allocatable, intent(out), dimension(:,:) :: board
    integer :: io, s
    character(len=150) :: line_buffer
    character(len=10) :: f

    open(newunit=io, file=filename, status="old", action="read")
    read(io,*) line_buffer
    s = len_trim(line_buffer)
    allocate(board(s,s))
    write(f,'(A,i3,A)')'(',s,'i1.1)'
    rewind(io)
    read(io,f) board
    close(io)
  end subroutine read_file

  subroutine generate_bigger_board(board,bigger_board)
    implicit none
    integer, dimension(:,:), intent(in) :: board
    integer, allocatable, dimension(:,:), intent(out) :: bigger_board
    integer :: m, n, s
    integer :: i, j
    allocate(bigger_board(size(board,1)*5,size(board,2)*5))
    s = size(board,1)
    do n=0,4
      do m=0,4
        do j=1,s
          do i=1,s
            bigger_board(m*s+i,n*s+j) = modulo(board(i,j) + m + n - 1,9) + 1
          end do
        end do
      end do
    end do

  end subroutine generate_bigger_board

  pure type(coord) function smallest_positive(board)
    implicit none
    integer, dimension(:,:), intent(in) :: board
    integer :: i, j
    integer :: smallest

    smallest = 1000000
    smallest_positive%x = 1
    smallest_positive%y = 1
    do j=1,size(board,2)
      do i=1,size(board,1)
        if (board(i,j) < smallest .and. board(i,j) >= 0) then
          smallest = board(i,j)
          smallest_positive%x = i
          smallest_positive%y = j
        end if
      end do
    end do
  end function smallest_positive

  pure function neighbours(c) result(cs)
    implicit none
    type(coord), intent(in) :: c
    type(coord) :: cs(4)
    cs(1)%x = c%x - 1
    cs(1)%y = c%y
    cs(2)%x = c%x + 1
    cs(2)%y = c%y
    cs(3)%x = c%x
    cs(3)%y = c%y - 1
    cs(4)%x = c%x
    cs(4)%y = c%y + 1
  end function neighbours

  pure integer function dijkstra(board)
    implicit none
    integer, dimension(:,:), intent(in) :: board
    integer, dimension(size(board,1),size(board,2)) :: paths
    integer :: i, j, candidate
    type(coord) :: cur, check(4)

    ! initialize
    do j=1,size(board,2)
      do i=1,size(board,1)
        paths(i,j) = 1000000
      end do
    end do
    paths(1,1) = 0
    cur%x = 1
    cur%y = 1

    do while (cur%x /= size(board,2) .OR. cur%y /= size(board,1))
      check = neighbours(cur)
      do i=1,4
        if (check(i)%x > 0 .AND. check(i)%y > 0 .AND. check(i)%x <= size(board,1) .AND. check(i)%y <= size(board,1)) then
          candidate = paths(cur%x,cur%y) + board(check(i)%x,check(i)%y)
          if (paths(check(i)%x,check(i)%y) > candidate) then
            paths(check(i)%x,check(i)%y) = candidate
          end if
        end if
      end do
      paths(cur%x,cur%y) = -1
      cur = smallest_positive(paths)
    end do
    dijkstra = paths(size(board,1),size(board,2))
  end function dijkstra
end program
