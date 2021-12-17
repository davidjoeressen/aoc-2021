program aoc_2021_day_15
  implicit none
  type coord
    integer :: x, y, priority
  end type coord
  type queue
    type(coord), dimension(2500000) :: heap
    integer :: last
  end type queue

  integer :: nargs
  character(len=256) :: filename
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

  pure function neighbours(c) result(cs)
    implicit none
    type(coord), intent(in) :: c
    type(coord) :: cs(4)
    integer :: i
    do i=1,size(cs)
      cs(i) = c
    end do
    cs(1)%x = cs(1)%x - 1
    cs(2)%x = cs(2)%x + 1
    cs(3)%y = cs(3)%y - 1
    cs(4)%y = cs(4)%y + 1
  end function neighbours

  subroutine init(q)
    type(queue), intent(inout) :: q
    q%last = 0
  end subroutine init

  subroutine swap(q, i, j)
    type(queue), intent(inout) :: q
    integer, intent(in) :: i, j
    type(coord) :: tmp
    if (i >= 1 .and. i <= q%last .and. j >= 1 .and. j <= q%last) then
      tmp = q%heap(i)
      q%heap(i) = q%heap(j)
      q%heap(j) = tmp
    end if
  end subroutine swap

  subroutine sink(q, i)
    type(queue), intent(inout) :: q
    integer, intent(in) :: i
    integer :: j, k
    integer :: p

    j = i
    k = j * 2
    do while (k <= q%last)
      p = q%heap(k)%priority
      if (k < q%last .and. q%heap(k+1)%priority < p) then
        k = k + 1
        p = q%heap(k+1)%priority
      end if
      if (q%heap(j)%priority <= p) then
        exit
      end if
      call swap(q,j,k)
      j = k
      k = j * 2
    end do
  end subroutine sink

  subroutine swim(q, i)
    type(queue), intent(inout) :: q
    integer, intent(in) :: i
    type(coord) :: tmp
    integer :: j, k

    j = i
    k = j / 2
    do while (k > 0 .and. q%heap(j)%priority < q%heap(k)%priority)
      call swap(q,j,k)
      j = k
      k = j / 2
    end do
  end subroutine swim

  function get(q) result(c)
    type(queue), intent(inout) :: q
    type(coord) :: c
    c = q%heap(1)
    q%heap(1) = q%heap(q%last)
    q%last = q%last - 1
    call sink(q,1)
  end function get

  subroutine insert(q, c)
    type(queue), intent(inout) :: q
    type(coord), intent(in) :: c
    q%last = q%last + 1
    q%heap(q%last) = c
    call swim(q,q%last)
  end subroutine insert

  integer function dijkstra(board)
    implicit none
    integer, dimension(:,:), intent(in) :: board
    integer, dimension(size(board,1),size(board,2)) :: paths
    integer :: i, j, candidate
    type(coord) :: cur, check(4)
    type(queue) :: q

    ! initialize
    do j=1,size(board,2)
      do i=1,size(board,1)
        paths(i,j) = 1000000
      end do
    end do
    paths(1,1) = 0
    cur%x = 1
    cur%y = 1

    do while (cur%x /= size(board,2) .or. cur%y /= size(board,1))
      check = neighbours(cur)
      do i=1,4
        if (check(i)%x > 0 .and. check(i)%y > 0 .and. check(i)%x <= size(board,1) .and. check(i)%y <= size(board,1)) then
          candidate = paths(cur%x,cur%y) + board(check(i)%x,check(i)%y)
          if (paths(check(i)%x,check(i)%y) > candidate) then
            paths(check(i)%x,check(i)%y) = candidate
            check(i)%priority = candidate
            call insert(q,check(i))
          end if
        end if
      end do
      cur = get(q)
    end do
    dijkstra = paths(size(board,1),size(board,2))
  end function dijkstra
end program
