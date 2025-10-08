! Funkcja do obliczania powierzchni łopatki
function A(x_1,y_1,x_2,y_2,x_3,y_3) 
  
  ! Zmienne liczbowe potrzebne do wyliczenia powierzchni
  real :: x_1, y_1, x_2, y_2, x_3, y_3, A 
  
  ! Składowa funkcji do obliczenia powierzchni
  A = abs(x_1*y_2+y_1*x_3+y_3*x_2-y_2*x_3-y_1*x_2-x_1*y_3)/2 
 
end function

program Algorytmlopatki
  implicit none
   
   ! Zmienne liczbowe do porowadzenia obliczeń w pętlach
    integer i,ierror, j 
   
    ! Zdefiniowanie tablic do współrzędnych po stornie ssącej (S) i ciśnieniowej (P)
    real, dimension(52) :: XS, YS, XP, YP 

   ! Definicja zmiennych liczbowych 
    real :: D, R, Pi, z, t, R_TE, R_LE,  o, c_x, c_t
    real :: B_in, Bout, B_out, e_in, e_out,S_dzeta, B_1, B_2, B_3, B_4, B_5
    real :: x_1, y_1, x_2, y_2, x_3, y_3, x_4, y_4, x_5, y_5
    real :: x_6, y_6, x_7, y_7, x_8, y_8, x_9, y_9, x_0, y_0, x_01, x_02
    real ::  R_0, YY2, s,x_cg, y_cg, A1, A2, A, Area, Tmax, Tm, n
    real :: DS, CS, BS, AS, DP, CP, BP, AP, DXP, DXS
    real :: Stagger, Zweifel, Sol, Pcti, Pcte, Thetac, CL, Xinfl
    character :: typ, PrzeliczPro, CieciwaZada

   ! Stałe zdefinowane
    Pi = 3.14 ! Liczba Pi
    D = 980 ! Średnica wirnika w mm
    R = D/2 ! Promień wirnika w mm
    z = 190 ! Liczba łopatek
    typ = "W" ! Tylko jaki ma być profil wygneroweny (K - kierownica, W - wirnika, przy innych oznaczeniach nic nie wygeneruje)
    PrzeliczPro = "N" ! Czy program ma przliczyć promienie krawędzi spływowej i wiodącej czy zostawić podaną wartość (T - tak, N - nie)
    CieciwaZada = "T" ! Wartość T - tak oznacza, że cięciwa zostałą zadana i na niej porgram ma oprzetć model łopatki, przy wartości N - nie, cięicwa zostanie wyliczona na końcu

    B_in = 23 ! Kąt wlotowy z łopatki w stopniach
    
    if (typ == "K") then

      B_in = B_in

    else if (typ == "W") then

      B_in = 90-B_in

    end if 

    Bout = 19.7 ! Kąt wylotowy z łopatki w stopniach
    B_out = Bout-90 ! Kąt wylotowy łopatki w stopniach (wartość potrzeba ujemna potrzebna ze względów przyjętego algorytmu, wartość musi być ujemna)

    S_dzeta = 5 ! Dzeta, kąt zakrętu profilu łopatki (unguided turning) w stopniach
    e_in = 5 ! Kąt półklinowy na wlocie/na krawędzi natarcia (half wedge angle) w stopniach (wartość maksymalna około 25 st.)
    
    R_LE = 0.5 ! Promień koła krawędzi wiodącej/natarcia (the trailing edge circle) w mm 
    R_TE = 0.3 ! Promień koła krawędzi spływowej (the leading edge circle) w mm 
    
    c_x = 0 ! Cięciwa osiowa łopatki w mm (podanie wartości 0 sprawi, że program ze wzoru obliczy wartość)
    o = 0 ! Szerokość gardła w mm (podanie wartości 0 sprawi, że program ze wzoru obliczy wartość)
    c_t = 0 ! Cięciwa styczna łopatki w mm (podanie wartości 0 sprawi, że program ze wzoru obliczy wartość) 
    s = 25 ! Cięciwa łopatki, dla podanej wartości program wyznaczy łopatkę, a przy wartości 0 wyliczy ją  

  do while (n < 1)
   ! Stałe wynikowe
    t = 2*Pi*R/z ! Podziałka
    e_out = 0.5*S_dzeta ! Kąt półklinowy wylotowy/na krawędzi spływowej (exit wedge angle) w stopniach
     
    B_1 = B_out - e_out ! Kąt w punkcie 1, w stponiach
    B_2 = B_out - e_out + S_dzeta ! Kąt w punkcie 2, w stponiach
    B_3 = B_in + e_in ! Kąt w punkcie 3, w stponiach   
    B_4 = B_in - e_in ! Kąt w punkcie 4, w stponiach
    B_5 = B_out + e_out ! Kąt w punkcie 5, w stponiach

   ! Funkcje/wozry do danych wartości jeśli nie zostały podane
    if ( S_dzeta <= 0 ) then
      S_dzeta = 0.01
    end if

    if (PrzeliczPro == "T") then
      if ( R_TE >= 2.0 ) then
        R_TE = R_TE/100*2*Pi*R/z*cos(B_out*Pi/180)/2
      end if

      if ( R_LE >= 2.0 ) then
        R_LE = R_LE/100*2*Pi*R/z*cos(B_in*Pi/180)/2
      end if
    else if (PrzeliczPro == "N") then 
      R_LE = R_LE
      R_TE = R_TE
    end if
    
    if ( o <= 0 ) then
      o = t*cos(B_out*Pi/180)-2*R_TE
    end if
    
    if ( c_x <= 0 ) then
      c_x = 4*Pi*R/0.8/z*sin((B_in-B_out)*Pi/180)*cos(B_out*Pi/180)/cos(B_in*Pi/180)
    end if

   ! Wyznczanie punktów chaakterystycznych łopatki
    x_1 = c_x - R_TE*(1+sin(B_1*Pi/180)) ! Współrzędne x punktu 1
    y_1 =  R_TE*cos(B_1*Pi/180) ! Współrzędne y punktu 1

    x_2 = c_x - R_TE + ((o+R_TE)*sin(B_2*Pi/180)) ! Współrzędne x punktu 2 
    y_2 = t - ((o+R_TE)*cos(B_2*Pi/180)) ! Współrzędne y punktu 2 

    x_3 = R_LE*(1-sin(B_3*Pi/180)) ! Współrzędne x punktu 3    
       
   ! Funkcje/wozry do danych wartości jeśli nie zostały podane
    
    if (CieciwaZada == "T") then
        
     c_t =  sqrt(s*s - c_x*c_x) ! Dla zadanej cięciwy zostanie wyznaczoana odpowiednia cięciwa styczna

    else if (CieciwaZada == "N") then ! Jeśli nie została zadana cięciwa to zostania ona wyliczona na podsatawy zmiennych wynikowych

     if ( c_t <= 0 ) then
      c_t = y_2+180/Pi*(x_2-x_3)/(B_2-B_3)*log(cos(B_2*Pi/180)/cos(B_3*Pi/180))-R_LE*cos(B_3*Pi/180)
     end if
 
     if ( c_t < 20.0 ) then
      c_t = c_x*tan(c_t*Pi/180)
     end if

    end if

    y_3 = c_t + R_LE*cos(B_3*Pi/180) ! Współrzędne y punktu 3
  
    x_4 = R_LE*(1+sin(B_4*Pi/180)) ! Współrzędne x punktu 4
    y_4 = c_t - R_LE*cos(B_4*Pi/180) ! Współrzędne y punktu 4 

    x_5 = c_x - R_TE*(1-sin(B_5*Pi/180)) ! Współrzędne x punktu 5 
    y_5 = -R_TE*cos(B_5*Pi/180) ! Współrzędne y punktu 5 

   ! Współrzędne pomocnicze łopatki
    x_6 = c_x ! Współrzędne x punktu 6
    y_6 = 0 ! Współrzędne y punktu 6

    x_7 = c_x - R_TE ! Współrzędne x punktu 7
    y_7 = 0 ! Współrzędne y punktu 7
    
    x_8 = 0 ! Współrzędne x punktu 8
    y_8 = c_t ! Współrzędne y punktu 8
    
    x_9 = R_LE ! Współrzędne x punktu 9
    y_9 = c_t ! Współrzędne y punktu 9

   ! Wyznaczanie współrzędnych środkowych i promienia odkrytego skrętu (uncovered turn circle) po stronie ssącej między punktami 2 i 1 
    x_01 = (y_1-y_2)*tan(B_1*Pi/180)*tan(B_2*Pi/180)+x_1*tan(B_2*Pi/180)-x_2*tan(B_1*Pi/180) ! Pomocnicza funkcja do obliczeń
    x_02 = (tan(B_2*Pi/180)-tan(B_1*Pi/180)) ! Pomocnicza funkcja do obliczeń
    x_0 = x_01/x_02 ! Współrzędna środkowa x koła osadzonego na łuku między punktami 2 i 1
    y_0 = -(x_0-x_1)/tan(B_1*Pi/180)+y_1 ! Współrzędna środkowa y koła osadzonego na łuku między punktami 2 i 1
    R_0 = sqrt((x_1-x_0)**2+(y_1-y_0)**2) ! Promień koła/łuku osadzonego między punktami 2 i 1

    ! Tutaj dla kąta klina wyjściowego jest przeprowadzana iteracja w celu usunięcia nieciągłosci w gardel jeśli taka się pojawi
    YY2 = y_0 + sqrt(R_0*R_0-(x_2-x_0)**2) ! Współrzędna y porównacza majaca na celu porówanie przesunięcia łuku względem punktu y_2

    ! Przyjeto, że nieciagłość w gardle pojawi się jeśli 
    !wartość wyrażenia będzie większa od 0.5, można ją dowolnie ustawić w zależności od prezyzji obliczeń
    do while ( abs(y_2 - YY2) > 0.5 ) 
      
      ! Nieciagosc w gardle usunięta jeśli zostanie spełnione rówanie wyżej
      e_out = e_out*(y_2/YY2)**4 
    
     ! Jeśli wartość kąta klina wyjściowego po kolejnych iteracjach osiagnie wartość 0.01 
     !to oznaczać będzie, że kąta zacznie zmierząc do wartości ujemny co jest błędem
     if ( e_out < 0.01 ) then
      print *, "Iteracja kata klina wyjsciowego nie powiodla sie!"
      print *, "Kat klina wyjsciowego dazy do wartosciu ujemnych lub nieskonczenie malych!"
      print *, "Zmniejsz kat klina wyjsciowego lub gardlo!"
      print "(a50,F8.2)", "Kat klina wyjsciowego e_out = ", e_out
      print "(a50,F8.2)", "Gardlo o = ", o
      call exit(123)

     else if ( e_out > 360 ) then
      print *, "Iteracja kata klina wyjsciowego nie powiodla sie!"
      print *, "Kat klina wyjsciowego dazy do wartosciu nieskonczenie duzych!"
      print *, "Zmniejsz kat klina wyjsciowego lub gardlo!"
      print "(a50,F8.2)", "Kat klina wyjsciowego e_out = ", e_out
      print "(a50,F8.2)", "Gardlo o = ", o
      call exit(123)
     end if

    end do
    
    ! Nie będą się pojawać w łopatce nieciągłości jeśli wartość bezwzględna z  
    !różnica między punktami y_2 i YY2 będzie mniejsza niż 0.5
    if ( abs(y_2 - YY2) < 0.5 ) then 
      print *, "Nie ma nie ciaglosci w gardle"
      n = 1
    end if 
    
  end do
     
    ! Stałe do rówania wielomianowego 3 stopnia, które będą wyznaczać łuk między punktami 2 i 3 po stronie ssącej
    DS = ((tan(B_3*Pi/180)+tan(B_2*Pi/180))/(x_3-x_2)**2)-((2*(y_3-y_2))/(x_3-x_2)**3)
    CS = ((y_3-y_2)/(x_3-x_2)**2)-(tan(B_2*Pi/180)/(x_3-x_2))-DS*(x_3+2*x_2) 
    BS = tan(B_2*Pi/180)-2*CS*x_2-3*DS*x_2**2 
    AS = y_2-BS*x_2-CS*x_2**2-DS*x_2**3 

    ! Stałe do rówania wielomianowego 3 stopnia, które będą wyznaczać łuk między punktami 4 i 5 po stronie ciśnienieowej
    DP = ((tan(B_4*Pi/180)+tan(B_5*Pi/180))/(x_4-x_5)**2)-((2*(y_4-y_5))/(x_4-x_5)**3) 
    CP = ((y_4-y_5)/(x_4-x_5)**2)-(tan(B_5*Pi/180)/(x_4-x_5))-DP*(x_4+2*x_5) 
    BP = tan(B_5*Pi/180)-2*CP*x_5-3*DP*x_5**2  
    AP = y_5-BP*x_5-CP*x_5**2-DP*x_5**3 

    !Sprawdzenie czy występuje punkt przegięcia na powierzchni naciskowej
    Xinfl = -CP/3/DP

   if (typ == "K") then
    if ( Xinfl > x_4 .and. Xinfl < x_5 ) then
      print *, "W powierzchni naciskowej znajduje sie punkt przeciegia!"
      print *, "Kat zakretu profilu lopatki lub zmniejsz cieciwe styczna"
      print "(a50,F8.2)", "Kat zakretu profilu lopatki, S_dzeta =  ", S_dzeta
      print "(a50,F8.2)", "Cieciwa styczna, c_t = ", c_t
      call exit(123)
    end if 
  end if

    ! Wyznaczanie punktów początkowych po stronie ssawnej i ciśnieniowej (50 punktów do wyznacznie profilu łopatki zostanie wyliczonych)
    XS(1)= x_8
    YS(1)= y_8
    XP(1)= x_8
    YP(1)= y_8

    ! Wyznaczenie łuku po stronie natarcia łopatki (między punktami 3 i 4)
    do i = 2, 10
      ! Stałe służące do wyznaczania kolejnych punktów x dla konkretnych wartości y
      DXP = (x_4-x_8)/9 
      DXS = (x_3-x_8)/9

      XP(i) = XP(i-1)+DXP
      YP(i) = y_9 - sqrt(R_LE*R_LE-(XP(i)-x_9)**2)

      XS(i) = XS(i-1)+DXS
      YS(i) = y_9 + sqrt(R_LE*R_LE-(XS(i)-x_9)**2)
    end do
    
    ! Wyznaczenie łuków z równiania wielomianu 3 stopnia po stronie ssącej i ciśnieniowej (między punktami 3 - 2 i 4 - 5)
    do i = 11, 30
      DXP = (x_5-x_4)/30
      DXS = (x_2-x_3)/20

      XP(i) = XP(i-1)+DXP
      YP(i) = AP+XP(i)*(BP+XP(i)*(CP+XP(i)*DP))

      XS(i) = XS(i-1)+DXS
      YS(i) = AS+XS(i)*(BS+XS(i)*(CS+XS(i)*DS))
    end do
    
    ! Wyznaczenie łuku z równiania wielomianu 3 stopnia po stronie ciśnieniowej (ciąg dalszy między punktami 4 i 5) i łuku z rówania koła po stronie ssącej (między punktami 2 i 1)
    do i = 31, 40 
      DXP = (x_5-x_4)/30
      DXS = (x_1-x_2)/10
 
      XP(i) = XP(i-1)+DXP
      YP(i) = AP+XP(i)*(BP+XP(i)*(CP+XP(i)*DP))
      
      XS(i) = XS(i-1)+DXS
      YS(i) = y_0+sqrt(R_0*R_0-(XS(i)-x_0)**2)
    end do

    ! Wyznaczenie łuku po stronie spływu łopatki (między punktami 1 i 5)
    do i = 41, 50
      DXP = (x_6-x_5)/10 
      DXS = (x_6-x_1)/10
  
      XP(i) = XP(i-1)+DXP
      
      ! Żeby punkty końcowe nie przekorczyły wartości cięciwy osiowej łopatki
      if ( XP(i) > c_x ) then 
        XP(i) = c_x
      end if

      YP(i) = y_7 - sqrt(R_TE*R_TE-(XP(i)-x_7)**2)
    
      XS(i) = XS(i-1)+DXS

      if ( XS(i) > c_x ) then
        XS(i) = c_x
      end if
      
      YS(i) = y_7+sqrt(R_TE*R_TE-(XS(i)-x_7)**2)
    end do

    ! Wyznaczanie własności profilu
    ! Obliczanie maksymalnej grubości łopatki 
    Tmax = 0
      do i = 1, 49
       Tm = 999
       do j = 1, 49
        Tm = min(Tm,sqrt((XS(i)-XP(j))**2+(YS(i)-YP(j))**2))
       end do
       Tmax = max(Tmax,Tm)
      end do

    ! Wyznaczaenie szerokość łopatki od krawędzi natarcia do krawędzi spływu (cięciwa)
    s = sqrt(c_t*c_t+c_x*c_x)

    ! 0bliczanie powierzchni i środka ciężkości łopatki (dzielenie łopatki na trójkąty)
    Area = A(XS(1),YS(1),XS(2),YS(2),XP(2),YP(2))
    x_cg = (XS(1)+XS(2)+XP(2))/3*Area
    y_cg = (YS(1)+YS(2)+YP(2))/3*Area
 
    do i=2,48

      A1 = A(XS(i),YS(i),XS(i+1),YS(i+1),XP(i),YP(i))
      Area = Area + A1

      x_cg = x_cg + (XS(i)+XS(i+1)+XP(i))/3*A1
      y_cg = y_cg + (YS(i)+YS(i+1)+YP(i))/3*A1

      A2 = A(XS(i),YS(i),XP(i),YP(i),XP(i+1),YP(i+1))
      Area = Area + A2

      x_cg = x_cg + (XS(i)+XP(i)+XP(i+1))/3*A2
      y_cg = y_cg + (YS(i)+YP(i)+YP(i+1))/3*A2

    end do

    ! Końcowe wyniki powierzchni i środka ciężkości łopatki
    Area = Area
    x_cg = x_cg/Area
    y_cg = y_cg/Area

    ! Obliczanie przesunięcia względem poziomu łopatki
    Stagger = atan(c_t/c_x)*180/Pi

    ! Obliczanie liczby zweifel, która opisuje siłę styczną aerodynamiczną generowną przez łopatkę
    Zweifel = 4*Pi*R/c_x/z*sin((B_in-Bout)*Pi/180)*cos(B_out*Pi/180)/cos(B_in*Pi/180)

    ! Solidność łopatki (parametrem konstrukcyjny wirnika o przepływie osiowym i jest definiowana jako stosunek długości cięciwy łopatek do rozstawu, solidity)
    Sol = s/t

    ! Procenotwa bloka na wlocie
    Pcti = 2*R_LE/cos(B_in*Pi/180)/t*100
    
    ! Procenotwa bloka na wylocie 
    Pcte = 2*R_TE/cos(B_out*Pi/180)/t*100

    ! Kąt kanału łopatkowego (camber angle)
    Thetac = B_in - B_out

    ! Współczynnik siły nośnej (lift coefficient)
    CL = t/s*(cos(B_in*Pi/180)+cos(B_out*Pi/180))*(tan(B_in*Pi/180)-tan(B_out*Pi/180))

    ! Zapisanie współrzędnych łopatki do pliktu txt
    open(unit=1,file='Wspolrzedne lopatki model nr 1.txt', status='unknown', action='write', iostat=ierror)
    
    write(1,"(F8.2,F8.2)") x_1,y_1
    write(1,"(F8.2,F8.2)") x_2,y_2
    write(1,"(F8.2,F8.2)") x_3,y_3
    write(1,"(F8.2,F8.2)") x_4,y_4
    write(1,"(F8.2,F8.2)") x_5,y_5
    write(1,"(F8.2,F8.2)") x_6,y_6
    write(1,"(F8.2,F8.2)") x_7,y_7
    write(1,"(F8.2,F8.2)") x_8,y_8
    write(1,"(F8.2,F8.2)") x_9,y_9
    write(1,"(F8.2,F8.2)") x_0,y_0
    write(1,"(F8.2,F8.2)") x_cg,y_cg

    do i = 2, 50
      write (1, "(F8.2,F8.2)") XP(i), YP(i)
      write (1, "(F8.2,F8.2)") XS(i), YS(i)
    end do

    ! Zapisanie podstawowtych danych łopatki do pliku txt
    open(unit=1,file='Parametry lopatki model nr 1.txt', status='unknown', action='write', iostat=ierror)

    write(1,"(a38,F8.2)") "Wartości są podane w mm i stopniach!"
    write(1,"(a29,F8.2)") ""
    write(1,"(a59,a2)") "Typ profilu łopatkowego (K - kierownica, W - wirnik) typ =",typ
    write(1,"(a20,F8.2)") "Promień wirnika R =", R
    write(1,"(a19,F8.2)") "Liczba łopatek z =", z
    write(1,"(a22,F8.2)") "Podziałka wirnika t =", t    
    write(1,"(a29,F8.2)") ""
    write(1,"(a78,F8.2)") "Szerokość łopatki od krawędzi natarcia do krawędzi spływu (cięciwa) s =", s
    write(1,"(a42,F8.2)") "Długość cięciwy osiowej łopatki c_x =", c_x
    write(1,"(a43,F8.2)") "Długość cięciwy stycznej łopatki c_t =", c_t
    write(1,"(a33,F8.2)") "Kąt na wolcie do łopatki B_in =", B_in
    write(1,"(a34,F8.2)") "Kąt na wylocie z łopatki B_out =", Bout
    write(1,"(a66,F8.2)") "Dzeta, kąt zakrętu profilu łopatki (unguided turning) s_dzeta =",  S_dzeta
    write(1,"(a75,F8.2)") "Kąt półklinowy na wlocie/na krawędzi natarcia (half wedge angle) e_in =",  e_in
    write(1,"(a69,F8.2)") "Promień koła krawędzi spływowej (the trailing edge circle) R_TE =",  R_TE
    write(1,"(a76,F8.2)") "Promień koła krawędzi wiodącej/natarcia (the leading edge circle) R_LE =",  R_LE
    write(1,"(a29,F8.2)") ""    
    write(1,"(a28,F8.2)") "Powierzchnia łopatki Area =", Area 
    write(1,"(a36,F8.2)") "Maksymalna grubość łopatki Tmax =", Tmax
    write(1,"(a53,F8.2)") "Współrzędna x środka ciężkości łopatki x_cg =", x_cg
    write(1,"(a53,F8.2)") "Współrzędna y środka ciężkości łopatki y_cg =", y_cg
    write(1,"(a29,F8.2)") ""
    write(1,"(a50,F8.2)") "Przesunięcia względem poziomu łopatki Stagger =", Stagger
    write(1,"(a24,F8.2)") "Współczynnik Zweifel =", Zweifel
    write(1,"(a31,F8.2)") "Solidność łopatki Solidity =", Sol
    write(1,"(a51,F8.2)") "Procentowa bloka przepływu na wlocie Intel Block =", Pcti
    write(1,"(a51,F8.2)") "Procentowa bloka przepływu na wylocie Exit Block =", Pcte
    write(1,"(a26,F8.2)") "Kąt komory Camber angle =", Thetac
    write(1,"(a47,F8.2)") "Współczynnik siły nośnej Lift coefficient =",CL
   
    call system('gnuplot -p plotting.gnu')

  end program Algorytmlopatki