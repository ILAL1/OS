SimpleScene.cpp :: 14~19 :: 
코드에 필요한 새로운 변수 정의 
(rotate는 rotate를 할지 말지 정한다.
rx, ry, rz, rsum은 modeling veiw에서 r의 회전축을 정할 때 사용된다.  
trans_state는 x축으 움직일지 y축을 움직일지 z축을 움직일지를 정하고
trans_state_backup은 이 상태를 backup한다.
spacestate는 modeling에서 움직일지 veiwing에서 움직일지 정하는 것이고
cx, cy, cz, csum은 eye space의 x축 벡터를 계산할 때 사용하였다. )

----------------------------------------------------------------------

SimpleScene.cpp :: 185~196 :: 
modeling에서 r을 할 때 하얀 회전축을 그리게 하였다.

----------------------------------------------------------------------

SimpleScene.cpp :: 378~402 :: 
veiwing에서 r을 할 때 하얀 회전축을 그리게 하였다. 
우선 월드 스페이스에서 카메라의 x축을 계산한다음 그것을 cow의 xyz값을 얻어 그곳으로 옮기게 하였다. 

----------------------------------------------------------------------

SimpleScene.cpp :: 474~475 :: 
나중에 드래그를 하며 cow를 움직일 때 움직이는 크기를 계산하기 위해 그전 드래그에서의 x값과 
나중의 x값을 계산하는 데 그 때 oldmouselocax라는 변수가 사용된다. 여기서는 이 변수의
처음 값을 정한다.

----------------------------------------------------------------------

SimpleScene.cpp :: 506~604 :: 
x-oldmouselocax 혹은 y-oldmouselocay를 이용해 mouse를 드래그해서 얼마나 움직였는지 계산하고 
그 값을 물체를 움직이는 데에 이용한다. spacestate는 1이면 modeling space에서의 움직임을
spacestate가 0이면 veiwing space에서 움직인다. 또 trans_state가 1,2,3이냐에 따라 x,y,z에 따라 움직일지 정한다.
또한 trans_state가 0이고 spacestate가 0이고 rotate가 켜져 있다면 veiwing space에서 rotate를 한다.

modeling space에서 transform을 할 때 우선 modeling coordinate를 transform한후 world로 움직이게 했다. 
따라서 cow2wld.matrix를 cow2wld.matrix*TranslateMatrix로 바꾸었다. 

veiwing space에서 transform을 할 때에는 우선 modeling coordinate를 world로 바꾸고 그 후 wld2cam을 이용해
eye coordinate으로 바꾼 후 transform을 하고 cam2wld를 이용해 world coordinate로 돌아왔다.
따라서 cow2wld.matrix를 cam2wld*TranslateMatrix*wld2cam*cow2wld.matrix로 바꾸었다.

veiwing space에서 rotate를 할 때에는 modeling coordinate를 world로 바꾸고 그 후 cow를 camera의 위치로 옮긴 후 
wld2cam을 했다. (이러면 cow는 원점에 있다) 그 후 rotate를 한 후 다시 cam2wld를 이용해 wold coordinate로 돌아온 후 
다시 cow를 원래 위치로 돌려보냈다.
따라서 cow2wld.matrix를 cameralocatocowlocaMatrix*cam2wld*RotateMatrix*wld2cam*cowlocatocameralocaMatrix*cow2wld.matrix

----------------------------------------------------------------------

SimpleScene.cpp :: 612~621 :: 
idlefunc를 이용해 modeling space에서 rotate할 수 있도록 void함수를 만듬.
----------------------------------------------------------------------


SimpleScene.cpp :: 637~777 :: 
'm'을 누르면 spacestate를 1로 만들어 transform이 modeling space에서 일어나게 한다.

'v'을 누르면 spacestate를 0로 만들어 transform이 veiwing space에서 일어나게 한다. 
(이 때 modeling space에서 rotate할 때 사용되는 random 축 값을 정하는데 이것은 
r을 누른후 m과 v를 번갈아 누를 때 modeling space에서의 ratate축을 계속 다르게 해주기 위함이다.
이를 m을 누를 때 한다면 r을 누르고 m을 여러번 누르면 m을 누를 때마다 축이 바뀌게 된다.)

'r'을 누르면 rotate를 1은 0으로 0은 1로 만든다. 또한 rx ry rz를 이용해 modeling space에서
rotate할 때 사용되는 rotate축을 정한다. 또한 rotate가 1이면 rotate를 0이면 rotate를 멈춘다.
또한 rotate가 1일 때 transstate_backup에 현재 trans_state를 저장한 후 
trans_state에 0을 넣어 다른 transform이 못 일어나게 막는다. 
(  ex) r을 누르기 전에 x를 누른 상황이었다면 transstate_backup에 1을 저장한다.)
또 rotate=0일 때 transstate_backup에 있던 값을 다시 trans_state에 넣는다.
space_state의 값에 따라 rotate를 modeling space에서 할지 veiwing space에서 할지 정한다. 

'x','y','z'를 누를 때 trans_state를 각각 1,2,3으로 바꿔준다. 
또한 roate 중이었다면 rotate값을 0으로 바꾼후 rotate를 멈춘다. 

----------------------------------------------------------------------













