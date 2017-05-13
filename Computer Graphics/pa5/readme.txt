MS Visual 10, Window 7 Professional K 사용

Lighting OK
Clipping OK
Triangulation almost OK  (almost인 이유 :  perspective모드일때 sphere모드를 켰을 때 sphere가 오른쪽에 걸쳤을 때 삼각형 하나가 전해지지 않았음)
Rasterization amost OK (almost인 이유 rasterization을 하면 백그라운드 배경은 안 밀리고 나머지 도형들이 1픽셀씩 밀림)		 
 -Linear interpolation OK
 -z-Buffer OK
 -Back-Face Culling OK (BackFaceCulling은 Triangulation에 구현함. triangulation 단계에서 wire mode로 구현 확인 가능)

더 자세한 comment는 code에 적어둠. 

또한 PA5_6 폴더의 debug폴더에 있는 testMyGL.exe파일을 testMyGL.aaa로 고침.