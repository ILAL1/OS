SimpleScene.cpp :: 14~19 :: 
�ڵ忡 �ʿ��� ���ο� ���� ���� 
(rotate�� rotate�� ���� ���� ���Ѵ�.
rx, ry, rz, rsum�� modeling veiw���� r�� ȸ������ ���� �� ���ȴ�.  
trans_state�� x���� �������� y���� �������� z���� ���������� ���ϰ�
trans_state_backup�� �� ���¸� backup�Ѵ�.
spacestate�� modeling���� �������� veiwing���� �������� ���ϴ� ���̰�
cx, cy, cz, csum�� eye space�� x�� ���͸� ����� �� ����Ͽ���. )

----------------------------------------------------------------------

SimpleScene.cpp :: 185~196 :: 
modeling���� r�� �� �� �Ͼ� ȸ������ �׸��� �Ͽ���.

----------------------------------------------------------------------

SimpleScene.cpp :: 378~402 :: 
veiwing���� r�� �� �� �Ͼ� ȸ������ �׸��� �Ͽ���. 
�켱 ���� �����̽����� ī�޶��� x���� ����Ѵ��� �װ��� cow�� xyz���� ��� �װ����� �ű�� �Ͽ���. 

----------------------------------------------------------------------

SimpleScene.cpp :: 474~475 :: 
���߿� �巡�׸� �ϸ� cow�� ������ �� �����̴� ũ�⸦ ����ϱ� ���� ���� �巡�׿����� x���� 
������ x���� ����ϴ� �� �� �� oldmouselocax��� ������ ���ȴ�. ���⼭�� �� ������
ó�� ���� ���Ѵ�.

----------------------------------------------------------------------

SimpleScene.cpp :: 506~604 :: 
x-oldmouselocax Ȥ�� y-oldmouselocay�� �̿��� mouse�� �巡���ؼ� �󸶳� ���������� ����ϰ� 
�� ���� ��ü�� �����̴� ���� �̿��Ѵ�. spacestate�� 1�̸� modeling space������ ��������
spacestate�� 0�̸� veiwing space���� �����δ�. �� trans_state�� 1,2,3�̳Ŀ� ���� x,y,z�� ���� �������� ���Ѵ�.
���� trans_state�� 0�̰� spacestate�� 0�̰� rotate�� ���� �ִٸ� veiwing space���� rotate�� �Ѵ�.

modeling space���� transform�� �� �� �켱 modeling coordinate�� transform���� world�� �����̰� �ߴ�. 
���� cow2wld.matrix�� cow2wld.matrix*TranslateMatrix�� �ٲپ���. 

veiwing space���� transform�� �� ������ �켱 modeling coordinate�� world�� �ٲٰ� �� �� wld2cam�� �̿���
eye coordinate���� �ٲ� �� transform�� �ϰ� cam2wld�� �̿��� world coordinate�� ���ƿԴ�.
���� cow2wld.matrix�� cam2wld*TranslateMatrix*wld2cam*cow2wld.matrix�� �ٲپ���.

veiwing space���� rotate�� �� ������ modeling coordinate�� world�� �ٲٰ� �� �� cow�� camera�� ��ġ�� �ű� �� 
wld2cam�� �ߴ�. (�̷��� cow�� ������ �ִ�) �� �� rotate�� �� �� �ٽ� cam2wld�� �̿��� wold coordinate�� ���ƿ� �� 
�ٽ� cow�� ���� ��ġ�� �������´�.
���� cow2wld.matrix�� cameralocatocowlocaMatrix*cam2wld*RotateMatrix*wld2cam*cowlocatocameralocaMatrix*cow2wld.matrix

----------------------------------------------------------------------

SimpleScene.cpp :: 612~621 :: 
idlefunc�� �̿��� modeling space���� rotate�� �� �ֵ��� void�Լ��� ����.
----------------------------------------------------------------------


SimpleScene.cpp :: 637~777 :: 
'm'�� ������ spacestate�� 1�� ����� transform�� modeling space���� �Ͼ�� �Ѵ�.

'v'�� ������ spacestate�� 0�� ����� transform�� veiwing space���� �Ͼ�� �Ѵ�. 
(�� �� modeling space���� rotate�� �� ���Ǵ� random �� ���� ���ϴµ� �̰��� 
r�� ������ m�� v�� ������ ���� �� modeling space������ ratate���� ��� �ٸ��� ���ֱ� �����̴�.
�̸� m�� ���� �� �Ѵٸ� r�� ������ m�� ������ ������ m�� ���� ������ ���� �ٲ�� �ȴ�.)

'r'�� ������ rotate�� 1�� 0���� 0�� 1�� �����. ���� rx ry rz�� �̿��� modeling space����
rotate�� �� ���Ǵ� rotate���� ���Ѵ�. ���� rotate�� 1�̸� rotate�� 0�̸� rotate�� �����.
���� rotate�� 1�� �� transstate_backup�� ���� trans_state�� ������ �� 
trans_state�� 0�� �־� �ٸ� transform�� �� �Ͼ�� ���´�. 
(  ex) r�� ������ ���� x�� ���� ��Ȳ�̾��ٸ� transstate_backup�� 1�� �����Ѵ�.)
�� rotate=0�� �� transstate_backup�� �ִ� ���� �ٽ� trans_state�� �ִ´�.
space_state�� ���� ���� rotate�� modeling space���� ���� veiwing space���� ���� ���Ѵ�. 

'x','y','z'�� ���� �� trans_state�� ���� 1,2,3���� �ٲ��ش�. 
���� roate ���̾��ٸ� rotate���� 0���� �ٲ��� rotate�� �����. 

----------------------------------------------------------------------













