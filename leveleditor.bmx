
Import sidesign.minib3d
Import Pub.FreeJoy
Strict

If Not JoyCount() RuntimeError "No joystick found!"

AppTitle="Crasher"

Graphics3D(1200,900,32,2,60)

Local cam:TCamera = CreateCamera()
PositionEntity cam,0,0.5,-9
CameraClsColor cam,210,210,80
Local mx,my,mz,wx,wy,entity:TEntity

Local sun:TLight = CreateLight()
TurnEntity sun,35,35,35

Local environ: Teditorenvironment= Teditorenvironment.neu()
Local punkt:tentity=CreateCube()
ScaleEntity punkt,0.5,0.5,0.5
EntityFX punkt,17
Local campiv:tpivot=CreatePivot(punkt)

EntityParent(cam,campiv)
Local tex:ttexture=LoadTexture("gfx/gitterbox.bmp",3)
EntityTexture(punkt,tex)

ttile.loadpath(CurrentDir()+"/tiles/")

enablepolledinput()

Local camrotation
Local setztile:ttile
Local alpha#,alphad#=0.005
Local mox#,moy#
Local mover:txyint
Local joymode%
'MoveMouse 200,200
'HideMouse
Repeat
	Cls
	joymode =0
	Tfps.update()
	tjoy.update()
	
'	If (tjoy.buttondown[7]) Then joymode=1
'	If (tjoy.buttondown[4]) Then joymode=2
'	If (tjoy.buttondown[6]) Then joymode=3
	If (tjoy.buttondown[2]) Then joymode=4

	Select joymode
		Case 0
			mover=calcmove(tjoy.rx,tjoy.ry,mox)
			mx=mx+mover.x
			my=my-mover.y
			mox=mox+tjoy.u*2	
			moy=moy+tjoy.r*2
			If moy < -80 Then moy=-80
			If moy>80 Then moy=80		
		Case 1
			mz=mz-tjoy.ry		
		Case 2
			camrotation= camrotation+90*tjoy.rx
		Case 3
			moy=moy-tjoy.ry*5
			mox=mox+tjoy.rx*5
			If moy < -80 Then moy=-80
			If moy>80 Then moy=80
		Case 4
			wx=wx+tjoy.rx
			wy=wy-tjoy.ry		
	End Select
	tjoy.flush()
	
	If tjoy.buttondown[0] Then
		If Not(setztile = Null) Then
			setztile.setzten(mx,mz,my,camrotation)
			'ttile.setzte(mx,mz,my,camrotation)
		EndIf
	EndIf
	If tjoy.buttondown[3] Then
		ttile.befreie(mx,mz,my)
	EndIf
	If (tjoy.buttonhit[1]) Then camrotation= camrotation+90
	If (tjoy.buttonhit[4]) Then mz=mz-1
	If (tjoy.buttonhit[5]) Then mz=mz+1
	If KeyHit(key_1) Then
		setztile=ttile.pref_wahl()
	EndIf
	If KeyHit(key_2) Then
		setztile=ttile.next_wahl()
	EndIf
	If KeyHit(key_s) Then
		ttile.saveit()
	EndIf
	If KeyHit(key_l) Then
		ttile.loadit()
	EndIf
	PositionEntity punkt, mx,mz,my
	If Not(setztile = Null) And Not(joymode =4) Then
		setztile.unhighlight_tile()
		PositionEntity setztile.mesh,mx,mz,my
		RotateEntity setztile.mesh,0,camrotation,0
	EndIf
	alpha=alpha+alphad
	If alpha > 1 Then
		alpha=1
		alphad=-0.005
	EndIf
	If alpha < 0 Then
		alpha=0
		alphad=0.005
	EndIf
'	MoveMouse 200,200
	If joymode=4 Then
		RotateEntity(campiv,0,0,0,0)
		PositionEntity campiv,wx*1.6,wy*1.2,14995,1
		ttile.showall()
		setztile=ttile.highlighttile_xy(wx,wy)
	Else
		RotateEntity(campiv,moy,mox,0,0)
		PositionEntity campiv,mx,mz,my,1
	EndIf
	If KeyDown(Key_m) Then
		environ.showmenu()
		RotateEntity(campiv,0,0,0,0)
		PositionEntity campiv,wx*1.6,wy*1.2,14995,1

	EndIf
	
	EntityAlpha(punkt,alpha)
	PositionEntity environ.gitter,mx,mz-0.5,my

	UpdateWorld
	RenderWorld
	Text 0,0,TFPS.fps
	text 0,40,mox + " / "+moy
	
	Flip
Until KeyHit(key_escape)

ShowMouse

Type TFPS
	Global old_ms%,fps#,new_fps#,old_fps#
	Function update()
		Local new_ms%=MilliSecs()
		new_fps = 1000.0/(new_ms-old_ms)
		fps=(old_fps+new_fps)/2
		old_fps=new_fps
		old_ms=new_ms
	End Function
End Type

Type Teditorenvironment
	Field gittertex:ttexture
	Field gitter:tmesh
	
	Field menunew:tsprite
	Field menuclose:tsprite
	Field menusave:tsprite
	Field menugreen:tsprite
	Field menured:tsprite
	Field menuload:tsprite
	Field oldcamposx,oldcamposy,oldcamposz
	
	Function neu:Teditorenvironment()
		Local o:Teditorenvironment=New Teditorenvironment
		
		o.menunew=LoadSprite("gfx/new.bmp",1+16+32)
		o.menuclose=LoadSprite("gfx/close.bmp",1+16+32)
		o.menusave=LoadSprite("gfx/save.bmp",1+16+32)
		o.menugreen=LoadSprite("gfx/green.bmp",1+16+32)
		o.menured=LoadSprite("gfx/red.bmp",1+16+32)
		o.menuload=LoadSprite("gfx/load.bmp",1+16+32)

		EntityAlpha(o.menunew,0)
		EntityAlpha(o.menuclose,1)

		HideEntity(o.menunew)
		HideEntity(o.menuclose)
		HideEntity(o.menusave)
		HideEntity(o.menugreen)
		HideEntity(o.menured)
		HideEntity(o.menuload)
		
		
		o.gittertex=LoadTexture("gfx/gitter.bmp",3)
		o.gitter=CreateMesh()
		Local surf:Tsurface=CreateSurface(o.gitter)
		Local v1=AddVertex(surf,-5.5,0,5.5,-5,6)
		Local v2=AddVertex(surf,-5.5,0,-5.5,-5,-5)
		Local v3=AddVertex(surf,5.5,0,-5.5,6,-5)
		Local v4=AddVertex(surf,5.5,0,5.5,6,6)
		AddTriangle(surf,v1,v3,v2)
		AddTriangle(surf,v1,v4,v3)
		EntityFX o.gitter,17
		EntityTexture o.gitter,o.gittertex
		Return o
	End Function
	Method showmenu()
		ShowEntity(menunew)
		PositionEntity menunew,0,-6,15000

		ShowEntity(menuclose)
		PositionEntity menuclose,0,-3,15000

		ShowEntity(menusave)
		PositionEntity menusave,0,0,15000

		ShowEntity(menuload)
		PositionEntity menuload,0,3,15000

	End Method
	Method hidemenu()
		HideEntity(menunew)
		HideEntity(menuclose)
		HideEntity(menusave)
		HideEntity(menuload)

	End Method
End Type

Type Ttile
	Global gewaehlt%
	Global tgewaehlt:ttile
	Global setzlist:TList
	Global wahllist:TList
	Global vorschaurotate%
	Global highlightbright%
	Field X,Y,Z
	Field Rotation
	Field mesh:tmesh
	Field name$
	Field namex%,namey%
	Function befreie(x%,y%,Z%)
		For Local m:ttile = EachIn setzlist
			If m.x=x And m.y=y And m.z = z Then
				setzlist.remove(m)
				FreeEntity(m.mesh)
				m=Null
			EndIf
		Next	
	End Function
	Function highlighttile_xy:ttile(x,y)
		Local ausgabe:ttile
		For Local temptile:ttile=EachIn ttile.wahllist
			If temptile.namex=y And temptile.namey=x Then
				temptile.highlight_tile()
				
				ausgabe=temptile
				
			Else
				temptile.unhighlight_tile()
				
			EndIf
		Next
		Return ausgabe
	End Function
	Method highlight_tile()
		Local t%=(MilliSecs() / 4) Mod 256
		EntityColor(mesh,t,t,t)
		ScaleEntity mesh, 1.2,1.2,1.2
		PositionEntity mesh,namey*1.6,namex*1.2,14993
		RotateEntity mesh,0,vorschaurotate*2,0
	End Method
	Method unhighlight_tile()
		EntityColor(mesh,255,255,255)
		ScaleEntity mesh, 1,1,1
	End Method
	Function showall()
		Local m%=0
		Local anzahl%=ttile.wahllist.count()
		
		For Local temptile:ttile=EachIn ttile.wahllist
			ShowEntity(temptile.mesh)
			If m=ttile.gewaehlt Then
				EntityColor(temptile.mesh,255,128,128)
			Else
				EntityColor(temptile.mesh,255,255,255)
			EndIf
			PositionEntity temptile.mesh,temptile.namey*1.6,temptile.namex*1.2,15000
			RotateEntity temptile.mesh,0,vorschaurotate,0
			
			m=m+1
		Next
		vorschaurotate=vorschaurotate+2
		If highlightbright= 128 Then highlightbright=255 Else highlightbright=128
	End Function 
	Function hideall()
		Local m%=0
		For Local temptile:ttile=EachIn ttile.wahllist
			HideEntity(temptile.mesh)
			EntityColor(temptile.mesh,255,255,255)
			m=m+1
		Next
		highlightbright=128
	End Function 
	Method setzten(rx%,ry%,rZ%,rRot%)
		Local neu%=True
		For Local m:ttile = EachIn setzlist
			If m.x=rx And m.y=ry And m.z = rz And m.rotation=rrot And m.name=name Then
				neu=False
			EndIf
		Next	
		If neu=True Then
			Local o:ttile=New ttile
			o.x=rx
			o.y=ry
			o.z=rz
			o.rotation=rrot
			o.name=name
			o.mesh=CopyMesh(mesh)
			PositionEntity o.mesh,rx,ry,rz
			RotateEntity o.mesh,0,rrot,0
			UpdateNormals(o.mesh)
			ShowEntity o.mesh
			setzlist.addlast(o)
		EndIf
	End Method
	
	Function setzte(x%,y%,Z%,Rotation%)
		Local neu%=True
		For Local m:ttile = EachIn setzlist
			If m.x=x And m.y=y And m.z = z And m.rotation=rotation And m.name=ttile(wahllist.valueatindex(gewaehlt)).name Then
				neu=False
			EndIf
		Next	
		If neu=True Then
			Local o:ttile=New ttile
			o.x=x
			o.y=y
			o.z=z
			o.rotation=rotation
			o.name=ttile(wahllist.valueatindex(gewaehlt)).name
			o.mesh=CopyMesh(ttile(wahllist.valueatindex(gewaehlt)).mesh)
			PositionEntity o.mesh,x,y,z
			RotateEntity o.mesh,0,rotation,0
			UpdateNormals(o.mesh)
			setzlist.addlast(o)
		EndIf
	End Function
	Function saveit()
		Local datei:TStream=WriteFile("newlevel")
		For Local m:ttile = EachIn setzlist
			WriteInt(datei,m.x)
			WriteInt(datei,m.y)
			WriteInt(datei,m.z)
			WriteInt(datei,m.rotation)
			WriteLine(datei,m.name)
		Next
		CloseFile datei
	End Function
	Function Loadit()
		For Local m:ttile = EachIn setzlist
			setzlist.remove(m)
			FreeEntity(m.mesh)
			m=Null
		Next
		Local infile:TStream=ReadFile("newlevel")
		Repeat
			Local x%= ReadInt(infile)
			Local y%= ReadInt(infile)
			Local z%= ReadInt(infile)
			Local rot%= ReadInt(infile)
			Local name$= ReadLine(infile)	
			For Local i%= 0 To wahllist.count()-1
				If ttile(wahllist.valueatindex(i)).name = name Then
					gewaehlt=i
					setzte(x,y,z,rot)
				EndIf
			Next
		Until Eof(infile)
		CloseFile(infile)
	End Function
	Function pref_wahl:ttile()
		HideEntity ttile(wahllist.valueatindex(gewaehlt)).mesh
		gewaehlt=gewaehlt-1
		If gewaehlt=-1 Then gewaehlt=wahllist.count()-1
		Local o:ttile=ttile(wahllist.valueatindex(gewaehlt))
		ShowEntity o.mesh
		Return o
	End Function
	Function next_wahl:ttile()
		HideEntity ttile(wahllist.valueatindex(gewaehlt)).mesh
		gewaehlt=gewaehlt+1
		If gewaehlt=wahllist.count() Then gewaehlt=0
		Local o:ttile=ttile(wahllist.valueatindex(gewaehlt))
		ShowEntity o.mesh
		Return o
	End Function
	Function Loadtile:ttile(path$)
		Local pos1%,pos2%
		Local o:ttile=New ttile
		o.name=StripDir(path$)
		pos1=Instr(o.name,"_",0)
		pos2=Instr(o.name,"_",pos1+1)
		If (pos1 >0) And (pos2>0) Then		
			o.namex=Int(Mid(o.name,0,pos1))
			o.namey=Int(Mid(o.name,pos1+1,pos2-pos1-1))
		EndIf
		'Print o.name+"-->"+o.namex+"x"+o.namey+" ("+pos1+", "+pos2+")"
		o.mesh=CreateMesh()
		Local infile:TStream=ReadFile(path)
		Repeat
			If ReadLine(infile)="SURF" Then
				Local name$=infile.ReadLine()
				Local texname$=infile.ReadLine()
				Local brush:Tbrush
				If texname ="" Then
					brush = CreateBrush()
				Else
					Print "BrushTexturepath: "+ texname
					brush = LoadBrush(texname)
				EndIf
				
				Local surf:tsurface=CreateSurface(o.mesh)
				
				PaintSurface(surf,brush)
				
				Local texx=ReadFloat(infile)
				Local texy=ReadFloat(infile)
				Local texsx=ReadFloat(infile)
				Local texsy=ReadFloat(infile)
				Local texr=ReadFloat(infile)
				Local textur:TTexture = LoadTexture(texname)
				
				PositionTexture(textur,texx,texy)
				ScaleTexture(textur,texsx,texsy)
				RotateTexture(textur,texr)	
				Local count%=ReadInt(infile)
				For Local i% = 0 To count -1
					If ReadLine(infile) = "TRIS" Then
						Local v#[3,3]
						Local uv#[3,2]
						For Local i% = 0 To 2
							v#[i,0]=infile.ReadFloat()
							v#[i,1]=infile.ReadFloat()
							v#[i,2]=infile.ReadFloat()
							uv[i,0]=infile.ReadFloat()
							uv[i,1]=infile.ReadFloat()
						Next
						Local vert%[3]
						For Local i=0 To 2
							vert[i]=AddVertex(surf,v[i,0],v[i,1],v[i,2],uv[i,0],uv[i,1])	
						Next
						Local id%=AddTriangle(surf,vert[0],vert[1],vert[2])
					Else
						Notify "problem in infile"
					EndIf
				Next 
			EndIf
		Until Eof(infile)
		
		CloseFile(infile)
		HideEntity o.mesh
		Return o
	End Function
	
	Function Loadpath(path$)
		
		If setzlist=Null Then setzlist=New TList
		If wahllist=Null Then wahllist=New TList
		
		Local dir%=ReadDir(path$)

		If Not dir RuntimeError "failed to read current directory"

		Repeat
			Local t$=NextFile( dir )
			If t="" Exit
			If t="." Or t=".." Or Left(t,1) = "." Then
				Continue
			Else
				Print FileType(path$+t) + ": "+t 
				If FileType(path$+t) = 1 Then wahllist.addlast(loadtile(path$+t))
			EndIf
			
		Forever

		CloseDir dir
	
	End Function
End Type


Type TXYInt
	Field X%,Y%
End Type

Function calcmove:TXYInt(X,Y,Rotation)
	Local o:TXYInt=New txyint
	While rotation < 0
		rotation = rotation + 360
	Wend
	rotation =rotation Mod 360
	If rotation > 315 Or rotation <=45 Then
		o.x=x
		o.y=y
	ElseIf rotation >45 And rotation <= 135 Then
		o.x=y
		o.y=-x
	ElseIf rotation > 135 And rotation <=225 Then
		o.x=-x
		o.y=-y
	ElseIf rotation > 225 And rotation <=315 Then
		o.x=-y
		o.y=x
	EndIf
	Return o
End Function

Type tjoy
	Global rx%,ry%,x%,y%,z%,t%,ly%,lx%
	Global u%,v%,r%,hat%
	Global buttondown[10]
	Global buttonhit[10]
	Global buttoncturbo[10]
	Global buttoncturbotime[10]
	Const reakt%=125
	Function flush()
		rx=0
		ry=0
	End Function
	Function init()
		rx=0
		ry=0
		t=0
	End Function
	Function update()
		Local zeit=MilliSecs()
		For Local i =0 To 9
			Local a%=JoyDown(i)
			
			If a=1 Then
				If zeit- buttoncturbotime[i] <reakt Then 
					buttoncturbotime[i]=zeit
					buttoncturbo[i]=1
				EndIf
				If buttondown[i] = 0 Then
					buttonhit[i]=1
				Else
					buttonhit[i]=0
				EndIf
			Else
				buttonhit[i]=0
				buttoncturbo[i]=0
				buttoncturbotime[i]=0
			EndIf
			buttondown[i]=a
		Next
		x=JoyX()
		y=JoyY()
		z=JoyZ()
		r=JoyR()
		u=JoyU()
		v=JoyV()
		hat=JoyHat()


		If Not(x=lx) Or Not(y=ly) Or (t+reakt < zeit) Then
			rx = rx+x
			ry = ry+y
			lx=x
			ly=y
			t=zeit
		EndIf
	End Function
End Type

