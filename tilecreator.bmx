
Import sidesign.minib3d
Import MaxGui.Drivers
Import maxgui.xpmanifest

Strict

AppTitle="mergeator"

Type T3dortsvektor
	Field x#,y#,z#
	Function neu:T3dortsvektor(X#=0,y#=0,z#=0)
		Local out:T3dortsvektor=New T3dortsvektor
		out.x=x
		out.y=y
		out.z=z
		Return out
	End Function
	Method position(x#,y#,z#)
		Self.x=x
		Self.y=y
		Self.z=z
	End Method
End Type

Type Tselectionsprite Extends T3dortsvektor
	Global list:TList
	Global three:Tselectionsprite[3]
	Global nummer%
	Field mesh:TSprite
	Method position(x#,y#,z#)
		Self.x=x
		Self.y=y
		Self.z=z
		PositionEntity(mesh,x,y,z)
	End Method
	Function neu:Tselectionsprite(X#=0,y#=0,z#=0)
		Local out:Tselectionsprite =New Tselectionsprite
		out.mesh=LoadSprite("gfx/selected.bmp",7)
		HideEntity out.mesh
		out.position(x,y,z)
		If list= Null Then
			list = New TList
		EndIf
		list.addlast(out)
		Return out
	End Function
	Function makethree()
		For Local i = 0 To 2
			three[i]=neu()
		Next
	End Function
	Function setpos(pos:T3dortsvektor)
		ShowEntity three[nummer].mesh
		three[nummer].position(pos.x,pos.y,pos.z)
		nummer=nummer+1
		If nummer =3 Then
			For Local i = 0 To 2
				HideEntity three[i].mesh
			Next
			nummer=0
		EndIf
	End Function
	Function scaleall(scale#)
		For Local i = 0 To 2
			three[i].mesh.ScaleSprite(scale,scale)
		Next
	End Function

End Type


Type Tanfasspunkt Extends T3dortsvektor
	Global list:TList
	Global anz%
	Global lx#,ly#,lz#,ls#
	Global transparenz#
	Global vx%,bx%,vy%,by%,vz%,bz%
	Field mesh:TMesh
	
	Method position(x#,y#,z#)
		Self.x=x
		Self.y=y
		Self.z=z
		PositionEntity(mesh,x,y,z)
	End Method
	Function neu:Tanfasspunkt(X#=0,y#=0,z#=0)
		Local out:Tanfasspunkt =New Tanfasspunkt
		out.mesh=CreateCube()
		EntityColor(out.mesh,255,0,0)
		out.position(x,y,z)
		EntityPickMode out.Mesh,2
		If list= Null Then
			list = New TList
		EndIf
		list.addlast(out)
		Return out
	End Function
	Function createfield(ANZ%)
	
		If ls=0 Then ls=1
		If list Then
			For Local raus:Tanfasspunkt = EachIn list
				FreeEntity raus.mesh
			Next
			list.clear()
		EndIf
		For Local i=0 To anz^3-1
			neu()
		Next
		Tanfasspunkt.anz=anz
		vx=0
		vy=0
		vz=0
		bx=anz
		by=anz
		bz=anz
		anordnen(lx,ly,lz,ls)

	EndFunction
	Method scale(scale#)
		ScaleEntity(mesh,scale,scale,scale)
	End Method
	Method alpha(alpha#)
		EntityAlpha(mesh,alpha)
	End Method
	Function scaleall(scale#)
		If list Then
			For Local punkt:Tanfasspunkt = EachIn list
				punkt.scale(scale)
			Next
		EndIf
		Tselectionsprite.scaleall(scale*2)
	End Function
	Function alphaall(alpha#)
		If list Then
			For Local punkt:Tanfasspunkt = EachIn list
				punkt.alpha(alpha)
			Next
		EndIf
	End Function
	Function hideall()
		If list Then
			For Local punkt:Tanfasspunkt = EachIn list
				HideEntity punkt.mesh
			Next
		EndIf
	End Function
	Function showall()
		If list Then
			Rem
			For Local punkt:Tanfasspunkt = EachIn list
				ShowEntity punkt.mesh
			Next
			EndRem
			anordnen(lx,ly,lz,ls)
		EndIf
	End Function
	Function resetanz(newanz%)
		If ls=0 Then ls=1
		createfield(newanz)
	End Function
	Function anordnen(x#,y#,z#,size#)
		lx=x
		ly=y
		lz=z
		ls=size
		Local xs,ys,zs
		Local ranz%=anz-1
		If list Then
			For Local punkt:Tanfasspunkt = EachIn list
				
					
				punkt.position(x-size/2.0+size*xs/ranz,y-size/2.0+size*ys/ranz,z-size/2.0+size*zs/ranz)
				HideEntity punkt.mesh
				If xs >= vx And xs <= bx Then
					If ys >= vy And ys <= by Then
						If zs >= vz And zs <= bz Then
							ShowEntity punkt.mesh
						EndIf
					EndIf
				EndIf
				xs=xs+1
				If xs= anz Then
					xs=0 
					ys=ys+1
				EndIf
				If ys= anz Then
					ys=0 
					zs=zs+1
				EndIf
			Next
		EndIf

	End Function
	
	Function getpos:T3dortsvektor(X#,Y#,Z#)
		Local minpunkt:T3dortsvektor
		Local mindist# = 2.0
		For Local search: T3dortsvektor =EachIn list
			Local dist#=Sqr(Abs((x-search.x)^2+(y-search.y)^2+(z-search.z)^2))
			If dist < mindist Then
				mindist=dist
				minpunkt = search
			EndIf
		Next
		If Not(minpunkt = Null) Then Return T3dortsvektor.neu(minpunkt.x,minpunkt.y,minpunkt.z)
	End Function
	Function find:Tanfasspunkt (X#,Y#,Z#)
		Local minpunkt:Tanfasspunkt 
		Local mindist# = 2.0
		For Local search:Tanfasspunkt =EachIn list
			Local dist#=Sqr(Abs((x-search.x)^2+(y-search.y)^2+(z-search.z)^2))
			If dist < mindist Then
				mindist=dist
				minpunkt = search
			EndIf
		Next
		If Not(minpunkt = Null) Then Return minpunkt 
	End Function
End Type

Type T3dvertice Extends T3dortsvektor
	Field u#,v#
	Field id#
	Function neu:T3dvertice(X#=0,y#=0,z#=0)
		Local out:T3dvertice =New T3dvertice
		out.x=x
		out.y=y
		out.z=z
		out.u=0
		out.v=0
		Return out
	End Function
	Method uv(u#,v#)
		Self.u=u
		Self.v=v
	End Method
	Function conv_ap:T3dvertice(in:T3dortsvektor)
		Local out:T3dvertice =New T3dvertice
		out.x=in.x
		out.y=in.y
		out.z=in.z
		out.u=0
		out.v=0
		Return out
	End Function
End Type

Type T3dtris
	Field V:T3dvertice[3]
	Field id%
	Const MTRIS%=0
	Const MCUBE%=1
	Const MCUBE2%=2
	Const MSPHERE%=3
	Const MZYLINDER%=4
	Const MPLANE%=5
	

	Function neu:T3dtris(V1:T3dvertice,V2:T3dvertice,V3:T3dvertice)
		Local out:T3dtris=New T3dtris
		out.v[0]=V1
		out.v[1]=V2
		out.v[2]=V3
		out.v[0].uv(0,0)
		out.v[1].uv(1,0)
		out.v[2].uv(1,1)
		Return out
	End Function
	Method turn()
		Local temp:T3dvertice
		temp=v[0]
		v[0]=v[1]
		v[1]=temp
	End Method
	Function Load:T3dtris(infile:TStream)
		Local v:T3dvertice[3]
		Local uv#[3,2]
		For Local i% = 0 To 2
			Local x#=infile.ReadFloat()
			Local y#=infile.ReadFloat()
			Local z#=infile.ReadFloat()
			v[i]=T3dvertice.neu(x,y,z)
			uv[i,0]=infile.ReadFloat()
			uv[i,1]=infile.ReadFloat()
		Next
		Local out:T3dtris=neu(v[0],v[1],v[2])
		For Local i% = 0 To 2
			v[i].uv(uv[i,0],uv[i,1])
		Next
		Return out
	End Function
	Method save(outfile:TStream)
		WriteLine (outfile,"TRIS")
		For Local i%=0 To 2
			WriteFloat(outfile,V[i].x)
			WriteFloat(outfile,V[i].y)
			WriteFloat(outfile,V[i].z)
			WriteFloat(outfile,V[i].u)
			WriteFloat(outfile,V[i].v)
		Next
	End Method
	Method update(mode%=0,surf:TSurface)
		Select mode
			Case MTRIS%
				v[0].uv(0,0)
				v[1].uv(1,0)
				v[2].uv(1,1)
			Case MCUBE%
				If (Floor(v[0].y*100.0)=Floor(v[1].y*100.0)) And (Floor(v[0].y*100.0)= Floor(v[2].y*100.0)) Then
					For Local i%=0 To 2
						v[i].uv(v[i].x-0.5,v[i].z-0.5)
					Next
				Else
					For Local i%=0 To 2
						v[i].uv(v[i].x+v[i].z,v[i].y+0.5)
					Next
				EndIf
			Case MCUBE2%
				If (Floor(v[0].y*1000.0)=Floor(v[1].y*1000.0)) And (Floor(v[0].y*1000.0)= Floor(v[2].y*1000.0)) Then
					v[0].uv(v[0].x-0.5,v[0].z-0.5)
					v[1].uv(v[1].x-0.5,v[1].z-0.5)
					v[2].uv(v[2].x-0.5,v[2].z-0.5)
				ElseIf (Floor(v[0].x*1000.0)=Floor(v[1].x*1000.0)) And (Floor(v[0].x*1000.0)= Floor(v[2].x*1000.0))
					v[0].uv(v[0].z+0.5,v[0].y+0.5)
					v[1].uv(v[1].z+0.5,v[1].y+0.5)
					v[2].uv(v[2].z+0.5,v[2].y+0.5)
				ElseIf (Floor(v[0].z*1000.0)=Floor(v[1].z*1000.0)) And (Floor(v[0].z*1000.0)= Floor(v[2].z*1000.0))
					v[0].uv(v[0].x+0.5,v[0].y+0.5)
					v[1].uv(v[1].x+0.5,v[1].y+0.5)
					v[2].uv(v[2].x+0.5,v[2].y+0.5)
				Else
					v[0].uv(v[0].x+v[0].z,v[0].y+0.5)
					v[1].uv(v[1].x+v[1].z,v[1].y+0.5)
					v[2].uv(v[2].x+v[2].z,v[2].y+0.5)
				EndIf
			Case MPLANE%
				Local minmax#[3,3]
				For Local i% = 0 To 2
					minmax#[i,0]=-1
					minmax#[i,1]=1
				Next
				For Local i% = 0 To 2
					If v[i].x > minmax#[0,0] Then minmax#[0,0]=v[i].x
					If v[i].x < minmax#[0,1] Then minmax#[0,1]=v[i].x
					If v[i].y > minmax#[1,0] Then minmax#[1,0]=v[i].y
					If v[i].y < minmax#[1,1] Then minmax#[1,1]=v[i].y
					If v[i].z > minmax#[2,0] Then minmax#[2,0]=v[i].z
					If v[i].z < minmax#[2,1] Then minmax#[2,1]=v[i].z
				Next
				
				For Local i% = 0 To 2
					minmax#[i,2]=Abs(minmax#[i,0]-minmax#[i,1])
				Next
				If minmax#[0,2] > minmax#[1,2] And minmax#[1,2] > minmax#[2,2] Then
					For Local i% = 0 To 2
						v[i].uv(v[i].x/minmax#[0,2],v[i].y/minmax#[1,2])
					Next
				EndIf
				If minmax#[0,2] > minmax#[2,2] And minmax#[2,2] > minmax#[1,2] Then
					For Local i% = 0 To 2
						v[i].uv(v[i].x/minmax#[0,2],v[i].z/minmax#[2,2])
					Next
				EndIf
				If minmax#[1,2] > minmax#[2,2] And minmax#[2,2] > minmax#[0,2] Then
					For Local i% = 0 To 2
						v[i].uv(v[i].y/minmax#[1,2],v[i].z/minmax#[2,2])
					Next
				EndIf
			Case MSPHERE%
				Local grad#[3]
					Local maxgrad#=0
					Local mingrad#=0
					For Local i%=0 To 2
						grad[i]=ATan2(v[i].x,v[i].z)
						If grad[i]>maxgrad Then maxgrad = grad[i]
						If grad[i]<mingrad Then mingrad = grad[i]
					Next
					If  maxgrad - mingrad > 180 Then
						For Local i%=0 To 2
							If grad[i] <0 Then grad[i]=grad[i]+360
							'Print grad[i]
						Next
					EndIf
					Local us#[3]
					For Local i%=0 To 2
						us[i]=grad[i]/90.0
					Next
					For Local i%=0 To 2
						v[i].uv(us[i],Sin(v[i].y*Pi))
					Next

			Case MZYLINDER%
				If (Floor(v[0].y*100.0)=Floor(v[1].y*100.0)) And (Floor(v[0].y*100.0)= Floor(v[2].y*100.0)) Then
					For Local i%=0 To 2
						v[i].uv(v[i].x-0.5,v[i].z-0.5)
					Next
				Else
					Local grad#[3]
					Local maxgrad#=0
					Local mingrad#=0
					For Local i%=0 To 2
						grad[i]=ATan2(v[i].x,v[i].z)
						If grad[i]>maxgrad Then maxgrad = grad[i]
						If grad[i]<mingrad Then mingrad = grad[i]
					Next
					If  maxgrad - mingrad > 180 Then
						For Local i%=0 To 2
							If grad[i] <0 Then grad[i]=grad[i]+360
							'Print grad[i]
						Next
					EndIf
					Local us#[3]
					For Local i%=0 To 2
						us[i]=grad[i]/90.0
					Next
					For Local i%=0 To 2
						v[i].uv(us[i],v[i].y+0.5)
					Next
					
				EndIf
			
		End Select
		For Local i = 0 To 2
			VertexTexCoords(surf,v[i].id,v[i].u,v[i].v)
		Next
	End Method
	Method renew(surface:TSurface)
		For Local i=0 To 2
			v[i].id=AddVertex(surface,v[i].x,v[i].y,v[i].z,v[i].u,v[i].v)	
		Next
		id=AddTriangle(surface,v[0].id,v[1].id,v[2].id)
	End Method
EndType

Type T3dsurface
	Global lasttrisid%
	Global lastsurface:T3dsurface
	Global lasttris:T3dtris
	Field texx#,texy#,texsx#,texsy#,texr#
	Field trislist:TList
	Field brush:TBrush
	Field name$
	Field surface:TSurface
	Function neu:T3dsurface(name$,mesh:TMesh,BrushTexturepath$="")
		Local out:T3dsurface=New t3dsurface
		Print name
		out.name=name
		out.texx=0
		out.texy=0
		out.texr=0
		out.texsx#=1
		out.texsy#=1
		If BrushTexturepath="" Then
			out.brush = CreateBrush()
		Else
			Print "BrushTexturepath: "+ BrushTexturepath
			out.brush = LoadBrush(BrushTexturepath)
		EndIf
		out.surface=CreateSurface(mesh)
		PaintSurface(out.surface,out.brush)
		Return out
	End Function
	Method renew()
		If trislist=Null Then trislist=New TList
		For Local tris:T3dtris = EachIn trislist
			tris.renew(surface)
		Next
	End Method
	Method addtris(tris:T3dtris,user%=0)
		If trislist=Null Then trislist=New TList
		trislist.addlast(tris)
		tris.renew(surface)
		If user=1 Then
			lasttrisid =tris.id
			lastsurface=Self
			lasttris=tris
		EndIf
	End Method
	Function turnlast()
		If lasttris Then
			Local newtris:T3dtris=lasttris
			lastsurface.removetris(lasttrisid-1)
			lasttris=Null
			newtris.turn()
			lastsurface.addtris(newtris)
			lasttris=newtris
			lasttrisid=newtris.id
		EndIf
	End Function
	Method removetris(id%)
		TSurface.removetri(surface,id)
		For Local tris:T3dtris = EachIn trislist
			If tris.id = id+1 Then
				trislist.remove(tris)
				tris=Null
			EndIf
		Next
	End Method
	Method updateuv(mode%)
		If trislist Then
			For Local tris:T3dtris= EachIn trislist
				tris.update(mode,surface)
			Next
		EndIf
	End Method
	Method save(outfile:TStream)
		WriteLine (outfile,"SURF")
		WriteLine (outfile,name)
		WriteLine (outfile,Replace(TextureName(brush.tex[0]),CurrentDir()+"/",""))
		WriteFloat (outfile,texx)
		WriteFloat (outfile,texy)
		WriteFloat (outfile,texsx)
		WriteFloat (outfile,texsy)
		WriteFloat (outfile,texr)				
		WriteInt (outfile,trislist.count())
		If trislist Then
			For Local i:T3dtris = EachIn trislist
				i.save(outfile)
			Next
		EndIf
	End Method
	Function Load:T3dsurface(infile:TStream,mesh:TMesh)
		Local name$=infile.ReadLine()
		Local texname$=infile.ReadLine()
		Local surf:T3dsurface=neu(name,mesh,texname)
		surf.texx=ReadFloat(infile)
		surf.texy=ReadFloat(infile)
		surf.texsx=ReadFloat(infile)
		surf.texsy=ReadFloat(infile)
		surf.texr=ReadFloat(infile)
		surf.brush.tex[0].PositionTexture(surf.texx,surf.texy)
		surf.brush.tex[0].ScaleTexture(surf.texsx,surf.texsy)
		surf.brush.tex[0].RotateTexture(surf.texr)	
		Local count%=ReadInt(infile)
		For Local i% = 0 To count -1
			If ReadLine(infile) = "TRIS" Then
				Local tris:T3dtris=t3dtris.Load(infile)
				surf.addtris(tris)
			Else
				Notify "problem in infile"
			EndIf
		Next 
		Return surf
	End Function
End Type

Type T3dmesh
	Global surfaces:TList
	Global Mesh:TMesh
	Global UVMODE%
	Function clean()
		For Local surf:T3dsurface= EachIn surfaces
			If surf.trislist Then surf.trislist.clear()
			surf.texx=0
			surf.texy=0
			surf.texr=0
			surf.texsx=1
			surf.texsy=1
		Next
		setpickmode()
	End Function
	Function clear()
		For Local surf:T3dsurface= EachIn surfaces
			If surf.trislist Then surf.trislist.clear()
			surf=Null
		Next
		surfaces.clear()
		FreeEntity(mesh)
		mesh=CreateMesh()
		setpickmode()
	End Function
	Function setpickmode()
		Local newmesh:TMesh=CreateMesh()
		For Local i =  0 To mesh.surf_list.count()-1
			For Local surf:T3dsurface= EachIn surfaces
				If surf.surface=TSurface(mesh.surf_list.valueatindex(i)) Then
					surf.surface=CreateSurface(newmesh)
					surf.renew()
					PaintSurface(surf.surface,surf.brush)
					surf.updateuv(UVMODE)
				EndIf
			Next			
		Next
		FreeEntity mesh
		UpdateNormals(newmesh)
		EntityPickMode(newmesh,2,1)
		mesh=newmesh
	End Function
	Function removetris(surface:TSurface,tris)
		For Local surf:T3dsurface= EachIn surfaces
			If surf.surface=surface Then
				surf.removetris(tris)
			EndIf
		Next
		setpickmode()
	End Function
	Function neu()
		mesh=CreateMesh()
		
		If surfaces= Null Then surfaces= New TList
		surfaces.clear()
	End Function
	Function getsurfacebyname:T3dsurface(name$)
		If surfaces= Null Then
			surfaces= New TList
			Notify "Surfaceliste musste neu erstellt werden. Gesuchtes Surface: "+name
		EndIf
		For Local search:T3dsurface= EachIn surfaces
			If search.name = name Then Return search
		Next
		Notify "Surface nicht in Liste. Gesuchtes Surface: "+name
		Return Null
	End Function
	Function getsurfacename$(surf:TSurface)
		If surfaces= Null Then
			surfaces= New TList
			Notify "Surfaceliste musste neu erstellt werden."
		EndIf
		For Local search:T3dsurface= EachIn surfaces
			If search.surface = surf Then Return search.name
		Next
		Notify "Surface nicht in Liste."
		Return Null
	End Function
	Function loadsurfacefile(path$)
		neu()
		Local infile:TStream=ReadFile(path)
		Repeat
			Local out:T3dsurface = T3dsurface.neu(ReadLine(infile),mesh,"gfx/testtexture.bmp")
			surfaces.addlast(out)
		Until Eof(infile)
		CloseFile(infile)
	End Function
	Function save(outfile:TStream)
		WriteLine(outfile,"MESH")
		For Local i:T3dsurface = EachIn surfaces
			i.save(outfile)
		Next
	End Function
	Function Load(infile:TStream)
		clean()
		clear()
		neu()
		Repeat
			If ReadLine(infile)="SURF" Then
				surfaces.addlast(T3dsurface.Load(infile,mesh))
			EndIf
		Until Eof(infile)
	End Function
End Type

Type Ttilecreator
	Global anz%
	Global mesh:TMesh
	Global surface:TSurface[4]
	Global Texture:TTexture
	Global punkte:T3dortsvektor[3]
	Global brush:TBrush[4]
	Global texmode%

	Function wahl(mausmode=0)
		Local entity:TEntity = CameraPick(Tbedienung.cam, Tbedienung.mx, Tbedienung.my)
		Select mausmode
		Case 0
			If Not(entity = Null) And Not(entity =T3dmesh.Mesh) Then
				punkte[anz]=tanfasspunkt.getpos(PickedX(),PickedY(),PickedZ())
				HideEntity tanfasspunkt.find(PickedX(),PickedY(),PickedZ()).mesh

				Tselectionsprite.setpos(punkte[anz])
				anz=anz+1
				If anz= 3 Then
					tanfasspunkt.showall()
					anz =0
					If SelectedGadgetItem(tbedienung.texbox) = - 1 Then
						SelectGadgetItem(tbedienung.texbox,0)
					EndIf
				
					Local surf:t3dsurface=T3dmesh.getsurfacebyname(GadgetItemText(tbedienung.texbox,SelectedGadgetItem(tbedienung.texbox)))
					surf.addtris(T3dtris.neu(t3dvertice.conv_ap(punkte[0]),t3dvertice.conv_ap(punkte[1]),t3dvertice.conv_ap(punkte[2])),1)
					T3dmesh.setpickmode()
				EndIf
			EndIf
		Case 1
			If Not(entity = Null) Then
				Tanfasspunkt.anordnen(PickedX(),PickedY(),PickedZ(),Tanfasspunkt.ls/2.0)
				Tanfasspunkt.scaleall(0.1/(SliderValue(tbedienung.anzslider)+1)*tanfasspunkt.ls)
				PositionEntity(Tbedienung.worldpivot,PickedX(),PickedY(),PickedZ())
			EndIf
		Case 2
			If entity =T3dmesh.Mesh Then
				Local surf:TSurface = PickedSurface()
				Local vertid%=PickedTriangle()
				t3dmesh.removetris(surf,vertid)
				T3dmesh.setpickmode()
			EndIf
		Case 3
			If entity =T3dmesh.Mesh Then
				Local surf:TSurface = PickedSurface()
				Local name$= t3dmesh.getsurfacename(surf)
				For Local i% = 0 To tbedienung.texbox.itemcount()-1
					If GadgetItemText(tbedienung.texbox,i)=name Then SelectGadgetItem(tbedienung.texbox,i)
				Next
			EndIf
		EndSelect
		
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
		o.x=-y
		o.y=x
	ElseIf rotation > 135 And rotation <=225 Then
		o.x=-x
		o.y=-y
	ElseIf rotation > 225 And rotation <=315 Then
		o.x=y
		o.y=-x
	EndIf
	Return o
End Function

Type Tbedienung
	Global win:TGadget
	Global can:TGadget
	Global cam:TCamera
	Global Light:TLight
	Global up_key:Int
	Global down_key:Int
	Global left_key:Int
	Global right_key:Int
	Global t_key
	Global mx,my,rotx#,roty#
	Global left_mouse:Int
	Global right_mouse:Int
	' used by fps code
	Global old_ms:Int=MilliSecs()
	Global renders:Int
	Global FPS:Int
	Global Worldpivot:TPivot
	
	Global auswahlxmin:tgadget
	Global auswahlxmax:tgadget
	Global auswahlymin:tgadget
	Global auswahlymax:tgadget
	Global auswahlzmin:tgadget
	Global auswahlzmax:tgadget
	Global auswahlxyzbutton:Tgadget
							
	Global filemenu:TGadget
	Global editmenu:TGadget
	Global helpmenu:TGadget
	Global texmenu:TGadget
	Global okbtn:TGadget
	Global cancelbtn:TGadget
	Global Wireframes%
	Global newwin:TGadget
	Global textfieldx:TGadget,textfieldy:TGadget,textfieldz:TGadget
	Global zoombtn:TGadget
	Global delbtn:TGadget
	Global zoomresetbtn:TGadget
	Global turnbtn:TGadget
	Global posbtn:TGadget
	Global texbtn:TGadget
	Global texBox:TGadget
	Global treeview:TGadget
	Global zoomslider:TGadget
	Global anzslider:TGadget
	Global mouseselctmode%
	Global texscalebtn:TGadget
	Global texmovebtn:TGadget
	Global texrotatebtn:TGadget
	Global getsurfbtn:TGadget
	Global texmode%
	Global gitterposx%,gitterposy%,gitterposz%
	
	Const MENU_NEW=101
	Const MENU_OPEN=102
	Const MENU_SAVE=103
	Const MENU_EXIT=105
	Const MENU_WIREFRAMES=106
	Const MENU_CENTER=107
	Const MENU_ABOUT=108
	Const MENU_SAVE3DS=109
	Const MENU_TRANSPARENT=110
	Const MENU_TEXMODETRIS=111
	Const MENU_TEXMODECUBE=112
	Const MENU_TEXMODESPHERE=113
	Const MENU_TEXMODECYLINDER=114
	Const MENU_TEXMODECUBE2=115
	Const MENU_TEXMODEPLANAR=116

	Function Erstelle()
		SetGraphicsDriver GLGraphicsDriver(),GRAPHICS_BACKBUFFER|GRAPHICS_DEPTHBUFFER
		EnablePolledInput()
		
		win:TGadget=CreateWindow("mergeator", 00, 00, 612, 550 )

		filemenu=CreateMenu("&File",0,WindowMenu(win))
		CreateMenu"&New",MENU_NEW,filemenu,KEY_N,MODIFIER_COMMAND
		CreateMenu"&Open",MENU_OPEN,filemenu,KEY_O,MODIFIER_COMMAND
		CreateMenu"",0,filemenu
		CreateMenu"&Save as",MENU_SAVE,filemenu,KEY_S,MODIFIER_COMMAND
		CreateMenu"&Export a 3DS Mesh",MENU_SAVE3DS,filemenu,KEY_E,MODIFIER_COMMAND
		CreateMenu"",0,filemenu
		CreateMenu"Exit",MENU_EXIT,filemenu,KEY_F4,MODIFIER_COMMAND

		editmenu=CreateMenu("&View",0,WindowMenu(win))
		CreateMenu "&Wireframes",MENU_WIREFRAMES,editmenu,KEY_W,MODIFIER_COMMAND
		CreateMenu "&Reset rotation",MENU_CENTER,editmenu,KEY_R,MODIFIER_COMMAND
		CreateMenu "change block &transpaenz",MENU_TRANSPARENT,editmenu,KEY_T,MODIFIER_COMMAND 

		texmenu=CreateMenu("Texturemode",0,WindowMenu(win))
		CreateMenu "Tris",MENU_TEXMODETRIS, texmenu
		CreateMenu "Cube",MENU_TEXMODECUBE, texmenu
		CreateMenu "Cube2",MENU_TEXMODECUBE2, texmenu
		CreateMenu "Planar",MENU_TEXMODEPLANAR, texmenu
		CreateMenu "Sphere",MENU_TEXMODESPHERE, texmenu
		CreateMenu "Cylinder",MENU_TEXMODECYLINDER, texmenu

		helpmenu=CreateMenu("&Help",0,WindowMenu(win))
		CreateMenu "&About",MENU_ABOUT,helpmenu

		UpdateWindowMenu win
		
		zoombtn=CreateButton( "Zoom", 375, 5, 80, 26,win)
		zoomresetbtn=CreateButton( "Reset Zoom", 455, 5, 120, 26,win)
		delbtn=CreateButton( "Delete", 375, 30, 80, 26,win)
		turnbtn=CreateButton( "turn last tris", 455, 30, 120, 26,win)
		getsurfbtn=CreateButton( "get surface", 375, 55, 80, 26,win)
		texrotatebtn=CreateButton( "rotate texture", 455, 55, 120, 26,win)
		texscalebtn=CreateButton( "move texture", 375, 80, 80, 26,win)
		texmovebtn=CreateButton( "scale texture", 455, 80, 120, 26,win)
		auswahlxyzbutton=CreateButton( "change X/Y/Z", 480, 511, 120, 26,win)
		
		
		SetGadgetLayout zoombtn,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout delbtn,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout zoomresetbtn,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout turnbtn,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout texscalebtn,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout texmovebtn,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout getsurfbtn,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout texrotatebtn,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout auswahlxyzbutton,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		
		anzslider=CreateSlider(375, 105, 200, 16,win,SLIDER_SCROLLBAR|SLIDER_HORIZONTAL)
		SetSliderRange(anzslider,1,9)
		SetSliderValue(anzslider,1)
		SetGadgetLayout anzslider,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		
		Local lbl2:TGadget=CreateLabel( "Texture: ", 375,130,50,20, win, LABEL_LEFT )
		texBox = CreateComboBox( 435,130,120,20, win, 0 )

		Local infile:TStream=ReadFile("config\surfacenames.txt")
		Repeat
			AddGadgetItem( texBox, ReadLine(infile))
		Until Eof(infile)
		CloseFile(infile)
		SelectGadgetItem(tbedienung.texbox,0)
		
		SetGadgetLayout texBox ,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout lbl2 ,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		

		treeview:TGadget=CreateTreeView(375,160,210,280,win)
		Fill_treeview(treeview:TGadget,CurrentDir()+"/tex")
		SetGadgetLayout treeview,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		
		texbtn=CreateButton( "set Texture", 375, 450, 120, 26,win)
		SetGadgetLayout texbtn,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		
		zoomslider=CreateSlider(375, 480, 200, 16,win,SLIDER_SCROLLBAR|SLIDER_HORIZONTAL)
		SetSliderRange(zoomslider,1,20)
		SetSliderValue(zoomslider,1)
		SetGadgetLayout zoomslider,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		
		auswahlxmin=CreateTextField(375,500,30,20,win)
		auswahlxmax=CreateTextField(375,525,30,20,win)
		
		auswahlymin=CreateTextField(410,500,30,20,win)
		auswahlymax=CreateTextField(410,525,30,20,win)
		
		auswahlzmin=CreateTextField(445,500,30,20,win)
		auswahlzmax=CreateTextField(445,525,30,20,win)
		
		SetGadgetLayout auswahlxmin ,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout auswahlxmax ,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout auswahlymin ,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout auswahlymax ,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout auswahlzmin ,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		SetGadgetLayout auswahlzmax ,0, EDGE_ALIGNED , EDGE_ALIGNED , 0
		
		SetGadgetText(auswahlxmin,"0")
		SetGadgetText(auswahlxmax,"0")
		SetGadgetText(auswahlymin,"0")
		SetGadgetText(auswahlymax,"0")
		SetGadgetText(auswahlzmin,"0")
		SetGadgetText(auswahlzmax,"0")
		
		
		can:TGadget=CreateCanvas(0,0,ClientWidth(win)-250,ClientHeight(win),win,0)
		SetGadgetLayout can, 1,1,1,1

		TGlobal.width=ClientWidth(can)
		TGlobal.height=ClientHeight(can)

		TGlobal.depth=32
		TGlobal.mode=0
		TGlobal.rate=60

		setgraph()

		TGlobal.GraphicsInit()
		Worldpivot=CreatePivot()
		cam:TCamera=CreateCamera(Worldpivot)
		PositionEntity cam,0,0,-2
		CameraRange(cam,0.1,30)
		CameraClsColor(cam,239,228,176)
		
		Local light:TLight=CreateLight(1)
		RotateEntity light,25,65,12
		
		T3dmesh.loadsurfacefile("config\surfacenames.txt")
		Tselectionsprite.makethree()
		
		CreateTimer( 60 )
		
		
	End Function
	Function setgraph()
		SetGraphics CanvasGraphics(can)
	End Function
	Function update()
		WaitEvent()
	
		Select EventID()
			Case EVENT_MENUACTION
				Select EventData()
					Case MENU_NEW
						If Confirm("All data will be lost.") Then
							T3dmesh.clean()
						EndIf
					Case MENU_SAVE
						Local filter$="Tile Files:tile;All Files:*"
						Local filename$=RequestFile( "Select tile file to save",filter$ ,1)
						If Not(filename = "") Then
							Local outstream:TStream=WriteFile(filename$)
							t3dmesh.save(outstream)
							CloseFile(outstream)
						EndIf
					Case MENU_SAVE3DS
						Local filter$="3DS Mesh:3ds;All Files:*"
						Local filename$=RequestFile( "Select 3DS file to export",filter$ ,1)
						If Not(filename = "") Then
							savemesh3ds(T3dmesh.Mesh,filename)
						EndIf
					Case MENU_OPEN
						If Confirm("All data will be lost.") Then
							Local filter$="Tile Files:tile;All Files:*"
							Local filename$=RequestFile( "Select tile file to open",filter$ )
							If Not(filename = "") Then
								Local instream:TStream=ReadFile(filename$)
								If ReadLine(instream)="MESH" Then t3dmesh.Load(instream)
								CloseFile(instream)
							EndIf
						EndIf
					Case MENU_EXIT
						End
					Case MENU_CENTER
						rotx=0
						roty=0
						RotateEntity Tbedienung.worldpivot,roty,-rotx,0
					Case MENU_ABOUT
						Notify "from Jan Kuhnert Germany, www.blitzforum.de"
					Case MENU_WIREFRAMES
						wireframes=1-wireframes
						Wireframe wireframes
					Case MENU_TRANSPARENT
						Tanfasspunkt.Transparenz=Tanfasspunkt.Transparenz-0.2
						If Tanfasspunkt.Transparenz < 0 Then Tanfasspunkt.Transparenz = 1
						Tanfasspunkt.alphaall(Tanfasspunkt.transparenz#)
					Case MENU_TEXMODETRIS
						T3dmesh.UVmode=T3dtris.mtris
						T3dmesh.setpickmode()
					Case MENU_TEXMODECUBE
						T3dmesh.UVmode=T3dtris.mcube
						T3dmesh.setpickmode()
					Case MENU_TEXMODECUBE2
						T3dmesh.UVmode=T3dtris.mcube2
						T3dmesh.setpickmode()
						
					Case MENU_TEXMODESPHERE
						T3dmesh.UVmode=T3dtris.msphere
						T3dmesh.setpickmode()
						
					Case MENU_TEXMODECYLINDER
						T3dmesh.UVmode=T3dtris.mzylinder
						T3dmesh.setpickmode()
					Case MENU_TEXMODEPLANAR
						T3dmesh.UVmode=T3dtris.mplane
						T3dmesh.setpickmode()
						
				End Select
			Case EVENT_KEYDOWN
				Local mover:TXYInt
				Select EventData()
					Case KEY_ESCAPE
						End
					Case KEY_UP
						up_key=True
						mover=calcmove(0,1,rotx)
						gitterposx%=gitterposx-mover.x
						gitterposy%=gitterposy+mover.y
						PositionEntity worldpivot,gitterposx,gitterposz,gitterposy
						Tanfasspunkt.anordnen(gitterposx,gitterposz,gitterposy,1)
						Tanfasspunkt.scaleall(0.1/(SliderValue(tbedienung.anzslider)+1)*tanfasspunkt.ls)
						tanfasspunkt.showall()
					Case KEY_T
						t_key=True
					Case KEY_DOWN
						down_key=True
						mover=calcmove(0,-1,rotx)
						gitterposx%=gitterposx-mover.x
						gitterposy%=gitterposy+mover.y
						PositionEntity worldpivot,gitterposx,gitterposz,gitterposy
						Tanfasspunkt.anordnen(gitterposx,gitterposz,gitterposy,1)
						Tanfasspunkt.scaleall(0.1/(SliderValue(tbedienung.anzslider)+1)*tanfasspunkt.ls)
						tanfasspunkt.showall()
					Case KEY_LEFT
						left_key=True
						mover=calcmove(-1,0,rotx)
						gitterposx%=gitterposx+mover.x
						gitterposy%=gitterposy-mover.y
						PositionEntity worldpivot,gitterposx,gitterposz,gitterposy
						Tanfasspunkt.anordnen(gitterposx,gitterposz,gitterposy,1)
						Tanfasspunkt.scaleall(0.1/(SliderValue(tbedienung.anzslider)+1)*tanfasspunkt.ls)
						tanfasspunkt.showall()
					Case KEY_RIGHT
						right_key=True
						mover=calcmove(1,0,rotx)
						gitterposx%=gitterposx+mover.x
						gitterposy%=gitterposy-mover.y
						PositionEntity worldpivot,gitterposx,gitterposz,gitterposy
						Tanfasspunkt.anordnen(gitterposx,gitterposz,gitterposy,1)
						Tanfasspunkt.scaleall(0.1/(SliderValue(tbedienung.anzslider)+1)*tanfasspunkt.ls)
						tanfasspunkt.showall()
					Case KEY_A
						gitterposz%=gitterposz+1
						PositionEntity worldpivot,gitterposx,gitterposz,gitterposy
						Tanfasspunkt.anordnen(gitterposx,gitterposz,gitterposy,1)
						Tanfasspunkt.scaleall(0.1/(SliderValue(tbedienung.anzslider)+1)*tanfasspunkt.ls)
						tanfasspunkt.showall()
					Case KEY_Z
						gitterposz%=gitterposz-1
						PositionEntity worldpivot,gitterposx,gitterposz,gitterposy
						Tanfasspunkt.anordnen(gitterposx,gitterposz,gitterposy,1)
						Tanfasspunkt.scaleall(0.1/(SliderValue(tbedienung.anzslider)+1)*tanfasspunkt.ls)
						tanfasspunkt.showall()
				EndSelect
			Case EVENT_MOUSEDOWN
			
				Select EventData()
					Case MOUSE_LEFT
						mx=MouseX()
						my=MouseY()
						Ttilecreator.wahl(mouseselctmode)
						mouseselctmode=0
						left_mouse=True
					Case MOUSE_RIGHT
						right_mouse=True
				End Select
			Case EVENT_MOUSEUP
			
				Select EventData()
					Case MOUSE_LEFT
						left_mouse=False
						texmode=0
					Case MOUSE_RIGHT
						right_mouse=False
						
				End Select
			Case EVENT_KEYUP
			
				Select EventData()
					Case KEY_UP
						up_key=False
					Case KEY_DOWN
						down_key=False			
					Case KEY_LEFT
						left_key=False
					Case KEY_RIGHT
						right_key=False	
					Case KEY_T
						t_key=False

				EndSelect
	
			Case EVENT_WINDOWCLOSE
				Select EventSource()
					Case	win
				            End
					Case newwin
						FreeGadget newwin
						EnableGadget win
						ActivateGadget win
				End Select
			Case EVENT_APPTERMINATE
				End
			Case EVENT_WINDOWSIZE
			
				TGlobal.width=ClientWidth(can)
				TGlobal.height=ClientHeight(can)
	
				cam.CameraViewport(0,0,ClientWidth(can),ClientHeight(can))
								
				DebugLog "EVENT_WINDOWSIZE" 
	
			Case EVENT_TIMERTICK
				Local rx#=MouseXSpeed()
				Local ry#=MouseYSpeed()
				If right_mouse Or t_key Then
					rotx=rotx+rx
					roty=roty+ry
					RotateEntity Tbedienung.worldpivot,roty,-rotx,0
				EndIf
				If left_mouse And texmode Then
					Local surf:t3dsurface=T3dmesh.getsurfacebyname(GadgetItemText(tbedienung.texbox,SelectedGadgetItem(tbedienung.texbox)))
					Select texmode
					Case 1
						surf.texx=surf.texx-rx/100.0
						surf.texy=surf.texy-ry/100.0
						
						surf.brush.tex[0].PositionTexture(surf.texx,surf.texy)
					Case 2
						surf.texsx=surf.texsx+rx/100.0
						surf.texsy=surf.texsy+ry/100.0
						
						surf.brush.tex[0].ScaleTexture(surf.texsx,surf.texsy)
					Case 3
						surf.texr=surf.texr+rx/2.0+ry/2.0
						surf.brush.tex[0].RotateTexture(surf.texr)		
					End Select
					
				EndIf
				RedrawGadget can
	              
			Case EVENT_GADGETPAINT
				
				SetGraphics CanvasGraphics(can)

				RenderWorld()
				
				Flip
			Case EVENT_GADGETACTION 
				Select EventSource()
					Case	okbtn

					Case cancelbtn

					Case texbtn
						If SelectedGadgetItem(tbedienung.texbox) = - 1 Then
							SelectGadgetItem(tbedienung.texbox,0)
						EndIf
						Local surf:T3dsurface = T3dmesh.getsurfacebyname(GadgetItemText(tbedienung.texbox,SelectedGadgetItem(tbedienung.texbox)) )
						surf.brush=LoadBrush(CurrentDir()+"/tex"+get_path_from_treeview$(treeview))
						PaintSurface(surf.surface,surf.brush)
					Case zoomslider
						CameraZoom(cam,SliderValue(zoomslider)/2.0+0.5)
					Case anzslider
						Tanfasspunkt.resetanz(SliderValue(anzslider)+2)
						Tanfasspunkt.scaleall(0.1/(SliderValue(anzslider)+1)*tanfasspunkt.ls)
					Case zoombtn
						mouseselctmode=1
					Case delbtn
						mouseselctmode=2
					Case getsurfbtn
						mouseselctmode=3
					Case zoomresetbtn
						Tanfasspunkt.anordnen(0,0,0,1)
						Tanfasspunkt.scaleall(0.1/(SliderValue(tbedienung.anzslider)+1)*tanfasspunkt.ls)
						PositionEntity(Tbedienung.worldpivot,0,0,0)
					Case turnbtn
						T3dsurface.turnlast()
						T3dmesh.setpickmode()
					Case texscalebtn
						mouseselctmode=-1
						texmode=1
					Case texmovebtn
						mouseselctmode=-1
						texmode=2
					Case texrotatebtn
						mouseselctmode=-1
						texmode=3
					Case auswahlxyzbutton
						Tanfasspunkt.vx=Int(TextFieldText(auswahlxmin))
						Tanfasspunkt.bx=Int(TextFieldText(auswahlxmax))
						Tanfasspunkt.vy=Int(TextFieldText(auswahlymin))
						Tanfasspunkt.by=Int(TextFieldText(auswahlymax))
						Tanfasspunkt.vz=Int(TextFieldText(auswahlzmin))
						Tanfasspunkt.bz=Int(TextFieldText(auswahlzmax))
						Tanfasspunkt.anordnen(Tanfasspunkt.lx,Tanfasspunkt.ly,Tanfasspunkt.lz,Tanfasspunkt.ls)
				End Select
		EndSelect
	End Function
EndType

Tbedienung.erstelle()
Tbedienung.setgraph()

Tanfasspunkt.createfield(3)
Tanfasspunkt.scaleall(0.05)
Tselectionsprite.neu()
'Tgitter.Create(2,2,2)

While True

	Tbedienung.update()
	
Wend


Function SaveMesh3DS(mesh:TMesh,filename$,texfile$="",diffuse=$CCCCCC)
 Local matname$,mnlen,tflen,si,mi,objname$,onlen
 Local tssize,tmsize,tfsize,tosize,tcsize,tvsize,otsize
 Local eobjsize,ematsize,editsize,i,l
 Local file:TStream
 Local piv:TPivot
 Local ent:TMesh
 Local surf:TSurface

 file=WriteFile(filename$)
 If Not file Return False 'fail code
 piv=CreatePivot() 'pivot To tform vertices
 matname$="Material"
 mnlen=Len(matname$)+1
 tflen=Len(texfile$)+1
 ent=mesh
 While ent
  For si=1 To CountSurfaces(ent)
   surf=GetSurface(ent,si)
   mi=mi+1
   If mi<10 Then objname$="mesh0"+mi Else objname$="mesh"+mi
   onlen=Len(objname$)+1
   tssize=6+(CountTriangles(surf)*4) 'SizeOf(Tri_Smooth)
   tmsize=8+mnlen+(CountTriangles(surf)*2) 'SizeOf(Tri_Material)
   tfsize=8+(CountTriangles(surf)*4*2)+tmsize+tssize 'SizeOf(Tri_FaceList)
   tosize=8+(CountVertices(surf)*2) 'SizeOf(Tri_VertexOptions)
   tcsize=8+(CountVertices(surf)*2*4) 'SizeOf(Tri_MappingCoords)
   tvsize=8+(CountVertices(surf)*3*4) 'SizeOf(Tri_VertexList)
   otsize=6+tvsize+tcsize+tosize+54+tfsize 'SizeOf(Object_TriMesh)
   eobjsize=eobjsize+(6+onlen+otsize) 'SizeOf(All_Edit_Objects)
  Next

  ent=NextChild(ent)

 Wend
 ematsize=6+(6+mnlen)+(15*3)+(14*6)+10 'SizeOf(Edit_Material)
 If tflen>1 Then ematsize=ematsize+(6+(6+tflen)) 'Mat_TextureMap1
 editsize=6+10+ematsize+10+eobjsize 'SizeOf(Edit3DS)

 'Main3DS Chunk
 WriteShort file,$4D4D 'wChunkID, Main3DS
 WriteInt file,6+10+editsize 'dwChunkSize, FileSize

 'Version3DS Chunk
 WriteShort file,$0002 'wChunkID, Version3DS
 WriteInt file,10 'dwChunkSize, SizeOf(Version3DS)
 WriteInt file,3 'ChunkData, dwVersion3DS

 'Edit3DS Chunk, Main3DS Subchunk
 WriteShort file,$3D3D 'wChunkID, Edit3DS
 WriteInt file,editsize 'dwChunkSize, editsize=6+ematsize+eobjsize
 'Edit_MeshVersion, Edit3DS Subchunk
 WriteShort file,$3D3E 'wChunkID, Edit_MeshVersion
 WriteInt file,10 'dwChunkSize, SizeOf(Edit_MeshVersion)
 WriteInt file,3 'ChunkData, dwMeshVersion

 'Edit_Material Chunk, Edit3DS Subchunk
 WriteShort file,$AFFF 'wChunkID, Edit_Material
 WriteInt file,ematsize 'dwChunkSize, ematsize=6+(6+mnlen)+(15*3)
 'Mat_Name01 Chunk, Edit_Material Subchunk
 WriteShort file,$A000 'wChunkID, Mat_Name01
 WriteInt file,6+mnlen 'dwChunkSize, SizeOf(Mat_Name01)
 WriteStringAscii(file,matname$) 'ChunkData, material name
 WriteByte file,0 'ChunkData, Ascii-z Null Byte
 'Mat_Ambient Chunk, Edit_Material Subchunk
 WriteShort file,$A010 'wChunkID, Mat_Ambient
 WriteInt file,15 'dwChunkSize, SizeOf(Mat_Ambient)
 WriteShort file,$0011 'wChunkID, bRGB
 WriteInt file,9 'dwChunkSize, SizeOf(bRGB)
 WriteRGBColor(file,$666666) 'ChunkData, ambient color
 'Mat_Diffuse Chunk, Edit_Material Subchunk
 WriteShort file,$A020 'wChunkID, Mat_Diffuse
 WriteInt file,15 'dwChunkSize, SizeOf(Mat_Diffuse)
 WriteShort file,$0011 'wChunkID, bRGB
 WriteInt file,9 'dwChunkSize, SizeOf(bRGB)
 WriteRGBColor(file,diffuse) 'ChunkData, diffuse color
 'Mat_Specular Chunk, Edit_Material Subchunk
 WriteShort file,$A030 'wChunkID, Mat_Specular
 WriteInt file,15 'dwChunkSize, SizeOf(Mat_Specular)
 WriteShort file,$0011 'wChunkID, bRGB
 WriteInt file,9 'dwChunkSize, SizeOf(bRGB)
 WriteRGBColor(file,$FFFFFF) 'ChunkData, specular color
 'Mat_Shininess Chunk, Edit_Material Subchunk
 WriteShort file,$A040 'wChunkID, Mat_Shininess
 WriteInt file,14 'dwChunkSize, SizeOf(Mat_Shininess)
 WriteShort file,$0030 'wChunkID, wPercent
 WriteInt file,8 'dwChunkSize, SizeOf(wPercent)
 WriteShort file,75 'ChunkData, Shininess
 'Mat_ShininessStrength Chunk, Edit_Material Subchunk
 WriteShort file,$A041 'wChunkID, Mat_ShininessStrength
 WriteInt file,14 'dwChunkSize, SizeOf(Mat_ShininessStrength)
 WriteShort file,$0030 'wChunkID, wPercent
 WriteInt file,8 'dwChunkSize, SizeOf(wPercent)
 WriteShort file,20 'ChunkData, ShininessStrength
 'Mat_Transparency Chunk, Edit_Material Subchunk
 WriteShort file,$A050 'wChunkID, Mat_Transparency
 WriteInt file,14 'dwChunkSize, SizeOf(Mat_Transparency)
 WriteShort file,$0030 'wChunkID, wPercent
 WriteInt file,8 'dwChunkSize, SizeOf(wPercent)
 WriteShort file,0 'ChunkData, Transparency
 'Mat_TransparencyFalloff Chunk, Edit_Material Subchunk
 WriteShort file,$A052 'wChunkID, Mat_TransparencyFalloff
 WriteInt file,14 'dwChunkSize, SizeOf(Mat_TransparencyFalloff)
 WriteShort file,$0030 'wChunkID, wPercent
 WriteInt file,8 'dwChunkSize, SizeOf(wPercent)
 WriteShort file,0 'ChunkData, TransparencyFalloff
 'Mat_ReflectionBlur Chunk, Edit_Material Subchunk
 WriteShort file,$A053 'wChunkID, Mat_ReflectionBlur
 WriteInt file,14 'dwChunkSize, SizeOf(Mat_ReflectionBlur)
 WriteShort file,$0030 'wChunkID, wPercent
 WriteInt file,8 'dwChunkSize, SizeOf(wPercent)
 WriteShort file,0 'ChunkData, ReflectionBlur
 'Mat_SelfIllumination Chunk, Edit_Material Subchunk
 WriteShort file,$A084 'wChunkID, Mat_SelfIllumination
 WriteInt file,14 'dwChunkSize, SizeOf(Mat_SelfIllumination)
 WriteShort file,$0030 'wChunkID, wPercent
 WriteInt file,8 'dwChunkSize, SizeOf(wPercent)
 WriteShort file,0 'ChunkData, SelfIllumination
 'Edit_WireThickness Chunk, Edit_Material Subchunk
 WriteShort file,$A087 'wChunkID, Edit_WireThickness
 WriteInt file,10 'dwChunkSize, SizeOf(Edit_WireThickness)
 WriteFloat file,1 'fWireThickness
 'Mat_TextureMap1 Chunk, Edit_Material Subchunk
 If tflen>1
  WriteShort file,$A200 'wChunkID, Mat_TextureMap1
  WriteInt file,6+(6+tflen) 'dwChunkSize, SizeOf(Mat_TextureMap1)
  'Mat_TextureFilename, Mat_TextureMap1 Subchunk
  WriteShort file,$A300 'wChunkID, Mat_TextureFilename
  WriteInt file,6+tflen 'dwChunkSize, SizeOf(Mat_TextureFilename)
  WriteStringAscii(file,texfile$) 'ChunkData, texture filename
  WriteByte file,0 'ChunkData, Ascii-z Null Byte
 EndIf

 'Edit_OneUnit Chunk, Edit3DS Subchunk
 WriteShort file,$0100 'wChunkID, Edit_OneUnit
 WriteInt file,10 'dwChunkSize, SizeOf(Edit_OneUnit)
 WriteFloat file,1 'fOneUnit

 'calculate each mesh Object chunk sizes
 ent=mesh 
 mi=0

 While ent
  For si=1 To CountSurfaces(ent)
   surf=GetSurface(ent,si)
   mi=mi+1
   If mi<10 Then objname$="mesh0"+mi Else objname$="mesh"+mi
   onlen=Len(objname$)+1
   tssize=6+(CountTriangles(surf)*4) 'SizeOf(Tri_Smooth)
   tmsize=8+mnlen+(CountTriangles(surf)*2) 'SizeOf(Tri_Material)
   tfsize=8+(CountTriangles(surf)*4*2)+tmsize+tssize 'SizeOf(Tri_FaceList)
   tosize=8+(CountVertices(surf)*2) 'SizeOf(Tri_VertexOptions)
   tcsize=8+(CountVertices(surf)*2*4) 'SizeOf(Tri_MappingCoords)
   tvsize=8+(CountVertices(surf)*3*4) 'SizeOf(Tri_VertexList)
   otsize=6+tvsize+tcsize+tosize+54+tfsize 'SizeOf(Object_TriMesh)
   eobjsize=6+onlen+otsize 'SizeOf(Edit_Object)
   'position/Rotate pivot To tform surface vertices
   PositionEntity piv,EntityX(ent,1),EntityY(ent,1),EntityZ(ent,1),1
   RotateEntity piv,EntityPitch(ent,1),EntityYaw(ent,1),EntityRoll(ent,1),1

   'Edit_Object Chunk, Edit3DS Subchunk
   WriteShort file,$4000 'wChunkID, Edit_Object
   WriteInt file,eobjsize 'dwChunkSize, eobjsize=6+onlen+otsize
   WriteStringAscii(file,objname$) 'ChunkData, Object name
   WriteByte file,0 'ChunkData, Ascii-z Null Byte

   'Object_TriMesh Chunk, Edit_Object Subchunk
   WriteShort file,$4100 'wChunkID, Object_TriMesh
   WriteInt file,otsize 'dwChunkSize, otsize=6+tvsize+tcsize+54+tfsize
   'Tri_VertexList Chunk, Object_TriMesh Subchunk
   WriteShort file,$4110 'wChunkID, Tri_VertexList
   WriteInt file,tvsize 'dwChunkSize, tvsize=8+(nVerts*3*4)
   WriteShort file,CountVertices(surf) 'ChunkData, wVerticesTotal
   For i=0 To CountVertices(surf)-1 'switch y And z axis
    TFormPoint VertexX(surf,i),VertexY(surf,i),VertexZ(surf,i),piv,Null
    WriteFloat file,TFormedX() 'fVertexX
    WriteFloat file,TFormedZ() 'fVertexY
    WriteFloat file,TFormedY() 'fVertexZ
   Next
   'Tri_MappingCoords Chunk, Object_TriMesh Subchunk
   WriteShort file,$4140 'wChunkID, Tri_MappingCoords
   WriteInt file,tcsize 'dwChunkSize, tcsize=8+(nVerts*2*4)
   WriteShort file,CountVertices(surf) 'ChunkData, wVerticesTotal
   For i=0 To CountVertices(surf)-1 'invert v coord
    WriteFloat file,VertexU(surf,i) 'fVertexU
    WriteFloat file,-VertexV(surf,i) 'fVertexV
   Next
   'Tri_VertexOptions Chunk, Object_TriMesh Subchunk
   WriteShort file,$4111 'wChunkID, Tri_VertexOptions
   WriteInt file,tosize 'dwChunkSize, tosize=8+(nVerts*2)
   WriteShort file,CountVertices(surf) 'ChunkData, wVerticesTotal
   For i=0 To CountVertices(surf)-1 'invert v coord
    WriteShort file,$0700 'AllVertexOptions
   Next
   'Tri_Local Chunk, Object_TriMesh Subchunk
   WriteShort file,$4160 'wChunkID, Tri_Local
   WriteInt file,54 'dwChunkSize, SizeOf(Tri_Local)
   For i=0 To 3 'X,Y,Z,Origin
    If i=0 Then WriteFloat file,1 Else WriteFloat file,0 'fLocal1
    If i=1 Then WriteFloat file,1 Else WriteFloat file,0 'fLocal2
    If i=2 Then WriteFloat file,1 Else WriteFloat file,0 'fLocal3
   Next
   'Tri_FaceList Chunk, Object_TriMesh Subchunk
   WriteShort file,$4120 'wChunkID, Tri_FaceList
   WriteInt file,tfsize 'dwChunkSize, tfsize=8+(nTris*4*2)+tmsize+tssize
   WriteShort file,CountTriangles(surf) 'ChunkData, wTrianglesTotal
   For i=0 To CountTriangles(surf)-1 'invert face order
    WriteShort file,TriangleVertex(surf,i,2) 'wTriangleVertexA
    WriteShort file,TriangleVertex(surf,i,1) 'wTriangleVertexB
    WriteShort file,TriangleVertex(surf,i,0) 'wTriangleVertexC
    WriteShort file,$0407 'wFaceFlags
   Next
   'Tri_Material Chunk, Tri_FaceList Subchunk
   WriteShort file,$4130 'wChunkID, Tri_Material
   WriteInt file,tmsize 'dwChunkSize, tmsize=8+mnlen+(nTris*2)
   WriteStringAscii(file,matname$) 'ChunkData, material name
   WriteByte file,0 'ChunkData, Ascii-z Null Byte
   WriteShort file,CountTriangles(surf) 'ChunkData, wTrianglesAssigned
   For i=0 To CountTriangles(surf)-1
    WriteShort file,i 'wTriangleIndex
   Next
   'Tri_Smooth Chunk, Tri_FaceList Subchunk
   WriteShort file,$4150 'wChunkID, Tri_Smooth
   WriteInt file,tssize 'dwChunkSize, tssize=6+(nTris*4)
   For i=0 To CountTriangles(surf)-1
    WriteInt file,1 'dwTriangleSmoothGroup
   Next

  Next

  ent=NextChild(ent)
 Wend

 FreeEntity piv
 CloseFile file
 Return True 'success code

End Function

Function WriteStringAscii(file:TStream,ascii$)
 'file=file handle, ascii$=ascii String

 Local i,char$
 For i=1 To Len(ascii$)
  char$=Mid(ascii$,i,1)
  WriteByte(file,Asc(char$))
 Next

End Function

Function WriteRGBColor(file:TStream,rgb)
 'file=file handle, rgb=3-Byte value

 WriteByte(file,(rgb And $FF0000) Shr 16) 'r
 WriteByte(file,(rgb And $00FF00) Shr 8) 'g
 WriteByte(file,(rgb And $0000FF)) 'b

End Function

Function NextChild:TMesh(ent:TMesh)
 'Returns Next child of entity as If it was on the same hierarchy level
 '"NextChild(entity)", by Beaker

 If CountChildren(ent)>0
  Return TMesh(GetChild(ent,1))
 EndIf
 Local siblingcnt
 Local foundunused=False
 Local foundent:TMesh=Null
 Local parent:TMesh
 Local sibling:TMesh
 While foundunused=False And Not(ent=Null)
  parent=TMesh(GetParent(ent))
  If Not(parent=Null)
   If CountChildren(parent)>1
    If GetChild(parent,CountChildren(parent))<>ent
     For siblingcnt=1 To CountChildren(parent)
      sibling=TMesh(GetChild(parent,siblingcnt))
      If sibling=ent
       foundunused=True
       foundent=TMesh(GetChild(parent,siblingcnt+1))
      EndIf
     Next
    EndIf
   EndIf
  EndIf
  ent=parent
 Wend
 Return foundent

End Function

Function Fill_treeview(tree:TGadget,pfad$="",node:TGadget=Null)
		'Local root:TGadget=TreeViewRoot(treeview)
		'Local help:TGadget=AddTreeViewNode("Help",root)
		'AddTreeViewNode "Topic 1",help

	If pfad="" Then
		pfad=CurrentDir()
	EndIf
	If node=Null
		node=TreeViewRoot(tree)
	EndIf
	Local dir=ReadDir(pfad)

	If Not dir RuntimeError "failed to read current directory"

	Repeat
		Local t$=NextFile( dir )
		If t="" Exit
		If t="." Or t=".." Continue
		Select FileType(pfad+"/"+t)
			Case 1
				If Upper(Right(t,4))=".JPG" Then
					AddTreeViewNode t,node
				EndIf
			Case 2
				Local newnode:TGadget=AddTreeViewNode(t,node)
				Fill_treeview(tree,pfad+"/"+t,newnode)
		End Select
	Forever

	CloseDir dir
End Function

Function get_path_from_treeview$(tree:TGadget,point:TGadget=Null)
	If point=Null Then point= SelectedTreeViewNode:TGadget(tree)
	If point <> TreeViewRoot(tree) Then
		Return get_path_from_treeview$(tree,GadgetGroup(point))+"/"+GadgetText(point)
	EndIf
End Function







