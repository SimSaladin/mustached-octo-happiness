input metauml;
beginfig(1);
iGroup.left:=5;
iGroup.right:=5;
iGroup.top:=5;
iGroup.bottom:=5;
iGroup.boxed:=1;

iAssoc.left:=6;
iAssoc.right:=6;
iAssoc.top:=6;
iAssoc.bottom:=6;

Class.U("K�ytt�j�")()();
Class.C("Kalenteri")()();
Class.E("Tapahtuma")()();
Class.T("Teht�v�")()();
Class.J("Muistiinpano")()();

leftToRight(30)(E,T,J);
Group.objects(E,T,J);
topToBottom(40)(U,C,objects);
objects.info.borderColor := (.6,.6,.6);

drawObjects(U,C,objects);
% ---------------------------------

% - objects -
clink(composition)(objects, C);

% - user -
clink(association)(U, C);
link(association)( pathStepX(U.w, objects.w, -108) );
link(association)( pathStepX(U.e, objects.e, 108) );

% ! ownership -
item(iAssoc)("1")(obj.ne = U.w);
item(iAssoc)("*")(obj.se = objects.w);
item(iAssoc)("omistaa")(obj.ne = (U.w - (45, 0)));
% cal
item(iAssoc)("1")(obj.ne = U.s);
item(iAssoc)("*")(obj.se = C.n);

% ! participiant -
item(iAssoc)("*")(obj.nw = U.e);
item(iAssoc)("*")(obj.sw = objects.e);
item(iAssoc)("on osallisena")(obj.nw = (U.e + (45, 0)));

% ! cal rels
item(iAssoc)("1..*")(obj.ne = C.s);
item(iAssoc)("*")(obj.se = objects.n);

% - event -
item(iAssoc)("*")(obj.ne = E.s);

endfig;
end
