% NOTE that this file is latin1-encoded, to work around metauml to output �/�.
input metauml;
beginfig(1);

Class.frontpage("Etusivu")("Julkinen")
      ("Rekister�inti", "Sis��nkirjaus");

Class.calendar_view("Kalenterisivu")()
      ("Viikkon�kym�");
Class.calendar_edit("Kalenterien hallinta")()();
Class.calendar_public("Julkinen kalenterisivu")("Julkinen")();

Class.object_view("Kohdesivu")()();
Class.object_edit("Kohteen hallinta")()("Lis�ys", "Muokkaus");

topToBottom(50)
      (frontpage, calendar_view
      , object_view, object_edit);
leftToRight(50)(calendar_view, calendar_edit);
topToBottom(50)(calendar_edit, calendar_public);
Group.views
      (frontpage, calendar_view, object_view, object_edit
      ,calendar_public, calendar_edit);

drawObjects(views);

clink(associationUni)(frontpage, calendar_view);

clink(associationUni)(calendar_view, object_view);
clink(associationUni)(calendar_view, calendar_edit);

clink(associationUni)(object_view, object_edit);

clink(associationUni)(calendar_public, object_view);
endfig;
end
