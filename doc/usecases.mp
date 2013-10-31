input metauml;
beginfig(1);

% - styles ----------
iUsecase.foreColor:=(.93,.93,.93);
iUsecase.borderColor:=(.5,.5,.5);
iNote.foreColor:=(.92,.92,.92);
iGroup.left:=5;
iGroup.right:=5;
iGroup.top:=5;
iGroup.bottom:=5;
iGroup.boxed:=1;

% - actors ---------
Actor.stranger("Vieras");
Actor.user("K‰ytt‰j‰", "(sis‰‰nkirjautunut)");

% - usecases: auth ----
Usecase.register("Rekisterˆidy");
Usecase.login("Kirjaudu sis‰‰n");
Usecase.logout("Kirjaudu ulos");

% - usecases: calendar --
Usecase.create_cal("Luo kalenteri");
Usecase.delete_cal("Poista kalenteri");
Usecase.modify_cal("Muokkaa kalenterin", "ominaisuuksia");
Usecase.view_cal("Tarkastele kalenteria", "(sen kohteita)");

% - usecases: objects ---
Usecase.create_obj("Luo kohde");
Usecase.delete_obj("Poista kohde");
Usecase.modify_obj("Muokkaa kohdetta");
Usecase.view_obj("Tarkastele kohdetta");

% - usecases: stranger --
Usecase.view_other_cal("Tarkastele ei-omaa", "julkista kalenteria");
Usecase.suggest_other_obj("Lis‰‰ oma kohde");

% - Auth ---------------------------
leftToRight(80)(logout, login, register);
Group.auth(register, login, logout);

% - Calendar -----------------------
topToBottom(9)
      (create_cal, create_obj, modify_cal, modify_obj);
leftToRight(9)
      (create_cal, view_cal);
topToBottom(9)
      (view_cal, view_obj, delete_cal, delete_obj);
Group.own_cal                  
      ( create_cal, create_obj
      , modify_cal, modify_obj
      , delete_cal, delete_obj
      , view_cal, view_obj);

% - Collab. & view -----------------
topToBottom(10)(view_other_cal, suggest_other_obj);
Group.col_cal(view_other_cal, suggest_other_obj);

% - actors -------------------------
topToBottom(98)(logout, user);
topToBottom(20)(register, stranger);

% - all together -------------------
topToBottom(20)(login, own_cal);
topToBottom(25)(stranger, col_cal);

% - Notes --------------------------
Note.implicit_cal("1. kirjautumisella", "luodaan kalenteri");
Note.register_note("K‰ytt‰j‰nimi", "+ salasana");
topToBottom(20)(implicit_cal, login);
topToBottom(20)(register_note, register);

% ---------------------------------
auth.info.borderColor    := (.7, .7,  0);
own_cal.info.borderColor := ( 0, .7, .7);
col_cal.info.borderColor := (.7,  0, .7);

% - draw ----------
drawObjects(stranger, user, auth, own_cal, col_cal, implicit_cal, register_note);

% - case-case assoc. ------
clink(association)(register, login);
clink(association)(login, create_cal);
clink(association)(create_cal, view_cal);

% - notes ------------
clink(inheritance)(implicit_cal, login);
clink(inheritance)(register_note, register);

% - user ------------
clink(association)(user.human, logout);
clink(association)(user.human, own_cal);
link(association)
      (pathStepY(user.s, col_cal.s, -70));

% - stranger ----------
clink(association)(stranger.n, register.s);
clink(association)(stranger.human, login);
link(association)(stranger.s -- col_cal.n);

endfig;
end
