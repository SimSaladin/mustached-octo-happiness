input metauml;
beginfig(1);

iUsecase.foreColor:=(.93,.93,.93);
iUsecase.borderColor:=(.5,.5,.5);
iNote.foreColor:=(.92,.92,.92);

iGroup.left:=5;
iGroup.right:=5;
iGroup.top:=5;
iGroup.bottom:=5;
iGroup.boxed:=1;
Actor.stranger("Vieras");
Actor.user("Käyttäjä", "(sisäänkirjautunut)");

Usecase.register("Rekisteröidy");
Usecase.login("Kirjaudu sisään");
Usecase.logout("Kirjaudu ulos");

Usecase.create_cal("Luo kalenteri");
Usecase.delete_cal("Poista kalenteri");
Usecase.view_cal("Tarkastele objekteja");

Usecase.create_obj("Luo objekti");
Usecase.delete_obj("Poista objekti");
Usecase.modify_obj("Muokkaa objektia");

Usecase.view_other_cal("Tarkastele ei-omaa", "julkista kalenteria");
Usecase.suggest_other_obj("Ehdota objektia");

% - Auth ---------------------------
leftToRight(40)(logout, login, register);
Group.auth(register, login, logout);

% - Modify -------------------------
topToBottom(5)(create_cal, view_cal, create_obj, modify_obj, delete_obj, delete_cal);
Group.own_cal( create_cal, view_cal, create_obj, modify_obj, delete_obj, delete_cal);

% - Collab. & view -----------------
topToBottom(10)(view_other_cal, suggest_other_obj);
Group.col_cal(view_other_cal, suggest_other_obj);

% - actors -------------------------
topToBottom(200)(logout, user);
topToBottom(40)(register, stranger);

% - all together -------------------
topToBottom(20)(login, own_cal);
topToBottom(50)(stranger, col_cal);

% - Notes --------------------------
Note.implicit_cal("1. kirjautumisella", "luodaan kalenteri");
Note.register_note("Käyttäjänimi", "+ salasana");
topToBottom(20)(implicit_cal, login);
topToBottom(20)(register_note, register);

% ---------------------------------
auth.info.borderColor    := (.7, .7,  0);
own_cal.info.borderColor := ( 0, .7, .7);
col_cal.info.borderColor := (.7,  0, .7);

% ---------------------------------
drawObjects(stranger, user, auth, own_cal, col_cal, implicit_cal, register_note);

clink(association)(stranger.human, register);
clink(association)(stranger.human, login);
clink(association)(register, login);
clink(association)(login, create_cal);
clink(association)(create_cal, view_cal);

clink(inheritance)(implicit_cal, login);
clink(inheritance)(register_note, register);

% - user ------------
clink(association)(user.human, logout);
clink(association)(user.human, own_cal);

link(association)
      (pathStepY(user.s, col_cal.s, -20));

% - stranger ----------
clink(association)(stranger.human, col_cal);

endfig;
end
