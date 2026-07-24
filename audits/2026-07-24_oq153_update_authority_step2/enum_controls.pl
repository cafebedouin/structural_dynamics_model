:- initialization(main).
main :-
    [stack],
    use_module(data_validation),
    format("~n(0) update_authority/2 declared & queryable (no clauses yet)? "),
    ( catch(( \+ narrative_ontology:update_authority(_,_) ), E, (format("ERROR ~w~n",[E]),fail))
      -> format("YES (empty, no error)~n") ; format("has clauses or failed~n") ),

    % clean slate for validation_error
    retractall(data_validation:validation_error(_,_,_)),

    % Control C (absence): before asserting anything, run the check -> should be clean, no default
    format("~n(C) absence: run check with ZERO authored facts:~n"),
    with_output_to(string(_), data_validation:validate_update_authority),
    findall(x, data_validation:validation_error(invalid_update_authority,_,_), C0),
    format("    validation_errors after empty run = ~w  (expect 0; no default imputed)~n",[C0]),

    % Control A (positive): out-of-enum value must be flagged
    assertz(narrative_ontology:update_authority(t_bad, bogus_value)),
    retractall(data_validation:validation_error(_,_,_)),
    with_output_to(string(_), data_validation:validate_update_authority),
    ( data_validation:validation_error(invalid_update_authority, t_bad, bogus_value)
      -> format("(A) positive control: bogus_value FLAGGED ✓~n") ; format("(A) positive control: NOT flagged ✗~n") ),

    % Control B (valid): each enum value must NOT be flagged
    retract(narrative_ontology:update_authority(t_bad, bogus_value)),
    forall(member(V,[licensed_revisable,frozen,absent_diffuse]),
        ( assertz(narrative_ontology:update_authority(t_ok, V)),
          retractall(data_validation:validation_error(_,_,_)),
          with_output_to(string(_), data_validation:validate_update_authority),
          ( data_validation:validation_error(invalid_update_authority, t_ok, _)
            -> format("(B) valid ~w: FLAGGED ✗~n",[V]) ; format("(B) valid ~w: not flagged ✓~n",[V]) ),
          retract(narrative_ontology:update_authority(t_ok, V)) )),
    halt.
