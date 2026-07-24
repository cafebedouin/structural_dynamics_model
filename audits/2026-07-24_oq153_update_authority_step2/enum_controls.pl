% OQ-153 step-2 controls for validate_update_authority (data_validation.pl).
% Script-only entry (initialization(main, main)) so a stray consult/glob CANNOT
% run main or hit the halt — the file asserts out-of-enum test facts and must be
% inert unless launched directly (swipl enum_controls.pl).
:- initialization(main, main).

err(Kind, C) :- data_validation:validation_error(Kind, C, _).

run_check :-
    retractall(data_validation:validation_error(_,_,_)),
    with_output_to(string(_), data_validation:validate_update_authority).

main :-
    [stack],
    corpus_loader:load_all_testsets,
    use_module(data_validation),
    corpus_loader:corpus_constraint(Real), !,   % a real corpus CID for membership/uniqueness
    format("~nreal corpus CID for controls = ~w~n", [Real]),

    format("~n(0) update_authority/2 declared & queryable (no clauses yet)? "),
    ( \+ narrative_ontology:update_authority(_,_) -> format("YES (empty, no error)~n") ; format("has clauses~n") ),

    format("~n(C) absence — check with ZERO authored facts:~n"),
    run_check,
    findall(x, data_validation:validation_error(_,_,_), C0),
    format("    validation_errors = ~w  (expect []; no default imputed)~n", [C0]),

    format("~n(A) membership positive — out-of-enum on a REAL cid:~n"),
    assertz(narrative_ontology:update_authority(Real, bogus_value)),
    run_check,
    ( err(invalid_update_authority, Real) -> format("    bogus_value FLAGGED invalid ✓~n") ; format("    NOT flagged ✗~n") ),
    ( err(orphan_update_authority, Real) -> format("    (also orphan? ✗ real cid mis-flagged)~n") ; format("    real cid not orphan ✓~n") ),
    retract(narrative_ontology:update_authority(Real, bogus_value)),

    format("~n(B) membership valid — each enum value on a REAL cid:~n"),
    forall(member(V,[licensed_revisable,frozen,absent_diffuse]),
        ( assertz(narrative_ontology:update_authority(Real, V)), run_check,
          ( err(invalid_update_authority, Real) -> format("    ~w FLAGGED ✗~n",[V]) ; format("    ~w not flagged ✓~n",[V]) ),
          retract(narrative_ontology:update_authority(Real, V)) )),

    format("~n(D) uniqueness — TWO facts on the same cid:~n"),
    assertz(narrative_ontology:update_authority(Real, frozen)),
    assertz(narrative_ontology:update_authority(Real, licensed_revisable)),
    run_check,
    ( err(duplicate_update_authority, Real) -> format("    duplicate FLAGGED ✓~n") ; format("    NOT flagged ✗~n") ),
    retractall(narrative_ontology:update_authority(Real, _)),

    format("~n(E) orphan — valid value on a NON-corpus cid:~n"),
    assertz(narrative_ontology:update_authority(typo_not_a_real_constraint, frozen)),
    run_check,
    ( err(orphan_update_authority, typo_not_a_real_constraint) -> format("    orphan FLAGGED ✓~n") ; format("    NOT flagged ✗~n") ),
    ( err(invalid_update_authority, typo_not_a_real_constraint) -> format("    (also invalid? ✗ valid value mis-flagged)~n") ; format("    valid value not invalid ✓~n") ),
    retract(narrative_ontology:update_authority(typo_not_a_real_constraint, frozen)),
    halt.
