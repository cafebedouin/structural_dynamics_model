% ============================================================================
% OQ-262 Phase A RECON probe (READ-ONLY; in-memory asserts only for overlay)
% ============================================================================
% Run from prolog/ (three separate processes — controls are PER-PROCESS):
%   swipl -l ../audits/2026-08-09_oq262_coexists_severance/recon_probe.pl \
%         -g "recon_live, halt" -t "halt(1)"
%   swipl -l ../audits/2026-08-09_oq262_coexists_severance/recon_probe.pl \
%         -g "recon_kernel_test, halt" -t "halt(1)"
%   swipl -l ../audits/2026-08-09_oq262_coexists_severance/recon_probe.pl \
%         -g "census_leg('testsets_haiku'), halt" -t "halt(1)"   % etc. per twin leg
% Sections:
%   self_check   — classifier positive controls (4 modes, constructed atoms; no corpus)
%   edge table   — resolved edges via cs_kernel_registry:cs_edge_target_member/4
%                  (mandated accessor; never raw target match) + resolution mode
%   inventory    — per-reading commitment inventory (axioms/status/grounding/frame/
%                  contradiction membership) + per-pair relation profile
%   census       — per-leg resolution-mode x relation counts + orphan-edge totality
% ============================================================================
:- [stack].

% --- classifier: resolution mode of one edge target within kernel K ---------
% Mirrors cs_kernel_registry:cs_edge_target_member/4 clause order exactly;
% 'unresolved' is its complement (cs_reading_relation_unresolved/4 shape).
edge_mode(_K, T, Pairs, exact) :-
    memberchk(_-T, Pairs), !.
edge_mode(K, T, Pairs, bare_to_prefixed) :-
    atom_concat(K, '__', Pfx), atom_concat(Pfx, T, C),
    memberchk(_-C, Pairs), !.
edge_mode(K, T, Pairs, prefixed_to_bare) :-
    atom_concat(K, '__', Pfx), atom_concat(Pfx, C, T),
    memberchk(_-C, Pairs), !.
edge_mode(_K, _T, _Pairs, unresolved).

% --- per-process positive control: all 4 modes on constructed atoms ---------
% Also cross-checks agreement with the registry resolver on the 3 resolving
% modes and its failure on the unresolved one.
self_check :-
    Pairs = [u1-alpha_reading, u2-'demo_kernel__beta_reading'],
    forall(member(T-Expect,
                  [alpha_reading-exact,
                   beta_reading-bare_to_prefixed,
                   'demo_kernel__alpha_reading'-prefixed_to_bare,
                   gamma_reading-unresolved]),
           ( edge_mode(demo_kernel, T, Pairs, M),
             ( M == Expect -> true
             ; format("SELF-CHECK FAIL: ~w classified ~w expected ~w~n", [T, M, Expect]),
               halt(1) ),
             ( Expect == unresolved
             -> ( cs_kernel_registry:cs_edge_target_member(demo_kernel, T, Pairs, _)
                -> format("SELF-CHECK FAIL: resolver resolved ~w~n", [T]), halt(1)
                ;  true )
             ;  ( once(cs_kernel_registry:cs_edge_target_member(demo_kernel, T, Pairs, _))
                -> true
                ;  format("SELF-CHECK FAIL: resolver did not resolve ~w~n", [T]), halt(1) ) ) )),
    format("self_check: 4/4 modes classified as constructed; resolver agrees~n").

% --- edge table for one kernel ----------------------------------------------
edge_table(K) :-
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    length(Pairs, NP),
    format("== edge table ~w: ~w registered readings ==~n", [K, NP]),
    forall(member(U-C, Pairs), format("  member ~w  (uid ~w)~n", [C, U])),
    format("-- directed edges (source -rel-> resolved target | authored form | mode) --~n"),
    forall(( member(U1-C1, Pairs),
             narrative_ontology:cs_reading_relation(U1, T, Rel) ),
           ( ( once(cs_kernel_registry:cs_edge_target_member(K, T, Pairs, C2))
             -> edge_mode(K, T, Pairs, Mode),
                format("  ~w -~w-> ~w   (authored: ~w, ~w)~n", [C1, Rel, C2, T, Mode])
             ;  format("  ~w -~w-> UNRESOLVED (authored: ~w)~n", [C1, Rel, T]) ) )),
    format("-- per-pair relation profile (dir1 = row source, dir2 = reverse) --~n"),
    forall(( member(U1-C1, Pairs), member(U2-C2, Pairs), U1 @< U2 ),
           ( findall(R1, cs_kernel_registry:kernel_pair_edge(K, Pairs, U1, C2, R1), R1s0),
             sort(R1s0, R1s),
             findall(R2, cs_kernel_registry:kernel_pair_edge(K, Pairs, U2, C1, R2), R2s0),
             sort(R2s0, R2s),
             format("  ~w -> ~w : ~w   |   ~w -> ~w : ~w~n",
                    [C1, C2, R1s, C2, C1, R2s]) )),
    % unordered coexists pairs (either direction carries coexists_with)
    aggregate_all(count,
                  ( member(U1-_, Pairs), member(U2-C2b, Pairs), U1 @< U2,
                    once(( cs_kernel_registry:kernel_pair_edge(K, Pairs, U1, C2b, coexists_with)
                         ; member(_-C1b, Pairs), C1b \== C2b,
                           cs_kernel_registry:kernel_pair_edge(K, Pairs, U2, C1b, coexists_with),
                           memberchk(U1-C1b, Pairs) )) ),
                  _NCoexRough),
    findall(C1-C2,
            ( member(U1-C1, Pairs), member(U2-C2, Pairs), U1 @< U2,
              once(( cs_kernel_registry:kernel_pair_edge(K, Pairs, U1, C2, coexists_with)
                   ; cs_kernel_registry:kernel_pair_edge(K, Pairs, U2, C1, coexists_with) )) ),
            CoexPairs),
    length(CoexPairs, NCoex),
    format("-- unordered coexists_with pairs: ~w~n", [NCoex]),
    forall(member(A-B, CoexPairs), format("     ~w | ~w~n", [A, B])).

% --- commitment inventory for one kernel -------------------------------------
inventory(K) :-
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    format("== commitment inventory ~w ==~n", [K]),
    forall(member(U-C, Pairs),
           ( format("  reading ~w (uid ~w)~n", [C, U]),
             forall(narrative_ontology:cs_axiom(U, Role, Ax),
                    ( ( narrative_ontology:cs_axiom_status(Ax, St) -> true ; St = '(none)' ),
                      ( narrative_ontology:cs_axiom_grounding(U, Ax, G) -> true ; G = '(none)' ),
                      ( narrative_ontology:cs_axiom_contradiction(Ax, Other)
                      -> format("    axiom[~w] ~w  status=~w grounding=~w  CONTRADICTS ~w~n",
                                [Role, Ax, St, G, Other])
                      ;  format("    axiom[~w] ~w  status=~w grounding=~w~n",
                                [Role, Ax, St, G]) ) )),
             ( \+ narrative_ontology:cs_axiom(U, _, _)
             -> format("    axioms: NONE AUTHORED~n") ; true ),
             forall(narrative_ontology:cs_reference_frame(U, F),
                    format("    reference_frame ~w~n", [F])) )),
    format("-- cs_axiom_contradiction facts whose BOTH atoms are owned in ~w --~n", [K]),
    forall(( narrative_ontology:cs_axiom_contradiction(A, B),
             A @< B,
             member(UA-CA, Pairs), narrative_ontology:cs_axiom(UA, _, A),
             member(UB-CB, Pairs), narrative_ontology:cs_axiom(UB, _, B) ),
           format("  ~w (~w)  <->  ~w (~w)~n", [A, CA, B, CB])).

% --- census over a loaded corpus ---------------------------------------------
census :-
    % NB: Cx^(module:goal) needs the parens — bare Cx^m:g/2 parses wrong (gotchas §13)
    findall(K, ( setof(Kx, Cx^(narrative_ontology:cs_kernel_id(Cx, Kx)), Ks),
                 member(K, Ks) ), Kernels),
    length(Kernels, NK),
    format("== census: ~w kernels with registered readings ==~n", [NK]),
    findall(K-Rel-Mode,
            ( member(K, Kernels),
              cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
              member(U-_, Pairs),
              narrative_ontology:cs_reading_relation(U, T, Rel),
              edge_mode(K, T, Pairs, Mode) ),
            Rows),
    length(Rows, NEdges),
    findall(Rel-Mode, member(_-Rel-Mode, Rows), RM0),
    msort(RM0, RM), clumped(RM, RMHist),
    format("  kernel-member edges: ~w~n  relation-mode histogram: ~w~n", [NEdges, RMHist]),
    % per-kernel non-exact detail (the Phase-D movement surface)
    findall(K2-Rel2-Mode2,
            ( member(K2-Rel2-Mode2, Rows), Mode2 \== exact ),
            NonExact),
    msort(NonExact, NESorted), clumped(NESorted, NEHist),
    format("  non-exact edges by kernel-relation-mode: ~w~n", [NEHist]),
    % totality: every cs_reading_relation fact is kernel-member-owned or orphan
    aggregate_all(count, narrative_ontology:cs_reading_relation(_, _, _), NTotal),
    aggregate_all(count,
                  ( narrative_ontology:cs_reading_relation(U3, _, _),
                    \+ ( narrative_ontology:cs_story_uid(C3, U3),
                         narrative_ontology:cs_kernel_id(C3, _) ) ),
                  NOrphan),
    NSum is NEdges + NOrphan,
    format("  totality: total=~w kernel-owned=~w orphan(source not in any kernel)=~w sum=~w~n",
           [NTotal, NEdges, NOrphan, NSum]),
    ( NSum =:= NTotal -> format("  totality CHECK OK~n")
    ; format("  totality MISMATCH (double-count or miss)~n") ).

% --- entry points -------------------------------------------------------------
load_and_count(ExpectPresent, ExpectAbsent) :-
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format("corpus loaded: ~w constraints~n", [NC]),
    ( corpus_loader:corpus_constraint(ExpectPresent)
    -> format("overlay control: ~w PRESENT (expected)~n", [ExpectPresent])
    ;  format("overlay control FAIL: ~w absent~n", [ExpectPresent]), halt(1) ),
    ( corpus_loader:corpus_constraint(ExpectAbsent)
    -> format("overlay control FAIL: ~w present (expected absent)~n", [ExpectAbsent]), halt(1)
    ;  format("overlay control: ~w ABSENT (expected)~n", [ExpectAbsent]) ).

recon_live :-
    self_check,
    load_and_count(empirical_precedent_reading, abolition_reading),
    edge_table(fiat_efficacy_kernel), nl,
    inventory(fiat_efficacy_kernel), nl,
    census.

recon_kernel_test :-
    self_check,
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, 'archives/datasets/kernel_test')),
    load_and_count(abolition_reading, empirical_precedent_reading),
    edge_table(state_execution_authority), nl,
    inventory(state_execution_authority), nl,
    edge_table(state_killing_authority), nl,
    inventory(state_killing_authority), nl,
    census.

census_leg(Leg) :-
    self_check,
    retractall(config:param(corpus_path, _)),
    assertz(config:param(corpus_path, Leg)),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format("leg ~w loaded: ~w constraints~n", [Leg, NC]),
    census.
