% ============================================================================
% OQ-261 C1 RECON probe — fiat_efficacy_kernel family frame (READ-ONLY)
% ============================================================================
% Run from prolog/:
%   swipl -l ../audits/2026-08-07_oq261_forced_gluing/family_frame_probe.pl \
%         -g "recon, halt" -t "halt(1)"
% Loads [stack] + live testsets. Computes, for the fiat family:
%   A. resolved per-pair edge table (through cs_edge_target_member/4 — never raw)
%   B. cs_kernel_obstruction/4 + status (committer axis, authored-edge-only)
%   C. kernel-family H1 frame: per-context reading-type vectors reconstructed
%      from cs_kernel_registry:compare_kernel_readings/3 Profile verdicts, fed
%      into pure grothendieck_cohomology:obstruction_from_vector/3; every H1
%      sanity-checked against the general-n spectrum H(n)
%      (docs/h1_gap_spectrum_general_n.md — out-of-spectrum = bug witness)
%   D. per-story seat reads: consensus_provenance/2, excluded-seat census,
%      agent-seat roster (stakeholder frame — a DIFFERENT frame from C; the
%      frame-mismatch note is in RECON.md)
% No MaxEnt reads (OQ-66 does not bite); no writes.
% ============================================================================
:- [stack].
:- corpus_loader:load_all_testsets.

fiat_kernel(fiat_efficacy_kernel).
fiat_family([empirical_precedent_reading, scholarship_reading,
             truth_procedure_reading, predictive_synthesis_reading,
             empathy_simulation_reading, utopian_fiction_reading,
             fiat_efficacy_kernel_flat_control]).

% --- H(n): reachable H1 spectrum over n real seats -------------------------
% H1(lambda) = C(n,2) - sum C(g_i,2) over the type-partition lambda of n.
% Derived by partition enumeration (matches docs/h1_gap_spectrum_general_n.md:
% Theorem A min-nonzero = n-1; four-seat record {0,3,4,5,6}).
h_spectrum(2, [0,1]).
h_spectrum(3, [0,2,3]).
h_spectrum(4, [0,3,4,5,6]).
h_spectrum(5, [0,4,6,7,8,9,10]).
h_spectrum(6, [0,5,8,9,11,12,13,14,15]).

% --- A. resolved edge table -------------------------------------------------
edge_table :-
    fiat_kernel(K),
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    length(Pairs, NP),
    format("== A. ~w: ~w registered readings ==~n", [K, NP]),
    forall(member(U-C, Pairs), format("  member ~w  (uid ~w)~n", [C, U])),
    format("-- resolved directed edges (source -> resolved target, relation) --~n"),
    forall(( member(U1-C1, Pairs),
             narrative_ontology:cs_reading_relation(U1, T, Rel),
             once(cs_kernel_registry:cs_edge_target_member(K, T, Pairs, C2)) ),
           format("  ~w -~w-> ~w   (authored target: ~w)~n", [C1, Rel, C2, T])),
    format("-- per-pair relation summary (either direction) --~n"),
    forall(( member(U1-C1, Pairs), member(U2-C2, Pairs), U1 @< U2 ),
           ( findall(R, ( member(UA-CA, [U1-C1, U2-C2]),
                          member(_-CB, [U1-C1, U2-C2]), CA \== CB,
                          cs_kernel_registry:kernel_pair_edge(K, Pairs, UA, CB, R) ),
                     Rs0),
             sort(Rs0, Rs),
             format("  pair ~w | ~w : ~w~n", [C1, C2, Rs]) )).

% --- B. obstruction ---------------------------------------------------------
obstruction :-
    fiat_kernel(K),
    cs_kernel_registry:cs_kernel_obstruction(K, H1r, ClosureN, PluralityN),
    cs_kernel_registry:cs_kernel_obstruction_status(K, S),
    format("== B. cs_kernel_obstruction(~w): H1r=~w ClosureN=~w PluralityN=~w status=~w ==~n",
           [K, H1r, ClosureN, PluralityN, S]).

% --- C. kernel-family H1 frame ---------------------------------------------
% Vector reconstruction from Profile verdicts (faithful for H0/H1 under the
% OQ-51 N/A rule):
%   agree(T, NUnk)        -> T x (N-NUnk) ++ unknown x NUnk
%   diverge(TypeMap,NUnk) -> the TypeMap's types verbatim (incl. unknowns)
%   undetermined(NReal,NUnk) -> <2 real seats; obstruction_from_vector gives
%                               null/null for ANY such vector, so a placeholder
%                               with NReal reals reproduces it exactly.
verdict_vector(agree(T, NUnk), N, V) :-
    NR is N - NUnk,
    length(Rs, NR), maplist(=(T), Rs),
    length(Us, NUnk), maplist(=(unknown), Us),
    append(Rs, Us, V).
verdict_vector(diverge(TypeMap, _NUnk), _N, V) :-
    findall(T, member(_-T, TypeMap), V).
verdict_vector(undetermined(NReal, NUnk), _N, V) :-
    length(Rs, NReal), maplist(=(placeholder_real), Rs),
    length(Us, NUnk), maplist(=(unknown), Us),
    append(Rs, Us, V).

family_h1 :-
    fiat_kernel(K),
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    length(Pairs, N),
    cs_kernel_registry:compare_kernel_readings(K, Profile, PairStats),
    length(Profile, NCtx),
    format("== C. family H1 frame over ~w contexts, ~w readings ==~n", [NCtx, N]),
    % per-context H0/H1 + spectrum check
    findall(Ctx-h(H0, H1),
            ( member(Ctx-Verdict, Profile),
              verdict_vector(Verdict, N, Vec),
              grothendieck_cohomology:obstruction_from_vector(Vec, H0, H1) ),
            CtxH),
    findall(H1, member(_-h(_, H1), CtxH), H1s),
    msort(H1s, H1sorted),
    clumped(H1sorted, H1Hist),
    format("  H1 histogram over contexts (H1-Count): ~w~n", [H1Hist]),
    % spectrum sanity: every numeric H1 must lie in H(n_real) for its context
    forall(( member(Ctx-h(_, H1), CtxH), number(H1) ),
           ( ctx_nreal(K, Ctx, NReal),
             ( h_spectrum(NReal, Spec), memberchk(H1, Spec)
             -> true
             ;  format("  OUT-OF-SPECTRUM (BUG WITNESS): ctx=~w H1=~w n_real=~w~n",
                       [Ctx, H1, NReal]) ) )),
    aggregate_all(count, ( member(_-h(_, H1a), CtxH), number(H1a), H1a > 0 ), NObstructed),
    aggregate_all(count, ( member(_-h(H0b, _), CtxH), H0b == 1 ), NGlued),
    aggregate_all(count, ( member(_-h(H0c, _), CtxH), H0c == null ), NUndet),
    format("  contexts: glued(H0=1)=~w obstructed(H1>0)=~w undetermined(null)=~w~n",
           [NGlued, NObstructed, NUndet]),
    format("  spectrum check: every numeric H1 verified against H(n_real) above (silence = all in-spectrum)~n"),
    length(PairStats, NPairs),
    format("  pairwise stats rows: ~w~n", [NPairs]),
    forall(member(pair(_-C1, _-C2)-stats(J, A, D), PairStats),
           format("    ~w | ~w : jaccard=~w agree=~w diverge=~w~n", [C1, C2, J, A, D])).

% n_real for a context: real (non-unknown) dr_type count over the kernel's readings
ctx_nreal(K, Ctx, NReal) :-
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    aggregate_all(count,
                  ( member(_-C, Pairs),
                    once(drl_core:dr_type(C, Ctx, T)),
                    T \== unknown ),
                  NReal).

% --- D. per-story seat reads (stakeholder frame) ----------------------------
seat_reads :-
    format("== D. per-story stakeholder-frame reads (7 stories) ==~n"),
    fiat_family(Cids),
    forall(member(C, Cids),
           ( ( stakeholder_seats:consensus_provenance(C, V) -> true ; V = 'FAILED(bug)' ),
             findall(N, ( narrative_ontology:constraint_stakeholder(C, N, R, _, _, _, _),
                          R == excluded ), Excl),
             stakeholder_seats:stakeholder_agent_seats(C, Agents),
             length(Agents, NA), length(Excl, NE),
             format("  ~w~n    consensus_provenance: ~w~n    agent_seats(~w): ~w~n    excluded(~w): ~w~n",
                    [C, V, NA, Agents, NE, Excl]) )).

recon :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format("corpus loaded: ~w constraints~n", [NC]),
    edge_table, nl,
    obstruction, nl,
    family_h1, nl,
    seat_reads.
