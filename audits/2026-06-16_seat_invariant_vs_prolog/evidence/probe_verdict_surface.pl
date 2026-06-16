% Phase A — verdict-surface enumeration over a corpus tier.
% Usage: swipl -q -g "consult(stack), consult('<thisfile>'), run(Tier), halt" -t "halt(1)"
% Caller overlays corpus_path BEFORE load via run/1.

run(Tier) :-
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, Tier)),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format('~n=== TIER ~w : overlay-took-effect witness ===~n', [Tier]),
    format('corpus_constraint count = ~w~n', [NC]),

    % cs_verdict firings, per atom
    format('~n--- cs_verdict firings (per corpus_constraint) ---~n'),
    findall(C-V,
            ( corpus_loader:corpus_constraint(C),
              cs_pattern_detection:cs_verdict(C, V) ),
            VPairs),
    length(VPairs, NV),
    format('total cs_verdict firings: ~w~n', [NV]),
    findall(V, member(_-V, VPairs), Vs),
    msort(Vs, VsSorted),
    tally(VsSorted),
    format('--- per-constraint cs_verdict list ---~n'),
    forall(member(C-V, VPairs), format('  VERDICT ~w :: ~w~n', [V, C])),

    % cs_pattern distribution
    format('~n--- cs_pattern distribution ---~n'),
    findall(P,
            ( corpus_loader:corpus_constraint(C),
              cs_pattern_detection:cs_pattern(C, P, _) ),
            Ps),
    msort(Ps, PsSorted),
    tally(PsSorted),

    % structural-diagnostic firings (the computed cross-check layer)
    format('~n--- structural diagnostics (computed cross-check layer) ---~n'),
    aggregate_all(count, ( corpus_loader:corpus_constraint(C), cs_pattern_detection:cs_authority_masking(C,_,_) ), NM),
    aggregate_all(count, ( corpus_loader:corpus_constraint(C), cs_pattern_detection:cs_cover_story_active(C,_) ), NCov),
    aggregate_all(count, ( corpus_loader:corpus_constraint(C), cs_pattern_detection:cs_displaced_beneficiary(C) ), ND),
    aggregate_all(count, ( corpus_loader:corpus_constraint(C), cs_pattern_detection:cs_grounding_mismatch(C,_,_) ), NG),
    format('cs_authority_masking: ~w~n', [NM]),
    format('cs_cover_story_active: ~w~n', [NCov]),
    format('cs_displaced_beneficiary: ~w~n', [ND]),
    format('cs_grounding_mismatch: ~w~n', [NG]),

    % signature_detection mismatch verdicts (Boltzmann-gated, computed)
    format('~n--- signature_detection computed mismatch verdicts ---~n'),
    aggregate_all(count, ( corpus_loader:corpus_constraint(C), catch(signature_detection:false_natural_law(C,_),_,fail) ), NFNL),
    aggregate_all(count, ( corpus_loader:corpus_constraint(C), catch(signature_detection:false_ci_rope(C,_),_,fail) ), NFCR),
    aggregate_all(count, ( corpus_loader:corpus_constraint(C), catch(signature_detection:false_summit_mountain(C,_),_,fail) ), NFSM),
    format('signature false_natural_law: ~w~n', [NFNL]),
    format('signature false_ci_rope: ~w~n', [NFCR]),
    format('signature false_summit_mountain: ~w~n', [NFSM]),

    % cross-axis CS drift mismatch
    format('~n--- cs_drift_mismatch (cross-axis, network-gated) ---~n'),
    aggregate_all(count, ( corpus_loader:corpus_constraint(C), narrative_ontology:cs_story_uid(C,U), cs_drift_mismatch:cs_drift_mismatch(U,_) ), NDM),
    format('cs_drift_mismatch firings: ~w~n', [NDM]),
    format('~n=== END TIER ~w ===~n', [Tier]).

tally([]).
tally([X|Xs]) :-
    take_run(X, [X|Xs], Run, Rest),
    length(Run, N),
    format('  ~w : ~w~n', [X, N]),
    tally(Rest).

take_run(_, [], [], []).
take_run(X, [Y|Ys], [Y|Run], Rest) :- X == Y, !, take_run(X, Ys, Run, Rest).
take_run(_, L, [], L).
