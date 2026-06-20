% ============================================================================
% KERNEL ORBIT EXPORT (OQ-150 → orbit_operator) — the two orbit-keys that
% pipeline_output.json does NOT serialise: per-kernel obstruction-class
% (cs_kernel_obstruction_status/2, a Tier-1 declared key) and per-reading
% axiom-grounding-profile (a Tier-2 reported key). Writes outputs/kernel_obstruction.json
% for the Python orbit_operator to join with pipeline_output.json.
%
% Single-canonical: this is the ONLY Prolog source for these two keys; the other
% six keys come from pipeline_output.json (no re-derivation here, Pattern 2).
% Same-run guard: stamps n_constraints so the joiner can fail-closed on a stale file
% (Pattern 1: assert same-run before joining).
%
% CWD-relative write (../outputs/...) — run from prolog/ with the corpus loaded.
% ============================================================================
:- module(kernel_orbit_export, [run_kernel_orbit_export/0, run_kernel_orbit_export_to/1]).

:- use_module(library(lists)).

run_kernel_orbit_export :-
    run_kernel_orbit_export_to('../outputs/kernel_obstruction.json').

run_kernel_orbit_export_to(Path) :-
    corpus_loader:ensure_corpus_loaded,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    % per-kernel obstruction status
    findall(K, narrative_ontology:cs_kernel_id(_, K), Ks0),
    sort(Ks0, Ks),
    findall(K-St,
            ( member(K, Ks),
              ( cs_kernel_registry:cs_kernel_obstruction_status(K, St) -> true ; St = error )
            ), KStatus),
    % per-reading grounding profile (sorted multiset atom; none when un-grounded)
    findall(C-Prof,
            ( corpus_loader:corpus_constraint(C),
              ( narrative_ontology:cs_story_uid(C, UID),
                findall(G, narrative_ontology:cs_axiom_grounding(UID, _, G), Gs0),
                Gs0 \== []
              ->  msort(Gs0, Gs), term_to_atom(Gs, Prof)
              ;   Prof = none )
            ), CProf),
    setup_call_cleanup(
        open(Path, write, S),
        write_export(S, NC, KStatus, CProf),
        close(S)),
    length(KStatus, NK), length(CProf, NCp),
    format(user_error, '[kernel_orbit_export] ~w kernels, ~w readings, n_constraints=~w -> ~w~n',
           [NK, NCp, NC, Path]).

write_export(S, NC, KStatus, CProf) :-
    format(S, '{~n', []),
    format(S, '  "source": "kernel_orbit_export.pl",~n', []),
    format(S, '  "n_constraints": ~w,~n', [NC]),
    format(S, '  "obstruction": {~n', []),
    write_pairs(S, KStatus),
    format(S, '  },~n', []),
    format(S, '  "grounding": {~n', []),
    write_pairs(S, CProf),
    format(S, '  }~n', []),
    format(S, '}~n', []).

write_pairs(_, []) :- !.
write_pairs(S, [K-V]) :- !,
    format(S, '    "~w": "~w"~n', [K, V]).
write_pairs(S, [K-V|T]) :-
    format(S, '    "~w": "~w",~n', [K, V]),
    write_pairs(S, T).
