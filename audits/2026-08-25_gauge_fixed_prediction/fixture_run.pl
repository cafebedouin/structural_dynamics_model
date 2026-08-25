% ============================================================================
% fixture_run.pl -- runs the cell-(c) ATTEMPT fixtures through the SAME
% emit_row/5 code path as the live-leg run, over a scratch overlay corpus.
%
% corpus_path is overlaid with asserta (NOT plain assertz): config.pl:489
% defines param(corpus_path, testsets) as the FIRST clause and the loader takes
% the first solution, so an appended assertz is silently ignored and you load
% the default leg while the count looks successful (CLAUDE.md, Corpus Loading).
% The path is ABSOLUTE, which passes straight through resolve_corpus_dir/2.
%
% Run from prolog/:
%   swipl -g "['../audits/2026-08-25_gauge_fixed_prediction/fixture_run.pl'], run_fixtures, halt" -t "halt(1)"
% ============================================================================

:- use_module(library(lists)).

run_fixtures :-
    ensure_loaded('../audits/2026-08-25_gauge_fixed_prediction/gauge_fixed_prediction_probe.pl'),
    % stack FIRST: config.pl must be loaded before the overlay is asserted, or
    % the consult lands its own param(corpus_path, testsets) after ours.
    ensure_loaded('../prolog/stack'),

    asserta(config:param(corpus_path,
        '/home/scott/bin/structural_dynamics_model/audits/2026-08-25_gauge_fixed_prediction/fixtures')),
    % witness the overlay TOOK EFFECT before loading anything
    once(config:param(corpus_path, Effective)),
    format(user_error, '[fixture] corpus_path in effect: ~w~n', [Effective]),
    (   sub_atom(Effective, _, _, _, fixtures)
    ->  true
    ;   format(user_error, '[fixture] OVERLAY DID NOT TAKE -- ABORT~n', []), fail
    ),

    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    length(Cs, NC),
    format(user_error, '[fixture] loaded members: ~w -> ~w~n', [NC, Cs]),
    (   NC =:= 4
    ->  true
    ;   format(user_error, '[fixture] EXPECTED 4 FIXTURE MEMBERS, GOT ~w -- the overlay loaded the wrong corpus~n', [NC]), fail
    ),

    constraint_indexing:site_contexts(Contexts),
    open('../audits/2026-08-25_gauge_fixed_prediction/fixture_rows.tsv', write, S),
    format(S, 'constraint\tkind\tctx_idx\tagent_power\tdr_type\trestricted_type\tgauge_fixed\tagreement~n', []),
    forall(( member(C, Cs), nth1(I, Contexts, Ctx) ), emit_row(S, C, I, Ctx)),
    close(S),

    % --- did ANY fixture row land in cell (c)?
    findall(C-I,
        ( member(C, Cs), nth1(I, Contexts, Ctx),
          obs(drl_core:dr_type(C, Ctx, X), X, DrT),
          memberchk(DrT, [mountain, snare]),
          obs(constraint_indexing:classify_from_restricted(C, Ctx, Y), Y, RestT),
          agreement(DrT, RestT, disagree),
          obs(dirac_classification:gauge_fixed(C, Ctx, G), G, false) ),
        CellC),
    length(CellC, NCellC),
    format(user_error, '[fixture] CELL (c) hits: ~w ~w~n', [NCellC, CellC]),
    (   NCellC > 0
    ->  format(user_error, '[fixture] CELL (c) IS REACHABLE~n', [])
    ;   format(user_error, '[fixture] CELL (c) DECLINED by every fixture -- see fixture_rows.tsv for where each landed~n', [])
    ),

    % --- the two structural reasons, each measured rather than argued
    format(user_error, '~n[fixture] REASON 1 -- which canonical contexts can perceive `mountain` at all:~n', []),
    forall(nth1(J, Contexts, Cx),
        ( Cx = context(agent_power(P), time_horizon(T), exit_options(E), _),
          (   constraint_indexing:effective_immutability_for_context(Cx, mountain)
          ->  M = yes ; M = no ),
          format(user_error, '    ctx~w ~w (~w,~w): perceives_mountain=~w~n', [J, P, T, E, M]) )),
    format(user_error, '[fixture] REASON 2 -- restricted vs config gate constants:~n', []),
    forall(member(Key-Lit,
            [mountain_suppression_ceiling-0.05, mountain_extractiveness_max-0.25,
             snare_chi_floor-0.66, snare_epsilon_floor-0.46, snare_suppression_floor-0.60,
             rope_chi_ceiling-0.35, rope_epsilon_ceiling-0.45]),
        ( config:param(Key, V),
          ( V =:= Lit -> Same = identical ; Same = DIFFERENT ),
          format(user_error, '    ~w: config=~w restricted_literal=~w -> ~w~n', [Key, V, Lit, Same]) )),
    format(user_error, '[fixture] done~n', []).
