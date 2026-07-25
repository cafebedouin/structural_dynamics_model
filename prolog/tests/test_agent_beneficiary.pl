% ============================================================================
% test_agent_beneficiary.pl — TWO-GATE ENFORCEMENT SUITE (OQ-66, 2026-07-25).
%
% CONTRACT CHANGE — read before editing. This file used to be an INERTNESS
% TRIPWIRE watching the drl_core natural_law_without_beneficiary/1 deferral
% ("filtering nlwb changes no final classification today"). That deferral is
% GONE: the filter landed 2026-07-25 (OQ-66 / ruling 63-A), so there is nothing
% left to watch. The suite is now ENFORCEMENT of the two-gate principle stated
% at prolog/narrative_ontology.pl:398-419.
%
% Why the rewrite was forced rather than chosen: the old suite's fixtures were
% wiped by the 2026-06-05 corpus reset. 0 of its 11 fsm_agent_mountains exist in
% the live corpus (recorded at
% audits/2026-06-14_oq122_fixture_triage/FINDINGS.md:38), so it sat 15/17 red
% for ~7 weeks — red for the wrong reason. Worse, its fsm_released_for_maxwell
% test PASSED VACUOUSLY: a \+ over a constraint that does not exist. Those
% constraints are not recoverable — generation is stochastic and a regenerated
% story is a new draw, not the same story (CLAUDE.md, determinism frontier).
%
% TWO DEFECTS IN THE OLD SUITE, recorded here so they are not reintroduced:
%
%   (1) VACUOUS MAXENT ARM. The old tripwire read maxent_top_type under a plain
%       [stack] + corpus load, which leaves MaxEnt UNFITTED (maxent_dist/3
%       empty). Every read failed and was mapped to `no_top` in BOTH arms, so
%       the MaxEnt half of the diff compared [no_top,...] against itself while
%       presenting as zero-diff. A suite that wants MaxEnt observables must
%       refit explicitly (maxent_cleanup + maxent_multi_run/2) and assert
%       maxent_dist/3 non-empty first. This suite therefore does NOT read
%       MaxEnt at all — the MaxEnt surface was measured once, at cutover, on
%       the real corpora (audits/2026-07-25_oq66_nlwb_filter_cutover/).
%
%   (2) CORPUS-COUPLED FIXTURES. Everything asserted here is either hermetic
%       (registry contents, config constants, clause structure) or an
%       INVARIANT quantified over whatever corpus is loaded, with an explicit
%       non-vacuity guard. The planted truth table lives in
%       tests/fixtures/nlwb_controls/ and is exercised by the pipeline gate's
%       second swipl process (python/run_pipeline.py _prolog_agency_gate),
%       because a corpus_path overlay needs a FRESH process — the
%       corpus_loaded/0 guard silently ignores an in-process overlay-after-load.
%
% REOPEN CONDITION: this suite goes red when a non_agent_beneficiary/1 entry is
% added or removed without the gate-2 convergence read, when the filter stops
% being exactly registry membership, when nlwb stops reading the filtered view,
% or when the snare floors stop being config constants.
%
% Run: cd prolog && swipl -g "[stack], [tests/test_agent_beneficiary], run_tests, halt" -t "halt(1)"
% ============================================================================

:- corpus_loader:ensure_corpus_loaded.

:- begin_tests(agent_beneficiary).

% ----------------------------------------------------------------------------
% (a) Registry — hermetic, corpus-independent
% ----------------------------------------------------------------------------

% The registry is exactly the two ruled values. Adding an entry without the
% gate-2 convergence read (host metrics AND narrative/omegas converge on
% genuine-law) must trip this. A NON-AGENT entry RELEASES a natural-law
% certification on its host, which is why it needs both gates; an AGENT tag
% only withholds one and needs gate 1 alone.
test(registry_exact_contents) :-
    findall(V, narrative_ontology:non_agent_beneficiary(V), Vs0),
    msort(Vs0, Vs),
    Vs == [entropic_universe_hypothesis,
           international_humanitarian_law_framework].

% Two-gate default: an unlisted value is AGENT-kind (fail-open to status quo).
% A new proposition-kind value not yet ruled must keep current FSM behaviour
% rather than silently granting a natural-law certification.
test(unlisted_value_defaults_to_agent) :-
    \+ narrative_ontology:non_agent_beneficiary(nlwb_ctl_extracting_guild).

% ----------------------------------------------------------------------------
% (b) The filter is EXACTLY registry membership — structural
% ----------------------------------------------------------------------------
% This is the load-bearing premise of the cutover audit. The zero-diff on all
% six legs was sourced off a fact-existence query (no beneficiary fact in any
% live leg carries a registered value), and that query entails extensional
% identity between constraint_beneficiary/2 and agent_beneficiary/2 ONLY IF
% the filter is exactly membership — no kind inference, no inheritance, no
% defaulting. If someone adds a second clause or a heuristic, the audit's
% conclusion silently stops following from its evidence.

test(agent_beneficiary_is_single_clause) :-
    predicate_property(narrative_ontology:agent_beneficiary(_, _),
                       number_of_clauses(N)),
    N == 1.

% Static, not dynamic: there is no runtime channel by which the filter's
% extension can drift from the loaded corpus. This is what makes the
% load-time registry check sufficient rather than a point-in-time read.
test(registry_is_static) :-
    \+ predicate_property(narrative_ontology:non_agent_beneficiary(_), dynamic).

% The iff, quantified over whatever corpus is loaded, with a non-vacuity guard
% so an empty beneficiary table cannot pass this by absence (Pattern 5).
test(filter_is_exactly_registry_membership) :-
    aggregate_all(count, narrative_ontology:constraint_beneficiary(_, _), NB),
    NB > 0,                                  % non-vacuity guard
    forall(narrative_ontology:constraint_beneficiary(C, B),
           (   narrative_ontology:non_agent_beneficiary(B)
           ->  \+ narrative_ontology:agent_beneficiary(C, B)
           ;   narrative_ontology:agent_beneficiary(C, B)
           )).

% ----------------------------------------------------------------------------
% (c) The cutover itself — nlwb reads the FILTERED view
% ----------------------------------------------------------------------------
% Behavioural, not a source grep: recomputes the intended extension from its
% three conjuncts and demands drl_core agree, over the whole loaded corpus.
% Reverting drl_core.pl:317 to constraint_beneficiary/2 turns this red on any
% corpus containing a registered beneficiary value — and stays green (correctly)
% on corpora containing none, which is why the planted fixture leg exists.

test(nlwb_reads_agent_filtered_view) :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    N > 0,                                   % non-vacuity guard
    forall(corpus_loader:corpus_constraint(C),
           (   (   domain_priors:emerges_naturally(C),
                   \+ domain_priors:requires_active_enforcement(C),
                   \+ narrative_ontology:agent_beneficiary(C, _)
               )
           ->  drl_core:natural_law_without_beneficiary(C)
           ;   \+ drl_core:natural_law_without_beneficiary(C)
           )).

% ----------------------------------------------------------------------------
% (d) Floor provenance — the fixture leg's reachability must not rot
% ----------------------------------------------------------------------------
% The nlwb_controls fixtures open the snare gate by clearing three floors.
% Hermetic fixtures decouple from corpus CONTENT, not corpus STATISTICS — if
% these floors ever become fitted or percentile-derived, a 4-story fixture leg
% would compute different floors than a 1,000-story leg and the planted flip
% would stop firing SILENTLY (the gate would still be green, measuring nothing).
% Asserting provenance here turns that migration LOUD instead.
% Fixture metrics (ε 0.80 / supp 0.75 / χ well above 0.66) carry margin against
% these values rather than sitting at them.
test(snare_floors_are_config_constants) :-
    config:param(snare_suppression_floor, 0.60),
    config:param(snare_chi_floor,         0.66),
    config:param(snare_epsilon_floor,     0.46).

:- end_tests(agent_beneficiary).
