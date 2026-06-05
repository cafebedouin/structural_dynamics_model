% ============================================================================
% CONSTRAINT STORY: israel_norwegian_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_norwegian_law, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: israel_norwegian_law
 *   human_readable: The Norwegian Law (Amendment to Article 42c)
 *   domain: political
 *
 * SUMMARY:
 *   The Norwegian Law in Israel allows ministers to resign from the Knesset
 *   to focus on executive duties, with their seats filled by the next person
 *   on their party's list. This mechanism concentrates power within the
 *   ruling coalition and affects different political actors differently.
 *   While proponents argue it improves governance by allowing ministers to
 *   dedicate their full attention to their portfolios, critics contend it
 *   undermines the separation of powers and can be used for political
 *   maneuvering.
 *
 * KEY AGENTS:
 *   - Coalition Parties: Primary beneficiary (institutional/arbitrage) - benefit from increased control and stability.
 *   - Opposition Parties: Primary target (powerless/trapped) - negatively impacted by the strengthened ruling coalition.
 *   - Smaller Parties: Secondary target (moderate/constrained) - face challenges to their influence within the coalition.
 *   - Sitting Ministers: Beneficiary, but at a cost (powerful/constrained)
 *   - Public Trust: Affected party (analytical/analytical) - public opinion and trust in the political process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_norwegian_law, 0.55).
domain_priors:suppression_score(israel_norwegian_law, 0.4).
domain_priors:theater_ratio(israel_norwegian_law, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_norwegian_law, extractiveness, 0.55).
narrative_ontology:constraint_metric(israel_norwegian_law, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(israel_norwegian_law, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_norwegian_law, tangled_rope).
narrative_ontology:human_readable(israel_norwegian_law, "The Norwegian Law (Amendment to Article 42c)").
narrative_ontology:topic_domain(israel_norwegian_law, "political").

domain_priors:requires_active_enforcement(israel_norwegian_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_norwegian_law, coalition_parties).
narrative_ontology:constraint_beneficiary(israel_norwegian_law, sitting_ministers).
narrative_ontology:constraint_victim(israel_norwegian_law, opposition_parties).
narrative_ontology:constraint_victim(israel_norwegian_law, smaller_parties).
narrative_ontology:constraint_victim(israel_norwegian_law, public_trust).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Opposition parties have limited ability to influence the law's application and are negatively impacted by the increased power of the ruling coalition.
constraint_indexing:constraint_classification(israel_norwegian_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Smaller parties within the coalition may benefit from increased representation but are also constrained by the dominant power of larger parties.
constraint_indexing:constraint_classification(israel_norwegian_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% Coalition parties benefit from the law by solidifying their power and control over the government.
constraint_indexing:constraint_classification(israel_norwegian_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The public may perceive the law as a cynical power grab, eroding trust in the political system, while also potentially leading to more effective governance.
constraint_indexing:constraint_classification(israel_norwegian_law, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% Sitting ministers benefit from focusing on their executive duties, but the law is also used as a tool for political maneuvering which is less effective over the long-term.
constraint_indexing:constraint_classification(israel_norwegian_law, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_norwegian_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_norwegian_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_norwegian_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israel_norwegian_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(israel_norwegian_law, TR),
    TR >= 0.70.

:- end_tests(israel_norwegian_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The law extracts power and influence from opposition parties and smaller parties, concentrating it within the ruling coalition. Suppression (0.40): The law limits the ability of opposition parties to effectively challenge the government. Theater Ratio (0.75): The law is presented as a means to improve governance, but it also serves political purposes. The increased theater ratio indicates a performative element.
 *
 * PERSPECTIVAL GAP:
 *   The opposition parties view the law as a snare, trapping them in a position of weakness. Smaller coalition parties see it as a tangled rope, offering some benefits but also imposing constraints. The coalition parties perceive it as a rope, facilitating their governance efforts. The public sees a piton, with potential positive or negative impact.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the coalition parties, as this legislation strengthens their grip on power. Victims are the opposition parties who find their ability to influence governance further reduced. The directionality aligns with the increase in the ruling coalition's power at the expense of those outside it.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by understanding the perspectives. While the coalition may claim it is a rope to improve governance, opposition parties see it as a snare. The reality is a tangled rope where some benefit at the expense of others, with the potential for the system to devolve into an empty piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_trust_erosion,
    'To what extent does the law erode public trust in the political system?',
    'Polling data and surveys measuring public opinion of government institutions.',
    'High erosion could lead to political instability and decreased civic engagement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_trust_erosion, empirical, 'The impact of the law on public trust in the political system.').

omega_variable(
    governance_effectiveness,
    'Does the law lead to more effective governance?',
    'Analysis of policy outcomes and government performance metrics.',
    'Increased effectiveness could justify the law despite concerns about power concentration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_effectiveness, empirical, 'The impact of the law on the effectiveness of governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_norwegian_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isra_tr_t0, israel_norwegian_law, theater_ratio, 0, 0.5).
narrative_ontology:measurement(isra_tr_t5, israel_norwegian_law, theater_ratio, 5, 0.6).
narrative_ontology:measurement(isra_tr_t10, israel_norwegian_law, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(isra_be_t0, israel_norwegian_law, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(isra_be_t5, israel_norwegian_law, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(isra_be_t10, israel_norwegian_law, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_norwegian_law, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
