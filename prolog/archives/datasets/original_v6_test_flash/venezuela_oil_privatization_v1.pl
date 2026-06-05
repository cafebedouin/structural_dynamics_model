% ============================================================================
% CONSTRAINT STORY: venezuela_oil_privatization_v1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_venezuela_oil_privatization_v1, []).

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
 *   constraint_id: venezuela_oil_privatization_v1
 *   human_readable: Shadow Privatization of Venezuela's Oil Sector
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   In response to crippling U.S. sanctions and economic mismanagement,
 *   Venezuela's oil sector, once a source of national pride and wealth, has
 *   undergone a process of 'shadow privatization.' This involves opaque deals
 *   with foreign companies, often shell corporations, granting them
 *   preferential access to oil fields in exchange for kickbacks and political
 *   favors. The state-owned oil company, PDVSA, is nominally still in
 *   control, but its operations are increasingly outsourced and its revenues
 *   diverted through corrupt channels, resulting in a decline in oil
 *   production and a humanitarian crisis for the Venezuelan people.
 *
 * KEY AGENTS:
 *   - Venezuelan Citizens: Primary victims (powerless/trapped) - suffer from declining living standards and lack of access to essential resources.
 *   - Corrupt PDVSA Officials: Primary beneficiaries (institutional/arbitrage) - enrich themselves through corrupt deals and have means to move assets offshore.
 *   - Foreign Shell Corporations: Secondary beneficiaries (powerful/mobile) - gain access to cheap oil but face risks of sanctions and legal challenges.
 *   - PDVSA National Interests: Nominal Institutional Actor (institutional/constrained) - increasingly hollow shell, held back by corruption and sanctions.
 *   - Analytical Observer: Sees mixed bag of coordination and extraction, enforced through corruption.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(venezuela_oil_privatization_v1, 0.75).
domain_priors:suppression_score(venezuela_oil_privatization_v1, 0.8).
domain_priors:theater_ratio(venezuela_oil_privatization_v1, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(venezuela_oil_privatization_v1, extractiveness, 0.75).
narrative_ontology:constraint_metric(venezuela_oil_privatization_v1, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(venezuela_oil_privatization_v1, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(venezuela_oil_privatization_v1, snare).
narrative_ontology:human_readable(venezuela_oil_privatization_v1, "Shadow Privatization of Venezuela's Oil Sector").
narrative_ontology:topic_domain(venezuela_oil_privatization_v1, "geopolitical/economic").

domain_priors:requires_active_enforcement(venezuela_oil_privatization_v1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(venezuela_oil_privatization_v1, corrupt_pdvsa_officials).
narrative_ontology:constraint_beneficiary(venezuela_oil_privatization_v1, foreign_shell_corporations).
narrative_ontology:constraint_victim(venezuela_oil_privatization_v1, venezuelan_citizens).
narrative_ontology:constraint_victim(venezuela_oil_privatization_v1, pdvsa_national_interests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Venezuelan citizens are trapped within the system, experiencing the shadow privatization as a pure extraction of national wealth. They lack the power and means to exit or meaningfully influence the process.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PDVSA, nominally a state-owned enterprise, is structurally constrained. The proclaimed benefits of national oil wealth are increasingly a theatrical performance, with the substance drained away via corrupt practices. The institution has become a piton, held in place by inertia and political maneuvering, but no longer delivering on its original function for the citizenry.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Corrupt PDVSA officials benefit from the shadow privatization, viewing it as a coordination mechanism to enrich themselves and their allies. They have arbitrage opportunities to move assets and avoid accountability.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Foreign shell corporations experience the shadow privatization as a mix of coordination and extraction. They benefit from access to Venezuelan oil but are exposed to risks of sanctions, legal challenges, and reputational damage. They have the power to be mobile and exit if the risks outweigh the rewards.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% From a civilizational perspective, the shadow privatization represents a mixed bag of coordination (for those who benefit) and extraction (for the Venezuelan people), enforced through corruption and suppression of dissent.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(venezuela_oil_privatization_v1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(venezuela_oil_privatization_v1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(venezuela_oil_privatization_v1, TR),
    TR >= 0.70.

:- end_tests(venezuela_oil_privatization_v1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. The shadow privatization extracts a substantial portion of Venezuela's national oil wealth, benefiting a small elite at the expense of the broader population. Suppression (0.80): High. Dissent is suppressed through political repression, limiting the ability of citizens to challenge the corrupt system. Theater ratio (0.60): Moderate. There is still some pretense of PDVSA operating as a state-owned enterprise, but increasingly it is a facade masking private interests.
 *
 * PERSPECTIVAL GAP:
 *   Venezuelan citizens see a snare - extraction of wealth with no exit. Corrupt PDVSA officials see a rope - a coordination mechanism for self-enrichment. Foreign shell corporations see a tangled rope - a mix of opportunity and risk. PDVSA national interests see a piton - a hollow institution. The Analytical Observer sees tangled rope - a system that coordinates extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the structural position. Victims have a high d, beneficiaries have a low d.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved because the primary structural feature is extraction from citizens. The classification of snare for the Venezuelan citizens represents the correct classification
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_oil_reserves,
    'What are the actual remaining recoverable oil reserves in Venezuela?',
    'Independent audit by international experts, cross-referencing multiple data sources.',
    'Higher reserves would make the extraction seem less urgent, lower reserves amplify the perceived theft.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_oil_reserves, empirical, 'Uncertainty about the quantity of recoverable oil.').

omega_variable(
    corruption_scale,
    'What is the precise scale and destination of funds diverted through corruption?',
    'International investigation with subpoena power; whistleblower testimony.',
    'Higher corruption levels would increase the perceived extraction from citizens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corruption_scale, empirical, 'Uncertainty about the scope of corruption.').

omega_variable(
    regime_stability,
    'How stable is the current political regime in Venezuela?',
    'Political risk analysis; assessment of internal opposition and external pressures.',
    'A more stable regime would entrench the shadow privatization, while a less stable one might offer opportunities for reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_stability, conceptual, 'Uncertainty about regime durability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(venezuela_oil_privatization_v1, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vene_tr_t0, venezuela_oil_privatization_v1, theater_ratio, 0, 0.8).
narrative_ontology:measurement(vene_tr_t5, venezuela_oil_privatization_v1, theater_ratio, 5, 0.7).
narrative_ontology:measurement(vene_tr_t10, venezuela_oil_privatization_v1, theater_ratio, 10, 0.6).

% Extraction over time
narrative_ontology:measurement(vene_be_t0, venezuela_oil_privatization_v1, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(vene_be_t5, venezuela_oil_privatization_v1, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(vene_be_t10, venezuela_oil_privatization_v1, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(venezuela_oil_privatization_v1, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
