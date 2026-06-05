% ============================================================================
% CONSTRAINT STORY: fptp_electoral_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fptp_electoral_system, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: fptp_electoral_system
 *   human_readable: The "First-Past-the-Post" (Plurality) Electoral System
 *   domain: political
 *
 * SUMMARY:
 *   The First-Past-the-Post (FPTP) voting system is a winner-take-all
 *   electoral mechanism where the candidate with the most votes in a district
 *   wins, regardless of whether they achieve a majority. This system can lead
 *   to disproportionate representation, where a party can win a majority of
 *   seats with less than a majority of the popular vote. It often
 *   disadvantages smaller parties and minority groups, leading to strategic
 *   voting and reduced voter turnout in areas where one party dominates.
 *
 * KEY AGENTS:
 *   - Minority Voters: Primary target (powerless/trapped) - consistently underrepresented
 *   - Major Political Parties: Primary beneficiary (institutional/arbitrage) - consolidates power
 *   - Third-Party Voters: Secondary target (moderate/constrained) - vote may be wasted
 *   - Incumbent Politicians: Benefit from name recognition and existing infrastructure (institutional/constrained)
 *   - Analytical Observer: Examines the system's effects on representation and voter behavior (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fptp_electoral_system, 0.6).
domain_priors:suppression_score(fptp_electoral_system, 0.7).
domain_priors:theater_ratio(fptp_electoral_system, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fptp_electoral_system, extractiveness, 0.6).
narrative_ontology:constraint_metric(fptp_electoral_system, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fptp_electoral_system, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fptp_electoral_system, snare).
narrative_ontology:human_readable(fptp_electoral_system, "The \"First-Past-the-Post\" (Plurality) Electoral System").
narrative_ontology:topic_domain(fptp_electoral_system, "political").

domain_priors:requires_active_enforcement(fptp_electoral_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fptp_electoral_system, major_political_parties).
narrative_ontology:constraint_beneficiary(fptp_electoral_system, incumbent_politicians).
narrative_ontology:constraint_victim(fptp_electoral_system, minority_voters).
narrative_ontology:constraint_victim(fptp_electoral_system, third_party_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of minority voters who consistently find their preferred candidates unable to win under FPTP. They are trapped in a system where their votes are often strategically wasted.
constraint_indexing:constraint_classification(fptp_electoral_system, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of major political parties who benefit from the system by consolidating power and limiting the viability of smaller parties. They can arbitrage the system by focusing resources on key districts.
constraint_indexing:constraint_classification(fptp_electoral_system, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of voters who support third-party candidates. They are constrained by the system's tendency to favor larger parties, but may occasionally influence the outcome or push major parties to adopt certain policies. Their vote has strategic consequences, constraining their options.
constraint_indexing:constraint_classification(fptp_electoral_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of incumbent politicians who benefit from the system's tendency to favor established candidates. They see the system as a degraded form of rope that maintains their status through institutional inertia.
constraint_indexing:constraint_classification(fptp_electoral_system, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical perspective views the system as a tangled rope, providing a coordination function by producing governments, but also extracting from minority interests and suppressing alternative electoral systems.
constraint_indexing:constraint_classification(fptp_electoral_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fptp_electoral_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fptp_electoral_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fptp_electoral_system, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fptp_electoral_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fptp_electoral_system, TR),
    TR >= 0.70.

:- end_tests(fptp_electoral_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The system extracts from minority voters by suppressing their representation. The beneficiaries are the major parties and incumbent politicians who perpetuate the system for their own benefit. The theater ratio is relatively high because the system does produce governments, but the quality of representation is questionable. Extractiveness is 0.6 because while there is a government forming capability, there is also very high extraction. Suppression is 0.7, because alternatives are actively blocked by those in power.
 *
 * PERSPECTIVAL GAP:
 *   The gap exists because major parties benefit from the system's power consolidation, while minority voters are trapped in a cycle of underrepresentation. The analytical observer sees both the coordination and extraction aspects of the system, leading to the tangled rope classification. The incumbent politician finds themself propped up by inertia, but still constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   Major political parties are declared beneficiaries because the system consolidates power in their hands. Minority and third-party voters are declared victims because their representation is suppressed. Incumbent politicians benefit from the status quo, but are also somewhat constrained by the system's limitations.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a Snare because of the high levels of extraction from the voters, especially from minorities. Voters would benefit from alternative systems that allow them to have better representation. It prevents mislabeling because it is obvious that the powerful are benefiting at the expense of the powerless in this system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    electoral_reform_viability,
    'Is electoral reform politically viable given the entrenched interests of major parties?',
    'Analysis of public opinion, political party platforms, and historical attempts at electoral reform.',
    'If reform is viable, the system may transition to a scaffold or rope. If not, it remains a snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_reform_viability, empirical, 'Political viability of electoral reform').

omega_variable(
    strategic_voting_impact,
    'How significantly does strategic voting distort the representation of voter preferences?',
    'Statistical analysis of voting patterns and simulation of alternative electoral systems.',
    'If strategic voting is high, the system''s extractiveness is amplified. If low, the system is closer to a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_voting_impact, empirical, 'Impact of strategic voting on representation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fptp_electoral_system, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fptp_tr_t0, fptp_electoral_system, theater_ratio, 0, 0.4).
narrative_ontology:measurement(fptp_tr_t5, fptp_electoral_system, theater_ratio, 5, 0.6).
narrative_ontology:measurement(fptp_tr_t10, fptp_electoral_system, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(fptp_be_t0, fptp_electoral_system, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(fptp_be_t5, fptp_electoral_system, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(fptp_be_t10, fptp_electoral_system, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fptp_electoral_system, enforcement_mechanism).
narrative_ontology:affects_constraint(fptp_electoral_system, gerrymandering).
narrative_ontology:affects_constraint(fptp_electoral_system, voter_suppression).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
