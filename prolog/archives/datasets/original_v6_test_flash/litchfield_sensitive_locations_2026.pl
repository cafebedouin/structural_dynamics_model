% ============================================================================
% CONSTRAINT STORY: litchfield_sensitive_locations_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_litchfield_sensitive_locations_2026, []).

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
 *   constraint_id: litchfield_sensitive_locations_2026
 *   human_readable: Litchfield School Perimeter Crisis
 *   domain: political/social
 *
 * SUMMARY:
 *   On Feb 5, 2026, federal agents were spotted within a block of the School
 *   of St. Barnaby in Litchfield, Connecticut, sparking a community uproar.
 *   The stated reason was heightened vigilance concerning potential terrorist
 *   activity. This incident resulted in increased surveillance, restricted
 *   access to public spaces, and a general feeling of unease among residents.
 *   The situation raises questions about the balance between security
 *   measures and civil liberties.
 *
 * KEY AGENTS:
 *   - Litchfield Residents: Primary victims (powerless/trapped) - experience increased surveillance and restrictions.
 *   - Student Families: Secondary victims (moderate/constrained) - concerned about safety and education.
 *   - Civil Liberties Groups: Organized advocates (organized/mobile) - concerned about rights violations.
 *   - Federal Law Enforcement: Institutional actor (institutional/constrained) - aims for security but risks distrust.
 *   - Local Political Establishment: Institutional beneficiary (institutional/arbitrage) - consolidating power and extracting federal resources.
 *   - Analytical Observer: Assesses (analytical/analytical) - complex entanglement of security and liberties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(litchfield_sensitive_locations_2026, 0.65).
domain_priors:suppression_score(litchfield_sensitive_locations_2026, 0.75).
domain_priors:theater_ratio(litchfield_sensitive_locations_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, extractiveness, 0.65).
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(litchfield_sensitive_locations_2026, snare).
narrative_ontology:human_readable(litchfield_sensitive_locations_2026, "Litchfield School Perimeter Crisis").
narrative_ontology:topic_domain(litchfield_sensitive_locations_2026, "political/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(litchfield_sensitive_locations_2026, federal_law_enforcement).
narrative_ontology:constraint_beneficiary(litchfield_sensitive_locations_2026, local_political_establishment).
narrative_ontology:constraint_victim(litchfield_sensitive_locations_2026, litchfield_residents).
narrative_ontology:constraint_victim(litchfield_sensitive_locations_2026, student_families).
narrative_ontology:constraint_victim(litchfield_sensitive_locations_2026, civil_liberties_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Residents feel trapped and powerless, subject to increased surveillance and potential disruptions to their daily lives. No real exit options.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Families are constrained by the need to ensure their children's safety and education. They may feel forced to accept the situation, even if they have reservations. Some exit options available but costly (relocation).
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% Civil liberties groups have some power to organize and protest, and to potentially challenge the actions legally. They benefit from public attention but face suppression from the political establishment. Mobile exit option - can shift focus elsewhere.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% From the perspective of the federal agents, there is a degraded sense of achieving their intended purpose of community protection with potential distrust eroding relationships. The institution is constrained to act despite uncertain impact.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From the perspective of the local political establishment, the crisis can be framed as a necessary measure to ensure public safety, thus consolidating their power and leveraging a higher threat profile to extract greater resources from federal agencies. Exit via blame deflection.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The analytical observer recognizes a complex mix of security concerns, political opportunism, and erosion of civil liberties. A tangled rope, where the claimed purpose is intertwined with extraction from local communities.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(litchfield_sensitive_locations_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(litchfield_sensitive_locations_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(litchfield_sensitive_locations_2026, TR),
    TR >= 0.70.

:- end_tests(litchfield_sensitive_locations_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: High (0.65). The local community bears the cost of increased surveillance, restricted access, and potential disruptions. The federal law enforcement benefits by demonstrating action and the local political establishment benefits from consolidating power. Suppression: High (0.75). Limited exit options for residents who feel trapped. Theater Ratio: Low (0.30). The actual functional security improvement is questionable compared to the disruption caused.
 *
 * PERSPECTIVAL GAP:
 *   The local residents and student families experience the situation as a snare. Civil liberties groups see the restrictions as a tangle of political posturing and rights violations. The federal law enforcement might perceive it as a necessary but imperfect tool constrained by trust erosion. Local establishment may see genuine coordination (rope) with federal agencies to leverage resources. The analytical observer identifies a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are determined by the agent's relationship to the imposition of federal authority. The beneficiaries have arbitrage exits. The residents and families have limited exit options (trapped/constrained), resulting in high directionality (d) and thus high experienced extraction (chi). The civil liberties group has the mobile option and thus lower extraction. The local establishment, as a beneficiary, experiences negative effective extraction as they gain political power.
 *
 * MANDATROPHY ANALYSIS:
 *   The situation is classified as a snare from the perspective of the affected community. However, other perspectives may view it differently. The central challenge is determining the appropriate level of security measures relative to the actual threat and the impact on civil liberties. This is a classic example of a situation where a well-intentioned action can have unintended consequences and disproportionately affect certain groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_threat_level,
    'What is the actual level of threat that necessitated the increased federal presence?',
    'Independent threat assessment by non-governmental security experts.',
    'If the threat is low, the justification for increased security measures is undermined. If the threat is high, the security measures may be warranted, changing classifications from snare to rope/scaffold for some perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_threat_level, empirical, 'Assessment of the real threat level driving increased federal presence.').

omega_variable(
    political_motivation,
    'To what extent are the increased security measures driven by political motivations rather than genuine security concerns?',
    'Analysis of public statements, internal memos, and financial records of relevant political actors.',
    'If political motivations are strong, the crisis is likely a snare for the local community. If genuine security concerns prevail, it could be a rope/scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_motivation, conceptual, 'Influence of political agendas on security decisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(litchfield_sensitive_locations_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(litc_tr_t0, litchfield_sensitive_locations_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(litc_tr_t3, litchfield_sensitive_locations_2026, theater_ratio, 3, 0.2).
narrative_ontology:measurement(litc_tr_t6, litchfield_sensitive_locations_2026, theater_ratio, 6, 0.3).

% Extraction over time
narrative_ontology:measurement(litc_be_t0, litchfield_sensitive_locations_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(litc_be_t3, litchfield_sensitive_locations_2026, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(litc_be_t6, litchfield_sensitive_locations_2026, base_extractiveness, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(litchfield_sensitive_locations_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(litchfield_sensitive_locations_2026, mass_surveillance_state).
narrative_ontology:affects_constraint(litchfield_sensitive_locations_2026, erosion_of_civil_liberties).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
