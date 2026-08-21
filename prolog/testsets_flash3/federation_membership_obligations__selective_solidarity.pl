% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__selective_solidarity, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Federation Membership Obligations: Selective Solidarity Reading
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes the 'selective solidarity' reading of
 *   federation membership obligations, where free movement rights are tiered
 *   based on an individual's economic contribution and activity status.
 *   Welfare access is tied to a contributory principle rather than a
 *   universal citizenship principle. This reading aims to reconcile national
 *   welfare state sustainability with federal free movement, but it creates a
 *   bifurcated class of mobile workers: those with full rights (economically
 *   active) and those with restricted rights (economically inactive). The
 *   constraint is actively enforced by member states through national
 *   legislation and administrative practices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.68).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.75).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Federation Membership Obligations: Selective Solidarity Reading").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, '914f9f62-6233-4fc6-80de-ba81a2a83f41').
narrative_ontology:cs_kernel_codification('914f9f62-6233-4fc6-80de-ba81a2a83f41', formalized).
narrative_ontology:cs_authority_grounding('914f9f62-6233-4fc6-80de-ba81a2a83f41', extraction).
narrative_ontology:cs_interpretation_layer_present('914f9f62-6233-4fc6-80de-ba81a2a83f41').
narrative_ontology:cs_reading_relation('914f9f62-6233-4fc6-80de-ba81a2a83f41', federation_membership_obligations__integration_primary, influences).
narrative_ontology:cs_reading_relation('914f9f62-6233-4fc6-80de-ba81a2a83f41', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('914f9f62-6233-4fc6-80de-ba81a2a83f41', foundational, welfare_access_by_contribution).
narrative_ontology:cs_axiom_status(welfare_access_by_contribution, holdable).
narrative_ontology:cs_axiom_grounding('914f9f62-6233-4fc6-80de-ba81a2a83f41', welfare_access_by_contribution, conventional).
narrative_ontology:cs_axiom('914f9f62-6233-4fc6-80de-ba81a2a83f41', foundational, fiscal_sustainability_priority).
narrative_ontology:cs_axiom_status(fiscal_sustainability_priority, holdable).
narrative_ontology:cs_axiom_grounding('914f9f62-6233-4fc6-80de-ba81a2a83f41', fiscal_sustainability_priority, instrumental).
narrative_ontology:cs_reference_frame('914f9f62-6233-4fc6-80de-ba81a2a83f41', contributory_welfare_federalism).
narrative_ontology:cs_drift_state('914f9f62-6233-4fc6-80de-ba81a2a83f41', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('914f9f62-6233-4fc6-80de-ba81a2a83f41', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, net_contributor_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, economically_active_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, net_recipient_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and enforce policies that link free movement rights to economic contribution, aiming to reduce perceived 'welfare tourism' and protect national welfare budgets. They benefit from reduced social expenditure on non-contributing migrants.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, net_contributor_member_states, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from relatively unhindered free movement and access to social security systems, provided they maintain employment and contribute to the host state's welfare system. Their rights are secured by their economic activity.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_active_migrants, beneficiary,
    moderate, biographical, mobile, regional).

% Face significant restrictions on welfare access and may be subject to deportation if they become a 'burden' on the host state's social assistance system. Their free movement rights are effectively curtailed by their economic status.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_migrants, payer,
    powerless, immediate, trapped, local).

% Often face pressure to implement stricter welfare access rules for migrants, even if it contradicts broader integration goals, to avoid being seen as 'welfare magnets'. They bear the social and political costs of managing a tiered system.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, net_recipient_member_states, payer,
    institutional, generational, constrained, national).

% Interprets EU law regarding free movement and welfare access, often balancing the rights of individuals against the fiscal concerns of member states. Its rulings shape the practical application of this constraint.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, european_court_of_justice, observer,
    institutional, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the fiscal sustainability of national welfare systems with the principle of free movement within a federal structure, by linking welfare access to individual economic contribution.
% TRANSFER_FUNCTION: Transfers the burden of welfare provision for economically inactive migrants from host member states to either the migrants themselves (through exclusion) or their states of origin, while securing free movement for economically active individuals.
% ABSENT_VOICES: Advocacy groups for migrant rights and social justice organizations would argue for universal welfare access based on residency, not contribution, and for a more robust federal solidarity mechanism to support all citizens.
% DISAPPEARANCE_RATIONALE: If this tiered system vanished, member states would either face immediate fiscal pressure from universal welfare access for all residents (including inactive migrants) or would revert to stricter national border controls, fundamentally altering the nature of free movement within the federation.
% FOUNDING_PROBLEM: The tension between national welfare state sovereignty and the principle of free movement, particularly concerning the fiscal impact of economically inactive migrants on host states' social security systems.
% FOUNDING_PROBLEM_CORROBORATION: Member states, particularly net contributors, consistently raise concerns about 'welfare tourism' and the sustainability of their social systems. Academic studies on migration and welfare state sustainability, from outside the directly benefiting parties, corroborate the ongoing nature of this tension, even if they dispute the policy response.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__selective_solidarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__selective_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is substantial, as it imposes significant costs on economically inactive migrants by denying them welfare access and potentially restricting their movement. Suppression (0.75) is high due to the active enforcement by member states to prevent 'welfare tourism' and the legal mechanisms used to differentiate migrant rights. Theater ratio (0.20) is relatively low, as the policies are genuinely implemented and have real effects, though the rhetoric around 'welfare tourism' may contain performative elements. Accessibility collapse (0.45) is moderate, as alternatives (e.g., returning to the state of origin, finding employment) exist but are often constrained. Resistance (0.60) is significant from migrant advocacy groups and some member states.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of net contributor member states, this constraint is a necessary coordination mechanism for fiscal sustainability. From the perspective of economically inactive migrants, it is a snare that denies fundamental rights based on economic status. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Net contributor member states and economically active migrants are beneficiaries, as the former protect their welfare budgets and the latter enjoy free movement. Economically inactive migrants are clear victims, facing restricted rights and potential exclusion. Net recipient member states are also victims, as they are pressured to adopt policies that may contradict their broader social integration goals or face political backlash.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_sustainability_vs_social_cohesion,
    'Is the fiscal sustainability achieved by selective solidarity outweighed by its impact on social cohesion and the principle of equal treatment for all citizens within the federation?',
    'Longitudinal studies on social integration, migrant well-being, and public attitudes in states implementing selective solidarity policies, compared to those with more universal access.',
    'If social cohesion is severely undermined, the constraint''s long-term viability as a coordination mechanism is questionable, potentially leading to reclassification towards a snare or a piton if the social costs become unsustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_vs_social_cohesion, empirical, 'Balancing fiscal benefits against social costs of tiered rights.').

omega_variable(
    contributory_principle_legitimacy,
    'Is the ''contributory principle'' a legitimate basis for differentiating welfare access among citizens of a common federation, or does it fundamentally undermine the concept of federal citizenship?',
    'Legal challenges and rulings by federal courts (e.g., ECJ) on the compatibility of such policies with foundational federal treaties and human rights principles.',
    'A ruling against the contributory principle would force a re-evaluation of the constraint''s legitimacy, potentially shifting it towards a snare if maintained through pure coercion, or a scaffold if a transitional period for reform is mandated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contributory_principle_legitimacy, conceptual, 'Legitimacy of economic contribution as a basis for rights differentiation.').

omega_variable(
    alternative_solidarity_mechanisms,
    'Are there viable federal-level solidarity mechanisms (e.g., a federal welfare fund) that could address member states'' fiscal concerns without resorting to tiered rights for mobile citizens?',
    'Feasibility studies and political negotiations on the creation and implementation of federal fiscal transfer mechanisms or social safety nets.',
    'The existence of a viable, less extractive alternative would expose the current constraint as a less efficient or more extractive choice, potentially reclassifying it as a tangled rope or snare if the alternative is suppressed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_solidarity_mechanisms, preference, 'Availability of less extractive federal solidarity mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__selective_solidarity, theater_ratio, 5, 0.23).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__selective_solidarity, theater_ratio, 10, 0.21).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__selective_solidarity, theater_ratio, 15, 0.2).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__selective_solidarity, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__selective_solidarity, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__selective_solidarity, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__selective_solidarity, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__selective_solidarity, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__selective_solidarity, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__selective_solidarity, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__selective_solidarity, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__selective_solidarity, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
