% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Border Normative Status: Sovereignty Primary Reading
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty primary' reading of the
 *   normative status of borders, asserting that states have foundational
 *   authority to control their borders as an instrument of collective
 *   self-determination. This reading legitimizes exclusion and places the
 *   burden of justification on those seeking entry, treating the displacement
 *   of non-citizens as an externality rather than a core issue. The metrics
 *   reflect the high extractiveness and suppression inherent in this strong
 *   sovereignty claim, particularly for excluded migrants.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.65).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.88).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Border Normative Status: Sovereignty Primary Reading").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, '72ef727d-598a-49e2-bad0-091df608f9e5').
narrative_ontology:cs_kernel_codification('72ef727d-598a-49e2-bad0-091df608f9e5', formalized).
narrative_ontology:cs_authority_grounding('72ef727d-598a-49e2-bad0-091df608f9e5', lineage).
narrative_ontology:cs_interpretation_layer_present('72ef727d-598a-49e2-bad0-091df608f9e5').
narrative_ontology:cs_reading_relation('72ef727d-598a-49e2-bad0-091df608f9e5', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('72ef727d-598a-49e2-bad0-091df608f9e5', border_normative_status__qualified_sovereignty, coexists_with).
narrative_ontology:cs_axiom('72ef727d-598a-49e2-bad0-091df608f9e5', foundational, state_territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(state_territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('72ef727d-598a-49e2-bad0-091df608f9e5', state_territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('72ef727d-598a-49e2-bad0-091df608f9e5', foundational, collective_self_determination_prioritizes_members).
narrative_ontology:cs_axiom_status(collective_self_determination_prioritizes_members, holdable).
narrative_ontology:cs_axiom_grounding('72ef727d-598a-49e2-bad0-091df608f9e5', collective_self_determination_prioritizes_members, deontological).
narrative_ontology:cs_reference_frame('72ef727d-598a-49e2-bad0-091df608f9e5', westphalian_sovereignty_model).
narrative_ontology:cs_drift_state('72ef727d-598a-49e2-bad0-091df608f9e5', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('72ef727d-598a-49e2-bad0-091df608f9e5', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, member_citizens).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, state_apparatus).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the perceived security, cultural cohesion, and resource allocation within the state's borders, protected from external pressures. Their self-determination is asserted through the state's border control.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, member_citizens, beneficiary,
    organized, generational, mobile, national).

% Exercises foundational authority to control borders, enforce immigration laws, and define membership. Its legitimacy is derived from its capacity to protect the collective self-determination of its citizens.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Bear the direct costs of exclusion, including denied entry, detention, deportation, and separation from families or opportunities. Their movement is severely restricted by state authority.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Face significant barriers to entry and often prolonged, precarious legal processes, despite international protections. Their claims are often subordinated to state sovereignty concerns.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Monitor border practices and advocate for the rights of migrants and asylum seekers, often challenging the absolute nature of state sovereignty claims. They operate within the existing legal and political frameworks.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, human_rights_advocates, observer,
    moderate, generational, analytical, global).

% Analyze the legal and ethical foundations of state sovereignty and border control, often highlighting tensions between state rights and individual rights. Their work influences policy debates but does not directly enforce outcomes.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, international_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective self-determination of a national community by defining its territorial and membership boundaries, enabling shared governance and resource allocation within those limits.
% TRANSFER_FUNCTION: Transfers the right to reside, work, and access state resources from non-members to member citizens, and transfers the costs of exclusion (e.g., displacement, denied opportunity) to non-members.
% ABSENT_VOICES: Migrants and stateless persons, who are directly affected by border policies but lack political representation or voice within the states that exclude them. Their perspectives are mediated through advocates or international bodies.
% DISAPPEARANCE_RATIONALE: If the normative status of borders as instruments of self-determination vanished, states would lose their primary justification for exclusion. This would lead to a fundamental reordering of global governance, citizenship, and resource distribution, as populations would move more freely, challenging existing national structures.
% FOUNDING_PROBLEM: The need for political communities to define themselves, manage their internal affairs, and protect their members' interests against external interference, particularly in a world of competing states.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and member citizens attest the problem is live, citing national security, economic stability, and cultural preservation. This is corroborated by the continued existence and function of the international state system, though challenged by human rights organizations and some international legal scholars.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant costs imposed on non-members, while the high suppression (0.88) indicates the active enforcement required to maintain these boundaries against human movement. The low theater ratio (0.1) suggests that border enforcement is largely functional in achieving its stated goal of exclusion, with minimal performative aspects. Resistance (0.4) is present from migrants and advocates but is often diffuse and outmatched by state power. Accessibility collapse (0.75) is high for those without legal pathways, as alternatives are severely limited.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of member citizens and the state, this constraint is a legitimate and necessary mechanism for self-governance and protection. From the perspective of excluded migrants, it is a highly extractive and suppressive barrier that denies fundamental freedoms. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Member citizens and the state apparatus are clear beneficiaries, gaining security, control, and the ability to define their collective. Excluded migrants and asylum seekers are the primary victims, bearing the costs of denied entry and restricted movement. Human rights advocates and international law scholars act as observers, analyzing and challenging the constraint without directly benefiting or being victimized by its core operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (collective self-determination via border control) is actively asserted and enforced. The contest is over the legitimacy and proportionality of this mandate, not its atrophy. The classification as Tangled Rope reflects the genuine coordination function for members alongside the asymmetric extraction from non-members, requiring active enforcement to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_self_determination_scope,
    'Does ''collective self-determination'' legitimately extend to absolute control over who enters a territory, or are there inherent limits imposed by universal human rights?',
    'International legal precedent from cases challenging border policies on human rights grounds, or philosophical consensus on the scope of collective rights versus individual rights.',
    'If limits are found, the ''sovereignty primary'' reading''s legitimacy would be undermined, potentially shifting its classification towards a Snare or a more constrained Tangled Rope. If absolute control is affirmed, the current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_self_determination_scope, conceptual, 'The conceptual boundary of collective self-determination in relation to border control.').

omega_variable(
    economic_impact_of_exclusion,
    'What are the long-term economic costs and benefits of strict exclusion policies for both sending and receiving states, beyond immediate security concerns?',
    'Comprehensive, longitudinal economic studies comparing states with different border regimes, accounting for demographic changes, labor market needs, and innovation.',
    'If significant economic costs are demonstrated for receiving states, the ''sovereignty primary'' reading''s instrumental justification would weaken, potentially leading to policy shifts that reduce extractiveness. If benefits are clear, the reading''s practical support is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_of_exclusion, empirical, 'Empirical assessment of the economic consequences of border exclusion.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, physical enforcement) or internalized (fear, lack of information, identity-based resignation among migrants)?',
    'Post-exit suppression trajectory: if suppression persists for migrants after legal barriers are removed (e.g., through trauma or social exclusion), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — migrants carry the suppression with them after exit or even after gaining legal status, impacting their integration and well-being.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for excluded migrants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__sovereignty_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bord_be_t10, border_normative_status__sovereignty_primary, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(bord_be_t20, border_normative_status__sovereignty_primary, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(bord_be_t30, border_normative_status__sovereignty_primary, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(bord_be_t40, border_normative_status__sovereignty_primary, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(bord_be_t50, border_normative_status__sovereignty_primary, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__sovereignty_primary, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(bord_su_t10, border_normative_status__sovereignty_primary, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(bord_su_t20, border_normative_status__sovereignty_primary, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(bord_su_t30, border_normative_status__sovereignty_primary, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(bord_su_t40, border_normative_status__sovereignty_primary, suppression_requirement, 40, 0.87).
narrative_ontology:measurement(bord_su_t50, border_normative_status__sovereignty_primary, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'border_normative_status' kernel. It focuses on the state's foundational authority to exclude, distinct from readings emphasizing freedom of movement or qualified sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
