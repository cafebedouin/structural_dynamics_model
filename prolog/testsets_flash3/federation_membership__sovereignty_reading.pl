% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership (Sovereignty Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint describes the 'sovereignty reading' of federation
 *   membership, where national authority retains primary legitimacy over
 *   borders and migration policy, and free movement is treated as a
 *   negotiable policy rather than an inherent right. It is a Tangled Rope
 *   because it coordinates inter-state relations while extracting from mobile
 *   citizens through restrictions on movement, requiring active enforcement
 *   to maintain national border controls.
 *
 * KEY AGENTS:
 *   - national_governments: Agenda-setter (institutional/constrained) — benefits from control
 *   - local_labor_markets: Beneficiary (organized/constrained) — benefits from regulated labor supply
 *   - mobile_citizens: Payer (powerless/constrained) — bears costs of restricted movement
 *   - migrant_workers: Payer (powerless/trapped) — bears severe costs of precarious status
 *   - supranational_institutions: Excluded (institutional/constrained) — their authority is resisted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.68).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.75).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '94c5045a-75fe-42c4-91aa-6b7fea5fe0ee').
narrative_ontology:cs_kernel_codification('94c5045a-75fe-42c4-91aa-6b7fea5fe0ee', formalized).
narrative_ontology:cs_authority_grounding('94c5045a-75fe-42c4-91aa-6b7fea5fe0ee', lineage).
narrative_ontology:cs_interpretation_layer_present('94c5045a-75fe-42c4-91aa-6b7fea5fe0ee').
narrative_ontology:cs_reading_relation('94c5045a-75fe-42c4-91aa-6b7fea5fe0ee', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('94c5045a-75fe-42c4-91aa-6b7fea5fe0ee', foundational, national_sovereignty_over_borders_is_primary).
narrative_ontology:cs_axiom_status(national_sovereignty_over_borders_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('94c5045a-75fe-42c4-91aa-6b7fea5fe0ee', national_sovereignty_over_borders_is_primary, deontological).
narrative_ontology:cs_axiom('94c5045a-75fe-42c4-91aa-6b7fea5fe0ee', foundational, free_movement_is_a_negotiable_policy).
narrative_ontology:cs_axiom_status(free_movement_is_a_negotiable_policy, holdable).
narrative_ontology:cs_axiom_grounding('94c5045a-75fe-42c4-91aa-6b7fea5fe0ee', free_movement_is_a_negotiable_policy, conventional).
narrative_ontology:cs_reference_frame('94c5045a-75fe-42c4-91aa-6b7fea5fe0ee', westphalian_state_system).
narrative_ontology:cs_drift_state('94c5045a-75fe-42c4-91aa-6b7fea5fe0ee', contemporary_globalization_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('94c5045a-75fe-42c4-91aa-6b7fea5fe0ee', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_governments).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, migrant_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain ultimate authority over borders and migration policy, viewing federation membership as a conditional treaty. They benefit from the ability to control labor supply and manage social services, but are constrained by existing federation agreements.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the ability to regulate the influx of labor, protecting domestic wages and employment. They exert political pressure on national governments to maintain border controls and restrict free movement.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, local_labor_markets, beneficiary,
    organized, biographical, constrained, local).

% Experience restrictions on their ability to move freely across federation borders for work or residence. They bear the costs of administrative hurdles, limited access to social benefits, and potential discrimination based on national origin within the federation.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    powerless, biographical, constrained, regional).

% Are particularly vulnerable to restrictive policies, facing precarious employment, limited rights, and the constant threat of deportation. Their ability to exit is severely limited by economic necessity and legal status.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, migrant_workers, payer,
    powerless, immediate, trapped, local).

% Are seen as having limited legitimate authority over national border policy in this reading. Their attempts to promote free movement are resisted by national governments, and their influence is often circumvented.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_institutions, excluded,
    institutional, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the terms of national participation in a federation, allowing member states to cooperate on certain issues while retaining sovereignty over others, particularly border control and migration.
% TRANSFER_FUNCTION: Transfers the right to control borders and migration policy from a potential supranational authority back to national governments, and transfers the costs of restricted mobility onto mobile citizens and migrant workers.
% ABSENT_VOICES: Advocates for universal free movement and supranational integration are structurally excluded from the core decision-making processes regarding border legitimacy and migration policy, as this reading prioritizes national sovereignty.
% DISAPPEARANCE_RATIONALE: If this reading of federation membership vanished, national governments would lose their claimed legitimacy for border control, leading to a rapid shift towards more open borders and potentially a stronger supranational authority. The political and economic landscape of the federation would fundamentally reorganize.
% FOUNDING_PROBLEM: The founding problem was to balance the benefits of inter-state cooperation (e.g., trade, security) with the desire of nation-states to retain control over their internal affairs and national identity.
% FOUNDING_PROBLEM_CORROBORATION: National governments and their electorates consistently attest that the balance between cooperation and sovereignty remains a live and critical issue, requiring ongoing negotiation and the assertion of national prerogatives. This is corroborated by public opinion polls and national legislative debates.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high due to the significant costs imposed on mobile citizens and migrant workers through restricted movement and unequal access to rights. Suppression (0.75) is also high, reflecting the active enforcement of border controls and immigration laws by national authorities. The theater ratio (0.20) is relatively low, as the enforcement of national borders is a genuine, functional activity for this reading, not merely performative. The claimed type is 'tangled_rope' because it genuinely coordinates national interests within a federation framework, but does so with significant asymmetric extraction from those seeking mobility.
 *
 * PERSPECTIVAL GAP:
 *   National governments and local labor markets perceive this constraint as a legitimate and necessary coordination mechanism for managing national interests and protecting domestic populations. Mobile citizens and migrant workers, however, experience it as a highly extractive and suppressive barrier to their fundamental freedoms and economic opportunities. Supranational institutions, if they were not excluded, would likely classify it as a snare due to its perceived undermining of integration principles.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and local labor markets are beneficiaries (low d) as they gain control over borders and labor supply. Mobile citizens and migrant workers are targets (high d) as they bear the direct costs of restricted movement and precarious status. Supranational institutions are excluded, meaning their directionality is effectively irrelevant to the constraint's operation in this reading, as their influence is actively suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'rope' (simple coordination) by highlighting the significant extraction and suppression involved. It also avoids mislabeling it as a 'snare' by acknowledging the genuine coordination function for national governments in managing their sovereignty within a federation. The 'tangled_rope' classification accurately captures the hybrid nature, where a coordination story for some masks extraction from others, requiring active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_sovereignty_vs_supranational_legitimacy,
    'Is national authority over borders an inherent, irreducible aspect of sovereignty, or is it a delegable function that can legitimately be transferred to a supranational body?',
    'A shift in international legal norms and political consensus, or a constitutional amendment within the federation explicitly granting supranational institutions primary border authority.',
    'If sovereignty is deemed delegable, this reading''s foundational premise is weakened, potentially reclassifying the constraint towards a snare as national border control becomes an illegitimate extraction. If irreducible, the current classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(national_sovereignty_vs_supranational_legitimacy, conceptual, 'Ambiguity regarding the ultimate locus of legitimate authority over borders.').

omega_variable(
    economic_benefit_vs_human_cost,
    'Do the economic benefits to national labor markets from restricted mobility outweigh the human and economic costs imposed on mobile citizens and migrant workers?',
    'Comprehensive, independent economic and social impact assessments that quantify both benefits and costs, including non-monetary factors like social integration and human rights.',
    'If costs significantly outweigh benefits, the justification for the constraint''s extractiveness is undermined, pushing it closer to a pure snare. If benefits are clearly dominant, the ''tangled_rope'' classification is reinforced, albeit with high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_benefit_vs_human_cost, empirical, 'The net societal impact of restricted mobility policies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t5, federation_membership__sovereignty_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(fede_tr_t10, federation_membership__sovereignty_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(fede_tr_t15, federation_membership__sovereignty_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(fede_tr_t20, federation_membership__sovereignty_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t5, federation_membership__sovereignty_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(fede_be_t10, federation_membership__sovereignty_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(fede_be_t15, federation_membership__sovereignty_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(fede_be_t20, federation_membership__sovereignty_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t5, federation_membership__sovereignty_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(fede_su_t10, federation_membership__sovereignty_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(fede_su_t15, federation_membership__sovereignty_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(fede_su_t20, federation_membership__sovereignty_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'sovereignty reading' of federation membership, which emphasizes national control over borders and migration. It stands in contrast to the 'integration reading' (integration_reading), which posits supranational authority and free movement as a right. The two readings represent a fundamental contest over the nature of the federation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
