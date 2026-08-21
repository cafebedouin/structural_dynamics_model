% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Jurisdictional Sovereignty in Border Control
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'jurisdictional sovereignty' reading of
 *   border control legitimacy, where a state's power to regulate within its
 *   territory does not automatically grant absolute border closure authority.
 *   Instead, legitimacy requires balancing protection obligations (for
 *   citizens and potentially refugees), labor needs, and public consent,
 *   while adhering to proportionality and necessity tests in enforcement. The
 *   constraint is claimed as a 'tangled_rope' because it serves a genuine
 *   coordination function (managing borders) but also involves asymmetric
 *   extraction from excluded migrants and potential costs to citizens if the
 *   balance is not maintained.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.65).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.7).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Jurisdictional Sovereignty in Border Control").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '7148d6f3-efe7-48fd-8985-7015f0589aa7').
narrative_ontology:cs_kernel_codification('7148d6f3-efe7-48fd-8985-7015f0589aa7', formalized).
narrative_ontology:cs_authority_grounding('7148d6f3-efe7-48fd-8985-7015f0589aa7', lineage).
narrative_ontology:cs_interpretation_layer_present('7148d6f3-efe7-48fd-8985-7015f0589aa7').
narrative_ontology:cs_reading_relation('7148d6f3-efe7-48fd-8985-7015f0589aa7', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('7148d6f3-efe7-48fd-8985-7015f0589aa7', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('7148d6f3-efe7-48fd-8985-7015f0589aa7', foundational, territorial_integrity_with_human_rights_limits).
narrative_ontology:cs_axiom_status(territorial_integrity_with_human_rights_limits, holdable).
narrative_ontology:cs_axiom_grounding('7148d6f3-efe7-48fd-8985-7015f0589aa7', territorial_integrity_with_human_rights_limits, deontological).
narrative_ontology:cs_axiom('7148d6f3-efe7-48fd-8985-7015f0589aa7', foundational, managed_migration_for_public_good).
narrative_ontology:cs_axiom_status(managed_migration_for_public_good, holdable).
narrative_ontology:cs_axiom_grounding('7148d6f3-efe7-48fd-8985-7015f0589aa7', managed_migration_for_public_good, instrumental).
narrative_ontology:cs_reference_frame('7148d6f3-efe7-48fd-8985-7015f0589aa7', post_westphalian_state_system_with_human_rights_norms).
narrative_ontology:cs_drift_state('7148d6f3-efe7-48fd-8985-7015f0589aa7', contemporary_globalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7148d6f3-efe7-48fd-8985-7015f0589aa7', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_institutions).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_citizens).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, employers_seeking_labor).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, citizens_impacted_by_unmanaged_migration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, state_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for defining and enforcing border policies, balancing national security, economic needs, and international obligations. They administer the legal framework and enforcement apparatus.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from perceived security, managed labor markets, and social cohesion. They also bear potential costs if migration is perceived as unmanaged or if enforcement is overly costly or inhumane. Their consent is a key legitimacy factor.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_citizens, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, state_citizens, payer).

% Bear the direct costs of exclusion, including denial of entry, separation from family, and precarious legal status. Their human rights are a central concern for the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Monitor state border practices, advocate for the rights of migrants, and challenge policies that violate international human rights law. They exert pressure for proportionality and necessity in enforcement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Benefit from access to a flexible labor supply, often from migrant populations. They lobby for policies that facilitate labor migration to meet economic demands.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, employers_seeking_labor, beneficiary,
    powerful, immediate, mobile, national).

% Interpret and apply international human rights law and refugee conventions, providing a framework against which state border policies are judged. They can issue non-binding recommendations or rulings.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_law_bodies, observer,
    institutional, generational, analytical, global).

% Bear social or economic costs if migration is perceived as unmanaged, leading to strain on public services, wage depression in certain sectors, or cultural anxieties. Their concerns contribute to the 'public consent' aspect of legitimacy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, citizens_impacted_by_unmanaged_migration, payer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To manage the flow of people across a state's territorial boundaries, balancing national security, economic needs (labor supply), and social cohesion (public consent) with international human rights obligations.
% TRANSFER_FUNCTION: Transfers security, social stability, and labor to state citizens and employers, while imposing costs of exclusion, precarity, and human rights violations on excluded migrants, and potential social/economic costs on citizens if the balance is not met.
% ABSENT_VOICES: Migrants who are denied entry and have no legal recourse or representation in policy debates; citizens whose specific local concerns about migration impacts are not adequately integrated into national policy frameworks.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, states would lose a fundamental mechanism for managing their populations, economies, and security. The global movement of people would become largely unregulated, leading to significant geopolitical, economic, and social upheaval, and potentially humanitarian crises.
% FOUNDING_PROBLEM: How to define and enforce the boundaries of a political community, manage population flows, and protect the interests of its members while interacting with a globalized world and respecting universal human rights.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies, migration studies scholars, and various national commissions consistently corroborate the ongoing challenge of balancing these complex factors, indicating the problem remains live and contested.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant costs imposed on excluded migrants and the potential for social/economic costs on citizens. Suppression (0.70) is high due to active border enforcement, but it is not absolute, as the reading implies constraints on arbitrary closure. The theater ratio (0.40) suggests that some enforcement actions are performative, aimed at satisfying public consent or political narratives, even if their functional necessity is debatable. Accessibility collapse (0.60) is moderate, as legal and illegal alternatives for movement exist, but are severely constrained. Resistance (0.55) is substantial, coming from human rights groups, migrant communities, and citizens concerned about either excessive or insufficient border control.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state institutions, this constraint is a necessary tool for national self-determination and order. From the perspective of excluded migrants, it is a barrier to fundamental rights and opportunities. For human rights advocates, it is a framework that must be rigorously tested against international law. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   State institutions are beneficiaries as they maintain order and control. State citizens and employers seeking labor also benefit from managed borders and labor supply. Excluded migrants are clear targets, bearing the direct costs of denial. Citizens impacted by unmanaged migration are also targets, bearing indirect costs. Human rights advocates and international law bodies act as observers, challenging the constraint's operation against its stated principles.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_necessity_enforcement,
    'How strictly are border enforcement actions actually held to proportionality and necessity tests in practice?',
    'Independent audits of border force operations, judicial review of individual cases, and empirical studies on the effectiveness and human cost of specific enforcement measures.',
    'If enforcement consistently fails proportionality/necessity tests, the constraint''s effective suppression and extractiveness are higher than stated, pushing it closer to a Snare. If tests are rigorously applied, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_necessity_enforcement, empirical, 'The practical application of legal limits on border enforcement.').

omega_variable(
    public_consent_measurement_bias,
    'How is ''public consent'' for border policies measured, and does this measurement adequately balance diverse citizen interests and avoid majoritarian bias against minority groups or humanitarian concerns?',
    'Analysis of public opinion polling methodologies, legislative debate records, and the inclusion of diverse stakeholder voices in policy formulation. Examination of whether ''consent'' is manufactured or genuinely deliberative.',
    'If ''public consent'' is found to be narrowly defined or manipulated, the legitimacy claim of the constraint weakens, and its extractive nature (especially towards excluded migrants) becomes more pronounced, potentially shifting it towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_consent_measurement_bias, conceptual, 'The definition and measurement of ''public consent'' in border policy.').

omega_variable(
    dual_victim_balance_efficacy,
    'How effectively does the constraint balance the interests of ''excluded_migrants'' (human rights) and ''citizens_impacted_by_unmanaged_migration'' (social/economic costs)?',
    'Longitudinal studies tracking the well-being of both migrant and citizen populations affected by border policies, including economic impacts, social integration metrics, and human rights compliance reports.',
    'If the balance consistently fails, leading to severe outcomes for either victim group, the constraint''s legitimacy crisis deepens. Persistent severe harm to excluded migrants without effective redress would push it towards a Snare; persistent severe harm to impacted citizens would indicate a failure of the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_victim_balance_efficacy, empirical, 'The practical balance between the two victim sets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.3).
narrative_ontology:measurement(bord_tr_t5, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 5, 0.33).
narrative_ontology:measurement(bord_tr_t10, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 10, 0.36).
narrative_ontology:measurement(bord_tr_t15, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 15, 0.38).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bord_be_t5, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(bord_be_t10, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(bord_be_t15, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(bord_su_t5, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(bord_su_t10, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(bord_su_t15, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
