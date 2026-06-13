% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation in Border Legitimacy
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint represents the 'humanitarian obligation' reading of
 *   border legitimacy, asserting that states have a moral and legal duty to
 *   admit those fleeing persecution or disaster, but retain the right to
 *   exclude general economic migrants. This creates a bifurcated system where
 *   some migrants are protected, while others face exclusion. The constraint
 *   is actively enforced through border controls and asylum processing,
 *   leading to a tangled rope classification due to its dual function of
 *   coordination (for humanitarian protection) and extraction (from economic
 *   migrants).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.45).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.7).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation in Border Legitimacy").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c').
narrative_ontology:cs_kernel_codification('0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c', formalized).
narrative_ontology:cs_authority_grounding('0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c', lineage).
narrative_ontology:cs_interpretation_layer_present('0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c').
narrative_ontology:cs_reading_relation('0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_axiom('0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c', foundational, state_has_humanitarian_duty).
narrative_ontology:cs_axiom_status(state_has_humanitarian_duty, holdable).
narrative_ontology:cs_axiom_grounding('0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c', state_has_humanitarian_duty, deontological).
narrative_ontology:cs_axiom('0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c', foundational, state_retains_right_to_exclude_economic_migrants).
narrative_ontology:cs_axiom_status(state_retains_right_to_exclude_economic_migrants, holdable).
narrative_ontology:cs_axiom_grounding('0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c', state_retains_right_to_exclude_economic_migrants, conventional).
narrative_ontology:cs_reference_frame('0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c', post_wwii_refugee_convention_framework).
narrative_ontology:cs_drift_state('0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c', contemporary_climate_migration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0ca05a48-9f5d-4b4f-b4e4-320459c7ce0c', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, states_maintaining_sovereignty).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, citizens_of_receiving_states).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_with_weak_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, genuine_refugees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states define and enforce border policies, balancing national interests with international obligations. They benefit from controlling who enters their territory and from the perceived legitimacy of their humanitarian efforts, while managing the costs of asylum processing and integration.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, states_maintaining_sovereignty, agenda_setter,
    institutional, generational, mobile, national).

% These citizens benefit from the perceived order and security of controlled borders, and from the moral satisfaction of their state fulfilling humanitarian duties. They may also bear some costs through taxes for migrant support, but generally support the distinction between refugees and economic migrants.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, citizens_of_receiving_states, beneficiary,
    organized, biographical, analytical, national).

% Individuals fleeing persecution or disaster who, in principle, should be admitted. Their lives depend on this obligation, but they face significant barriers in proving their claims and navigating complex legal systems, making their 'beneficiary' status highly conditional and precarious.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, genuine_refugees, beneficiary,
    powerless, immediate, trapped, global).

% Individuals seeking better economic opportunities who are explicitly excluded by this reading. They face border walls, detention, and deportation, bearing the full cost of the constraint's exclusionary function. Their options are to attempt illegal entry, return to their home country, or seek legal pathways that are often non-existent.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, immediate, trapped, global).

% Individuals who claim asylum but whose cases do not meet the strict legal definitions of 'refugee' under this reading. They are caught in a lengthy, often dehumanizing process, facing high legal costs, uncertainty, and eventual deportation, bearing significant personal and financial burdens.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_with_weak_claims, payer,
    powerless, immediate, constrained, global).

% These organizations monitor state compliance with international law and advocate for broader protections for all migrants. They challenge the narrow definitions of 'refugee' and the enforcement practices that lead to human rights violations, often litigating on behalf of migrants.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate international efforts to protect those genuinely fleeing persecution or disaster, while allowing states to manage their borders and populations according to national interests.
% TRANSFER_FUNCTION: Transfers security and stability to receiving states and their citizens by controlling entry, while transferring the burden of displacement and exclusion onto economic migrants and those whose claims are deemed 'weak'.
% ABSENT_VOICES: Those advocating for a universal right to freedom of movement are largely excluded from the policy-making discourse, as are the voices of economic migrants themselves, who would challenge the legitimacy of their categorical exclusion.
% DISAPPEARANCE_RATIONALE: If this distinction vanished, states would either face an uncontrolled influx of all migrants (leading to significant social and economic reorganization) or would default to a purely exclusionary 'sovereignty' model, abandoning humanitarian obligations. The current system's balance, however imperfect, structures global migration.
% FOUNDING_PROBLEM: The problem of managing large-scale human displacement due to conflict and disaster, while preserving state sovereignty and national identity, particularly in the post-WWII era with the rise of international refugee law.
% FOUNDING_PROBLEM_CORROBORATION: International legal bodies (UNHCR), human rights organizations, and many states attest that the problem of managing forced displacement remains live and complex. However, critics (e.g., freedom of movement advocates) argue that the 'solution' has become a tool for exclusion rather than genuine humanitarian aid.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).
:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while genuine refugees are theoretically admitted, the distinction between 'refugee' and 'economic migrant' is often contested and used to justify exclusion. Suppression (0.7) is high due to robust border enforcement and legal mechanisms designed to deter unauthorized entry. Theater ratio (0.2) is low, as the humanitarian function is genuinely performed for some, but the distinction-making process itself can be performative to justify exclusion. The metrics reflect a system that, while claiming a humanitarian purpose, also actively manages and restricts migration flows.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states, this constraint is a necessary balance between sovereignty and humanitarian duty. From the perspective of economic migrants, it is a Snare that denies fundamental human mobility. Genuine refugees experience it as a Rope, albeit one with significant friction and uncertainty. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   States maintaining sovereignty and their citizens are beneficiaries (d near 0.0-0.2), as they control who enters and preserve national resources. Economic migrants and asylum seekers with weak claims are victims (d near 0.8-1.0), facing exclusion and hardship. Genuine refugees are intended beneficiaries, but their actual experience can vary widely depending on the rigor and fairness of the asylum process, placing them closer to a constrained position (d around 0.5-0.7) due to the high stakes and often difficult process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine humanitarian obligation, or a cover for selective exclusion?',
    'Analysis of state practice: if states consistently admit genuine refugees and process claims fairly, it supports the humanitarian framing. If claims are systematically denied or processes are designed to deter, it suggests a cover for exclusion.',
    'If a genuine obligation, the constraint functions as a Rope for refugees and a Snare for economic migrants. If a cover, it is a Snare for all excluded parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''humanitarian_obligation_reading'' of the ''border_legitimacy'' kernel. Sibling readings include ''sovereignty_reading'' (states have a right to exclude) and ''freedom_of_movement_reading'' (borders are illegitimate). This reading creates a bifurcated victim set and allows for moderate extraction on categorical exclusion.').

omega_variable(
    economic_migrant_definition_ambiguity,
    'How is ''economic migrant'' defined, and does this definition genuinely exclude those with legitimate, albeit non-persecution-based, needs?',
    'Legal review of national asylum and migration laws, and empirical analysis of how ''economic migrant'' status is applied in practice, particularly for those fleeing climate disaster or generalized violence not meeting refugee criteria.',
    'A narrow, rigid definition of ''economic migrant'' that excludes those with genuine needs would increase the effective extraction and suppression for a broader class of individuals, pushing the constraint closer to a Snare. A more flexible definition would reduce it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_migrant_definition_ambiguity, empirical, 'Ambiguity in defining ''economic migrant'' vs. those fleeing other forms of hardship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bord_tr_t10, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(bord_tr_t20, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(bord_tr_t30, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bord_be_t10, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(bord_be_t20, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(bord_be_t30, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(bord_su_t10, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(bord_su_t20, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(bord_su_t30, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
