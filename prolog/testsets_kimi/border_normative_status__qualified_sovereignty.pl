% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Qualified Sovereignty Border Control Norm
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates the qualified_sovereignty reading of the
 *   border_normative_status kernel: states retain border control authority
 *   conditioned on proportionality and consistency with human rights
 *   obligations. It operates through treaty regimes (1951 Refugee Convention,
 *   ECHR, ICCPR), international and domestic court adjudication, and state
 *   administrative apparatus. The constraint coordinates state behavior by
 *   providing a shared legitimising framework, while extracting from excluded
 *   migrants and displaced citizens through legally sanctioned exclusion, and
 *   imposing adjudication burdens on states. It is contested by the
 *   sovereignty_primary reading (absolutist exclusion) and the
 *   freedom_primary reading (movement as fundamental right).
 *
 * KEY AGENTS:
 *   - Receiving states (institutional/constrained): Agenda-setters who exercise border control and bear adjudication costs.
 *   - Excluded migrants (powerless/trapped): Primary targets who suffer exclusion despite proportionality rhetoric.
 *   - Displaced citizens (powerless/trapped): Secondary targets channelled by border regimes.
 *   - International courts and tribunals (institutional/constrained): Agenda-setters who interpret proportionality and constitute their authority through the constraint.
 *   - Human rights advocacy networks (organized/mobile): Beneficiaries who gain litigation framework and institutional legitimacy.
 *   - Origin states (institutional/constrained): Excluded actors marginal in the adjudication framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.65).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.75).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified Sovereignty Border Control Norm").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, '86bb71dd-b4dc-42ba-8313-697d2e287a04').
narrative_ontology:cs_kernel_codification('86bb71dd-b4dc-42ba-8313-697d2e287a04', formalized).
narrative_ontology:cs_authority_grounding('86bb71dd-b4dc-42ba-8313-697d2e287a04', lineage).
narrative_ontology:cs_interpretation_layer_present('86bb71dd-b4dc-42ba-8313-697d2e287a04').
narrative_ontology:cs_reading_relation('86bb71dd-b4dc-42ba-8313-697d2e287a04', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('86bb71dd-b4dc-42ba-8313-697d2e287a04', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('86bb71dd-b4dc-42ba-8313-697d2e287a04', foundational, state_authority_conditioned_on_proportionality).
narrative_ontology:cs_axiom_status(state_authority_conditioned_on_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('86bb71dd-b4dc-42ba-8313-697d2e287a04', state_authority_conditioned_on_proportionality, conventional).
narrative_ontology:cs_axiom('86bb71dd-b4dc-42ba-8313-697d2e287a04', foundational, non_refoulement_as_peremptory_limit).
narrative_ontology:cs_axiom_status(non_refoulement_as_peremptory_limit, holdable).
narrative_ontology:cs_axiom_grounding('86bb71dd-b4dc-42ba-8313-697d2e287a04', non_refoulement_as_peremptory_limit, conventional).
narrative_ontology:cs_reference_frame('86bb71dd-b4dc-42ba-8313-697d2e287a04', state_sovereignty_qualified_by_universal_rights).
narrative_ontology:cs_drift_state('86bb71dd-b4dc-42ba-8313-697d2e287a04', contemporary_securitization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('86bb71dd-b4dc-42ba-8313-697d2e287a04', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, receiving_states).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, human_rights_advocacy_networks).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise border control authority under a framework that requires justification as proportionate and rights-compliant. Bear adjudication costs before international and domestic courts. Retain sovereignty but subject to legal oversight and potential sanction for non-compliance.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, receiving_states, agenda_setter,
    institutional, generational, constrained, national).

% Subject to border control decisions that may be lawful under proportionality tests but still result in exclusion, family separation, detention, or return to unsafe conditions. Lack voice in the legal framework that governs their movement.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, regional).

% Include refugees and internally displaced persons whose movement is blocked or channelled by border regimes that claim to act proportionately but may still deny access to protection or durable solutions.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens, payer,
    powerless, immediate, trapped, regional).

% Gain a litigation and advocacy framework from the proportionality and human rights compliance requirements. Secure funding, legal victories, and institutional legitimacy by holding states accountable to the norm.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_advocacy_networks, beneficiary,
    organized, biographical, mobile, global).

% Adjudicate whether state border control meets proportionality and human rights standards. Interpret treaty texts and develop jurisprudence that defines the boundaries of legitimate exclusion. Their authority is constituted by the constraint.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, international_courts_and_tribunals, agenda_setter,
    institutional, generational, constrained, global).

% Their citizens are subject to exclusion decisions made by receiving states and international courts in which origin states have limited standing. Would object to the treatment of their nationals but are structurally marginal in the adjudication framework.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, origin_states, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, diffuse).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared legal framework for states to exercise border control while maintaining international legitimacy and avoiding arbitrary exclusion; solves the coordination problem of how to manage cross-border movement without universal open borders or unilateral absolute sovereignty.
% TRANSFER_FUNCTION: Moves authority to exclude from unconstrained state discretion to a proportionality-tested, rights-conditioned framework; moves costs of exclusion onto excluded migrants and displaced citizens, and moves adjudication burden onto states.
% ABSENT_VOICES: Migrants themselves are largely absent from the drafting and interpretation of proportionality tests; origin states are marginal in adjudication; open-border advocates and absolute sovereignty proponents are present in discourse but structurally underweight in treaty drafting rooms.
% DISAPPEARANCE_RATIONALE: If the norm vanished, states would revert to either unilateral exclusion or open borders, international court dockets would collapse, human rights NGOs would lose a primary litigation framework, and millions of migrants would face qualitatively different legal treatment.
% FOUNDING_PROBLEM: Post-WWII need to prevent arbitrary state exclusion and refugee crises while preserving state sovereignty; balancing collective self-determination with individual human rights protection.
% FOUNDING_PROBLEM_CORROBORATION: International legal historians and refugee scholars corroborate the post-war origin; critical migration scholars outside the beneficiary set argue the founding problem has mutated into a legitimation structure for racialized exclusion, and no neutral corroborator agrees on the current status.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderate-high because the framework legitimizes significant harm to migrants through legally sanctioned exclusion, externalization, and detention. Suppression (0.75) is high because alternatives (open borders, absolute sovereignty) are actively suppressed in legal discourse and through border enforcement. Theater ratio (0.50) reflects that proportionality review provides genuine legal function but is increasingly performative, masking pushbacks and externalization as rights-compliant. Accessibility collapse (0.70) is high because legal imagination tends to collapse toward managed exclusion once the framework is accepted. Resistance (0.75) is high: states resist adjudication burdens, migrants resist barriers, and populist movements challenge the framework itself. The temporal series show monotonic increase in extraction, theater, and suppression from 1951 to 2021, reflecting securitization and externalization trends.
 *
 * PERSPECTIVAL GAP:
 *   Receiving states experience the constraint as a coordination mechanism that legitimizes their control but imposes litigation costs; excluded migrants experience it as a rights-coded barrier to safety; courts experience it as an interpretive mandate that constitutes their authority. The engine computes these divergent seat types from the structural asymmetry in power, exit options, and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving states are declared beneficiaries with constrained exit, placing their directionality near the beneficiary pole; excluded migrants and displaced citizens are declared victims with trapped exit, placing their directionality near the full-target pole. Human rights networks are beneficiaries with mobile exit, placing them low on the extraction axis. International courts sit between beneficiary and symmetric: they gain authority from the constraint but are bound to interpret it. Origin states are excluded with constrained exit, placing them toward the target side though not directly governed by the constraint's extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to prevent arbitrary exclusion post-WWII. Its founding problem status is contested: some argue the problem is live, others that the framework has become a tool for managed exclusion rather than protection. The drift toward securitization, externalization agreements, and pushbacks suggests mandatrophy riskâif proportionality review becomes a rubber stamp for exclusion, the coordination function atrophies and the constraint slides toward snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_as_legitimation,
    'Does the proportionality requirement function as a genuine legal limit on sovereign exclusion, or as a mechanism that legitimizes extraction by rendering exclusion rights-compliant?',
    'Comparative analysis of exclusion rates and rights outcomes before and after the introduction of proportionality review in selected jurisdictions.',
    'If legitimation, classification shifts toward snare; if a genuine limit, classification shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_as_legitimation, empirical, 'Whether proportionality review limits or legitimizes extraction.').

omega_variable(
    state_burden_nature,
    'Is the adjudication burden on states a form of asymmetric extraction, or a symmetric coordination cost of maintaining a rules-based border order?',
    'Compare cost and benefit distributions across states with varying exposure to international litigation and border management costs.',
    'If extraction, states join the victim set and the rope element weakens; if coordination cost, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_burden_nature, conceptual, 'Whether state adjudication burden is extraction or coordination cost.').

omega_variable(
    kernel_reading_dominance,
    'Which reading of the border normative status kernel is structurally dominant, and does this constraint represent the actual operational framework or an aspirational one?',
    'Corpus analysis of state practice, treaty text, and judicial decisions to measure alignment with qualified sovereignty versus sovereignty_primary or freedom_primary.',
    'If aspirational, metrics may overstate enforcement and the true constraint may be closer to sovereignty_primary; if operational, current classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_dominance, conceptual, 'Whether qualified sovereignty is operational or aspirational relative to sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__qualified_sovereignty, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bord_tr_t10, border_normative_status__qualified_sovereignty, theater_ratio, 10, 0.22).
narrative_ontology:measurement(bord_tr_t20, border_normative_status__qualified_sovereignty, theater_ratio, 20, 0.25).
narrative_ontology:measurement(bord_tr_t30, border_normative_status__qualified_sovereignty, theater_ratio, 30, 0.3).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__qualified_sovereignty, theater_ratio, 40, 0.38).
narrative_ontology:measurement(bord_tr_t50, border_normative_status__qualified_sovereignty, theater_ratio, 50, 0.45).
narrative_ontology:measurement(bord_tr_t60, border_normative_status__qualified_sovereignty, theater_ratio, 60, 0.48).
narrative_ontology:measurement(bord_tr_t70, border_normative_status__qualified_sovereignty, theater_ratio, 70, 0.5).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__qualified_sovereignty, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bord_be_t10, border_normative_status__qualified_sovereignty, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(bord_be_t20, border_normative_status__qualified_sovereignty, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(bord_be_t30, border_normative_status__qualified_sovereignty, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(bord_be_t40, border_normative_status__qualified_sovereignty, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(bord_be_t50, border_normative_status__qualified_sovereignty, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(bord_be_t60, border_normative_status__qualified_sovereignty, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(bord_be_t70, border_normative_status__qualified_sovereignty, base_extractiveness, 70, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__qualified_sovereignty, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bord_su_t10, border_normative_status__qualified_sovereignty, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(bord_su_t20, border_normative_status__qualified_sovereignty, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(bord_su_t30, border_normative_status__qualified_sovereignty, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(bord_su_t40, border_normative_status__qualified_sovereignty, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(bord_su_t50, border_normative_status__qualified_sovereignty, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(bord_su_t60, border_normative_status__qualified_sovereignty, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(bord_su_t70, border_normative_status__qualified_sovereignty, suppression_requirement, 70, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_normative_status kernel. The kernel decomposes into at least three structurally distinct constraints: sovereignty_primary, qualified_sovereignty, and freedom_primary. Each reading has a different beneficiary/victim structure and epsilon profile. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
