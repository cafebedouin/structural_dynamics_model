% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta (1215) as Universal Due Process Precedent
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint models the 'universal rights' reading of Magna Carta
 *   (1215), where Clause 39 ('No free man shall be seized or imprisoned...
 *   except by the lawful judgment of his equals or by the law of the land')
 *   is interpreted as a transhistorical precedent for universal due process,
 *   applying to all persons and constraining all state power. This reading
 *   expands the original scope of 'free men' beyond feudal barons to
 *   encompass all individuals, establishing a foundational principle against
 *   arbitrary state action. The constraint is claimed as a Rope due to its
 *   genuine coordination function in establishing legal order and its broad,
 *   non-extractive benefits under this interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.15).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.2).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta (1215) as Universal Due Process Precedent").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, 'ba292514-f985-4cca-a171-a767bacb272c').
narrative_ontology:cs_kernel_codification('ba292514-f985-4cca-a171-a767bacb272c', fixed_text).
narrative_ontology:cs_authority_grounding('ba292514-f985-4cca-a171-a767bacb272c', lineage).
narrative_ontology:cs_interpretation_layer_present('ba292514-f985-4cca-a171-a767bacb272c').
narrative_ontology:cs_reading_relation('ba292514-f985-4cca-a171-a767bacb272c', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('ba292514-f985-4cca-a171-a767bacb272c', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('ba292514-f985-4cca-a171-a767bacb272c', foundational, due_process_is_universal).
narrative_ontology:cs_axiom_status(due_process_is_universal, holdable).
narrative_ontology:cs_axiom_grounding('ba292514-f985-4cca-a171-a767bacb272c', due_process_is_universal, deontological).
narrative_ontology:cs_axiom('ba292514-f985-4cca-a171-a767bacb272c', foundational, state_power_is_limited_by_law).
narrative_ontology:cs_axiom_status(state_power_is_limited_by_law, holdable).
narrative_ontology:cs_axiom_grounding('ba292514-f985-4cca-a171-a767bacb272c', state_power_is_limited_by_law, deontological).
narrative_ontology:cs_reference_frame('ba292514-f985-4cca-a171-a767bacb272c', enlightenment_universal_rights).
narrative_ontology:cs_drift_state('ba292514-f985-4cca-a171-a767bacb272c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ba292514-f985-4cca-a171-a767bacb272c', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, all_persons_under_law).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, judicial_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, state_actors).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, due_process_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected by the principle of due process against arbitrary state action. Their benefit is the security and predictability of legal proceedings. While they cannot 'exit' the legal system, the constraint provides a fundamental safeguard.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, all_persons_under_law, beneficiary,
    organized, generational, constrained, universal).

% Interprets and applies the principles of due process derived from Magna Carta. It benefits from a clear, foundational legal precedent that guides its function and legitimizes its authority. It actively enforces the constraint through rulings and precedents.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, judicial_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Are constrained by the due process requirement, preventing arbitrary arrests, detentions, and punishments. While this limits their power, it is understood as a legitimate boundary rather than an extraction. Their 'cost' is the inability to act outside established legal procedures.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, state_actors, payer,
    institutional, biographical, constrained, national).

% Analyze and debate the historical context, interpretation, and contemporary relevance of Magna Carta, including the 'universal rights' reading. They contribute to the ongoing understanding and application of the constraint.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% While historically central to Magna Carta's origins, under the 'universal rights' reading, their specific feudal privileges are no longer the primary focus. They are 'excluded' from the contemporary interpretation's central narrative, which prioritizes universal application over historical class-specific rights.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, baronial_descendants, excluded,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__universal_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_1215__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a foundational principle of due process, coordinating legal systems globally around the idea that state power must be exercised according to law, not arbitrary will. This provides a stable framework for justice and governance.
% TRANSFER_FUNCTION: Transfers the right to fair legal process from the potential arbitrary power of the state to all individuals, ensuring that no person can be deprived of liberty or property without 'the law of the land'.
% ABSENT_VOICES: Those who advocate for unchecked executive power or arbitrary state action would object, as this constraint fundamentally limits such actions. They are largely absent from mainstream legal discourse that accepts due process as a given.
% DISAPPEARANCE_RATIONALE: If the principle of universal due process derived from Magna Carta vanished, legal systems worldwide would lose a foundational pillar. Arbitrary state actions would become legitimate, leading to widespread instability, injustice, and a fundamental reordering of the relationship between citizens and the state.
% FOUNDING_PROBLEM: The problem of arbitrary rule by monarchs and powerful lords, leading to unjust seizures, imprisonments, and punishments without legal recourse.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and international bodies consistently attest that the problem of arbitrary state power remains live, requiring constant vigilance and the application of due process principles. This corroboration comes from outside the direct beneficiaries of state power.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under the universal rights reading, extractiveness is low (0.15) because the constraint primarily establishes a framework for fair legal process, benefiting all citizens rather than extracting from specific groups. Suppression is also low (0.20) as its persistence relies on its acceptance as a foundational legal principle, not active coercion against dissenters. Theater ratio is low (0.10) because its principles are genuinely invoked in legal practice, not merely performed. Accessibility collapse is high (0.75) because the principle of due process, once established, significantly limits alternative arbitrary actions by the state. Resistance is low (0.10) as the principle is widely accepted as a cornerstone of modern legal systems.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'all_persons_under_law' and the 'judicial_system', this constraint is a clear Rope, providing essential coordination for a just legal order. From the perspective of state actors seeking to exercise arbitrary power, it acts as a Mountain, an unyielding limit on their actions. The engine's classification should reflect this broad, non-extractive benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, 'all_persons_under_law' are the primary beneficiaries, as the constraint protects them from arbitrary state action. The 'judicial_system' also benefits by having a clear, foundational principle to apply. There are no identifiable 'victims' as the constraint is understood to limit illegitimate state power, not to extract from any specific group. State actors are constrained but not victimized, as the constraint defines the legitimate bounds of their authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_scope_ambiguity,
    'Is Magna Carta''s original intent limited to baronial privilege, or does it contain transhistorical principles applicable to all persons?',
    'Historical-legal scholarship on the evolution of ''free men'' and the reception of Magna Carta in subsequent legal systems.',
    'If limited to baronial privilege, this constraint would be reclassified as a historical Piton or a narrow Rope; if transhistorical, its universal application is affirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_scope_ambiguity, conceptual, 'Contested scope of Magna Carta''s original application.').

omega_variable(
    reading_impact_on_state_power,
    'How does the ''universal rights'' reading of Magna Carta structurally constrain contemporary state power, compared to other readings?',
    'Comparative legal analysis of judicial decisions citing Magna Carta under different interpretive frameworks.',
    'If this reading effectively limits state power more broadly, its classification as a Rope is strengthened; if its impact is largely symbolic, it might drift towards Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_impact_on_state_power, empirical, 'Structural impact of the universal rights reading on state authority.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''universal_rights_reading'' of the ''magna_carta_1215'' kernel. What structural elements would change under the ''baronial_privilege_reading'' or ''living_document_reading''?',
    'Analysis of the alternative readings'' declared beneficiary/victim sets and the scope of their claimed protections.',
    'The ''baronial_privilege_reading'' would narrow beneficiaries to a specific historical class and reduce the constraint''s scope. The ''living_document_reading'' would emphasize interpretive evolution over fixed original meaning, potentially altering the mechanism of enforcement and adaptation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Documents this constraint as one reading of the Magna Carta kernel and outlines the structural deltas of sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 1215, 2015).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_1215__universal_rights_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(magn_tr_t100, magna_carta_1215__universal_rights_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement(magn_tr_t400, magna_carta_1215__universal_rights_reading, theater_ratio, 400, 0.1).
narrative_ontology:measurement(magn_tr_t800, magna_carta_1215__universal_rights_reading, theater_ratio, 800, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_1215__universal_rights_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(magn_be_t100, magna_carta_1215__universal_rights_reading, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(magn_be_t400, magna_carta_1215__universal_rights_reading, base_extractiveness, 400, 0.15).
narrative_ontology:measurement(magn_be_t800, magna_carta_1215__universal_rights_reading, base_extractiveness, 800, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_1215__universal_rights_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(magn_su_t100, magna_carta_1215__universal_rights_reading, suppression_requirement, 100, 0.18).
narrative_ontology:measurement(magn_su_t400, magna_carta_1215__universal_rights_reading, suppression_requirement, 400, 0.2).
narrative_ontology:measurement(magn_su_t800, magna_carta_1215__universal_rights_reading, suppression_requirement, 800, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, us_bill_of_rights_due_process).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, universal_declaration_human_rights).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Magna Carta kernel. Other readings (baronial_privilege_reading, living_document_reading) are distinct constraints with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
