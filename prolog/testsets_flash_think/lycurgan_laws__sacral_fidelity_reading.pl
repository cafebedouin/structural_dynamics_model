% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Laws as Sacred, Unchangeable Divine Ordinance (Sacral Fidelity Reading)
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sacral_fidelity_reading' of the
 *   Lycurgan laws kernel. From this perspective, the laws are a divinely
 *   ordained, unchangeable ordinance requiring absolute adherence, forming
 *   the bedrock of Spartan society. Any challenges or decline are attributed
 *   to external pressures or citizen vice, not to the inherent design or
 *   rigidity of the legal system itself. The immutability of the laws is seen
 *   as a virtue, ensuring stability and order. The classification as a
 *   Mountain reflects this reading's view of the laws as a natural,
 *   unalterable feature of their political reality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.15).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.6).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws as Sacred, Unchangeable Divine Ordinance (Sacral Fidelity Reading)").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__sacral_fidelity_reading).
domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, 'bfc874d4-b99a-496d-89f7-30945f7362c3').
narrative_ontology:cs_kernel_codification('bfc874d4-b99a-496d-89f7-30945f7362c3', formalized).
narrative_ontology:cs_authority_grounding('bfc874d4-b99a-496d-89f7-30945f7362c3', lineage).
narrative_ontology:cs_reading_relation('bfc874d4-b99a-496d-89f7-30945f7362c3', lycurgan_laws__demographic_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('bfc874d4-b99a-496d-89f7-30945f7362c3', lycurgan_laws__adaptive_fiction_reading, forecloses).
narrative_ontology:cs_axiom('bfc874d4-b99a-496d-89f7-30945f7362c3', foundational, lycurgan_laws_divinely_ordained).
narrative_ontology:cs_axiom_status(lycurgan_laws_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('bfc874d4-b99a-496d-89f7-30945f7362c3', lycurgan_laws_divinely_ordained, theological).
narrative_ontology:cs_axiom('bfc874d4-b99a-496d-89f7-30945f7362c3', foundational, immutability_is_virtue).
narrative_ontology:cs_axiom_status(immutability_is_virtue, holdable).
narrative_ontology:cs_axiom_grounding('bfc874d4-b99a-496d-89f7-30945f7362c3', immutability_is_virtue, deontological).
narrative_ontology:cs_reference_frame('bfc874d4-b99a-496d-89f7-30945f7362c3', divine_mandate_unquestionable_order).
narrative_ontology:cs_drift_state('bfc874d4-b99a-496d-89f7-30945f7362c3', classical_sparta_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bfc874d4-b99a-496d-89f7-30945f7362c3', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, spartan_citizens).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, divine_right_of_law).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, social_stability_through_immutability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adhere absolutely to the Lycurgan laws, believing them to be divinely ordained and the source of Spartan order and virtue. They benefit from the stability and martial prowess of the state but pay with their individual freedoms and absolute obedience.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_citizens, beneficiary,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, spartan_citizens, payer).

% Are the custodians and enforcers of the Lycurgan laws, which they present as sacred and unchangeable. Their authority is derived from upholding this divine mandate. They benefit from the stability and control it provides, but are also bound by its strictures.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_ephors_kings, agenda_setter,
    institutional, generational, constrained, local).

% Are the external forces that threaten Sparta. From the sacral fidelity reading, any decline or challenge to Sparta is attributed to these external pressures or internal moral failings, never to the inherent design of the Lycurgan system itself.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, external_enemies, excluded,
    powerful, biographical, mobile, regional).

% Study the Lycurgan laws and their impact from a detached, scholarly perspective. This seat analyzes the claims of divine origin and immutability against historical evidence and comparative political theory.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rigid, hierarchical social and military order in Sparta, ensuring absolute discipline, collective identity, and martial superiority through a divinely sanctioned legal framework.
% TRANSFER_FUNCTION: Transfers absolute obedience, individual autonomy, and resources from Spartan citizens to the state, in exchange for social stability, military strength, and the perceived favor of the gods.
% ABSENT_VOICES: Any voices advocating for legal reform, adaptation to changing circumstances, or questioning the divine origin of the laws are structurally absent or suppressed, as such dissent would be considered sacrilege or treason.
% DISAPPEARANCE_RATIONALE: If the belief in the Lycurgan laws as sacred, unchangeable divine ordinance vanished overnight, the entire Spartan social, political, and military structure would collapse. The rigid hierarchy, collective identity, and absolute adherence that defined Sparta were entirely predicated on this foundational belief.
% FOUNDING_PROBLEM: To create an unshakeable, virtuous, and militarily superior society that would endure through divine mandate and absolute adherence to its foundational laws.
% FOUNDING_PROBLEM_CORROBORATION: Ancient Spartan accounts, as transmitted by historians like Plutarch and Xenophon, corroborate the belief in the divine origin and enduring necessity of the Lycurgan laws for Spartan virtue and stability. This corroboration comes from within the historical and philosophical tradition that revered Lycurgus, rather than from external, critical analysis.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects the reading's view that the laws are for the collective good, not for individual gain, and any 'cost' is a necessary part of a divinely ordered society. Suppression (0.60) is moderate, as absolute adherence is enforced, but it's justified by the sacred nature of the laws. Theater ratio (0.10) is low because the belief in the laws' divine origin and unchangeability is genuine within this reading. Accessibility collapse (0.90) is high, as alternatives are considered sacrilege. Resistance (0.10) is low due to the absolute adherence demanded. The temporal measurements show stability, reflecting the reading's emphasis on the unchanging nature of the laws.
 *
 * PERSPECTIVAL GAP:
 *   From this reading, the Lycurgan laws are a beneficial, immutable Mountain. Other readings (e.g., 'demographic_trap_reading' or 'adaptive_fiction_reading') would classify the same laws as a Snare or Tangled Rope, highlighting their extractive or dysfunctional aspects. The engine's computation of per-seat classification will reveal this divergence, particularly for the 'spartan_citizens' who are both beneficiaries and payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Spartan citizens are beneficiaries of the divine order and stability, but also payers through their absolute adherence and sacrifice of individual freedoms. The Ephors and Kings are agenda-setters, enforcing the laws and deriving their authority from them. External enemies are excluded from the system but are seen as the source of any societal challenges, deflecting blame from the laws themselves.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_constructed_origin,
    'Are the Lycurgan laws genuinely divine ordinance, or a human construct presented as such to enforce social order and maintain elite power?',
    'Archaeological or textual evidence of pre-Lycurgan legal traditions, or comparative analysis with other ancient legal systems claiming divine origin, could shed light on the historical contingency versus divine mandate.',
    'If a human construct, the constraint shifts from a genuine mountain to a false summit (e.g., Tangled Rope or Snare), with the Spartan elite as beneficiaries of the enforced order, and the ''emerges_naturally'' flag would be false.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_vs_constructed_origin, empirical, 'Ambiguity of divine vs. human origin of Lycurgan laws.').

omega_variable(
    immutability_as_virtue_or_trap,
    'Is the unchangeability of the Lycurgan laws a virtue that ensures stability, or a structural trap that prevents necessary adaptation and leads to decline?',
    'Comparative historical analysis of other rigid vs. adaptive constitutional systems, or counterfactual historical modeling of Spartan development under different legal frameworks.',
    'If immutability is a trap, the ''sacral_fidelity_reading'' would be reclassified, likely to a Snare or Tangled Rope, as its core premise of beneficial unchangeability is undermined. The ''resistance'' metric would also be re-evaluated for latent, unexpressed resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_as_virtue_or_trap, conceptual, 'Whether immutability is a beneficial feature or a detrimental flaw.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lycu_tr_t25, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(lycu_tr_t50, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(lycu_tr_t75, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(lycu_be_t25, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(lycu_be_t50, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(lycu_be_t75, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 75, 0.15).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lycu_su_t25, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(lycu_su_t50, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(lycu_su_t75, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 75, 0.6).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__sacral_fidelity_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'lycurgan_laws' kernel, each representing a distinct structural interpretation of the same historical legal system. This 'sacral_fidelity_reading' emphasizes divine origin and immutability, contrasting with readings that focus on demographic collapse or covert adaptation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
