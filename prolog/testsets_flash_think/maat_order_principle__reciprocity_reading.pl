% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Order Principle (Reciprocity Reading)
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint represents the 'reciprocity reading' of the ancient
 *   Egyptian concept of Ma'at, where cosmic balance and social order are
 *   maintained through mutual obligations. Pharaoh is bound to provide
 *   justice, stability, and proper resource distribution, and in return,
 *   society provides legitimacy and support. This reading emphasizes the
 *   conditional nature of Pharaoh's rule, where failure to uphold Ma'at can
 *   justify resistance or withdrawal of support, and implies a moderate
 *   extraction ceiling grounded in this reciprocal norm.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.45).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.65).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Order Principle (Reciprocity Reading)").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, '4b77ba47-5635-4f4e-9338-b34117864ee2').
narrative_ontology:cs_kernel_codification('4b77ba47-5635-4f4e-9338-b34117864ee2', formalized).
narrative_ontology:cs_authority_grounding('4b77ba47-5635-4f4e-9338-b34117864ee2', lineage).
narrative_ontology:cs_interpretation_layer_present('4b77ba47-5635-4f4e-9338-b34117864ee2').
narrative_ontology:cs_reading_relation('4b77ba47-5635-4f4e-9338-b34117864ee2', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('4b77ba47-5635-4f4e-9338-b34117864ee2', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('4b77ba47-5635-4f4e-9338-b34117864ee2', foundational, pharaoh_accountable_to_maat).
narrative_ontology:cs_axiom_status(pharaoh_accountable_to_maat, holdable).
narrative_ontology:cs_axiom_grounding('4b77ba47-5635-4f4e-9338-b34117864ee2', pharaoh_accountable_to_maat, deontological).
narrative_ontology:cs_axiom('4b77ba47-5635-4f4e-9338-b34117864ee2', foundational, legitimacy_from_reciprocal_provision).
narrative_ontology:cs_axiom_status(legitimacy_from_reciprocal_provision, holdable).
narrative_ontology:cs_axiom_grounding('4b77ba47-5635-4f4e-9338-b34117864ee2', legitimacy_from_reciprocal_provision, conventional).
narrative_ontology:cs_reference_frame('4b77ba47-5635-4f4e-9338-b34117864ee2', ideal_pharaonic_reciprocity).
narrative_ontology:cs_drift_state('4b77ba47-5635-4f4e-9338-b34117864ee2', late_dynastic_period_decline, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4b77ba47-5635-4f4e-9338-b34117864ee2', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, egyptian_society).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, commoners).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, pharaoh).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, egyptian_society).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, cosmic_order_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, divine_kingship_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divinely appointed ruler, responsible for upholding Ma'at through justice, stability, and resource distribution. Benefits from the legitimacy and stability Ma'at provides, but is also bound by its reciprocal obligations. Failure to uphold Ma'at can lead to loss of legitimacy or divine disfavor, making them a victim of the constraint if they fail.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh, agenda_setter,
    institutional, generational, constrained, national).

% The populace, including commoners, farmers, and artisans, who benefit from the order and justice Ma'at is supposed to provide. They contribute labor, taxes, and loyalty to Pharaoh, expecting the reciprocal provision of stability and prosperity. Their options are limited by the social and religious structure; they become victims if Pharaoh's rule becomes unjust or overly extractive.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, egyptian_society, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, egyptian_society, payer).

% Interpreters and guardians of Ma'at, responsible for rituals and advising Pharaoh. They reinforce the ideology of Ma'at and benefit from their central role in maintaining cosmic order and Pharaoh's legitimacy. They have a vested interest in the system's stability and act as enforcers of the constraint's interpretation.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, priestly_class, agenda_setter,
    institutional, generational, constrained, national).

% The abstract principle of truth, justice, and cosmic balance that Ma'at represents. It is the ultimate referent for the constraint's legitimacy and the standard against which Pharaoh's rule is measured, though it is not an active agent.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, cosmic_order, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(maat_order_principle__reciprocity_reading, cosmic_order).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for social and cosmic order, ensuring stability, justice, and reciprocal obligations between the divine ruler and society, preventing chaos and famine.
% TRANSFER_FUNCTION: Transfers legitimacy, divine favor, and societal resources (labor, taxes) to Pharaoh in exchange for justice, stability, and proper resource distribution to Egyptian society.
% ABSENT_VOICES: Any who might question the divine origin of Ma'at or the Pharaoh's specific interpretation of its demands. Their dissent would be seen as a threat to cosmic order and suppressed by religious and state authority.
% DISAPPEARANCE_RATIONALE: If the principle of Ma'at and its reciprocal obligations vanished overnight, the entire socio-political and religious structure of ancient Egypt would collapse. Pharaoh's legitimacy would vanish, social order would dissolve into chaos, and the perceived cosmic balance would be irrevocably broken, leading to widespread instability and existential crisis.
% FOUNDING_PROBLEM: The inherent human tendency towards chaos, injustice, and self-interest, threatening the stability and prosperity of society and the cosmic order itself.
% FOUNDING_PROBLEM_CORROBORATION: The priestly class and state scribes consistently attest to the ongoing necessity of Ma'at to prevent societal collapse. Historical records of periods of disorder (e.g., Intermediate Periods) are cited as evidence of what happens when Ma'at is not upheld, corroborating the problem's live status from outside the immediate beneficiaries of Pharaoh's rule.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).
:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates cosmic and social order (benefiting all) but also involves asymmetric extraction (Pharaoh's power and resources) and requires active enforcement by the priestly class and state. Extractiveness is moderate (0.45) due to the reciprocal nature of the obligations, which theoretically limits excessive demands. Suppression is moderate-high (0.65) as deviation from Ma'at is severely discouraged and punished to maintain order. Theater ratio is low (0.25) because the rituals and enforcement are considered genuinely functional for cosmic balance, not merely performative. Accessibility collapse is high (0.7) as the alternative to Ma'at is perceived as chaos. Resistance is low (0.3) due to the pervasive belief system and severe consequences for dissent.
 *
 * PERSPECTIVAL GAP:
 *   From the Pharaoh's perspective, Ma'at is the divine framework that legitimizes their rule and guides their actions, ensuring stability. From the perspective of Egyptian society, Ma'at is the promise of justice and order in exchange for their loyalty and labor. The priestly class views it as the sacred duty they uphold. The engine's per-seat classification will highlight how the same constraint is experienced as a source of legitimacy and benefit by the Pharaoh, but also as a binding obligation, and as a source of both order and potential extraction by society.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh is a beneficiary due to the legitimacy and stability provided, but also a victim if they fail their obligations. Egyptian society is a beneficiary of the order but a payer of resources and labor, becoming a victim if extraction becomes unjust. The priestly class benefits from its central role in interpreting and enforcing Ma'at. The 'reciprocity' aspect means directionality is not purely one-sided.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharaoh_accountability_mechanism,
    'What specific mechanisms exist to enforce Pharaoh''s reciprocal obligations under Ma''at, and how effective are they in practice?',
    'Analysis of historical records for instances of successful resistance or withdrawal of support against a failing Pharaoh, or evidence of priestly intervention.',
    'If strong mechanisms exist, the constraint''s effective extractiveness on society is lower, and Pharaoh''s directionality shifts more towards ''target''. If mechanisms are weak or absent, the constraint functions more as a Snare for society, with Pharaoh as a pure beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaoh_accountability_mechanism, empirical, 'Examines the practical enforceability of Pharaoh''s obligations.').

omega_variable(
    reciprocity_rhetoric_vs_reality,
    'Is the ''reciprocity'' inherent in this reading of Ma''at a genuine structural feature, or primarily a rhetorical device to legitimize Pharaoh''s rule and extraction?',
    'Comparative analysis with other ancient Near Eastern legal/religious systems, focusing on the actual distribution of power and resources during periods of stability and crisis.',
    'If primarily rhetorical, the base extractiveness is higher, and the constraint leans more towards a Snare. If genuinely structural, the Tangled Rope classification is reinforced, and the coordination function is more prominent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_rhetoric_vs_reality, conceptual, 'Distinguishes between the claimed and actual function of reciprocity.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the ''reciprocity_reading'' represent the most accurate framing of the Ma''at kernel, or would a sibling reading (e.g., ''divine_mandate_reading'') better capture the structural reality of ancient Egyptian governance?',
    'Further archaeological and textual discoveries, or a shift in scholarly consensus regarding the primary source of Pharaoh''s authority and accountability.',
    'If the ''divine_mandate_reading'' were adopted, the constraint would likely reclassify as a Mountain (for Pharaoh) or Snare (for society), with significantly higher extractiveness and suppression, as Pharaoh would be seen as above challenge. This reading''s emphasis on Pharaoh''s accountability would be lost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Documents the alternative framings of the Ma''at kernel and their classification implications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__reciprocity_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__reciprocity_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__reciprocity_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__reciprocity_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__reciprocity_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__reciprocity_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__reciprocity_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__reciprocity_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__reciprocity_reading, base_extractiveness, 80, 0.47).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__reciprocity_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__reciprocity_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__reciprocity_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(maat_su_t60, maat_order_principle__reciprocity_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__reciprocity_reading, suppression_requirement, 80, 0.67).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__reciprocity_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, identity_coordination).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'Ma'at order principle' kernel, each representing a different structural interpretation of the same core concept. They are linked to capture the contested nature of Ma'at's application in ancient Egypt.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
