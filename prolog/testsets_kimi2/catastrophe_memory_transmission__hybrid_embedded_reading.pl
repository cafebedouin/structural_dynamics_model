% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_embedded_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Hybrid Embedded Ritual Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_embedded_reading of the
 *   catastrophe_memory_transmission kernel. The core claim is that survival
 *   competence and symbolic ritual form are ontologically inseparable:
 *   altering form degrades function, and function only exists through enacted
 *   form. The constraint governs how ritual communities transmit operational
 *   capacity across generations. There is no clear extractive agenda setter;
 *   the arrangement functions as coordination through shared practice, though
 *   its high accessibility collapse gives it a mountain-like substrate. The
 *   reading is contested by an operational_competence_reading (instrumental
 *   separability) and a symbol_continuity_reading (intrinsic symbolic value).
 *   Claimed type is rope; metrics are authored independently.
 *
 * KEY AGENTS:
 *   - ritual_communities: Primary beneficiary (organized/generational) â retain survival competence through enacted ritual fidelity.
 *   - secular_educators: Excluded voice (institutional/global) â argue for propositional extraction and codification.
 *   - cognitive_ethnographers: Analytical observer (analytical/civilizational) â document the form-function link without stake in its continuation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.12).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.08).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Hybrid Embedded Ritual Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, '983a7da4-f71c-4d48-b0e8-7a704c6468ca').
narrative_ontology:cs_kernel_codification('983a7da4-f71c-4d48-b0e8-7a704c6468ca', distributed).
narrative_ontology:cs_authority_grounding('983a7da4-f71c-4d48-b0e8-7a704c6468ca', lineage).
narrative_ontology:cs_interpretation_layer_present('983a7da4-f71c-4d48-b0e8-7a704c6468ca').
narrative_ontology:cs_reading_relation('983a7da4-f71c-4d48-b0e8-7a704c6468ca', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('983a7da4-f71c-4d48-b0e8-7a704c6468ca', catastrophe_memory_transmission__operational_competence_reading, influences).
narrative_ontology:cs_axiom('983a7da4-f71c-4d48-b0e8-7a704c6468ca', foundational, form_function_co_constitution).
narrative_ontology:cs_axiom_status(form_function_co_constitution, holdable).
narrative_ontology:cs_axiom_grounding('983a7da4-f71c-4d48-b0e8-7a704c6468ca', form_function_co_constitution, empirically_contingent).
narrative_ontology:cs_reference_frame('983a7da4-f71c-4d48-b0e8-7a704c6468ca', co_constitutive_form_function).
narrative_ontology:cs_drift_state('983a7da4-f71c-4d48-b0e8-7a704c6468ca', contemporary_secularization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('983a7da4-f71c-4d48-b0e8-7a704c6468ca', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_communities).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, non_propositional_memory_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, embodied_cognition_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain ritual practice as the living substrate of survival competence. Their operational capacity depends on performing traditional forms correctly; they cannot extract the knowledge into manuals or explicit instruction without loss. Exit from the practice means exit from the competence and from the social identity built around it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_communities, beneficiary,
    organized, generational, identity_locked, regional).

% Advocate for explicit, propositional instruction and formal curricula as alternatives to ritual transmission. They are structurally outside traditional ritual systems and would argue that competence can be codified, but they are not consulted when ritual communities assess transmission failure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, secular_educators, excluded,
    institutional, generational, analytical, global).

% Study the relationship between embodied practice and retained competence across cultures. They observe whether ritual alteration leads to skill degradation, providing external corroboration without participating in the ritual economy or having a stake in its continuation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, cognitive_ethnographers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserve and transmit survival-relevant operational competence across generations when explicit verbal or written codification is insufficient, by embedding the competence in symbolic ritual that must be physically enacted to be retained.
% TRANSFER_FUNCTION: Moves embodied, non-propositional knowledge from experienced practitioners to novices through repeated ritual performance; the cost is the sustained labor of correct enactment, and the benefit is the community's retained capacity to respond to threats.
% ABSENT_VOICES: Secular educators and propositional-knowledge institutions argue that the same competence can be extracted, formalized, and taught explicitly; they are structurally excluded from ritual transmission contexts and are not consulted when communities assess competence loss.
% DISAPPEARANCE_RATIONALE: If the binding between ritual form and operational competence dissolved, communities would need to rebuild transmission through explicit pedagogy or technological recording; the current social organization around ritual mentorship and embodied apprenticeship would collapse and reorganize around formal schooling.
% FOUNDING_PROBLEM: How to preserve complex, context-dependent survival skillsâsuch as threat timing, emotional regulation, and coordinated group responseâacross generations when explicit instruction fails to transmit embodied know-how.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive anthropologists and ethnographers of practice attest from outside the ritual beneficiary community that explicit instruction alone frequently fails to transmit procedural and indexical competencies; independent field studies of ritual alteration support the claim that competence degrades when embodied form is disrupted.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).
:- end_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint moves non-propositional knowledge to the community itself without asymmetric rent; suppression is minimal (0.08) because the constraint persists through the failure of alternatives rather than active exclusion. Theater ratio is near zero (0.05) because ritual enactment carries genuine operational function rather than performative maintenance. Accessibility collapse is high (0.85) because once the embodied nature of the competence is understood, explicit-manual alternatives cease to be viable substitutes; resistance is negligible (0.05) because the arrangement is largely self-reinforcing through successful transmission. The metric profile may compute toward mountain; the claimed rope records the absence of an extractive seat.
 *
 * PERSPECTIVAL GAP:
 *   From within the ritual community, the constraint is experienced as the natural and necessary shape of competent practice; from the excluded secular seat, it appears as an unnecessary tradition that could be replaced by explicit instruction. The engine should compute divergent directionality: ritual communities near the beneficiary pole (subsidized by the constraint), secular educators near the target pole (excluded and indirectly burdened by the constraint's resistance to codification), and observers neutral.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual_communities are beneficiaries because the constraint directly subsidizes their retention of survival competence. Secular_educators are not victims in the extractive sense but bear a structural cost: their preferred alternative (propositional instruction) is rendered non-viable by the accessibility collapse, giving them a mild target directionality. No agenda setter extracts from the arrangement; the coordination is diffuse.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling because there is no identifiable party that profits from the arrangement while others pay. The founding problem (transmission of non-codifiable competence) is still live, corroborated by external ethnographic evidence, and the disappearance verdict is world_rearranges â all consistent with a genuine coordination rope rather than a decayed piton or disguised snare. If the founding problem were dead and the arrangement persisted with high theater, it would risk piton classification; here theater is minimal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'This constraint instantiates the hybrid_embedded_reading of the catastrophe_memory_transmission kernel. Would adopting the operational_competence_reading or symbol_continuity_reading change the structural classification by separating form and function?',
    'Comparative analysis of ritual alteration outcomes: if competence degrades when form changes, hybrid is supported; if competence persists or form persists without competence, sibling readings are supported.',
    'If siblings are vindicated, the constraint decomposes into either a pure coordination scaffold (operational) or an identity-based mechanism (symbol), reducing the mountain-like accessibility collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Kernel reading structural ambiguity between hybrid and sibling readings.').

omega_variable(
    form_function_naturality,
    'Is the co-constitution of ritual form and operational competence a natural cognitive limit or a socially reinforced tradition that has naturalized itself?',
    'Cross-cultural developmental studies comparing ritual versus explicit instruction outcomes for survival competence transmission; neurological studies of procedural memory versus declarative memory in threat-response contexts.',
    'If the inseparability is a genuine cognitive universal, the constraint is a false-summit mountain candidate and resistance should remain near zero; if contingent, the rope classification holds but the constraint becomes vulnerable to reform pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_function_naturality, empirical, 'Whether the constraint''s high accessibility collapse reflects a natural law or a constructed tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 50, 0.06).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 50, 0.13).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__hybrid_embedded_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_embedded_reading of the catastrophe_memory_transmission kernel, which decomposes into three structurally distinct readings: hybrid_embedded (co-constitutive form/function), operational_competence (instrumental encoding), and symbol_continuity (intrinsic symbolic preservation). Each reading carries a different epsilon and stakeholder structure; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
