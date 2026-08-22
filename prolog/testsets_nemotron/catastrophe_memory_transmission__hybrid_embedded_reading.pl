% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Ritual Form as Embedded Survival Competence (Hybrid Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint describes the hybrid_embedded_reading of catastrophe
 *   memory transmission: the claim that survival competence is encoded within
 *   symbolic form such that the two are inseparable, and that ritual fidelity
 *   transmits operational capacity through non-propositional knowledge
 *   embedded in practice. This is one of three readings of the
 *   catastrophe_memory_transmission kernel. The reading asserts a
 *   co-constitutive relationship: altering ritual form degrades survival
 *   function, but survival function only exists through enacted form. The
 *   constraint operates as coordination through shared practice (rope) with a
 *   mountain-like substrate in the physical/cognitive constraints of embodied
 *   knowledge transmission.
 *
 * KEY AGENTS:
 *   - practitioner_community: Primary beneficiary (organized/identity_locked) — holds and transmits the practice
 *   - knowledge_holders: Primary beneficiary (organized/identity_locked) — specialized carriers of ritual precision
 *   - intergenerational_recipients: Beneficiary (organized/constrained) — receive transmitted competence through participation
 *   - external_analysts: Observer (analytical/analytical) — study the transmission system from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Ritual Form as Embedded Survival Competence (Hybrid Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, '1d0593d1-736b-47d6-a015-3e3f9ac4523b').
narrative_ontology:cs_kernel_codification('1d0593d1-736b-47d6-a015-3e3f9ac4523b', implicit).
narrative_ontology:cs_authority_grounding('1d0593d1-736b-47d6-a015-3e3f9ac4523b', practice).
narrative_ontology:cs_interpretation_layer_present('1d0593d1-736b-47d6-a015-3e3f9ac4523b').
narrative_ontology:cs_reading_relation('1d0593d1-736b-47d6-a015-3e3f9ac4523b', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d0593d1-736b-47d6-a015-3e3f9ac4523b', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('1d0593d1-736b-47d6-a015-3e3f9ac4523b', foundational, form_function_coconstitution).
narrative_ontology:cs_axiom_status(form_function_coconstitution, holdable).
narrative_ontology:cs_axiom_grounding('1d0593d1-736b-47d6-a015-3e3f9ac4523b', form_function_coconstitution, deontological).
narrative_ontology:cs_axiom('1d0593d1-736b-47d6-a015-3e3f9ac4523b', foundational, ritual_fidelity_as_epistemic_condition).
narrative_ontology:cs_axiom_status(ritual_fidelity_as_epistemic_condition, holdable).
narrative_ontology:cs_axiom_grounding('1d0593d1-736b-47d6-a015-3e3f9ac4523b', ritual_fidelity_as_epistemic_condition, empirically_contingent).
narrative_ontology:cs_reference_frame('1d0593d1-736b-47d6-a015-3e3f9ac4523b', intact_transmission_chain).
narrative_ontology:cs_drift_state('1d0593d1-736b-47d6-a015-3e3f9ac4523b', contemporary_documentation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1d0593d1-736b-47d6-a015-3e3f9ac4523b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, practitioner_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, knowledge_holders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, intergenerational_recipients).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, embodied_knowledge_primacy).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_fidelity_as_epistemic_condition).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, symbolic_form_operational_unity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community that maintains and enacts the ritual practice. Their identity and survival competence are constituted through participation. Exit would mean abandoning the practice that constitutes their communal identity and the embodied knowledge it carries. They experience the constraint as the condition of their continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, practitioner_community, beneficiary,
    organized, generational, identity_locked, regional).

% Specialized practitioners (elders, ritual specialists, initiates) who hold the precise form of the ritual. They set the standard of fidelity and transmit it through direct apprenticeship. Their authority derives from their embodiment of the practice, not from institutional office. Exit is identity_locked — their role is constituted by the practice they guard.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, knowledge_holders, beneficiary,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, knowledge_holders, agenda_setter).

% Younger or newer participants who acquire survival competence through ritual participation. They benefit from the transmission but have not yet mastered the practice. Their exit is constrained — they could leave the community, but would lose access to the competence the practice transmits. Their participation is the mechanism of their own formation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, intergenerational_recipients, beneficiary,
    organized, biographical, constrained, regional).

% Scholars of ritual studies, collective memory, and religious studies who analyze the transmission system from outside. They do not participate in the practice and do not bear its costs or receive its competence. Their exit is analytical — they can change frameworks without personal cost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, external_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__hybrid_embedded_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__hybrid_embedded_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits catastrophe survival competence across generations without propositional codification, using ritual enactment as the sole transmission vehicle for knowledge that cannot be written down or taught abstractly.
% TRANSFER_FUNCTION: Moves embodied survival competence from knowledge_holders to intergenerational_recipients through the practitioner_community's ritual enactment, with ritual fidelity as the transmission condition. No material resources flow; the transfer is epistemic and identity-constitutive.
% ABSENT_VOICES: Communities that have lost their ritual transmission chains — they would testify to the irreversibility of the loss, but they are no longer in the practice to speak. Potential practitioners who never received the transmission because the chain broke before their generation.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight (ritual fidelity no longer required for transmission), the practitioner community would lose the mechanism that preserves survival competence. The knowledge would degrade within a generation. The community's identity, constituted through the practice, would dissolve or transform. The world rearranges because the practice is the mechanism of its own reproduction.
% FOUNDING_PROBLEM: How to transmit complex, context-sensitive catastrophe survival competence (navigation, resource identification, threat recognition, social coordination under extremity) across generations in the absence of writing, when propositional instruction fails to capture the embodied judgment required.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic records from multiple indigenous and traditional communities (Inuit navigation rituals, Andean water management ceremonies, Pacific wayfinding chants) document the same claim: the practice transmits competence that cannot be extracted into propositions. Corroborated by cognitive science research on embodied cognition and the limits of explicit instruction for complex motor-perceptual skills (e.g., Dreyfus model of skill acquisition, Varela/Thompson/Rosch enactive cognition).
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Low extractiveness (0.18) reflects that the constraint's primary operation is coordination of knowledge transmission, not resource extraction. Participants are net beneficiaries: the practice solves a genuine coordination problem (intergenerational transmission of complex survival competence without propositional codification). Low suppression (0.12) because participation is voluntary and alternatives (written records, formal training) exist but are claimed to be insufficient for this domain. Low theater_ratio (0.08) because ritual fidelity is functionally necessary, not performative. High accessibility_collapse (0.72) because once the embodied knowledge is lost, propositional substitutes cannot recover it — the constraint's operation creates genuine irreversibility. Low resistance (0.15) because the constraint is maintained by participants' conviction of its necessity, not coercion.
 *
 * PERSPECTIVAL GAP:
 *   The practitioner seat experiences the constraint as mountain-like (the embodied knowledge feels like a natural law of their world), while the analytical observer sees rope-like coordination. The engine will compute this divergence from the structural data: practitioners are identity_locked with generational time horizons, making the constraint feel fixed; analysts have analytical exit and see the coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   All named agents are beneficiaries: the practitioner community, knowledge holders, and intergenerational recipients all gain survival competence through the practice. No victims exist in steady-state operation — the constraint coordinates rather than extracts. Directionality for all beneficiaries derives toward d ≈ 0.1–0.2 (subsidy position). The constraint's extractiveness is the maintenance cost of the practice itself (time, attention, precision), which participants bear voluntarily because the alternative (loss of competence) is worse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (intergenerational transmission of catastrophe survival competence without writing) remains live for communities that maintain the practice. The constraint is not mandatrophic — its function has not atrophied. However, for communities that have lost the practice, the constraint becomes a mountain (the knowledge is gone) rather than a rope (the coordination continues). This creates a boundary condition where the constraint's type depends on whether the transmission chain is intact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the hybrid_embedded_reading''s claim that form and function are co-constitutive logically foreclose the operational_competence_reading''s claim that ritual transmits competence through pattern recognition and rehearsal, or do they coexist as different analytical lenses on the same practice?',
    'Compare whether the operational_competence_reading requires propositional extraction of competence from ritual (which hybrid_embedded_reading denies) or merely describes functional outcomes of embodied practice (which hybrid_embedded_reading could accommodate). Empirical test: do practitioners who hold the hybrid view reject functional-analysis accounts as category errors?',
    'If forecloses, the kernel has a genuine logical split; if coexists_with, the readings are complementary analytical framings. Affects cs_structure.reading_relations classification and the kernel''s CS pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between hybrid embedded and operational competence readings of catastrophe memory transmission').

omega_variable(
    symbolic_vs_embodied_primacy,
    'Is the ''symbolic form'' in the hybrid reading''s formulation a distinct ontological category from ''embodied practice'', or are they two descriptions of the same phenomenon?',
    'Ethnographic investigation of whether practitioners distinguish ''the symbols'' from ''the practice'' in their own accounts, or whether the distinction is an analytic imposition. Phenomenological analysis of ritual performance.',
    'If symbolic form and embodied practice are analytically distinct, the hybrid reading makes a stronger ontological claim; if they are analytically conflated, the reading may be tautological. Affects extractiveness measurement (tautological constraints show lower ε).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_vs_embodied_primacy, conceptual, 'Ontological status of the symbolic/embodied distinction in the hybrid reading').

omega_variable(
    discontinuation_victim_status,
    'If practice is discontinued, who (if anyone) is a victim of the constraint''s dissolution rather than its operation?',
    'Track communities that have lost ritual transmission chains: do they experience the loss as extraction (something taken from them) or as attrition (something they failed to maintain)? Compare with communities that voluntarily reformed practice.',
    'If discontinuation creates identifiable victims, the constraint may have snare-like properties at the dissolution margin even if rope-like in steady state. Affects classification boundary analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuation_victim_status, empirical, 'Whether practice discontinuation creates victims, implicating snare dynamics at the constraint''s boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmt_her_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cmt_her_tr_t50, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 50, 0.06).
narrative_ontology:measurement(cmt_her_tr_t100, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 100, 0.07).
narrative_ontology:measurement(cmt_her_tr_t150, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 150, 0.08).

% Extraction over time
narrative_ontology:measurement(cmt_her_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cmt_her_be_t50, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 50, 0.14).
narrative_ontology:measurement(cmt_her_be_t100, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 100, 0.16).
narrative_ontology:measurement(cmt_her_be_t150, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 150, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(cmt_her_su_t0, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(cmt_her_su_t50, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 50, 0.09).
narrative_ontology:measurement(cmt_her_su_t100, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 100, 0.11).
narrative_ontology:measurement(cmt_her_su_t150, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 150, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__hybrid_embedded_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the catastrophe_memory_transmission kernel. The hybrid_embedded_reading asserts co-constitutive unity of ritual form and survival function. The operational_competence_reading treats ritual as a vehicle for propositionally analyzable competence transmission. The symbol_continuity_reading treats symbolic continuity as the survival mechanism itself. All three readings share the same referent (catastrophe memory transmission practices) but author different ε values and beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
