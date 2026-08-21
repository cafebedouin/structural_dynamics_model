% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Catastrophe Memory Transmission: Hybrid Embedded Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid embedded' reading of catastrophe
 *   memory transmission, where survival competence is understood as
 *   inseparable from its symbolic ritual form. Ritual fidelity is not merely
 *   symbolic continuity but the direct transmission of operational capacity
 *   through non-propositional knowledge embedded in practice. This reading
 *   emphasizes the co-constitutive nature of form and function: altering the
 *   ritual form degrades the operational function, but the function itself
 *   only exists through the enacted form. It is claimed as a Rope due to its
 *   genuine coordination function in transmitting vital knowledge, with a low
 *   extractiveness, but acknowledges a Mountain-like substrate in the
 *   embodied nature of knowledge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.25).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Catastrophe Memory Transmission: Hybrid Embedded Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, 'e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d').
narrative_ontology:cs_kernel_codification('e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d', implicit).
narrative_ontology:cs_authority_grounding('e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d', practice).
narrative_ontology:cs_interpretation_layer_present('e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d').
narrative_ontology:cs_reading_relation('e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_axiom('e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d', foundational, form_and_function_are_co_constitutive).
narrative_ontology:cs_axiom_status(form_and_function_are_co_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d', form_and_function_are_co_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d', foundational, tacit_knowledge_requires_ritual_fidelity).
narrative_ontology:cs_axiom_status(tacit_knowledge_requires_ritual_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d', tacit_knowledge_requires_ritual_fidelity, empirically_contingent).
narrative_ontology:cs_reference_frame('e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d', integrated_ritual_competence).
narrative_ontology:cs_drift_state('e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d', contemporary_secular_analysis, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('e0d887ea-f5cc-413c-ac56-b7c4ac22bc6d', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the transmission of survival knowledge and collective identity through ritual. Their sense of belonging and capacity to respond to future crises is tied to the fidelity of the practice. Exit means losing access to this embedded competence and communal bond.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, community_members, beneficiary,
    organized, generational, identity_locked, local).

% Are responsible for maintaining ritual fidelity and transmitting the embedded knowledge. They derive status and purpose from this role. Any deviation from established form is seen as a threat to the constraint's efficacy. Their identity is fused with the practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_practitioners, agenda_setter,
    moderate, biographical, identity_locked, local).

% Study the mechanisms of cultural transmission and collective memory, observing how ritual forms encode and transmit non-propositional knowledge. They seek to understand the structural relationship between symbolic fidelity and operational competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action and memory by embedding survival competence within specific symbolic forms and ritual practices, ensuring that critical knowledge is transmitted across generations non-propositionally.
% TRANSFER_FUNCTION: Transfers non-propositional survival competence and collective memory from past generations to present and future community members, through the faithful enactment of ritual forms.
% ABSENT_VOICES: Those who prioritize propositional, explicit knowledge transmission might argue for more direct, less ritualized forms of education, but their perspective is often marginalized in communities where embodied knowledge is paramount.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the community would lose its primary mechanism for transmitting critical survival knowledge and collective identity. Future generations would be less prepared for similar catastrophes, and the social fabric would weaken as shared memory and practice erode.
% FOUNDING_PROBLEM: How to transmit critical survival knowledge and collective memory of past catastrophes across generations, especially when explicit propositional knowledge is insufficient or easily lost.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of communities that have endured repeated environmental or social crises corroborate the ongoing need for robust, resilient memory transmission mechanisms. Community elders and historians outside the immediate ritual practitioners attest to the problem's persistence.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.15) because the primary function is coordination and knowledge transmission, not rent-seeking. Any 'cost' is inherent to the effort of maintaining fidelity. Suppression (0.25) is moderate, reflecting the social pressure to conform to ritual forms for the sake of collective efficacy, but it's not coercive in a punitive sense. Theater ratio is low (0.1) as the practices are genuinely believed to be functional. Accessibility collapse is high (0.75) because the non-propositional nature of the knowledge means alternatives (e.g., written manuals) are largely ineffective substitutes. Resistance is low (0.05) because the community generally accepts the efficacy of the ritual.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between this 'hybrid embedded' reading and other interpretations of ritual. Those who see ritual as purely symbolic (symbol_continuity_reading) or purely functional (operational_competence_reading) would experience the constraint differently, potentially seeing less inherent value in the form itself or more 'superfluous' elements. This reading asserts the inseparability.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members are beneficiaries, gaining survival competence and identity. Ritual practitioners are also beneficiaries and agenda-setters, as they maintain the practice and derive purpose from it. There are no clear 'victims' as all participants are net beneficiaries of the knowledge transmission, though the constraint requires adherence to specific forms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_function_separability,
    'To what extent are the symbolic form and operational competence truly inseparable, or could the operational competence be transmitted through alternative, less ritualized forms?',
    'Empirical studies of communities that have intentionally altered ritual forms to test the impact on operational outcomes, or comparative studies across cultures with varying degrees of ritual fidelity.',
    'If separable, the constraint''s ''mountain'' aspect (inseparability) would weaken, potentially reclassifying it closer to a pure Rope or even a Tangled Rope if fidelity is enforced beyond functional necessity. If inseparable, the current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_function_separability, empirical, 'Ambiguity regarding the co-constitutive nature of ritual form and function.').

omega_variable(
    tacit_knowledge_measurement,
    'How can the ''non-propositional knowledge'' embedded in practice be objectively measured and its transmission efficacy quantified, independent of the ritual''s symbolic aspects?',
    'Development of new ethnographic and cognitive science methodologies capable of isolating and measuring tacit knowledge transfer in ritual contexts, distinct from explicit instruction or symbolic meaning.',
    'Improved measurement would provide stronger empirical grounding for the ''operational competence'' aspect of this reading, potentially shifting extractiveness if the ''cost'' of fidelity is found to exceed measurable functional gain. Lack of measurement leaves the claim vulnerable to conceptual challenges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tacit_knowledge_measurement, empirical, 'Challenges in objectively measuring non-propositional knowledge transmission.').

omega_variable(
    reading_framing_choice,
    'Is the ''hybrid_embedded_reading'' the most appropriate framing for this constraint, or would a ''symbol_continuity_reading'' or ''operational_competence_reading'' better capture its essence?',
    'Analysis of community self-description, historical records, and observed outcomes: if the community primarily emphasizes identity preservation, the symbol_continuity_reading might be more apt; if explicit skill transfer, the operational_competence_reading.',
    'Adopting a different reading would lead to a different constraint classification, as the core ε and stakeholder relationships would shift. For example, a pure symbol_continuity_reading might have lower extractiveness but higher theater_ratio if the ''operational'' aspect is deemed secondary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'Under-determination of the constraint''s primary function and framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 75, 0.09).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 75, 0.14).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 50, 0.25).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 75, 0.24).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 100, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
