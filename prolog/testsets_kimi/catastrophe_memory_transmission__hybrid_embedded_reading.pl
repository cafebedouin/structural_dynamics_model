% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
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
 *   This constraint instantiates the hybrid_embedded_reading of the
 *   catastrophe_memory_transmission kernel: ritual form and operational
 *   survival competence are co-constitutive, and ritual fidelity transmits
 *   operational capacity through non-propositional knowledge embedded in
 *   practice. The kernel is contested by the operational_competence_reading
 *   (instrumental encoding of competence) and the symbol_continuity_reading
 *   (symbolic preservation as intrinsic communal good). This reading treats
 *   the form-function binding as a coordination mechanism (rope) underwritten
 *   by the physical constraint of embodied cognition (mountain substrate).
 *
 * KEY AGENTS:
 *   - ritual_community (beneficiary/organized/identity_locked) â receives survival competence encoded in ritual form
 *   - ritual_guardians (agenda_setter/moderate/constrained) â maintain canonical ritual form and transmit practice
 *   - modernizing_reformers (excluded/moderate/mobile) â advocate propositional alternatives excluded from the traditional framework
 *   - disaster_anthropologists (observer/analytical/analytical) â document the form-function linkage from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.25).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Hybrid Embedded Ritual Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, 'e06fd7c8-3679-42d8-a768-7d543ecdadcb').
narrative_ontology:cs_kernel_codification('e06fd7c8-3679-42d8-a768-7d543ecdadcb', implicit).
narrative_ontology:cs_authority_grounding('e06fd7c8-3679-42d8-a768-7d543ecdadcb', practice).
narrative_ontology:cs_interpretation_layer_present('e06fd7c8-3679-42d8-a768-7d543ecdadcb').
narrative_ontology:cs_reading_relation('e06fd7c8-3679-42d8-a768-7d543ecdadcb', catastrophe_memory_transmission__operational_competence_reading, influences).
narrative_ontology:cs_reading_relation('e06fd7c8-3679-42d8-a768-7d543ecdadcb', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('e06fd7c8-3679-42d8-a768-7d543ecdadcb', foundational, form_function_co_constitution).
narrative_ontology:cs_axiom_status(form_function_co_constitution, holdable).
narrative_ontology:cs_axiom_grounding('e06fd7c8-3679-42d8-a768-7d543ecdadcb', form_function_co_constitution, empirically_contingent).
narrative_ontology:cs_axiom('e06fd7c8-3679-42d8-a768-7d543ecdadcb', foundational, non_propositional_transmission).
narrative_ontology:cs_axiom_status(non_propositional_transmission, holdable).
narrative_ontology:cs_axiom_grounding('e06fd7c8-3679-42d8-a768-7d543ecdadcb', non_propositional_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('e06fd7c8-3679-42d8-a768-7d543ecdadcb', embodied_competence_unity).
narrative_ontology:cs_drift_state('e06fd7c8-3679-42d8-a768-7d543ecdadcb', contemporary_secular_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e06fd7c8-3679-42d8-a768-7d543ecdadcb', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts the ritual cycle as a collective body, receiving survival-relevant operational competenceâdisaster response, ecological calibration, resource coordinationâencoded in symbolic and embodied form. Participation constitutes both identity and practical knowledge; exit would mean abandoning the community and the non-propositional competence it carries.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_community, beneficiary,
    organized, generational, identity_locked, regional).

% Maintain canonical ritual form, correct performance errors, and initiate novices. Their authority derives from demonstrated fidelity to the practice; they are accountable to tradition and to the community for preserving the link between form and function, and they bear blame if altered form degrades competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_guardians, agenda_setter,
    moderate, generational, constrained, regional).

% Advocate for replacing ritual transmission with formal education, written protocols, or technical training. They argue that operational competence can be abstracted from symbolic form and transmitted propositionally. They are structurally excluded from the ritual community's decision-making and their alternatives are not tested within the traditional framework.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, modernizing_reformers, excluded,
    moderate, biographical, mobile, national).

% Document and analyze how ritual practice encodes survival competence across cultures. They observe the structural relationship between form and function without participating in the ritual's authority system or identity economy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, disaster_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transmission of survival-relevant operational competence across generations without relying on stable propositional memory, written archives, or formal institutions that are vulnerable to catastrophic disruption.
% TRANSFER_FUNCTION: Moves non-propositional, embodied knowledgeâpattern recognition, threat-response rehearsal, ecological timing, solidarity activationâfrom experienced practitioners to novices through enacted symbolic form.
% ABSENT_VOICES: Modernizing reformers and secular education advocates would argue that ritual form is detachable from operational content and that propositional instruction could substitute; they are excluded from the ritual community's deliberative process.
% DISAPPEARANCE_RATIONALE: If the ritual transmission constraint vanished, the community would lose access to operational competence that is not codifiable in propositional form; disaster-response patterns, ecological calendars, and intergenerational solidarity mechanisms would degrade within a generation because no substitute transmission channel exists for the embodied knowledge.
% FOUNDING_PROBLEM: Catastrophic events disrupt propositional memory, written records, and institutional continuity; communities needed a resilient transmission channel for survival competence that does not depend on stable text or formal education.
% FOUNDING_PROBLEM_CORROBORATION: Archaeological evidence of cyclical disasters (e.g., volcanic winters, floods) and ethnographic records from outside the benefiting ritual community attest that propositional memory fails during societal collapse, corroborating the founding problem from observer seats.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.22, 'kimi-k2.6', 'none', direct).

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
 *   Metrics reflect a genuine coordination mechanism with low extraction: the ritual solves a collective-action problem (intergenerational competence transfer) without identifiable victims. Accessibility collapse is high (0.82) because the embodied, non-propositional nature of the knowledge makes substitutes structurally inadequate. Resistance is low (0.22) because the constraint is not actively coercive outside the community's own identity boundaries. Theater ratio is low (0.18) because the ritual is functionally loaded rather than performatively hollow. The slight upward drift in base_extractiveness and theater ratio over the interval reflects modernity's pressure on the practice, not an inherent extractive ratchet.
 *
 * PERSPECTIVAL GAP:
 *   The ritual community experiences the constraint as identity-constitutive benefit (low d), while the modernizing reformer experiences it as an unnecessary identity lock (high d). The agenda-setter (ritual guardians) sits between: they bear the cost of maintaining fidelity but also derive status and generational continuity (moderate d). The engine should compute these divergences from the structural data without reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   The ritual_community is the declared beneficiary: they receive survival competence through participation, placing them toward the beneficiary end of directionality. The ritual_guardians administer the constraint but are not primary capturers of extraction; they receive status and continuity, but the low extractiveness means there is little surplus to capture. The modernizing_reformers are not victims of extraction but are structurally excluded from redefining the constraint, giving them a high-d observer position. The diffuse, non-monetizable nature of the benefit means no single seat concentrates the gains, and effective extraction remains low across all positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a case of mandatrophy because its founding problemâtransmitting operational competence through catastrophic disruptionâremains live for the communities that maintain the practice. There is no decayed mandate being theatrically maintained; the ritual's functional load, while under pressure from modernity, is still operative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the kernel catastrophe_memory_transmission. How would classifying the kernel under operational_competence_reading or symbol_continuity_reading change the beneficiary structure and directionality?',
    'Comparative analysis of the sibling constraint stories to identify divergent epsilon and victim/beneficiary declarations.',
    'If operational_competence_reading decouples form from function, the constraint may reclassify as scaffold or rope with lower identity-lock; if symbol_continuity_reading severs operational content entirely, it may reclassify as identity_coordination with higher theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural ambiguity arising from kernel reading selection').

omega_variable(
    embodied_inseparability,
    'Is the inseparability of ritual form and operational competence a biologically grounded cognitive constraint or a culturally stabilized convention?',
    'Cross-cultural cognitive archaeology and embodied cognition experiments testing whether ritual form can be substituted while preserving operational transfer.',
    'If biologically grounded, the constraint carries mountain-like immunity to reform; if conventional, it is a rope vulnerable to functional substitution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(embodied_inseparability, empirical, 'Naturalness ambiguity of the form-function binding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 50, 0.18).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 20, 0.21).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 50, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__hybrid_embedded_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_transmission kernel decomposes into three structurally distinct constraints: hybrid_embedded_reading (co-constitutive form-function), operational_competence_reading (instrumental encoding), and symbol_continuity_reading (intrinsic symbolic preservation). Each reading carries a different epsilon and stakeholder topology; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
