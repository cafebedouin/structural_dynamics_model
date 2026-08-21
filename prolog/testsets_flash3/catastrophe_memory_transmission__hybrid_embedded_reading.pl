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
 *   This constraint describes the intergenerational transmission of
 *   catastrophe memory and survival competence through ritual, specifically
 *   from a 'hybrid embedded' reading. It posits that survival competence is
 *   not merely carried by, but is inseparable from, the symbolic form of the
 *   ritual. Fidelity to the ritual's form is thus essential for transmitting
 *   its operational capacity, as non-propositional knowledge is embedded
 *   directly in the practice. This reading emphasizes the co-constitutive
 *   nature of form and function.
 *
 * KEY AGENTS:
 *   - community_members: Beneficiary (moderate/constrained) — receive and transmit knowledge.
 *   - ritual_practitioners: Agenda-setter (organized/identity_locked) — maintain ritual fidelity.
 *   - future_generations: Beneficiary (powerless/trapped) — depend on faithful transmission.
 *   - analytical_observers: Observer (analytical/analytical) — study the constraint's structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Catastrophe Memory Transmission: Hybrid Embedded Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, 'cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd').
narrative_ontology:cs_kernel_codification('cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd', implicit).
narrative_ontology:cs_authority_grounding('cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd', practice).
narrative_ontology:cs_interpretation_layer_present('cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd').
narrative_ontology:cs_reading_relation('cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_axiom('cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd', foundational, form_and_function_are_co_constitutive).
narrative_ontology:cs_axiom_status(form_and_function_are_co_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd', form_and_function_are_co_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd', foundational, non_propositional_knowledge_is_embedded_in_practice).
narrative_ontology:cs_axiom_status(non_propositional_knowledge_is_embedded_in_practice, holdable).
narrative_ontology:cs_axiom_grounding('cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd', non_propositional_knowledge_is_embedded_in_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd', integrated_ritual_competence).
narrative_ontology:cs_drift_state('cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd', contemporary_secularization_pressure, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('cb7d4c3b-b659-49fc-b2b8-1d0e5a35fbdd', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in rituals that transmit critical survival knowledge and collective memory. They benefit from the coordination and resilience fostered by these practices, but are constrained by the need to maintain fidelity to the ritual form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, community_members, beneficiary,
    moderate, biographical, constrained, local).

% Are responsible for the faithful transmission and enactment of rituals. Their identity is deeply intertwined with the practice, and they ensure the integrity of the symbolic form, believing it essential for the embedded operational competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_practitioners, agenda_setter,
    organized, generational, identity_locked, local).

% Will inherit the survival competence and collective memory encoded in the rituals. They are entirely dependent on the fidelity of current transmission for their future resilience.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Study the mechanisms of cultural transmission and collective memory, analyzing how ritual form and operational competence are intertwined in this context. They seek to understand the structural properties of the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action and memory transmission across generations by embedding critical survival knowledge within specific, faithfully reproduced symbolic and practical forms, ensuring that operational competence is not lost.
% TRANSFER_FUNCTION: Transfers non-propositional knowledge, collective identity, and operational resilience from past to present and future generations through the enactment of ritual, without explicit monetary or material transfer.
% ABSENT_VOICES: Those who might advocate for 'streamlining' or 'modernizing' rituals, believing the symbolic form is merely a container for propositional content, are absent from the core interpretive community. They would argue for separating form from function, potentially degrading the embedded competence.
% DISAPPEARANCE_RATIONALE: If the constraint of ritual fidelity vanished, the embedded survival competence would rapidly degrade as forms are altered or abandoned. The community's ability to respond to future catastrophes, and its collective memory of past ones, would be severely compromised, leading to a loss of resilience and identity.
% FOUNDING_PROBLEM: The problem of transmitting complex, non-propositional survival knowledge and collective memory of catastrophic events across generations in a way that ensures fidelity and efficacy.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of communities with long-term disaster resilience, historical accounts of post-catastrophe recovery, and the ongoing need for intergenerational knowledge transfer in vulnerable populations corroborate the live status of this problem, from sources outside the immediate ritual practitioners.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low (0.15) because the primary function is coordination and transmission, with minimal direct cost to participants beyond the effort of practice. Suppression is also low (0.2) as adherence is largely voluntary, driven by perceived benefit and cultural continuity, rather than active coercion. Accessibility collapse is high (0.7) because once the specific ritual form is lost or significantly altered, the embedded knowledge is difficult to recover or transmit through other means. Resistance is low (0.05) as the community generally recognizes the value of the practice. The claimed type is 'rope' due to its strong coordination function and net benefit to participants, with a 'mountain' substrate reflecting the irreducible link between form and function in this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members and future generations, the constraint is a vital mechanism for survival and cultural continuity. From an external, purely 'rational' perspective that seeks to extract propositional content, the fidelity to symbolic form might appear arbitrary or inefficient, leading to a different classification if the embedded nature of competence is not recognized.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are beneficiaries, as they receive vital knowledge and resilience. Ritual practitioners are agenda-setters, responsible for maintaining the practice, and are identity-locked due to their deep connection to the ritual. There are no clear 'victims' as the constraint primarily serves a collective good, and costs are largely intrinsic to the practice itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine coordination as pure extraction by highlighting the functional necessity of ritual fidelity for knowledge transmission. The low extractiveness and suppression, coupled with high accessibility collapse for alternatives, indicate that the constraint's mandate (intergenerational knowledge transfer) is actively served by its operation, rather than having atrophied into mere performance or extraction. The 'mountain' substrate further reinforces the idea that the form-function link is an irreducible limit, not a policy choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_function_separability,
    'Is the operational competence truly inseparable from the symbolic form, or could the ''content'' be transmitted through alternative, less rigid means?',
    'Empirical studies of communities that have attempted to ''modernize'' or ''streamline'' such rituals, observing the long-term impact on survival competence and collective memory.',
    'If separable, the constraint''s ''mountain'' substrate would weaken, and the ''rope'' classification might shift towards ''tangled_rope'' if fidelity is enforced beyond functional necessity. If inseparable, the current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_function_separability, empirical, 'Tests the core premise of form-function inseparability.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''hybrid embedded'' reading of catastrophe memory transmission, or is it better understood as a variant of ''operational competence'' or ''symbol continuity''?',
    'Further conceptual analysis and ethnographic data to delineate the precise mechanisms of knowledge embedding and transmission, comparing against the core tenets of sibling readings.',
    'If reclassified, the structural properties (especially the ''mountain'' substrate and the specific coordination function) would shift to align with the dominant sibling reading, altering the overall classification and network relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifies the precise nature of this reading within the broader kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 75, 0.15).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 25, 0.2).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 75, 0.2).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_transmission' kernel. This 'hybrid_embedded_reading' emphasizes the inseparability of symbolic form and operational competence, distinct from readings that prioritize either symbolic continuity or pure operational content.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
