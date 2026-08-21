% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Laws: Demographic Trap Reading
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   This constraint is the 'demographic trap' reading of the Lycurgan laws
 *   kernel. It focuses on how the laws' unrevisability and specific social
 *   structures (kleros system, strict citizenship requirements) led to a
 *   declining Spartiate population and the eventual collapse of Sparta, in
 *   contrast to readings that emphasize sacral fidelity or covert adaptation.
 *   The laws, intended to create stability, ultimately became a snare,
 *   extracting the very demographic vitality they needed to sustain
 *   themselves.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.85).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.9).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Laws: Demographic Trap Reading").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '5aace049-144f-4054-82d4-34384da911d3').
narrative_ontology:cs_kernel_codification('5aace049-144f-4054-82d4-34384da911d3', formalized).
narrative_ontology:cs_authority_grounding('5aace049-144f-4054-82d4-34384da911d3', lineage).
narrative_ontology:cs_reading_relation('5aace049-144f-4054-82d4-34384da911d3', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5aace049-144f-4054-82d4-34384da911d3', lycurgan_laws__adaptive_fiction_reading, forecloses).
narrative_ontology:cs_axiom('5aace049-144f-4054-82d4-34384da911d3', foundational, unrevisability_leads_to_brittleness).
narrative_ontology:cs_axiom_status(unrevisability_leads_to_brittleness, holdable).
narrative_ontology:cs_axiom_grounding('5aace049-144f-4054-82d4-34384da911d3', unrevisability_leads_to_brittleness, empirically_contingent).
narrative_ontology:cs_axiom('5aace049-144f-4054-82d4-34384da911d3', foundational, kleros_system_causes_demographic_decline).
narrative_ontology:cs_axiom_status(kleros_system_causes_demographic_decline, holdable).
narrative_ontology:cs_axiom_grounding('5aace049-144f-4054-82d4-34384da911d3', kleros_system_causes_demographic_decline, empirically_contingent).
narrative_ontology:cs_reference_frame('5aace049-144f-4054-82d4-34384da911d3', lycurgan_ideal_of_stability).
narrative_ontology:cs_drift_state('5aace049-144f-4054-82d4-34384da911d3', spartan_hegemony_decline, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5aace049-144f-4054-82d4-34384da911d3', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, spartiate_elite).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartiate_citizens).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, helots).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the system's initial stability, their privileged landholdings (kleros), and social status. Their identity was deeply intertwined with the Lycurgan system, making systemic reform unthinkable, even as the demographic base eroded.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartiate_elite, agenda_setter,
    institutional, generational, identity_locked, national).

% Bore the costs of the rigid social structure, including mandatory military training, communal living, and the kleros system which, combined with strict citizenship requirements, led to a declining population base and eventual demographic collapse.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartiate_citizens, payer,
    powerless, generational, trapped, national).

% Already enslaved, their status was rigidly maintained by the Lycurgan system, providing the labor base for the Spartiate economy. The system's immutability ensured their continued exploitation without possibility of social mobility or escape.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helots, payer,
    powerless, generational, trapped, local).

% Magistrates responsible for enforcing the Lycurgan laws. While holding significant power, they were also bound by the laws' unrevisability, preventing them from implementing necessary adaptations to address the growing demographic crisis.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, ephors, agenda_setter,
    institutional, biographical, constrained, national).

% Analyze the long-term consequences of the Lycurgan system, identifying the causal links between its rigid structure, the kleros system, citizenship restrictions, and the demographic decline of the Spartiate population.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__demographic_trap_reading, spartiate_elite).
narrative_ontology:fixing_cost_class(lycurgan_laws__demographic_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a highly disciplined, militaristic society focused on collective defense and internal stability, coordinating social roles, resource distribution (kleros), and military training among Spartiates.
% TRANSFER_FUNCTION: Transferred labor and resources from the helot population to support the Spartiate military class, and enforced a rigid social hierarchy that extracted adaptability and demographic vitality from the Spartiate citizenry.
% ABSENT_VOICES: Any voices advocating for social or constitutional reform, or for more flexible citizenship and land tenure policies, were structurally suppressed by the laws' immutability and the rigid political system. These voices would have argued for changes to prevent demographic collapse.
% DISAPPEARANCE_RATIONALE: If the Lycurgan laws and their enforcement vanished overnight, the foundational structure of Sparta would collapse. The kleros system would dissolve, helots would likely revolt, and the Spartiate military and social identity would cease to exist, leading to a complete reorganization of the region's political and social landscape.
% FOUNDING_PROBLEM: To create a stable, militarily superior society capable of maintaining control over a large subjugated population (helots) and resisting external threats, while preventing internal dissent and luxury.
% FOUNDING_PROBLEM_CORROBORATION: Ancient historians like Plutarch and Xenophon describe the laws and their intent. Modern historical and demographic analyses, from outside the Spartan elite, corroborate that the system's rigidity ultimately led to its demographic and political decline, rather than its sustained stability, indicating the founding problem was not solved in the long term.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the system's rigidity, particularly the kleros system and citizenship rules, directly led to a shrinking Spartiate population, effectively extracting future generations and adaptability. Suppression is very high due to the absolute and immutable nature of the laws, which actively prevented any internal reform or deviation. Theater ratio is low because the system was genuinely functional and strictly enforced for centuries, even if its long-term consequences were destructive. The rising extractiveness over time reflects the deepening demographic crisis.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Spartiate elite, the laws were the foundation of their power and identity, a source of stability. From the perspective of the declining Spartiate citizenry and analytical historians, the same laws were a demographic trap, extracting their future. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spartiate elite, as agenda-setters, initially benefited from the system's stability and their privileged position, but became identity-locked, unable to adapt. Spartiate citizens and helots were the primary targets, bearing the costs of the rigid social structure and exploitation, with virtually no exit options. The Ephors, while powerful enforcers, were also constrained by the laws' immutability.
 *
 * MANDATROPHY ANALYSIS:
 *   The Lycurgan laws were designed to solve problems of instability and external threat. However, the 'demographic trap' reading argues that the laws' unrevisability meant they outlived their functional utility for long-term stability, instead becoming a self-destructive mechanism. The founding problem is 'dead' because the laws ultimately failed to sustain Sparta, yet the system persisted due to its inherent rigidity and the elite's identity-lock, indicating a form of mandatrophy where the original mandate was undermined by the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unrevisability_vs_external_factors,
    'To what extent was Sparta''s demographic collapse primarily due to the unrevisability of the Lycurgan laws (internal rigidity) versus external factors (e.g., prolonged warfare, natural disasters)?',
    'Comparative historical analysis with other ancient states facing similar external pressures but possessing more flexible constitutional structures, assessing their demographic resilience.',
    'If internal rigidity is the dominant factor, the Snare classification is strongly reinforced. If external factors are primary, the constraint''s extractiveness might be re-evaluated as less intrinsic to the laws themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unrevisability_vs_external_factors, empirical, 'Attribution of Sparta''s decline to internal vs. external causes.').

omega_variable(
    covert_adaptation_possibility,
    'Did the Lycurgan system possess covert mechanisms for adaptation or informal revision that mitigated its apparent rigidity, as suggested by the ''adaptive fiction'' reading?',
    'Detailed archaeological and textual analysis for evidence of unwritten customs or political practices that allowed for de facto flexibility despite the formal immutability.',
    'If significant covert adaptation is found, the suppression and extractiveness metrics would be lower, potentially shifting the classification towards a Tangled Rope or even a Rope, as the system would have been less of a ''trap''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covert_adaptation_possibility, empirical, 'Presence of covert adaptation mechanisms in the Lycurgan system.').

omega_variable(
    sacral_justification_impact,
    'How much did the sacral justification of the Lycurgan laws (divine origin, absolute adherence) contribute to their unrevisability and the resulting demographic trap, versus purely political or social inertia?',
    'Analysis of religious texts and practices in Sparta, and comparative studies of other ancient societies with divinely sanctioned laws, to gauge the practical impact of sacrality on constitutional flexibility.',
    'If sacrality was a primary driver of unrevisability, it reinforces the ''identity_locked'' exit option for the elite and the high suppression, highlighting the ideological component of the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacral_justification_impact, conceptual, 'Role of sacral justification in Lycurgan immutability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lycu_tr_t60, lycurgan_laws__demographic_trap_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(lycu_tr_t120, lycurgan_laws__demographic_trap_reading, theater_ratio, 120, 0.1).
narrative_ontology:measurement(lycu_tr_t180, lycurgan_laws__demographic_trap_reading, theater_ratio, 180, 0.1).
narrative_ontology:measurement(lycu_tr_t240, lycurgan_laws__demographic_trap_reading, theater_ratio, 240, 0.1).
narrative_ontology:measurement(lycu_tr_t300, lycurgan_laws__demographic_trap_reading, theater_ratio, 300, 0.1).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(lycu_be_t60, lycurgan_laws__demographic_trap_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(lycu_be_t120, lycurgan_laws__demographic_trap_reading, base_extractiveness, 120, 0.75).
narrative_ontology:measurement(lycu_be_t180, lycurgan_laws__demographic_trap_reading, base_extractiveness, 180, 0.8).
narrative_ontology:measurement(lycu_be_t240, lycurgan_laws__demographic_trap_reading, base_extractiveness, 240, 0.83).
narrative_ontology:measurement(lycu_be_t300, lycurgan_laws__demographic_trap_reading, base_extractiveness, 300, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(lycu_su_t60, lycurgan_laws__demographic_trap_reading, suppression_requirement, 60, 0.82).
narrative_ontology:measurement(lycu_su_t120, lycurgan_laws__demographic_trap_reading, suppression_requirement, 120, 0.85).
narrative_ontology:measurement(lycu_su_t180, lycurgan_laws__demographic_trap_reading, suppression_requirement, 180, 0.87).
narrative_ontology:measurement(lycu_su_t240, lycurgan_laws__demographic_trap_reading, suppression_requirement, 240, 0.89).
narrative_ontology:measurement(lycu_su_t300, lycurgan_laws__demographic_trap_reading, suppression_requirement, 300, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Lycurgan laws' kernel. This 'demographic trap' reading focuses on the laws' unrevisability leading to societal collapse, contrasting with the 'sacral fidelity' reading (emphasizing divine origin) and the 'adaptive fiction' reading (positing covert flexibility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
