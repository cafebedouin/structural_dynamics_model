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
    narrative_ontology:coordination_type/2,
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
 *   This constraint story analyzes the Lycurgan laws of ancient Sparta
 *   through the 'demographic trap' reading. It posits that the laws, despite
 *   their initial aim to create a stable and militarily superior society,
 *   became a snare due to their extreme rigidity and unrevisability. This
 *   structural flaw, particularly concerning land distribution (kleros
 *   system) and citizenship restrictions, led to a continuous decline in the
 *   Spartiate population, ultimately undermining the very military and social
 *   order they were designed to protect. The constraint is classified as a
 *   Snare because its coordination function (military strength, social order)
 *   became a cover for a system that extracted the demographic viability of
 *   its core population.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.85).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.92).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Laws: Demographic Trap Reading").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '91a2def1-fd3d-4441-a19b-b65f4dea5e90').
narrative_ontology:cs_kernel_codification('91a2def1-fd3d-4441-a19b-b65f4dea5e90', formalized).
narrative_ontology:cs_authority_grounding('91a2def1-fd3d-4441-a19b-b65f4dea5e90', lineage).
narrative_ontology:cs_reading_relation('91a2def1-fd3d-4441-a19b-b65f4dea5e90', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_reading_relation('91a2def1-fd3d-4441-a19b-b65f4dea5e90', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('91a2def1-fd3d-4441-a19b-b65f4dea5e90', foundational, unrevisability_as_structural_flaw).
narrative_ontology:cs_axiom_status(unrevisability_as_structural_flaw, holdable).
narrative_ontology:cs_axiom_grounding('91a2def1-fd3d-4441-a19b-b65f4dea5e90', unrevisability_as_structural_flaw, empirically_contingent).
narrative_ontology:cs_axiom('91a2def1-fd3d-4441-a19b-b65f4dea5e90', secondary, rigid_social_structures_cause_demographic_decline).
narrative_ontology:cs_axiom_status(rigid_social_structures_cause_demographic_decline, holdable).
narrative_ontology:cs_axiom_grounding('91a2def1-fd3d-4441-a19b-b65f4dea5e90', rigid_social_structures_cause_demographic_decline, empirically_contingent).
narrative_ontology:cs_reference_frame('91a2def1-fd3d-4441-a19b-b65f4dea5e90', lycurgan_ideal_of_stability).
narrative_ontology:cs_drift_state('91a2def1-fd3d-4441-a19b-b65f4dea5e90', late_spartan_period, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('91a2def1-fd3d-4441-a19b-b65f4dea5e90', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, spartiate_elite).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, ephors).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartiate_citizens).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, helots).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, spartan_women).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartiate_elite).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartan_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initially benefited from the rigid social order and land distribution (kleros system) that secured their power and status. Over time, as the Spartiate population declined due to the system's rigidity, they became payers of the system's ultimate failure, trapped by the very laws they upheld.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartiate_elite, agenda_setter,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, spartiate_elite, payer).

% Subject to the severe discipline and rigid social structures of the Lycurgan laws, including strict land inheritance rules and citizenship requirements. They bore the costs of a system that, through its unrevisability, led to their demographic decline and eventual extinction as a distinct class.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartiate_citizens, payer,
    powerless, biographical, identity_locked, national).

% The enslaved population whose labor supported the Spartiate system. They bore the most extreme costs of the system, with no rights or exit options, their existence entirely dictated by the laws designed to maintain Spartiate dominance.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helots, payer,
    powerless, generational, trapped, local).

% The chief magistrates responsible for enforcing the Lycurgan laws. They benefited from the power and authority granted by the system, but were also constrained by its immutability, unable to adapt the laws even as their destructive effects became apparent.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, ephors, agenda_setter,
    institutional, biographical, constrained, national).

% Managed households and property, and were expected to produce strong Spartiate offspring. While they held more social freedom than women in other Greek states, they were still subject to the rigid social engineering of the laws, bearing the burden of demographic reproduction within a declining system.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartan_women, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, spartan_women, beneficiary).

% Analyze the historical record of Sparta, including the Lycurgan laws and their long-term consequences, from an external, critical perspective. They are not subject to the constraint but can identify its structural flaws.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, foreign_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__demographic_trap_reading, diffuse).
narrative_ontology:fixing_cost_class(lycurgan_laws__demographic_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a highly disciplined, militarized society focused on collective strength and internal stability, coordinating all aspects of life around military training and civic virtue.
% TRANSFER_FUNCTION: Transferred individual autonomy and economic flexibility from Spartiate citizens to the state, in exchange for a rigid social order and military supremacy. It also transferred labor and resources from the helot population to the Spartiate class.
% ABSENT_VOICES: Disenfranchised Perioikoi (free non-citizens), rebellious Helots, and future generations of Spartiates who would face the demographic trap. Their perspectives, if heard, would highlight the system's unsustainability and inherent injustices.
% DISAPPEARANCE_RATIONALE: The Lycurgan laws were the foundational structure of Spartan society. Their disappearance would lead to a complete collapse of the social, economic, and military order, necessitating a total reorganization of land ownership, citizenship, and governance.
% FOUNDING_PROBLEM: To create a stable, militarily invincible society capable of controlling a large enslaved population and resisting external threats, by eliminating luxury, fostering civic virtue, and ensuring equality among Spartiates.
% FOUNDING_PROBLEM_CORROBORATION: Ancient historians (e.g., Thucydides, Xenophon, Plutarch) and modern historical analysis corroborate the initial goals of the Lycurgan system. However, the demographic decline of the Spartiate population, documented by these same sources and later scholarship, attests that the system ultimately failed to sustain its core population, rendering its founding problem 'dead' in terms of long-term success.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.85) is high because the system, through its immutable laws, extracted the future and demographic sustainability of the Spartiate citizens. Suppression (0.92) is extremely high due to the absolute nature of the laws, the severe penalties for deviation, and the lack of alternatives for Spartiates. Theater ratio (0.15) is low because the laws were genuinely enforced and functional, albeit with destructive long-term consequences. Accessibility collapse (0.95) is near total, as Spartiates had no viable exit from the system. Resistance (0.1) was minimal due to the pervasive suppression and ideological indoctrination. The metrics show a gradual increase in extractiveness and suppression over time, reflecting the tightening grip of the unrevisable system as its flaws became more pronounced.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Lycurgan system's architects (if they could observe its long-term effects), it might have been claimed as a Rope or even a Mountain, a natural order for a superior society. However, from the perspective of the Spartiate citizens experiencing demographic decline, or from a modern analytical observer, the same structure operates as a Snare, extracting their very existence through its unrevisable design. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spartiate elite and Ephors initially benefited from the power and stability the laws provided, placing them closer to the beneficiary end. However, as the system's flaws led to demographic collapse, they too became trapped by its rigidity, bearing the costs of its unsustainability. Spartiate citizens and Helots were clear targets, bearing the direct costs of rigid social control, economic stagnation, and forced labor, leading to high directionality. Spartan women occupied a complex position, benefiting from certain social freedoms but also constrained by the system's reproductive demands.
 *
 * MANDATROPHY ANALYSIS:
 *   The Lycurgan laws exemplify mandatrophy: the original mandate to create a stable, militarily superior society became obsolete as the system's unrevisability prevented adaptation to changing circumstances (e.g., war losses, economic shifts). The persistence of the laws, despite their destructive demographic impact, demonstrates a system whose function had atrophied into a self-destructive trap. The 'dead' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, signals this mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unrevisability_as_design_flaw,
    'Was the demographic collapse an inherent and unavoidable consequence of the Lycurgan laws'' unrevisable design, or primarily due to external factors (e.g., prolonged warfare, natural disasters)?',
    'Comparative historical analysis of other rigid constitutional systems and their demographic trajectories, controlling for external shocks. Counterfactual historical modeling.',
    'If inherent, it strengthens the Snare classification by highlighting the internal, structural mechanism of extraction. If primarily external, it might shift the classification towards a Piton (atrophied function exacerbated by external pressures) or a Tangled Rope (coordination failing under stress).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unrevisability_as_design_flaw, empirical, 'Whether the system''s unrevisability was the primary cause of its demographic failure.').

omega_variable(
    coordination_extraction_boundary,
    'To what extent was the ''equality'' and ''stability'' promised by the Lycurgan system a genuine coordination function, versus a cover for the extraction of individual autonomy and demographic viability?',
    'Analysis of the lived experience of Spartiate citizens over time, particularly the decline in their numbers, against the stated ideals of the laws. Examination of the enforcement mechanisms'' primary targets.',
    'If the coordination function was largely performative or became secondary to extraction, it reinforces the Snare classification. If a genuine, sustained coordination benefit can be identified for the majority of Spartiates, it might suggest a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Distinguishing genuine coordination from extractive cover in the Lycurgan system.').

omega_variable(
    kernel_reading_divergence,
    'How do the ''sacral_fidelity_reading'' and ''adaptive_fiction_reading'' of the Lycurgan laws structurally differ from this ''demographic_trap_reading''?',
    'Direct comparison of the core axioms, beneficiary/victim declarations, and claimed types across the three constraint stories for the ''lycurgan_laws'' kernel.',
    'This omega documents the distinct structural claims of each reading, demonstrating how different interpretations of the same kernel yield distinct constraints with different classifications and implications for historical analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Documents the structural differences between the ''demographic_trap_reading'' and its sibling readings of the Lycurgan laws kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 280).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lycu_tr_t50, lycurgan_laws__demographic_trap_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__demographic_trap_reading, theater_ratio, 100, 0.13).
narrative_ontology:measurement(lycu_tr_t150, lycurgan_laws__demographic_trap_reading, theater_ratio, 150, 0.14).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__demographic_trap_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement(lycu_tr_t280, lycurgan_laws__demographic_trap_reading, theater_ratio, 280, 0.15).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(lycu_be_t50, lycurgan_laws__demographic_trap_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__demographic_trap_reading, base_extractiveness, 100, 0.75).
narrative_ontology:measurement(lycu_be_t150, lycurgan_laws__demographic_trap_reading, base_extractiveness, 150, 0.8).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__demographic_trap_reading, base_extractiveness, 200, 0.83).
narrative_ontology:measurement(lycu_be_t280, lycurgan_laws__demographic_trap_reading, base_extractiveness, 280, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(lycu_su_t50, lycurgan_laws__demographic_trap_reading, suppression_requirement, 50, 0.85).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__demographic_trap_reading, suppression_requirement, 100, 0.88).
narrative_ontology:measurement(lycu_su_t150, lycurgan_laws__demographic_trap_reading, suppression_requirement, 150, 0.9).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__demographic_trap_reading, suppression_requirement, 200, 0.91).
narrative_ontology:measurement(lycu_su_t280, lycurgan_laws__demographic_trap_reading, suppression_requirement, 280, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'lycurgan_laws' kernel, each with its own structural properties and classification. This 'demographic_trap_reading' focuses on the system's unrevisability leading to population decline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
