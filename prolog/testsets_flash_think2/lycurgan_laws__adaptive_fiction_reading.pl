% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__adaptive_fiction_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Immutability (Adaptive Fiction Reading)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This constraint story analyzes the Lycurgan laws of Sparta through the
 *   lens of an 'adaptive fiction' reading. It posits that the proclaimed
 *   immutability of these laws was a 'noble lie' (a rhetorical device)
 *   maintained by the Spartan elite (ephors, kings, gerousia) to ensure
 *   social and political stability. While publicly presented as unchangeable,
 *   the laws were covertly adapted through interpretation and flexible
 *   enforcement by the ruling class, allowing the system to persist despite
 *   changing circumstances. The claimed type is 'mountain' reflecting the
 *   rhetoric, but the metrics reveal a highly performative and extractive
 *   operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.55).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.75).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Immutability (Adaptive Fiction Reading)").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).
domain_priors:emerges_naturally(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '9032a849-abe0-40e9-b32d-44700b2c37c6').
narrative_ontology:cs_kernel_codification('9032a849-abe0-40e9-b32d-44700b2c37c6', fixed_text).
narrative_ontology:cs_authority_grounding('9032a849-abe0-40e9-b32d-44700b2c37c6', lineage).
narrative_ontology:cs_interpretation_layer_present('9032a849-abe0-40e9-b32d-44700b2c37c6').
narrative_ontology:cs_reading_relation('9032a849-abe0-40e9-b32d-44700b2c37c6', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9032a849-abe0-40e9-b32d-44700b2c37c6', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('9032a849-abe0-40e9-b32d-44700b2c37c6', foundational, immutability_as_rhetorical_device).
narrative_ontology:cs_axiom_status(immutability_as_rhetorical_device, holdable).
narrative_ontology:cs_axiom_grounding('9032a849-abe0-40e9-b32d-44700b2c37c6', immutability_as_rhetorical_device, conventional).
narrative_ontology:cs_axiom('9032a849-abe0-40e9-b32d-44700b2c37c6', foundational, elite_interpretive_flexibility_is_real).
narrative_ontology:cs_axiom_status(elite_interpretive_flexibility_is_real, holdable).
narrative_ontology:cs_axiom_grounding('9032a849-abe0-40e9-b32d-44700b2c37c6', elite_interpretive_flexibility_is_real, empirically_contingent).
narrative_ontology:cs_reference_frame('9032a849-abe0-40e9-b32d-44700b2c37c6', lycurgan_original_design).
narrative_ontology:cs_drift_state('9032a849-abe0-40e9-b32d-44700b2c37c6', classical_spartan_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9032a849-abe0-40e9-b32d-44700b2c37c6', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_ephors).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_kings).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_gerousia).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartan_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The chief magistrates of Sparta, responsible for administering the Lycurgan laws. They covertly adapted the laws through interpretation and enforcement, maintaining the 'noble lie' of immutability while ensuring the system's practical functionality and their own authority.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_ephors, agenda_setter,
    institutional, generational, constrained, national).

% The dual monarchs of Sparta, who benefited from the stability and legitimacy provided by the Lycurgan system. They participated in the covert adaptation of laws, particularly in military and foreign policy, without openly challenging the doctrine of immutability.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_kings, beneficiary,
    institutional, generational, constrained, national).

% The council of elders, who served as a legislative and judicial body. They benefited from the established order and contributed to the interpretive flexibility of the laws, ensuring the system could respond to changing circumstances without formal amendment.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_gerousia, beneficiary,
    institutional, generational, constrained, national).

% The male citizens of Sparta, who lived under the rigid public facade of the Lycurgan laws. They bore the costs of limited open political participation and the suppression of dissent, but benefited from the perceived stability and military strength of the state.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_citizens, payer,
    powerless, biographical, trapped, national).

% Ancient and modern political theorists (e.g., Aristotle, Plutarch) who analyzed the Spartan constitution, often noting the discrepancy between its claimed immutability and its practical operation or eventual decline.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, external_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, unified legal and social framework for Sparta, preventing open factionalism and ensuring military focus by presenting a divinely sanctioned, immutable code.
% TRANSFER_FUNCTION: Transfers political agency and the right to open legal revision from the general citizenry to a small elite (ephors, kings, gerousia) in exchange for perceived stability and divine legitimacy, while allowing the elite to adapt the system covertly.
% ABSENT_VOICES: Any Spartan citizen advocating for open constitutional reform or transparent legal adaptation would be suppressed. External political theorists who critiqued Spartan rigidity were not part of the internal discourse.
% DISAPPEARANCE_RATIONALE: The entire Spartan social and political order was predicated on the Lycurgan laws and the belief in their immutability. Their sudden disappearance would lead to immediate collapse of the state, internal conflict, and likely external conquest, as the foundational commitment system would vanish.
% FOUNDING_PROBLEM: To establish a stable, militarily powerful, and egalitarian (among citizens) society in Sparta, overcoming internal strife, economic inequality, and external threats through a comprehensive, divinely inspired legal code.
% FOUNDING_PROBLEM_CORROBORATION: Spartan elite narratives (e.g., Plutarch's 'Life of Lycurgus') attest to the founding problem and its ongoing relevance. External historians and political philosophers (e.g., Thucydides, Aristotle) corroborate the initial problems but critique the long-term efficacy and rigidity of the solution, suggesting the problem shifted or was only partially solved, and the system's persistence relied on covert adaptation rather than inherent perfection.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__adaptive_fiction_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high 'theater_ratio' (0.80 at interval end) reflects the significant gap between the public claim of immutability and the reality of covert adaptation. 'Extractiveness' (0.55) is substantial, as the elite extracted political agency and the right to open revision from citizens. 'Suppression' (0.75) is high, necessary to maintain the 'noble lie' and prevent open challenges to the system. 'Accessibility_collapse' is moderate-high due to the powerful rhetoric of divine sanction, but not absolute due to the elite's internal flexibility. 'Resistance' is low, as the system's stability and covert adaptation reduced the pressure for overt opposition. The temporal measurements show a gradual increase in extractiveness, suppression, and theatricality as the system aged, suggesting the 'noble lie' required more effort to maintain over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Spartan elite, the system was a necessary and effective means of maintaining order and power, with their covert adaptations being pragmatic governance. From the perspective of the citizens, it was a rigid, divinely sanctioned system that demanded absolute obedience. The engine's classification will highlight the divergence between the claimed 'mountain' and the actual extractive, theatrical operation.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spartan elite (ephors, kings, gerousia) are the primary beneficiaries, as they maintain power and stability through the system, and their covert adaptation allows them to navigate challenges. Spartan citizens are the primary targets, bearing the costs of limited political participation and the rigid public facade. External observers are analytical, neither benefiting nor paying directly.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the Lycurgan system a genuine natural law (as claimed by its proponents) or a constructed constraint that benefits identifiable agents?',
    'Analysis of historical evidence for divine origin versus human design and political utility; examination of who benefits from the ''naturalness'' claim.',
    'If genuinely natural, its classification as a mountain would be robust. If constructed, its ''mountain'' claim is a false summit, reclassifying it to a more extractive type (e.g., tangled_rope) for the beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Ambiguity between claimed natural law and constructed constraint.').

omega_variable(
    noble_lie_efficacy_and_belief,
    'To what extent was the ''noble lie'' of immutability genuinely believed by the Spartan citizenry, versus merely accepted as a necessary political fiction?',
    'Archaeological evidence of public dissent, analysis of non-elite historical accounts (if available), and comparative studies of similar commitment systems.',
    'If widely believed, the constraint''s suppression and accessibility collapse would be more internalized. If merely accepted, the external enforcement mechanisms would be more prominent, potentially increasing the effective suppression score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(noble_lie_efficacy_and_belief, empirical, 'The degree of belief in the Lycurgan ''noble lie''.').

omega_variable(
    covert_adaptation_limits_and_impact,
    'What were the actual limits of the elite''s covert adaptation, and did it genuinely prevent systemic failure or merely delay it?',
    'Detailed historical analysis of specific instances of adaptation, their outcomes, and the long-term trajectory of Spartan society, particularly in relation to demographic and economic challenges.',
    'If adaptation was highly effective, the constraint''s extractiveness might be lower, as it provided genuine, albeit hidden, coordination. If adaptation was insufficient, the system''s eventual decline would be more directly attributable to its inherent rigidity, despite the elite''s efforts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_adaptation_limits_and_impact, empirical, 'Effectiveness and limits of covert adaptation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(lycu_tr_t20, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 20, 0.66).
narrative_ontology:measurement(lycu_tr_t40, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 40, 0.71).
narrative_ontology:measurement(lycu_tr_t60, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 60, 0.75).
narrative_ontology:measurement(lycu_tr_t80, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 80, 0.78).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 100, 0.8).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(lycu_be_t20, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(lycu_be_t40, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(lycu_be_t60, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 60, 0.51).
narrative_ontology:measurement(lycu_be_t80, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 80, 0.53).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 100, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lycu_su_t20, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(lycu_su_t40, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(lycu_su_t60, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(lycu_su_t80, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 80, 0.74).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, spartan_military_discipline).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, spartan_social_hierarchy).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'adaptive_fiction_reading' of the 'lycurgan_laws' kernel. It focuses on the elite's covert adaptation to maintain stability despite rhetorical immutability. Sibling readings explore the sacral aspect and the demographic consequences of rigidity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
