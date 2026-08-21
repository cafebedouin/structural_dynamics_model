% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Overdetermined Decline of Dueling
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint describes the 'overdetermined_composite_reading' of
 *   dueling's decline, positing that its disappearance was not due to a
 *   single cause but to the simultaneous action of multiple independent
 *   sufficient conditions: legal prohibition, institutional modernization
 *   (e.g., courts, banking), cultural shifts (dignity culture displacing
 *   honor culture), and traumatic events (like the American Civil War). The
 *   constraint itself is the cumulative mechanism of this disappearance. It
 *   is claimed as a Tangled Rope because it involved both the coordination of
 *   new social orders and the extraction of the old practice.
 *
 * KEY AGENTS:
 *   - legal_systems: Agenda_setter/Beneficiary (institutional/mobile)
 *   - modern_society: Beneficiary (organized/mobile)
 *   - emerging_institutions: Beneficiary (institutional/arbitrage)
 *   - honor_culture_adherents: Payer/Excluded (powerless/identity_locked)
 *   - duelists: Payer (powerless/trapped)
 *   - cultural_modernizers: Agenda_setter/Beneficiary (moderate/mobile)
 *   - historians_sociologists: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.85).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.9).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Overdetermined Decline of Dueling").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, '1d441f3c-e536-4272-aebd-cd7c01168d11').
narrative_ontology:cs_kernel_codification('1d441f3c-e536-4272-aebd-cd7c01168d11', implicit).
narrative_ontology:cs_authority_grounding('1d441f3c-e536-4272-aebd-cd7c01168d11', distributed).
narrative_ontology:cs_reading_relation('1d441f3c-e536-4272-aebd-cd7c01168d11', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d441f3c-e536-4272-aebd-cd7c01168d11', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('1d441f3c-e536-4272-aebd-cd7c01168d11', foundational, decline_is_multi_causal).
narrative_ontology:cs_axiom_status(decline_is_multi_causal, holdable).
narrative_ontology:cs_axiom_grounding('1d441f3c-e536-4272-aebd-cd7c01168d11', decline_is_multi_causal, empirically_contingent).
narrative_ontology:cs_axiom('1d441f3c-e536-4272-aebd-cd7c01168d11', foundational, no_single_necessary_condition).
narrative_ontology:cs_axiom_status(no_single_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('1d441f3c-e536-4272-aebd-cd7c01168d11', no_single_necessary_condition, empirically_contingent).
narrative_ontology:cs_reference_frame('1d441f3c-e536-4272-aebd-cd7c01168d11', dueling_as_viable_social_practice).
narrative_ontology:cs_drift_state('1d441f3c-e536-4272-aebd-cd7c01168d11', post_civil_war_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('1d441f3c-e536-4272-aebd-cd7c01168d11', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_systems).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, modern_society).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, emerging_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, cultural_modernizers).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively enforced legal prohibitions against dueling, gradually criminalizing the practice and asserting state monopoly on violence. Benefited from increased authority and social order.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_systems, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_systems, beneficiary).

% Benefited from the reduction of interpersonal violence and the establishment of more stable, predictable social norms. Shifted towards valuing dignity over honor in public discourse.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, modern_society, beneficiary,
    organized, generational, mobile, national).

% Institutions like courts, banks, and libel law provided alternative, less violent, and more effective mechanisms for dispute resolution and reputation defense, displacing dueling's function.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, emerging_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Lost a central ritual for defending personal honor and social standing. Faced increasing legal penalties and social ostracization for attempting to uphold the dueling code. Their identity was deeply tied to the practice.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_adherents, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_adherents, excluded).

% Individuals who, by tradition or personal conviction, felt compelled to duel to defend their honor. Faced severe legal consequences, social stigma, and the ultimate risk of death, with diminishing social support for their actions.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, duelists, payer,
    powerless, immediate, trapped, local).

% Advocated for new cultural norms that emphasized dignity, self-control, and institutional justice over the violent defense of honor. Their efforts contributed to the cultural shift away from dueling.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, cultural_modernizers, agenda_setter,
    moderate, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, cultural_modernizers, beneficiary).

% Analyze the complex interplay of factors that led to dueling's decline, seeking to understand the causal pathways and their relative contributions without direct participation in the historical events.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, historians_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Multiple independent social, legal, and cultural forces converged to coordinate society around non-violent, institutionalized forms of dispute resolution and honor maintenance, effectively replacing dueling as a legitimate social practice.
% TRANSFER_FUNCTION: Transferred the right and responsibility to adjudicate honor and resolve disputes from individuals (and their seconds) to formal legal and social institutions. It also transferred the social cost of violence from individuals to the state (via enforcement and incarceration).
% ABSENT_VOICES: Those who clung to the honor code and saw dueling as a necessary, if dangerous, means of preserving personal reputation were increasingly marginalized and criminalized. Their voices were actively suppressed by legal and cultural shifts.
% DISAPPEARANCE_RATIONALE: If the mechanisms that led to dueling's decline had not occurred, the social fabric around honor, law, and violence would be fundamentally different. The rise of modern legal systems and a different social contract were deeply intertwined with the shift away from dueling.
% FOUNDING_PROBLEM: The problem of unchecked individual violence and the perceived necessity of dueling for honor defense in a pre-modern social order, where formal legal recourse for personal affronts was often inadequate or unavailable.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and cultural anthropologists corroborate that dueling was a significant social problem and that its decline was a major societal shift, supported by extensive historical records, legal statutes, and cultural analyses from the period. The problem it solved is no longer relevant in most modern societies.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the comprehensive and irreversible removal of dueling as a social practice. Suppression (0.90) is also high, driven by legal enforcement, social stigma, and the lack of viable alternatives for honor defense. The low theater ratio (0.10) indicates that the decline was a genuine structural shift, not merely performative. Accessibility collapse is very high (0.95) as the social and legal space for dueling effectively vanished. Resistance (0.40) was moderate, reflecting the gradual nature of the decline and pockets of adherence to honor culture.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of legal systems and modern society, the decline of dueling was a beneficial coordination towards a more orderly society. From the perspective of honor culture adherents, it was a coercive extraction that undermined their social identity and means of defending reputation. The 'overdetermined' nature of the decline means that no single agent or mechanism can be solely credited or blamed, making the victim set diffuse across the various causal pathways.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal systems, modern society, emerging institutions, and cultural modernizers are beneficiaries, as they gained from the shift away from dueling. Honor culture adherents and duelists are victims, as they lost a central means of social expression and faced severe penalties. The multiple, converging mechanisms meant that the 'extraction' was diffuse but powerful, targeting the practice itself and those who upheld it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_pathway_separability,
    'Given the claim of overdetermination, how can the ''extraction'' of dueling be meaningfully attributed to specific causal pathways, or is it only measurable as a composite effect?',
    'Counterfactual historical analysis or agent-based modeling that attempts to isolate the impact of individual causal factors. If such isolation is impossible without fundamentally altering the historical context, the composite reading is strengthened.',
    'If individual pathways can be separated and measured, it might suggest decomposing this composite constraint into multiple, linked constraints, each with its own ε. If not, the composite ε remains the most appropriate measure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_pathway_separability, conceptual, 'Ambiguity in measuring extraction due to non-separable causal pathways in an overdetermined historical event.').

omega_variable(
    victim_set_attribution,
    'Which specific causal mechanism (legal, institutional, cultural, traumatic) was most responsible for the ''victimization'' of honor culture adherents and duelists, and how does this affect the definition of the victim set?',
    'Detailed historical case studies focusing on specific regions or social groups where one causal factor demonstrably dominated the others in the decline of dueling. This would clarify the primary mechanism of extraction for those specific victims.',
    'If one mechanism consistently dominates, the victim set might be more narrowly defined, and the constraint might lean more towards a Snare (if purely extractive) or a different type. If the impact remains diffuse, the current Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_attribution, empirical, 'Ambiguity in attributing victimization across multiple, overdetermined causal mechanisms.').

omega_variable(
    constraint_definition_ambiguity,
    'Is ''dueling''s decline'' a coherent ''constraint'' in itself, or is it merely the aggregate outcome of multiple distinct constraints (legal prohibitions, new institutions, cultural norms) acting in concert?',
    'Conceptual analysis of the ''constraint'' as a system-level property versus a collection of individual rules. If the emergent property of ''overdetermined decline'' has its own causal efficacy beyond the sum of its parts, it functions as a single constraint. Otherwise, it should be decomposed.',
    'If decomposed, this story would be replaced by a network of individual constraints (e.g., ''anti_dueling_laws'', ''dignity_culture_norms''), each with its own classification. If it holds as a single constraint, the current classification is valid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constraint_definition_ambiguity, conceptual, 'Conceptual challenge of defining a ''constraint'' as an overdetermined disappearance mechanism versus a collection of individual constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1820, 0.12).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1860, 0.08).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1880, 0.09).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1800, 0.6).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1820, 0.68).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1840, 0.75).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1860, 0.8).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1880, 0.83).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1900, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1800, 0.65).
narrative_ontology:measurement(duel_su_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1820, 0.72).
narrative_ontology:measurement(duel_su_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1840, 0.78).
narrative_ontology:measurement(duel_su_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1860, 0.84).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1880, 0.88).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1900, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
