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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Overdetermined Decline of Dueling (Composite Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint describes the historical process of dueling's decline,
 *   understood as a causally overdetermined phenomenon. It is a reading of
 *   the 'dueling_disappearance_mechanism' kernel, focusing on the
 *   simultaneous action of multiple independent sufficient conditions (legal
 *   prohibition, institutional modernization, cultural shift, Civil War
 *   trauma). The constraint itself is the *system of these interacting
 *   forces* that collectively led to the practice's near-total disappearance.
 *   The claimed type is Tangled Rope, reflecting that this composite system
 *   both coordinated a new social order and extracted the social license for
 *   dueling.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.65).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.75).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Overdetermined Decline of Dueling (Composite Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, '8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c').
narrative_ontology:cs_kernel_codification('8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c', distributed).
narrative_ontology:cs_authority_grounding('8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c', extraction).
narrative_ontology:cs_interpretation_layer_present('8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c').
narrative_ontology:cs_reading_relation('8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c', foundational, causal_pluralism_in_historical_change).
narrative_ontology:cs_axiom_status(causal_pluralism_in_historical_change, holdable).
narrative_ontology:cs_axiom_grounding('8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c', causal_pluralism_in_historical_change, empirically_contingent).
narrative_ontology:cs_axiom('8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c', foundational, overdetermination_as_explanatory_principle).
narrative_ontology:cs_axiom_status(overdetermination_as_explanatory_principle, holdable).
narrative_ontology:cs_axiom_grounding('8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c', overdetermination_as_explanatory_principle, empirically_contingent).
narrative_ontology:cs_reference_frame('8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c', multi_causal_historical_analysis).
narrative_ontology:cs_drift_state('8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c', contemporary_historical_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8defe44c-1f96-40a4-9e2f-c5e0de3dbc1c', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, modern_legal_system).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, civil_society).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, non_dueling_elites).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, traditional_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively enforced legal prohibitions against dueling, gradually asserting the state's monopoly on violence. Benefited from increased social order and legitimacy.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, modern_legal_system, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefited from the reduction in violence and the establishment of more predictable, less honor-driven social interactions. Supported the cultural shift away from dueling.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, civil_society, beneficiary,
    organized, generational, mobile, national).

% Members of the elite who did not rely on dueling for status or dispute resolution, and who benefited from a more stable social environment. Their influence grew as dueling declined.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, non_dueling_elites, beneficiary,
    powerful, biographical, mobile, regional).

% Individuals whose social standing and self-conception were deeply tied to the honor culture and the practice of dueling. They lost a fundamental mechanism for defending their reputation and faced social ostracism or legal penalties for attempting to duel.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_adherents, payer,
    powerless, biographical, trapped, local).

% Segments of the elite whose power and social rituals were historically intertwined with dueling. They experienced a decline in their traditional means of asserting authority and faced pressure to conform to new social norms.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, traditional_elites, payer,
    powerful, biographical, constrained, regional).

% Analyze the complex interplay of factors that led to dueling's decline, seeking to understand the causal mechanisms and their societal impact without direct participation in the historical events.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The various legal, institutional, and cultural forces collectively coordinated to dismantle the social legitimacy and practical viability of dueling, establishing new, state-sanctioned norms for conflict resolution.
% TRANSFER_FUNCTION: Transferred the authority for dispute resolution and the maintenance of personal honor from individual combatants and their social circles to state legal systems and evolving civic norms. This involved a transfer of social capital and the monopoly on legitimate violence.
% ABSENT_VOICES: Those who continued to uphold the honor code as a moral imperative, or who viewed dueling as a necessary aristocratic privilege, were increasingly marginalized by legal enforcement and cultural condemnation, effectively excluded from the evolving social discourse.
% DISAPPEARANCE_RATIONALE: The overdetermined decline of dueling fundamentally reshaped elite social interactions, the role of the state in regulating violence, and the very concept of honor, leading to a significant reordering of social and legal institutions.
% FOUNDING_PROBLEM: Dueling was a pervasive and often fatal practice among elites that challenged state authority, caused significant social disruption, and was seen as an archaic impediment to modern civil order.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, sociologists, and cultural anthropologists widely corroborate that dueling was a significant social problem that undermined state authority and civil order, and that the specific practice of dueling is no longer a live problem in most Western societies. This is attested by numerous academic works and historical records from outside the direct beneficiaries of the decline.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) and suppression (0.75) are high because the composite effect of legal, cultural, and institutional pressures effectively dismantled a deeply ingrained social practice, imposing new norms and penalties. Accessibility collapse is very high (0.85) as alternatives to dueling (e.g., courts, libel law) became dominant and the social space for dueling vanished. Resistance (0.40) was moderate, as dueling persisted in some pockets and forms for decades, but ultimately failed to reverse the trend. Theater ratio is low (0.10) because the mechanisms were genuinely effective in their stated and unstated goals, with little performative maintenance of a defunct system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the modernizing state and civil society, the decline of dueling was a necessary and beneficial evolution towards a more rational and less violent society. From the perspective of honor culture adherents, it was a loss of a vital mechanism for maintaining personal dignity and social order, a form of cultural extraction. The 'overdetermined composite' reading acknowledges these multiple perspectives by identifying diverse beneficiaries and victims of the overall process.
 *
 * DIRECTIONALITY LOGIC:
 *   The modern legal system and civil society were primary beneficiaries, gaining a monopoly on violence and a more stable social order. Non-dueling elites also benefited from the shift in status mechanisms. Honor culture adherents and traditional elites were the primary targets, losing a central aspect of their social identity and power. The composite constraint extracted the social and legal legitimacy of dueling from these groups.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_pathway_separability,
    'To what extent are the individual causal pathways (legal, cultural, institutional, traumatic) truly non-separable in their contribution to dueling''s decline, or could their individual contributions be quantitatively disentangled?',
    'Counterfactual historical analysis or comparative historical sociology across different national contexts where some factors were absent or weaker.',
    'If separable, the ''overdetermined composite'' reading''s claim of non-separability would be weakened, potentially leading to a re-evaluation of the constraint as a collection of linked, individually measurable constraints rather than a single composite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_pathway_separability, empirical, 'Ambiguity regarding the analytical separability of the multiple causal factors in dueling''s decline.').

omega_variable(
    victim_identification_ambiguity,
    'Given the composite nature of the constraint, which specific causal mechanism (legal, cultural, institutional, trauma) was most responsible for the ''extraction'' experienced by the identified victims, and does this shift the victim set?',
    'Detailed micro-historical studies focusing on individual experiences of dueling''s decline and their direct causal antecedents.',
    'If one mechanism disproportionately caused extraction, the victim set might be refined to reflect those most affected by that specific force, potentially altering the directionality and classification for certain groups.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_identification_ambiguity, conceptual, 'Uncertainty in attributing extraction to specific causal pathways within the overdetermined composite.').

omega_variable(
    kernel_framing_underdetermination,
    'Does framing dueling''s decline as an ''overdetermined composite'' accurately capture the primary structural dynamics, or would a focus on ''cultural contraction'' or ''institutional displacement'' provide a more salient classification?',
    'Comparative analysis of the explanatory power and predictive utility of each reading across different historical contexts and theoretical frameworks.',
    'If an alternative framing (e.g., ''contraction_reading'') were adopted, the constraint''s claimed type, metrics, and stakeholder dynamics would likely shift to reflect that specific causal emphasis, potentially reclassifying it as a different type (e.g., a Rope for cultural coordination or a Snare for institutional capture).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Under-determination of the most appropriate analytical framing for dueling''s decline, given multiple coherent readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 1800, 1870).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1800, 0.25).
narrative_ontology:measurement(duel_tr_t1810, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1810, 0.2).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1820, 0.15).
narrative_ontology:measurement(duel_tr_t1830, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1830, 0.12).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1860, 0.07).
narrative_ontology:measurement(duel_tr_t1870, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1870, 0.1).

% Extraction over time
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1800, 0.45).
narrative_ontology:measurement(duel_be_t1810, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1810, 0.5).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1820, 0.55).
narrative_ontology:measurement(duel_be_t1830, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1830, 0.6).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1840, 0.63).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1850, 0.65).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1860, 0.66).
narrative_ontology:measurement(duel_be_t1870, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1870, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1800, 0.5).
narrative_ontology:measurement(duel_su_t1810, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1810, 0.58).
narrative_ontology:measurement(duel_su_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1820, 0.65).
narrative_ontology:measurement(duel_su_t1830, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1830, 0.7).
narrative_ontology:measurement(duel_su_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1840, 0.73).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1850, 0.75).
narrative_ontology:measurement(duel_su_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1860, 0.76).
narrative_ontology:measurement(duel_su_t1870, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1870, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dueling_disappearance_mechanism' kernel, focusing on the overdetermined composite causality. The other readings emphasize cultural contraction or institutional displacement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
