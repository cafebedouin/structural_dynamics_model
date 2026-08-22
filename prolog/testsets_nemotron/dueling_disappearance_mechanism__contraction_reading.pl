% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dueling Disappearance via Dignity-Culture Contraction (Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the contraction_reading of the
 *   dueling_disappearance_mechanism kernel. It claims that dueling became
 *   culturally unthinkable through the internal expansion of dignity culture
 *   — a moral framework treating all persons as bearers of intrinsic worth —
 *   which axiomatically displaced the honor-culture logic that made dueling
 *   intelligible as a status-maintenance practice. The constraint operates as
 *   a mountain: dignity culture, once established, makes the honor-culture
 *   subject position structurally impossible, not merely disadvantageous. The
 *   measurement series tracks the transition from honor-culture dominance
 *   (1750, high extractiveness, high theater) to dignity-culture saturation
 *   (1900, low extractiveness, low theater, near-total accessibility
 *   collapse). Resistance remains low throughout because the constraint works
 *   by making the alternative cognitively unavailable, not by coercing
 *   compliance. Beneficiaries include dignity_culture_practitioners (who gain
 *   moral intelligibility) and modern_legal_subjects (who gain universal
 *   rights frameworks). Victims include honor_culture_practitioners (whose
 *   framework becomes illegible) and dueling_specialists (whose expertise is
 *   devalued to zero).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.12).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.88).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dueling Disappearance via Dignity-Culture Contraction (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '7ea03092-56ab-4cbe-aec2-4f1ebddaedea').
narrative_ontology:cs_kernel_codification('7ea03092-56ab-4cbe-aec2-4f1ebddaedea', distributed).
narrative_ontology:cs_authority_grounding('7ea03092-56ab-4cbe-aec2-4f1ebddaedea', practice).
narrative_ontology:cs_interpretation_layer_present('7ea03092-56ab-4cbe-aec2-4f1ebddaedea').
narrative_ontology:cs_reading_relation('7ea03092-56ab-4cbe-aec2-4f1ebddaedea', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ea03092-56ab-4cbe-aec2-4f1ebddaedea', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('7ea03092-56ab-4cbe-aec2-4f1ebddaedea', foundational, dignity_culture_axiomatic_expansion_irreversible).
narrative_ontology:cs_axiom_status(dignity_culture_axiomatic_expansion_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('7ea03092-56ab-4cbe-aec2-4f1ebddaedea', dignity_culture_axiomatic_expansion_irreversible, deontological).
narrative_ontology:cs_axiom('7ea03092-56ab-4cbe-aec2-4f1ebddaedea', foundational, honor_culture_subject_position_structurally_illegible_under_dignity).
narrative_ontology:cs_axiom_status(honor_culture_subject_position_structurally_illegible_under_dignity, holdable).
narrative_ontology:cs_axiom_grounding('7ea03092-56ab-4cbe-aec2-4f1ebddaedea', honor_culture_subject_position_structurally_illegible_under_dignity, conventional).
narrative_ontology:cs_reference_frame('7ea03092-56ab-4cbe-aec2-4f1ebddaedea', honor_culture_dominance).
narrative_ontology:cs_drift_state('7ea03092-56ab-4cbe-aec2-4f1ebddaedea', dignity_culture_saturation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('7ea03092-56ab-4cbe-aec2-4f1ebddaedea', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, modern_legal_subjects).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, dueling_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adherents of the emerging dignity culture (Enlightenment philosophers, religious reformers, legal theorists, abolitionists) who gain moral intelligibility and political standing from the universalist axioms. They can move between dignity-culture frameworks (Kantian, Christian, republican) without losing the core substrate. Their exit is mobile because the dignity-culture framework is expansive and pluralistic within its universalist constraints.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners, beneficiary,
    organized, generational, mobile, continental).

% The emergent class of citizens bearing universal rights under state law. They gain legal standing, property protection, and dispute resolution without personal violence. Their exit is arbitrage-grade: they can invoke multiple legal frameworks (national, international, human rights) and the constraint (dignity culture as legal substrate) only expands their option set.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, modern_legal_subjects, beneficiary,
    institutional, civilizational, arbitrage, global).

% Aristocrats, military officers, Southern planters, urban elites for whom honor is the organizing principle of selfhood. They lose the ability to make their status claims intelligible — a duel challenge becomes not a status defense but a crime; an insult becomes not an honor wound but a legal tort. Their exit is identity_locked: honor culture constitutes their self-concept; conversion to dignity culture dissolves their identity. They cannot 'choose' to exit without ceasing to be who they are.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer).

% Seconds, fencing masters, pistol makers, dueling-code jurists — specialists whose expertise and livelihood depend entirely on the honor-culture framework. As dueling becomes culturally unthinkable, their skills have zero market value and no transferable application. They are trapped: no alternative coordination mechanism employs their specific capital, and their identity is fused to a practice that has become illegible.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dueling_specialists, payer,
    powerless, immediate, trapped, local).

% Courts, legislatures, police, military academies that progressively prohibit dueling while building substitute institutions (criminal law, libel law, officer corps honor codes). They set the agenda by criminalizing the old coordination mechanism and legitimating the new one. Their exit is arbitrage-grade: they control the institutional architecture and can modify it.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Analysts who observe the transition from honor to dignity culture as a structural transformation of moral frameworks. They neither collect from nor pay into the constraint; they map its operation across the kernel's readings.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honor culture coordinated status claims and dispute resolution in contexts where state monopoly on violence was incomplete or distrusted. Dueling provided a ritualized, bounded mechanism for equal-status men to defend reputation without feuding.
% TRANSFER_FUNCTION: The contraction transfers moral intelligibility and legal standing from honor-culture practitioners (who lose the capacity to make their status claims recognized) to dignity-culture practitioners and modern legal subjects (who gain universal recognition). It transfers the coordination function from personal violence to impersonal law.
% ABSENT_VOICES: Enslaved persons, women, non-property-holding men, indigenous peoples — all excluded from both honor culture (which was gendered, racialized, and propertied) and early dignity culture (which universalized in principle but excluded in practice). They would object to both frameworks but were not in the conversation when either operated.
% DISAPPEARANCE_RATIONALE: If the dignity-culture contraction vanished overnight, the honor-culture subject position would not automatically return — the material conditions (state monopoly on violence, market society, universal education) that made dignity culture dominant would remain. But the moral substrate would collapse, reopening the space for honor-logic revivals (as seen in gang cultures, military subcultures, mafia codes). The world rearranges because the constraint is the substrate itself.
% FOUNDING_PROBLEM: How to coordinate status claims and resolve disputes among equals without a trusted third-party enforcer — the honor-culture solution was ritualized violence (dueling) bounded by codes.
% FOUNDING_PROBLEM_CORROBORATION: Weber, Elias, and Möller (outside the beneficiary set) attest that the founding problem (coordination without state monopoly) was substantially solved by state institutions by 1850. The contraction_reading's claim that dignity culture axiomatically displaced honor culture is corroborated by Taylor (Sources of the Self) and Appiah (The Honor Code) from analytical seats, not by dignity-culture beneficiaries.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness drops from 0.45 to 0.12 as the constraint shifts from active honor-culture enforcement (theater-heavy, coordination on status defense) to dignity-culture substrate (minimal extraction, self-maintaining). Suppression requirement rises from 0.35 to 0.88 because dignity culture's triumph requires the active suppression of honor-culture alternatives — legal prohibition of dueling, cultural marginalization of honor logic, educational formation in universalist axioms. Theater ratio falls from 0.65 to 0.15 as performative honor displays (duels, challenges, seconds) are replaced by the quiet, self-evident operation of dignity norms. Accessibility collapse reaches 0.92 by 1900: no coherent honor-culture subject position remains available within the dignity-culture framework. Resistance stays near zero (0.08) because the constraint operates by restructuring the space of the thinkable, not by defeating opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the dignity-culture seat, the constraint appears as a mountain of moral progress — the natural unfolding of universal human dignity. From the honor-culture seat, it appears as a snare of epistemic violence — the systematic erasure of a coherent lifeworld. The engine computes this divergence from the structural data (beneficiary/victim declarations, exit_options, identity_locked status). The contraction_reading holds that the mountain classification is correct from the analytical seat because the constraint's motor is the internal logic of dignity culture, not the extraction from honor culture.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (dignity_culture_practitioners, modern_legal_subjects) sit at d≈0.15: they gain moral intelligibility, legal standing, and universal rights without bearing the transition costs. Victims (honor_culture_practitioners, dueling_specialists) sit at d≈0.85: their entire status-economy is rendered illegible, their expertise worthless, their subject position cognitively unavailable. The exit options for victims are identity_locked — honor culture constitutes their self-concept; conversion to dignity culture is not a choice but a dissolution of identity. This is why the constraint functions as a mountain despite identifiable victims: the extraction is not the point; the cognitive restructuring is.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination of status claims without state monopoly on violence) was live in 1750 and dead by 1850. The arrangement (dueling as honor-culture coordination mechanism) persisted theatrically for decades after its founding problem died — the mandate atrophied. However, this constraint story is about the mechanism that MADE the arrangement disappear (dignity-culture contraction), not about the arrangement itself. The mandatrophy is resolved: the honor-culture coordination mechanism is gone, replaced by a different substrate. The contraction_reading claims the replacement substrate is irreversible (mountain); the institutional_displacement_reading claims it is substitutable (rope/tangled_rope); the composite_reading claims it is overdetermined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_irreversibility,
    'Is dignity culture''s displacement of honor culture a genuine natural-law-like irreversibility, or a constructed constraint that benefits identifiable agents (modern legal subjects, state institutions)?',
    'Counterfactual historical analysis: could honor culture have persisted as a viable subculture under modern conditions without being rendered illegible? If viable alternatives were structurally suppressed, the constraint is constructed; if no coherent honor-culture subject position remains possible, the constraint may approach natural-law status.',
    'If constructed, the constraint reclassifies from mountain to tangled_rope or snare via FSM, and the beneficiary structure (dignity_culture_practitioners, modern_legal_subjects) becomes the engine of classification. If natural-law, mountain holds and beneficiaries are incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_irreversibility, conceptual, 'Whether the dignity-culture substrate is genuinely irreversible or a contingent historical achievement that suppresses alternatives.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three readings of the dueling_disappearance_mechanism kernel disagree structurally?',
    'Map each reading''s causal claims: contraction_reading locates causal power in the internal logic of dignity culture (axiomatic displacement); institutional_displacement_reading locates it in state-provided substitutes (courts, banking, libel law); overdetermined_composite_reading denies a single locus. The disagreement is whether the constraint''s primary motor is cultural-logical (contraction), institutional-functional (displacement), or genuinely multi-causal (composite).',
    'If the disagreement is located in the cultural-logical vs. institutional-functional axis, the constraint family splits along that seam. If the composite reading is correct, all three single-motor readings are false summits. This omega records the committer-frame structure: this constraint is one reading of kernel_id=dueling_disappearance_mechanism, reading_id=contraction_reading, with sibling readings institutional_displacement_reading and overdetermined_composite_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Commitment-frame structure: location of disagreement among kernel readings.').

omega_variable(
    honor_culture_practitioners_victim_status,
    'Are honor-culture practitioners genuine victims of extraction, or did they lose a coordination game they could have exited?',
    'Analyze exit options available to honor-culture practitioners at each measurement point. If exit was structurally available (migration, conversion, sub-cultural persistence) but not taken, they are not victims. If the dignity-culture framework rendered honor-culture subject positions unintelligible — making exit cognitively impossible — they are victims of epistemic suppression.',
    'Victim status triggers snare/tangled_rope gates and affects directionality computation. If honor_culture_practitioners are victims, the constraint has asymmetric extraction despite low base extractiveness. If they are merely outcompeted, the constraint may be a genuine mountain with incidental losers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_culture_practitioners_victim_status, empirical, 'Whether the honor-culture practitioner seat is a victim of extraction or a loser in a coordination transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1750, 0.65).
narrative_ontology:measurement(duel_tr_t1780, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1780, 0.45).
narrative_ontology:measurement(duel_tr_t1810, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1810, 0.3).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1840, 0.2).
narrative_ontology:measurement(duel_tr_t1870, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1870, 0.15).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.15).

% Extraction over time
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1750, 0.45).
narrative_ontology:measurement(duel_be_t1780, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1780, 0.32).
narrative_ontology:measurement(duel_be_t1810, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1810, 0.21).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1840, 0.15).
narrative_ontology:measurement(duel_be_t1870, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1870, 0.12).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1750, 0.35).
narrative_ontology:measurement(duel_su_t1780, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1780, 0.55).
narrative_ontology:measurement(duel_su_t1810, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1810, 0.7).
narrative_ontology:measurement(duel_su_t1840, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1840, 0.82).
narrative_ontology:measurement(duel_su_t1870, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1870, 0.88).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__contraction_reading, 0.08).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the natural-language concept 'dueling disappearance' into three structurally distinct claims per ε-invariance. contraction_reading: ε=0.12 (low, dignity culture as substrate), mountain. institutional_displacement_reading: ε≈0.35 (moderate, institutional substitution as coordination), rope/tangled_rope. composite_reading: ε≈0.25 (distributed), no single type. The ε values differ because the referent of extraction differs: contraction_reading assesses extraction from the dignity-culture substrate's perspective (minimal); institutional_displacement_reading assesses extraction from the state-institution-builder's perspective (moderate, building courts/banking/libel law); composite_reading averages across motors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__contraction_reading, organized, 0.15).
constraint_indexing:directionality_override(dueling_disappearance_mechanism__contraction_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
