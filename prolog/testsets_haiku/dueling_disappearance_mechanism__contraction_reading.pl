% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity-Culture Displacement of Honor-Culture Axioms (Contraction Reading)
 *   domain: cultural_anthropology/legal_history/sociological
 *
 * SUMMARY:
 *   This constraint tells the story of dueling's cultural disappearance as a
 *   contraction of the honor-culture axiom system by dignity-culture axioms.
 *   The contraction reading treats dignity as an emerging, irreversible moral
 *   substrate that systematically delegitimizes the entire framework on which
 *   honor culture rests—that reputation is something that can be lost and
 *   must be violently defended, that personhood is relational and
 *   status-dependent, that combat is a legitimate dispute-resolution venue.
 *   Under the contraction reading, dueling becomes culturally unthinkable not
 *   because institutions displaced it (institutional reading) or because
 *   multiple independent causes converged (overdetermined reading), but
 *   because the axioms of human personhood shifted from relational/earned to
 *   intrinsic/inalienable. The constraint is claimed as a mountain because
 *   dignity culture operates as an irreversible cognitive/moral substrate
 *   shift—once the axioms change, the old framework becomes unintelligible,
 *   much like how the shift from geocentric to heliocentric cosmology made
 *   the old frame unthinkable. The extracted victims are those whose identity
 *   was constituted in honor-culture terms; they experience the shift as a
 *   kind of epistemic death. The beneficiaries are those whose interests
 *   align with dignity-culture institutions: bourgeois civil society,
 *   powerless persons whose personhood was denied under honor culture, and
 *   judicial institutions. This reading is distinct from the
 *   institutional-displacement reading (which emphasizes courts and law
 *   outcompeting dueling as a dispute mechanism) and the overdetermined
 *   reading (which treats dueling's decline as multiply caused). It is the
 *   metaphysical/axiomatically-grounded reading.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: landed gentry and military officers whose identity is constituted through honor-defense; experience dignity shift as existential threat
 *   - bourgeois_civil_society: merchant and professional classes whose economic interests align with law-based standing and institutional order
 *   - women_and_enslaved_persons: structurally excluded from honor-culture self-defense; gain conceptual opening under dignity but lack institutional protection
 *   - judicial_institutions: become the authorized venue for reputation management under dignity axioms; gain cultural backing from the shift
 *   - enlightenment_philosophers: articulate dignity axioms; occupy the analytical seat from which the shift is visible
 *   - enslaved_and_colonized_populations: excluded from both frameworks; their absence underscores that the dignity shift is partial and predicated on ongoing exclusion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.62).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.71).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.19).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Displacement of Honor-Culture Axioms (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "cultural_anthropology/legal_history/sociological").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, 'be72fbf5-460f-4758-ae89-c08dea65f9da').
narrative_ontology:cs_kernel_codification('be72fbf5-460f-4758-ae89-c08dea65f9da', distributed).
narrative_ontology:cs_authority_grounding('be72fbf5-460f-4758-ae89-c08dea65f9da', lineage).
narrative_ontology:cs_interpretation_layer_present('be72fbf5-460f-4758-ae89-c08dea65f9da').
narrative_ontology:cs_reading_relation('be72fbf5-460f-4758-ae89-c08dea65f9da', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_reading_relation('be72fbf5-460f-4758-ae89-c08dea65f9da', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('be72fbf5-460f-4758-ae89-c08dea65f9da', foundational, human_dignity_intrinsic_inalienable).
narrative_ontology:cs_axiom_status(human_dignity_intrinsic_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('be72fbf5-460f-4758-ae89-c08dea65f9da', human_dignity_intrinsic_inalienable, deontological).
narrative_ontology:cs_axiom('be72fbf5-460f-4758-ae89-c08dea65f9da', foundational, personhood_constituted_by_law_not_combat).
narrative_ontology:cs_axiom_status(personhood_constituted_by_law_not_combat, holdable).
narrative_ontology:cs_axiom_grounding('be72fbf5-460f-4758-ae89-c08dea65f9da', personhood_constituted_by_law_not_combat, instrumental).
narrative_ontology:cs_reference_frame('be72fbf5-460f-4758-ae89-c08dea65f9da', honor_culture_personhood_framework).
narrative_ontology:cs_drift_state('be72fbf5-460f-4758-ae89-c08dea65f9da', post_enlightenment_dignity_consolidation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('be72fbf5-460f-4758-ae89-c08dea65f9da', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, bourgeois_civil_society).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, women_and_enslaved_persons).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, judicial_institutions).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, landed_gentry).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, military_officer_class).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

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
 *   Extractiveness rises from 0.38 (1750, honor culture still dominant) to 0.62 (1900, dignity axioms consolidated) because the dignity framework systematically invalidates honor-culture claims without replacing them institutionally at first—practitioners lose legitimacy for their defense mechanism before alternative dispute resolution is fully available. Suppression rises sharply (0.15 → 0.71) because enforcing dignity axioms against honor-culture practitioners requires active criminalization of dueling, social shaming, and institutional pressure—the framework does not emerge naturally but must be actively defended against the old axioms. Theater ratio rises slowly (0.05 → 0.28) because dueling becomes a performance of illegality rather than legitimate dispute resolution; the ratio stabilizes at end-of-interval because the residual dueling is purely theatrical (isolated incidents among old-regime holdouts). Accessibility collapse is very high (0.89) because once dignity axioms are internalized, honor-culture logic becomes not just wrong but unintelligible—the alternatives (combat-based standing) are not available as live options, they are incoherent. Resistance is low (0.19) because the shift is axiomatically foundational: those socialized into dignity culture do not experience resistance to it; those still operating on honor-culture axioms are increasingly isolated and lack institutional support. The measurement series shares one time grid (every metric at every time point) to avoid false type transitions from misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the honor-culture practitioner's seat, the constraint is experienced as ontological collapse: the entire framework that made personhood intelligible vanishes, leaving their claims to standing unheard and their identity incoherent. From the dignity-culture beneficiary's seat, the constraint appears as progressive moral enlightenment—the recognition of intrinsic human worth. From the judicial institution's seat, it is a shift in the source of legitimacy for their authority: from enforcement of honor codes to enforcement of dignity and rights. The engine computes these divergent types from the structural data: payer/identity-locked seats will show extraction; beneficiary/arbitrage seats will show subsidy; observer seats will show a type shift.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-culture practitioners are identity-locked victims (d near 1.0): their exit is blocked not by external barriers but by identity fusion—to exit honor culture is to cease to be intelligible as a person. Bourgeois beneficiaries have arbitrage-quality exit (d near 0.0): they can operate comfortably in either framework and benefit from the shift without being bound to it. Women and enslaved persons are powerless and trapped, but they are beneficiaries rather than payers because the dignity shift opens conceptual space for their personhood, even though institutional barriers persist. Judicial institutions have institutional power and are clearly agenda-setters (they enforce the new axioms). Landed gentry are dual-positioned: they are targets of the axiom shift (lose honor-culture legitimacy) but partially benefit from legal-institutional security that dignity culture provides. The military officer class is purely identity-locked payer. No directionality overrides needed; the derivation chain produces the correct structure from beneficiary/victim + exit declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (personal reputation in the absence of institutional courts) is definitively dead by 1900: modern courts, libel law, and legal standing systems fully adjudicate reputation claims. The constraint persists not because the founding problem is still live but because dignity axioms have become so deeply embedded that they appear natural and inevitable. The theater ratio rising (0.05 → 0.28) indicates that residual dueling is increasingly performative—a gesture toward a delegitimized framework—rather than functional dispute resolution. The suppression requirement rising (0.15 → 0.71) shows that the axiom shift requires active enforcement; without criminalization and social pressure, honor-culture logic would persist among pockets of practitioners. This is the classic mandatrophy signature: a constraint whose founding function is dead but whose axiomatically-grounded structure persists and must be actively maintained. The machinery of justice enforces dignity axioms not because they solve the original problem (courts solve it institutionally) but because they are now the legitimacy foundation of the entire civil order.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    axiom_shift_vs_institutional_competition,
    'Was the dignity-axiom shift the NECESSARY cause of dueling''s disappearance, or merely one sufficient cause among several? Could institutional competition (courts, law, banking) have displaced dueling without the axiom shift?',
    'Historical counterfactual analysis: compare societies that adopted dignity axioms without strong institutional alternatives (would dueling persist?) and societies that built strong institutions without axiom shifts (would dueling persist?). Comparative anthropology of honor-culture societies with and without institutional disruption.',
    'If axiom shift is necessary, the constraint is a mountain (irreversible substrate shift). If institutions alone suffice, it is a tangled rope (institutional displacement with some axiomatically binding elements). The classification hinges on the causal direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_shift_vs_institutional_competition, conceptual, 'Whether dignity axioms were the necessary or merely sufficient cause of dueling''s cultural disappearance.').

omega_variable(
    identity_locked_exit_mechanisms,
    'Is the identity-lock of honor-culture practitioners structural (the axioms literally make exit unintelligible) or internalized (practitioners could exit if they chose, but the identity cost is prohibitive)?',
    'Post-transition analysis: did practitioners who formally abandoned honor culture report that the axioms became incomprehensible, or that they painfully rejected a framework they still understood? Longitudinal interviews with descendants of dueling families; examination of diaries and letters from practitioners during the transition.',
    'If identity-lock is structural (axiomatically grounded), suppression is a side effect of axiom shift, not an enforcement mechanism. If internalized, the constraint requires more active suppression than the structural reading suggests. The distinction affects whether the constraint is a mountain (axiom-driven) or a snare (enforced against intelligible alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanisms, empirical, 'Whether honor-culture practitioners'' inability to exit is axiomatically structural or internalized/psychological.').

omega_variable(
    women_and_enslaved_benefit_realization,
    'Did women and enslaved persons actually benefit from dignity-axiom shifts during the 1750–1900 interval, or was the benefit purely rhetorical and institutional realization lagged by generations?',
    'Legal history: track when dignity axioms were institutionally extended to women (suffrage, property rights, bodily autonomy) and colonized persons (abolition, citizenship). Identify the lag between axiom shift and institutional protection.',
    'If benefit was realized during the interval, women and enslaved persons are genuine beneficiaries. If purely rhetorical with no institutional change, they are better characterized as excluded (trapped under both frameworks, with dignity axioms opening future possibility but providing no current escape). The characterization affects whether the constraint''s beneficiary structure is inclusive or exclusive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(women_and_enslaved_benefit_realization, empirical, 'Whether dignity-axiom shifts produced measurable institutional benefits for powerless groups during the transition or remained rhetorical for generations.').

omega_variable(
    supremacy_of_kernel_reading_distinction,
    'Is this contraction reading genuinely axiomatically distinct from the institutional-displacement reading, or is the distinction merely rhetorical—do the two readings actually predict different outcomes when tested against new data?',
    'Comparative study of societies undergoing institutional modernization without dignity-axiom shifts (China''s legal system modernization, Islamic law reform, customary law jurisdictions with strong courts but retained honor culture). If institutional courts displace dueling WITHOUT dignity axioms shifting, the readings are empirically distinct. If honor culture persists even with institutional courts, the distinction is real.',
    'If empirically distinct, the reading is structurally valid and maps to different constraint types (mountain vs. tangled rope). If the readings make identical predictions, the distinction is rhetorical scaffolding, not substantive, and the constraint may be better characterized as overdetermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supremacy_of_kernel_reading_distinction, empirical, 'Whether the contraction reading is axiomatically distinct from institutional-displacement reading or merely alternate framing of the same causal structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(duel_tr_t1830, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1830, 0.14).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1860, 0.22).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1880, 0.27).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.28).

% Extraction over time
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1750, 0.38).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1800, 0.48).
narrative_ontology:measurement(duel_be_t1830, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1830, 0.55).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1860, 0.6).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1880, 0.62).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1750, 0.15).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1800, 0.28).
narrative_ontology:measurement(duel_su_t1830, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1830, 0.42).
narrative_ontology:measurement(duel_su_t1860, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1860, 0.58).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1880, 0.69).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__contraction_reading, 0.12).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, honor_culture_institutionalization).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dignity_axioms_codification).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, state_monopoly_on_violence_claim).

% DUAL FORMULATION NOTE:
% This constraint is ONE reading of dueling_disappearance_mechanism kernel. The contraction reading emphasizes axiom-shift (dignity axioms displacing honor axioms) as the foundational cause. The institutional_displacement_reading emphasizes institutional competition (courts, law, banking displacing dueling). The overdetermined_composite_reading treats decline as multiply caused. All three readings are structurally valid and describe the same historical phenomenon; they differ in what mechanism they identify as necessary vs. sufficient. This reading's ε-invariant formulation treats dignity-axiom displacement as the core extractiveness mechanism; the engine should compute different classifications for payer vs. beneficiary seats. The sibling readings have different beneficiary/victim structures and will compute different directionalities. Each reading is its own constraint with its own ε, not three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
