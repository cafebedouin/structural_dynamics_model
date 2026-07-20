% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__originalist_limitation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Magna Carta Clause 39 â Originalist Limitation Reading (1215 Context)
 *   domain: constitutional/legal/historical
 *
 * SUMMARY:
 *   Clause 39 of the 1215 Magna Carta, in the originalist limitation reading,
 *   is a negotiated constraint that binds King John's royal prerogative only
 *   to the specific abuses documented in the 1215 baronial grievances. It is
 *   not a universal due-process guarantee but a feudal peace treaty with a
 *   defined beneficiary set (the baronial class) and a defined victim (the
 *   crown's arbitrary power). The constraint coordinates the immediate civil
 *   war but extracts from royal prerogative asymmetrically.
 *
 * KEY AGENTS:
 *   - baronial_leadership: Primary agenda-setter (powerful/constrained) â negotiated the charter and enforces it through committees of barons.
 *   - english_crown: Primary payer (institutional/constrained) â bears the loss of arbitrary imprisonment and dispossession prerogatives.
 *   - baronial_class: Beneficiary (powerful/constrained) â receives protection against documented royal abuses.
 *   - papal_authority: Observer (institutional/analytical) â annuls the charter on grounds of duress, standing outside the English feudal bargain.
 *   - free_men_non_baronial: Excluded (moderate/trapped) â lack protection under the originalist reading's narrow ambit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.38).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.55).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Magna Carta Clause 39 â Originalist Limitation Reading (1215 Context)").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional/legal/historical").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, '69ed38cd-dee3-418a-a79f-5758781d013a').
narrative_ontology:cs_kernel_codification('69ed38cd-dee3-418a-a79f-5758781d013a', fixed_text).
narrative_ontology:cs_authority_grounding('69ed38cd-dee3-418a-a79f-5758781d013a', lineage).
narrative_ontology:cs_reading_relation('69ed38cd-dee3-418a-a79f-5758781d013a', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_reading_relation('69ed38cd-dee3-418a-a79f-5758781d013a', magna_carta_clause_39__liberal_due_process_reading, forecloses).
narrative_ontology:cs_axiom('69ed38cd-dee3-418a-a79f-5758781d013a', foundational, clause_39_bounded_by_1215_grievances).
narrative_ontology:cs_axiom_status(clause_39_bounded_by_1215_grievances, holdable).
narrative_ontology:cs_axiom_grounding('69ed38cd-dee3-418a-a79f-5758781d013a', clause_39_bounded_by_1215_grievances, empirically_contingent).
narrative_ontology:cs_axiom('69ed38cd-dee3-418a-a79f-5758781d013a', foundational, protection_limited_to_baronial_negotiators).
narrative_ontology:cs_axiom_status(protection_limited_to_baronial_negotiators, holdable).
narrative_ontology:cs_axiom_grounding('69ed38cd-dee3-418a-a79f-5758781d013a', protection_limited_to_baronial_negotiators, empirically_contingent).
narrative_ontology:cs_reference_frame('69ed38cd-dee3-418a-a79f-5758781d013a', feudal_constitution_of_1215).
narrative_ontology:cs_drift_state('69ed38cd-dee3-418a-a79f-5758781d013a', common_law_interpretation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('69ed38cd-dee3-418a-a79f-5758781d013a', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, baronial_leadership).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, baronial_class).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, english_crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated the 1215 charter at Runnymede and enforced its terms through committees of barons and the threat of renewed military action. Theirsituation is one of feudal landlords securing written guarantees against arbitrary royal dispossession and imprisonment, limited to the grievances they documented against King John.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, baronial_leadership, agenda_setter,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__originalist_limitation_reading, baronial_leadership, beneficiary).

% Bears the constraint on royal prerogative to imprison, dispossess, or outlaw without judgment by peers or the law of the land. The crown's arbitrary power is limited to the specific documented abuses that provoked the 1215 rebellion, and it actively resisted through papal annulment and subsequent renegotiation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, english_crown, payer,
    institutional, generational, constrained, national).

% Receives procedural protection against arbitrary royal acts, but only insofar as those protections map onto the specific grievances raised in 1215. The benefit is not generalized to all free men and remains tied to the feudal bargaining position of the class.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, baronial_class, beneficiary,
    powerful, generational, constrained, national).

% Pope Innocent III annulled the 1215 charter as extorted from the king under duress, operating from a canonical framework that denied legitimacy to oaths taken by force. Observes from outside the English feudal bargain and does not participate in its enforcement.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, papal_authority, observer,
    institutional, generational, analytical, continental).

% Free men below baronial rank who would benefit from broader due-process protections but are excluded from the constraint's ambit under the originalist reading, which binds only the documented grievances of the baronial negotiators. They remain fully exposed to royal prerogative.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, free_men_non_baronial, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__originalist_limitation_reading, baronial_class).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the immediate civil war between King John and the rebel barons by codifying specific, documented limits on royal prerogative, restoring feudal order through written guarantee rather than sustained force of arms.
% TRANSFER_FUNCTION: Moves immunity from arbitrary imprisonment and dispossession out of the unchecked royal prerogative and into the hands of the baronial negotiating class, strictly bounded to the grievances documented in the 1215 context.
% ABSENT_VOICES: Non-baronial free men, villeins, and women are absent from the 1215 negotiation; they would claim broader procedural protection but are structurally excluded by the originalist reading's limitation to documented baronial grievances. Papal authority also objects from a canonical legitimacy framework but is external to the English arrangement.
% DISAPPEARANCE_RATIONALE: If the specific limitation on documented royal abuses vanished overnight in 1215, the immediate feudal settlement collapses; the barons lose their negotiated safeguards and the crown reasserts the arbitrary prerogative that produced the First Barons' War, rearranging the English political order.
% FOUNDING_PROBLEM: King John's arbitrary exercise of royal prerogative to imprison, dispossess, and outlaw barons without judgment by peers or the law of the land, violating established feudal custom and provoking baronial rebellion.
% FOUNDING_PROBLEM_CORROBORATION: Chroniclers Roger of Wendover and the Barnwell annalist attest the specific abuses from outside the baronial beneficiary set. Papal registers recording Innocent III's annulment corroborate the coercive origin from a non-beneficiary seat.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.38, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).
:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint genuinely redistributes power from crown to barons but is strictly bounded to documented 1215 grievances; it does not generalize. Suppression (0.55) reflects the active baronial enforcement required to hold John to the bargain, including the threat of renewed war. Theater is low-moderate (0.25): the charter's text is functional in 1215 but grows symbolic performative weight as it is reissued and cited. Accessibility collapse (0.50) is moderate because alternatives (royal absolutism) remain structurally available though politically costly. Resistance (0.70) is high because the crown actively sought papal annulment and repeatedly tested the constraint's limits.
 *
 * PERSPECTIVAL GAP:
 *   From the baronial seats, the constraint is a restored ancient custom and necessary brake on tyranny; from the crown's seat, it is an extorted diminution of legitimate feudal prerogative. The engine will compute divergent per-seat classifications: baronial seats see a rope-like coordination mechanism with moderate extraction, while the crown seat sees a tangled rope or snare-like extraction. The originalist reading insists on the narrow scope that produces this asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Baronial leadership and baronial class are declared beneficiaries, producing low directionality (subsidy/protection). The english_crown is the declared victim, producing high directionality (target). The free_men_non_baronial are excluded: they are neither beneficiaries nor victims under this reading, reflecting the originalist boundedness. Papal authority is an analytical observer with no directional stake in the English feudal arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading prevents mandatrophy mislabeling by bounding the constraint to a live founding problem (King John's specific abuses) and a narrow beneficiary set. If the constraint were claimed as a universal mountain of law, the absence of a sunset clause and the presence of identifiable baronial beneficiaries would trigger false-summit detection. By claiming tangled_rope and documenting the active enforcement, the story records the coordination-extraction hybrid accurately. The founding problem is corroborated by non-beneficiary chroniclers, satisfying the R5 provenance requirement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_vs_universal_scope,
    'Does Clause 39''s protection extend only to the specific baronial grievances documented in 1215, or does it establish a broader principle applicable to all subjects?',
    'Historical philology of the 1215 text and records of the baronial negotiations; comparison with subsequent reissues (1225, 1297) that generalized language.',
    'If broader, the beneficiary set expands and the constraint migrates toward liberal_due_process_reading; if strictly bounded, it remains a narrow feudal extraction limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_vs_universal_scope, conceptual, 'Scope ambiguity between originalist limitation and universalist readings.').

omega_variable(
    enforcement_duress_legitimacy,
    'Does the duress under which King John sealed the charter invalidate the constraint as a coordination mechanism, or does negotiated settlement under threat of war remain a legitimate coordination type?',
    'Canonical legal-historical assessment of medieval treaty legitimacy; whether subsequent reissue under Henry III cured the duress defect.',
    'If duress invalidates coordination, the constraint is pure extraction (snare-like); if duress is a standard feudal bargaining mode, it retains tangled_rope status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_duress_legitimacy, conceptual, 'Legitimacy of constraint founded in coercive negotiation.').

omega_variable(
    documented_abuse_exhaustiveness,
    'Are the royal abuses documented in the 1215 context exhaustive of the constraint''s reach, or do they merely exemplify a broader category?',
    'Close reading of the Articles of the Barons and the 1215 charter against the Pipe Rolls and chronicle evidence of specific grievances.',
    'If exhaustive, the victim set is limited to the crown''s loss of specific documented prerogatives; if exemplary, the constraint generalizes beyond originalist bounds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(documented_abuse_exhaustiveness, empirical, 'Whether 1215 grievances exhaust clause scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcc39olr_tr_t0, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mcc39olr_tr_t5, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(mcc39olr_tr_t10, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(mcc39olr_tr_t15, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(mcc39olr_tr_t20, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(mcc39olr_be_t0, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mcc39olr_be_t5, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mcc39olr_be_t10, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(mcc39olr_be_t15, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(mcc39olr_be_t20, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(mcc39olr_su_t0, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(mcc39olr_su_t5, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(mcc39olr_su_t10, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(mcc39olr_su_t15, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(mcc39olr_su_t20, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, feudal_prerogative_reading).

% DUAL FORMULATION NOTE:
% This constraint is the originalist reading of the Magna Carta Clause 39 kernel, decomposed from the liberal due process and feudal prerogative readings per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
