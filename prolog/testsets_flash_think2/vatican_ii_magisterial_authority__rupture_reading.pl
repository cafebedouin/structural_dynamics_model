% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Magisterial Authority (Rupture Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rupture reading' of Vatican II,
 *   which asserts that the Council represents a fundamental break with
 *   pre-conciliar Catholic teaching and ecclesiology. From this perspective,
 *   conciliar texts authorize radical implementation, supersede prior
 *   positions (e.g., 'error has no rights'), legitimate liturgical
 *   experimentation, and acknowledge contradictions (e.g., religious freedom
 *   in Dignitatis Humanae) as doctrinal progress. The constraint's operation
 *   involves actively enforcing this new paradigm and marginalizing
 *   traditionalist resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.78).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.85).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Magisterial Authority (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, '249baaf1-743c-452f-a0a4-24c4bed07cb5').
narrative_ontology:cs_kernel_codification('249baaf1-743c-452f-a0a4-24c4bed07cb5', fixed_text).
narrative_ontology:cs_authority_grounding('249baaf1-743c-452f-a0a4-24c4bed07cb5', lineage).
narrative_ontology:cs_interpretation_layer_present('249baaf1-743c-452f-a0a4-24c4bed07cb5').
narrative_ontology:cs_reading_relation('249baaf1-743c-452f-a0a4-24c4bed07cb5', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('249baaf1-743c-452f-a0a4-24c4bed07cb5', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('249baaf1-743c-452f-a0a4-24c4bed07cb5', foundational, religious_freedom_contradicts_prior_teaching).
narrative_ontology:cs_axiom_status(religious_freedom_contradicts_prior_teaching, holdable).
narrative_ontology:cs_axiom_grounding('249baaf1-743c-452f-a0a4-24c4bed07cb5', religious_freedom_contradicts_prior_teaching, deontological).
narrative_ontology:cs_axiom('249baaf1-743c-452f-a0a4-24c4bed07cb5', foundational, aggiornamento_supersedes_traditional_forms).
narrative_ontology:cs_axiom_status(aggiornamento_supersedes_traditional_forms, holdable).
narrative_ontology:cs_axiom_grounding('249baaf1-743c-452f-a0a4-24c4bed07cb5', aggiornamento_supersedes_traditional_forms, conventional).
narrative_ontology:cs_reference_frame('249baaf1-743c-452f-a0a4-24c4bed07cb5', post_conciliar_aggiornamento).
narrative_ontology:cs_drift_state('249baaf1-743c-452f-a0a4-24c4bed07cb5', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('249baaf1-743c-452f-a0a4-24c4bed07cb5', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, modernizing_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, liberal_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_theological_schools).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The official teaching authority of the Catholic Church, which promulgates and interprets the documents of Vatican II. From the rupture reading's perspective, it actively implements and enforces the new ecclesiology, marginalizing dissenting traditionalist views.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, vatican_magisterium, agenda_setter,
    institutional, generational, constrained, global).

% Academics and thinkers who interpret Vatican II as a fundamental break, providing intellectual justification for radical implementation and doctrinal progress. They benefit from the new theological landscape and the opportunities for innovation it presents.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians, beneficiary,
    powerful, biographical, mobile, global).

% Clergy who embrace the rupture reading, implementing liturgical changes, pastoral approaches, and theological interpretations consistent with a new ecclesiology. They benefit from alignment with the perceived direction of the Church and often gain influence.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, modernizing_clergy, beneficiary,
    organized, biographical, constrained, local).

% Lay faithful who find the rupture reading liberating, aligning the Church with modern values and fostering greater engagement with the world. They benefit from a more inclusive and adaptable Church, often finding their own views validated.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, liberal_laity, beneficiary,
    moderate, biographical, mobile, local).

% Clergy who adhere to pre-conciliar teachings and practices, viewing Vatican II as a rupture that undermines tradition. They bear the cost of marginalization, suppression of traditional rites, and often face disciplinary action for non-compliance with the new paradigm. Their identity is deeply tied to the older forms.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy, payer,
    organized, biographical, identity_locked, local).

% Lay faithful who feel alienated by the changes wrought by Vatican II, perceiving a loss of sacred tradition and doctrinal clarity. They pay through loss of preferred liturgical forms, theological confusion, and often feel excluded from mainstream Church life. Their identity is fused with the pre-conciliar Church.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_laity, payer,
    powerless, biographical, identity_locked, local).

% Academic and formation institutions whose theological frameworks are rooted in pre-conciliar thought. They face pressure to conform to the new ecclesiology, losing funding, accreditation, or influence if they resist. Their intellectual tradition is actively superseded.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_theological_schools, payer,
    institutional, generational, trapped, national).

% Scholars who study the historical development of Catholic doctrine and institutions, analyzing the impact of Vatican II from a detached academic perspective. They observe the contestation without being directly subject to its enforcement.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, historical_theologians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To reorient the Catholic Church towards greater engagement with the modern world, fostering ecumenism, religious freedom, and internal renewal, thereby coordinating its mission in a new global context.
% TRANSFER_FUNCTION: Transfers theological authority and interpretive legitimacy from pre-conciliar traditions and practices to a new ecclesiological paradigm, from traditionalist adherents to progressive proponents, and from older liturgical forms to new ones.
% ABSENT_VOICES: Radical traditionalists and sedevacantists who reject the legitimacy of Vatican II entirely are structurally excluded from the internal hermeneutical debate; they would argue for a complete repudiation of the Council and a return to a pre-1962 Church.
% DISAPPEARANCE_RATIONALE: If the rupture reading of Vatican II vanished, the entire post-conciliar Catholic Church's self-understanding, liturgical practice, and relationship with the modern world would be fundamentally destabilized. The theological and institutional landscape would reorganize around either a strict continuity reading or a recognition of irreconcilable internal contradictions.
% FOUNDING_PROBLEM: The perceived irrelevance and isolation of the Catholic Church in a rapidly modernizing world, coupled with a need for internal spiritual and pastoral renewal (aggiornamento).
% FOUNDING_PROBLEM_CORROBORATION: Progressive theological schools and many secular historians of religion corroborate that the Church faced significant challenges in adapting to modernity. However, traditionalist groups and some historical theologians contest whether the 'problem' was correctly identified or whether the 'solution' was appropriate, arguing the Church's perceived isolation was a strength, not a weakness.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the rupture reading demands significant shifts in belief and practice, imposing a new theological framework on those who adhere to prior teachings. Suppression is very high, reflecting the active marginalization, silencing, and disciplinary actions against traditionalist clergy and laity who resist the new paradigm. Theater ratio is moderate, as there is genuine theological and pastoral work, but also performative aspects of 'modernity' and 'aggiornamento' that serve to legitimize the new direction. Accessibility collapse is high for traditionalists, as the 'old way' becomes largely inaccessible within official Church structures. Resistance is high, reflecting ongoing, organized opposition from traditionalist groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries (proponents of the rupture reading), the constraint is a necessary and beneficial coordination mechanism for the Church's renewal. From the perspective of the victims (traditionalists), it is a highly extractive and suppressive force that undermines the very identity of the Church. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Vatican Magisterium, progressive theologians, modernizing clergy, and liberal laity are beneficiaries, as they align with and actively implement the new ecclesiology, gaining influence and validation. Traditionalist clergy, traditionalist laity, and pre-conciliar theological schools are victims, bearing the costs of marginalization, suppression, and the invalidation of their theological frameworks. Their exit options are often identity-locked, as leaving the Church is unthinkable for many, despite their profound disagreement with its current direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (which would ignore the significant extraction and suppression) or a pure Snare (which would ignore the genuine, albeit contested, coordination function of reorienting the Church's mission). The 'founding problem' of Church relevance in the modern world is still 'live' for proponents of the rupture reading, but its 'status' is 'contested' by traditionalists, indicating a potential for mandatrophy if the coordination function is perceived as having fully atrophied for a significant segment of the faithful.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_incompatibility_extent,
    'What is the true extent of doctrinal incompatibility between Vatican II texts (as interpreted by the rupture reading) and prior magisterial teaching?',
    'Comprehensive historical-theological analysis by a neutral, inter-confessional body, or a future magisterial clarification that explicitly reconciles or repudiates specific points of tension.',
    'If incompatibility is proven to be absolute, the rupture reading gains stronger empirical grounding, potentially reclassifying the constraint as a Snare for those forced to accept it. If reconciliation is possible, the rupture reading''s claims of ''doctrinal progress'' through contradiction are weakened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_incompatibility_extent, empirical, 'The degree to which Vatican II''s teachings contradict prior doctrine.').

omega_variable(
    legitimacy_of_radical_implementation,
    'Does the rupture reading''s interpretation of Vatican II genuinely authorize radical implementation and liturgical experimentation, or does it exceed the Council''s actual intent?',
    'Analysis of the Council Fathers'' original intentions, periti notes, and subsequent magisterial interventions that clarify the scope of conciliar reforms.',
    'If the radical implementation is found to exceed intent, the extractiveness and suppression associated with it would be reclassified as illegitimate, potentially shifting the constraint closer to a Snare. If fully authorized, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_radical_implementation, conceptual, 'Whether radical implementation is a legitimate consequence of the rupture reading.').

omega_variable(
    identity_lock_vs_structural_suppression,
    'For traditionalist clergy and laity, what proportion of their ''identity_locked'' exit option is due to internalized identity fusion versus structural barriers imposed by the Magisterium?',
    'Longitudinal studies of ex-traditionalists'' post-exit experiences, or analysis of the impact of ''liberalizing'' policies (e.g., wider access to traditional rites) on retention rates.',
    'If internalized identity fusion is dominant, the effective suppression is higher than structural measures suggest, as the target carries the suppression with them. If structural barriers are dominant, policy changes could more effectively reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for traditionalists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1985, 0.4).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1995, 0.45).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1975, 0.7).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2005, 0.77).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2015, 0.79).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1975, 0.8).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1995, 0.83).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2005, 0.86).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2015, 0.84).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Vatican II magisterial authority' kernel. This 'rupture reading' asserts a fundamental break with prior teaching, in contrast to the 'continuity reading' (organic development) and the 'composite overdetermination reading' (incompatible visions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
