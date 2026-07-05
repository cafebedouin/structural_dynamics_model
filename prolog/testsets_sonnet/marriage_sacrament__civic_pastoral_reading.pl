% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Relationship: Discernment-Based Reading of Indissolubility
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This story instantiates the civic/pastoral reading of the marriage
 *   sacrament kernel: marriage is a relationship subject to human failure,
 *   and indissolubility functions as a normative ideal to be approached
 *   through compassionate, case-by-case discernment rather than enforced as
 *   an exceptionless bright line adjudicated solely by canonical tribunal.
 *   This is one of two structurally distinct readings of the same kernel —
 *   the sibling, hierarchical_indissolubility_reading, treats marriage as an
 *   ontological reality requiring hierarchical adjudication where
 *   indissolubility is constitutive rather than aspirational. The two
 *   readings are not the same constraint measured differently; they have
 *   different beneficiary/victim structures, different enforcement profiles,
 *   and different ε trajectories, and are authored as separate stories per
 *   the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - divorced_and_remarried_catholics: primary beneficiary (powerless/constrained) — regain sacramental access via discernment
 *   - pastoral_ministers: agenda_setter (moderate/constrained) — administer discernment case-by-case
 *   - reform_oriented_bishops: agenda_setter (institutional/mobile) — set diocesan-level policy
 *   - traditionalist_catholics: primary payer (moderate/identity_locked) — experience doctrinal erosion
 *   - annulment_seeking_first_spouses: payer (powerless/trapped) — bypassed by informal process
 *   - canon_law_purists: payer (organized/constrained) — professional authority diminished
 *   - vatican_doctrinal_congregation: observer (institutional/analytical) — monitors doctrinal consistency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.42).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.38).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Relationship: Discernment-Based Reading of Indissolubility").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, 'c15773fc-e64f-4c85-b137-4fc187cdc49f').
narrative_ontology:cs_kernel_codification('c15773fc-e64f-4c85-b137-4fc187cdc49f', fixed_text).
narrative_ontology:cs_authority_grounding('c15773fc-e64f-4c85-b137-4fc187cdc49f', lineage).
narrative_ontology:cs_interpretation_layer_present('c15773fc-e64f-4c85-b137-4fc187cdc49f').
narrative_ontology:cs_reading_relation('c15773fc-e64f-4c85-b137-4fc187cdc49f', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('c15773fc-e64f-4c85-b137-4fc187cdc49f', foundational, indissolubility_as_regulative_ideal).
narrative_ontology:cs_axiom_status(indissolubility_as_regulative_ideal, holdable).
narrative_ontology:cs_axiom_grounding('c15773fc-e64f-4c85-b137-4fc187cdc49f', indissolubility_as_regulative_ideal, instrumental).
narrative_ontology:cs_axiom('c15773fc-e64f-4c85-b137-4fc187cdc49f', foundational, primacy_of_individual_conscience_in_sacramental_admission).
narrative_ontology:cs_axiom_status(primacy_of_individual_conscience_in_sacramental_admission, holdable).
narrative_ontology:cs_axiom_grounding('c15773fc-e64f-4c85-b137-4fc187cdc49f', primacy_of_individual_conscience_in_sacramental_admission, deontological).
narrative_ontology:cs_reference_frame('c15773fc-e64f-4c85-b137-4fc187cdc49f', tridentine_juridical_indissolubility).
narrative_ontology:cs_drift_state('c15773fc-e64f-4c85-b137-4fc187cdc49f', post_amoris_laetitia_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c15773fc-e64f-4c85-b137-4fc187cdc49f', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, divorced_and_remarried_catholics).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_ministers).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, diocesan_tribunals_offering_internal_forum).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, reform_oriented_bishops).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditionalist_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, annulment_seeking_first_spouses).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, canon_law_purists).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, primacy_of_conscience_doctrine).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, pastoral_accompaniment_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have remarried after a civil divorce, sometimes without a formal annulment, and wish to remain in full communion with the Church including reception of the sacraments. Under this reading, a pastoral process of accompaniment and internal-forum discernment can lead a priest or bishop to admit them to communion case-by-case, without a tribunal declaring the first marriage null. This restores their access to sacramental life but leaves their canonical status ambiguous and diocese-dependent.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, divorced_and_remarried_catholics, beneficiary,
    powerless, biographical, constrained, national).

% Parish priests and diocesan pastoral staff who apply discernment criteria (drawn from documents like Amoris Laetitia) to individual cases. They gain real pastoral flexibility and relief from mechanically enforcing exclusion, but they also absorb responsibility for judgment calls that used to be handled by a tribunal, and face inconsistent guidance from their own bishops.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_ministers, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, pastoral_ministers, beneficiary).

% Diocesan bishops who authorize footnote-based discernment pathways and internal-forum solutions in their jurisdictions. They set local policy on how much pastoral latitude is extended, gaining influence over doctrinal application without needing a change to universal canon law, and can calibrate the practice to local pastoral needs.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, reform_oriented_bishops, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, reform_oriented_bishops, beneficiary).

% Catholics whose religious and personal identity is built around indissolubility as an unconditional, exceptionless teaching. They experience the discernment framework as doctrinal erosion in practice even where the language of the ideal is formally retained. They cannot simply 'exit' the discomfort without abandoning the Church itself, since their identity is fused with the institution's teaching authority; many remain and resist internally, through petitions, dubia, and appeals to Rome.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditionalist_catholics, payer,
    moderate, generational, identity_locked, global).

% The abandoned or first spouse in a marriage where the other party has remarried and been pastorally admitted to communion without a tribunal ruling on the validity of the first marriage. They experience the informal internal-forum pathway as bypassing due process that would otherwise formally examine whether their marriage was valid, potentially leaving their own canonical and emotional situation unresolved or unacknowledged.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, annulment_seeking_first_spouses, payer,
    powerless, biographical, trapped, national).

% Canon lawyers, tribunal officials, and curial officials whose professional function is the formal adjudication of marriage validity. Diocese-level informal discernment pathways route around their expertise and jurisdiction, reducing the practical authority and consistency of the tribunal system they administer and depend on for institutional relevance.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, canon_law_purists, payer,
    organized, generational, constrained, global).

% Monitors whether diocesan-level pastoral discernment practices remain consistent with universal doctrine, occasionally issuing clarifications (e.g. responses to dubia) that push back on divergent local applications without fully resolving the underlying tension.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, vatican_doctrinal_congregation, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__civic_pastoral_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_sacrament__civic_pastoral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a pastoral mechanism for reintegrating divorced-and-remarried Catholics into full sacramental life without requiring every case to pass through a lengthy, sometimes inaccessible, formal tribunal annulment process — coordinating mercy and accompaniment with continued institutional membership.
% TRANSFER_FUNCTION: Moves normative clarity and institutional consistency away from traditionalist Catholics and canon law tribunals, and moves sacramental access and pastoral relief toward divorced-and-remarried Catholics; also moves interpretive authority from centralized canon law adjudication toward local bishops and parish-level pastoral judgment.
% ABSENT_VOICES: Abandoned first spouses are rarely centered in discernment conversations focused on the remarried party's pastoral needs; their interest in a clear, adjudicated resolution of the first marriage's validity is structurally sidelined by an internal-forum process that does not require a tribunal finding.
% DISAPPEARANCE_RATIONALE: Reform-oriented pastoral ministers and divorced-and-remarried Catholics would say the world rearranges badly — sacramental access would revert to strict tribunal-gated exclusion, reproducing real pastoral harm. Traditionalist Catholics and canon law purists would say the world would improve — doctrinal clarity and consistent enforcement would return, and disputed diocese-by-diocese variation would end. The disagreement is exactly the kernel contest itself, not a side issue.
% FOUNDING_PROBLEM: Rigid formal annulment procedures were producing widespread de facto exclusion from the sacraments for divorced-and-remarried Catholics, especially where annulment access was slow, expensive, or practically unavailable, driving many away from active practice entirely.
% FOUNDING_PROBLEM_CORROBORATION: Pastoral sociologists studying Mass attendance and sacramental participation rates (a source outside both the beneficiary clergy and the traditionalist critics) corroborate that formal annulment inaccessibility was a real driver of disengagement prior to this reading's adoption. Canon law scholars critical of the reading corroborate that the underlying tribunal-access problem was real but argue the discernment solution transfers the cost rather than solving it — from excluded remarried Catholics to doctrinal clarity and abandoned first spouses.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, contested).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).
:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate (0.42) and rising over the interval: the reading itself is not designed to extract, but its uneven, diocese-by-diocese application produces a real transfer of normative clarity and institutional consistency away from those whose identity depends on doctrinal stability, and away from the professional authority of canon law tribunals. Suppression is moderate (0.38) — there is no coercive machinery forcing traditionalist Catholics to accept the reading, but institutional pressure (episcopal authorization, changed pastoral norms, altered common practice) constrains their ability to insist on the prior enforcement regime within the same institution. Theater ratio is modest but rising (0.28) as 'discernment' language sometimes substitutes for substantive doctrinal resolution — cases are pastorally managed rather than canonically settled, which can function as institutional avoidance dressed as mercy.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of divorced-and-remarried Catholics and reform-oriented pastoral ministers, this reading is coordination: it solves a genuine pastoral access problem that rigid tribunal gating was failing to solve. From the seat of traditionalist Catholics and canon law purists, the identical structure operates as extraction — of doctrinal clarity, of institutional consistency, of the settled meaning that their religious identity depends on. The engine should compute divergent seat-level types from this same structural data; that divergence is real, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (divorced-and-remarried Catholics, pastoral ministers, reform bishops) sit toward the low-d end: the reading subsidizes their access and authority. Victims (traditionalist Catholics, canon law purists, annulment-seeking first spouses) sit toward the high-d end: they bear the cost of relativized doctrinal certainty, diminished tribunal authority, and bypassed formal adjudication respectively. Traditionalist Catholics are marked identity_locked rather than merely constrained because their objection is not merely economic or logistical — their religious identity is constituted through commitment to the teaching's unconditional status, so exit from the discomfort would require exit from the institution itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inaccessible or overly rigid annulment procedures driving genuine pastoral exclusion — is genuinely contested as live vs. dead: it remains partly live (tribunal backlogs and costs persist in many dioceses) even as the informal discernment pathway has become an alternate track running alongside rather than replacing tribunal reform. This is not classic mandatrophy (a dead problem with a persisting arrangement) but a case of parallel-track proliferation: the old mechanism (tribunals) was not dismantled, a new mechanism (discernment) was added alongside it, and the coexistence itself generates the moderate extraction measured here through inconsistency rather than through either mechanism failing outright.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_marriage_sacrament,
    'Is the civic_pastoral_reading a legitimate development of doctrine within a single coherent framework, or does it structurally foreclose the hierarchical_indissolubility_reading by treating a constitutive claim as merely aspirational?',
    'Magisterial clarification (e.g. a definitive doctrinal statement from the highest teaching authority) resolving whether discernment-based internal-forum admission is compatible with, or contradicts, the ontological reading of indissolubility. Absent such clarification, the two readings persist as coexisting positions held by different factions within the same institution.',
    'If the readings genuinely foreclose one another, only one can be the Church''s actual operative doctrine and the other is a deviation requiring correction. If they coexist, the Church tolerates two structurally incompatible operative practices simultaneously, which is itself a distinct governance phenomenon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_marriage_sacrament, conceptual, 'Whether the civic/pastoral and hierarchical readings of marriage sacrament indissolubility can coexist within one institutional framework or are logically incompatible.').

omega_variable(
    discernment_consistency_measurability,
    'Is the diocese-by-diocese variation in applying discernment criteria a genuine pastoral adaptation to local conditions, or is it de facto doctrinal fragmentation that the ''discernment'' framing obscures?',
    'Comparative study of outcomes across dioceses with differing discernment policies — tracking rates of sacramental admission, first-spouse outcomes, and clergy consistency in applying stated criteria.',
    'If variation tracks genuine local pastoral need, the extraction measured here is closer to legitimate coordination cost. If variation tracks arbitrary or unaccountable clerical discretion, the extraction is closer to institutional failure dressed as pastoral sensitivity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discernment_consistency_measurability, empirical, 'Whether diocesan variation in discernment practice reflects genuine pastoral adaptation or unaccountable inconsistency.').

omega_variable(
    first_spouse_standing_ambiguity,
    'Does the internal-forum pathway adequately account for the interests and rights of the first spouse, or does it structurally sideline them by never requiring a tribunal finding on the validity of the first marriage?',
    'Canonical and pastoral review specifically tracking cases from the first spouse''s perspective — whether they are consulted, informed, or given standing in the discernment process.',
    'If first spouses are consistently excluded from any formal process, this reading''s moderate extraction rating understates the harm to that specific victim group and might warrant reclassification toward a higher extractiveness or even snare-adjacent reading for that subgroup specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_spouse_standing_ambiguity, empirical, 'Whether abandoned first spouses have meaningful standing in the discernment pathway or are structurally excluded from it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(marr_tr_t4, marriage_sacrament__civic_pastoral_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(marr_tr_t8, marriage_sacrament__civic_pastoral_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(marr_tr_t12, marriage_sacrament__civic_pastoral_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(marr_tr_t16, marriage_sacrament__civic_pastoral_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__civic_pastoral_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(marr_be_t4, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(marr_be_t8, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(marr_be_t12, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(marr_be_t16, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(marr_su_t4, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(marr_su_t8, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(marr_su_t12, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(marr_su_t16, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__civic_pastoral_reading, 0.1).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% This constraint and hierarchical_indissolubility_reading are the two declared readings of the marriage_sacrament kernel. They share the same underlying text and institutional lineage (Catholic canon law and magisterial teaching on marriage) but diverge on whether indissolubility is aspirational (subject to pastoral discernment) or constitutive (requiring hierarchical tribunal adjudication with no informal bypass). Each reading is authored as its own constraint with its own ε, beneficiaries, and victims; do not average their metrics or treat them as one constraint measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
