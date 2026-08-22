% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Relationship — Civic-Pastoral (Discernment) Reading
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This story authors the civic-pastoral reading of the marriage sacrament
 *   kernel: marriage is a pastoral relationship subject to genuine human
 *   failure, and indissolubility functions as an ideal toward which couples
 *   strive rather than an ontological fact adjudicated purely by hierarchical
 *   tribunal. Under this reading, discernment-based pastoral accompaniment
 *   (culminating institutionally in documents like Amoris Laetitia's
 *   internal-forum approach) allows divorced-and-remarried Catholics a path
 *   back to sacramental life without waiting on, or sometimes without ever
 *   obtaining, a formal annulment. This is NOT the same constraint as the
 *   hierarchical-indissolubility reading (a separate story), which holds
 *   marriage as ontologically fixed and adjudicated exclusively through
 *   canonical tribunal — that reading's ε, beneficiaries, and victims differ
 *   substantially and are authored separately, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - pastoral_ministers_exercising_discernment: administers the discretionary pathway
 *   - divorced_and_remarried_catholics: primary beneficiary of pastoral flexibility
 *   - traditional_catholic_laity: bears the cost of doctrinal relativization, identity-locked
 *   - canon_lawyers_committed_to_uniform_doctrine: professional stake in tribunal uniformity
 *   - first_marriage_spouses_seeking_vindication: powerless, trapped, denied closure
 *   - vatican_curia: analytical seat straddling both readings without resolving the contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.42).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.31).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Relationship — Civic-Pastoral (Discernment) Reading").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '6cf147cb-82ea-44fe-8f8d-16850811efb4').
narrative_ontology:cs_kernel_codification('6cf147cb-82ea-44fe-8f8d-16850811efb4', fixed_text).
narrative_ontology:cs_authority_grounding('6cf147cb-82ea-44fe-8f8d-16850811efb4', lineage).
narrative_ontology:cs_interpretation_layer_present('6cf147cb-82ea-44fe-8f8d-16850811efb4').
narrative_ontology:cs_reading_relation('6cf147cb-82ea-44fe-8f8d-16850811efb4', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('6cf147cb-82ea-44fe-8f8d-16850811efb4', foundational, indissolubility_as_regulative_ideal).
narrative_ontology:cs_axiom_status(indissolubility_as_regulative_ideal, holdable).
narrative_ontology:cs_axiom_grounding('6cf147cb-82ea-44fe-8f8d-16850811efb4', indissolubility_as_regulative_ideal, instrumental).
narrative_ontology:cs_axiom('6cf147cb-82ea-44fe-8f8d-16850811efb4', foundational, conscience_mediated_sacramental_access).
narrative_ontology:cs_axiom_status(conscience_mediated_sacramental_access, holdable).
narrative_ontology:cs_axiom_grounding('6cf147cb-82ea-44fe-8f8d-16850811efb4', conscience_mediated_sacramental_access, conventional).
narrative_ontology:cs_reference_frame('6cf147cb-82ea-44fe-8f8d-16850811efb4', tridentine_tribunal_exclusivity).
narrative_ontology:cs_drift_state('6cf147cb-82ea-44fe-8f8d-16850811efb4', post_amoris_laetitia_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6cf147cb-82ea-44fe-8f8d-16850811efb4', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, divorced_and_remarried_catholics).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_ministers_exercising_discernment).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, diocesan_tribunals_administering_internal_forum).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholic_laity).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, canon_lawyers_committed_to_uniform_doctrine).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, first_marriage_spouses_seeking_vindication).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Local priests and bishops applying case-by-case pastoral judgment (the 'internal forum' approach) to admit divorced-and-remarried Catholics to communion without full canonical annulment. They administer the discernment process, set its practical criteria parish by parish, and gain pastoral flexibility and reduced conflict with parishioners, at the cost of inconsistent application across dioceses.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_ministers_exercising_discernment, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, pastoral_ministers_exercising_discernment, beneficiary).

% Catholics in second marriages without annulment who receive pastoral accompaniment and, in many dioceses, access to sacraments previously withheld. They gain reintegration into community life but remain dependent on which pastor or bishop they happen to have, since no uniform rule protects the outcome.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, divorced_and_remarried_catholics, beneficiary,
    moderate, biographical, constrained, national).

% Local ecclesial bodies that process discernment cases outside the formal annulment tribunal system. They gain administrative discretion and reduced caseload pressure on formal tribunals, but their inconsistent standards from diocese to diocese are the visible seam in the doctrine's claimed universality.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, diocesan_tribunals_administering_internal_forum, agenda_setter,
    institutional, generational, constrained, national).

% Catholics whose religious and moral identity is built on marriage's indissolubility as an unconditional, teachable norm. They experience the pastoral-discernment approach as doctrinal erosion — the same sacrament now depends on which confessor one draws — and cannot simply 'exit' since their identity and communal belonging are constituted by adherence to the Church's moral teaching as they understand it.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_catholic_laity, payer,
    moderate, generational, identity_locked, global).

% Canonists and moral theologians who built careers on the formal tribunal system as the sole legitimate path to remarriage. The internal-forum approach bypasses their expertise and jurisdiction, devaluing the tribunal process they administer and the doctrinal consistency their professional identity depends on.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, canon_lawyers_committed_to_uniform_doctrine, payer,
    organized, generational, constrained, global).

% Abandoned spouses who sought a formal annulment process to have their marriage's status adjudicated and their standing vindicated. Under pastoral discernment, their former spouse's remarriage may be quietly regularized without any finding about the validity of the first marriage, leaving them without the closure or vindication the formal process was meant to provide.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, first_marriage_spouses_seeking_vindication, payer,
    powerless, biographical, trapped, local).

% The central magisterial authority that must adjudicate between the pastoral-discernment reading and the hierarchical-indissolubility reading without formally repudiating either, producing documents (e.g., Amoris Laetitia and its footnotes) deliberately allowing both readings to persist.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, vatican_curia, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a pastoral mechanism for reintegrating divorced-and-remarried Catholics into sacramental life without requiring every case to pass through a lengthy, sometimes inaccessible formal tribunal, coordinating mercy-oriented ministry with the practical reality that many marriages fail irreparably.
% TRANSFER_FUNCTION: Moves normative clarity and doctrinal uniformity away from traditional laity and canon lawyers (who valued a single legible standard) toward divorced-and-remarried Catholics and the local clergy who now hold discretionary power over their sacramental status; also moves closure away from abandoned first spouses who no longer receive an adjudicated finding.
% ABSENT_VOICES: Abandoned first-marriage spouses are rarely centered in the pastoral-discernment discourse, which focuses on the remarried party's reintegration; traditional laity's objections are frequently characterized as rigorism rather than engaged as a substantive claim about doctrinal stability, keeping their objection outside the deliberative frame.
% DISAPPEARANCE_RATIONALE: If pastoral discernment and the internal-forum pathway disappeared overnight, divorced-and-remarried Catholics would again face uniform exclusion from communion pending formal annulment; local pastors would lose their current administrative discretion; and the visible inconsistency across dioceses that traditional laity cite as evidence of doctrinal erosion would collapse back into a single enforced standard, for good or ill.
% FOUNDING_PROBLEM: The formal annulment tribunal system was slow, expensive, psychologically punishing, and inaccessible to many Catholics in irregular unions, especially the poor and those in regions with few functioning tribunals; the pastoral-discernment approach was built to solve real, unaddressed pastoral suffering that the formal system left unresolved.
% FOUNDING_PROBLEM_CORROBORATION: Pastoral ministers and many divorced Catholics attest the tribunal-access problem is still live and the discernment approach genuinely answers it. Canon lawyers and traditionalist theologians, from outside the group that benefits from discernment, argue the tribunal system's accessibility problems could be fixed by streamlining tribunals directly (as Pope Francis's own 2015 reforms attempted) without dissolving the uniform standard, and that discernment persists less to solve the access problem than to avoid an unresolved doctrinal confrontation.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.42) and rising over the interval: the doctrinal cost to traditional laity and canon lawyers accumulates as discernment practice becomes institutionally normalized (post-Amoris Laetitia), even though no single actor extracts a concentrated material rent. Suppression is moderate-low and DECLINING (0.40 → 0.31) because pastoral discernment by design reduces the coercive uniformity of the older tribunal-only regime — it substitutes discretion for enforcement, which lowers structural suppression even as it raises the diffuse cost of inconsistency. Theater ratio rises modestly (0.12 → 0.28) as institutional language increasingly frames ad hoc pastoral variance as principled discernment rather than acknowledging doctrinal drift outright.
 *
 * PERSPECTIVAL GAP:
 *   From the pastoral-minister and divorced-Catholic seats, this is a genuine coordination improvement — a rope solving real access and mercy problems the tribunal system failed to solve. From the traditional-laity and canon-lawyer seats, the identical structure functions as extraction: the predictable, teachable, defensible doctrinal good they relied on is eroded by inconsistent enforcement they did not consent to and cannot exit without abandoning core religious identity. The engine should compute this divergence from the structural data (identity_locked exit for traditional laity vs. constrained/beneficiary exit for pastoral seats) rather than from any single narrative frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Pastoral ministers and diocesan tribunals administering the internal forum sit near the beneficiary end: they gain discretion and reduced institutional friction. Divorced-and-remarried Catholics are moderate beneficiaries — real gain, but contingent and non-portable across dioceses. Traditional laity are targets: they are identity_locked (their religious self-understanding is constituted by doctrinal uniformity) and cannot exit without a costly identity rupture, which the derivation chain correctly pushes toward high d. First-marriage spouses seeking vindication are the most acute targets — powerless, trapped locally, and structurally denied even a procedural response.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is deliberate: the coordination function is real and serves an actual pastoral need (tribunal inaccessibility was a genuine, documented problem), which prevents mislabeling this as pure extraction. But the requirement of active enforcement (dioceses must actively maintain inconsistent, locally-varying discernment criteria, and the Vatican must actively tolerate the resulting doctrinal ambiguity rather than resolve it) plus the clearly named victim group (traditional laity, canon lawyers, abandoned spouses) who pay a real cost through the same structure that pastoral seats benefit from, both confirm this is not simple coordination. The founding problem (tribunal inaccessibility) remains partially live, which is why founding_problem_status is authored as 'contested' rather than 'dead' — reclassifying this reading as a pure snare would understate its genuine coordination content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discernment_is_doctrine_or_erosion,
    'Does pastoral discernment represent a legitimate development of doctrine consistent with the Church''s own tradition of casuistry and epikeia, or is it a de facto abandonment of indissolubility dressed in pastoral language?',
    'A future magisterial document that either formally codifies discernment criteria uniformly (resolving toward legitimate development) or is formally repudiated by a subsequent pontificate (resolving toward erosion). Absent either, the ambiguity persists indefinitely by design.',
    'If codified uniformly, this reading converges toward a rope (real coordination function, reduced arbitrary variance). If repudiated or left to fracture further by diocese, this reading drifts toward snare (extraction disguised as mercy, sustained by institutional non-decision).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discernment_is_doctrine_or_erosion, conceptual, 'Whether pastoral discernment is doctrinal development or doctrinal erosion.').

omega_variable(
    diocesan_variance_measurement,
    'How much does the actual practical outcome for a divorced-and-remarried Catholic vary by diocese, and is that variance narrowing or widening over time?',
    'Comparative survey of diocesan pastoral guidelines issued post-Amoris Laetitia (2016 onward) across multiple bishops'' conferences, tracked longitudinally.',
    'Narrowing variance would support the coordination-function reading (a workable norm is emerging); widening variance would support the extraction reading (inconsistency is structural, not transitional) and would push suppression_requirement''s declining trend into question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diocesan_variance_measurement, empirical, 'Whether diocesan discernment practice is converging or fragmenting.').

omega_variable(
    kernel_reading_framing_choice,
    'Is the correct unit of analysis ''the marriage sacrament kernel with two readings'' or ''two separate doctrines that happen to share a canonical text''? A reader could argue the hierarchical and pastoral approaches are not really readings of one kernel at all, but two doctrines in live succession (pre- and post-Amoris Laetitia), which would change how sibling relations should be typed.',
    'Examine whether both readings are simultaneously invoked by different actual dioceses/bishops in the present tense (supporting the coexisting-readings framing) versus whether the pastoral reading has functionally superseded the hierarchical one in practice (supporting a succession framing).',
    'If coexisting, coexists_with is the correct reading_relations value (as authored). If succession, the correct relation might be closer to influences or even a claim that this reading forecloses the practical operation of the sibling in most dioceses, even though it does not foreclose it doctrinally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_choice, conceptual, 'Whether the two readings genuinely coexist or one has functionally superseded the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(marr_tr_t8, marriage_sacrament__civic_pastoral_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(marr_tr_t16, marriage_sacrament__civic_pastoral_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(marr_tr_t24, marriage_sacrament__civic_pastoral_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(marr_tr_t32, marriage_sacrament__civic_pastoral_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__civic_pastoral_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(marr_be_t8, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(marr_be_t16, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(marr_be_t24, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(marr_be_t32, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t8, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(marr_su_t16, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(marr_su_t24, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(marr_su_t32, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 32, 0.32).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 40, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__civic_pastoral_reading, 0.1).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% This constraint and marriage_sacrament__hierarchical_indissolubility_reading decompose a single natural-language label ('the Church's teaching on marriage indissolubility') into two structurally distinct constraints per the ε-invariance principle. This story (civic_pastoral_reading) authors moderate, rising extraction against traditional laity and canon lawyers, with declining suppression as discretion replaces uniform enforcement. The sibling authors extraction against divorced-and-remarried Catholics denied pastoral accommodation, with suppression sustained by tribunal gatekeeping. The two share the same kernel text (canon 1141, magisterial teaching on indissolubility) but have inverted beneficiary/victim sets and different ε trajectories — they are not the same constraint measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
