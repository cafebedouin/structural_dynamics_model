% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Rupture Reading: Magisterial Authority as Doctrinal Supersession
 *   domain: ecclesiological/institutional
 *
 * SUMMARY:
 *   The Second Vatican Council (1962-1965) produced texts that the rupture
 *   reading interprets as encoding a new ecclesiology fundamentally
 *   incompatible with pre-conciliar teaching. This reading has become the
 *   operative hermeneutic for a segment of the post-conciliar magisterium and
 *   progressive clergy, authorizing liturgical experimentation, religious
 *   freedom as an intrinsic right (Dignitatis Humanae), and collegial
 *   governance. From this reading's perspective, the constraint is the
 *   standing magisterial arrangement that enforces this rupture â
 *   suppressing pre-conciliar theological positions and traditional
 *   liturgical practice as 'superseded' while coordinating progressive
 *   implementation. The reading endorses the rupture, but the constraint
 *   story assesses the arrangement's structural operation: a coordination
 *   mechanism for Church modernization that asymmetrically extracts from
 *   traditionalist Catholics through active enforcement.
 *
 * KEY AGENTS:
 *   - conciliar_hierarchy: Primary agenda-setter (institutional/arbitrage) â administers the rupture hermeneutic and enforces doctrinal boundaries
 *   - progressive_clergy: Primary beneficiary (organized/mobile) â collects institutional space, career legitimacy, and liturgical freedom from the new ecclesiology
 *   - traditionalist_laity: Primary target (moderate/identity_locked) â bears liturgical suppression and doctrinal delegitimization
 *   - pre_conciliar_theologians: Secondary target (moderate/identity_locked) â bears intellectual supersession and professional marginalization
 *   - irregular_traditional_communities: Excluded resistance (organized/trapped) â structurally barred from canonical regularity and institutional voice
 *   - academic_historians: Analytical observer (analytical/analytical) â sees the textual ambiguity and enforcement gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.72).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.78).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Rupture Reading: Magisterial Authority as Doctrinal Supersession").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiological/institutional").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, 'c9d8fbb9-3388-4ea7-83f8-e1a0ace37514').
narrative_ontology:cs_kernel_codification('c9d8fbb9-3388-4ea7-83f8-e1a0ace37514', fixed_text).
narrative_ontology:cs_authority_grounding('c9d8fbb9-3388-4ea7-83f8-e1a0ace37514', lineage).
narrative_ontology:cs_interpretation_layer_present('c9d8fbb9-3388-4ea7-83f8-e1a0ace37514').
narrative_ontology:cs_reading_relation('c9d8fbb9-3388-4ea7-83f8-e1a0ace37514', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('c9d8fbb9-3388-4ea7-83f8-e1a0ace37514', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('c9d8fbb9-3388-4ea7-83f8-e1a0ace37514', foundational, conciliar_texts_supersede_prior_magisterium).
narrative_ontology:cs_axiom_status(conciliar_texts_supersede_prior_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('c9d8fbb9-3388-4ea7-83f8-e1a0ace37514', conciliar_texts_supersede_prior_magisterium, conventional).
narrative_ontology:cs_axiom('c9d8fbb9-3388-4ea7-83f8-e1a0ace37514', foundational, religious_freedom_as_intrinsic_right).
narrative_ontology:cs_axiom_status(religious_freedom_as_intrinsic_right, holdable).
narrative_ontology:cs_axiom_grounding('c9d8fbb9-3388-4ea7-83f8-e1a0ace37514', religious_freedom_as_intrinsic_right, deontological).
narrative_ontology:cs_reference_frame('c9d8fbb9-3388-4ea7-83f8-e1a0ace37514', conciliar_renewal_realized).
narrative_ontology:cs_drift_state('c9d8fbb9-3388-4ea7-83f8-e1a0ace37514', post_traditionis_custodes_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c9d8fbb9-3388-4ea7-83f8-e1a0ace37514', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, conciliar_hierarchy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the authoritative interpretation of Vatican II as rupture with pre-conciliar teaching; issues disciplinary documents that restrict traditional liturgical practice and theological positions; determines orthodoxy by the conciliar standard and collects magisterial authority from the new ecclesiology's institutional center.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, conciliar_hierarchy, agenda_setter,
    institutional, generational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, conciliar_hierarchy, beneficiary).

% Advance liturgical experimentation and theological frameworks under the conciliar mandate; their careers, institutional legitimacy, and academic positions depend on the rupture reading remaining dominant; they collect status and ecclesial space cleared by the delegitimization of pre-conciliar frameworks.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_clergy, beneficiary,
    organized, generational, mobile, global).

% Attached to pre-conciliar liturgical and catechetical forms; experience restrictions on the Traditional Latin Mass, parish closures of traditional apostolates, social marginalization within diocesan structures, and the psychological cost of seeing their spiritual patrimony designated as superseded and incompatible with the Church's authentic expression.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_laity, payer,
    moderate, generational, identity_locked, global).

% Work within theological paradigms such as integralism, strict ecclesiocentrism, and the thesis that error has no rights; their intellectual framework has been formally superseded by conciliar assertions of religious freedom and collegiality; they face professional isolation, censorship, or doctrinal correction from conciliar enforcement bodies.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_theologians, payer,
    moderate, biographical, identity_locked, global).

% SSPX and similar groups maintain pre-conciliar sacramental and catechetical life outside canonical regularity; are structurally excluded from the institutional theological conversation and magisterial consultation; would re-enter regular communion if the rupture reading were abandoned but are trapped in irregular status by the enforcement of conciliar novelty.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, irregular_traditional_communities, excluded,
    organized, generational, trapped, global).

% Study the conciliar acta, drafting histories, and fifty-year reception record; observe the gap between the texts' ambiguous compromise formulations and the rupture reading's definitive hermeneutic claims; document when enforcement outruns textual warrant but do not participate in magisterial governance.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, academic_historians, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the Catholic Church's mid-20th-century encounter with modernity by authorizing a new ecclesiology â collegial governance, religious freedom as an intrinsic right, ecumenical openness, and liturgical reform â that restructures the Church's relationship to secular states, other Christians, and its own members.
% TRANSFER_FUNCTION: Moves magisterial authority and liturgical legitimacy from pre-conciliar theological frameworks and traditional rites to progressive implementers and reformed norms; moves the cost of adaptation from the institutional center to traditionalist Catholics and pre-conciliar theologians who must abandon or hide their prior identity.
% ABSENT_VOICES: Pre-conciliar theologians who died before 1965 and could not contest the retrospective characterization of their work as superseded; irregular traditional communities are canonically excluded from the magisterial conversation; Eastern Orthodox observers at the Council who warned against ecclesiological reductionism are no longer in the room.
% DISAPPEARANCE_RATIONALE: If the rupture reading vanished overnight as an authoritative hermeneutic, progressive clergy would lose their primary legitimating framework, traditionalist Catholics would regain canonical regularity for their rites and theological positions, and the conciliar hierarchy would face a crisis of authority as the texts' ambiguities resurfaced without the rupture narrative to resolve them in a progressive direction.
% FOUNDING_PROBLEM: The mid-20th-century Catholic Church faced a crisis of relevance: shrinking European church attendance, anti-clerical political movements, ecumenical isolation, and a laity exposed to modern biblical criticism and secular philosophy without adequate theological frameworks. The Council was convened to address these 'signs of the times'.
% FOUNDING_PROBLEM_CORROBORATION: Progressive clergy and the conciliar hierarchy attest the problem was real and required radical reform. Traditionalist Catholics and some academic historians attest the 'crisis' was manufactured or exaggerated by progressives to justify doctrinal rupture; corroboration from outside the benefiting parties is split, with secular historians noting institutional decline but disputing whether doctrinal rupture was the necessary remedy.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extraction is high (0.72) because traditionalists lose liturgical standing and theological legitimacy without commensurate gain; suppression is higher (0.78) because the arrangement requires active enforcement (Traditionis Custodes, theological censorship, canonical restrictions) to maintain the supersession narrative against persistent traditionalist identity. Theater is moderate (0.45): the conciliar texts contain genuine doctrinal content, but a substantial share of enforcement activity defends the 'spirit of the Council' â a progressive interpretive layer that often exceeds the texts' ambiguous formulations. Accessibility collapse is high (0.68) because once the rupture is accepted within the institution, pre-conciliar positions become nearly unthinkable as live orthodox options. Resistance is moderate (0.58) because traditionalist movements, though marginalized, have proven organizationally durable and have generated significant intra-ecclesial pushback. The measurement series use one shared time grid so every metric is authored at every examined time point, capturing the Benedictine dÃ©tente (T=40) and subsequent re-intensification.
 *
 * PERSPECTIVAL GAP:
 *   The conciliar hierarchy and progressive clergy experience the constraint as genuine coordination (modernizing the Church, solving the 'signs of the times', enabling ecumenism). Traditionalist laity and pre-conciliar theologians experience it as extraction â their religious and intellectual patrimony is declared obsolete and actively suppressed. The engine computes this divergence from the structural data: same conciliar texts, opposite directionalities depending on institutional position and identity fusion. The authored claim (tangled_rope) reflects this dual character; the metrics are authored to describe the actual operation, not to reconcile the seats' perceptions.
 *
 * DIRECTIONALITY LOGIC:
 *   The conciliar hierarchy is agenda-setter with arbitrage-grade exit â they can soften or intensify the rupture reading when politically necessary (see the Benedict XVI period). Progressive clergy are beneficiaries with mobile exit â they can move to academic or secular posts if the ecclesial climate shifts. Traditionalist laity and pre-conciliar theologians are payers with identity_locked exit â their religious and intellectual self-concepts are fused to the pre-conciliar paradigm, making exit equivalent to apostasy or self-annihilation. Irregular communities are trapped in canonical limbo. Directionality is thus strongly bifurcated: low d for the progressive camp, very high d for the traditionalist camp.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by acknowledging the genuine coordination function (addressing modernity, ecumenism, religious freedom, liturgical participation) while documenting the asymmetric extraction (traditionalists pay through suppression of their rites and theology). Without this dual recognition, the constraint would be misread as pure snare (ignoring the progressive coordination) or pure rope (ignoring the identity-locked extraction of traditionalists). The founding problem â 20th-century Church crisis â is contested in status, suggesting the original mandate may have outlived its justification but persists through progressive theological commitment and institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_as_construction_vs_discovery,
    'Is the rupture between Vatican II and pre-conciliar teaching a genuine semantic discovery in the conciliar texts, or a hermeneutic construction imposed by progressive interpreters during post-conciliar reception?',
    'Close philological and historical analysis of the conciliar acta, pericope drafting histories, and reception chronologies; detection of whether the rupture reading was projectively inscribed during 1965-1975 interpretation rather than encoded in the signed texts.',
    'If construction, the constraint''s extractiveness is higher than claimed â the supersession is an enforcement of an invented discontinuity upon identity-locked traditionalists; if discovery, the extraction is the necessary cost of doctrinal correction and genuine ecclesial development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_as_construction_vs_discovery, empirical, 'Whether rupture is textually encoded or interpretively projected').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of pre-conciliar positions achieved through structural canonical enforcement alone, or through internalized theological shame and identity fusion with the new ecclesiology?',
    'Post-exit suppression trajectory: if traditionalists who leave the institutional Church for irregular communities continue to self-censor, accept marginalization, or exhibit identity-dissonance, suppression is partially internalized; if suppression ends immediately upon canonical exit, it is purely structural.',
    'If internalized, effective suppression exceeds the structural measure â the target carries the constraint with them after exit; if structural, exit to irregular communities genuinely reduces extraction and the constraint''s scope is institutionally bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    founding_problem_veracity,
    'Was the mid-20th century ''crisis of the Church'' sufficiently severe to warrant a doctrinal rupture, or was it manageable through reform in continuity?',
    'Comparative historical analysis of Catholic institutional vitality indices (vocations, attendance, intellectual output, missionary activity) in 1950-1965 relative to other periods of reform; assessment of whether organic reform was structurally available without the conciliar rupture narrative.',
    'If the crisis was exaggerated or manageable, the rupture reading''s founding justification collapses and the constraint functions as post-hoc rationalization for progressive capture; if the crisis was existential, the coordination function is stronger and the extraction is the price of institutional survival.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_veracity, conceptual, 'Whether the pre-conciliar crisis warranted doctrinal rupture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(v2ma_rupture_tr_t0, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(v2ma_rupture_tr_t10, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(v2ma_rupture_tr_t20, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(v2ma_rupture_tr_t30, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(v2ma_rupture_tr_t40, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(v2ma_rupture_tr_t50, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 50, 0.44).
narrative_ontology:measurement(v2ma_rupture_tr_t60, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(v2ma_rupture_be_t0, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(v2ma_rupture_be_t10, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(v2ma_rupture_be_t20, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(v2ma_rupture_be_t30, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(v2ma_rupture_be_t40, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(v2ma_rupture_be_t50, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(v2ma_rupture_be_t60, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(v2ma_rupture_su_t0, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(v2ma_rupture_su_t10, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(v2ma_rupture_su_t20, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(v2ma_rupture_su_t30, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(v2ma_rupture_su_t40, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(v2ma_rupture_su_t50, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(v2ma_rupture_su_t60, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 60, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is the rupture reading of the kernel vatican_ii_magisterial_authority. The kernel decomposes into three structurally distinct readings because the conciliar texts and their reception admit multiple incompatible Îµ profiles. This reading asserts high extractiveness through doctrinal supersession; the continuity reading asserts low extraction through organic development; the composite reading asserts indeterminate extraction through textual overdetermination. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
