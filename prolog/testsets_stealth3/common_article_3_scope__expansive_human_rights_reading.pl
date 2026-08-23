% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__expansive_human_rights_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Common Article 3 Universal Floor — Expansive Human Rights Reading
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Common Article 3 scope kernel:
 *   the expansive human rights reading, under which Common Article 3 attaches
 *   as a floor of minimum humanitarian standards to ANY organized armed
 *   violence, regardless of how the violence is classified. The standing
 *   arrangement under contest — and the referent of epsilon — is that
 *   universal-floor regime as this reading sees it: every internal security
 *   operation enters the constraint's perimeter, all detainees and affected
 *   populations enter the protected set, and state security operations face
 *   external monitoring and potential prosecution. The reading was
 *   consolidated through the ad hoc tribunal jurisprudence of the 1990s, the
 *   humanization-of-IHL movement, and convergence between human rights law
 *   and the law of armed conflict; it is resisted by states that maintain
 *   threshold-gated scope and tracked uneasily by the customary-law process
 *   that records what states actually accept. KEY AGENTS (by structural
 *   relationship): - contracting_states_conducting_internal_operations:
 *   Primary target (institutional/constrained) — bears compliance costs,
 *   monitoring exposure, and prosecution risk -
 *   detained_persons_in_internal_conflicts: Primary beneficiary
 *   (powerless/trapped) — holds the enforceable floor without classification
 *   precondition - civilian_populations_in_low_intensity_violence: Secondary
 *   beneficiary (powerless/trapped) — enters the protected set under
 *   universal scope - international_criminal_tribunals: Enforcement
 *   administrator (institutional/analytical) — interprets scope, accrues
 *   jurisdiction and docket - icrc_detention_monitoring: Access beneficiary
 *   (institutional/constrained) — converts the reading into detention-visit
 *   leverage - human_rights_monitoring_bodies: Mandate beneficiary
 *   (institutional/mobile) — extends scrutiny into internal security
 *   operations - non_state_armed_groups: Dual-positioned participant
 *   (organized/constrained) — gains fighter protections while absorbing
 *   mirrored obligations - intelligence_detention_operators: Excluded actor
 *   (powerful/constrained) — runs the covert detention function the reading
 *   would expose - international_humanitarian_law_scholars: Analytical
 *   observer (analytical/analytical) — maps the kernel contest without
 *   collecting from it The interval 0–30 maps approximately to 1995–2025,
 *   from the landmark non-international-conflict jurisprudence to the present
 *   consolidation period. All measurement points fall within the documented
 *   historical record and carry observed basis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.48).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.6).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 Universal Floor — Expansive Human Rights Reading").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, '4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c').
narrative_ontology:cs_kernel_codification('4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c', fixed_text).
narrative_ontology:cs_authority_grounding('4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c', lineage).
narrative_ontology:cs_interpretation_layer_present('4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c').
narrative_ontology:cs_reading_relation('4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c', common_article_3_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c', foundational, classification_independent_humane_floor).
narrative_ontology:cs_axiom_status(classification_independent_humane_floor, holdable).
narrative_ontology:cs_axiom_grounding('4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c', classification_independent_humane_floor, deontological).
narrative_ontology:cs_axiom('4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c', secondary, martens_clause_gap_filling_expansion).
narrative_ontology:cs_axiom_status(martens_clause_gap_filling_expansion, holdable).
narrative_ontology:cs_axiom_grounding('4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c', martens_clause_gap_filling_expansion, deontological).
narrative_ontology:cs_reference_frame('4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c', universal_minimum_humanitarian_floor).
narrative_ontology:cs_drift_state('4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c', contemporary_state_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4fcda39d-b8b7-42c8-b28d-c5aa3a746c2c', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detained_persons_in_internal_conflicts).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, civilian_populations_in_low_intensity_violence).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, icrc_detention_monitoring).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, human_rights_monitoring_bodies).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, contracting_states_conducting_internal_operations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, martens_clause_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, humane_treatment_minimum_standard_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct counterinsurgency and internal security operations on their own territory. Under this reading, every episode of organized armed violence triggers Common Article 3's minimum standards regardless of how the state classifies the violence. They bear detention-procedure costs, impartial-humanitarian-access requests they cannot cleanly refuse, domestic and international prosecution exposure for violations, and standing monitoring by UN bodies. Their preferred alternative — classifying violence below the conflict threshold to escape treaty limits — is precisely what this reading removes.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, contracting_states_conducting_internal_operations, payer,
    institutional, generational, constrained, global).

% Persons held by state forces or armed groups during internal violence. Under this reading they hold Common Article 3 protections without anyone needing to classify the violence as an armed conflict at all. What flows to them is an enforceable floor on treatment and a legal hook for impartial humanitarian access. Exit is not a category that applies — they are in custody.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detained_persons_in_internal_conflicts, beneficiary,
    powerless, immediate, trapped, local).

% Communities exposed to internal disturbances, riots, and low-level insurgency that narrower readings leave unregulated. This reading extends the humane-treatment floor to them; they receive protection indirectly, through constraints on the conduct of every party operating amid their villages and cities.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, civilian_populations_in_low_intensity_violence, beneficiary,
    powerless, biographical, trapped, regional).

% Ad hoc tribunals and the permanent international criminal court interpret the reach of Common Article 3, authorize indictments resting on non-international conflict classifications, and thereby administer this reading. Each expansive ruling enlarges their docket and doctrinal authority. They accrue jurisdiction, caseload, and interpretive precedence from the reading's breadth.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_criminal_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Offers its services and conducts detention visits under Common Article 3's impartial-humanitarian-body clause. The expansive reading multiplies the situations in which its access offer carries legal weight, converting every internal security operation into potential visit terrain. It collects access, mandate breadth, and operational presence; its leverage depends on remaining acceptable to detaining authorities.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, icrc_detention_monitoring, beneficiary,
    institutional, generational, constrained, global).

% UN treaty bodies, special procedures, and humanitarian NGOs invoke this reading to extend scrutiny into internal security operations that would otherwise sit outside any armed-conflict framework. They collect reporting mandates, site access, and agenda relevance; their findings feed naming-and-shaming and donor conditionality.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, human_rights_monitoring_bodies, beneficiary,
    institutional, generational, mobile, global).

% Insurgent and rebel organizations whose captured fighters gain Common Article 3 protections under this reading without any classification concession being demanded of them. Symmetrically, the same reading binds their conduct and exposes their commanders to prosecution. They gain standing and protection for their people while absorbing obligations they often lack the capacity to implement.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups, payer).

% Run off-books detention and interrogation programs deliberately kept outside acknowledged conflict frameworks. The expansive reading would pull their facilities into Common Article 3's monitoring and prosecution perimeter. They are absent from the interpretive conversation that defines the reading's reach, and would object that operational effectiveness depends on the classification ambiguity this reading eliminates.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, intelligence_detention_operators, excluded,
    powerful, immediate, constrained, global).

% Track the reading's doctrinal consolidation, publish scope analyses, and supply the interpretive arguments that both advocates and opponents of the expansive reading deploy. They see the full structure of the kernel contest and collect nothing from any outcome of it.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_humanitarian_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__expansive_human_rights_reading, international_criminal_tribunals).
narrative_ontology:fixing_cost_class(common_article_3_scope__expansive_human_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, pre-committed minimum-treatment floor for all organized armed violence, so that parties to internal conflicts and third parties assessing them operate from one predictable baseline instead of case-by-case classification fights, and so that impartial humanitarian bodies can make access offers with legal weight in every internal violence.
% TRANSFER_FUNCTION: Moves legal exposure and operational discretion from state security apparatuses (and, mirrored, armed-group commands) toward international prosecutorial and monitoring institutions, and moves enforceable treatment protections toward detainees and civilians caught in internal violence.
% ABSENT_VOICES: Intelligence detention operators and the commanders of covert programs sit outside the interpretive conversation entirely; so do populations whose security depends on decisive state operations and who absorb the tactical costs of restrained counterinsurgency. States holding the state-centric reading appear in treaty forums, but their interpretive position is a persistent minority voice inside the judicial and monitoring venues where this reading consolidated.
% DISAPPEARANCE_RATIONALE: If the universal floor vanished overnight, classification contests would again become the sole gate to humanitarian protection: detainees in violence no state acknowledges as a conflict would lose their enforceable floor, tribunals would lose the non-international-conflict basis for most of their internal-violence docket, monitoring access would contract to threshold-meeting wars, and states would recover the discretion they currently spend compliance costs to defend — with reciprocal-atrocity risk rising on the other side.
% FOUNDING_PROBLEM: In 1949 the drafters confronted the classification gap: between international peace and interstate war lay civil wars in which no law applied, because states denied that a conflict existed and escaped every limit. Common Article 3 was built to close that gap; this reading's founding problem is the residual version — states still using classification denial (this is a disturbance, not a conflict) to escape even the minimum floor during internal violence.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by the 1949 Diplomatic Conference preparatory records (which document the classification-gap problem the negotiating states themselves sought to close), by national military judge-advocate doctrine acknowledging the denial-of-classification loophole, and by the documented record of classification disputes in subsequent internal conflicts. None of these attestations comes from the reading's institutional beneficiaries.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim and the metrics are independent authored facts. Claimed type: tangled_rope, because the arrangement possesses BOTH a genuine coordination function AND asymmetric extraction requiring active enforcement. The coordination half is real: a pre-committed minimum floor solves the reciprocal-brutalization problem in internal conflicts and gives impartial bodies a legal hook for access — remove it and every party faces case-by-case classification fights with no baseline. The extraction half is also real: the reading binds state security operations that never consented to this breadth, transfers discretion and legal exposure toward prosecutorial and monitoring institutions, and does so through enforcement machinery (tribunal indictments, universal jurisdiction, monitoring reports, donor conditionality) that must stay active against state resistance.
 *   
 *   Metric rationale: extractiveness 0.48 — the floor is by design MINIMUM humanitarian standards, so the burden on governed parties is bounded, but it is decoupled from any reciprocity guarantee in asymmetric conflicts, where the state pays compliance and prosecution costs while receiving no assured protective return. Suppression 0.60 — structural, not interpersonal: treaty permanence (denunciation is diplomatically catastrophic), customary-law creep that follows states even if they exit, and jurisdictional reach constrain exit; this is a raw structural property and is NOT scaled by power or scope in the engine's computation — only extractiveness is scaled. Theater ratio 0.28 — real function dominates (detention visits, actual indictments), but a growing share of activity is performative: pro forma investigations, selective outrage calibrated to geopolitical alignment, formal adherence outrunning practice. Accessibility collapse 0.35 — LOW, and honestly so: the principal alternative (the threshold-gated state-centric reading) remains a live, practiced position; understanding this constraint does not eliminate rival readings. Resistance 0.65 — sustained and organized: major military powers contest scope in doctrine and practice, and every counterinsurgency regenerates classification-denial arguments. The temporal series run on ONE shared grid (points 0, 5, 10, 15, 20, 25, 30) so every metric is authored at every examined time point; base_extractiveness rises monotonically (accumulating scope and prosecution exposure), suppression_requirement rises (enforcement machinery matured from ad hoc tribunals to a permanent court and monitoring lattice), and theater_ratio rises modestly as formal adherence outpaces practice.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats compute radically different types from identical structural data. From the state seat, the arrangement is enforced extraction: an obligation set it cannot exit, administered by institutions it does not control, applied to operations it classifies as policing. From the detainee seat, the same structure is pure subsidy: protection arriving without any precondition it could influence. Two institutional seats diverge just as sharply — the tribunals experience the reading as expanding mandate, the contracting states as shrinking discretion — despite nominally comparable institutional power, because their structural RELATIONSHIP to the constraint (administrator versus governed) differs, not their power level. Non-state armed groups straddle: their captured fighters are subsidized while their commands are targeted. The engine computes these per-seat classifications from the structural data; this commentary explains why they diverge without adjudicating which seat's view is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the beneficiary/victim declarations plus exit structure. Contracting states sit nearest the full-target end: they are the declared victims, their exit is constrained (treaty lock-in plus customary creep), and the reading's entire enforcement apparatus aims at their conduct. Detained persons and civilian populations sit at the beneficiary end: full subsidy, zero exit relevance (trapped in custody or in place). The ICRC and human rights monitoring bodies derive low-to-moderate directionality — they collect access and mandate without running the underlying arrangement, though the ICRC's dependence on detaining-authority consent pulls it slightly back from pure beneficiary. International criminal tribunals derive low directionality as administrators who accrue jurisdiction; they are the seat the constraint's gains demonstrably accrue to. Non-state armed groups derive a middling value: declared beneficiaries (fighter protection, equal-standing recognition) carrying a secondary payer position (mirrored obligations, commander prosecution exposure). No directionality overrides are authored: the derivation from declared roles and exit options captures these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — classification denial as an escape hatch from humanitarian limits — is LIVE: every counterinsurgency regenerates it, so this is not a vestige maintained by inertia, and no mandatrophy resolution is declared. The tangled_rope classification does specific preventive work here. Reading the arrangement as a pure snare would erase the genuine reciprocal-floor function that makes the constraint valuable to parties on BOTH sides of an internal conflict and would misdescribe the detained-person seat's uncompensated subsidy. Reading it as a pure rope would launder the asymmetric enforcement exposure — the fact that compliance costs and prosecution risk concentrate on state security operations (and on losing parties generally) while the reading's institutional beneficiaries accrue jurisdiction and mandate. Holding both halves visible is exactly what the tangled-rope category exists to do. The piton test fails clearly: the administrator seats benefit enough to maintain the arrangement actively, and the enforcement machinery is functional, not theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (expansive_human_rights_reading) of the common_article_3_scope kernel: which scope predicate does the kernel''s text and purpose actually fix — a universal floor, a threshold gate, or a practice-tracked boundary?',
    'Authoritative convergence across the sibling readings'' venues: appellate jurisprudence on non-international conflict scope, contracting-state practice and official doctrine, and the customary-law tracking process; whichever reading the converging authorities adopt as controlling re-issues this constraint with a different victim set and target set.',
    'If the state-centric sibling prevails, the victim set shrinks to detainees in threshold-meeting conflicts and sub-threshold state security operations exit the constraint entirely, collapsing measured extraction toward the narrow-conflict domain; if the customary sibling prevails, scope becomes time-varying and this reading''s epsilon holds only as long as practice supports breadth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the CA3 scope kernel is structurally correct; determines victim-set membership and target population.').

omega_variable(
    state_practice_trajectory,
    'Does accumulating state practice and opinio juris support the expansive scope (consolidating this reading) or trend back toward threshold-gating (re-narrowing it)?',
    'Systematic coding of national military manuals, court decisions across jurisdictions, treaty reservations and objections, and voting patterns in UN fora over successive review cycles.',
    'Consolidation stabilizes this reading''s epsilon and entrenches the tangled-rope structure; re-narrowing transfers scope-setting authority to the icrc_customary_reading sibling and would date a type transition in this constraint''s lifecycle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_practice_trajectory, empirical, 'Direction of state practice relative to the universal-floor scope predicate.').

omega_variable(
    victors_justice_asymmetry,
    'Does prosecutorial exposure under this reading concentrate on losing or weaker parties, such that the constraint''s extraction lands unevenly across otherwise similarly situated belligerents?',
    'Cross-tribunal docket composition analysis: situation selection criteria versus case outcomes disaggregated by side, across the ad hoc tribunals and the permanent court.',
    'If concentration is confirmed, the losing-party seat''s effective extraction approaches full-target levels and the arrangement trends snare-flavored for that seat even while the coordination function persists globally — a seat-divergent reclassification the engine would register from updated structural data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victors_justice_asymmetry, empirical, 'Whether enforcement asymmetry concentrates extraction on defeated parties.').

omega_variable(
    reciprocity_dependence,
    'Do state forces receive reciprocal protective returns under this reading — their captured personnel treated according to the floor by armed groups — or does the floor bind states unilaterally in asymmetric internal conflicts?',
    'Comparative dataset of detainee-treatment reciprocity across non-international armed conflicts, pairing state compliance indicators with armed-group treatment of captured government personnel.',
    'Absent reciprocity, the state seat''s extraction is uncompensated and its net-benefit assessment shifts toward the extraction pole, strengthening the tangled-rope reading and raising computed effective extraction for the state seat; robust reciprocity would support a heavier coordination weighting and soften the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_dependence, empirical, 'Whether the floor''s protective returns to state forces materialize or bind unilaterally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t5, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(comm_tr_t5, observed).
narrative_ontology:measurement(comm_tr_t10, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(comm_tr_t10, observed).
narrative_ontology:measurement(comm_tr_t15, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(comm_tr_t15, observed).
narrative_ontology:measurement(comm_tr_t20, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement_basis(comm_tr_t20, observed).
narrative_ontology:measurement(comm_tr_t25, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement_basis(comm_tr_t25, observed).
narrative_ontology:measurement(comm_tr_t30, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(comm_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t5, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 5, 0.34).
narrative_ontology:measurement_basis(comm_be_t5, observed).
narrative_ontology:measurement(comm_be_t10, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(comm_be_t10, observed).
narrative_ontology:measurement(comm_be_t15, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(comm_be_t15, observed).
narrative_ontology:measurement(comm_be_t20, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(comm_be_t20, observed).
narrative_ontology:measurement(comm_be_t25, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 25, 0.47).
narrative_ontology:measurement_basis(comm_be_t25, observed).
narrative_ontology:measurement(comm_be_t30, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(comm_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t5, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement_basis(comm_su_t5, observed).
narrative_ontology:measurement(comm_su_t10, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(comm_su_t10, observed).
narrative_ontology:measurement(comm_su_t15, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement_basis(comm_su_t15, observed).
narrative_ontology:measurement(comm_su_t20, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(comm_su_t20, observed).
narrative_ontology:measurement(comm_su_t25, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(comm_su_t25, observed).
narrative_ontology:measurement(comm_su_t30, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(comm_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'CA3 scope' decomposes into three structurally distinct constraints — one per reading of the common_article_3_scope kernel — because the scope predicate (universal floor vs. threshold gate vs. practice-tracked boundary) yields different epsilon values, different victim sets, and different enforcement surfaces. Per the epsilon-invariance principle these are separate stories, linked here: this expansive reading structurally influences both siblings (each expansive judicial ruling becomes data inside the customary tracker's practice record, and each consolidation raises the legitimacy cost of the threshold-gate position), while neither sibling is logically eliminable by this file's claim alone. The upstream/downstream structure runs: this reading's jurisprudence feeds the customary sibling's evidentiary base; state resistance documented under the state-centric sibling feeds this reading's resistance metric.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
