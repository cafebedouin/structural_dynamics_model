% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: 'Spirit of the Council' Progressive-Rupture Reading of Vatican II Authority
 *   domain: ecclesiology/institutional history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the progressive-rupture reading of the contested
 *   Vatican II authority kernel: that the Council constituted a necessary
 *   break with pre-conciliar rigidity, and that the documents' textual
 *   ambiguities were intentional openings meant to authorize ongoing reform
 *   via 'the spirit of the Council' rather than merely explicating implicit
 *   prior teaching. Under this reading, religious freedom (Dignitatis
 *   Humanae) is read as a genuine reversal of the Syllabus of Errors'
 *   condemnations, and post-conciliar implementation — even where it exceeds
 *   explicit textual mandate — is treated as the authentic realization of
 *   what the Council fathers intended. This is a distinct constraint from the
 *   continuity reading (which denies any doctrinal reversal occurred) and
 *   from the traditionalist-rupture reading (which agrees rupture occurred
 *   but treats it as corruption rather than legitimate development); each
 *   carries a different epsilon and different victim set and is authored as a
 *   separate story linked by network edges.
 *
 * KEY AGENTS:
 *   - progressive_episcopal_conferences: agenda_setter/beneficiary (institutional/arbitrage) — administers implementation, invokes conciliar spirit
 *   - post_conciliar_liturgical_reformers: beneficiary (organized/mobile) — gained institutional position from the reading
 *   - academic_theological_faculties: beneficiary (organized/mobile) — built disciplinary careers on rupture-development framing
 *   - traditionalist_clergy: payer (moderate/constrained) — lost faculties and assignments under implementation
 *   - lay_faithful_attached_to_prior_forms: payer (powerless/trapped) — bore disruption with no voice
 *   - religious_orders_disbanded_or_reformed_by_fiat: payer (powerless/trapped) — mandated reform without independent recourse
 *   - vatican_curial_authority: observer/agenda_setter (institutional/analytical) — ambivalent oversight, periodic correctives
 *   - traditionalist_and_continuity_scholars: excluded (moderate/constrained) — marginalized dissenting interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.58).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.47).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "'Spirit of the Council' Progressive-Rupture Reading of Vatican II Authority").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'c52c1b6c-fc61-4005-a78a-95d36e1d2fcd').
narrative_ontology:cs_kernel_codification('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', fixed_text).
narrative_ontology:cs_authority_grounding('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', lineage).
narrative_ontology:cs_interpretation_layer_present('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd').
narrative_ontology:cs_reading_relation('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', foundational, necessary_rupture_with_pre_conciliar_rigidity).
narrative_ontology:cs_axiom_status(necessary_rupture_with_pre_conciliar_rigidity, holdable).
narrative_ontology:cs_axiom_grounding('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', necessary_rupture_with_pre_conciliar_rigidity, conventional).
narrative_ontology:cs_axiom('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', foundational, conciliar_textual_ambiguity_as_intentional_license).
narrative_ontology:cs_axiom_status(conciliar_textual_ambiguity_as_intentional_license, holdable).
narrative_ontology:cs_axiom_grounding('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', conciliar_textual_ambiguity_as_intentional_license, instrumental).
narrative_ontology:cs_axiom('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', secondary, post_conciliar_implementation_as_authentic_conciliar_intent).
narrative_ontology:cs_axiom_status(post_conciliar_implementation_as_authentic_conciliar_intent, holdable).
narrative_ontology:cs_axiom_grounding('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', post_conciliar_implementation_as_authentic_conciliar_intent, conventional).
narrative_ontology:cs_reference_frame('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', pre_conciliar_neo_scholastic_synthesis).
narrative_ontology:cs_drift_state('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', post_conciliar_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c52c1b6c-fc61-4005-a78a-95d36e1d2fcd', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_episcopal_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, post_conciliar_liturgical_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, academic_theological_faculties).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, lay_faithful_attached_to_prior_forms).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, religious_orders_disbanded_or_reformed_by_fiat).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, aggiornamento_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, development_of_doctrine_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National and regional bishops' conferences that read the Council's ambiguous or open-textured passages as authorizing wide latitude in liturgical practice, catechesis, and pastoral discipline. They administer seminaries, liturgical commissions, and catechetical offices, and they invoke 'the spirit of the Council' to justify practices not explicit in the conciliar texts. They set the implementing agenda and face little effective check from Rome during the period of loosest enforcement.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_episcopal_conferences, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_episcopal_conferences, beneficiary).

% Liturgists, seminary faculty, and diocesan offices who redesigned worship, formation, and religious life on the premise that the Council intended a decisive break with pre-conciliar forms. They gained institutional positions, publishing platforms, and formation authority from this reading; if the continuity reading prevailed, much of their reform program would lose its justificatory basis.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, post_conciliar_liturgical_reformers, beneficiary,
    organized, biographical, mobile, national).

% University theology departments and journals that built research programs, tenure cases, and curricula on the premise of conciliar rupture and ongoing development beyond the text. Their disciplinary standing and funding streams depend substantially on this reading remaining institutionally credentialed.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, academic_theological_faculties, beneficiary,
    organized, generational, mobile, global).

% Priests and religious formed in or attached to pre-conciliar liturgical and doctrinal forms who found their faculties, assignments, or religious communities restricted or dissolved under this reading's implementation. Their exit options are limited to schism-adjacent movements, quiet noncompliance, or submission; canonical recourse against episcopal implementation decisions has historically been weak.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy, payer,
    moderate, biographical, constrained, national).

% Parishioners whose parishes, devotions, and sacramental forms were altered or suppressed under rapid post-conciliar implementation justified by 'the spirit of the Council.' Most had no formal voice in the changes and either adapted, drifted from practice, or sought increasingly scarce alternative communities.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, lay_faithful_attached_to_prior_forms, payer,
    powerless, biographical, trapped, local).

% Communities of religious life that underwent mandated aggiornamento of habit, rule, and apostolate, often resulting in severe membership decline. Many had no independent mechanism to contest whether the mandated changes were authentically conciliar or an extrapolation beyond the documents' text; the changes were imposed as implementation of Council intent.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, religious_orders_disbanded_or_reformed_by_fiat, payer,
    powerless, generational, trapped, national).

% The Roman curia and papacy, which periodically corrected or reined in the widest 'spirit of the Council' claims (e.g., later magisterial documents clarifying that the letter of the texts constrains implementation) while also, at other moments, ratifying rupture-reading outcomes as legitimate development. Its ambivalence is part of what let the reading operate with reduced central enforcement for decades.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_curial_authority, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_curial_authority, agenda_setter).

% Scholars and clergy arguing for the continuity or traditionalist readings were frequently marginalized from seminary faculties, official liturgical commissions, and mainstream theological publishing during the period when the progressive-rupture reading held institutional dominance. Their objection — that 'spirit' claims exceed textual warrant — was rarely given equal institutional standing to contest implementation decisions in real time.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_and_continuity_scholars, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_episcopal_conferences).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutic that lets a large, geographically dispersed institution adapt rapidly to mid-20th-century social change (vernacular liturgy, ecumenical engagement, religious liberty) without requiring line-by-line textual warrant for every pastoral adjustment, coordinating a generation of reform under a single interpretive banner.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional resources (seminary formation, liturgical control, publishing platforms, religious-order governance) away from actors committed to pre-conciliar forms and toward actors and institutions committed to open-ended post-conciliar development, using 'conciliar intent' as the justifying transfer mechanism rather than explicit textual mandate.
% ABSENT_VOICES: Traditionalist clergy, religious communities dissolved or reformed under implementation mandates, and lay faithful attached to prior liturgical forms rarely had a seat in the episcopal and academic bodies setting the pace and content of 'spirit of the Council' implementation; their objections were addressed, when at all, well after major changes were institutionalized.
% DISAPPEARANCE_RATIONALE: Progressive institutional actors would say the underlying reforms (vernacular liturgy, ecumenical posture, collegiality) are now settled facts of Church life independent of the 'spirit' framing, so its disappearance would change little. Traditionalist and continuity-reading actors would say that without the 'spirit' warrant, a substantial share of post-conciliar implementation decisions would lose their claimed textual authorization and become contestable again — a genuine rearrangement of authority claims, even if the practices themselves persisted by inertia.
% FOUNDING_PROBLEM: The Church faced a genuine adaptation problem in the mid-20th century: liturgical forms, ecclesial structures, and relations to other faiths and to modern political orders had accumulated centuries of encrustation that many bishops and theologians judged unsuited to engaging contemporary culture and the lay faithful; the Council was convened to address this.
% FOUNDING_PROBLEM_CORROBORATION: Progressive episcopal conferences and academic theological faculties attest the adaptation problem remains live and ongoing development is still required. Independent historians of the Council period and later magisterial statements (including papal correctives to 'the Council of the media' framing) attest from outside the beneficiary set that much of what was implemented under the 'spirit' rubric exceeded what the founding problem actually required, and that the founding adaptation problem was substantially resolved by the textual reforms alone.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 — substantial but not extreme — because the reading does carry a genuine coordination function (rapid, coherent institutional adaptation to modern conditions was a real problem) alongside asymmetric costs imposed on those attached to prior forms who had no voice in the pace or content of change. Suppression (0.47) reflects that the reading operated less through direct coercion than through control of formation pipelines, publishing, and episcopal appointment — soft-power exclusion of dissenting voices from institutional standing rather than formal condemnation. Theater ratio (0.34) captures that a meaningful share of 'spirit of the Council' invocation in later decades became performative justification for administrative decisions already made on other grounds, without the original founding urgency. Accessibility collapse is moderate (0.40): traditionalist and continuity alternatives remained visible and articulable throughout, never fully suppressed, which is why this is authored as tangled_rope rather than snare. Resistance is high (0.72) because organized pushback (traditionalist movements, later papal correctives, the 2007 and subsequent liturgical reconsiderations) has been persistent and institutionally consequential.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive episcopal conferences and allied academic/liturgical institutions sit near the beneficiary end: they set the implementation agenda, gained institutional capital, and retain arbitrage-grade exit (they can shift interpretive emphasis as needed). Traditionalist clergy, disbanded religious orders, and attached laity sit near the target end: trapped or constrained exit, no independent recourse against implementation decisions framed as authentic conciliar intent. The curial authority is split — sometimes ratifying, sometimes correcting the rupture reading — which is why it is given both observer and agenda_setter roles rather than a clean beneficiary or payer designation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mid-century pastoral and liturgical adaptation) was substantially addressed by the mid-1970s through the textual reforms themselves; the ongoing invocation of 'the spirit of the Council' to authorize further, non-textually-warranted change in later decades is where mandatrophy risk concentrates — the mandate (adapt to genuine 1960s conditions) persists as institutional practice (open-ended interpretive license) well past the conditions that motivated it. The founding_problem_status is marked contested precisely because progressive-reading beneficiaries assert ongoing need while independent and traditionalist-adjacent corroboration asserts the problem was resolved and the license has outlived its warrant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_intent_or_accident,
    'Were the Council documents'' ambiguous or open-textured passages deliberately crafted as compromise language authorizing later development, or are they ordinary drafting ambiguities later read expansively to license changes the text does not actually warrant?',
    'Examination of conciliar drafting history (relatio texts, floor debates, the mens of the periti and bishops as recorded in acta) to determine whether openness to later development was an articulated drafting goal versus a retrospective interpretive imposition.',
    'If deliberate, the progressive reading has a stronger textual warrant and the constraint looks more like genuine coordination (rope-leaning); if retrospective imposition, the extraction component is larger and the tangled_rope classification undersells the extractive share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ambiguity_intent_or_accident, empirical, 'Whether textual openness was intended by the Council or imposed by later interpreters.').

omega_variable(
    dignitatis_humanae_reversal_or_development,
    'Does Dignitatis Humanae''s teaching on religious freedom constitute a genuine doctrinal reversal of the Syllabus of Errors'' condemnations, or a development consistent with underlying continuous principles applied to changed political circumstances?',
    'Close doctrinal-historical comparison of the propositions actually condemned in the Syllabus against the specific claims affirmed in Dignitatis Humanae, adjudicated against the technical criteria magisterial teaching itself uses to distinguish reversal from development.',
    'This is the central empirical fork between the rupture readings and the continuity reading; it does not resolve within this story but is the single largest driver of which sibling reading is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignitatis_humanae_reversal_or_development, conceptual, 'Central doctrinal-historical dispute underlying the whole kernel contest.').

omega_variable(
    spirit_versus_letter_enforcement_asymmetry,
    'To what extent did ''spirit of the Council'' implementation decisions that harmed traditionalist and lay-conservative parties receive less institutional scrutiny than decisions harming progressive interests, given that progressive actors largely controlled the implementing bodies?',
    'Comparative institutional-history review of contested implementation episodes (seminary closures, religious-order reforms, liturgical suppressions) versus episodes where progressive proposals were checked by curial intervention, to assess enforcement symmetry.',
    'A documented asymmetry would support treating suppression as understated in this story (favoring reclassification toward snare for the specific implementation-enforcement mechanism); a genuine two-directional check would support the tangled_rope classification as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_versus_letter_enforcement_asymmetry, empirical, 'Whether enforcement of the progressive reading was symmetrically or asymmetrically applied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1985, 0.33).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2010, 0.34).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2025, 0.34).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2010, 0.57).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1965, 0.3).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1975, 0.45).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2010, 0.46).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2025, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the vatican_ii_doctrinal_authority kernel. Each reading is authored as a structurally distinct constraint with its own epsilon: composite_overdetermination_reading treats 'Vatican II reform' as several independently-caused structural changes bundled under one label (likely the lowest epsilon, closest to a genuine multi-rope coordination story); continuity_reading denies doctrinal reversal occurred and treats novelty as explication (low epsilon, closer to rope or mountain-adjacent for the doctrinal-continuity claim specifically); this reading (rupture_progressive_reading) treats rupture as necessary and licenses ongoing extra-textual development (moderate-high epsilon, tangled_rope, as authored here); rupture_traditionalist_reading agrees rupture occurred but treats it as corruption enabling heterodox capture (likely highest epsilon, snare-leaning, with an inverted beneficiary/victim structure relative to this file). No story averages across the others; each stands alone and is linked here for contamination-propagation and family-tracing purposes only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
