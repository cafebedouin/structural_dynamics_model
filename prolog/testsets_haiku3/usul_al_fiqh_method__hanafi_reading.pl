% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Expansive Qiyas and Istihsan Framework
 *   domain: legal/religious
 *
 * SUMMARY:
 *   The Hanafi school of Islamic jurisprudence grounds its authority in a
 *   distinctive methodological approach to deriving law when textual sources
 *   are silent. Qiyas (analogical reasoning) is applied expansively; ra'y
 *   (reasoned jurist opinion) supplements where analogy reaches limits;
 *   istihsan (juristic preference for public interest) permits departure from
 *   strict analogy. This reading is ONE instantiation of the contested kernel
 *   'usul al-fiqh method' — the foundational principles governing legal
 *   derivation. The Hanafi reading instantiates the rationalist pole: lowest
 *   textual restrictiveness, highest scope for jurist-driven reasoning.
 *   Sibling readings (Hanbali, Maliki, Shafi'i) operate with tighter textual
 *   constraint or different source hierarchies. This constraint story
 *   describes the Hanafi framework as its jurists and beneficiaries
 *   understand it, with the key structural asymmetry: jurists gain
 *   professional authority and discretionary power; textualist interpretive
 *   claims are systematically subordinated; novel legal questions migrate to
 *   jurist reasoning rather than remaining textually regulated.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.45).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Jurisprudential Method: Expansive Qiyas and Istihsan Framework").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "legal/religious").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, 'db846ef9-e401-40e4-875a-101e90c80414').
narrative_ontology:cs_kernel_codification('db846ef9-e401-40e4-875a-101e90c80414', fixed_text).
narrative_ontology:cs_authority_grounding('db846ef9-e401-40e4-875a-101e90c80414', lineage).
narrative_ontology:cs_interpretation_layer_present('db846ef9-e401-40e4-875a-101e90c80414').
narrative_ontology:cs_reading_relation('db846ef9-e401-40e4-875a-101e90c80414', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_reading_relation('db846ef9-e401-40e4-875a-101e90c80414', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('db846ef9-e401-40e4-875a-101e90c80414', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('db846ef9-e401-40e4-875a-101e90c80414', foundational, jurist_reason_coequal_with_text).
narrative_ontology:cs_axiom_status(jurist_reason_coequal_with_text, holdable).
narrative_ontology:cs_axiom_grounding('db846ef9-e401-40e4-875a-101e90c80414', jurist_reason_coequal_with_text, deontological).
narrative_ontology:cs_axiom('db846ef9-e401-40e4-875a-101e90c80414', foundational, istihsan_overrides_strict_qiyas).
narrative_ontology:cs_axiom_status(istihsan_overrides_strict_qiyas, holdable).
narrative_ontology:cs_axiom_grounding('db846ef9-e401-40e4-875a-101e90c80414', istihsan_overrides_strict_qiyas, instrumental).
narrative_ontology:cs_reference_frame('db846ef9-e401-40e4-875a-101e90c80414', classical_hanafi_jurisprudential_authority).
narrative_ontology:cs_drift_state('db846ef9-e401-40e4-875a-101e90c80414', contemporary_islamic_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('db846ef9-e401-40e4-875a-101e90c80414', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, rationalist_legal_tradition).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_interpretive_claim).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, non_hanafi_legal_schools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, muslim_communities_under_hanafi_law).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, legal_innovation_seekers).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, muslim_communities_under_hanafi_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jurists trained in the Hanafi school set the interpretive canon by applying qiyas expansively, supplementing with ra'y when analogy reaches limits, and invoking istihsan to depart from strict analogy for public interest. They administer the framework's evolution, train successors in the method, and generate legal rulings (fatawa) that perpetuate the approach across centuries and geographies. Their professional identity and scholarly lineage are constituted through mastery of this specific jurisprudential apparatus.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, civilizational, identity_locked, global).

% The broader rationalist (Mu'tazilite-influenced) legal epistemology benefits from the Hanafi framework's legitimation of human reason as a primary source of legal derivation. The method vindicates the philosophical position that reason, not textual literalism alone, grounds Islamic jurisprudence. This reading expands the scope for jurist-driven innovation and philosophical refinement.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rationalist_legal_tradition, beneficiary,
    institutional, civilizational, analytical, global).

% The textualist position — that textual sources (Quran and authenticated hadith) are maximally restrictive and qiyas should be minimized — bears the cost of being subordinated within the Hanafi framework. Textualist hermeneutics must justify their constraints against the jurist's expansive interpretive prerogatives. The framework's rules actively exclude pure textualist restriction as a controlling principle.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_interpretive_claim, payer,
    powerful, civilizational, constrained, global).

% Hanbali, Maliki, and Shafi'i schools operate under different methodological constraints — tighter textual restriction, different source weightings, different istihsan permissions. They bear the cost of defending their narrower jurisprudential scope against Hanafi expansion. They are excluded from setting the terms of Hanafi reasoning but compete in the broader legal marketplace for adherents and jurisdictional authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, non_hanafi_legal_schools, payer,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, non_hanafi_legal_schools, excluded).

% Communities living under Hanafi jurisprudence receive comprehensive rulings that adapt to changing circumstances through istihsan and ra'y — law remains responsive to novelty. They also bear the cost of jurisprudential discretion: where strict analogy is supplanted by jurist preference, predictability and textual accessibility decrease. Exit is constrained by geography, institutional authority, and religious belonging.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, muslim_communities_under_hanafi_law, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, muslim_communities_under_hanafi_law, payer).

% Scholars advocating strict textual restriction and minimal qiyas are not seated at the Hanafi jurisprudential table; their interpretive claims are systematically overruled. They would argue for legal constraint and textual fidelity but are excluded by the framework's epistemic rules. Their objections do not reset the jurist's authority to invoke istihsan.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_legal_scholars, excluded,
    powerful, civilizational, constrained, global).

% Actors seeking to establish new legal positions (merchants, administrators, reformers) benefit from the framework's permission to depart from strict analogy via istihsan for public interest. They gain interpretive pathways that textualist schools foreclose. Their ability to arbitrage between jurisdictions — seeking out Hanafi authorities for favorable rulings — is enabled by the method's expansive scope.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, legal_innovation_seekers, beneficiary,
    powerful, biographical, arbitrage, global).

% The Quran and authenticated hadith collections are the epistemic foundation all readings claim to build upon, but the Hanafi reading interprets their scope narrowly — their silence on a matter is framed as permission for jurist reasoning, not constraint. The Hanafi reading redefines what textual silence means: not a gap to be avoided, but a space for reasoned extension.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, foundational_textual_tradition, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, hanafi_jurists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, adaptive jurisprudential method that permits communities to resolve novel legal questions without requiring new textual revelation. The framework coordinates lawgiving authority around a stable set of interpretive rules (qiyas, ra'y, istihsan) that allow jurists to extend Islamic law to circumstances the foundational texts do not explicitly address.
% TRANSFER_FUNCTION: Moves interpretive authority from the text (or the most restrictive reading of it) to the jurist class trained in rationalist reasoning. Communities gain regulatory adaptation; textualist constraint is subordinated. The jurist gains professional status, pedagogical authority, and the power to shape law through discretionary reasoning.
% ABSENT_VOICES: Textualist scholars and strict literalist traditions are structurally excluded from the Hanafi jurisprudential conversation — their objection to expansive qiyas and istihsan is not answered within the framework but overruled by it. Communities preferring legal constraint and textual transparency are not consulted on whether jurist discretion should be preferred to textual restriction.
% DISAPPEARANCE_RATIONALE: If the Hanafi framework's permission for expansive qiyas, ra'y, and istihsan vanished, legal authority would migrate to textualist schools (Hanbali/Shafi'i dominance) or to formal legislative process outside jurisprudence. The jurisprudential profession would lose professional prerogative; novel legal questions would be resolved by restricted analogy or by silence. Communities would reorganize around tighter textual constraint or external law-making authority.
% FOUNDING_PROBLEM: Early Islamic law faced an urgent epistemic crisis: the foundational texts (Quran and hadith) could not be continuously reauthored, yet new social circumstances arose that the texts did not explicitly address. How could Islamic law remain authoritative while adapting to novelty? The Hanafi solution was to permit jurists trained in rationalist reasoning to extend the law through analogy, reason, and juristic preference when textual sources were silent.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi jurists and rationalist scholars attest the founding problem is still live: novelty arises faster than textual hermeneutics can settle it; jurist reasoning remains necessary. Textualist and Hanbali scholars attest the problem is misframed: they argue the founding texts are sufficiently comprehensive and that jurist expansion beyond them violates the principle of textual fidelity. Comparative legal historians and Islamic law scholars outside the jurisprudential traditions recognize the problem as genuine but note the Hanafi solution trades constraint for adaptability — a value choice, not a neutral epistemic resolution.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the framework systematically privileges jurist reasoning over textual restriction — textual constraint is structurally subordinated. The beneficiary (hanafi_jurists) collects professional authority and discretionary power; the victims (textualist claims and non-Hanafi schools) bear the cost of interpretive subordination. Suppression is moderate (0.45) because the framework's rules are transmitted through pedagogy and scholarly consensus rather than coercive force — jurist discretion is defended through rational argument and institutional authority, not police power. Theater ratio is low (0.22) because the jurisprudential reasoning is substantive: jurists genuinely apply qiyas, ra'y, and istihsan to novel cases; the method is not primarily performative. The measurement series shows extractiveness rising from 0.42 to 0.68 over the interval (0–1400 CE): as the Hanafi school consolidated institutional power and jurist discretion became more entrenched in Ottoman and Indian Islamic law, the framework's extraction of authority from textualist constraint to jurist reasoning intensified. Theater rose modestly (0.08 to 0.22) as commentarial layers accumulated — more attention to justifying departures from strict analogy, more rhetoric around 'protecting public interest,' less direct jurisprudential work. Suppression remained stable (0.38 to 0.45) because textualist objections were never truly silenced, only overruled — the framework required constant institutional maintenance to keep textualist alternatives subordinated.
 *
 * PERSPECTIVAL GAP:
 *   The Hanafi jurist seat and the textualist seat will compute dramatically different types from the same constraint structure. The jurist sees a coordination mechanism that permits lawgiving to continue despite textual silence — a Rope or even a necessary infrastructure. The textualist sees the subordination of textual authority to human discretion — a Snare that extracts fidelity from the texts in exchange for jurist prerogative. This divergence is exactly what the per-seat classification system is designed to capture. The authored claim (Tangled Rope) reflects the structure from the coordinating seat's perspective: yes, there is genuine coordination (adaptation to novel circumstances), but it is asymmetric (jurist authority vs. textual constraint). The textualist would classify this as pure extraction (Snare). The engine's per-seat computation should reveal this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The Hanafi jurists sit at d near 0.0 (full beneficiary): they set the agenda, administer the framework, control access to professional authority, and capture the discretionary power the method generates. The textualist interpretive claim sits at d near 1.0 (full target): its constraint-generating potential is systematically subordinated by the framework's epistemic rules. Non-Hanafi schools sit at d near 0.7 (high target): they operate under different constraints and lose adherents and jurisdictional reach where Hanafi authority is dominant. Muslim communities sit at d near 0.5 (symmetric): they gain adaptive law but lose constraint and textual transparency; the benefit and cost are comparable. The measurement of directionality from the Hanafi jurist seat and the textualist seat should diverge sharply: from the jurist's position, istihsan and ra'y are essential tools for justice and adaptation; from the textualist's position, they are unauthorized departures from the foundational texts. The asymmetry is structural: one seat has interpretive authority, the other does not.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanafi framework resolves the potential Tangled Rope / Snare boundary: it claims to coordinate (adaptation to novel law) while it systematically extracts authority from textualist constraint. Mandatrophy does not apply here because the founding problem (novelty requires adaptation) remains live and contested — textualists argue the problem is misframed (texts are sufficient). The framework's persistence does not depend on the founding problem's obsolescence; it depends on the Hanafi community's continuous institutional maintenance of the jurist's authority. If textualists succeeded in demonstrating that textual sources are comprehensive enough without jurist discretion, the framework would lose legitimacy — but that would be a cognitive change, not a mandate expiration. Currently the framework shows zero mandatrophy: its founding problem is disputed but the arrangement persists because the beneficiary (jurist class) actively maintains it and the alternative (textualist constraint) remains contested rather than defeated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_sufficiency_empirical,
    'Are the foundational Islamic texts (Quran and authenticated hadith) genuinely insufficient to derive Islamic law for novel circumstances, or can textualist hermeneutics extract answers from the texts without supplementary jurist reasoning?',
    'Comparative analysis of novel legal questions: enumerate categories of issues that arose after the foundational texts were codified (e.g., inheritance law for contemporary marriage forms, financial law for modern instruments) and assess whether textualist hermeneutics can derive determinate answers without invoking jurist discretion. If textualists consistently produce determinate, non-contradictory rulings using only qiyas from explicit precedents, the Hanafi claim to necessity is weakened.',
    'If textualist hermeneutics proves sufficient, the Hanafi framework''s extraction of authority to the jurist class is revealed as a choice (rationalist preference) rather than a necessity (epistemic gap). Extraction remains high but the legitimacy frame shifts from ''coordination'' to pure ''authority transfer.'' If textualist hermeneutics produces contradictions or gaps, the coordination claim is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_sufficiency_empirical, empirical, 'Whether textual sources are insufficient or textualist hermeneutics can fill apparent gaps.').

omega_variable(
    istihsan_as_mask_or_instrument,
    'Does istihsan (juristic preference for public interest) serve as a genuine constraint on jurist discretion (requiring reasoned justification for departing from strict analogy), or does it function as a post-hoc cover story that permits jurists to reach predetermined outcomes?',
    'Doctrinal analysis: examine cases where Hanafi jurists invoke istihsan and assess whether the public-interest justification is logically anterior to the ruling or post-hoc rationalization. Compare istihsan invocations with non-Hanafi schools'' rejections of the same rulings: where Hanafi jurists cite ''public interest,'' do Hanbali textualists demonstrate the ruling contradicts the texts'' intent? If contradictions are structural, istihsan is more mask than constraint.',
    'If istihsan functions as genuine constraint, the suppression metric should be recalibrated upward — jurist discretion is bounded. If istihsan is consistently a post-hoc cover, effective extraction is higher than measured (hidden discretion amplifies extraction). The theater ratio might also rise (performative justification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_as_mask_or_instrument, conceptual, 'Whether istihsan constrains jurist discretion or masks unconstrained authority.').

omega_variable(
    reading_foreclosure_via_religious_authority,
    'Within a single Islamic community, can the Hanafi reading and the Hanbali reading coexist as equally valid jurisprudential options, or does institutional authority pressure adherents toward one reading and away from the other?',
    'Historical analysis of fatwa networks, institutional affiliations, and educational transmission: examine whether Ottoman, Indian, or contemporary Islamic communities maintain genuine doctrinal pluralism across schools or whether political/institutional authority concentrates adherents in one school. Assess whether conversion between schools is encouraged or discouraged by institutional mechanisms.',
    'If coexistence is genuine, the readings are structurally in ''coexists_with'' relation (not ''forecloses''). If institutional authority forecloses alternatives, the relation migrates toward ''forecloses'' — the Hanafi reading''s institutionalization has made other readings structurally unavailable in some communities. This affects the strategic structure: coexistence is low-conflict; foreclosure is high-stakes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_via_religious_authority, empirical, 'Whether jurisprudential readings coexist pluralistically or are institutionally foreclosed.').

omega_variable(
    jurist_rationalism_vs_authority_capture,
    'Is the Hanafi emphasis on jurist reasoning (ra''y) grounded in a genuine epistemology that human rationality can derive law from foundational principles, or is it primarily a mechanism by which institutional jurists capture authority for themselves?',
    'Philosophical analysis: examine whether Hanafi epistemology articulates a defensible theory of reason''s role in law, or whether the rationalist framing is post-hoc justification for institutional authority consolidation. Compare with non-Hanafi schools'' philosophical critiques of expansive ra''y. Assess whether Hanafi epistemology has survived external philosophical challenge or has been abandoned as indefensible.',
    'If rationalist epistemology is genuine, the framework is more legitimately classifiable as Tangled Rope (real coordination + real asymmetry). If rationalism is cover story for authority capture, the framework is more Snare-like. The beneficiary structure shifts: if epistemology is genuine, hanafi_jurists and rationalist_legal_tradition are genuine beneficiaries; if cover story, they are mere extractors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurist_rationalism_vs_authority_capture, conceptual, 'Whether jurist rationalism is an epistemology or a capture mechanism.').

omega_variable(
    suppression_internalization_in_adherent_communities,
    'Does the Hanafi framework''s authority over adherent communities rest on external enforcement (institutional control, coercive power) or on internalized acceptance (communities genuinely prefer the jurist''s discretionary method)?',
    'Ethnographic and historical analysis: examine whether communities following Hanafi law do so because institutional power enforces it, or because they find the method legitimate and adaptive. Assess whether communities exit Hanafi law when institutional enforcement weakens, or whether they maintain adherence from internal acceptance. Survey contemporary communities for satisfaction with jurist-driven discretion vs. preference for textual constraint.',
    'If suppression is primarily external/institutional, the measured suppression (0.45) is accurate structural coercion. If suppression is internalized (communities have absorbed the jurist''s authority as legitimate), the effective suppression is lower — the framework persists through consensus rather than force. This affects strategic stability: internal suppression is more robust; external suppression is fragile to institutional weakening.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_in_adherent_communities, empirical, 'Whether suppression of textualist alternatives is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method__hanafi_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement(usul_tr_t400, usul_al_fiqh_method__hanafi_reading, theater_ratio, 400, 0.15).
narrative_ontology:measurement(usul_tr_t700, usul_al_fiqh_method__hanafi_reading, theater_ratio, 700, 0.19).
narrative_ontology:measurement(usul_tr_t1000, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1000, 0.21).
narrative_ontology:measurement(usul_tr_t1400, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1400, 0.22).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 200, 0.51).
narrative_ontology:measurement(usul_be_t400, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 400, 0.58).
narrative_ontology:measurement(usul_be_t700, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 700, 0.64).
narrative_ontology:measurement(usul_be_t1000, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1000, 0.66).
narrative_ontology:measurement(usul_be_t1400, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1400, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 200, 0.4).
narrative_ontology:measurement(usul_su_t400, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 400, 0.42).
narrative_ontology:measurement(usul_su_t700, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 700, 0.44).
narrative_ontology:measurement(usul_su_t1000, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1000, 0.45).
narrative_ontology:measurement(usul_su_t1400, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1400, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% The kernel 'usul_al_fiqh_method' (foundational principles of Islamic jurisprudence) decomposes into four structurally distinct constraints, one per jurisprudential school. Each reading instantiates a different ε, beneficiary/victim structure, and authority grounding. The Hanafi reading (this file) emphasizes expansive qiyas and jurist discretion — lowest textual restrictiveness, highest extraction to jurist class. Hanbali emphasizes textual maximalism and qiyas minimization — highest textual restrictiveness, lowest extraction to jurist class. Maliki integrates Medinan practice and maslaha — moderate restrictiveness, balanced extraction. Shafi'i systematizes hadith authentication — moderate restrictiveness, proceduralized extraction. Each reading is held by different institutional communities and historical periods; they coexist in the jurisprudential marketplace without logically foreclosing each other, though institutional authority creates competitive pressure. All four readings link via affects_constraints; sibling readings should include cross-references.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__hanafi_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
