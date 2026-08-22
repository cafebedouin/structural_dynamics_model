% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: CA3 Scope via ICRC Customary-Law Tracking (Procedural Coordination Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint is the ICRC customary-law reading of the Common Article 3
 *   scope kernel: rather than fixing CA3's applicability threshold in the
 *   treaty text itself (state-centric reading) or reading it as an open floor
 *   for any organized armed violence (expansive human rights reading), this
 *   reading treats scope as a question resolved procedurally — through
 *   accumulation and documentation of state practice and opinio juris over
 *   time, chiefly via the ICRC's customary IHL study and its ongoing updates.
 *   The coordination function is real: it lets 190+ treaty parties avoid the
 *   near-impossible task of renegotiating Geneva Convention text every time a
 *   new conflict form emerges. But the mechanism that performs this
 *   coordination also concentrates interpretive power in institutions with
 *   the infrastructure to generate and document 'practice' (states,
 *   tribunals, the ICRC itself), while the populations whose protection is
 *   actually at stake — armed group fighters, civilians, detainees in
 *   classification limbo — have no comparable evidentiary standing. That
 *   asymmetry is why this reading, despite its genuine procedural utility,
 *   computes as a tangled rope rather than a clean rope: it coordinates real
 *   interests (states avoiding treaty renegotiation, tribunals needing
 *   citable authority) while extracting protective certainty from parties who
 *   cannot contribute to the record that determines their own protection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.38).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.42).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "CA3 Scope via ICRC Customary-Law Tracking (Procedural Coordination Reading)").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__icrc_customary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, '5f11ce26-0e44-434d-acae-22eff9688a47').
narrative_ontology:cs_kernel_codification('5f11ce26-0e44-434d-acae-22eff9688a47', distributed).
narrative_ontology:cs_authority_grounding('5f11ce26-0e44-434d-acae-22eff9688a47', practice).
narrative_ontology:cs_interpretation_layer_present('5f11ce26-0e44-434d-acae-22eff9688a47').
narrative_ontology:cs_reading_relation('5f11ce26-0e44-434d-acae-22eff9688a47', common_article_3_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('5f11ce26-0e44-434d-acae-22eff9688a47', common_article_3_scope__expansive_human_rights_reading, influences).
narrative_ontology:cs_axiom('5f11ce26-0e44-434d-acae-22eff9688a47', foundational, scope_determined_by_evolving_practice_not_fixed_text).
narrative_ontology:cs_axiom_status(scope_determined_by_evolving_practice_not_fixed_text, holdable).
narrative_ontology:cs_axiom_grounding('5f11ce26-0e44-434d-acae-22eff9688a47', scope_determined_by_evolving_practice_not_fixed_text, conventional).
narrative_ontology:cs_axiom('5f11ce26-0e44-434d-acae-22eff9688a47', secondary, gradual_customary_expansion_preferred_to_treaty_amendment).
narrative_ontology:cs_axiom_status(gradual_customary_expansion_preferred_to_treaty_amendment, holdable).
narrative_ontology:cs_axiom_grounding('5f11ce26-0e44-434d-acae-22eff9688a47', gradual_customary_expansion_preferred_to_treaty_amendment, instrumental).
narrative_ontology:cs_reference_frame('5f11ce26-0e44-434d-acae-22eff9688a47', geneva_1949_undefined_threshold).
narrative_ontology:cs_drift_state('5f11ce26-0e44-434d-acae-22eff9688a47', post_tadic_customary_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5f11ce26-0e44-434d-acae-22eff9688a47', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc_and_treaty_depositaries).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, states_with_stable_military_doctrine).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, international_criminal_tribunals).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, civilians_in_ambiguous_conflicts).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, detainees_in_contested_classification_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compiles state practice and opinio juris into customary law studies (notably the 2005 ICRC Customary IHL Study), publishes interpretive commentaries, and convenes expert meetings that shape how CA3's scope is read. Does not adjudicate individual cases but its compilations are cited as authoritative by tribunals and states, giving it outsized interpretive leverage without formal treaty-amendment authority.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc_and_treaty_depositaries, agenda_setter,
    institutional, civilizational, analytical, global).

% Have legal departments and military lawyers who track ICRC compilations and shape their own practice to influence future customary readings. Benefit from a scope-determination process that moves gradually and predictably, avoiding sudden treaty renegotiation, and can characterize their own conduct as evidence of (or against) an emerging norm.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_with_stable_military_doctrine, beneficiary,
    institutional, generational, arbitrage, global).

% Rely on ICRC customary-law compilations as evidentiary shortcuts when ruling on CA3 applicability in prosecutions, avoiding the need to independently reconstruct state practice for every case. Gains a workable interpretive resource, but inherits whatever gaps or state-favoring skew exists in the underlying compilation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_criminal_tribunals, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, international_criminal_tribunals, observer).

% Have no standing to contribute to opinio juris formation — customary law is built from state practice and state legal opinion, so armed groups whose conduct is directly governed by the scope question cannot participate in defining it. Whether CA3 protections attach to their fighters and detainees depends on a practice record they cannot author or contest.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, non_state_armed_groups, payer,
    powerless, immediate, trapped, regional).

% Live inside conflicts whose classification (internal disturbance vs. non-international armed conflict) is unresolved for years while state practice accumulates. Their access to humanitarian protections during that ambiguity window depends on a slow-moving customary determination they have no mechanism to accelerate or invoke directly.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, civilians_in_ambiguous_conflicts, payer,
    powerless, immediate, trapped, local).

% Held by state or non-state forces in conflicts where classification is contested; their entitlement to CA3's minimum guarantees (humane treatment, judicial guarantees, prohibition of violence) can be denied by the detaining authority pending resolution of a scope question that may never be authoritatively settled in their lifetime of detention.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, detainees_in_contested_classification_zones, payer,
    powerless, immediate, trapped, local).

% Document violations and argue for expansive readings of CA3's floor, but their advocacy does not itself constitute state practice or opinio juris under the customary-law methodology — they can influence the discourse but are structurally excluded from the formal evidentiary record that determines scope.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, human_rights_ngos, excluded,
    organized, generational, constrained, global).

% Analyze the customary-law methodology itself, critique selection bias in which state practice gets counted, and track how the ICRC's compilations interact with tribunal jurisprudence and state objections (persistent objector doctrine).
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, academic_ihl_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__icrc_customary_reading, diffuse).
narrative_ontology:fixing_cost_class(common_article_3_scope__icrc_customary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for CA3's scope to evolve and be authoritatively documented without requiring the 190+ states party to the Geneva Conventions to renegotiate treaty text — customary law tracking lets practice-based consensus crystallize into recognized legal content incrementally.
% TRANSFER_FUNCTION: Moves interpretive authority over who counts as protected under CA3 from a fixed textual threshold toward a rolling record of state conduct and legal opinion; this shifts the burden of establishing protection onto whichever party can marshal evidence of accepted practice, favoring parties with legal infrastructure (states, tribunals, the ICRC) over parties without it (armed groups, civilians, detainees).
% ABSENT_VOICES: Non-state armed groups and civilians have no formal channel to contribute to opinio juris; human rights NGOs document extensively but their submissions are not themselves state practice. Their exclusion means the customary record can lag or diverge from lived humanitarian need for extended periods.
% DISAPPEARANCE_RATIONALE: States and tribunals would say the underlying treaty text and case law would remain and scope questions would simply be litigated ad hoc without the ICRC compilation shortcut — a procedural loss but not a substantive one. Advocates for affected populations would say the loss of a mechanism for gradual, evidence-based expansion would freeze scope determination at whatever the last authoritative reading was, removing the primary lawful pathway for extending protection to new conflict forms (cyber conflict, transnational non-state violence) without formal treaty amendment, which is politically near-impossible to achieve.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions text left 'armed conflict not of an international character' undefined, and the drafters could not anticipate every future conflict form; customary international law tracking was adopted as the mechanism to let CA3's scope adapt without reopening treaty negotiation, which requires near-universal state consent and is realistically foreclosed.
% FOUNDING_PROBLEM_CORROBORATION: International Court of Justice jurisprudence (Nicaragua v. United States, 1986) and the ICTY Tadić decision independently affirm that CA3 reflects customary law and that its scope has evolved through practice — corroboration from adjudicative bodies outside the ICRC itself. However, academic critiques (e.g., persistent-objector scholarship and critical IHL scholarship) note that the practice record disproportionately reflects state military lawyers' submissions and that affected populations and armed groups have no comparable evidentiary footprint, meaning the 'live problem' framing is corroborated for states and tribunals but contested as adequate by scholars documenting whose practice counts.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, contested).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) and has risen slowly since 1949 as the customary-law apparatus has grown more elaborate and more frequently dispositive in tribunal rulings, without any corresponding growth in avenues for affected populations to shape the record. Suppression (0.42) reflects the structural exclusion of non-state and civilian voices from opinio juris formation — this is not coercive suppression but methodological suppression: the customary-law method itself only counts certain actors' conduct and statements as evidence. Theater ratio (0.30) captures the growing gap between the appearance of a rigorous, evolving legal science (the ICRC study, expert meetings, extensive citation apparatus) and the underlying reality that scope determinations remain contested and slow, leaving real humanitarian gaps unaddressed for years. Accessibility collapse (0.45) is moderate — states retain some ability to be persistent objectors or advance rival practice, so the constraint has not fully foreclosed contestation. Resistance (0.55) is substantial: academic critique, NGO advocacy, and rival kernel readings (state-centric and expansive) actively contest this procedural framing.
 *
 * DIRECTIONALITY LOGIC:
 *   States with stable military legal infrastructure and the ICRC/tribunal complex sit near the beneficiary end: they control or heavily influence what counts as 'practice,' and the gradualism of the mechanism suits institutional actors who prefer predictable, incremental change over sudden treaty renegotiation. Non-state armed groups, civilians, and detainees sit near the full-target end: the constraint's operation directly determines whether they receive CA3's protections, yet they have no mechanism to contribute to, contest, or accelerate the customary determination that governs them — their exit options are trapped by definition (armed conflict does not allow opting out of the classification question). Human rights NGOs occupy an intermediate position: organized and vocal, but structurally excluded from the formal evidentiary record despite extensive documentation efforts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (treaty text cannot anticipate every conflict form; renegotiation is politically foreclosed) remains live by the corroboration of international tribunals, which is why this is not simply a mandatrophy case — the customary-law mechanism continues to perform genuine adaptive work (e.g., extending recognition to certain non-international conflict forms not contemplated in 1949). What prevents this reading from being mislabeled as pure extraction is that the coordination function is independently verifiable: ICJ and ICTY jurisprudence did not need the ICRC's institutional interest to conclude CA3 reflects customary law. What prevents it from being mislabeled as clean coordination is the persistent, uncorrected asymmetry in whose practice counts — three-quarters of a century in, the evidentiary record still structurally cannot hear from the parties most affected by scope determinations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_process_legitimacy_vs_state_capture,
    'Is the customary-law tracking mechanism a genuinely legitimate, self-correcting coordination process, or is it structurally captured by the states and institutions whose practice constitutes the evidentiary record — i.e., is this reading itself a state-centric reading wearing procedural clothing?',
    'Longitudinal analysis of whether customary-law scope determinations have ever moved AGAINST the preferences of powerful military states, or whether documented expansions have occurred only where they were cost-free or advantageous to major military powers.',
    'If the process has never meaningfully constrained powerful states against their preference, the icrc_customary_reading collapses structurally into the state_centric_reading with extra procedural legitimation — a false-summit-adjacent finding for a tangled_rope, not a mountain, but relevant to whether the ''coordination'' half of the classification is real or cosmetic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_process_legitimacy_vs_state_capture, empirical, 'Whether customary-law tracking is genuinely autonomous coordination or state preference laundered through procedure.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the three kernel readings locate their disagreement — is it about the SOURCE of scope determination (this reading: evolving practice) versus the CONTENT of the threshold (state-centric: intensity/organization; expansive: any organized violence), and can a single legal system coherently hold more than one simultaneously?',
    'Doctrinal analysis of whether tribunals applying the customary method have in practice converged toward either the state-centric or expansive substantive thresholds, which would indicate the procedural reading is not independently load-bearing but is a proxy fight for one of the substantive readings.',
    'If tribunals using customary tracking consistently land on state-centric intensity thresholds, this reading functions as a disguised vehicle for the state-centric reading; if they consistently expand protection, it functions as a disguised vehicle for the expansive reading. Either finding would suggest the procedural framing is doing less independent work than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether the procedural (source-of-scope) reading is independently coherent or a proxy for one of the substantive (content-of-scope) readings.').

omega_variable(
    excluded_party_evidentiary_standing,
    'Could non-state armed groups, civilian populations, or detainees ever be given formal evidentiary standing in customary international law formation for IHL purposes, and would doing so improve or destabilize the mechanism''s coordination function?',
    'Comparative study of analogous expansions of standing in other international law domains (e.g., individual petition rights in human rights law) and their effect on doctrinal stability.',
    'If standing expansion is feasible without destabilizing the coordination function, the current exclusion is a remediable design choice rather than an inherent feature, which would lower the justified extractiveness score in a future measurement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_party_evidentiary_standing, preference, 'Whether excluding affected non-state parties from opinio juris formation is structurally necessary or a policy choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 1949, 0.12).
narrative_ontology:measurement(comm_tr_t1977, common_article_3_scope__icrc_customary_reading, theater_ratio, 1977, 0.16).
narrative_ontology:measurement(comm_tr_t1995, common_article_3_scope__icrc_customary_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(comm_tr_t2005, common_article_3_scope__icrc_customary_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(comm_tr_t2015, common_article_3_scope__icrc_customary_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__icrc_customary_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1949, 0.22).
narrative_ontology:measurement(comm_be_t1977, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1977, 0.26).
narrative_ontology:measurement(comm_be_t1995, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(comm_be_t2005, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2005, 0.33).
narrative_ontology:measurement(comm_be_t2015, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2015, 0.36).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1949, 0.25).
narrative_ontology:measurement(comm_su_t1977, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1977, 0.3).
narrative_ontology:measurement(comm_su_t1995, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1995, 0.34).
narrative_ontology:measurement(comm_su_t2005, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2005, 0.37).
narrative_ontology:measurement(comm_su_t2015, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__icrc_customary_reading, 0.12).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the common_article_3_scope kernel. The state_centric_reading fixes scope to intensity/organization thresholds derived from treaty text and tribunal tests (e.g., Tadić); the expansive_human_rights_reading treats CA3 as an unconditional humanitarian floor; this icrc_customary_reading treats scope as procedurally determined by an evolving evidentiary record. Each reading carries a distinct beneficiary/victim structure and a distinct epsilon: the state-centric reading's epsilon is driven by exclusion of low-intensity violence from protection; the expansive reading's epsilon (if authored) would be driven by state resistance to its broad triggering; this reading's epsilon is driven by asymmetric evidentiary access to the scope-determination process itself. The three are not the same constraint measured three ways — each reading routes a structurally distinct extraction mechanism, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
