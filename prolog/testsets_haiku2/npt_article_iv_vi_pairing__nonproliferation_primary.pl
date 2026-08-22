% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__nonproliferation_primary, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV-VI Pairing: Nonproliferation-Primary Reading
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   The NPT is a 1968 treaty committing non-weapon states to foreswear
 *   nuclear weapons and accept IAEA safeguards on all nuclear material
 *   (Article IV, conditional on Article III verification), in exchange for
 *   weapon states' commitment to disarm (Article VI). Under the
 *   nonproliferation-primary reading, Article IV's conditionality on
 *   safeguards is binding and self-executing; Article VI is aspirational and
 *   non-justiciable (weapon states commit to 'pursue' disarmament, not to
 *   achieve it by a specified date). Authority derives from weapon states'
 *   security interest in preventing horizontal proliferation and managing
 *   deterrence stability. This reading has dominated formal treaty
 *   interpretation since the 1970s and is instantiated in IAEA mandate,
 *   review conference procedural rules, and the UN Security Council's
 *   nonproliferation resolutions. The constraint is CLAIMED as tangled_rope
 *   (genuine coordination function + asymmetric extraction) and the authored
 *   metrics support that claim: extraction is substantial (0.68 at interval
 *   end) because the asymmetry is structural (non-weapon states pay
 *   immediately and perpetually; weapon states' obligations are indefinitely
 *   deferred). Suppression is high (0.71) because the constraint's
 *   persistence depends on excluding or sidelining alternative
 *   interpretations (grand_bargain, abolitionist) from formal institutional
 *   channels.
 *
 * KEY AGENTS:
 *   - Weapon state security establishment (P5 + de facto: India, Pakistan, Israel) — agenda-setter, controls interpretation, collects strategic benefit, perpetually exempt from Article VI timelines
 *   - Non-weapon states (NAM bloc, Arab states, some European states) — organized payers, accept Article IV restrictions conditional on Article VI enforcement that never materializes
 *   - International Atomic Energy Agency — beneficiary and co-agenda-setter, administers Article III verification, institutional survival tied to nonproliferation frame remaining primary
 *   - Disarmament advocates and civil society — excluded from formal interpretation but push grand_bargain and abolitionist readings in review conferences and public discourse
 *   - Emerging proliferation states (Iran, North Korea pre-withdrawal, hypothetical threshold states) — trapped outside the treaty (weapons-acquisition signal) or bound by it (fuel-cycle restraint)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.71).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV-VI Pairing: Nonproliferation-Primary Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '7989d95e-deb9-4fec-902e-4cc520e536ca').
narrative_ontology:cs_kernel_codification('7989d95e-deb9-4fec-902e-4cc520e536ca', fixed_text).
narrative_ontology:cs_authority_grounding('7989d95e-deb9-4fec-902e-4cc520e536ca', extraction).
narrative_ontology:cs_interpretation_layer_present('7989d95e-deb9-4fec-902e-4cc520e536ca').
narrative_ontology:cs_reading_relation('7989d95e-deb9-4fec-902e-4cc520e536ca', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_reading_relation('7989d95e-deb9-4fec-902e-4cc520e536ca', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('7989d95e-deb9-4fec-902e-4cc520e536ca', foundational, horizontal_proliferation_prevention_primary).
narrative_ontology:cs_axiom_status(horizontal_proliferation_prevention_primary, holdable).
narrative_ontology:cs_axiom_grounding('7989d95e-deb9-4fec-902e-4cc520e536ca', horizontal_proliferation_prevention_primary, instrumental).
narrative_ontology:cs_axiom('7989d95e-deb9-4fec-902e-4cc520e536ca', foundational, article_vi_non_justiciable).
narrative_ontology:cs_axiom_status(article_vi_non_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('7989d95e-deb9-4fec-902e-4cc520e536ca', article_vi_non_justiciable, conventional).
narrative_ontology:cs_axiom('7989d95e-deb9-4fec-902e-4cc520e536ca', secondary, weapon_state_security_interest_paramount).
narrative_ontology:cs_axiom_status(weapon_state_security_interest_paramount, holdable).
narrative_ontology:cs_axiom_grounding('7989d95e-deb9-4fec-902e-4cc520e536ca', weapon_state_security_interest_paramount, empirically_contingent).
narrative_ontology:cs_reference_frame('7989d95e-deb9-4fec-902e-4cc520e536ca', two_tier_nonproliferation_order).
narrative_ontology:cs_drift_state('7989d95e-deb9-4fec-902e-4cc520e536ca', contemporary_review_conference_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7989d95e-deb9-4fec-902e-4cc520e536ca', '2026-06-12T14:33:27Z').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_state_security_establishment).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, civil_society_disarmament_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, international_atomic_energy_agency).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, disarmament_advocates_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the interpretation of Article IV's conditionality on safeguards and Article VI's aspirational (non-binding) character. Controls IAEA board composition and verification protocols. Interprets their own arsenals as exempt from disarmament timelines via the security-interest framing. Collects the strategic benefit of maintaining deterrence capacity while constraining rival proliferation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_state_security_establishment, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Accept NPT Article IV restrictions on fuel-cycle development (enrichment, reprocessing) justified as nonproliferation safeguards, conditional on weapon states' disarmament progress under Article VI. That progress has not materialized (arsenals modernized, not reduced). They cannot credibly exit the treaty without signaling weapons intent; staying means accepting permanent restraint while Article VI obligations remain unenforceable against the parties who set the interpretation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states, payer,
    organized, generational, constrained, global).

% Administers Article III verification (safeguards inspections) on non-weapon state civil nuclear programs. Authority is granted by and flows through the nonproliferation-primary reading: Article IV is conditional on passing IAEA verification; Article VI enforcement is outside IAEA's mandate. Institutional survival and budget depend on the nonproliferation frame remaining primary; questioning it weakens the agency's role.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, international_atomic_energy_agency, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, international_atomic_energy_agency, agenda_setter).

% Push for enforcement of Article VI disarmament obligations and argue Article IV's legitimacy depends on reciprocal weapon state restraint. They are excluded from formal treaty review conferences by convention; their argument (grand_bargain and abolitionist readings) remains live in public discourse but powerless in formal interpretation. Their exit is exit from the advocacy space itself, not from the treaty structure.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, disarmament_advocates_civil_society, payer,
    moderate, biographical, constrained, global).

% States with nascent nuclear programs or weapons intent cannot safely join NPT without accepting Article IV restrictions; cannot credibly exit without triggering sanctions and regional destabilization. They are excluded from setting Article IV terms but trapped by them. The nonproliferation-primary reading forecloses their exit options by making safeguards mandatory for entry.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, emerging_proliferation_states, excluded,
    moderate, biographical, trapped, global).

% Non-weapon states collectively attempt to assert Article VI enforcement at five-yearly review conferences. They have produced repeated consensus language calling for disarmament progress but lack enforcement mechanism. The weapon states' nonproliferation-primary interpretation treats Article VI as aspirational; each review cycle reproduces the stalled negotiation without structural change.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, treaty_review_conference_bloc, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_state_security_establishment).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a verifiable, tiered nonproliferation system: non-weapon states accept fuel-cycle restrictions enforced by IAEA inspection; weapon states maintain deterrence arsenals unsupervised. Solves the horizontal proliferation problem by making weapons-relevant technology access conditional on accepting inspection, without requiring vertical disarmament.
% TRANSFER_FUNCTION: Transfers restraint capacity from non-weapon states to weapon states: non-weapon states forgo fuel-cycle autonomy (enrichment, reprocessing capability) and submit to continuous inspection; weapon states retain unrestricted arsenal modernization and disarmament discretion. The transfer is asymmetric time-valuation: non-weapon states pay the cost immediately; weapon states' Article VI obligations are indefinitely deferred.
% ABSENT_VOICES: Emerging proliferation states that want dual-use capability are structurally excluded (trapped outside the treaty if they refuse, bound by it if they join). Disarmament advocates and civil society are present in review conferences but non-voting; they would argue Article IV legitimacy is forfeited if Article VI remains unenforced, but this argument is procedurally sidelined.
% DISAPPEARANCE_RATIONALE: If the nonproliferation-primary reading and its Article IV-VI pairing vanished, non-weapon states would immediately demand fuel-cycle access or withdraw; weapon states would lose the legitimacy framing for nonproliferation safeguards; the IAEA's mandate would collapse; new nuclear states would emerge in regions currently constrained by the treaty (Middle East, Asia-Pacific). The international order around dual-use technology and restraint would reorganize entirely.
% FOUNDING_PROBLEM: Horizontal proliferation: after the Soviet Union and China developed weapons, the risk that other states would acquire them became acute. The NPT was negotiated to prevent new weapons states from emerging while allowing non-weapon states access to peaceful nuclear technology.
% FOUNDING_PROBLEM_CORROBORATION: The weapon state establishment attests the problem is perpetually live and requires indefinite Article IV constraints. Non-weapon states and independent analysts attest the founding problem is partially solved (proliferation did slow), but the constraint now functions to PERPETUATE weapon state privilege rather than solve the original problem, because Article VI remains unenforced. The grand_bargain and abolitionist readings, corroborated by treaty-review conference transcripts and NGO analysis, dispute the nonproliferation-primary interpretation directly.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.42 → 0.68) reflects accumulating asymmetry: as time passed without Article VI enforcement, the nonproliferation-primary reading's central claim (that Article VI enforcement is unnecessary for Article IV legitimacy) became more transparently extractive — non-weapon states paid the Article IV cost without collecting the Article VI benefit. Theater ratio (0.22 → 0.48) rises because the procedural performance at review conferences has intensified (stronger consensus language, more elaborate diplomatic performances calling for 'progress') while the structural outcome remains unchanged — non-weapon states coordinated theater without institutional power to enforce change. Suppression requirement (0.48 → 0.71) rises because the weapon states' interpretation required increasing institutional apparatus to maintain: stricter IAEA board control, procedural rules limiting review-conference amendment power, diplomatic pressure on non-weapon states to accept the nonproliferation frame even when it was transparently asymmetric. The measurements use a single shared time grid (all three metrics authored at each point: 0, 10, 20, 30, 40, 50) so temporal analysis has aligned data.
 *
 * PERSPECTIVAL GAP:
 *   The weapon state security establishment and the IAEA compute the constraint as genuine, necessary coordination: preventing horizontal proliferation and maintaining deterrence stability require permanent non-weapon state restraint and weapon state discretion on disarmament. Non-weapon states compute it as enforced extraction: they accepted Article IV restraint as a bridge to Article VI enforcement; the bridge was never built; they are now permanently constrained without reciprocal benefit. The engine computes per-seat classification from the structural data; the nonproliferation-primary reading's own logic produces this divergence — it is not a failure of the reading but the reading's central function.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states sit at d ≈ 0.1–0.2 (full beneficiaries): they collect strategic benefit (maintained deterrence, constrained rivals) without material cost; they set the interpretation; they have exit options (withdrawal, reinterpretation) but exercise them only to tighten control, not to exit. Non-weapon states sit at d ≈ 0.8–0.9 (full targets): they bear the Article IV cost immediately and perpetually; they have constrained exit (departure signals weapons intent, triggering sanctions); their only leverage is collective review-conference action, which the nonproliferation frame renders procedurally toothless. The IAEA sits at d ≈ 0.3–0.4 (partial beneficiary): it collects institutional authority and budget from administering Article III safeguards; it has some independence but derives legitimacy from weapon states' framework; its power to reinterpret is limited by its dependence on the nonproliferation frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The nonproliferation-primary reading exhibits early-stage mandatrophy: the founding problem (preventing horizontal proliferation) has been substantially achieved (new weapons states are rare; most remain within NPT constraints), but the constraint persists because weapon states derive benefit from perpetuating it. The grandness of Article VI disarmament language (aspirational, non-justiciable) masks the constraint's true function: perpetuating the two-tier international order. Mandatrophy is not yet complete because horizontal proliferation remains a live policy concern, but the asymmetry between founding problem (achieved) and constraint persistence (unabated) is widening. The constraint should be reclassified as piton (inertial performance) if Article VI enforcement attempts have been genuinely exhausted and the weapon states' commitment to disarmament is exclusively theatrical. At present (interval end), it remains tangled_rope because the coordination function (preventing horizontal proliferation) is still real enough to justify the asymmetry, but the theater-ratio rise and suppression-requirement intensification suggest the theater is crowding out the coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_enforceability_ambiguity,
    'Is Article VI''s use of ''pursue'' and ''facilitate'' language a legally binding obligation with implied timelines, or a good-faith aspirational commitment without enforcement mechanism?',
    'International Court of Justice advisory opinion on Article VI binding force; weapon states'' formal reinterpretation of the language; or consensus revision by treaty amendment requiring weapon state ratification (near-impossible, so functionally permanent uncertainty).',
    'If Article VI is binding with implied timelines: the nonproliferation-primary reading''s legal foundation collapses; Article IV''s legitimacy becomes conditional on Article VI enforcement (grand_bargain reading becomes structurally dominant); non-weapon states gain leverage to demand enforcement or amendment. If Article VI remains aspirational: the nonproliferation-primary reading is strengthened; non-weapon states'' only recourse is exit or defection (treaty withdrawal).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_enforceability_ambiguity, conceptual, 'Whether Article VI contains binding disarmament timelines or is purely aspirational.').

omega_variable(
    horizontal_vs_vertical_proliferation_framing,
    'Is the founding problem preventing horizontal proliferation (new weapons states), or preventing the continuation of vertical proliferation (existing arsenals modernizing and expanding)? Does the nonproliferation-primary reading answer the right problem?',
    'Comparative historical analysis of proliferation rates pre- and post-NPT, controlled for other variables (Cold War deterrence, military technology diffusion, sanctions regimes); analysis of whether weapon state arsenal stability or non-weapon state restraint was the binding constraint.',
    'If horizontal proliferation is the primary risk and NPT successfully constrains it: the nonproliferation-primary reading''s focus on Article IV (non-weapon state restraint) is correctly prioritized. If vertical proliferation and weapons modernization pose equal or greater risk and Article VI enforcement is necessary to prevent new weapons races: the grand_bargain and abolitionist readings become empirically dominant; the nonproliferation frame is revealed as rationalization for weapon state privilege.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizontal_vs_vertical_proliferation_framing, empirical, 'Whether the nonproliferation-primary frame addresses the actual binding constraint on global nuclear risk.').

omega_variable(
    iaea_institutional_capture,
    'To what degree is the IAEA''s administration of Article III verification shaped by the nonproliferation-primary reading''s assumption that Article VI is non-enforceable, and would IAEA mandate or operational approach change if Article VI enforcement were formally prioritized?',
    'Documentary analysis of IAEA board decision-making on safeguards standards and enforcement; interview testimony from IAEA technical staff on how Article VI assumptions shape verification protocols; comparison with hypothetical IAEA operations under grand_bargain or abolitionist interpretations.',
    'If institutional capture is high: the IAEA has become a tool of the nonproliferation-primary reading and would vigorously resist alternative interpretations (siding with weapon states on any formal reinterpretation). If capture is partial or contestable: IAEA could shift to dual-track verification (Article III safeguards + Article VI compliance monitoring) under alternative reading dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iaea_institutional_capture, empirical, 'Whether the IAEA''s institutional structure and mandate constitute capture by the nonproliferation-primary reading.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'To what degree is non-weapon states'' acceptance of the nonproliferation-primary reading due to structural barriers (military dependence on weapon states, lack of enforcement leverage) versus internalized legitimacy (they genuinely accept that disarmament is not their right to demand)?',
    'Post-exit suppression trajectory: if a non-weapon state exits NPT and maintains uranium enrichment without immediately being sanctioned or attacked, the suppression was substantially internalized (the exit removes structural barriers but the payer-role expectation persists). Comparative analysis of how non-weapon states discuss their restraint in private vs. public discourse.',
    'If suppression is substantially structural: lifting it (credible deterrence against weapon states, independent verification capacity) would produce rapid shift to alternative readings. If internalized: even structural changes would not quickly shift interpretation because the payer identity has been normalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether non-weapon states'' suppression under the nonproliferation frame is structural or internalized.').

omega_variable(
    reading_kernel_identity,
    'Is the NPT Article IV-VI pairing a single kernel with multiple readings, or are the readings themselves coterminous with distinct documents (the nonproliferation treaty, the humanitarian disarmament movement, the deterrence stability doctrine)?',
    'Genealogical analysis of when the readings diverged and whether they share a common textual reference (they do: Articles IV and VI) or are deployed as proxies for wholly independent frameworks.',
    'If readings are genuine committer variations on a shared kernel: the nonproliferation-primary reading is structurally comparable to grand_bargain and abolitionist readings, and the engine''s reading_relations classification applies. If readings are post-hoc rationalizations of independent doctrines: the ''kernel'' framing is an illusion, and the constraint should be decomposed into three separate, unlinked constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Whether the three readings instantiate a single kernel or three separate constraint families mislabeled as readings of one kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement(npt__tr_t10, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 10, 0.28).
narrative_ontology:measurement(npt__tr_t20, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 20, 0.35).
narrative_ontology:measurement(npt__tr_t30, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 30, 0.42).
narrative_ontology:measurement(npt__tr_t40, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 40, 0.46).
narrative_ontology:measurement(npt__tr_t50, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(npt__be_t10, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(npt__be_t20, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(npt__be_t30, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(npt__be_t40, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(npt__be_t50, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(npt__su_t10, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(npt__su_t20, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(npt__su_t30, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(npt__su_t40, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(npt__su_t50, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__nonproliferation_primary, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_verification_system).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, enrichment_fuel_cycle_restriction).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, weapons_state_deterrence_stability).

% DUAL FORMULATION NOTE:
% This story is one reading of the NPT Article IV-VI pairing kernel. The nonproliferation_primary reading dominates formal international law interpretation and IAEA operations. The grand_bargain reading (affects_constraints link) asserts reciprocal Article IV-VI obligations; the abolitionist reading asserts Article VI mandates complete disarmament and Article IV is illegitimate absent it. All three are readings of the same contested kernel — the binding force of Article VI and the conditionality of Article IV restraint on weapon state progress. They have different ε values, different victim/beneficiary sets, and different type classifications because they instantiate structurally distinct constraints from the same source material. The sibling readings are authored as separate JSON files with the same network.affects_constraints cross-references.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
