% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA as Scaled Reciprocal Commitment with Graduated Enforcement
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This story instantiates the graduated-compliance reading of the JCPOA
 *   kernel: the agreement functions as a scaled reciprocal commitment in
 *   which sanctions relief and enrichment restraint move together
 *   incrementally, verified by IAEA snapshots, with dispute resolution
 *   designed to de-escalate rather than force binary legal closure. This is
 *   one of three structurally distinct readings of the same underlying kernel
 *   (the JCPOA text and its diplomatic history); the
 *   binding_multilateral_reading treats the same text as a treaty requiring
 *   consensus modification, and the transactional_provisional_reading treats
 *   it as voidable upon unilateral bad-faith determination. Each reading has
 *   a different beneficiary/victim structure and a different persistence
 *   logic — they are not the same constraint viewed from different angles,
 *   they are three constraints sharing a text.
 *
 * KEY AGENTS:
 *   - pragmatic_diplomacy_advocates: institutional beneficiaries who built diplomatic capital on the calibration architecture
 *   - european_commercial_actors: organized beneficiaries seeking incremental market re-entry
 *   - iranian_technocratic_reformists: moderate-power dual beneficiary/payer whose domestic standing rides on the deal working
 *   - iranian_civilian_population: powerless, trapped payer bearing reversible relief cycles
 *   - regional_security_partners: powerful payer bearing strategic ambiguity costs
 *   - iaea_inspectorate: institutional agenda-setter whose technical certifications are the operational hinge
 *   - us_congress_hardliners: excluded voice rejecting the graduated-calibration premise entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.42).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.38).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA as Scaled Reciprocal Commitment with Graduated Enforcement").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, 'c84ad6df-0ab3-4381-b735-02b4b3ee4c10').
narrative_ontology:cs_kernel_codification('c84ad6df-0ab3-4381-b735-02b4b3ee4c10', formalized).
narrative_ontology:cs_authority_grounding('c84ad6df-0ab3-4381-b735-02b4b3ee4c10', distributed).
narrative_ontology:cs_reading_relation('c84ad6df-0ab3-4381-b735-02b4b3ee4c10', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('c84ad6df-0ab3-4381-b735-02b4b3ee4c10', jcpoa_treaty_bindingness__transactional_provisional_reading, influences).
narrative_ontology:cs_axiom('c84ad6df-0ab3-4381-b735-02b4b3ee4c10', foundational, compliance_response_must_be_proportional).
narrative_ontology:cs_axiom_status(compliance_response_must_be_proportional, holdable).
narrative_ontology:cs_axiom_grounding('c84ad6df-0ab3-4381-b735-02b4b3ee4c10', compliance_response_must_be_proportional, instrumental).
narrative_ontology:cs_axiom('c84ad6df-0ab3-4381-b735-02b4b3ee4c10', foundational, dispute_resolution_prioritizes_deescalation_over_formal_closure).
narrative_ontology:cs_axiom_status(dispute_resolution_prioritizes_deescalation_over_formal_closure, holdable).
narrative_ontology:cs_axiom_grounding('c84ad6df-0ab3-4381-b735-02b4b3ee4c10', dispute_resolution_prioritizes_deescalation_over_formal_closure, conventional).
narrative_ontology:cs_reference_frame('c84ad6df-0ab3-4381-b735-02b4b3ee4c10', incremental_verified_reciprocity).
narrative_ontology:cs_drift_state('c84ad6df-0ab3-4381-b735-02b4b3ee4c10', post_2018_us_withdrawal, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c84ad6df-0ab3-4381-b735-02b4b3ee4c10', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, european_commercial_actors).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_technocratic_reformists).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_civilian_population).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_security_partners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_technocratic_reformists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Foreign ministries and multilateral institutions (E3/EU coordinators, IAEA technical staff) that built careers and institutional credibility on the graduated-compliance architecture. They gain diplomatic capital and policy relevance from a framework that rewards incremental verification and calibrated response, and can shift the calibration dials to manage crises without abandoning the whole structure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    institutional, generational, arbitrage, global).

% European firms and trade ministries seeking partial market re-entry into Iran calibrated to compliance snapshots. They benefit from graduated sanctions relief because it lets them re-enter incrementally without full political exposure, and can retreat quickly if the compliance dial swings back toward violation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, european_commercial_actors, beneficiary,
    organized, biographical, mobile, continental).

% Iranian officials and technical negotiators whose political standing depends on the deal being read as a working, adjustable mechanism rather than a binding treaty or a bad-faith gambit. They gain leverage domestically when graduated relief materializes, but bear the cost when the graduated framework produces slow, partial, and reversible relief instead of full sanctions termination.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_technocratic_reformists, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_technocratic_reformists, payer).

% Bears the economic consequences of a sanctions-relief schedule pegged to enrichment-level snapshots that can reverse on ambiguous verification findings. Graduated withdrawal of relief means gradual reduction of shortages does not translate into durable economic stability; each escalation cycle re-imposes hardship with no exit available to ordinary citizens.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_civilian_population, payer,
    powerless, biographical, trapped, national).

% Gulf states and Israel who argue that calibrated, proportional response to incremental violations effectively normalizes slow-motion enrichment creep. They pay in the form of prolonged strategic uncertainty and periodic need for costly deterrent posturing, unable to force a binary compliance/non-compliance verdict onto a framework built around graduated response.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_security_partners, payer,
    powerful, generational, constrained, regional).

% Administers the technical verification snapshots that trigger graduated escalation or de-escalation. Its assessments are the operational hinge of the entire reading — it sets the agenda for what counts as a proportional response by certifying enrichment levels and access compliance, and its technical judgments are what other parties' calibrated responses are pegged to.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_inspectorate, agenda_setter,
    institutional, generational, analytical, global).

% Legislators who reject the premise that compliance should be assessed on a graduated scale at all, preferring a binary bad-faith-triggered snapback. They are structurally outside the graduated-compliance reading's own logic — their preferred remedy (unilateral termination) is precisely what this reading is built to avoid, so their objections register as external pressure rather than as a voice inside the framework.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, us_congress_hardliners, excluded,
    powerful, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for de-escalating a nuclear proliferation standoff without requiring either side to accept total capitulation: enrichment limits are traded incrementally against sanctions relief, verified stepwise by IAEA snapshots, so neither party has to commit to an all-or-nothing settlement upfront.
% TRANSFER_FUNCTION: Moves sanctions relief from the P5+1/EU sanctions architecture toward Iran in increments pegged to verified enrichment reductions, and moves diplomatic and commercial risk from advocacy institutions and European firms onto the Iranian population (who bear the cost of reversals) and onto regional security partners (who bear the cost of prolonged ambiguity).
% ABSENT_VOICES: US Congressional hardliners and hawkish regional constituencies who reject graduated calibration as a category are outside the room in which the graduated-compliance framework is negotiated and administered; their preferred remedy (binary termination) has no seat inside this reading's own operating logic.
% DISAPPEARANCE_RATIONALE: Diplomatic seats and European commercial actors argue the world rearranges sharply if the graduated architecture disappears — sanctions relief would either lock into full termination or full snapback, eliminating the calibrated middle ground they depend on for continued engagement. Regional security partners and hardliners argue the world would barely change, since the graduated system already tolerates enough ambiguity that its removal would just make explicit what informal deterrence postures already assume.
% FOUNDING_PROBLEM: Halting Iranian nuclear weapons capability without a war, in a context where neither full trust nor full verification was achievable at the outset, by trading incremental relief for incremental verified restraint.
% FOUNDING_PROBLEM_CORROBORATION: IAEA technical reports (an institution outside the beneficiary set of pragmatic-diplomacy advocates) corroborate that the founding verification problem remains partially live — enrichment levels have fluctuated above and below JCPOA caps at various points, with inspection access itself periodically contested. Independent nonproliferation analysts outside government advocacy structures have documented both continued technical relevance of the verification problem and significant erosion of the graduated framework's practical operation since 2018.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).
:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.22 to a peak near 0.45 around the 2018-2019 period (US withdrawal and Iranian enrichment escalation), then partially recedes to 0.42 as parties re-stabilize around a degraded but still-graduated framework. Suppression tracks a similar but flatter arc (0.20 to 0.42 to 0.38) because active enforcement of the calibration apparatus — snapback mechanisms, verification demands — intensified during the crisis period without fully retreating. Theater ratio rises correspondingly (0.15 to 0.38 to 0.30) as diplomatic statements increasingly performed continued adherence to graduated logic even as the underlying calibration mechanism was structurally undermined by unilateral US sanctions reimposition, which the graduated-compliance framework was not designed to survive.
 *
 * PERSPECTIVAL GAP:
 *   From the IAEA and pragmatic-diplomacy seats, the structure looks like functioning coordination — a real technical process converting verified compliance into calibrated relief. From the Iranian civilian population's seat, the same structure computes as extraction with a coordination veneer: relief promised proportionally to compliance rarely fully materializes, while the costs of reversals land immediately and disproportionately on ordinary people who have no say in the calibration dial's setting. Regional security partners see a third profile again: neither coordination nor extraction in the classic sense, but an externality-generating arrangement whose graduated tolerance for incremental violation imposes costs on parties outside the P5+1/Iran dyad entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (diplomacy advocates, European commercial actors, Iranian reformists) sit near the low-d end: they gain career capital, market access, or domestic political standing from the graduated mechanism's continued operation, and hold mobile or arbitrage-grade exit. Victims (Iranian civilians, regional security partners) sit near the high-d end: Iranian civilians are trapped and bear reversal costs with zero exit; regional security partners are constrained — unable to unilaterally alter the framework's calibration logic despite bearing real security costs from its tolerance of incremental enrichment creep. The IAEA occupies an agenda-setting seat whose 'benefit' is institutional relevance rather than direct capture, and its analytical exit option reflects its technical rather than partisan positioning.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (halting weapons capability without war, via incremental trust-building) is genuinely contested as live vs. dead: IAEA technical corroboration shows the verification problem persists in modified form, but graduated relief has been so degraded by unilateral sanctions reimposition that the mechanism increasingly performs the founding function rather than executing it. This is precisely the theater_ratio signal (rising toward 0.38 during the crisis) — the classification should not mistake continued diplomatic invocation of 'graduated compliance' for the mechanism still functioning as originally designed. The graduated-compliance reading resists both over-crediting the framework as a live Rope and dismissing it outright as pure Snare — hence Tangled Rope, with a real coordination function that has been substantially degraded by asymmetric extraction (Iranian civilians bearing costs the calibration mechanism no longer reliably compensates).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    graduated_reading_vs_sibling_readings,
    'Does the JCPOA kernel genuinely support a graduated-compliance framing distinct from the binding_multilateral_reading and transactional_provisional_reading, or is the graduated language itself a diplomatic device papering over an underlying binary (treaty-bound vs. void-at-will) dispute that the graduated framing cannot actually resolve?',
    'Track whether disputes under the JCPOA were in practice resolved via the graduated Joint Commission dispute-resolution mechanism (supporting this reading) or via unilateral action outside that mechanism (supporting either sibling reading, depending on which party acted unilaterally and why).',
    'If graduated dispute resolution was consistently bypassed by unilateral action (as occurred with US withdrawal in 2018), the graduated-compliance reading''s coordination claim weakens substantially and its classification should drift toward the transactional_provisional_reading''s territory over time — this is exactly the sibling-reading disagreement location.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_reading_vs_sibling_readings, conceptual, 'Whether the graduated-compliance framing is a stable structural reading or a transitional description of a framework already collapsing toward one of its sibling readings.').

omega_variable(
    compliance_metric_manipulability,
    'How resistant is the IAEA verification snapshot process to political pressure from either side, given that the entire graduated mechanism''s legitimacy depends on the snapshots being read as technical rather than political?',
    'Compare IAEA safeguards reporting language and access-request patterns across periods of high vs. low political tension between JCPOA parties; look for evidence of technical assessment shifting independent of underlying enrichment activity.',
    'If verification snapshots are shown to be politically responsive rather than purely technical, the ''graduated'' calibration this reading depends on is partly performative — raising the theater_ratio finding and strengthening a piton-adjacent reading of the post-2018 framework rather than a live tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_metric_manipulability, empirical, 'Whether the technical verification hinge of the graduated framework is insulated from political pressure or itself contested terrain.').

omega_variable(
    beneficiary_capture_of_calibration_dial,
    'Who actually controls the calibration dial that determines how much sanctions relief corresponds to how much verified compliance — is it jointly administered as the framework claims, or effectively controlled by whichever party currently holds more leverage?',
    'Historical analysis of which party''s compliance assessments were operative in practice at moments of dispute (e.g., snapback triggering, relief suspension) versus the formally joint mechanism.',
    'If the dial is effectively unilaterally controlled by the more powerful party at any given moment, the ''graduated reciprocal'' framing partially masks an asymmetric extraction structure, which would push toward re-weighting the extractiveness/suppression metrics upward and toward a stronger snare-adjacent reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_calibration_dial, conceptual, 'Whether the graduated calibration mechanism is genuinely bilaterally administered or captured by the more powerful party''s unilateral leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jcpo_tr_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(jcpo_tr_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(jcpo_tr_t12, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(jcpo_tr_t16, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(jcpo_tr_t22, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 22, 0.3).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(jcpo_be_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(jcpo_be_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(jcpo_be_t12, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(jcpo_be_t16, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(jcpo_be_t22, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 22, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(jcpo_su_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(jcpo_su_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(jcpo_su_t12, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(jcpo_su_t16, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(jcpo_su_t22, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 22, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints sharing the JCPOA text as their kernel. binding_multilateral_reading treats the agreement as a treaty requiring consensus-based dissolution (higher accessibility_collapse, lower theater_ratio, Rope-leaning claim). transactional_provisional_reading treats it as voidable upon unilateral bad-faith determination (higher suppression from the threat of unilateral termination, Snare-leaning claim from the perspective of the party subject to that determination). This graduated_compliance_reading occupies the structural middle: it authorizes proportional response rather than binary determination, producing a Tangled Rope profile — genuine coordination function (incremental de-escalation) combined with asymmetric extraction (civilians and regional partners bearing costs the calibration mechanism does not reliably offset). Each reading has its own stable ε; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
