% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   domain: international_law/nuclear_non_proliferation
 *
 * SUMMARY:
 *   This story instantiates the graduated-compliance reading of the JCPOA
 *   kernel: the arrangement as a scaled reciprocal commitment in which
 *   enforcement (snapback of sanctions) and reward (relief) are calibrated
 *   continuously to assessed compliance rather than triggered by a binary
 *   breach/no-breach determination. This reading is administered primarily
 *   through IAEA verification cycles and E3/EU diplomatic coordination, and
 *   it is the reading favored by actors invested in incremental
 *   de-escalation. It is a distinct constraint from the
 *   binding_multilateral_reading (which treats the JCPOA as a treaty
 *   requiring consensus for any modification) and the
 *   transactional_provisional_reading (which treats it as voidable at will on
 *   a unilateral bad-faith determination) — each of those readings has its
 *   own beneficiary structure, its own extraction profile, and is authored as
 *   a separate constraint story linked via network.affects_constraints.
 *
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
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA as Scaled Reciprocal Commitment with Graduated Enforcement").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_non_proliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, 'eba5b868-eaff-4f38-8244-c94aa7a101ba').
narrative_ontology:cs_kernel_codification('eba5b868-eaff-4f38-8244-c94aa7a101ba', distributed).
narrative_ontology:cs_authority_grounding('eba5b868-eaff-4f38-8244-c94aa7a101ba', distributed).
narrative_ontology:cs_reading_relation('eba5b868-eaff-4f38-8244-c94aa7a101ba', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('eba5b868-eaff-4f38-8244-c94aa7a101ba', jcpoa_treaty_bindingness__transactional_provisional_reading, influences).
narrative_ontology:cs_axiom('eba5b868-eaff-4f38-8244-c94aa7a101ba', foundational, compliance_is_a_continuous_calibrated_variable).
narrative_ontology:cs_axiom_status(compliance_is_a_continuous_calibrated_variable, holdable).
narrative_ontology:cs_axiom_grounding('eba5b868-eaff-4f38-8244-c94aa7a101ba', compliance_is_a_continuous_calibrated_variable, instrumental).
narrative_ontology:cs_axiom('eba5b868-eaff-4f38-8244-c94aa7a101ba', secondary, de_escalation_priority_over_formal_legal_closure).
narrative_ontology:cs_axiom_status(de_escalation_priority_over_formal_legal_closure, holdable).
narrative_ontology:cs_axiom_grounding('eba5b868-eaff-4f38-8244-c94aa7a101ba', de_escalation_priority_over_formal_legal_closure, conventional).
narrative_ontology:cs_reference_frame('eba5b868-eaff-4f38-8244-c94aa7a101ba', id_2015_incremental_calibration_framework).
narrative_ontology:cs_drift_state('eba5b868-eaff-4f38-8244-c94aa7a101ba', post_2018_unilateral_withdrawal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eba5b868-eaff-4f38-8244-c94aa7a101ba', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, european_commercial_actors).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_technocratic_faction).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_apparatus).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_civilian_population).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_non_nuclear_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, snapback_exposed_third_party_traders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_technocratic_faction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diplomats, technical negotiators, and policy architects (E3 foreign ministries, Iranian moderates, EU coordinators) who designed and administer the graduated calibration mechanism — snapback tied to verified enrichment thresholds, relief tied to verified rollback. They gain political capital and institutional relevance from the framework's continued operation as a scaled instrument rather than a binary treaty; their careers and diplomatic standing are invested in demonstrating that graduated de-escalation works.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, agenda_setter).

% European firms and financial intermediaries who benefit from partial, calibrated sanctions relief that lets them re-enter Iranian markets incrementally as compliance is assessed. They can exit into other markets if the graduated framework collapses, giving them real optionality that the arrangement's other parties lack.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, european_commercial_actors, beneficiary,
    organized, biographical, mobile, continental).

% Iranian officials and technical experts who negotiated and manage compliance reporting; they benefit from the graduated framework's legitimation of incremental relief but are also the ones who absorb domestic political cost when partial sanctions relief fails to materialize proportionally to what was promised, and who must justify continued restraint to domestic hardliners.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_technocratic_faction, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_technocratic_faction, payer).

% The IAEA administers the proportional compliance assessments that trigger graduated enforcement steps. Its authority and continued funding depend on the framework treating compliance as a matter of measurable degree rather than binary breach — a role it would not have under either a hard binding-treaty reading or a purely transactional one.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_apparatus, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_apparatus, observer).

% Bears the economic consequences of graduated sanctions relief that arrives slowly, partially, and reversibly — banking restrictions, inflation, and shortages persist through years of incremental calibration even when Iran is found substantially compliant. Has no voice in the proportionality assessments and cannot exit the national economy.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_civilian_population, payer,
    powerless, biographical, trapped, national).

% Gulf states and regional powers who live with the security consequences of a framework that permits continued Iranian enrichment capacity calibrated to a compliance curve rather than eliminated outright. They bear the strategic risk of graduated tolerance without being party to the proportionality assessments that determine its pace.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_non_nuclear_states, payer,
    moderate, generational, constrained, regional).

% Firms and individuals who entered commercial relationships during a relief phase and are exposed to sudden re-imposition of sanctions when the graduated assessment finds a violation — the calibration mechanism's reversibility is a feature for the parties administering it and a trap for those who transacted in good faith during a relief window.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, snapback_exposed_third_party_traders, payer,
    powerless, immediate, trapped, global).

% Political factions in both the US and Iran who reject the premise of graduated calibration entirely — one side wants binding treaty enforcement with full consensus dissolution rights, the other wants unilateral exit rights on suspicion of bad faith. Neither is structurally represented in the graduated-compliance framework's own institutional logic, which treats their positions as noise around the calibration curve rather than legitimate alternative readings.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_domestic_factions, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__graduated_compliance_reading, diffuse).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__graduated_compliance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for de-escalating a nuclear proliferation standoff incrementally, allowing verified partial compliance to be met with partial relief rather than forcing an all-or-nothing choice between full trust and total breakdown — this genuinely solves the problem of negotiating under mutual distrust where neither side will front-load full concessions.
% TRANSFER_FUNCTION: Moves sanctions relief, market access, and diplomatic legitimacy from Western state and commercial actors to Iranian technocratic and commercial actors in increments pegged to IAEA-assessed compliance; moves the cost of calibration delay and reversal risk onto the Iranian civilian population and onto third parties who transact during relief windows.
% ABSENT_VOICES: Hardline factions on both sides who reject proportionality itself as the wrong frame reject it precisely because the graduated-compliance reading's institutional machinery has no seat for an all-or-nothing verdict; regional states bearing the security externality of tolerated enrichment capacity are not party to the compliance assessments that set the pace.
% DISAPPEARANCE_RATIONALE: If the graduated-calibration mechanism vanished, the underlying dispute would not disappear — it would revert to being adjudicated under one of the sibling readings (a hard binding-treaty dispute-resolution process or a unilateral transactional exit), each of which redistributes leverage very differently. Relief transactions in progress would be stranded, IAEA's calibration role would lose its institutional anchor, and the diplomatic actors invested in the graduated framework would lose their primary instrument.
% FOUNDING_PROBLEM: In 2015 neither side would accept a framework requiring full, immediate, and irreversible concessions from itself while trusting the other to reciprocate — a calibrated, reversible, incremental structure was built to make mutual distrust tractable rather than requiring it be resolved first.
% FOUNDING_PROBLEM_CORROBORATION: IAEA verification reports and European diplomatic communiqués (parties administering the framework) attest the calibration mechanism functioned as designed through several compliance cycles. Independent nonproliferation analysts outside the framework's administering parties — and openly hostile domestic factions in both Washington and Tehran — attest the graduated structure has instead become a vehicle for indefinite non-resolution, letting each side claim technical compliance while the underlying proliferation and sanctions questions remain permanently unsettled.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42 at interval end) reflecting genuine coordination value (de-escalation under mutual distrust) alongside real asymmetric cost-bearing (Iranian civilians and third-party traders absorb the calibration mechanism's reversibility risk while diplomatic and commercial beneficiaries capture the relief and legitimacy gains). Suppression is moderate (0.38) because the mechanism does not foreclose exit for powerful parties (European firms retain arbitrage-like mobility) but does trap powerless parties (civilian population, exposed traders) inside a framework whose pace they cannot influence. Theater ratio rose from 0.12 to 0.3 across the interval as compliance-assessment cycles increasingly served to demonstrate ongoing process rather than resolve the underlying proliferation question — consistent with the founding_problem_status being contested rather than resolved.
 *
 * DIRECTIONALITY LOGIC:
 *   Diplomatic architects and the IAEA verification apparatus sit near the beneficiary end: their institutional relevance depends on continued operation of graduated calibration, and they administer rather than bear its costs. European commercial actors are moderate beneficiaries with real exit options (mobile capital). Iranian civilians and regional non-nuclear states sit near the target end: trapped or constrained exit, no voice in the compliance assessments, and they absorb the deferred-resolution cost that the calibration mechanism generates by design. Third-party traders exposed to snapback occupy an especially sharp target position — they transact in good faith during a relief window and bear reversal risk they did not create and cannot price.
 *
 * MANDATROPHY ANALYSIS:
 *   The graduated-compliance reading resists a naive good/bad binary: it is neither pure coordination (the founding problem — mutual distrust preventing negotiation — has not been resolved, and the mechanism now shows signs of serving indefinite deferral rather than eventual resolution) nor pure extraction (the coordination function of making incremental trust-building tractable was and remains real). The tangled_rope classification captures this: genuine coordination function (de-escalation under distrust) coexists with asymmetric extraction (diplomatic/commercial beneficiaries capture legitimacy and market access gains while powerless parties absorb reversal risk and delay costs) requiring active enforcement (IAEA verification cycles, sanctions administration) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    graduated_reading_vs_binding_reading_disagreement_locus,
    'Where exactly does the graduated-compliance reading and the binding-multilateral reading structurally diverge — is it the dispute-resolution mechanism, the modification-consensus requirement, or the underlying theory of what ''breach'' means?',
    'Compare formal dispute-resolution invocations under each reading''s operative logic against the JCPOA''s actual Dispute Resolution Mechanism text and observe which reading better predicts state behavior at each snapback trigger point.',
    'If the binding-multilateral reading''s consensus requirement is the operative legal fact, the graduated reading''s calibration mechanism is itself an informal practice riding on top of a formally binding instrument, changing where authority actually sits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_reading_vs_binding_reading_disagreement_locus, conceptual, 'Locates the structural disagreement between the graduated and binding-treaty readings.').

omega_variable(
    graduated_reading_vs_transactional_reading_disagreement_locus,
    'Does the graduated-compliance reading''s continuous calibration foreclose the transactional-provisional reading''s unilateral bad-faith exit, or do they coexist as different parties'' operative theories held simultaneously?',
    'Examine whether any party that invoked unilateral withdrawal (2018 US withdrawal) treated the graduated calibration mechanism as having any continuing legal force afterward, or treated it as void from the point of unilateral determination.',
    'If unilateral withdrawal is treated by state practice as fully voiding the graduated mechanism''s claims, the two readings are in practice incompatible for the withdrawing party even though they coexist across different parties'' frameworks — this bears on whether ''coexists_with'' or something closer to mutual foreclosure better describes the relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_reading_vs_transactional_reading_disagreement_locus, conceptual, 'Tests whether the graduated and transactional readings genuinely coexist or partially foreclose each other in practice.').

omega_variable(
    calibration_mechanism_deferral_or_resolution,
    'Is the graduated calibration mechanism a genuine path toward eventual resolved compliance, or has it become a stable equilibrium of indefinite technical non-resolution that benefits the administering parties more than it advances non-proliferation?',
    'Track whether IAEA compliance assessments over multi-year cycles show convergence toward a stable verified state or persistent oscillation without closure; compare against theater_ratio trend.',
    'If the mechanism is structurally biased toward indefinite deferral, the tangled_rope classification would strengthen toward snare as the coordination justification becomes retrospective cover for institutional self-perpetuation rather than a live transition mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_mechanism_deferral_or_resolution, empirical, 'Whether graduated calibration is progressing toward resolution or has become self-perpetuating.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 96).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jcpo_tr_t16, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(jcpo_tr_t32, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 32, 0.24).
narrative_ontology:measurement(jcpo_tr_t48, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 48, 0.28).
narrative_ontology:measurement(jcpo_tr_t64, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 64, 0.26).
narrative_ontology:measurement(jcpo_tr_t80, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 80, 0.29).
narrative_ontology:measurement(jcpo_tr_t96, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 96, 0.3).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(jcpo_be_t16, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(jcpo_be_t32, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 32, 0.35).
narrative_ontology:measurement(jcpo_be_t48, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 48, 0.4).
narrative_ontology:measurement(jcpo_be_t64, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 64, 0.38).
narrative_ontology:measurement(jcpo_be_t80, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(jcpo_be_t96, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 96, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(jcpo_su_t16, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(jcpo_su_t32, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 32, 0.34).
narrative_ontology:measurement(jcpo_su_t48, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 48, 0.36).
narrative_ontology:measurement(jcpo_su_t64, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 64, 0.35).
narrative_ontology:measurement(jcpo_su_t80, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(jcpo_su_t96, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 96, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, transactional_provisional_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'JCPOA bindingness' per the ε-invariance principle. Each reading of the jcpoa_treaty_bindingness kernel is authored as a separate constraint with its own ε, beneficiary/victim structure, and classification: binding_multilateral_reading (treaty-consensus theory), graduated_compliance_reading (this story — scaled reciprocal calibration), and transactional_provisional_reading (unilateral-exit theory). They are linked via affects_constraints rather than merged because measuring 'is the JCPOA binding' under each reading's own logic yields structurally different ε values and different victim sets, which is precisely the signal that they are different constraints, not one constraint observed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
