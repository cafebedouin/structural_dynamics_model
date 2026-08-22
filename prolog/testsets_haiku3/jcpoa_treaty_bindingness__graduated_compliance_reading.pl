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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA Graduated Compliance Framework Reading
 *   domain: international/political/legal
 *
 * SUMMARY:
 *   The JCPOA (Joint Comprehensive Plan of Action), negotiated 2015, treats
 *   Iran's nuclear compliance as a scalar commitment subject to proportional
 *   enforcement rather than a binary gate. This reading instantiates that
 *   graduated compliance logic: Iranian enrichment limits, inspector access,
 *   and uranium stockpile caps are the baseline constraint; detected
 *   violations trigger tiered sanctions relief withdrawal proportional to
 *   violation severity; dispute resolution prioritizes de-escalation and
 *   renegotiation over treaty termination. The constraint's beneficiaries are
 *   pragmatic diplomacy advocates who prefer managed engagement and economic
 *   actors (European trade, sanctions relief recipients) who benefit from
 *   phased sanctions relief. Victims are hardline Iranian factions
 *   identity-locked to resistance, U.S. domestic skeptics who read graduated
 *   enforcement as insufficient, and sanctioned entities trapped outside the
 *   framework's relief mechanisms. This reading coexists with a
 *   binding-multilateral reading (treaty as immutable multilateral commitment
 *   requiring consensus modification) and a transactional reading (treaty as
 *   provisional mechanism voidable on unilateral bad-faith determination).
 *   The graduated reading does NOT foreclose the others; different
 *   institutional seats and national governments hold all three
 *   simultaneously.
 *
 * KEY AGENTS:
 *   - Joint Commission (IAEA, P5+1, EU): administers graduated enforcement calibration; sets compliance tiers; authorizes sanctions adjustments
 *   - Iran (government and private actors): bound to enrichment limits and inspector access; receives phased relief as compliance is verified
 *   - European powers and trade interests: benefit from partial normalization without binary sanctions/capitulation choice
 *   - U.S. skeptics and Iranian hardliners: payers who see graduated enforcement as inadequate constraint or humiliating limitation
 *   - Sanctioned entities outside JCPOA scope: trapped; excluded from relief mechanisms regardless of Iranian compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.58).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.48).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA Graduated Compliance Framework Reading").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international/political/legal").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, 'b5f5f42e-22d9-4e11-b094-3772f4305285').
narrative_ontology:cs_kernel_codification('b5f5f42e-22d9-4e11-b094-3772f4305285', fixed_text).
narrative_ontology:cs_authority_grounding('b5f5f42e-22d9-4e11-b094-3772f4305285', extraction).
narrative_ontology:cs_interpretation_layer_present('b5f5f42e-22d9-4e11-b094-3772f4305285').
narrative_ontology:cs_reading_relation('b5f5f42e-22d9-4e11-b094-3772f4305285', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5f5f42e-22d9-4e11-b094-3772f4305285', jcpoa_treaty_bindingness__transactional_provisional_reading, coexists_with).
narrative_ontology:cs_axiom('b5f5f42e-22d9-4e11-b094-3772f4305285', foundational, graduated_enforcement_preserves_engagement).
narrative_ontology:cs_axiom_status(graduated_enforcement_preserves_engagement, holdable).
narrative_ontology:cs_axiom_grounding('b5f5f42e-22d9-4e11-b094-3772f4305285', graduated_enforcement_preserves_engagement, instrumental).
narrative_ontology:cs_axiom('b5f5f42e-22d9-4e11-b094-3772f4305285', foundational, proportional_response_constrains_escalation).
narrative_ontology:cs_axiom_status(proportional_response_constrains_escalation, holdable).
narrative_ontology:cs_axiom_grounding('b5f5f42e-22d9-4e11-b094-3772f4305285', proportional_response_constrains_escalation, empirically_contingent).
narrative_ontology:cs_reference_frame('b5f5f42e-22d9-4e11-b094-3772f4305285', reciprocal_compliance_with_proportional_enforcement).
narrative_ontology:cs_drift_state('b5f5f42e-22d9-4e11-b094-3772f4305285', contemporary_accumulated_violations, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b5f5f42e-22d9-4e11-b094-3772f4305285', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, sanctions_relief_beneficiaries).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, european_trade_interests).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_hardliners).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, us_domestic_skeptics).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, sanctioned_entities_unable_to_transition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diplomatic actors and international organizations invested in graduated compliance frameworks where proportional responses to violations preserve negotiation space and avoid all-or-nothing breakdowns. They benefit from the constraint's logic of partial enforcement because it keeps all parties within engagement bounds and maintains the possibility of renegotiation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    institutional, generational, constrained, global).

% Iranian government, private sector actors, and foreign firms with business interests in Iran. They benefit from graduated enforcement because they receive phased sanctions relief keyed to Iranian compliance tiers, allowing business resumption even if perfect compliance is never fully achieved. Exit would be refusing to comply, which forecloses the relief.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, sanctions_relief_beneficiaries, beneficiary,
    moderate, biographical, constrained, global).

% European governments and corporations with energy and trade stakes in Iran. They benefit from graduated compliance frameworks because they enable partial normalization without the binary choice between full sanctions or full capitulation. Their exit is available (they can break ranks and ignore sanctions), but their preference is for managed escalation rather than binary outcomes.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, european_trade_interests, beneficiary,
    powerful, biographical, mobile, continental).

% Iranian political and military actors opposed to the framework itself. They pay through constraints on uranium enrichment, inspector access, and periodic verification visits. Their identity is constituted through resistance to what they read as humiliating limitations; exit means the constraint dissolves, which is their goal but requires overcoming the framework's institutional lock-in.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_hardliners, payer,
    organized, generational, identity_locked, national).

% U.S. political actors (Congress, hardline executive branch figures) who oppose the framework because they read it as insufficient constraint on Iranian ambitions and as extracting concessions (sanctions relief) disproportionate to Iranian compliance. Their position is that graduated enforcement is cover for eventual Iranian nuclear breakout. Exit is available via unilateral withdrawal, which happened in 2018.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, us_domestic_skeptics, payer,
    powerful, biographical, mobile, national).

% Entities sanctioned prior to JCPOA or sanctioned outside its scope who cannot access relief mechanisms because they remain on secondary designation lists. They bear costs—financial exclusion, reputational damage, frozen assets—without the benefit of the graduated compliance logic; they are trapped outside the framework's relief mechanisms regardless of Iranian compliance tier.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, sanctioned_entities_unable_to_transition, payer,
    powerless, immediate, trapped, global).

% The JCPOA's Joint Commission (IAEA, P5+1, EU, Iran) that administers compliance assessment and enforcement calibration. They set the metrics for detecting violations, determine what compliance tier applies, and authorize corresponding sanctions adjustments. Their authority is constrained by the agreement's terms but their discretion over interpretation and phasing is substantial.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, joint_commission_administrators, agenda_setter,
    institutional, generational, constrained, global).

% International Atomic Energy Agency technical staff who measure Iranian compliance and report findings to the Joint Commission. They are technically neutral but their measurements become the factual basis for enforcement escalation or de-escalation. They have no formal role in the graduated enforcement decision but their data drives it.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_inspectors_and_analysts, observer,
    moderate, biographical, constrained, global).

% Political actors arguing the framework is a provisional arrangement voidable on unilateral determination of bad faith and that graduated enforcement is merely delay before Iranian breakout. They are excluded from the Joint Commission deliberations; their position is that the entire constraint should be abandonment-ready.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, unilateral_exit_voices, excluded,
    powerful, immediate, mobile, national).

% The doctrine of pacta sunt servanda and proportionality-based treaty enforcement. The graduated reading vindicates this doctrine by treating treaty compliance as a scalar rather than binary property and enforcement response as proportional to violation severity rather than default-to-termination.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, international_legal_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__graduated_compliance_reading, international_legal_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__graduated_compliance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the multi-party nuclear proliferation prevention problem through reciprocal transparency (Iran accepts inspections and enrichment limits), reciprocal economic incentive (sanctions relief phased with Iranian compliance), and graduated de-escalation (violations trigger proportional enforcement response, not automatic termination). Coordinates among parties with conflicting security interests by making compliance observable and responses predictable.
% TRANSFER_FUNCTION: Moves sanctions relief (economic access, unfrozen assets, trade normalization) from the P5+1 coalition to Iran as Iran demonstrates compliance tiers; moves constraint on enrichment activities, inspector access, and uranium stockpile management from Iran to the Joint Commission's verification authority. The transfer is conditional and graduated: more compliance = more relief; detected violations trigger relief clawback proportional to violation severity.
% ABSENT_VOICES: Sanctioned entities outside the JCPOA's scope (non-nuclear-related designations) have no input into the framework; the Iranian population facing domestic political costs of compliance (restrictions on energy independence, inspection visibility) is not directly represented in the Joint Commission; unilateral-exit political factions are structurally excluded from collaborative deliberation.
% DISAPPEARANCE_RATIONALE: If graduated compliance enforcement disappeared and reverted to binary treaty termination rules, diplomatic response options would collapse to breakup-or-accept, sanctions relief would become all-or-nothing, and Iranian enrichment would move outside international verification. The multi-party coordination structure would unravel into bilateral confrontation.
% FOUNDING_PROBLEM: 2013–2015: Iran's nuclear program was advancing toward breakout capability while the international community lacked mechanisms to enforce negotiated limits other than comprehensive sanctions (collective punishment) or military intervention. The founding problem was creating a framework that verifies Iranian compliance in real time and allows proportional response to specific violations without triggering automatic treaty collapse.
% FOUNDING_PROBLEM_CORROBORATION: The JCPOA signatories attest the founding problem remains live; they argue Iranian compliance is ongoing and violations detected via IAEA have been addressed through the graduated framework. U.S. withdrawal advocates attest the founding problem was never solved, arguing Iranian enrichment has progressed despite the framework and that graduated enforcement is cover for Iranian procrastination. International nuclear analysts from outside the signatory set note mixed evidence: IAEA reports detect compliance in some areas and technical violations in others; the interpretation of whether the framework is effectively constraining or deferring breakout depends on assumption about Iran's ultimate intentions.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 endpoint) because the constraint's core is genuine coordination (multi-party nuclear verification) AND asymmetric relief distribution (Iran receives phased access; the P5+1 receives compliance assurance but not economic benefit). The graduation mechanism is the analytic center: violations trigger proportional rather than maximal response, preserving negotiation space. This makes extraction lower than a binary snare (all-or-nothing termination threat) but higher than pure rope (balanced coordination without unilateral capture of relief flows). Suppression is lower (0.48) because the constraint's persistence depends on voluntary Iranian compliance within the graduated tiers, not on external coercion strong enough to override identity or domestic political costs; Iranian hardliners resist, but the framework offers them a compliance ladder rather than a single demand. Theater is moderate (0.42) because dispute resolution and compliance assessment produce real procedural activity (Joint Commission meetings, IAEA inspections, technical deliberations), but a growing share of that activity is performative: violations are detected but enforcement responses are delayed or diluted in negotiation, and the framework's ability to constrain Iranian ambitions long-term is contested even by signatories. The time series shows extractiveness rising (0.48→0.61 through year 18, measured when violations accumulate and negotiation costs compound) then moderating slightly at projection (0.58), reflecting contestation over whether graduated enforcement can survive repeated Iranian technical violations without either hardening into binary termination (the binding-multilateral reading) or collapsing into provisional transactionality (the transactional reading). The theater ratio rises gradually and stabilizes, indicating steady institutionalization of compliance rituals; suppression requirement tracks with extractiveness because more enforcement calibration is needed as violations accumulate.
 *
 * PERSPECTIVAL GAP:
 *   From the Joint Commission's seat, the constraint is genuine graduated coordination that preserves engagement. From the Iranian hardliners' seat, it is humiliating limitation that extracts sovereignty over nuclear independence. From the U.S. skeptics' seat, it is inadequate and extractive (Iran gets relief before full constraint). From the sanctioned entities' seat outside the framework, the constraint extracts pure cost with no relief pathway. The engine computes these per-seat divergences from the structural data (different power, exit options, time horizons); the author's claim and metrics remain independent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is differentiated per stakeholder: Pragmatic diplomacy advocates have d≈0.25 (beneficiary; they capture the framework's preservation benefit). Sanctions relief beneficiaries have d≈0.35 (moderate; they receive graduated relief but remain bound by verification constraints). European trade interests have d≈0.30 (beneficiary with exit; their mobile exit_options lower directionality despite beneficiary role). Iranian hardliners have d≈0.75 (target; identity-locked to resistance, bearing enrichment limits). U.S. skeptics have d≈0.65 (target; mobile exit available, exercised 2018). Sanctioned entities outside the framework have d≈0.95 (full target; trapped, excluded from any relief mechanism). The Joint Commission as agenda-setter sits at d≈0.40 (moderately extractive from their seat; they set the calibration rules but also bear the burden of escalation management and political blame when violations occur). Directionality derivation follows the chain: beneficiary/victim declarations + exit options → d values → effective extraction χ = ε × d × scope-modifier (engine computed).
 *
 * MANDATROPHY ANALYSIS:
 *   The graduated compliance reading faces a mandatrophy risk: the founding problem was to create a framework that enforces Iranian nuclear limits through verifiable compliance tiers and proportional response. Evidence of mandatrophy would be: (1) Iranian enrichment advances despite graduated enforcement and violations go unrepaired; (2) the framework persists but its enforcement mechanisms become purely theatrical (inspections happen, violations are reported, but sanctions relief continues regardless); (3) all parties abandon confidence in the framework's ability to constrain while the institutional apparatus persists. The measurement series shows extractiveness rising through year 18, which could indicate either (a) the constraint is functioning as designed (violations accumulate, enforcement responses escalate in proportion) or (b) the constraint is degrading (violations accumulate while enforcement responses weaken). Theater remains moderate rather than spiking, which suggests the procedural apparatus is still functional. However, the projected value at year 24 (0.58, lower than the year-18 peak of 0.61) reflects uncertainty about whether the graduated framework survives the next violation cycle or collapses into either hardened binary enforcement (binding-multilateral reading takes over) or abandonment (transactional reading's voidability argument prevails). A mandatrophy verdict would require measuring whether the framework's enforcement authority is genuine or increasingly performative; this story does not resolve that question and carries omegas to mark it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandatrophy_verification_degradation,
    'Is the graduated enforcement framework maintaining genuine constraint on Iranian enrichment, or is it degrading into theater while Iranian capabilities advance?',
    'Post-interval empirical assessment: IAEA technical analysis of whether Iranian enrichment stockpile and centrifuge capacity remain below breakout thresholds despite detected violations; comparison of enforcement response severity to violation severity to determine if graduated responses are proportional or asymmetrically weak.',
    'If genuine constraint is maintained: the reading''s extractiveness classification holds; graduated enforcement is functioning as designed. If degradation is evident: extractiveness should reclassify upward (closer to pure snare) and the constraint transitions toward mandatrophy — the foundational problem (verifiable enforcement) is dead but the institutional apparatus persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_verification_degradation, empirical, 'Whether graduated enforcement is maintaining real constraint or degrading into performative compliance theater.').

omega_variable(
    reading_stability_across_violation_cycles,
    'Will the graduated reading survive as the governing interpretation of JCPOA bindingness through cycles of detected Iranian violations and calibrated enforcement response, or will violations accumulate until one party (most likely U.S. or Iranian hardliners) abandons the graduated reading and adopts either binding-multilateral (demand termination unless all consent) or transactional (unilateral exit on bad faith).',
    'Historical observation of Joint Commission response to the next detected Iranian violation category (e.g., excess uranium stockpile, undeclared enrichment site, expired inspector deadlines): does the Commission implement proportional enforcement or does a member state escalate the demand or exit?',
    'If the graduated reading remains stable: the reading persists as an institutional commitment that constrains all parties'' options to within-framework negotiation. If a member state adopts transactional reading and exercises unilateral voidability: the kernel reading contest resolves via exit rather than consensus, and the graduated framework is overrun by the transactional reading''s logic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_stability_across_violation_cycles, empirical, 'Whether graduated compliance remains the stable reading of JCPOA bindingness or is displaced by unilateral exit politics.').

omega_variable(
    commission_discretion_vs_binding_constraint,
    'Does the Joint Commission''s authority to calibrate graduated enforcement represent genuine discretion to adjust sanctions relief proportionally, or is it constrained by implicit binding requirements (e.g., all members must consent to any sanction relief withdrawal, making it effectively binary)?',
    'Test case: Iran commits a technical violation (undeclared enrichment activity, excess uranium stockpile); observe whether the Joint Commission can implement partial sanctions relief withdrawal or whether any member state can veto the adjustment and demand either no adjustment or full sanctions re-imposition.',
    'If Commission has genuine proportional calibration authority: the graduated reading''s core mechanism is real and extractiveness is correctly calibrated to moderate levels. If Commission discretion is vetoed by binding multilateral requirement: the constraint''s actual operation is closer to binary (either proportional adjustments proceed unanimously or not at all), and the effective reading shifts toward binding-multilateral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commission_discretion_vs_binding_constraint, empirical, 'Whether graduated enforcement is genuinely calibrated or constrained by implicit binding-multilateral requirements.').

omega_variable(
    reading_vs_reading_distinction_in_framework,
    'The three readings (graduated, binding-multilateral, transactional) are distinct interpretations of the same treaty text. Are they genuinely incommensurable (different frameworks produce incompatible classifications), or do they represent different weights on the same underlying values (all embrace constraint, but differ on enforcement tightness)?',
    'Formal comparative analysis of each reading''s axioms and reference frames: if axioms contradict fundamentally (e.g., one reading asserts Iran''s nuclear development is inherently destabilizing, another asserts it is inherently legitimate), the readings are incommensurable. If axioms are compatible but differ in risk tolerance, they are commensurate variations.',
    'If incommensurable: the readings are three genuinely different constraint stories; the corpus should carry all three as separate constraint instances. If commensurate: the readings are weight-distributions on a single underlying structure, and the graduated reading may be collapsed into a single story with weight parameters rather than three separate readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_reading_distinction_in_framework, conceptual, 'Whether the three JCPOA bindingness readings are fundamentally distinct constraints or variations on a single underlying commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_graduated_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(jcpoa_graduated_tr_t0, observed).
narrative_ontology:measurement(jcpoa_graduated_tr_t3, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 3, 0.37).
narrative_ontology:measurement_basis(jcpoa_graduated_tr_t3, observed).
narrative_ontology:measurement(jcpoa_graduated_tr_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement_basis(jcpoa_graduated_tr_t6, observed).
narrative_ontology:measurement(jcpoa_graduated_tr_t12, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement_basis(jcpoa_graduated_tr_t12, observed).
narrative_ontology:measurement(jcpoa_graduated_tr_t18, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 18, 0.44).
narrative_ontology:measurement_basis(jcpoa_graduated_tr_t18, observed).
narrative_ontology:measurement(jcpoa_graduated_tr_t24, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(jcpoa_graduated_tr_t24, projected).

% Extraction over time
narrative_ontology:measurement(jcpoa_graduated_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(jcpoa_graduated_be_t0, observed).
narrative_ontology:measurement(jcpoa_graduated_be_t3, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 3, 0.51).
narrative_ontology:measurement_basis(jcpoa_graduated_be_t3, observed).
narrative_ontology:measurement(jcpoa_graduated_be_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement_basis(jcpoa_graduated_be_t6, observed).
narrative_ontology:measurement(jcpoa_graduated_be_t12, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(jcpoa_graduated_be_t12, observed).
narrative_ontology:measurement(jcpoa_graduated_be_t18, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 18, 0.61).
narrative_ontology:measurement_basis(jcpoa_graduated_be_t18, observed).
narrative_ontology:measurement(jcpoa_graduated_be_t24, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement_basis(jcpoa_graduated_be_t24, projected).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_graduated_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(jcpoa_graduated_su_t0, observed).
narrative_ontology:measurement(jcpoa_graduated_su_t3, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 3, 0.43).
narrative_ontology:measurement_basis(jcpoa_graduated_su_t3, observed).
narrative_ontology:measurement(jcpoa_graduated_su_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement_basis(jcpoa_graduated_su_t6, observed).
narrative_ontology:measurement(jcpoa_graduated_su_t12, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement_basis(jcpoa_graduated_su_t12, observed).
narrative_ontology:measurement(jcpoa_graduated_su_t18, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement_basis(jcpoa_graduated_su_t18, observed).
narrative_ontology:measurement(jcpoa_graduated_su_t24, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement_basis(jcpoa_graduated_su_t24, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_nuclear_breakout_capability).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, us_unilateral_treaty_withdrawal_authority).

% DUAL FORMULATION NOTE:
% This story is part of the JCPOA bindingness kernel family. The kernel is the treaty text and commitment structure; three readings are instantiated as separate constraints. (1) graduated_compliance_reading (THIS): moderate extraction, graduated enforcement. (2) binding_multilateral_reading: high extraction, consensus-required modification. (3) transactional_provisional_reading: very high extraction, unilaterally escapable. Each reading has a distinct epsilon-value, beneficiary/victim structure, and classification because each reading changes the meaning of what 'compliance' and 'enforcement' are. They are not alternate measurements of the same constraint; they are structurally distinct constraints derived from the same kernel via competing interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__graduated_compliance_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
