% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Right as Sovereignty Exercise
 *   domain: international_law/security
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the NPT's contested kernel:
 *   the interpretation that Article X (withdrawal right) codifies state
 *   sovereignty to exit if security environment deteriorates, and that
 *   Article VI (disarmament obligation) is therefore contingent on reciprocal
 *   nuclear-power disarmament, not an unconditional binding obligation. Under
 *   this reading, threshold states retain credible option value from nuclear
 *   weapons (they can withdraw legitimately if disarmament stalls), while
 *   non-nuclear signatories bear verification burden without reciprocal
 *   binding disarmament guarantee. The reading emerged from threshold-state
 *   security doctrine (Japan, South Korea, Iran) and is now explicit in US
 *   strategic-arms-control negotiating positions. It differs structurally
 *   from the oligopoly-enforcement reading (which treats horizontal
 *   nonproliferation as primary and vertical disarmament as aspirational) and
 *   the reciprocal-disarmament reading (which treats Article VI as binding
 *   with temporal urgency).
 *
 * KEY AGENTS:
 *   - Threshold states: gain nuclear option value from withdrawal-sovereignty framing; retain hedging capability under treaty cover
 *   - Established nuclear powers: administer the treaty; claim Article VI conditionality for themselves while enforcing horizontal nonproliferation against others
 *   - Non-nuclear signatories: bear verification and non-acquisition commitments; lose reciprocal binding guarantee if Article VI is contingent
 *   - Regime stability norm: loses enforceability when obligations become revocable rather than binding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.52).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right as Sovereignty Exercise").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/security").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, '307e540c-1376-458d-a83b-0f16e981b9c1').
narrative_ontology:cs_kernel_codification('307e540c-1376-458d-a83b-0f16e981b9c1', fixed_text).
narrative_ontology:cs_authority_grounding('307e540c-1376-458d-a83b-0f16e981b9c1', extraction).
narrative_ontology:cs_interpretation_layer_present('307e540c-1376-458d-a83b-0f16e981b9c1').
narrative_ontology:cs_reading_relation('307e540c-1376-458d-a83b-0f16e981b9c1', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('307e540c-1376-458d-a83b-0f16e981b9c1', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('307e540c-1376-458d-a83b-0f16e981b9c1', foundational, state_exit_right_sovereign_prerogative).
narrative_ontology:cs_axiom_status(state_exit_right_sovereign_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('307e540c-1376-458d-a83b-0f16e981b9c1', state_exit_right_sovereign_prerogative, deontological).
narrative_ontology:cs_axiom('307e540c-1376-458d-a83b-0f16e981b9c1', foundational, article_vi_obligation_contingent_on_security_environment).
narrative_ontology:cs_axiom_status(article_vi_obligation_contingent_on_security_environment, holdable).
narrative_ontology:cs_axiom_grounding('307e540c-1376-458d-a83b-0f16e981b9c1', article_vi_obligation_contingent_on_security_environment, empirically_contingent).
narrative_ontology:cs_reference_frame('307e540c-1376-458d-a83b-0f16e981b9c1', treaty_obligations_conditional_on_reciprocal_performance).
narrative_ontology:cs_drift_state('307e540c-1376-458d-a83b-0f16e981b9c1', post_cold_war_threshold_state_hedging_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('307e540c-1376-458d-a83b-0f16e981b9c1', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, security_discretion_doctrine).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_signatories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, state_sovereignty_supremacy).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, security_environment_conditionality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with latent nuclear capability (Japan, South Korea, Iran, Saudi Arabia, Turkey). This reading grants them legal cover to withdraw if security environment deteriorates, preserving nuclear option value without immediate proliferation. They benefit from treaty legitimacy while maintaining credible exit threat. They also pay through arms-control verification burden and international scrutiny.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, beneficiary,
    moderate, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, payer).

% US, Russia, UK, France, China. Drafted and administer the NPT; interpret Article X's scope and conditions. Under this reading, they retain withdrawal right for themselves while claiming Article VI (disarmament) obligations are contingent on external security factors. They enforce the treaty's horizontal-proliferation provisions selectively, using withdrawal threat as leverage in bilateral negotiations.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, established_nuclear_powers, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Non-nuclear states bound by full verification, material controls, and non-acquisition commitments with no reciprocal binding obligation from nuclear powers to disarm. Under this reading, they bear the cost of a treaty that nuclear powers can exit unilaterally. Withdrawal threat by any state (including threshold states) undermines compliance incentive for all non-nuclear signatories.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_signatories, payer,
    powerless, generational, trapped, global).

% The institutional understanding that treaty obligations create binding constraints on state behavior even under security pressure. This reading treats obligations as revocable, which corrodes the norm's enforceability and makes all arms-control regimes more brittle.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).

% Israel, Pakistan, India, other non-signatories or threshold states in regional competition. They would welcome explicit acknowledgment that NPT obligations are contingent on security environment; their absence from the treaty or their withdrawal option becomes a live competitive advantage. Their voice would amplify the conditionality argument.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regional_security_competitors, excluded,
    powerful, biographical, constrained, regional).

% IAEA, arms-control NGOs, academic experts. Monitor compliance and argue for binding interpretation of Article VI and against withdrawal clauses. They document how the withdrawal-as-sovereignty reading erodes compliance credibility and enable threshold states to frame nuclear hedging as legitimate security adaptation.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nonproliferation_advocacy_organizations, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__withdrawal_sovereignty_reading, established_nuclear_powers).
narrative_ontology:fixing_cost_class(npt_treaty_1970__withdrawal_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates horizontal nonproliferation (preventing spread of nuclear capability to additional states) through collective verification and material-control commitments, while preserving state sovereignty to exit if security environment changes.
% TRANSFER_FUNCTION: Moves verification burden, material restrictions, and transparency requirements from nuclear to non-nuclear states, while moving exit flexibility and security discretion toward any state claiming environmental change. Nuclear powers claim Article VI (disarmament) obligation is contingent on security; non-nuclear signatories claim it is binding. The transfer is asymmetric verification + contingent disarmament obligation.
% ABSENT_VOICES: Non-signatories (Israel, Pakistan, India) and would-be withdrawers (Iran, North Korea) are excluded from the treaty's internal framing but their strategic logic validates the withdrawal-sovereignty reading — their absence or exit demonstrates the reading's structural coherence. Their voice would crystallize the conditionality argument that this reading enshrines.
% DISAPPEARANCE_RATIONALE: If this reading were formally superseded by a binding-obligation reading (Article VI obligations non-contingent, Article X withdrawal only under Article LI force majeure conditions), threshold states would lose legal cover for nuclear hedging, nuclear powers would lose strategic ambiguity in disarmament claims, and compliance certification would shift from contingency-tracking to breach detection. The regime would become either more binding or more transparent in its breakdown.
% FOUNDING_PROBLEM: The NPT's core paradox: horizontal nonproliferation depends on threshold states believing nuclear powers will disarm (Article VI), but threshold states also need exit flexibility if security environment deteriorates and disarmament does not materialize. Article X codifies this ambiguity — it permits withdrawal but does not settle whether obligations are binding or conditional on external factors.
% FOUNDING_PROBLEM_CORROBORATION: Threshold-state governments (Japan, South Korea official statements 2016–2024), regional security analysts, and US/Russian strategic-arms negotiators in the Helsinki Commission and UN Open-Ended Working Group have all affirmed that Article X withdrawal credibility rides on Article VI conditionality — that states reserve the right to exit if disarmament does not occur within their security timeline. This corroboration comes from outside the non-nuclear-signatory bloc (it is explicit in threshold-state doctrine) and outside the beneficiary framing (US/Russian negotiators treat it as strategic fact, not policy preference).
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.48 to 0.68 over the interval (1970–2020) because the withdrawal-sovereignty reading became explicit doctrine in threshold-state security planning and US strategic communications, converting implicit exit option value into articulated leverage. The measurement series reflects this crystallization: early extractiveness (0.48) when Article X withdrawal right was treated as emergency-only by consensus; later extractiveness (0.68) when threshold states and nuclear powers openly discussed withdrawal credibility as security-policy instrument. Theater ratio remains moderate (0.28 at interval end) because verification and nonproliferation review conferences continue the coordination function in earnest, but an increasing fraction of enforcement activity involves managing the withdrawal-threat credibility itself rather than preventing proliferation. Suppression requirement plateaus (0.52) because the reading does not require active suppression of alternative interpretations — it is openly defended by powerful threshold states and nuclear powers; the suppression that exists is directed at non-nuclear signatories' resistance to the conditionality framing. The claim/metric divergence is deliberate: this reading is CLAIMED as tangled rope because it does coordinate horizontal nonproliferation while extracting asymmetric verification burden and contingent disarmament obligation. The authored metrics describe that structure honestly.
 *
 * PERSPECTIVAL GAP:
 *   From threshold-state seats, the reading appears as a legitimate security doctrine protecting their interests against great-power disarmament failure. From non-nuclear-signatory seats, the same reading appears as a ruse that benefits threshold states and nuclear powers while locking non-nuclear states into asymmetric obligations. From the established nuclear powers' seats, the reading is a strategic fact that structures negotiation leverage: they can claim Article VI contingency for themselves while enforcing it more strictly against others. The engine computes these divergences from the structural data (beneficiary/victim declarations, exit options, power differentials) — the authored claim does not adjudicate which perspective is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states occupy the beneficiary end of the directionality spectrum (d near 0.0–0.3): they gain nuclear option value, have exit flexibility (arbitrage-quality exit options under this reading), and sit at moderate power. Established nuclear powers occupy the beneficiary end (d near 0.0–0.2): they set the treaty terms, interpret Article X/VI, and can withdraw under the same sovereignty logic they grant to threshold states, yet retain deterrent forces during any transition. Non-nuclear signatories occupy the target end (d near 0.8–1.0): they are locked into full verification, material controls, and non-acquisition commitments with no reciprocal binding guarantee, and they are trapped (no exit option at acceptable cost — withdrawal isolates them diplomatically and leaves them vulnerable). The regime-stability norm is coded as payer (non-agent) because the reading's core function is to erode the norm's enforceability: treating obligations as revocable undermines the norm's structural role in binding all signatories equally. Regional security competitors are excluded: their strategic interest (nuclear hedging remains available) aligns with the reading's structural logic, so their absence from the treaty or their explicit endorsement of withdrawal-sovereignty doctrine would amplify the reading's persuasiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows no mandatrophy (the founding problem — balancing horizontal nonproliferation with threshold-state security concerns — remains live and is managed through the withdrawal-sovereignty reading). However, the reading itself contains a latent mandatrophy risk: if threshold states eventually acquire openly deployed nuclear weapons without Treaty X withdrawal (e.g., India, Pakistan, Israel path), the reading's coordinating function (preventing additional proliferation via regime commitment) will have collapsed while the extraction (verification burden on non-nuclear signatories, contingent disarmament obligation from nuclear powers) persists. This would shift the constraint from tangled rope toward snare. The oscillation in measurements (extractiveness rising 0.48→0.68 then plateauing, theater ratio rising then stabilizing) reflects the reading's trajectory: adoption as doctrine, then stabilization as strategic equilibrium without collapse-toward-snare. Monitor the disappearance verdict: if a future generation of threshold states withdraws openly, mandatrophy resolution would follow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_bindingness_ambiguity,
    'Is Article VI (disarmament obligation) a binding legal commitment with enforcement mechanisms, or a programmatic commitment contingent on security environment?',
    'Sustained enforcement action against nuclear powers by the International Court of Justice, UN General Assembly, or collective sanctions from non-nuclear signatories. If nuclear powers face consequences for non-compliance with Article VI, bindingness is established; if consequences are diplomatic only and contingency is accepted, contingency reading prevails.',
    'If binding: the withdrawal-sovereignty reading is undermined; Article X becomes emergency-only, and non-nuclear signatories'' compliance incentives strengthen. If contingent: the reading is vindicated; threshold states'' exit option value is legally sound, and regime stability depends on nuclear-power voluntary compliance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_bindingness_ambiguity, empirical, 'The enforceability of Article VI determines whether obligations are revocable or binding.').

omega_variable(
    regime_stability_vs_state_sovereignty_priority,
    'When state sovereignty (right to exit) and regime stability (binding obligations on all parties) come into conflict, which takes precedence in international law?',
    'Examination of state practice in other arms-control regimes (CTBT, CWC, BWC) and consensus in UN General Assembly and treaty review conferences. If withdrawal-sovereignty is treated as overriding in other contexts, the priority is established; if regimes enforce binding obligation even against sovereignty claims, stability takes precedence.',
    'High-priority sovereignty would vindicate this reading and make other arms-control regimes equally extractive (contingent obligations benefit powerful states). High-priority regime stability would suggest this reading is an outlier and trigger reconsideration of the withdrawal right''s scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_stability_vs_state_sovereignty_priority, conceptual, 'The hierarchical relationship between sovereignty and regime obligation in international law.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.52) structural (enforcement action against withdrawers) or internalized (non-nuclear signatories accept contingency doctrine and comply despite absence of reciprocal binding)?',
    'Track non-nuclear signatory compliance behavior and stated rationale over 10-year horizon. If compliance persists after explicit withdrawal by a threshold state and the signatory does NOT follow, suppression is internalized (they believe the doctrine). If compliance breaks or withdrawal cascades, suppression was structural.',
    'If internalized: the regime has achieved normative acceptance of the withdrawal-sovereignty reading; extraction is maintained by doctrine, not force. If structural: the regime''s stability depends on active diplomatic/institutional suppression of withdrawal cascade risk — more fragile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative behaviors is maintained by belief or by coercive threat.').

omega_variable(
    reading_foreclosure_risk,
    'Does explicit adoption of the withdrawal-sovereignty reading by threshold states and nuclear powers foreclose the reciprocal-disarmament reading''s core premise (Article VI as binding), or do the readings remain coexistent as different parties'' simultaneous commitments?',
    'Monitor whether states adopting the withdrawal-sovereignty reading explicitly deny the reciprocal-disarmament reading''s premises in legal briefs, treaty-review statements, or ICJ filings. Foreclosure occurs when one reading''s adoption makes the other logically impossible within a unified framework; coexistence persists when different parties hold different readings without attempting logical unification.',
    'If readings foreclose each other: the NPT kernel may eventually fracture into incompatible interpretations that destabilize the treaty. If readings coexist indefinitely: the treaty operates as a working fiction where multiple incompatible doctrines are performed simultaneously (characteristic of distributed-authority kernels).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_risk, conceptual, 'Whether the withdrawal-sovereignty reading logically eliminates the reciprocal-disarmament reading or whether both can persist as live competing interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(npt__tr_t0, observed).
narrative_ontology:measurement(npt__tr_t8, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement_basis(npt__tr_t8, observed).
narrative_ontology:measurement(npt__tr_t16, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement_basis(npt__tr_t16, observed).
narrative_ontology:measurement(npt__tr_t24, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement_basis(npt__tr_t24, observed).
narrative_ontology:measurement(npt__tr_t32, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement_basis(npt__tr_t32, observed).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(npt__tr_t40, projected).
narrative_ontology:measurement(npt__tr_t50, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(npt__tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(npt__be_t0, observed).
narrative_ontology:measurement(npt__be_t8, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(npt__be_t8, observed).
narrative_ontology:measurement(npt__be_t16, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(npt__be_t16, observed).
narrative_ontology:measurement(npt__be_t24, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(npt__be_t24, observed).
narrative_ontology:measurement(npt__be_t32, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(npt__be_t32, observed).
narrative_ontology:measurement(npt__be_t40, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(npt__be_t40, projected).
narrative_ontology:measurement(npt__be_t50, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(npt__be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(npt__su_t0, observed).
narrative_ontology:measurement(npt__su_t8, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement_basis(npt__su_t8, observed).
narrative_ontology:measurement(npt__su_t16, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement_basis(npt__su_t16, observed).
narrative_ontology:measurement(npt__su_t24, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement_basis(npt__su_t24, observed).
narrative_ontology:measurement(npt__su_t32, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement_basis(npt__su_t32, observed).
narrative_ontology:measurement(npt__su_t40, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(npt__su_t40, projected).
narrative_ontology:measurement(npt__su_t50, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement_basis(npt__su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__withdrawal_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-story constraint family decomposing the NPT kernel (npt_treaty_1970). Each story instantiates a different reading: withdrawal-sovereignty (this story) emphasizes state exit rights and Article VI contingency; oligopoly-enforcement emphasizes horizontal proliferation prevention and P5 control; reciprocal-disarmament emphasizes binding Article VI obligation with temporal urgency. The three readings have mutually exclusive core premises about whether disarmament is binding, whether exit is contingent on security, and whether proliferation control is primary. They are linked via network.affects_constraints because adoption of one reading by major parties shifts the legitimacy conditions and strategic incentives for the other two. The withdrawal-sovereignty reading influences both siblings by establishing exit-option credibility as a live variable in the disarmament negotiation, which constrains how strictly reciprocal-disarmament can demand binding obligations and how tightly oligopoly-enforcement can suppress withdrawal threats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
