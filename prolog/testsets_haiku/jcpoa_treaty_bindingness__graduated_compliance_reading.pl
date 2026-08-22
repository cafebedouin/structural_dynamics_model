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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA Scaled Reciprocal Commitment with Graduated Enforcement
 *   domain: international_law/nuclear_non_proliferation
 *
 * SUMMARY:
 *   The JCPOA, signed in 2015 by Iran and the P5+1, represents a scaled
 *   reciprocal commitment where sanctions relief is granted in tranches
 *   contingent on IAEA-verified Iranian compliance with enrichment caps and
 *   monitoring access. Violations trigger graduated sanctions re-imposition
 *   proportional to assessed breach severity. This reading instantiates the
 *   constraint as a tangled rope: it coordinates nonproliferation monitoring
 *   and de-escalatory response protocols (genuine coordination function)
 *   while simultaneously extracting compliance constraints from Iran and
 *   conditional sanctions benefits from the P5+1 (asymmetric extraction).
 *   Active enforcement is required to maintain the graduated response
 *   mechanism and to police the distinction between permitted and prohibited
 *   nuclear activity. This is ONE reading of the contested kernel 'JCPOA
 *   treaty bindingness'; it is NOT the binding multilateral reading (which
 *   treats JCPOA as legally immutable without consensus) or the transactional
 *   provisional reading (which treats it as voidable at unilateral
 *   determination). This reading specifically privileges graduated
 *   enforcement calibrated to violation severity and de-escalation over
 *   binary legal closure.
 *
 * KEY AGENTS:
 *   - P5+1 signatories: collectively set framework, assess violations, trigger sanctions adjustments (institutional power, generational horizon, constrained exit)
 *   - Iran nuclear program: subject to enrichment constraints and monitoring; receives phased sanctions relief contingent on compliance (powerful state, constrained exit by framework design)
 *   - Pragmatic diplomacy advocates: benefit from model of managed escalation; have mobile exit but prefer this framework (organized, biographical horizon)
 *   - Economic engagement actors: benefit from sanctions relief windows; face exposure when relief is withdrawn (organized, constrained exit)
 *   - IAEA verification regime: observes and reports compliance; drives sanctions adjustment cycle (institutional, analytical seat)
 *   - Excluded hardliners and unilateralists: oppose framework; excluded from day-to-day but retain structural power to withdraw (powerful, trapped in initial architecture but mobile via withdrawal)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.52).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.38).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA Scaled Reciprocal Commitment with Graduated Enforcement").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_non_proliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, 'ed6eb7ca-f735-4f9b-893e-c57f918b30b7').
narrative_ontology:cs_kernel_codification('ed6eb7ca-f735-4f9b-893e-c57f918b30b7', fixed_text).
narrative_ontology:cs_authority_grounding('ed6eb7ca-f735-4f9b-893e-c57f918b30b7', lineage).
narrative_ontology:cs_interpretation_layer_present('ed6eb7ca-f735-4f9b-893e-c57f918b30b7').
narrative_ontology:cs_reading_relation('ed6eb7ca-f735-4f9b-893e-c57f918b30b7', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed6eb7ca-f735-4f9b-893e-c57f918b30b7', jcpoa_treaty_bindingness__transactional_provisional_reading, influences).
narrative_ontology:cs_axiom('ed6eb7ca-f735-4f9b-893e-c57f918b30b7', foundational, graduated_enforcement_proportional_to_violation_severity).
narrative_ontology:cs_axiom_status(graduated_enforcement_proportional_to_violation_severity, holdable).
narrative_ontology:cs_axiom_grounding('ed6eb7ca-f735-4f9b-893e-c57f918b30b7', graduated_enforcement_proportional_to_violation_severity, instrumental).
narrative_ontology:cs_axiom('ed6eb7ca-f735-4f9b-893e-c57f918b30b7', foundational, de_escalation_prioritized_over_legal_closure).
narrative_ontology:cs_axiom_status(de_escalation_prioritized_over_legal_closure, holdable).
narrative_ontology:cs_axiom_grounding('ed6eb7ca-f735-4f9b-893e-c57f918b30b7', de_escalation_prioritized_over_legal_closure, deontological).
narrative_ontology:cs_reference_frame('ed6eb7ca-f735-4f9b-893e-c57f918b30b7', scaled_reciprocal_commitment).
narrative_ontology:cs_drift_state('ed6eb7ca-f735-4f9b-893e-c57f918b30b7', post_us_withdrawal_2018, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ed6eb7ca-f735-4f9b-893e-c57f918b30b7', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_engagement_actors).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_signatories).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_enrichment_capacity_constrained).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, western_sanctions_relief_conditional).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_nuclear_program).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_engagement_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The P5+1 powers (US, UK, France, Russia, China, Germany) signed the JCPOA in 2015 and jointly administer its compliance assessment and sanctions-relief mechanics. They set the framework's terms, assess Iranian violations via IAEA reports, and decide whether to re-impose sanctions. Each signatory benefits from reduced proliferation risk and diplomatic engagement but bears verification costs and political exposure when domestic opponents argue the framework is inadequate. Signatories retain de facto unilateral withdrawal power, making their exit option 'constrained' rather than 'trapped' — constrained because withdrawal destabilizes but does not dissolve the framework for other signatories.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_signatories, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_signatories, beneficiary).

% Iran commits to limits on uranium enrichment (capped at 3.67% U-235), centrifuge deployment (frozen at 2015 levels), and stockpile reduction (diluting excess uranium to low-enriched form). IAEA conducts intrusive inspections with access to undeclared sites under the Additional Protocol. Iran receives phased sanctions relief contingent on compliance. Iran's exit options are constrained: full withdrawal re-exposes Iran to comprehensive sanctions; partial violation risks graduated sanctions re-imposition calibrated to the measured breach severity. Iran's enrichment capacity is the primary tradeable asset in the constraint.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_nuclear_program, payer,
    powerful, generational, constrained, global).

% International nongovernmental organizations, academic experts, and moderate diplomatic factions who view the JCPOA as a successful model for managed escalation and de-escalatory response to perceived violations. They benefit from a framework that establishes precedent: international cooperation can manage proliferation risk through graduated enforcement rather than binary military choice. They have mobile exit — they can advocate for alternative frameworks or different dispute-resolution mechanisms — but they prefer the JCPOA's operating logic because it normalizes proportional response.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    organized, biographical, mobile, global).

% International firms in energy, trade, finance, and pharmaceuticals that operate under the assumption JCPOA compliance holds. They benefit from sanctions relief windows and Iranian market access; their investment and supply-chain decisions depend on predictable sanctions cycles. When Iran violates and signatories re-impose sanctions, these actors face stranded assets, supply disruption, and regulatory risk. Their exit is constrained by sunk investment and long-term contracts; they cannot easily shift operations if the framework destabilizes. They also carry secondary payer role because they bear stranded-asset costs when compliance cycles fail.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_engagement_actors, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_engagement_actors, payer).

% The International Atomic Energy Agency maintains continuous monitoring of Iranian nuclear facilities, operates cameras and seals on enrichment equipment, and issues quarterly compliance reports to the JCPOA Joint Commission. IAEA assessment of Iranian violations directly drives the entire sanctions relief/re-imposition cycle. The IAEA is formally independent but structurally depends on signatory-state funding and political support for access; it has no formal exit from the framework but can affect its operation through inspection rigor, assessment framing, and reporting emphasis. IAEA observed-violation data is the constraint's primary enforcement trigger.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_regime, observer,
    institutional, generational, analytical, global).

% States (Israel, Saudi Arabia, Gulf states) and factions within signatory states that oppose the JCPOA as inadequately constraining Iran or as too permissive of weaponization-adjacent activities. They are excluded from the JCPOA decision-making architecture; their objections are treated as external positions rather than stakeholder input. Their exclusion is structural: the framework was designed for the P5+1 and Iran, not for regional parties with security interests in Iranian power. They retain structural power to destabilize via military action (Israel) or pressure on signatories (Saudi Arabia via oil/investment leverage), but they are formally excluded from compliance assessment and sanctions-adjustment decisions.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_regional_opponents, excluded,
    powerful, generational, trapped, global).

% Political factions within the US and other signatory states that view JCPOA as a strategic vulnerability, violation of national sovereignty, or naïve concession to Iranian deception. They advocate for withdrawal without renegotiation. They are excluded from day-to-day framework administration but retain structural power to withdraw unilaterally — a capacity they exercised in 2018 with the US withdrawal. Their exclusion from ongoing compliance discussion is temporary and contingent on political change within their home state; they have mobile exit because they can change domestic politics to enable withdrawal.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, unilateralist_withdrawal_advocates, excluded,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_signatories).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__graduated_compliance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared measurement regime (IAEA inspections conducted under a uniform protocol) and a graduated-response procedure (sanctions relief tranches and re-imposition calibrated to assessed Iranian violation severity) that allows both Iran and the signatories to reduce proliferation risk while maintaining economic and diplomatic engagement despite periodic compliance breaches and verification uncertainties. The coordination problem is: how do parties with deep mutual distrust cooperate on nuclear constraints without forcing binary choice between total war and total capitulation? The JCPOA answer: establish independent monitoring (IAEA), define proportional response levels (minor violations trigger partial re-imposition, major violations trigger comprehensive re-imposition), and maintain negotiating channels (Joint Commission) to dispute compliance assessments without abandoning the framework.
% TRANSFER_FUNCTION: Moves sanctions relief (access to frozen assets, re-entry to SWIFT financial system, lifting of oil export caps, restoration of trade relationships) from the signatories to Iran in tranches, proportional to Iranian compliance milestones and time elapsed. In return, Iran transfers compliance constraints (enrichment caps, centrifuge limits, stockpile reduction) and transparency (IAEA monitoring, access to undeclared sites, provision of evidence). The arrangement is reciprocal in structure but asymmetric in timing: Iran pays compliance upfront; signatories pay relief over time. Violations trigger partial clawback of relief proportional to breach severity.
% ABSENT_VOICES: Regional opponents of Iranian power (Israel, Saudi Arabia, UAE) have no formal voice in JCPOA compliance assessment or sanctions adjustment; they would argue Iran is using the framework to maintain development-adjacent capabilities while sanctions relief funds regional military expansion. Unilateralist factions within the US and other signatories are excluded from day-to-day administration; they would argue the framework is naïve capitulation and Iran is systematically deceiving inspectors. Both excluded groups have superior information-processing capability (intelligence agencies) and strong incentives to challenge the framework; their exclusion represents a deliberate choice to prioritize signatory-Iran consensus over broader stakeholder input.
% DISAPPEARANCE_RATIONALE: If the JCPOA framework disappeared overnight: Iran would immediately resume full nuclear development without monitoring, resuming enrichment at higher levels and restarting reactor operations halted under the agreement; the P5+1 would lose the graduated-response lever and face binary choice between military intervention and nuclear hedging by regional states; international investors would face comprehensive sanctions regimes and supply-chain collapse in Iranian markets; and nonproliferation norms globally would shift away from multilateral graduated constraint toward bilateral coercion and military deterrence. The entire post-JCPOA security architecture would reorganize around unilateral power rather than scaled reciprocal commitment.
% FOUNDING_PROBLEM: After years of ad-hoc negotiations, escalating sanctions, and nuclear-program acceleration by Iran, the international community faced a coordination crisis: Iran's enrichment capacity was advancing toward weapons-usable levels, military intervention by regional or Western powers was increasingly probable, and bilateral coercion had failed to constrain Iranian development. The coordination problem was simultaneously technical (how to detect and measure enrichment levels reliably), political (how to balance Iranian sovereignty with Western security interests and regional concerns), and economic (how to phase sanctions relief to incentivize compliance while maintaining leverage for later breaches).
% FOUNDING_PROBLEM_CORROBORATION: The P5+1 signatories and pragmatic nonproliferation experts attest the founding problem was acute and the framework successfully addresses it: enrichment was capped, weapons-level stockpiles were eliminated, and intrusive monitoring was established for the first time. Hardline opponents and unilateralist factions attest the problem remains unsolved or has worsened: Iran maintains development-adjacent capabilities, enriches uranium at higher levels after US withdrawal, and uses sanctions relief to advance regional military power. IAEA technical reports document the accuracy of monitoring and detection of specific violations; they also document Iran's periodic technical violations and the difficulty of distinguishing prohibited development from permitted research. Independent proliferation analysis supports both readings depending on baseline assumptions about Iranian intent and the credibility of stated enrichment purposes.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.52 at interval end) because the constraint imposes real costs on both sides — Iran foregoes development capacity and sovereignty autonomy; the P5+1 forfeit hard-line pressure and full sanctions leverage — but both receive genuine coordination benefit (reduced proliferation risk, predictable escalation ladder, economic engagement windows). Suppression is moderate-low (0.38) because the constraint operates primarily through mutual agreement monitoring and graduated response rather than unilateral coercion; Iran violates periodically and often faces only partial sanctions re-imposition, not comprehensive reimposition. Theater ratio is moderate (0.44) because part of the activity is genuine verification and de-escalatory negotiation, but part is theatrical compliance display: Iran conducts minor permitted activities designed to appear maximally concerning; the P5+1 conduct sanctions adjustments designed to appear maximally severe without triggering Iranian withdrawal. The measurement series tracks a slight rise in extractiveness and theater through the mid-interval (t=10-15), then stabilization, reflecting the cycle of minor Iranian violations, measured Western responses, and resumed engagement. The shared time grid ensures all metrics are authored at all examined points.
 *
 * PERSPECTIVAL GAP:
 *   The signatories' seat and Iran's seat should compute different types from the same structural data. From the signatories' perspective, this is genuine rope: a coordination function that benefits both parties, with Iran consenting to constraints and receiving relief in proportion. From Iran's perspective, the same structure is extractive: enrichment constraints are imposed, sanctions relief is conditional and reversible, and the framework is administered by parties with superior military power. The graduated-enforcement reading privileges de-escalation framing, which makes the coordination narrative more salient from Western seats; Iranian seats experience the same structure as calibrated constraint with periodic penalties. Directionality derivation: signatories are low-d beneficiaries (control framework, conditional relief); Iran is high-d target (constrained enrichment, conditional relief); pragmatic advocates are low-d beneficiaries (benefit from coordination without running it); economic actors are moderate-d payers (benefit from relief windows, but constrained exit if sanctions cycle resumes). IAEA is analytical (observes, reports, drives but does not control outcomes).
 *
 * DIRECTIONALITY LOGIC:
 *   The P5+1 signatories hold d~0.2-0.35 (beneficiaries: they set terms, assess violations, trigger response; their exit options are mobile — they can withdraw and the framework continues for the rest, though destabilized). Iran holds d~0.65-0.75 (target: enrichment is constrained, exit is costly, relief is conditional on compliance assessed by others). Pragmatic advocates hold d~0.4-0.5 (symmetric: they benefit from coordination without bearing direct costs, but they have mobile exit — they can advocate for different frameworks). Economic actors hold d~0.55-0.65 (payers: they bear stranded-asset exposure when sanctions cycle resumes, though they benefit from relief windows; exit is constrained by investment sunk costs). Hardliners and unilateralists are excluded from routine operation (e.g., d is undefined or analytical), but they retain structural power to alter the framework via withdrawal — a non-standard capacity that the baseline directionality model does not fully capture, addressed in an omega.
 *
 * MANDATROPHY ANALYSIS:
 *   The graduated-compliance reading avoids the mandatrophy trap by explicitly recognizing that the founding problem (how to manage Iran's enrichment without military war) changes shape as the framework operates: early-interval high extractiveness reflects the initial high cost of Iranian compliance commitment and Western verification burden; mid-interval stabilization reflects normalization of the graduated response cycle; late-interval resistance remains high (0.72) because exclusions (Israel, Saudi, unilateralists) continuously challenge the framework's legitimacy and Iran periodically probes violation boundaries. The constraint does not drift toward pure coordination (extraction does not fall below 0.45) because Iran never fully internalizes the enrichment cap as legitimate — it continues to view enrichment restriction as imposed, not chosen. Nor does it drift toward pure snare (extraction does not rise above 0.54) because signatories genuinely bear verification costs and Iran retains exit-via-withdrawal optionality that would impose costs on signatories. The measurement data shows theater ratio rising to 0.46 mid-interval, then holding steady, indicating that performative compliance display and measured-appearance enforcement maintain their relative weight rather than one overtaking the other — consistent with a stable tangled rope operating within its design parameters rather than degrading toward piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    violation_severity_measurement_ambiguity,
    'How is the severity of Iranian enrichment violations operationalized and assessed for proportionality of sanctions response? What distinguishes minor technical violations from material breach?',
    'Formal IAEA assessment criteria, historical sanctions-adjustment decisions mapped to specific violations, counterfactual analysis of what violations would trigger different response levels.',
    'If violation severity is objectively measurable and signatories respond consistently to like violations, the graduated-response reading is validated (tangled rope with clear extraction scaling). If severity assessment is politically negotiated and responses vary inconsistently, the constraint drifts toward theater — performative compliance display rather than genuine graduated enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violation_severity_measurement_ambiguity, empirical, 'Whether violation severity can be objectively calibrated or is politically negotiated.').

omega_variable(
    unilateralist_exit_power_incompleteness,
    'The framework is designed for consensus-based modification by the P5+1, but signatories retain de facto unilateral withdrawal power (exercised by the US in 2018). How does this incompleteness affect classification?',
    'Structural analysis of what changes if a signatory withdraws: does the framework persist (as happened post-2018 US withdrawal), does it collapse, or does it transform into a different constraint? Under what conditions does withdrawal trigger binding obligation on remaining parties?',
    'If withdrawal does NOT trigger obligatory dissolution, then the framework is weaker-than-binding (supports transactional or graduated readings). If withdrawal DOES trigger renegotiation pressure on all parties (even non-withdrawing ones), then effective bindingness persists despite formal unilateral capacity. The classification hinges on whether unilateral exit power is a structural feature of this reading or a design gap the reading must acknowledge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unilateralist_exit_power_incompleteness, conceptual, 'Whether unilateral withdrawal power undermines the binding status of the framework for non-withdrawing parties.').

omega_variable(
    iran_consent_dynamics_under_asymmetric_power,
    'Does Iran''s signature on the JCPOA represent genuine consent to enrichment constraints, or is it extraction legitimized by formal signature?',
    'Analysis of Iranian elite discourse: did Iran ever internalize enrichment caps as legitimate, or does it continuously view them as imposed constraints it accepted under duress (military threat, comprehensive sanctions)? Counterfactual: if Western military pressure had been lower, would Iran have signed at lower extraction levels?',
    'If Iranian leadership views enrichment constraints as imposed, not chosen, the constraint is extractive regardless of mutual-agreement framing — the reading drifts toward snare at the Iranian seat. If Iran internalized enrichment caps over time, the reading holds as genuine tangled rope with Iran''s directionality shifting downward. This is the core legitimacy question for the graduated-compliance frame: whether it represents scaled reciprocity or ''reciprocity'' imposed by power asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(iran_consent_dynamics_under_asymmetric_power, conceptual, 'Whether Iran consented to enrichment constraints or accepted them under duress.').

omega_variable(
    excluded_parties_structural_power_incompleteness,
    'How does the structural exclusion of Israel, Saudi Arabia, and hardline US factions from JCPOA decision-making affect the constraint''s bindingness and sustainability?',
    'Historical analysis: did exclusion reduce framework legitimacy? Did excluded parties'' pressure (via unilateralist advocacy, regional military action, domestic political opposition) destabilize the framework? Counterfactual: if excluded parties had been formally included (e.g., in extended signatories or observer status), would the framework persist longer?',
    'High-magnitude external pressure from excluded parties (witnessed in US 2018 withdrawal, Israeli and Saudi opposition) suggests the framework''s sustainability is hostage to exclusion politics. The graduated-compliance reading assumes de-escalatory internal logic, but excluded parties provide external escalatory pressure. If exclusion systematically destabilizes the framework, the reading may mischaracterize the constraint as more stable/reciprocal than it is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_parties_structural_power_incompleteness, empirical, 'Whether structural exclusion of interested parties undermines framework sustainability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(jcpo_tr_t0, observed).
narrative_ontology:measurement(jcpo_tr_t5, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(jcpo_tr_t5, observed).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 10, 0.43).
narrative_ontology:measurement_basis(jcpo_tr_t10, observed).
narrative_ontology:measurement(jcpo_tr_t15, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(jcpo_tr_t15, observed).
narrative_ontology:measurement(jcpo_tr_t20, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(jcpo_tr_t20, observed).
narrative_ontology:measurement(jcpo_tr_t25, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement_basis(jcpo_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(jcpo_be_t0, observed).
narrative_ontology:measurement(jcpo_be_t5, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(jcpo_be_t5, observed).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(jcpo_be_t10, observed).
narrative_ontology:measurement(jcpo_be_t15, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(jcpo_be_t15, observed).
narrative_ontology:measurement(jcpo_be_t20, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(jcpo_be_t20, observed).
narrative_ontology:measurement(jcpo_be_t25, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(jcpo_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(jcpo_su_t0, observed).
narrative_ontology:measurement(jcpo_su_t5, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement_basis(jcpo_su_t5, observed).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(jcpo_su_t10, observed).
narrative_ontology:measurement(jcpo_su_t15, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(jcpo_su_t15, observed).
narrative_ontology:measurement(jcpo_su_t20, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(jcpo_su_t20, observed).
narrative_ontology:measurement(jcpo_su_t25, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement_basis(jcpo_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_nuclear_enrichment_capacity).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, western_sanctions_architecture).

% DUAL FORMULATION NOTE:
% The JCPOA kernel has three structurally distinct readings: (1) binding_multilateral_reading treats it as immutable without consensus, producing higher signatory extraction cost and lower Iranian violation incentive; (2) graduated_compliance_reading treats it as scaled-response framework, this constraint, with moderate extraction and de-escalatory enforcement; (3) transactional_provisional_reading treats it as voidable at unilateral determination, producing lower signatory binding cost but higher Iran uncertainty and framework volatility. The three readings have different ε values (binding is lowest extraction, transactional is highest), different beneficiary/victim structures, and different suppression profiles. They are three constraints, not three views of one constraint — their ε values are structurally distinct (the binding reading has lower ε because mutual consensus requirement suppresses unilateral exit and thus reduces negotiation-level extraction; the transactional reading has higher ε because unilateral voidability shifts burden to Iran to maintain signatory confidence). The sibling readings are linked via network.affects_constraints in each constraint's JSON file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__graduated_compliance_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
