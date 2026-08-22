% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO Dispute Settlement Binding Authority (Binding Referee Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint instantiates the binding-referee reading of the WTO
 *   Dispute Settlement Body's authority: DSB panels issue binding rulings
 *   grounded in treaty law; member states surrendered policy discretion
 *   within WTO-covered domains as the price of market access. The referent is
 *   the standing institutional arrangement where DSB rulings trigger
 *   compliance obligations and authorized retaliation for non-compliance.
 *   This reading sees the DSB as a binding arbitral tribunal; sibling
 *   readings contest this, framing the DSB alternately as an advisory
 *   coordination mechanism or as a judicially activist institution exceeding
 *   its mandate. The measurement series traces both extractiveness and
 *   enforcement machinery intensification across the 30-year interval,
 *   showing mounting extractive asymmetry and increased suppression burden on
 *   constrained states.
 *
 * KEY AGENTS:
 *   - WTO Dispute Settlement Body — institutional agenda-setter administering binding rulings and retaliation authorization
 *   - Large trading blocs (USA, EU, major exporters) — beneficiaries using DSB to lock in market access and constrain protectionist measures by others
 *   - Policy-constrained member states (moderate power) — payers who surrendered discretion within trade-covered domains; subject to binding compliance
 *   - Small developing economies (powerless) — trapped victims lacking litigation capacity and facing developmental policy constraints; cannot afford exit
 *   - Domestic policy constituencies (labor, environment, public health) — excluded voices structurally outside DSB framework
 *   - Panel interpreter community — beneficiaries whose authority and career advancement depend on maintaining expansive binding-authority model
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.68).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.72).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO Dispute Settlement Binding Authority (Binding Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, '7f8389c2-fb8c-4f74-a5bf-96d07d8229df').
narrative_ontology:cs_kernel_codification('7f8389c2-fb8c-4f74-a5bf-96d07d8229df', fixed_text).
narrative_ontology:cs_authority_grounding('7f8389c2-fb8c-4f74-a5bf-96d07d8229df', lineage).
narrative_ontology:cs_interpretation_layer_present('7f8389c2-fb8c-4f74-a5bf-96d07d8229df').
narrative_ontology:cs_reading_relation('7f8389c2-fb8c-4f74-a5bf-96d07d8229df', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f8389c2-fb8c-4f74-a5bf-96d07d8229df', wto_dsb_authority__judicial_activism_reading, influences).
narrative_ontology:cs_axiom('7f8389c2-fb8c-4f74-a5bf-96d07d8229df', foundational, dsb_panels_issue_binding_rulings).
narrative_ontology:cs_axiom_status(dsb_panels_issue_binding_rulings, holdable).
narrative_ontology:cs_axiom_grounding('7f8389c2-fb8c-4f74-a5bf-96d07d8229df', dsb_panels_issue_binding_rulings, conventional).
narrative_ontology:cs_axiom('7f8389c2-fb8c-4f74-a5bf-96d07d8229df', foundational, sovereignty_explicitly_traded_for_market_access).
narrative_ontology:cs_axiom_status(sovereignty_explicitly_traded_for_market_access, holdable).
narrative_ontology:cs_axiom_grounding('7f8389c2-fb8c-4f74-a5bf-96d07d8229df', sovereignty_explicitly_traded_for_market_access, deontological).
narrative_ontology:cs_reference_frame('7f8389c2-fb8c-4f74-a5bf-96d07d8229df', treaty_grounded_binding_arbitration).
narrative_ontology:cs_drift_state('7f8389c2-fb8c-4f74-a5bf-96d07d8229df', contemporary_expanded_jurisprudence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7f8389c2-fb8c-4f74-a5bf-96d07d8229df', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_dispute_settlement_body).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, large_trading_bloc_exporters).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, policy_constrained_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, small_developing_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, panel_interpreter_community).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, large_exporting_coalition_veto_coalition).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, treaty_law_supremacy_over_domestic_policy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues binding rulings on trade disputes grounded in treaty text and prior panel precedent. Administers the compliance mechanism: member states must implement rulings or face authorized retaliation. Maintains institutional authority through consistent application of treaty interpretation and enforcement of the ruling mechanism itself.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).

% Gain predictable market access commitments locked in by binding rulings that constrain other member states' policy discretion. Can invoke DSB authority to challenge protectionist measures by trading partners. Possess resources to litigate effectively and influence panel composition and jurisprudence through repeated engagement.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, large_trading_bloc_exporters, beneficiary,
    powerful, generational, arbitrage, global).

% Surrendered policy discretion in trade-covered domains when acceding to WTO. DSB rulings override domestic policy choices; non-compliance triggers authorized retaliation. Cannot defend labor standards, environmental policies, or industrial development strategies that conflict with trade liberalization commitments, even when these serve legitimate domestic interests.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, policy_constrained_member_states, payer,
    moderate, biographical, constrained, national).

% Cannot afford effective legal representation in DSB proceedings; lack technical capacity to navigate complex trade litigation. Face binding rulings that constrain infant-industry protection, domestic subsidy programs, and policy flexibility that larger economies used during their own development. Exit via withdrawal carries catastrophic costs (loss of market access, investment, development finance conditionality).
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, small_developing_economies, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, small_developing_economies, excluded).

% Environmental, labor, public health, and social protection constituencies in member states would advocate for policy autonomy in their domains, but these voices are structurally outside the DSB framework. Cannot present to panels; must route demands through member state governments already committed to the trade-liberalization mandate.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_policy_advocates, excluded,
    powerless, immediate, trapped, national).

% Trade law specialists and institutional actors whose professional standing and authority depend on maintaining the binding-ruling model. Derive career advancement, consulting opportunities, and institutional power from DSB authority. Incentivized to interpret treaty language expansively to extend coverage of trade disciplines, reinforcing institutional scope creep.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, panel_interpreter_community, beneficiary,
    moderate, generational, constrained, global).

% Coalition of large trading blocs that can prevent DSB reform or limitations on panel authority through consensus requirements. Benefits from binding authority as a locked-in mechanism for enforcing their market-access commitments against smaller states' protective measures.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, large_exporting_coalition_veto_coalition, beneficiary,
    powerful, generational, arbitrage, global).

% National competition regulators are excluded from DSB proceedings and cannot defend antitrust, merger review, or competition-based policy choices that conflict with trade liberalization. A DSB ruling can override legitimate competition policy with trade-liberalization precedent.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, competition_authorities_national, excluded,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__binding_referee_reading, wto_dispute_settlement_body).
narrative_ontology:fixing_cost_class(wto_dsb_authority__binding_referee_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a binding dispute-resolution mechanism replacing unilateral retaliation with rule-based arbitration. Coordinates member state behavior within agreed trade commitments by furnishing credible, enforceable interpretation of treaty obligations and authorized remedies for breach.
% TRANSFER_FUNCTION: Transfers policy discretion from member states to the DSB institutional apparatus and panel experts; transfers compliance burden from large exporters (who must negotiate bilaterally for market access) to smaller/protectionist states (who must implement rulings or face retaliation). Large trading blocs gain assured market access; smaller economies lose policy flexibility.
% ABSENT_VOICES: Domestic environmental, labor, public health, and social protection constituencies; national competition authorities; indigenous and subnational communities affected by DSB-mandated policy changes. These actors would demand either exclusion of their policy domains from DSB authority or explicit carve-outs for legitimate policy objectives, but they are structurally outside the framework.
% DISAPPEARANCE_RATIONALE: If binding DSB authority vanished, member states would revert to bilateral negotiation and unilateral retaliation; trade liberalization would remain but enforcement would become unstable. Large exporters would lose the locked-in certainty DSB authority provides; smaller economies would regain policy discretion but face renewed retaliation risk. The global trade system would reorganize around negotiated settlements rather than binding adjudication.
% FOUNDING_PROBLEM: Post-GATT trade disputes were resolved through diplomatic negotiation and power-based retaliation; no neutral mechanism existed to interpret obligations or constrain strong states from arbitrary trade barriers. Smaller economies had no recourse against discriminatory measures by larger trading partners except costly unilateral retaliation.
% FOUNDING_PROBLEM_CORROBORATION: Large exporters and DSB institutional actors attest the founding problem remains live: without binding authority, weak states would face renewed discriminatory barriers and trade rules would collapse. Small developing economies and policy-autonomy advocates attest the founding problem was partially solved (neutral interpretation exists) but the solution created a new problem (policy discretion surrendered; developing economies constrained). Independent trade law scholarship documents the interpretive drift of panel rulings beyond the original Uruguay Round mandate, supporting the contested framing.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 at interval end because the binding-authority model concentrates market-access gains in large exporters while distributing compliance costs asymmetrically to weaker states. The measurement series shows extractiveness rising 0.16 points over the 30-year interval as jurisprudence expands the scope of trade disciplines and smaller economies accumulate compliance burdens. Suppression is high (0.72) and rising (0.14-point increase) because enforcement depends on continuous threat of authorized retaliation and on constraining member states' ability to invoke policy exceptions. Theater ratio climbs from 0.12 to 0.28 (2.33x) as panel rhetoric emphasizes dispute-neutrality while interpretive scope expands — the performance of neutral arbitration increasingly masks institutional scope creep. Accessibility collapse is high (0.79): once a member state is bound by DSB authority, the alternatives (bilateral negotiation, exit, unilateral retaliation) are substantially foreclosed by retaliation authorization and market-access loss. Resistance is moderate-high (0.58) because developing economies and policy advocates mount consistent legal and political challenges, but the large-exporter coalition's veto power over DSB reform prevents structural change.
 *
 * PERSPECTIVAL GAP:
 *   Large exporters and DSB institutional actors compute this constraint as legitimate coordination: binding authority creates certainty, prevents discrimination, and makes trade rules enforceable. Policy-constrained member states and small economies compute it as enforced extraction: their sovereignty is traded away, their domestic policy autonomy is surrendered, and they bear disproportionate suppression burden. The engine derives this divergence from power atom + exit options + beneficiary/victim declarations: institutional actors with arbitrage exit and beneficiary roles experience this constraint differently than powerless, trapped actors in victim roles. No override is needed; structural data alone produces the seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The DSB institutional apparatus and large exporters sit near d=0.0 (beneficiaries): the constraint subsidizes their market certainty and locks in their preferred rules. Policy-constrained moderate-power states sit near d=0.6-0.7 (asymmetric extraction): they bear compliance costs and lost discretion but retain some negotiating capacity. Small developing economies sit near d=0.95 (full targets): trapped exit, powerlessness, and victim status combine to make the constraint's suppressive apparatus fall directly on them. Directionality for moderate-power states reflects their double position: they benefit from the rule-of-law mechanism (certainty, neutral arbitration) but pay heavily in surrendered discretion. No override is needed; the derived d from beneficiary/victim + exit produces accurate positioning.
 *
 * MANDATROPHY ANALYSIS:
 *   The binding-referee reading avoids false-summit error: it explicitly declares beneficiaries (DSB body, large exporters) and victims (constrained states, small economies), so no mountain claim hides behind institutional rhetoric. It names active enforcement (compliance threat, retaliation authorization) that tangled_rope classification requires. The mandate (neutral trade dispute resolution grounded in treaty law) IS live — DSB panels still adjudicate trade disputes, large exporters still depend on binding authority, smaller economies still lose policy discretion. But the reading acknowledges the contested boundary: whether this is legitimate treaty-grounded binding authority (this reading's stance) or illegitimate judicial activism (sibling judicial_activism_reading) or merely advisory coordination dressed up as binding (sibling advisory_coordination_reading) is exactly what the kernel contest addresses. Mandatrophy would be resolved only if the founding problem (unilateral retaliation, power-based trade chaos) genuinely disappeared — but the reading documents this problem as contested: large actors attest it is live, smaller actors attest it was partially solved but the solution created a new problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_authority_vs_treaty_intent,
    'Did the Uruguay Round drafters explicitly intend DSB panels to issue binding rulings, or was binding authority an institutional evolution beyond original intent?',
    'Treaty drafting history and negotiating records (GATT Secretariat archives, WTO founding documents); comparative analysis with prior dispute mechanisms (GATT panels were non-binding advisory); panel jurisprudence evolution tracing the first assertion of binding authority.',
    'If binding authority was explicitly intended, the constraint is legitimate treaty-grounded institutional design; if it evolved beyond intent, the judicial-activism sibling reading gains structural plausibility. The boundary between these readings turns on this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_authority_vs_treaty_intent, empirical, 'Whether binding DSB authority is treaty-explicit or institutionally evolved.').

omega_variable(
    policy_discretion_surrender_scope,
    'Did member states knowingly and deliberately surrender policy discretion in trade-covered domains, or did they bind negotiated commitments while retaining implicit policy flexibility not captured by treaty text?',
    'Analysis of developing-economy accession negotiations and side agreements; domestic legislative ratification debates and reservation statements; subsequent member-state invocation of policy-exception clauses (GATT Article XX carve-outs); litigation patterns showing whether states attempt to defend non-trade policies as out-of-scope.',
    'Full deliberate surrender supports the binding-referee reading; partial surrender or implicit flexibility supports the advisory-coordination sibling (states retain ultimate discretion). The extractiveness rating depends on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_discretion_surrender_scope, empirical, 'Whether policy discretion surrender was explicit and deliberate or partial and contested.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) a structural mechanism (retaliation threat, market-access loss) or internalized (member states have adopted trade-liberalization ideology and no longer wish to deviate)?',
    'Comparative analysis of DSB rulings where affected states resist vs. where they rapidly comply without retaliation threat; member-state statements defending trade-liberalization commitments; post-exit trajectories of members that have withdrawn from WTO or renegotiated commitments.',
    'If suppression is mostly structural, exit-driven alternatives are genuinely closed and the constraint''s extractiveness is accurate. If suppression is largely internalized, exit cost is lower and member states have less claim to victim status; the constraint becomes more consensual.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in DSB compliance.').

omega_variable(
    institutional_identity_lock_panel_experts,
    'To what extent has the panel-interpreter community fused its professional identity with the binding-authority model, creating institutional path-dependence that prevents recognition of scope creep?',
    'Career trajectory analysis of trade law specialists; survey of panel-expert institutional incentives and prestige allocation; examination of whether dissident voices within the community publicly acknowledge interpretive drift or challenge the binding model.',
    'High identity-lock among panel experts reinforces scope expansion and makes reform-oriented redesign structurally difficult. Recognition of this lock enables the judicial-activism sibling reading to gain plausibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_lock_panel_experts, conceptual, 'Professional identity fusion in the DSB interpreter community.').

omega_variable(
    kernel_contested_reading_ambiguity,
    'Is this constraint best understood as the binding-referee reading (panels are binding arbiters grounded in explicit treaty surrender), or is the advisory-coordination sibling equally defensible within the treaty text?',
    'Close reading of DSB establishment text (WTO Understanding on Rules and Procedures for Dispute Settlement); comparison of language describing DSB authority with language in prior non-binding mechanisms (GATT panels); judicial interpretation patterns showing whether panels invoke binding authority as explicit or derived.',
    'If the treaty text genuinely supports both readings (ambiguous language), then this constraint and its advisory sibling are equally legitimate readings of the same kernel. If the binding reading is clearly textually grounded, the advisory sibling is a misreading. This determines whether the siblings coexist or whether one forecloses the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contested_reading_ambiguity, conceptual, 'Textual ambiguity in DSB authority framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_dsb_authority__binding_referee_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(wto__tr_t0, observed).
narrative_ontology:measurement(wto__tr_t5, wto_dsb_authority__binding_referee_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(wto__tr_t5, observed).
narrative_ontology:measurement(wto__tr_t10, wto_dsb_authority__binding_referee_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(wto__tr_t10, observed).
narrative_ontology:measurement(wto__tr_t15, wto_dsb_authority__binding_referee_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(wto__tr_t15, observed).
narrative_ontology:measurement(wto__tr_t20, wto_dsb_authority__binding_referee_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(wto__tr_t20, observed).
narrative_ontology:measurement(wto__tr_t25, wto_dsb_authority__binding_referee_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(wto__tr_t25, observed).
narrative_ontology:measurement(wto__tr_t30, wto_dsb_authority__binding_referee_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(wto__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_dsb_authority__binding_referee_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(wto__be_t0, observed).
narrative_ontology:measurement(wto__be_t5, wto_dsb_authority__binding_referee_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(wto__be_t5, observed).
narrative_ontology:measurement(wto__be_t10, wto_dsb_authority__binding_referee_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(wto__be_t10, observed).
narrative_ontology:measurement(wto__be_t15, wto_dsb_authority__binding_referee_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(wto__be_t15, observed).
narrative_ontology:measurement(wto__be_t20, wto_dsb_authority__binding_referee_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(wto__be_t20, observed).
narrative_ontology:measurement(wto__be_t25, wto_dsb_authority__binding_referee_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(wto__be_t25, observed).
narrative_ontology:measurement(wto__be_t30, wto_dsb_authority__binding_referee_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(wto__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_dsb_authority__binding_referee_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(wto__su_t0, observed).
narrative_ontology:measurement(wto__su_t5, wto_dsb_authority__binding_referee_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(wto__su_t5, observed).
narrative_ontology:measurement(wto__su_t10, wto_dsb_authority__binding_referee_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(wto__su_t10, observed).
narrative_ontology:measurement(wto__su_t15, wto_dsb_authority__binding_referee_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(wto__su_t15, observed).
narrative_ontology:measurement(wto__su_t20, wto_dsb_authority__binding_referee_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(wto__su_t20, observed).
narrative_ontology:measurement(wto__su_t25, wto_dsb_authority__binding_referee_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(wto__su_t25, observed).
narrative_ontology:measurement(wto__su_t30, wto_dsb_authority__binding_referee_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(wto__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__binding_referee_reading, 0.12).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, regional_trade_agreement_dispute_mechanisms).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, infant_industry_protection_constraints).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, labor_and_environmental_policy_carve_outs).

% DUAL FORMULATION NOTE:
% This constraint is one reading of kernel_wto_dsb_authority. The kernel is the institutional authority of DSB panels to adjudicate trade disputes. Sibling readings (separate constraint stories) include advisory_coordination_reading (panels facilitate negotiated settlements; members retain discretion) and judicial_activism_reading (panels exceed mandate through interpretive drift). The three readings share the same referent (DSB institutional setup) but differ in their core structural claim: this reading asserts binding authority grounded in explicit treaty surrender; siblings contest either the binding nature or the legitimacy of that authority. Decomposition follows ε-invariance: each reading instantiates different beneficiary/victim structures, different assessed extractiveness, and different legitimacy framings of the same institutional arrangement. Link all three stories via network.affects_constraints for kernel-analysis navigation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
