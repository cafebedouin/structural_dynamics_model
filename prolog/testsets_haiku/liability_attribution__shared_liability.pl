% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__shared_liability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_attribution__shared_liability
 *   human_readable: Joint Liability Attribution Along Value Chain
 *   domain: legal/technological governance
 *
 * SUMMARY:
 *   Joint liability frameworks distribute responsibility for harms across the
 *   developer-deployer value chain, allocating liability according to causal
 *   contribution and operational control. Developers are held liable for code
 *   defects and capability design choices; deployers are held liable for
 *   operational decisions, configuration, and context-specific deployment.
 *   The framework is presented as efficient incentive alignment—making each
 *   party responsible for harms they can influence. The claimed type is
 *   tangled_rope: genuine coordination problem (developers and deployers
 *   cannot independently control system safety) meets asymmetric extraction
 *   (insurance and legal intermediaries capture significant rents, while
 *   developers and deployers both bear expanded liability exposure and
 *   transaction costs). The claim/metric independence principle is applied:
 *   the constraint is CLAIMED as tangled_rope based on structural
 *   interdependence, while metrics describe moderately high extractiveness
 *   (0.68 at interval end) because coordination overhead (indemnification,
 *   insurance, dispute resolution) consumes significant value without
 *   corresponding harm reduction.
 *
 * KEY AGENTS:
 *   - developers: creators of capability; bear joint liability for code defects and capability design; constrained exit (cannot serve markets enforcing shared liability)
 *   - deployers: operational decision-makers; bear joint liability for deployment context, configuration, and operational decisions; constrained exit (cannot adopt capabilities without accepting shared liability)
 *   - liability_insurance_market: benefits from bifurcated premium structures and expanded underwriting scope; mobile beneficiary position
 *   - legal_intermediaries: benefits from complexity of causal attribution disputes and indemnification contract negotiation; mobile beneficiary position
 *   - regulatory_authorities: set the allocation rules and define causal contribution; agenda_setter role; institutional power
 *   - single_liability_advocates: excluded from shared liability regime design; would restructure to single-party allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.68).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.55).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Joint Liability Attribution Along Value Chain").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "legal/technological governance").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb').
narrative_ontology:cs_kernel_codification('43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb', formalized).
narrative_ontology:cs_authority_grounding('43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb', extraction).
narrative_ontology:cs_interpretation_layer_present('43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb').
narrative_ontology:cs_reading_relation('43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb', foundational, joint_causal_accountability).
narrative_ontology:cs_axiom_status(joint_causal_accountability, holdable).
narrative_ontology:cs_axiom_grounding('43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb', joint_causal_accountability, deontological).
narrative_ontology:cs_axiom('43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb', foundational, distributed_control_requires_distributed_liability).
narrative_ontology:cs_axiom_status(distributed_control_requires_distributed_liability, holdable).
narrative_ontology:cs_axiom_grounding('43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb', distributed_control_requires_distributed_liability, instrumental).
narrative_ontology:cs_reference_frame('43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb', proportional_causal_contribution_allocation).
narrative_ontology:cs_drift_state('43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb', contemporary_transaction_cost_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('43c9fcc9-c5ad-4e59-8dc6-4f618d28b3cb', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, liability_insurance_market).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, legal_intermediaries).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, deployers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, developers).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Create software capabilities and distribute them to deployers. Under shared liability, they bear joint responsibility for harms from deployment even when they lack visibility into deployment context, configuration, or downstream use. They pay through liability exposure, insurance premiums, and indemnification obligations. They benefit from market access—deployers will adopt their product—but the liability framework introduces friction and uncertainty. Exit means not developing certain capabilities or not participating in jurisdictions that impose shared liability.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, developers, beneficiary).

% Operate systems using developer-created capabilities in specific contexts with direct control over deployment architecture, configuration, and operational decisions. Under shared liability, they bear joint responsibility for harms even where developer decisions or code defects contributed. They pay through liability exposure and insurance. They benefit from access to developer innovation and reduced total-system liability concentration. Exit means forgoing capability adoption or migrating to single-liability regimes.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, deployers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, deployers, beneficiary).

% Underwrites both developer and deployer liability exposure under shared liability regimes. The framework creates a coordination problem for insurers—they must assess causal contribution across the value chain, creating new underwriting categories, risk modeling, and premium structures. They benefit from expanded insurable populations and higher aggregate premiums (multiple seats paying insurance rather than single-party regimes). The indemnification and contractual allocation requirements create downstream insurance products and cross-policing mechanisms.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, liability_insurance_market, beneficiary,
    powerful, generational, mobile, global).

% Advise developers and deployers on liability allocation, indemnification clauses, and risk mitigation under shared liability. The framework's complexity—allocating causal contribution and control across contractual and operational boundaries—creates recurring demand for legal expertise. They benefit from higher transaction volumes and complexity-driven billable hours. Exit is uncompelled; they are mobile beneficiaries.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, legal_intermediaries, beneficiary,
    powerful, biographical, mobile, global).

% Suffer harms from system failures and seek recovery through liability mechanisms. Under shared liability, they must identify and pursue multiple defendants (developers and deployers), creating litigation complexity. They benefit from expanded defendant pools—more parties to recover from—but bear the burden of proving causal contribution to establish claims against each. They have no role in setting the constraint; their observation seat notes the constraint's effects on their capacity to recover.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, liability_claimants, observer,
    powerless, immediate, trapped, global).

% Enact and enforce shared liability frameworks via statute, common law evolution, or regulatory guidance. They set the allocation rules, define causal contribution and control, and shape how disputes are adjudicated. They enforce through courts and administrative bodies. Their authority is exercised through jurisdictional law; exit is not an option within their territorial scope. They set the agenda because they define the liability attribution framework itself.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Argue for single-party (developer-only or deployer-only) liability regimes on grounds of clear incentive alignment, reduced transaction costs, and alignment with actual control. They are excluded from the shared liability design process because the constraint represents a rejection of their framing. They would restructure the liability framework if given design authority but are not party to the shared liability regime's governance.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, single_liability_advocates, excluded,
    organized, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, liability_insurance_market).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes liability exposure and responsibility across the value chain to align incentives: developers are exposed to harms from their code decisions; deployers are exposed to harms from their operational decisions and context control. The framework aims to incentivize both parties to exercise due care rather than concentrating liability in one party who cannot control the other's decisions.
% TRANSFER_FUNCTION: Moves liability exposure and insurance costs from concentrated parties (developers or deployers alone) to both, and simultaneously moves wealth to insurance and legal services providers through indemnification clauses, insurance premiums, and dispute resolution costs. Liability claimants gain expanded defendant pools but face increased litigation complexity.
% ABSENT_VOICES: Single-liability advocates (developers and deployers who argue for regime change) and deployment contexts where opacity prevents meaningful causal attribution (embedded systems, supply-chain dependencies) are structurally excluded. They cannot participate in shared liability regime design and would argue the framework is unworkable without full transparency or clear control boundaries.
% DISAPPEARANCE_RATIONALE: If joint liability attribution vanished, liability regimes would revert to single-party (developer or deployer) allocation, indemnification contract terms would collapse, liability insurance products would consolidate, and legal demand for causal-attribution disputes would fall sharply. The value chain would reorganize around whichever single party bears the liability burden.
% FOUNDING_PROBLEM: Early strict-liability or single-party regimes created perverse incentives: developer liability frameworks left deployers with no accountability for operational decisions; deployer liability frameworks left developers with no accountability for code defects or capabilities. Neither party could control the other; liability concentration in one party created systematic under-care incentives for the other.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory authorities and legal scholarship outside the benefiting parties (insurance, law firms) attest the founding problem was real in early regimes. However, whether shared liability actually solves it remains contested: developers and deployers argue the coordination overhead exceeds the incentive benefit; insurance carriers and legal services providers attest to market growth; empirical causal studies on incentive alignment remain sparse.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__shared_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__shared_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__shared_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.48 → 0.68) as indemnification contract markets mature and insurance carriers develop specialized products for shared liability regimes—the transaction-cost overhead grows without corresponding reduction in actual harm rates. Theater_ratio plateaus at 0.42 by t=20, indicating that causal attribution disputes and allocation mechanisms consume a stable but significant share of enforcement activity. Suppression_requirement climbs to 0.55 because regulatory authorities must actively enforce causal-contribution allocation rules against parties incentivized to shift liability unilaterally—deployers want developers to bear more; developers want deployers to bear more. The temporal pattern captures two dynamics: (1) early maturation of insurance and legal markets (t=0–10: rapid extractiveness growth), and (2) stabilization as liability-allocation norms ossify and transaction costs become predictable (t=15–25: slower growth, flattening theater ratio). The metrics do not reconcile with the claimed tangled_rope type by design—the claim rests on genuine coordination interdependence; the metrics capture the actual extraction overlay that markets and legal services impose.
 *
 * PERSPECTIVAL GAP:
 *   Developers and deployers experience this constraint differently due to opacity and control asymmetry. A developer with full visibility into all deployed instances has higher d (directionality toward target) than a deployer with full deployment context control but no visibility into code complexity. A developer operating open-source software with no contractual indemnification agreements is trapped; a developer with standard SaaS indemnification clauses is constrained but not trapped. A deployer with operational control but no ability to audit code is constrained; a deployer with contract-negotiated liability caps is less constrained. The engine computes per-seat directionality from these structural differences; from the regulatory perspective, the constraint is symmetric (both parties share liability); from the market-access perspective, developers bear higher structural exposure (they cannot see deployment choices) and deployers bear higher operational exposure (they cannot control code). This perspectival gap is the constraint's structural signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers: moderate power, constrained exit (cannot serve markets enforcing shared liability without accepting the framework), biographical time horizon. They are structurally targets because liability exposure exceeds their actual control over deployment outcomes. Without full visibility into deployment, they face open-ended liability for harms from deployer decisions. Directionality: ~0.68 (moderately toward target). Deployers: organized power (collectively), constrained exit (cannot adopt capabilities without accepting shared liability), biographical time horizon. They face symmetric structural exposure: operational control gives them accountability for deployment decisions, but they cannot control code quality or capability design. Directionality: ~0.55 (slightly toward target, balanced by operational control). Insurance carriers and legal services: institutional/powerful power, mobile exit (can enter/exit markets offering shared-liability products), generational/biographical time horizon. They are structurally beneficiaries—shared liability creates new risk categories, indemnification contracts, and dispute complexity. Directionality: ~0.15 (clearly toward beneficiary). Regulatory authorities: institutional power, analytical exit, set the rules. Directionality: ~0.5 (symmetric, or overridden by their agenda-setter role). The directionality chain derives from: (1) beneficiary/victim declarations in base_properties, (2) exit_options and power from stakeholders, (3) no overrides needed—structural derivation captures the asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (single-party liability created perverse incentives) was real but is now contested as solved. Developers and deployers argue the shared liability framework substitutes coordination overhead for actual harm reduction—they bear higher transaction costs without measurable improvement in safety outcomes. Regulatory authorities and insurance carriers attest the framework is working as designed and creates appropriate incentive alignment. The classification prevents the constraint from being misread as pure rope (genuine coordination) or pure snare (pure extraction): it is tangled_rope because genuine coordination interdependence exists (neither party can unilaterally ensure safety) AND extraction is substantial (insurance and legal services capture significant rents). The mandatrophy signal is the flattening theater_ratio after t=15—once indemnification contracts and insurance categories mature, the causal-attribution disputes become increasingly ritualized, suggesting the coordination function is atrophying while extraction persists. This is a leading indicator of potential mandatrophy: the founding problem (incentive misalignment) may be dead, but the constraint persists as institutional inertia because exit from shared liability regimes is too costly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_opacity,
    'Can causal contribution and operational control be reliably attributed across the value chain when code complexity, supply-chain dependencies, and deployment architecture create irreducible opacity?',
    'Empirical audits of liability dispute resolution: track what fraction of causal-attribution disputes can be adjudicated with high confidence vs. settle on indemnification defaults. Compare across jurisdictions with different opacity-handling rules (mandatory disclosure, presumptions, caps).',
    'High opacity would suggest causal attribution is theatrical—disputes resolve via contract negotiation and insurance rather than genuine causal measurement—which would elevate theater_ratio closer to 0.6+ and reclassify toward piton. Low opacity would support the coordination claim and sustain tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_opacity, empirical, 'Whether causal contribution can be reliably measured or defaults to contractual allocation.').

omega_variable(
    extraction_vs_coordination_overhead,
    'Does the measured extractiveness (insurance premiums, legal fees, indemnification complexity) reflect genuine coordination costs (necessary to align incentives) or rent capture by intermediaries (insurance/legal services)?',
    'Comparative study of liability dispute costs (legal time, insurance underwriting, settlement duration) across shared_liability, developer_liability, and deployer_liability regimes. Measure harm-reduction outcomes per unit cost invested.',
    'If extractiveness scales with intermediary profits but NOT with harm reduction, the constraint is snare-flavored (coordination story is cover). If extractiveness scales with both, tangled_rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_overhead, empirical, 'Whether transaction costs correspond to coordination value or intermediary rent-seeking.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Within a single regulatory jurisdiction, can shared_liability coexist with developer_liability or deployer_liability, or does one reading''s adoption foreclose the others?',
    'Regulatory history: jurisdictions that shifted from one reading to another; analysis of whether transition was mandated foreclosure or optional substitution.',
    'If readings foreclose each other within a jurisdiction, the reading_relations should be ''forecloses''. If jurisdictions can adopt any reading independently, the relation is ''coexists_with'' (which is the current omega''s working assumption). Foreclosure would indicate strong normative commitment; coexistence would indicate reading choice is jurisdictional policy, not logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether liability readings are mutually exclusive or policy choices.').

omega_variable(
    mandatrophy_signal_validation,
    'Is the flattening theater_ratio (t=15–25) a sign that causal-attribution disputes have become ritualized (mandatrophy candidate), or a sign that norms have stabilized and transaction costs have become predictable (healthy equilibrium)?',
    'Time-series analysis of dispute outcomes: track variance in causal-attribution judgments over time. Declining variance would suggest ritualization (mandatrophy); stable variance would suggest equilibrium.',
    'If mandatrophy is confirmed, the constraint should be reclassified as piton by t=25. If norms have simply stabilized, tangled_rope holds. The distinction is measurable via judgment variance, not via theater_ratio alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_signal_validation, empirical, 'Whether theater-ratio plateau indicates normalization or atrophied function.').

omega_variable(
    shared_liability_reading_identity,
    'Is the shared_liability reading a stable equilibrium interpretation of the liability_attribution kernel, or is it an unstable middle position that regulators use transitionally before settling on a single-party reading?',
    'Regulatory longitudinal study: track how many jurisdictions adopt shared_liability and remain stable vs. revert to or pivot toward single-party readings over a 30+ year horizon.',
    'Persistent adoption would support shared_liability as a legitimate reading coexisting with single-party alternatives. High reversion would suggest shared_liability is a transient policy experiment, not a stable reading of the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(shared_liability_reading_identity, empirical, 'Whether shared liability is a stable reading or a transitional regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(liab_tr_t0, observed).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__shared_liability, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(liab_tr_t5, observed).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__shared_liability, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(liab_tr_t10, observed).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__shared_liability, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(liab_tr_t15, observed).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(liab_tr_t20, observed).
narrative_ontology:measurement(liab_tr_t25, liability_attribution__shared_liability, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(liab_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(liab_be_t0, observed).
narrative_ontology:measurement(liab_be_t5, liability_attribution__shared_liability, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(liab_be_t5, observed).
narrative_ontology:measurement(liab_be_t10, liability_attribution__shared_liability, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(liab_be_t10, observed).
narrative_ontology:measurement(liab_be_t15, liability_attribution__shared_liability, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(liab_be_t15, observed).
narrative_ontology:measurement(liab_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(liab_be_t20, observed).
narrative_ontology:measurement(liab_be_t25, liability_attribution__shared_liability, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(liab_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(liab_su_t0, observed).
narrative_ontology:measurement(liab_su_t5, liability_attribution__shared_liability, suppression_requirement, 5, 0.47).
narrative_ontology:measurement_basis(liab_su_t5, observed).
narrative_ontology:measurement(liab_su_t10, liability_attribution__shared_liability, suppression_requirement, 10, 0.51).
narrative_ontology:measurement_basis(liab_su_t10, observed).
narrative_ontology:measurement(liab_su_t15, liability_attribution__shared_liability, suppression_requirement, 15, 0.53).
narrative_ontology:measurement_basis(liab_su_t15, observed).
narrative_ontology:measurement(liab_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(liab_su_t20, observed).
narrative_ontology:measurement(liab_su_t25, liability_attribution__shared_liability, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(liab_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(liability_attribution__shared_liability, 0.16).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).

% DUAL FORMULATION NOTE:
% The liability_attribution kernel decomposes into three structurally distinct constraints: developer_liability (developers bear primary responsibility), deployer_liability (deployers bear primary responsibility), and shared_liability (joint responsibility distributed by causal contribution and control). Each reading instantiates different beneficiary/victim structures, different ε values, and different enforcement mechanisms. The three readings coexist across different jurisdictions and regulatory regimes. Shared_liability is the upstream reading in the network: it influences and responds to both single-party readings because it occupies the structural middle ground—it creates pressure on single-party regimes to compete on efficiency but does not foreclose either single-party reading. The constraint family is linked via network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
