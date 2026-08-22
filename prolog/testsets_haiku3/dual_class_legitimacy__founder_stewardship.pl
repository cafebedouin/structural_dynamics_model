% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__founder_stewardship, []).

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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Dual-Class Share Structure as Mission Protection (Founder Stewardship Reading)
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   Under the founder-stewardship reading of dual-class legitimacy,
 *   concentrated founder control is justified as enabling long-horizon
 *   mission execution. The founder holds Class A shares with supermajority
 *   voting power; other shareholders (including long-term capital allocators)
 *   hold Class B shares with minimal voting rights but full economic
 *   interest. The reading frames control concentration as coordination — the
 *   founder acts as a trusted steward whose interests align with all
 *   shareholders' long-term interests. Class A voting concentration protects
 *   against short-term activist pressure that would corrupt the mission. The
 *   constraint is ONE reading of a contested kernel; the sibling readings
 *   (minority_extraction, disclosure_consent) instantiate different
 *   constraints with different ε values and different benefit/victim
 *   structures.
 *
 * KEY AGENTS:
 *   - Founder (steward with locked-in control, identity-fused with the enterprise)
 *   - Long-term shareholders (benefit from founder's multi-decade strategy)
 *   - Minority Class B shareholders (bear governance risk, benefit from mission success)
 *   - Activist investors (excluded, arbitrage available)
 *   - Securities regulator (enforces disclosure and fiduciary-duty framing)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.38).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.22).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.38).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Share Structure as Mission Protection (Founder Stewardship Reading)").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, 'bd956c45-594e-4f5b-9336-451e99289375').
narrative_ontology:cs_kernel_codification('bd956c45-594e-4f5b-9336-451e99289375', formalized).
narrative_ontology:cs_authority_grounding('bd956c45-594e-4f5b-9336-451e99289375', extraction).
narrative_ontology:cs_interpretation_layer_present('bd956c45-594e-4f5b-9336-451e99289375').
narrative_ontology:cs_reading_relation('bd956c45-594e-4f5b-9336-451e99289375', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('bd956c45-594e-4f5b-9336-451e99289375', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('bd956c45-594e-4f5b-9336-451e99289375', foundational, founder_mission_alignment_serves_all).
narrative_ontology:cs_axiom_status(founder_mission_alignment_serves_all, holdable).
narrative_ontology:cs_axiom_grounding('bd956c45-594e-4f5b-9336-451e99289375', founder_mission_alignment_serves_all, instrumental).
narrative_ontology:cs_axiom('bd956c45-594e-4f5b-9336-451e99289375', foundational, voting_parity_enables_short_termism).
narrative_ontology:cs_axiom_status(voting_parity_enables_short_termism, holdable).
narrative_ontology:cs_axiom_grounding('bd956c45-594e-4f5b-9336-451e99289375', voting_parity_enables_short_termism, empirically_contingent).
narrative_ontology:cs_reference_frame('bd956c45-594e-4f5b-9336-451e99289375', founder_steward_credible_commitment).
narrative_ontology:cs_drift_state('bd956c45-594e-4f5b-9336-451e99289375', contemporary_activist_pressure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bd956c45-594e-4f5b-9336-451e99289375', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_steward).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, long_term_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, mission_beneficiaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, minority_class_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, minority_class_shareholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the company through concentrated Class A voting shares. Justifies dual-class structure as necessary to pursue long-term mission without pressure from quarterly earnings cycles or activist investors. Cannot exit without dissolving personal identity constituted through the enterprise; exit is the negation of the founder role itself.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_steward, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Hold Class B shares (or equivalent minority-voting class) and benefit from founder's mission-driven decisions: R&D investments that payoff over decades, refusal to cut corners for quarterly targets, strategic patience in new markets. Can exit by selling shares in liquid markets; market liquidity makes exit mobile despite control concentration.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, long_term_shareholders, beneficiary,
    powerful, generational, mobile, global).

% Hold Class B (or lower-voting) shares providing economic interest but limited voting power. Bear the cost of decisions they do not control (founder can commit capital to long-term bets without their consent). Also benefit from founder's long-horizon strategy when it succeeds. Their exit is liquid but constrained: selling means abandoning any future upside from the mission they partly funded but could not direct.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, minority_class_shareholders, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, minority_class_shareholders, beneficiary).

% Non-shareholder beneficiaries of the company's stated mission (e.g., users of a free service, beneficiaries of impact investments, public-good consumers). Benefit from founder's ability to refuse short-term monetization or asset-strip decisions. Have no voice in governance and cannot exit except by ceasing to use the service.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, mission_beneficiaries, beneficiary,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(dual_class_legitimacy__founder_stewardship, mission_beneficiaries).

% Would acquire shares and push for immediate value extraction (dividend, share buyback, acquisition at premium, cost-cutting) if voting control were proportional to capital. Excluded by dual-class structure; their exclusion is the enforcement object itself. Arbitrage exit: can buy competitor or invest elsewhere immediately.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, activist_investor, excluded,
    powerful, immediate, arbitrage, global).

% Enforces disclosure obligations and corporate-governance rules. Under this reading, accepts dual-class structure as compatible with fiduciary duty IF the founder's stewardship track record and disclosure are adequate. Can require enhanced disclosure or impose remedies if stewardship fails or becomes exploitative.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_regulator, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__founder_stewardship, founder_steward).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__founder_stewardship, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates long-horizon investment and mission commitment by protecting strategic decisions from short-term capital markets pressure, activist demands, and quarterly earnings focus. Solves the founder's problem: how to credibly commit to multi-decade bets when public capital markets reward near-term returns.
% TRANSFER_FUNCTION: Transfers voting power from proportional capital ownership to concentrated founder control. Moves governance discretion from all shareholders to the founder; majority Class B shareholders accept voting disadvantage in exchange for founder's mission-driven strategy and capital discipline.
% ABSENT_VOICES: Short-term traders and activist investors are structurally excluded; they would argue for liquidity-focused, near-term value extraction and find the dual-class constraint a barrier. Rival founders or alternative-governance coalitions who might propose shareholder democracy are kept out by the founder's locked-in control. Minority shareholders who dissent from the mission have no mechanism to redirect strategy despite bearing capital risk.
% DISAPPEARANCE_RATIONALE: If dual-class structure and founder lock-in vanished overnight, activist investors would immediately acquire stakes and push for value extraction, company strategy would shift to short-term returns and cost-cutting, long-term R&D investments would be cut, and the mission would be deprioritized in favor of shareholder payouts. The company's governance would reorganize around capital-weighted voting and quarterly earnings targets.
% FOUNDING_PROBLEM: Founder needed credible assurance that strategic control would survive public capital markets, so that long-term bets and mission investments could be made without fear of activist takeover or forced sale.
% FOUNDING_PROBLEM_CORROBORATION: Founder attests the problem is live, citing historical examples of mission-dilution after activist pressure (Yahoo, Hewlett-Packard) and the structural incentives of public markets. Long-term investors and board-aligned observers corroborate that founder lock-in has enabled multi-year R&D cycles that would not survive proportional-voting governance. Financial economists studying founder-led companies (Shleifer, Wolfenzon) document the credible-commitment function. Dissident shareholders and activism scholars (Bebchuk) contest that the founding problem justifies the degree of founder insulation observed.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__founder_stewardship_tests).
:- end_tests(dual_class_legitimacy__founder_stewardship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end, rising from 0.28 at start) because the founder's control enables him to make decisions other shareholders would not choose, and minority shareholders have no remedy — that is extraction by the structural definition. However, the extraction is bounded: the founder cannot freely pocket capital (disclosure and fiduciary duty apply); his benefit is constrained to strategic discretion, not cash flow diversion. Theater is low (0.18) because the structure's function is genuine (credible long-horizon commitment) and the operational framing (fiduciary stewardship) is substantive, not theatrical. Suppression is low (0.22) because minority shareholders retain exit (liquid markets), and founder lock-in functions through structural lock (dual-class voting), not through coercive impediment to alternatives. The measurement series show extractiveness and suppression requirements rising gently over the interval — a sign that activist pressure has increased and the constraint's enforcement burden has grown as founder ages or company faces strategic crossroads.
 *
 * PERSPECTIVAL GAP:
 *   The founder-steward seat experiences the constraint as enabling coordination — the structure makes his long-term strategy credible and protects mission integrity. The minority-shareholder seat experiences the same structure as extractive governance concentration — they fund the business but cannot direct strategy, and exit is available but costly (holding forfeits future upside). Long-term shareholders aligned with founder strategy experience coordination benefit; short-term traders excluded by the structure experience it as a barrier. The engine computes this divergence from power + time_horizon + exit_options + directed capital flows; the authored claim (rope / coordination) does not adjudicate the computed classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The founder (institutional power, civilizational horizon, identity-locked) sits near the beneficiary end of the directionality spectrum — the structure advances his strategic autonomy and aligns his incentives with the mission. Long-term shareholders (powerful, generational, mobile exit) sit near symmetric — they fund the enterprise and benefit from strategy, but accept voting disadvantage. Minority Class B shareholders (moderate power, biographical horizon, constrained exit) sit closer to the target end — they bear governance risk without corresponding control. The founder's identity-lock is structural: exit means ceasing to be the founder, so his exit_options are identity-locked by definition, which modulates his directionality toward full beneficiary. Activist investors (excluded) are not stakeholders in this reading's constraint — their exclusion is the enforcement object itself, not a party to the coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is LIVE under this reading: founder still needs credible assurance against activist takeover and short-term pressure, long-term shareholders still need founder lock-in to protect strategy. If the mandate had died (e.g., the company executed the mission, founder retired, and the long-term strategy is now locked in by institutional momentum), the constraint would become a piton — maintained theatrically by founder attachment and board ritual, no longer serving the original function. Current state: mandate is live; extractiveness is moderate and bounded by fiduciary duty; resistance from excluded parties (activists, dissident shareholders) is real but cannot overturn the structure. The constraint is a ROPE by this reading's framing (genuine coordination function), though the sibling readings (minority_extraction, disclosure_consent) classify the same structure as extractive or consent-dependent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_vs_entrenchment_boundary,
    'At what point does founder control shift from enabling long-horizon mission execution to entrenchment that serves founder interests at shareholder expense?',
    'Post-founder succession: does the constraint persist and continue serving long-term shareholders, or does it become a mechanism for founder''s estate/legacy protection at economic cost? Comparative analysis across founder-led companies with/without succession crises.',
    'If succession maintains mission-alignment and shareholder benefit, stewardship framing is validated (constraint remains rope). If succession reveals founder control was primarily value-extraction mechanism, reclassify as snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_vs_entrenchment_boundary, empirical, 'Whether founder lock-in serves mission or founder-legacy extraction.').

omega_variable(
    mission_definition_contestation,
    'Is the founder''s mission definition binding on all shareholders, or should shareholders have ongoing voice in mission revision?',
    'Governance contest or shareholder revolt over mission pivot: if founder unilaterally changes mission and shareholders lack remedy, governance was extraction all along. If shareholder voice constrains founder mission-setting, coordination was genuine.',
    'If mission is unilaterally founder-set, extraction is higher than measured — founder extracts not just governance discretion but also the right to redefine what the company does. If shareholders retain mission-voice despite voting disadvantage, extraction is lower — discretion is bounded by stakeholder expectations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mission_definition_contestation, conceptual, 'Whether mission is founder-controlled or stakeholder-negotiated.').

omega_variable(
    founder_age_and_succession_uncertainty,
    'Does founder age or health uncertainty alter the constraint''s classification? Is near-term succession planning visible, or is the control transition opaque?',
    'Founder succession announcement or death: does the constraint persist in recognizable form or does it collapse/transform? Does board have pre-existing succession plan or does control pass to founder''s heirs by default?',
    'If succession is planned and board-managed, stewardship framing is stronger (constraint is rope with transition mechanism). If succession is opaque or defaults to founder''s heirs, constraint becomes snare or piton (extraction mechanism for founder dynasty, not for mission).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_age_and_succession_uncertainty, empirical, 'Whether founder control is meant to be permanent or is structured for transition.').

omega_variable(
    reading_foreclosure_boundary,
    'Does the founder_stewardship reading logically foreclose the minority_extraction reading, or do they coexist as live positions in public discourse?',
    'Adjudication by Delaware courts, securities regulators, or legislative bodies: does law recognize stewardship as a legitimate governance function, or does it classify founder lock-in as inherently extractive? Can both readings coexist within a single legal framework?',
    'If foreclosed: one reading wins and the other is legally unavailable; classification becomes deterministic. If coexists_with: both readings remain live positions held by different parties (founder + aligned shareholders vs. dissidents + activists); the constraint has genuine class disagreement about its nature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether stewardship and extraction readings are logically incompatible or structurally coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.08).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__founder_stewardship, theater_ratio, 5, 0.11).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__founder_stewardship, theater_ratio, 10, 0.14).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__founder_stewardship, theater_ratio, 15, 0.16).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__founder_stewardship, theater_ratio, 20, 0.17).
narrative_ontology:measurement(dual_tr_t25, dual_class_legitimacy__founder_stewardship, theater_ratio, 25, 0.18).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__founder_stewardship, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__founder_stewardship, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__founder_stewardship, base_extractiveness, 15, 0.37).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__founder_stewardship, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(dual_be_t25, dual_class_legitimacy__founder_stewardship, base_extractiveness, 25, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__founder_stewardship, suppression_requirement, 5, 0.15).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__founder_stewardship, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__founder_stewardship, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__founder_stewardship, suppression_requirement, 20, 0.21).
narrative_ontology:measurement(dual_su_t25, dual_class_legitimacy__founder_stewardship, suppression_requirement, 25, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, identity_coordination).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__founder_stewardship, 0.12).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the dual_class_legitimacy kernel. The sibling constraints (minority_extraction, disclosure_consent) instantiate different ε values and beneficiary/victim structures because they reading the same institutional arrangement (dual-class voting) from different normative frames. Decomposition follows DP-001 (ε-invariance): a single natural-language concept (dual-class legitimacy) that decomposes into structurally distinct constraints when different reading frames are applied.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__founder_stewardship, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
