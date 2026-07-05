% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__developmental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__developmental_reading, []).

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
 *   constraint_id: wto_treaty_framework__developmental_reading
 *   human_readable: WTO Treaty Framework — Developmental Reading (Policy Space, S&D, Technology Transfer)
 *   domain: international_trade_law/development_economics
 *
 * SUMMARY:
 *   This story instantiates the developmental reading of the WTO treaty
 *   framework kernel: the treaty text is read as an equal-status commitment
 *   among unequal parties, where Special and Differential Treatment (S&D)
 *   provisions are permanent structural accommodation for asymmetric starting
 *   conditions, not temporary transitional exceptions, and technology
 *   transfer obligations are read as core, binding commitments rather than
 *   aspirational language. Under this reading, tariff flexibility, subsidy
 *   space, and compulsory licensing authority for developing states are
 *   treaty-protected policy space, and multinational IP holders bear a
 *   genuine (if incompletely enforced) obligation to transfer technology.
 *   This is a distinct constraint from the sibling market_access_reading,
 *   which reads the same treaty text as establishing trade liberalization as
 *   a symmetric universal obligation with S&D as time-limited exceptions —
 *   that sibling has a substantially higher extractiveness profile from the
 *   vantage of Global South states and is authored as a separate story linked
 *   via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, 0.38).
domain_priors:suppression_score(wto_treaty_framework__developmental_reading, 0.42).
domain_priors:theater_ratio(wto_treaty_framework__developmental_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__developmental_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__developmental_reading, "WTO Treaty Framework — Developmental Reading (Policy Space, S&D, Technology Transfer)").
narrative_ontology:topic_domain(wto_treaty_framework__developmental_reading, "international_trade_law/development_economics").

domain_priors:requires_active_enforcement(wto_treaty_framework__developmental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__developmental_reading, '7e614610-dd58-4fce-a1d6-a31f910e2cb3').
narrative_ontology:cs_kernel_codification('7e614610-dd58-4fce-a1d6-a31f910e2cb3', fixed_text).
narrative_ontology:cs_authority_grounding('7e614610-dd58-4fce-a1d6-a31f910e2cb3', extraction).
narrative_ontology:cs_interpretation_layer_present('7e614610-dd58-4fce-a1d6-a31f910e2cb3').
narrative_ontology:cs_reading_relation('7e614610-dd58-4fce-a1d6-a31f910e2cb3', wto_treaty_framework__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('7e614610-dd58-4fce-a1d6-a31f910e2cb3', foundational, asymmetric_conditions_warrant_permanent_accommodation).
narrative_ontology:cs_axiom_status(asymmetric_conditions_warrant_permanent_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('7e614610-dd58-4fce-a1d6-a31f910e2cb3', asymmetric_conditions_warrant_permanent_accommodation, empirically_contingent).
narrative_ontology:cs_axiom('7e614610-dd58-4fce-a1d6-a31f910e2cb3', secondary, technology_transfer_is_binding_not_aspirational).
narrative_ontology:cs_axiom_status(technology_transfer_is_binding_not_aspirational, holdable).
narrative_ontology:cs_axiom_grounding('7e614610-dd58-4fce-a1d6-a31f910e2cb3', technology_transfer_is_binding_not_aspirational, conventional).
narrative_ontology:cs_reference_frame('7e614610-dd58-4fce-a1d6-a31f910e2cb3', asymmetric_starting_conditions_accommodation).
narrative_ontology:cs_drift_state('7e614610-dd58-4fce-a1d6-a31f910e2cb3', post_doha_round_stalemate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7e614610-dd58-4fce-a1d6-a31f910e2cb3', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__developmental_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, global_south_member_states).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, domestic_infant_industries).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, multinational_ip_holders).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, developed_country_exporters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke Special and Differential Treatment provisions to retain tariff flexibility, subsidy space, and compulsory licensing authority. Coalition-negotiate for longer transition periods and technology transfer commitments at ministerial rounds. Can shape agenda through coalition blocs (G77, LDC Group) but cannot unilaterally rewrite treaty text against developed-country resistance.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, global_south_member_states, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, global_south_member_states, agenda_setter).

% Receive the most generous S&D carve-outs (extended implementation periods, duty-free quota-free access commitments) but lack the negotiating capacity or market leverage to enforce technology transfer obligations against reluctant IP holders. Exiting the multilateral system entirely would mean losing preferential access altogether, so exit is not a real option.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, least_developed_countries, beneficiary,
    powerless, generational, trapped, global).

% Protected by tariff and subsidy space preserved under the developmental reading, giving nascent manufacturers and producers time to develop competitive capacity before facing full international competition. Their survival is directly tied to whether S&D flexibilities are honored in practice.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, domestic_infant_industries, beneficiary,
    powerless, biographical, trapped, national).

% Bear the cost of compulsory licensing authority and technology transfer obligations that compel disclosure or licensing of patented technology (particularly pharmaceuticals and green technology) on terms below market rate. Can partially arbitrage through jurisdictional structuring and lobbying for narrower interpretation of transfer obligations, but cannot fully exit a system that grants market access to the bulk of their revenue base.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, multinational_ip_holders, payer,
    institutional, biographical, arbitrage, global).

% Face asymmetric market access — their goods encounter tariff protections in Global South markets that reciprocal goods from those markets do not encounter in reverse under non-reciprocal S&D terms. Can redirect trade flows or lobby domestic trade representatives to press for reciprocity, and have meaningfully more exit optionality than IP holders because their exports are diversifiable across markets.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developed_country_exporters, payer,
    powerful, biographical, mobile, global).

% Administers dispute settlement and monitors S&D compliance, interpreting the scope of policy-space commitments and technology transfer language when disputes arise. Its interpretive choices determine whether S&D functions as binding structural accommodation or as aspirational, non-enforceable language.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, wto_secretariat_and_dispute_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Advocate for aggressive use of compulsory licensing (e.g., for essential medicines) and full technology transfer, but have no standing in the WTO dispute settlement process itself — their influence runs only through pressuring member state delegations from outside the formal treaty architecture.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, global_south_civil_society_and_public_health_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__developmental_reading, diffuse).
narrative_ontology:fixing_cost_class(wto_treaty_framework__developmental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a multilateral framework in which states at very different levels of industrial development can participate in the same trading system without the weaker parties being forced to accept identical obligations immediately — coordinating trade liberalization with a built-in recognition that starting conditions are not symmetric.
% TRANSFER_FUNCTION: Moves policy flexibility (tariff room, subsidy authority, licensing power) toward Global South and least-developed states, and moves technology and licensed knowledge, in principle, from multinational IP holders toward developing-country industries; developed-country exporters absorb the cost of non-reciprocal market access.
% ABSENT_VOICES: Civil society and public health advocacy groups pushing for aggressive compulsory licensing exercise (e.g., pandemic-response drug access) have no formal seat in WTO dispute settlement; their objections that S&D and technology transfer commitments are systematically under-enforced are heard only indirectly, through state delegations that may or may not carry the message forward.
% DISAPPEARANCE_RATIONALE: If the developmental reading's S&D architecture and technology transfer commitments disappeared, Global South states would lose codified policy space for tariffs, subsidies, and compulsory licensing; infant industries would face full-strength competition immediately; IP holders would face materially reduced pressure to license technology on preferential terms; the entire negotiating architecture of successive WTO rounds (which is organized substantially around defending or eroding S&D) would need to be reconstructed from scratch.
% FOUNDING_PROBLEM: Post-colonial and newly industrializing states entering GATT/WTO faced radically different starting conditions than incumbent industrial powers — asymmetric capital, technology, and institutional capacity — and a formally symmetric treaty obligation regime risked locking in that asymmetry permanently under the banner of neutral rules.
% FOUNDING_PROBLEM_CORROBORATION: Global South trade negotiators and UNCTAD economists attest the founding problem (structural asymmetry) remains live and that S&D has been progressively narrowed in scope across rounds (Uruguay, Doha) despite the underlying asymmetry persisting; independent development economists outside the negotiating blocs (World Bank research staff, academic trade economists) corroborate that S&D provisions are frequently 'best endeavor' language lacking binding enforcement teeth, while developed-country trade representatives dispute that the asymmetry still justifies permanent (rather than transitional) accommodation.
narrative_ontology:disappearance_verdict(wto_treaty_framework__developmental_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__developmental_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__developmental_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_treaty_framework__developmental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__developmental_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__developmental_reading_tests).
:- end_tests(wto_treaty_framework__developmental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) reflecting the developmental reading's genuine coordination function (permitting differentiated obligations across radically different capacity levels) combined with real but partial extraction from IP holders and exporters who bear non-reciprocal costs. Theater ratio rises across the interval (0.20 to 0.40) because S&D and technology-transfer commitments increasingly function as declaratory 'best-endeavor' language honored more in text than in dispute-settlement enforcement — this is the Goodhart-style drift the temporal series is meant to surface. Suppression is moderate (0.42): developing states are not coerced into the arrangement so much as structurally locked into a multilateral system whose exit costs (loss of preferential market access) are high. Accessibility collapse is moderate-low (0.35) because bilateral and regional alternatives to the multilateral system do exist and have grown over the interval, giving some Global South states partial alternative paths. Resistance is moderate-high (0.55), reflecting active, sustained developing-country coalition pressure (G77, LDC Group, India-Brazil coordination) to preserve and expand S&D against developed-country efforts to narrow it.
 *
 * PERSPECTIVAL GAP:
 *   From the Global South agenda-setting seat, this reading is read as a rope-like coordination achievement: differentiated obligations matching differentiated capacity, protecting the weak from premature exposure. From the multinational IP holder and developed-exporter payer seats, the same structure reads as extraction — non-reciprocal terms enforced via treaty machinery they cannot exit without losing global market access. The engine's per-seat computation is expected to diverge along exactly this line; that divergence is the analytical payload of authoring both seats faithfully rather than collapsing to one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South states, LDCs, and domestic infant industries are declared beneficiaries: the developmental reading's entire structural logic exists to protect their policy space, which derives low directionality (near the beneficiary end) for them. Multinational IP holders and developed-country exporters are declared victims: they bear the transfer costs and non-reciprocal access terms, deriving high directionality (near the target end). LDCs sit closer to the trapped end of exit options than the broader Global South bloc because their negotiating leverage and alternative market access are thinner — this differentiates two same-side beneficiary groups by real capacity, not by label.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (structural asymmetry between industrializing and industrialized states) remains substantively live by most independent economic accounts, which weighs against declaring S&D mandatrophic. But the rising theater_ratio and the well-documented pattern of S&D provisions functioning as unenforceable 'best-endeavor' text rather than binding commitments suggests partial mandatrophy at the level of enforcement mechanism, even where the underlying justification remains sound — the accommodation is real in text, increasingly theatrical in operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sd_binding_vs_aspirational_status,
    'Are S&D provisions and technology transfer obligations genuinely binding treaty commitments enforceable through dispute settlement, or best-endeavor language that developed states can decline to operationalize without consequence?',
    'Review of WTO dispute settlement body rulings invoking S&D or technology transfer articles: count of successful enforcement actions versus dismissed or unenforceable claims over the treaty''s lifetime.',
    'If predominantly unenforceable, the developmental reading''s protective function is substantially theatrical and the constraint drifts toward a scaffold-with-failed-sunset or piton pattern; if meaningfully enforceable, the tangled_rope classification with genuine coordination function is well supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sd_binding_vs_aspirational_status, empirical, 'Whether S&D and technology transfer commitments carry real enforcement teeth.').

omega_variable(
    kernel_reading_contest_location,
    'Where precisely does the WTO treaty framework kernel leave the developmental_reading and market_access_reading readings genuinely underdetermined by the text itself, versus determined by which negotiating history and preparatory materials are treated as authoritative?',
    'Comparative textual and travaux préparatoires analysis identifying specific treaty articles (e.g., GATT Part IV, Enabling Clause, TRIPS Article 66.2) where the ''permanent structural accommodation'' versus ''temporary transitional exception'' readings diverge in what the text alone can settle.',
    'If the text substantially underdetermines the reading, both readings remain live and coexist as competing interpretive traditions held by different negotiating blocs, consistent with the coexists_with relation authored in cs_structure; if the text more clearly supports one reading, the sibling reading''s persistence becomes better explained as power politics than genuine interpretive ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Where the developmental and market-access readings genuinely diverge on textual grounds versus negotiating power.').

omega_variable(
    infant_industry_protection_natural_vs_constructed,
    'Is the case for permanent (not merely transitional) policy space for infant industries an empirically grounded development-economics finding, or a negotiating position whose economic justification is contested by comparable-development-stage counterexamples (e.g., states that liberalized early and industrialized successfully)?',
    'Comparative development economics literature review: outcomes of states that retained versus surrendered tariff/subsidy flexibility at comparable development stages, controlling for other growth determinants.',
    'If infant-industry protection is robustly supported, the developmental reading''s core normative claim is well-grounded; if contested, the reading''s claimed coordination function is weaker than authored and the extraction borne by exporters and IP holders is harder to justify as coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(infant_industry_protection_natural_vs_constructed, empirical, 'Empirical robustness of the infant-industry justification underlying the developmental reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__developmental_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_treaty_framework__developmental_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(wto__tr_t2001, wto_treaty_framework__developmental_reading, theater_ratio, 2001, 0.26).
narrative_ontology:measurement(wto__tr_t2007, wto_treaty_framework__developmental_reading, theater_ratio, 2007, 0.31).
narrative_ontology:measurement(wto__tr_t2013, wto_treaty_framework__developmental_reading, theater_ratio, 2013, 0.35).
narrative_ontology:measurement(wto__tr_t2019, wto_treaty_framework__developmental_reading, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(wto__tr_t2025, wto_treaty_framework__developmental_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_treaty_framework__developmental_reading, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement(wto__be_t2001, wto_treaty_framework__developmental_reading, base_extractiveness, 2001, 0.26).
narrative_ontology:measurement(wto__be_t2007, wto_treaty_framework__developmental_reading, base_extractiveness, 2007, 0.3).
narrative_ontology:measurement(wto__be_t2013, wto_treaty_framework__developmental_reading, base_extractiveness, 2013, 0.33).
narrative_ontology:measurement(wto__be_t2019, wto_treaty_framework__developmental_reading, base_extractiveness, 2019, 0.36).
narrative_ontology:measurement(wto__be_t2025, wto_treaty_framework__developmental_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_treaty_framework__developmental_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(wto__su_t2001, wto_treaty_framework__developmental_reading, suppression_requirement, 2001, 0.34).
narrative_ontology:measurement(wto__su_t2007, wto_treaty_framework__developmental_reading, suppression_requirement, 2007, 0.37).
narrative_ontology:measurement(wto__su_t2013, wto_treaty_framework__developmental_reading, suppression_requirement, 2013, 0.39).
narrative_ontology:measurement(wto__su_t2019, wto_treaty_framework__developmental_reading, suppression_requirement, 2019, 0.41).
narrative_ontology:measurement(wto__su_t2025, wto_treaty_framework__developmental_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__developmental_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_treaty_framework__developmental_reading, 0.12).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, wto_treaty_framework__market_access_reading).

% DUAL FORMULATION NOTE:
% This story and wto_treaty_framework__market_access_reading are sibling readings of a single contested kernel (wto_treaty_framework). They share treaty text but diverge on whether S&D is permanent structural accommodation (this story) or a temporary transitional exception (the sibling), producing different ε, different beneficiary/victim structures, and different claimed types. Both are authored as ε-invariant, independently classified constraints per the ε-invariance principle; neither supersedes the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__developmental_reading, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
