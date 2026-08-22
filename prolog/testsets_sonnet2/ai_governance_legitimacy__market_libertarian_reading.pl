% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: AI Governance Legitimacy via Voluntary Exchange and Property Rights (Market-Libertarian Reading)
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This story instantiates the market-libertarian reading of the AI
 *   governance legitimacy kernel: the claim that legitimacy derives from
 *   voluntary exchange, property rights, and exit-mediated dignity, and that
 *   the encyclical's solidarity demands are illegitimate coercion even while
 *   its subsidiarity principle (decentralization) is selectively endorsed.
 *   Structurally, this reading claims mountain status for property rights and
 *   voluntary exchange — treating them as pre-political facts rather than
 *   contested political constructs. Metrics are authored low (ε 0.20-0.30 per
 *   the expected structural delta) because, by this reading's own lights, the
 *   standing arrangement (contract- and market-mediated AI governance)
 *   genuinely coordinates innovation with minimal coercion. The declared
 *   beneficiaries (entrepreneurs, investors, mobile technical labor) and
 *   victims (workers without market power, coordination-failure communities,
 *   algorithmically excluded consumers) are authored because this reading's
 *   own premises — dignity through exit and competitive markets — do not in
 *   fact hold uniformly: exit is real for mobile capital and scarce labor and
 *   illusory for gig workers and monopsony-market workers, which is precisely
 *   the FSM signature this story is built to expose.
 *
 * KEY AGENTS:
 *   - ai_entrepreneurs: primary beneficiary (powerful/arbitrage) — captures upside of deregulated deployment
 *   - venture_investors: primary beneficiary (institutional/arbitrage) — capital allocation protected from mandate
 *   - gig_platform_workers: primary target (powerless/constrained) — nominal exit, no real alternative
 *   - monopsony_labor_market_workers: primary target (powerless/trapped) — competitive-market premise does not describe their actual labor market
 *   - magisterial_authorities: excluded institutional voice — solidarity claims denied binding force within this reading
 *   - political_theology_scholars: analytical observer — notes the selective adoption of subsidiarity without solidarity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.27).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.38).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.27).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "AI Governance Legitimacy via Voluntary Exchange and Property Rights (Market-Libertarian Reading)").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '7d22e31a-8936-407b-a766-13f69a5b1f65').
narrative_ontology:cs_kernel_codification('7d22e31a-8936-407b-a766-13f69a5b1f65', distributed).
narrative_ontology:cs_authority_grounding('7d22e31a-8936-407b-a766-13f69a5b1f65', distributed).
narrative_ontology:cs_reading_relation('7d22e31a-8936-407b-a766-13f69a5b1f65', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('7d22e31a-8936-407b-a766-13f69a5b1f65', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d22e31a-8936-407b-a766-13f69a5b1f65', ai_governance_legitimacy__democratic_pluralist_reading, influences).
narrative_ontology:cs_axiom('7d22e31a-8936-407b-a766-13f69a5b1f65', foundational, property_rights_prepolitical).
narrative_ontology:cs_axiom_status(property_rights_prepolitical, holdable).
narrative_ontology:cs_axiom_grounding('7d22e31a-8936-407b-a766-13f69a5b1f65', property_rights_prepolitical, deontological).
narrative_ontology:cs_axiom('7d22e31a-8936-407b-a766-13f69a5b1f65', foundational, solidarity_mandates_constitute_coercion).
narrative_ontology:cs_axiom_status(solidarity_mandates_constitute_coercion, holdable).
narrative_ontology:cs_axiom_grounding('7d22e31a-8936-407b-a766-13f69a5b1f65', solidarity_mandates_constitute_coercion, conventional).
narrative_ontology:cs_reference_frame('7d22e31a-8936-407b-a766-13f69a5b1f65', classical_liberal_natural_rights_order).
narrative_ontology:cs_drift_state('7d22e31a-8936-407b-a766-13f69a5b1f65', contemporary_ai_regulatory_contest, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7d22e31a-8936-407b-a766-13f69a5b1f65', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, ai_entrepreneurs).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, venture_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_technical_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, gig_platform_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, monopsony_labor_market_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, coordination_failure_communities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, algorithmically_excluded_consumers).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, property_rights_as_prepolitical).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, exit_based_dignity_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, spontaneous_order_of_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and deploy AI systems under a legitimacy framework that treats contract, property, and voluntary exchange as the sole legitimate basis for governance. Free from mandated audits, participatory oversight, or solidarity-based redistribution obligations, they capture the upside of rapid deployment and can relocate operations to friendlier jurisdictions if regulation tightens.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, ai_entrepreneurs, beneficiary,
    powerful, biographical, arbitrage, global).

% Allocate capital on the premise that innovation flourishes precisely when unencumbered by collective mandates. Portfolio diversification and jurisdictional mobility give them near-total insulation from any single governance regime; the market-libertarian reading directly protects the return profile they underwrite.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, venture_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Skilled engineers and researchers with scarce, portable expertise. They can exit any single employer or jurisdiction for another, so a governance regime built on exit options and competitive markets genuinely protects their interests rather than merely rhetoricizing them.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_technical_workers, beneficiary,
    organized, biographical, mobile, global).

% Subject to algorithmic management and dispatch systems governed only by the platform's terms of service and private arbitration clauses. Their nominal 'exit option' — quitting the app — does not function as real exit because comparable platforms impose the same terms; dignity-through-exit is theoretical, not operative, at their power level.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, gig_platform_workers, payer,
    powerless, immediate, constrained, national).

% Work in labor markets where a small number of AI-adopting employers set wages and conditions with little competitive check. The reading's assumption of competitive markets as the dignity-protecting mechanism does not describe their actual market structure; they bear the costs of AI-driven labor reorganization with no meaningful alternative employer to exit to.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, monopsony_labor_market_workers, payer,
    powerless, biographical, trapped, regional).

% Face harms from AI deployment (algorithmic redlining, automated displacement, environmental costs of compute infrastructure) that require collective response but which the reading treats as illegitimate coercion if addressed through mandate rather than voluntary contract. No individual firm has incentive to internalize these diffuse costs, and the community has no contractual party to negotiate exit or compensation from.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, coordination_failure_communities, payer,
    powerless, generational, trapped, regional).

% Denied credit, insurance, or services by opaque AI scoring systems, with contract law and private arbitration as their only recourse. Reputational mechanisms the reading relies on for accountability require transparency the firms are not compelled to provide, so the accountability loop this reading depends on is structurally incomplete for this group.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, algorithmically_excluded_consumers, payer,
    powerless, immediate, constrained, national).

% Advocate that solidarity obligations toward the vulnerable are a legitimate and binding component of governance legitimacy, not mere aspiration. This reading rejects their authority to bind economic actors to solidarity demands, characterizing such demands as coercion beyond the state's or church's legitimate remit — their voice is present in public discourse but has no binding purchase inside this reading's framework.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, magisterial_authorities, excluded,
    institutional, civilizational, analytical, global).

% Would impose collective mandates (safety audits, algorithmic transparency requirements, labor protections) that this reading treats as illegitimate interference with voluntary exchange. Their statutory authority is not disputed in law, but this reading denies it moral legitimacy beyond enforcing contracts and property rights.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, regulatory_bodies, excluded,
    institutional, generational, analytical, national).

% Analyze how this reading selectively adopts the encyclical's subsidiarity principle (decentralization) while rejecting its solidarity principle (obligations to the vulnerable), noting the two are presented in Catholic Social Doctrine as mutually constraining, not severable.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, political_theology_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__market_libertarian_reading, ai_entrepreneurs).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__market_libertarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates economic actors through price signals, contract enforcement, and reputational mechanisms rather than centralized mandate — allowing rapid, decentralized experimentation in AI development and deployment without requiring prior collective agreement on rules.
% TRANSFER_FUNCTION: Moves the costs of governance failure (labor displacement, algorithmic harm, diffuse externalities) from firms and investors who could internalize them contractually onto workers, consumers, and communities who lack the market power or exit options the framework assumes universal.
% ABSENT_VOICES: Magisterial authorities and regulatory bodies are structurally present in public discourse but excluded from binding force within this reading's own legitimacy framework — their solidarity and mandate claims are characterized as coercion rather than engaged as competing legitimacy claims. Powerless payers (gig workers, monopsony workers, excluded consumers) are procedurally present as contracting parties but have no practical voice in setting the terms they nominally consent to.
% DISAPPEARANCE_RATIONALE: From the beneficiary seats, if this legitimacy framework disappeared, innovation would be materially slowed by newly legitimate collective mandates — the world clearly rearranges for them. From the payer seats, the framework's disappearance in favor of enforceable solidarity obligations would rearrange their world for the better (real recourse, real exit); from their view the current arrangement is what needs to change, not what protects them. The parties dispute which direction 'rearrangement' runs, which is itself the kernel contest.
% FOUNDING_PROBLEM: The problem this reading answers is the historical failure of centrally planned and heavily mandated economic systems to generate innovation and prosperity, and the perceived risk that any political authority — religious or secular — claiming to define the 'common good' will use that authority to entrench incumbents and suppress competitive entry.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the libertarian tradition corroborate that centrally mandated systems have in fact produced innovation failures in some historical cases, supporting part of the founding problem's premise. However, the same outside sources (labor economists studying platform and monopsony markets, and the encyclical's own drafters) dispute that the solution required is the elimination of solidarity-based mandates specifically — they attest the founding problem of innovation-stifling central planning is real but contest that it generalizes to justify rejecting all collective governance of AI, which is a further claim asserted primarily by the reading's own beneficiaries.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, contested).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.27, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.27 at interval end) because, from within this reading, the coordination function (decentralized experimentation, price-mediated resource allocation) is genuinely operative for the mobile, capital-backed, and skill-scarce agents. Suppression is moderate (0.38) rather than low, because the reading's dependence on private arbitration and contract law to resolve disputes is itself a coercive apparatus for parties without bargaining power — arbitration clauses are typically non-negotiable for gig workers and consumers, which is suppression exercised through nominally voluntary contract rather than through visible state mandate. Accessibility collapse is moderate-high (0.58): once inside a platform ecosystem or a monopsony labor market, alternatives collapse substantially despite the framework's formal claim that exit is always available. Resistance is moderate (0.55): labor organizing, platform-worker litigation, and civil-society critique of algorithmic accountability represent real, ongoing resistance to this arrangement — a genuine mountain would not meet resistance at this level.
 *
 * PERSPECTIVAL GAP:
 *   The claimed type is mountain (property rights and voluntary exchange as pre-political, natural facts about legitimate order) but the declared beneficiary structure is exactly the FSM signature: a claim of naturalness paired with identifiable, concentrated beneficiaries (entrepreneurs, investors, mobile technical labor) and identifiable victims (workers and communities without market power). The engine's false-summit detection is expected to fire on this structure — the reading asserts a natural mountain, but the metrics and beneficiary/victim declarations describe a constructed arrangement that reliably transfers costs from mobile capital to immobile labor.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries hold arbitrage or mobile exit options and short-to-generational time horizons matched to capital cycles; they experience the framework as genuine coordination with negligible personal cost. Victims hold trapped or constrained exit and immediate-to-generational horizons mismatched to their power level — a gig worker's immediate need for income and a coordination-failure community's generational exposure to environmental or economic harm both sit at powerless/trapped-or-constrained, which the derivation chain correctly reads toward the target end of directionality despite the reading's formal claim that everyone has meaningful exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does not present a mandatrophy claim in its own terms — it denies that any collective mandate was ever legitimately founded, so there is no mandate to have outlived its function. But the six-questions genealogy interview surfaces a different problem: the founding problem (innovation-stifling central planning) is real and corroborated by outside economic history, yet the reading's own beneficiaries extend that corroborated founding problem into a much broader claim (that all solidarity-based governance of AI is illegitimate) that is not independently corroborated. This is the reading's own internal mandatrophy candidate: a narrow, defensible founding claim stretched into a much larger legitimacy doctrine that primarily serves those already holding market power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_rights_natural_or_constructed,
    'Are property rights and voluntary exchange genuinely pre-political natural facts (as this reading''s mountain claim asserts), or are they themselves the product of specific legal and political construction that happens to benefit those with existing capital and mobility?',
    'Comparative legal history of property rights regimes across jurisdictions and eras: if property-rights content varies substantially with political power configurations rather than converging on a stable natural baseline, the constructed reading is favored.',
    'If constructed rather than natural, the mountain claim fails and the constraint reclassifies toward tangled_rope or snare, since the same structure that coordinates capital-holder investment also extracts from those without bargaining power through the same legal apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_rights_natural_or_constructed, conceptual, 'Whether property-rights-as-legitimacy-basis is natural law or constructed doctrine benefiting specific agents (FSM candidate ambiguity).').

omega_variable(
    exit_option_reality_gap,
    'Does the framework''s dignity-through-exit claim describe a real mechanism for powerless agents (gig workers, monopsony-market workers), or only for agents who already possess mobile capital or scarce skills?',
    'Empirical labor-market studies measuring actual switching rates, wage effects of switching, and availability of comparable alternative employers/platforms for low-power workers versus high-skill workers.',
    'If exit is empirically unavailable for powerless agents, the claimed universal dignity-protection mechanism is shown to function only for the beneficiary class, sharpening the tangled-rope/FSM reading and reducing confidence in the mountain claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_reality_gap, empirical, 'Whether exit options function as claimed across all agent classes or only for mobile/scarce-skill agents.').

omega_variable(
    subsidiarity_solidarity_severability,
    'Can subsidiarity (decentralization) be coherently endorsed while solidarity (obligations to the vulnerable) is rejected, given that Catholic Social Doctrine presents them as a single mutually constraining principle?',
    'Textual and doctrinal analysis of the encyclical and its tradition: determine whether subsidiarity is ever presented, within that tradition, as operative independent of solidarity constraints.',
    'If not severable within the source tradition, this reading''s selective endorsement is a doctrinal misreading rather than a coherent alternative framework, which would not change the engine''s structural classification but would affect the reading''s claim to fidelity with the source text it partially invokes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_solidarity_severability, conceptual, 'Whether this reading''s selective adoption of subsidiarity-without-solidarity is internally coherent relative to the source doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.27).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_governance_legitimacy__market_libertarian_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__market_libertarian_reading, 0.12).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the ai_governance_legitimacy kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. This reading claims the lowest ε (0.20-0.30) of the family, reflecting its own-lights assessment that voluntary exchange minimizes coercion; the magisterial_subsidiarity_reading is expected to author substantially higher ε for the same standing arrangement, since from that reading's lights the absence of solidarity mandates constitutes a moral failure with real victims. The technocratic_optimization_reading and democratic_pluralist_reading occupy intermediate positions with different beneficiary/victim structures. All four share the same underlying contested kernel (what makes AI governance legitimate) but instantiate structurally distinct constraints with distinct ε, distinct stakeholders, and distinct claimed types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_legitimacy__market_libertarian_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
