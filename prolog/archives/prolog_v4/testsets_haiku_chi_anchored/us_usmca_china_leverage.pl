% ============================================================================
% CONSTRAINT STORY: us_usmca_china_leverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_usmca_china_leverage, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_usmca_china_leverage
 *   human_readable: US leveraging of USMCA ratification to constrain Canadian foreign policy on China
 *   domain: geopolitical/trade/foreign_policy
 *
 * SUMMARY:
 *   During the Trump administration (2017-2021) and continuing into the Biden
 *   era, the United States exerted significant pressure on Canada to adopt a
 *   more confrontational stance toward China, explicitly linking this policy
 *   alignment to USMCA ratification and favorable trade terms. This
 *   constraint exemplifies asymmetric leverage within a bilateral
 *   relationship framed as reciprocal coordination. The US possesses
 *   structural advantages (size, market access, alternative trade partners)
 *   that give it coercive capacity; Canada's dependence on US trade (35% of
 *   exports) creates extraction risk. However, the constraint also contains
 *   genuine elements of security coordination — US and Canadian interests in
 *   managing China's rise are not purely fictional. The constraint's
 *   structure is therefore Tangled Rope: it performs a real coordination
 *   function (alliance alignment, burden-sharing) while simultaneously
 *   extracting policy autonomy from the less powerful actor. The theater
 *   ratio has declined over the measurement interval, indicating that the US
 *   has moved from diplomatic rhetoric (early 2017) toward direct
 *   conditionality (mid-2020), reducing performative framing in favor of
 *   explicit coercion.
 *
 * KEY AGENTS:
 *   - US Administration: Primary beneficiary (institutional/arbitrage) — achieves China containment strategy, exports security costs, maintains hegemonic leverage
 *   - US National Security Establishment (Pentagon, State Dept, CSIS): Beneficiary (institutional/arbitrage) — gains allied commitment to strategic alignment without proportional domestic political cost
 *   - Canadian Government: Primary victim (powerful/constrained) — forced to adopt policies that diverge from preferred bilateral relationship with China, constrained by trade dependence
 *   - Canadian Business Sector: Secondary victim (organized/constrained) — benefits from USMCA market access but faces extraction through geopolitical constraints; exposed to Chinese retaliation
 *   - Chinese Government: Indirect actor (powerful/mobile) — not formally constrained but threat perception drives constraint mechanism; may impose countervailing pressure on Canada
 *   - Analytical Observer: Civilizational/systems perspective — observes that constraint has both coordination and extraction elements; legitimacy depends on whether benefits flow both directions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_usmca_china_leverage, 0.58).
domain_priors:suppression_score(us_usmca_china_leverage, 0.68).
domain_priors:theater_ratio(us_usmca_china_leverage, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_usmca_china_leverage, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_usmca_china_leverage, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_usmca_china_leverage, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_usmca_china_leverage, tangled_rope).
narrative_ontology:human_readable(us_usmca_china_leverage, "US leveraging of USMCA ratification to constrain Canadian foreign policy on China").
narrative_ontology:topic_domain(us_usmca_china_leverage, "geopolitical/trade/foreign_policy").

domain_priors:requires_active_enforcement(us_usmca_china_leverage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_usmca_china_leverage, us_administration).
narrative_ontology:constraint_beneficiary(us_usmca_china_leverage, us_national_security_establishment).
narrative_ontology:constraint_victim(us_usmca_china_leverage, canadian_policy_autonomy).
narrative_ontology:constraint_victim(us_usmca_china_leverage, canada_us_bilateral_relationship).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CANADIAN GOVERNMENT (SNARE) — Canada faces a genuine dilemma: ratifying USMCA is essential for trade stability and economic growth, but ratification is made conditional on adopting hostile China policies that conflict with Canadian strategic interests (maintaining trade relationships, avoiding economic retaliation). Exit options are constrained by the asymmetry of trade dependence (35% of Canadian exports go to US; 18% of US exports go to Canada). d≈0.78, f(d)≈1.20, σ=1.1 → χ≈0.77.
constraint_indexing:constraint_classification(us_usmca_china_leverage, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 2: CANADIAN BUSINESS & LABOR COALITION (TANGLED ROPE) — Organized interests benefit from USMCA access to US market and employment stability, but also bear costs of constrained foreign policy autonomy and potential Chinese retaliation against Canadian exports. Coalition has some agency (can organize pressure on government) but cannot exit USMCA framework without severe costs. d≈0.62, f(d)≈0.85, σ=1.1 → χ≈0.56.
constraint_indexing:constraint_classification(us_usmca_china_leverage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US ADMINISTRATION & FOREIGN POLICY ESTABLISHMENT (ROPE) — The US framework presents the constraint as coordination: aligning North American security strategy against China. From the US perspective, the mechanism is cooperative (uniting allies, creating common front), not extraction. The US experiences low effective extraction because it has multiple alternatives (trade with allies, security partnerships) and can walk away from USMCA without critical costs. d≈0.08, f(d)≈-0.11, σ=1.1 → χ≈-0.06.
constraint_indexing:constraint_classification(us_usmca_china_leverage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: INTERNATIONAL RULES-BASED ORDER ADVOCATES (SCAFFOLD) — From a global institutional perspective, the constraint is temporary: as multilateral institutions strengthen and alliance cohesion increases, individual nations will have more exit options and leverage will decline. The framework itself (conditional trade access for foreign policy alignment) is seen as transient — eventually superseded by rules-based mechanisms that decouple trade from geopolitical coercion. d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.30. Sunset mechanism: maturation of WTO dispute resolution, strengthening of UNCTAD norms, and plurilateral trade agreements that reduce dependence on US-led frameworks.
constraint_indexing:constraint_classification(us_usmca_china_leverage, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL NAFTA/USMCA INSTITUTIONS (PITON) — The trade agreement apparatus itself has become partially degraded: the formal dispute resolution mechanism (Chapter 20 under NAFTA, modernized in USMCA) is undercut by informal political pressure and conditionality. The institutions persist through treaty obligation and path dependence, but their primary function (neutral arbitration) has atrophied, replaced by political leverage. theater_ratio≈0.45 is borderline for piton gate; the institutional degradation is partial, not total.
constraint_indexing:constraint_classification(us_usmca_china_leverage, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE / SYSTEMIC VIEW) — From a systems perspective, the constraint is a hybrid: genuine security coordination (common interest in managing China's rise) combined with asymmetric extraction (US using trade leverage to externalize security costs onto Canada). The constraint is not a false mountain — it genuinely reflects US security interests — but it also genuinely extracts from Canada. Both elements are real. d≈0.64, f(d)≈0.92, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(us_usmca_china_leverage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_usmca_china_leverage_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_usmca_china_leverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_usmca_china_leverage, TR),
    TR >= 0.70.

:- end_tests(us_usmca_china_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The US uses trade agreement ratification — a genuine mutual benefit — as leverage to force policy compliance on an orthogonal issue (China stance). This is extraction: Canada obtains market access but at the cost of policy autonomy. The extractiveness is not maximal because legitimate security coordination elements exist and USMCA provides real benefits to Canada. Suppression (0.68): High. Canada's exit options are severely limited by trade dependence, structural power asymmetry, and institutional path dependence. Exit mechanisms (rejecting USMCA, forming alternative trade blocs, seeking Chinese compensation) all carry severe economic costs that are credible and well-understood. Theater ratio (0.45): Moderate. The US has progressively reduced diplomatic framing (theater) in favor of explicit conditionality. Early Trump-era messaging emphasized 'alliance burden-sharing' and 'shared security interests' — high theater. By 2020, US statements became more directly coercive ('ratify USMCA or face consequences') — lower theater. The trend toward explicit coercion indicates that the constraint's real mechanism has become visible.
 *
 * PERSPECTIVAL GAP:
 *   The Canadian government sees Snare (forced compliance, no exit), while the US administration sees Rope (coordination of aligned interests). The analytical observer sees both: the constraint is genuinely a Tangled Rope because US security interests and Canadian security interests are not perfectly opposed, and because the coordination function (North American alignment on China) is real, even if the distribution of costs is asymmetric. The gap emerges from directionality: from Canada's position, the constraint extracts policy autonomy (d=0.78); from the US position, the constraint delivers security benefits at acceptable cost (d=0.08). The two positions are structurally incompatible — they describe the same mechanism from opposite ends. The Australian case (China trade coercion 2020-2021) provides a comparable example where the Australian government eventually classified the constraint as Snare (pure extraction without reciprocal security benefit), while the US characterized it as Rope (alliance burden-sharing). The USMCA case differs: Canada retains more exit capacity than Australia did (USMCA is genuinely valuable), making the classification Tangled Rope rather than pure Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Canadian Government: Victim (non-voluntary compliance) + constrained (limited exit options) → d≈0.78, f(d)≈1.20. High directionality toward target. US Administration: Beneficiary (achieves strategic objective) + arbitrage (multiple alternative strategies) → d≈0.08, f(d)≈-0.11. Low directionality, net beneficiary. The asymmetry is the key structural feature: Canada cannot easily exit, while US can. This asymmetry is intrinsic to the constraint, not perspective-dependent. However, both parties derive coordination benefits (alliance alignment reduces bilateral security costs), which prevents the constraint from being pure Snare. The Tangled Rope classification captures this hybrid: real coordination function + asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids mandatrophy (false coordination vs false extraction mislabeling) by acknowledging both elements. The US can truthfully claim security coordination benefits — aligning Canada against Chinese economic/military expansion serves legitimate North American interests. Canada can truthfully claim extraction — it bears disproportionate costs (policy inflexibility, Chinese retaliation risk, diplomatic friction) for benefits that accrue more to the US. Neither claim is false; they describe different parts of the same constraint. The Tangled Rope classification accommodates both truths: it requires active enforcement (true: US makes access conditional), possesses beneficiaries (true: US gains strategic alignment), and possesses victims (true: Canada's autonomy is constrained). The distinguishing factor between 'legitimate alliance coordination that happens to be asymmetric' and 'pure extraction dressed as coordination' is whether the subordinate actor retains meaningful choice and benefits. Canada retains choice (it could reject USMCA; it could challenge US publicly; it could seek Chinese economic compensation) and does benefit (market access, security alliance, integration with US innovation). The constraint is therefore coordinative, not purely coercive. Mandatrophy is resolved by this dual-benefit analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_intent_coordination_vs_coercion,
    'Does the US administration genuinely believe USMCA-conditional China policy alignment serves North American security, or is it primarily extractive coercion?',
    'Declassified policy memoranda, interviews with national security officials, comparison of stated security justifications with threat assessments from CSIS/RAND/Pentagon think tanks',
    'If genuine coordination intent: constraint is primarily Rope/Tangled Rope with legitimate security benefits to Canada. If primarily coercive: constraint is primarily Snare with false coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_intent_coordination_vs_coercion, empirical, 'Whether US intent is genuine security coordination or coercive extraction').

omega_variable(
    canadian_exit_capacity_counterfactual,
    'Could Canada have rejected US pressure and ratified USMCA without severe economic consequences?',
    'Counterfactual analysis using trade models; empirical examination of other countries'' ability to maintain independence from US within trade frameworks; analysis of Chinese retaliatory capacity and likelihood',
    'If exit was genuinely available: Canadian constraint is more Rope than Snare (constraint was chosen). If exit was illusory: Canadian constraint is more Snare than Rope (constraint was forced).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(canadian_exit_capacity_counterfactual, empirical, 'Whether Canada genuinely had exit options from US pressure').

omega_variable(
    china_retaliation_credibility,
    'Would China actually retaliate economically against Canadian firms that adopt US-aligned China policies, and at what scale?',
    'Historical analysis of Chinese retaliation against other countries for US alignment (Australia, Japan, Korea); empirical examination of Chinese bargaining leverage over Canadian sectors; monitoring of actual retaliatory actions post-2020',
    'If credible: Canada''s constraint is tightened (both US and China exert pressure). If not credible: US leverage is stronger (Canada fears phantom retaliation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_retaliation_credibility, empirical, 'Credibility and scale of Chinese economic retaliation threat').

omega_variable(
    multilateral_coalition_formation,
    'Can Canada and other US-aligned countries form a coalition to dilute individual US leverage, or does US capacity for bilateral coercion prevent coalition formation?',
    'Empirical analysis of coalition dynamics in trade negotiations; examination of Australian, Japanese, Korean, and European responses to US pressure; tracking of plurilateral agreement formation (CPTPP, RCEP alternatives)',
    'If coalition feasible: constraint becomes Scaffold (organized agents building exit paths). If US bilateral leverage prevents coalition: constraint remains Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilateral_coalition_formation, empirical, 'Whether coalition formation can reduce bilateral US leverage').

omega_variable(
    china_policy_internalization,
    'Over time, has Canada internalized US-aligned China policy as its own strategic preference, or does it remain extracted compliance?',
    'Discourse analysis of Canadian policy statements; interviews with Canadian foreign service and defense officials; tracking of institutional adoption of China containment frameworks in CSEC and DND; comparison with Australian case (where policy internalization occurred rapidly)',
    'If internalized: constraint has shifted from coercive (Snare) to coordinative (Rope). If extraction persists: constraint remains Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_policy_internalization, empirical, 'Whether Canada has internalized or merely complies with US China policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_usmca_china_leverage, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usmca_tr_t0, us_usmca_china_leverage, theater_ratio, 0, 0.55).
narrative_ontology:measurement(usmca_tr_t2, us_usmca_china_leverage, theater_ratio, 2, 0.48).
narrative_ontology:measurement(usmca_tr_t4, us_usmca_china_leverage, theater_ratio, 4, 0.45).

% Extraction over time
narrative_ontology:measurement(usmca_be_t0, us_usmca_china_leverage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usmca_be_t2, us_usmca_china_leverage, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(usmca_be_t4, us_usmca_china_leverage, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_usmca_china_leverage, enforcement_mechanism).
narrative_ontology:affects_constraint(us_usmca_china_leverage, canadian_china_export_dependency).
narrative_ontology:affects_constraint(us_usmca_china_leverage, us_hegemonic_alliance_management).
narrative_ontology:affects_constraint(us_usmca_china_leverage, trilateral_north_american_security_integration).

% DUAL FORMULATION NOTE:
% This constraint is part of a family: the US-China strategic competition creates downstream constraints on all US-aligned trade partners. Upstream constraints include US strategic competition with China (ε≈0.50, Tangled Rope at global scale); downstream constraints include specific sectoral coercions (Canadian rare earth dependency on China, Canadian semiconductor exposure). This story focuses on the bilateral leverage mechanism; sibling stories address sectoral and global-scale dimensions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_usmca_china_leverage, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
