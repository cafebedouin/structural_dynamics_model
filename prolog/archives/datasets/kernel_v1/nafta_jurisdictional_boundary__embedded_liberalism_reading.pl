% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA Jurisdictional Boundary (Embedded Liberalism Reading)
 *   domain: international_trade_law/regulatory_federalism
 *
 * SUMMARY:
 *   The embedded liberalism reading of NAFTA's jurisdictional boundary treats
 *   the trade agreement as a framework that deliberately balances market
 *   access with preserved domestic policy space. This reading is ONE READING
 *   of a contested kernel (the NAFTA text itself). The kernel codifies
 *   obligations (market access, non-discrimination) but leaves 'legitimate
 *   objectives' undefined, creating space for competing interpretations. The
 *   embedded liberalism reading holds that environmental and labor standards,
 *   when applied non-discriminatorially, fall within legitimate policy space
 *   and are defensible against trade challenge. This contrasts with the
 *   capital_supremacy_reading (which interprets 'legitimate objectives'
 *   narrowly, prioritizing investor protection) and the
 *   sovereignty_primacy_reading (which rejects the binding authority of trade
 *   obligations entirely). The embedded liberalism reading instantiates a
 *   hybrid constraint: genuine coordination function (both markets and policy
 *   space are preserved) layered with asymmetric extraction (litigation
 *   costs, risk of regulatory narrowing, suppression through ISDS threat).
 *   Extractiveness has risen from 0.28 to 0.38 over the 12-year interval,
 *   driven by ISDS case proliferation and increasing litigation costs that
 *   suppress domestic regulatory experimentation even when governments
 *   ultimately prevail in disputes.
 *
 * KEY AGENTS:
 *   - Domestic Regulatory Authorities (Mexican environmental agencies, Canadian labor boards): trapped within NAFTA jurisdiction; powerless/trapped exit; primary victims of litigation costs and interpretive narrowing
 *   - Environmental/Labor Coalitions (NGOs, unions, advocacy networks): organized actors defending regulatory space; constrained exit (coalition-building is costly); secondary beneficiaries (the embedded liberalism framing legitimates their regulatory goals); constrained exit
 *   - Transnational Capital (multinational corporations, investor consortia): powerful actors with arbitrage capacity; benefit from market access and ISDS threat; constrained by non-discrimination requirements and legitimate-objectives boundary; institutional actors managing costs
 *   - NAFTA Dispute Settlement System (secretariat, panels, roster of adjudicators): institutional actor with arbitrage capacity; manages the boundary between market access and policy space; bears responsibility for consistent interpretation
 *   - NAFTA States (Canada, Mexico, United States): institutional actors with constrained exit; must negotiate between domestic constituencies (environmental/labor advocates) and capital interests; maintain facade of embedded liberalism while managing capital pressure
 *   - Post-War Bretton Woods Regime: civilizational-scale institutional form; degraded through changed context (financialization, ISDS mechanisms); maintained through institutional inertia despite eroded functional capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.38).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.48).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary (Embedded Liberalism Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, '76887130-3a7d-4263-ac9d-e24734da94b1').
narrative_ontology:cs_kernel_codification('76887130-3a7d-4263-ac9d-e24734da94b1', formalized).
narrative_ontology:cs_authority_grounding('76887130-3a7d-4263-ac9d-e24734da94b1', extraction).
narrative_ontology:cs_interpretation_layer_present('76887130-3a7d-4263-ac9d-e24734da94b1').
narrative_ontology:cs_reading_relation('76887130-3a7d-4263-ac9d-e24734da94b1', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('76887130-3a7d-4263-ac9d-e24734da94b1', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('76887130-3a7d-4263-ac9d-e24734da94b1', foundational, non_discriminatory_regulation_is_defensible).
narrative_ontology:cs_axiom_status(non_discriminatory_regulation_is_defensible, holdable).
narrative_ontology:cs_axiom_grounding('76887130-3a7d-4263-ac9d-e24734da94b1', non_discriminatory_regulation_is_defensible, empirically_contingent).
narrative_ontology:cs_axiom('76887130-3a7d-4263-ac9d-e24734da94b1', foundational, market_access_and_policy_autonomy_coexist).
narrative_ontology:cs_axiom_status(market_access_and_policy_autonomy_coexist, holdable).
narrative_ontology:cs_axiom_grounding('76887130-3a7d-4263-ac9d-e24734da94b1', market_access_and_policy_autonomy_coexist, instrumental).
narrative_ontology:cs_reference_frame('76887130-3a7d-4263-ac9d-e24734da94b1', post_war_embedded_liberalism_compromise).
narrative_ontology:cs_drift_state('76887130-3a7d-4263-ac9d-e24734da94b1', contemporary_financialized_trade_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('76887130-3a7d-4263-ac9d-e24734da94b1', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_policy_space_defenders).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_labor_advocates).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, transnational_capital_litigation_targets).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, regulatory_sovereignty_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC REGULATORY AUTHORITY (SNARE) — A Mexican environmental agency or Canadian labor board cannot exit NAFTA's jurisdiction without abandoning its own legal mandate. Trapped within the agreement's framework. High suppression: litigation threat, investor-state dispute settlement (ISDS) costs, and the need to defend 'legitimate objectives' internally drain regulatory capacity. No exit option; maximum experienced extraction.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__embedded_liberalism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ENVIRONMENTAL/LABOR COALITION (TANGLED ROPE) — Organized domestic constituencies (environmental NGOs, labor unions, public health advocates) benefit from the embedded liberalism framework's legitimation of domestic policy space ('consistent with legitimate objectives'). But they also bear extraction costs: proving non-discrimination, defending against dilution arguments, and absorbing litigation threats from capital. Constrained exit (organizing alternative trade regimes is costly and uncertain) but genuine agency through coalition pressure.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRADE DISPUTE SETTLEMENT SYSTEM (ROPE) — The embedded liberalism reading treats NAFTA's dispute mechanisms as coordination infrastructure: panels interpret 'legitimate objectives' defensively, enabling regulatory experimentation while maintaining market access commitments. Institutional actor with arbitrage (can defer to state sovereignty or capital interests as negotiation dynamics shift). Views the constraint as solving a genuine coordination problem: how to sustain open markets while preserving regulatory autonomy.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__embedded_liberalism_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: TRANSNATIONAL CAPITAL (TANGLED ROPE) — Large corporations benefit from market access and the threat of ISDS litigation (extraction mechanism). But the embedded liberalism reading constrains their extraction: environmental and labor standards are 'legitimate objectives' and non-discriminatory regulations are defensible. Capital is constrained (cannot easily relocate production across NAFTA zone to escape regulations), bears litigation costs, and experiences suppression through coalition countervailing power. The framework coordinates market access while limiting pure extraction.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: NAFTA SECRETARIAT / DISPUTE PANELS (TANGLED ROPE) — Institutional actors managing boundary enforcement. The embedded liberalism reading constrains their own authority: they cannot simply defer to capital interests (domestic constituencies resist); they cannot simply defer to sovereignty claims (capital litigation threatens state credibility). Requires active enforcement of the 'legitimate objectives' boundary. Theater moderately high (panels publish reasoned decisions, giving appearance of neutral arbitration) but functional legitimacy depends on consistently defending environmental/labor standards.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: POST-WAR BRETTON WOODS COMPROMISE (PITON) — At civilizational scale, embedded liberalism itself is a degraded institutional form. The original compromise (GATT/IMF) coordinated capital mobility with domestic policy space by keeping capital flows controlled. NAFTA inherits the rhetoric of embedded liberalism but within a world of financialized capital and ISDS mechanisms that the original compromise lacked. The framework performs the role of defending policy space (theater ≥ 0.61) but the underlying machinery for capital control has atrophied. Piton: inertially maintained through institutional familiarity and legacy legitimacy, functionally degraded by changed context.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__embedded_liberalism_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, all trade agreements must navigate an immutable tension: markets require non-discrimination rules, but states require policy autonomy. The 'legitimate objectives' boundary is presented as an inherent requirement of any trade system that respects sovereignty. The engine's false-summit detector will identify this as naturalization: the legitimacy of 'non-discrimination' and 'legitimate objectives' as compatible framings is not a law of nature but a specific institutional choice that benefits identifiable actors.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__embedded_liberalism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__embedded_liberalism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__embedded_liberalism_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, TR),
    TR >= 0.70.

:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The embedded liberalism reading identifies genuine coordination gains (both markets and regulatory space are preserved) alongside extraction through litigation costs and interpretive narrowing. The extractiveness is lower than capital_supremacy_reading would assign (which would emphasize ISDS as pure extraction mechanism) because legitimate-objectives framing provides real defensive capacity. But it is higher than pure-rope readings because litigation threat and ISDS costs are non-trivial, creating a suppression layer. Suppression (0.48): Moderate-high. Multiple suppression mechanisms: (1) litigation cost barriers deter regulatory experimentation even in ultimately defensible areas; (2) uncertainty about panel interpretation creates self-censorship incentive; (3) asymmetric information advantage for capital (litigation-funding capacity, legal expertise, repeat-player advantage in ISDS); (4) burden-of-proof requirements (states must affirmatively defend 'legitimacy' rather than capital affirmatively proving discrimination). Theater ratio (0.61): Moderate-high. The dispute settlement system publishes reasoned decisions, creating appearance of neutral adjudication, but the underlying process exhibits high performance dimensions: panels must publicly justify 'legitimate objectives' findings (performative legitimacy work), states must frame policy goals in NAFTA-compatible language (rhetorical translation), and the civil society hearings (absent in early NAFTA, now included) are partly theatrical. The theater_ratio has risen from 0.48 to 0.61, reflecting increased legitimacy work required to maintain embedded liberalism framing against capital pressure.
 *
 * PERSPECTIVAL GAP:
 *   This reading is distinguished from its siblings by how it resolves the market/regulation tension. The capital_supremacy_reading narrows legitimate objectives so far that most environmental/labor regulations classify as protectionist — the Snare and Piton perspectives intensify, and perspectives other than capital see high extraction. The sovereignty_primacy_reading rejects trade obligations entirely — the Rope perspective disappears, and all actors see the constraint as illegitimate. The embedded liberalism reading attempts to hold both poles: genuine market access AND genuine policy space. This attempt fails at the Piton and Mountain perspectives (civilizational scale reveals that the underlying coordination machinery — capital controls — has degraded, making embedded liberalism an inertial form). But at biographical and generational scales, the reading sustains multiple internally coherent perspectives with measurable extraction but also measurable coordination gains.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from power level, exit options, and beneficiary/victim status. Domestic regulators (powerless/trapped/victim) derive d ≈ 0.95 → high f(d) ≈ 1.42 → high experienced extraction χ. Environmental coalitions (organized/constrained/mixed) derive d ≈ 0.40-0.50 → moderate f(d) ≈ 0.40-0.65 → moderate χ. Trade system (institutional/arbitrage/beneficiary) derives d ≈ 0.10 → low f(d) ≈ -0.01 → negative/minimal χ. Transnational capital (powerful/constrained/mixed) derives d ≈ 0.50 → moderate f(d) ≈ 0.65 → moderate χ (beneficiary + constrained = mixed directionality). NAFTA states (institutional/constrained/mixed) derive d ≈ 0.45 → moderate f(d) ≈ 0.55 → moderate χ. The directionality profiles reveal why the embedded liberalism reading is contested: most actors experience non-negligible extracted value (except the trade system itself, which experiences the constraint as functional coordination). No override needed — the structural derivation captures the actual relationships.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    non_discrimination_definition_contestation,
    'What counts as ''non-discriminatory'' environmental/labor regulation versus disguised protectionism? Is the boundary empirically measurable or inherently contestable?',
    'Meta-analysis of NAFTA dispute settlement decisions: frequency and consistency of ''legitimate objectives'' findings; identification of systematic patterns favoring capital or domestic regulators; comparison with parallel dispute mechanisms (EU, WTO)',
    'If boundary is measurable and consistently applied: embedded liberalism framework is functional (Rope to Tangled Rope). If boundary is inherently contestable and systematically favors capital: framework is false-summit (actually capital supremacy), making this reading foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_discrimination_definition_contestation, empirical, 'Empirical definition of non-discriminatory versus protectionist regulation').

omega_variable(
    legitimate_objectives_scope_drift,
    'How much extraction occurs through narrowing the ''legitimate objectives'' category via dispute panel interpretation over time? Does the scope remain stable or erode?',
    'Longitudinal analysis of NAFTA disputes: track panel rulings on ''legitimate objectives'' scope (what regulations count as within scope); measure success rates of environmental/labor defenses; compare early vs late decisions for systematic drift',
    'If scope is stable: embedded liberalism constraint is authentic (Tangled Rope). If scope erodes over time: extraction mechanism is litigation-driven narrowing (shifts to Snare dynamics). If scope expands: reading may be overoptimistic about regulatory space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimate_objectives_scope_drift, empirical, 'Temporal drift in ''legitimate objectives'' scope via dispute panel interpretation').

omega_variable(
    isds_litigation_cost_deterrence,
    'Do ISDS litigation costs (even when governments win disputes) create preventive suppression? Do regulators self-censor plausible non-discriminatory policies to avoid litigation expense?',
    'Qualitative interviews with regulatory officials and cost analysis: document cases where anticipated litigation costs altered policy design; survey environmental/labor agencies on litigation threat perception; compare policy stringency in NAFTA jurisdictions versus non-NAFTA with similar environmental baselines',
    'If litigation costs create significant suppression: extraction mechanism is structural (suppression ≥ 0.48 confirmed) but not overtly extractive (Tangled Rope confirmed). If suppression is minimal: embedded liberalism framework is working as intended. If suppression is severe and systematic: reading is false-summit (actually snare for regulatory agencies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isds_litigation_cost_deterrence, empirical, 'Whether ISDS litigation threat suppresses non-discriminatory regulation').

omega_variable(
    sovereignty_primacy_coexistence_question,
    'Can the sovereignty_primacy_reading coexist with this embedded_liberalism_reading within a single NAFTA framework, or does one reading logically foreclose the other?',
    'Textual and meta-jurisdictional analysis: do NAFTA provisions simultaneously affirm ''legitimate policy space'' (embedded liberalism) and ''investor protection'' (capital supremacy)? Are these genuinely coexistent (different parties interpreting the same text) or mutually foreclosing (one interpretation contradicts the other)?',
    'If coexistent: the kernel reading_relation should be ''coexists_with''. If foreclosing: reading_relation should be ''forecloses''. The distinction determines whether embedded liberalism is a live interpretive position or a superseded reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_primacy_coexistence_question, conceptual, 'Whether embedded liberalism and sovereignty primacy readings coexist or foreclose each other').

omega_variable(
    legitimacy_grounding_axiom_empirical_challenge,
    'The embedded liberalism reading rests on an axiom that environmental/labor standards can be empirically shown to be ''legitimate objectives'' (non-protectionist). If empirical evidence shows systematic correlation between environmental stringency and trade balance outcomes, does this undermine the axiom?',
    'Econometric analysis: correlate environmental/labor regulation stringency with trade balances, FDI flows, and regulatory adoption patterns across NAFTA jurisdictions. If strict regulations systematically improve trade balances or reduce FDI inflows, the ''non-discriminatory objective'' axiom is empirically challenged.',
    'If axiom is empirically challenged: status changes from ''holdable'' to ''overridden'' within this reading''s own tradition. The reading must either abandon the empirical grounding or shift to deontological grounding (environmental rights, labor dignity as intrinsic goods, not contingent policy preferences).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_grounding_axiom_empirical_challenge, empirical, 'Whether empirical evidence challenges the ''legitimate environmental/labor objectives'' axiom').

omega_variable(
    reading_ascendancy_moment_timing,
    'At what historical moment did the embedded liberalism reading become ascendant in NAFTA interpretation? Was it present from ratification or emerge later through dispute panel practice?',
    'Historical analysis of NAFTA negotiating record, early dispute panels, and contemporary political economy scholarship; identify first prominent invocation of ''legitimate objectives'' framing and track subsequent adoption by panels, governments, and civil society',
    'If present from ratification: reading is endogenous to the agreement''s design (kernel_codification: formalized). If emergent: reading is a constructed interpretation imposed after the fact (kernel_codification: distributed or implicit). Timing affects the reading''s authority grounding and axiom status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_ascendancy_moment_timing, empirical, 'Historical emergence of embedded liberalism interpretation in NAFTA practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_emlib_tr_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nafta_emlib_tr_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 6, 0.58).
narrative_ontology:measurement(nafta_emlib_tr_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 12, 0.61).

% Extraction over time
narrative_ontology:measurement(nafta_emlib_be_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(nafta_emlib_be_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(nafta_emlib_be_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 12, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(nafta_emlib_su_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(nafta_emlib_su_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement(nafta_emlib_su_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 12, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, resource_allocation).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, investor_state_dispute_settlement_threat).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_regulation_harmonization).

% DUAL FORMULATION NOTE:
% The nafta_jurisdictional_boundary kernel decomposes into three constraint stories, one for each reading (embedded liberalism, capital supremacy, sovereignty primacy). Each story has its own ε value reflecting the empirical prevalence and structural stability of that reading. The embedded_liberalism_reading has ε=0.38 because it genuinely coordinates market access and policy space but extraction occurs through litigation costs and interpretive narrowing. The capital_supremacy_reading would have higher ε (more pure extraction, less coordination). The sovereignty_primacy_reading would have different measurement along the political-economy axis (extraction by states FROM capital, not capital from states). All three stories share the same base kernel but instantiate structurally distinct constraints with different ε values, beneficiary/victim structures, and perspectives. They are linked through network.affects_constraints to enable modeling of how contestation over the kernel's interpretation structures the broader trade regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
