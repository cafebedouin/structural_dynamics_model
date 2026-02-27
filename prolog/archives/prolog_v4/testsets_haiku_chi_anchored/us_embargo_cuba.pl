% ============================================================================
% CONSTRAINT STORY: us_embargo_cuba
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_embargo_cuba, []).

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
 *   constraint_id: us_embargo_cuba
 *   human_readable: US Embargo of Cuba
 *   domain: political/economic
 *
 * SUMMARY:
 *   The US Embargo of Cuba, initiated in 1962 and maintained continuously for
 *   over six decades, represents a structural extraction mechanism justified
 *   by security and geopolitical coordination narratives. The constraint
 *   emerged from Cold War threat containment (legitimate coordination
 *   function) but persists through institutional inertia and domestic
 *   political coalitions long after the original threat justification has
 *   degraded. The embargo operates as a layered extraction system: primary
 *   victims (Cuban population, economy) are trapped with no exit; secondary
 *   victims (regional trade partners, international firms) are constrained
 *   through secondary sanctions and financial system control; beneficiaries
 *   (US strategic coalition, Cuban exile political leadership) maintain the
 *   mechanism through legislative architecture (Helms-Burton Act) and threat
 *   narrative renewal. The perspectival analysis reveals all six constraint
 *   types are coherent readings of the same structural data, but mandatrophy
 *   resolution pivots on whether the original coordination function (Cold War
 *   containment) remains genuine or has degraded into pure extraction
 *   maintained by inertia. The increasing theater ratio (0.25 → 0.58)
 *   indicates growing performative maintenance: Cold War threat narrative
 *   persists despite geopolitical irrelevance; actual enforcement is
 *   inconsistent; humanitarian exceptions and partial normalization attempts
 *   suggest the mechanism no longer serves clear coordination function.
 *
 * KEY AGENTS:
 *   - Cuban population and state: Primary victim (powerless/trapped) — bears full extraction cost; cannot exit; no meaningful negotiating leverage
 *   - Cuban economy: Primary victim (powerless/trapped) — excluded from US markets; constrained from third-party trade; subject to secondary sanctions
 *   - Regional trade partners (Mexico, Canada, Caribbean states): Secondary victim (moderate/constrained) — face choice between Cuba trade and US market access; many choose constrained compliance
 *   - US strategic coalition (State Department, Defense, Cuban exile leadership): Primary beneficiary (institutional/arbitrage) — maintains embargo as geopolitical alignment mechanism; experiences as legitimate coordination
 *   - US corporate and financial sector: Mixed (powerful/arbitrage) — excluded from Cuban market (victim), but benefits from sanctions compliance mechanisms and market concentration (beneficiary)
 *   - International trade and humanitarian organizations: Organized challenger (organized/mobile) — frames embargo as violation of international norms; advocates for negotiated sunset via normalization
 *   - Institutional Cold War structure: Inertial beneficiary (institutional/constrained) — Helms-Burton Act, legislative restrictions, threat narratives maintain mechanism through institutional momentum
 *   - Analytical observer: Global trade law perspective (analytical/analytical) — sees embargo as violation of WTO/OAS/UN principles; classifies as pure snare despite coordination framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_embargo_cuba, 0.68).
domain_priors:suppression_score(us_embargo_cuba, 0.75).
domain_priors:theater_ratio(us_embargo_cuba, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_embargo_cuba, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_embargo_cuba, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_embargo_cuba, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_embargo_cuba, snare).
narrative_ontology:human_readable(us_embargo_cuba, "US Embargo of Cuba").
narrative_ontology:topic_domain(us_embargo_cuba, "political/economic").

domain_priors:requires_active_enforcement(us_embargo_cuba).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_embargo_cuba, us_strategic_interests).
narrative_ontology:constraint_beneficiary(us_embargo_cuba, us_aligned_latin_american_governments).
narrative_ontology:constraint_beneficiary(us_embargo_cuba, cuban_exile_political_coalition).
narrative_ontology:constraint_victim(us_embargo_cuba, cuban_population).
narrative_ontology:constraint_victim(us_embargo_cuba, cuban_economy).
narrative_ontology:constraint_victim(us_embargo_cuba, regional_trade_partners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CUBAN POPULATION / ECONOMY (SNARE) — No meaningful exit option. Cuba cannot unilaterally lift the embargo; cannot fully bypass US financial system; cannot access US market; constrained from trading with US companies and third parties doing business in US. Trapped exit + victim status → d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.86. Pure extraction with minimal coordination function.
constraint_indexing:constraint_classification(us_embargo_cuba, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL TRADE PARTNERS (SNARE) — Countries trading with Cuba face choice: maintain Cuba trade or access US markets/investment. Many choose constrained compliance rather than full exit. Effective extraction mechanism: secondary sanctions and loss of US investment. d≈0.70, f(d)≈1.05, σ=0.9 → χ≈0.64. Constrained exit limits alternative pathways but not zero-cost.
constraint_indexing:constraint_classification(us_embargo_cuba, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US STRATEGIC COALITION (ROPE) — US policymakers, aligned Latin American governments, Cuban exile leadership coordinate embargo maintenance as geopolitical alignment mechanism. For these actors, the embargo serves a coordination function: signaling strategic commitment, managing regional bloc discipline. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.09. Net beneficiary; experiences constraint as legitimate coordination.
constraint_indexing:constraint_classification(us_embargo_cuba, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US CORPORATE / FINANCIAL SECTOR (TANGLED ROPE) — US firms excluded from Cuban market (victim of coordination enforcement), but dominant US financial institutions benefit from capital controls and sanctions compliance mechanisms (beneficiary of extraction fees, compliance consulting, market concentration). Dual relationship creates mixed experience. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.41. Moderate mixed extraction and coordination.
constraint_indexing:constraint_classification(us_embargo_cuba, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL ORGS / HUMANITARIAN COALITION (SCAFFOLD) — UN resolutions, OAS arguments, humanitarian organizations frame embargo as temporary violation of international trade norms with explicit sunset logic: diplomatic normalization would end embargo. Coalition sees constraint as coordination failure to be overcome via negotiation with built-in exit (normalized relations). d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.37. Theater ratio (0.58) reflects mix of humanitarian rhetoric and geopolitical realism; has_sunset_clause reflects negotiation pathways.
constraint_indexing:constraint_classification(us_embargo_cuba, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL INERTIA / CIVILIZATIONAL VIEW (PITON) — The embargo persists through institutional momentum and legislation (Helms-Burton Act, 1996) despite degraded original function (Cold War containment irrelevant post-1991). Theater_ratio (0.58) reflects substantial performative maintenance: embargo renewal justified via outdated threat narratives, Cold War rhetoric persists in policy discussions, actual enforcement is inconsistent. Institutional inertia keeps mechanism alive despite functional atrophy. d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.46.
constraint_indexing:constraint_classification(us_embargo_cuba, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational/global scope, the embargo violates established international trade law (WTO principles, OAS charter, UN charter). Observer sees structural extraction: Cuba trapped, enforcement mechanisms active, suppression high (financial system control), with rationalization via Cold War threat narrative that no longer applies. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.94. High effective extraction; observer sees snare despite coordination framing.
constraint_indexing:constraint_classification(us_embargo_cuba, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_embargo_cuba_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_embargo_cuba, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_embargo_cuba, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_embargo_cuba, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_embargo_cuba, TR),
    TR >= 0.70.

:- end_tests(us_embargo_cuba_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The embargo extracts significant costs from Cuba: reduced GDP growth (~2-3% annual loss estimates), medical/food shortages, brain drain, technological isolation. The extraction is substantial but not absolute (Cuba maintains some trade with third parties, limited services economy). The 0.68 value reflects that the mechanism does work as intended (constrains Cuban economic development) but is not maximally tight. Suppression (0.75): High. Cuba has minimal exit options: cannot unilaterally lift embargo (requires US action); cannot fully bypass US financial system (dollar dominance); faces secondary sanctions if third parties trade too openly. Regional partners also face suppression: US market access penalty for Cuba trade. Theater ratio (0.58): Moderate-high. The original Cold War justification (Soviet proxy, Communist threat) is now substantially performative (Soviet Union dissolved 1991; Cuba poses minimal military threat; actual enforcement is selective). Threat narratives persist in policy debates and legislative renewal, but actual security measures are inconsistent. Theater has increased from 0.25 (early Cold War period when threat was structurally real) to 0.58 (contemporary period when narrative is maintained despite degraded functionality). Claimed type (snare): Justified by high extractiveness (0.68 > 0.46), high suppression (0.75 > 0.60), effective extraction χ in snare range. Mandatrophy resolved (true): The constraint exists at the intersection of two interpretations (coordination vs extraction). Mandatrophy is resolved by acknowledging both are present: the constraint has genuine geopolitical coordination function (US-Cuba regional positioning) AND asymmetric extraction (Cuba bears costs). The resolution is that a snare can be a legitimate instrument of statecraft (security extraction) without being mislabeled. Whether the original security justification remains genuine is the omega variable that would shift the mandatrophy resolution from 'legitimate snare' to 'illegitimate snare'.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim is maximal. The Cuban population (trapped, powerless) experiences pure extraction with no coordination benefit (snare). Regional trade partners (constrained, moderate) experience mixed extraction and coordination pressure (snare, but with partial coordination rationale). The US strategic coalition (institutional, arbitrage) experiences pure coordination (rope) — geopolitical alignment and regional bloc discipline are genuine coordination functions. The analytical observer (analytical/global scope) sees the system as violating international trade law, classifying as snare despite beneficiary framing as coordination. The international humanitarian coalition (organized, mobile) frames the constraint as a temporary coordination failure with explicit sunset (scaffold) — negotiation can resolve it. The institutional inertia perspective (piton) reveals that much of the constraint's maintenance is performative ritual divorced from original function. These six perspectives cover the full range from pure snare (victim view) to pure rope (beneficiary view) to piton (institutional view), revealing that the constraint's classification depends entirely on the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Cuban population: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Regional trade partners: Victim + constrained → d≈0.70, f(d)≈1.05. Significant extraction but not absolute (some can exit via economic diversification). US strategic coalition: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; experiences as legitimate coordination. US corporate sector: Mixed beneficiary/victim + arbitrage → d≈0.45, f(d)≈0.50. Dual role: excluded from market (victim), benefits from compliance infrastructure (beneficiary). International orgs/humanitarian: Organized observer + mobile → d≈0.50, f(d)≈0.65. Neutral observer with exit option to advocate alternative. Institutional inertia: Institutional + constrained → d≈0.55, f(d)≈0.75. Somewhat constrained by its own maintenance costs; high theater indicates degrading function. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Standard analytical perspective; sees high effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED AS: Snare classification is legitimate under the current security justification framework. The constraint exhibits all snare properties (ε=0.68, suppression=0.75, χ=0.86) and serves an explicit coordination function (geopolitical alignment, regional bloc discipline) that is not merely rhetorical. However, the omega variable 'cold_war_threat_reality' is the critical uncertainty: if the original threat justification is revealed to be primarily narrative rather than genuine, the mandatrophy pivots. Current resolution assumes threat narrative has some basis, making the extraction a legitimate if problematic security instrument. The increasing theater ratio (0.58) indicates the narrative is degrading — if theater_ratio crosses 0.70, the constraint would shift toward piton classification, indicating the coordination function is substantially atrophied and the extraction is maintained primarily by institutional inertia. The scaffold perspective (international orgs) argues for explicit sunset via negotiated normalization — if a credible negotiation pathway exists (2015-2017 precedent suggests yes), the constraint could transition to scaffold classification. Current assessment is snare with degrading coordination function; future trajectory is piton (if theater rises and coordination atrophies) or scaffold (if normalization becomes viable).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cold_war_threat_reality,
    'Is Cuba a genuine strategic threat to US security, or is the threat narrative maintained primarily for domestic political coalitions?',
    'Declassified intelligence assessment; comparison of stated threat justifications (1960s, 1980s, 2000s, 2020s); analysis of actual military/security incidents vs projected threat level',
    'If genuine threat: snare classification is justified security extraction. If primarily narrative: snare is unmasked as pure extraction mechanism absent coordination function. Shifts mandatrophy from resolved-as-legitimate to resolved-as-illicit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cold_war_threat_reality, empirical, 'Whether Cuban threat narrative is genuine security concern or maintained for political coalition').

omega_variable(
    embargo_economic_efficacy,
    'Does the embargo demonstrably constrain Cuban regime behavior, or does it entrench regime by eliminating reform pressure and providing external scapegoat?',
    'Comparative analysis of regime behavior under embargo vs during periods of normalized trade (1960s pre-embargo, brief 2015-2017 normalization window); econometric modeling of counterfactual Cuban political trajectory without embargo',
    'If efficacious: supports beneficiary claim of strategic value. If counterproductive: reveals extraction mechanism disguised as coordination; pivots classification from snare-with-coordination to pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embargo_economic_efficacy, empirical, 'Whether embargo constrains regime behavior or entrenches it').

omega_variable(
    regional_support_fragility,
    'Is the Latin American coalition support for embargo enforcement fragile (maintained via pressure) or stable (genuinely shared strategic interest)?',
    'Analysis of voting patterns in OAS on embargo-related resolutions; interviews with non-aligned Latin American governments on rationale for any embargo compliance; tracking of trade partnership patterns with Cuba during embargo period',
    'If fragile: region-scale snare revealed (region trapped by US pressure). If stable: genuine regional coordination. Differentiates whether suppression is bilateral (US-Cuba) or systemic (US-enforced regional control).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_support_fragility, empirical, 'Whether regional support for embargo is fragile or stable').

omega_variable(
    normalization_exit_credibility,
    'Would actual diplomatic normalization (as negotiated 2015-2017) constitute a genuine sunset, or would new enforcement mechanisms (tariffs, sectoral sanctions) replace embargo terminology?',
    'Analysis of 2015-2017 normalization agreement implementation; comparison of trade/financial restrictions before vs after normalization renaming; tracking of sanctions architecture evolution in other cases (Iran, Russia, Vietnam)',
    'If genuine sunset: scaffold classification is structurally sound. If replacement mechanisms: sunset is illusory; constraint persists via relabeling (snare persists as modified snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalization_exit_credibility, empirical, 'Whether normalization constitutes genuine sunset or mechanism replacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_embargo_cuba, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emb_tr_t0, us_embargo_cuba, theater_ratio, 0, 0.25).
narrative_ontology:measurement(emb_tr_t15, us_embargo_cuba, theater_ratio, 15, 0.42).
narrative_ontology:measurement(emb_tr_t32, us_embargo_cuba, theater_ratio, 32, 0.58).

% Extraction over time
narrative_ontology:measurement(emb_be_t0, us_embargo_cuba, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(emb_be_t15, us_embargo_cuba, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(emb_be_t32, us_embargo_cuba, base_extractiveness, 32, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_embargo_cuba, enforcement_mechanism).
narrative_ontology:affects_constraint(us_embargo_cuba, us_sanctions_regime_architecture).
narrative_ontology:affects_constraint(us_embargo_cuba, secondary_sanctions_financial_system).
narrative_ontology:affects_constraint(us_embargo_cuba, regional_trade_bloc_formation).

% DUAL FORMULATION NOTE:
% The US embargo of Cuba exists at the intersection of two constraint types: (1) Pure geopolitical extraction (snare) targeting Cuban economic development, and (2) Regional enforcement mechanism (enforcement_mechanism coordination type) maintaining US-aligned Latin American bloc discipline. The snare classification emphasizes the extraction/suppression axis; the enforcement_mechanism designation emphasizes the coordination function. Network links reveal that the embargo's persistence affects secondary sanctions architecture (enforcement_mechanism design) and regional bloc formation responses (adaptive coordination). Normalization would shift the constraint toward scaffold or eliminate it entirely, with downstream effects on sanctions regime design and regional trade patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_embargo_cuba, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
