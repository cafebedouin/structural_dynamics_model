% ============================================================================
% CONSTRAINT STORY: trump_indian_tariffs_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trump_indian_tariffs_2026, []).

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
 *   constraint_id: trump_indian_tariffs_2026
 *   human_readable: Trump's Tariff Reduction on India (2026)
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   In 2026, a Trump administration reduces tariffs on Indian goods from
 *   their previous level to 18% after the Modi government agrees to reduce
 *   oil purchases from Russia, aligning with US geopolitical objectives
 *   against Russian energy revenues. This constraint combines trade
 *   coordination (the tariff reduction provides genuine market access and
 *   economic benefit to Indian exporters and US import-dependent businesses)
 *   with geopolitical extraction (India is coerced into energy policy
 *   compliance). The constraint exhibits a fundamental tension: framed as a
 *   'deal' or 'trade agreement' (implying mutual benefit and Rope-like
 *   coordination), it is structurally a conditional leverage mechanism
 *   (Tangled Rope with strong Snare characteristics for US labor). The
 *   theater ratio has increased over the interval as the constraint's
 *   geopolitical character has become explicit, displacing the pretense of
 *   pure trade negotiation. Extractiveness has risen as the real extraction
 *   mechanism (Russia sanctions compliance as a non-negotiable condition) has
 *   become structurally embedded.
 *
 * KEY AGENTS:
 *   - Indian Exporters: Organized beneficiary (organized/arbitrage) — gain market access and export competitiveness; can exit by shifting supply chains if policy changes
 *   - Indian Government: Powerful state actor (powerful/constrained) — forced to choose between tariff benefit and Russian oil partnership; has limited exit (constrained by alliance options and energy security constraints)
 *   - US Domestic Manufacturing Labor: Powerless victim (powerless/trapped) — displaced by import competition; geographically immobile and skill-constrained; no exit option from tariff policy
 *   - US Administration: Institutional enforcer (institutional/mobile) — uses tariff as tool for geopolitical compliance; sees constraint as revocable (mobile exit: can reimpose tariffs)
 *   - US Import-Dependent Business: Moderate mixed actor (moderate/constrained) — benefits from tariff reduction (lower input costs) but constrained by global supply chain integration
 *   - Multilateral Trade System: Institutional rule-set (institutional/analytical) — WTO-based framework for tariff negotiation is bypassed; bilateral conditionality degrades rule-based system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trump_indian_tariffs_2026, 0.52).
domain_priors:suppression_score(trump_indian_tariffs_2026, 0.65).
domain_priors:theater_ratio(trump_indian_tariffs_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trump_indian_tariffs_2026, tangled_rope).
narrative_ontology:human_readable(trump_indian_tariffs_2026, "Trump's Tariff Reduction on India (2026)").
narrative_ontology:topic_domain(trump_indian_tariffs_2026, "economic/geopolitical").

domain_priors:requires_active_enforcement(trump_indian_tariffs_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trump_indian_tariffs_2026, indian_exporters).
narrative_ontology:constraint_beneficiary(trump_indian_tariffs_2026, us_administration_geopolitical_goals).
narrative_ontology:constraint_victim(trump_indian_tariffs_2026, us_domestic_manufacturing_labor).
narrative_ontology:constraint_victim(trump_indian_tariffs_2026, indian_government_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIAN EXPORTERS (ROPE) — Organized industrial actors gain market access (18% vs previous higher tariff). Experience constraint as coordination mechanism: tariff reduction enables trade flow with predictable access. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01. Net beneficiary with strong arbitrage options (can shift supply chains if tariffs shift again).
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: US DOMESTIC MANUFACTURING LABOR (SNARE) — Powerless actors in displaced US manufacturing face job losses from increased Indian import competition. Trapped by geographic immobility, skill constraints, and lack of retraining resources. No meaningful exit from tariff reduction policy. d≈0.92, f(d)≈1.39, σ=1.0 → χ≈0.72. High effective extraction.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INDIAN GOVERNMENT (TANGLED ROPE) — Powerful state actor obtains tariff relief (coordination benefit) but is coerced into reducing Russian oil purchases (extraction). The quid pro quo structure is enforced: tariff reduction is conditional on Russia sanctions compliance. Coordination function (trade access) + asymmetric extraction (geopolitical coercion). d≈0.58, f(d)≈0.68, σ=1.2 → χ≈0.42.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: US ADMINISTRATION (SCAFFOLD) — Institutional actor uses tariff reduction as temporary leverage device with implicit sunset: tariff structure is presented as conditional and revocable. Theater of 'deal-making' obscures the permanent structural coercion (removal of Russian oil purchases is permanent; tariff conditionality is the mechanism). d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07. Sees the constraint as a tool with built-in negotiation dynamics (can raise tariffs again if terms violated). Theater_ratio elevated because tariff reduction is marketed as 'winning' deal-making rather than geopolitical coercion mechanism.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: US DOMESTIC BUSINESS (CONSTRAINED MIDDLE) (TANGLED ROPE) — US firms dependent on Indian inputs or with global supply chains experience tariff reduction as beneficial coordination (lower input costs, market stability). But domestic manufacturing sectors face extraction through job displacement. Moderate power, constrained exit (integrated global supply chains lock in actors). d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34. Mixed experience: coordination benefit + extraction cost.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MULTILATERAL TRADE SYSTEM (PITON) — WTO-based rules and norms for tariff negotiation are performatively maintained but functionally degraded by bilateral conditionality (tariff reduction tied to Russia policy, outside WTO framework). Rules persist through inertia; actual enforcement happens through direct state coercion. Theater_ratio=0.58 reflects this: tariff discussions use trade language, but the real mechanism is geopolitical leverage. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.80.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (APPARENT MOUNTAIN) — From a universal/civilizational frame, one might naturalize this as immutable: 'Great powers always condition trade on geopolitics; this is how the international system works.' However, base extraction (0.52), suppression (0.65), and theater (0.58) contradict the mountain gates. This is a false summit: the constraint is institutional, not natural law. The claim 'geopolitics always corrupts trade' is descriptive of current practice, not an inherent logical limit.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_indian_tariffs_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trump_indian_tariffs_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_indian_tariffs_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trump_indian_tariffs_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trump_indian_tariffs_2026, TR),
    TR >= 0.70.

:- end_tests(trump_indian_tariffs_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The tariff reduction provides genuine benefit to Indian exporters (approximately 10-15 percentage point improvement in market access), but the conditioning on Russian oil policy represents extraction from India (forced policy change outside India's preferred alignment). The overall extractiveness reflects the mixed nature: coordination benefit (tariff reduction) + coercion (Russia policy compliance). Theater ratio (0.58): Rising. Early presentation emphasized 'deal-making' and mutual benefit language; actual conditionality mechanism (tariff contingent on Russia policy) has become explicit, reducing performative theater relative to underlying coercion. Suppression (0.65): Moderate-high. US labor has no meaningful exit or alternative (geographic immobility, retraining barriers); India has constrained exit (energy security alternatives are limited and expensive). The constraint is maintained through threat (tariff reinstatement if compliance falters).
 *
 * PERSPECTIVAL GAP:
 *   Indian exporters see Rope (coordination, market access, predictable rules). Indian government sees Tangled Rope (coordination benefit + geopolitical coercion). US domestic labor sees Snare (extraction without exit, job displacement). US import businesses see Tangled Rope (mixed benefit from lower tariffs, harm from labor cost effects). US administration sees Scaffold (temporary leverage tool with implicit revision capability). Multilateral system sees Piton (rule-based framework degraded to performative function, actual enforcement through bilateral coercion). The perspectival gap reveals that the constraint functions differently depending on structural position: beneficiary sees coordination; coerced state sees mixed extraction; powerless victim sees pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Indian Exporters: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary. Indian Government: Beneficiary (tariff) + victim (Russia policy coercion) + constrained exit → d≈0.58, f(d)≈0.68. Mixed but leaning toward extraction due to constrained energy alternatives. US Domestic Labor: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction — no geographical or skill-based exit. US Administration: Institutional beneficiary + mobile (can enforce or reverse policy) → d≈0.05, f(d)≈-0.12. Net beneficiary with high agency. US Business: Mixed beneficiary/victim + constrained → d≈0.50, f(d)≈0.65. Balanced extraction and coordination benefit. Multilateral System: Institutional + analytical → d≈0.72, f(d)≈1.15. Degraded by extraction mechanism layered onto framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy surfaces the labeling problem: Is this a 'trade agreement' (Rope frame) or 'geopolitical coercion' (Snare/Tangled Rope frame)? The base extraction (0.52) is high enough to prevent Rope classification from the analytical perspective. The structural data reveals that framing this as a pure trade deal (Rope) is naturalization of coercion. The Tangled Rope classification (beneficiaries: Indian exporters + US geopolitical goals; victims: US labor + Indian government autonomy; enforcement required) resolves the mandatrophy by acknowledging both the genuine coordination function (tariff reduction does enable trade) and the extraction mechanism (Russia policy compliance is forced). The analytical observer risks the false summit: seeing 'great powers always condition trade on geopolitics' as immutable law rather than contingent institutional practice. The extraction mechanism (tariff threat + Russia compliance) is reversible and policy-dependent, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    russian_oil_substitution_feasibility,
    'Can India actually replace Russian oil imports with alternative sources (Middle Eastern, African) without significant economic cost or energy security degradation?',
    'Energy modeling of substitution costs; analysis of alternative supplier availability and long-term contract terms; assessment of India''s foreign exchange reserves to absorb price premiums',
    'If feasible at low cost: Indian government extraction is moderate (constrained exit option is real). If very costly or infeasible: Indian government is effectively trapped (exit option degrades to trapped, d moves to 0.90+, classification shifts toward snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(russian_oil_substitution_feasibility, empirical, 'Feasibility and cost of substituting Russian oil imports').

omega_variable(
    tariff_conditionality_enforcement,
    'Is the 18% tariff reduction permanently structural, or functionally a conditional/revocable tool that reverts if Russia policy changes?',
    'Analysis of tariff schedule enforcement; tracking of tariff policy statements post-agreement; historical pattern of tariff reversals; legal/administrative mechanism for reinstatement',
    'If permanent: constraint is stable tangled_rope with long-term extraction. If revocable: scaffold logic applies; sunset is implicit (threat of reimposition enforces compliance); constraint is coercion mechanism with built-in theater (negotiation leverage).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tariff_conditionality_enforcement, conceptual, 'Whether tariff reduction is permanent or conditional/revocable').

omega_variable(
    us_labor_adjustment_capacity,
    'What proportion of US manufacturing jobs displaced by increased Indian imports can be reabsorbed through regional economic development, retraining, or alternative sectors within 5-10 years?',
    'Labor displacement analysis; sectoral absorption potential; comparison to historical US manufacturing transitions; regional development program effectiveness',
    'If >60% reabsorption: snare classification for labor is overstated; some exit options emerge. If <30% reabsorption: snare is structural (trapped classification correct); victims have persistent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_labor_adjustment_capacity, empirical, 'US labor adjustment and reabsorption capacity post-tariff reduction').

omega_variable(
    geopolitical_conditionality_precedent,
    'Does this tariff-for-Russia-policy trade set a durable precedent for conditioning trade agreements on non-trade foreign policy compliance?',
    'Tracking of subsequent tariff negotiations; analysis of whether US applies similar conditionality to other countries; study of international normalization (whether other states adopt similar conditional tariff structures)',
    'If precedent-setting: multilateral system degrades further (piton classification strengthened; theater ratio likely to increase as compliance becomes performative); scaffold logic weakens (constraint becomes permanent institutional feature). If isolated: scaffold sunset logic holds; constraint may fade as admin priorities shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_conditionality_precedent, empirical, 'Whether tariff conditionality becomes durable precedent for other agreements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trump_indian_tariffs_2026, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tariff_tr_t0, trump_indian_tariffs_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tariff_tr_t6, trump_indian_tariffs_2026, theater_ratio, 6, 0.52).
narrative_ontology:measurement(tariff_tr_t12, trump_indian_tariffs_2026, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(tariff_be_t0, trump_indian_tariffs_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tariff_be_t6, trump_indian_tariffs_2026, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(tariff_be_t12, trump_indian_tariffs_2026, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trump_indian_tariffs_2026, resource_allocation).
narrative_ontology:affects_constraint(trump_indian_tariffs_2026, russian_energy_sanctions_regime).
narrative_ontology:affects_constraint(trump_indian_tariffs_2026, us_india_strategic_partnership).
narrative_ontology:affects_constraint(trump_indian_tariffs_2026, us_manufacturing_labor_displacement).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the Russian sanctions regime (upstream, higher empirical stability) and represents a distinct mechanism for enforcing sanctions compliance through trade conditionality. The tariff structure creates a separate extraction vector that requires its own measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trump_indian_tariffs_2026, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
