% ============================================================================
% CONSTRAINT STORY: carbon_externality_pricing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_carbon_externality_pricing, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: carbon_externality_pricing
 *   human_readable: Carbon Externality Pricing Mechanisms
 *   domain: environmental_economics/climate_policy
 *
 * SUMMARY:
 *   Carbon externality pricing represents a structural attempt to internalize
 *   the atmospheric commons into market prices, solving a coordination
 *   problem (global emissions require coordinated reduction) while
 *   simultaneously extracting rents from carbon-intensive actors and
 *   vulnerable populations. The constraint exhibits characteristics of all
 *   major DR types: it coordinates around climate damage reduction (rope),
 *   uses enforcement to extract from emitters (snare), combines coordination
 *   with asymmetric costs (tangled rope), maintains performative elements
 *   through offset theaters (piton), and aims at a technological sunset
 *   through renewable transition (scaffold). The distributional asymmetry is
 *   severe: developed economies historically imposed carbonization on
 *   development, then impose decarbonization costs on vulnerable populations
 *   and developing economies that depend on fossil fuel revenues.
 *   Extractiveness has increased over 20 years (0.35→0.58) as carbon pricing
 *   schemes proliferated, regulatory density increased, and behavioral costs
 *   accumulated. Theater ratio increased similarly as offset markets
 *   expanded, suggesting Goodhart drift — the metric of compliance
 *   increasingly substitutes for the real outcome (actual emissions
 *   reduction). The constraint now sits at a critical juncture: renewable
 *   energy cost curves threaten to render carbon pricing economically
 *   obsolete within 30 years, potentially transforming it from indefinite
 *   extraction (Snare) into temporary coordination (Scaffold). But that
 *   transition is not guaranteed — political actors may perpetuate carbon
 *   pricing mechanisms even after technology makes them economically
 *   unnecessary, creating a Piton (degraded ritual maintained through
 *   inertia). Identity-locked perspectives (coal communities whose
 *   self-concept is constituted through extraction industries) show trapped
 *   agents experiencing the constraint as existential rather than merely
 *   economic.
 *
 * KEY AGENTS:
 *   - Low-Income Energy Consumers: Primary victim (powerless/trapped) — bear immediate cost increases with no exit options; locked into energy dependence
 *   - Carbon-Intensive Industries: Secondary beneficiary and victim (organized/constrained) — benefit from regulatory clarity but bear pricing costs asymmetrically across jurisdictions and firm efficiency profiles
 *   - Renewable Energy Industries: Primary beneficiary (institutional/arbitrage) — capture market share and investment flows from carbon pricing; high exit optionality
 *   - Developing Economy Governments: Tertiary victim (institutional/constrained) — face constrained exit from global carbon pricing architecture; dependent on fossil revenues; bear adjustment costs imposed by developed economies
 *   - Coal-Dependent Regional Communities: Compound victim (moderate/identity_locked) — structurally mobile but identity-locked into extraction culture; extraction is material and existential
 *   - Government Revenue Streams: Beneficiary (institutional/arbitrage) — capture carbon tax revenues and trading profits; reallocate costs through welfare and subsidy programs
 *   - Climate-Forward Policy Coalition: Organized beneficiary (organized/mobile) — see constraint as temporary with renewable technology sunset; have exit pathway through decarbonization
 *   - Carbon Offset Markets: Piton actor (institutional/arbitrage) — maintain themselves through compliance requirements and auditing rituals despite low functional verification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carbon_externality_pricing, 0.52).
domain_priors:suppression_score(carbon_externality_pricing, 0.48).
domain_priors:theater_ratio(carbon_externality_pricing, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carbon_externality_pricing, extractiveness, 0.52).
narrative_ontology:constraint_metric(carbon_externality_pricing, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(carbon_externality_pricing, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carbon_externality_pricing, tangled_rope).
narrative_ontology:human_readable(carbon_externality_pricing, "Carbon Externality Pricing Mechanisms").
narrative_ontology:topic_domain(carbon_externality_pricing, "environmental_economics/climate_policy").

domain_priors:requires_active_enforcement(carbon_externality_pricing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carbon_externality_pricing, low_carbon_industries).
narrative_ontology:constraint_beneficiary(carbon_externality_pricing, government_revenue_streams).
narrative_ontology:constraint_beneficiary(carbon_externality_pricing, environmental_goods_beneficiaries).
narrative_ontology:constraint_victim(carbon_externality_pricing, carbon_intensive_industries).
narrative_ontology:constraint_victim(carbon_externality_pricing, energy_consumers_low_income).
narrative_ontology:constraint_victim(carbon_externality_pricing, developing_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME ENERGY CONSUMER (SNARE) — Trapped in energy dependence with no realistic alternatives. Carbon pricing increases costs immediately and unavoidably. No exit: relocation is expensive, energy alternatives are inaccessible, and substitution options (electric vehicles, home insulation) require capital. Bears extraction with minimal coordination benefit to themselves.
constraint_indexing:constraint_classification(carbon_externality_pricing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COAL-DEPENDENT REGIONAL ECONOMY (TANGLED ROPE) — Faces constrained exit: regional economy is structurally dependent on coal employment and tax revenue. Genuine coordination problem exists (global climate requires coordinated emissions reduction), but extraction is asymmetric — this region bears disproportionate adjustment costs while benefits (climate stability, avoided damages) accrue globally. Can transition but at high social cost over decades.
constraint_indexing:constraint_classification(carbon_externality_pricing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RENEWABLE ENERGY INDUSTRY (ROPE) — Benefits from carbon pricing through relative cost advantages and increased investment flows. High exit optionality: can relocate production, adapt supply chains, capture markets. Experiences the constraint as coordination: carbon pricing aligns incentives with decarbonization transition, enabling market expansion. Net beneficiary.
constraint_indexing:constraint_classification(carbon_externality_pricing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CARBON-INTENSIVE MANUFACTURING (TANGLED ROPE) — Organized actors (steel, cement, chemicals) face genuine coordination need: carbon pricing prevents a regulatory race-to-the-bottom, protecting all firms from unregulated competitors. But pricing imposes asymmetric costs across jurisdictions and firm-level efficiency profiles. Constrained exit: cannot immediately relocate massive capital assets but can lobby for exemptions, border adjustments, or gradual phase-ins. Mixed coordination and extraction.
constraint_indexing:constraint_classification(carbon_externality_pricing, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: CLIMATE-FORWARD POLICY COALITION (SCAFFOLD) — Organized actors (EU, some Nordic states, climate NGOs) see carbon pricing as a temporary coordination mechanism with a decarbonization sunset: as clean energy costs decline and technology matures, the economic case for carbon pricing weakens (renewable energy becomes cheaper than fossil fuels without subsidy). Coalition has exit path via technology transition and sees the extraction as declining over time. Sunset: 30-50 years as renewable parity is achieved.
constraint_indexing:constraint_classification(carbon_externality_pricing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CARBON-DEPENDENT REGIONAL COMMUNITY (SNARE/IDENTITY_LOCKED) — Structurally could exit (has transferable skills, physical mobility) but identity-locked into coal mining/extraction culture and place-based identity. Community identity constituted through extraction industry; exit would mean psychological displacement and cultural dissolution. Bearing extraction cost not just materially but through identity dissolution. Classification reflects that structural mobility (constrained → mobile) is occluded by identity lock.
constraint_indexing:constraint_classification(carbon_externality_pricing, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: CARBON OFFSET MARKETS (PITON) — Ostensibly a coordination mechanism for cost-effective emissions reduction, but largely theatrical: offset quality is unverifiable at scale, additionality is systematically overstated, and the market enables avoidance of real behavioral change. High theater ratio: the appearance of action replaces actual decarbonization. Maintains itself through institutional inertia (compliance requirements, auditing rituals) despite low functional verification. Theater has increased as offset demand has grown.
constraint_indexing:constraint_classification(carbon_externality_pricing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: DEVELOPING ECONOMY GOVERNMENT (TANGLED ROPE) — Faces constrained exit from global carbon pricing architecture: dependent on fossil fuel revenues and export competitiveness, but international pressure and climate finance requirements embed carbon pricing into conditional aid and trade agreements. Genuine coordination benefit exists (climate damages fall on vulnerable economies), but extraction is asymmetric: developed economies impose pricing mechanisms they developed domestically, transferring adjustment burden to less wealthy states with fewer alternatives.
constraint_indexing:constraint_classification(carbon_externality_pricing, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (ROPE) — From a universal perspective, carbon pricing could be viewed as pure coordination: the atmosphere is a commons, externalities require price signals to align incentives. The constraint solves a genuine coordination problem with low coercive overhead relative to the scope. But structural data reveals asymmetric extraction — the beneficiary/victim gap prevents this from being a true mountain. This perspective risks naturalizing what is actually a contingent institutional arrangement with distributional consequences.
constraint_indexing:constraint_classification(carbon_externality_pricing, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carbon_externality_pricing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carbon_externality_pricing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carbon_externality_pricing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(carbon_externality_pricing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(carbon_externality_pricing, TR),
    TR >= 0.70.

:- end_tests(carbon_externality_pricing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Carbon pricing does solve a genuine coordination problem (global emissions require coordinated reduction) but does so through asymmetric cost imposition. Low-income consumers, carbon-dependent regions, and developing economies bear immediate costs; renewable energy industries and governments capturing tax revenue benefit. The extractiveness is not maximum because some genuine coordination benefit exists (climate damages are real and asymmetrically distributed toward vulnerable populations), but the mechanism for addressing this creates new extraction. The 20-year trajectory (0.35→0.58) shows extractiveness accumulating as pricing schemes proliferate and behavioral costs mount. Suppression (0.48): Moderate. Significant barriers to exit include capital costs for technology transition, behavioral lock-in, geographic dependence on fossil fuel industries, and international agreements that embed carbon pricing into conditionality. Suppression is not maximum because alternatives do exist (renewable energy, energy efficiency) and costs are declining. But for low-income populations and developing economies, suppression is high because alternative access is blocked by capital constraints. Theater ratio (0.58): Moderate. Carbon offset markets are highly theatrical — offset quality is systematically overstated, additionality is unverifiable at scale, and the market enables avoidance of real behavioral change. But carbon pricing mechanisms themselves (carbon taxes, cap-and-trade) have more direct verification than offsets. The 16-point increase in theater ratio (0.42→0.58) reflects that as offsets have grown as a cost-containment mechanism, the functional verification of actual emissions reduction has declined. Claimed type (Tangled Rope): The constraint coordinates around climate damage reduction (genuine coordination benefit exists) while using enforcement to extract asymmetrically from carbon-intensive actors and vulnerable populations. Both coordination function and asymmetric extraction are essential.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between low-income consumer (Snare) and renewable energy industry (Rope) is maximal. One perceives pure extraction with no exit; the other perceives pure coordination with high optionality. The gap between developed-economy policy coalition (Scaffold) and developing-economy government (Tangled Rope) reflects asymmetric power: the coalition sees a temporary problem being solved by technology transition; the developing economy sees indefinite extraction embedded in international agreements. The identity_locked perspective (coal communities) shows that structural mobility (could exit, retrain, relocate) is occluded by identity fusion, creating a perception gap between the actual exit options and the experienced constraint. The piton perspective (offset markets) shows institutional theater: the appearance of action replaces the action itself, allowing policy actors to claim success while real decarbonization lags.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural position in the extraction flow. Low-income consumers: victims of trapped exit → high d (0.95) → high f(d) → high experienced extractiveness. Renewable energy industries: beneficiaries with arbitrage options → low d (0.10) → negative f(d) → negative experienced extractiveness (they subsidize the constraint). Carbon-intensive manufacturing: mixed position — they are enforced targets but also organized beneficiaries (carbon pricing prevents regulatory race-to-the-bottom) → moderate d (0.55) → moderate f(d). Developing economy governments: victims of constrained exit, but also dependent on fossil revenues that carbon pricing eliminates → high d (0.80) → high f(d). The pipeline derives d from beneficiary/victim declarations and exit options, computing experienced extractiveness χ = ε × f(d) × σ(S). Scope modifiers: global scope (σ=1.2) amplifies extraction because verification and enforcement are harder at planetary scale, creating complexity overhead.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by showing that carbon pricing is genuinely a Tangled Rope — it coordinates around climate damage reduction (real coordination problem) while extracting asymmetrically (real extraction mechanism). The beneficiary/victim distinction is not equivocal: renewables and developed governments benefit from relative advantage and tax revenues; low-income consumers, coal regions, and developing economies bear costs. The enforcement is real: compliance mechanisms (audits, tariffs, conditional aid) sustain the pricing architecture. The asymmetry is real: cost imposition is not proportional to historical responsibility or current vulnerability. However, the constraint shows TEMPORAL DRIFT in its mandatrophy structure. At inception (2005-2010, ε=0.35), the constraint was closer to Rope: carbon pricing was voluntary, compliance was low, and enforcement was minimal. As it matured (2015-2025, ε=0.52-0.58), the extraction mechanism strengthened: pricing became mandatory, enforcement increased, and offset theater expanded as a cost-shifting mechanism. The future mandatrophy depends on technological development: if renewable parity is achieved within 30 years, the constraint transitions to Scaffold (temporary coordination with sunset). If technology lags and carbon pricing persists despite renewable cost parity, it becomes Piton (degraded ritual maintained through institutional inertia). The current classification as Tangled Rope captures the present state: genuine coordination function + real asymmetric extraction + active enforcement. But the constraint's trajectory is not determined — it depends on empirical outcomes (technology costs, political willingness to phase down pricing after technology maturity) that are not yet resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_intensity_baseline_ambiguity,
    'What baseline for carbon accounting establishes the externality boundary? Is the externality the absolute emissions volume, or the deviation from some reference standard?',
    'Empirical comparison of real-world carbon accounting baselines across jurisdictions; analysis of how baseline selection changes extracted value distribution',
    'Different baselines dramatically alter who appears as beneficiary vs victim. Generous baselines (e.g., including past emissions growth) extract less from historical emitters; strict baselines (e.g., per-capita convergence) extract more from developed economies. Classification could shift from Tangled Rope (moderate extraction) to Snare (high extraction) or Rope (low extraction) depending on baseline choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_intensity_baseline_ambiguity, preference, 'Baseline selection determines externality boundary and benefit distribution').

omega_variable(
    technology_transition_velocity,
    'Will renewable energy cost declines outpace the timeline required to decarbonize hard sectors (cement, steel, aviation), rendering carbon pricing either unnecessary or insufficient?',
    'Projected technology cost curves from IPCC, IEA, and industrial research; comparison of decarbonization timeline vs renewable parity timeline',
    'If renewable parity achieved before hard-sector decarbonization: carbon pricing becomes a temporary scaffold (Scaffold classification strengthened). If hard-sector requires decarbonization faster than technology can deliver: carbon pricing becomes indefinite extraction (Tangled Rope or Snare classification strengthened). If technology outpaces requirement: pricing could become unnecessary (shifts toward Rope/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transition_velocity, empirical, 'Technology transition velocity relative to decarbonization timeline').

omega_variable(
    energy_democracy_exit_accessibility,
    'For low-income consumers, what is the actual cost of exit options (distributed solar, heat pumps, electric vehicle adoption) relative to household income? Is exit truly constrained or trapped?',
    'Household-level cost-benefit analysis for exit pathways; comparison of capital requirements vs income and available financing; tracking of actual adoption by income quintile',
    'If exit is genuinely constrained (high cost but achievable with subsidy/financing): classification remains Snare with potential downgrade to Tangled Rope if program design improves access. If exit is trapped (capital requirements exceed realistic access): Snare classification sustained. If exit becomes mobile (cost subsidized): shifts toward Rope from low-income perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(energy_democracy_exit_accessibility, empirical, 'True cost of exit options for low-income energy consumers').

omega_variable(
    border_carbon_adjustment_enforcement,
    'Can border carbon adjustments (carbon tariffs on imports from non-pricing jurisdictions) actually enforce carbon pricing globally, or do they become a protectionist mechanism disguised as environmental policy?',
    'Empirical analysis of BCA design, implementation, and actual enforcement; comparison of trade impacts vs emissions impacts; legal challenges and dispute resolution outcomes',
    'If BCA enforces coordinated pricing: extraction mechanism becomes more symmetric (moves toward Rope from developing-economy perspective). If BCA becomes protectionist: extraction mechanism is sustained and weaponized (Snare classification strengthened for vulnerable economies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(border_carbon_adjustment_enforcement, empirical, 'Border carbon adjustment enforcement capability and actual design').

omega_variable(
    offset_market_additionality_verification,
    'Can offset quality and additionality be verified at the scale required for carbon pricing mechanisms, or is the market inherently theatrical?',
    'Systematic audit of offset projects; comparison of claimed vs actual emissions reductions; tracking of offset market composition (fraudulent, inflated, or genuinely additional)',
    'If verification is achievable: offset markets function as legitimate cost-containment mechanisms (Rope from policy actor perspective). If verification fails: offset markets are pure theater sustaining Piton classification and enabling avoidance of real decarbonization, strengthening Snare classification for actors unable to use offsets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(offset_market_additionality_verification, empirical, 'Carbon offset additionality verification capability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carbon_externality_pricing, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carbon_tr_t0, carbon_externality_pricing, theater_ratio, 0, 0.42).
narrative_ontology:measurement(carbon_tr_t10, carbon_externality_pricing, theater_ratio, 10, 0.52).
narrative_ontology:measurement(carbon_tr_t20, carbon_externality_pricing, theater_ratio, 20, 0.58).
narrative_ontology:measurement(carbon_tr_t30, carbon_externality_pricing, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(carbon_be_t0, carbon_externality_pricing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(carbon_be_t10, carbon_externality_pricing, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(carbon_be_t20, carbon_externality_pricing, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(carbon_be_t30, carbon_externality_pricing, base_extractiveness, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carbon_externality_pricing, resource_allocation).
narrative_ontology:boltzmann_floor_override(carbon_externality_pricing, 0.18).
narrative_ontology:affects_constraint(carbon_externality_pricing, renewable_energy_subsidy_dependence).
narrative_ontology:affects_constraint(carbon_externality_pricing, fossil_fuel_stranded_assets).
narrative_ontology:affects_constraint(carbon_externality_pricing, developing_economy_debt_climate_conditionality).

% DUAL FORMULATION NOTE:
% Carbon externality pricing decomposes into at least three structurally distinct constraints: (1) The coordination problem of global emissions reduction (pure coordination, Rope-like); (2) The distributional asymmetry in cost imposition (pure extraction, Snare-like); (3) The offset market theater (degraded verification, Piton-like). These are presented as one story because they operate within the same institutional architecture, but the ε-invariance principle suggests potential future decomposition into separate stories as carbon pricing mechanisms bifurcate (e.g., developed-economy pricing vs. developing-economy climate finance mechanisms). Currently integrated into one Tangled Rope story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(carbon_externality_pricing, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
