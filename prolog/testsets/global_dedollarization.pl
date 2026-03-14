% ============================================================================
% CONSTRAINT STORY: global_dedollarization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_dedollarization, []).

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
 *   constraint_id: global_dedollarization
 *   human_readable: Global Dedollarization: Monetary Hegemony and Coordination Failure
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Global dedollarization represents a fundamental structural tension
 *   between the coordination benefits of a universal reserve currency and the
 *   extractive costs imposed on non-issuing nations. The dollar's role as
 *   global settlement medium enables multi-party trade, commodity pricing
 *   stability, and liquidity provision — genuine coordination functions.
 *   Simultaneously, the dollar constraint extracts through seigniorage (US
 *   gets free purchasing power from dollar creation), coercive monetary
 *   policy (US interest rate shocks force global deleveraging), sanctions
 *   enforcement (dollar payment dominance enables financial warfare), and
 *   capital controls that trap peripheral economies in dollar-denominated
 *   debt. This constraint exhibits all six DR types across different
 *   institutional positions. The same structural phenomenon — dollar
 *   dominance in global settlement — appears as inevitable natural law
 *   (mountain), pure extraction (snare), mixed coordination-extraction
 *   (tangled rope), temporary scaffolding (scaffold), degraded ritual
 *   (piton), or genuine coordination (rope), depending on the observer's
 *   structural position and time horizon. The theater ratio (0.58) reflects
 *   that dedollarization rhetoric often exceeds actual substitution: BRICS
 *   currencies settle only 5-10% of intra-bloc trade; CBDCs remain national
 *   and non-interoperable; most alternative payment systems are theater
 *   signaling independence while relying on dollar infrastructure for final
 *   settlement. The extractiveness trajectory (0.35→0.58 over interval) shows
 *   that as dedollarization accelerates, the US enforces constraint more
 *   visibly through sanctions, secondary penalties, and capital controls,
 *   increasing measured extraction.
 *
 * KEY AGENTS:
 *   - Peripheral Economies: Primary victim (powerless/trapped) — dependent on dollar reserves, facing currency risk and capital control traps; no exit option without geopolitical cost
 *   - BRICS Coalition: Secondary beneficiary and victim (organized/constrained) — builds parallel payment infrastructure but faces sanctions risk and capital flight penalties; mixed position
 *   - US Monetary Hegemony: Primary beneficiary (institutional/arbitrage) — captures seigniorage, capital inflows, and geopolitical leverage; experiences constraint as coordination
 *   - Non-Aligned Middle Powers: Secondary victim and beneficiary (institutional/constrained) — coordinate regional trade in local currencies while maintaining dollar reserves; moderate mixed experience
 *   - International Institutions (IMF/World Bank): Institutional enforcer (institutional/arbitrage) — maintain dollar hegemony through conditionality; act as degraded vestiges (piton)
 *   - Dedollarization Technology Stack: Organizational agent (organized/constrained) — builds temporary scaffolds (CBDCs, mBridge, alternative settlement) with sunset logic as interoperability matures
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as hegemonic laws of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_dedollarization, 0.58).
domain_priors:suppression_score(global_dedollarization, 0.65).
domain_priors:theater_ratio(global_dedollarization, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_dedollarization, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_dedollarization, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(global_dedollarization, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_dedollarization, tangled_rope).
narrative_ontology:human_readable(global_dedollarization, "Global Dedollarization: Monetary Hegemony and Coordination Failure").
narrative_ontology:topic_domain(global_dedollarization, "geopolitical/economic").

domain_priors:requires_active_enforcement(global_dedollarization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_dedollarization, dollar_reserve_currency_system).
narrative_ontology:constraint_beneficiary(global_dedollarization, us_financial_institutions).
narrative_ontology:constraint_beneficiary(global_dedollarization, petrostate_exporters).
narrative_ontology:constraint_victim(global_dedollarization, peripheral_economies).
narrative_ontology:constraint_victim(global_dedollarization, non_reserve_currency_issuers).
narrative_ontology:constraint_victim(global_dedollarization, alternative_payment_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL ECONOMY (SNARE) — Small nations dependent on dollar reserves for imports, trade finance, and inflation protection face no genuine exit. Switching to alternative currencies (CNY, INR, BRICS) carries currency risk, capital controls risk, and geopolitical retaliation risk. The dollar constraint extracts through seigniorage, dollar-denominated debt servicing, and forced reserves holdings. No coordination benefit flows to peripheral economies; the extraction is pure.
constraint_indexing:constraint_classification(global_dedollarization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: BRICS COALITION (TANGLED ROPE) — Organized alternative (Brazil, Russia, India, China, South Africa) seeks to build parallel payment infrastructure (mBridge, BRICS settlement, local currency trade). Coordination genuine: reduces transaction costs and FX volatility within bloc. Extraction present: enforced adoption of blocs' political preferences, exclusion from dollar-denominated liquidity, capital flight penalties. Coalition has agency but faces high exit costs into dollar system and sanctions risks. Mixed classification reflects genuine coordination of non-dollar settlement alongside asymmetric enforcement of political alignment.
constraint_indexing:constraint_classification(global_dedollarization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US MONETARY HEGEMONY (ROPE) — The dollar-centric system coordinates global trade, finance, and commodity pricing. All states benefit from the liquidity, price stability, and settlement infrastructure the dollar provides. The system is self-reinforcing: demand for dollar reserves increases dollar value, which increases demand. US institutions (Federal Reserve, Treasury, Wall Street banks) experience no extraction — they are net beneficiaries. This perspective sees the constraint as pure coordination.
constraint_indexing:constraint_classification(global_dedollarization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NON-ALIGNED MIDDLE POWERS (TANGLED ROPE) — India, Indonesia, Vietnam, Mexico coordinate trade in local currencies (rupee settlement, bilateral arrangements). Genuine coordination benefit: reduced FX volatility, faster settlement, political autonomy. Extraction present: sanctions risk if too visibly dedollarizing, capital controls if dollar outflows spike, exclusion from SWIFT. These actors have more exit capacity than peripheral economies but less than US; constrained rather than trapped. Mixed experience of coordination gain and extraction cost.
constraint_indexing:constraint_classification(global_dedollarization, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL INSTITUTIONS (PITON) — IMF and World Bank mandate dollar-denominated lending and reserve holdings. Functionally, these institutions are vestiges of Bretton Woods that have lost their original coordination purpose but persist through inertia. They enforce dollar hegemony through conditionality and structural adjustment, not because dollar dominance solves the coordination problem (it doesn't — it creates the problem), but because institutional survival depends on maintaining the system they were created to manage. Theater ratio high: the institutions perform technical competence while actually policing monetary conformity. Extraction mechanism relies on theatrical legitimacy.
constraint_indexing:constraint_classification(global_dedollarization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEDOLLARIZATION TECHNOLOGY STACK (SCAFFOLD) — Blockchain payment systems, CIPS (China International Payments System), mBridge, central bank digital currencies (CBDCs), alternative settlement networks represent temporary coordination scaffolds. These systems have genuine coordination function: enable non-dollar trade settlement, reduce intermediation costs, lower transaction times. Extraction present through network lock-in and political conditionality, but the systems are designed with sunset logic — as CBDCs mature and interoperability standards solidify, the temporary scaffolding becomes permanent infrastructure. Theater moderate: these systems are functionally novel but contain elements of PR messaging about independence.
constraint_indexing:constraint_classification(global_dedollarization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: HEGEMONIC STABILITY THEOREM / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the constraint appears as an immutable law of international political economy: dominant powers always impose their currency standard, and competing powers cannot overthrow hegemony without war or global collapse. This perspective frames dedollarization attempts as inevitable failures because the underlying power distribution (US military, capital accumulation, institutional capacity) is unchangeable. However, the structural data contradicts this classification — the engine will compute this as a false summit, revealing that 'monetary hegemony is inevitable' naturalizes contingent institutional arrangements (US capital controls, SWIFT dominance, sanctions enforcement mechanisms) that are not laws of physics but political choices.
constraint_indexing:constraint_classification(global_dedollarization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_dedollarization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_dedollarization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_dedollarization, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_dedollarization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_dedollarization, TR),
    TR >= 0.70.

:- end_tests(global_dedollarization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The dollar constraint extracts through multiple channels: seigniorage (US gets real purchasing power from dollar creation), coercive monetary policy (US rate shocks force global deleveraging disproportionately impacting non-dollar economies), sanctions enforcement (dollar payment dominance enables selective financial exclusion), and capital control traps (peripheral economies must hold dollar reserves to prevent currency collapse, reducing capital available for local investment). However, extraction is not maximal because the dollar system provides genuine liquidity and settlement services that periphery cannot fully replace. Suppression (0.65): Moderate-high. Barriers to exit include currency risk (switching to alternative currency increases FX volatility), capital controls (central banks must maintain dollar reserves as shock absorber), sanctions risk (overt dedollarization invites secondary sanctions), technological lock-in (dollar infrastructure dominates global settlement networks — SWIFT, Eurodollar markets, Treasury bond markets), and geopolitical retaliation (US has used dollar dominance as a lever to enforce policy compliance on sanctions targets and non-aligned states). Theater ratio (0.58): Moderate-high. Dedollarization messaging significantly exceeds actual substitution: BRICS currencies settle only 5-10% of intra-bloc trade despite rhetoric; CBDCs are designed as national systems, not interoperable infrastructure; alternative payment networks (mBridge, CIPS) are partial and politically gated. Much of the theater reflects institutional signaling of independence while actual settlement remains dollar-denominated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates stark perspectival disagreement. US institutions perceive coordination; peripheral economies perceive extraction. BRICS perceives mixed coordination-extraction with increasing enforcement risk. Non-aligned powers perceive moderate mixed experience with growing options. International institutions perceive their own function as coordination while structural evidence shows degradation (piton). The gap reflects real structural differences: agents occupying different positions in the extraction flow literally experience different constraints. A beneficiary with exit options experiences the system as coordination; a victim without exit experiences extraction. The engine's task is not to resolve which perspective is 'correct' — the framework's fundamental insight is that all perspectives are structurally accurate from their position. The gap is the data.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural relationship to extraction flow. The US (beneficiary + arbitrage exit) experiences low or negative d → low or negative χ → experiences constraint as rope. BRICS (organized agents building alternatives + constrained exit through sanctions) derive d around 0.40-0.55 → moderate χ → experience mixed coordination-extraction. Peripheral economies (victims + trapped exit) derive high d → high χ → experience extraction as snare. Non-aligned middle powers (partial beneficiaries + constrained exit) derive moderate d → moderate χ → experience tangled rope. International institutions (formally beneficiary through mandate but functionally captured by arrangement they enforce) derive d around 0.15-0.25, but override applies: they are partly victims of their own institutional obsolescence, so d_override upward to 0.35-0.40, producing piton classification reflecting degradation. The pipeline computes scope modifier σ(S) = 1.2 for global scope, raising effective extraction χ for all agents. The formula χ = ε × f(d) × σ(S) produces the highest χ for trapped peripheral agents (d≈0.95, f(d)≈1.42, σ≈1.2), yielding χ≈0.98, consistent with snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint avoids mandatrophy by declaring all three required elements: (1) beneficiaries = dollar_reserve_currency_system, us_financial_institutions, petrostate_exporters (coordination function is providing universal liquidity and settlement infrastructure); (2) victims = peripheral_economies, non_reserve_currency_issuers, alternative_payment_systems (extraction occurs through seigniorage, coercive monetary policy, and sanctions enforcement); (3) requires_active_enforcement = true (the constraint persists only through continuous enforcement of dollar dominance through capital controls, sanctions, and institutional pressure — if enforcement relaxed, dedollarization would accelerate). The tangled rope classification is not a default between rope and snare but a structural hybrid: the constraint simultaneously solves a coordination problem (global settlement) AND extracts asymmetrically (peripheral economies bear disproportionate costs). Removing the coordination function would leave only snare; removing the extraction would leave only rope. The mandatrophy is resolved by showing that both functions are empirically present and functionally coupled. The analytical observer's mountain perspective fails mandatrophy — it claims inevitability without explaining the mechanism by which inevitability is enforced. The engine detects false summit: the 'natural law' of hegemonic stability is actually a political choice to maintain enforcement, not a law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    brics_currency_credibility,
    'Can BRICS currencies (CNY, INR, BRL) achieve reserve currency status and settlement liquidity comparable to dollar without backing by US capital markets?',
    'Longitudinal tracking of currency volatility, capital flight rates, real interest rates, and settlement velocity in BRICS trade corridors; comparison to dollar-denominated trade metrics',
    'If credible: dedollarization scaffolds become permanent infrastructure (tangled rope → rope equilibrium). If not credible: BRICS system remains constrained alternative with high extraction costs (tangled rope → snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brics_currency_credibility, empirical, 'Whether alternative reserve currencies can achieve dollar-comparable liquidity and stability').

omega_variable(
    sanctions_enforcement_capacity,
    'As dedollarization progresses, can secondary sanctions on non-compliant nations and institutions enforce dollar hegemony, or does sanctions fatigue and sanctions evasion tech reduce enforcement capacity?',
    'Empirical tracking of sanctions evasion success rates; correlation between sanctions tightness and actual capital flow substitution; measurement of secondary sanctions effectiveness vs evasion vectors (crypto, hawala, barter, commodity swaps)',
    'If enforcement capacity declines: suppression metric drops, constraint reclassifies from snare to tangled rope for peripheral economies. If enforcement capacity holds: suppression stays high and constraint remains snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_enforcement_capacity, empirical, 'Whether secondary sanctions can sustain dollar hegemony enforcement').

omega_variable(
    cbdc_interoperability_standard,
    'Will central bank digital currencies (CBDCs) interoperate freely across national boundaries, or will each CBDC be siloed behind capital controls and political gatekeeping?',
    'Technical analysis of CBDC protocols; measurement of cross-border CBDC settlement volumes and speed; assessment of whether CBDCs reduce or reinforce national monetary autarky',
    'If interoperable: CBDCs become genuine dedollarization infrastructure with low theater (scaffold sunrise into rope). If siloed: CBDCs are performative (high theater) and don''t reduce dollar dominance (piton). Constraint reclassifies based on outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cbdc_interoperability_standard, empirical, 'Whether CBDCs will achieve cross-border interoperability or remain national silos').

omega_variable(
    extraction_asymmetry_threshold,
    'At what ratio of dollar-denominated global trade to alternative-currency trade does the extraction mechanism lose coercive force?',
    'Measurement of dedollarization thresholds in different trade corridors; correlation between alternative-currency adoption rates and capital control relaxation; identification of critical mass points where switching becomes self-reinforcing',
    'If threshold < 30%: dedollarization accelerates rapidly after crossing threshold (positive feedback). If threshold > 60%: dedollarization stalls before achieving escape velocity. Threshold level determines whether the constraint''s extractiveness increases (reclassification to snare) or decreases (reclassification to rope/scaffold).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_asymmetry_threshold, empirical, 'Critical mass threshold for self-reinforcing dedollarization').

omega_variable(
    hegemonic_stability_falsifiability,
    'Can the mountain perspective''s ''inevitable hegemony'' claim be falsified, or is it a theological statement immune to empirical test?',
    'Logical analysis of hegemonic stability theorem''s scope conditions; identification of predictions that would contradict the theory; historical comparison to earlier currency transitions (sterling→dollar, gold standard→fiat); assessment of whether sustained >50% non-dollar trade is compatible with theorem',
    'If falsifiable: the mountain classification is a false summit (naturalization of contingent arrangements) and the analytical perspective drops to rope/tangled rope. If unfalsifiable: the theorem is theological and the mountain classification is misapplied — reclassify to piton (performative theory). Either way, this omega resolves the hegemonic stability claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hegemonic_stability_falsifiability, conceptual, 'Whether hegemonic stability theorem is empirically falsifiable or theological').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_dedollarization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(glob_tr_t0, global_dedollarization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(glob_tr_t5, global_dedollarization, theater_ratio, 5, 0.51).
narrative_ontology:measurement(glob_tr_t10, global_dedollarization, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(glob_be_t0, global_dedollarization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(glob_be_t5, global_dedollarization, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(glob_be_t10, global_dedollarization, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_dedollarization, resource_allocation).
narrative_ontology:affects_constraint(global_dedollarization, petrodollar_hegemony).
narrative_ontology:affects_constraint(global_dedollarization, sanctions_enforcement_mechanism).
narrative_ontology:affects_constraint(global_dedollarization, reserve_currency_demand).
narrative_ontology:affects_constraint(global_dedollarization, cbdc_interoperability).

% DUAL FORMULATION NOTE:
% Global dedollarization is upstream of specific geopolitical constraints (petrodollar hegemony, sanctions enforcement) and coupled to technical infrastructure constraints (CBDC interoperability). Each downstream constraint has its own extractiveness value reflecting domain-specific mechanisms; dedollarization represents the integrative structural phenomenon across all.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_dedollarization, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
