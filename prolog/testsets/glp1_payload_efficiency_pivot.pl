% ============================================================================
% CONSTRAINT STORY: glp1_payload_efficiency_pivot
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_glp1_payload_efficiency_pivot, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: glp1_payload_efficiency_pivot
 *   human_readable: GLP-1 Adoption and the Airline Fuel-Weight Constraint
 *   domain: economic/technological
 *
 * SUMMARY:
 *   GLP-1 agonist adoption (semaglutide, tirzepatide, others) has created an
 *   unexpected structural constraint at the intersection of pharmaceutical
 *   markets, healthcare systems, and airline operations. The constraint
 *   emerges because GLP-1-induced weight reduction (30-40 lbs typical, 50+
 *   lbs achievable) reduces aircraft payload weight, improving fuel
 *   efficiency by 4-5% per pound of passenger mass reduction. This creates a
 *   tangled hybrid: pharmaceutical manufacturers benefit from expanded market
 *   demand; airlines benefit from fuel cost reductions and can externalize
 *   the environmental benefit as greenwashing; obese patients experience
 *   genuine health benefits but also face dependency risks and cost barriers;
 *   healthcare systems bear enforcement overhead and cost increases;
 *   lower-income populations experience regressive access barriers as
 *   pharmaceutical budgets expand; and federal regulators face conflicting
 *   mandates around coverage, pricing, and equity. The constraint's
 *   extractiveness (0.38) reflects that GLP-1 is genuinely therapeutic (not
 *   pure extraction), but the pharmaceutical pricing model, patient lock-in
 *   mechanisms, and the naturalizing frame ('airline efficiency is a health
 *   benefit') create asymmetric extraction of healthcare resources. The
 *   theater ratio (0.48) is moderate: the medical community performs genuine
 *   metabolic monitoring and counseling, but the performative content
 *   (framing weight loss as individual responsibility rather than systems
 *   design failure) is also present. The constraint is classified as Tangled
 *   Rope because it possesses both genuine coordination function (helping
 *   people manage weight-related health risks) AND asymmetric extraction
 *   (pharmaceutical capture of healthcare budgets, patient lock-in through
 *   rebound risk, airline benefit externalization). The five perspectives
 *   from different structural positions (patient, insurer, manufacturer,
 *   airline, regulator) reveal the constraint's full complexity: what appears
 *   as coordination from the manufacturer and airline perspective appears as
 *   extraction from the patient and equity advocate perspective. The piton
 *   perspective on metabolic medicine institutional inertia reveals that
 *   GLP-1 adoption may be substituting for more fundamental food system and
 *   activity environment design changes. The analytical observer's natural
 *   law hypothesis is rejected as a false summit: obesity is not an immutable
 *   physical constraint but a contingent product of the current food and
 *   economic system.
 *
 * KEY AGENTS:
 *   - Pharmaceutical manufacturers (GLP-1 producers): Primary beneficiary (institutional/arbitrage) — expanded market, premium pricing, patent protection, externality capture from airline efficiency gains
 *   - Obese patient populations: Primary victim (powerless/trapped) — genuine health benefits but also dependency risk (rebound weight gain), cost barriers, social pressure, entrance into long-term pharmaceutical dependency
 *   - Airlines and air cargo operators: Secondary beneficiary (organized/mobile) — fuel efficiency gains, operational cost reductions, greenwashing benefit without active suppression enforcement
 *   - Insurance companies and healthcare systems: Constrained beneficiary (moderate/constrained) — reduced long-term obesity-related claims but increased short-term GLP-1 budget and enforcement overhead
 *   - Federal regulators (FDA, CMS, HHS): Powerful but conflicted (powerful/mobile) — mandate to approve/cover while managing pharmaceutical cost inflation and equity concerns
 *   - Health equity coalitions and lower-income populations: Victims of regressive transfer (moderate/constrained, powerless/trapped) — bear obesity health burden but face access barriers to GLP-1; healthcare budgets shift to pharmaceutical rather than public health
 *   - Metabolic medicine institutional establishment: Perpetuators of piton constraint (institutional/arbitrage) — maintain therapeutic dependency framing despite systems design alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(glp1_payload_efficiency_pivot, 0.38).
domain_priors:suppression_score(glp1_payload_efficiency_pivot, 0.42).
domain_priors:theater_ratio(glp1_payload_efficiency_pivot, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(glp1_payload_efficiency_pivot, extractiveness, 0.38).
narrative_ontology:constraint_metric(glp1_payload_efficiency_pivot, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(glp1_payload_efficiency_pivot, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(glp1_payload_efficiency_pivot, tangled_rope).
narrative_ontology:human_readable(glp1_payload_efficiency_pivot, "GLP-1 Adoption and the Airline Fuel-Weight Constraint").
narrative_ontology:topic_domain(glp1_payload_efficiency_pivot, "economic/technological").

domain_priors:requires_active_enforcement(glp1_payload_efficiency_pivot).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(glp1_payload_efficiency_pivot, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(glp1_payload_efficiency_pivot, airline_fuel_efficiency_advocates).
narrative_ontology:constraint_victim(glp1_payload_efficiency_pivot, obese_patient_populations).
narrative_ontology:constraint_victim(glp1_payload_efficiency_pivot, healthcare_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBESE PATIENT (SNARE) — Trapped within a healthcare market where GLP-1 agonist adoption creates conflicting extraction pressures. Weight loss itself is presented as liberation, but the constraint operates through medical necessity framing: patients cannot easily exit GLP-1 dependency once started (biological tolerance, rebound weight gain risk, social expectation shift). The fuel-efficiency framing naturalizes pharmaceutical extraction as an externality of health improvement, hiding the asymmetry: patients bear biological and financial costs, while airline profit margins capture the benefit. No alternative coordinate system available — either take the drug or accept social stigma and metabolic disadvantage.
constraint_indexing:constraint_classification(glp1_payload_efficiency_pivot, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSURANCE PROVIDERS (TANGLED ROPE) — Constrained by actuarial necessity to cover GLP-1 (obesity-related costs exceed GLP-1 costs over 5-year horizon) but also benefit from reduced claims for diabetes, cardiovascular disease, and joint stress. However, they bear enforcement overhead: prior authorization, adherence monitoring, rebate negotiation with manufacturers. Suppression is high (insurers cannot refuse coverage without reputational risk and regulatory pressure), but coordination function exists (GLP-1 does reduce long-term claims). Mixed extraction and benefit.
constraint_indexing:constraint_classification(glp1_payload_efficiency_pivot, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GLP-1 MANUFACTURERS (ROPE) — Institutional beneficiary with arbitrage options. Manufactures face minimal suppression: regulatory approval is standard pathway, patent protection provides monopoly pricing window, and they control supply. Benefits from both obesity treatment market AND emerging market segmentation (airline weight optimization). They experience the constraint as pure coordination: the market they are coordinating is weight reduction as a commercial product. Suppression is not their constraint — it is the patient's and insurer's constraint. From manufacturer perspective, this is low-extraction rope: they are solving a coordination problem (connecting people with weight loss tools) that clients actually want.
constraint_indexing:constraint_classification(glp1_payload_efficiency_pivot, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: AIRLINES (ROPE) — Organized actors (major carriers, cargo operators) benefit from reduced payload weight: ~4-5% fuel efficiency gain per passenger weight reduction of 30-40 lbs is economically significant at scale. Airlines have some exit options (dynamic pricing for heavier passengers, cargo weight fees, alternative fuel investments) but GLP-1 adoption provides a 'natural' efficiency channel they do not need to actively enforce. Benefits from coordination (weight reduction) without visible suppression — the pharmaceutical system bears the suppression, not them. This looks like free-rider benefit on an externality. Low suppression from airline perspective makes this rope, not tangled rope.
constraint_indexing:constraint_classification(glp1_payload_efficiency_pivot, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL REGULATORS (TANGLED ROPE) — Powerful agents (FDA, CMS, HHS) face conflicting mandates: approve/cover GLP-1 as obesity treatment (which reduces downstream public health costs), but also manage pharmaceutical cost inflation and off-label usage spiraling. They benefit from the efficiency gains (reduced Medicare/Medicaid burden for obesity-related comorbidities), but also bear enforcement costs (monitoring for unapproved uses, managing equity concerns, pricing negotiations). They have exit options (restrict coverage, cap pricing, mandate generic competition) but using them risks political backlash from patients and manufacturers. Significant extraction of regulatory labor without full authority — tangled rope signature.
constraint_indexing:constraint_classification(glp1_payload_efficiency_pivot, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: HEALTH EQUITY ADVOCATES (SCAFFOLD) — Organized but constrained actors (patient advocacy groups, public health NGOs, equity-focused researchers) see the constraint as temporary: the current pricing and access model for GLP-1 creates a 'pharmaceutical lottery' where wealthy, insured patients get weight loss benefits while lower-income populations bear the health burden. This is framed as solvable through generic competition (expected 2028-2030), price regulation, or public manufacturing. The coalition views GLP-1 adoption as a scaffold: a temporary extraction phase before alternative models mature. Suppression is high now (access barriers, cost), but sunset is structural (patent cliffs, manufacturing scale). Theater is moderate (the equity framing is genuine, not performative).
constraint_indexing:constraint_classification(glp1_payload_efficiency_pivot, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: HISTORICAL INSTITUTIONAL VIEW (PITON) — From a civilizational scale, the BMI-centered metabolic medicine paradigm is itself a degraded constraint. Earlier caloric restriction models, food system engineering approaches, and behavioral intervention infrastructure have atrophied or been abandoned. GLP-1 has been reinvented as a pharmaceutical solution to a problem that was partly a systems-design failure. The therapeutic system maintains the piton through institutional inertia: medical training still emphasizes willpower/diet despite metabolic complexity; public health funding emphasizes individual behavior change over food system regulation; pharmaceutical marketing actively maintains the therapeutic dependency frame. Theater is high: the medical community performs intensive counseling and monitoring while the actual effective mechanism (GLP-1 chemistry) carries the load. Theaters ratio 0.65+. This is a piton perspective on a constraint that looks like 'obesity treatment' at the surface but is actually 'metabolic pharmaceutical dependency maintenance' at the structural level.
constraint_indexing:constraint_classification(glp1_payload_efficiency_pivot, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL / NATURAL LAW HYPOTHESIS (MOUNTAIN) — This perspective claims that obesity-induced weight is a fundamental physical constraint on aircraft efficiency: heavier payloads require more fuel, and human adiposity is structurally entangled with metabolic necessity. From this view, GLP-1 adoption is not a constraint at all but a natural correction mechanism — humans optimizing body weight in response to environmental (fuel cost) and social (airline efficiency premium) signals. The mountain hypothesis says: pharmacological weight optimization is an immutable feature of rational resource allocation once the health trade-offs are calibrated. However, the structural data contradicts this naturalization. The accessibility_collapse (humans cannot easily exit adiposity without pharmaceutical intervention) and resistance (strong cultural, metabolic, and economic counterpressures) would require accessibility_collapse ≥ 0.85 and resistance ≤ 0.15 for true mountain classification. The data shows accessibility_collapse ≈ 0.65 and resistance ≈ 0.50: people can maintain weight without GLP-1 through alternative means, and substantial populations resist the pharmaceutical framing. This is a FALSE SUMMIT — the 'immutable law' framing naturalizes what is a contingent market structure.
constraint_indexing:constraint_classification(glp1_payload_efficiency_pivot, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(glp1_payload_efficiency_pivot_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(glp1_payload_efficiency_pivot, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(glp1_payload_efficiency_pivot, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(glp1_payload_efficiency_pivot, TR),
    TR >= 0.70.

:- end_tests(glp1_payload_efficiency_pivot_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. GLP-1 is not pure extraction — it provides genuine therapeutic benefit and reduces downstream obesity-related healthcare costs, creating real coordination value. However, the current pharmaceutical pricing model captures 60-70% of the cost reduction as profit; patient lock-in through rebound weight gain risk creates dependency; and the airline efficiency externality is captured by carriers without being reflected in patient access. The 0.38 value reflects that this is mixed: ~30% genuine coordination (weight management + health outcomes), ~38% pharmaceutical extraction (pricing power, market expansion, locked dependency), ~32% efficiency externality capture (airline benefit without patient compensation). If GLP-1 were priced at cost + reasonable margin, extractiveness would be 0.12-0.15. The current market structure drives extractiveness to 0.38. Suppression (0.42): Moderate. Barriers include: (a) cost ($300-1200/month without insurance; insurance requires prior authorization and income limits), (b) biological lock-in (rebound weight gain if discontinued), (c) social pressure (weight loss is framed as individual moral responsibility, not systemic change), (d) alternative path opacity (caloric restriction without pharmaceutical intervention is presented as impossible, not difficult). Suppression is not total — some patients access via insurance, some manage weight through alternative means — but significant. Theater ratio (0.48): Moderate. The medical system performs genuine metabolic monitoring and outcome tracking. However, the theatrical content includes: (a) framing weight loss as individual willpower/medication adherence rather than systems change, (b) obscuring that the same population could achieve comparable outcomes through food environment changes + activity design, (c) naturalizing pharmaceutical dependency as health optimization rather than market structure choice, (d) the emerging 'airline efficiency' narrative that externalizes the pharmaceutical extraction into an environmental benefit. The theater has been rising (measurement shows 0.35→0.52) as GLP-1 adoption creates more visible 'success stories,' increasing the social pressure frame.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. From the pharmaceutical manufacturer's institutional perspective with arbitrage options, GLP-1 is pure rope: they are solving a genuine market coordination problem (connecting people with weight management tools) and benefiting from it. From the patient's powerless/trapped perspective, it is snare: they face genuine health costs (rebound risk, lock-in, side effects), cost barriers, social pressure, and no exit option that doesn't involve reaccepting obesity health burden. From the airline perspective with mobile options, it is rope with zero enforcement cost: they get fuel savings without needing to enforce weight standards or create dynamic pricing. From the insurer's constrained position, it is tangled rope: they benefit from reduced long-term obesity claims but bear enforcement overhead (prior auth, adherence monitoring, rebate negotiation) and cannot refuse coverage without reputational damage. From the federal regulator's powerful but conflicted position, it is also tangled rope but inverted: they benefit from reduced public health costs but face enforcement mandates (monitoring for off-label use, managing equity, price negotiations) without clear authority to resolve the tensions. From the health equity coalition's organized but constrained position, it is scaffold: the current pricing creates a temporary extraction phase before generics arrive (patent cliff 2028-2029), at which point the constraint sunsets and extractiveness drops. From the metabolic medicine historical perspective, it is piton: the institutional system maintains a therapeutic dependency frame even though the problem was partly a systems design failure (food environment, activity patterns). From the analytical observer's civilizational perspective, the natural law hypothesis (obesity is immutable without pharmaceutical correction) is false: accessibility_collapse is 0.65 (people can maintain weight through alternative means), and resistance is 0.50 (substantial populations resist the pharmaceutical framing). The perspectival gap reveals that the same constraint appears as coordination (rope) from one view, pure extraction (snare) from another, temporary extraction with sunset (scaffold) from a third, and degraded institutional theater (piton) from a fourth. This is the core diagnostic value: no single 'true' classification exists. The presheaf of perspectives IS the full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) and experienced effective extractiveness (χ) flow from the agent's structural position relative to the extraction mechanism and their exit options. Pharmaceutical manufacturers (institutional/arbitrage): d ≈ 0.05 (full beneficiary, control supply and pricing, arbitrage options to other markets), f(d) ≈ -0.12, χ ≈ -0.12×0.38×1.0 = negative (they are net extractors, not victims). Obese patients (powerless/trapped): d ≈ 0.95 (full target, high health/financial cost, no exit without accepting obesity burden), f(d) ≈ 1.42, χ ≈ 1.42×0.38×1.0 = 0.54 (maximum experienced extraction). Airlines (organized/mobile): d ≈ 0.35 (indirect beneficiary through fuel savings, mobile options to alternative efficiency mechanisms), f(d) ≈ 0.30, χ ≈ 0.30×0.38×1.0 = 0.11 (minimal experienced extraction, they benefit from externality). Insurers (moderate/constrained): d ≈ 0.55 (mixed: benefit from reduced long-term claims, but constrained by coverage mandates and enforcement overhead), f(d) ≈ 0.75, χ ≈ 0.75×0.38×1.0 = 0.285 (moderate extraction). Federal regulators (powerful/mobile): d ≈ 0.50 (symmetric: benefit from public health improvements, but bear regulatory enforcement costs), f(d) ≈ 0.65, χ ≈ 0.65×0.38×1.0 = 0.247 (moderate). Health equity coalitions (organized/constrained): d ≈ 0.70 (partly victims of regressive access, but also advocating for change), f(d) ≈ 1.10, χ ≈ 1.10×0.38×1.0 = 0.418 (significant extraction, but constrained exit improves timeline visibility). The directionality values are not overridden: the beneficiary/victim declarations (manufacturers as beneficiary, patients as victims, insurers as mixed, regulators as mixed) generate these d values through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy through temporal decomposition and perspectival heterogeneity. The constraint is NOT a false mislabeling of coordination as extraction. GLP-1 adoption genuinely combines both. The pharmaceutical system provides coordination value: it connects people with a tool for weight management that has real health benefits. But it is also genuinely extractive: the pricing model captures rents, the lock-in mechanism (rebound risk) reduces exit options, and the airline efficiency externality is captured without patient benefit sharing. The constraint is Tangled Rope at the base level (ε=0.38, suppression=0.42, requires_active_enforcement=true, beneficiaries=[manufacturers, airlines], victims=[patients, low-income populations]). However, the temporal dimension resolves potential mandatrophy: by 2028-2029 (omega: generic competition timing), the constraint may transition from Tangled Rope to Rope if generic competition arrives on schedule. Alternatively, if pharmaceutical companies successfully extend monopoly through formulation changes or regulatory extensions, the constraint will remain Tangled Rope or worsen toward Snare. The equity coalition's scaffold perspective is legitimate only if the patent cliff actually produces generic entry; if it doesn't, the scaffold is aspirational theater and the true constraint is snare. The four omegas are constructed to resolve this mandatrophy: rebound_weight_gain_risk determines whether patients are genuinely trapped (d→1.0, snare) or merely constrained (d→0.65, tangled rope); airline_fuel_savings_attribution determines whether the airline benefit is significant (rope confirmed) or negligible (constraint is patient-centric); generic_competition_timing determines whether the constraint sunsets (scaffold confirmed) or persists (snare is permanent); healthcare_equity_moral_hazard determines whether the framing naturalizes regressive transfer (validates snare victim classification) or reflects genuine health coordination. Collectively, these omegas transform the mandatrophy question from 'Is this rope or snare?' to 'Under which structural trajectories does each classification obtain?' The analytical observer's false summit reveals that the natural law hypothesis (obesity requires pharmaceutical correction) is contingent on market structure, not immutable. The constraint is Tangled Rope in current form (2024-2026), with conditional pathways to Rope (if generics arrive), Snare (if monopoly extends and rebound risk is high), or Scaffold-with-sunset (if equity coalitions successfully push generic entry + price regulation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rebound_weight_gain_risk,
    'What fraction of GLP-1 patients experience rebound weight gain > 30% after discontinuation, and does this lock-in constitute genuine dependency or reversible adaptation?',
    'Long-term follow-up data from GLP-1 discontinuation trials; metabolic mechanism analysis (glucagon-like peptide signaling adaptation vs permanent metabolic change)',
    'If rebound > 60% and permanent: patients are genuinely trapped (d→1.0, snare confirmed). If rebound < 30% and reversible: exit options improve (d→0.65, constraining to mobile, tangled rope weakens toward rope). If heterogeneous by phenotype: classification is subpopulation-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebound_weight_gain_risk, empirical, 'Magnitude and reversibility of rebound weight gain after GLP-1 discontinuation').

omega_variable(
    airline_fuel_savings_attribution,
    'What fraction of airline fuel savings are actually attributable to population-level GLP-1 adoption vs. other efficiency mechanisms (newer aircraft, operational changes, pricing models)?',
    'Econometric decomposition of fuel consumption 2024-2030; counterfactual fuel projections excluding GLP-1 adoption; airline efficiency gain attribution analysis',
    'If GLP-1 drives > 40% of savings: airline beneficiary classification strengthens (rope confirmed, high dependency). If < 10%: airline benefit is noise (rope weakens, constraint becomes primarily patient-centric). If heterogeneous by carrier: some airlines exploit, others ignore (perspectival gap widens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(airline_fuel_savings_attribution, empirical, 'Quantified contribution of GLP-1 adoption to airline fuel efficiency gains').

omega_variable(
    generic_competition_timing,
    'Will generic GLP-1 competition arrive by 2028-2029 (patent cliff) or will pharmaceutical companies successfully extend monopoly through formulation changes, combination therapies, or regulatory extensions?',
    'Patent landscape analysis; FDA approvals for generic applications; pricing trends 2026-2030; regulatory intervention pressure',
    'If generics arrive on schedule: scaffold sunset is real, extractiveness drops to 0.15-0.20 by 2030 (snare→rope transition). If monopoly extends: extractiveness remains 0.38+ through 2030 (snare persists, scaffold is aspirational). This is the single largest determinant of whether extraction is temporary or structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generic_competition_timing, empirical, 'Timeline for generic GLP-1 market entry and pharmaceutical monopoly duration').

omega_variable(
    healthcare_equity_moral_hazard,
    'Does framing GLP-1 weight loss as an airline efficiency benefit (carbon footprint reduction, fuel cost externality) obscure pharmaceutical extraction of healthcare resources from lower-income populations who do not fly regularly?',
    'Demographic analysis of GLP-1 access by income/insurance status; airline passenger demographic vs obesity prevalence correlation; healthcare budget allocation shift pre/post GLP-1 adoption',
    'If yes: the constraint embeds a regressive transfer (public health resources → airlines'' efficiency gains + pharma profit, bypassing those with greatest weight-related health burden). This validates snare classification for low-income patients (victims) and reveals the ''fuel efficiency'' framing as a theatrical justification for pharmaceutical extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(healthcare_equity_moral_hazard, conceptual, 'Whether GLP-1 adoption creates a regressive transfer from healthcare equity to airline efficiency').

omega_variable(
    metabolic_science_vs_pharmaceutical_framing,
    'Is obesity fundamentally a metabolic disorder (biological constraint requiring pharmaceutical correction) or a systems design failure (food environment, activity patterns, economic incentives) that happens to be pharmacologically addressable?',
    'Historical analysis of obesity etiology research funding; comparison of pharmaceutical vs public health intervention efficacy; metabolic mechanism studies establishing whether GLP-1 ''fixes'' an underlying disorder or bypasses systemic design',
    'If metabolic disorder: GLP-1 is treating a disease, snare classification is inappropriate (patients are victims of biology, not institutional extraction). If systems failure: GLP-1 is a band-aid that perpetuates the design failure, snare classification is appropriate (patients are victims of institutional choices + pharmaceutical capture). This determines the entire moral legitimacy of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metabolic_science_vs_pharmaceutical_framing, conceptual, 'Ontological framing of obesity as metabolic disease vs systems design failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(glp1_payload_efficiency_pivot, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(glp1_tr_t0, glp1_payload_efficiency_pivot, theater_ratio, 0, 0.35).
narrative_ontology:measurement(glp1_tr_t2, glp1_payload_efficiency_pivot, theater_ratio, 2, 0.41).
narrative_ontology:measurement(glp1_tr_t4, glp1_payload_efficiency_pivot, theater_ratio, 4, 0.48).
narrative_ontology:measurement(glp1_tr_t6, glp1_payload_efficiency_pivot, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(glp1_be_t0, glp1_payload_efficiency_pivot, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(glp1_be_t2, glp1_payload_efficiency_pivot, base_extractiveness, 2, 0.33).
narrative_ontology:measurement(glp1_be_t4, glp1_payload_efficiency_pivot, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(glp1_be_t6, glp1_payload_efficiency_pivot, base_extractiveness, 6, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(glp1_payload_efficiency_pivot, resource_allocation).
narrative_ontology:affects_constraint(glp1_payload_efficiency_pivot, pharmaceutical_pricing_monopoly).
narrative_ontology:affects_constraint(glp1_payload_efficiency_pivot, airline_carbon_accountability).
narrative_ontology:affects_constraint(glp1_payload_efficiency_pivot, healthcare_equity_access_disparity).

% DUAL FORMULATION NOTE:
% GLP-1 adoption sits at the intersection of three constraint families: (1) pharmaceutical pricing monopoly (upstream: controls access and price, extractiveness 0.55+); (2) airline operational efficiency (lateral: benefits from weight reduction without enforcement, extractiveness 0.05-0.15); (3) healthcare equity access disparity (downstream: regressive transfer of resources). This story captures the hybrid constraint where GLP-1 is both a genuine therapeutic tool AND a rent-extraction mechanism. The upstream pharmaceutical pricing constraint (higher extractiveness) enables the GLP-1 adoption constraint (moderate extractiveness); the downstream equity constraint (high extractiveness of access barriers) is created by the GLP-1 adoption structure. The network edges capture these dependencies. GLP-1 adoption is NOT a single constraint viewed from different angles; it is a distinct structural phenomenon (the market equilibrium where pharmaceutical companies expand GLP-1 sales by capturing airline efficiency externalities) that sits downstream of pharmaceutical market structure and upstream of healthcare equity outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
