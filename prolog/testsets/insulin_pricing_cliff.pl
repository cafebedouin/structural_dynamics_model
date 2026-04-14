% ============================================================================
% CONSTRAINT STORY: insulin_pricing_cliff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_insulin_pricing_cliff, []).

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
 *   constraint_id: insulin_pricing_cliff
 *   human_readable: Insulin Pricing Cliff: Market Capture and Extraction from Insulin-Dependent Patients
 *   domain: healthcare/pharmaceutical_pricing/metabolic_disease
 *
 * SUMMARY:
 *   The insulin pricing cliff represents a structural extraction mechanism
 *   targeting patients with Type 1 diabetes — a population with zero
 *   negotiating power due to absolute biological dependency. Insulin is not a
 *   discretionary pharmaceutical; it is a metabolic necessity. The constraint
 *   operates through a multi-layered enforcement apparatus: insulin
 *   manufacturers set list prices; pharmacy benefit managers layer rebate
 *   structures and formulary restrictions; insurance companies enforce prior
 *   authorization and coverage denials; primary care physicians are embedded
 *   in systems that reward gatekeeping delays. The patient, biologically
 *   locked into demand, absorbs the extraction. Over the past 16 years, the
 *   extractiveness of the constraint has roughly doubled as manufacturers
 *   have implemented continuous-price-escalation strategies, patent-extension
 *   tactics, and biosimilar pricing that remains 90%+ of branded versions.
 *   The theater ratio reflects the legitimizing narratives that obscure the
 *   underlying extraction: 'market innovation,' 'rebate optimization,'
 *   'formulary management for cost control,' and 'safety monitoring' are all
 *   institutional theaters that maintain the system while extraction flows
 *   outward. The constraint is a canonical Snare: the victims
 *   (insulin-dependent patients) have no exit options, the beneficiaries
 *   (manufacturers and PBMs) have full arbitrage capacity, and the
 *   suppression mechanism is biological necessity backed by institutional
 *   enforcement.
 *
 * KEY AGENTS:
 *   - Insulin-Dependent Patients: Primary victims (powerless/trapped) — biologically locked into absolute demand with no substitutes; cannot negotiate, delay, or refuse treatment; bear maximum extraction through out-of-pocket costs, rationing, and medical complications from coverage denials
 *   - Insulin Manufacturers (Novo Nordisk, Eli Lilly, Sanofi): Primary beneficiaries (institutional/arbitrage) — capture list-price margin and can shift production, adjust pricing by market, or innovate; net beneficiaries with full strategic flexibility
 *   - Pharmacy Benefit Managers (CVS Caremark, Express Scripts, Optum): Secondary beneficiaries (institutional/arbitrage) — capture rebates and formulary placement leverage; have arbitrage options in network design and contracting
 *   - Primary Care Physicians: Secondary actors (moderate/constrained) — constrained by insurance formularies and prior authorization protocols; coordinate care delivery while enforcing extraction through gatekeeping delays
 *   - Patient Advocacy Organizations (JDRF, ADA): Organized agents (organized/constrained) — provide access programs and negotiate discounts but remain structurally embedded in the pricing system they attempt to reform
 *   - FDA Regulatory System: Institutional actor (institutional/arbitrage) — maintains performative oversight (safety certification) while lacking pricing authority; legitimizes the market as 'regulated' without controlling extraction
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes that insulin extraction is fundamentally biological lock masked by market mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(insulin_pricing_cliff, 0.68).
domain_priors:suppression_score(insulin_pricing_cliff, 0.72).
domain_priors:theater_ratio(insulin_pricing_cliff, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(insulin_pricing_cliff, extractiveness, 0.68).
narrative_ontology:constraint_metric(insulin_pricing_cliff, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(insulin_pricing_cliff, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(insulin_pricing_cliff, snare).
narrative_ontology:human_readable(insulin_pricing_cliff, "Insulin Pricing Cliff: Market Capture and Extraction from Insulin-Dependent Patients").
narrative_ontology:topic_domain(insulin_pricing_cliff, "healthcare/pharmaceutical_pricing/metabolic_disease").

domain_priors:requires_active_enforcement(insulin_pricing_cliff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(insulin_pricing_cliff, insulin_manufacturers).
narrative_ontology:constraint_beneficiary(insulin_pricing_cliff, pharmacy_benefit_managers).
narrative_ontology:constraint_victim(insulin_pricing_cliff, insulin_dependent_patients).
narrative_ontology:constraint_victim(insulin_pricing_cliff, type_1_diabetes_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSULIN-DEPENDENT PATIENT (SNARE) — Trapped by biological necessity. Type 1 diabetes requires insulin for survival; there is no substitute, no delay, no negotiation. The constraint extracts maximum rent from a population with zero exit optionality. Patients cannot choose generic alternatives, skip doses without fatal consequence, or delay treatment. Suppression is structural: the biological need cannot be negotiated or waived. This is the perspective of maximum experienced extraction.
constraint_indexing:constraint_classification(insulin_pricing_cliff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIMARY CARE PHYSICIAN (TANGLED ROPE) — Constrained by formulary restrictions and insurance denial-of-coverage protocols but also embedded in a system that generates consistent referral revenue and specialist dependence. Physicians coordinate necessary care while simultaneously enforcing extraction mechanisms through prior authorization delays and formulary gatekeeping. They experience both coordination function (ensuring appropriate insulin selection) and asymmetric extraction (being paid to delay or deny coverage).
constraint_indexing:constraint_classification(insulin_pricing_cliff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSULIN MANUFACTURER (ROPE) — Experiences the constraint as coordination: managing supply, determining dosing protocols, and maintaining market distribution. From the manufacturer's view, high pricing is merely market-clearing and R&D compensation. The constraint coordinates the production and delivery of an essential drug. The manufacturer has arbitrage options: can shift production, adjust market positioning, or innovate. Net beneficiary position; effective extraction runs toward this agent, not away from them.
constraint_indexing:constraint_classification(insulin_pricing_cliff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PATIENT ADVOCACY COALITION (TANGLED ROPE) — Organized agents (JDRF, ADA, diabetes councils) coordinate insulin access programs and negotiate bulk pricing while also being structurally embedded in a system that depends on high prices to justify their interventions. They have constrained exit: can advocate for price controls but cannot exit the market structure they are trying to reform. They both reduce extraction (through access programs) and enforce it (by legitimizing the market as 'working' when supplementary programs exist).
constraint_indexing:constraint_classification(insulin_pricing_cliff, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FDA REGULATORY SYSTEM (PITON) — Maintains formal insulin approval and safety monitoring but is largely performative in pricing oversight. The FDA certifies biological equivalence and manufacturing safety but has no authority over pricing (delegated to 'market mechanisms'). The regulatory theater persists through institutional inertia — FDA presence legitimizes the market as 'regulated' while actual pricing extraction occurs outside FDA jurisdiction. Theater ratio is elevated because compliance with FDA rules (manufacturing standards, adverse event reporting) is mistaken for oversight of market extraction.
constraint_indexing:constraint_classification(insulin_pricing_cliff, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a biological/civilizational view, the insulin extraction mechanism is structurally analogous to other metabolically-driven snares. Type 1 diabetes is an autoimmune condition where the body attacks insulin-producing cells; the constraint is that the destroyed metabolic capacity must be externally sourced and maintained for life. This is not a market mechanism — it is metabolic biology being captured by a market mechanism. The analytical perspective reveals that the snare's foundation is biological necessityaccess, not economic coordination. The constraint's power derives entirely from the fact that insulin is non-negotiable for survival.
constraint_indexing:constraint_classification(insulin_pricing_cliff, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(insulin_pricing_cliff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(insulin_pricing_cliff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(insulin_pricing_cliff, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(insulin_pricing_cliff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(insulin_pricing_cliff, TR),
    TR >= 0.70.

:- end_tests(insulin_pricing_cliff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Insulin list prices have increased 1,200% over the past two decades while manufacturing costs have remained flat. Patient out-of-pocket costs have tripled since 2010. The escalation is not driven by manufacturing innovation (biosynthetic insulin process was perfected in the 1980s) but by market consolidation and pricing strategy. The 0.68 value reflects that the extraction is severe and sustained but not absolute — some patient access occurs through manufacturer assistance programs and state Medicaid coverage, creating a floor. Full extraction would be 0.85+. Suppression (0.72): High. Biological necessity provides absolute suppression: patients cannot refuse insulin without fatal consequence. Insurance barriers add institutional suppression: prior authorization delays, formulary restrictions, and coverage denials. The 0.72 value reflects that suppression operates at both biological and institutional levels. Patients who achieve insurance coverage still face high out-of-pocket costs (copays, deductibles), creating effective suppression through financial constraint. Theater ratio (0.58): Moderate-high. The constraint maintains substantial institutional theater: FDA regulation appears to oversee the market but lacks pricing authority; rebate structures claim to 'optimize costs' while obscuring true pricing; pharmacy benefit managers claim formulary restrictions control costs while actually enabling manufacturer pricing power through formulary placement leverage; manufacturer assistance programs claim to help patients while creating data collection infrastructure that reinforces pricing power. However, the theater is lower than pure pitons because the extraction mechanism is direct and visible — patients and physicians understand that insulin costs are high and rising. The visibility prevents full theater (0.70+). The measurement trajectory shows accelerating extractiveness (0.35→0.68 over 16 years) while theater ratio has remained stable around 0.50, suggesting that the extraction has become more overt rather than more theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full dimensionality of perspectival difference. The insulin manufacturer sees coordination: we develop, manufacture, and distribute an essential drug; we manage supply chains; we conduct clinical research; we maintain manufacturing standards. This is a Rope perspective from the beneficiary position. The patient sees pure extraction: my insulin costs $300/vial, I cannot negotiate, I cannot substitute, I cannot refuse, and my survival depends on payment. This is a Snare perspective from the trapped victim position. The primary care physician sees mixed extraction and coordination: I coordinate appropriate insulin selection based on patient needs AND I enforce insurance gatekeeping that delays patient access. This is a Tangled Rope perspective from the moderate constrained position. The patient advocacy organization sees a temporary problem in need of reform: policy changes (price controls, reference pricing, patent reform) could restructure the market. This is a Scaffold perspective from the organized constrained position. The FDA sees its regulatory function as complete: we approve insulins, monitor safety, enforce manufacturing standards. We do not control pricing — that is market function. This is a Rope perspective from the institutional arbitrage position, but it is performative (Piton) because FDA oversight obscures rather than addresses pricing extraction. The analytical observer sees the structure: insulin extraction is biological lock (Type 1 diabetes requires insulin) captured by market mechanism (monopolistic pricing). The constraint's power derives entirely from the biological necessity, not from market efficiency. The perspectival gap reveals that most agents experience only their local view — the manufacturer sees innovation, the patient sees unaffordable necessity, the physician sees appropriate gatekeeping, the FDA sees regulatory compliance. The analytical perspective reveals the full structure: it is a Snare that uses institutional theater (rebates, formulary management, safety monitoring) to maintain extraction from a trapped population.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality d for each perspective is determined by their structural relationship to the extraction flow and their exit capacity. Trapped patients (powerless exit) experience d ≈ 0.95 (nearly perfect victims), yielding high f(d) ≈ 1.42 and high experienced extractiveness χ. Institutional manufacturers with arbitrage exit experience d ≈ 0.05 (nearly perfect beneficiaries), yielding negative f(d) ≈ -0.12 and negative χ (extraction runs toward them). Moderate physicians with constrained exit and mixed benefit/victim status experience d ≈ 0.55, yielding f(d) ≈ 0.75 and moderate χ. The pipeline reveals why the system is stable: the maximum-extraction agents (beneficiaries with arbitrage) experience negative χ (no felt extraction cost), while maximum-extraction-bearing agents (patients with trapped exit) experience maximum χ (cannot escape). The system is stable because the extraction-bearing population has no leverage to resist.
 *
 * MANDATROPHY ANALYSIS:
 *   The insulin pricing constraint resolves mandatrophy by establishing that the beneficiary (manufacturer) and victim (patient) perspectives are structurally inverse, with no shared classification across power levels. The manufacturer sees Rope; the patient sees Snare. The gap between these is not perspectival relativity — it is structural asymmetry. The manufacturer has exit options (can reduce prices, can innovate different models, can shift markets); the patient has none (biologically locked). The constraint is unambiguously Snare from the analytical position because the suppression mechanism is absolute and the extraction mechanism has no coordination benefit for the victims. There is no mandatrophy confusion about whether this is extraction masked as coordination — the coordination (drug manufacturing and distribution) is genuine but is completely separate from the extraction mechanism (pricing capture). A manufacturer could maintain the same supply chain, distribution, clinical research, and manufacturing standards at 10% of current prices and the coordination function would remain intact. The extraction is not necessary for coordination. This is what makes it a Snare rather than a Tangled Rope: the extraction is surplus to the coordination function. The Tangled Rope would require that dismantling the extraction mechanism would also disable the coordination. In insulin markets, that is false — price reductions would not prevent insulin manufacture or distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generic_insulin_availability,
    'Do generic and biosimilar insulin formulations represent genuine alternatives that reduce extraction, or are they priced within 90-95% of branded versions, reproducing the same extraction structure?',
    'Market analysis of generic insulin pricing trajectories; comparison of insulin cost structure to other generic pharmaceutical categories; analysis of pharmacy benefit manager formulary placement of generics vs branded',
    'If generics are true alternatives: classification downgrade from Snare to Tangled Rope (patients have constrained exit). If generics reproduce pricing: confirms Snare classification (market structure prevents price competition even at generic tier).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generic_insulin_availability, empirical, 'Whether generic and biosimilar insulins provide genuine price alternatives').

omega_variable(
    international_price_differential,
    'Why is insulin 3-10x cheaper in Canada, Europe, and Australia than in the US despite identical molecular formulations and equivalent manufacturing quality?',
    'Regulatory analysis of pricing authority differences; comparison of FDA vs EMA vs Health Canada approval processes; analysis of reference pricing mechanisms in other countries',
    'If differential is regulatory: US pricing is policy choice, not market outcome — reclassifies from natural market mechanism to institutional extraction. If differential is demand-driven: validates US market pricing as clearing mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_price_differential, empirical, 'Root cause of 3-10x US-international insulin price differential').

omega_variable(
    patient_death_attributable_to_rationing,
    'What is the annual count of insulin-dependent patients who die or suffer severe complications from insulin rationing (dose stretching, switching to less-suitable formulations) and how does this trend relate to insurance coverage changes?',
    'Analysis of death certificates and adverse event reports; medical claims database analysis correlating insulin coverage denials with hospitalization/complications; qualitative documentation from endocrinology case files',
    'If death count > 0 (documented): establishes extraction as lethal; classification confirmed as Snare with no ambiguity about coercion threshold. If death count is low/unmeasured: suggests some patients have exit options or adaptive strategies; potential reclassification to Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_death_attributable_to_rationing, empirical, 'Mortality and morbidity attributable to insulin access denial').

omega_variable(
    pbm_rebate_opacity,
    'What proportion of insulin manufacturer list prices are recaptured by pharmacy benefit managers as rebates, and are these rebates passed to patients or retained as PBM margin?',
    'Pharmacy Benefit Manager disclosure analysis; insulin rebate contract database analysis; claims data showing manufacturer list price vs patient out-of-pocket across PBM networks',
    'If rebates are retained by PBMs: identifies a secondary extraction mechanism that hides true manufacturer margin while creating appearance of ''discounting''. If rebates are passed to patients: validates some pricing legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pbm_rebate_opacity, empirical, 'Proportion of insulin rebates retained by PBMs vs passed to patients').

omega_variable(
    patent_listing_strategy,
    'Are insulin manufacturers extending patent protection through formulation changes (insulin analogs, new delivery devices) that provide marginal clinical benefit primarily to maintain monopoly pricing, or do these innovations represent genuine therapeutic advances?',
    'Comparative clinical effectiveness analysis of new vs legacy insulin formulations; patent timeline analysis correlating new patent filings with impending expiry of original compounds; endocrinology literature review of innovation necessity vs market positioning',
    'If marginal benefit: patent strategy is an extraction mechanism that extends monopoly artificially; confirms Snare classification with institutional enforcement. If genuine advances: validates some premium pricing as innovation reward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_listing_strategy, empirical, 'Whether insulin formulation changes provide genuine clinical benefit or extend monopoly pricing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(insulin_pricing_cliff, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(insulin_tr_t0, insulin_pricing_cliff, theater_ratio, 0, 0.42).
narrative_ontology:measurement(insulin_tr_t8, insulin_pricing_cliff, theater_ratio, 8, 0.5).
narrative_ontology:measurement(insulin_tr_t16, insulin_pricing_cliff, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(insulin_be_t0, insulin_pricing_cliff, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(insulin_be_t8, insulin_pricing_cliff, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(insulin_be_t16, insulin_pricing_cliff, base_extractiveness, 16, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(insulin_pricing_cliff, resource_allocation).
narrative_ontology:affects_constraint(insulin_pricing_cliff, pharmaceutical_patent_extension).
narrative_ontology:affects_constraint(insulin_pricing_cliff, pharmacy_benefit_manager_rebate_opacity).
narrative_ontology:affects_constraint(insulin_pricing_cliff, medicaid_insulin_coverage_variation).

% DUAL FORMULATION NOTE:
% The insulin pricing cliff decomposes into three linked constraints: (1) insulin_pricing_cliff (this story) models the primary extraction mechanism of market consolidation and price escalation; (2) pharmaceutical_patent_extension models the intellectual property enforcement that blocks generic competition; (3) pharmacy_benefit_manager_rebate_opacity models the secondary extraction mechanism through rebate capture. Each has distinct ε: patent_extension operates at ε ≈ 0.45 (prevents competition but does not directly extract from patients), pbm_rebate_opacity operates at ε ≈ 0.55 (extracts from manufacturers but obscures patient-facing costs), insulin_pricing_cliff operates at ε ≈ 0.68 (direct patient extraction). All three are causally linked and should be analyzed together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(insulin_pricing_cliff, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
