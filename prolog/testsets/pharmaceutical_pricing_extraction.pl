% ============================================================================
% CONSTRAINT STORY: pharmaceutical_pricing_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_pricing_extraction, []).

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
 *   constraint_id: pharmaceutical_pricing_extraction
 *   human_readable: Pharmaceutical Pricing Extraction Mechanism
 *   domain: healthcare/pharmaceutical_economics
 *
 * SUMMARY:
 *   Pharmaceutical pricing extraction represents a core global health
 *   constraint where patent protections, regulatory structures, and
 *   information asymmetries combine to create high-extraction,
 *   high-suppression mechanisms that disproportionately harm powerless agents
 *   (uninsured and underinsured patients in wealthy nations, public health
 *   systems in developing countries). The constraint exhibits Snare
 *   characteristics at its core — mandatory medical need eliminates
 *   negotiating power and creates trapped populations — but also displays
 *   institutional complexity (tangled rope dynamics for insured patients,
 *   scaffold dynamics for generic entry movements, piton dynamics for
 *   regulatory agencies). The extractiveness has risen over the 20-year
 *   interval from 0.45 to 0.68 as drug prices have accelerated beyond
 *   inflation, driven by combination of longer patent protections (term
 *   extension, evergreening strategies), reduced generic competition
 *   (barriers to entry), and regulatory capture preventing price
 *   negotiations. Theater ratio has also risen (0.35 to 0.58) as the
 *   legitimating narratives (innovation justification, R&D cost recovery)
 *   have become more elaborate while actual innovation rates have not
 *   proportionally increased, indicating growing performative component.
 *
 * KEY AGENTS:
 *   - Uninsured Patients: Primary victim (powerless/trapped) — medical necessity eliminates exit options; bears full price extraction
 *   - Insured Patients: Secondary victim (moderate/constrained) — insurance negotiates but cost-sharing mechanisms shift burden to individual
 *   - Developing Nation Health Systems: Primary victim (moderate/constrained at national, trapped at local population level) — international patent enforcement prevents generic production
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — patent monopolies and pricing power with high exit mobility
 *   - Generic Manufacturers: Organized potential beneficiary (organized/mobile) — scaffold perspective; patent expiration creates viable exit pathway
 *   - Insurance Payers / PBMs: Secondary beneficiary (organized/constrained) — extract margins as intermediaries between manufacturers and patients
 *   - Regulatory Agencies: Institutional actor with degraded function (institutional/constrained) — safety review is real (piton); pricing review entirely absent
 *   - Open-Source Drug Initiatives: Organized reformers (organized/mobile) — challenge patent regime through alternative models
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_pricing_extraction, 0.68).
domain_priors:suppression_score(pharmaceutical_pricing_extraction, 0.72).
domain_priors:theater_ratio(pharmaceutical_pricing_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_pricing_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(pharmaceutical_pricing_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(pharmaceutical_pricing_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_pricing_extraction, snare).
narrative_ontology:human_readable(pharmaceutical_pricing_extraction, "Pharmaceutical Pricing Extraction Mechanism").
narrative_ontology:topic_domain(pharmaceutical_pricing_extraction, "healthcare/pharmaceutical_economics").

domain_priors:requires_active_enforcement(pharmaceutical_pricing_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_pricing_extraction, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(pharmaceutical_pricing_extraction, patent_holders).
narrative_ontology:constraint_beneficiary(pharmaceutical_pricing_extraction, brand_name_producers).
narrative_ontology:constraint_victim(pharmaceutical_pricing_extraction, patients_uninsured).
narrative_ontology:constraint_victim(pharmaceutical_pricing_extraction, patients_underinsured).
narrative_ontology:constraint_victim(pharmaceutical_pricing_extraction, public_health_systems).
narrative_ontology:constraint_victim(pharmaceutical_pricing_extraction, developing_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED PATIENT (SNARE) — Faces life-or-death medication decisions with zero exit capacity. Patent protections and pricing power create artificial scarcity; no generic alternatives available during patent window. Drug cost directly trades against food, housing, other medical care. Maximum experienced extraction with full suppression — medical necessity eliminates negotiating power entirely.
constraint_indexing:constraint_classification(pharmaceutical_pricing_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSURED PATIENT (TANGLED ROPE) — Insurance negotiates on behalf of patient (genuine coordination function) but pharmaceutical manufacturers extract through formulary restrictions, prior authorization requirements, and high deductibles that shift costs to individual patients. Mixed experience: some coordination benefit (access to negotiated rates) alongside significant extraction (cost-sharing burden, restricted drug choice).
constraint_indexing:constraint_classification(pharmaceutical_pricing_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEVELOPING NATION HEALTH SYSTEM (SNARE) — Faces choice between pharmaceutical prices designed for wealthy-nation incomes or patient death from untreated disease. International patent enforcement prevents generic production; no negotiating power. Structural dependency on multinational pricing regimes. High suppression — exit would require either violating patent law or abandoning treatment options entirely.
constraint_indexing:constraint_classification(pharmaceutical_pricing_extraction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMACEUTICAL MANUFACTURER (ROPE) — Experiences pricing regime as coordination mechanism: patent protections enable recovery of R&D costs, pricing power funds innovation pipeline. Net beneficiary with high exit mobility (can choose markets, pricing strategies, M&A). Extraction runs entirely toward this actor; suppression serves this actor's interests through patent enforcement.
constraint_indexing:constraint_classification(pharmaceutical_pricing_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AGENCY (PITON) — FDA/EMA approval process is largely performative theater for pharmaceutical manufacturers: agencies rubber-stamp manufacturer dossiers with high approval rates, rarely delay or deny based on pricing rationale (explicitly outside mandate). Theater persists through institutional inertia — the agency sees its own regulatory function as degraded relative to stated consumer protection mission. Theater ratio high (0.58) because safety review is real but pricing review is absent.
constraint_indexing:constraint_classification(pharmaceutical_pricing_extraction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INSURANCE PAYERS / PHARMACY BENEFIT MANAGERS (TANGLED ROPE) — Organized agents with real negotiating capacity in some contexts (reference pricing, formulary design) but also subject to extraction through mandatory coverage requirements, rebate opacity, and pharmacy monopoly gatekeeping. Experience both coordination benefits (collective bargaining) and extraction (margins captured by PBM intermediaries, gag clauses preventing transparency).
constraint_indexing:constraint_classification(pharmaceutical_pricing_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From universal timescale, might frame drug pricing as natural consequence of R&D costs and innovation incentives: high-risk pharmaceutical development requires capital recovery; pricing reflects risk allocation. But the structural data contradicts this — the constraint requires active enforcement (patent law, trademark protection, regulatory capture preventing generic entry, international IP treaties). These are contingent institutional arrangements, not laws of nature. Engine will classify as false summit.
constraint_indexing:constraint_classification(pharmaceutical_pricing_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: GENERIC MANUFACTURERS / PATENT REFORMERS (SCAFFOLD) — Organized actors (India generic manufacturers, open-source drug initiatives, patent challenge litigation, compulsory licensing movements) see the pricing extraction as a temporary institutional arrangement with clear sunset pathway. Patent expirations, biosimilar alternatives, and international patent harmonization pressures create exit pathways. Theater low for this perspective because generic entry immediately replaces brand-name extraction with cost-based pricing — no performative ritual needed.
constraint_indexing:constraint_classification(pharmaceutical_pricing_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_pricing_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_pricing_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_pricing_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_pricing_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_pricing_extraction, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_pricing_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. Base extraction reflects sustained pricing power maintained through patent enforcement, regulatory barriers to generic entry, and information asymmetries preventing price transparency. The 20-year trend (0.45→0.68) shows acceleration driven by: (1) patent term extension strategies (Hatch-Waxman gaming, evergreening), (2) biosimilar approval barriers, (3) consolidation reducing manufacturer competition, (4) PBM margin expansion. The magnitude reflects that drug prices in US are 2-10x international reference prices for identical medications, indicating substantial rent extraction above cost recovery. Suppression (0.72): Very high. Structural barriers include: (a) medical necessity (cannot exit by refusing treatment without health consequences), (b) patent enforcement (legal barriers to generic alternatives), (c) information asymmetry (patients don't know reference prices), (d) insurance gatekeeping (formulary restrictions, prior authorization), (e) international IP treaties (prevent compulsory licensing in developing nations). Suppression persists even when patients have nominal exit options (different insurance, international purchase) because actual exit is blocked by regulatory, financial, or informational barriers. Theater ratio (0.58): Moderate-high and rising. FDA approval rhetoric emphasizes safety and efficacy evaluation, but pricing review is entirely absent from the mandate. Regulatory theater consists of safety theater (genuinely valuable but decoupled from pricing justification) plus justification theater (R&D cost narratives that have become increasingly elaborate as actual innovation metrics have plateaued). The rising theater trend (0.35→0.58) reflects increasing narrative elaboration to justify prices that cannot be justified by cost accounting.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence rooted in directionality and exit options. The manufacturer experiences rope-like coordination (patent protections enable innovation funding) because they are the beneficiary with high exit mobility. The uninsured patient experiences snare (trapped by medical necessity) because they are a victim with no exit. The insured patient experiences tangled rope (partial negotiation benefit + cost-shifting extraction) because they have constrained exit and mixed beneficiary/victim status. The generic manufacturer/patent reformer experiences scaffold because they perceive a clear sunset (patent expiration, biosimilar alternatives, international price pressure) creating a timeline for extraction reduction. The regulatory agency sees itself as piton (performative safety theater masking absence of pricing review) because its actual function has degraded. The analytical observer at civilizational scale risks false naturalness (seeing innovation incentives as inherent necessity) but structural data contradicts this — patent terms, regulatory barriers, and price controls are contingent institutional choices, not laws of nature. The perspectival gaps reveal that no single classification is 'correct' — the presheaf of perspectives over different structural positions IS the analytical reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to extraction flow. Uninsured patients: medical necessity + trapped exit → d~0.95 → maximum experienced extraction. Insured patients: some negotiating power through collective insurance + constrained exit (high cost but possible) → d~0.75 → high extraction. Developing nation systems: international enforcement + constrained exit → d~0.80 → high extraction. Manufacturers: beneficiary position + arbitrage exit (can choose markets, pricing, M&A) → d~0.10 → low/negative experienced extraction. Generic manufacturers: will-be beneficiaries + mobile exit (patent expiration creates clear pathway) → d~0.35 → moderate extraction with clear sunset. Insurance payers: mixed position (coordinate on behalf of insured, extract as intermediary) + constrained exit → d~0.60 → moderate-high extraction. Regulatory agencies: constrained exit (institutional role) but arbitrage in practice (can choose regulatory stringency) → d~0.40 → moderate extraction. Open-source initiatives: organized reformers + mobile exit (alternative model pathways) → d~0.30 → moderate extraction with clear sunset. The beneficiary/victim declarations drive these derivations: uninsured and uninsured are victims; manufacturers and PBMs are beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE PRIMARY CLASSIFICATION RESOLVED: The claimed type (snare) is confirmed at the uninsured/uninsured perspective (the primary target populations). This is the core structural reality: pharmaceutical pricing creates extraction from those with medical necessity and no exit. However, the constraint also exhibits genuine tangled rope and scaffold dynamics for secondary actors, which would trigger mandatrophy concern if we claimed single type. The resolution is perspectival: the constraint is Snare for powerless trapped agents (uninsured), Tangled Rope for moderate constrained agents (insured, developing nations), Rope/Scaffold for organized/institutional actors with exit pathways. The claimed_type reflects the primary victim class (uninsured patients), but the commentary and perspectives documentation make the institutional complexity explicit. This prevents mislabeling extraction as pure coordination (which would hide the victim class) while acknowledging that some institutional actors experience genuine coordination benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    r_and_d_cost_recovery_baseline,
    'What proportion of pharmaceutical pricing reflects legitimate R&D cost recovery vs. rent extraction above cost-recovery levels?',
    'Cost accounting analysis: separate development costs (manufacturing, clinical trials, regulatory approval) from monopoly pricing rents. International price comparison (US vs EU vs India) controlling for income differences. Generic pricing analysis post-patent expiration.',
    'If rent extraction > 50% of pricing: Snare classification strengthened across all victim perspectives. If rent extraction < 20%: pricing partially reframes as coordination mechanism for innovation funding. If ambiguous: omega persists as irreducible uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(r_and_d_cost_recovery_baseline, empirical, 'Proportion of pricing that reflects R&D cost recovery vs. monopoly rent').

omega_variable(
    generic_entry_barrier_nature,
    'Are barriers to generic entry primarily patent-based (legitimate time-limited protection) or institutional (regulatory capture, pay-to-delay agreements, frivolous patent litigation)?',
    'Patent litigation data; FDA generic approval timelines vs patent expiration dates; frequency and outcome of pay-for-delay settlements; international generic entry timelines in different regulatory regimes.',
    'If primarily patent-based: constraints shifts toward Tangled Rope (temporary extraction with sunset). If institutional barriers dominate: classification remains Snare with suppression ≥0.70. If mixed with institutional dominance: identifies regulatory capture as co-constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generic_entry_barrier_nature, empirical, 'Nature of barriers to generic pharmaceutical entry').

omega_variable(
    patient_exit_elasticity,
    'At what price point do patients actually abandon expensive medications? Is ''trapped'' classification accurate or do patients have real exit options (cheaper countries, generic alternatives, untreated disease as baseline)?',
    'Medication adherence data by price tier; international price elasticity studies; cross-border pharmacy usage; epidemiological tracking of disease outcomes by insurance status and price sensitivity.',
    'If elasticity high: ''trapped'' classification overstates suppression; reclassify some perspectives as ''constrained'' rather than ''trapped''. If elasticity low: true trap confirmed; suppression ≥0.72 justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_exit_elasticity, empirical, 'Patient exit capacity and price elasticity for pharmaceutical demand').

omega_variable(
    innovation_counterfactual,
    'Would R&D pipeline and innovation rates decline materially if pharmaceutical pricing extraction were eliminated via price controls, patent reform, or generic acceleration?',
    'Natural experiments: compare R&D investment trends in countries with price controls (EU) vs monopoly pricing (US). Analysis of small-molecule vs biologics innovation under different pricing regimes. Longitudinal tracking of drug pipeline by therapeutic class post-price regulation.',
    'If innovation declines significantly: pricing extraction partly justified as innovation incentive (reclassify toward Tangled Rope). If innovation stable or increases: extraction mechanism is primarily rent-seeking (classification remains Snare). If heterogeneous by drug class: constraint family decomposition needed (separate stories for different therapeutic classes).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_counterfactual, empirical, 'Impact of pricing constraints on pharmaceutical innovation rates').

omega_variable(
    regulatory_capture_mechanism,
    'Does FDA approval process actively suppress generic and biosimilar entry through capture by manufacturers, or does it operate neutrally with generic and brand manufacturers equally protected?',
    'Comparative analysis of approval timelines for generics vs brand-name drugs; litigation data on FDA decisions; campaign finance / revolving-door data between pharma and regulatory agencies; international comparison (FDA vs EMA biosimilar approval rates).',
    'If capture confirmed: identifies regulatory capture as separate co-constraint (network.affects_constraints). Theater ratio interpretation shifts — high theater may reflect intentional obfuscation of capture rather than genuine safety review difficulty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Presence and extent of regulatory capture in pharmaceutical approval process').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression primarily structural (legal barriers, international enforcement, economic dependency) or internalized (patients believe high prices are justified, accept scarcity framing, don''t seek alternatives)?',
    'Survey data on patient beliefs about drug pricing justification; analysis of patient advocacy positions; behavior post-price reduction (does suppression persist when structural barriers are removed?); international comparison of patient agency in countries with price controls.',
    'If structural suppression dominates: barrier removal (patent reform, price controls) should reduce extraction. If internalized suppression dominates: extraction persists through narrative capture even after structural barriers removed. If both: omega identifies dual suppression mechanism requiring separate omega for internalization pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs internalized nature of pharmaceutical pricing suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_pricing_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharma_tr_t0, pharmaceutical_pricing_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pharma_tr_t10, pharmaceutical_pricing_extraction, theater_ratio, 10, 0.48).
narrative_ontology:measurement(pharma_tr_t20, pharmaceutical_pricing_extraction, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(pharma_be_t0, pharmaceutical_pricing_extraction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pharma_be_t10, pharmaceutical_pricing_extraction, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(pharma_be_t20, pharmaceutical_pricing_extraction, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_pricing_extraction, resource_allocation).
narrative_ontology:affects_constraint(pharmaceutical_pricing_extraction, healthcare_access_rationing).
narrative_ontology:affects_constraint(pharmaceutical_pricing_extraction, patent_term_extension_mechanisms).
narrative_ontology:affects_constraint(pharmaceutical_pricing_extraction, pharmacy_benefit_manager_intermediation).
narrative_ontology:affects_constraint(pharmaceutical_pricing_extraction, international_drug_pricing_harmonization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pharmaceutical_pricing_extraction, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
