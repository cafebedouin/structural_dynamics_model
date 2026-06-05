% ============================================================================
% CONSTRAINT STORY: pharmaceutical_pricing_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_pricing_monopoly, []).

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
 *   constraint_id: pharmaceutical_pricing_monopoly
 *   human_readable: Pharmaceutical Pricing Monopoly
 *   domain: healthcare/pharmaceutical_economics
 *
 * SUMMARY:
 *   Pharmaceutical pricing monopolies represent a structural constraint that
 *   combines patent-law enforcement, regulatory capture, and asymmetric
 *   information to extract rents from patients with inelastic demand for
 *   essential medications. The constraint operates globally but exhibits
 *   differentiated impacts: uninsured patients in high-income countries face
 *   catastrophic costs; developing nations face both price extraction and
 *   legal barriers to generic manufacturing. The manufacturer benefits from
 *   patent protection framed as innovation incentive, but the innovation
 *   argument masks an extractive regime that suppresses alternatives (generic
 *   drugs, prize-fund models, open-source drug development). The constraint
 *   is enforced through multiple interlocking mechanisms: patent law, trade
 *   agreements (TRIPS), regulatory approval gatekeeping, and industry
 *   influence over pricing bodies. Theater has increased over time as
 *   manufacturers have invested in direct-to-consumer marketing and
 *   disease-awareness campaigns that frame high prices as necessary for
 *   innovation.
 *
 * KEY AGENTS:
 *   - Uninsured Patients: Primary victim (powerless/trapped) — bears catastrophic costs with no exit option
 *   - Developing Nations: Structural victim (powerless/trapped) — prevented from generic manufacturing by trade agreements and IP enforcement
 *   - Public Health Systems: Mixed agent (moderate/constrained) — benefits from drug approval coordination but bears cost of price-driven rationing decisions
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — captures monopoly rents through patent protection with geographic price discrimination options
 *   - Government Regulators: Captured institutional actor (powerful/mobile) — supposed to regulate in public interest but subject to industry influence
 *   - Patent System: Degraded institutional structure (institutional/arbitrage) — enforces monopolies that exceed innovation requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_pricing_monopoly, 0.68).
domain_priors:suppression_score(pharmaceutical_pricing_monopoly, 0.72).
domain_priors:theater_ratio(pharmaceutical_pricing_monopoly, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_pricing_monopoly, extractiveness, 0.68).
narrative_ontology:constraint_metric(pharmaceutical_pricing_monopoly, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(pharmaceutical_pricing_monopoly, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_pricing_monopoly, snare).
narrative_ontology:human_readable(pharmaceutical_pricing_monopoly, "Pharmaceutical Pricing Monopoly").
narrative_ontology:topic_domain(pharmaceutical_pricing_monopoly, "healthcare/pharmaceutical_economics").

domain_priors:requires_active_enforcement(pharmaceutical_pricing_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_pricing_monopoly, pharmaceutical_manufacturer).
narrative_ontology:constraint_victim(pharmaceutical_pricing_monopoly, patients_without_insurance).
narrative_ontology:constraint_victim(pharmaceutical_pricing_monopoly, developing_nations).
narrative_ontology:constraint_victim(pharmaceutical_pricing_monopoly, public_health_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED PATIENT (SNARE) — No structural exit from the pricing regime. Faces choice between medical bankruptcy, treatment rationing, or forgoing essential medication. High suppression via legal patent enforcement prevents generic alternatives. Maximum experienced extraction with zero coordination benefit.
constraint_indexing:constraint_classification(pharmaceutical_pricing_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATIONS (SNARE) — Patent enforcement mechanisms and trade agreements (TRIPS) prevent local generic manufacturing. Population lacks purchasing power to negotiate prices. Trapped by international legal regime and economic dependency. Suppression enforced through trade sanctions and IP litigation threats.
constraint_indexing:constraint_classification(pharmaceutical_pricing_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PUBLIC HEALTH SYSTEMS (TANGLED ROPE) — Benefit from coordinated drug development and regulatory approval processes, but face extraction through pricing power. Constrained by budget caps and political feasibility of price negotiations. Some agency through bulk purchasing leverage, but high political cost of rationing decisions.
constraint_indexing:constraint_classification(pharmaceutical_pricing_monopoly, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL MANUFACTURER (ROPE) — Experiences the constraint as pure coordination: recouping R&D costs through patent-protected pricing. Captures monopoly rents during patent life. Has arbitrage options (geographic price discrimination, tiered pricing, licensing). Net beneficiary experiencing the constraint as legitimate coordination mechanism.
constraint_indexing:constraint_classification(pharmaceutical_pricing_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GOVERNMENT REGULATOR (TANGLED ROPE) — Coordinates drug approval process and safety standards (genuine coordination function), but also subject to regulatory capture through revolving-door employment and lobbying. Mobile enough to impose price controls but politically constrained by industry influence. Mixed coordination and extraction from regulatory power asymmetry.
constraint_indexing:constraint_classification(pharmaceutical_pricing_monopoly, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: PATENT SYSTEM INFRASTRUCTURE (PITON) — Maintains intellectual property enforcement mechanisms originally justified by innovation incentives. Theater ratio high because the patent system persists long after serving its stated coordination function. Pharmaceutical patents generate rents exceeding innovation incentives. Maintained through institutional inertia and legal doctrine despite pharmaceutical alternatives (prize funds, open-source models) existing.
constraint_indexing:constraint_classification(pharmaceutical_pricing_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational perspective, the constraint exhibits all hallmarks of pure extraction: high suppression (patent law, trade agreements), minimal coordination benefit (innovation incentives could be achieved through alternatives), and asymmetric power (pharmaceutical companies enforce monopolies against dispersed patients). The constraint persists through suppression mechanisms rather than genuine social value alignment.
constraint_indexing:constraint_classification(pharmaceutical_pricing_monopoly, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_pricing_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_pricing_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_pricing_monopoly, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_pricing_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_pricing_monopoly, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_pricing_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Manufacturers capture substantial rents above competitive drug costs, justified nominally by innovation incentives but substantially in excess of necessary incentives (evidence suggests 40-60% of pharmaceutical innovation would persist with alternative incentive structures). The constraint prevents price competition (generics) and blocks lower-cost alternatives (prize funds, public manufacturing). Trajectory shows increasing extractiveness over 15 years as manufacturers expanded pricing power and developed new therapeutic areas with captive markets. Suppression (0.72): High. Patent law, trade agreements (TRIPS), trademark protections, data exclusivity, and regulatory approval gatekeeping all suppress alternatives to paying monopoly prices. Legal threats against generic manufacturers and developing nations manufacturing essential drugs. Career barriers for generics manufacturers. Suppression is rising as manufacturers deploy ever-more complex strategies to extend patent lives (evergreening, combination therapies, formulation changes). Theater ratio (0.55): Moderate-high. Manufacturer framing of prices as necessary for innovation is partially theatrical — innovation story is cover for rent extraction. But genuine R&D costs are real and require recoupment. Increasing theater as marketing budgets exceed R&D for many manufacturers, and direct-to-consumer advertising frames expensive drugs as essential.
 *
 * PERSPECTIVAL GAP:
 *   The manufacturer sees the constraint as legitimate coordination (Rope): patent protection enables R&D investment, which benefits society through drug innovation. The uninsured patient sees the constraint as pure extraction (Snare): they face medical bankruptcy with no escape. The public health system sees mixed coordination and extraction (Tangled Rope): the drug approval and safety coordination function is real, but the pricing power is extractive. The regulator sees themselves as coordinating drug safety but is actually party to regulatory capture (Tangled Rope from their perspective, though they may not perceive it). The patent system sees itself as coordinating innovation incentives (Rope from its own perspective) but functions primarily to enforce rent extraction (Snare from victims' perspectives, Piton from analytical perspective because it persists through inertia beyond its stated purpose). The analytical observer sees pure extraction (Snare) because the coordination benefits could be achieved through alternative mechanisms (public prizes, accelerated FDA approval for generic manufacturers, prize funds for neglected diseases) while the suppression mechanisms could not. The perspectival collapse between manufacturer-as-rope and victim-as-snare is the diagnostic signature of this constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional flow: manufacturers (d ≈ 0.05, beneficiary + arbitrage) experience negative effective extraction — the constraint subsidizes them. Uninsured patients (d ≈ 0.95, victim + trapped) experience maximum extraction. Public health systems (d ≈ 0.65, victim + constrained) experience high extraction but retain some negotiating agency. Regulators (d ≈ 0.45, victim + mobile) are partly captured but retain agency, reflecting the moderate-powerful classification. The perspectival gap between manufacturer (Rope) and powerless patient (Snare) is maximal — same structural constraint, radically different classifications due to directionality differences (d varies from 0.05 to 0.95) and exit options (arbitrage versus trapped).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that Snare classification is correct from the analytical perspective and from the victims' perspectives. The manufacturer's Rope classification is a misclassification driven by beneficiary status — they experience the constraint as coordinating innovation incentives, but this is a cover story for extraction. The mandatrophy is resolved by recognizing that multiple institutional actors (manufacturers, regulators, patent offices) have internalized a false narrative about the constraint's necessity that permits them to classify it as benign coordination when it is actually extraction. The constraint qualifies as Snare: ε=0.68 (≥0.46), suppression=0.72 (≥0.60), and χ (effective extraction) is high across all victim perspectives. The manufacturer's Rope perspective is rejected as a false positive derived from beneficiary bias. The piton classification reveals the patent system as degraded infrastructure — the suppression mechanisms have accumulated (data exclusivity, evergreening, trademarking) far beyond what innovation incentive theory justifies. These are the added layers that push extractiveness from justified innovation incentive (~0.25) to extractive monopoly (~0.68).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_counterfactual,
    'How much innovation would persist without pharmaceutical patent monopolies? What portion of drug development is driven by patent rents versus by competitive advantage, reputation, and alternative incentive structures?',
    'Historical analysis of drug development patterns pre/post-patent expansion; comparison with prize-fund-supported drug development; analysis of orphan drug development with different incentive structures',
    'If substantial innovation would persist: patent monopoly appears as pure extraction (Snare). If innovation would collapse: patent regime appears as legitimate coordination mechanism (Rope). Current evidence suggests 40-60% of pharmaceutical innovation would persist without patent protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_counterfactual, empirical, 'Whether pharmaceutical innovation requires current patent monopoly regime').

omega_variable(
    suppression_mechanism_identity,
    'Is suppression of generic alternatives primarily structural (patent law) or internalized (belief in innovation incentive legitimacy among policymakers)?',
    'Analysis of policy responses in jurisdictions with aggressive generic enforcement (India, Brazil) versus strict IP enforcement (US, EU). Longitudinal tracking of policy shift correlations with public health outcomes.',
    'If primarily structural: suppression persists through law enforcement; removing patent protection changes the constraint. If internalized: many policymakers would resist generic alternatives even without patent law because they believe the innovation story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_identity, empirical, 'Whether suppression is structural legal enforcement or internalized belief system').

omega_variable(
    geographic_arbitrage_sustainability,
    'Can parallel importation and geographic price discrimination persist indefinitely, or will price convergence eventually eliminate arbitrage opportunities for manufacturers?',
    'Econometric modeling of long-term price dynamics across markets; analysis of manufacturer response to parallel trade threats; comparison with electronics and luxury goods pricing convergence patterns',
    'If arbitrage opportunities decline: manufacturer classification shifts from institutional/arbitrage (Rope) toward institutional/constrained (Tangled Rope). Extracted rents would decrease over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_arbitrage_sustainability, empirical, 'Long-term viability of geographic price discrimination in pharmaceuticals').

omega_variable(
    regulatory_capture_extent,
    'What fraction of pharmaceutical pricing extraction is enabled by regulatory capture (industry influence over approval and pricing decisions) versus structural patent rights?',
    'Campaign finance analysis; revolving-door employment tracking; comparison of regulatory decisions pre/post industry funding events; jurisdiction comparison (countries with strict regulatory independence versus captured regulators)',
    'If capture is primary: removing industry influence changes the constraint more than removing patents. If patents are primary: regulatory reform has limited impact. Current evidence suggests 30-40% of extraction operates through capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Extent to which regulatory capture enables pharmaceutical pricing extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_pricing_monopoly, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharma_price_tr_t0, pharmaceutical_pricing_monopoly, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pharma_price_tr_t5, pharmaceutical_pricing_monopoly, theater_ratio, 5, 0.48).
narrative_ontology:measurement(pharma_price_tr_t10, pharmaceutical_pricing_monopoly, theater_ratio, 10, 0.55).
narrative_ontology:measurement(pharma_price_tr_t15, pharmaceutical_pricing_monopoly, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(pharma_price_be_t0, pharmaceutical_pricing_monopoly, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(pharma_price_be_t5, pharmaceutical_pricing_monopoly, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(pharma_price_be_t10, pharmaceutical_pricing_monopoly, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(pharma_price_be_t15, pharmaceutical_pricing_monopoly, base_extractiveness, 15, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_pricing_monopoly, resource_allocation).
narrative_ontology:affects_constraint(pharmaceutical_pricing_monopoly, generic_drug_access_barriers).
narrative_ontology:affects_constraint(pharmaceutical_pricing_monopoly, orphan_drug_pricing).
narrative_ontology:affects_constraint(pharmaceutical_pricing_monopoly, tiered_pricing_mechanisms).

% DUAL FORMULATION NOTE:
% Pharmaceutical pricing monopoly decomposes into three constraint stories: patent_protection_mechanism (ε=0.45, justifiable innovation incentive), regulatory_gatekeeping (ε=0.38, security coordination function), and monopoly_rent_extraction (ε=0.68, pure extraction). The unified constraint story treats them as one system; analytical decomposition reveals that suppression mechanisms (evergreening, data exclusivity, price discrimination) transform the first two into pure extraction. This story focuses on the combined monopoly regime; downstream constraints address specific implementation mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pharmaceutical_pricing_monopoly, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
