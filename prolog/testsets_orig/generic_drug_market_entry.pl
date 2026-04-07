% ============================================================================
% CONSTRAINT STORY: generic_drug_market_entry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_generic_drug_market_entry, []).

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
 *   constraint_id: generic_drug_market_entry
 *   human_readable: Generic Drug Market Entry Barriers and Regulatory Coordination
 *   domain: pharmaceutical/regulatory/economic
 *
 * SUMMARY:
 *   Generic drug market entry is constrained by a hybrid structure of patent
 *   law, regulatory data exclusivity, and litigation barriers that
 *   simultaneously enable innovation funding (coordination function) and
 *   extract patient surplus (extraction function). The constraint exhibits
 *   tangled structure: brand pharmaceutical manufacturers benefit from
 *   exclusivity periods that justify high-risk R&D investment, but these same
 *   periods prevent generic competitors from entering until years after
 *   patent expiration. Generic manufacturers face trapped structural position
 *   — they must navigate complex regulatory approval (ANDA), design-around
 *   patent litigation (Paragraph IV challenges), and data exclusivity
 *   blocking periods (up to 12 years even after patent expiration). Patients
 *   bear the extraction cost: high brand prices persist throughout
 *   exclusivity windows despite zero ongoing innovation benefit. The
 *   constraint's base extractiveness (0.52) reflects that brand manufacturers
 *   extract 5-7 years of supracompetitive pricing after drug exclusivity
 *   ends, and suppression (0.65) reflects multiple barriers that prevent
 *   generic entry: regulatory complexity, litigation costs ($5-10M per
 *   Paragraph IV challenge), and strategic patent prosecution (evergreening).
 *   Theater ratio (0.48) is moderate because regulatory review serves genuine
 *   safety functions but patent prosecution increasingly involves procedural
 *   tactics disconnected from substance.
 *
 * KEY AGENTS:
 *   - Brand Pharmaceutical Manufacturer: Primary beneficiary (institutional/arbitrage) — captures supracompetitive margins during exclusivity window; extraction is enabled by patent/exclusivity regime
 *   - Generic Manufacturer: Primary victim (powerless/trapped) — faces regulatory complexity, litigation cost, and exclusivity barriers; cannot exit market entry process without losing investment
 *   - Patient Seeking Generic Access: Secondary victim (powerless/trapped) — bears inflated prices during exclusivity window; dependent on medications; no exit option
 *   - FDA Regulatory Agency: Institutional actor (institutional/constrained) — genuinely coordinates safety review but also subject to regulatory capture (data exclusivity periods exceed safety necessity)
 *   - Generic Manufacturer Coalition: Organized victim group (organized/constrained) — can coordinate on standards and advocacy but remains structurally disadvantaged by litigation cost and exclusivity barriers
 *   - Patent System: Institutional legacy (institutional/arbitrage) — maintains itself through procedural inertia; actual protective function has atrophied as development timelines have extended (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(generic_drug_market_entry, 0.52).
domain_priors:suppression_score(generic_drug_market_entry, 0.65).
domain_priors:theater_ratio(generic_drug_market_entry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(generic_drug_market_entry, extractiveness, 0.52).
narrative_ontology:constraint_metric(generic_drug_market_entry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(generic_drug_market_entry, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(generic_drug_market_entry, accessibility_collapse, 0.0).
narrative_ontology:constraint_metric(generic_drug_market_entry, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(generic_drug_market_entry, tangled_rope).
narrative_ontology:human_readable(generic_drug_market_entry, "Generic Drug Market Entry Barriers and Regulatory Coordination").
narrative_ontology:topic_domain(generic_drug_market_entry, "pharmaceutical/regulatory/economic").

domain_priors:requires_active_enforcement(generic_drug_market_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(generic_drug_market_entry, brand_pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(generic_drug_market_entry, regulatory_agencies).
narrative_ontology:constraint_victim(generic_drug_market_entry, generic_manufacturers).
narrative_ontology:constraint_victim(generic_drug_market_entry, patients_seeking_affordability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERIC MANUFACTURER (SNARE) — Trapped by regulatory complexity, patent litigation expense, and data exclusivity exclusions. Cannot exit the market entry process without abandoning investment. Faces maximum coercive overhead: patent cliffs designed to delay generic entry, regulatory exclusivity periods that block market access despite patent expiration, and litigation costs that exceed small-to-midsize generic firms' annual R&D budgets. Extraction is structural and near-inescapable.
constraint_indexing:constraint_classification(generic_drug_market_entry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PATIENTS SEEKING GENERIC ACCESS (SNARE) — Trapped by price barriers that regulatory gatekeeping maintains. Cannot exit the market; dependent on medications. Extraction flows through inflated brand prices that exclusivity windows protect. From a generational perspective, entire patient cohorts aging into chronic disease face locked-in high costs.
constraint_indexing:constraint_classification(generic_drug_market_entry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: BRAND PHARMACEUTICAL MANUFACTURER (ROPE) — Experiences the constraint as coordination: regulatory exclusivity periods provide predictable market protection that justifies high-risk drug development investment. Patent law and data exclusivity enable recovery of R&D costs before generic competition. This agent perceives genuine coordination benefit alongside their extraction advantage.
constraint_indexing:constraint_classification(generic_drug_market_entry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCY / FDA (TANGLED ROPE) — Constrained by mandate to approve safe efficacious drugs while maintaining patent incentives for innovation. Genuine coordination function: ensures drug quality and safety through ANDA review. But also subject to regulatory capture: data exclusivity periods are longer than necessary for safety assurance, and regulatory review backlogs delay generic approval even after patent expiration. Hybrid structure — both coordinating and extracting.
constraint_indexing:constraint_classification(generic_drug_market_entry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GENERIC MANUFACTURER COALITION (TANGLED ROPE) — Organized agents (industry association, research-based generics firms) benefit from regulatory clarity and standardized approval pathways that reduce uncertainty. But also bear extraction costs: paragraph IV patent litigation is expensive, and exclusivity periods prevent parallel regulatory pathways. Coalition has some agency (can coordinate on standards, lobby for reform) but constrained by legal/regulatory structure.
constraint_indexing:constraint_classification(generic_drug_market_entry, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PATENT SYSTEM LEGACY (PITON) — The 20-year patent term from filing is increasingly performative as drug development timelines extend. Average clinical trial duration now consumes 7-10 years, leaving 10-13 years of market exclusivity. The patent grant ritual persists (examination, prosecution, litigation) but its actual protective function has narrowed. Theater ratio high because much of patent prosecution is procedural rather than substantive — designing around patents via formulation changes or combination therapies is routine. The system maintains itself through institutional inertia, not functional effectiveness.
constraint_indexing:constraint_classification(generic_drug_market_entry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some market exclusivity is necessary for innovation funding: drug development requires massive upfront investment with binary success/failure outcomes. The patent-innovation tradeoff appears immutable — sacrifice some patient access for the innovations that enable cure. This perspective naturalizes what is actually a policy choice. The engine will detect this as a false summit, revealing that the framing 'exclusivity is necessary for innovation' conflates empirical claim (exclusivity provides incentive) with naturalization (this specific exclusivity level is immutable).
constraint_indexing:constraint_classification(generic_drug_market_entry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(generic_drug_market_entry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(generic_drug_market_entry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(generic_drug_market_entry, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(generic_drug_market_entry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(generic_drug_market_entry, TR),
    TR >= 0.70.

:- end_tests(generic_drug_market_entry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts patient surplus through supracompetitive pricing during exclusivity windows. However, not all extracted value is extractive overhead — some compensates genuine innovation risk. The 0.52 value reflects that brand manufacturers extract 5-7 years of 200-400% pricing above marginal cost after patent expiration, which exceeds reasonable innovation recovery. The measurement trajectory (0.35 → 0.52) reflects increasing extractiveness as patents have become less effective (evergreening and design-around strategies) while data exclusivity has extended the protection window. Suppression (0.65): Moderate-high. Regulatory approval complexity ($2-3M cost, 12-24 months), Paragraph IV litigation ($5-10M, 30% success rate), and data exclusivity blocking create substantial barriers. But suppression is not total — many generics do enter (>90% of brand drugs have generics within 10 years of patent expiration), and some countries have lower barriers. Theater ratio (0.48): Moderate. Patent prosecution serves some substantive function (legal clarity) but increasingly involves procedural tactics (continuation applications, new indication patents) disconnected from genuine novelty. Regulatory review has genuine safety content but some procedural steps are perfunctory (bioequivalence testing, manufacturing site approval) rather than innovative work.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap separates brand manufacturers from generic manufacturers. Brand perspective (rope): exclusivity periods provide necessary and reasonable innovation incentive. They perceive coordination (funding mechanism) with minimal extraction. Generic perspective (snare): exclusivity extends far beyond innovation recovery and functions as rent extraction. Patent litigation is an extraction mechanism, not IP protection. FDA perspective (tangled rope): genuine coordination function (safety review) is real, but regulatory capture (data exclusivity periods longer than safety-justified, ANDA review backlogs) enables extraction. Patient perspective (snare from most timepoints): trapped in high-cost markets despite expired patents. The analytical observer's natural law view (mountain) risks naturalizing a policy choice ('exclusivity is necessary') as immutable law ('innovation requires exclusivity'). The piton perspective on patent system legacy reveals that patent prosecution has become increasingly performative as development timelines have extended — the system maintains itself through institutional weight rather than functional effectiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the constraint. Brand manufacturers are beneficiaries with arbitrage exit options (can license, create authorized generics, adjust formulation) — derived d ≈ 0.15, producing low/negative effective extraction from their perspective (rope classification). Generic manufacturers are victims with trapped exit options (must navigate full regulatory/litigation gauntlet or abandon drug) — derived d ≈ 0.92, producing high effective extraction (snare classification). FDA is a constrained institutional actor — derived d ≈ 0.55, producing moderate extraction (tangled rope classification). The perspectival gap reflects real structural differences: what appears as necessary coordination to brand manufacturers appears as extractive gatekeeping to generic manufacturers. The generic coalition has partial agency (can coordinate on standards, lobby) but insufficient power to change structural rules — constrained institutional exit, placing them at d ≈ 0.60 (tangled rope, not snare).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by decomposing the constraint into two structural claims that have different ε values and different primary mechanisms: (1) INNOVATION_FUNDING_COORDINATION (ε ≈ 0.25, Rope) — patent term and data exclusivity genuinely enable innovation risk recovery; (2) MARKET_EXTRACTION_MECHANISM (ε ≈ 0.62, Snare) — exclusivity periods extend supracompetitive pricing beyond innovation recovery and function as extraction. These are different constraints with different beneficiaries/victims and should be tracked separately. The current story treats them as one tangled constraint (0.52 extractiveness) because the institutional structure mixes them. The classification resolves by accepting the hybrid: the constraint exhibits both coordination and extraction, requiring tangled_rope type. The mandatrophy is resolved by documenting that 'necessary innovation incentive' and 'extracted patient surplus' are both real and both present in the same institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exclusivity_necessity_threshold,
    'What exclusivity duration is minimally necessary to incentivize drug development, and does actual policy meet or exceed that threshold?',
    'International comparative analysis: R&D investment levels correlated with exclusivity duration across jurisdictions (EU, US, Japan, India). Pharmaceutical economist surveys of minimum ROI thresholds.',
    'If actual exclusivity > necessary threshold: policy is extractive beyond innovation incentive. If actual ≈ necessary: policy is primarily coordination. If actual < necessary: innovation would collapse without intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusivity_necessity_threshold, empirical, 'Whether actual exclusivity duration exceeds minimal innovation incentive threshold').

omega_variable(
    generic_entry_timeline_optimization,
    'Is the current regulatory review timeline (12-24 months for ANDA approval post-patent expiration) determined by technical safety requirements or by procedural bottlenecks?',
    'FDA data on ANDA review phases; identification of rate-limiting step (chemistry review vs bioequivalence vs manufacturing); comparison with expedited review timelines for priority drugs.',
    'If technical requirement: suppression is structural and necessary. If procedural bottleneck: suppression could be reduced without safety loss, revealing extractive overhead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generic_entry_timeline_optimization, empirical, 'Whether ANDA review timeline reflects safety requirements or procedural inefficiency').

omega_variable(
    patent_cliff_extraction_mechanism,
    'Do ''authorized generics'' (brand companies releasing generic versions of their own drugs before patent expiration) lower prices or maintain margin through controlled supply?',
    'Price analysis: authorized generic entry timing vs independent generic entry timing; comparison of authorized generic prices to subsequent independent generic competition.',
    'If authorized generics lower prices: patent cliff is benign coordination transition. If authorized generics maintain high prices: patent cliff is a controlled extraction mechanism, and the brand manufacturer exercises de facto market power post-patent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patent_cliff_extraction_mechanism, empirical, 'Whether authorized generics function as price discipline or extraction control').

omega_variable(
    data_exclusivity_vs_patent_overlap,
    'How much of the observed market protection comes from data exclusivity (regulatory exclusion from patent examination) vs patent enforcement?',
    'Legal analysis of drug approvals with and without patent protection; identification of drugs where data exclusivity alone extends market protection beyond patent expiration.',
    'If data exclusivity is primary: the constraint is regulatory rather than patent-based, and regulatory capture is the dominant extraction mechanism. If patent is primary: the constraint is legal property rights. Classification impact depends on which mechanism controls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_exclusivity_vs_patent_overlap, empirical, 'Relative contribution of data exclusivity vs patent rights to market exclusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(generic_drug_market_entry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(generic_tr_t0, generic_drug_market_entry, theater_ratio, 0, 0.32).
narrative_ontology:measurement(generic_tr_t10, generic_drug_market_entry, theater_ratio, 10, 0.4).
narrative_ontology:measurement(generic_tr_t20, generic_drug_market_entry, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(generic_be_t0, generic_drug_market_entry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(generic_be_t10, generic_drug_market_entry, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(generic_be_t20, generic_drug_market_entry, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(generic_drug_market_entry, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(generic_drug_market_entry, 0.12).
narrative_ontology:affects_constraint(generic_drug_market_entry, pharmaceutical_pricing_sustainability).
narrative_ontology:affects_constraint(generic_drug_market_entry, drug_development_return_on_investment).
narrative_ontology:affects_constraint(generic_drug_market_entry, healthcare_access_equity).

% DUAL FORMULATION NOTE:
% Generic drug market entry constraints decompose into: INNOVATION_FUNDING (ε ≈ 0.25, rope) — patent/exclusivity as coordination for R&D cost recovery; MARKET_EXTRACTION (ε ≈ 0.62, snare) — exclusivity as rent extraction via sustained supracompetitive pricing. These stories are linked but structurally distinct. The current story (ε = 0.52, tangled_rope) represents the aggregate institutional arrangement where both mechanisms operate simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(generic_drug_market_entry, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
