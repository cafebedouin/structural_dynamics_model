% ============================================================================
% CONSTRAINT STORY: rare_disease_drug_development
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_disease_drug_development, []).

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
 *   constraint_id: rare_disease_drug_development
 *   human_readable: Orphan Drug Development and Pricing Asymmetry
 *   domain: pharmaceutical/regulatory/healthcare
 *
 * SUMMARY:
 *   Rare disease drug development exists at the intersection of market
 *   failure and extraction. Pharmaceutical companies face genuine commercial
 *   barriers to developing treatments for diseases affecting fewer than
 *   200,000 people in the US (the statutory definition): small patient
 *   populations cannot support research costs through normal pricing
 *   mechanisms. Orphan drug regulations (tax credits, market exclusivity,
 *   accelerated approval pathways) solve this market failure by providing
 *   regulatory incentives. However, the same regulatory mechanism that
 *   enables drug development also eliminates competitive constraints on
 *   pricing. Pharmaceutical developers can recoup R&D costs and then continue
 *   extracting value through pricing power backed by regulatory exclusivity.
 *   This creates a hybrid structure: genuine coordination (the orphan drug
 *   framework solves the market failure problem) combined with asymmetric
 *   extraction (pricing power leverages the patients' trapped exit options).
 *   The constraint exhibits all six DR types depending on the observer's
 *   structural position: pure extraction for the patient with no
 *   alternatives, coordination for the pharmaceutical company that benefits
 *   from the regulatory framework, mixed coordination-extraction for
 *   healthcare systems that must purchase under regulatory obligation, a
 *   temporary problem with a sunset for organized patient advocacy groups
 *   building alternative models, a degraded institutional ritual for patent
 *   protections that persist despite regulatory exclusivity carrying the
 *   functional burden, and a false natural law for analytical observers who
 *   naturalize what is contingent institutional choice.
 *
 * KEY AGENTS:
 *   - Rare Disease Patients: Primary victims (powerless/trapped) — disease eliminates exit options; pricing holds them ransom to urgency and uniqueness of available treatment
 *   - Healthcare System Administrators: Secondary victims (moderate/constrained) — face budgetary pressure and legal obligation to provide treatment; can negotiate but constrained by regulatory and ethical requirements
 *   - Pharmaceutical Developers: Primary beneficiaries (institutional/arbitrage) — benefit from orphan drug regulations that both enable R&D funding and protect pricing power; can exit rare-disease development entirely
 *   - Patient Advocacy Coalition: Organized agents (organized/constrained) — rare disease foundations and patient networks building alternative pathways (value-based pricing, tiered models, public funding); have agency but constrained by pharmaceutical industry political power
 *   - Patent System: Institutional mechanism (institutional/arbitrage) — patent exclusivity persists as inertial protection despite regulatory exclusivity carrying actual functional burden
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional choice (orphan drug pricing asymmetry) as inherent scientific necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_disease_drug_development, 0.58).
domain_priors:suppression_score(rare_disease_drug_development, 0.65).
domain_priors:theater_ratio(rare_disease_drug_development, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_disease_drug_development, extractiveness, 0.58).
narrative_ontology:constraint_metric(rare_disease_drug_development, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(rare_disease_drug_development, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_disease_drug_development, tangled_rope).
narrative_ontology:human_readable(rare_disease_drug_development, "Orphan Drug Development and Pricing Asymmetry").
narrative_ontology:topic_domain(rare_disease_drug_development, "pharmaceutical/regulatory/healthcare").

domain_priors:requires_active_enforcement(rare_disease_drug_development).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_disease_drug_development, pharmaceutical_developers).
narrative_ontology:constraint_beneficiary(rare_disease_drug_development, patient_advocacy_groups).
narrative_ontology:constraint_victim(rare_disease_drug_development, patients_with_rare_diseases).
narrative_ontology:constraint_victim(rare_disease_drug_development, healthcare_system_affordability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RARE DISEASE PATIENT (SNARE) — Patient has no exit option: the drug is the only treatment available, and patient cannot survive without it. Disease eliminates arbitrage options (cannot switch to competing treatment), creates trapped exit status. High suppression from disease urgency. Bears extraction through prohibitive pricing with no alternative source. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(rare_disease_drug_development, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HEALTHCARE SYSTEM ADMINISTRATOR (TANGLED ROPE) — Experiences genuine coordination: orphan drug regulations exist to solve the market failure problem (insufficient commercial incentive for rare-disease research). But also experiences extraction: pricing power leverages the system's obligation to provide treatment. Can theoretically exit via managed care restrictions or price negotiations, but constrained by legal/ethical obligations to patient populations. Moderate power enables some negotiation but limited arbitrage.
constraint_indexing:constraint_classification(rare_disease_drug_development, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL DEVELOPER (ROPE) — Benefits from orphan drug regulations (tax credits, market exclusivity, accelerated approval pathways). Experiences the constraint as pure coordination: regulatory framework solves the fundamental problem that rare diseases cannot support R&D costs via normal commercial channels. High arbitrage: can exit rare-disease development entirely and move to larger-market diseases. The constraint exists to benefit this agent — extraction runs toward them.
constraint_indexing:constraint_classification(rare_disease_drug_development, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PATIENT ADVOCACY COALITION (SCAFFOLD) — Organized agents (rare disease foundations, patient networks, policy advocates) perceive a temporary coordination problem with sunset dynamics. Value-based pricing frameworks, international reference pricing, and biosimilar competition are creating alternative pathways to sustainable pricing. Sunset mechanism: as rare disease prevalence classification boundaries become more sophisticated and as regulatory harmonization improves, the current pricing asymmetry model becomes unsustainable. Constrained by political resistance but with agency and an exit vision.
constraint_indexing:constraint_classification(rare_disease_drug_development, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PATENT SYSTEM PROTECTIONS (PITON) — Patent exclusivity for orphan drugs persists as institutional inertia despite degraded function. Original purpose: ensure developer recoups R&D costs during market exclusivity. Current theater: patent protections extend beyond cost recovery into rent extraction, but the mechanism no longer efficiently solves the target problem. Orphan drugs now operate within regulatory exclusivity frameworks (7-10 years in US) that often exceed patent life. Patent protection becomes performative while regulatory exclusivity carries the actual enforcement. Theater ratio high because the patent ritual persists despite regulatory exclusivity carrying the functional burden.
constraint_indexing:constraint_classification(rare_disease_drug_development, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, some verification of drug efficacy in rare populations is inherent to the scientific process: small sample sizes make randomized trials difficult, and natural history data accumulates slowly. This perspective views the pricing asymmetry as an inevitable structural feature of rare-disease markets — no alternative mechanism could exist. However, the structural data contradicts mountain classification: comparator countries with different pricing frameworks (NHS, German public insurance) achieve similar drug access at lower cost, revealing the asymmetry as contingent on institutional choice rather than natural law.
constraint_indexing:constraint_classification(rare_disease_drug_development, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_disease_drug_development_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_disease_drug_development, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_disease_drug_development, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_disease_drug_development, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rare_disease_drug_development, TR),
    TR >= 0.70.

:- end_tests(rare_disease_drug_development_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The pharmaceutical developer captures significant pricing power during market exclusivity (7-10 years), justified in part by R&D cost recovery but extending into rent extraction. The value of 0.58 reflects that the extraction is real and severe for patients but not maximal — the constraint still serves the legitimate function of enabling drug development. Without pricing incentives, fewer orphan drugs would be developed; with current pricing, drugs are priced at the extraction maximum that the market will bear. Suppression (0.65): Moderate-high. Significant barriers to patient exit include: disease uniqueness (no alternative treatment in many cases), market size that prevents competitor development, regulatory exclusivity that prevents generics, international price controls that limit arbitrage, and disease urgency that eliminates patient negotiating power. Some reduction from maximum suppression reflects that patient advocacy has achieved some concessions (Medicaid price negotiations, manufacturer assistance programs) and that biosimilar pathways are beginning to create post-patent competition. Theater ratio (0.58): Moderate. The orphan drug regulatory process contains significant theater: accelerated approval pathways rely on limited evidence, advisory committee reviews are influenced by pharmaceutical funding of patient advocacy groups, and patent protections persist despite regulatory exclusivity carrying the functional burden. But theater is not dominant — there is genuine scientific review and real uncertainty about therapeutic benefit in rare populations. The measurement trajectory shows increasing theater over time as regulatory pathways have accelerated and as patient advocacy has become increasingly pharma-funded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full indexical range from a single structural scenario. The pharmaceutical developer sees coordination (Rope) — the orphan drug framework solves the market failure that prevented rare-disease research. The healthcare system sees mixed coordination-extraction (Tangled Rope) — the framework enables drug access but pricing power is asymmetrically exploited. The rare disease patient sees pure extraction (Snare) — they experience only the pricing asymmetry with no access to the coordination benefit. The patient advocacy coalition sees a temporary problem with alternatives emerging (Scaffold) — value-based pricing, international reference pricing, and biosimilar competition are creating pathways to lower-cost drug access. The patent system sees its own degraded function (Piton) — patent exclusivity persists as institutional inertia despite regulatory exclusivity carrying the actual enforcement mechanism. The analytical observer risks seeing natural law (Mountain) — rare diseases inherently have small markets that cannot support R&D through normal mechanisms — but structural data reveals this as false summit: comparator countries achieve equivalent drug development with different pricing frameworks, showing the asymmetry is institutional choice rather than immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's power level, exit options, and structural relationship to extraction flow. Rare disease patients have high d (0.95): powerless, trapped, full victim — maximum experienced extractiveness. Healthcare administrators have moderate d (0.58): moderate power, constrained exit, both beneficiary (they receive regulatory framework that ensures drug availability) and victim (they bear cost). Pharmaceutical developers have low d (0.05): institutional power, arbitrage exit (can choose not to develop orphan drugs), full beneficiary — negative effective extractiveness from their position. Patient advocacy coalition has moderate d (0.55): organized but constrained, partly beneficiary (they secure drug access) and partly victim (they carry pharma-influenced framing). The sigmoid f(d) transforms these d values into the effective power modifier that scales base extractiveness into chi. Patients experience high chi; developers experience negative chi; intermediaries experience moderate chi.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by exposing the perspectival depth of a single structural phenomenon. The mandatrophy question is not 'which type is correct?' but 'which agent's position are you measuring from?' The tangled rope classification holds at the analytical level because the constraint genuinely coordinates (solves the market failure for orphan drug development) while asymmetrically extracting (pricing power exploits trapped patients). The snare perspective (patient view) and rope perspective (developer view) are both correct — they reveal that the beneficiary and victim experience the same constraint as opposite types. The scaffold perspective is real (alternative pricing models exist) but faces structural resistance from pharmaceutical industry political power. The piton perspective is real (patent exclusivity has become performative) but regulatory exclusivity provides functional enforcement. The mountain perspective is a false summit: the constraint naturalizes institutional choice as scientific necessity. The resolution strategy: decompose the constraint into subcomponents reflecting different mechanisms (R&D cost recovery vs. pricing extraction, regulatory exclusivity vs. patent protection) and apply value-based pricing frameworks that separate legitimate cost recovery from extractive rent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rare_disease_definition_boundary,
    'What epidemiological threshold defines ''rare'' and is that threshold inherent or institutional?',
    'Cross-national comparison of rare-disease definitions; analysis of whether threshold shifts correlate with regulatory changes or with biological disease frequency',
    'If threshold is institutional: rare-disease classification is contingent on regulatory choice and pricing asymmetry can be reformed. If threshold is biological: asymmetry is structural and requires different intervention points.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rare_disease_definition_boundary, empirical, 'Whether rare-disease definition is inherent or institutional').

omega_variable(
    r_and_d_cost_necessity,
    'Do orphan drug R&D costs genuinely require pricing asymmetry, or do alternative funding models (public funding, tiered pricing, prize mechanisms) achieve equivalent development rates?',
    'Comparative analysis of drug development timelines and success rates across funding models; economic modeling of public funding sufficiency for target therapeutic areas',
    'If asymmetry is necessary: tangled rope classification confirmed. If alternatives exist: the rope (coordination) component is genuine but the extraction component is unnecessary — constraint should reclassify as snare or degrade to piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(r_and_d_cost_necessity, empirical, 'Whether pricing asymmetry is necessary for rare-disease R&D').

omega_variable(
    patient_advocacy_capture_risk,
    'Do rare disease patient advocacy groups remain independent advocates for patients or become coopted into defending pharmaceutical pricing models?',
    'Analysis of funding sources for patient advocacy; correlation between advocacy group funding from pharmaceutical industry and stated positions on pricing; comparative messaging across advocacy groups with different funding structures',
    'If capture occurs: patient advocacy coalition (scaffold perspective) becomes identity-locked institutional actor, and its sunset vision collapses. If independence maintained: scaffold perspective remains structurally viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_advocacy_capture_risk, empirical, 'Whether patient advocacy groups resist pharmaceutical capture').

omega_variable(
    international_arbitrage_feasibility,
    'Can patients in high-price markets access lower-priced drugs from international sources without legal/regulatory barriers?',
    'Analysis of importation regulations across jurisdictions; documented cases of cross-border access; comparison of effective pricing after accounting for access mechanisms',
    'If arbitrage is feasible: patient exit options are more mobile than trapped classification suggests, and extractiveness should decrease. If arbitrage is effectively prevented: suppression is higher and snare classification is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_arbitrage_feasibility, empirical, 'Whether international arbitrage is feasible for rare disease drugs').

omega_variable(
    biosimilar_competition_dynamics,
    'Do biosimilar entrants actually reduce pricing power for originator rare-disease biologics, or do market segmentation and switching cost dynamics preserve extraction?',
    'Price trajectory analysis post-biosimilar entry for rare-disease biologics; market share dynamics; patient switching rates; comparison with large-market biologics',
    'If biosimilars reduce pricing power: competition mechanism is functional and scaffold perspective''s sunset is structurally sound. If market segmentation preserves pricing: extraction persists and scaffold sunset is aspirational rather than inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biosimilar_competition_dynamics, empirical, 'Whether biosimilar competition reduces rare-disease drug pricing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_disease_drug_development, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rdd_tr_t0, rare_disease_drug_development, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rdd_tr_t10, rare_disease_drug_development, theater_ratio, 10, 0.5).
narrative_ontology:measurement(rdd_tr_t20, rare_disease_drug_development, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(rdd_be_t0, rare_disease_drug_development, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rdd_be_t10, rare_disease_drug_development, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rdd_be_t20, rare_disease_drug_development, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_disease_drug_development, resource_allocation).
narrative_ontology:affects_constraint(rare_disease_drug_development, pharmaceutical_price_regulation).
narrative_ontology:affects_constraint(rare_disease_drug_development, healthcare_access_equity).
narrative_ontology:affects_constraint(rare_disease_drug_development, patent_system_drug_pricing).

% DUAL FORMULATION NOTE:
% Rare disease drug development is downstream of broader pharmaceutical pricing systems but represents a distinct structural constraint. The R&D market failure (small populations cannot fund research) is upstream and creates the rationale for orphan drug regulations. The pricing extraction mechanism is downstream and represents how regulatory solutions become exploitative. These form a constraint family linked by causal dependency: orphan drug regulations exist because of R&D market failure; pricing extraction exists because regulations created pricing power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
