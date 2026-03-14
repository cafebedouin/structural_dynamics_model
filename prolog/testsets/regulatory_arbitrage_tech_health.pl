% ============================================================================
% CONSTRAINT STORY: regulatory_arbitrage_tech_health
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_arbitrage_tech_health, []).

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
 *   constraint_id: regulatory_arbitrage_tech_health
 *   human_readable: Regulatory Arbitrage in Health Technology Development
 *   domain: healthcare_technology_policy
 *
 * SUMMARY:
 *   Regulatory arbitrage in health technology creates a structural incentive
 *   for technology firms to design and manufacture to the lowest applicable
 *   standard across multiple jurisdictions, exploiting differences in
 *   approval rigor for devices and drugs. The constraint exhibits the full
 *   spectrum of DR classification depending on the observer's structural
 *   position. Technology firms see pure coordination — achieving global
 *   market access while minimizing product variants. Regulatory agencies
 *   experience mixed coordination and extraction — they genuinely need to
 *   cooperate internationally, but the threat of firm exit to lighter
 *   jurisdictions extracts policy concessions. Patients in low-regulation
 *   jurisdictions are trapped — they receive devices and drugs approved under
 *   permissive standards with no pathway for standards alignment. The
 *   constraint's extractiveness has increased over the interval as the number
 *   of jurisdictions with delegitimized regulatory capacity has grown and as
 *   firms have become more skilled at leveraging regulatory variance. Theater
 *   ratio has also risen, indicating that formal mutual recognition
 *   agreements have become increasingly performative — negotiated as if they
 *   harmonize standards while actually laundering approval-gap devices into
 *   regulated markets.
 *
 * KEY AGENTS:
 *   - Technology Firms: Primary beneficiary (institutional/arbitrage) — capture global market access by designing to lowest applicable standard; face minimal compliance cost across jurisdictions
 *   - Patients in Low-Regulation Jurisdictions: Primary victim (powerless/trapped) — receive devices and drugs approved under materially lower safety standards; cannot exit; bear direct health risk
 *   - High-Regulation Jurisdictions: Constrained victim (institutional/constrained) — face competitive pressure to lower standards or watch firms and investment relocate; MRA obligations force recognition of foreign approvals made under lower standards
 *   - Low-Regulation Jurisdictions: Mixed beneficiary/victim (institutional/constrained) — attract biotech investment and tax revenue through permissive standards; also export approval-gap products to trading partners and absorb their externalities
 *   - Regulatory Agencies: Constrained actors (institutional/constrained) — must coordinate with international bodies and maintain domestic standards while facing exit threats from firms; mutual recognition agreements structure this constraint
 *   - Mutual Recognition Agreements: Institutional mechanism (institutional/arbitrage) — provide formal cover for arbitrage while mimicking standard-setting activity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_arbitrage_tech_health, 0.58).
domain_priors:suppression_score(regulatory_arbitrage_tech_health, 0.65).
domain_priors:theater_ratio(regulatory_arbitrage_tech_health, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_arbitrage_tech_health, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_arbitrage_tech_health, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regulatory_arbitrage_tech_health, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_arbitrage_tech_health, tangled_rope).
narrative_ontology:human_readable(regulatory_arbitrage_tech_health, "Regulatory Arbitrage in Health Technology Development").
narrative_ontology:topic_domain(regulatory_arbitrage_tech_health, "healthcare_technology_policy").

domain_priors:requires_active_enforcement(regulatory_arbitrage_tech_health).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_arbitrage_tech_health, technology_firms).
narrative_ontology:constraint_beneficiary(regulatory_arbitrage_tech_health, jurisdictions_with_light_regulation).
narrative_ontology:constraint_victim(regulatory_arbitrage_tech_health, patients_subject_to_approval_gaps).
narrative_ontology:constraint_victim(regulatory_arbitrage_tech_health, regulatory_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENTS IN LOW-REGULATORY JURISDICTIONS (SNARE) — Trapped by geography and health need. Device or drug approved under minimal standards in jurisdiction A arrives in jurisdiction B with no pathway for retroactive standards alignment. Patient has no exit option; bears full cost of approval gap. Maximum experienced extraction — the constraint extracts from this agent's safety margin.
constraint_indexing:constraint_classification(regulatory_arbitrage_tech_health, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGULATORY AGENCIES (TANGLED ROPE) — Constrained by sovereignty and resource limitations. Must coordinate with international standards bodies and clinical communities (rope function) while simultaneously facing pressure from tech firms threatening to exit to lighter-regulation jurisdictions (extraction mechanism). Genuine coordination need exists alongside asymmetric extraction — the threat of regulatory arbitrage extracts policy concessions.
constraint_indexing:constraint_classification(regulatory_arbitrage_tech_health, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY FIRMS (ROPE) — Primary beneficiary. Experiences constraint as pure coordination: by designing to meet the lowest applicable standard, firms optimize global commercialization and coordinate manufacturing, supply chain, and marketing. The arbitrage IS the coordination solution — exploit regulatory variance to achieve harmonized go-to-market at the lowest common denominator. Net benefit to this agent.
constraint_indexing:constraint_classification(regulatory_arbitrage_tech_health, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LOW-REGULATION JURISDICTIONS (TANGLED ROPE) — Constrained by competition for tax revenue and innovation centers. Coordination function: lighter regulatory standards enable faster device/drug commercialization and biotech cluster formation. Extraction mechanism: other jurisdictions' standards drift downward as firms threaten exit, creating a regulatory race to the bottom. Both beneficiary (attracts firms and tax revenue) and victim (standards externality imposed on trading partners).
constraint_indexing:constraint_classification(regulatory_arbitrage_tech_health, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MUTUAL RECOGNITION AGREEMENTS (PITON) — Formal bilateral/multilateral agreements to recognize each other's device/drug approvals are theater masking the underlying arbitrage. MRAs are supposed to harmonize standards; instead they provide institutional cover for approval-gap laundering. The ritual of MRA negotiation persists despite low functional coordination — each jurisdiction retains approval authority but exercises it under extraction pressure. Theater ratio high because the MRA mechanism produces negotiation activity and diplomatic engagement that mimics standard-setting without constraining arbitrage.
constraint_indexing:constraint_classification(regulatory_arbitrage_tech_health, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the constraint exhibits genuine coordination function (enabling faster global drug/device access, reducing product fragmentation) alongside clear asymmetric extraction (safety standards drift toward the permissive, externalities imposed on regulated jurisdictions and vulnerable patients). The constraint persists because the coordination benefit is real and unevenly distributed — firms benefit immediately, harm to patient cohorts accumulates over time and is dispersed across populations.
constraint_indexing:constraint_classification(regulatory_arbitrage_tech_health, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_arbitrage_tech_health_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_arbitrage_tech_health, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_arbitrage_tech_health, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_arbitrage_tech_health, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_arbitrage_tech_health, TR),
    TR >= 0.70.

:- end_tests(regulatory_arbitrage_tech_health_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from patients and health systems in two ways: (1) direct harm from devices/drugs approved under lower standards (safety failures, off-label use, inadequate post-market surveillance); (2) indirect harm from standards drift — high-regulation jurisdictions face competitive pressure to weaken approval criteria to retain biotech industry. The value of 0.58 reflects that extraction is real but not total — many devices approved under low standards are safe, and firms do maintain quality levels above the minimum. Suppression (0.65): High. Barriers to exit and alternatives include: (a) patients cannot choose their jurisdiction; (b) regulatory agencies cannot unilaterally reject foreign approvals without breaching trade agreements; (c) firms face no penalty for designing to low standards; (d) adverse event data linking harms to approval-gap devices is fragmented across jurisdictions and underreported. Theater ratio (0.68): High and rising. Mutual recognition agreements create substantial performative activity — technical committees, negotiation cycles, diplomatic engagement — that mimics standard harmonization without constraining the underlying arbitrage mechanism. The ratio has increased because MRAs now cover more device classes and more jurisdictions, expanding the theater footprint while approval standards continue to diverge. Claimed type is Tangled Rope: genuine coordination function (enabling faster global access, reducing product fragmentation) exists alongside clear asymmetric extraction (standards drift, externalities imposed on regulated jurisdictions).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim is stark and persistent. Technology firms see coordination — the constraint enables them to design once and deploy globally. Regulatory agencies see mixed constraint — genuine international coordination need exists but is exploited to extract concessions. Patients see pure extraction — trapped by geography, subject to approval-gap devices with no recourse. Low-regulation jurisdictions see partial benefit masked by externalities — attracting investment but exporting harm. High-regulation jurisdictions see competitive pressure, not coordination — forced to recognize foreign approvals or lose industry. The analytical observer sees tangled rope — real coordination function (global access) coupled with real extraction (standards externality). The crucial gap is temporal: firm benefits are immediate and concentrated; patient harms accumulate over time and are dispersed across populations, creating a natural political economy where extraction persists despite net harm.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) represents each agent's structural position in the extraction flow. Technology firms (beneficiary + arbitrage exit) derive low d → negative χ — they experience the constraint as enabling their preferred outcome. Patients (victim + trapped exit) derive high d → high f(d) → high χ — they experience maximum extraction with no escape. Regulatory agencies (mixed role + constrained exit) derive moderate d reflecting their position between coordination need and exit threat. Low-regulation jurisdictions (beneficiary + constrained exit) derive moderate d because they benefit financially but cannot easily exit the competitive dynamics driving race-to-the-bottom. The directionality pipeline computes these values from the structural declarations, not from nominal power levels — two institutional actors have equal nominal power but drastically different structural positions within this specific constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy because it exhibits genuine coordination function (enabling global device/drug access, reducing product variants, supporting international trade) alongside genuine asymmetric extraction (safety standards externality, concentrated benefits vs dispersed harms, patient trapping). Neither pure coordination nor pure extraction alone captures the constraint's structure. The tangled rope classification reveals that: (1) eliminating the constraint entirely would harm global access and delay life-saving devices; (2) leaving it uncontrolled extracts unacceptably from patients and regulatory autonomy; (3) the resolution lies in reforms that preserve coordination while constraining extraction (mutual recognition agreements with real teeth, global minimum standards, post-market surveillance harmonization, liability frameworks that penalize approval-gap products). The mandatrophy is resolved not by choosing a single type but by analyzing the constraint's dual function and designing interventions that preserve benefits while suppressing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    approval_gap_harm_quantification,
    'What proportion of adverse events in high-regulation jurisdictions are attributable to devices/drugs approved in low-regulation jurisdictions under materially different standards?',
    'Pharmacovigilance data linking device failures or adverse drug reactions to approval history in originating jurisdiction; case-control analysis of harm rates by approval pathway',
    'If harm rate > 5%: snare classification for patient victims is correct; extraction is severe. If harm rate < 1%: extraction mechanism is overstated; constraint may be more rope-like (legitimate standards variance rather than approval gap).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(approval_gap_harm_quantification, empirical, 'Quantifying patient harm from approval standard gaps').

omega_variable(
    regulatory_race_threshold,
    'At what point do mutual recognition agreements cease to enforce meaningful standards convergence and become mere arbitrage laundering?',
    'Time-series analysis of device/drug approval criteria divergence before and after MRA signing; comparison of rejection rates across jurisdictions for same product classes',
    'If MRAs maintain standards within 10%: coordination function preserved. If standards diverge beyond 30%: MRA is theater, extraction mechanism dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_race_threshold, empirical, 'Threshold for MRA functional degradation').

omega_variable(
    firm_exit_threat_credibility,
    'How many firms actually relocate R&D or manufacturing to low-regulation jurisdictions when faced with higher approval standards?',
    'Comparative analysis of R&D location decisions; survey data on regulatory burden perception; econometric study of approval standard tightness vs. firm location choice',
    'If exit threat > 30% credible: extraction pressure on regulators is real; tangled rope from regulatory agency perspective confirmed. If threat < 10% credible: regulators have more bargaining power; constraint approaches rope-only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(firm_exit_threat_credibility, empirical, 'Credibility and frequency of firm exit threats').

omega_variable(
    patient_harm_versus_access_tradeoff,
    'What is the net health impact of faster global access enabled by regulatory arbitrage versus harm from approval-gap safety failures?',
    'Health economic model comparing lives saved by faster access to life-saving devices/drugs versus lives harmed by safety failures; disability-adjusted life years (DALYs) accounting for both benefit and harm populations',
    'If benefit > harm: constraint is net beneficial coordination mechanism; may reclassify toward rope. If harm > benefit: constraint is net extractive; snare classification for patient victims is diagnostically correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patient_harm_versus_access_tradeoff, preference, 'Net health impact of approval-gap arbitrage').

omega_variable(
    jurisdictional_capacity_asymmetry,
    'Are low-regulation jurisdictions choosing permissive standards strategically (to attract biotech investment) or due to resource constraints and regulatory capacity limitations?',
    'Qualitative analysis of regulatory agency resources and expertise; comparison of standards stringency vs. regulatory budget; interviews with regulatory decision-makers about constraint drivers',
    'If strategic choice: low-regulation jurisdictions are extractive beneficiaries; constraint is designed. If capacity-limited: harm is structural/unintended; may suggest capacity-building interventions rather than arbitrage suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_capacity_asymmetry, conceptual, 'Whether regulatory permissiveness is strategic or capacity-driven').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_arbitrage_tech_health, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regtech_tr_t0, regulatory_arbitrage_tech_health, theater_ratio, 0, 0.42).
narrative_ontology:measurement(regtech_tr_t5, regulatory_arbitrage_tech_health, theater_ratio, 5, 0.55).
narrative_ontology:measurement(regtech_tr_t10, regulatory_arbitrage_tech_health, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(regtech_be_t0, regulatory_arbitrage_tech_health, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(regtech_be_t5, regulatory_arbitrage_tech_health, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(regtech_be_t10, regulatory_arbitrage_tech_health, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_arbitrage_tech_health, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_arbitrage_tech_health, pharmaceutical_approval_fragmentation).
narrative_ontology:affects_constraint(regulatory_arbitrage_tech_health, medical_device_classification_variance).
narrative_ontology:affects_constraint(regulatory_arbitrage_tech_health, post_market_surveillance_gaps).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_arbitrage_tech_health, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
