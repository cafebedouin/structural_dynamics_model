% ============================================================================
% CONSTRAINT STORY: rural_healthcare_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rural_healthcare_autonomy, []).

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
 *   constraint_id: rural_healthcare_autonomy
 *   human_readable: Rural Healthcare Autonomy: Coordination vs. Regulatory Extraction
 *   domain: healthcare/rural_policy/medical_regulation
 *
 * SUMMARY:
 *   Rural healthcare autonomy presents a paradigmatic case of Tangled Rope
 *   constraint: genuine coordination functions (standardized protocols,
 *   emergency referral networks, pharmaceutical safety) are layered with
 *   regulatory extraction that concentrates power and resources in urban
 *   medical centers. Rural healthcare providers face a paradox — they require
 *   coordination with larger medical systems for specialist access and
 *   emergency referral capacity, but the regulatory framework that enables
 *   that coordination also constrains their local autonomy, staffing
 *   flexibility, and practice scope. The constraint has intensified over the
 *   past 20 years as regulatory requirements have accumulated (documentation
 *   standards, continuing education mandates, liability insurance), even as
 *   telemedicine and scope-of-practice expansions create sunset pathways
 *   toward decentralized healthcare delivery. The theater ratio (0.55)
 *   reflects that while much regulatory activity performs genuine safety
 *   functions, a significant portion consists of compliance rituals that
 *   don't materially improve patient outcomes in rural contexts — mandatory
 *   training hours, certification maintenance, and documentation requirements
 *   that are designed for complex urban environments with specialist backup.
 *
 * KEY AGENTS:
 *   - Rural Patient Populations: Primary victims (powerless/trapped) — geographically immobilized, dependent on local providers, facing delayed care and higher mortality
 *   - Rural Healthcare Providers: Secondary victims and partial beneficiaries (moderate/constrained) — benefit from coordination networks but bear extraction costs of compliance
 *   - Urban Medical Systems: Primary beneficiaries (institutional/arbitrage) — capture value through centralized referral networks and drug pricing leverage
 *   - Regulatory Agencies: Institutional enforcers (institutional/constrained) — coordinate safety standards but extract through gatekeeping and licensing control; constrained by legitimate liability concerns
 *   - Rural Health Advocacy Coalition: Organized reformers (organized/mobile) — building exit pathways through telemedicine, reciprocity, and scope expansion with sunset logic
 *   - Pharmaceutical Distribution Networks: Secondary beneficiaries (institutional/arbitrage) — extract through pricing controls and distribution gatekeeping
 *   - International Medical Standards Bodies: Institutional performers (institutional/arbitrage) — maintain credentialing theater with low functional necessity in resource-constrained rural settings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rural_healthcare_autonomy, 0.58).
domain_priors:suppression_score(rural_healthcare_autonomy, 0.62).
domain_priors:theater_ratio(rural_healthcare_autonomy, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rural_healthcare_autonomy, extractiveness, 0.58).
narrative_ontology:constraint_metric(rural_healthcare_autonomy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rural_healthcare_autonomy, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rural_healthcare_autonomy, tangled_rope).
narrative_ontology:human_readable(rural_healthcare_autonomy, "Rural Healthcare Autonomy: Coordination vs. Regulatory Extraction").
narrative_ontology:topic_domain(rural_healthcare_autonomy, "healthcare/rural_policy/medical_regulation").

domain_priors:requires_active_enforcement(rural_healthcare_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rural_healthcare_autonomy, urban_medical_systems).
narrative_ontology:constraint_beneficiary(rural_healthcare_autonomy, regulatory_agencies).
narrative_ontology:constraint_beneficiary(rural_healthcare_autonomy, pharmaceutical_distribution_networks).
narrative_ontology:constraint_victim(rural_healthcare_autonomy, rural_healthcare_providers).
narrative_ontology:constraint_victim(rural_healthcare_autonomy, rural_patient_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Rural patients cannot exit the healthcare system; trapped by geography, limited transportation, income constraints, and absence of alternatives. Bears full extraction cost through reduced access, delayed care, and higher mortality rates. No coordinated action possible due to dispersion and resource scarcity.
constraint_indexing:constraint_classification(rural_healthcare_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Rural doctors and clinics face mixed coordination-extraction dynamics. Benefit from standardized protocols, prescription networks, and emergency referral systems (genuine coordination). But experience extraction through staffing regulations, licensing requirements, pharmaceutical pricing controls, and liability standards designed for urban contexts. High cost of compliance; constrained exit due to community dependence and limited job alternatives.
constraint_indexing:constraint_classification(rural_healthcare_autonomy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Urban hospitals and health networks experience the constraint as pure coordination: standardized protocols enable referral networks, pharmaceutical supply chains, and liability management. Net beneficiary from regulatory uniformity; can arbitrage across compliance costs through scale economies. Captures value through centralized drug purchasing and specialist referral leverage.
constraint_indexing:constraint_classification(rural_healthcare_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Medical boards and health authorities coordinate essential safety standards (genuine function: prevent unsafe practice, protect patients). But also extract through: licensing gatekeeping, continuing education monopolies, costly compliance documentation, and liability standards that favor defensive medicine. Constrained by political pressure and malpractice litigation; cannot fully exit enforcement role. Active enforcement required for legitimacy.
constraint_indexing:constraint_classification(rural_healthcare_autonomy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Organized rural health coalitions (state medical associations, rural health networks) see the constraint as a coordination failure with a sunset clause. Lobbying for: telehealth licensure reciprocity, scope-of-practice waivers, reduced continuing education burdens, and rural-specific liability standards. Alternative pathways (telemedicine, physician assistants, nurse practitioners) bypass traditional autonomy constraints. Exit is structurally possible through policy reform; coalition has capacity and is deploying it.
constraint_indexing:constraint_classification(rural_healthcare_autonomy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Global medical standards (MD equivalency, specialty board certification, pharmaceutical naming) are largely performative at the implementation level. Rural practitioners in wealthy countries must comply with international credentialing despite local irrelevance — the protocols persist through institutional inertia and professional prestige, not functional necessity. Theater ratio high because compliance rituals (continuing education hours, documentation) don't correlate with clinical competence in resource-constrained settings.
constraint_indexing:constraint_classification(rural_healthcare_autonomy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a global analytical perspective, some constraint on rural medical autonomy appears natural: medical safety requires standardization, and some loss of local autonomy is the cost of that safety. Patient protection via standardized protocols is presented as inherent, unchangeable. However, base properties contradict this — regulatory agencies require active enforcement, urban systems arbitrage the rules, and advocates are building sunset pathways. The mountain framing naturalizes what is contingent institutional design.
constraint_indexing:constraint_classification(rural_healthcare_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rural_healthcare_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rural_healthcare_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rural_healthcare_autonomy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rural_healthcare_autonomy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rural_healthcare_autonomy, TR),
    TR >= 0.70.

:- end_tests(rural_healthcare_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the asymmetry between coordination benefits and regulatory burden. Rural providers genuinely depend on standardized protocols and referral networks, but the regulatory framework extracts through: licensing fees, continuing education mandates (often requiring travel to urban centers), malpractice insurance premiums calibrated to urban risk profiles, and scope-of-practice restrictions that prevent cost-effective staffing. The 20-year trajectory (0.35 → 0.58) shows extraction has increased as regulatory complexity accumulated. Suppression (0.62): Moderate-high. Rural providers cannot exit the medical system (patients require licensed practitioners). Geographic dispersion prevents collective action. Limited job markets constrain career mobility. But suppression is not total — some providers have exited to telemedicine, other professions, or urban markets. Patients cannot exit the rural context but can delay care or travel for specialist care at high cost. Theater ratio (0.55): Moderate. Significant regulatory activity serves genuine functions (infection control, medication safety, diagnostic standards). But substantial portion is performative: continuing education hour counting doesn't correlate with competence; documentation requirements exceed clinical information needs; licensing ceremonies mark professional identity more than demonstrable competence.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer's mountain perspective is a false summit. The claim that 'medical autonomy must be constrained for safety' naturalizes a contingent institutional arrangement. The constraint persists not because safety requires it but because: (1) urban medical systems have captured regulatory power and use it to maintain referral leverage, (2) pharmaceutical companies use standardization to control distribution, (3) professional guilds use licensing to maintain prestige and restrict competition, and (4) malpractice liability creates defensive-medicine incentives. Each of these is a policy choice, not a law of nature. The scaffold perspective reveals the sunset logic: telemedicine, nurse practitioner expansion, and interstate licensure reciprocity are building real alternative pathways. Rural healthcare autonomy is constrained by institutional design, not inherent necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to the constraint. Rural patients with no exit options (trapped) and victim status have maximum d ≈ 0.95, producing high f(d) and maximum experienced extraction. Rural providers with constrained exit and victim status have moderate-high d ≈ 0.70, producing elevated extraction. Urban systems as beneficiaries with arbitrage exit have low d ≈ 0.15, producing negative or minimal f(d) — they experience the constraint as coordination because they can arbitrage its rules. Regulatory agencies face mixed directionality: as enforcers (institutional/constrained), they have d ≈ 0.50 (symmetric), but as beneficiaries of professional prestige and gatekeeping power, they have lower effective d. This mixed status explains why they classify as Tangled Rope rather than pure Rope — they both coordinate and extract. Advocacy coalitions with organized power and mobile exit have d ≈ 0.40, producing moderate extraction experience and scaffold classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival decomposition. The question 'Is this pure coordination (Rope) or extractive (Snare)?' has no single answer — it depends on where you sit. For rural patients, extraction is unambiguous. For urban systems, coordination is unambiguous. For rural providers, both are real simultaneously. The Tangled Rope classification (at the regulatory agency and rural provider perspectives) captures this hybrid structure: genuine coordination function (safety standardization, emergency referral) coexists with genuine extraction (gatekeeping, compliance burden). The mandatrophy dissolves when you recognize that perspectival classification is not indeterminacy but precision — the constraint IS tangled at the institutional level because it performs both functions. The analytical observer's temptation to see a mountain (natural law) is the final test: natural law perspectives always turn out to be false summits when examined through Deferential Realism, revealing instead contingent institutional arrangements that appear natural because their beneficiaries have framed them as such.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_standardization_necessity,
    'How much of the regulatory constraint is genuinely necessary for patient safety versus how much is professional gatekeeping disguised as safety?',
    'Comparative health outcome analysis between high-regulation and low-regulation rural jurisdictions; historical analysis of regulatory tightening vs. safety improvement correlation; analysis of malpractice rates in urban vs rural settings accounting for case complexity',
    'If 70%+ necessary: constraint is Rope (coordination) for most perspectives. If <50% necessary: constraint is Snare with regulatory capture components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_standardization_necessity, empirical, 'Proportion of regulatory constraint driven by safety necessity vs. professional gatekeeping').

omega_variable(
    telehealth_capability_gap,
    'Do telemedicine and remote specialist consultation genuinely close the urban-rural capability gap or do they create new dependencies on urban infrastructure?',
    'Longitudinal outcome tracking in rural clinics with telemedicine adoption; analysis of specialist availability changes and patient survival rates; measurement of downstream referral pressure and rural capacity changes',
    'If genuine closure: scaffold perspective confirmed with strong sunset mechanism. If new dependencies: constraint shifts from physical isolation to digital infrastructure control (substitute snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(telehealth_capability_gap, empirical, 'Whether telemedicine genuinely closes the urban-rural capability gap').

omega_variable(
    licensure_reciprocity_implementation,
    'Can interstate/interprovincial medical licensure reciprocity be implemented without creating patient safety risks or just shifting liability between jurisdictions?',
    'Comparative analysis of reciprocity regimes (US telehealth compacts, ECFMG equivalency standards); outcome tracking in high-reciprocity vs low-reciprocity jurisdictions; malpractice litigation patterns',
    'If implementable without risk: major structural validation for scaffold sunset. If risk-shifting only: reciprocity reform is theater without functional change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensure_reciprocity_implementation, empirical, 'Whether licensure reciprocity can work without safety degradation').

omega_variable(
    scope_of_practice_expansion_limits,
    'What are the actual performance limits for nurse practitioners, physician assistants, and paramedics providing primary care in rural settings?',
    'Outcome-based scope analysis comparing expanded vs restricted practice regions; diagnostic accuracy and patient satisfaction metrics; emergency referral patterns and case complexity',
    'If expanded scope safe: significant autonomy recovery for rural systems. If limited: constraint persists due to genuine safety bounds rather than regulatory gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_practice_expansion_limits, empirical, 'Performance limits for non-physician rural primary care providers').

omega_variable(
    pharmaceutical_access_mechanisms,
    'Is the rural pharmaceutical supply constraint due to legitimate distribution economics or artificial pricing and distribution monopolies?',
    'Cost structure analysis of rural vs urban pharmacy operations; correlation between regulatory frameworks and drug availability; analysis of direct pharmaceutical supply and community pharmacy models',
    'If legitimate economics: constraint reflects Rope (coordination of distribution). If monopolistic: constraint is Tangled Rope or Snare with extraction via pricing control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_access_mechanisms, empirical, 'Whether pharmaceutical constraints reflect economics or monopoly extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rural_healthcare_autonomy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rha_tr_t0, rural_healthcare_autonomy, theater_ratio, 0, 0.32).
narrative_ontology:measurement(rha_tr_t10, rural_healthcare_autonomy, theater_ratio, 10, 0.42).
narrative_ontology:measurement(rha_tr_t20, rural_healthcare_autonomy, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(rha_be_t0, rural_healthcare_autonomy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rha_be_t10, rural_healthcare_autonomy, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rha_be_t20, rural_healthcare_autonomy, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rural_healthcare_autonomy, resource_allocation).
narrative_ontology:boltzmann_floor_override(rural_healthcare_autonomy, 0.18).
narrative_ontology:affects_constraint(rural_healthcare_autonomy, medical_specialist_concentration).
narrative_ontology:affects_constraint(rural_healthcare_autonomy, rural_hospital_viability).
narrative_ontology:affects_constraint(rural_healthcare_autonomy, pharmaceutical_supply_chain).
narrative_ontology:affects_constraint(rural_healthcare_autonomy, telemedicine_licensure_barriers).

% DUAL FORMULATION NOTE:
% Rural healthcare autonomy decomposes into multiple structurally distinct constraints: (1) specialist concentration (upstream — causes the autonomy need), (2) regulatory gatekeeping (this story), (3) pharmaceutical access (parallel extraction mechanism), and (4) telemedicine barriers (emerging sunset pathway). Each has its own extractiveness profile and temporal trajectory. The upstream constraint (specialist concentration, ε=0.25) creates the coordination need that the regulatory system exploits (this constraint, ε=0.58). The downstream telemedicine constraint (ε=0.35, Scaffold with sunset) represents an alternative resolution pathway.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rural_healthcare_autonomy, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
