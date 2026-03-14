% ============================================================================
% CONSTRAINT STORY: global_pandemic_surveillance_networks
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_pandemic_surveillance_networks, []).

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
 *   constraint_id: global_pandemic_surveillance_networks
 *   human_readable: Global Pandemic Surveillance Networks
 *   domain: public_health/governance/technology
 *
 * SUMMARY:
 *   Global pandemic surveillance networks emerged as infrastructure response
 *   to COVID-19 and subsequent zoonotic threats, creating centralized systems
 *   for real-time pathogen tracking. The constraint exhibits genuine
 *   coordination function (early-warning capacity prevents exponential
 *   outbreak dynamics) alongside systematic extraction: behavioral
 *   modification through algorithmic scoring, data monetization by
 *   pharmaceutical and technology companies, differential resource allocation
 *   based on surveillance data, and geopolitical asymmetry where surveillance
 *   capacity concentrates in high-income nations. The theater ratio (0.48)
 *   reflects that governance structures (ethical review, privacy frameworks,
 *   international agreements) provide partial legitimation and constraint but
 *   are often unable to prevent extractive secondary uses. Extractiveness has
 *   risen over the interval as surveillance systems have matured and
 *   expanded: initial pandemic emergency yielded to permanent infrastructure,
 *   secondary uses proliferated, and extraction mechanisms deepened. This is
 *   a diagnostic case for identity_locked agents (global south health
 *   professionals whose career and epistemic identity depends on centralized
 *   systems) and for the distinction between necessary coordination and
 *   contingent institutional architecture.
 *
 * KEY AGENTS:
 *   - Surveilled Citizens: Primary victims (powerless/trapped) — passively enrolled in data collection through digital infrastructure with minimal consent or recourse; bear cost of behavioral modification and surveillance-enabled control
 *   - Public Health Agencies: Primary beneficiary (moderate/constrained) — genuine coordination benefit from early detection but constrained by legacy systems and pressure from extractive partners to maintain centralized architecture
 *   - Pharmaceutical Companies: Secondary beneficiary (institutional/arbitrage) — benefit from population-level data without data collection costs; minimal extraction burden; can exit if surveillance underperforms
 *   - Government Ministries: Tertiary beneficiary (powerful/arbitrage) — extract political value through surveillance-enabled mandate enforcement and population control; high organizational power enables arbitrage exit
 *   - Digital Rights Coalition: Organized challenger (organized/constrained) — see temporary coordination problem with technical sunset; building federated and privacy-preserving alternatives
 *   - Global South Health Workers: Mixed victim-beneficiary (moderate/identity_locked) — structurally mobile but identity-locked to centralized systems through career dependence and epistemic commitment; bear labor extraction burden
 *   - WHO Coordination Framework: Institutional actor (institutional/arbitrage) — nominally coordinates equitable pandemic response but degraded by geopolitical extraction; maintains appearance of function through theater
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent surveillance architecture as immutable public health necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_pandemic_surveillance_networks, 0.58).
domain_priors:suppression_score(global_pandemic_surveillance_networks, 0.65).
domain_priors:theater_ratio(global_pandemic_surveillance_networks, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_pandemic_surveillance_networks, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_pandemic_surveillance_networks, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(global_pandemic_surveillance_networks, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_pandemic_surveillance_networks, tangled_rope).
narrative_ontology:human_readable(global_pandemic_surveillance_networks, "Global Pandemic Surveillance Networks").
narrative_ontology:topic_domain(global_pandemic_surveillance_networks, "public_health/governance/technology").

domain_priors:requires_active_enforcement(global_pandemic_surveillance_networks).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_pandemic_surveillance_networks, public_health_agencies).
narrative_ontology:constraint_beneficiary(global_pandemic_surveillance_networks, epidemiological_research_institutions).
narrative_ontology:constraint_beneficiary(global_pandemic_surveillance_networks, pharmaceutical_companies).
narrative_ontology:constraint_victim(global_pandemic_surveillance_networks, individual_privacy_commons).
narrative_ontology:constraint_victim(global_pandemic_surveillance_networks, marginalized_populations).
narrative_ontology:constraint_victim(global_pandemic_surveillance_networks, global_south_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED CITIZEN (SNARE) — Cannot exit data collection systems; health data collected passively through digital infrastructure, medical records, movement tracking. Genuine coordination function (early warning of pathogen spread) exists but is overwhelmed by extraction: behavioral modification through algorithmic scoring, differential access to medicines or transit, data monetization. Suppression is maximal — citizens are not informed of data flows, cannot meaningfully consent, have no practical recourse.
constraint_indexing:constraint_classification(global_pandemic_surveillance_networks, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC HEALTH AGENCY (TANGLED ROPE) — Genuine coordination function: real-time pathogen surveillance prevents exponential outbreak. But also extracts: concentrates control over outbreak response, enables mandate enforcement through surveillance infrastructure, shifts data collection costs to individuals. Exit options exist (federated, privacy-preserving alternatives) but are organizationally constrained by funding, legacy systems, and pressure from extractive partners.
constraint_indexing:constraint_classification(global_pandemic_surveillance_networks, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL COMPANY (ROPE) — Benefits from surveillance data: population-level disease burden metrics accelerate drug development, market sizing, and clinical trial recruitment. Experiences constraint as coordination: pathway to efficacy trials and revenue. Minimal extraction cost — data flows toward them as free input to their production. Arbitrage exit: can shift to alternative data sources (insurance claims, electronic health records) if surveillance network underperforms.
constraint_indexing:constraint_classification(global_pandemic_surveillance_networks, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL RIGHTS COALITION (SCAFFOLD) — Organized actors (privacy advocates, civil society, technologists) see surveillance network as temporary coordination problem with structural sunset: federated learning architectures, differential privacy algorithms, and local-first epidemiology offer alternatives that provide similar early-warning capacity without centralized data collection. Sees high theater ratio masking extraction as unavoidable public health necessity. Sunset mechanism: technical capabilities for privacy-preserving epidemiology mature over 5-10 years.
constraint_indexing:constraint_classification(global_pandemic_surveillance_networks, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GOVERNMENT MINISTRY (TANGLED ROPE) — Genuine coordination benefit (real-time health data enables resource allocation and outbreak response). But also extracts: surveillance infrastructure weaponized for political control, suppression of dissent through health mandate enforcement, differential resource allocation to favored populations. High organizational power allows arbitrage: can exit to alternative data sources but chooses not to because the extraction is valuable.
constraint_indexing:constraint_classification(global_pandemic_surveillance_networks, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: WHO COORDINATION FRAMEWORK (PITON) — International coordination mechanism ostensibly designed for equitable pandemic response, but degraded by geopolitical extraction: wealthy nations hoard data, supply chains prioritize high-income countries, surveillance capacity transfers north to south without reciprocal benefit. Theater ratio high: elaborate governance structures mask de facto extraction. Functionality attenuated by institutional inertia — the framework persists because alternatives haven't been fully operationalized, not because it performs its stated function.
constraint_indexing:constraint_classification(global_pandemic_surveillance_networks, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: GLOBAL SOUTH HEALTH WORKER (TANGLED ROPE) — Structurally mobile (could theoretically refuse to participate in data collection), but identity locked: professional identity constituted through participation in global health systems, career advancement tied to engagement with international surveillance frameworks, epistemic commitment to evidence-based medicine dependent on centralized data platforms. Genuine coordination benefit (access to global research, epidemic intelligence), but extraction is embedded: data flows toward high-income countries, surveillance capacity extracted without equivalent investment in local capacity, labor extracted to maintain systems that benefit foreign institutions.
constraint_indexing:constraint_classification(global_pandemic_surveillance_networks, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale, the collection of health data to detect disease is an inherent structural requirement of preventing pandemics: some surveillance is necessary and cannot be eliminated without accepting increased epidemic risk. This perspective naturalizes the constraint as immutable. However, the structural data contradicts the mountain classification: the extracted component (behavioral modification, political control, data monetization, geopolitical asymmetry) is contingent, not necessary. The false summit reveals that 'surveillance is necessary' naturalizes what is actually a contingent institutional arrangement around how surveillance is organized, who controls it, and who benefits.
constraint_indexing:constraint_classification(global_pandemic_surveillance_networks, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_pandemic_surveillance_networks_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_pandemic_surveillance_networks, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_pandemic_surveillance_networks, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_pandemic_surveillance_networks, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_pandemic_surveillance_networks, TR),
    TR >= 0.70.

:- end_tests(global_pandemic_surveillance_networks_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The surveillance network provides genuine early-warning capacity that prevents pathogen exponential spread, justifying a base ε of ~0.35-0.40 for the coordination component. However, significant extraction mechanisms layer onto this: behavioral modification (algorithmic scoring that modulates access), data monetization (pharmaceutical companies extracting market intelligence), geopolitical asymmetry (data flows concentrate in high-income nations), and political control (governments weaponizing surveillance infrastructure for mandate enforcement). These extraction mechanisms add ~0.15-0.20 to the base, yielding ε≈0.58. The constraint is not a snare (extraction dominates) because the coordination function is genuinely valuable and many agents benefit. It is not a rope (pure coordination) because the asymmetry is severe and suppression is high. Suppression (0.65): High. Multiple barriers constrain agents' exit options: (1) digital infrastructure dependency — data collection happens passively through devices and medical systems; (2) institutional inevitability — surveillance is presented as non-negotiable public health requirement; (3) legal architecture — contract terms and regulatory requirements enforce participation; (4) epistemic capture — health professionals are trained to believe centralized systems are necessary for evidence-based medicine. Theater ratio (0.48): Moderate. Governance structures (privacy frameworks, ethical review, international agreements) provide real but partial constraint on extraction. However, theater is increasing: as extractive uses proliferate, governance structures become more elaborate to provide legitimacy while constraining less. The measurement trajectory shows theater increasing from 0.25 to 0.48 as surveillance infrastructure matured.
 *
 * PERSPECTIVAL GAP:
 *   The original research beneficiary (public health agency) sees coordination (rope)—they genuinely solve the problem of early pathogen detection. The pharmaceutical company sees the constraint as enabling (rope or scaffold to them)—market data flows toward them as free input. Surveilled citizens see pure extraction (snare)—maximal cost, no coordination benefit, no exit. The digital rights coalition sees a temporary problem with a technical sunset (scaffold)—privacy-preserving alternatives maturing over 5-10 years. The government ministry sees both coordination and control extraction (tangled rope at best, snare at worst)—the surveillance infrastructure solves health coordination but enables political control. The global south health worker sees identity lock (tangled rope)—they cannot imagine stepping out of centralized systems because their professional identity is constituted through them. The WHO sees its own degraded ritual (piton)—the coordination framework persists through institutional inertia while geopolitical asymmetries undermine its stated function. The analytical observer risks seeing immutable natural law (mountain)—surveillance is necessary—but the structural data reveals the contingency: the necessity is for epidemiological data collection, not for the specific institutional arrangements (centralization, corporate access, geopolitical asymmetry) that characterize current networks.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation follows the structural positions of beneficiaries and victims. Surveilled citizens are victims (enter victim group) with no exit options (trapped exit modulation → d≈0.95 → f(d)≈1.42 → maximum experienced χ). Public health agencies are beneficiaries (coordinate) but constrained (constrained exit modulation → d≈0.55, moderate χ). Pharmaceutical companies are beneficiaries with arbitrage exit (d≈0.05, negative or minimal χ—they experience no extraction). Government ministries are beneficiaries with powerful institutions and arbitrage exit (d≈0.40, low χ). Digital rights coalition are organized agents with constrained exit (d≈0.50, moderate χ—they have some agency). Global south health workers are both beneficiaries (access to global research) and victims (labor and data extraction) with identity_locked exit: the identity lock means they cannot fully exercise their structural mobility (would require becoming a different professional), yielding d≈0.70 (higher than constrained but lower than trapped) and f(d)≈1.00 (moderate experienced extraction). Scope is global (σ=1.2), amplifying χ across all perspectives by 20%.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the six types are legitimate perspectival readings from different structural positions. The mandatrophy question is not 'is it extraction or coordination?' but 'from whom does coordination flow and to whom does extraction flow?' From the public health agency's perspective, the constraint is genuine coordination (rope or tangled rope)—they are solving a real problem. From the surveilled citizen's perspective, it is pure extraction (snare)—they bear all costs with no participation in the coordination function. From the pharmaceutical company's perspective, it is enabling (rope)—coordination and benefit flow toward them. The global south health worker perspective reveals where identity lock most tightly binds: they are structurally mobile (could exit to local health systems), but their identity is so thoroughly constituted through participation in centralized systems that exit is unthinkable from within their current frame. The false summit (mountain from analytical perspective) is the most diagnostically important: it reveals that 'surveillance is necessary for pandemics' naturalizes what is actually a contingent institutional arrangement. The necessity claim is true for the coordination function (epidemiological data collection is necessary). The contingency claim is true for the architecture (centralization, corporate access, geopolitical asymmetry are not necessary and could be eliminated by alternative designs without sacrificing epidemic early warning). The mandatrophy resolution is to show that both claims are true simultaneously—the coordination necessity and the architectural contingency—and that the false summit consists in using the true necessity claim to legitimize the contingent extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_contingency_boundary,
    'What component of pandemic surveillance is structurally necessary to prevent epidemic spread, and what component is extractive institutional overhead?',
    'Comparative analysis of alternative epidemiological architectures: federated learning models, privacy-preserving aggregation systems, and local-first disease surveillance. Measurement of early-warning capacity, detection latency, and outbreak control effectiveness across alternative designs.',
    'If surveillance necessity is minimal (e.g., <20% of collected data is epidemiologically useful): snare/extraction classification dominates. If necessity is high (>60%): tangled rope classification more robust. If necessity varies by pathogen type: constraint family decomposition required — different stories for endemic surveillance vs pandemic emergency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_contingency_boundary, empirical, 'Necessary vs contingent components of surveillance architecture').

omega_variable(
    extraction_measurement_asymmetry,
    'How much of the extractive component (behavioral control, data monetization, geopolitical asymmetry) is structurally inevitable given the decision to collect centralized data, versus how much could be eliminated by alternative governance?',
    'Analysis of extraction mechanisms in federated, decentralized, and local-first systems. Measurement of how much extraction persists when data ownership, storage, and access control are distributed. Comparison of differential privacy vs access-restricted vs federated architectures.',
    'If extraction is inevitable given any centralized collection: suppression ≥0.65 is justified. If extraction could be substantially reduced through decentralized alternatives: suppression metric overstates constraint severity, and mandatrophy analysis must address whether alternative designs constitute the ''true'' constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_measurement_asymmetry, empirical, 'Structural vs contingent extraction mechanisms in surveillance design').

omega_variable(
    identity_lock_persistence,
    'For the Global South health worker, is the identity lock (professional identity constituted through global health systems) persistent across generations or is it generational cohort artifact?',
    'Longitudinal study of career trajectories: do younger health workers in global south maintain epistemic commitment to centralized systems when alternative local capacity matures? Analysis of institutional incentive shifts as local health systems develop independent research capacity.',
    'If identity lock is persistent: the constraint will remain tangled rope for the global south professional even as institutional alternatives emerge. If cohort-dependent: identity lock dissipates as local institutions mature and offer alternative identity pathways, moving the constraint toward rope or scaffold from that perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Persistence of professional identity lock across generational transition').

omega_variable(
    theater_functionality_correlation,
    'Does the theatrical component (elaborate governance, ethical review boards, data protection frameworks) actually constrain extraction or does it serve as legitimating cover while extraction proceeds?',
    'Analysis of governance constraints on data use: frequency of privacy violations, enforcement actions, cases where governance frameworks blocked extractive practices. Comparison with jurisdictions that lack theater (ad-hoc surveillance) to measure whether outcomes differ.',
    'If theater constrains extraction: suppression metric is overstated. If theater is pure legitimation: current suppression estimate is accurate. If theater constrains some extraction while enabling other extraction: suppression is differentially distributed across victim groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_functionality_correlation, empirical, 'Whether governance theater constrains or legitimates extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_pandemic_surveillance_networks, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpsnw_tr_t0, global_pandemic_surveillance_networks, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gpsnw_tr_t5, global_pandemic_surveillance_networks, theater_ratio, 5, 0.4).
narrative_ontology:measurement(gpsnw_tr_t10, global_pandemic_surveillance_networks, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(gpsnw_be_t0, global_pandemic_surveillance_networks, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gpsnw_be_t5, global_pandemic_surveillance_networks, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(gpsnw_be_t10, global_pandemic_surveillance_networks, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_pandemic_surveillance_networks, global_infrastructure).
narrative_ontology:affects_constraint(global_pandemic_surveillance_networks, pharmaceutical_development_acceleration).
narrative_ontology:affects_constraint(global_pandemic_surveillance_networks, digital_identity_enrollment).
narrative_ontology:affects_constraint(global_pandemic_surveillance_networks, geopolitical_medical_asymmetry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_pandemic_surveillance_networks, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
