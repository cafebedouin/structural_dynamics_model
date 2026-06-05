% ============================================================================
% CONSTRAINT STORY: ice_memory_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ice_memory_archive, []).

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
 *   constraint_id: ice_memory_archive
 *   human_readable: The imperative to create a global ice core archive before glaciers melt
 *   domain: environmental/technological/geopolitical
 *
 * SUMMARY:
 *   The global ice core archive imperative emerged in the late 2000s as
 *   climate science recognized that glaciers and polar ice sheets are melting
 *   faster than models predicted. Ice cores provide the most precise
 *   800,000-year climate records available: layered atmospheric composition,
 *   dust, aerosols, isotopic ratios, and trapped gases. The constraint is
 *   framed as inevitable: 'we must archive before they melt' becomes a moral
 *   and scientific imperative. Yet this framing obscures a tangled structure.
 *   Western institutions (NSF, USGS, Swiss Federal Institute) control archive
 *   locations and governance. Resource-constrained nations (Peru, Bhutan,
 *   Kenya) whose glaciers are disappearing fastest have minimal voice in
 *   extraction decisions. The archive enables genuine climate science
 *   (coordination function) while simultaneously concentrating data control
 *   in wealthy institutions (extraction function). The theater ratio reflects
 *   performative urgency: the climate crisis narrative justifies the
 *   archive's existence, but the archive's actual democratizing function
 *   (open data access) is much lower than its performative promise.
 *
 * KEY AGENTS:
 *   - Climate-vulnerable nations with ice resources (powerless/trapped) — Greenland, Peru, Bhutan, Kenya: contribute essential samples but have no control over archive decisions or access protocols
 *   - Developing nations with technical capacity (moderate/constrained) — Argentina, Chile, China: can participate in extraction but cannot fund archive infrastructure; benefit from scientific partnerships yet lose data sovereignty
 *   - International consortia (organized/constrained) — IPCC, IODP, NSF, WMO: coordinate global collection but operate under unequal participation; genuine coordination function plus asymmetric power
 *   - Western research universities and funding bodies (institutional/mobile) — NSF, USGS, Alfred Wegener Institute, Swiss Federal Institute: control archive locations, governance, and data access protocols; extract career capital and scientific control
 *   - Indigenous peoples in ice-rich regions (powerless/trapped) — Inuit, Quechua, Tibetan communities: territories are sampled; have no consultation in archive decisions; future climate knowledge reflects their land extraction, not their knowledge
 *   - Open science advocates (organized/constrained) — OSF, Center for Open Science: push for distributed digital access to reduce centralized control; constrained by institutional inertia
 *   - Publication system and journals (institutional/arbitrage) — Nature Climate Change, Science Advances: control narrative authority over ice core research; derive prestige from 'open data' rhetoric while maintaining closed-access publication gates
 *   - Analytical observer (analytical/analytical) — Civilizational perspective: risks naturalizing the archive as inevitable response to physics (glaciers melting, data dying) rather than contingent geopolitical structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ice_memory_archive, 0.52).
domain_priors:suppression_score(ice_memory_archive, 0.68).
domain_priors:theater_ratio(ice_memory_archive, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ice_memory_archive, extractiveness, 0.52).
narrative_ontology:constraint_metric(ice_memory_archive, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ice_memory_archive, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ice_memory_archive, tangled_rope).
narrative_ontology:human_readable(ice_memory_archive, "The imperative to create a global ice core archive before glaciers melt").
narrative_ontology:topic_domain(ice_memory_archive, "environmental/technological/geopolitical").

domain_priors:requires_active_enforcement(ice_memory_archive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ice_memory_archive, climate_research_community).
narrative_ontology:constraint_beneficiary(ice_memory_archive, future_scientific_capability).
narrative_ontology:constraint_victim(ice_memory_archive, resource_constrained_nations).
narrative_ontology:constraint_victim(ice_memory_archive, climate_sovereignty).
narrative_ontology:constraint_victim(ice_memory_archive, data_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-VULNERABLE NATIONS (SNARE) — Trapped by the urgency imperative and resource constraints. Must contribute ice cores from their territories (Greenland, Antarctica, Himalayan regions) but have no capacity to fund, extract, or archive their own samples. Archive control is transferred to wealthy Northern institutions. d≈0.92, f(d)≈1.39, σ=1.2 → χ≈0.67.
constraint_indexing:constraint_classification(ice_memory_archive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATIONS WITH ICE RESOURCES (TANGLED ROPE) — Constrained by lack of capital and technical capacity; also benefit from the archive infrastructure, international partnerships, and scientific prestige. The extraction is real (data extracted without compensation; archive location decisions made externally) but so is coordination benefit. d≈0.68, f(d)≈1.03, σ=1.1 → χ≈0.56.
constraint_indexing:constraint_classification(ice_memory_archive, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL CONSORTIA (ROPE) — See the archive primarily as a coordination mechanism solving collective action: the tragedy of the commons in glacier preservation. Organized capacity (funds, logistics, scientific protocols) solves a problem no single nation could address. The coordination function is genuine even if unequal participation is permitted. d≈0.42, f(d)≈0.44, σ=1.2 → χ≈0.29.
constraint_indexing:constraint_classification(ice_memory_archive, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: WESTERN RESEARCH INSTITUTIONS (SCAFFOLD) — Extract data, career capital, and control over archive governance, but see this as temporary coordination under time pressure. Climate change and glacier melt create urgency; once digital preservation of ice core data matures, physical archive control becomes less critical. Sunset clause: transition from physical archive exclusivity to distributed digital access as blockchain-verified open datasets replace proprietary archive gates. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(ice_memory_archive, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLICATION AND ACCESS SYSTEM (PITON) — The archive reinforces journal gatekeeping: ice core data is cited but rarely accessed beyond the publishing elite. The 'open science' rhetoric of the archive masks closed-access publication norms. theater_ratio=0.58 captures this: the performance of data stewardship exceeds the function of equitable access. Archive preservation theater exceeds actual access democratization.
constraint_indexing:constraint_classification(ice_memory_archive, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — The civilizational analytical perspective risks naturalizing the archive imperative as a law of physics: 'glaciers are melting, data will be lost, archive is inevitable.' This frames the constraint as immutable. However, base properties (ε=0.52, suppression=0.68, theater=0.58) reveal the mountain as a false summit: the archive is a contingent institutional arrangement, not a natural law. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(ice_memory_archive, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ice_memory_archive_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ice_memory_archive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ice_memory_archive, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ice_memory_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ice_memory_archive, TR),
    TR >= 0.70.

:- end_tests(ice_memory_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52, moderate-high): The archive extracts data from climate-vulnerable regions without proportional compensation or governance participation. Western institutions capture scientific prestige, career capital, and knowledge control. The extraction is not total exploitation (genuine climate science benefit exists) but is substantial enough to meet the tangled rope threshold. The value reflects that the imperative's moral framing (save the climate record) obscures its structural asymmetry. Suppression (0.68, high): Significant barriers limit equitable participation: capital requirements for cryo-sampling, technical infrastructure concentration, geopolitical access to polar regions (Antarctica), institutional prestige differentials. Climate vulnerability creates urgency that suppresses negotiation of terms. Developing nations cannot say 'we'll archive our own ice' because they lack capital and technical capacity; the urgency narrative ('glaciers are melting NOW') forecloses alternative governance arrangements. Theater ratio (0.58, moderate-high): The archive is framed as an emergency response to climate crisis, but much of the activity is performative. The performance of urgency (dramatic messaging, crisis rhetoric, expedited funding) exceeds the actual function of equitable data access. Open-access rhetoric masks journal gatekeeping on ice core publications. The rate of increase in theater ratio (0.32→0.58 over 20 years) reflects that the archive's public-facing urgency performance has outpaced its actual democratization of access.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a stark perspectival gap between beneficiaries and victims. Western institutions and the climate research community see primarily coordination (Rope) — solving a genuine collective action problem with voluntary participation and mutual benefit. Climate-vulnerable nations see extraction (Snare) — their data is taken without consent, governance voice, or benefit-sharing. The open science coalition sees a temporary problem with a sunset (Scaffold) — blockchain and distributed digital preservation will eventually break the centralized archive's control. The publication system sees degraded gatekeeping (Piton) — the 'open data' performance masks closed-access journals. The analytical observer at civilizational scale risks seeing a natural law (Mountain) — 'glaciers are melting, data must be saved' — but the structural data reveals this as a false summit. The urgency is socially constructed through a particular institutional framing, not inherent to physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate-vulnerable nations: Victims + trapped → d≈0.92, f(d)≈1.39. Maximum extraction. No exit option, no voice in archive governance. Developing nations with capacity: Victims + constrained → d≈0.68, f(d)≈1.03. High extraction but some agency through scientific partnerships. International consortia: Beneficiaries + constrained → d≈0.42, f(d)≈0.44. Coordination function generates genuine benefits (scientific capability) alongside extraction (power concentration). Western institutions: Beneficiaries + mobile → d≈0.15, f(d)≈-0.01. Net beneficiary; can exit (redirect to other research) but choose to lead the archive. Publication system: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification comes from theater gate, not directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival; engine flags false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY BY DISAGGREGATING INSTITUTIONAL ACTORS. The archive appears in naive framing as pure coordination (Rope: 'global science cooperation to preserve climate knowledge'). But disaggregating by institutional perspective reveals extraction (Snare from climate-vulnerable nations, Tangled Rope from mid-capacity nations, Rope from Western institutions). The mandatrophy is resolved by recognizing that the 'archive imperative' is voiced by actors with minimal skin in the asymmetry. Climate scientists see coordination; Greenland sees resource extraction. The temporal drift in extractiveness (0.28→0.52) and theater ratio (0.32→0.58) shows that as the archive has matured, its extraction mechanism has become more entrenched and its performative content has increased. The scaffold perspective (sunset via digital distribution) provides a real alternative that would break the extraction mechanism — but Western institutions' control of archive governance means the sunset is not guaranteed unless enforced by external pressure (e.g., data commons regulation, open science mandates).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_sovereignty_vs_data_commons,
    'Does global ice core archiving constitute legitimate climate data commons or neo-colonial extraction of environmental knowledge?',
    'Analysis of governance structure: voting rights, data ownership, benefit-sharing agreements, technology transfer in archive access; comparison with equitable knowledge-sharing frameworks (e.g., Nagoya Protocol, CARE principles)',
    'If commons: constraint is primarily coordination (Rope from more perspectives). If extraction: constraint is primarily asymmetric power transfer (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_sovereignty_vs_data_commons, conceptual, 'Whether archive represents data commons or neo-colonial extraction').

omega_variable(
    ice_core_irreplaceability,
    'Are ice cores the only reliable archive of climate data for the 800ka-4Ma periods, or can alternative proxies (ocean sediments, tree rings, stalagmites) substitute with acceptable loss of resolution or precision?',
    'Paleoclimate reconstruction fidelity comparison: ice core vs non-ice proxies for shared temporal windows; assessment of unique climate parameters only recoverable from ice (aerosol records, atmospheric composition, isotopic ratios)',
    'If irreplaceable: archive creates genuine irreversible loss (extraction justified by data scarcity, suppression acceptable). If substitutable: archive urgency is constructed (extraction not justified by unique value).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ice_core_irreplaceability, empirical, 'Whether ice cores are irreplaceable or substitutable by alternative proxies').

omega_variable(
    digital_preservation_viability,
    'Can distributed digital preservation (blockchain, decentralized storage, institutional mirrors) provide equivalent scientific access without requiring centralized physical archives and associated control structures?',
    'Pilot deployment of digital-first ice core datasets; comparison of discovery, usability, and citation rates for digitally-archived vs physically-archived samples; assessment of digital preservation longevity vs physical archiving (100+ year horizons)',
    'If viable: scaffold sunset is real — physical archive can transition to digital commons, reducing extraction mechanism. If not viable: physical centralization is necessary, extraction mechanism becomes structural and permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_preservation_viability, empirical, 'Whether digital preservation can substitute for physical archives').

omega_variable(
    global_political_will_enforcement,
    'What enforcement mechanism sustains the archive imperative? Does it rely on climate crisis framing, institutional reputation, or coercive funding mechanisms, and which mode of enforcement is most vulnerable to collapse?',
    'Analysis of funding continuity, institutional commitments, geopolitical climate politics; scenario modeling for conditions under which archive funding would be deprioritized (e.g., climate denial political shift, competing resource crises)',
    'If enforcement is reputational: archive survives climate-skeptical political periods. If enforcement is funding-dependent on crisis framing: archive is vulnerable to narrative shift or political reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_political_will_enforcement, preference, 'What enforcement mechanism sustains the archive imperative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ice_memory_archive, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ice_mem_tr_t0, ice_memory_archive, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ice_mem_tr_t10, ice_memory_archive, theater_ratio, 10, 0.45).
narrative_ontology:measurement(ice_mem_tr_t20, ice_memory_archive, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(ice_mem_be_t0, ice_memory_archive, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ice_mem_be_t10, ice_memory_archive, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(ice_mem_be_t20, ice_memory_archive, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ice_memory_archive, global_infrastructure).
narrative_ontology:boltzmann_floor_override(ice_memory_archive, 0.45).
narrative_ontology:affects_constraint(ice_memory_archive, climate_data_sovereignty).
narrative_ontology:affects_constraint(ice_memory_archive, knowledge_commons_extraction).

% DUAL FORMULATION NOTE:
% The ice core archive is downstream of the broader climate science framework and knowledge extraction from vulnerable regions. The archive represents a specific instantiation of how climate crisis urgency can be mobilized to justify asymmetric data governance. Related constraints: knowledge_commons_extraction (general pattern of scientific data extraction from developing regions) and climate_data_sovereignty (political right to control environmental data from one's territory).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ice_memory_archive, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
