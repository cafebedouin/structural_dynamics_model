% ============================================================================
% CONSTRAINT STORY: digital_colonialism_data_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_colonialism_data_extraction, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_colonialism_data_extraction
 *   human_readable: Digital Colonialism: Data Extraction from Global South
 *   domain: technology_ethics/political_theology/global_health
 *
 * SUMMARY:
 *   Digital colonialism in health data extraction replicates historical
 *   colonial patterns through technological infrastructure: Northern tech
 *   actors, pharmaceutical companies, and AI research institutions extract
 *   health, genetic, and demographic data from Global South populations,
 *   creating new dependencies while concentrating benefits in the North. The
 *   constraint operates through digitized health systems, mobile health
 *   applications, genomic research partnerships, and AI training datasets.
 *   Data flows from low-resource to high-resource regions; ownership and
 *   control of aggregated datasets remain with Northern institutions;
 *   benefit-sharing arrangements are minimal or performative. The structure
 *   exhibits rising extraction (0.55 → 0.78 over 12 years) and rising theater
 *   (0.42 → 0.68) as consent frameworks and partnership rhetoric proliferate
 *   without changing underlying power asymmetries. Suppression has
 *   intensified (0.68 → 0.82) as infrastructure lock-in deepens and
 *   alternative healthcare pathways remain underdeveloped. From a Catholic
 *   Social Teaching analytical frame, this constraint violates core
 *   principles: human dignity (persons as data sources), common good
 *   (concentrated benefits), subsidiarity (no local control), solidarity
 *   (asymmetric extraction), and justice (no reciprocity). The technocratic
 *   paradigm critique applies directly: persons treated as resources to be
 *   optimized rather than ends in themselves.
 *
 * KEY AGENTS:
 *   - Global South Populations: Primary victims (powerless/trapped) — health data extracted through infrastructure dependency; no meaningful exit; minimal benefit return
 *   - Local Health Systems: Secondary victims (moderate/constrained) — accept Northern platforms for funding access; lose data sovereignty; costly exit
 *   - Northern Tech Actors: Primary beneficiaries (institutional/arbitrage) — capture data ownership, IP rights, algorithmic control; can exit any specific partnership without market loss
 *   - Pharmaceutical Companies: Primary beneficiaries (institutional/arbitrage) — access diverse genetic data for drug development; no reciprocal benefit-sharing; extraction naturalized as research coordination
 *   - Data Sovereignty Coalitions: Organized resistance (organized/constrained) — build alternative frameworks but face network effects and capital barriers; mixed coordination/extraction experience
 *   - International Health Organizations: Ambiguous intermediaries (institutional/constrained) — coordinate global health data initiatives while facilitating Northern extraction; mission-locked into extractive partnership models
 *   - Open Data Commons Movement: Organized alternative-builders (organized/mobile) — see current extraction as temporary; building data trusts, commons ownership, benefit-sharing protocols with sunset logic
 *   - CST Analytical Frame: Civilizational observer (analytical/analytical) — sees pure extraction masked by development rhetoric; coordination story is cover; technocratic paradigm reduces persons to data resources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_colonialism_data_extraction, 0.78).
domain_priors:suppression_score(digital_colonialism_data_extraction, 0.82).
domain_priors:theater_ratio(digital_colonialism_data_extraction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_colonialism_data_extraction, extractiveness, 0.78).
narrative_ontology:constraint_metric(digital_colonialism_data_extraction, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(digital_colonialism_data_extraction, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_colonialism_data_extraction, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(digital_colonialism_data_extraction, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_colonialism_data_extraction, snare).
narrative_ontology:human_readable(digital_colonialism_data_extraction, "Digital Colonialism: Data Extraction from Global South").
narrative_ontology:topic_domain(digital_colonialism_data_extraction, "technology_ethics/political_theology/global_health").

domain_priors:requires_active_enforcement(digital_colonialism_data_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_colonialism_data_extraction, '0be6fde6-b862-40df-8944-350a93193685').
narrative_ontology:cs_kernel_codification('0be6fde6-b862-40df-8944-350a93193685', formalized).
narrative_ontology:cs_authority_grounding('0be6fde6-b862-40df-8944-350a93193685', lineage).
narrative_ontology:cs_interpretation_layer_present('0be6fde6-b862-40df-8944-350a93193685').
narrative_ontology:cs_created_at('0be6fde6-b862-40df-8944-350a93193685', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_colonialism_data_extraction, northern_tech_actors).
narrative_ontology:constraint_beneficiary(digital_colonialism_data_extraction, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(digital_colonialism_data_extraction, ai_research_institutions).
narrative_ontology:constraint_victim(digital_colonialism_data_extraction, global_south_populations).
narrative_ontology:constraint_victim(digital_colonialism_data_extraction, local_health_systems).
narrative_ontology:constraint_victim(digital_colonialism_data_extraction, indigenous_communities).
narrative_ontology:constraint_vindicates(digital_colonialism_data_extraction, data_as_natural_resource_doctrine).
narrative_ontology:constraint_vindicates(digital_colonialism_data_extraction, technological_inevitability_thesis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SOUTH POPULATIONS (SNARE) — Trapped by infrastructure dependency, lack of alternative healthcare pathways, and asymmetric information about data use. Health/genetic/demographic data extracted through digitized health systems, mobile health apps, and genomic research partnerships with no meaningful consent infrastructure. Cannot exit: alternative healthcare often unavailable, and data extraction is bundled with service access. Maximum extraction: data flows North, benefits flow minimally or not at all back to source communities.
constraint_indexing:constraint_classification(digital_colonialism_data_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL HEALTH SYSTEMS (SNARE) — Constrained by funding dependencies and technology lock-in. Accept Northern digital health platforms and data-sharing agreements to access resources, but lose control over patient data and algorithmic governance. Exit costly: switching platforms requires infrastructure investment and risks losing donor funding. Substantial extraction: data sovereignty traded for operational capacity.
constraint_indexing:constraint_classification(digital_colonialism_data_extraction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DATA SOVEREIGNTY COALITIONS (TANGLED ROPE) — Organized advocacy groups (African Union data policy initiatives, Latin American digital rights networks) see both coordination function (regional data governance frameworks) and extraction (Northern platforms dominate despite local regulation). Constrained exit: can build alternative infrastructure but face network effects and capital barriers. Mixed experience: some agency to resist, but extraction persists through structural power asymmetries.
constraint_indexing:constraint_classification(digital_colonialism_data_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: NORTHERN TECH ACTORS (ROPE) — Primary beneficiaries with arbitrage-level exit. Experience the constraint as coordination: data partnerships enable AI model training, drug development, and precision medicine research. Can exit any specific partnership without losing market position. Net beneficiary: extraction flows toward this agent through data ownership, intellectual property claims, and algorithmic control.
constraint_indexing:constraint_classification(digital_colonialism_data_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PHARMACEUTICAL COMPANIES (ROPE) — Benefit from access to diverse genetic datasets for drug development without reciprocal benefit-sharing. Arbitrage exit: can source data from multiple regions and jurisdictions. Experience as coordination: data access enables research that 'benefits humanity.' Extraction invisible from this position: the asymmetry in who captures value from resulting therapies is naturalized as market efficiency.
constraint_indexing:constraint_classification(digital_colonialism_data_extraction, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN DATA COMMONS MOVEMENT (SCAFFOLD) — Organized actors building alternative data governance models (data trusts, commons-based ownership, benefit-sharing protocols). See current extraction as temporary coordination failure with sunset logic: as Global South nations develop data sovereignty infrastructure and international data governance norms mature, the extractive asymmetry will diminish. Mobile exit: can shift advocacy focus as norms evolve. Sunset mechanism: combination of regional data protection laws, WHO benefit-sharing frameworks, and indigenous data sovereignty protocols.
constraint_indexing:constraint_classification(digital_colonialism_data_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL HEALTH ORGANIZATIONS (TANGLED ROPE) — WHO, Gates Foundation, Wellcome Trust occupy ambiguous position: coordinate global health data initiatives (genuine coordination function) while also facilitating Northern data access through partnership structures that embed extraction. Constrained exit: mission-locked into existing partnership models; shifting to data sovereignty frameworks risks losing Northern funding and political support. Mixed experience: see both the coordination value and the structural asymmetry but are institutionally captured by the current arrangement.
constraint_indexing:constraint_classification(digital_colonialism_data_extraction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: CST ANALYTICAL FRAME (SNARE) — From the civilizational/universal analytical position grounded in Catholic Social Doctrine, this constraint violates multiple core principles: human dignity (persons reduced to data sources), common good (benefits concentrated in North), subsidiarity (local communities lack decision-making power over their data), solidarity (asymmetric extraction contradicts preferential option for poor), and justice (no reciprocal benefit-sharing). The analytical frame sees this as pure extraction masked by development rhetoric. The 'coordination' story (data partnerships enable global health research) is cover for a structure that systematically transfers value from vulnerable populations to powerful institutions. CST's technocratic paradigm critique applies directly: the constraint treats persons as resources to be optimized rather than as ends in themselves.
constraint_indexing:constraint_classification(digital_colonialism_data_extraction, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_colonialism_data_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_colonialism_data_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_colonialism_data_extraction, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_colonialism_data_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_colonialism_data_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. Northern institutions capture data ownership, intellectual property, and algorithmic control. Benefits (precision medicine, AI diagnostics, drug development) flow minimally back to source populations. The value asymmetry is structural: data extracted from populations with limited healthcare access enables products marketed primarily to high-resource populations. Rising trajectory (0.55 → 0.78) reflects deepening extraction as AI training datasets scale and genomic databases expand. Suppression (0.82): High. Infrastructure dependency creates coercive consent conditions: data extraction bundled with healthcare access. Alternative pathways (local health systems without Northern platform integration) are underdeveloped or unavailable. Exit costly or impossible for individuals and constrained for institutions. Rising trajectory (0.68 → 0.82) reflects infrastructure lock-in and donor funding dependencies. Theater ratio (0.68): Moderate-high. Consent frameworks, ethical review boards, and partnership agreements proliferate, but these processes rarely change underlying power asymmetries or benefit flows. Consent is often formal rather than meaningful given structural dependencies. Rising trajectory (0.42 → 0.68) reflects the gap between governance rhetoric and actual practice. Accessibility collapse (0.35): Low-moderate. Alternatives exist in principle (local data governance, indigenous sovereignty frameworks, regional data protection laws) but face network effects and capital barriers. The constraint is not naturalized as inevitable — resistance is organized and visible. Resistance (0.62): Moderate-high. Data sovereignty coalitions, indigenous rights movements, and regional policy initiatives actively contest the extraction. The constraint requires active enforcement (platform terms of service, IP law, funding conditionalities) and meets substantial pushback.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Northern tech actors and pharmaceutical companies experience rope (coordination enabling research) with negative or minimal effective extraction — they are net beneficiaries. Global South populations and local health systems experience snare (pure extraction with no meaningful exit) with maximum effective extraction. Data sovereignty coalitions experience tangled_rope (mixed coordination and extraction with constrained agency). International health organizations occupy an ambiguous institutional position: they coordinate global health initiatives (genuine function) while facilitating extraction through partnership structures that embed Northern control. The open data commons movement sees scaffold (temporary problem with sunset logic as alternative governance models mature). The CST analytical frame sees snare from the civilizational/universal position: the coordination story is cover for systematic value transfer that violates human dignity, common good, subsidiarity, solidarity, and justice. The gap between the beneficiary's rope and the victim's snare is the measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Northern tech actors and pharmaceutical companies are declared beneficiaries with arbitrage exit → low d → negative or minimal chi (they experience subsidy, not extraction). Global South populations are declared victims with trapped exit → high d → maximum chi (they bear full extraction). Local health systems are victims with constrained exit → high d modulated by some agency → substantial chi. Data sovereignty coalitions are neither pure beneficiaries nor pure victims (they coordinate regional governance while resisting extraction) with constrained exit → moderate d → moderate chi. International health organizations are beneficiaries (they coordinate global health) but also partly captured (mission-locked into extractive partnerships) with constrained exit → override to moderate d reflecting their ambiguous position. Open data commons movement are beneficiaries (building alternatives) with mobile exit → low d → low chi. CST analytical frame has analytical exit → d derived from the frame's structural position as observer of extraction, not participant.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the coordination function (data partnerships enable global health research) and the extraction function (value transfer from South to North) are structurally inseparable in the current arrangement. The tangled_rope classification from organized and institutional perspectives captures this: there IS a genuine coordination problem (global health research requires diverse datasets), AND there is asymmetric extraction (benefits concentrate in the North, costs and risks concentrate in the South). The snare classification from powerless and analytical perspectives is also structurally accurate: from the position of those with no exit, the coordination story is cover — the constraint operates as pure extraction. The scaffold perspective (open data commons movement) identifies the sunset mechanism: alternative governance models (data trusts, commons ownership, benefit-sharing protocols, indigenous sovereignty frameworks) are maturing and could dissolve the extractive asymmetry. The mandate (enable global health research) has not outlived its function, but the current implementation (Northern platform dominance, minimal benefit-sharing, coercive consent) is extractive. Mandatrophy is not resolved in the sense of 'the constraint should end' but in the sense of 'the perspectival structure is fully specified and the extraction/coordination distinction is measurable.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_infrastructure_threshold,
    'At what level of consent infrastructure does data extraction shift from coercive to voluntary? Can meaningful consent exist within structural dependency?',
    'Empirical study of consent practices in low-resource settings; philosophical analysis of autonomy under conditions of necessity; comparison of data governance outcomes in contexts with/without alternative healthcare access',
    'If meaningful consent is possible within dependency: some data partnerships reclassify as tangled_rope (mixed coordination/extraction). If consent requires genuine alternatives: all partnerships within dependency contexts remain snare (pure extraction with consent theater).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_infrastructure_threshold, conceptual, 'Whether meaningful consent can exist within structural healthcare dependency').

omega_variable(
    benefit_sharing_sufficiency,
    'What constitutes adequate benefit-sharing? Is capacity-building (training, infrastructure) sufficient reciprocity, or does justice require co-ownership of resulting intellectual property and profits?',
    'Normative analysis grounded in CST principles of distributive justice and common good; empirical tracking of value flows in existing benefit-sharing arrangements; comparison with natural resource extraction benefit-sharing frameworks',
    'If capacity-building suffices: some partnerships reclassify as tangled_rope. If co-ownership required: current arrangements remain snare regardless of capacity-building investments. CST''s preferential option for the poor suggests the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_sharing_sufficiency, preference, 'What level of benefit-sharing satisfies justice requirements').

omega_variable(
    data_sovereignty_effectiveness,
    'Can regional data protection laws and sovereignty frameworks actually constrain Northern extraction, or do network effects and capital asymmetries render legal sovereignty merely formal?',
    'Longitudinal analysis of data governance outcomes in jurisdictions with strong data protection laws (GDPR, African Union data policy); assessment of enforcement capacity and cross-border data flow patterns; evaluation of whether legal frameworks change actual extraction or merely add compliance theater',
    'If effective: scaffold perspective confirmed — sunset is structurally real. If ineffective: legal sovereignty is theater, and extraction persists regardless of formal protections (scaffold collapses to snare with higher theater_ratio).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_sovereignty_effectiveness, empirical, 'Whether data sovereignty frameworks can constrain extraction or merely add compliance theater').

omega_variable(
    indigenous_data_sovereignty_recognition,
    'Do indigenous data sovereignty principles (CARE principles, OCAP framework) represent a genuinely alternative governance model, or are they being absorbed into extractive partnerships as legitimation theater?',
    'Case studies of indigenous data governance implementation; analysis of whether CARE/OCAP principles change data ownership and control or function as consent-washing; tracking of indigenous community outcomes in partnerships claiming to follow these frameworks',
    'If genuinely alternative: indigenous communities have mobile exit options, reducing their experienced extraction. If absorbed as theater: indigenous sovereignty claims become part of the extraction mechanism (higher theater_ratio, no change in chi for indigenous communities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_data_sovereignty_recognition, empirical, 'Whether indigenous data sovereignty frameworks provide genuine alternative or legitimation theater').

omega_variable(
    cst_doctrinal_development_necessity,
    'Does digital colonialism require NEW Catholic Social Doctrine specifically addressing data as a category of human dignity concern, or is APPLICATION of existing principles (dignity, common good, justice) sufficient?',
    'Theological analysis of whether data extraction creates novel moral questions not addressed by existing CST framework; examination of whether Laudato Si'' and Fratelli Tutti principles extend to digital contexts without remainder; assessment of whether Magisterial teaching authority has already developed adequate doctrine or whether res nova status requires further development',
    'If new doctrine required: the constraint reveals a gap in CST''s interpretive capacity (the tradition is catching up to technological reality). If existing principles sufficient: the constraint is already fully addressed by CST, and the problem is APPLICATION failure, not doctrinal inadequacy. This omega directly addresses the commitment system recognition''s finding that CST presents a unified position rather than contested readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cst_doctrinal_development_necessity, conceptual, 'Whether digital colonialism requires new CST doctrine or application of existing principles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_colonialism_data_extraction, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digcol_theater_2010, digital_colonialism_data_extraction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(digcol_theater_2013, digital_colonialism_data_extraction, theater_ratio, 3, 0.51).
narrative_ontology:measurement(digcol_theater_2016, digital_colonialism_data_extraction, theater_ratio, 6, 0.58).
narrative_ontology:measurement(digcol_theater_2019, digital_colonialism_data_extraction, theater_ratio, 9, 0.64).
narrative_ontology:measurement(digcol_theater_2022, digital_colonialism_data_extraction, theater_ratio, 12, 0.68).

% Extraction over time
narrative_ontology:measurement(digcol_extract_2010, digital_colonialism_data_extraction, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(digcol_extract_2013, digital_colonialism_data_extraction, base_extractiveness, 3, 0.64).
narrative_ontology:measurement(digcol_extract_2016, digital_colonialism_data_extraction, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(digcol_extract_2019, digital_colonialism_data_extraction, base_extractiveness, 9, 0.76).
narrative_ontology:measurement(digcol_extract_2022, digital_colonialism_data_extraction, base_extractiveness, 12, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(digcol_suppress_2010, digital_colonialism_data_extraction, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(digcol_suppress_2016, digital_colonialism_data_extraction, suppression_requirement, 6, 0.76).
narrative_ontology:measurement(digcol_suppress_2022, digital_colonialism_data_extraction, suppression_requirement, 12, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_colonialism_data_extraction, resource_allocation).
narrative_ontology:affects_constraint(digital_colonialism_data_extraction, ai_governance_accountability).

% DUAL FORMULATION NOTE:
% This constraint is downstream of ai_governance_accountability (the upstream tangled_rope around AI governance structures). The governance vacuum enables the extractive data flows: lack of enforceable international data governance allows Northern actors to operate with minimal accountability. The two constraints are structurally linked but have distinct epsilon values: ai_governance_accountability has moderate extraction (0.45-0.55 range, governance coordination with embedded capture), while digital_colonialism_data_extraction has high extraction (0.78, systematic value transfer). They should be modeled as separate stories with a network edge, not collapsed into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_colonialism_data_extraction, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
