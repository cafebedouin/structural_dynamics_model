% ============================================================================
% CONSTRAINT STORY: neuroimaging_technology_accessibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_neuroimaging_technology_accessibility, []).

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
 *   constraint_id: neuroimaging_technology_accessibility
 *   human_readable: Neuroimaging Technology Accessibility and Research Equity
 *   domain: neuroscience/research_infrastructure/global_health
 *
 * SUMMARY:
 *   Neuroimaging technology accessibility creates a structural constraint
 *   between the capital requirements of cutting-edge brain imaging equipment
 *   and the global distribution of research capacity. The constraint operates
 *   through infrastructure gatekeeping: multi-million-dollar scanners (fMRI,
 *   MEG, high-field MRI) are concentrated in wealthy institutions and
 *   nations, creating asymmetric access to research capability. Well-funded
 *   institutions benefit from exclusive access and scientific priority;
 *   underfunded researchers face career barriers due to equipment
 *   inaccessibility. The constraint exhibits genuine coordination functions
 *   (multi-site data sharing enables larger studies, standardized protocols
 *   improve reproducibility) alongside asymmetric extraction (capital access
 *   determines research opportunity). Over the interval (2010-2026),
 *   extractiveness has increased as research has become more data-intensive
 *   and equipment more expensive, while theater has increased as
 *   institutional equity commitments have proliferated as performative
 *   responses without proportional resource allocation. Open-source
 *   neuroimaging tools and distributed computing models represent genuine
 *   technological alternatives maturing under scaffold logic — as these
 *   alternatives reach functional parity with commercial systems, the
 *   equipment access monopoly weakens.
 *
 * KEY AGENTS:
 *   - Well-funded research institutions (e.g., MIT, Harvard, Max Planck): Institutional/arbitrage — primary beneficiary, captures scientific priority and first-author advantages
 *   - Underfunded researchers in low-resource settings: Powerless/trapped — primary victim, lacks capital access, cannot advance neuroimaging research career
 *   - Middle-income institutional research groups: Moderate/constrained — secondary victim with some agency, can access through consortia partnerships but faces disproportionate burden
 *   - Equipment manufacturers (Siemens, Philips, GE, OMEGA): Institutional/arbitrage — secondary beneficiary, maintains revenue from capital scarcity and replacement cycles
 *   - Open Neuroscience Coalition (INCF, software developers, federated computing projects): Organized/constrained — builds alternative pathways with sunset logic
 *   - Traditional funding gatekeepers (NIH review systems): Institutional/arbitrage — maintains structural barriers while performing equity commitment (Piton perspective)
 *   - Analytical observer: Analytical/analytical — recognizes constraint as genuine tangled rope with real coordination and real extraction mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(neuroimaging_technology_accessibility, 0.58).
domain_priors:suppression_score(neuroimaging_technology_accessibility, 0.62).
domain_priors:theater_ratio(neuroimaging_technology_accessibility, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(neuroimaging_technology_accessibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(neuroimaging_technology_accessibility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(neuroimaging_technology_accessibility, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(neuroimaging_technology_accessibility, tangled_rope).
narrative_ontology:human_readable(neuroimaging_technology_accessibility, "Neuroimaging Technology Accessibility and Research Equity").
narrative_ontology:topic_domain(neuroimaging_technology_accessibility, "neuroscience/research_infrastructure/global_health").

domain_priors:requires_active_enforcement(neuroimaging_technology_accessibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(neuroimaging_technology_accessibility, well_funded_research_institutions).
narrative_ontology:constraint_beneficiary(neuroimaging_technology_accessibility, imaging_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(neuroimaging_technology_accessibility, wealthy_nations_neuroscience_communities).
narrative_ontology:constraint_victim(neuroimaging_technology_accessibility, underfunded_researchers).
narrative_ontology:constraint_victim(neuroimaging_technology_accessibility, lower_income_countries).
narrative_ontology:constraint_victim(neuroimaging_technology_accessibility, patient_populations_in_low_resource_settings).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERFUNDED RESEARCHER (SNARE) — Trapped by capital and infrastructure barriers. Cannot conduct cutting-edge neuroimaging research without access to multi-million-dollar equipment. No alternative pathways for individual researchers. Bears extraction cost (cannot advance career without institutional resources they cannot obtain) with minimal coordination benefit. Maximum perceived extractiveness.
constraint_indexing:constraint_classification(neuroimaging_technology_accessibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-INCOME INSTITUTION (TANGLED ROPE) — Faces high capital costs and maintenance barriers but can pursue access through consortium partnerships and shared facility models. Benefits from coordination mechanisms (multi-site research networks, equipment sharing protocols) while bearing disproportionate labor and infrastructure costs. Constrained exit due to career/funding pressure. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(neuroimaging_technology_accessibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WELL-FUNDED INSTITUTION (ROPE) — Benefits from exclusive access to state-of-the-art equipment. Experiences the constraint as coordination mechanism: their data collection enables global research networks, and they capture first-mover scientific advantage. Net beneficiary. Can exit through private equipment investment if desired. Experiences constraint as manageable coordination challenge with clear benefits.
constraint_indexing:constraint_classification(neuroimaging_technology_accessibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EQUIPMENT MANUFACTURER (ROPE) — Benefits from capital scarcity and high replacement costs. High-cost equipment creates stable revenue streams and reduces competition. Experiences the constraint as coordination mechanism: standardization of protocols, user training networks, and maintenance contracts. Net beneficiary with low extraction cost.
constraint_indexing:constraint_classification(neuroimaging_technology_accessibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN NEUROSCIENCE COALITION (SCAFFOLD) — Organized agents (open-source neuroimaging software initiatives, equipment consortia, shared data repositories, federated computing networks) are building alternative pathways that bypass exclusive institutional access. These approaches have sunset logic: as open-source tools mature (SPM, FSL, AFNI, MNE-Python alternatives) and distributed computing models scale, the extraction mechanism of exclusive equipment access loses force. Estimated sunset: 15-20 years as cloud-based neuroimaging and open-source pipelines mature.
constraint_indexing:constraint_classification(neuroimaging_technology_accessibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL FUNDING GATEKEEPING (PITON) — The NIH peer review system for neuroimaging access is largely performative: reviewers assess novelty and feasibility on paper but cannot verify access feasibility for researchers from under-resourced institutions. The system produces the theater of 'equal opportunity' while maintaining structural barriers. Increasingly seen by its operators as degraded — access equity calls and alternative funding models proliferate because the traditional system is acknowledged as insufficient. Maintains itself through institutional inertia despite recognized inadequacy.
constraint_indexing:constraint_classification(neuroimaging_technology_accessibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, neuroimaging technology creates genuine coordination benefits (standardized protocols enable multi-site research, shared data repositories accelerate discovery) alongside asymmetric extraction (capital access determines research opportunity, marginalizing under-resourced researchers). The constraint has real coordination function AND real extraction mechanism. Neither can be eliminated without eliminating the other. Tangled rope from the full structural view.
constraint_indexing:constraint_classification(neuroimaging_technology_accessibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(neuroimaging_technology_accessibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(neuroimaging_technology_accessibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(neuroimaging_technology_accessibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(neuroimaging_technology_accessibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(neuroimaging_technology_accessibility, TR),
    TR >= 0.70.

:- end_tests(neuroimaging_technology_accessibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated-high. The capital barrier creates genuine extraction: researchers without equipment access cannot conduct frontier neuroimaging research regardless of scientific merit. The extraction is not total (alternative pathways exist, consortium access is possible) but substantial enough to shape career trajectories. The value has increased over the interval as equipment has become more expensive and data requirements more stringent. Suppression (0.62): Moderate-high. Barriers include capital costs ($2-10M per scanner), facility maintenance, technical expertise requirements, and institutional access protocols. But these are not absolute — some barriers are surmountable through consortium partnerships, cloud computing, and mobile neuroimaging. Suppression persists but with some permeability. Theater ratio (0.55): Moderate. Institutional commitments to access equity (shared facilities, partnership programs, open data initiatives) are partially genuine but also contain performative elements. The theater has increased over the interval as equity rhetoric has grown while structural barriers persist. The constraint requires active enforcement through access policies and consortia maintenance, confirming Tangled Rope classification.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary (Rope) and victim (Snare) perspectives reveals the constraint's hybrid nature. Beneficiaries experience it as manageable coordination with clear research benefits. Victims experience it as insurmountable capital barrier. The gap width indicates that real extraction is occurring — if the constraint were pure coordination (Rope from all views), all perspectives would converge. If it were pure extraction (Snare from all views), beneficiaries would also perceive extraction (organized resistance). Instead, beneficiaries see benefits while victims see barriers — diagnostic of Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position: power level, exit options, and beneficiary/victim status. Well-funded institutions (institutional/arbitrage/beneficiary) derive low d → experienced extraction runs toward them (negative chi). Underfunded researchers (powerless/trapped/victim) derive high d → maximum experienced extraction runs away from them (high chi). Middle-income institutions (moderate/constrained/victim) derive mid-range d → moderate experienced extraction. The open science coalition (organized/constrained/victim-to-beneficiary-in-future) derives mid-range d with sunset trajectory. The sigmoid f(d) amplifies experienced extractiveness for high-d agents (trapped powerless) and dampens it for low-d agents (arbitrage institutional). Scope modifier σ(S) scales extractiveness globally (σ=1.2) — the barrier affects global research equity, not just local access.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint is neither pure extraction nor pure coordination. It is Tangled Rope because: (1) Genuine coordination functions exist — multi-site research networks require standardization, data sharing enables larger studies, open-source tools reduce barriers. (2) Asymmetric extraction exists in parallel — capital access determines who can participate in frontier research, well-funded institutions capture scientific priority, equipment monopolies persist despite open-access rhetoric. (3) Active enforcement maintains the hybrid — consortium policies, access agreements, funding decisions all actively maintain both the coordination function and the extraction asymmetry. The mandatrophy resolves by recognizing that scaffold alternatives (open-source, distributed computing, mobile devices) are genuinely maturing and could eventually decouple the coordination from the extraction — if those alternatives reach sufficient technical parity, the equipment monopoly weakens. Until that threshold is crossed, the constraint remains Tangled Rope with a real sunset mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_source_viability_threshold,
    'At what computational capability threshold do open-source neuroimaging tools become genuine functional substitutes for commercial equipment software, versus remaining limited-use approximations?',
    'Comparative analysis of research outcomes using open-source vs commercial pipelines; replication studies using same datasets through both pathways; adoption rates among resource-constrained institutions',
    'If threshold crossed in next 5 years: scaffold perspective confirmed, sunset accelerates. If threshold remains > 10 years: open-source alternatives remain niche, extraction mechanism persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_viability_threshold, empirical, 'Viability threshold for open-source neuroimaging as functional alternative').

omega_variable(
    distributed_computing_model_scalability,
    'Can federated learning and distributed neuroimaging analysis on cloud infrastructure achieve the statistical power and validation standards of centralized equipment-based approaches?',
    'Multi-site federated studies comparing statistical power and artifact detection; validation against gold-standard centralized protocols; cost analysis of distributed vs centralized approaches',
    'If scalable to equivalent standards: alternative pathway matures (Scaffold), equipment access monopoly weakens. If persistent limitations: distributed model remains supplementary, equipment access remains gating constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_computing_model_scalability, empirical, 'Scalability of distributed computing models for neuroimaging analysis').

omega_variable(
    mobile_neuroimaging_technical_readiness,
    'Can portable neuroimaging technologies (mobile EEG, accessible fMRI devices, wearable sensors) achieve sufficient resolution and standardization to democratize brain research access?',
    'Technical capability assessment of mobile devices relative to clinical standards; validation studies comparing mobile vs fixed neuroimaging outcomes; adoption and sustainability in low-resource settings',
    'If mobile systems achieve 80%+ capability parity: access bottleneck loosens significantly (transforms from snare to constrained for underfunded researchers). If mobile systems remain limited to niche applications: centralized equipment remains gating constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobile_neuroimaging_technical_readiness, empirical, 'Technical readiness of mobile and accessible neuroimaging technologies').

omega_variable(
    institutional_equity_commitment_persistence,
    'Are institutional commitments to neuroimaging access equity (shared facility partnerships, equipment consortia, cross-border collaborations) sustained commitments with genuine resource allocation, or performative compliance theater?',
    'Longitudinal funding analysis for equity initiatives; comparison of stated access targets vs actual utilization by under-resourced researchers; tracking of consortium sustainability vs dissolution rates',
    'If genuine commitment: barrier reduction mechanisms strengthen (Scaffold strengthens). If performative: theater increases while barriers persist (Piton classification strengthens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_equity_commitment_persistence, empirical, 'Authenticity of institutional equity commitment to neuroimaging access').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(neuroimaging_technology_accessibility, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neuro_tr_t0, neuroimaging_technology_accessibility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(neuro_tr_t8, neuroimaging_technology_accessibility, theater_ratio, 8, 0.48).
narrative_ontology:measurement(neuro_tr_t16, neuroimaging_technology_accessibility, theater_ratio, 16, 0.55).
narrative_ontology:measurement(neuro_tr_t4, neuroimaging_technology_accessibility, theater_ratio, 4, 0.4).

% Extraction over time
narrative_ontology:measurement(neuro_be_t0, neuroimaging_technology_accessibility, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(neuro_be_t8, neuroimaging_technology_accessibility, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(neuro_be_t16, neuroimaging_technology_accessibility, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(neuro_be_t4, neuroimaging_technology_accessibility, base_extractiveness, 4, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(neuroimaging_technology_accessibility, resource_allocation).
narrative_ontology:boltzmann_floor_override(neuroimaging_technology_accessibility, 0.18).
narrative_ontology:affects_constraint(neuroimaging_technology_accessibility, research_publication_access).
narrative_ontology:affects_constraint(neuroimaging_technology_accessibility, global_neuroscience_workforce_equity).

% DUAL FORMULATION NOTE:
% Neuroimaging accessibility is downstream of both research infrastructure investment decisions and equipment manufacturing economics. It is upstream of research publication access and workforce development in neuroscience. The constraint family includes separate stories for equipment monopoly dynamics (pure extraction mechanics) vs. data-sharing coordination (pure coordination mechanics), linked by network decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(neuroimaging_technology_accessibility, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
