% ============================================================================
% CONSTRAINT STORY: neuroimaging_standardization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_neuroimaging_standardization, []).

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
 *   constraint_id: neuroimaging_standardization
 *   human_readable: Neuroimaging Standardization: Coordination vs. Vendor Lock-In
 *   domain: neuroscience/biomedical_technology
 *
 * SUMMARY:
 *   Neuroimaging standardization creates a structural tension between the
 *   genuine coordination problem (incomparable datasets from different
 *   hardware/software configurations) and vendor extraction mechanisms that
 *   have entrenched themselves within standardization requirements. The
 *   constraint exhibits features of both rope (coordination enabling
 *   multi-site collaboration) and snare (vendor lock-in trapping
 *   resource-limited actors). The theater_ratio rising from 0.42 to 0.58
 *   reflects increasing performative compliance with standardization
 *   protocols that create appearance of methodological rigor without
 *   guaranteeing reproducibility. Early-career researchers and
 *   resource-limited institutions face extraction through proprietary
 *   software licensing, vendor-specific preprocessing requirements, and
 *   inability to access established scanning infrastructure. Established
 *   research groups and equipment manufacturers benefit through network
 *   effects and market concentration. The open neuroimaging movement
 *   (OpenNeuro, BIDS standard, open-source alternatives) represents a genuine
 *   sunset mechanism — as open standards mature and funding agencies mandate
 *   open formats, the vendor-imposed extraction architecture loses structural
 *   force. The classification as tangled_rope reflects that standardization
 *   simultaneously performs coordination (enabling multi-site research) and
 *   extraction (locking actors into vendor ecosystems). The field's epistemic
 *   reliability is a structured victim — methodological replicability depends
 *   on standardization fidelity, but standardization ambiguities and
 *   vendor-specific variations systematically degrade replication success
 *   rates.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary victims (powerless/trapped) — lack resources for proprietary software and established facility access; cannot exit without abandoning neuroimaging research
 *   - Resource-Limited Institutions: Secondary victims (moderate/constrained) — coordinate through standardization but face extraction via vendor licensing and technical support costs
 *   - Equipment Manufacturers: Primary beneficiaries (institutional/arbitrage) — benefit from standardization through network effects; can arbitrage between research, clinical, and commercial markets
 *   - Established Research Groups: Secondary beneficiaries (powerful/constrained) — benefit from coordination and wield influence over which standards become canonical; create path-dependency in preprocessing choices
 *   - Open Neuroimaging Movement: Organized agents (organized/mobile) — building sunset pathways via OpenNeuro, BIDS, open-source alternatives; developing technical exit routes from vendor lock-in
 *   - Field Methodological Reliability: Primary victim (powerless/trapped) — abstract collective good; bears cost of standardization ambiguities and vendor-specific variations that degrade reproducibility
 *   - Journal Publication System: Institutional actor (institutional/arbitrage) — maintains standardization enforcement through methods requirements; enforcement is largely performative (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(neuroimaging_standardization, 0.48).
domain_priors:suppression_score(neuroimaging_standardization, 0.52).
domain_priors:theater_ratio(neuroimaging_standardization, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(neuroimaging_standardization, extractiveness, 0.48).
narrative_ontology:constraint_metric(neuroimaging_standardization, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(neuroimaging_standardization, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(neuroimaging_standardization, tangled_rope).
narrative_ontology:human_readable(neuroimaging_standardization, "Neuroimaging Standardization: Coordination vs. Vendor Lock-In").
narrative_ontology:topic_domain(neuroimaging_standardization, "neuroscience/biomedical_technology").

domain_priors:requires_active_enforcement(neuroimaging_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(neuroimaging_standardization, equipment_manufacturers).
narrative_ontology:constraint_beneficiary(neuroimaging_standardization, established_research_groups).
narrative_ontology:constraint_victim(neuroimaging_standardization, early_career_researchers).
narrative_ontology:constraint_victim(neuroimaging_standardization, resource_limited_institutions).
narrative_ontology:constraint_victim(neuroimaging_standardization, methodological_replicability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER RESEARCHER (SNARE) — Trapped by the requirement to publish in established journals that demand standardized neuroimaging protocols. Lacks resources to acquire proprietary software licenses or access established scanning facilities. Cannot exit without abandoning neuroimaging research or relocating to institutions with infrastructure. Bears full cost of standardization without benefiting from the vendor ecosystem.
constraint_indexing:constraint_classification(neuroimaging_standardization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESOURCE-LIMITED INSTITUTION (TANGLED ROPE) — Genuinely coordinates data collection through standardization protocols, enabling multi-site collaboration and methodological consistency. However, faces extraction through mandatory proprietary software subscriptions, vendor-specific training requirements, and technical support costs. Coordination benefits exist but are entangled with asymmetric extraction.
constraint_indexing:constraint_classification(neuroimaging_standardization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EQUIPMENT MANUFACTURER (ROPE) — Experiences standardization as pure coordination: establishes DICOM standards, publishes processing pipelines, and enables data sharing that increases system utility and market reach. Gains from ecosystem development. Can arbitrage between markets (research, clinical, commercial). Net beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(neuroimaging_standardization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ESTABLISHED RESEARCH GROUP (TANGLED ROPE) — Benefits from standardization through enhanced reproducibility and multi-site collaboration. Simultaneously extracts from the ecosystem: controls methodological standards through citation authority, influences which analysis pipelines become canonical, creates path-dependency in preprocessing choices. High institutional power but constrained by peer norms and funding agency requirements.
constraint_indexing:constraint_classification(neuroimaging_standardization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN NEUROIMAGING MOVEMENT (SCAFFOLD) — Organized agents (OpenNeuro, BIDS standard, FSL, SPM free alternatives, cloud-based platforms) are building sunset pathways away from vendor lock-in. See standardization as temporary institutional rigidity being replaced by open-source protocols with transparent methods. As open alternatives mature and funding agencies mandate open formats, the vendor-imposed extraction mechanism loses force. Estimated sunset: 10-15 years for open standards to dominate.
constraint_indexing:constraint_classification(neuroimaging_standardization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: JOURNAL PUBLICATION SYSTEM (PITON) — Maintains standardization requirement through methods sections and supplementary material mandates, but the enforcement is largely performative. Editors and reviewers cannot reproduce neuroimaging pipelines from descriptions alone. The standardization ritual persists through inertia while actual verification remains limited. Theater ratio reflects that methods standardization creates appearance of reproducibility without guaranteeing it.
constraint_indexing:constraint_classification(neuroimaging_standardization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, standardization of neuroimaging is inherent to neuroscientific knowledge production: complex biological measurements always require shared protocols, and equipment variation necessarily produces incomparable datasets. This perspective naturalizes standardization as immutable. However, the structural data (vendor extraction, mobility of open-source alternatives, organized movement toward open standards) reveals this as a false summit — the constraint is contingent institutional arrangement, not natural law.
constraint_indexing:constraint_classification(neuroimaging_standardization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(neuroimaging_standardization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(neuroimaging_standardization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(neuroimaging_standardization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(neuroimaging_standardization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(neuroimaging_standardization, TR),
    TR >= 0.70.

:- end_tests(neuroimaging_standardization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The constraint exhibits genuine coordination function — standardized protocols do enable multi-site collaboration and data sharing. However, extraction is substantial and increasing: proprietary software licensing, vendor-specific preprocessing requirements, and path-dependency in analytical choices create asymmetric costs. The rising trajectory (0.32→0.48) reflects vendor consolidation and increasing complexity of proprietary ecosystems. Suppression (0.52): Moderate-high. Barriers to exit include sunk costs in software training, requirement to publish in standardized formats, inability to access established scanning facilities without institutional affiliation, and career risk of methodological nonconformity. However, suppression is not total — open alternatives exist and are expanding. Theater ratio (0.58): Moderate-high. Journal methods sections and standardization protocols create appearance of methodological rigor and reproducibility, but enforcement is largely passive. Reviewers cannot verify preprocessing fidelity or reproduce pipelines from published methods. Theater is increasing because standardization requirements are multiplying (BIDS, preprocessing certification, data sharing mandates) while actual verification capacity remains limited. Claimed type (tangled_rope): The constraint simultaneously coordinates multi-site research and extracts through vendor lock-in. Beneficiaries (equipment manufacturers, established groups) gain network effects and methodological authority. Victims (early-career researchers, resource-limited institutions, methodological reliability) bear extraction costs.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival disagreement. Equipment manufacturers see rope (pure coordination enabling market growth). Established research groups see rope (coordination enabling collaboration and citation authority). Early-career researchers see snare (trapped by standardization requirements without resources to access them). Resource-limited institutions see tangled_rope (coordination benefits entangled with vendor extraction). The open neuroimaging movement sees scaffold (temporary institutional rigidity being replaced by open standards). The journal system sees piton (performative standardization ritual maintained by inertia). The civilizational analytical observer risks seeing mountain (standardization as inherent to neuroscience), but this is a false summit — the structural data reveals that standardization architecture is contingent on institutional power concentration, not natural law. The perspectival gap is driven by power asymmetry: beneficiaries experience coordination; victims experience extraction; organized actors see institutional inertia weakening.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the extraction flow. Equipment manufacturers are beneficiaries with arbitrage options: low d (≈0.15), resulting in negative or near-zero effective extraction. Established research groups are beneficiaries with constrained exit: moderate d (≈0.35), resulting in moderate positive but manageable extraction. Resource-limited institutions are mixed victims/beneficiaries with constrained exit: moderate-high d (≈0.55), resulting in substantial experienced extraction. Early-career researchers are victims with trapped exit: high d (≈0.90), resulting in maximum experienced extraction. The field's methodological reliability has high d (≈0.95) as an abstract victim with no exit options. Organized open science actors have mobile exit (≈0.45), moderating their experienced extraction and enabling them to see a sunset pathway. The piton institutional actor (journal system) has arbitrage options but sees its own enforcement as degraded, resulting in unusual perspective combination.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine coordination (multi-site data sharing, methodological consistency) from contingent institutional arrangements (vendor lock-in, standardization enforcement through journal gatekeeping). The mandate is not to choose one type but to decompose the structural claim: standardization-as-coordination is real and generates rope behavior. Standardization-as-vendor-lock is real and generates snare behavior for trapped agents. The two mechanisms coexist — the extraction is not alternative to coordination but woven into it. The tangled_rope classification captures this entanglement. The theater ratio tracks institutional degradation: as standardization requirements proliferate without verification capacity increasing proportionally, the performative ratio rises. The open science movement offers genuine sunset: as BIDS, OpenNeuro, and open-source pipelines mature, vendor-imposed standardization loses structural force. The classification prevents mislabeling standardization as pure coordination (rope) or pure extraction (snare) — it is demonstrably both, and the perspectival gap shows that different actors experience the mix differently based on power asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vendor_lock_in_magnitude,
    'What proportion of current standardization rigidity is driven by genuine technical necessity vs. vendor contractual dependencies and proprietary data formats?',
    'Comparative analysis of technical costs for format conversion and pipeline portability vs. economic costs of vendor switching; audit of DICOM extension proprietary elements vs. open standard coverage',
    'If technical necessity is dominant (>70%): standardization is closer to rope (coordination justified by real constraints). If vendor lock-in is dominant (>50%): extraction mechanism is primary, snare classification more justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_lock_in_magnitude, empirical, 'Proportion of standardization rigidity from necessity vs. vendor dependency').

omega_variable(
    open_source_pipeline_maturity,
    'Are open-source neuroimaging pipelines (FSL, SPM, AFNI, BIDS derivatives) mature enough to replace proprietary alternatives across all research domains, or do they maintain capability gaps that justify vendor lock-in?',
    'Cross-domain audit of open vs proprietary pipeline capability; tracking of publication rates using open vs proprietary tools; measurement of support/documentation quality gaps',
    'If open pipelines are 90%+ capable: scaffold sunset is technically feasible, extraction mechanism is weakening. If capability gaps remain (>15% of use cases): open science movement is aspirational, vendor dependence remains structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_source_pipeline_maturity, empirical, 'Whether open-source pipelines have achieved functional parity').

omega_variable(
    standardization_enforcement_mechanism,
    'Are journal-enforced standardization requirements primarily enforced through active gatekeeping or through passive assumption that authors follow them?',
    'Meta-analysis of methods reproducibility in published neuroimaging papers; correlation between standardization enforcement stringency and actual reproducibility rates; audit of editor/reviewer corrections for standardization violations',
    'If active gatekeeping: standardization is functional coordination mechanism (higher rope classification). If passive assumption: enforcement is performative (piton classification strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standardization_enforcement_mechanism, empirical, 'Actual enforcement mechanism behind standardization requirements').

omega_variable(
    methodological_replicability_cost_allocation,
    'Who actually bears the cost of failed replication attempts due to standardization ambiguities or vendor-specific preprocessing variations?',
    'Retrospective analysis of published replication attempts; tracking of resources devoted to debugging vendor-specific effects; measurement of time/cost differential for replication using original vs alternative pipelines',
    'If cost falls primarily on replicators: they are a structured victim group (snare from their perspective). If cost is distributed: extraction is diffuse and snare classification weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_replicability_cost_allocation, empirical, 'Who bears the cost of replication failures from standardization ambiguities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(neuroimaging_standardization, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neuro_tr_t0, neuroimaging_standardization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(neuro_tr_t7, neuroimaging_standardization, theater_ratio, 7, 0.5).
narrative_ontology:measurement(neuro_tr_t14, neuroimaging_standardization, theater_ratio, 14, 0.58).

% Extraction over time
narrative_ontology:measurement(neuro_be_t0, neuroimaging_standardization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(neuro_be_t7, neuroimaging_standardization, base_extractiveness, 7, 0.4).
narrative_ontology:measurement(neuro_be_t14, neuroimaging_standardization, base_extractiveness, 14, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(neuroimaging_standardization, information_standard).
narrative_ontology:affects_constraint(neuroimaging_standardization, neuroimaging_data_access).
narrative_ontology:affects_constraint(neuroimaging_standardization, preprocessing_pipeline_reproducibility).
narrative_ontology:affects_constraint(neuroimaging_standardization, vendor_software_licensing).

% DUAL FORMULATION NOTE:
% Neuroimaging standardization decomposes into three structurally distinct constraints: (1) the coordination problem of making datasets from different hardware/software comparable (information_standard, genuine rope), (2) vendor lock-in through proprietary preprocessing requirements (extraction mechanism, snare for trapped agents), and (3) journal enforcement of standardization through performative methods sections (institutional theater, piton). This story integrates all three into a single tangled_rope classification. Downstream constraints in preprocessing reproducibility and data access inherit the extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(neuroimaging_standardization, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
