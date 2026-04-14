% ============================================================================
% CONSTRAINT STORY: biomedical_sensing_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biomedical_sensing_integration, []).

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
 *   constraint_id: biomedical_sensing_integration
 *   human_readable: Biomedical Sensing Integration: Coordination and Extraction in Medical Device Ecosystems
 *   domain: medical_technology/healthcare_infrastructure
 *
 * SUMMARY:
 *   Biomedical sensing integration creates a structural constraint where the
 *   legitimate coordination problem of connecting heterogeneous medical
 *   sensors across manufacturers is systematized into extractive lock-in
 *   mechanisms. Hospital systems need continuous physiological data
 *   aggregated and accessible; patients need monitoring; clinicians need
 *   integrated alerts. These are genuine coordination problems. But the
 *   constraint's structure concentrates extraction toward device
 *   manufacturers through proprietary APIs, closed firmware, patent thickets,
 *   and regulatory requirements written by incumbents. The ecosystem
 *   fragments into incompatible sensor standards, each manufacturer controls
 *   their data format, integration requires expensive middleware, and
 *   independent developers face regulatory and technical barriers. Patients'
 *   physiological data flows into proprietary systems with no exit option.
 *   The theater ratio (0.65) reflects that much integration activity is
 *   performative: compatibility layers, redundant data mappings, FDA
 *   certification overhead for minor software changes, and ritualized
 *   standards committee work that produces slowly-adopted specifications.
 *   Open interoperability movements (FHIR, open-source medical device
 *   projects) are building genuine alternatives with lower theater and
 *   visible sunsets, suggesting scaffold classification from the organized
 *   actor perspective.
 *
 * KEY AGENTS:
 *   - Patient Data Autonomy: Primary victim (powerless/trapped) — physiological data automatically captured and locked into proprietary ecosystems; no exit options or alternatives
 *   - Clinical Interoperability: Secondary victim (powerless/trapped) — collective need for cross-manufacturer data exchange faces active fragmentation; healthcare systems bear integration cost
 *   - Independent Device Developers: Tertiary victim (powerless/trapped) — startups and research groups cannot interoperate with dominant platforms without licensing; barriers include patents, closed firmware, and regulatory burden
 *   - Hospital Systems: Secondary beneficiary-victim (moderate/constrained) — benefit from coordination function but face lock-in switching costs; partially captured by device manufacturer incentives
 *   - Device Manufacturers: Primary beneficiary (institutional/arbitrage) — capture vendor lock-in advantage through proprietary formats; experience constraint as pure coordination enabling market expansion
 *   - Standards Bodies and Regulators (FDA, HL7, DICOM): Partially captured institutional actor (organized/constrained) — attempt to impose standards while protecting manufacturer incumbents; see genuine coordination value alongside asymmetric extraction
 *   - Open Interoperability Movement: Organized agent building alternatives (organized/constrained) — FHIR, open-source projects, healthcare IT consortiums creating parallel ecosystems with sunset timeline
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choices as inherent healthcare complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biomedical_sensing_integration, 0.52).
domain_priors:suppression_score(biomedical_sensing_integration, 0.58).
domain_priors:theater_ratio(biomedical_sensing_integration, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biomedical_sensing_integration, extractiveness, 0.52).
narrative_ontology:constraint_metric(biomedical_sensing_integration, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(biomedical_sensing_integration, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biomedical_sensing_integration, tangled_rope).
narrative_ontology:human_readable(biomedical_sensing_integration, "Biomedical Sensing Integration: Coordination and Extraction in Medical Device Ecosystems").
narrative_ontology:topic_domain(biomedical_sensing_integration, "medical_technology/healthcare_infrastructure").

domain_priors:requires_active_enforcement(biomedical_sensing_integration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biomedical_sensing_integration, device_manufacturers).
narrative_ontology:constraint_beneficiary(biomedical_sensing_integration, hospital_systems).
narrative_ontology:constraint_beneficiary(biomedical_sensing_integration, regulatory_bodies).
narrative_ontology:constraint_victim(biomedical_sensing_integration, patient_data_autonomy).
narrative_ontology:constraint_victim(biomedical_sensing_integration, clinical_interoperability).
narrative_ontology:constraint_victim(biomedical_sensing_integration, independent_device_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT DATA AUTONOMY (SNARE) — Patients cannot exit the biomedical sensing ecosystem; their physiological data is automatically captured, aggregated, and locked into proprietary device ecosystems. Minimal coordination benefit; maximum extraction. Data flows one direction. No alternatives for patients who need continuous monitoring.
constraint_indexing:constraint_classification(biomedical_sensing_integration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLINICAL INTEROPERABILITY (SNARE) — The collective need to integrate sensor data across device manufacturers and hospital systems faces active fragmentation. Proprietary data formats and closed APIs prevent standard communication. Healthcare systems bear the cost of integration work while device manufacturers extract vendor lock-in advantage. No exit without disrupting patient care.
constraint_indexing:constraint_classification(biomedical_sensing_integration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INDEPENDENT DEVICE DEVELOPER COMMUNITY (SNARE) — Startups and research groups cannot interoperate with the dominant platforms without licensing, reverse-engineering, or regulatory burden. Patent thickets, closed firmware, and regulatory capture prevent market entry. Trapped by ecosystem dominance and regulatory requirements designed around incumbent players.
constraint_indexing:constraint_classification(biomedical_sensing_integration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: HOSPITAL SYSTEMS (TANGLED ROPE) — Constrained by capital investment in device fleets and integration costs, but also benefit from the coordination function the sensing ecosystem provides: continuous monitoring, networked alerts, data aggregation. High switching costs limit exit options, but genuine coordination benefit exists. Extraction runs asymmetrically toward device manufacturers.
constraint_indexing:constraint_classification(biomedical_sensing_integration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEVICE MANUFACTURERS (ROPE) — Experience the constraint as pure coordination: integrating sensors into standardized platforms enables market expansion and multi-hospital deployment. Capture lock-in through proprietary data formats and APIs that create switching costs. Net beneficiary with full arbitrage — can exit the integration burden by maintaining closed ecosystems.
constraint_indexing:constraint_classification(biomedical_sensing_integration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: STANDARDS BODIES AND REGULATORS (TANGLED ROPE) — FDA, HL7, DICOM committees attempt to impose interoperability standards, but are captured by manufacturer influence and liability concerns. Must enforce standards while protecting manufacturers from disruption. See genuine coordination value (safe data exchange, clinical integration) alongside asymmetric extraction (standards written to protect incumbents, high compliance costs for small vendors).
constraint_indexing:constraint_classification(biomedical_sensing_integration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN INTEROPERABILITY MOVEMENT (SCAFFOLD) — Organized actors (FHIR, open-source medical software projects, healthcare IT consortiums) are building alternative integration pathways with genuine sunset logic. Open standards, API transparency, and decoupled data models are creating parallel ecosystems that bypass proprietary lock-in. Lower extraction because the movement has agency and a visible exit path. Sunset: 10-15 years as open standards mature in clinical workflows.
constraint_indexing:constraint_classification(biomedical_sensing_integration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: LEGACY INTEGRATION MIDDLEWARE (PITON) — Decades-old HL7 and proprietary middleware systems persist through institutional inertia despite lower functional adequacy than modern APIs. These systems are maintained performatively — healthcare IT departments spend resources on ritualized version upgrades and certification compliance while acknowledging the systems are technically degraded. Theater ratio: 0.72. Exit path visible (modern stacks) but switching cost deters replacement.
constraint_indexing:constraint_classification(biomedical_sensing_integration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (MOUNTAIN - FALSE SUMMIT) — From the civilizational perspective, some data integration overhead is inherent to multi-vendor healthcare systems: different sensors have different calibration, protocols, and data representations. This perspective risks naturalizing the bottleneck as a law of nature — 'healthcare is complex, integration is hard.' But structural data reveals this as a false summit: the extraction is driven by contingent institutional choices (proprietary APIs, patent thickets, regulatory requirements written by incumbents), not by physics or mathematics.
constraint_indexing:constraint_classification(biomedical_sensing_integration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biomedical_sensing_integration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biomedical_sensing_integration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biomedical_sensing_integration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biomedical_sensing_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biomedical_sensing_integration, TR),
    TR >= 0.70.

:- end_tests(biomedical_sensing_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Device manufacturers capture significant revenue through lock-in (device sales at premium to hospitals, potential data monetization), but extraction is not as severe as pure monopoly because hospital systems have exit options (at high cost) and open-standard movement is viable. The measurement trajectory shows extractiveness increasing from 0.38 to 0.52 over the interval, reflecting accumulation of integration debt and deepening vendor entrenchment. Suppression (0.58): Moderate-high. Significant barriers to exit include capital investment in device fleets, switching costs for integration infrastructure, regulatory requirements imposing compatibility testing, and patent thickets on medical sensor protocols. But barriers are not total — some interoperability is possible at cost, and alternative paths exist. Theater ratio (0.65): Moderate-high and increasing. Much biomedical integration activity is performative: compatibility layers that add process cost without clinical benefit, certification requirements that delay innovation, standards committee work producing specifications adopted slowly, and redundant data mapping infrastructure. The trajectory shows theater increasing as complexity outpaces coordination function — each new device type adds integration points, increasing performative overhead. Open standards (FHIR, JSON APIs) have lower theater but slower adoption due to incumbent resistance.
 *
 * PERSPECTIVAL GAP:
 *   The gap between device manufacturer (Rope) and patient data autonomy (Snare) is maximal: the same constraint appears as pure coordination from the beneficiary perspective (integrating sensors enables market expansion) and pure extraction from the victim perspective (data flows one direction with no consent, no portability, no alternatives). Hospital systems see Tangled Rope — genuine coordination benefit (integrated monitoring) paired with asymmetric extraction (high switching costs, vendor control over feature access). Regulators see Tangled Rope — must enforce safety (coordination value) while protecting incumbents (asymmetric extraction). The open-source movement sees Scaffold — the constraint is real but temporary; modern open standards are creating viable alternatives with sunset timeline. The legacy middleware perspective (Piton) reveals performative integration work: compatibility layers and middleware persist through institutional inertia despite lower technical adequacy than modern approaches. The analytical civilizational view risks false summit: naturalizing 'healthcare integration is complex' without recognizing that much complexity is engineered lock-in, not inherent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position in the data extraction flow. Device manufacturers are beneficiaries with arbitrage options (can maintain closed ecosystems and exit integration burden) — their d is low, producing negative or minimal effective extraction from their perspective. Patient data autonomy is trapped with no exit — d approaches 1.0, maximum extraction from this perspective. Hospital systems are constrained but also benefit — d is moderate (0.55-0.65), producing mixed extraction experience. Standards bodies are organized but partially captured by manufacturer influence — d is moderate-high (0.60-0.70), experiencing constrained agency. The open interoperability movement has organized power and visible exit paths — d is constrained by the genuine coordination challenge (patients still need monitoring, sensors still have heterogeneous outputs) but not by the extractive lock-in — d stays lower (0.40-0.50) than for trapped or captured agents.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival structure. The naive view that 'biomedical sensing is just coordination' (Rope) collapses when applied to trapped agents (Snare) and constrained actors (Tangled Rope). The classification suite shows that the constraint is simultaneously genuine coordination (devices do need interoperability) and genuine extraction (the system is engineered to prevent interoperability). The scaffolding perspective (open standards, 10-15 year sunset) resolves the tension by showing that alternative coordination mechanisms are viable — the extraction is not necessitated by the coordination problem. The piton perspective adds that much integration activity is performative overhead, not essential function. The false summit (mountain) perspective reveals the key diagnostic: naturalizing 'integration complexity' as a law of nature prevents recognition that the complexity is engineered through proprietary standards, closed APIs, and patent thickets. When these mechanisms are removed (or made transparent as in open-source alternatives), the coordination problem remains but the extraction disappears. This resolves mandatrophy: the constraint is Tangled Rope (genuine coordination plus asymmetric extraction) whose extraction is contingent on institutional choices, not immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proprietary_necessity_vs_choice,
    'Are closed APIs and proprietary data formats technically necessary for safety/security, or are they strategic choices to maintain lock-in?',
    'Comparative analysis: open-source medical device projects (OpenAPS, Ardupilot, medical imaging libraries) against proprietary equivalents; security audit of claim justifications in patent litigation and regulatory filings',
    'If necessary: reclassify from Snare toward Tangled Rope (extraction has coordination justification). If strategic: confirms Snare classification and reveals false safety narratives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proprietary_necessity_vs_choice, empirical, 'Whether proprietary formats are technically necessary or strategically chosen').

omega_variable(
    regulatory_capture_depth,
    'To what degree do FDA/EMA regulations reflect genuine safety requirements vs. incumbents'' regulatory capture?',
    'Cost-benefit analysis of regulatory requirements; comparison of approval timelines and cost for open vs proprietary device designs; audit of rulemaking influence by manufacturers',
    'If primarily capture: regulatory bodies are primary beneficiaries, not neutral enforcers. Directionality overrides needed. If primarily safety-driven: regulators are partially captured but retain independent function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Degree of regulatory capture in medical device approval').

omega_variable(
    interoperability_standard_adoption,
    'Are FHIR and modern open-standard adoption actually reducing lock-in, or are manufacturers implementing performative compliance (FHIR façade over proprietary backends)?',
    'Audit of actual data portability: measure time/cost to export patient records in standard formats; track whether FHIR implementation enables genuine switching or merely creates compliance theater',
    'If genuine adoption: scaffold perspective is real, sunset timeline is credible. If performative: open standards movement is slower and extraction persists longer than organized actors recognize.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interoperability_standard_adoption, empirical, 'Whether FHIR adoption is genuine or performative compliance').

omega_variable(
    patient_data_monetization_scope,
    'How much of device manufacturer revenue derives from patient data extraction/resale vs. device hardware/service?',
    'Financial analysis of manufacturer revenue streams; audit of data licensing agreements; tracking of de-identified data aggregation and sale to pharma/insurance',
    'If data extraction is major revenue: strengthens Snare classification and victim status. If minor: extraction is less severe than structural constraints alone suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_data_monetization_scope, empirical, 'Revenue contribution of patient data monetization').

omega_variable(
    safety_incident_correlation,
    'Do incompatible sensor data formats and delayed integration contribute to adverse events, or is integration lag clinically neutral?',
    'Epidemiological analysis: audit reports, malpractice claims, adverse event databases for incidents where interoperability failure contributed to patient harm',
    'If significant correlation: extraction is not merely rent-seeking but has genuine safety costs. Amplifies victim status of patient data autonomy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_incident_correlation, empirical, 'Safety impact of interoperability failures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biomedical_sensing_integration, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bsi_tr_t0, biomedical_sensing_integration, theater_ratio, 0, 0.48).
narrative_ontology:measurement(bsi_tr_t5, biomedical_sensing_integration, theater_ratio, 5, 0.58).
narrative_ontology:measurement(bsi_tr_t10, biomedical_sensing_integration, theater_ratio, 10, 0.65).
narrative_ontology:measurement(bsi_tr_t15, biomedical_sensing_integration, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(bsi_be_t0, biomedical_sensing_integration, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bsi_be_t5, biomedical_sensing_integration, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(bsi_be_t10, biomedical_sensing_integration, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(bsi_be_t15, biomedical_sensing_integration, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biomedical_sensing_integration, resource_allocation).
narrative_ontology:affects_constraint(biomedical_sensing_integration, healthcare_data_portability).
narrative_ontology:affects_constraint(biomedical_sensing_integration, medical_device_regulatory_capture).
narrative_ontology:affects_constraint(biomedical_sensing_integration, health_information_exchange_standards).

% DUAL FORMULATION NOTE:
% Biomedical sensing integration is downstream of specific device design constraints and regulatory requirements, but represents a distinct coordination-extraction hybrid. The upstream constraints (device firmware lock-down, FDA approval timelines) have their own extractiveness reflecting technical/regulatory necessity; the sensing integration constraint shows how these are systematized into ecosystem-level vendor lock-in. Decomposition enables separate analysis of which extraction components are technically justified vs. strategically chosen.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biomedical_sensing_integration, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
