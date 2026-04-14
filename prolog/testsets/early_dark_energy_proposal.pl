% ============================================================================
% CONSTRAINT STORY: early_dark_energy_proposal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_early_dark_energy_proposal, []).

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
 *   constraint_id: early_dark_energy_proposal
 *   human_readable: Early Dark Energy Proposal in Cosmology
 *   domain: cosmology/fundamental_physics
 *
 * SUMMARY:
 *   The early dark energy (EDE) proposal addresses the Hubble tension—a
 *   discrepancy between the expansion rate of the universe measured locally
 *   (using supernovae and Cepheid variables) and globally (using cosmic
 *   microwave background observations). EDE proposes that dark energy was
 *   significant in the early universe, providing a mechanism to reconcile
 *   these measurements. This constraint exhibits mixed coordination and
 *   extraction characteristics. EDE generates genuine scientific coordination
 *   by focusing diverse observational programs (CMB precision, supernova
 *   distance measurements, baryon acoustic oscillations) on a specific
 *   hypothesis testable against data. Simultaneously, it exhibits asymmetric
 *   extraction: funding and institutional attention concentrate around
 *   EDE-motivated research, constraining alternative anomaly-resolution
 *   programs and reducing visibility for models that do not assume the
 *   tension requires new physics. The constraint's theater ratio (0.65)
 *   reflects the performative elements of hypothesis testing: the scientific
 *   community conducts observational tests while simultaneously maintaining
 *   Lambda-CDM as the institutional baseline model despite acknowledged
 *   tensions. Theater increases over time as the gap between claim (EDE
 *   solves Hubble tension) and evidence (observational constraints remain
 *   inconclusive) persists.
 *
 * KEY AGENTS:
 *   - EDE Research Group: Primary beneficiary (institutional/arbitrage) — obtains funding priority, visibility, and scientific leadership in anomaly resolution
 *   - Alternative Cosmology Programs: Primary victim (powerless/trapped) — locked into incompatible theoretical frameworks; face funding disadvantage and reduced visibility for non-EDE anomaly resolutions
 *   - Observational Cosmology Groups: Secondary victim (moderate/constrained) — constrained by career risk of pursuing observations that exclude EDE; benefit from EDE-motivated advances in measurement precision
 *   - Anomaly Resolution Community: Organized actor (organized/constrained) — temporary coordination around testing EDE; sunset logic when data definitively includes or excludes the model
 *   - CMB Measurement Consortia: Organized actor (organized/constrained) — funded for EDE-relevant measurements; constrained by concentration of funding around EDE questions
 *   - Lambda-CDM Institutional Framework: Institutional actor (institutional/arbitrage) — maintains privileged status as null hypothesis despite acknowledged tensions; benefits from role as reference point for all alternatives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the Hubble tension as an immutable empirical fact requiring new physics, when the tension may reflect measurement methodology or institutional precedence choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(early_dark_energy_proposal, 0.58).
domain_priors:suppression_score(early_dark_energy_proposal, 0.48).
domain_priors:theater_ratio(early_dark_energy_proposal, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(early_dark_energy_proposal, extractiveness, 0.58).
narrative_ontology:constraint_metric(early_dark_energy_proposal, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(early_dark_energy_proposal, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(early_dark_energy_proposal, tangled_rope).
narrative_ontology:human_readable(early_dark_energy_proposal, "Early Dark Energy Proposal in Cosmology").
narrative_ontology:topic_domain(early_dark_energy_proposal, "cosmology/fundamental_physics").

domain_priors:requires_active_enforcement(early_dark_energy_proposal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(early_dark_energy_proposal, edr_research_group).
narrative_ontology:constraint_beneficiary(early_dark_energy_proposal, anomaly_resolution_specialists).
narrative_ontology:constraint_victim(early_dark_energy_proposal, lambda_cdm_research_programs).
narrative_ontology:constraint_victim(early_dark_energy_proposal, alternative_cosmology_visibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE COSMOLOGY PROGRAMS (SNARE) — Trapped by the gravitational well of major project commitments and established theoretical frameworks. Cannot exit due to sunk costs in infrastructure, trained personnel, and published priors. The EDE proposal extracts attention and funding allocation without providing coordination benefit to programs attempting non-EDE resolutions. Maximum extraction experienced by agents locked into incompatible theoretical commitments.
constraint_indexing:constraint_classification(early_dark_energy_proposal, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OBSERVATIONAL COSMOLOGY GROUPS (TANGLED ROPE) — Constrained by career risk of pursuing observations that may exclude EDE models; also benefit from the EDE framework's focus on high-precision CMB and SNe measurements that advance general observational capability. Real coordination value (precision measurement requirements) exists alongside asymmetric extraction (funding concentration around EDE-favored observations). Exit cost: reorienting research instruments toward alternative hypotheses risks obsolescence and funding discontinuity.
constraint_indexing:constraint_classification(early_dark_energy_proposal, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EDE RESEARCH GROUP (ROPE) — Primary beneficiary. Experiences the constraint as legitimate coordination: marshaling resources to test a specific model against observational data. Net beneficiary through increased visibility, funding access, and scientific priority. Can arbitrage to alternative models if needed; maintains institutional position regardless of EDE validation outcome.
constraint_indexing:constraint_classification(early_dark_energy_proposal, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANOMALY RESOLUTION COMMUNITY (SCAFFOLD) — Organized multi-institutional effort to resolve Hubble tension. EDE serves as a temporary coordination mechanism bringing diverse measurement programs (CMB, local H0, BAO) into alignment around a specific hypothesis. Has sunset logic: if EDE is excluded by observations, the constraint dissolves; if EDE is validated, the coordination becomes permanent (reclassifies as rope). Exit path is clear: accumulating data will close the question definitively within 5-10 years.
constraint_indexing:constraint_classification(early_dark_energy_proposal, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LAMBDA-CDM INSTITUTIONAL FRAMEWORK (PITON) — The baseline cosmological model persists partly through institutional inertia despite the Hubble tension challenge. Lambda-CDM remains the reference frame for all alternative proposals, including EDE. Performative function: Lambda-CDM is maintained as the 'null hypothesis' even as EDE and other models compete. Theater ratio reflects the ritual of hypothesis testing against an increasingly strained model. Institutional investment is substantial; exit would require coordinating alternative institutional baseline, which is difficult despite empirical motivation.
constraint_indexing:constraint_classification(early_dark_energy_proposal, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CMB MEASUREMENT CONSORTIA (TANGLED ROPE) — Organized actors (Planck, ACT, SPT, future CMB-S4) experience EDE as both coordination and extraction. Coordination: high-precision CMB measurements are essential to test EDE. Extraction: funding for next-generation CMB instruments is concentrated around EDE-motivated science questions, constraining broader measurement agendas. Exit cost is high: reorienting instruments to address non-EDE-related questions risks losing committed funding streams.
constraint_indexing:constraint_classification(early_dark_energy_proposal, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the Hubble tension is an empirical fact that any complete theory must address. The constraint to test anomaly-resolving proposals appears immutable: cosmology must account for all measurements. However, this perspective risks naturalizing what is contingent: the institutional commitment to reconciling specific measurements (local H0 vs CMB) through a single theoretical framework, rather than questioning measurement methodology or allowing persistent tensions.
constraint_indexing:constraint_classification(early_dark_energy_proposal, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(early_dark_energy_proposal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(early_dark_energy_proposal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(early_dark_energy_proposal, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(early_dark_energy_proposal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(early_dark_energy_proposal, TR),
    TR >= 0.70.

:- end_tests(early_dark_energy_proposal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The EDE proposal extracts attention and resources from alternative anomaly-resolution pathways. However, extraction is not maximal because EDE does provide genuine coordination value: it motivates high-precision measurements that advance cosmology broadly. The value reflects that some benefits accrue to the broader research ecosystem. Suppression (0.48): Moderate. Alternative anomaly-resolution programs face significant barriers: sunk costs in incompatible theoretical frameworks, funding concentration around EDE, publication bias favoring EDE-consistent results. But suppression is incomplete—some alternative research persists, and the observational data itself constrains excessive EDE promotion (empirical falsifiability limits extractive theater). Theater ratio (0.65): Moderate-high. Significant performative content exists: the scientific community tests EDE against data while maintaining Lambda-CDM as the institutional reference despite acknowledged tensions. The performance involves ritual hypothesis testing, publication of constraints on EDE parameter space, and gradual accumulation of observational data—activities that appear to advance the science without definitively resolving the underlying tension. Theater increases over time as the temporal gap between EDE proposal and conclusive observational resolution widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows strong perspectival divergence driven by power level and exit options. The powerless/trapped agent (alternative programs) classifies the same structural phenomenon as snare—extraction without coordination benefit. The organized/constrained agent (anomaly resolution) sees scaffold—temporary coordination with sunset. The institutional/arbitrage agent (EDE group) sees rope—coordination benefit. The analytical/analytical agent risks mountain—naturalizing the tension. No single classification is 'correct'—the presheaf over the observation site reveals the constraint's true structure: it is simultaneously a coordination mechanism, a temporary framework with sunset logic, an extractive mechanism trapping alternative approaches, and an institutionalized response to an empirical tension that may not require new physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position and exit capacity. The EDE research group (institutional/arbitrage) has low d—they can arbitrage to alternative models if needed and maintain institutional position regardless of EDE outcome, producing negative effective extraction. Alternative cosmology programs (powerless/trapped) have high d—they cannot exit the institutional commitment to test EDE and bear the cost of reduced funding and visibility, producing high effective extraction. Observational cosmology groups (moderate/constrained) have moderate d: they face exit costs (career risk, instrument reorientation) but also benefit from EDE-motivated precision advances. CMB consortia (organized/constrained) occupy intermediate d: they have some organizational power to pursue alternative measurement agendas, but funding concentration around EDE raises their exit costs. The Lambda-CDM framework (institutional/arbitrage) has low d—as the institutional reference point, it benefits from its role in structuring all alternative proposals. The anomaly-resolution community (organized/constrained) has moderate d: they can coordinate around alternative hypotheses if data warrant, but exit from EDE focus carries research program disruption costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that EDE serves simultaneous coordination and extraction functions, with the balance dependent on structural position. From the EDE group's perspective, it is pure coordination (rope). From trapped alternative programs' perspective, it is pure extraction (snare). From the organized anomaly-resolution community's perspective, it is a time-limited coordination mechanism with sunset (scaffold). The constraint's true structure is tangled rope: it provides genuine coordination value (focusing measurement precision on testable hypotheses) while simultaneously extracting resources and visibility from alternative pathways. The analytical perspective risks false mountain classification by naturalizing the Hubble tension as an immutable empirical fact requiring new physics; the structural data reveals this as a contingent institutional choice about measurement precedence. Theater ratio increase over time (0.42 → 0.65) indicates that the performative element grows as the gap between claim and conclusive evidence widens—the constraint persists by performing the act of hypothesis testing, not by definitively resolving the anomaly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hubble_tension_systematic_error,
    'Is the Hubble tension a genuine empirical discrepancy requiring new physics (EDE), or an unresolved systematic error in H0 measurement methodology?',
    'Independent H0 measurement programs using orthogonal methodologies (gravitational lensing, megamaser distances, gravitational waves); identification of shared vs independent systematic errors across measurement classes',
    'If systematic error: EDE becomes unnecessary, constraint dissolves to rope/scaffold (coordination around measurement standardization). If genuine discrepancy: EDE remains necessary, constraint persists as tangled_rope (coordination + extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hubble_tension_systematic_error, empirical, 'Whether Hubble tension reflects new physics or measurement systematics').

omega_variable(
    ede_observational_exclusion_timeline,
    'What is the realistic timeline for observational data to definitively exclude or confirm early dark energy models?',
    'Forecast-based analysis from future CMB and large-scale structure surveys; quantification of parameter space remaining open after Planck + current SNe data; model-independent constraint projections from DESI, CMB-S4, and future experiments',
    'If timeline < 5 years: scaffold perspective is accurate, sunset is imminent. If timeline > 15 years: scaffold classification is aspirational; constraint persists longer than sunset logic predicts, reclassifying toward permanent tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ede_observational_exclusion_timeline, empirical, 'Timeline for definitively testing early dark energy models').

omega_variable(
    institutional_capture_risk,
    'Does EDE funding concentration risk institutionally capturing anomaly-resolution research toward this specific model, reducing incentive to explore alternative hypotheses?',
    'Funding distribution analysis: proportion of anomaly-resolution funding allocated to EDE vs alternatives; publication bias analysis (acceptance rates and visibility for non-EDE anomaly resolutions); career trajectory data for researchers proposing non-EDE models',
    'If significant capture: suppression value increases (0.48 → 0.65+), constraint reclassifies toward pure snare. If negligible capture: suppression decreases, constraint reclassifies toward rope/scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_risk, empirical, 'Whether EDE funding concentration creates institutional capture of anomaly research').

omega_variable(
    measurement_precedence_ambiguity,
    'Which measurement platform (CMB, local H0, BAO) should be considered the empirical truth-bearer when they conflict?',
    'Foundational analysis of measurement hierarchies in cosmology; analysis of historical cases where different platforms gave conflicting results and subsequent resolution; examination of which measurement communities hold institutional priority',
    'If CMB privileged: EDE is necessary. If local H0 privileged: Lambda-CDM must change. If no clear hierarchy: ambiguity persists, extraction mechanism continues (different agents advocate for different measurement precedence).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_precedence_ambiguity, conceptual, 'Epistemic status of conflicting cosmological measurements').

omega_variable(
    early_dark_energy_theoretical_coherence,
    'Is EDE a well-motivated theoretical proposal or an ad hoc patch designed primarily to fit observational discrepancies?',
    'Comparison with theoretical motivation from first principles: quantum field theory predictions, string theory constraints, gravitational theory extensions; assessment of whether EDE arises naturally from fundamental physics or requires auxiliary assumptions',
    'If well-motivated: classification reclassifies toward rope (legitimate coordination around testing well-grounded hypothesis). If ad hoc: reclassifies toward snare (extraction mechanism disguised as science).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(early_dark_energy_theoretical_coherence, conceptual, 'Theoretical foundations and motivation for early dark energy models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(early_dark_energy_proposal, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ede_tr_t0, early_dark_energy_proposal, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ede_tr_t3, early_dark_energy_proposal, theater_ratio, 3, 0.54).
narrative_ontology:measurement(ede_tr_t6, early_dark_energy_proposal, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(ede_be_t0, early_dark_energy_proposal, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ede_be_t3, early_dark_energy_proposal, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ede_be_t6, early_dark_energy_proposal, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(early_dark_energy_proposal, resource_allocation).
narrative_ontology:affects_constraint(early_dark_energy_proposal, hubble_tension_local_measurement).
narrative_ontology:affects_constraint(early_dark_energy_proposal, cmb_measurement_precision).
narrative_ontology:affects_constraint(early_dark_energy_proposal, lambda_cdm_institutional_framework).

% DUAL FORMULATION NOTE:
% Early dark energy is a specific proposal within the broader Hubble tension constraint family. The EDE constraint (ε=0.58) represents the institutional and funding concentration effects of promoting this particular anomaly resolution. The upstream Hubble tension constraint (ε=0.45) represents the empirical discrepancy itself; the downstream local H0 measurement constraint (ε=0.35) represents measurement methodology challenges. EDE extracts institutional resources and attention from both the parent tension and alternative resolution pathways, making it a structurally distinct constraint despite causal dependency on the parent tension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(early_dark_energy_proposal, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
