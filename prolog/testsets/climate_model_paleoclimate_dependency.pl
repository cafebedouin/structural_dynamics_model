% ============================================================================
% CONSTRAINT STORY: climate_model_paleoclimate_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_model_paleoclimate_dependency, []).

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
 *   constraint_id: climate_model_paleoclimate_dependency
 *   human_readable: Climate Model Paleoclimate Dependency Constraint
 *   domain: climate_science/paleoclimatology/model_validation
 *
 * SUMMARY:
 *   Climate models require external validation, and paleoclimate data —
 *   reconstructions of past climates from proxy records like ice cores and
 *   ocean sediments — serves as the primary source of out-of-sample evidence.
 *   This creates a structural dependency: paleoclimate interpretations are
 *   filtered through climate models, and paleoclimate researchers compete for
 *   legitimacy within a scientific ecosystem where consistency with climate
 *   model outputs is implicitly required for funding and publication. The
 *   constraint exhibits tangled rope structure: it coordinates genuine
 *   scientific work (calibrating models against past climates) while
 *   simultaneously extracting from paleoclimate autonomy (subordinating
 *   paleoclimate interpretation to model requirements). Theater has increased
 *   over 30 years as model complexity outpaced paleoclimate resolution
 *   capacity, turning paleoclimate validation increasingly performative —
 *   cited for gravitas rather than for mechanistic constraint. The open
 *   paleoclimate data movement represents a scaffold with sunset logic:
 *   distributed, independent paleoclimate frameworks could decouple
 *   paleoclimate interpretation from modeling centers, but are not yet mature
 *   enough to replace the current arrangement.
 *
 * KEY AGENTS:
 *   - Paleoclimate Data Fidelity: Primary victim (powerless/trapped) — proxy records have no mechanism to reject contradictory model interpretations; cannot exit the dependency
 *   - Climate Modeling Centers: Primary beneficiary (institutional/arbitrage) — paleoclimate validation bolsters model authority and policy legitimacy; can arbitrage between contradictory paleoclimate interpretations
 *   - Independent Paleoclimate Researchers: Secondary victim (moderate/constrained) — face funding and publication pressure to support model consistency; also benefit from model-provided calibration tools and collaborative access
 *   - Open Paleoclimate Data Initiative: Organized challenger (organized/constrained) — PAGES, PMIP, open repositories building decentralized verification pathways with sunset logic
 *   - IPCC Assessment Framework: Institutional performer (institutional/arbitrage) — maintains paleoclimate chapter for rhetorical validation despite atrophied genuine validation capacity
 *   - Policy Decision-Makers: Mixed position (powerful/mobile) — depend on coordinated models for emissions scenarios while being extracted from through reduced opacity of model structural limitations
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as inherent constraint on climate science
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_model_paleoclimate_dependency, 0.52).
domain_priors:suppression_score(climate_model_paleoclimate_dependency, 0.48).
domain_priors:theater_ratio(climate_model_paleoclimate_dependency, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_model_paleoclimate_dependency, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_model_paleoclimate_dependency, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(climate_model_paleoclimate_dependency, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_model_paleoclimate_dependency, tangled_rope).
narrative_ontology:human_readable(climate_model_paleoclimate_dependency, "Climate Model Paleoclimate Dependency Constraint").
narrative_ontology:topic_domain(climate_model_paleoclimate_dependency, "climate_science/paleoclimatology/model_validation").

domain_priors:requires_active_enforcement(climate_model_paleoclimate_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_model_paleoclimate_dependency, climate_modeling_community).
narrative_ontology:constraint_beneficiary(climate_model_paleoclimate_dependency, policy_makers_seeking_certainty).
narrative_ontology:constraint_victim(climate_model_paleoclimate_dependency, paleoclimate_data_fidelity).
narrative_ontology:constraint_victim(climate_model_paleoclimate_dependency, model_uncertainty_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALEOCLIMATE DATA FIDELITY (SNARE) — The epistemic foundation of paleoclimate reconstruction cannot exit the dependency constraint. Ice cores, ocean sediments, and tree rings are proxy data — indirect measurements of past climate — and their interpretation requires calibration against instrumental records and forward modeling. Modern climate models enforce an asymmetric relationship: paleoclimate data must serve model validation without any mechanism to reject models that contradict paleoclimate evidence. The data bears full cost of the dependency through selective interpretation and normalization of conflicts.
constraint_indexing:constraint_classification(climate_model_paleoclimate_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT PALEOCLIMATE RESEARCHERS (TANGLED ROPE) — Constrained by funding structures that prioritize climate model validation over paleoclimate method development. Face publication pressure to show consistency with consensus climate models. Yet genuinely benefit from access to model outputs for proxy calibration and from collaborative verification of paleoclimate methods through cross-disciplinary work. Significant extraction (publication bias, funding asymmetry) alongside real coordination benefits (data sharing, methodological exchange).
constraint_indexing:constraint_classification(climate_model_paleoclimate_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CLIMATE MODELING CENTERS (ROPE) — Primary beneficiary. Paleoclimate data provides external constraint on model behavior, enabling validation claims and policy legitimacy. Experience the dependency as pure coordination: paleoclimate evidence supports model outputs and strengthens confidence in future projections. Can arbitrage between paleoclimate interpretations when they conflict. No significant cost to the modeling institution — the constraint functions to bolster institutional authority.
constraint_indexing:constraint_classification(climate_model_paleoclimate_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN PALEOCLIMATE DATA INITIATIVE (SCAFFOLD) — Organized agents (PAGES, Paleoclimate Modelling Intercomparison Project, open-data repositories) building alternative verification pathways that decentralize paleoclimate interpretation. Distributed paleoclimate databases enable independent researchers to evaluate model-data consistency without institutional mediation. As these databases mature and standardization improves, the original dependency's extraction mechanism loses force. Sunset logic: 10-15 years for open-source paleoclimate frameworks to establish peer verification capacity independent of modeling centers.
constraint_indexing:constraint_classification(climate_model_paleoclimate_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: IPCC PALEOCLIMATE ASSESSMENT FRAMEWORK (PITON) — Performative integration of paleoclimate evidence into climate projections. The IPCC employs 'paleoclimate support' as rhetorical validation: past warm periods (Pliocene, Last Interglacial) are cited as evidence that future warming is plausible, but the actual mechanism-level tests (comparing Pliocene CO2-to-temperature sensitivity with modern CO2-to-temperature projections) remain incomplete or contradictory. The paleoclimate chapter persists through institutional inertia — it provides gravitas and appears rigorous — but genuine validation capacity has atrophied as model complexity outpaced paleoclimate resolution. Theater ratio reflects this degradation.
constraint_indexing:constraint_classification(climate_model_paleoclimate_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: POLICY DECISION-MAKERS (TANGLED ROPE) — Depend on climate models for emissions scenarios and impact projections, but cannot independently verify model fidelity. Face extraction through reduced opacity: paleoclimate-validated models create false confidence, suppressing scrutiny of structural assumptions (cloud feedbacks, vegetation dynamics, tipping points). Yet genuinely benefit from coordinated climate science: a unified modeling framework enables consistent policy signals across jurisdictions. Extraction stems from information asymmetry; coordination from shared epistemic infrastructure.
constraint_indexing:constraint_classification(climate_model_paleoclimate_dependency, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the paleoclimate dependency appears immutable: validating future climate projections against past climate is a structural requirement of any forward modeling enterprise. Paleoclimate calibration is not optional — it is inherent to how climate science must work. However, the structural data contradicts the mountain classification. The engine will identify this as a false summit, revealing that the 'inherent to climate science' framing naturalizes what is actually a contingent institutional arrangement: the choice to center model outputs and treat paleoclimate data as subsidiary validation rather than treating paleoclimate and modern observations as symmetrical constraints.
constraint_indexing:constraint_classification(climate_model_paleoclimate_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_model_paleoclimate_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_model_paleoclimate_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_model_paleoclimate_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_model_paleoclimate_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_model_paleoclimate_dependency, TR),
    TR >= 0.70.

:- end_tests(climate_model_paleoclimate_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The modeling community captures epistemic authority over paleoclimate interpretation through funding concentration, publication gate-keeping, and institutional legitimacy. Paleoclimate researchers must frame their work as supporting model validation rather than providing independent constraint. The extraction is substantial but not total — genuine collaborative benefits exist, and paleoclimate data does provide real external constraint on models (preventing obviously false projections). The 0.52 value reflects mixed extraction and coordination. Suppression (0.48): Moderate. Significant barriers to independent paleoclimate interpretation include: access to specialized data (ice cores require drilling expeditions), computational requirements for forward modeling, publication bias against model-paleoclimate contradictions, and career risk for researchers who question model primacy. These barriers are real but not insurmountable — distributed data repositories and open-source paleoclimate tools are lowering barriers. Theater ratio (0.65): Moderately high. Paleoclimate chapters in assessments like the IPCC cite paleoclimate evidence for rhetorical validation but engage minimally with mechanism-level consistency tests. Past warm periods are invoked to show future warming is 'plausible' without detailed comparison of forcing sensitivities or cloud feedback mechanisms. The theater has increased over the 30-year interval as model complexity outpaced paleoclimate resolution, making genuine validation harder while rhetorical use continued.
 *
 * PERSPECTIVAL GAP:
 *   The modeling centers see rope (pure coordination — paleoclimate data enables better models). Independent researchers see tangled rope (genuine collaboration mixed with unfair pressure to support model consensus). Paleoclimate fidelity sees snare (bears full cost of dependency with no escape). The open paleoclimate data initiative sees scaffold (temporary problem with a structural sunset as decentralized verification matures). The IPCC assessment framework sees piton (performative validation persisting through institutional inertia). Policy makers see tangled rope (depend on models for coordinated signals, but face extraction through reduced transparency about model limitations). The analytical observer risks seeing mountain (paleoclimate validation is inherent to climate science) — but the engine's false summit detector will flag this as naturalization of a contingent institutional choice. The perspectival gap is diagnostic: if all perspectives saw rope, the constraint would be pure coordination. If all saw snare, pure extraction. The spread across five types reveals the true nature of the hybrid system.
 *
 * DIRECTIONALITY LOGIC:
 *   The modeling centers (institutional/arbitrage) experience paleoclimate as flowing toward them — paleoclimate validation bolsters their authority, they can select among contradictory paleoclimate interpretations, and they have exit options (switching proxies, adjusting calibration). This derives d near 0.2 (beneficiary + arbitrage exit) and produces negative f(d) — low effective extraction experienced by beneficiaries. Paleoclimate fidelity (powerless/trapped) has no exit option and bears full cost of the arrangement — d approaches 1.0, f(d) approaches 1.42, producing maximum experienced extraction. Independent paleoclimate researchers (moderate/constrained) are constrained by funding but can organize and have some exit options (publish contrarian results, seek alternative funding) — d around 0.6, producing f(d) in the 0.8–1.0 range, moderate experienced extraction. Policy makers (powerful/mobile) are largely kept outside the technical dispute — they depend on models but can in principle shift to alternative forecasting frameworks — d around 0.55, producing f(d) near 0.75. The piton classification derives from theater_ratio (0.65 approaching 0.70) rather than from high extractiveness: the institutional assessment framework sees its own paleoclimate validation as increasingly performative while continuing to invoke it for authority.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that 'paleoclimate validation' conflates two distinct claims: (1) paleoclimate data can provide external constraint on model behavior (coordination function, rope logic), and (2) current institutional arrangements for paleoclimate interpretation privilege modeling centers over paleoclimate autonomy (extraction function, snare logic for fidelity, tangled rope for researchers). Both are true. The mandatrophy resolution requires holding both simultaneously: the coordination function is real and necessary, but the extraction mechanism is also real and contingent. The false summit (mountain perspective) tries to collapse this distinction by naturalizing the extraction as part of the coordination requirement — 'you cannot have external validation without privileging the validators.' But this is false: independent, distributed paleoclimate interpretation (the scaffold vision) would preserve the coordination function while removing the extraction mechanism. The theater_ratio increase over time indicates that the coordination function has atrophied relative to the performative use — the constraint is drifting toward pure extraction even as its rhetoric claims coordination. This is the diagnostic signature of mandatrophy: the original justification (paleoclimate provides real constraint) is becoming theatricalized (paleoclimate is invoked for authority) while structural extraction increases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_interpretation_indeterminacy,
    'Do paleoclimate proxies have inherent ambiguity in interpretation (fundamental constraint) or is the ambiguity primarily due to under-investment in paleoclimate method development (contingent institutional choice)?',
    'Historical trend analysis: proxy precision improvements with methodological investment; comparison of precision gains in well-funded vs under-funded paleoclimate methods; laboratory studies of proxy response mechanisms',
    'If inherent: paleoclimate dependency is closer to mountain (inescapable limitation). If institutional: the dependency is a tangled rope maintained by research funding priorities, not by nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_interpretation_indeterminacy, empirical, 'Whether paleoclimate proxy ambiguity is fundamental or methodologically remediable').

omega_variable(
    feedback_loop_circularity,
    'Does paleoclimate calibration of modern models risk circular reasoning — using past climate reconstructions (which depend on climate models for validation of proxy interpretation) to validate future climate models?',
    'Epistemological audit: tracking which models and data are used to reconstruct paleoclimate, then checking for overlap with models being validated; independent paleoclimate reconstruction using pre-climate-model theory; statistical tests for circularity in Bayesian hierarchies',
    'If circular: paleoclimate validation is performative (theater_ratio should increase). If independent: paleoclimate evidence has genuine external constraint power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feedback_loop_circularity, empirical, 'Whether paleoclimate calibration introduces circular reasoning into model validation').

omega_variable(
    mechanism_level_consistency,
    'Do paleoclimate and modern climate observations show consistent climate sensitivity (CO2 forcing per degree warming) across different timescales and forcings, or do they diverge in ways that reveal structural model limitations?',
    'Comparative analysis of climate sensitivity estimates from: (1) Last Glacial Maximum paleoclimate data, (2) Last Interglacial data, (3) Pliocene data, (4) Instrumental record, (5) satellite era; meta-analysis of sensitivity estimates; investigation of forcing-specific sensitivity variations',
    'If consistent: models capture underlying climate physics reliably (mountain logic). If divergent: models are fitting data rather than capturing mechanisms (snare logic — paleoclimate is being normalized despite contradictions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mechanism_level_consistency, empirical, 'Whether climate sensitivity is mechanism-consistently estimated across paleoclimate and modern observations').

omega_variable(
    funding_dependency_mechanism,
    'Would paleoclimate research funding and publication venues change structurally if paleoclimate results could contradict climate models without reputational consequence?',
    'Comparative funding analysis: paleoclimate vs modeling budgets across institutions; publication bias audit (negative model-paleoclimate consistency results); career trajectory analysis of paleoclimate researchers who publish model critiques',
    'If yes: dependency is maintained by institutional incentives, not by science (tangled rope). If no: dependency reflects genuine methodological necessity (rope or mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_dependency_mechanism, empirical, 'Whether institutional incentives maintain the paleoclimate dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_model_paleoclimate_dependency, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climpd_tr_t0, climate_model_paleoclimate_dependency, theater_ratio, 0, 0.48).
narrative_ontology:measurement(climpd_tr_t10, climate_model_paleoclimate_dependency, theater_ratio, 10, 0.58).
narrative_ontology:measurement(climpd_tr_t20, climate_model_paleoclimate_dependency, theater_ratio, 20, 0.65).
narrative_ontology:measurement(climpd_tr_t30, climate_model_paleoclimate_dependency, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(climpd_be_t0, climate_model_paleoclimate_dependency, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(climpd_be_t10, climate_model_paleoclimate_dependency, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(climpd_be_t20, climate_model_paleoclimate_dependency, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(climpd_be_t30, climate_model_paleoclimate_dependency, base_extractiveness, 30, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_model_paleoclimate_dependency, resource_allocation).
narrative_ontology:affects_constraint(climate_model_paleoclimate_dependency, climate_model_parametric_uncertainty).
narrative_ontology:affects_constraint(climate_model_paleoclimate_dependency, cloud_feedback_closure).

% DUAL FORMULATION NOTE:
% Paleoclimate dependency is downstream of specific paleoclimate claims (Last Glacial Maximum climate sensitivity, Pliocene temperature, Last Interglacial ice sheet stability) and upstream of climate policy projections. These specific paleoclimate claims have their own extractiveness values reflecting their empirical status. The paleoclimate dependency constraint operates at the meta-level: it governs how paleoclimate evidence of any kind is institutionally integrated into climate modeling. A constraint family would decompose: (1) specific paleoclimate claims (e.g., LGM sensitivity), (2) paleoclimate reconstruction methodology, (3) paleoclimate-model integration framework (this constraint), (4) policy projection framework. This story focuses on (3).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_model_paleoclimate_dependency, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
