% ============================================================================
% CONSTRAINT STORY: climate_model_initialization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_model_initialization, []).

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
 *   constraint_id: climate_model_initialization
 *   human_readable: Climate Model Initialization Dependency and Parameterization Lock-In
 *   domain: climate_science/computational_modeling
 *
 * SUMMARY:
 *   Climate model initialization — the process of setting up the atmospheric,
 *   oceanic, and cryospheric state from which a climate simulation begins —
 *   creates a structural constraint that exhibits both genuine coordination
 *   function and significant extraction. The established modeling centers
 *   (GFDL, ECMWF, MPI, NCAR) generate initialization datasets and coupled
 *   model frameworks that become de facto standards through IPCC
 *   participation and publication norms. New research groups cannot credibly
 *   produce climate projections without these datasets due to computational
 *   cost (~10 million core-hours to generate independent initialization),
 *   lack of funding for initialization work, and the impossibility of
 *   publication without standardized comparisons. This creates a mixed
 *   constraint: the standardized initialization genuinely enables rapid model
 *   intercomparison and scientific progress (rope function), but it also
 *   locks in specific parameterization choices and constrains methodological
 *   innovation (extraction mechanism). The theater ratio measures how much
 *   climate modeling effort goes into CMIP compliance rituals versus
 *   advancing climate physics — this ratio has increased from ~0.40 in the
 *   2000s to ~0.68 today as CMIP protocols have become increasingly
 *   prescriptive while computational sophistication has made some
 *   standardization choices obsolete.
 *
 * KEY AGENTS:
 *   - Established Modeling Centers (GFDL, ECMWF, MPI, NCAR, IPSL): Primary beneficiary (institutional/arbitrage) — generate standard datasets, control parameterization choices, accumulate citation advantage and funding concentration
 *   - Emerging Research Groups (universities, research institutes, developing-world climate centers): Primary victim (powerless/trapped) — face computational barriers to independent initialization; forced to adopt standard parameterizations; cannot innovate without massive research investment
 *   - Regional Downscaling Community: Secondary victim (moderate/constrained) — depends on global initialization but needs flexibility to develop regional parameterizations; constrained by global model choices
 *   - Model Diversity (as collective): Victim (powerless/trapped) — structural bias toward established modeling frameworks; alternative approaches face publication and funding barriers
 *   - Open Climate Modeling Initiative (ESMValTool developers, open-source frameworks): Organized actor (organized/constrained) — building alternative pathways with sunset logic; currently underfunded relative to established center infrastructure
 *   - CMIP Standardization Protocol: Institutional mechanism (institutional/arbitrage) — prescribes initialization methods and parameter ranges; maintains itself through publication pressure and historical precedent
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional arrangements as physical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_model_initialization, 0.52).
domain_priors:suppression_score(climate_model_initialization, 0.58).
domain_priors:theater_ratio(climate_model_initialization, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_model_initialization, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_model_initialization, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_model_initialization, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_model_initialization, tangled_rope).
narrative_ontology:human_readable(climate_model_initialization, "Climate Model Initialization Dependency and Parameterization Lock-In").
narrative_ontology:topic_domain(climate_model_initialization, "climate_science/computational_modeling").

domain_priors:requires_active_enforcement(climate_model_initialization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_model_initialization, established_modeling_centers).
narrative_ontology:constraint_beneficiary(climate_model_initialization, computational_infrastructure_operators).
narrative_ontology:constraint_victim(climate_model_initialization, emerging_research_groups).
narrative_ontology:constraint_victim(climate_model_initialization, model_diversity).
narrative_ontology:constraint_victim(climate_model_initialization, initialization_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING RESEARCH GROUP (SNARE) — New modeling groups cannot produce credible climate projections without access to established initialization datasets and coupled model infrastructure. Barriers include: computational cost of generating independent initial conditions (~10M core-hours), lack of institutional funding for initialization work, and impossibility of publication without using standardized datasets for comparison. Maximum extraction with minimal coordination benefit — the constraint forces dependency rather than enabling collaboration.
constraint_indexing:constraint_classification(climate_model_initialization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL MODELING COMMUNITY (TANGLED ROPE) — Regional downscaling models genuinely benefit from standardized global initialization (coordination function) but are constrained by the parameterization choices embedded in those initializations. They can develop alternative schemes at significant cost (~2-3 year research program per major parameter) or adapt existing ones (lower cost, high constraint). Both paths show mixed coordination and asymmetric extraction.
constraint_indexing:constraint_classification(climate_model_initialization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED MODELING CENTERS (ROPE) — IPCC-participating centers (GFDL, ECMWF, MPI, NCAR) experience initialization as pure coordination: their datasets enable rapid model intercomparison, their parameterization choices become scientific standards, their computational dominance creates network effects that benefit all users. They have arbitrage options (can always run independent initialization streams) but find the standard pathway more efficient. Net institutional benefit — extraction runs toward them, not away.
constraint_indexing:constraint_classification(climate_model_initialization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN CLIMATE MODELING INITIATIVE (SCAFFOLD) — Organized coalitions (ESMValTool, PyESMValTool, CLIVAR data standardization efforts) are building alternative initialization pathways with lower parameterization lock-in. Open-source initialization schemes reduce dependency on proprietary modeling center datasets. Sunset logic applies: as distributed initialization frameworks mature (estimated 10-15 years), the bottleneck's extraction mechanism loses force. Current suppression is moderate because alternative pathways exist but are underfunded relative to established center infrastructure.
constraint_indexing:constraint_classification(climate_model_initialization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CMIP STANDARDIZATION RITUAL (PITON) — Coupled Model Intercomparison Project (CMIP) protocols prescribe specific initialization methods, parameter ranges, and output standards. These were functionally necessary in CMIP5 (enabling comparison across heterogeneous models) but have become largely performative in CMIP6+. Model groups invest effort in CMIP compliance that is decoupled from scientific validity — parameterization choices are constrained by CMIP protocols rather than by climate physics. Theater ratio is high because the ritual persists (inertia) despite availability of more sophisticated alternatives. The standardization mechanism maintains itself through publication pressure: non-CMIP models have limited visibility even if scientifically superior.
constraint_indexing:constraint_classification(climate_model_initialization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, initialization constraints appear inevitable: climate models require initial atmospheric state, that state must come from observations, and observations have systematic errors. Any initialization method must make choices about how to handle model-observation mismatch. This perspective sees the bottleneck as inherent to coupled Earth system modeling. However, the structural data contradicts mountain classification — the extraction is not from physical limits (accessibility_collapse would need to exceed 0.85) but from institutional choices about which initialization methods are 'standard.'
constraint_indexing:constraint_classification(climate_model_initialization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_model_initialization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_model_initialization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_model_initialization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_model_initialization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_model_initialization, TR),
    TR >= 0.70.

:- end_tests(climate_model_initialization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Emerging groups must invest 2-5 years and significant funding to develop alternative initialization schemes, or accept parameterization lock-in from established centers. The extraction is not total (alternatives exist, some funding is available) but the path of least resistance requires adoption of standard initializations, which imposes costs on innovation. The measurement trajectory shows extractiveness increasing over time as CMIP protocols have become more prescriptive (driven by model complexity rather than scientific necessity). Suppression (0.58): Moderate-high. Structural barriers include: computational cost of independence (~10M core-hours = $5-10M USD), funding agency bias toward CMIP-compliant projects, publication bias against non-standard methods, and tacit knowledge concentration in established centers. These barriers are not absolute (some groups have overcome them, e.g., MIROC, GFDL) but they are significant and systematic. Theater ratio (0.68): High and increasing. CMIP5→CMIP6→CMIP7 protocols have become increasingly prescriptive about initialization procedures, output formatting, and metadata standards. Much of this is performative — the protocols ensure intercomparability but constrain experimentation. Open-source alternatives (ESMValTool) are reducing theater but remain underfunded relative to established center infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows a dramatic perspectival gap between established centers and emerging groups. Established centers (institutional/arbitrage) classify the constraint as Rope — pure coordination that enables scientific progress and rapid model intercomparison. Emerging groups (powerless/trapped) classify it as Snare — the same standardization mechanism that enables intercomparison also forces adoption of specific parameterizations and blocks innovation. Regional modeling communities (moderate/constrained) see Tangled Rope — genuine coordination benefit from global initialization plus asymmetric constraint on parameterization flexibility. The analytical observer at civilizational scope risks seeing Mountain (initialization constraints are inherent to climate modeling) but the structural data contradicts this: the extraction mechanism is not from physical necessity but from institutional choice about which initialization methods are standard. The theater ratio (0.68) reveals that institutional choice dominates — much climate modeling effort goes into CMIP compliance rituals that could be redesigned if institutional lock-in were not present.
 *
 * DIRECTIONALITY LOGIC:
 *   Established centers derive low directionality (d ≈ 0.15-0.20): institutional power + arbitrage exit options + beneficiary status = effective institutional actors who experience the constraint as enabling coordination. They can always run independent initialization streams if needed but find standardization more efficient. Emerging groups derive high directionality (d ≈ 0.85-0.90): powerless agents + trapped exit options + victim status = maximum experienced extraction. They cannot easily leave the standard pathway and must pay costs to innovate. Regional modeling community derives moderate directionality (d ≈ 0.55-0.65): moderate power + constrained exit options + mixed victim/beneficiary status = mixed experienced extraction. They benefit from global initialization but are constrained by parameterization choices. Open climate initiatives derive moderate directionality (d ≈ 0.45-0.55): organized power + constrained exit options + agent of change = moderate extraction with exit pathway visible.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing institutional coordination (Rope function — standardization enables intercomparison) from parameterization lock-in (Snare extraction mechanism — specific choices are forced on non-beneficiary groups). The tension is real: standardization is necessary for climate science progress, but standardization can also lock in suboptimal choices. The Tangled Rope classification captures both functions: yes, standardization coordinates model intercomparison; yes, standardization also constrains innovation and concentrates power. The Scaffold perspective points toward resolution: open-source initialization frameworks are building alternative pathways with lower parameterization lock-in. The sunset is real — as distributed initialization and parameter sharing improve, the extraction mechanism loses force. The Piton perspective documents that CMIP protocols maintain themselves through institutional inertia: the ritual persists (theater_ratio = 0.68) despite availability of more flexible alternatives. The false mountain perspective (analytical/civilizational) reveals naturalization: institutional choice appears as physical necessity. The structural data shows this is naturalization — observational uncertainty is significant but does not dominate parameterization uncertainty, meaning institutional choices about how to handle that uncertainty are the binding constraint, not the uncertainty itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parameterization_versus_initialization,
    'Is the binding constraint the initialization dataset itself, or the parameterization choices embedded in how that dataset was generated?',
    'Sensitivity analysis: decompose model sensitivity to (a) initial conditions uncertainty vs (b) parameterization uncertainty. If (a) dominates, initialization is the constraint; if (b) dominates, parameterization lock-in is the constraint.',
    'If initialization dominates: constraint type shifts toward Rope (coordination of initial state). If parameterization dominates: Snare/Tangled Rope classification confirmed — the extraction mechanism is forcing adoption of specific modeling choices, not just initial conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parameterization_versus_initialization, empirical, 'Whether constraint is initialization dependency or parameterization lock-in').

omega_variable(
    observational_uncertainty_floor,
    'What fraction of initialization uncertainty is irreducible observational error versus institutional choice in how errors are handled?',
    'Ensemble uncertainty quantification: compare confidence intervals from initialization-alone vs initialization-plus-parameterization. Compare error budgets across modeling centers using identical observational data.',
    'High irreducible uncertainty (>70%): initialization becomes mountain-like — all choices converge to natural error limits. Low irreducible uncertainty (<30%): parameterization choices dominate, and institutional capture is clear.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observational_uncertainty_floor, empirical, 'Observational uncertainty floor versus institutional choice').

omega_variable(
    open_initialization_maturity,
    'Are open-source initialization frameworks (e.g., ESMValTool-based schemes) scientifically equivalent to proprietary center methods, or do they produce systematically degraded climate projections?',
    'Blind intercomparison: regional downscaling models using open vs proprietary initialization; comparison of skill metrics for hindcasts and present-day simulation statistics.',
    'If equivalent: scaffold sunset is real, constraint can decay as open alternatives mature. If degraded: open pathways are not true alternatives, and emerging groups face real structural barriers regardless of institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_initialization_maturity, empirical, 'Scientific equivalence of open initialization frameworks').

omega_variable(
    publication_bias_against_alternatives,
    'Do journals and funding agencies systematically penalize climate models that deviate from CMIP-standard initialization, independent of scientific quality?',
    'Bibliometric analysis: publication success rates and citation patterns for non-standard vs standard initialization approaches, controlling for model skill metrics.',
    'If penalty is significant (>30% lower publication/funding success): suppression is structural and institutional. If penalty is absent: constraints are mostly scientific/technical, not extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publication_bias_against_alternatives, empirical, 'Publication bias against non-standard initialization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_model_initialization, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_init_tr_t0, climate_model_initialization, theater_ratio, 0, 0.4).
narrative_ontology:measurement(clim_init_tr_t10, climate_model_initialization, theater_ratio, 10, 0.55).
narrative_ontology:measurement(clim_init_tr_t20, climate_model_initialization, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(clim_init_be_t0, climate_model_initialization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_init_be_t10, climate_model_initialization, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(clim_init_be_t20, climate_model_initialization, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_model_initialization, information_standard).
narrative_ontology:affects_constraint(climate_model_initialization, climate_model_parameterization_space).
narrative_ontology:affects_constraint(climate_model_initialization, climate_projection_skill_ceiling).
narrative_ontology:affects_constraint(climate_model_initialization, developing_world_climate_capacity).

% DUAL FORMULATION NOTE:
% Climate model initialization decomposes into two structurally distinct constraints: (1) initial condition availability (genuine coordination problem — all models need initial state from observations), (2) parameterization standardization (institutional choice — could allow more flexibility). This story covers both; more detailed analysis would decompose into separate constraint families per the ε-invariance principle. The theater ratio (0.68) indicates high performative content, suggesting the parameterization standardization component dominates the institutional lock-in observed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_model_initialization, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
