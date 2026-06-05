% ============================================================================
% CONSTRAINT STORY: kardashev_scale_progress_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kardashev_scale_progress_narrative, []).

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
 *   constraint_id: kardashev_scale_progress_narrative
 *   human_readable: The Kardashev Scale as a Narrative of Civilizational Progress
 *   domain: social/technological
 *
 * SUMMARY:
 *   The Kardashev scale, introduced by Nikolai Kardashev in 1964 as a SETI
 *   heuristic for estimating distant civilizations' energy output, has become
 *   a dominant cultural narrative equating civilizational progress with
 *   energy consumption throughput. Originally descriptive (a measurement
 *   framework for technosignature detection), the scale has been reframed as
 *   prescriptive: advancement requires moving from Type I (planetary-scale
 *   energy mastery) toward Type II and Type III civilization. This narrative
 *   performs a dual function — it legitimates expanding energy infrastructure
 *   as inherently 'progressive' while naturalizing the displacement of
 *   alternative visions of development (localized resilience,
 *   efficiency-based sufficiency, distributed energy systems). The constraint
 *   exhibits classic tangled-rope structure: it provides genuine coordination
 *   function (aligning technological research agendas, justifying renewable
 *   infrastructure investment) while simultaneously extracting from
 *   ecological sustainability framings and localized energy models. Theater
 *   has increased over the 60-year interval as the scale moved from
 *   specialized astrophysics heuristic to mass-market civilization-ranking
 *   framework, despite deepening acknowledgment within SETI research that the
 *   scale inadequately captures efficiency, distribution, or sustainability
 *   dimensions. The constraint is characterized by mandatrophy ambiguity: it
 *   is genuinely useful for certain technical purposes (SETI signal analysis)
 *   while simultaneously naturalizing contestable civilizational priorities
 *   (energy maximization as progress metric).
 *
 * KEY AGENTS:
 *   - Energy Infrastructure Corporations: Primary beneficiary (institutional/arbitrage) — extract legitimacy and expansion justification from Kardashev narrative; benefit from public support for megaprojects
 *   - Ecological Commons: Primary victim (powerless/trapped) — bears extraction through resource depletion and climate forcing naturalized as 'progress'; no exit from dominant institutional discourse
 *   - Energy-Transition Communities: Secondary victim (moderate/constrained) — constrained by material requirements of transition infrastructure; also motivated by Kardashev logic for renewable deployment; mixed extraction
 *   - Degrowth and Post-Development Movements: Organized agents (organized/constrained) — developing alternative progress metrics with sunset clause logic; building institutional alternatives
 *   - SETI Research Community: Institutional actor (institutional/arbitrage) — originators of scale; now maintain it through institutional inertia despite acknowledged limitations; piton perspective
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing thermodynamic constraints (genuine upper bounds) while obscuring social construction of which constraints are treated as binding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kardashev_scale_progress_narrative, 0.52).
domain_priors:suppression_score(kardashev_scale_progress_narrative, 0.58).
domain_priors:theater_ratio(kardashev_scale_progress_narrative, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kardashev_scale_progress_narrative, extractiveness, 0.52).
narrative_ontology:constraint_metric(kardashev_scale_progress_narrative, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kardashev_scale_progress_narrative, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kardashev_scale_progress_narrative, tangled_rope).
narrative_ontology:human_readable(kardashev_scale_progress_narrative, "The Kardashev Scale as a Narrative of Civilizational Progress").
narrative_ontology:topic_domain(kardashev_scale_progress_narrative, "social/technological").

domain_priors:requires_active_enforcement(kardashev_scale_progress_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kardashev_scale_progress_narrative, energy_intensive_industries).
narrative_ontology:constraint_beneficiary(kardashev_scale_progress_narrative, technological_expansion_narratives).
narrative_ontology:constraint_beneficiary(kardashev_scale_progress_narrative, centralized_power_infrastructure).
narrative_ontology:constraint_victim(kardashev_scale_progress_narrative, ecological_sustainability_framing).
narrative_ontology:constraint_victim(kardashev_scale_progress_narrative, localized_resilience_models).
narrative_ontology:constraint_victim(kardashev_scale_progress_narrative, decentralized_energy_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECOLOGICAL COMMONS (SNARE) — Cannot exit the narrative constraint that equates progress with energy throughput. Bears full structural cost: resource depletion, climate forcing, ecosystem degradation are naturalized as necessary stages of 'advancement.' No alternative framing available in dominant institutional discourse. Maximum extraction.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENERGY-TRANSITION COMMUNITIES (TANGLED ROPE) — Constrained by material requirements of transition infrastructure (mining, manufacturing, grid buildout), but also motivated by the Kardashev narrative itself as justification for renewable energy deployment. Extract resources under the banner of 'progress' while being trapped by the framework's energy-maximization logic. Some agency but significant structural extraction.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENERGY INFRASTRUCTURE CORPORATIONS (ROPE) — Primary beneficiaries experiencing the constraint as pure coordination mechanism. The Kardashev scale legitimates continuous energy infrastructure expansion; the narrative solves the collective action problem of maintaining public support for megaprojects. Benefits from first-mover advantage in infrastructure deployment.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEGROWTH AND POST-DEVELOPMENT MOVEMENTS (SCAFFOLD) — Organized agents (academic critiques, policy alternatives, community-based energy models) see the Kardashev narrative as a temporary institutional framing with a sunset clause. Alternative metrics (genuine progress indicators, flourishing indices, resilience measures) are being developed as replacement paradigms. Extraction experienced as moderate because these movements have some institutional traction and clear exit pathway through paradigm shift.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SETI RESEARCH COMMUNITY (PITON) — Original creators of the Kardashev classification as a search heuristic (1964). The constraint now persists in inertial form: SETI researchers largely acknowledge the scale's limitations (inability to account for efficiency, sustainability, distributed systems) but continue using it as a research organizing principle because alternatives haven't fully replaced it. Performative rather than functionally necessary. High theater from ritualized use despite acknowledged inadequacy.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL CONSTRAINT VIEW (MOUNTAIN) — From a universal physical perspective, thermodynamic limits on energy harvesting and waste heat dissipation create real upper bounds on civilizational energy consumption. The Kardashev scale can be reframed as measuring proximity to Eddington luminosity limits. However, this natural law reading naturalizes what is actually a contingent choice about which physical constraints to treat as binding. The constraint story reveals false summit: the thermodynamic reading masks the social construction of 'progress.'
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kardashev_scale_progress_narrative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kardashev_scale_progress_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(kardashev_scale_progress_narrative, TR),
    TR >= 0.70.

:- end_tests(kardashev_scale_progress_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The narrative extracts from alternative development framings by naturalizing energy-maximization as synonymous with progress. Legitimates resource extraction and infrastructure expansion under the banner of 'civilizational advancement.' But extraction is not total because alternative metrics (GNH, genuine progress indicators, degrowth frameworks) have institutional traction in some policy domains. The value reflects sustained but contested dominance of the energy narrative. Suppression (0.58): Moderate-high. Significant barriers to alternative progress narratives include: institutional path-dependency of energy-based economic accounting, infrastructure lock-in (centralized grids, long-lived capital stock), elite alignment with energy expansion, and cultural internalization of Kardashev logic as 'natural' metric. But suppression is not total — academic alternatives exist, policy communities are experimenting with alternatives, and ecological constraints are becoming undeniable. Theater ratio (0.68): High. The constraint's performative content has increased sharply. The original 1964 formulation was a technical search heuristic; current popular use as civilization-ranking metric ('we are Type 0.73' culture) performs primarily symbolic function. SETI community largely acknowledges the scale's technical inadequacy; continued use is inertial, not functionally necessary. The theater rise from 0.35 (1964) to 0.68 (2024) reflects Goodhart drift — the metric optimized for institutional expansion (justifying megaprojects) rather than tracking genuine progress.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates perpectival divergence between institutional beneficiaries and ecological victims. Energy corporations see pure coordination (Rope) — the narrative solves their collective action problem of maintaining public legitimacy for expansion. SETI community sees degraded ritual (Piton) — acknowledges inadequacy but continues use. Ecological systems see pure extraction (Snare) — resource depletion and climate forcing naturalized as necessary progress. Degrowth movements see temporary problem with sunset (Scaffold) — alternative metrics are being institutionalized, creating exit pathway. Energy-transition communities experience hybrid (Tangled Rope) — constrained by material requirements of renewable deployment justified by the same Kardashev narrative they are trying to escape. The analytical observer risks false summit (Mountain) by treating thermodynamic limits as immutable, naturalizing what is actually social choice about which constraints bind.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's structural position determines their experienced extraction. Energy corporations benefit from narratives legitimating expansion (d ≈ 0.05-0.15, low f(d), negative χ). Ecological systems cannot exit and bear costs (d ≈ 0.95, high f(d), high χ). Degrowth movements have some institutional exit pathways and alternative framings (d ≈ 0.55, moderate f(d), moderate χ). SETI researchers derive d from beneficiary status (institutional maintenance of scale) combined with arbitrage options (could adopt alternatives but don't) — d ≈ 0.15 canonical institutional, but override to 0.30 to reflect that they actively defend a degraded tool. Ecological commons cannot be overridden — d = 0.95 is structural, not choice-dependent.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is not resolvable at single level because the constraint conflates two distinct structural claims: (1) TECHNICAL: 'The Kardashev scale is a useful SETI heuristic for estimating civilization energy output.' (2) NORMATIVE: 'Civilizational progress consists of increasing energy consumption.' Claim 1 is likely true for its original purpose (technosignature detection); Claim 2 is social construction presented as natural law. The constraint story shows that indexical classification separates these: from SETI research perspective (immediate, institutional, arbitrage), claim 1 justifies continued use (Rope/Piton). From ecological perspective (generational, powerless, trapped), claim 2 naturalizes extraction (Snare). The mandatrophy resolution requires decomposing the single 'Kardashev scale' label into two constraints: (a) kardashev_seti_heuristic (ε ≈ 0.15, technical adequacy for detection purposes, Mountain or Rope), (b) kardashev_progress_narrative (ε ≈ 0.52, normative claim about civilizational development, Tangled Rope). Current constraint story captures the narrative confusion as high theater_ratio — the scale persists as dominant cultural frame despite acknowledged technical inadequacy for its new use case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_decoupling_feasibility,
    'Can technological efficiency gains decouple economic growth from energy consumption indefinitely, or is current decoupling statistical artifact from accounting methodology?',
    'Long-term empirical tracking of total system energy (including embodied energy in manufacturing, logistics, waste) vs GDP; comparison of growth rates across measurement methodologies (IEA vs territorial vs consumption-based accounting)',
    'If true decoupling possible: Kardashev framing becomes optional rather than necessary — efficiency pathways exist. If accounting artifacts: the scale naturalizes genuinely required energy growth, increasing classification toward pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_decoupling_feasibility, empirical, 'Whether efficiency can decouple growth from energy indefinitely').

omega_variable(
    alternative_progress_metrics_adoption,
    'Will post-development metrics (genuine progress indicators, Bhutan''s GNH, UN Sustainable Development Framework) institutionalize at scale to compete with energy-based progress narratives?',
    'Institutional adoption rates: how many policy regimes use alternative metrics as primary progress measure vs secondary; GDP replacement analysis across 10-20 year horizon',
    'If alternative metrics institutionalize: scaffold sunset becomes real, extraction mechanism weakens. If remain academic/marginal: Kardashev narrative persists as dominant institutional constraint despite acknowledged limitations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_progress_metrics_adoption, empirical, 'Whether alternative progress metrics will institutionalize at scale').

omega_variable(
    biosphere_resilience_threshold,
    'At what energy infrastructure deployment rate does planetary ecosystem capacity become the binding constraint rather than energy supply?',
    'Ecosystem services modeling; comparative analysis of Holocene baseline vs current extraction rates for key materials (rare earths, metals); tipping point identification in carbon/nutrient cycles',
    'If threshold crossed before technological saturation: energy-maximization strategy becomes self-defeating (extraction mechanism revealed). If threshold remains distant: Kardashev logic continues viable as civilizational strategy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biosphere_resilience_threshold, empirical, 'At what energy scale does ecosystem capacity become the binding constraint').

omega_variable(
    anthropomorphic_projection_necessity,
    'Is the Kardashev scale necessary for SETI detection strategy, or does it represent projection of human-centric energy assumptions onto alien civilizations?',
    'Alternative SETI signatures analysis: technosignature detection methods not dependent on energy scale (information density, computation efficiency, waste heat spectrum, gravitational engineering); comparison of detection sensitivity across methodologies',
    'If alternative signatures prove equally/more detectable: Kardashev becomes optional framework. If energy-scale remains optimal: the scale''s physical naturalness is confirmed rather than socially constructed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anthropomorphic_projection_necessity, conceptual, 'Whether Kardashev scale is necessary or anthropomorphic projection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kardashev_scale_progress_narrative, 1964, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ksp_tr_t0, kardashev_scale_progress_narrative, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ksp_tr_t30, kardashev_scale_progress_narrative, theater_ratio, 30, 0.52).
narrative_ontology:measurement(ksp_tr_t60, kardashev_scale_progress_narrative, theater_ratio, 60, 0.68).

% Extraction over time
narrative_ontology:measurement(ksp_be_t0, kardashev_scale_progress_narrative, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ksp_be_t30, kardashev_scale_progress_narrative, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(ksp_be_t60, kardashev_scale_progress_narrative, base_extractiveness, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kardashev_scale_progress_narrative, information_standard).
narrative_ontology:affects_constraint(kardashev_scale_progress_narrative, renewable_energy_infrastructure_overcapitalization).
narrative_ontology:affects_constraint(kardashev_scale_progress_narrative, ecological_resilience_metric_suppression).
narrative_ontology:affects_constraint(kardashev_scale_progress_narrative, centralized_power_grid_lock_in).

% DUAL FORMULATION NOTE:
% The Kardashev scale narrative decomposes into two structurally distinct constraints: (1) kardashev_seti_heuristic — technical adequacy for technosignature detection, low ε, upstream Mountain/Rope claim that drives SETI research priorities; (2) kardashev_progress_narrative — normative claim that progress = energy consumption, ε ≈ 0.52, Tangled Rope, downstream narrative constraint. This story addresses the narrative constraint. The SETI technical constraint would be separate, with ε ≈ 0.12-0.18 (low base extraction, mostly coordination function) and Mountain or Rope classification depending on whether alternative signatures are equally detectable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kardashev_scale_progress_narrative, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
