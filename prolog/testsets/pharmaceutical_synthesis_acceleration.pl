% ============================================================================
% CONSTRAINT STORY: pharmaceutical_synthesis_acceleration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_synthesis_acceleration, []).

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
 *   constraint_id: pharmaceutical_synthesis_acceleration
 *   human_readable: Pharmaceutical Synthesis Acceleration Constraint
 *   domain: pharmaceutical/biomedical_research
 *
 * SUMMARY:
 *   Pharmaceutical synthesis acceleration—the technological and
 *   methodological push to dramatically reduce the time from drug target to
 *   synthesized candidate molecule—exhibits properties of both genuine
 *   coordination (enabling faster discovery) and extractive institutional
 *   capture (benefiting established pharma while displacing academic research
 *   and generic manufacturers). The constraint arises from convergence of
 *   high-throughput automation, AI-assisted retrosynthesis, and proprietary
 *   platform licensing. Major pharmaceutical corporations and venture capital
 *   have captured the acceleration narrative, deploying proprietary hardware
 *   and algorithms that create moving targets for generic competition and
 *   raise barriers for academic and small-firm entry. The tension is not
 *   between speed and safety—most acceleration gains are in preclinical
 *   synthesis, not clinical trials—but between who controls the acceleration
 *   technology and who bears the costs of the coordination shift. Academic
 *   chemistry labs face obsolescence as drug discovery outsources synthesis
 *   to specialized platforms. Generic manufacturers face accelerated
 *   obsolescence of known synthesis routes. Patient access faces extended IP
 *   exclusivity driven by novel patentable synthesis routes. Yet acceleration
 *   also provides genuine coordination value: faster candidate screening,
 *   shorter development timelines, and reduced development costs when
 *   innovation is concentrated in firms with platform access.
 *
 * KEY AGENTS:
 *   - Major Pharmaceutical Corporations: Primary beneficiary (institutional/arbitrage) — control proprietary platforms, capture acceleration value through patent strategy
 *   - Venture Capital Investment: Secondary beneficiary (institutional/arbitrage) — shorter venture horizons for biotech exits reduce risk-adjusted time
 *   - Academic Chemistry Laboratories: Primary victim (powerless/trapped) — displaced by institutional barriers, funding gaps, IP disadvantage
 *   - Generic Drug Manufacturers: Secondary victim (powerless/trapped) — face accelerated obsolescence of reverse-engineered routes
 *   - Patient Access Equity: Tertiary victim (moderate/constrained) — access inequality reinforced by extended IP exclusivity windows
 *   - Biotech Startups: Mixed (moderate/constrained) — benefit from acceleration access but forced to adopt expensive proprietary platforms
 *   - Public Health Systems: Mixed (powerful/mobile) — benefit from discovery acceleration but constrained by policy and lobbying
 *   - Regulatory Approval Process: Institutional observer (institutional/arbitrage) — performative element (theater_ratio 0.58 reflects compliance documentation burden)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_synthesis_acceleration, 0.48).
domain_priors:suppression_score(pharmaceutical_synthesis_acceleration, 0.62).
domain_priors:theater_ratio(pharmaceutical_synthesis_acceleration, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_synthesis_acceleration, extractiveness, 0.48).
narrative_ontology:constraint_metric(pharmaceutical_synthesis_acceleration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(pharmaceutical_synthesis_acceleration, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_synthesis_acceleration, tangled_rope).
narrative_ontology:human_readable(pharmaceutical_synthesis_acceleration, "Pharmaceutical Synthesis Acceleration Constraint").
narrative_ontology:topic_domain(pharmaceutical_synthesis_acceleration, "pharmaceutical/biomedical_research").

domain_priors:requires_active_enforcement(pharmaceutical_synthesis_acceleration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_synthesis_acceleration, major_pharmaceutical_firms).
narrative_ontology:constraint_beneficiary(pharmaceutical_synthesis_acceleration, venture_capital_biotech).
narrative_ontology:constraint_victim(pharmaceutical_synthesis_acceleration, academic_chemistry_labs).
narrative_ontology:constraint_victim(pharmaceutical_synthesis_acceleration, generic_drug_manufacturers).
narrative_ontology:constraint_victim(pharmaceutical_synthesis_acceleration, patient_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACADEMIC CHEMISTRY LAB (SNARE) — Trapped by equipment barriers, publication pressure, and funding dependency. Cannot exit the synthesis acceleration race; losing labs are displaced by industry or forced to specialize in support roles. Academic researchers bear extraction through underfunded infrastructure, unfavorable IP terms, and career pressure to adopt proprietary methods.
constraint_indexing:constraint_classification(pharmaceutical_synthesis_acceleration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GENERIC DRUG MANUFACTURERS (SNARE) — Trapped by accelerated patented synthesis routes that shift the innovation boundary. Generic manufacturers cannot compete on synthesis speed; high-throughput methods create moving targets for reverse-engineering. Extraction flows from generics toward patent holders through accelerated obsolescence of known routes.
constraint_indexing:constraint_classification(pharmaceutical_synthesis_acceleration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: BIOTECH STARTUPS (TANGLED_ROPE) — Constrained by capital requirements and time-to-market pressure. These agents both benefit from acceleration (faster development paths, faster FDA approval potential) and bear costs (forced adoption of expensive proprietary platforms, licensing fees, IP entanglement). Mixed extraction and coordination.
constraint_indexing:constraint_classification(pharmaceutical_synthesis_acceleration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR PHARMA (ROPE) — Primary beneficiary (institutional/arbitrage). Controls proprietary high-throughput platforms, AI-assisted retrosynthesis, automated synthesis hardware. Experiences constraint as pure coordination: accelerated synthesis enables faster candidate screening, time-to-market advantage, patent value capture. Net flow is toward pharma.
constraint_indexing:constraint_classification(pharmaceutical_synthesis_acceleration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: VENTURE CAPITAL (ROPE) — Beneficiary (institutional/arbitrage). Acceleration reduces risk-adjusted time horizon for biotech exits; companies that synthesize candidates faster reach clinical trials sooner, shortening venture fund J-curves. Pure coordination from VC perspective; extraction runs toward capital.
constraint_indexing:constraint_classification(pharmaceutical_synthesis_acceleration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PATIENT ACCESS EQUITY (SNARE) — Constrained by pricing barriers and access inequality. Accelerated synthesis increases patentability window (more novel routes defensible) and supports premium pricing. Lower-income populations cannot exit; access gaps widen as acceleration enables stronger IP capture. Pure extraction from patient access perspective.
constraint_indexing:constraint_classification(pharmaceutical_synthesis_acceleration, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PUBLIC HEALTH / GENERIC SYSTEMS (TANGLED_ROPE) — Mobile but constrained by policy. Public health benefits from accelerated discovery (faster access to novel drugs when IP expires) but bears extraction through extended exclusivity windows. Policy intervention is possible but faces pharma lobbying; moderate extraction with residual coordination function.
constraint_indexing:constraint_classification(pharmaceutical_synthesis_acceleration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: REGULATORY APPROVAL (PITON) — FDA/EMA processes are theater-heavy and largely performative relative to actual safety testing. Acceleration rhetoric creates pressure for faster review, but approval process is constrained by biological timeline (clinical trials cannot be accelerated below safety minimums). Regulatory system persists through institutional inertia; speed gains are primarily in synthesis/preclinical, not regulatory. Theater ratio driven by compliance documentation rather than functional acceleration.
constraint_indexing:constraint_classification(pharmaceutical_synthesis_acceleration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational frame, some synthesis complexity is inherent to organic chemistry: bond formation constraints, selectivity limits, and thermodynamic feasibility cannot be fully overcome. This perspective risks naturalizing what is actually a contingent institutional arrangement (IP-driven acceleration incentives) as a natural limit of chemistry itself.
constraint_indexing:constraint_classification(pharmaceutical_synthesis_acceleration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_synthesis_acceleration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_synthesis_acceleration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_synthesis_acceleration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_synthesis_acceleration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_synthesis_acceleration, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_synthesis_acceleration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderately high. The constraint provides genuine coordination benefits (faster drug discovery, reduced development costs) alongside significant extraction mechanisms (IP moat reinforcement, platform licensing fees, academic displacement). The value reflects that acceleration benefits are concentrated in proprietary firms while costs are distributed across powerless and trapped actors. Measurement trajectory (0.25→0.48 over interval) reflects institutional adoption of acceleration platforms and corresponding extraction accumulation as proprietary control deepens. Suppression (0.62): Moderately high. Barriers to independent acceleration include capital requirements (high-throughput equipment costs $5M-50M), specialized expertise concentration, IP protection of synthesis methods, and funding dependency on venture capital aligned with pharma interests. Academic labs face suppression through equipment access barriers and publication bias against synthesis-focused research. Generic manufacturers face suppression through accelerated route obsolescence. Theater ratio (0.58): Moderate-high. Acceleration platforms emphasize speed metrics and throughput claims (marketing theater) while actual discovery bottlenecks remain in candidate screening, target validation, and clinical trials—stages where acceleration has less impact. Regulatory approval process adds theater through compliance documentation. Trajectory (0.35→0.58) reflects increasing emphasis on speed narratives as adoption spreads.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals the constraint's core asymmetry: acceleration technology is captured by institutional beneficiaries (pharma, venture capital) who experience it as pure coordination (Rope). All powerless and constrained actors (academic labs, generics, patient access, biotech startups) experience extraction (Snare or Tangled Rope). The gap is not epistemic but structural—the constraint genuinely delivers different outcomes to different actors based on their access to proprietary platforms. No amount of analytical perspective-taking bridges this gap because it reflects real institutional control asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharma's d ≈ 0.05 (beneficiary + arbitrage): Full control of acceleration platforms; they set the speed and licensing terms. Venture capital's d ≈ 0.08 (beneficiary + arbitrage): Invests in firms with platform access; compressed venture horizons reduce risk. Academic labs' d ≈ 0.92 (trapped victim): Displaced by institutional barriers; no exit without abandoning drug discovery. Generics' d ≈ 0.88 (trapped victim): Accelerated route obsolescence locks them into reverse-engineering known methods. Biotech startups' d ≈ 0.55 (constrained mixed): Access to platforms requires licensing; some benefit from accelerated development, but forced adoption extracts fees. Patient access' d ≈ 0.85 (constrained victim): No input into access policy; extended exclusivity windows reduce generic availability. Public health's d ≈ 0.48 (powerful but constrained): Policy levers exist but are captured; residual agency remains. Regulatory system's d ≈ 0.15 (institutional with extracted value): Approval demand rises with acceleration pipeline volume; performative compliance creates revenue streams for regulatory consultancies.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by showing that acceleration is a genuine mixed constraint—NOT pure coordination masked as extraction, and NOT pure extraction masked as coordination. Academic displacement is real extraction (snare-level for trapped actors). Generic manufacturer obsolescence is real extraction (snare-level through accelerated route cycling). Yet acceleration also delivers real coordination gains: faster drug discovery, reduced development costs, improved time-to-clinical-trial. The constraint is Tangled Rope at the institutional median because it requires active enforcement (proprietary platform licensing, IP protection, venture capital deployment) to maintain asymmetric distribution of benefits. The pitfall to avoid: claiming acceleration is 'just natural chemistry complexity' (false mountain) that naturally favors well-resourced actors. The institutional arrangement is contingent—it could be organized differently (open-source platforms, public synthesis infrastructure, patent reform) to distribute benefits more widely. The theater ratio (0.58) reflects this contingency: much acceleration rhetoric is performative marketing (speed claims, throughput metrics) that obscures that actual bottlenecks remain in target discovery and clinical validation—stages where acceleration provides less impact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthesis_bottleneck_location,
    'Is the actual synthesis bottleneck chemical complexity (inherent selectivity/yield limits) or institutional/economic barriers (equipment access, capital requirements, IP licensing)?',
    'Comparative analysis: open-source synthesis routes vs proprietary methods for identical compounds; measurement of yield/selectivity improvements attributable to hardware vs methodology vs IP control',
    'If chemical: acceleration is bounded by natural limits (mountain elements persist). If institutional: acceleration is extractive rent-seeking (snare elements dominate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthesis_bottleneck_location, empirical, 'Whether synthesis limitations are chemical or institutional').

omega_variable(
    open_source_platform_viability,
    'Can open-source synthesis platforms (e.g., community-developed retrosynthesis algorithms, open-source lab automation) provide comparable acceleration to proprietary systems?',
    'Head-to-head comparison of synthesis time and cost for identical drug candidates using open vs proprietary platforms; longitudinal tracking of open-source project maturity and adoption rates',
    'If viable: acceleration is accessible, constraining pharma rent capture. If not: acceleration becomes proprietary moat reinforcing extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_source_platform_viability, empirical, 'Viability of open-source synthesis acceleration platforms').

omega_variable(
    ip_extension_mechanism,
    'Does accelerated synthesis actually extend patent protection duration or merely create new patentable routes while old routes expire?',
    'Patent database analysis: correlation between synthesis acceleration adoption and effective patent exclusivity periods; examination of incremental vs breakthrough patent strategies post-acceleration',
    'If extends: constraint is extraction mechanism (higher χ). If creates new routes: constraint is coordination with extractive overlay (tangled rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ip_extension_mechanism, empirical, 'Whether synthesis acceleration extends or redistributes patent protection').

omega_variable(
    access_equity_causal_link,
    'Does accelerated synthesis methodology adoption causally worsen patient access equity or is access inequality driven by independent pricing/policy factors?',
    'Causal inference analysis: drug prices and generic availability before/after synthesis acceleration adoption; confound adjustment for policy changes, patent reforms, regulatory shifts',
    'If causal: patient_access_equity bears extraction. If confounded: constraint is neutral on access; other forces are primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_equity_causal_link, empirical, 'Causal relationship between synthesis acceleration and access equity').

omega_variable(
    academic_lab_displacement_threshold,
    'At what acceleration rate do academic chemistry labs become functionally obsolete in drug discovery, and is displacement irreversible?',
    'Historical analysis of academic lab closures and scope reductions post-adoption of industry acceleration standards; tracking of academic chemists'' career transitions and research area shifts',
    'If threshold crossed: academic labs are experiencing snare-level extraction. If reversible: exit options are constrained but not trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_lab_displacement_threshold, empirical, 'Academic lab displacement timeline and reversibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_synthesis_acceleration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharma_synth_tr_t0, pharmaceutical_synthesis_acceleration, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pharma_synth_tr_t5, pharmaceutical_synthesis_acceleration, theater_ratio, 5, 0.48).
narrative_ontology:measurement(pharma_synth_tr_t10, pharmaceutical_synthesis_acceleration, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(pharma_synth_be_t0, pharmaceutical_synthesis_acceleration, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(pharma_synth_be_t5, pharmaceutical_synthesis_acceleration, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(pharma_synth_be_t10, pharmaceutical_synthesis_acceleration, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_synthesis_acceleration, resource_allocation).
narrative_ontology:affects_constraint(pharmaceutical_synthesis_acceleration, pharmaceutical_patent_protection).
narrative_ontology:affects_constraint(pharmaceutical_synthesis_acceleration, generic_drug_access).
narrative_ontology:affects_constraint(pharmaceutical_synthesis_acceleration, academic_biomedical_research_funding).

% DUAL FORMULATION NOTE:
% Synthesis acceleration decomposes into three structurally distinct constraints: (1) technological acceleration (hardware/algorithm capability), (2) institutional control of acceleration platforms (who owns/licenses technology), (3) policy/pricing effects (how acceleration translates to access). This story focuses on institutional control and its extractive effects. Technological acceleration has lower ε (≈0.15, Rope/Mountain); pricing effects have higher ε when combined with IP strategy (downstream story).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pharmaceutical_synthesis_acceleration, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
