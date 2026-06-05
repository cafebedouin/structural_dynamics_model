% ============================================================================
% CONSTRAINT STORY: doctoral_training_debt_pipeline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doctoral_training_debt_pipeline, []).

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
 *   constraint_id: doctoral_training_debt_pipeline
 *   human_readable: Doctoral Training Debt Pipeline: Extraction Through Educational Promise
 *   domain: education/labor/economic
 *
 * SUMMARY:
 *   The doctoral training pipeline structures knowledge production and
 *   researcher development in advanced economies, creating a system where
 *   aspiring researchers commit 5-7 years to subsidized labor with uncertain
 *   career outcomes. The constraint exhibits the classical tangled rope
 *   pattern: genuine coordination function (training the next generation,
 *   producing research) layered with asymmetric extraction (years of
 *   below-market labor, career dependence, credential gatekeeping). The
 *   constraint's extractiveness has increased over the measured interval as
 *   labor market outcomes for PhD holders have degraded (most PhDs exit
 *   academia) while training intensity has increased. Theater ratio has also
 *   increased as prestige metrics and rankings have proliferated, creating
 *   performative signals of research quality that do not translate to
 *   improved career outcomes. The constraint operates through multiple
 *   psychological and institutional mechanisms: identity fusion (researcher
 *   self-concept), sunk cost fallacy (years invested), and credential lock-in
 *   (PhD required to access certain positions). Six distinct perspectives
 *   emerge: the doctoral candidate experiences snare-like entrapment;
 *   early-career researchers experience tangled rope mixed coordination and
 *   extraction; universities experience rope-like coordination; labor
 *   organizers see a scaffold with sunset mechanisms; prestige institutions
 *   see piton-like degradation; and analytical observers risk naturalizing
 *   the constraint as an inevitable feature of knowledge economies.
 *
 * KEY AGENTS:
 *   - Doctoral Candidates: Primary victims (powerless/trapped) — bear full extraction cost of subsidized labor, credential dependence, uncertain career outcomes
 *   - Early-Career Researchers: Secondary victims (moderate/constrained) — constrained by postdoc precarity, publication pressure, limited faculty positions; also benefit from knowledge networks
 *   - Research Universities: Primary beneficiary (institutional/arbitrage) — receive subsidized research labor, prestige metrics, grant overhead; exit costs are zero
 *   - Faculty Advisors: Secondary beneficiary (institutional/constrained) — depend on student labor for publication productivity, grant execution; face career pressure to mentor but have limited capacity
 *   - Graduate Workers Movement: Organized collective action (organized/constrained) — unionization campaigns, labor organizing building sunset mechanism through wage floors and benefits expansion
 *   - Institutional Prestige System: Performative structure (institutional/arbitrage) — rankings, citation metrics, advisor reputation maintain pipeline through theater despite degraded career outcomes
 *   - Knowledge Accessibility Commons: Structural victim (powerless/trapped) — bears epistemic cost of knowledge sequestration in long dissertations, delayed publication, gatekeeping through advisor access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doctoral_training_debt_pipeline, 0.58).
domain_priors:suppression_score(doctoral_training_debt_pipeline, 0.65).
domain_priors:theater_ratio(doctoral_training_debt_pipeline, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doctoral_training_debt_pipeline, extractiveness, 0.58).
narrative_ontology:constraint_metric(doctoral_training_debt_pipeline, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(doctoral_training_debt_pipeline, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doctoral_training_debt_pipeline, tangled_rope).
narrative_ontology:human_readable(doctoral_training_debt_pipeline, "Doctoral Training Debt Pipeline: Extraction Through Educational Promise").
narrative_ontology:topic_domain(doctoral_training_debt_pipeline, "education/labor/economic").

domain_priors:requires_active_enforcement(doctoral_training_debt_pipeline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doctoral_training_debt_pipeline, university_research_enterprise).
narrative_ontology:constraint_beneficiary(doctoral_training_debt_pipeline, faculty_publication_incentives).
narrative_ontology:constraint_beneficiary(doctoral_training_debt_pipeline, institutional_prestige_metrics).
narrative_ontology:constraint_victim(doctoral_training_debt_pipeline, doctoral_candidates).
narrative_ontology:constraint_victim(doctoral_training_debt_pipeline, early_career_researchers).
narrative_ontology:constraint_victim(doctoral_training_debt_pipeline, knowledge_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DOCTORAL CANDIDATE (SNARE) — Trapped by credential lock-in, career pathway dependence, and sunk cost investment. Exit costs are catastrophic: abandoning doctoral credentials means forfeiting career trajectory, losing access to funded positions, and bearing psychological cost of 'failure.' Minimal coordination benefit — the constraint primarily extracts 5-7 years of subsidized labor for research production with no guaranteed outcome. Maximum experienced suppression from structural position.
constraint_indexing:constraint_classification(doctoral_training_debt_pipeline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE EARLY-CAREER RESEARCHER (TANGLED ROPE) — Constrained by postdoc precarity and limited faculty positions but also coordinating knowledge production with peer networks. Experiences both extraction (unpaid or underpaid labor, publication pressure, grant-writing burden) and coordination benefit (mentorship, research access, knowledge community). Significant cost to exit (retraining, geographic relocation) but some agency through funding diversification and lateral moves within academia.
constraint_indexing:constraint_classification(doctoral_training_debt_pipeline, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE RESEARCH UNIVERSITY (ROPE) — Experiences the constraint as pure coordination: doctoral students are the mechanism for research production, knowledge dissemination, and training the next generation. Exit options abundant (can reduce doctoral cohorts, hire postdocs instead, outsource research) but arbitrage available through prestige metrics and grant overhead. Sees the constraint as solving a genuine coordination problem: how to fund research while training researchers. Low experienced extraction because exit is costless.
constraint_indexing:constraint_classification(doctoral_training_debt_pipeline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE GRADUATE WORKERS MOVEMENT (SCAFFOLD) — Organized collective action (unionization, labor organizing) reveals the constraint as temporary and soluble through power redistribution. Sees sunset mechanism in wage floors, benefits expansion, and credential recognition. The movement is building alternative verification pathways for labor value (union contracts, grievance procedures) that bypass the prestige-based extraction logic. Scaffold classification reflects that suppression is declining as organizing capacity grows.
constraint_indexing:constraint_classification(doctoral_training_debt_pipeline, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL PRESTIGE SYSTEM (PITON) — Rankings, citation metrics, and reputation hierarchies maintain the doctoral pipeline through performative measurement. The system has degraded significantly: doctoral credentials no longer guarantee academic employment (most PhDs exit academia), yet the pipeline persists through theater (departmental prestige rankings, advisor reputation, publication metrics). The prestige system performs verification of research quality but provides no actual guarantee of career placement or knowledge impact. Maintained through institutional inertia despite reduced functional value.
constraint_indexing:constraint_classification(doctoral_training_debt_pipeline, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — From a universal perspective, the doctoral pipeline might appear as an inevitable feature of knowledge economies: all advanced economies require research training, all require knowledge production, and all require credential systems to signal competence. This perspective risks naturalizing the constraint as an immutable law of modern science and education. However, structural comparison with other models (German apprenticeship systems, direct-hire research positions, distributed mentorship networks) reveals the pipeline as contingent institutional arrangement, not natural law. The mountain classification is a false summit.
constraint_indexing:constraint_classification(doctoral_training_debt_pipeline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doctoral_training_debt_pipeline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(doctoral_training_debt_pipeline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(doctoral_training_debt_pipeline, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(doctoral_training_debt_pipeline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(doctoral_training_debt_pipeline, TR),
    TR >= 0.70.

:- end_tests(doctoral_training_debt_pipeline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. The doctoral pipeline extracts 5-7 years of below-market labor (stipends $20k-30k annually vs. postdoc market rate $60k-80k). Extraction magnitude grew from 0.35 to 0.58 as labor market outcomes deteriorated (fewer PhDs achieve academic positions) while training duration remained constant. The constraint is not purely extractive — genuine training coordination exists — but the gap between training promise and labor market reality has widened the extraction component. Suppression (0.65): High. Doctoral candidates face multiple overlapping barriers to exit: sunk cost (5-7 years already invested), credential lock-in (PhD required for certain positions), visa dependence (international students), funding contingency (stipend tied to enrollment), and identity fusion (researcher self-concept internalized). Not all suppression is structural — some is internalized through identity lock — but the structural components (funding, visa status, credential requirements) are substantial. Theater ratio (0.62): Moderate-high, increasing over interval. Prestige metrics (advisor reputation, departmental rankings, publication counts) proliferated as doctoral labor markets tightened, creating performative signals that do not predict career outcomes. Doctoral training explicitly performs verification of research ability, but the actual predictive signal for career success has weakened (most PhDs do not secure academic positions). Theater increased from 0.42 to 0.62 as institutional emphasis on rankings and metrics intensified.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals how the same structural phenomenon — doctoral training — is perceived entirely differently across positions. The university sees coordination; the candidate sees entrapment; the early-career researcher sees mixed extraction and benefit; the labor organizers see a solvable temporary problem; the prestige metrics see their own theater; the analytical observer risks naturalizing contingency. The perspectival gap is not resolvable by clarifying the facts — all perspectives are describing real structural features of the system. Rather, the gap reflects how different positions in the extraction pipeline have access to different information and face different constraints. The doctoral candidate cannot perceive the university's coordination rationale because they are trapped within the extraction mechanism. The university cannot perceive the candidate's entrapment because the arrangement is voluntary at the institutional level (universities could hire postdocs instead). The labor organizers can see both extraction and coordination because organizing requires analyzing the system from outside the individual candidate position while remaining inside the institutional reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from the structural relationship to the extraction flow: Who benefits? Universities receive subsidized research labor and prestige metrics; faculty gain publication productivity and student networks; prestige institutions gain ranking advantage. Who bears costs? Doctoral candidates bear years of below-market labor and career uncertainty; knowledge accessibility bears epistemic cost from delayed publication; early-career researchers bear postdoc precarity. Exit options determine how experienced extractiveness is scaled: universities have zero cost to exit (hire postdocs instead), so d ≈ 0.1 (negative f(d), negative χ). Candidates have catastrophic exit costs (credential sunk, career derailment), so d ≈ 0.95 (positive f(d), positive χ). Organizers have declining exit costs through unionization (sunset mechanism), so d declines from 0.60 to 0.40 over the measurement interval. The formula χ = ε × f(d) × σ(S) applies: extractiveness is scaled by directionality (whether you're a beneficiary or victim) and by scope (national scope σ(S) = 1.0; global scope slightly increases comparable extractiveness across borders).
 *
 * MANDATROPHY ANALYSIS:
 *   The doctoral pipeline resolves mandatrophy through perspectival decomposition. The temptation is to ask: 'Is this coordination (Rope) or extraction (Snare)?' The mandate-resolving answer is: 'Both, depending on position.' From the university's arbitrage-exit institutional position, it is coordination (Rope). From the doctoral candidate's trapped powerless position, it is extraction (Snare). Both classifications are correct — they are measuring the same constraint from different structural positions. The tangled rope classification from the analytical moderate perspective reflects the mixed experience: moderate agents see both the coordination function and the asymmetric extraction. The mandatrophy does not resolve to 'which type is true' but to 'which perspectives are coherent and which are naturalizations of contingency.' The mountain perspective (analytical/civilizational/analytical) is the false summit — the constraint appears immutable only when viewed from a position of universal abstraction that ignores the specific institutional arrangements (credential gatekeeping, funding structures, prestige metrics) that make it contingent. Removing any of these institutional features would make the pipeline soluble.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_inflation_spiral,
    'Is the doctoral pipeline self-sustaining through credential inflation (PhD required to access research positions), or could alternative credentialing systems replace it?',
    'Historical analysis of credential requirements in research employment; comparative study of non-U.S. research credentialing systems; labor market simulation with reduced PhD supply',
    'If self-sustaining: trap is structural and requires institutional disruption to break. If replaceable: alternative credentialing can create sunset mechanism, supporting scaffold classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_spiral, empirical, 'Whether PhD credential requirement is self-perpetuating or replaceable').

omega_variable(
    research_productivity_attribution,
    'What proportion of research output attributed to ''doctoral training'' is genuinely training vs. subsidized labor extraction?',
    'Longitudinal tracking of doctoral student research outcomes; measurement of knowledge transfer, skill acquisition, and publication equity; comparison with postdoc research output per capita',
    'If > 60% is genuine training: tangled_rope classification sustained. If < 40% is training: reclassify as snare with minimal coordination function. If 40-60%: tangled_rope confirmed with moderate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(research_productivity_attribution, empirical, 'Ratio of genuine training to subsidized labor in doctoral research').

omega_variable(
    postdoctoral_substitution_feasibility,
    'Could universities sustainably replace doctoral students with hired postdoctoral researchers at market wages without reducing research output?',
    'Cost-benefit analysis of postdoc hiring at $60k-80k annual salary vs. doctoral stipends ($20k-30k); measurement of research output per dollar across models; survey of faculty preference constraints',
    'If substitution feasible: reveals that doctoral pipeline is extraction mechanism chosen for cost, not coordination necessity. Supports snare classification. If infeasible: supports rope coordination rationale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(postdoctoral_substitution_feasibility, empirical, 'Whether postdoctoral hiring could replace doctoral student research').

omega_variable(
    career_outcome_credential_value,
    'Does the doctoral credential actually increase lifetime earning potential or career security relative to terminal master''s degree + research position entry?',
    'Longitudinal career tracking cohort studies; salary comparison at 5, 10, 15-year marks; employment stability metrics; underemployment rates by credential type',
    'If credential provides genuine value: training coordination is real, supports rope/tangled_rope. If credential provides minimal premium or negative premium (overtraining for non-academic paths): credential is theater, supports snare/piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(career_outcome_credential_value, empirical, 'Whether PhD credential provides genuine career advantage').

omega_variable(
    suppression_internalization_mechanism,
    'Is suppression of doctoral candidate exit structurally enforced (economic dependency, visa status) or internalized (identity fusion with research identity, intellectual ambition narrative)?',
    'Qualitative analysis of exit narratives; comparison of exit rates across funding models (fully funded vs. self-funded); measurement of identity-locked indicators (self-concept dependence on PhD status, difficulty imagining non-research identity)',
    'If structural: exit barrier removal could solve constraint. If internalized: constraint persists after structural barriers removed due to identity lock. Affects suppression mechanism classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression is structural or internalized identity lock').

omega_variable(
    knowledge_accessibility_harm_measurement,
    'What is the measurable epistemic harm caused by doctoral training delays (years-long publication timelines, gatekeeping through advisor access, knowledge sequestration in theses)?',
    'Citation lag analysis; comparison of publication rates with and without doctoral training requirement; measurement of knowledge replication and accessibility across discipline boundaries',
    'If harm is significant: victims array should include knowledge accessibility, supporting snare classification. If harm is minimal: coordination benefit dominates, supporting rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_accessibility_harm_measurement, empirical, 'Epistemic harm from knowledge sequestration in doctoral training').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doctoral_training_debt_pipeline, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dtdp_tr_t0, doctoral_training_debt_pipeline, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dtdp_tr_t15, doctoral_training_debt_pipeline, theater_ratio, 15, 0.54).
narrative_ontology:measurement(dtdp_tr_t30, doctoral_training_debt_pipeline, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(dtdp_be_t0, doctoral_training_debt_pipeline, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dtdp_be_t15, doctoral_training_debt_pipeline, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(dtdp_be_t30, doctoral_training_debt_pipeline, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doctoral_training_debt_pipeline, identity_coordination).
narrative_ontology:boltzmann_floor_override(doctoral_training_debt_pipeline, 0.12).
narrative_ontology:affects_constraint(doctoral_training_debt_pipeline, academic_labor_market_precarity).
narrative_ontology:affects_constraint(doctoral_training_debt_pipeline, publication_prestige_hierarchy).
narrative_ontology:affects_constraint(doctoral_training_debt_pipeline, credential_inflation_spiral).

% DUAL FORMULATION NOTE:
% The doctoral pipeline is part of a constraint family with three structurally distinct stories: (1) doctoral_training_debt_pipeline (ε=0.58, Tangled Rope) — the training apparatus itself; (2) academic_labor_market_precarity (ε=0.70, Snare) — the employment outcomes for PhD holders; (3) publication_prestige_hierarchy (ε=0.52, Tangled Rope) — the metrics system that maintains the pipeline through theater. Each story has its own extractiveness value reflecting different observables. The pipeline story focuses on training-labor asymmetry; the labor market story focuses on credential oversupply; the prestige story focuses on performative measurement. The three stories are causally linked: the prestige hierarchy drives the pipeline, which floods the labor market, which creates precarity that justifies expanded doctoral training as the only career path for ambitious researchers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doctoral_training_debt_pipeline, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
