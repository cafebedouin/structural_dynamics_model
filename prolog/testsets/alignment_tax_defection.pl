% ============================================================================
% CONSTRAINT STORY: alignment_tax_defection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alignment_tax_defection, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: alignment_tax_defection
 *   human_readable: Alignment Tax Defection in AI Development
 *   domain: ai_development/technology_governance/organizational_dynamics
 *
 * SUMMARY:
 *   The alignment tax defection constraint describes a multi-agent trap in AI
 *   development: individual labs face structural incentives to reduce safety
 *   investment when competitors may defect, even when all labs would prefer
 *   coordinated safety investment to a race-to-the-bottom. This is a classic
 *   public goods problem where safety infrastructure is systematically
 *   underproduced. The constraint exhibits high extractiveness (0.68) because
 *   the competitive dynamic forces labs to externalize catastrophic risk onto
 *   downstream populations and the epistemic commons. Suppression is high
 *   (0.72) because exit options are limited: labs that unilaterally invest in
 *   safety lose competitive position, talent, and funding; labs cannot exit
 *   the race without organizational death. Theater ratio (0.58) reflects that
 *   safety commitments are increasingly performative: voluntary pledges,
 *   ethics boards, and safety teams operate without binding enforcement
 *   mechanisms or third-party verification. The constraint has intensified
 *   over the 6-year interval as capability scaling has accelerated, capital
 *   concentration has increased, and the competitive landscape has narrowed
 *   to a small number of frontier labs. The measurements show monotonic
 *   increases in all three metrics, indicating lifecycle drift toward greater
 *   extraction, suppression, and theater.
 *
 * KEY AGENTS:
 *   - First-Mover Lab: Primary beneficiary (institutional/arbitrage) — captures market dominance, talent acquisition, regulatory capture opportunity through early deployment
 *   - Investor Class: Secondary beneficiary (institutional/arbitrage) — captures returns from first-mover rents; short time horizons create structural pressure for defection
 *   - Collective Safety Infrastructure: Primary victim (powerless/trapped) — epistemic commons of safety research and institutional oversight capacity; systematically underproduced as public good
 *   - Downstream Affected Populations: Primary victim (powerless/trapped) — bear deployment risks (labor displacement, algorithmic harm, capability misuse) with no exit and no voice
 *   - Safety-Focused Researchers: Secondary victim (moderate/constrained) — face career penalties for safety work but benefit from coordination infrastructure when it exists
 *   - Second-Tier Labs: Mixed position (institutional/constrained) — cannot win the race but cannot exit without organizational death; forced into under-investment in safety
 *   - International Governance Coalition: Organized agents (organized/mobile) — building binding frameworks with verification mechanisms; see constraint as temporary with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies the constraint as a pure multi-agent trap with coordination story as cover for extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alignment_tax_defection, 0.68).
domain_priors:suppression_score(alignment_tax_defection, 0.72).
domain_priors:theater_ratio(alignment_tax_defection, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alignment_tax_defection, extractiveness, 0.68).
narrative_ontology:constraint_metric(alignment_tax_defection, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(alignment_tax_defection, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alignment_tax_defection, snare).
narrative_ontology:human_readable(alignment_tax_defection, "Alignment Tax Defection in AI Development").
narrative_ontology:topic_domain(alignment_tax_defection, "ai_development/technology_governance/organizational_dynamics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alignment_tax_defection, first_mover_lab).
narrative_ontology:constraint_beneficiary(alignment_tax_defection, investor_class).
narrative_ontology:constraint_victim(alignment_tax_defection, collective_safety_infrastructure).
narrative_ontology:constraint_victim(alignment_tax_defection, downstream_affected_populations).
narrative_ontology:constraint_victim(alignment_tax_defection, safety_focused_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COLLECTIVE SAFETY INFRASTRUCTURE (SNARE) — The epistemic commons of AI safety research and the institutional capacity for oversight cannot exit the race dynamic. Bears full extraction: safety investment is systematically underproduced as a public good. No advocate, no exit, maximum experienced extraction.
constraint_indexing:constraint_classification(alignment_tax_defection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM AFFECTED POPULATIONS (SNARE) — Populations exposed to deployment risks (labor displacement, algorithmic harm, capability misuse) have no exit from the deployment trajectory and no voice in the race dynamics. Pure extraction: bear costs with no agency.
constraint_indexing:constraint_classification(alignment_tax_defection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SAFETY-FOCUSED RESEARCHERS (TANGLED ROPE) — Individual researchers face career constraints: safety work is less publishable, less fundable, and carries reputational risk if it delays deployment. But they also benefit from the coordination function: labs that invest in safety create research infrastructure, datasets, and methods that advance the field. Mixed extraction: constrained by career incentives but not fully trapped.
constraint_indexing:constraint_classification(alignment_tax_defection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FIRST-MOVER LAB (ROPE) — The lab that deploys first captures market dominance, talent acquisition advantage, and regulatory capture opportunity. Experiences the constraint as coordination: the race dynamic is a competitive mechanism that rewards speed. Net beneficiary with arbitrage exit: can shift resources between safety and capability as strategic conditions change.
constraint_indexing:constraint_classification(alignment_tax_defection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SECOND-TIER LAB (TANGLED ROPE) — Labs without first-mover advantage face a dilemma: investing in safety reduces competitive position, but racing without safety increases catastrophic risk that could destroy the entire sector. Mixed coordination and extraction: the race dynamic coordinates competitive behavior but extracts through forced under-investment in safety. Constrained exit: cannot leave the race without losing funding and talent.
constraint_indexing:constraint_classification(alignment_tax_defection, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL GOVERNANCE COALITION (SCAFFOLD) — Organized actors (IAEA-style verification regimes, compute governance frameworks, international safety standards) see the defection dynamic as a temporary coordination failure with a sunset: binding international agreements with verification mechanisms can internalize the externality. Sunset logic: as governance infrastructure matures, the race-to-the-bottom loses force. Estimated timeline: 5-15 years for binding frameworks to emerge.
constraint_indexing:constraint_classification(alignment_tax_defection, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the alignment tax defection is a pure multi-agent trap: all labs prefer coordinated safety investment, but unilateral safety investment is dominated by defection in the absence of enforcement. The coordination story (labs are solving the legitimate problem of capability development) is cover for extraction (systematic underproduction of safety as a public good). High extractiveness, high suppression (exit suppressed by competitive dynamics and capital concentration), substantial theater (safety commitments without binding mechanisms).
constraint_indexing:constraint_classification(alignment_tax_defection, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alignment_tax_defection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alignment_tax_defection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alignment_tax_defection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alignment_tax_defection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(alignment_tax_defection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The competitive dynamic systematically underproduces safety investment as a public good. Labs externalize catastrophic risk onto downstream populations and the epistemic commons. The extraction is not total (some safety work occurs, some labs have longer time horizons) but is substantial and increasing. The value reflects that defection is individually rational in the absence of enforcement, even when all labs prefer coordination. Suppression (0.72): High. Exit options are severely limited by competitive dynamics and capital concentration. Labs that unilaterally invest in safety lose market position, talent, and funding. Labs cannot exit the race without organizational death. The suppression is structural rather than coercive: no single actor enforces the race dynamic, but the multi-agent structure makes defection dominant. Theater ratio (0.58): Moderate-high. Safety commitments are increasingly performative: voluntary pledges without binding mechanisms, ethics boards without veto power, safety teams without resource guarantees, third-party audits without verification infrastructure. The theater has increased over the interval as competitive pressure has intensified and the gap between stated commitments and actual resource allocation has widened. The value is not higher because some labs do maintain genuine safety programs, and some coordination infrastructure (model cards, red-teaming, capability evaluations) has real function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a characteristic snare pattern: beneficiaries see coordination (rope), victims see extraction (snare), and intermediate agents see mixed dynamics (tangled rope). The first-mover lab experiences the race as a competitive coordination mechanism that rewards speed and capability. The collective safety infrastructure and downstream populations experience pure extraction: systematic underproduction of safety with no exit and no voice. Safety-focused researchers and second-tier labs experience the constraint as both coordination (the race does produce capability advances and some safety infrastructure) and extraction (career penalties, forced under-investment, externalized risk). The international governance coalition sees a temporary coordination failure with a sunset: binding frameworks can internalize the externality. The analytical observer identifies the multi-agent trap: all labs prefer coordinated safety investment, but unilateral investment is dominated by defection in the absence of enforcement. The perspectival gap is not a disagreement about facts but a structural consequence of different positions in the extraction flow. The coordination story is not false from the beneficiary's perspective — they genuinely experience coordination. But the analytical perspective reveals that the coordination function is cover for systematic extraction from those without exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. First-mover labs and investors are primary beneficiaries: they capture rents from early deployment and have arbitrage exit options (can shift resources strategically). Their directionality is low (near 0.0), producing low or negative effective extraction — they experience the constraint as coordination. Collective safety infrastructure and downstream populations are primary victims with no exit: they bear the full cost of underproduced safety. Their directionality is maximum (1.0), producing maximum effective extraction — they experience the constraint as pure extraction. Safety-focused researchers and second-tier labs occupy intermediate positions: constrained by career and competitive dynamics but not fully trapped, benefiting from coordination infrastructure when it exists but bearing costs from the race dynamic. Their directionality is moderate (0.4-0.6), producing moderate effective extraction — they experience the constraint as mixed coordination and extraction (tangled rope). The international governance coalition has mobile exit and sees the constraint as temporary (scaffold): their directionality is low because they have agency to build alternative coordination mechanisms. The analytical observer identifies the structural trap from outside: high directionality because the analysis reveals the extraction mechanism that the coordination story obscures.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the classification depends on the observer's structural position and the time horizon of analysis. From the first-mover lab's immediate perspective, the constraint is coordination (rope): the race dynamic rewards capability development and creates competitive incentives that drive innovation. From the collective safety infrastructure's perspective, the constraint is pure extraction (snare): safety is systematically underproduced as a public good, and the epistemic commons bears the cost. From the safety researcher's perspective, the constraint is mixed (tangled rope): career incentives penalize safety work, but coordination infrastructure does exist when labs invest in it. From the governance coalition's generational perspective, the constraint is temporary (scaffold): binding international frameworks with verification mechanisms can internalize the externality and restore incentive compatibility. From the analytical civilizational perspective, the constraint is a multi-agent trap (snare): the coordination story is cover for extraction, and the race dynamic is a structural failure mode that all labs would prefer to escape but cannot without enforcement. The mandatrophy is not 'which type is correct?' but 'which perspective and time horizon are you measuring from?' All classifications are legitimate readings of the same structural data from different observation sites.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_threshold,
    'What enforcement mechanism strength is sufficient to make safety investment incentive-compatible for all labs simultaneously?',
    'Game-theoretic modeling of enforcement regimes; empirical observation of compliance rates under different governance frameworks (compute monitoring, third-party audits, liability regimes)',
    'If threshold is low (voluntary commitments + transparency): scaffold perspective confirmed, sunset is near. If threshold is high (requires state coercion + verification): snare persists until geopolitical coordination, timeline extends to decades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_threshold, empirical, 'Enforcement strength required for incentive-compatible safety investment').

omega_variable(
    capability_discontinuity,
    'Does recursive self-improvement create a discontinuous jump in capability that makes incremental safety investment obsolete?',
    'Empirical observation of capability scaling curves; detection of phase transitions in model behavior; measurement of safety technique transferability across capability regimes',
    'If discontinuous: safety investment before the jump is wasted effort, defection is rational even for safety-preferring labs. If continuous: incremental safety investment accumulates, coordination is achievable. Determines whether the constraint is a genuine dilemma or a false framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_discontinuity, empirical, 'Whether capability growth is continuous or discontinuous').

omega_variable(
    safety_tax_magnitude,
    'What is the true competitive cost of state-of-the-art safety investment as a percentage of total development cost?',
    'Detailed cost accounting from labs with transparent safety programs; comparison of time-to-deployment for safety-invested vs racing models at equivalent capability scores',
    'If tax < 10%: defection is cheap talk, not structural. If tax > 30%: defection is rational even with strong safety preference. Determines whether the constraint is coordination (low tax) or extraction (high tax).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_tax_magnitude, empirical, 'Competitive cost of safety investment').

omega_variable(
    investor_time_horizon,
    'Do AI lab investors have short-term exit horizons (3-5 years to liquidity event) or long-term value horizons (10+ years)?',
    'Analysis of investor composition (venture capital vs sovereign wealth vs founder-controlled); empirical observation of deployment pressure during funding rounds; correlation between investor type and safety investment levels',
    'If short-term: investors structurally prefer defection (capture first-mover rents before risks materialize). If long-term: investors prefer coordination (sector-wide catastrophic risk destroys portfolio value). Determines whether capital structure is a binding constraint or a governance target.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(investor_time_horizon, empirical, 'Investor time horizon and risk exposure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alignment_tax_defection, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(align_tax_theater_t0, alignment_tax_defection, theater_ratio, 0, 0.35).
narrative_ontology:measurement(align_tax_theater_t2, alignment_tax_defection, theater_ratio, 2, 0.44).
narrative_ontology:measurement(align_tax_theater_t4, alignment_tax_defection, theater_ratio, 4, 0.52).
narrative_ontology:measurement(align_tax_theater_t6, alignment_tax_defection, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(align_tax_extract_t0, alignment_tax_defection, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(align_tax_extract_t2, alignment_tax_defection, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(align_tax_extract_t4, alignment_tax_defection, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(align_tax_extract_t6, alignment_tax_defection, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(align_tax_suppress_t0, alignment_tax_defection, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(align_tax_suppress_t2, alignment_tax_defection, suppression_requirement, 2, 0.62).
narrative_ontology:measurement(align_tax_suppress_t4, alignment_tax_defection, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(align_tax_suppress_t6, alignment_tax_defection, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alignment_tax_defection, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of automation_velocity_vs_oversight_capacity (the speed of capability development outpaces institutional oversight capacity, creating the conditions for the race dynamic) and recursive_capability_threshold (the possibility of discontinuous capability jumps changes the game-theoretic structure of safety investment). The alignment tax defection is a distinct structural constraint with its own extractiveness value reflecting the multi-agent trap dynamics, separate from the upstream constraints' values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
