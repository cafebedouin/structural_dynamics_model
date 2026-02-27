% ============================================================================
% CONSTRAINT STORY: narrative_capacity_exhaustion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_narrative_capacity_exhaustion, []).

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
 *   constraint_id: narrative_capacity_exhaustion
 *   human_readable: The Storytelling Burnout: Narrative Capacity Exhaustion in Digital Content Creation
 *   domain: social/technological
 *
 * SUMMARY:
 *   The Storytelling Burnout represents a constraint where the ostensible
 *   coordination function of digital platforms — connecting creators with
 *   audiences seeking authentic narrative — becomes a treadmill that demands
 *   output far exceeding creators' lived experience inventory. Initially, the
 *   ecosystem appeared as pure coordination (Rope): creators share stories,
 *   audiences discover them, platforms enable connection. However, as
 *   algorithmic reward systems incentivize publishing frequency, audience
 *   attention fragmentation requires constant novelty, and platform income
 *   depends on consistent engagement, the constraint shifted toward
 *   extraction (Tangled Rope, then Snare). Creators face a forced choice:
 *   maintain publishing cadence by fabricating narratives, mining others'
 *   stories, or retreating into genre formulas; or exit the platform and lose
 *   audience and income. The suppression is structural — alternative
 *   platforms exist but lack network effects and monetization maturity;
 *   switching costs are high; and the cultural expectation of 'authentic
 *   constant production' penalizes gaps. The theater ratio reflects that
 *   contemporary content creation has become substantially performative:
 *   creators perform the role of authentic storytellers experiencing constant
 *   insight and growth, while the actual narrative production is decoupled
 *   from lived experience. This manifests as recycled formats, parasocial
 *   audience relationships based on personality rather than story quality,
 *   and the commodification of vulnerability itself.
 *
 * KEY AGENTS:
 *   - Content Creators (powerless/trapped): Primary victims — face algorithmic demand and audience retention pressure requiring output beyond lived experience capacity. Career and income depend on maintaining publishing cadence. Cannot exit without losing platform foothold.
 *   - Platform Operators (institutional/arbitrage): Primary beneficiaries — capture advertising value from engagement and creator-generated content. Benefit from coordination but have exit options to other monetization models. Can adjust algorithmic reward structures.
 *   - Engaged Audiences (moderate/constrained): Secondary participants — benefit from narrative abundance and discovery mechanisms but pay cost in attention extraction, narrative quality degradation, and parasocial relationship depletion. Constrained by algorithm-driven recommendation dependencies.
 *   - Alternative Platform Ecosystem (organized/constrained): Potential exit infrastructure (Substack, Patreon, independent blogs) but immature relative to dominant platforms. Organized by creators seeking alternatives but constrained by lack of network effects and discovery mechanisms.
 *   - Cultural Authenticity Expectation (analytical/analytical): Diffuse institutional force — the expectation that creators maintain 'authentic constant production' naturalizes the burnout as inevitable. Analytical observer sees this as theatrical performance of authenticity rather than genuine narrative capacity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(narrative_capacity_exhaustion, 0.58).
domain_priors:suppression_score(narrative_capacity_exhaustion, 0.62).
domain_priors:theater_ratio(narrative_capacity_exhaustion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(narrative_capacity_exhaustion, extractiveness, 0.58).
narrative_ontology:constraint_metric(narrative_capacity_exhaustion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(narrative_capacity_exhaustion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(narrative_capacity_exhaustion, tangled_rope).
narrative_ontology:human_readable(narrative_capacity_exhaustion, "The Storytelling Burnout: Narrative Capacity Exhaustion in Digital Content Creation").
narrative_ontology:topic_domain(narrative_capacity_exhaustion, "social/technological").

domain_priors:requires_active_enforcement(narrative_capacity_exhaustion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(narrative_capacity_exhaustion, platform_operators).
narrative_ontology:constraint_beneficiary(narrative_capacity_exhaustion, audience_engagement_ecosystems).
narrative_ontology:constraint_victim(narrative_capacity_exhaustion, content_creators).
narrative_ontology:constraint_victim(narrative_capacity_exhaustion, narrative_authenticity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXHAUSTED CREATOR (SNARE) — Caught in algorithmic demand for constant output. Career, income, and audience retention depend on maintaining publishing cadence regardless of lived experience inventory. Cannot exit without losing platform foothold and audience. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96. Pure extraction: the constraint extracts narrative labor from lived experience depletion.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PLATFORM OPERATOR (ROPE) — Benefits from coordination function: enabling creator-audience connection generates engagement and advertising value. Experiences the constraint as a coordination mechanism solving the 'how do audiences find narratives?' problem. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.08. Net beneficiary. The platform benefits from the ecosystem and can exit to other monetization models (arbitrage).
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ENGAGED AUDIENCE (TANGLED ROPE) — Benefits from narrative abundance and discovery mechanisms (coordination), but pays a cost in narrative quality degradation, repetition, and parasocial relationship depletion. Constrained by switching costs and algorithm dependency. d≈0.58, f(d)≈0.65, σ=1.0 → χ≈0.38. Mixed experience: genuine coordination benefit undercut by extraction of attention and emotional labor.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (PITON) — The constraint persists as theatrical performance: 'constant authenticity' becomes a brand performance requirement, while the underlying function (delivering genuine human narrative) has atrophied. Content creators perform the role of authentic storytellers, but the narrative production is decoupled from actual lived experience. theater_ratio=0.68 satisfies piton gate. The constraint is maintained by institutional inertia (platform dependencies, audience expectations) despite degraded function.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(narrative_capacity_exhaustion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(narrative_capacity_exhaustion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(narrative_capacity_exhaustion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(narrative_capacity_exhaustion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(narrative_capacity_exhaustion, TR),
    TR >= 0.70.

:- end_tests(narrative_capacity_exhaustion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts narrative labor from creators by requiring output volume that exceeds sustainable lived experience. The trajectory from 0.30 (early platforms functioned primarily as coordination tools) to 0.58 (current state with algorithmic optimization and platform competition) shows systematic increase in extractiveness. The constraint is not at maximum extraction (snare threshold ≥0.66) because creators can still theoretically exit or adjust output, and some creators do maintain sustainable practices. Suppression (0.62): Moderate-high. Significant barriers to exit: platform-dependent income, audience switching costs, algorithmic gatekeeping, lack of mature alternative monetization, and cultural pressure ('you must post regularly to stay relevant'). Career consequences of publishing gaps are severe. However, suppression is not total — some creators maintain selective output, sabbaticals, or migrate to alternatives. Theater ratio (0.68): High. The constraint has shifted toward performative content: creators perform the role of authentic storytellers experiencing constant insight, growth, and emotional availability. The actual narrative production is increasingly decoupled from lived experience — recycled formats, repetitive themes, manufactured crises, parasocial relationship performance. This represents theater growth from 0.35 to 0.68 over six time points, indicating Piton degradation: the original coordination function (audience discovery) persists but increasingly through theatrical ritual rather than genuine narrative innovation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the collapse of alignment between beneficiary and victim. The platform operator genuinely experiences coordination (Rope): the platform solves a real problem (connecting creators and audiences). The creator genuinely experiences extraction (Snare approaching): narrative demand exceeds lived experience, forcing a choice between fabrication, parasitism, or exit. The audience experiences mixed effects (Tangled Rope): they benefit from narrative abundance and discovery but pay a cost in attention extraction and quality degradation. The analytical observer sees performative theater (Piton): the constraint is maintained by institutional inertia — creators perform authenticity, audiences perform engagement, platforms perform discovery — but the underlying authentic narrative function has degraded. The perspectival gap reveals that this constraint started as genuine coordination but has shifted toward extraction as platforms optimized for engagement rather than narrative quality. The beneficiary (platform) maintains the structure; the victims (creators, audiences, narrative authenticity) bear the cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Content creators: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction from the creator's perspective — they cannot exit without losing career and income. Platform operators: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiaries with exit options. Engaged audiences: Victims + constrained → d≈0.58, f(d)≈0.65. Constrained by algorithmic recommendation dependency and switching costs, but with more options than creators. Alternative platforms: Organized + constrained → d≈0.40, f(d)≈0.40. Constrained by network effects but organized by creator migration efforts. Cultural authenticity expectation: Analytical → d≈0.72, f(d)≈1.15. Observes the constraint as naturalized inevitability; the analytical perspective risks legitimizing a contingent institutional arrangement as law of narrative production.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_depletion_vs_invention,
    'At what point does narrative generation transition from drawing on lived experience to fabrication or parasitic reuse of others'' stories?',
    'Longitudinal analysis of creator disclosures, audience detection of fabrication, plagiarism detection, creator exit interviews revealing experience inventory gaps',
    'If threshold is low (early): many creators are already fabricating; constraint is snare from all perspectives. If threshold is high (late): constraint permits sustainable narrative creation; rope classification holds longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_depletion_vs_invention, empirical, 'Threshold where narrative generation shifts from experience to fabrication').

omega_variable(
    algorithmic_demand_elasticity,
    'Is the demand for narrative output driven by algorithms, audience expectations, creator insecurity, or a combination?',
    'Comparative analysis of creator behavior on platforms with different algorithmic rewarding (frequency-dependent vs quality-dependent); creator behavior changes when algorithms change; survey data on creator motivation and exit barriers',
    'If algorithm-driven: constraint is institutional extraction (snare). If creator-driven (insecurity): constraint is self-imposed (rope). If audience-driven: constraint is coordination response. Different sources imply different intervention points.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_demand_elasticity, empirical, 'Whether demand is algorithmic, audience, or creator-generated').

omega_variable(
    narrative_authenticity_collapse,
    'Does the requirement for constant output necessarily degrade narrative authenticity, or can creators develop sustainable authenticity practices?',
    'Audience perception studies (can audiences detect degraded authenticity?); creator retention rates on platforms with different output expectations; quality metrics for narrative depth and originality over time',
    'If necessarily degraded: the constraint forces a choice between career sustainability and authenticity (snare). If sustainable: creators can maintain both (rope). If variable: depends on creator skill and audience tolerance (tangled rope holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_authenticity_collapse, empirical, 'Whether constant output inherently degrades authenticity').

omega_variable(
    exit_infrastructure_maturity,
    'Do viable alternative platforms, funding models, or audience structures exist that permit sustainable narrative creation without constant output requirements?',
    'Enumeration of alternative platforms and models (Patreon, Substack, podcasts, books); success rate of creator migration; sustainability metrics for creators on alternative models vs algorithmic platforms',
    'If mature: exit option is real (mobile); constraint is tangled rope or rope with moderate χ. If immature: exit requires resource investment creators lack (trapped); constraint is snare. Current state suggests immature alternatives for most creators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_infrastructure_maturity, empirical, 'Maturity of alternative platforms and funding models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(narrative_capacity_exhaustion, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(narr_tr_t0, narrative_capacity_exhaustion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(narr_tr_t3, narrative_capacity_exhaustion, theater_ratio, 3, 0.52).
narrative_ontology:measurement(narr_tr_t6, narrative_capacity_exhaustion, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(narr_be_t0, narrative_capacity_exhaustion, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(narr_be_t3, narrative_capacity_exhaustion, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(narr_be_t6, narrative_capacity_exhaustion, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(narrative_capacity_exhaustion, information_standard).
narrative_ontology:affects_constraint(narrative_capacity_exhaustion, algorithmic_attention_extraction).
narrative_ontology:affects_constraint(narrative_capacity_exhaustion, parasocial_relationship_commodification).
narrative_ontology:affects_constraint(narrative_capacity_exhaustion, content_creator_labor_classification).

% DUAL FORMULATION NOTE:
% The storytelling burnout is downstream of algorithmic attention extraction and platform business models but represents a distinct structural constraint on narrative capacity. The upstream constraint (algorithmic attention extraction) drives platform demand; the storytelling burnout reflects the creator-side consequences of that demand on narrative authenticity and sustainability. Related constraints on labor classification and parasocial commodification share structural similarities (extraction of emotional/narrative labor) but have distinct ε values and institutional manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(narrative_capacity_exhaustion, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
