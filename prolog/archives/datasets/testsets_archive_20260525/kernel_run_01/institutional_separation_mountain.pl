% ============================================================================
% CONSTRAINT STORY: institutional_separation_mountain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_separation_mountain, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_separation_mountain
 *   human_readable: Institutional Separation Between Existential and Near-Term AI Risk Communities
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   The institutional separation between existential AI risk
 *   (catastrophic/extinction scenarios from advanced AI systems) and
 *   near-term AI harms (present discrimination, misinformation, labor
 *   displacement) appears at first as a natural consequence of different time
 *   horizons, verification methodologies, and research agendas. However,
 *   structural analysis reveals a constraint maintained partly by
 *   institutional incentives rather than pure epistemic necessity. The 83.1%
 *   in-group collaboration rate indicates strong network clustering; separate
 *   funding streams, academic departments, and policy engagement pathways
 *   reinforce the split. Recent advances in generative AI (scaling laws,
 *   capability emergence, rapid deployment cycles) are forcing
 *   epistemological convergence despite institutional barriers: researchers
 *   increasingly recognize that near-term deployment harms and long-term
 *   extinction risk trajectories are coupled through capability escalation,
 *   training data characteristics, and alignment mechanisms. The constraint
 *   exhibits all six DR types from different structural positions, making it
 *   a diagnostic case for how institutional path dependence can mask or
 *   naturalize contingent arrangements as immutable limits. The false summit
 *   detector triggers on the declared beneficiaries: both communities claim
 *   to benefit from separation, but the structural data reveals that the
 *   separation also extracts from agents experiencing compound harms
 *   (intersectional victims who are 'off-topic' for both communities) and
 *   from researchers attempting integrative work (who face identity-based
 *   barriers despite structural mobility).
 *
 * KEY AGENTS:
 *   - Existential Risk Community: Institutional beneficiary (institutional/arbitrage) — captures funding concentration, research autonomy, ability to set extinction-focused research agendas without pressure for near-term policy relevance
 *   - Near-Term AI Harms Community: Institutional beneficiary (institutional/arbitrage) — captures problem-set clarity, direct stakeholder engagement, policy-relevant research without requiring existential framing
 *   - Intersectional Harm Victims: Primary victims (powerless/trapped) — experience compound harms (e.g., algorithmic discrimination in high-unemployment communities; misinformation targeting vulnerable populations) and are 'off-topic' for both institutional communities
 *   - Integrative Researchers: Secondary victims (moderate/identity_locked) — structurally mobile but identity-fused with one subdiscipline; face career costs and institutional barriers to publishing cross-cutting work
 *   - Academic Institutional Structure: Maintains performative separation (institutional/arbitrage) — benefits from administrative convenience of separate departments, grant categories, and hiring lines despite coordination costs
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent properties of risk analysis across timescales
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_separation_mountain, 0.18).
domain_priors:suppression_score(institutional_separation_mountain, 0.42).
domain_priors:theater_ratio(institutional_separation_mountain, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_separation_mountain, extractiveness, 0.18).
narrative_ontology:constraint_metric(institutional_separation_mountain, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(institutional_separation_mountain, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(institutional_separation_mountain, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(institutional_separation_mountain, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_separation_mountain, tangled_rope).
narrative_ontology:human_readable(institutional_separation_mountain, "Institutional Separation Between Existential and Near-Term AI Risk Communities").
narrative_ontology:topic_domain(institutional_separation_mountain, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(institutional_separation_mountain).
domain_priors:emerges_naturally(institutional_separation_mountain).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_separation_mountain, existential_risk_community).
narrative_ontology:constraint_beneficiary(institutional_separation_mountain, near_term_harm_community).
narrative_ontology:constraint_victim(institutional_separation_mountain, holistic_ai_risk_understanding).
narrative_ontology:constraint_victim(institutional_separation_mountain, resource_allocation_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOLISTIC AI RISK UNDERSTANDING (MOUNTAIN) — The epistemic commons faces an immutable constraint: the mathematical, mechanistic, and empirical domains of existential and near-term AI risks are genuinely different in scope, timeline, and verification capacity. Long-horizon extinction scenarios operate on civilizational timescales with limited empirical access; near-term harms operate on biographical timescales with direct observability. This separation appears natural — a consequence of how risk operates across time horizons. However, the 83.1% in-group collaboration pattern and institutional segregation reveal this 'natural' divide is partly sustained by structural incentives rather than epistemic necessity alone.
constraint_indexing:constraint_classification(institutional_separation_mountain, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNDERREPRESENTED INTERSECTIONAL HARMS (SNARE) — Agents experiencing compound harms (e.g., algorithmic discrimination in hiring affecting already-marginalized groups; AI misinformation targeting vulnerable populations; labor displacement in communities with high baseline unemployment) cannot exit the system and experience extraction from both near-term and existential risk framings. Near-term researchers deprioritize intersectional complexity as 'scope creep'; existential risk researchers treat them as background context for long-term concerns. Trapped between two institutional perspectives that each find different excuses for non-engagement.
constraint_indexing:constraint_classification(institutional_separation_mountain, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: EXISTENTIAL RISK COMMUNITY (ROPE) — Benefits from institutional separation through funding concentration, research autonomy, and ability to set research agendas without constant pressure to address present harms. Experiences the constraint as functional coordination: clear boundaries enable deep technical work on extinction-level scenarios without distraction. The community sees separation as necessary — different timescales and methodologies warrant distinct institutions. Low direct extraction experienced; genuine coordination benefits.
constraint_indexing:constraint_classification(institutional_separation_mountain, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NEAR-TERM AI HARMS COMMUNITY (ROPE) — Benefits from institutional separation through problem-set clarity, stakeholder engagement pathways, and ability to conduct policy-relevant research without existential framing requiring decade-long timescales for relevance. Experiences the constraint as functional coordination: distinct institutions enable direct engagement with affected communities and policymakers. Low direct extraction experienced; genuine coordination benefits from separation.
constraint_indexing:constraint_classification(institutional_separation_mountain, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INDIVIDUAL RESEARCHERS ATTEMPTING INTEGRATION (TANGLED ROPE) — Researchers who recognize the need for integrated analysis (e.g., studying how near-term algorithmic harms interact with long-term AI trajectory considerations) face identity-based barriers: publishing in existential risk venues requires existential framing; publishing in near-term harm venues requires near-term policy relevance. Career advancement mechanisms reward specialization within one community. These agents have structural mobility (could move between communities) but identity fusion with their subdiscipline makes exit unthinkable — their professional identity and citation networks are constituted through one institutional home. Moderate extraction: some resources available for integration work, but career costs are real.
constraint_indexing:constraint_classification(institutional_separation_mountain, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: ACADEMIC INSTITUTIONAL STRUCTURE (PITON) — Universities and funding bodies maintain separate departments, grant categories, and hiring lines for existential risk and near-term harms. This institutional scaffolding persists through administrative inertia and path dependence. The theater ratio (0.65) reflects that much of the institutional separation is performative: regular conferences claim to bridge communities; grant programs claim cross-cutting relevance; working groups are formed and disbanded repeatedly. The underlying institutional arrangements remain unchanged because they are convenient administrative categories, not because they serve optimal epistemic coordination. The piton reflects degraded coordination function maintained by institutional inertia.
constraint_indexing:constraint_classification(institutional_separation_mountain, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER - NATURAL LAW FRAMING (MOUNTAIN) — From a civilizational analytical perspective, the separation appears immutable: different time horizons naturally yield different research methodologies, verification timescales, and policy relevance windows. Long-term extinction scenarios require speculative foresight and mathematical modeling; near-term harms require empirical documentation and immediate intervention. This structural difference appears as an irreducible feature of how risk analysis operates across scales. However, the declared beneficiaries and the 83.1% in-group collaboration rate trigger the false summit detector: the constraint is partly naturalized institutional path-dependence, not pure epistemic necessity. Generative AI advances (scaling laws, capability emergence, societal deployment speed) are forcing convergence despite the institutional barriers.
constraint_indexing:constraint_classification(institutional_separation_mountain, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_separation_mountain_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_separation_mountain, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_separation_mountain, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_separation_mountain, TR),
    TR >= 0.70.

:- end_tests(institutional_separation_mountain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low base value reflecting that both communities do experience genuine coordination benefits from separation and the extraction mechanism is not as severe as pure snare dynamics. However, the low value is partly an artifact of the false summit detection: the declared beneficiaries elevate the measurement away from what a victim-centered analysis would produce. The trend shows accumulation (0.08 → 0.18 over 20 years) as generative AI advances force convergence while institutional barriers remain, creating growing friction and uncompensated transition costs for integrative researchers. Suppression (0.42): Moderate. Barriers to exit include career costs (publication bias against cross-cutting work), identity fusion (professional identity constituted through one institutional home), and institutional path dependence (departments, funding categories, hiring criteria organized around the separation). These are not total barriers — researchers can and do integrate — but they are substantial. Theater ratio (0.65): Moderate-high. Much of the institutional separation apparatus is performative: regular 'bridge' conferences that produce no institutional change; working groups formed and dissolved repeatedly; grant programs claiming cross-cutting relevance while maintaining separate review panels and success criteria. The performative content has increased as pressure for integration grows but institutional structures resist change.
 *
 * PERSPECTIVAL GAP:
 *   Each institutional perspective experiences the separation differently based on structural position. Beneficiaries (both risk communities) experience coordination: clear boundaries enable autonomy. Victims (intersectional harms, integrative researchers) experience extraction: invisible to both communities and penalized for cross-cutting work. The gap is not one of disagreement but of structural asymmetry: agents with arbitrage options see functional coordination; agents with trapped options see suppression. The false summit trigger reveals that the mountain classification (natural law of risk analysis) naturalizes what is actually institutional path dependence benefiting agents with power to define research boundaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation derives d from beneficiary/victim declarations and exit options. Existential risk community: beneficiary + arbitrage → low d → low χ (rope). Near-term harm community: beneficiary + arbitrage → low d → low χ (rope). Intersectional harm victims: victim + trapped → high d → high χ (snare). Integrative researchers: victim + identity_locked → high d (0.89) but identity_locked exit reflects cognitive/identity-based entrapment despite structural mobility — these agents could theoretically move between communities but their professional identity is constituted through one institutional home, making exit unthinkable from within their frame. The identity_locked classification is critical here: at biographical time horizon, an identity-locked agent perceives the constraint as changeable in principle (rope-like perception) but is unable to exercise that change because their identity frame prevents it. The engine's classification will show rope at biographical from the identity-locked perspective's own perception, but the gap between this rope perception and the actual constraint structure (tangled rope for that agent's objective position) reveals the oracle gap: the agent cannot see from within their identity frame what cross-position analysis reveals. The false summit detection fires because the declared beneficiaries exist but the 83.1% in-group collaboration rate and the identifiable extraction from intersectional harm victims indicate the separation is not epistemically pure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating how institutional separation can function simultaneously as coordination mechanism (for beneficiary communities with exit options) and extraction mechanism (for victims without exit options), with the two functions maintained by the same structural arrangement. The mountain classification from the analytical observer perspective is a false summit: the constraint appears natural (different timescales inherently require different research methodologies) but the structural data reveals contingency (83.1% in-group collaboration is far higher than necessary cooperation would require; the beneficiaries are identifiable and benefit from exclusion; integration attempts face identity-based rather than epistemological barriers). The false summit does not make separation unjustified — the rope and tangled rope perspectives reveal genuine coordination benefits for some agents — but it reveals that the constraint is institutionally sustained, not naturally inevitable. Generative AI advances are testing this boundary: as the near-term/long-term coupling becomes empirically undeniable, the separation persists only through institutional inertia (piton degradation) and career cost enforcement. The mandatrophy is resolved: the constraint is tangled rope (genuine coordination + asymmetric extraction + active enforcement) with false summit dynamics (the mountain framing naturalizes what is actually path-dependent institutional arrangement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemological_necessity_vs_institutional_path_dependence,
    'Is the separation between existential and near-term AI risk research rooted in irreducible epistemological differences, or is it sustained by institutional incentives and historical path dependence?',
    'Historical analysis of how the institutional split emerged (did it follow from epistemic structure or from funding/careerism dynamics?); examination of research programs that successfully integrate both timescales and their institutional barriers; comparison of research outputs from separated vs integrated institutions',
    'If epistemologically necessary: separation is legitimate and convergence attempts will fail. If path-dependent: the constraint is a tangled rope or snare that coordination mechanisms could resolve; the false summit classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemological_necessity_vs_institutional_path_dependence, conceptual, 'Whether separation is epistemically natural or institutionally constructed').

omega_variable(
    generative_ai_forcing_function,
    'Are recent generative AI advances (scaling laws, capability emergence, deployment speed) forcing genuine epistemological convergence, or merely creating rhetorical pressure while institutional separation persists?',
    'Measurement of collaboration rates across institutional boundaries pre- and post-GPT-3 (2022) and post-GPT-4 (2023); tracking of cross-community citations and co-authored papers; analysis of whether new research programs (e.g., mechanistic interpretability, alignment research) actually integrate long-term and near-term considerations or merely claim to',
    'If genuine convergence: the constraint is degrading (scaffold with sunset) and will resolve naturally. If rhetorical only: institutional separation persists and the false summit framing remains necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generative_ai_forcing_function, empirical, 'Whether recent AI advances are forcing genuine institutional convergence').

omega_variable(
    intersectional_harm_invisibility_mechanism,
    'What portion of the suppression experienced by agents affected by compound harms (intersection of near-term and existential risk factors) stems from institutional separation itself, versus other sources?',
    'Documentation of specific policy and research failures where institutional separation prevented holistic analysis of intersectional harms; comparison of suppression levels in integrated vs separated institutional contexts; qualitative analysis of how marginalized communities experience being ''off-topic'' for both risk communities',
    'If institutional separation is a major mechanism: constraint is a snare targeting vulnerable agents, and has high-priority resolution threshold. If minor: suppression is driven by other factors and addressing institutional separation alone will not resolve the harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intersectional_harm_invisibility_mechanism, empirical, 'Portion of intersectional harm suppression attributable to institutional separation').

omega_variable(
    identity_lock_escape_feasibility,
    'Can researchers who are identity-locked into one risk tradition (existential or near-term) actually transition their professional identity to integrative work without career destruction, or is the identity lock structurally permanent within current incentive systems?',
    'Longitudinal tracking of career outcomes for researchers who publish across both institutional domains; analysis of citation patterns and hiring decisions for integrative researchers; interviews with researchers who have attempted or considered integration',
    'If identity lock is escapable: the constraint has lower suppression than measured (agents have real exits despite high costs). If permanent: suppression accurately reflects structural inability to exit, confirming identity_locked exit classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_escape_feasibility, empirical, 'Whether identity-locked researchers can transition to integrative work').

omega_variable(
    false_summit_beneficiary_artifact,
    'Are the declared beneficiaries (existential and near-term risk communities) genuinely benefiting from separation, or is the declaration an artifact of the false summit detection mechanism itself?',
    'Comparison of research outputs, funding levels, and institutional growth rates in separated vs integrated contexts; analysis of whether communities claiming to benefit from separation would actually lose resources if full integration occurred; examination of whether stated reasons for separation match empirical justifications',
    'If beneficiaries are genuine: separation serves real coordination functions and is partly justified. If artifact: the constraint is pure extraction masked by institutional theorizing, and false summit reclassification is fully warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_artifact, conceptual, 'Whether beneficiary status is genuine or false summit artifact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_separation_mountain, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(instsep_tr_t0, institutional_separation_mountain, theater_ratio, 0, 0.52).
narrative_ontology:measurement(instsep_tr_t10, institutional_separation_mountain, theater_ratio, 10, 0.59).
narrative_ontology:measurement(instsep_tr_t20, institutional_separation_mountain, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(instsep_be_t0, institutional_separation_mountain, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(instsep_be_t10, institutional_separation_mountain, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(instsep_be_t20, institutional_separation_mountain, base_extractiveness, 20, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_separation_mountain, information_standard).
narrative_ontology:affects_constraint(institutional_separation_mountain, ai_alignment_research_funding_allocation).
narrative_ontology:affects_constraint(institutional_separation_mountain, governance_capability_assessment_methodologies).
narrative_ontology:affects_constraint(institutional_separation_mountain, labor_displacement_policy_response_speed).

% DUAL FORMULATION NOTE:
% The institutional separation constraint is structurally upstream of specific AI risk domains (alignment, governance, labor) but represents a distinct institutional coordination problem. The downstream constraints each experience the institutional separation as either enabling (if they fit neatly into existential or near-term framings) or constraining (if they require integration). The separation also affects resource allocation: funding, researcher time, and policy attention divided between two institutional silos creates inefficiency and prevents compound-risk analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_separation_mountain, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
