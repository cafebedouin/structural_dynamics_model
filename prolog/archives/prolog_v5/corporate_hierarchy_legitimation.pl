% ============================================================================
% CONSTRAINT STORY: corporate_hierarchy_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_corporate_hierarchy_legitimation, []).

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
 *   constraint_id: corporate_hierarchy_legitimation
 *   human_readable: Corporate Hierarchy Legitimation Through Merit Narrative
 *   domain: organizational_behavior/political_economy
 *
 * SUMMARY:
 *   Corporate hierarchies maintain asymmetric power and wealth distribution
 *   through a legitimation narrative that frames position in the
 *   organizational structure as the result of merit—individual ability,
 *   effort, and performance. This constraint exhibits properties of both pure
 *   extraction (Snare) and hybrid coordination-extraction (Tangled Rope)
 *   depending on the observer's structural position. The same hierarchy
 *   appears as a natural necessity to executives, a coordination mechanism to
 *   middle management, a trap to entry-level workers, a degraded ritual to HR
 *   departments, and a false natural law to analytical observers. The
 *   constraint's theater ratio (0.68) reflects that formal performance
 *   evaluation mechanisms are substantially performative: promotion decisions
 *   are shaped by sponsorship, proximity to power, demographic homogeneity,
 *   and structural opportunity more than by measurable performance metrics.
 *   The merit narrative serves both coordination functions (aligning
 *   individual effort with organizational goals, legitimizing authority,
 *   enabling accountability) and extraction functions (justifying wage
 *   compression, enabling blame displacement, suppressing awareness of
 *   structural inequality). The theatrical component has increased over the
 *   interval as awareness of meritocratic failure has grown, requiring
 *   escalation of legitimation performance to maintain the same extraction
 *   level.
 *
 * KEY AGENTS:
 *   - Entry-Level Workers: Primary victims (powerless/trapped) — bear extraction through wage suppression, lack of advancement paths, and internalized belief in own merit deficiency
 *   - Senior Executives: Primary beneficiaries (institutional/arbitrage) — capture surplus value through hierarchical authority, merit narrative justifies their compensation premium
 *   - Middle Managers: Secondary agents (moderate/constrained) — experience genuine coordination (delegation, authority) alongside extraction (supervisory pressure, invisible labor); compressed between upper and lower tiers
 *   - Capital Holders: Structural beneficiaries (institutional/arbitrage) — benefit from suppressed labor costs and legitimated authority structure that prevents workers from recognizing coordination opportunities
 *   - Labor Coalitions: Organized victims (organized/constrained) — see through merit narrative, build counter-narrative, constrain extraction through collective action and visibility
 *   - HR Department: Institutional maintainers (institutional/arbitrage) — maintain the performative apparatus of merit evaluation; aware of gap between merit narrative and reality but locked into enforcement role
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating contingent institutional choice (hierarchical authority with merit legitimation) as immutable law of organizational nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(corporate_hierarchy_legitimation, 0.58).
domain_priors:suppression_score(corporate_hierarchy_legitimation, 0.65).
domain_priors:theater_ratio(corporate_hierarchy_legitimation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(corporate_hierarchy_legitimation, extractiveness, 0.58).
narrative_ontology:constraint_metric(corporate_hierarchy_legitimation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(corporate_hierarchy_legitimation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(corporate_hierarchy_legitimation, tangled_rope).
narrative_ontology:human_readable(corporate_hierarchy_legitimation, "Corporate Hierarchy Legitimation Through Merit Narrative").
narrative_ontology:topic_domain(corporate_hierarchy_legitimation, "organizational_behavior/political_economy").

domain_priors:requires_active_enforcement(corporate_hierarchy_legitimation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(corporate_hierarchy_legitimation, senior_executives).
narrative_ontology:constraint_beneficiary(corporate_hierarchy_legitimation, capital_holders).
narrative_ontology:constraint_victim(corporate_hierarchy_legitimation, lower_tier_workers).
narrative_ontology:constraint_victim(corporate_hierarchy_legitimation, organizational_epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTRY-LEVEL WORKER (SNARE) — Trapped by economic dependency and labor market segmentation. The merit narrative is the mechanism binding them to extraction: they internalize that their position reflects ability/effort, accept subordination as justified, and defer exit. Restructuring one's career to escape requires abandoning the identity narrative constructed within the organization. Maximum extraction with minimal perceived coercion because the binding is cognitive not just material.
constraint_indexing:constraint_classification(corporate_hierarchy_legitimation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE MANAGER (TANGLED ROPE) — Experiences genuine coordination: delegation of authority, resource allocation across departments, performance feedback. Also experiences asymmetric extraction: constrained by supervisory pressure from above, salary compression against inflation, invisible labor burden (emotional, administrative). Has some exit options (lateral moves, startup joining) but at significant career cost. Both coordination function and extraction coexist.
constraint_indexing:constraint_classification(corporate_hierarchy_legitimation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SENIOR EXECUTIVE (ROPE) — Experiences the hierarchy primarily as coordination: organizational structure enables delegation, accountability, resource mobilization. The merit narrative legitimizes their authority and aligns subordinate incentives with organizational goals. Extraction runs toward them, not away. They have arbitrage options (board seats, consulting, equity packages) and experience the constraint as functional necessity.
constraint_indexing:constraint_classification(corporate_hierarchy_legitimation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR COALITION (TANGLED ROPE) — Organized workers see both genuine coordination (wage standardization, benefits pooling, workplace safety protocols) and extraction (wage suppression below productivity, benefit constraints, power asymmetry in negotiation). Coalition building creates exit options and leverage; union membership is constrained by legal barriers and capital counterorganization. Coalition extracts value from the legitimation constraint by making extraction visible.
constraint_indexing:constraint_classification(corporate_hierarchy_legitimation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HR DEPARTMENT (PITON) — Maintains the performative apparatus of merit evaluation: performance reviews, promotion criteria, competency frameworks. The department knows these are substantially theatrical — promotion paths are shaped by sponsorship, in-group bias, and structural opportunity more than measurable performance. The ritual persists through institutional inertia (it signals meritocracy to employees and regulators) despite low functional verification. Theater ratio is high; extraction mechanism is degraded because the legitimacy narrative itself is wearing thin.
constraint_indexing:constraint_classification(corporate_hierarchy_legitimation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, hierarchy itself appears as an immutable organizational necessity: large-scale coordination requires authority structure, decision-making asymmetry, and role differentiation. The merit narrative appears as natural consequence of rational selection. However, the structural data contradicts the mountain classification — the specificity of corporate hierarchy legitimation through meritocratic narrative, the measurable theater in HR processes, and the contingency of alternative organizational forms (flat hierarchies, cooperative models, algorithmic delegation) all reveal this as naturalization of a contingent institutional choice, not a law of nature.
constraint_indexing:constraint_classification(corporate_hierarchy_legitimation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(corporate_hierarchy_legitimation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(corporate_hierarchy_legitimation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(corporate_hierarchy_legitimation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(corporate_hierarchy_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(corporate_hierarchy_legitimation, TR),
    TR >= 0.70.

:- end_tests(corporate_hierarchy_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The hierarchy extracts value through wage suppression, effort multiplication (workers exceed contractual obligations due to internalized merit belief), and opportunity hoarding (advancement restricted to those with network access). The extraction is significant but not maximal because genuine coordination functions exist—resource allocation, accountability assignment, and organizational coherence all require some hierarchical structure. The 0.58 value reflects the extraction overlaid on legitimate coordination costs. Suppression (0.65): Moderate-high. Workers face material barriers to exit (labor market segmentation, geographic immobility, skill-specificity, benefits coupling to tenure) and cognitive barriers (internalized merit narrative, identity fusion with organizational role, belief that hierarchy reflects natural ability differences). Suppression is high enough to prevent widespread exit even when extraction becomes visible. Theater ratio (0.68): Moderately high. Performance evaluation processes are largely performative—reviews assess subjective cultural fit and supervisor preference more than objective performance; promotion committees use opaque criteria; advancement paths follow sponsorship patterns more than stated merit criteria. The theater has increased as workers gain access to information about promotion statistics and demographic disparities, necessitating escalated legitimation performance to maintain the same suppression level.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon—hierarchical authority coupled with merit legitimation—produces contradictory classifications based on structural position. The entry-level worker's Snare is not the same as the executive's Rope viewed from a different angle; they are genuinely different structural experiences of the same constraint. The coal is that the worker's Snare is often misclassified as Rope by analyzing the constraint from the executive's perspective or the analytical observer's civilizational view. The labor coalition's organized Tangled Rope perspective makes the extraction visible—it exposes that the middle manager's mixed experience is actually the middle manager being squeezed from both directions (pressure from above, pressure from workers below, surplus value extracted in both directions). The perspectival gap is the diagnostic tool for detecting false natural law claims: if a constraint classifies as Mountain from the analytical view but as Snare from the powerless view, the mountain classification is likely a false summit naturalizing contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Entry-level workers and lower-tier workers are structural victims: they bear suppression costs through economic dependency, labor market barriers, and cognitive capture. Their directionality (d) is high—they experience extraction (high f(d), high χ). Executives and capital holders are structural beneficiaries: they extract value through hierarchical authority and advantageous positioning. Their directionality (d) is low—extraction flows toward them (low/negative f(d)). Middle managers and labor coalitions are in mixed positions: they face suppression (medium-high d) but have some exit options and agency (constrained or organized power levels), producing moderate experienced extraction. The directionality differences are not a function of their individual traits but of their structural position in the extraction flow—same worker's d value would differ significantly if they had arbitrage options (startup equity, consultant status, skill portability) versus trapped dependency (sole income source, specialized credentials, geographic constraints). The analysis assumes beneficiary/victim declarations reflect genuine structural flows, not subjective perception.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by clarifying that corporate hierarchy legitimation is genuinely a Tangled Rope at the institutional level (it coordinates real functions: authority delegation, resource allocation, accountability assignment) while simultaneously being a Snare at the victim level (workers are trapped and extraction is maximal from their perspective). The mandatrophy—'Is this coordination or extraction?'—is answered correctly by saying BOTH, depending on structural position. The executives legitimately solve coordination problems and experience Rope (low/negative χ). The workers experience pure extraction because they have no exit and no coordination benefit. The middle managers experience Tangled Rope because they both coordinate laterally (managing teams, resources) and extract/are-extracted-from (supervisory pressure from above, authority over below). The false summit (mountain) from the analytical perspective is revealed by the structural data: the legitimation narrative is performative (theater_ratio 0.68), the merit criteria are opaque and subjective, and alternative organizational forms (flat hierarchies, cooperative models) exist empirically. The constraint is NOT immutable—it is maintained through active enforcement of the legitimation narrative and periodic escalation of theater to counter growing awareness. If workers gain collective consciousness of the extraction mechanism, the constraint's classification shifts toward Snare (making explicit what was implicit) and becomes politically contestable in a way that a mountain constraint would not be.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merit_definition_ambiguity,
    'What constitutes ''merit'' in hierarchical advancement—individual performance, cultural fit, structural opportunity, or some unmeasurable combination?',
    'Longitudinal tracking of promotion criteria against actual organizational outcomes; comparison of promotion rates across demographic groups controlling for performance metrics; analysis of criteria used in promotion committees',
    'If merit is measurable and objective: hierarchy is largely coordination-driven (Rope from more perspectives). If merit is subjective and contextual: legitimation narrative is primary extraction mechanism (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(merit_definition_ambiguity, empirical, 'Definition and measurability of merit in promotion decisions').

omega_variable(
    identity_lock_persistence,
    'After workers leave the organization, does the internalized hierarchy persist as identity lock (they carry the subordination narrative with them) or does it dissolve as situational constraint?',
    'Post-employment surveys and interviews tracking self-efficacy, ambition, and career confidence; comparison of career trajectories of employees who left early vs advanced within hierarchy; analysis of whether former employees carry organizational framing into new contexts',
    'If identity lock persists: suppression mechanism is partially internalized and self-sustaining (exit does not fully remove extraction). If it dissolves: suppression is primarily structural and dissipates with organizational exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Persistence of internalized hierarchy after organizational exit').

omega_variable(
    coordination_function_necessity,
    'What proportion of hierarchy''s coordination function could be achieved through alternative structures (flat delegation, rotating leadership, algorithmic role assignment)?',
    'Comparative organizational analysis of firms with non-hierarchical coordination models; simulation of alternative authority structures; historical case studies of organizations that successfully flattened hierarchies',
    'If high proportion replaceable: extracted value from legitimate coordination is high, classification shifts toward Snare. If low proportion replaceable: hierarchy is primarily coordination-driven, classification shifts toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, conceptual, 'Necessity of hierarchical structure for organizational coordination').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression primarily structural (economic dependency, labor market barriers) or is it internalized through acceptance of merit narrative?',
    'Measurement of suppression before and after workers develop critical consciousness of hierarchy legitimacy; comparison of suppression levels in organizations with high-transparency vs opaque merit criteria; analysis of union organizing effectiveness as consciousness-raising intervention',
    'If primarily structural: suppression persists regardless of consciousness and removal requires external intervention (policy, collective action). If partially internalized: consciousness-raising and transparency can reduce suppression independent of structural barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs internalized components of suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(corporate_hierarchy_legitimation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corp_hier_tr_t0, corporate_hierarchy_legitimation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(corp_hier_tr_t10, corporate_hierarchy_legitimation, theater_ratio, 10, 0.6).
narrative_ontology:measurement(corp_hier_tr_t20, corporate_hierarchy_legitimation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(corp_hier_be_t0, corporate_hierarchy_legitimation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(corp_hier_be_t10, corporate_hierarchy_legitimation, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(corp_hier_be_t20, corporate_hierarchy_legitimation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(corporate_hierarchy_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(corporate_hierarchy_legitimation, wage_compression_equilibrium).
narrative_ontology:affects_constraint(corporate_hierarchy_legitimation, organizational_mobility_bottleneck).
narrative_ontology:affects_constraint(corporate_hierarchy_legitimation, merit_theater_expansion).

% DUAL FORMULATION NOTE:
% Corporate hierarchy legitimation is upstream of multiple organizational extraction mechanisms: wage compression (suppressed labor costs enabled by hierarchy), mobility bottlenecks (advancement gates protected by merit narrative), and theater expansion (performance evaluation increasingly performative as legitimation narrative loses credibility). Each downstream constraint has its own extractiveness value reflecting domain-specific mechanisms; the hierarchy legitimation story captures the primary extraction mechanism binding them together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(corporate_hierarchy_legitimation, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
