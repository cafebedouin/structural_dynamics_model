% ============================================================================
% CONSTRAINT STORY: peter_principle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_peter_principle, []).

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
 *   constraint_id: peter_principle
 *   human_readable: The Peter Principle (Promotion to Incompetence)
 *   domain: organizational/social
 *
 * SUMMARY:
 *   The Peter Principle is a structural mechanism by which organizations
 *   systematically promote their most competent people into positions where
 *   they become incompetent, then trap them there through sunk costs,
 *   institutional inertia, and the career threat of demotion. This creates an
 *   asymmetric extraction: incompetent managers retain salary and status
 *   while competent subordinates bear the cost of compensating for their
 *   failures. The principle operates through a coordination mechanism (stable
 *   promotion hierarchies reduce recruitment churn and organize ambition)
 *   that simultaneously functions as an extraction mechanism (blocking the
 *   advancement of competent subordinates and protecting incompetent
 *   incumbents from displacement). The theater ratio has increased over the
 *   measurement interval as organizations have adopted more elaborate
 *   performance evaluation systems, 360-degree feedback, and merit mythology
 *   to justify promotions that are increasingly driven by tenure, office
 *   politics, and the Peter Principle itself. The constraint exhibits all six
 *   DR classifications from different structural perspectives, making it a
 *   diagnostic exemplar for how the same institutional mechanism can appear
 *   as natural law, coordination, extraction, or degraded ritual depending on
 *   the observer's position.
 *
 * KEY AGENTS:
 *   - Incompetent Incumbent: Primary beneficiary (institutional/arbitrage) — captures salary, status, and job security despite poor performance; benefits from protection afforded by institutional inertia
 *   - Competent Subordinates: Primary victims (powerless/trapped) — stalled careers, blocked advancement, forced to compensate for management failure; cannot exit without sacrificing career capital invested in current organization
 *   - Promotion Gatekeepers: Secondary beneficiary (institutional/arbitrage) — benefit from predictable advancement structures that reduce recruitment churn; perceive Peter Principle as minor coordination cost
 *   - Organization: Dual-position agent (institutional/constrained) — benefits from stable hierarchy but extracts costs through reduced effectiveness; actively enforces promotion systems that generate the principle
 *   - Competent Peers: Secondary victims (moderate/constrained) — face competition with those promoted beyond competence; career trajectory distorted by need to work around incompetent managers
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement as inevitable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(peter_principle, 0.52).
domain_priors:suppression_score(peter_principle, 0.58).
domain_priors:theater_ratio(peter_principle, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(peter_principle, extractiveness, 0.52).
narrative_ontology:constraint_metric(peter_principle, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(peter_principle, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(peter_principle, tangled_rope).
narrative_ontology:human_readable(peter_principle, "The Peter Principle (Promotion to Incompetence)").
narrative_ontology:topic_domain(peter_principle, "organizational/social").

domain_priors:requires_active_enforcement(peter_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(peter_principle, incompetent_incumbents).
narrative_ontology:constraint_beneficiary(peter_principle, promotion_gatekeepers).
narrative_ontology:constraint_beneficiary(peter_principle, organizational_inertia).
narrative_ontology:constraint_victim(peter_principle, competent_subordinates).
narrative_ontology:constraint_victim(peter_principle, organizational_effectiveness).
narrative_ontology:constraint_victim(peter_principle, merit_signal_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE COMPETENT SUBORDINATE (SNARE) — Trapped beneath an incompetent manager who cannot be displaced, cannot be worked around, and cannot be appealed to. The subordinate has performed excellently in their role but faces a ceiling: promotion requires approval from the incompetent incumbent, who has incentive to block rivals. Career advancement is blocked; lateral moves are constrained by the incumbent's control of performance metrics. Full extraction: the subordinate bears all costs (stalled career, wasted talent, cognitive load of compensating for incompetent leadership) while the incompetent manager captures all benefits (salary, status, authority) without performing the role's actual function.
constraint_indexing:constraint_classification(peter_principle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PROMOTION GATEKEEPER (ROPE) — HR systems and promotion committees benefit from a stable escalation ladder: predictable promotion cycles reduce recruitment churn, preserve institutional knowledge, and maintain status signaling. The gatekeeper sees promotion-to-incompetence as a minor coordination cost (occasional deadwood in mid-management) offset by the stability of rank-ordered advancement. Exit is high: promotion gatekeepers can easily define performance metrics, rotate underperformers, or restructure — but they perceive low extraction. The system 'coordinates' individual ambition with organizational hierarchy, even if the coordination creates perverse incentives.
constraint_indexing:constraint_classification(peter_principle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE INCOMPETENT INCUMBENT (TANGLED ROPE) — Benefits from the Peter Principle (permanent position at maximum incompetence ensures job security and continued compensation), but also constrained by it (incompetence is recognized, career advancement is blocked, authority is undermined by visibility of failures). The incumbent experiences a hybrid: they gain extraction (protected position despite poor performance) but lose autonomy (subordinates work around them, higher-ups monitor closely, peer respect collapses). Active enforcement is required — the incumbent must maintain a facade of legitimacy through theater (blame-shifting, credit-claiming, performative activity) to sustain the position. Without enforcement, subordinates would simply stop reporting to them.
constraint_indexing:constraint_classification(peter_principle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ORGANIZATION (TANGLED ROPE) — Paradoxically benefits from promotion-to-incompetence at the same time it extracts costs. Benefit: predictable hierarchy preserves institutional culture, reduces lateral conflict by formalizing advancement paths, maintains management stability. Cost: incompetent managers directly reduce organizational effectiveness, create hostile work environments that increase turnover of competent staff, and accumulate into organizational decay. The organization is both beneficiary (stable structure) and victim (degraded function). Active enforcement: the hierarchy actively enforces the promotion system (salary structures, promotion schedules, performance evaluations) that generates the Peter Principle. Without active enforcement, merit-based advancement would displace incompetent incumbents.
constraint_indexing:constraint_classification(peter_principle, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE MERIT MYTHOLOGY (PITON) — Organizations maintain an ideology that 'promotion is based on merit' and 'those in charge earned their positions through excellence.' This narrative is largely performative: real promotion decisions are driven by seniority, office politics, demographic factors, and the Peter Principle itself. The merit mythology persists through institutional inertia — it justifies hierarchies without requiring actual meritocracy. Theater is high (0.68): performance reviews, promotion committees, and advancement criteria all perform the theater of merit-based selection while the structural mechanism (rise to incompetence) systematically violates it. The original function (identify and promote the best) has atrophied; the myth is maintained because the alternative — acknowledging that hierarchies reward longevity and luck — would destabilize organizational legitimacy.
constraint_indexing:constraint_classification(peter_principle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scope, the Peter Principle appears as an inevitable consequence of promotion criteria: if advancement is based on performance in the current role (a rational heuristic), then by definition, advancement cannot predict performance in a different role. The principle emerges naturally from any system that uses backward-looking (current performance) criteria to predict forward-looking (future performance) outcomes. This perspective sees the constraint as a natural law of organizational mechanics. However, the structural data contradicts the mountain classification — active enforcement, suppression, beneficiary/victim asymmetries, and theater all indicate a contingent institutional arrangement, not a law of nature. The false summit reveals that 'inevitability' naturalizes what is actually a policy choice (whom to promote, on what basis, with what feedback mechanisms).
constraint_indexing:constraint_classification(peter_principle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peter_principle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(peter_principle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peter_principle, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(peter_principle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(peter_principle, TR),
    TR >= 0.70.

:- end_tests(peter_principle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Peter Principle extracts from competent subordinates (stalled careers, forced compensation work, reduced autonomy) and delivers to incompetent incumbents (protected positions, unearned compensation). The extraction is not total (0.80+) because: (1) some organizations successfully displace incompetent managers, (2) some talented people leave and find better environments, (3) incompetent managers face real costs (reputation damage, peer isolation, blocked further advancement). The 0.52 value reflects that the extraction is substantial but contested — exits exist and are used, though at high cost. Suppression (0.58): Moderate-high. Barriers to escaping incompetent management include: sunk costs (invested career capital, pension vesting, specialized skills non-portable), information barriers (difficulty assessing management quality from outside), switching costs (recruiting cycles, relocation), and institutional barriers (internal promotion rules, reference dependence on current manager). However, suppression is not total (0.75+) because: external job markets exist, some organizations have flatter hierarchies, some industries have high mobility. Theater ratio (0.68): High. The Peter Principle is sustained through elaborate theater: performance reviews that claim to measure merit but are driven by tenure and office politics, promotion committees that perform deliberation but follow seniority, merit mythology that justifies hierarchies while the structural mechanism violates merit at every step. This theater has increased from 0.45 to 0.68 over the measurement interval as organizations have adopted more complex evaluation systems — the theater has *increased* as the structural mechanism has become more entrenched.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism can be perceived as coordination (Rope), extraction (Snare), hybrid (Tangled Rope), or degraded ritual (Piton) depending on the observer's power, exit options, and structural relationship. The beneficiary (incompetent incumbent) sees their position as natural, earned, and stable — a Rope outcome from their perspective. The victim (competent subordinate) sees a Snare — they are trapped, their merit is blocked, extraction is asymmetric and irreversible. The gatekeeper sees Rope — coordination of ambition into hierarchy. The organization sees Tangled Rope — benefits from stable structure, costs from incompetent management. The merit mythology sees Piton — the performance evaluation theater persists though the actual meritocratic function has atrophied. The civilizational observer risks seeing Mountain — 'organizations have always had this problem' — but the structural data (active enforcement, suppression, asymmetric benefits/costs, theater) reveals this as a false summit: the Peter Principle is a contingent institutional choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and exit options. Incompetent incumbents are beneficiaries with high structural protection (institutional power, arbitrage exit options once promoted, blocked demotion pathways) — their d value is low (~0.10), producing negative or minimal χ, meaning they experience the constraint as beneficial or neutral. Competent subordinates are victims with low exit options (powerless relative to management authority, trapped by sunk costs, restricted to internal promotion) — their d value is high (~0.90), producing high f(d) and high χ, meaning they experience maximum extraction. Promotion gatekeepers are beneficiaries with arbitrage exit (institutional power, can restructure at will) — their d value is very low (~0.05), producing negative χ, meaning they perceive coordination rather than extraction. The organization is split: as a victim of reduced effectiveness it would have high d; as a beneficiary of stable structure it would have low d. The tangled_rope classification reflects this duality — the organization both enforces and is harmed by the Peter Principle.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by showing that the Peter Principle is NOT a natural law but a tangled rope: it has genuine coordination function (stable promotion hierarchies reduce recruitment churn, organize ambition, preserve institutional knowledge) AND asymmetric extraction (incompetent managers are protected while competent subordinates are blocked). The false summit (mountain classification from analytical view) is revealed by noting that the principle operates through active enforcement (promotion systems, career structures, compensation rules) and theater (merit mythology, performance reviews). If these were removed, incompetent managers would be displaced. The principle is therefore contingent on institutional choices, not inevitable. The resolution also reveals that the piton perspective is correct — the merit mythology that justifies promotions has become largely performative, maintained by institutional inertia rather than actual function. A genuine meritocratic system would: (1) assess forward-looking competence, not backward-looking performance, before promotion, (2) provide lateral career tracks with equivalent status/compensation to vertical advancement, (3) enable rapid demotion or sideways movement when incompetence is revealed, (4) decouple job security from rank. Organizations that implement these mechanisms can substantially reduce the Peter Principle without eliminating promotion hierarchies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feedback_loop_sufficiency,
    'Do performance evaluation systems provide sufficient feedback to identify incompetence before promotion, or is incompetence necessarily revealed only after promotion to a new role?',
    'Longitudinal analysis of employee performance metrics pre- and post-promotion; comparison of pre-promotion assessment accuracy across roles at different hierarchy levels; identification of which signal types (360 feedback, task-specific metrics, cross-functional assessments) predict role-specific competence',
    'If pre-promotion signals are sufficient: Peter Principle is a failure of selection mechanisms, not a natural law — competence can be identified and promotion criteria can be redesigned to include forward-looking skill assessment. If pre-promotion signals are fundamentally insufficient: Peter Principle is closer to a natural law, and the constraint is architectural rather than removable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_loop_sufficiency, empirical, 'Whether feedback systems can predict role-specific competence before promotion').

omega_variable(
    organizational_exit_cost_threshold,
    'At what organizational size or hierarchy depth does the cost of removing incompetent managers exceed the cost of tolerating them?',
    'Comparative analysis across organizations of different sizes; measurement of severance/litigation costs vs productivity loss from incompetent managers; identification of inflection points where organizational structure makes displacement infeasible',
    'If threshold is low (small organizations can easily displace): Peter Principle is contingent on size and structure, not inevitable. If threshold is high (even small organizations find displacement impossible): constraint approaches natural law status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organizational_exit_cost_threshold, empirical, 'Cost threshold for removing incompetent managers relative to organizational size').

omega_variable(
    lateral_mobility_sufficiency,
    'Can lateral career paths (sideways moves without promotion, specialized roles, project-based advancement) provide equivalent status and compensation to vertical promotion, reducing pressure for upward movement?',
    'Survey data on satisfaction and compensation equity between vertical and lateral career tracks; analysis of organizational cultures that successfully maintain dual-track systems; identification of why most organizations collapse lateral tracks back to vertical hierarchies',
    'If lateral mobility is sufficiently appealing: Peter Principle can be mitigated by removing the pressure to promote beyond competence. If vertical advancement is inherently more rewarding: organizations will continue pushing talented people upward until they fail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lateral_mobility_sufficiency, preference, 'Whether lateral career paths can replace vertical promotion as incentive mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(peter_principle, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(peter_tr_t0, peter_principle, theater_ratio, 0, 0.45).
narrative_ontology:measurement(peter_tr_t15, peter_principle, theater_ratio, 15, 0.62).
narrative_ontology:measurement(peter_tr_t30, peter_principle, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(peter_be_t0, peter_principle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(peter_be_t15, peter_principle, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(peter_be_t30, peter_principle, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(peter_principle, enforcement_mechanism).
narrative_ontology:affects_constraint(peter_principle, organizational_hierarchy_legitimacy).
narrative_ontology:affects_constraint(peter_principle, meritocracy_signaling_collapse).
narrative_ontology:affects_constraint(peter_principle, leadership_capability_cascade).

% DUAL FORMULATION NOTE:
% The Peter Principle decomposes into two structurally distinct constraints: (1) Selection Mechanism Bias (ε=0.15, Rope) — the use of backward-looking performance to predict forward-looking competence is a pure coordination problem with solution (use forward-looking assessments, multi-role evaluation, lateral paths). (2) Incumbent Protection Architecture (ε=0.58, Tangled Rope) — the institutional structure that makes displacement of incompetent managers prohibitively costly, once they have accumulated tenure and compensation. These are linked: the first creates incompetent managers, the second protects them. The Peter Principle story (ε=0.52) is a composite treating both mechanisms as a unified constraint. Alternative decomposition available in sibling stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(peter_principle, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
