% ============================================================================
% CONSTRAINT STORY: treaty_land_entrenchment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_land_entrenchment, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: treaty_land_entrenchment
 *   human_readable: Treaty Sovereignty (The Crown-Indigenous Mountain)
 *   domain: legal/political
 *
 * SUMMARY:
 *   Treaty entrenchment under the Canadian Constitution (Section 35,
 *   Constitution Act 1982) represents a structural constraint that operates
 *   as an immutable legal ceiling on state power. Treaties 6, 7, and 8, which
 *   govern Alberta territory, are constitutionally protected agreements
 *   between the Crown and Indigenous nations. Once entrenched in
 *   constitutional text, these agreements create binding legal obligations
 *   that no Crown actor — federal, provincial, or territorial — can
 *   unilaterally override without destroying the constitutional foundation
 *   itself. This constraint exhibits the defining characteristics of a
 *   mountain: it emerges from the logical structure of constitutional law (if
 *   a provision is entrenched, it cannot be changed by ordinary legislation),
 *   it has zero degrees of freedom for all agents, and its existence is
 *   verified through jurisprudence spanning decades (Sparrow 1990, Gladstone
 *   1996, Delgamuukw 1997, Haida Nation 2004, Tsilqot'in 2014). The
 *   constraint's extractiveness is low (0.18) because it is not a mechanism
 *   for extracting value from one group to benefit another — it is a
 *   distributional constraint allocating rights. Suppression is minimal
 *   (0.04) because the constraint operates through transparent constitutional
 *   law, not through coercion or denial of alternatives. Theater is
 *   negligible (0.08) because the constraint's function is entirely
 *   non-performative: constitutional entrenchment works through legal facts,
 *   not through ritual or pretense.
 *
 * KEY AGENTS:
 *   - Indigenous Peoples (Treaty 6, 7, 8 Nations): Primary beneficiary (powerless/trapped → protected by entrenchment floor) — hold constitutional rights that cannot be unilaterally extinguished
 *   - Federal Crown: Institutional actor (institutional/arbitrage → bound by constitutional ceiling) — cannot override treaties without amending Constitution Act 1982
 *   - Alberta Provincial Government: Powerful actor (powerful/mobile → constrained by constitutional hierarchy) — cannot unilaterally extinguish or override treaty rights within provincial jurisdiction
 *   - Non-Indigenous Settlers and Corporations: Powerful actors (powerful/mobile → limited by allocation of land/resource rights to Indigenous peoples) — cannot freely access treaty lands without consultation or consent
 *   - Canadian Courts: Institutional enforcer (institutional/arbitrage) — interpret and enforce Section 35 protections; serve as the mechanism through which the constraint is made binding
 *   - Constitutional Analyst: Analytical observer (analytical/analytical) — sees entrenchment as a structural feature of constitutional law itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_land_entrenchment, 0.18).
domain_priors:suppression_score(treaty_land_entrenchment, 0.04).
domain_priors:theater_ratio(treaty_land_entrenchment, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_land_entrenchment, extractiveness, 0.18).
narrative_ontology:constraint_metric(treaty_land_entrenchment, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(treaty_land_entrenchment, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_land_entrenchment, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(treaty_land_entrenchment, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_land_entrenchment, mountain).
narrative_ontology:human_readable(treaty_land_entrenchment, "Treaty Sovereignty (The Crown-Indigenous Mountain)").
narrative_ontology:topic_domain(treaty_land_entrenchment, "legal/political").

domain_priors:emerges_naturally(treaty_land_entrenchment).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL ANALYST (MOUNTAIN) — From a universal, civilizational view, treaty entrenchment is a structural feature of constitutional law itself. Once a treaty is constitutionally entrenched (as Treaties 6, 7, and 8 are in Canadian law via the Constitution Act 1982, Section 35), the constraint on subsequent legislation becomes an immutable legal fact. No Crown actor at any power level can unilaterally override it without destroying the constitutional foundation itself. ε≈0.18, accessibility_collapse=0.92, resistance=0.08 establish the mountain classification.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: FEDERAL CROWN (MOUNTAIN) — Even the institutional sovereign (the Crown) experiences treaty entrenchment as an immutable constraint. The Crown cannot exit or arbitrage away the constitutional obligation. The constraint is embedded in the constitutional text itself (Section 35). Exit would require constitutional amendment (requiring provincial/territorial consent), which is nearly impossible given the political lock-in. From the Crown's perspective, treaty land rights are a non-negotiable constitutional ceiling.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PROVINCIAL GOVERNMENT (MOUNTAIN) — Even powerful provincial actors (Alberta government) experience treaty entrenchment as a binding constraint. While Alberta has some regulatory authority within provinces, it cannot unilaterally extinguish or override treaty rights. Any attempt to do so would violate Section 35 of the Constitution Act 1982, which is beyond provincial jurisdiction to amend. The constraint is enforced through federal courts and the constitutional hierarchy.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: INDIGENOUS PEOPLES (MOUNTAIN) — From the perspective of Indigenous peoples holding treaty rights, entrenchment is a mountain — a structural protection that cannot be removed by ordinary legislation. The rights themselves are anchored in constitutional law, creating an immutable legal fact. However, the constraint operates differently here: it is experienced as a protective floor, not an extraction. The mountain prevents degradation, not limitation. d≈0.05 (beneficiary of the entrenchment), f(d)≈-0.12, χ≈-0.02. Negative effective extraction indicates net protection rather than imposition.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: JURISPRUDENTIAL OBSERVER (MOUNTAIN) — Legal doctrine on treaty rights (established in cases like Sparrow, Gladstone, Delgamuukw, Haida Nation) establishes that Aboriginal and treaty rights are inherent, not granted by the Crown. Once recognized and entrenched constitutionally, they create binding legal facts that no actor can unilaterally dissolve. The extraction coefficient reflects the constraint's role as a structural feature of the legal system itself, not as a coercive mechanism.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: NON-INDIGENOUS ACTORS (MOUNTAIN) — From the perspective of settler society and resource extraction corporations operating on treaty lands, the entrenchment appears as a structural constraint on their freedom to exploit land and resources without consultation or consent. They cannot exit this constraint through ordinary market mechanisms or provincial regulation. However, this is not extraction in the DR sense — it is a constraint that limits their actions, not a mechanism that extracts value from them to benefit a specific actor. The constraint is distributional (it allocates rights, not wealth), not extractive.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_land_entrenchment_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(treaty_land_entrenchment, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(treaty_land_entrenchment, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(treaty_land_entrenchment, ExtMetricName, E),
    domain_priors:suppression_score(treaty_land_entrenchment, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(treaty_land_entrenchment),
    narrative_ontology:constraint_metric(treaty_land_entrenchment, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(treaty_land_entrenchment, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(treaty_land_entrenchment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint is not extractive in the DR sense because it does not transfer value from one actor to another — it allocates rights. The 0.18 value reflects the minimal structural imposition on the Crown: it cannot act on treaty lands unilaterally, which limits its freedom but does not extract resources. If the constraint were analyzed as pure extraction, the score would be inflated; the mountain classification corrects for this by treating the constraint as a distributional floor, not a wealth-transfer mechanism. Suppression (0.04): Minimal. The constraint operates through transparent constitutional law. No agent is denied knowledge of the constraint or alternatives. The only suppression is the legal fact itself: alternatives (unilateral Crown action) are not available, but this is not coercive suppression — it is structural constraint. Theater ratio (0.08): Near-zero. Constitutional entrenchment has no performative component. It either binds through legal fact or it does not. The slight non-zero value reflects marginal ritual in court proceedings and constitutional interpretation, but the core mechanism (amendment lock + judicial enforcement) is purely functional.
 *
 * PERSPECTIVAL GAP:
 *   All six perspectives converge on the mountain classification, which indicates a uniform-type constraint. This is theoretically sound: constitutional entrenchment creates a legal fact that binds all agents, regardless of their power level or exit options. The Crown, provinces, Indigenous peoples, and settler society all experience the same structural constraint — treaty rights cannot be unilaterally dissolved by ordinary legislation. The gap that does exist is not perspectival (different classification types) but experiential: Indigenous peoples experience the constraint as a protective floor; the Crown experiences it as a ceiling on its power; settlers and corporations experience it as a limitation on their access to land/resources. However, all parties classify it the same way structurally: it is a mountain — an immutable legal fact. The uniformity across perspectives is evidence of the constraint's robustness.
 *
 * DIRECTIONALITY LOGIC:
 *   The traditional directionality derivation (beneficiary/victim + exit options) does not apply to pure mountains in the same way it applies to other constraint types. Mountains have ε and f(d) = immutable; no agent can negotiate or arbitrage the constraint away. However, we can still identify who benefits and who bears costs: Indigenous peoples benefit from the entrenchment (they are protected); the Crown and settler society bear the cost of constrained freedom. In directionality terms: Indigenous peoples as beneficiaries → d≈0.05, f(d)≈-0.12 (they are protected, not extracted from). The Crown as institutional actor → d≈0.00 (canonical fallback for institutional power in a constraint that limits institutional freedom). Settler society as victims of allocation → d≈0.85 (they are constrained from accessing treaty lands), but this is not extraction — it is rights allocation. The key insight: a mountain can protect one group and constrain another without being an extraction mechanism. The constraint redistributes rights, not wealth.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN (NO MANDATROPHY): Treaty entrenchment resolves any mandatrophy by its uniform classification across all perspectives. The constraint is not vulnerable to the mandatrophy error (mislabeling coordination as extraction, or vice versa) because it operates through constitutional law, not through coordination mechanisms or extraction mechanisms. The constraint's function is pure: it creates a legal ceiling that no actor can cross. There is no hidden coordination function that could be mislabeled as extraction, and no extraction mechanism that could be mislabeled as coordination. The accessibility_collapse metric (0.92) indicates that the constraint is nearly perfectly accessible — everyone can verify that treaties are entrenched in the Constitution Act 1982, and no one disputes the fact. The resistance metric (0.08) indicates that the constraint is nearly universally accepted as legitimate, even by those constrained by it. This high accessibility and low resistance are characteristic of mountains: they command assent not through coercion but through the transparency of their logical structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    title_vs_rights_distinction,
    'Does treaty entrenchment establish absolute Aboriginal title or use-and-harvesting rights, and does this distinction affect the constraint''s structural nature?',
    'Analysis of Supreme Court jurisprudence (Delgamuukw, Tsilqot''in) distinguishing title, harvesting rights, and consultation rights; examination of specific treaty language and its judicial interpretation over time',
    'If entrenchment secures absolute title: constraint is mountain (immutable ownership fact). If entrenchment secures consultation rights only: constraint might degrade to tangled_rope (coordination + extraction) as Crown actors seek ways to limit consultation scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(title_vs_rights_distinction, empirical, 'Whether entrenchment establishes title or limited use rights').

omega_variable(
    amendment_lock_robustness,
    'Can Section 35 be effectively amended through creative constitutional interpretation or political-legal work-arounds without formal amendment?',
    'Longitudinal analysis of court decisions on Section 35; examination of Crown attempts to narrow or reinterpret treaty obligations; assessment of whether political majorities could pressure courts to dilute protections',
    'If amendment lock is robust: mountain classification holds indefinitely. If courts or political actors find interpretive pathways to narrow Section 35: constraint degrades to tangled_rope or snare as entrenchment becomes theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_lock_robustness, empirical, 'Whether Section 35 entrenchment is truly amendment-proof').

omega_variable(
    implementation_gap,
    'Does the distinction between legal entrenchment and practical implementation create a hidden extraction mechanism where Indigenous peoples hold formal rights but cannot enforce them without enormous legal cost?',
    'Analysis of litigation patterns, settlement rates, and cost-shifting in Section 35 cases; comparison of litigation costs borne by Indigenous claimants vs. the Crown; examination of whether de facto implementation lags behind de jure entrenchment',
    'If implementation gap is large: mountain classification applies only to the formal legal structure; actual constraint might be tangled_rope (formal rights + informal extraction via litigation costs). If gap is small: mountain classification holds for both law and practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_gap, empirical, 'Whether legal entrenchment translates to practical enforceability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_land_entrenchment, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(treaty_tr_t0, treaty_land_entrenchment, theater_ratio, 0, 0.05).
narrative_ontology:measurement(treaty_tr_t50, treaty_land_entrenchment, theater_ratio, 50, 0.08).
narrative_ontology:measurement(treaty_tr_t100, treaty_land_entrenchment, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(treaty_be_t0, treaty_land_entrenchment, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(treaty_be_t50, treaty_land_entrenchment, base_extractiveness, 50, 0.17).
narrative_ontology:measurement(treaty_be_t100, treaty_land_entrenchment, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
