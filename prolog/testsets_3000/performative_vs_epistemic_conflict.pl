% ============================================================================
% CONSTRAINT STORY: performative_vs_epistemic_conflict
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performative_vs_epistemic_conflict, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performative_vs_epistemic_conflict
 *   human_readable: Performative vs Epistemic Conflict in Discourse
 *   domain: epistemology/social_psychology/philosophy_of_discourse
 *
 * SUMMARY:
 *   The distinction between performative and epistemic conflict is a
 *   coordination mechanism in discourse. Performative conflict aims at
 *   prevailing — winning the argument, maintaining status, defending
 *   position. Epistemic conflict aims at joint investigation — mutual
 *   revision toward truth, collaborative error-correction, shared
 *   understanding. The constraint is the recognition that these are
 *   structurally different activities requiring different norms, and that
 *   conflating them produces coordination failure. Observable markers
 *   include: score-keeping behaviors (tracking concessions, claiming
 *   victories), deflection patterns (changing subject when challenged),
 *   reframing moves (restating opponent's position to make it weaker), and
 *   mutual revision rate (how often participants update their views based on
 *   the exchange). High mutual revision indicates epistemic mode; high
 *   score-keeping indicates performative mode. The constraint solves a real
 *   coordination problem: participants need to know which game they're
 *   playing to engage productively. Mismatched expectations — one participant
 *   in epistemic mode, another in performative mode — produce frustration and
 *   wasted effort. The distinction enables explicit norm-setting and context
 *   selection.
 *
 * KEY AGENTS:
 *   - Epistemic Participant: Moderate power, mobile exit — seeks joint investigation; benefits from recognizing when discourse is productive vs performative
 *   - Performative Participant: Moderate power, mobile exit — seeks to prevail; benefits from recognizing when performative engagement is appropriate vs when epistemic norms apply
 *   - Epistemic Community: Organized, mobile exit — benefits from explicit discourse norms; can allocate resources appropriately (peer review vs public debate)
 *   - Academic Institution: Institutional power, arbitrage exit — benefits from distinguishing research discourse (epistemic) from public engagement (performative)
 *   - Analytical Observer: Sees the distinction as a low-extraction coordination mechanism solving a genuine problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performative_vs_epistemic_conflict, 0.18).
domain_priors:suppression_score(performative_vs_epistemic_conflict, 0.22).
domain_priors:theater_ratio(performative_vs_epistemic_conflict, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performative_vs_epistemic_conflict, extractiveness, 0.18).
narrative_ontology:constraint_metric(performative_vs_epistemic_conflict, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(performative_vs_epistemic_conflict, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performative_vs_epistemic_conflict, rope).
narrative_ontology:human_readable(performative_vs_epistemic_conflict, "Performative vs Epistemic Conflict in Discourse").
narrative_ontology:topic_domain(performative_vs_epistemic_conflict, "epistemology/social_psychology/philosophy_of_discourse").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performative_vs_epistemic_conflict, epistemic_community).
narrative_ontology:constraint_beneficiary(performative_vs_epistemic_conflict, discourse_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC PARTICIPANT (ROPE) — Participant oriented toward joint investigation experiences the distinction as a coordination mechanism. Recognizing performative vs epistemic modes enables selection of productive discourse contexts. Low extraction — the constraint helps rather than hinders truth-seeking.
constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: PERFORMATIVE PARTICIPANT (ROPE) — Participant oriented toward prevailing also benefits from the distinction. Knowing when discourse is epistemic vs performative enables appropriate strategy selection. The constraint coordinates expectations about discourse norms without imposing costs.
constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: EPISTEMIC COMMUNITY (ROPE) — Communities of practice benefit from explicit recognition of discourse modes. The distinction enables norm-setting: journals enforce epistemic standards, debates allow performative engagement. Coordination function with minimal overhead.
constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: ACADEMIC INSTITUTION (ROPE) — Institutions benefit from the distinction by allocating resources appropriately: peer review for epistemic discourse, public engagement for performative discourse. The constraint solves a genuine coordination problem with low enforcement cost.
constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From the analytical perspective, the performative/epistemic distinction is a low-extraction coordination mechanism. It names a real structural difference in discourse goals and enables participants to select appropriate contexts. No significant extraction detected — the constraint functions as claimed.
constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performative_vs_epistemic_conflict_tests).
:- end_tests(performative_vs_epistemic_conflict_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint imposes minimal cost. Recognizing the distinction helps participants select appropriate contexts and set expectations. Some extraction exists — participants must learn the distinction and may face social cost for violating norms (calling out performative behavior in epistemic contexts, or vice versa). But the extraction is substantially lower than the coordination benefit. Suppression (0.22): Low. Participants can exit discourse contexts that don't match their goals. The constraint doesn't trap anyone — it names a choice. Some suppression exists in institutional contexts where norms are enforced (peer review rejecting performative arguments, public debates penalizing epistemic hedging), but these are coordination mechanisms rather than coercive barriers. Theater ratio (0.35): Low-moderate. Some performative overlay exists — participants may claim epistemic motives while engaging performatively, or use epistemic language as a status signal. But the distinction itself is functional, not theatrical. The theater comes from misapplication, not from the constraint's structure.
 *
 * PERSPECTIVAL GAP:
 *   No significant perspectival gap exists for this constraint. All perspectives classify as Rope because the structural relationship is uniform: the distinction solves a coordination problem for all participants regardless of their discourse goals. Epistemic participants benefit by finding productive contexts. Performative participants benefit by recognizing appropriate venues. Communities and institutions benefit by setting norms. The analytical observer confirms the rope classification — no hidden extraction detected. This uniformity is diagnostic: the constraint is a genuine coordination mechanism with minimal extractive overhead. The lack of perspectival gap is itself informative — it indicates that the constraint's claimed function matches its structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives show beneficiary relationships with mobile or arbitrage exit options. Epistemic participants benefit by recognizing when discourse is productive. Performative participants benefit by recognizing when performative engagement is appropriate. Communities benefit by setting explicit norms. Institutions benefit by allocating resources appropriately. No victim groups identified — the constraint doesn't extract from anyone. The low base extractiveness reflects that the coordination benefit substantially exceeds the learning and norm-enforcement costs. Directionality values are derived from beneficiary status across all power levels, producing low d values and correspondingly low or negative chi values. The constraint functions as a pure coordination mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates rope classification without mandatrophy risk. The distinction between performative and epistemic conflict is not a cover story for extraction — it names a real structural difference in discourse goals and enables productive coordination. The low extractiveness (0.18) and low suppression (0.22) reflect genuine coordination function. Observable markers (mutual revision rate, score-keeping behaviors) provide empirical verification that the distinction tracks real phenomena rather than imposing arbitrary categories. The constraint could degrade into extraction if institutions used the distinction to gatekeep (declaring dissent 'performative' to dismiss it) or if epistemic norms were weaponized (demanding infinite revision as a deflection tactic). But these failure modes would show up as increased extractiveness and suppression in measurements, not as inherent features of the distinction itself. The current metrics indicate the constraint is functioning as a coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performative_vs_epistemic_conflict, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performative_vs_epistemic_conflict, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of consensus_as_cognitive_cost (mountain). The upstream constraint establishes that consensus-building has inherent cognitive cost. This constraint (performative_vs_epistemic_conflict) is a coordination response: by distinguishing discourse modes, participants can allocate cognitive resources appropriately — investing in mutual revision when epistemic goals justify the cost, and using performative shortcuts when consensus is not the goal. The rope classification depends on the mountain being genuine — if consensus were costless, the distinction would be unnecessary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
