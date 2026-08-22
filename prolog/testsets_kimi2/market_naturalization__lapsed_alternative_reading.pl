% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__lapsed_alternative_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Closure (No-Active-Maintenance Reading)
 *   domain: political economy / economic history / institutional analysis
 *
 * SUMMARY:
 *   This constraint is the lapsed_alternative_reading of the contested kernel
 *   market_naturalization. It models market dominance not as actively
 *   defended by incumbent capital, but as a self-sustaining lapsed closure: a
 *   historical coordination solution that persists by inertia because
 *   alternatives atrophied through non-use. There is no identifiable
 *   beneficiary class extracting rents; the only remaining 'extraction' is
 *   the diffuse coordination cost of path dependence. The constraint is
 *   structurally a piton â an atrophied arrangement kept in place by
 *   institutional inertia and the prohibitive cost of collective
 *   re-coordination.
 *
 * KEY AGENTS:
 *   - dominant_platform_operators (agenda_setter, institutional/constrained) â inherit dominance but are locked in by installed base, do not actively maintain
 *   - market_participants (payer, organized/constrained) â bear diffuse coordination costs of the dominant standard
 *   - failed_alternative_providers (excluded, moderate/constrained) â locked out by network effects, not active suppression
 *   - institutional_economists (observer, analytical) â document path dependence and lock-in
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.16).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.12).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (No-Active-Maintenance Reading)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political economy / economic history / institutional analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '6aaa2591-71b5-40ca-8050-ea1b73e3632c').
narrative_ontology:cs_kernel_codification('6aaa2591-71b5-40ca-8050-ea1b73e3632c', implicit).
narrative_ontology:cs_authority_grounding('6aaa2591-71b5-40ca-8050-ea1b73e3632c', diffuse_epistemic).
narrative_ontology:cs_reading_relation('6aaa2591-71b5-40ca-8050-ea1b73e3632c', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('6aaa2591-71b5-40ca-8050-ea1b73e3632c', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('6aaa2591-71b5-40ca-8050-ea1b73e3632c', foundational, dominance_requires_no_active_beneficiary).
narrative_ontology:cs_axiom_status(dominance_requires_no_active_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('6aaa2591-71b5-40ca-8050-ea1b73e3632c', dominance_requires_no_active_beneficiary, empirically_contingent).
narrative_ontology:cs_axiom('6aaa2591-71b5-40ca-8050-ea1b73e3632c', foundational, alternatives_atrophy_by_non_use).
narrative_ontology:cs_axiom_status(alternatives_atrophy_by_non_use, holdable).
narrative_ontology:cs_axiom_grounding('6aaa2591-71b5-40ca-8050-ea1b73e3632c', alternatives_atrophy_by_non_use, empirically_contingent).
narrative_ontology:cs_reference_frame('6aaa2591-71b5-40ca-8050-ea1b73e3632c', lapsed_closure_inertia).
narrative_ontology:cs_drift_state('6aaa2591-71b5-40ca-8050-ea1b73e3632c', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('6aaa2591-71b5-40ca-8050-ea1b73e3632c', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, market_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the dominant market position inherited from historical competition. Could theoretically deviate from the standard or open interfaces, but face massive coordination costs and customer lock-in that make unilateral change prohibitively expensive. Do not actively suppress alternatives; alternatives have simply failed to gain adoption.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, dominant_platform_operators, agenda_setter,
    institutional, generational, constrained, global).

% Use the dominant standard because it is the default. Bear the costs of suboptimal compatibility, reduced variety, and path-dependent inefficiency. Exit is possible only through collective coordination onto an alternative, which has repeatedly failed to materialize.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, market_participants, payer,
    organized, biographical, constrained, global).

% Developed alternative standards or platforms that were technically viable but could not overcome network effects. Their exclusion is not enforced by any entity but is the result of coordination failure and user inertia.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, failed_alternative_providers, excluded,
    moderate, biographical, constrained, regional).

% Study path dependence and lock-in as features of market evolution. Document cases where superior alternatives lost to inferior but established standards.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, institutional_economists, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(market_naturalization__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a de facto standard or dominant practice that solves coordination problems among market participants by providing a single focal point, even if suboptimal.
% TRANSFER_FUNCTION: Moves coordination costs and path-dependence burdens from market participants to the inherited structure itself; no active transfer to a concentrated beneficiary class.
% ABSENT_VOICES: Entrepreneurs with alternative standards or business models that failed due to network-effect barriers rather than active exclusion; they would argue the dominance is maintained by inertia, not merit.
% DISAPPEARANCE_RATIONALE: If the lapsed closure vanished, market participants would need to re-coordinate on a new standard or plurality of standards; the sudden removal of path dependence would force active choice and temporary fragmentation.
% FOUNDING_PROBLEM: Original coordination problem that required a single dominant standard or platform to solve trust, compatibility, and scale economies.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and institutional analysts attest that the original coordination problem was solved long ago; the current arrangement persists past its functional necessity. Corroboration comes from academic institutional economics outside any benefiting incumbent interest, though some industry narratives still invoke the founding problem.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.16, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).
:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.16) because the reading models only coordination costs, not rent extraction. Suppression is minimal (0.12) because alternatives atrophied through non-use rather than active exclusion. Theater ratio is moderate (0.42) because performative narratives of market efficiency and naturalness substitute for active maintenance. Accessibility collapse is high (0.78) because the atrophy of alternatives has been thorough; resistance is negligible (0.08) because no party is sufficiently harmed to mount active opposition.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (dominant platforms) experiences the constraint as an inherited coordination standard they are locked into as much as they administer; the payer seat (market participants) experiences it as suboptimal but unavoidable default. The observer seat sees the entire structure as path-dependent inertia. There is no concentrated beneficiary seat to provide an opposing framing.
 *
 * DIRECTIONALITY LOGIC:
 *   With no declared beneficiaries and only diffuse coordination costs borne by market participants, directionality is weakly toward the payer end for participants and near-symmetric for the agenda_setter. The absence of a beneficiary array means no agent is structurally subsidized by the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (establishing a compatible standard) is dead, yet the arrangement persists. No party benefits enough to maintain it, and no party is hurt enough to bear the cost of fixing it. This prevents misclassification as a snare (which requires a beneficiary) or a rope (which requires active coordination benefits that exceed costs). The classification as piton captures the inertial persistence and prohibitive cost of collective exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_naturalization_reading_location,
    'This constraint is the lapsed_alternative_reading of kernel market_naturalization. Would a sibling reading (beneficiary_maintained_reading) identify incumbent firms as active beneficiaries and suppressors of alternatives?',
    'Historical case-study analysis tracing incumbent action versus inaction during alternative entry attempts.',
    'If incumbents actively suppressed alternatives, the reading collapses toward beneficiary_maintained_reading or hybrid_reading, raising extractiveness and suppression scores.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_naturalization_reading_location, conceptual, 'Whether this reading or the sibling better captures the empirical mechanism of dominance persistence.').

omega_variable(
    atrophy_vs_suppression_mechanism,
    'Did alternatives atrophy through genuine non-use and network effects, or through covert suppression that leaves no documentary trace?',
    'Counterfactual reconstruction of alternative entry paths and incumbent responses; examination of failed alternatives for evidence of passive versus active exclusion.',
    'If suppression is covert but real, suppression rises, the constraint acquires a beneficiary class, and classification shifts toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_vs_suppression_mechanism, empirical, 'Structural versus internalized suppression in the atrophy of market alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(market_nat_lapsed_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(market_nat_lapsed_tr_t10, market_naturalization__lapsed_alternative_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(market_nat_lapsed_tr_t20, market_naturalization__lapsed_alternative_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(market_nat_lapsed_tr_t30, market_naturalization__lapsed_alternative_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(market_nat_lapsed_tr_t40, market_naturalization__lapsed_alternative_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(market_nat_lapsed_tr_t50, market_naturalization__lapsed_alternative_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(market_nat_lapsed_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(market_nat_lapsed_be_t10, market_naturalization__lapsed_alternative_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(market_nat_lapsed_be_t20, market_naturalization__lapsed_alternative_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(market_nat_lapsed_be_t30, market_naturalization__lapsed_alternative_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(market_nat_lapsed_be_t40, market_naturalization__lapsed_alternative_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(market_nat_lapsed_be_t50, market_naturalization__lapsed_alternative_reading, base_extractiveness, 50, 0.16).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(market_naturalization__lapsed_alternative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, information_standard).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The market_naturalization kernel decomposes into three readings: beneficiary_maintained_reading (active extraction), lapsed_alternative_reading (inertial persistence), and hybrid_reading (mixed). Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
