% ============================================================================
% CONSTRAINT STORY: indexical_relativity_of_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indexical_relativity_of_extraction, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indexical_relativity_of_extraction
 *   human_readable: Indexical Relativity of Extraction in Constraint Classification
 *   domain: moral_psychology/systems_of_obligation/meta_theoretical
 *
 * SUMMARY:
 *   The indexical relativity of extraction is a meta-theoretical constraint:
 *   it is a property of the Deferential Realism classification system itself
 *   rather than a property of the social, economic, or physical constraints
 *   the system analyzes. The constraint states that identical constraint
 *   mechanisms (same epsilon, suppression, coordination function) produce
 *   categorically different extractiveness values and classification types
 *   based solely on the observer's structural position (power level, exit
 *   options, time horizon, scope). This is not a bug or limitation of the
 *   framework — it is a direct consequence of the chi formula chi = epsilon ×
 *   f(d) × sigma(S), where d is derived from the agent's relationship to the
 *   constraint (beneficiary vs victim, exit capacity). The framework's
 *   central claim is that extraction is not an objective property of a
 *   mechanism but an experienced property that depends on structural
 *   position. Indexical relativity is the formalization of this claim. The
 *   constraint has very low extractiveness (0.08) because it imposes minimal
 *   cost on agents using the framework — it is a logical feature of the
 *   formalism, not an institutional barrier. Suppression is near-zero (0.03)
 *   because no enforcement mechanism prevents agents from recognizing or
 *   discussing indexical relativity. Theater ratio is very low (0.05) because
 *   the constraint is functional rather than performative — it does the work
 *   it claims to do (model position-dependent extraction) with minimal
 *   overhead. The constraint qualifies as a mountain because it emerges
 *   naturally from the mathematical structure of the chi formula, has very
 *   high accessibility collapse (0.92) — once you understand the formula, the
 *   relativity is immediately apparent — and very low resistance (0.08) — no
 *   agent can change the fact that different d values produce different chi
 *   values.
 *
 * KEY AGENTS:
 *   - Analytical Observer: Meta-theoretical position (analytical/analytical) — sees indexical relativity as a logical consequence of the formalism
 *   - Framework Designer: Institutional position (institutional/arbitrage) — sees indexical relativity as an unavoidable design feature of any power-aware classification system
 *   - Methodological Critic: Powerful position (powerful/mobile) — might object to relativity but cannot change the mathematical structure
 *   - Practitioner: Moderate position (moderate/constrained) — experiences indexical relativity as an operational requirement when analyzing real constraints
 *   - Coalition Builder: Organized position (organized/mobile) — uses indexical relativity strategically to identify asymmetric extraction and build coalitions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indexical_relativity_of_extraction, 0.08).
domain_priors:suppression_score(indexical_relativity_of_extraction, 0.03).
domain_priors:theater_ratio(indexical_relativity_of_extraction, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indexical_relativity_of_extraction, extractiveness, 0.08).
narrative_ontology:constraint_metric(indexical_relativity_of_extraction, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(indexical_relativity_of_extraction, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(indexical_relativity_of_extraction, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(indexical_relativity_of_extraction, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indexical_relativity_of_extraction, mountain).
narrative_ontology:human_readable(indexical_relativity_of_extraction, "Indexical Relativity of Extraction in Constraint Classification").
narrative_ontology:topic_domain(indexical_relativity_of_extraction, "moral_psychology/systems_of_obligation/meta_theoretical").

domain_priors:emerges_naturally(indexical_relativity_of_extraction).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — The indexical relativity of extraction is a structural property of the classification system itself. Given fixed base extractiveness epsilon and the chi formula chi = epsilon × f(d) × sigma(S), different values of d (derived from power/exit/beneficiary-victim position) mathematically guarantee different experienced extraction values. This is not a contingent institutional arrangement but a logical consequence of the formalism. The same constraint mechanism with epsilon = 0.40 will classify as Rope from a beneficiary with arbitrage exit (d ≈ 0.05, f(d) ≈ -0.12, chi < 0), as Tangled Rope from a moderate agent with constrained exit (d ≈ 0.65, f(d) ≈ 1.00, chi ≈ 0.40), and as Snare from a powerless agent with trapped exit (d ≈ 0.95, f(d) ≈ 1.42, chi ≈ 0.57). The divergence is not observer error or measurement artifact — it is the system correctly detecting that structural position determines experienced extraction.
constraint_indexing:constraint_classification(indexical_relativity_of_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: FRAMEWORK DESIGNER (MOUNTAIN) — From the institutional perspective of those who design classification systems, indexical relativity is an unavoidable feature of any formalism that attempts to model power-dependent extraction. The alternative — a single 'objective' classification independent of observer position — would require either (a) ignoring power differentials entirely, or (b) privileging one structural position as the 'true' viewpoint. Both alternatives collapse under scrutiny: (a) makes the system blind to asymmetric extraction, (b) reintroduces the very power dynamics the system is meant to detect. The indexical approach is not a choice but a logical necessity once you commit to modeling extraction as experienced rather than as an abstract property.
constraint_indexing:constraint_classification(indexical_relativity_of_extraction, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: METHODOLOGICAL CRITIC (MOUNTAIN) — Even from a biographical/national perspective with mobile exit options, the indexical relativity principle is unchangeable. A critic might object that 'the same constraint should have the same classification,' but this objection misunderstands what 'the same constraint' means in the DR framework. The constraint is not just the mechanism (epsilon, suppression, coordination function) — it is the mechanism AS EXPERIENCED by an agent in a specific structural position. The chi formula encodes this: chi is not epsilon; chi is epsilon modulated by the agent's relationship to the constraint. Two agents experiencing different chi values from the same epsilon are not disagreeing about the constraint — they are correctly reporting different structural realities.
constraint_indexing:constraint_classification(indexical_relativity_of_extraction, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PRACTITIONER (MOUNTAIN) — From the immediate/local perspective of someone applying the framework to real constraints, indexical relativity is experienced as an operational requirement rather than a theoretical abstraction. When analyzing a constraint like payday lending, the practitioner cannot assign a single classification — the lender experiences Rope (coordination of liquidity provision with low effective extraction), the borrower experiences Snare (high extraction with trapped exit), and the regulatory analyst experiences Tangled Rope (mixed coordination and extraction with constrained exit). The practitioner does not choose to use multiple perspectives; the structural data forces it. Attempting to collapse these into a single classification would require discarding information about power differentials, which would make the analysis incomplete.
constraint_indexing:constraint_classification(indexical_relativity_of_extraction, mountain,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: COALITION BUILDER (MOUNTAIN) — From the organized/generational perspective of agents building coalitions to change constraints, indexical relativity is a strategic reality. Coalition formation depends on recognizing that different agents experience the same constraint differently — the coalition's task is to shift the constraint's structure so that more agents experience lower extraction. But this task presupposes that extraction IS indexical: if all agents experienced the same chi regardless of position, there would be no asymmetry to address. The coalition builder cannot escape indexical relativity by organizing; organization changes the agents' power level and exit options, which changes their d value, which changes their experienced chi. The relativity persists at every power level.
constraint_indexing:constraint_classification(indexical_relativity_of_extraction, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indexical_relativity_of_extraction_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(indexical_relativity_of_extraction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indexical_relativity_of_extraction, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(indexical_relativity_of_extraction, ExtMetricName, E),
    domain_priors:suppression_score(indexical_relativity_of_extraction, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(indexical_relativity_of_extraction),
    narrative_ontology:constraint_metric(indexical_relativity_of_extraction, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(indexical_relativity_of_extraction, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(indexical_relativity_of_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes minimal cost on framework users. Recognizing that extraction is indexical does not prevent analysis; it enables more accurate analysis by forcing explicit declaration of observer position. The small non-zero value reflects the cognitive cost of tracking multiple perspectives rather than collapsing to a single 'objective' view. Suppression (0.03): Near-zero. No institutional mechanism prevents recognition of indexical relativity. The framework's documentation explicitly teaches it. The small non-zero value reflects only the learning curve — new users may initially expect a single classification per constraint. Theater ratio (0.05): Very low. The constraint is highly functional. The chi formula actually computes different values for different d inputs; the indexical relativity is not a narrative overlay but a mathematical property. The small non-zero value reflects only the documentation overhead (explaining why the same epsilon produces different classifications). Accessibility collapse (0.92): Very high. Once an agent understands the chi formula and the derivation chain (beneficiary/victim → d → f(d) → chi), the indexical relativity is immediately apparent. No hidden complexity or tacit knowledge required. Resistance (0.08): Very low. No agent can change the fact that f(d) is a monotonic function mapping d ∈ [0,1] to f(d) ∈ [-0.12, 1.42]. Different d values mathematically guarantee different chi values. The small non-zero value reflects only that an agent could choose to use a different classification system that does not model position-dependent extraction.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in classification type — all five perspectives classify as mountain. This is expected for a true natural law constraint: the indexical relativity of extraction is invariant across all observer positions. However, there is a perspectival gap in SALIENCE. The analytical observer sees indexical relativity as the central organizing principle of the framework. The institutional designer sees it as an unavoidable design constraint. The methodological critic sees it as a potential objection to address. The practitioner sees it as an operational requirement. The coalition builder sees it as a strategic tool. These are not disagreements about whether indexical relativity exists (all agree it does) but differences in how the constraint matters to each agent's goals. The framework correctly models this: same classification type (mountain), different experienced relevance.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims in the traditional sense because it is a meta-theoretical property rather than a social mechanism. All agents using the framework are subject to indexical relativity equally — it is not imposed on some agents by others. The directionality values for each perspective are derived from the canonical fallback (power atom → canonical d) because there is no structural asymmetry to model. The analytical observer gets d ≈ 0.73 (canonical for analytical), the institutional designer gets d ≈ 0.00 (canonical for institutional), the powerful critic gets d ≈ 0.48 (canonical for powerful), the moderate practitioner gets d ≈ 0.65 (canonical for moderate), and the organized coalition builder gets d ≈ 0.40 (canonical for organized). All perspectives classify as mountain because the constraint's base properties (epsilon = 0.08, suppression = 0.03, emerges_naturally = true, accessibility_collapse = 0.92, resistance = 0.08) meet the mountain thresholds regardless of the observer's chi value.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy resolution at the meta-level. The mandatrophy asks: 'How do we distinguish genuine coordination (Rope) from extraction disguised as coordination (Snare)?' The answer is indexical classification: the same mechanism can be BOTH, depending on observer position. A payday loan is Rope from the lender's perspective (coordination of liquidity with low experienced extraction) and Snare from the borrower's perspective (high extraction with trapped exit). The mandatrophy is resolved not by choosing one classification but by recognizing that both are structurally accurate descriptions of different experiences of the same mechanism. Indexical relativity is the formal principle that makes this resolution possible. Without indexical relativity, the framework would be forced to choose a single 'true' classification, which would either privilege the beneficiary's view (missing the extraction) or privilege the victim's view (missing the coordination function). The indexical approach preserves both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indexical_relativity_of_extraction, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indexical_relativity_of_extraction, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is foundational to the DR framework itself. It does not decompose into multiple stories because it has a single, stable epsilon value (0.08) across all measurement approaches. The constraint is the mathematical property that OTHER constraints decompose into families when their epsilon values are observer-dependent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
