% ============================================================================
% CONSTRAINT STORY: scylla_charybdis_navigation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scylla_charybdis_navigation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: scylla_charybdis_navigation
 *   human_readable: Scylla-Charybdis Navigation Dilemma
 *   domain: strategic_choice/institutional_constraints
 *
 * SUMMARY:
 *   The Scylla-Charybdis constraint is a canonical forced-choice structure:
 *   an agent faces two paths, both carrying severe costs, with no legitimate
 *   third option available. The constraint is distinguished from mere
 *   coordination problems by its asymmetry — one path benefits the constraint
 *   architect (or a privileged class), while the other benefits a different
 *   class, and both are worse than the agent's preferred outcome.
 *   Historically instantiated in maritime chokepoints (actual Strait of
 *   Messina navigation), military blockades, regulatory regimes with no safe
 *   compliance path, and strategic business competition. The constraint
 *   exhibits all six DR types depending on the observer's structural
 *   relationship to the binary choice and their time horizon. The extraction
 *   derives from the architect's ability to maintain the binary structure and
 *   prevent tertiary options. As alternative routes, technology, or
 *   organizational power develops, the constraint degrades to Piton
 *   (performative maintenance) or Scaffold (temporary before supersession).
 *
 * KEY AGENTS:
 *   - Navigating Agent: Primary victim (powerless/trapped) — forced to choose between severe costs; experiences maximum extraction
 *   - Constraint Architect: Primary beneficiary (institutional/arbitrage) — benefits from coerced binary structure; maintains architecture through suppression
 *   - Coalition of Navigators: Secondary agent (moderate/constrained) — can reduce aggregate extraction through coordination but face isolation barriers
 *   - Organized Circumvention Networks: Tertiary agent (organized/mobile) — develop workarounds that eventually obsolete the constraint
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the constraint as immutable law rather than architectural choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scylla_charybdis_navigation, 0.68).
domain_priors:suppression_score(scylla_charybdis_navigation, 0.72).
domain_priors:theater_ratio(scylla_charybdis_navigation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scylla_charybdis_navigation, extractiveness, 0.68).
narrative_ontology:constraint_metric(scylla_charybdis_navigation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(scylla_charybdis_navigation, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scylla_charybdis_navigation, snare).
narrative_ontology:human_readable(scylla_charybdis_navigation, "Scylla-Charybdis Navigation Dilemma").
narrative_ontology:topic_domain(scylla_charybdis_navigation, "strategic_choice/institutional_constraints").

domain_priors:requires_active_enforcement(scylla_charybdis_navigation).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scylla_charybdis_navigation, constraint_architect).
narrative_ontology:constraint_victim(scylla_charybdis_navigation, navigating_agent).
narrative_ontology:constraint_victim(scylla_charybdis_navigation, collective_outcome).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED NAVIGATOR (SNARE) — No exit option exists. Both paths carry severe costs; inaction is also catastrophic. Maximum extraction without coordination benefit. The agent must accept loss; the choice is only which loss.
constraint_indexing:constraint_classification(scylla_charybdis_navigation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: BIOGRAPHICAL VICTIM (SNARE) — Over a lifetime, the navigator who encounters Scylla-Charybdis constraints repeatedly bears cumulative extraction. No learning or adaptation changes the structural constraint — only which cost manifests. Structural trap persists across time.
constraint_indexing:constraint_classification(scylla_charybdis_navigation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: CONSTRAINED COALITION (TANGLED ROPE) — Multiple navigators coordinating can sometimes reduce aggregate extraction through load-sharing (different routes for different agents) or negotiated passage. But coordination requires overcoming the initial isolation and bears its own costs. Asymmetric — some members bear larger losses to enable collective benefit.
constraint_indexing:constraint_classification(scylla_charybdis_navigation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ORGANIZED CIRCUMVENTION (SCAFFOLD) — Across generations, organized agents (pilot associations, military logistics networks, trade guilds) develop workarounds: alternative routes, seasonal windows, technological mitigations (stronger ships, warning systems). These are temporary scaffolds — they eventually fade as the constraint becomes irrelevant (new technology obsoletes the constraint entirely). Sunset logic applies: as alternatives mature, the original constraint loses coercive power.
constraint_indexing:constraint_classification(scylla_charybdis_navigation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL NATURALIZER (FALSE SUMMIT) — Viewed from maximum universality and timescale, the Scylla-Charybdis constraint appears to be an immutable law: 'all non-trivial choices involve unavoidable trade-offs' or 'the universe enforces conservation of badness.' This is naturalization of a contingent institutional/environmental feature. The constraint IS mutable — better information, alternative routes, or structural redesign reduces or eliminates it. The mountain classification is a false summit.
constraint_indexing:constraint_classification(scylla_charybdis_navigation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: CONSTRAINT ARCHITECT (ROPE) — The agent who designs or maintains the constraint structure (fortress builders, regulatory architects, strategic competitors) experiences the constraint as pure coordination: they are solving the problem of forcing a choice. No extraction is experienced — the constraint is their tool. They see the navigator's loss as legitimate coercion, not extraction.
constraint_indexing:constraint_classification(scylla_charybdis_navigation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scylla_charybdis_navigation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scylla_charybdis_navigation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scylla_charybdis_navigation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scylla_charybdis_navigation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(scylla_charybdis_navigation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The forced binary choice extracts significant value from navigating agents. Over the interval (0 to 9 time units), extractiveness rose from 0.48 to 0.68, reflecting increasing sophistication of the constraint architecture — the architect learned to maximize extraction by tightening the choice and suppressing information about alternatives. Theater ratio (0.45): Moderate-low. The constraint maintains relatively low performative content — the threat is credible, not theatrical. However, the theater rises over time as the architect increasingly must justify why the binary is necessary and inevitable, indicating Goodhart drift toward appearance-management. Suppression (0.72): High. Strong barriers prevent exit: information asymmetry about true costs, social/legal prohibition of refusal, resource monopolization by architect, and cultural framing that naturalizes the binary as inevitable. The suppression is structural, not easily reduced by individual agent effort.
 *
 * PERSPECTIVAL GAP:
 *   The architect sees Rope or even pure coordination (solving their own control problem). The trapped navigator sees pure Snare — no escape, only which loss to accept. The coalition sees mixed extraction-and-coordination (Tangled Rope) — collective action can reduce aggregate extraction but requires overcoming isolation. The organized circumvention network sees a sunset Scaffold — technological and organizational workarounds are building alternatives. The civilizational analyst risks false naturalization: viewing trade-off as immutable law rather than contingent architecture. The perspectival gap reveals that the constraint's classification depends entirely on structural position and time horizon — there is no single 'true' type, only different readings from different standpoints. The Snare classification at the powerless/trapped perspective is the engine's primary diagnosis; other types represent either misframing (Rope from architect) or partial mitigation (Scaffold, Tangled Rope from agents developing alternatives).
 *
 * DIRECTIONALITY LOGIC:
 *   The architect (institutional/arbitrage) derives d ≈ 0.05 — full beneficiary with exit options. The engine computes f(d) ≈ -0.12, producing negative effective extraction (the constraint serves this agent). The navigator (powerless/trapped) derives d ≈ 0.95 — full target with no exit. The engine computes f(d) ≈ 1.42, producing maximum experienced extraction. The coalition (moderate/constrained) derives d ≈ 0.75 — victim with high exit cost but some collective agency. The engine computes f(d) ≈ 1.15, producing moderate experienced extraction. These directionality values emerge automatically from beneficiary/victim declarations and exit options; no overrides are needed. The scope modifier σ(local=0.8) dampens the effective extraction for local constraints, while σ(global=1.2) would amplify it for planetary-scale forced choices.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint avoids misclassification into pure coordination by maintaining structural asymmetry. The architect is not solving a problem that the navigator also wants solved — the architect is solving their own control problem at the navigator's expense. The binary choice persists despite navigator preferences because the architect enforces suppression. The architecture is mutable (alternative routes, technological innovation, organizational coalitions can break the binary), but the constraint maintains coercive force in the present through information control and suppression. The Snare classification is stable: extractiveness (0.68) > 0.46, suppression (0.72) > 0.60, χ > 0.66 (computed from d and f(d) for trapped navigator). The false summit (mountain perspective) is diagnosed as false because the constraint is architecturally contingent: civilizational view that naturalizes trade-offs as immutable law misses that the binary structure itself is enforced, not inherent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binary_necessity_ambiguity,
    'Is the binary choice truly exhaustive, or does the constraint architect obscure tertiary options to maintain coercive structure?',
    'Historical case analysis: do navigators who discover hidden third options (technical innovations, political negotiations, alternative routes) succeed in escaping both costs? Comparison of constraint persistence before and after third-option availability.',
    'If third options are genuinely unavailable: constraint may be closer to Mountain (immutable trade-off). If architect actively suppresses third options: constraint is pure Snare (extractive architecture). Classification hinges on whether the binarity is structural or enforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binary_necessity_ambiguity, empirical, 'Whether the binary choice is truly unavoidable or architect-enforced').

omega_variable(
    cost_asymmetry_source,
    'Are the costs to Scylla and Charybdis paths equal, or does the architect bias the trade-off to extract from specific agent types?',
    'Measurement of cost distributions across multiple navigators; identification of whether apparent equivalence is statistical or enforced symmetry vs. actual asymmetry masked by averaging.',
    'If costs are genuinely symmetric: the constraint is a coordination problem (Rope, Tangled Rope possible). If architect biases costs: extraction flows systematically toward one path, revealing Snare structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_asymmetry_source, empirical, 'Whether cost asymmetry reveals architect extraction bias').

omega_variable(
    information_access_inequality,
    'Do all navigators have equal information about the Scylla and Charybdis costs before choosing, or does the architect withhold information to influence which loss occurs?',
    'Epistemic audit: comparison of information available to different agent classes. Do navigators with more resources have better intelligence? Does the architect leak information asymmetrically?',
    'If information is equal: choice is genuinely constrained. If asymmetric: the constraint includes hidden information manipulation (raising suppression measure and confirming Snare). Directly affects whether agents can coordinate effectively.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_access_inequality, empirical, 'Whether information inequality enables architect extraction bias').

omega_variable(
    counterfactual_exit_cost,
    'What is the actual cost of refusing both Scylla and Charybdis (attempting tertiary exit, negotiation, or stasis)? Does the architect enforce the binary through inflated refusal costs?',
    'Historical tracking of exit attempts: what happened to navigators who attempted to refuse, negotiate, or find alternatives? Comparison of stated costs vs. actual enforcement costs.',
    'If refusal cost is very high (higher than either path cost): the architect is enforcing the binary through suppression. If refusal cost is moderate or lower: the constraint is genuinely a trade-off, not a coerced choice. This determines Snare vs. Tangled Rope vs. Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_exit_cost, empirical, 'Whether architect inflates refusal costs to enforce binary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scylla_charybdis_navigation, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scylla_tr_t0, scylla_charybdis_navigation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(scylla_tr_t3, scylla_charybdis_navigation, theater_ratio, 3, 0.38).
narrative_ontology:measurement(scylla_tr_t6, scylla_charybdis_navigation, theater_ratio, 6, 0.45).
narrative_ontology:measurement(scylla_tr_t9, scylla_charybdis_navigation, theater_ratio, 9, 0.45).

% Extraction over time
narrative_ontology:measurement(scylla_be_t0, scylla_charybdis_navigation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(scylla_be_t3, scylla_charybdis_navigation, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(scylla_be_t6, scylla_charybdis_navigation, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(scylla_be_t9, scylla_charybdis_navigation, base_extractiveness, 9, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scylla_charybdis_navigation, enforcement_mechanism).
narrative_ontology:affects_constraint(scylla_charybdis_navigation, regulatory_compliance_impossibility).
narrative_ontology:affects_constraint(scylla_charybdis_navigation, arms_race_mutual_escalation).
narrative_ontology:affects_constraint(scylla_charybdis_navigation, zero_sum_competitive_dynamics).

% DUAL FORMULATION NOTE:
% Scylla-Charybdis navigation is a family of forced-choice constraints. This story covers the canonical symmetric-cost dilemma. Related constraints include asymmetric versions (one path costs agent, other costs collective), information-hidden versions (costs unknown at choice point), and time-dependent versions (costs evolve as agent moves through the constraint). Each variant has different ε and suppression values and should be decomposed into separate stories if the observable (cost measurement basis) changes classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
