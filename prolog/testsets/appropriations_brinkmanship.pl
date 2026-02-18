% ============================================================================
% CONSTRAINT STORY: appropriations_brinkmanship
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_appropriations_brinkmanship, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: appropriations_brinkmanship
 *   human_readable: Government Shutdown Threat via Appropriations Process
 *   domain: political
 *
 * SUMMARY:
 *   This constraint describes the recurring political tactic of using the
 *   legislative appropriations deadline as a leverage point to force policy
 *   concessions. A minority faction within a legislative body can threaten
 *   to block a full-year funding bill, triggering a government shutdown. This
 *   imposes severe costs on federal employees and the public, creating
 *   pressure on the majority party to concede to demands unrelated to the
 *   budget itself. The mechanism combines a necessary coordination function
 *   (funding the government) with asymmetric, coercive extraction.
 *
 * KEY AGENTS (by structural relationship):
 *   - DHS Federal Employees: Primary target (powerless/trapped) — bear the cost of furloughs and pay freezes.
 *   - Minority Party Caucus: Primary beneficiary (institutional/arbitrage) — uses the shutdown threat as leverage to extract policy wins.
 *   - Governing Majority Party: Secondary target (institutional/constrained) — bears political and operational costs, forced to negotiate under duress.
 *   - Analytical Observer: Analytical — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(appropriations_brinkmanship, 0.65).
domain_priors:suppression_score(appropriations_brinkmanship, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(appropriations_brinkmanship, 0.60).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(appropriations_brinkmanship, extractiveness, 0.65).
narrative_ontology:constraint_metric(appropriations_brinkmanship, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(appropriations_brinkmanship, theater_ratio, 0.60).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(appropriations_brinkmanship, tangled_rope).
narrative_ontology:human_readable(appropriations_brinkmanship, "Government Shutdown Threat via Appropriations Process").
narrative_ontology:topic_domain(appropriations_brinkmanship, "political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(appropriations_brinkmanship). % Required for Tangled Rope. The deadline is a hard, enforced rule.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(appropriations_brinkmanship, minority_party_caucus).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(appropriations_brinkmanship, dhs_federal_employees).
narrative_ontology:constraint_victim(appropriations_brinkmanship, governing_majority_party).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% DHS federal employees are trapped in the system. They face furloughs or working
% without pay. The high extraction (lost wages, uncertainty) and high suppression
% (no alternative funding mechanism) make this a Snare.
% Engine derives: victim + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42 -> high χ
constraint_indexing:constraint_classification(appropriations_brinkmanship, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The minority party caucus uses the threat as a tool for internal coordination
% and external leverage. For them, it is a low-cost, high-reward mechanism.
% The extraction is directed away from them, making χ appear low or negative.
% Engine derives: beneficiary + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12 -> low χ
constraint_indexing:constraint_classification(appropriations_brinkmanship, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the legitimate coordination function (passing a budget)
% and the parasitic, extractive rider (policy concessions under duress). This
% dual nature is the hallmark of a Tangled Rope.
% Engine derives d ≈ 0.72 -> f(d) ≈ 1.15
constraint_indexing:constraint_classification(appropriations_brinkmanship, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The governing majority party is also an institutional actor, but their relationship
% to the constraint is starkly different from the minority caucus. They are a
% target of the extraction and their exit is constrained. They cannot simply
% walk away without causing the shutdown they are trying to avoid.
% Engine derives: victim + constrained exit -> d ≈ 0.7-0.8 -> high χ
constraint_indexing:constraint_classification(appropriations_brinkmanship, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(appropriations_brinkmanship_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between the primary target and primary beneficiary.
    constraint_indexing:constraint_classification(appropriations_brinkmanship, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(appropriations_brinkmanship, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('... Target (powerless) sees Snare, Beneficiary (institutional/arbitrage) sees Rope. Gap confirmed.~n').

test(inter_institutional_gap) :-
    % Verify gap between the two institutional actors.
    constraint_indexing:constraint_classification(appropriations_brinkmanship, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(appropriations_brinkmanship, TypeTarget, context(agent_power(institutional), _, exit_options(constrained), _)),
    TypeTarget \= TypeBeneficiary,
    format('... Beneficiary inst. (arbitrage) sees ~w, Target inst. (constrained) sees ~w. Inter-institutional gap confirmed.~n', [TypeBeneficiary, TypeTarget]).

test(analytical_claim_matches_type) :-
    narrative_ontology:constraint_claim(appropriations_brinkmanship, ClaimedType),
    constraint_indexing:constraint_classification(appropriations_brinkmanship, ClaimedType, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_compliance) :-
    % A Tangled Rope must have beneficiary, victim, AND active enforcement.
    narrative_ontology:constraint_beneficiary(appropriations_brinkmanship, _),
    narrative_ontology:constraint_victim(appropriations_brinkmanship, _),
    domain_priors:requires_active_enforcement(appropriations_brinkmanship).

:- end_tests(appropriations_brinkmanship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): High. The constraint's primary function in this context is not
 *     efficient governance but coercion. It extracts massive costs (economic disruption, lost wages,
 *     security risks) and converts them into political leverage.
 *   - Suppression (0.80): High. The US political system's structure, particularly in a polarized
 *     environment, offers few alternatives. Mechanisms like automatic continuing resolutions are
 *     actively suppressed, making this high-stakes confrontation almost unavoidable.
 *   - Theater (0.60): High. The process is a public spectacle. Countdown clocks, press conferences,
 *     and partisan messaging are integral to the strategy, making it highly theatrical but still
 *     functionally potent.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For a DHS employee (powerless, trapped), the shutdown is a pure Snare that
 *   threatens their livelihood for reasons beyond their control. For the leveraging political faction
 *   (institutional, arbitrage), it is a powerful Rope for enforcing caucus discipline and achieving
 *   policy goals, with costs externalized to others. This difference in classification is a direct
 *   result of their opposing structural relationships to the constraint, correctly captured by the
 *   directionality (d) derived from beneficiary/victim status and exit options.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a canonical example of inter-institutional conflict. Both the minority and majority
 *   parties are 'institutional' actors, but their experiences diverge based on their exit options.
 *   The minority caucus has 'arbitrage' exit; they can choose to deploy this tactic or not, profiting
 *   from the disruption. The majority party has 'constrained' exit; they are responsible for governing
 *   and cannot easily walk away, making them a target. The system correctly classifies the former's
 *   view as Rope and the latter's as Tangled Rope, measuring the power imbalance.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework correctly identifies the constraint as a Tangled Rope from an analytical view,
 *   avoiding two key errors. It is not a pure Snare, because it is grafted onto a necessary
 *   coordination function (funding government). It is not a pure Rope, because of the enormous,
 *   asymmetrically distributed extraction it generates. By modeling both components, the Tangled
 *   Rope classification provides a precise diagnosis of a coordination mechanism that has been
 *   weaponized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_appropriations_brinkmanship,
    'Is appropriations brinkmanship a stable, albeit dysfunctional, feature of polarized politics, or is it a terminal symptom leading to systemic breakdown?',
    'Longitudinal analysis over several more electoral cycles. Resolution would require observing whether the system develops counter-mechanisms (e.g., automatic funding) or if the frequency and severity of shutdowns escalate.',
    'If stable, it remains a Tangled Rope. If terminal, it is a precursor to a more fundamental state failure, potentially a phase transition of the entire political system.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(appropriations_brinkmanship, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This tactic has intensified over the last decade. It began as a rare, extreme measure
% and has evolved into a routine part of the political toolkit, indicating both
% extraction_accumulation and an increase in performative theater.
% Required because base_extractiveness (0.65) > 0.46.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(abrink_tr_t0, appropriations_brinkmanship, theater_ratio, 0, 0.30).
narrative_ontology:measurement(abrink_tr_t5, appropriations_brinkmanship, theater_ratio, 5, 0.50).
narrative_ontology:measurement(abrink_tr_t10, appropriations_brinkmanship, theater_ratio, 10, 0.60).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(abrink_ex_t0, appropriations_brinkmanship, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(abrink_ex_t5, appropriations_brinkmanship, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(abrink_ex_t10, appropriations_brinkmanship, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The core function, however corrupted, is allocating resources.
narrative_ontology:coordination_type(appropriations_brinkmanship, resource_allocation).

% Network relationships (structural influence edges)
% The constant use of short-term, crisis-driven funding structurally
% damages the ability of government agencies to engage in effective
% long-term strategic planning.
narrative_ontology:affects_constraint(appropriations_brinkmanship, long_term_strategic_planning).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The automatic derivation chain
% correctly distinguishes between the two institutional actors based on their
% declared victim/beneficiary status and their different exit options
% ('arbitrage' vs. 'constrained'). The resulting 'd' values accurately
% reflect their distinct structural positions.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */