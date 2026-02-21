% ============================================================================
% CONSTRAINT STORY: nero_imperial_expropriation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_nero_imperial_expropriation, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nero_imperial_expropriation
 *   human_readable: Nero's Imperial Legitimacy via Expropriation and Spectacle
 *   domain: political/economic
 *
 * SUMMARY:
 *   This constraint models the political-economic system of Nero's later reign
 *   (c. 62-68 AD), wherein the emperor secured popular support and funded massive
 *   public works (the "coordination" aspect) through the systematic and often
 *   lethal expropriation of wealth from the Roman senatorial and equestrian
 *   classes (the "extraction" aspect). Legitimacy was maintained by directing
 *   benefits to the plebeians while concentrating costs on a politically
 *   suppressed elite.
 *
 * KEY AGENTS (by structural relationship):
 *   - Roman Senatorial Class: Primary target (powerful/trapped) — bears the full cost of extraction via property seizure and execution.
 *   - Imperial Court (Nero & Allies): Primary beneficiary (institutional/arbitrage) — directs the system, controls the rules, and consolidates power.
 *   - Roman Plebeians: Secondary beneficiary (powerless/mobile) — receives public works, grain, and spectacles, providing the popular support that legitimizes the system.
 *   - Analytical Observer (e.g., Tacitus, modern historians): Sees the full structure of coordinated public benefit funded by coercive extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nero_imperial_expropriation, 0.75).
domain_priors:suppression_score(nero_imperial_expropriation, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(nero_imperial_expropriation, 0.55).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nero_imperial_expropriation, extractiveness, 0.75).
narrative_ontology:constraint_metric(nero_imperial_expropriation, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(nero_imperial_expropriation, theater_ratio, 0.55).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(nero_imperial_expropriation, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(nero_imperial_expropriation). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(nero_imperial_expropriation, imperial_court).
narrative_ontology:constraint_beneficiary(nero_imperial_expropriation, roman_plebeians).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(nero_imperial_expropriation, roman_senatorial_class).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET
%   Snare:        victim required; beneficiary optional -> MET

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (A ROMAN SENATOR)
% From the perspective of a wealthy senator, this system is a lethal trap.
% The 'powerless' atom reflects their inability to resist the emperor's will,
% and 'trapped' reflects that fleeing meant forfeiture of property and being
% declared an enemy of the state. The engine derives d≈0.95 -> f(d)≈1.42 -> high χ.
constraint_indexing:constraint_classification(nero_imperial_expropriation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE IMPERIAL COURT)
% For Nero and his administration, this is a highly effective tool for governance,
% funding, and power consolidation. 'Institutional' power and 'arbitrage' exit
% (they set the rules) means the engine derives d≈0.05 -> f(d)≈-0.12 -> negative χ.
% From this viewpoint, the system appears as pure coordination (a Rope).
constraint_indexing:constraint_classification(nero_imperial_expropriation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (HISTORIAN)
% The analytical view must account for both the coordination function (public works,
% popular support) and the highly extractive mechanism. The declaration of both
% beneficiary and victim groups, plus active enforcement, meets the criteria
% for a Tangled Rope. This is the canonical classification.
constraint_indexing:constraint_classification(nero_imperial_expropriation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nero_imperial_expropriation_tests).

test(perspectival_gap) :-
    % Verify the core perspectival gap between the Senator (Target) and the Court (Beneficiary).
    constraint_indexing:constraint_classification(nero_imperial_expropriation, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(nero_imperial_expropriation, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival gap validated: Target sees Snare, Beneficiary sees Rope.~n').

test(analytical_classification_is_tangled_rope) :-
    % Ensure the analytical perspective resolves the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(nero_imperial_expropriation, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_for_high_extraction) :-
    % Verify that base metrics meet the criteria for a high-extraction constraint.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nero_imperial_expropriation, ExtMetricName, E),
    E >= 0.46,
    narrative_ontology:constraint_metric(nero_imperial_expropriation, suppression_requirement, S),
    S >= 0.60.

:- end_tests(nero_imperial_expropriation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): The extraction was not a tax but the full seizure of assets from wealthy families, a near-total form of expropriation.
 *   - Suppression Score (0.80): Opposition to Nero, especially in his later reign, was met with forced suicide or execution. There were no viable, safe alternatives for the targeted elite, making suppression extremely high.
 *   - Theater Ratio (0.55): The system was not pure theater; real public works were built and grain was distributed. However, the justifications for seizure (conspiracy, treason) were often theatrical show trials, and the public spectacles were a key component of maintaining legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a senator, the rule of law has collapsed into a predatory trap (Snare). For Nero's court, it is an efficient, if ruthless, state-building and resource-allocation mechanism (Rope). The plebeians, receiving benefits, would also likely view it as a Rope, or at worst a legitimate form of redistributive justice. This gap is what the Deferential Realism framework is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The flow of resources is from the `roman_senatorial_class` (victim) to the `imperial_court` and `roman_plebeians` (beneficiaries). The declarations directly model this structure. A senator's `trapped` status derives a high 'd', amplifying perceived extraction. The court's `arbitrage` exit status derives a low 'd', making extraction appear negative (i.e., a subsidy).
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This classification avoids two key errors. First, it avoids labeling the system a pure Snare, which would ignore the genuine coordination function of funding the state and maintaining popular support. Second, it avoids labeling it a Rope, which would whitewash the brutal, coercive extraction at its core. The Tangled Rope classification correctly identifies it as a hybrid system where a public-facing coordination function is inextricably linked to, and dependent upon, severe asymmetric extraction. The high extraction score is justified by the nature of the expropriation (total asset seizure), and the framework correctly identifies the hybrid nature rather than defaulting to a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_nero_intent,
    'Was the system a calculated populist policy to redistribute wealth for the public good, or a tyrant''s cynical ploy to eliminate rivals and fund personal extravagance under the guise of public benefit?',
    'Unrecoverable historical data, such as private correspondence or records from Nero''s inner circle detailing motivations.',
    'If primarily populist, the "coordination" aspect is stronger, though still coercive. If primarily tyrannical, the "extraction" is the true purpose and coordination is mere theater, pushing the Theater Ratio higher.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_nero_intent, empirical, 'The degree to which Nero''s expropriations were driven by populist policy versus personal tyranny, which would alter the theater ratio.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing, modeling the ~14 year reign.
narrative_ontology:interval(nero_imperial_expropriation, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the degradation of Nero's reign over time. His early years under
% Seneca were seen as a "golden age", while his later years became tyrannical.
% The final values (T=14) must match the base properties in Section 2.
% The constraint itself is defined by its final, most severe state.

% Theater ratio over time (Goodhart drift):
narrative_ontology:measurement(nero_imperial_expropriation_tr_t0, nero_imperial_expropriation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(nero_imperial_expropriation_tr_t7, nero_imperial_expropriation, theater_ratio, 7, 0.35).
narrative_ontology:measurement(nero_imperial_expropriation_tr_t14, nero_imperial_expropriation, theater_ratio, 14, 0.55).

% Extraction over time (Extraction accumulation):
narrative_ontology:measurement(nero_imperial_expropriation_ex_t0, nero_imperial_expropriation, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(nero_imperial_expropriation_ex_t7, nero_imperial_expropriation, base_extractiveness, 7, 0.50).
narrative_ontology:measurement(nero_imperial_expropriation_ex_t14, nero_imperial_expropriation, base_extractiveness, 14, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system's function was to re-allocate resources from one class to another
% to fund state functions and public works.
narrative_ontology:coordination_type(nero_imperial_expropriation, resource_allocation).

% Network relationships (structural influence edges)
% This system of extraction directly undermined the power of the Senate.
narrative_ontology:affects_constraint(nero_imperial_expropriation, roman_senatorial_power).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation based on
% beneficiary/victim declarations and exit options accurately captures the
% power dynamics between the Imperial Court, the Senate, and the Plebeians.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */