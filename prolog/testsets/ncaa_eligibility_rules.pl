% ============================================================================
% CONSTRAINT STORY: ncaa_eligibility_rules
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_ncaa_eligibility_rules, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ncaa_eligibility_rules
 *   human_readable: NCAA Eligibility Rules for Student Athletes
 *   domain: economic
 *
 * SUMMARY:
 *   The NCAA eligibility rules govern who can participate in college athletics,
 *   restricting the economic opportunities of student-athletes while generating
 *   significant revenue for universities and the NCAA.
 *
 * KEY AGENTS (by structural relationship):
 *   - Student-Athletes: Primary target (powerless/trapped) — bears extraction
 *   - NCAA: Primary beneficiary (institutional/arbitrage) — benefits from rules
 *   - Universities: Secondary beneficiary (institutional/constrained)
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ncaa_eligibility_rules, 0.55).
domain_priors:suppression_score(ncaa_eligibility_rules, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(ncaa_eligibility_rules, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ncaa_eligibility_rules, extractiveness, 0.55).
narrative_ontology:constraint_metric(ncaa_eligibility_rules, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(ncaa_eligibility_rules, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ncaa_eligibility_rules, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(ncaa_eligibility_rules). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ncaa_eligibility_rules, ncaa).
narrative_ontology:constraint_beneficiary(ncaa_eligibility_rules, universities).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ncaa_eligibility_rules, student_athletes).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE/MOUNTAIN)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
%
% UNIFORM-TYPE EXCEPTION: For natural law constraints (mountain-only) or pure
% coordination constraints (rope-only), perspectives 1 and 2 may use any power
% atoms — the classification is the same from all perspectives. Include at
% least 2-3 perspectives to demonstrate the invariance.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(ncaa_eligibility_rules, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2A: THE SECONDARY BENEFICIARY
% Universities benefit, but are also constrained by the rules.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ncaa_eligibility_rules_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(ncaa_eligibility_rules, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ncaa_eligibility_rules, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ncaa_eligibility_rules, ExtMetricName, E),
    E >= 0.46. % Mountain or high-extraction Snare/Tangled.

:- end_tests(ncaa_eligibility_rules_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The NCAA eligibility rules extract value from student-athletes by
 *   limiting their economic freedom (name, image, likeness restrictions),
 *   suppressing alternative pathways (professional leagues, direct employment),
 *   while generating revenue for the NCAA and universities.
 *   The suppression score is high because the NCAA actively prevents
 *   alternatives and enforces these rules stringently.
 *
 * PERSPECTIVAL GAP:
 *   Student-athletes perceive the rules as a snare, limiting their
 *   opportunities. The NCAA perceives them as a necessary rope,
 *   maintaining competitive balance and academic standards (though this is debated).
 *   Universities see a tangled rope. They benefit from access to talent and revenue,
 *   but are increasingly constrained by compliance requirements and legal challenges.
 *
 * DIRECTIONALITY LOGIC:
 *   Student-athletes are the victims, as the rules restrict their economic
 *   opportunities. The NCAA and universities are beneficiaries, receiving
 *   revenue and prestige from college sports.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling as a pure
 *   extraction mechanism because there *is* a coordination function: setting eligibility standards
 *   to promote a level playing field and ensure athletes are also students.
 *   However, the asymmetric extraction from athletes to institutions makes it a Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ncaa_eligibility,
    'To what extent does the NCAA eligibility process genuinely promote academic standards versus acting as a revenue protection scheme?',
    'Empirical studies tracking athletes academic progress vs. athletic revenue generation.',
    'If academic standards are primary, a lower base_extractiveness is appropriate. If revenue protection is primary, a higher base_extractiveness is appropriate',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ncaa_eligibility_rules, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ncaa_eligibility_rules_tr_t0, ncaa_eligibility_rules, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ncaa_eligibility_rules_tr_t5, ncaa_eligibility_rules, theater_ratio, 5, 0.20).
narrative_ontology:measurement(ncaa_eligibility_rules_tr_t10, ncaa_eligibility_rules, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ncaa_eligibility_rules_ex_t0, ncaa_eligibility_rules, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(ncaa_eligibility_rules_ex_t5, ncaa_eligibility_rules, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(ncaa_eligibility_rules_ex_t10, ncaa_eligibility_rules, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(ncaa_eligibility_rules, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(ncaa_eligibility_rules, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(ncaa_eligibility_rules, [other_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(ncaa_eligibility_rules, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */