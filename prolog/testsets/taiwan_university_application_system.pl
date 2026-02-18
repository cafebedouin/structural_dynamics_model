% ============================================================================
% CONSTRAINT STORY: taiwan_university_application_system
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_taiwan_university_application_system, []).

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
 *   constraint_id: taiwan_university_application_system
 *   human_readable: Taiwan's Application-Based University Admission System
 *   domain: social/economic
 *
 * SUMMARY:
 *   Taiwan's 'Application-Based Admission' (個人申請) system for university
 *   entrance was intended to provide a holistic assessment of students beyond
 *   standardized tests. However, its complexity and emphasis on elaborate
 *   portfolios have created a system that heavily favors students from affluent
 *   families who can afford expensive cram schools and portfolio preparation
 *   services (costing upwards of TWD 100,000). This creates a significant
 *   access barrier for less affluent students, reinforcing social stratification.
 *
 * KEY AGENTS (by structural relationship):
 *   - less_affluent_students: Primary target (powerless/trapped) — systemically disadvantaged by costs they cannot bear.
 *   - ministry_of_education: Institutional architect (institutional/arbitrage) — views the system as a legitimate coordination tool they manage.
 *   - affluent_students_and_families: Primary beneficiary (powerful/mobile) — leverage financial resources to secure admission advantages.
 *   - cram_schools_and_prep_services: Secondary beneficiary (organized/arbitrage) — profit from the system's complexity and demands.
 *   - analytical_observer: Analytical observer — sees the dual function of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_university_application_system, 0.52).
domain_priors:suppression_score(taiwan_university_application_system, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(taiwan_university_application_system, 0.35).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_university_application_system, extractiveness, 0.52).
narrative_ontology:constraint_metric(taiwan_university_application_system, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(taiwan_university_application_system, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not applicable; this is a human-constructed system.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(taiwan_university_application_system, tangled_rope).
narrative_ontology:human_readable(taiwan_university_application_system, "Taiwan's Application-Based University Admission System").
narrative_ontology:topic_domain(taiwan_university_application_system, "social/economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(taiwan_university_application_system). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(taiwan_university_application_system, affluent_students_and_families).
narrative_ontology:constraint_beneficiary(taiwan_university_application_system, cram_schools_and_prep_services).
narrative_ontology:constraint_beneficiary(taiwan_university_application_system, ministry_of_education).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(taiwan_university_application_system, less_affluent_students).

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
% Less affluent students face a system where success is gated by costs they
% cannot afford. From their perspective, it is a coercive trap that extracts
% opportunity. Engine derives d from victim membership + trapped exit → d ≈ 0.95
% → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(taiwan_university_application_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTIONAL ARCHITECT (ROPE)
% The Ministry of Education or university committees that designed and maintain the
% system see it as a legitimate coordination mechanism for holistic assessment.
% They are institutional beneficiaries whose role is defined by managing this system.
% Engine derives d from beneficiary membership + arbitrage exit -> d ≈ 0.05
% -> f(d) ≈ -0.12 -> negative χ.
constraint_indexing:constraint_classification(taiwan_university_application_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PRIMARY BENEFICIARY (ROPE)
% Affluent families see the system as a coordination mechanism. They use their
% resources to navigate its rules effectively, turning complexity into an advantage.
% Engine derives d from beneficiary membership + mobile exit → d ≈ 0.15 → f(d) ≈ -0.01
% → low/negative χ.
constraint_indexing:constraint_classification(taiwan_university_application_system, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: SECONDARY BENEFICIARY (ROPE)
% The education prep industry profits from the system's rules. For them, it is a pure
% coordination problem that creates a lucrative market.
% Engine derives d from beneficiary membership + arbitrage exit → d ≈ 0.05
% → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(taiwan_university_application_system, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees both the stated coordination goal (holistic assessment) and the
% severe, asymmetric extraction. The system is a hybrid that coordinates for some
% while extracting from others. This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(taiwan_university_application_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_university_application_system_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and institutional beneficiary.
    constraint_indexing:constraint_classification(taiwan_university_application_system, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_university_application_system, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(taiwan_university_application_system, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A Tangled Rope must have beneficiaries, victims, and active enforcement.
    narrative_ontology:constraint_beneficiary(taiwan_university_application_system, _),
    narrative_ontology:constraint_victim(taiwan_university_application_system, _),
    domain_priors:requires_active_enforcement(taiwan_university_application_system).

:- end_tests(taiwan_university_application_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.52): This value reflects the significant financial barrier to entry (TWD 100k+ prep costs) which effectively extracts opportunity from the less affluent and transfers it to the affluent. It's a direct measure of the cost to fully participate.
 *   - Suppression (0.65): While an alternative path (standardized test-only) exists, the application-based system is the primary route for top-tier university programs, making it difficult to opt-out without penalty.
 *   - Tangled Rope Classification: The system meets the core requirements: a genuine coordination function (matching students and universities), asymmetric extraction (benefiting the wealthy), and active enforcement (university/government rules).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For less affluent students ('powerless'/'trapped'), the system is a Snare. Its rules are coercive, and the cost of entry is a trap that locks them out of opportunities. For institutional actors and affluent families, the system is a Rope. They see the rules not as a barrier but as a coordination game they can win with superior resources (for the families) or a legitimate policy tool (for the institution). The system isn't broken for them; it's working as intended.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'affluent_students_and_families' directly benefit by converting wealth into educational advantage. 'cram_schools_and_prep_services' benefit by monetizing the system's complexity. The 'ministry_of_education' benefits as the system's existence justifies its role. Their low derived `d` values result in low or negative effective extraction (χ), classifying the system as a Rope.
 *   - Victims: 'less_affluent_students' bear the full cost. Their status as victims combined with a 'trapped' exit status (they have few viable alternatives to access top universities) results in a high `d` value (~0.95), maximizing effective extraction (χ) and leading to a Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework correctly identifies the system as a Tangled Rope from an analytical view, preventing two common errors. It avoids mislabeling it as a pure Snare, which would ignore its real (though warped) coordination function. It also avoids mislabeling it as a pure Rope, which would whitewash the severe, structurally embedded extraction that harms disadvantaged students. The Tangled Rope classification captures this duality precisely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_taiwan_uni_app,
    'Was the extractive nature of the system an intentional design to favor elites, or an unforeseen consequence of a well-intentioned policy (i.e., Hanlon''s Razor vs. designed Snare)?',
    'Review of internal policy-making documents from the Ministry of Education during the system''s design phase; interviews with original architects.',
    'If intentional, the system is a Snare masquerading as a Rope. If unintentional, it is a Rope that has become tangled through neglect and market forces.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(taiwan_university_application_system, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The system's extractive properties have intensified over time as the prep
% industry grew and the "arms race" for portfolio quality escalated.
% This models its drift from a Rope towards a Tangled Rope/Snare.
% Base_extractiveness > 0.46, so this section is required.

% Theater ratio over time (portfolio grooming replacing genuine holistic review):
narrative_ontology:measurement(tua_tr_t0, taiwan_university_application_system, theater_ratio, 0, 0.10).
narrative_ontology:measurement(tua_tr_t5, taiwan_university_application_system, theater_ratio, 5, 0.25).
narrative_ontology:measurement(tua_tr_t10, taiwan_university_application_system, theater_ratio, 10, 0.35).

% Extraction over time (rising costs of participation):
narrative_ontology:measurement(tua_ex_t0, taiwan_university_application_system, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(tua_ex_t5, taiwan_university_application_system, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(tua_ex_t10, taiwan_university_application_system, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(taiwan_university_application_system, resource_allocation).

% Network relationships (structural influence edges)
% This admissions system directly impacts social mobility and inequality.
narrative_ontology:affects_constraint(taiwan_university_application_system, taiwan_social_mobility_constraint).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options accurately models the
% structural relationships between the agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */