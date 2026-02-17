% ============================================================================
% CONSTRAINT STORY: us_israel_faa_502b_nonenforcement
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-10-27
% ============================================================================

:- module(constraint_us_israel_faa_502b_nonenforcement, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_israel_faa_502b_nonenforcement
 *   human_readable: Non-enforcement of US Foreign Assistance Act Section 502B for Israel
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint models the de facto non-enforcement of Section 502B of the
 *   US Foreign Assistance Act, which legally prohibits security assistance to
 *   countries engaged in a consistent pattern of gross human rights violations.
 *   Despite internal State Department reports documenting such potential
 *   violations, US military aid to Israel has continued without interruption,
 *   creating a significant gap between the law as written (de jure) and its
 *   application (de facto).
 *
 * KEY AGENTS (by structural relationship):
 *   - Palestinian Civilians: Primary target (powerless/trapped) — bear the costs of military actions enabled by the unconditional aid.
 *   - Israeli Government & Military: Primary beneficiary (institutional/arbitrage) — receive military aid and diplomatic cover without human rights conditionality.
 *   - US Political Leadership (Congress/White House): Primary beneficiary (institutional/arbitrage) — maintains a key strategic alliance and satisfies domestic political interests.
 *   - US State Department Human Rights Bureaus: Institutional victim (institutional/constrained) — their mandate and own reporting are politically sidelined, rendering their function theatrical.
 *   - Analytical Observer: Sees the full structure, including the law's coordination intent and the extractive reality of its non-enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_israel_faa_502b_nonenforcement, 0.75).
domain_priors:suppression_score(us_israel_faa_502b_nonenforcement, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_israel_faa_502b_nonenforcement, 0.60).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_israel_faa_502b_nonenforcement, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_israel_faa_502b_nonenforcement, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(us_israel_faa_502b_nonenforcement, theater_ratio, 0.60).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_israel_faa_502b_nonenforcement, tangled_rope).
narrative_ontology:human_readable(us_israel_faa_502b_nonenforcement, "Non-enforcement of US Foreign Assistance Act Section 502B for Israel").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_israel_faa_502b_nonenforcement). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, israeli_government_and_military).
narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, us_political_leadership).
narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, us_defense_industry).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, palestinian_civilians).
narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, us_state_department_human_rights_bureaus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Palestinian civilians experience the direct consequences of unconditional
% military aid. They are trapped by the geopolitical reality, which is heavily
% shaped by the US-Israel alliance.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → high χ
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Israeli government and US political leadership see this as a pure
% coordination mechanism for the strategic alliance. The non-enforcement is the
% "grease" that makes the relationship function smoothly.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → negative χ
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view sees both the coordination function (maintaining an alliance)
% and the severe asymmetric extraction (bypassing human rights law at great cost
% to the victims). The high ε, high suppression, and active enforcement of the
% non-enforcement confirm the Tangled Rope classification.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE CONSTRAINED INSTITUTION (TANGLED ROPE)
% US State Department human rights officials are caught in the middle. They are
% part of a powerful institution but are victims of the political capture of
% their legally mandated function. Their exit is constrained by career paths and
% political pressure. They experience the constraint as a broken coordination
% mechanism coupled with coercive internal politics.
% Engine derives d from: victim membership + institutional power + constrained exit → intermediate/high d
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_israel_faa_502b_nonenforcement_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(tangled_rope_gate_validation) :-
    narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, _),
    narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, _),
    domain_priors:requires_active_enforcement(us_israel_faa_502b_nonenforcement).

test(analytical_claim_matches_type) :-
    narrative_ontology:constraint_claim(us_israel_faa_502b_nonenforcement, tangled_rope),
    constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, tangled_rope, context(agent_power(analytical), _, _, _)).


:- end_tests(us_israel_faa_502b_nonenforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Set high to reflect the immense value (billions in military aid annually plus diplomatic immunity) transferred by circumventing the legal conditions.
 *   - Suppression (0.80): Set high due to the US's unique geopolitical power (e.g., UN veto) and the strong domestic political consensus which suppresses alternatives like enforcing the law or passing new legislation.
 *   - Theater (0.60): The State Department must still write and publish human rights reports, and officials must testify before Congress. This creates a performance of oversight, while the functional outcome (unconditional aid) remains unchanged.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the Israeli government and US leadership (beneficiaries), the non-enforcement is a Rope, a smooth coordination tool for a strategic alliance. For Palestinian civilians (victims), it is a Snare, a system that enables harm while they are trapped with no recourse. This Rope/Snare duality is characteristic of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The Israeli government receives advanced weaponry and diplomatic shielding. The US political establishment maintains a critical alliance and satisfies powerful lobbying interests. US defense contractors secure stable, long-term contracts.
 *   - Victims: Palestinian civilians bear the physical cost of military actions conducted with the aid. US State Department officials in human rights roles are also structural victims, as their legally-mandated function is politically neutralized, undermining their institutional purpose. This mapping drives the divergent directionality `d` values and thus the different classifications.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The constraint reveals a fault line within the US government. The political arm (White House/Congress) acts as a beneficiary with `arbitrage` exit, freely choosing to maintain the policy. The bureaucratic/legal arm (State Dept. human rights bureaus) acts as a victim with `constrained` exit. They are tasked with upholding a law that the political leadership actively chooses to ignore, placing them in an institutionally compromised position. This difference in exit options and structural relationship to the constraint explains why two institutional actors classify it differently.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful example of mandatrophy. A law designed for coordination (aligning foreign aid with human rights values) has been captured. The system correctly identifies it not as a simple Snare (which would miss the coordination aspect of the alliance) nor as a Rope (which would ignore the massive extraction and harm), but as a Tangled Rope. This classification acknowledges both the genuine strategic coordination function and the severe, asymmetric extraction that results from the non-enforcement of its legal guardrails. [RESOLVED MANDATROPHY]
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_us_israel_faa_502b_nonenforcement,
    'Is the non-enforcement a continuous, deliberate political decision, or has it become a deeply embedded institutional inertia (a nascent Piton)?',
    'Analysis of internal communications and decision-making processes over decades; or observing the system response if a major political realignment were to challenge the status quo.',
    'If deliberate, it remains a Tangled Rope. If inertial, its theater ratio would climb above 0.70 as the original strategic justifications become less relevant, and it would transition into a Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. Interval represents roughly 40 years of policy.
narrative_ontology:interval(us_israel_faa_502b_nonenforcement, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This policy has become more entrenched over time. Initial non-enforcement
% may have been less extractive and theatrical, but has intensified over decades.
% Base_extractiveness > 0.46, so temporal data is required.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(us_israel_faa_502b_nonenforcement_tr_t0, us_israel_faa_502b_nonenforcement, theater_ratio, 0, 0.30).
narrative_ontology:measurement(us_israel_faa_502b_nonenforcement_tr_t20, us_israel_faa_502b_nonenforcement, theater_ratio, 20, 0.45).
narrative_ontology:measurement(us_israel_faa_502b_nonenforcement_tr_t40, us_israel_faa_502b_nonenforcement, theater_ratio, 40, 0.60).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(us_israel_faa_502b_nonenforcement_ex_t0, us_israel_faa_502b_nonenforcement, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(us_israel_faa_502b_nonenforcement_ex_t20, us_israel_faa_502b_nonenforcement, base_extractiveness, 20, 0.70).
narrative_ontology:measurement(us_israel_faa_502b_nonenforcement_ex_t40, us_israel_faa_502b_nonenforcement, base_extractiveness, 40, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The non-enforcement serves to coordinate the US-Israel strategic alliance,
% making it a form of (perverse) enforcement mechanism.
narrative_ontology:coordination_type(us_israel_faa_502b_nonenforcement, enforcement_mechanism).

% Network relationships (structural influence edges)
% The non-enforcement of this specific law has a corrosive effect on the
% broader international legal framework governing conflict and occupation.
narrative_ontology:affects_constraint(us_israel_faa_502b_nonenforcement, international_law_on_occupation).
narrative_ontology:affects_constraint(us_israel_faa_502b_nonenforcement, us_arms_export_control_act).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are used. The structural model, with distinct beneficiary and
% victim groups (including an institutional victim), is sufficient for the
% engine to derive accurate directionality values for the different perspectives,
% particularly the key inter-institutional dynamic between political and
% bureaucratic actors.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */