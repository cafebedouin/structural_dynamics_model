% ============================================================================
% CONSTRAINT STORY: fda_gonorrhea_efficacy_standard
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-01
% ============================================================================

:- module(constraint_fda_gonorrhea_efficacy_standard, []).

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
 *   constraint_id: fda_gonorrhea_efficacy_standard
 *   human_readable: "FDA Unified Efficacy Standard for Gonorrhea Treatment"
 *   domain: technological/regulatory
 *
 * SUMMARY:
 *   An FDA advisory panel voted against approving a new oral antibiotic for
 *   uncomplicated gonorrhea. While effective for common urogenital infections,
 *   its lower efficacy for throat and rectal infections led to the rejection.
 *   This enforces a high, unified standard of efficacy across all infection
 *   sites, which acts as a constraint that prevents a potentially useful,
 *   albeit imperfect, drug from reaching patients amid a growing public
 *   health crisis of antibiotic resistance.
 *
 * KEY AGENTS (by structural relationship):
 *   - Gonorrhea Patients (urogenital): Primary target (powerless/trapped) — denied access to a convenient oral treatment.
 *   - Public Health Bodies (e.g., CDC): Secondary target (institutional/constrained) — hampered in efforts to combat antibiotic resistance.
 *   - FDA Regulatory Body: Primary beneficiary (institutional/arbitrage) — maintains a simple, risk-averse standard, protecting institutional reputation.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fda_gonorrhea_efficacy_standard, 0.48).
domain_priors:suppression_score(fda_gonorrhea_efficacy_standard, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(fda_gonorrhea_efficacy_standard, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fda_gonorrhea_efficacy_standard, extractiveness, 0.48).
narrative_ontology:constraint_metric(fda_gonorrhea_efficacy_standard, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(fda_gonorrhea_efficacy_standard, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fda_gonorrhea_efficacy_standard, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(fda_gonorrhea_efficacy_standard). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fda_gonorrhea_efficacy_standard, fda_regulatory_body).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fda_gonorrhea_efficacy_standard, gonorrhea_patients_urogenital).
narrative_ontology:constraint_victim(fda_gonorrhea_efficacy_standard, public_health_bodies).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The patient with a urogenital infection, for whom the drug is effective,
% but who is denied access. From this view, the standard is a purely
% extractive barrier to a needed treatment.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The FDA as an institution. The standard is a pure coordination mechanism
% that simplifies the regulatory landscape, ensures a high quality bar, and
% minimizes the risk of treatment failure and reputational harm.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the valid coordination function (maintaining drug
% standards) and the asymmetric extraction (denying a useful tool to a
% specific patient population). This duality is the definition of a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4A: THE PUBLIC HEALTH BODY (e.g., CDC)
% An institutional actor whose mission is hampered by the constraint. They are a
% 'victim' but have institutional power. Their constrained exit (they must work
% within the FDA framework) results in a higher directionality `d` than the FDA.
% They see the coordination function but feel the extraction more keenly.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fda_gonorrhea_efficacy_standard_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(tangled_rope_detection) :-
    constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, tangled_rope, context(agent_power(analytical), _, _, _)).

test(high_extraction_and_suppression_metrics) :-
    domain_priors:base_extractiveness(fda_gonorrhea_efficacy_standard, E), E >= 0.46,
    domain_priors:suppression_score(fda_gonorrhea_efficacy_standard, S), S >= 0.60.

:- end_tests(fda_gonorrhea_efficacy_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value reflects the significant societal cost of withholding a new, effective oral antibiotic for an 'urgent threat' pathogen. The extraction is the opportunity cost imposed on public health and individual patients.
 *   - Suppression (0.85): The FDA's decision is binding and effectively removes this treatment option from the market. There are no alternative regulatory paths, making suppression very high.
 *   - The constraint is a Tangled Rope because it possesses both a crucial coordination function (ensuring high standards for all approved drugs, which is a public good) and a clear asymmetric extraction (the cost of this rigid standard is borne by patients who would benefit from the drug).
 *
 * PERSPECTIVAL GAP:
 *   - The patient (powerless/trapped) experiences the standard as a Snare: a seemingly arbitrary rule that denies them a better treatment option for no discernible benefit to them.
 *   - The FDA (institutional/arbitrage) experiences it as a Rope: a necessary and beneficial coordination tool to manage risk and maintain public trust in the drug supply.
 *   - The analytical view resolves this tension by classifying it as a Tangled Rope, acknowledging the validity of both the coordination and extraction components.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: The `fda_regulatory_body` benefits from a simplified, risk-averse standard that is easy to enforce and defend. This reduces institutional complexity and potential for blame if a partially effective drug leads to negative outcomes.
 *   - Victims: The `gonorrhea_patients_urogenital` are the primary victims, bearing the direct cost of being denied a convenient treatment. `public_health_bodies` are secondary victims, as their toolkit for fighting a major health threat is limited by this standard. This structural data drives the engine's directionality calculation, assigning a low `d` to the FDA and high `d` to patients.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model includes separate perspectives for the FDA (beneficiary, arbitrage exit) and other public health bodies like the CDC (victim, constrained exit). While both are institutional actors, their different structural relationships and exit options lead to different derived `d` values. The CDC perceives more extraction from the constraint because its primary mission is hampered, whereas the FDA's primary mission (gatekeeping) is fulfilled. This models the real-world friction between regulatory caution and public health urgency.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework prevents mislabeling. A naive analysis might call the FDA standard "pure bureaucracy" (a Snare). An institutional defender might call it "pure public safety" (a Rope). The Tangled Rope classification is more precise: it correctly identifies that the standard is *both* a vital coordination mechanism and an extractive instrument. It forces an analysis of whether the coordination benefit justifies the extractive cost, which is the core policy question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_fda_gonorrhea_efficacy_standard,
    'Is the risk of accelerating antibiotic resistance by releasing a "partially" effective drug greater than the public health benefit of having a new oral option for the most common infection sites?',
    'Long-term epidemiological modeling and controlled post-market surveillance studies comparing treatment outcomes and resistance patterns.',
    'If risk > benefit, the standard is a justified Rope. If benefit > risk, the standard is a highly extractive Tangled Rope bordering on a Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fda_gonorrhea_efficacy_standard, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models a gradual increase in regulatory rigidity over time,
% where standards for approval become higher and less flexible, thus
% increasing the extractive cost of the regulatory framework.

% Theater ratio over time (constant, as the process remains functional):
narrative_ontology:measurement(fda_ge_tr_t0, fda_gonorrhea_efficacy_standard, theater_ratio, 0, 0.10).
narrative_ontology:measurement(fda_ge_tr_t5, fda_gonorrhea_efficacy_standard, theater_ratio, 5, 0.10).
narrative_ontology:measurement(fda_ge_tr_t10, fda_gonorrhea_efficacy_standard, theater_ratio, 10, 0.10).

% Extraction over time (increasing as standards tighten):
narrative_ontology:measurement(fda_ge_ex_t0, fda_gonorrhea_efficacy_standard, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fda_ge_ex_t5, fda_gonorrhea_efficacy_standard, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(fda_ge_ex_t10, fda_gonorrhea_efficacy_standard, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The FDA standard functions as a benchmark for information about drug quality.
narrative_ontology:coordination_type(fda_gonorrhea_efficacy_standard, information_standard).

% Network relationships (structural influence edges)
% This specific standard can have downstream effects on the economic incentives
% for developing new antibiotics, as it raises the bar for success.
narrative_ontology:affects_constraint(fda_gonorrhea_efficacy_standard, antibiotic_pipeline_incentives).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural derivation chain,
% using the declared beneficiary/victim groups and their distinct exit options
% (arbitrage, trapped, constrained), accurately captures the directionality
% for each key agent.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */