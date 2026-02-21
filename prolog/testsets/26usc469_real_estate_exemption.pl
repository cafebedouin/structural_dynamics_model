% ============================================================================
% CONSTRAINT STORY: 26usc469_real_estate_exemption
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-21
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_26usc469_real_estate_exemption, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
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
 *   constraint_id: 26usc469_real_estate_exemption
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code generally prevents taxpayers from deducting passive
 *   activity losses against active income. The "(c)(7)" exemption carves out a special
 *   exception for "real estate professionals," but defines this status with a strict,
 *   two-part test (750 hours AND more than half of personal services). This creates a
 *   significant, often insurmountable, barrier for individuals with high-income W-2 jobs,
 *   effectively bifurcating taxpayers into those who can and cannot access these deductions.
 *
 * KEY AGENTS (by structural relationship):
 *   - Hybrid W-2 Investors: Primary target (powerless/trapped) — bears extraction via disallowed losses.
 *   - Full-Time Real Estate Professionals: Primary beneficiary (moderate/mobile) — benefits from the clear qualification path.
 *   - The IRS: Enforcing institution (institutional/arbitrage) — benefits from a bright-line rule for administration.
 *   - Analytical Observer: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Forces a choice between 750 hours of specific labor or the extraction of tax revenue through disallowed losses.
domain_priors:base_extractiveness('26usc469_real_estate_exemption', 0.75).
% Explicitly suppresses alternative qualification paths, narrowing the path to the hours-based test.
domain_priors:suppression_score('26usc469_real_estate_exemption', 0.80).
% The rule is functional and not performative; its effects are direct.
domain_priors:theater_ratio('26usc469_real_estate_exemption', 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric('26usc469_real_estate_exemption', extractiveness, 0.75).
narrative_ontology:constraint_metric('26usc469_real_estate_exemption', suppression_requirement, 0.80).
narrative_ontology:constraint_metric('26usc469_real_estate_exemption', theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim('26usc469_real_estate_exemption', tangled_rope).
narrative_ontology:human_readable('26usc469_real_estate_exemption', "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain('26usc469_real_estate_exemption', "economic/legal").

% --- Binary flags ---
% Requires active enforcement by the IRS during audits.
domain_priors:requires_active_enforcement('26usc469_real_estate_exemption').

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary('26usc469_real_estate_exemption', full_time_real_estate_professionals).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim('26usc469_real_estate_exemption', hybrid_w2_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE HYBRID W-2 INVESTOR (SNARE)
% For a doctor, lawyer, or programmer with a demanding W-2 job, the rule is a Snare.
% The "more than half of personal services" test is mathematically impossible to meet,
% so their legitimate real estate losses are disallowed. The rule strangles their
% ability to benefit from tax advantages available to full-time professionals.
constraint_indexing:constraint_classification('26usc469_real_estate_exemption', snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE FULL-TIME REAL ESTATE PROFESSIONAL (ROPE)
% For the full-time professional, the rule is a Rope. It is a clear, functional
% pathway to classifying themselves as "active" and unlocking significant tax
% benefits by deducting paper losses against their income.
constraint_indexing:constraint_classification('26usc469_real_estate_exemption', rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE IRS / TREASURY (ROPE)
% For the IRS, the rule is a Rope. It creates a clear, objective, and enforceable
% bright-line test to distinguish between active professionals and passive hobbyists,
% simplifying administration and preventing perceived abuse of the tax code.
constraint_indexing:constraint_classification('26usc469_real_estate_exemption', rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (a clear rule for the IRS and
% full-time professionals) and the asymmetric extraction (disallowing losses for
% a specific, trapped group). This dual nature is the definition of a Tangled Rope.
constraint_indexing:constraint_classification('26usc469_real_estate_exemption', tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests('26usc469_real_estate_exemption_tests').

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification('26usc469_real_estate_exemption, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification('26usc469_real_estate_exemption', TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(tangled_rope_structural_properties) :-
    % Verify the three required properties for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary('26usc469_real_estate_exemption', _), % -> has_coordination_function
    narrative_ontology:constraint_victim('26usc469_real_estate_exemption', _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement('26usc469_real_estate_exemption').

:- end_tests('26usc469_real_estate_exemption_tests').

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.75): High. The rule directly results in higher tax revenue
 *     (extraction) from a specific group (hybrid investors) who cannot meet its terms.
 *   - Suppression (0.80): High. The rule explicitly suppresses alternative qualification
 *     methods, forcing taxpayers into a narrow, hours-based test that is impossible for
 *     some to pass.
 *   - Theater Ratio (0.10): Low. The rule is not performative; it is a functional piece
 *     of the tax code with direct, material consequences.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the hybrid W-2 investor, the "more than half" test is an
 *   unbreakable barrier, making it a Snare. For the full-time professional and the IRS,
 *   it's a clear, predictable rule that simplifies their work, making it a Rope. The
 *   analytical view recognizes both functions co-exist, hence Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `full_time_real_estate_professionals`. They benefit from a clear
 *     harbor and reduced competition for tax-advantaged investment strategies.
 *   - Victim: `hybrid_w2_investors`. They bear the cost directly through higher taxes
 *     from disallowed passive losses.
 *   This clear division drives the directionality calculation, leading to high
 *   effective extraction (χ) for the victim and low/negative χ for the beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a classic Tangled Rope. Labeling it a pure Snare would ignore its genuine
 *   coordination function for the IRS and full-time professionals. Labeling it a pure
 *   Rope would ignore the severe, targeted extraction imposed on the trapped group of
 *   hybrid investors. The framework correctly identifies it as a hybrid structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_26usc469,
    "Is the 750-hour threshold an objective proxy for \'material participation\', or was it a deliberately constructed regulatory moat to protect full-time real estate incumbents from competition by high-income professionals?",
    "Audit of legislative history and lobbyist influence (e.g., National Association of Realtors) in the 1993 Revenue Reconciliation Act.",
    "If proxy: It is a clumsy but functional Rope. If a moat: It is a predatory Snare.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_26usc469, empirical, "Distinguishing legislative intent (coordination) from protectionist effect (extraction) in the 750-hour rule.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval('26usc469_real_estate_exemption', 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness > 0.46).
% The law has been stable since 1993, so we model a flat trajectory.
%
% Theater ratio over time:
narrative_ontology:measurement(rep_exemption_tr_t0, 26usc469_real_estate_exemption, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rep_exemption_tr_t5, 26usc469_real_estate_exemption, theater_ratio, 5, 0.10).
narrative_ontology:measurement(rep_exemption_tr_t10, 26usc469_real_estate_exemption, theater_ratio, 10, 0.10).

% Extraction over time:
narrative_ontology:measurement(rep_exemption_ex_t0, 26usc469_real_estate_exemption, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(rep_exemption_ex_t5, 26usc469_real_estate_exemption, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(rep_exemption_ex_t10, 26usc469_real_estate_exemption, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The rule is an enforcement mechanism for distinguishing taxpayer classes.
narrative_ontology:coordination_type(26usc469_real_estate_exemption, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
