% ============================================================================
% CONSTRAINT STORY: fatf_grey_list_russia
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-25
% ============================================================================

:- module(constraint_fatf_grey_list_russia, []).

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
 *   constraint_id: fatf_grey_list_russia
 *   human_readable: FATF/EU 'Grey List' Sanction on the Russian Federation
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The Financial Action Task Force (FATF) maintains a "grey list" of
 *   countries with strategic deficiencies in their regimes to counter money
 *   laundering, terrorist financing, and proliferation financing (AML/CFT/CPF).
 *   This constraint models the EU's move to place the Russian Federation on its
 *   own high-risk list, which triggers enhanced due diligence and sanctions.
 *   While framed as a technical compliance issue (coordination), it functions
 *   as a powerful geopolitical tool for economic extraction and pressure.
 *
 * KEY AGENTS (by structural relationship):
 *   - Russian Federation Financial Sector: Primary target (institutional/trapped) — bears the costs of increased scrutiny, transactional friction, and reputational damage.
 *   - EU and FATF Member States: Primary beneficiary (institutional/arbitrage) — enforces global financial norms, exerts political pressure, and reduces perceived risk within their own systems.
 *   - Russian Citizens & Small Businesses: Secondary target (powerless/trapped) - bear the downstream costs of financial friction with no agency.
 *   - Analytical Observer: Sees the dual function as both a coordination mechanism and an extractive geopolitical weapon.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fatf_grey_list_russia, 0.65).
domain_priors:suppression_score(fatf_grey_list_russia, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(fatf_grey_list_russia, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fatf_grey_list_russia, extractiveness, 0.65).
narrative_ontology:constraint_metric(fatf_grey_list_russia, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(fatf_grey_list_russia, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fatf_grey_list_russia, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(fatf_grey_list_russia). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fatf_grey_list_russia, eu_and_fatf_member_states).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fatf_grey_list_russia, russian_federation_financial_sector).

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

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% This is a classic inter-institutional case where two state-level actors
% experience the same mechanism in diametrically opposed ways.

% PERSPECTIVE 1: THE PRIMARY TARGET (The Russian Federation)
% As the victim of the constraint with no viable exit from the global financial
% system, Russia experiences this as a highly coercive and extractive mechanism.
% Engine derives d from: victim + institutional + trapped -> high d -> high χ
constraint_indexing:constraint_classification(fatf_grey_list_russia, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (The European Union / FATF)
% As the enforcer and beneficiary, the EU views this as a low-cost tool for
% coordinating global financial policy and achieving geopolitical goals.
% Engine derives d from: beneficiary + institutional + arbitrage -> low d -> negative χ
constraint_indexing:constraint_classification(fatf_grey_list_russia, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical view recognizes both the genuine coordination function (AML
% standards are real) and the massive asymmetric extraction. This combination,
% along with active enforcement, defines a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. With global scope σ=1.2,
% χ = 0.65 * 1.15 * 1.2 ≈ 0.90, classifying it as Tangled Rope.
constraint_indexing:constraint_classification(fatf_grey_list_russia, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INDIVIDUAL ACTOR (SNARE)
% A powerless agent within the target nation (e.g., a small business owner or
% citizen) experiences the downstream effects of financial friction and
% isolation without any agency.
% Engine derives d from: powerless + trapped -> d ≈ 1.0 -> f(d) ≈ 1.42 -> high χ
constraint_indexing:constraint_classification(fatf_grey_list_russia, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fatf_grey_list_russia_tests).

test(inter_institutional_gap, [nondet]) :-
    % Verify the gap between the two institutional actors.
    constraint_indexing:constraint_classification(fatf_grey_list_russia, snare,
        context(agent_power(institutional), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(fatf_grey_list_russia, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(powerless_perspective_is_snare) :-
    constraint_indexing:constraint_classification(fatf_grey_list_russia, snare,
        context(agent_power(powerless), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(fatf_grey_list_russia, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % A Tangled Rope requires all three: beneficiary, victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(fatf_grey_list_russia, _),
    narrative_ontology:constraint_victim(fatf_grey_list_russia, _),
    domain_priors:requires_active_enforcement(fatf_grey_list_russia).

:- end_tests(fatf_grey_list_russia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): High. The primary effect on the target is not
 *     improved compliance but significant economic friction, reputational
 *     damage, and isolation from capital markets. The value extracted is the
 *     economic and political leverage gained by the enforcer.
 *   - Suppression (0.80): Very high. The global financial system, centered on
 *     the US dollar and euro and governed by bodies like FATF and SWIFT, has no
 *     viable, scaled alternative. Russia is attempting to build alternatives
 *     (e.g., with BRICS), but exit remains prohibitively costly.
 *   - Theater (0.20): Low. The consequences of being listed are concrete and
 *     severe, involving real changes in banking procedures and risk assessment.
 *     This is not a performative gesture.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal. The EU (beneficiary) sees a Rope: a legitimate
 *   instrument for enforcing globally-agreed standards for financial health.
 *   Russia (target) sees a Snare: a politically motivated weapon using
 *   bureaucratic mechanisms to inflict economic harm, with compliance standards
 *   as a pretext. The true structure is a Tangled Rope, as it possesses both
 *   a genuine coordination function (AML/CFT standards are valuable) and is being
 *   used for massive, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction is unambiguous. Costs are imposed on the
 *   `russian_federation_financial_sector`. Benefits (political leverage,
 *   perceived financial system integrity) accrue to the `eu_and_fatf_member_states`.
 *   This clear beneficiary/victim structure is the primary input for the
 *   directionality derivation, creating the large `d` gap between the two
 *   institutional perspectives.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a textbook example of inter-institutional constraint dynamics.
 *   Two powerful, institutional actors (EU, Russia) interact via a single
 *   regulatory mechanism. Their classification differs not because of their power
 *   level (both are `institutional`) but because of their structural relationship
 *   to the constraint and their exit options. The EU has `arbitrage` exit (it
 *   controls the rules), while Russia has `trapped` exit (it cannot easily leave
 *   the system being policed).
 *
 * MANDATROPHY ANALYSIS:
 *   A simplistic analysis would label this a pure Snare (geopolitical weapon) or a
 *   pure Rope (technical standard). The Deferential Realism framework, by
*    identifying it as a Tangled Rope, correctly captures the dual nature that makes
 *   it so effective and difficult to counter: its extractive function is masked by
 *   a legitimate, and even necessary, coordination function. This prevents
 *   mischaracterization and focuses analysis on the *tension* between its two roles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_fatf_grey_list_russia,
    'Is the primary driver for this listing a genuine assessment of AML/CFT risk, or is it predominantly a tool of geopolitical coercion?',
    'Analysis of confidential FATF mutual evaluation reports compared against the listing decisions for other non-Western countries. If Russia is held to a demonstrably different standard, the coercion motive is stronger.',
    'If risk-based (Rope-like), the constraint might be resolved through technical compliance. If coercion-based (Snare-like), it can only be resolved through a shift in the geopolitical balance of power.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_fatf_grey_list_russia, empirical, 'Whether the listing is driven by technical risk assessment versus geopolitical coercion, resolvable by comparative analysis of evaluation reports.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fatf_grey_list_russia, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has high extraction (ε=0.65 > 0.46), so temporal data is required.
% The FATF framework evolved from a pure coordination mechanism (low ε) to an
% instrument of geopolitical power (high ε). This progression models that drift.
%
% T=0: Early FATF (c. 2000) - focus on technical standards (Rope-like)
% T=5: Post 9/11, post-2014 - increased use as a political tool (Tangled Rope)
% T=10: Current - Application as a major sanction (Snare-like application)

% Theater ratio over time (remains low as consequences are always real)
narrative_ontology:measurement(fatf_grey_list_russia_tr_t0, fatf_grey_list_russia, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fatf_grey_list_russia_tr_t5, fatf_grey_list_russia, theater_ratio, 5, 0.20).
narrative_ontology:measurement(fatf_grey_list_russia_tr_t10, fatf_grey_list_russia, theater_ratio, 10, 0.20).

% Extraction over time (shows drift from coordination to extraction)
narrative_ontology:measurement(fatf_grey_list_russia_ex_t0, fatf_grey_list_russia, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fatf_grey_list_russia_ex_t5, fatf_grey_list_russia, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(fatf_grey_list_russia_ex_t10, fatf_grey_list_russia, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a classic example of an enforcement mechanism.
narrative_ontology:coordination_type(fatf_grey_list_russia, enforcement_mechanism).

% Network relationships (structural influence edges)
% Being placed on this list directly impacts access to other parts of the
% global financial system, such as SWIFT.
narrative_ontology:affects_constraint(fatf_grey_list_russia, global_swift_access).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation chain,
% using the declared beneficiary/victim groups and the distinct exit options
% for the institutional actors (`trapped` vs. `arbitrage`), correctly models
% the directionality and produces the observed perspectival gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */