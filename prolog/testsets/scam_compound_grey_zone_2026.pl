% ============================================================================
% CONSTRAINT STORY: scam_compound_grey_zone_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_scam_compound_2026, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: scam_compound_2026
 *   human_readable: The Southeast Asian Scam Compound "Grey Zone"
 *   domain: social/humanitarian/criminal
 *
 * SUMMARY:
 *   In fortified enclaves, often in regions with weak governance, the
 *   distinction between victims of modern slavery and criminal perpetrators
 *   has collapsed. Workers are trafficked under false pretenses (e.g., high-paying
 *   tech jobs) but are then compelled through debt bondage, violence, and
 *   coercion to perpetrate online scams. This creates a "victim-offender
 *   overlap" that defies standard legal "Ideal Victim" categories, complicating
 *   law enforcement and humanitarian intervention.
 *
 * KEY AGENTS (by structural relationship):
 *   - Trafficked Workers: Primary target (powerless/trapped) — bear the direct extraction and are forced into criminality.
 *   - Organized Crime Syndicates: Primary beneficiary (institutional/arbitrage) — profit from both the forced labor and the scams.
 *   - Regional Law Enforcement: Analytical observer (analytical/analytical) — struggle to classify the situation, often treating it as a standard criminal enterprise rather than a mass human trafficking crisis.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scam_compound_2026, 0.85).
domain_priors:suppression_score(scam_compound_2026, 0.90).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(scam_compound_2026, 0.65).       % High, but not >0.7; function is primary.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scam_compound_2026, extractiveness, 0.85).
narrative_ontology:constraint_metric(scam_compound_2026, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(scam_compound_2026, theater_ratio, 0.65).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(scam_compound_2026, snare).
narrative_ontology:human_readable(scam_compound_2026, "The Southeast Asian Scam Compound \"Grey Zone\"").

% --- Binary flags ---
domain_priors:requires_active_enforcement(scam_compound_2026). % Physical confinement, violence.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(scam_compound_2026, organized_crime_syndicates).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(scam_compound_2026, trafficked_migrant_labor).

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

% PERSPECTIVE 1: THE TRAFFICKED WORKER (SNARE)
% Victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% The worker experiences total coercion, extraction of labor, and forced
% participation in criminal activity with no viable exit.
constraint_indexing:constraint_classification(scam_compound_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE SYNDICATE OPERATOR (ROPE)
% Beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% From the operator's view, this is a highly efficient (if illicit) coordination
% mechanism for labor and capital, generating immense profit.
constraint_indexing:constraint_classification(scam_compound_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The analytical view pierces the coordination narrative and sees the
% structure for what it is: a system of pure extraction based on coercion
% and the suppression of all alternatives for its victims.
constraint_indexing:constraint_classification(scam_compound_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scam_compound_2026_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(scam_compound_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scam_compound_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == snare,
    TypeBeneficiary == rope,
    TypeTarget \= TypeBeneficiary.

test(analytical_classification_is_snare) :-
    constraint_indexing:constraint_classification(scam_compound_2026, snare, context(agent_power(analytical), _, _, _)).

:- end_tests(scam_compound_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.85): Extremely high. The system extracts not only
 *     labor value but also forces victims to generate further revenue through
 *     scams, effectively capturing 100% of their economic output under duress.
 *   - Suppression Score (0.90): Near-total. Physical confinement, violence,
 *     debt bondage, and the threat of legal jeopardy (due to forced criminality)
 *     eliminate any viable alternatives for the victims.
 *   - Theater Ratio (0.65): The initial recruitment relies on deception (theater),
 *     but the day-to-day operation is brutally functional. The system is not
 *     primarily performative; its purpose is raw extraction. This value is
 *     below the 0.70 threshold for a Piton, correctly reflecting its active,
 *     functional nature.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the trafficked worker, it is an inescapable Snare,
 *   a system of total coercion. For the syndicate operator, it is a Rope—a
 *   highly effective, albeit illegal, method of coordinating resources (labor)
 *   to achieve a goal (profit). The operator does not perceive extraction
 *   because they are the sole beneficiary.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `organized_crime_syndicates`. They are the architects and
 *     sole financial beneficiaries of the system.
 *   - Victim: `trafficked_migrant_labor`. They bear the full cost: loss of
 *     freedom, forced labor, psychological trauma, and implication in crimes.
 *   This clear division drives the directionality calculation, leading to
 *   maximum positive χ for victims and negative χ for beneficiaries.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   A naive analysis might focus on the "work" being done (running scams) and
 *   misclassify this as a form of coercive employment. The Deferential Realism
 *   framework prevents this by centering the analysis on structural coercion
 *   and extraction. The suppression score of 0.90 and the `trapped` exit option
 *   for victims make it impossible to classify this as anything but a Snare from
 *   their perspective. The system correctly identifies that any "coordination"
 *   function exists purely to service the extraction, which is the defining
 *   feature of a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_scam_compound_2026,
    'To what extent are local/regional state actors complicit versus merely negligent?',
    'Leaked documents, whistleblower testimony from law enforcement, or forensic accounting of protection payments.',
    'If complicity is high, the constraint is a Tangled Rope involving state actors as beneficiaries. If it is negligence, it remains a Snare enabled by state failure.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(scam_compound_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This model tracks the intensification of the scam compound model from its
% origins in less coercive online gambling operations to the current model of
% mass trafficking and forced criminality.
%
% Theater ratio over time:
narrative_ontology:measurement(scam_compound_2026_tr_t0, scam_compound_2026, theater_ratio, 0, 0.75).
narrative_ontology:measurement(scam_compound_2026_tr_t5, scam_compound_2026, theater_ratio, 5, 0.70).
narrative_ontology:measurement(scam_compound_2026_tr_t10, scam_compound_2026, theater_ratio, 10, 0.65).

% Extraction over time:
narrative_ontology:measurement(scam_compound_2026_ex_t0, scam_compound_2026, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(scam_compound_2026_ex_t5, scam_compound_2026, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(scam_compound_2026_ex_t10, scam_compound_2026, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No specific coordination type or network relationships declared for this model.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately reflects the power dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */