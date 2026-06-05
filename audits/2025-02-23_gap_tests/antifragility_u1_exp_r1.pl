% ============================================================================
% CONSTRAINT STORY: antifragility_u1_exp_r1
% ============================================================================
% Version: 3.5 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u1_exp_r1, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: antifragility_u1_exp_r1
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that increase in capability, resilience,
 *   or robustness in response to stressors, shocks, and volatility. While
 *   this is a fundamental property of evolution (a Mountain), its application
 *   in social and economic systems creates a stark perspectival gap. For the
 *   informed practitioner who can structure their exposure, it is a tool for
 *   gain (a Rope). For the fragile subject whose stability is sacrificed for
 *   the system's resilience, it is a highly extractive trap (a Snare).
 *
 * KEY AGENTS:
 *   - The Optimized Serf: Primary target (powerless/trapped) - bears the externalized costs of volatility.
 *   - The Barbell Practitioner: Primary beneficiary (moderate/arbitrage) - harvests upside from volatility.
 *   - The Fragilista/Bureaucrat: Institutional enforcer (institutional/constrained) - attempts to manage risk, often creating the fragile conditions that enable extraction.
 *   - The Evolutionary Observer: Analytical observer (analytical/analytical) - sees the process as a natural law of selection.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u1_exp_r1, 0.75).
domain_priors:suppression_score(antifragility_u1_exp_r1, 0.65).
domain_priors:theater_ratio(antifragility_u1_exp_r1, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u1_exp_r1, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u1_exp_r1, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u1_exp_r1, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u1_exp_r1, tangled_rope).
narrative_ontology:human_readable(antifragility_u1_exp_r1, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u1_exp_r1, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u1_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u1_exp_r1, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u1_exp_r1, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u1_exp_r1, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: The target who bears the externalized costs of volatility and gains no upside. The system extracts resilience from them, making it a Snare.
constraint_indexing:constraint_classification(antifragility_u1_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: The beneficiary who structures their affairs to harvest upside from volatility while capping downside. For them, it is a pure coordination tool (Rope).
constraint_indexing:constraint_classification(antifragility_u1_exp_r1, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: The institutional actor attempting to manage risk, who sees both the system-stabilizing benefits and the extractive costs imposed on constituents.
constraint_indexing:constraint_classification(antifragility_u1_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: From a sufficiently long and abstract viewpoint, the process of gaining from disorder is a fundamental property of complex adaptive systems, appearing as a law of nature (Mountain).
constraint_indexing:constraint_classification(antifragility_u1_exp_r1, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u1_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u1_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u1_exp_r1, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u1_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u1_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high base extractiveness (0.75) reflects the 'convexity bias' where a beneficiary harvests unbounded upside while their downside is capped and externalized to others. The high suppression (0.65) represents the lack of alternatives for those trapped in fragile systems (e.g., employees with non-portable benefits, citizens under brittle governments) who are forced to absorb these externalized costs.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme: the beneficiary sees a tool for personal gain (Rope), the victim sees a trap (Snare), the manager sees a complex trade-off (Tangled Rope), and the abstract observer sees a law of nature (Mountain). This demonstrates how a single dynamic can be functionally different depending on one's structural position and exposure to its costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the clear beneficiary/victim structure. The 'antifragile_practitioner' is the beneficiary, gaining from the system's volatility with limited downside. The 'optimized_serfs' and 'fragile_institutions' are the victims, absorbing the uncompensated downside risk. This asymmetry drives the high effective extraction for the powerless and negative extraction (i.e., subsidy) for the practitioner.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by classifying the constraint as a Tangled Rope from the analytical perspective that acknowledges human agency. To label it a pure Mountain would be to naturalize the suffering of the fragile. To label it a pure Snare would be to ignore its genuine adaptive function at a systemic level. The Tangled Rope classification correctly identifies that it has both a coordination function (enabling systems to adapt) and a severe, asymmetric extraction mechanism that requires active enforcement through institutional structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the high extraction (0.75) an unavoidable functional necessity for system evolution, or is it a predatory feature enabled by the narrative?',
    'Comparative analysis of systems with and without 'skin in the game' mechanisms for the beneficiaries. If downside can be successfully re-internalized without system collapse, the extraction is predatory.',
    'If necessity: Mountain. If predatory: Snare. The current Tangled Rope classification reflects this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, empirical, 'Whether the high extraction is a functional necessity or predatory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u1_exp_r1, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u1_exp_r1, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t5, antifragility_u1_exp_r1, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u1_exp_r1, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u1_exp_r1, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_be_t5, antifragility_u1_exp_r1, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(anti_be_t10, antifragility_u1_exp_r1, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
