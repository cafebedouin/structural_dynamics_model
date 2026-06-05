% ============================================================================
% CONSTRAINT STORY: antifragility_u2_exp_r5
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u2_exp_r5, []).

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
 *   constraint_id: antifragility_u2_exp_r5
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that gain from disorder, stressors, and
 *   volatility. While it can be seen as a fundamental property of evolution
 *   (a Mountain), its application in human systems creates a stark
 *   perspectival gap. For the informed practitioner with agency, it is a tool
 *   for growth (a Rope). For the subject trapped within such a system, whose
 *   stability is sacrificed for the system's 'resilience', it is a highly
 *   extractive Snare.
 *
 * KEY AGENTS:
 *   - Optimized Serfs (Victim): Powerless individuals whose stability is consumed as 'fuel' for the system's antifragility.
 *   - Antifragile Practitioner (Beneficiary): Moderate-power individuals or firms who can structure their affairs to benefit from volatility.
 *   - Fragile Institutions (Victim): Legacy systems and their managers (bureaucrats) who bear the costs of volatility externalized by practitioners.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u2_exp_r5, 0.75).
domain_priors:suppression_score(antifragility_u2_exp_r5, 0.65).
domain_priors:theater_ratio(antifragility_u2_exp_r5, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u2_exp_r5, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u2_exp_r5, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u2_exp_r5, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u2_exp_r5, tangled_rope).
narrative_ontology:human_readable(antifragility_u2_exp_r5, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u2_exp_r5, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u2_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u2_exp_r5, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u2_exp_r5, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u2_exp_r5, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Experiences the downside of volatility (e.g., job loss from 'creative destruction') without access to the upside. The system extracts their stability for others' gain.
constraint_indexing:constraint_classification(antifragility_u2_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Uses antifragility as a strategy (a tool/Rope) to harvest upside from volatility while capping downside, often by operating in systems where risk is externalized.
constraint_indexing:constraint_classification(antifragility_u2_exp_r5, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Manages a system that claims to seek stability (coordination) but structurally creates fragility and externalizes risk (extraction), benefiting select actors.
constraint_indexing:constraint_classification(antifragility_u2_exp_r5, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% Views antifragility as a fundamental, unchangeable property of complex adaptive systems. Individual gains and losses are irrelevant at this scale.
constraint_indexing:constraint_classification(antifragility_u2_exp_r5, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u2_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u2_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u2_exp_r5, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u2_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u2_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (ε=0.75) represents the 'convexity bias' where beneficiaries capture disproportionate upside from positive shocks, while the downside from negative shocks is socialized or absorbed by the victims. Suppression (0.65) reflects the lack of alternative systems that prioritize individual stability over systemic 'learning from failure'.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The practitioner sees a Rope for navigating reality. The serf sees a Snare that sacrifices them for an abstract 'greater good'. The bureaucrat sees a Tangled Rope, a necessary but costly trade-off. The analyst sees a Mountain, an amoral law of nature. The classification depends entirely on whether one is harvesting the convexity or paying for it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the 'antifragile practitioners' who have arbitrage options and can position themselves to gain from chaos. Victims are the 'optimized serfs' and 'fragile institutions' who are trapped and must absorb the costs. This clear division of roles—one harvesting, one subsidizing—is what drives the directionality calculation and the high effective extraction (χ) for the powerless.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by classifying the constraint as a Tangled Rope from the institutional/analytical perspective. This correctly identifies that antifragility possesses a genuine coordination function (adapting the whole system to reality) but that this function is coupled with a severe, asymmetric extraction mechanism. A simple Snare classification would miss the adaptive function, while a Rope classification would ignore the immense cost paid by the fragile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the high extraction (0.75) an unavoidable functional necessity for evolutionary adaptation, or a predatory feature of its socio-economic implementation?',
    'Comparative analysis of systems with symmetric vs. asymmetric risk profiles. Quantifying the degree to which 'skin in the game' is enforced on beneficiaries.',
    'If it's a functional necessity, the constraint trends towards Mountain. If it's a predatory implementation, it's a Snare at its core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, conceptual, 'Whether the high extraction is a functional necessity or a predatory implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u2_exp_r5, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u2_exp_r5, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t5, antifragility_u2_exp_r5, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u2_exp_r5, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u2_exp_r5, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_be_t5, antifragility_u2_exp_r5, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(anti_be_t10, antifragility_u2_exp_r5, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
