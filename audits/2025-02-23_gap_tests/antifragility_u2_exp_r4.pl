% ============================================================================
% CONSTRAINT STORY: antifragility_u2_exp_r4
% ============================================================================
% Version: 4.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u2_exp_r4, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: antifragility_u2_exp_r4
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that gain from disorder, stressors, and
 *   volatility. While this is a powerful mechanism for adaptation and
 *   survival at a systemic level, it operates by transferring risk and harm.
 *   The 'antifragile' part of the system benefits directly from the failure
 *   and fragility of other parts. This creates a stark perspectival gap
 *   between those who can position themselves to benefit and those who serve
 *   as the raw material for their gains.
 *
 * KEY AGENTS:
 *   - Antifragile Practitioner: The primary beneficiary, who uses volatility for gain (moderate/arbitrage).
 *   - Optimized Serfs: The primary victims, whose stability is sacrificed for system optionality (powerless/trapped).
 *   - Fragile Institutions: Institutional victims and enforcers, who must manage the fallout while being vulnerable themselves (institutional/constrained).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u2_exp_r4, 0.75).
domain_priors:suppression_score(antifragility_u2_exp_r4, 0.65).
domain_priors:theater_ratio(antifragility_u2_exp_r4, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u2_exp_r4, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u2_exp_r4, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u2_exp_r4, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u2_exp_r4, tangled_rope).
narrative_ontology:human_readable(antifragility_u2_exp_r4, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u2_exp_r4, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u2_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u2_exp_r4, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u2_exp_r4, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u2_exp_r4, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the individual whose job or livelihood is made precarious to create optionality for the system, the constraint is a pure Snare. They bear the costs of volatility without sharing in the gains.
constraint_indexing:constraint_classification(antifragility_u2_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the agent who can structure their affairs to harvest upside from volatility (the 'barbell strategy'), the principle is a pure coordination tool (Rope) for personal advantage.
constraint_indexing:constraint_classification(antifragility_u2_exp_r4, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% For the institution attempting to manage the system, it's a Tangled Rope. They must enforce the painful coordination that creates resilience, but also face the extractive consequences and political blowback.
constraint_indexing:constraint_classification(antifragility_u2_exp_r4, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view recognizes both the genuine coordination function (enabling long-term system survival) and the severe, asymmetric extraction required to achieve it. This matches the claimed type.
constraint_indexing:constraint_classification(antifragility_u2_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u2_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u2_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u2_exp_r4, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u2_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u2_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) represents the 'convexity bias'—the process by which beneficiaries capture unbounded upside from volatility while externalizing the downside to victims. The high suppression (0.65) reflects how such systems eliminate stable, 'fragile' alternatives, forcing participation in a volatile environment. The theater ratio (0.55) has increased as the concept became a consulting buzzword, often applied performatively without true skin-in-the-game.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The practitioner with arbitrage options sees a Rope, a tool for navigating reality. The trapped serf sees a Snare, a system designed to extract their security and well-being. The analyst sees a Tangled Rope, acknowledging the valid coordination function (system survival) is inextricably linked with a brutal extractive mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural positions. The 'antifragile_practitioner' is a declared beneficiary with arbitrage exit, yielding a low 'd' value and thus low or negative effective extraction (χ). The 'optimized_serfs' are declared victims with trapped exit, yielding a high 'd' and thus high χ. This difference in structure, not just belief, is what drives the different classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope resolves the mandatrophy of mislabeling a highly extractive system as a pure 'law of nature' (Mountain) or a benign tool (Rope). It correctly identifies that the system has a genuine coordination function (adaptation) but that this function is achieved through active, asymmetric extraction, which requires enforcement and creates victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_emergence,
    'Is the extraction from the fragile an intentional, predatory design by beneficiaries, or an unavoidable emergent property of complex adaptive systems?',
    'Comparative analysis of systems with and without 'skin-in-the-game' rules for beneficiaries. If insulating beneficiaries from downside is a consistent design pattern, it suggests predation.',
    'If predatory, the constraint is a pure Snare at the analytical level. If emergent and unavoidable, it approaches a Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_emergence, conceptual, 'Distinguishing between predatory extraction and emergent functional properties of antifragile systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u2_exp_r4, 2012, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t2012, antifragility_u2_exp_r4, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(anti_tr_t2018, antifragility_u2_exp_r4, theater_ratio, 2018, 0.35).
narrative_ontology:measurement(anti_tr_t2024, antifragility_u2_exp_r4, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t2012, antifragility_u2_exp_r4, base_extractiveness, 2012, 0.4).
narrative_ontology:measurement(anti_be_t2018, antifragility_u2_exp_r4, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(anti_be_t2024, antifragility_u2_exp_r4, base_extractiveness, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility_u2_exp_r4, resource_allocation).
narrative_ontology:affects_constraint(antifragility_u2_exp_r4, financialization).
narrative_ontology:affects_constraint(antifragility_u2_exp_r4, platform_capitalism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
