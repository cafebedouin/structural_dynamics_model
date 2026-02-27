% ============================================================================
% CONSTRAINT STORY: scam_compound_grey_zone_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scam_compound_grey_zone_2026, []).

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
 *   constraint_id: scam_compound_grey_zone_2026
 *   human_readable: The Southeast Asian Scam Compound "Grey Zone"
 *   domain: social/humanitarian/criminal
 *
 * SUMMARY:
 *   In fortified enclaves, often in regions with weak governance, the
 *   distinction between victims of modern slavery and criminal perpetrators
 *   has collapsed. These 'grey zone' scam compounds operate by trapping
 *   individuals and forcing them to perpetrate online scams. The victims are
 *   often trafficked individuals, while the beneficiaries are compound
 *   operators and corrupt officials. Law enforcement efforts are hampered by
 *   corruption and lack of resources.
 *
 * KEY AGENTS:
 *   - Trafficked Individuals: Primary victim (powerless/trapped) - bears the brunt of extraction and suppression
 *   - Scam Targets: Secondary victim (moderate/constrained) - subject to financial and psychological exploitation
 *   - Regional Law Enforcement: Institutional Actor (institutional/constrained) - nominally powerful but structurally constrained by corruption
 *   - Compound Operators: Primary beneficiary (institutional/arbitrage) - extracts profits from the scams
 *   - Corrupt Government Officials: Secondary beneficiary (powerful/constrained) - receives bribes and protection money
 *   - Analytical Observer: Civilizational view (analytical/analytical) - observes the systemic factors enabling the compound's existence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scam_compound_grey_zone_2026, 0.85).
domain_priors:suppression_score(scam_compound_grey_zone_2026, 0.9).
domain_priors:theater_ratio(scam_compound_grey_zone_2026, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scam_compound_grey_zone_2026, extractiveness, 0.85).
narrative_ontology:constraint_metric(scam_compound_grey_zone_2026, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(scam_compound_grey_zone_2026, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scam_compound_grey_zone_2026, snare).
narrative_ontology:human_readable(scam_compound_grey_zone_2026, "The Southeast Asian Scam Compound \"Grey Zone\"").
narrative_ontology:topic_domain(scam_compound_grey_zone_2026, "social/humanitarian/criminal").

domain_priors:requires_active_enforcement(scam_compound_grey_zone_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scam_compound_grey_zone_2026, compound_operators).
narrative_ontology:constraint_beneficiary(scam_compound_grey_zone_2026, corrupt_officials).
narrative_ontology:constraint_victim(scam_compound_grey_zone_2026, trafficked_individuals).
narrative_ontology:constraint_victim(scam_compound_grey_zone_2026, scam_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Trafficked individuals are trapped within the compound, forced to perpetrate scams, and have no realistic exit options. High extraction and suppression. They are the primary victims.
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Scam targets are victims of the scams perpetrated from within the compound. Their exit option is to refuse to send money, but once exploited are subject to blackmail and continued targeting. While not physically trapped, the psychological and financial constraints are substantial.
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% Regional law enforcement is nominally responsible for shutting down these compounds, but corruption and lack of resources limit their effectiveness. The law enforcement apparatus exists, but its functionality is degraded, making it a Piton. They are nominally powerful but structurally constrained.
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Compound operators benefit directly from the scams and have significant arbitrage opportunities due to weak governance and corruption. They experience the constraint as a coordination mechanism for illicit profit. They are the primary beneficiaries.
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Corrupt government officials benefit from bribes and protection rackets associated with the compounds. They face constraints from potential exposure or crackdowns but also benefit directly from the compound's existence. Tangled Rope captures their mixed position.
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% From a global, civilizational perspective, the scam compound represents a complex interplay of factors: weak governance, globalization of crime, and technological exploitation. The grey zone arises from the collapsing distinction between victim and perpetrator. Tangled Rope reflects this mixed nature.
constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scam_compound_grey_zone_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scam_compound_grey_zone_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scam_compound_grey_zone_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(scam_compound_grey_zone_2026, TR),
    TR >= 0.70.

:- end_tests(scam_compound_grey_zone_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85): Very high. The compounds extract significant wealth from scam targets and exploit trafficked individuals. Suppression (0.90): Very high. Trafficked individuals have virtually no freedom or autonomy. Theater ratio (0.75): High. The compounds engage in some level of performative activity to maintain the illusion of legitimacy and deter scrutiny, such as staged events or charitable donations.
 *
 * PERSPECTIVAL GAP:
 *   The trafficked individual experiences a Snare: trapped and exploited. Scam targets, while not physically imprisoned, face substantial constraints. Compound operators view the situation as a Rope, coordinating illicit activities. Corrupt officials see it as a Tangled Rope, balancing benefits and risks. Regional law enforcement exists, but is constrained by corruption or lack of resources: a Piton. The observer sees a complex interplay, captured by the Tangled Rope designation.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural relationships. Trafficked individuals are victims with no exit, hence d close to 1.0. Compound operators are beneficiaries with arbitrage, hence d close to 0.0. Other actors have intermediate values based on their power and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The claim is that the situation can be differentiated as such: the victims are forced and trapped, and the perpetrators are acting through exploitation and the compounds are set up for the extraction of illicit gains. Therefore, this should be considered a Snare as claimed. It can be resolved by assessing how the individuals are being exploited and whether these are forced and trapped. The high extractiveness is justified by the extreme exploitation of victims and the significant profits extracted by the operators. The mandatrophy is resolved by the clear distinction between the trapped victims and the exploiting perpetrators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_crackdown_effectiveness,
    'How effective are regional governance crackdowns at dismantling the compounds and preventing their re-establishment?',
    'Longitudinal tracking of compound dismantling efforts vs. re-establishment rates; analysis of factors contributing to success or failure',
    'If effective: the compounds are a temporary phenomenon. If ineffective: they are a persistent feature of the regional landscape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_crackdown_effectiveness, empirical, 'Effectiveness of governance crackdowns.').

omega_variable(
    victim_perpetrator_differentiation,
    'To what extent can trafficked individuals be rehabilitated and reintegrated into society after escaping the compounds?',
    'Long-term studies of rehabilitation programs; analysis of factors influencing successful reintegration',
    'If differentiable: rehabilitation efforts are worthwhile. If indistinct: the grey zone is intractable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_perpetrator_differentiation, conceptual, 'Differentiation between victim and perpetrator.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scam_compound_grey_zone_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scam_tr_t0, scam_compound_grey_zone_2026, theater_ratio, 0, 0.5).
narrative_ontology:measurement(scam_tr_t5, scam_compound_grey_zone_2026, theater_ratio, 5, 0.6).
narrative_ontology:measurement(scam_tr_t10, scam_compound_grey_zone_2026, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(scam_be_t0, scam_compound_grey_zone_2026, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(scam_be_t5, scam_compound_grey_zone_2026, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(scam_be_t10, scam_compound_grey_zone_2026, base_extractiveness, 10, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
