% ============================================================================
% CONSTRAINT STORY: india_nuclear_liability_act_2010
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_india_nuclear_liability_act_2010, []).

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
 *   constraint_id: india_nuclear_liability_act_2010
 *   human_readable: India's Civil Liability for Nuclear Damage Act of 2010
 *   domain: economic/political
 *
 * SUMMARY:
 *   India's Civil Liability for Nuclear Damage Act of 2010 establishes the
 *   legal framework for compensating victims in the event of a nuclear
 *   accident. The act balances the need for nuclear energy development with
 *   the protection of citizens, but its provisions on supplier liability and
 *   compensation caps have been controversial.
 *
 * KEY AGENTS:
 *   - Accident Victims and Local Communities: Primary targets (powerless/trapped) - bear the brunt of a nuclear accident with limited exit options.
 *   - Nuclear Plant Operators: Primary beneficiaries (institutional/arbitrage) - benefit from liability limitations and risk transfer.
 *   - Foreign Suppliers: (institutional/constrained) - are partially shielded from direct liability.
 *   - The Indian Government: regulator (institutional/constrained)
 *   - Analytical Observer: Evaluates the Act's overall impact and effectiveness (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_nuclear_liability_act_2010, 0.55).
domain_priors:suppression_score(india_nuclear_liability_act_2010, 0.45).
domain_priors:theater_ratio(india_nuclear_liability_act_2010, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_nuclear_liability_act_2010, extractiveness, 0.55).
narrative_ontology:constraint_metric(india_nuclear_liability_act_2010, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(india_nuclear_liability_act_2010, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(india_nuclear_liability_act_2010, tangled_rope).
narrative_ontology:human_readable(india_nuclear_liability_act_2010, "India's Civil Liability for Nuclear Damage Act of 2010").
narrative_ontology:topic_domain(india_nuclear_liability_act_2010, "economic/political").

domain_priors:requires_active_enforcement(india_nuclear_liability_act_2010).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(india_nuclear_liability_act_2010, nuclear_plant_operators).
narrative_ontology:constraint_beneficiary(india_nuclear_liability_act_2010, foreign_suppliers).
narrative_ontology:constraint_victim(india_nuclear_liability_act_2010, accident_victims).
narrative_ontology:constraint_victim(india_nuclear_liability_act_2010, local_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of accident victims: Limited recourse and compensation due to liability caps and legal complexities. Trapped due to displacement and health impacts.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of nuclear plant operators: Benefits from limited liability and risk transfer to the government and insurance pools, enabling investment and operation. Arbitrage due to risk-mitigation mechanisms.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The government is constrained by the need to ensure nuclear safety while also fostering energy security and international collaboration.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The act represents a complex balance between promoting nuclear energy and protecting citizens, with inherent risks and uncertainties.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_nuclear_liability_act_2010_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(india_nuclear_liability_act_2010, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(india_nuclear_liability_act_2010_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate to high. The act limits the liability of nuclear operators and suppliers, transferring some of the risk to the government and the public. Suppression (0.45): Moderate. The Act's legal complexities and compensation caps limit the ability of victims to obtain full redress. Theater Ratio (0.30): Low. The act is primarily functional, with some element of demonstrating commitment to international norms.
 *
 * PERSPECTIVAL GAP:
 *   Accident victims experience the act as a snare due to limited compensation and legal recourse. Nuclear plant operators view it as a rope, enabling investment and operation with reduced risk. The government is constrained by the need to balance energy security and public safety. The analytical observer sees a tangled rope with inherent risks and uncertainties.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims are trapped and bear the costs (high directionality). Operators benefit from limited liability (low directionality). The government's directionality is intermediate due to its dual role as regulator and promoter of nuclear energy. The act requires active enforcement to maintain this balance.
 *
 * MANDATROPHY ANALYSIS:
 *   The act is classified as a tangled rope to reflect its dual nature: facilitating nuclear energy development while also providing a framework for victim compensation. Misclassifying it as a pure snare would ignore the benefits of nuclear energy and the compensation mechanisms in place. Misclassifying it as a pure rope would ignore the limitations on liability and the potential for inadequate compensation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extent_of_supplier_liability,
    'To what extent can foreign suppliers be held liable for defects or negligence?',
    'Legal interpretation and precedent setting through court cases.',
    'Greater supplier liability would shift the constraint towards a rope for victims and a snare for suppliers. Limited liability favors operators and disfavors victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_supplier_liability, conceptual, 'The scope of liability for foreign suppliers remains a contested issue.').

omega_variable(
    adequacy_of_compensation,
    'Is the compensation provided under the act adequate to cover the long-term impacts of a nuclear accident?',
    'Socio-economic impact assessments and comparisons with international standards.',
    'If compensation is inadequate, the constraint remains a snare for victims. If adequate, it shifts towards a tangled rope with a more balanced distribution of costs and benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_of_compensation, empirical, 'The adequacy of compensation for victims is a critical uncertainty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(india_nuclear_liability_act_2010, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indi_tr_t0, india_nuclear_liability_act_2010, theater_ratio, 0, 0.2).
narrative_ontology:measurement(indi_tr_t5, india_nuclear_liability_act_2010, theater_ratio, 5, 0.3).
narrative_ontology:measurement(indi_tr_t10, india_nuclear_liability_act_2010, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(indi_be_t0, india_nuclear_liability_act_2010, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(indi_be_t5, india_nuclear_liability_act_2010, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(indi_be_t10, india_nuclear_liability_act_2010, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(india_nuclear_liability_act_2010, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
