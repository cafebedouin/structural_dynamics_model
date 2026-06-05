% ============================================================================
% CONSTRAINT STORY: coe_ukraine_reparations_register
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coe_ukraine_reparations_register, []).

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
 *   constraint_id: coe_ukraine_reparations_register
 *   human_readable: Council of Europe's Register of Damage for Ukraine
 *   domain: geopolitical/legal
 *
 * SUMMARY:
 *   The Council of Europe's Register of Damage for Ukraine aims to record
 *   claims of loss and injury resulting from Russia's invasion. It serves as
 *   a coordination mechanism for documenting damages and a potential basis
 *   for future reparations. However, its effectiveness depends on overcoming
 *   challenges related to claim validation, enforcement, and geopolitical
 *   considerations. The register involves various actors, including the
 *   Council of Europe, the Ukrainian government, affected civilians, and the
 *   Russian Federation, each with their own perspectives and interests.
 *
 * KEY AGENTS:
 *   - Council of Europe: Primary coordinator (institutional/arbitrage) - benefits from promoting international law and accountability.
 *   - Ukrainian Civilians: Primary victims (powerless/trapped) - bear the costs of war and face challenges in accessing compensation.
 *   - Ukraine Government: Secondary beneficiary (moderate/constrained) - benefits from documenting damages and seeking reparations, but faces administrative and political burdens.
 *   - Russian Federation: Target (powerless/trapped) - bears the costs of reputational damage and potential financial liabilities.
 *   - Claimants: beneficiaries (individuals), but also face hurdles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coe_ukraine_reparations_register, 0.55).
domain_priors:suppression_score(coe_ukraine_reparations_register, 0.4).
domain_priors:theater_ratio(coe_ukraine_reparations_register, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coe_ukraine_reparations_register, extractiveness, 0.55).
narrative_ontology:constraint_metric(coe_ukraine_reparations_register, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(coe_ukraine_reparations_register, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coe_ukraine_reparations_register, tangled_rope).
narrative_ontology:human_readable(coe_ukraine_reparations_register, "Council of Europe's Register of Damage for Ukraine").
narrative_ontology:topic_domain(coe_ukraine_reparations_register, "geopolitical/legal").

domain_priors:requires_active_enforcement(coe_ukraine_reparations_register).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coe_ukraine_reparations_register, council_of_europe).
narrative_ontology:constraint_beneficiary(coe_ukraine_reparations_register, ukraine_government).
narrative_ontology:constraint_beneficiary(coe_ukraine_reparations_register, claimants).
narrative_ontology:constraint_victim(coe_ukraine_reparations_register, russian_federation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Ukrainian civilians who have suffered losses due to the war. They are trapped in the system and bear the costs of bureaucratic hurdles, potential delays in compensation, and the risk of inadequate redress.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% The Ukrainian government benefits from the register as a tool for documenting damages and seeking reparations, but is constrained by the ongoing conflict and the need to balance competing priorities. It faces extraction in the form of administrative burdens and political pressure to ensure fair and timely compensation.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The Council of Europe benefits from the register as a means of asserting its role in upholding international law and promoting accountability. It experiences the constraint as a coordination mechanism, facilitating the collection and documentation of damages.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% The Russian Federation is the target of the register, as it is intended to serve as a basis for future reparations claims. It bears the costs of reputational damage and the potential for financial liabilities. It is essentially trapped, facing international legal and political pressure.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% An analytical observer sees the register as a mixed mechanism. It coordinates the documentation of war damages but also extracts from Russia through the potential for future reparations claims. The effectiveness of the register in achieving justice and accountability remains uncertain.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coe_ukraine_reparations_register_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coe_ukraine_reparations_register, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coe_ukraine_reparations_register, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coe_ukraine_reparations_register, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coe_ukraine_reparations_register_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The register extracts from Russia by documenting damages and potentially paving the way for future reparations. The actual amount will depend on many factors. Suppression (0.40): Moderate. There are barriers to entry (documentation requirements, legal processes, access to the register). Claimants may have issues of language or digital access. Theater ratio (0.30): Low. The register is primarily functional, focused on collecting and documenting evidence of damages. The performative element is relatively low, as it is not intended to be a public relations exercise but rather a legal tool.
 *
 * PERSPECTIVAL GAP:
 *   The Ukrainian civilians see the register as a snare, as they are trapped and facing administrative difficulties. The Ukrainian government sees it as a tangled rope, as they face both opportunities and administrative burdens. The Council of Europe sees it as a rope, as they are coordinating the process. The Analytical observer sees the register as a tangled rope, as the practical enforcement is uncertain.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the relationship to the extraction flow. Ukrainian civilians are primary victims, facing loss and difficulty in accessing compensation. The Ukranian Government and Council of Europe are beneficiaries. The Russian Federation is the target of extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The register is classified as a tangled rope due to its mixed nature. It coordinates the documentation of war damages, but also extracts from the Russian Federation and places burdens on victims. Distinguishing it from a pure snare or a pure rope requires considering the various perspectives and the complexities of the geopolitical context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reparation_enforcement,
    'How will reparations be enforced against Russia, given its potential non-cooperation?',
    'Development of international legal mechanisms for asset seizure and compensation funds.',
    'Without effective enforcement, the register becomes a symbolic gesture with limited practical impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reparation_enforcement, empirical, 'Enforcement mechanisms for reparations').

omega_variable(
    claim_validation_integrity,
    'How can the register ensure the integrity and validity of claims, preventing fraudulent or exaggerated submissions?',
    'Establishment of rigorous verification procedures and independent oversight mechanisms.',
    'Compromised claim validation undermines the legitimacy and credibility of the entire process.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(claim_validation_integrity, empirical, 'Integrity of claim validation').

omega_variable(
    geopolitical_feasibility,
    'To what extent can the process continue given ongoing geopolitical tensions, and the need to navigate the interests of all involved parties?',
    'Political negotiation and diplomacy among key stakeholders.',
    'Geopolitical factors could enable or restrict the success of the register.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_feasibility, conceptual, 'Geopolitical feasibility of register').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coe_ukraine_reparations_register, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coe__tr_t0, coe_ukraine_reparations_register, theater_ratio, 0, 0.2).
narrative_ontology:measurement(coe__tr_t5, coe_ukraine_reparations_register, theater_ratio, 5, 0.3).
narrative_ontology:measurement(coe__tr_t10, coe_ukraine_reparations_register, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(coe__be_t0, coe_ukraine_reparations_register, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(coe__be_t5, coe_ukraine_reparations_register, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(coe__be_t10, coe_ukraine_reparations_register, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coe_ukraine_reparations_register, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
