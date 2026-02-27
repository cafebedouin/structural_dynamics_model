% ============================================================================
% CONSTRAINT STORY: dk_us_alliance_espionage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dk_us_alliance_espionage, []).

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
 *   constraint_id: dk_us_alliance_espionage
 *   human_readable: Implicit 'No Industrial Espionage' Norm within Western Alliances
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint models the implicit norm within NATO that member states,
 *   particularly the hegemon (USA), will not conduct industrial espionage
 *   against allied nations (e.g., Denmark). While the alliance is intended
 *   for mutual defense and security, the power asymmetry creates the
 *   potential for the US to exploit its position for economic or
 *   technological advantage through espionage. This extraction is typically
 *   suppressed to maintain alliance cohesion, but it remains a structural
 *   feature of the relationship.
 *
 * KEY AGENTS:
 *   - US Intelligence Agencies: Primary beneficiary (institutional/arbitrage) - benefits from access and potential exploitation.
 *   - Danish Private Sector: Primary victim (powerless/trapped) - target of potential espionage with limited recourse.
 *   - Danish Government: Secondary victim (moderate/constrained) - constrained in ability to openly challenge US, given power imbalance.
 *   - Analytical Observer: Sees the structural implications of the power imbalance (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dk_us_alliance_espionage, 0.45).
domain_priors:suppression_score(dk_us_alliance_espionage, 0.6).
domain_priors:theater_ratio(dk_us_alliance_espionage, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dk_us_alliance_espionage, extractiveness, 0.45).
narrative_ontology:constraint_metric(dk_us_alliance_espionage, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(dk_us_alliance_espionage, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dk_us_alliance_espionage, tangled_rope).
narrative_ontology:human_readable(dk_us_alliance_espionage, "Implicit 'No Industrial Espionage' Norm within Western Alliances").
narrative_ontology:topic_domain(dk_us_alliance_espionage, "geopolitical").

domain_priors:requires_active_enforcement(dk_us_alliance_espionage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dk_us_alliance_espionage, us_intelligence_agencies).
narrative_ontology:constraint_victim(dk_us_alliance_espionage, danish_private_sector).
narrative_ontology:constraint_victim(dk_us_alliance_espionage, danish_government).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Danish private sector experiences the norm as a snare because they are the target of potential espionage and have limited recourse against a more powerful ally. They are largely 'trapped' within the alliance structure.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: The Danish government experiences a tangled rope. They benefit from the overall alliance security but are constrained in their ability to openly challenge or retaliate against potential US espionage, given the power imbalance and reliance on the US for defense. Significant extraction, constrained exit.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: US intelligence agencies benefit from the norm because it allows them to focus resources on external threats while maintaining a degree of access and influence within allied nations. They can strategically 'arbitrage' the norm when necessary.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: An analytical observer sees the implicit norm as a tangled rope because it provides coordination (alliance cohesion) alongside asymmetric extraction (potential industrial espionage). The US benefits more than smaller allies, but the alliance provides collective security benefits. The analytical observer can 'exit' by analyzing the structure and reporting their findings.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dk_us_alliance_espionage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dk_us_alliance_espionage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dk_us_alliance_espionage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(dk_us_alliance_espionage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): Moderate. The potential for espionage exists, but it's not a constant drain. It's more of an opportunistic extraction that can occur depending on strategic priorities. Suppression (0.60): High. Openly accusing the US of espionage would severely damage diplomatic relations and alliance cohesion, leading to significant consequences. Therefore, violations are often downplayed or addressed through private channels. Theater Ratio (0.30): Low. The activity is not overtly theatrical, but there is a performance of trust and alliance solidarity that masks the underlying potential for extraction.
 *
 * PERSPECTIVAL GAP:
 *   The Danish private sector experiences the norm as a snare because they are the target and have limited power to resist. The Danish government experiences a tangled rope; they benefit from the alliance but are constrained by the power dynamics. The US intelligence agencies see the norm as a rope, allowing them to operate within certain boundaries while maintaining access and influence. The analytical observer sees the broader structure as a tangled rope, acknowledging both the coordination and extraction aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's position within the alliance structure. US intelligence agencies, as the potential extractors with more power and exit options, experience a low d value. The Danish private sector, as the potential targets with fewer exit options, experience a high d value. The Danish government's d value is moderate due to their constrained position.
 *
 * MANDATROPHY ANALYSIS:
 *   The analysis avoids mislabeling coordination as pure extraction by recognizing the alliance provides genuine collective security benefits that are highly valued by the Danish government. This coordination function explains why the Danish government would not classify as snare despite the extraction. The analysis avoids mislabeling extraction as pure coordination by recognizing the power asymmetry and the potential for the US to exploit this imbalance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_threshold,
    'At what level of espionage activity does the violation of the norm trigger a significant response from the victim nation?',
    'Historical analysis of espionage cases and alliance responses; correlation between the severity of espionage and diplomatic/economic repercussions.',
    'If the threshold is low, the norm is strongly enforced, promoting trust. If the threshold is high, the norm is weak, leading to mistrust and potential alliance fracturing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_threshold, empirical, 'Level of espionage activity required to trigger a significant response.').

omega_variable(
    alternative_information_sources,
    'To what extent do US intelligence agencies rely on espionage against allies compared to other sources of information (e.g., open-source intelligence, diplomatic channels)?',
    'Declassified intelligence reports; comparative analysis of information sources used in specific intelligence assessments; interviews with intelligence officials.',
    'If espionage is a primary source, the extraction is high. If espionage is a secondary source, the extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_information_sources, empirical, 'Relative reliance on espionage compared to other information sources.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dk_us_alliance_espionage, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dk_u_tr_t0, dk_us_alliance_espionage, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dk_u_tr_t10, dk_us_alliance_espionage, theater_ratio, 10, 0.3).
narrative_ontology:measurement(dk_u_tr_t20, dk_us_alliance_espionage, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(dk_u_be_t0, dk_us_alliance_espionage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dk_u_be_t10, dk_us_alliance_espionage, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(dk_u_be_t20, dk_us_alliance_espionage, base_extractiveness, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dk_us_alliance_espionage, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
