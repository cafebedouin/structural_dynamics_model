% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause (Originalist Narrow Reading)
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   This constraint instantiates the originalist narrow reading of the
 *   Commerce Clause: federal regulatory authority extends only to
 *   transactions and instrumentalities that cross state borders or directly
 *   facilitate interstate movement of goods and services. Intrastate economic
 *   activity, production, and regulation remain under state police power,
 *   even when those intrastate activities have aggregate effects on
 *   interstate commerce. This reading is one of three structurally distinct
 *   constraints arising from the contested Commerce Clause kernel. The
 *   originalist narrow reading benefits state governments (preserving
 *   autonomy) and federalism advocates (preventing federal consolidation)
 *   while victimizing national coordination bodies, environmental regulators,
 *   and interstate commerce standardizers (who lose federal authority to
 *   regulate intrastate sources of externalities or fragmentation). The
 *   constraint persists through ongoing constitutional litigation and
 *   interpretive authority wielded by originalist jurists, not through
 *   organic coordination benefit.
 *
 * KEY AGENTS:
 *   - state_governments: primary agenda-setters; control police power over intrastate activity; defend the narrow reading through litigation and legislative advocacy
 *   - originalist_judges: interpretive authority; apply the narrow reading through constitutional reasoning and statutory construction
 *   - federalism_advocates: beneficiaries; argue the reading preserves constitutional design and prevents federal overreach
 *   - environmental_regulators: primary victims; lose federal commerce-clause authority to regulate intrastate pollution sources and require uniform standards
 *   - interstate_commerce_standardizers: victims; cannot justify uniform national standards on commerce grounds; face state fragmentation and higher compliance costs
 *   - national_coordination_bodies: victims; federal administrative capacity for interstate externality management is curtailed
 *   - expansive_federalism_advocates: excluded; their position (broad federal authority) is foreclosed by the reading's constitutional premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.62).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.71).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause (Originalist Narrow Reading)").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, '43fee258-c0b3-43af-8a46-4b63dcfd4fb2').
narrative_ontology:cs_kernel_codification('43fee258-c0b3-43af-8a46-4b63dcfd4fb2', fixed_text).
narrative_ontology:cs_authority_grounding('43fee258-c0b3-43af-8a46-4b63dcfd4fb2', lineage).
narrative_ontology:cs_interpretation_layer_present('43fee258-c0b3-43af-8a46-4b63dcfd4fb2').
narrative_ontology:cs_reading_relation('43fee258-c0b3-43af-8a46-4b63dcfd4fb2', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('43fee258-c0b3-43af-8a46-4b63dcfd4fb2', commerce_clause_text__substantial_effects_limited_reading, coexists_with).
narrative_ontology:cs_axiom('43fee258-c0b3-43af-8a46-4b63dcfd4fb2', foundational, commerce_means_trade_and_exchange).
narrative_ontology:cs_axiom_status(commerce_means_trade_and_exchange, holdable).
narrative_ontology:cs_axiom_grounding('43fee258-c0b3-43af-8a46-4b63dcfd4fb2', commerce_means_trade_and_exchange, empirically_contingent).
narrative_ontology:cs_axiom('43fee258-c0b3-43af-8a46-4b63dcfd4fb2', foundational, among_states_requires_crossing_state_lines).
narrative_ontology:cs_axiom_status(among_states_requires_crossing_state_lines, holdable).
narrative_ontology:cs_axiom_grounding('43fee258-c0b3-43af-8a46-4b63dcfd4fb2', among_states_requires_crossing_state_lines, empirically_contingent).
narrative_ontology:cs_reference_frame('43fee258-c0b3-43af-8a46-4b63dcfd4fb2', state_police_power_allocation).
narrative_ontology:cs_drift_state('43fee258-c0b3-43af-8a46-4b63dcfd4fb2', contemporary_integrated_economy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('43fee258-c0b3-43af-8a46-4b63dcfd4fb2', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, federalism_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, environmental_regulators).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, interstate_commerce_standardizers).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, national_coordination_bodies).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62) because the constraint allocates regulatory jurisdiction away from federal agencies to states, depriving interstate coordination bodies of authority they would exercise under alternative readings. Suppression is high (0.71) because the narrow reading's constitutional closure is enforced through judicial review: statutes reaching intrastate activity are struck down or narrowly construed regardless of their interstate effects or utility. Theater is moderate (0.48) because the originalist methodology and constitutional text are genuinely invoked and applied, not mere cover, but the invocation of 'original meaning' increasingly performs the legitimacy work (describing the constraint as natural law / constitutional text) even as the measured extraction shows active suppression of alternative federal authority. The measurement series projects forward over 40 time units assuming continued competition between originalist and expansive readings, with extractiveness and suppression stabilizing as the reading's competitive position stabilizes in jurisprudence.
 *
 * PERSPECTIVAL GAP:
 *   This is a kernel-reading case where the same constitutional text grounds two incompatible readings. A state government and a federal regulator reading the same Commerce Clause clause perceive radically different constraints: one sees preserved state authority, the other sees suppressed federal authority. Neither is misreading; both are reading the same kernel under different interpretive frames (originalist narrow frame vs. substantial-effects frame). The engine computes different types for each seat precisely because the structural consequences differ: the state benefits (coordination of federalism), the federal regulator loses (extraction of authority).
 *
 * DIRECTIONALITY LOGIC:
 *   State governments are the primary beneficiaries and agenda-setters (d near 0.0 for state governments) — they retain authority and autonomy. Originalist judges are agenda-setters but with institutional rather than personal benefit; their d depends on whether they derive power from interpretive authority (analytical d) or capture some other benefit from state autonomy. Environmental regulators, federal coordinators, and interstate standardizers are the primary victims (d near 1.0) — they lose authority and face coordination failures. Federalism advocates are partial beneficiaries (d moderately low) because the reading advances their cause, but they do not run the constraint directly. The exclusion of expansive-federalism advocates places them outside the directionality computation entirely (excluded stakes do not feed d).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was state protectionism and trade-barrier fragmentation. The constraint coordinates the solution to that specific problem (allocating federal authority over border-crossing transactions). However, modern interstate commerce generates externalities (pollution, labor standards, market fragmentation) that were not the original concern. If the founding problem is solved but the constraint persists to prevent federal regulation of these new problems, mandatrophy has occurred: the constraint's original function is dead, but active suppression of alternative readings keeps it in place. The measurement trajectory (extractiveness rising toward a plateau) is consistent with a constraint that has solved its original function and now operates as institutional inertia plus active litigation-based enforcement. An omega variable names the uncertainty: is the founding problem still live (new externalities are new versions of the coordination problem), or has it been solved and replaced by a different institutional concern (state autonomy for its own sake)?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_18th_century_meaning,
    'What did ''commerce among the several States'' mean to the Framers and ratifiers in 1787–1791, and does that historical meaning constrain modern interpretation?',
    'Historical analysis of founding-era usage, contemporary commercial practice, and ratification-era understanding. Scholarly consensus or divergence on original public meaning.',
    'If the original meaning was indeed narrowly trade-crossing-borders, this reading''s constitutional grounding is strong; if original meaning encompassed effects on interstate commerce or economic integration, the narrow reading rests on selective history.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalist_18th_century_meaning, empirical, 'Whether historical evidence supports the narrow originalist interpretation of interstate commerce.').

omega_variable(
    foundation_problem_evolution,
    'Has the founding problem (state protectionism/trade barriers) been solved, or has the problem evolved to include managing national economic externalities and market fragmentation?',
    'Empirical analysis of state protectionism in modern economy (tariff levels, discriminatory regulation, interstate barriers); comparison to Framers'' era. Policy analysis of whether intrastate regulation generates significant interstate effects.',
    'If the founding problem is solved and modern regulation is incidental, the narrow reading is appropriate; if the problem has evolved to include cross-state externalities, the narrow reading leaves solved problems unaddressed and creates new coordination failures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundation_problem_evolution, preference, 'Whether the original problem is solved or has transformed.').

omega_variable(
    vertical_authority_separation_viability,
    'Is a clean vertical separation of authority (federal over interstate, states over intrastate) structurally possible when intrastate activity generates interstate effects?',
    'Empirical case studies of regulatory domains (environmental, labor, financial) where intrastate rules have significant cross-state effects. Analysis of whether states can regulate intrastate activity without affecting interstate commerce.',
    'If clean separation is impossible (all intrastate activity has some interstate effect), the narrow reading creates perpetual boundary disputes and coordination failure. If separation is workable, the reading is operationally coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vertical_authority_separation_viability, empirical, 'Whether vertical authority separation is viable given modern economic integration.').

omega_variable(
    interpretive_methodology_selection,
    'Does originalist methodology constrain interpretation toward the narrow reading, or is the narrow reading one of several plausible originalist constructions?',
    'Originalist scholarship and jurisprudence examining multiple originalist readings of the Commerce Clause. Whether non-originalist methodologies (living constitutionalism, purposivism) would produce different readings.',
    'If narrow reading is the only coherent originalist reading, methodology drives the conclusion; if multiple originalist readings are viable, the choice among them depends on contestable framing (foundational axioms in the cs_structure sense).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_methodology_selection, conceptual, 'Whether originalist methodology uniquely determines the narrow reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_text__originalist_narrow_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(comm_tr_t8, commerce_clause_text__originalist_narrow_reading, theater_ratio, 8, 0.41).
narrative_ontology:measurement(comm_tr_t16, commerce_clause_text__originalist_narrow_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(comm_tr_t24, commerce_clause_text__originalist_narrow_reading, theater_ratio, 24, 0.47).
narrative_ontology:measurement(comm_tr_t32, commerce_clause_text__originalist_narrow_reading, theater_ratio, 32, 0.48).
narrative_ontology:measurement(comm_tr_t40, commerce_clause_text__originalist_narrow_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comm_be_t8, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(comm_be_t16, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(comm_be_t24, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(comm_be_t32, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(comm_be_t40, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(comm_su_t8, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(comm_su_t16, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(comm_su_t24, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(comm_su_t32, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(comm_su_t40, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__originalist_narrow_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the commerce_clause_text kernel. The three readings (originalist_narrow_reading, expansive_federal_reading, substantial_effects_limited_reading) are structurally distinct constraints with different ε values and beneficiary/victim structures. The narrow reading constrains federal authority; the expansive reading expands it; the limited reading provides a middle path. Each reading is held by different parties (originalist judges vs. expansive federalism advocates vs. pragmatic federalists). The network links record that interpretive competition over the same constitutional text drives the structure of all three constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__originalist_narrow_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
