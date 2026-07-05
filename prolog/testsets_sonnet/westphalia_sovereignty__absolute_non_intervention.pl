% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Sovereignty as Categorical Territorial Inviolability (Absolute Non-Intervention Reading)
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested Westphalian
 *   sovereignty kernel: sovereignty as categorical territorial inviolability,
 *   under which external interference in a state's domestic affairs is per se
 *   illegitimate regardless of what that state does to the people inside its
 *   borders. This is NOT a story about sovereignty in general — it is the
 *   specific structural claim that internal conduct is legally irrelevant to
 *   the legitimacy of external intervention. Two sibling constraints exist
 *   and are NOT part of this file: conditional_responsibility (sovereignty
 *   forfeited by atrocity) and graded_sovereignty (sovereignty as a scalar
 *   capacity). Each sibling has a different victim set, a different
 *   beneficiary structure, and a different epsilon; they are linked here only
 *   by network reference, never merged into this story's classification.
 *
 * KEY AGENTS:
 *   - authoritarian_state_elites: Primary beneficiary (institutional/arbitrage) — uses the categorical shield to insulate internal rule from external correction
 *   - permanent_security_council_members: Agenda-setter (institutional/arbitrage) — administers the enforcement apparatus and invokes the doctrine selectively
 *   - populations_under_authoritarian_control: Primary target (powerless/trapped) — bears the extraction with no standing to invoke or waive the rule governing them
 *   - international_law_scholars: Analytical observer — traces the gap between the doctrine's categorical text and its selective practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.72).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Sovereignty as Categorical Territorial Inviolability (Absolute Non-Intervention Reading)").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '129e30d2-6822-45fd-855c-005091c7b5fa').
narrative_ontology:cs_kernel_codification('129e30d2-6822-45fd-855c-005091c7b5fa', formalized).
narrative_ontology:cs_authority_grounding('129e30d2-6822-45fd-855c-005091c7b5fa', extraction).
narrative_ontology:cs_interpretation_layer_present('129e30d2-6822-45fd-855c-005091c7b5fa').
narrative_ontology:cs_reading_relation('129e30d2-6822-45fd-855c-005091c7b5fa', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('129e30d2-6822-45fd-855c-005091c7b5fa', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('129e30d2-6822-45fd-855c-005091c7b5fa', foundational, internal_conduct_categorically_irrelevant_to_intervention_legitimacy).
narrative_ontology:cs_axiom_status(internal_conduct_categorically_irrelevant_to_intervention_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('129e30d2-6822-45fd-855c-005091c7b5fa', internal_conduct_categorically_irrelevant_to_intervention_legitimacy, conventional).
narrative_ontology:cs_axiom('129e30d2-6822-45fd-855c-005091c7b5fa', foundational, territorial_boundary_is_the_sole_predicate_of_legitimate_authority).
narrative_ontology:cs_axiom_status(territorial_boundary_is_the_sole_predicate_of_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('129e30d2-6822-45fd-855c-005091c7b5fa', territorial_boundary_is_the_sole_predicate_of_legitimate_authority, conventional).
narrative_ontology:cs_reference_frame('129e30d2-6822-45fd-855c-005091c7b5fa', peace_of_westphalia_territorial_settlement).
narrative_ontology:cs_drift_state('129e30d2-6822-45fd-855c-005091c7b5fa', post_cold_war_r2p_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('129e30d2-6822-45fd-855c-005091c7b5fa', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, permanent_security_council_members).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, incumbent_regimes_facing_internal_dissent).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, ethnic_and_religious_minorities_facing_state_violence).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, domestic_opposition_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, weaker_states_with_contested_borders).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, territorial_integrity_norm).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, non_intervention_doctrine).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, juridical_equality_of_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern territories where internal conduct — repression, ethnic cleansing, mass detention — would otherwise invite external scrutiny or intervention. Invoke the categorical non-intervention norm at the UN and in bilateral diplomacy to foreclose outside review, and vote as a bloc to preserve the doctrine because it is the primary shield protecting their rule from external correction.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites, agenda_setter).

% Hold veto power that operationalizes the non-intervention norm selectively: they invoke categorical sovereignty to block intervention against allies or against themselves, while permitting or conducting intervention against adversaries under other legal theories. They administer the enforcement machinery (UN Charter Article 2(4) framework) that gives the doctrine its teeth.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, permanent_security_council_members, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Live under regimes whose internal conduct is placed categorically beyond external remedy by the doctrine. Cannot appeal to international bodies for protection from their own government because the sovereignty norm treats the relationship between a state and its population as internal, non-justiciable, and closed to outside actors regardless of severity.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control, payer,
    powerless, biographical, trapped, local).

% Face targeted violence, displacement, or genocide inside a state's borders. Under the categorical reading, the classification of the violence as a purely domestic matter is dispositive against any legal basis for external protection, no matter how well-documented the atrocity. Exit means fleeing across a border into refugee status, not remedy within the system.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, ethnic_and_religious_minorities_facing_state_violence, payer,
    powerless, biographical, trapped, local).

% Organize against incumbent regimes and are frequently the direct target of the repression the doctrine shields from outside intervention. International sympathizers are structurally barred from providing more than rhetorical support without the intervening state itself violating the non-intervention norm, which weakens the material backing available to internal dissent.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, domestic_opposition_movements, payer,
    moderate, biographical, constrained, national).

% Rely on the categorical norm as their principal defense against annexation or great-power interference, since they lack the military capacity to deter intervention by force. The same absolute framing that shields authoritarian abuse also protects weaker states from being carved up or occupied under humanitarian or other pretexts.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, weaker_states_with_contested_borders, beneficiary,
    moderate, generational, constrained, national).

% Document atrocities and argue for a responsibility-to-protect framework that would condition sovereignty on conduct. Their position is structurally excluded from the categorical reading's own legal logic — the doctrine defines their claims as themselves illegitimate interference, not merely as claims to be weighed and rejected.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, humanitarian_intervention_advocates, excluded,
    organized, biographical, constrained, global).

% Analyze the doctrine's textual basis (UN Charter Art. 2(7), the Peace of Westphalia's traditional lineage), track state practice and opinio juris, and note the gap between the categorical text and the selective practice of powerful states who invoke it inconsistently.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__absolute_non_intervention, diffuse).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__absolute_non_intervention, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line rule preventing states from using humanitarian, ideological, or security pretexts to invade or destabilize one another, which under a world of great-power rivalry functions as a genuine check against pretextual wars of conquest dressed as intervention.
% TRANSFER_FUNCTION: Moves the cost of internal state violence entirely onto the population subjected to it and withholds any externally-backed remedy; simultaneously transfers strategic insulation to incumbent elites and to permanent Security Council members who can invoke the doctrine selectively.
% ABSENT_VOICES: Populations suffering under the regimes the doctrine shields have no standing to invoke or waive the norm that governs them — the doctrine is a rule about states, authored by states, applied to a state-population relationship in which the population is not a party.
% DISAPPEARANCE_RATIONALE: If categorical non-intervention vanished overnight without any replacement doctrine, both a shield and a pretext would be gone at once: authoritarian regimes would lose their strongest legal defense against external pressure, but weaker states would simultaneously lose their strongest legal defense against pretextual invasion by stronger ones. The Security Council veto structure, alliance systems, and refugee law would all reorganize around a different baseline.
% FOUNDING_PROBLEM: Built to end the post-Reformation European wars fought partly over the right to impose religious and political order across borders — the 1648 settlement fixed each sovereign's exclusive authority over their own territory's internal arrangements to stop cross-border wars of religious/political correction.
% FOUNDING_PROBLEM_CORROBORATION: The 20th-century UN Charter drafters and most incumbent state governments attest the founding problem (cross-border wars of imposed order) remains live and the categorical rule remains its necessary guard. Independent international law scholarship, the drafters and proponents of the Responsibility to Protect doctrine (2005 World Summit outcome document), and human rights monitoring bodies outside any beneficiary government attest that mass-atrocity prevention has become a distinct and partially incompatible problem the categorical rule does not solve and in practice blocks.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the categorical framing forecloses any legal remedy for populations subjected to severe internal state violence, and that this foreclosure is a stable, structural feature of the reading rather than an occasional side effect. Suppression (0.72) is high because the doctrine's persistence depends on active enforcement — Security Council vetoes, UN Charter invocation, diplomatic reciprocity norms — that actively blocks intervention attempts, not merely on the absence of interest in intervening. Theater ratio rose from 0.2 to 0.4 over the interval as the gap between the doctrine's stated universal application and its selectively-invoked practice (protecting allies, ignoring the same conduct in rivals) widened following the Cold War and the R2P debates. Accessibility collapse (0.6) and resistance (0.58) reflect that this is a constructed legal doctrine with real contestation and visible alternative framings (R2P, humanitarian intervention doctrine) rather than a natural law with no live opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the authoritarian_state_elites and permanent_security_council_members seats, the doctrine reads as the legitimate coordination mechanism preventing predatory intervention and pretextual war. From the populations_under_authoritarian_control and minorities_facing_state_violence seats, the identical structure operates as an enforced denial of any external remedy for extreme harm. The engine computes these as different seat-level classifications from the same structural data — the categorical reading does not resolve this gap, it is defined by holding both readings apart by design (internal conduct is legally irrelevant to legitimacy, full stop).
 *
 * DIRECTIONALITY LOGIC:
 *   State elites who benefit from the shield sit near the beneficiary end of directionality (low d) because the constraint subsidizes their insulation from correction. Populations under their control sit near the full-target end (high d) because they are trapped, cannot exit the jurisdiction without becoming refugees, and have no standing under the doctrine to seek external remedy for what is done to them. Weaker states with contested borders are a genuine mixed case: they benefit from the same categorical shield against invasion even though the shield's cost falls on populations inside authoritarian states elsewhere — this is why weaker_states_with_contested_borders is coded as beneficiary despite moderate power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (ending cross-border wars of imposed religious/political order, 1648) remains partially live — great-power wars of conquest dressed as correction of another state's internal order are still a real risk the categorical rule guards against. But the doctrine's application to mass-atrocity prevention is a distinct problem the 1648 settlement never addressed, and treating the categorical rule as settled law for THAT problem is where the mandatrophy candidate lives: R2P proponents and post-1990s scholarship argue the founding problem for the non-intervention piece has partially decayed relative to a newer problem (genocide, ethnic cleansing) the doctrine was never built to solve and now blocks solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_rule_vs_selective_practice,
    'Is the categorical non-intervention norm actually applied as a universal rule, or does state practice reveal it functions as a selectively-invoked shield available disproportionately to powerful states and their allies?',
    'Comparative analysis of Security Council intervention votes and abstentions across comparable atrocity situations (e.g., comparing responses to similar-severity events involving allies of permanent members versus non-aligned states) to test whether invocation correlates with geopolitical alignment rather than principled consistency.',
    'If invocation is systematically selective, the categorical framing functions less as a genuine coordination norm and more as a legitimating cover for power-political outcomes — strengthening the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_rule_vs_selective_practice, empirical, 'Whether the categorical rule is applied consistently or selectively invoked by powerful actors.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the Westphalian sovereignty kernel genuinely indeterminate between the categorical, conditional, and graded readings, or does the 1648 settlement''s own text/practice history favor one reading as the historically dominant one?',
    'Historical-legal analysis of the actual Peace of Westphalia treaty texts and subsequent centuries of state practice, compared against the post-1990s emergence of R2P as a claimed evolution rather than a departure.',
    'If the categorical reading is the historically dominant one and the others are later normative innovations, this reading''s claim to represent ''true'' sovereignty is stronger, though this does not change its measured extractiveness — it only affects the genealogical legitimacy claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether one kernel reading has stronger historical claim to represent the original Westphalian settlement.').

omega_variable(
    atrocity_threshold_ambiguity,
    'Even within the categorical reading''s own logic, is there an unstated threshold of severity (e.g., genocide) at which even proponents of absolute non-intervention concede some external response is legitimate, making the ''categorical'' framing rhetorically absolute but practically graded?',
    'Survey of diplomatic statements and voting records from states that formally endorse the categorical doctrine to see whether they nonetheless support intervention or sanctions in extreme cases (e.g., Rwanda, Bosnia) while maintaining the categorical rhetoric.',
    'If proponents systematically carve out atrocity exceptions in practice while maintaining categorical rhetoric, the ''categorical'' claimed_type may itself be a form of theater — the doctrine''s stated absoluteness would diverge from its practiced conditionality, which would be evidence for rising theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrocity_threshold_ambiguity, empirical, 'Whether the categorical rule holds in practice or is quietly conditioned by unstated severity thresholds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1945, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(west_tr_t1960, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(west_tr_t1975, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(west_tr_t2005, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(west_tr_t2015, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(west_tr_t2025, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1945, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(west_be_t1960, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(west_be_t1975, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(west_be_t2005, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(west_be_t2015, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(west_be_t2025, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1945, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1945, 0.55).
narrative_ontology:measurement(west_su_t1960, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1960, 0.58).
narrative_ontology:measurement(west_su_t1975, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(west_su_t1990, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1990, 0.63).
narrative_ontology:measurement(west_su_t2005, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(west_su_t2015, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(west_su_t2025, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__absolute_non_intervention, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% Part of the westphalia_sovereignty kernel family (3 readings). This file (absolute_non_intervention) is the categorical reading: internal conduct is legally irrelevant to intervention legitimacy, victim set is populations under authoritarian control, beneficiaries are state elites and permanent Security Council members. The sibling conditional_responsibility reading treats sovereignty as forfeited by atrocity — its victim set and beneficiary structure differ because it re-admits mass-atrocity populations as protected parties rather than excluded ones. The sibling graded_sovereignty reading treats territorial authority as a scalar capacity rather than a binary status, producing a still-different structural map (intervention legitimacy calibrated to state capacity rather than triggered by conduct thresholds). All three are linked here for contamination/coupling analysis; none is merged into this story's classification per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
