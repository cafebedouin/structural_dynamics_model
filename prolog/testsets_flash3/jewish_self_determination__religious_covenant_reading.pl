% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Jewish Self-Determination: Religious Covenant Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the 'religious covenant' reading of Jewish
 *   self-determination, where the claim to the land is derived from a divine
 *   command, making territorial sovereignty a religious obligation. While
 *   claimed as a 'mountain' due to its immutable divine origin, its
 *   operationalization in a contested political space, requiring active
 *   enforcement and suppressing alternative frameworks, leads to high
 *   effective extraction. The metrics reflect the increasing entrenchment and
 *   enforcement of this religious claim over time, particularly since 1967.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.85).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.9).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Jewish Self-Determination: Religious Covenant Reading").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).
domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '49ffca56-6dd4-4c3e-bef3-6d86355924ce').
narrative_ontology:cs_kernel_codification('49ffca56-6dd4-4c3e-bef3-6d86355924ce', fixed_text).
narrative_ontology:cs_authority_grounding('49ffca56-6dd4-4c3e-bef3-6d86355924ce', lineage).
narrative_ontology:cs_interpretation_layer_present('49ffca56-6dd4-4c3e-bef3-6d86355924ce').
narrative_ontology:cs_reading_relation('49ffca56-6dd4-4c3e-bef3-6d86355924ce', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('49ffca56-6dd4-4c3e-bef3-6d86355924ce', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('49ffca56-6dd4-4c3e-bef3-6d86355924ce', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('49ffca56-6dd4-4c3e-bef3-6d86355924ce', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('49ffca56-6dd4-4c3e-bef3-6d86355924ce', foundational, divine_covenant_absolute_land_claim).
narrative_ontology:cs_axiom_status(divine_covenant_absolute_land_claim, holdable).
narrative_ontology:cs_axiom_grounding('49ffca56-6dd4-4c3e-bef3-6d86355924ce', divine_covenant_absolute_land_claim, theological).
narrative_ontology:cs_axiom('49ffca56-6dd4-4c3e-bef3-6d86355924ce', foundational, territorial_sovereignty_religious_obligation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_religious_obligation, holdable).
narrative_ontology:cs_axiom_grounding('49ffca56-6dd4-4c3e-bef3-6d86355924ce', territorial_sovereignty_religious_obligation, deontological).
narrative_ontology:cs_reference_frame('49ffca56-6dd4-4c3e-bef3-6d86355924ce', biblical_covenant_unconditional).
narrative_ontology:cs_drift_state('49ffca56-6dd4-4c3e-bef3-6d86355924ce', contemporary_political_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('49ffca56-6dd4-4c3e-bef3-6d86355924ce', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_political_frameworks).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_negotiators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the divine covenant as an immutable command for Jewish sovereignty over the entire land, actively shaping policy and promoting settlement. Their identity is fused with this interpretation, making any compromise unthinkable.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, agenda_setter,
    institutional, generational, identity_locked, national).

% Directly benefits from the religious justification for territorial expansion, receiving state support and ideological backing. Their existence is predicated on the covenantal claim, making exit from this framework equivalent to abandoning their core purpose.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, generational, identity_locked, local).

% Attempt to negotiate territorial disputes based on international law, demographics, and security concerns. The religious covenant reading fundamentally undermines their legitimacy and capacity for compromise, forcing them to either accommodate or be sidelined.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_political_frameworks, payer,
    institutional, biographical, constrained, national).

% Seek a resolution based on self-determination and international recognition. The religious covenant reading denies the very premise of their claims, rendering secular negotiation frameworks ineffective and their position fundamentally undermined.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_negotiators, payer,
    powerless, generational, trapped, regional).

% Attempts to mediate the conflict using secular legal and political frameworks. The religious covenant reading presents an intractable challenge to their efforts, as it operates on a different ontological plane.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_diplomatic_community, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: For adherents, it coordinates collective action and identity around a shared, divinely ordained purpose for the land, providing a coherent framework for territorial claims and settlement.
% TRANSFER_FUNCTION: Transfers ultimate authority over territorial claims from secular political processes and international law to a divinely revealed covenant, effectively transferring land and resources to those acting on the covenant's behalf.
% ABSENT_VOICES: Secular Jewish voices who prioritize universal ethics or diaspora flourishing, and Palestinian voices advocating for indigenous rights and self-determination, are excluded from the covenantal framework, which renders their claims illegitimate by definition.
% DISAPPEARANCE_RATIONALE: If the belief in the divine covenant as the basis for territorial sovereignty vanished, the religious Zionist movement would lose its foundational legitimacy, the settlement enterprise would be stripped of its primary justification, and the entire framework for territorial claims would shift dramatically towards secular and international legal norms, fundamentally altering the conflict dynamics.
% FOUNDING_PROBLEM: The historical dispersion of the Jewish people and the desire for a secure, sovereign homeland, understood through a theological lens as the fulfillment of a divine promise.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts and traditions attest to the divine covenant. However, secular historians and political scientists, as well as Palestinian narratives, contest the interpretation of this covenant as a mandate for exclusive territorial sovereignty in a modern political context, arguing it is a theological claim, not a political one.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading demands exclusive sovereignty based on a non-negotiable divine mandate, effectively extracting land and political agency from those who do not share this theological premise. Suppression (0.90) is very high as it actively delegitimizes and suppresses secular or alternative claims to the same territory, requiring significant enforcement to maintain. Theater ratio (0.20) is low because the religious conviction is genuine, but the 'naturalness' of the claim is performatively asserted in secular contexts to resist compromise. Accessibility collapse (0.70) is substantial because it fundamentally alters the terms of engagement, making secular alternatives appear illegitimate or irrelevant. Resistance (0.80) is high due to the direct conflict with other claims to the land.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the religious Zionist movement, this is a divine, immutable 'mountain' – a natural law of their existence. From the perspective of secular political actors or Palestinian negotiators, it functions as a 'snare' or 'tangled rope' – a constructed, highly extractive constraint enforced through political and military power, leveraging a religious narrative to justify its claims. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious Zionist movement and settlement enterprise are clear beneficiaries, as the constraint provides the ultimate justification for their actions and existence (d near 0.0). Secular political frameworks and Palestinian negotiators are victims, as their claims are undermined and their options suppressed by the divine mandate (d near 1.0). The international diplomatic community acts as an observer, attempting to apply secular frameworks that are often rejected by the covenantal reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (divine command) is considered eternally 'live' by its adherents, preventing mandatrophy from being acknowledged within this framework. However, from an external perspective, the 'founding problem' of securing a homeland has been achieved, yet the constraint persists and expands, suggesting a potential for mandatrophy if viewed through a secular lens. The high extractiveness and suppression, coupled with the 'live' status of the founding problem, indicate that the constraint is actively maintained for ongoing benefit, rather than merely persisting by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_political_authority,
    'Is the claim to the land primarily a theological truth or a political assertion?',
    'Analysis of the constraint''s enforcement mechanisms: if it relies on state power and military force rather than purely religious adherence, it functions as a political assertion, regardless of its theological origin.',
    'If primarily theological, the constraint is a genuine mountain for adherents, with low internal extraction. If primarily a political assertion, it functions as a snare or tangled rope, with high effective extraction from those outside the religious framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_vs_political_authority, conceptual, 'Ambiguity between theological and political grounding of territorial claims.').

omega_variable(
    natural_law_vs_constructed_constraint,
    'Is the divine covenant an immutable natural law, or a constructed interpretation used to justify political ends?',
    'Examination of interpretive flexibility within religious traditions: if interpretations of the covenant have historically varied or adapted to political realities, it suggests a constructed element. If it is universally held as a literal, unchanging command, it leans towards natural law.',
    'If a natural law, the constraint''s ''mountain'' classification is robust. If constructed, it is a ''false summit'' (tangled rope or snare) that benefits identifiable agents, despite its naturalistic framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_constraint, conceptual, 'Ambiguity of divine covenant as natural law or political construct.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (military occupation, legal exclusion) or internalized (religious conviction, identity fusion)?',
    'Post-exit suppression trajectory: if suppression of alternative claims persists after the military/legal mechanisms are removed, reclassify as partially internalized. If it collapses, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making compromise harder. If structural, removal of external barriers would significantly alter the constraint''s dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for territorial claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__religious_covenant_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__religious_covenant_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__religious_covenant_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(jewi_tr_t2005, jewish_self_determination__religious_covenant_reading, theater_ratio, 2005, 0.19).
narrative_ontology:measurement(jewi_tr_t2015, jewish_self_determination__religious_covenant_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__religious_covenant_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1967, 0.75).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1993, 0.8).
narrative_ontology:measurement(jewi_be_t2005, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2005, 0.82).
narrative_ontology:measurement(jewi_be_t2015, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2015, 0.84).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1993, 0.85).
narrative_ontology:measurement(jewi_su_t2005, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(jewi_su_t2015, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2015, 0.89).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
