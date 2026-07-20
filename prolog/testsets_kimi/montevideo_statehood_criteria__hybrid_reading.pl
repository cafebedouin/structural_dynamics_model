% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Montevideo Hybrid Statehood Criteria (Objective + Normative Legitimacy)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid reading of the Montevideo
 *   statehood criteria kernel: the claim that statehood requires not only the
 *   four objective criteria (permanent population, defined territory,
 *   government, capacity to enter relations) but also normative legitimacy
 *   (democratic governance, human rights, non-aggression). This reading
 *   emerged from post-WWII and post-Cold War liberal internationalism and is
 *   contested by declaratory purists (who assert objective criteria suffice)
 *   and constitutive theorists (who emphasize recognition). The hybrid
 *   reading structurally benefits liberal democratic states and great powers
 *   by supplying normative language to deny recognition to inconvenient
 *   secessionists and to cloak intervention in legality, while non-liberal
 *   secessionist movements and autocratic regimes bear the costs of exclusion
 *   and delegitimation.
 *
 * KEY AGENTS:
 *   - great_powers (agenda_setter/institutional/arbitrage): Administer recognition decisions and enforcement, strategically toggling between legal readings.
 *   - liberal_democratic_states (beneficiary/powerful/constrained): Collect legitimacy subsidy and recognition-denial power.
 *   - non_liberal_secessionists (payer/powerless/trapped): Satisfy objective criteria but are denied recognition on normative grounds.
 *   - autocratic_regimes (payer/powerful/constrained): Suffer conditional sovereignty and intervention framing despite formal statehood.
 *   - international_judicial_bodies (observer/institutional/analytical): Interpret and propagate the hybrid doctrine.
 *   - post_colonial_critics (excluded/moderate/constrained): Object to the normative overlay as neo-imperial but are marginalized in recognition forums.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.72).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Montevideo Hybrid Statehood Criteria (Objective + Normative Legitimacy)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, '24211061-afe1-453f-a88b-d5b4373bb26f').
narrative_ontology:cs_kernel_codification('24211061-afe1-453f-a88b-d5b4373bb26f', formalized).
narrative_ontology:cs_authority_grounding('24211061-afe1-453f-a88b-d5b4373bb26f', lineage).
narrative_ontology:cs_interpretation_layer_present('24211061-afe1-453f-a88b-d5b4373bb26f').
narrative_ontology:cs_reading_relation('24211061-afe1-453f-a88b-d5b4373bb26f', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('24211061-afe1-453f-a88b-d5b4373bb26f', montevideo_statehood_criteria__constitutive_reading, influences).
narrative_ontology:cs_axiom('24211061-afe1-453f-a88b-d5b4373bb26f', foundational, liberal_governance_as_statehood_prerequisite).
narrative_ontology:cs_axiom_status(liberal_governance_as_statehood_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('24211061-afe1-453f-a88b-d5b4373bb26f', liberal_governance_as_statehood_prerequisite, deontological).
narrative_ontology:cs_axiom('24211061-afe1-453f-a88b-d5b4373bb26f', foundational, recognition_may_be_denied_for_normative_failure).
narrative_ontology:cs_axiom_status(recognition_may_be_denied_for_normative_failure, holdable).
narrative_ontology:cs_axiom_grounding('24211061-afe1-453f-a88b-d5b4373bb26f', recognition_may_be_denied_for_normative_failure, conventional).
narrative_ontology:cs_reference_frame('24211061-afe1-453f-a88b-d5b4373bb26f', liberal_international_order).
narrative_ontology:cs_drift_state('24211061-afe1-453f-a88b-d5b4373bb26f', multipolar_challenge_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24211061-afe1-453f-a88b-d5b4373bb26f', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, great_powers).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, autocratic_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shape and enforce the hybrid criteria through diplomatic recognition decisions, UN Security Council actions, and conditional sovereignty frameworks. They can arbitrage between declaratory, constitutive, and hybrid readings as strategic interests demand, but benefit most from the hybrid frame's normative flexibility and legal cover for intervention.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, great_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain normative justification to deny recognition to non-liberal secessionist movements and to frame non-intervention as conditional on human rights compliance. Their own statehood is never questioned under this framework, giving them a structural subsidy in legitimacy and a lower cost of excluding rivals.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, beneficiary,
    powerful, generational, constrained, global).

% Control territory and population, often exercise effective government, yet are systematically denied recognition because their political form fails the liberal democratic and human rights tests, regardless of satisfying the four objective Montevideo criteria. They cannot exit the recognition system and cannot compel admission.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists, payer,
    powerless, biographical, trapped, regional).

% Retain UN membership and formal statehood but suffer normative delegitimation under the hybrid reading. Their sovereignty is treated as conditional, exposing them to intervention framing, sanctions, and contested recognition without being formally dissolved.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, autocratic_regimes, payer,
    powerful, generational, constrained, national).

% Render advisory opinions and judgments that progressively incorporate human rights and democratic governance into statehood doctrine, reflecting and reinforcing the hybrid reading's authority without directly enforcing it against states.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_judicial_bodies, observer,
    institutional, civilizational, analytical, global).

% Argue that the normative criteria are a post-colonial mechanism for delegitimizing non-Western political forms and maintaining hierarchical sovereignty. Their scholarship circulates in academic and some diplomatic venues but is systematically marginalized in the formal recognition institutions where the hybrid criteria are enforced.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, post_colonial_critics, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for the orderly admission of new states into the international community, ostensibly replacing pure power-politics recognition with rule-governed criteria that combine objective state capacity with normative standards of legitimate governance.
% TRANSFER_FUNCTION: Moves the power to legitimately deny recognition and withhold sovereign equality from non-liberal entities to the community of liberal democratic states and great powers, while transferring the cost of exclusion, statelessness, and intervention vulnerability to secessionist movements and non-conforming regimes.
% ABSENT_VOICES: Non-liberal secessionist movements and populations in contested territories are excluded from the forums where recognition criteria are interpreted; post-colonial critics are present in discourse but excluded from institutional decision-making. Autocratic states are present but their objections to the normative overlay are treated as self-interested and illegitimate by the framework's own logic.
% DISAPPEARANCE_RATIONALE: Without the hybrid criteria, the international system would revert toward either purely factual statehood or purely political recognition. Secessionist conflicts would be adjudicated differently, liberal states would lose the normative language to deny recognition to inconvenient breakaways, and the legal cover for humanitarian intervention and regime change would dissolve or migrate to other frames.
% FOUNDING_PROBLEM: The dual founding problem was preventing chaotic proliferation of unrecognized entities and wars of recognition, and justifying the exclusion of fascist and totalitarian states during and after WWII by embedding liberal values into the legal definition of statehood.
% FOUNDING_PROBLEM_CORROBORATION: Liberal democratic states and Western international law scholars attest the problem remains live, citing failed states and authoritarian expansion. Post-colonial scholars, non-aligned movements, and some Global South diplomats attest the founding problem has mutated into a tool of neo-imperial exclusion; their testimony originates outside the primary beneficiary set.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the normative overlay converts recognition from a legal fact into a discretionary political tool wielded by the liberal camp. Suppression (0.72) is high because the constraint's persistence requires active diplomatic, economic, and occasionally military enforcement against secessionist claims and autocratic assertions of unconditional sovereignty. Theater ratio (0.50) reflects the growing gap between the universalist rhetoric of the criteria and their selective application (e.g., recognition of Kosovo versus non-recognition of Abkhazia). Accessibility collapse (0.60) captures the marginalization of the pure declaratory alternative in Western-dominated forums, though the constitutive alternative persists in multipolar contexts. Resistance (0.55) reflects organized pushback from non-aligned movements, autocratic blocs, and some post-colonial legal scholars.
 *
 * PERSPECTIVAL GAP:
 *   From the great-power seat, the hybrid criteria are necessary evolutionary safeguards against failed-state proliferation and authoritarian aggression; from the secessionist and autocratic seats, the same criteria operate as a movable bar that preserves the international club for the like-minded. The engine should compute low directionality for liberal states (subsidized legitimacy) and high directionality for trapped secessionists (pure target).
 *
 * DIRECTIONALITY LOGIC:
 *   Great powers and liberal democratic states are structural beneficiaries: the constraint subsidizes their recognition decisions with normative language and lowers the cost of excluding rivals. Non-liberal secessionists are full targets: they meet objective tests but are barred by subjective criteria they cannot unilaterally satisfy. Autocratic regimes sit in an intermediate-high range: formally inside the state system, but the hybrid reading steadily raises their directionality by converting their sovereignty from a right into a conditional privilege.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing recognition chaos and justifying anti-fascist exclusionâhas partially died, but the arrangement persists because it now serves the strategic interests of the liberal camp. This is not a pure snare because the coordination function (ordering admission to the international system) remains real; it is not a pure rope because the enforcement is asymmetric and the criteria are applied strategically. The hybrid reading prevents mandatrophy misclassification by forcing the analysis to account for both the genuine coordination benefit and the extractive asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_legitimacy,
    'Does the hybrid reading reflect an emerging objective legal synthesis of Montevideo criteria and normative legitimacy, or is it a constructed justification for the exclusionary preferences of liberal great powers?',
    'Historical analysis of treaty negotiation records and correlation between recognition practice and strategic interest versus normative consistency over the post-Cold War period.',
    'If purely constructed, the constraint''s extraction is higher and its coordination function is cover; if genuinely emergent customary law, it may function as a real coordination mechanism with inherent costs rather than asymmetric rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_legitimacy, conceptual, 'Whether the hybrid reading is a discovered legal synthesis or a constructed power tool.').

omega_variable(
    secessionist_victim_ambiguity,
    'Are non-liberal secessionists genuinely victimized by the normative overlay, or are they simply denied a benefit (recognition) they never possessed?',
    'Comparative analysis of material harms beyond non-recognition, such as economic blockade, intervention frequency, and internal development constraints imposed by exclusion.',
    'If exclusion produces material harms beyond withholding a privilege, the victim status is confirmed and extraction is asymmetric; if it merely denies a discretionary benefit, the extraction reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secessionist_victim_ambiguity, empirical, 'Whether exclusion from recognition constitutes extraction or mere privilege denial.').

omega_variable(
    reading_stability,
    'Can the hybrid reading stabilize without collapsing into either the declaratory or constitutive reading under geopolitical pressure?',
    'Track recognition crises such as Kosovo, Crimea, and Palestine to see whether decision-makers invoke objective criteria, political will, or the hybrid normative frame during stress.',
    'If the hybrid frame is abandoned for power-politics or pure factuality during crises, it functions as a theatrical overlay rather than a stable tangled rope, signaling piton-like degradation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_stability, empirical, 'Stability of the hybrid reading under recognition crisis pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(mont_tr_t6, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(mont_tr_t12, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(mont_tr_t18, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 18, 0.5).
narrative_ontology:measurement(mont_tr_t24, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 24, 0.55).
narrative_ontology:measurement(mont_tr_t30, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 30, 0.5).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mont_be_t6, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(mont_be_t12, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(mont_be_t18, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 18, 0.72).
narrative_ontology:measurement(mont_be_t24, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(mont_be_t30, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(mont_su_t6, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(mont_su_t12, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(mont_su_t18, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 18, 0.65).
narrative_ontology:measurement(mont_su_t24, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(mont_su_t30, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__constitutive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Montevideo statehood criteria kernel, decomposed per the epsilon-invariance principle because the hybrid reading's normative overlay produces a structurally distinct beneficiary-victim pattern and extractiveness profile compared to the pure declaratory and pure constitutive readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
