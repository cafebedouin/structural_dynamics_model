% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Criteria — Declaratory Reading of Statehood
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The declaratory reading, codified in Article 3 of the 1933 Montevideo
 *   Convention, holds that an entity satisfying the four objective criteria
 *   (population, territory, government, capacity for foreign relations) is a
 *   state as a matter of law, and that recognition by other states is merely
 *   evidentiary acknowledgment of a pre-existing fact, not a constitutive
 *   act. This removes the formal power of the existing state community to
 *   gatekeep new statehood claims through withheld recognition. In practice
 *   the doctrine coexists uneasily with an international institutional
 *   architecture (UN membership, treaty regimes, diplomatic relations) that
 *   continues to condition practical participation on recognition — so
 *   entities that are states 'in law' under this reading often remain
 *   excluded from the benefits statehood is supposed to confer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.42).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.38).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Criteria — Declaratory Reading of Statehood").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, 'e0323485-5dd7-4018-a217-9068b3656c87').
narrative_ontology:cs_kernel_codification('e0323485-5dd7-4018-a217-9068b3656c87', formalized).
narrative_ontology:cs_authority_grounding('e0323485-5dd7-4018-a217-9068b3656c87', distributed).
narrative_ontology:cs_reading_relation('e0323485-5dd7-4018-a217-9068b3656c87', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('e0323485-5dd7-4018-a217-9068b3656c87', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('e0323485-5dd7-4018-a217-9068b3656c87', foundational, statehood_is_objective_fact_not_political_grant).
narrative_ontology:cs_axiom_status(statehood_is_objective_fact_not_political_grant, holdable).
narrative_ontology:cs_axiom_grounding('e0323485-5dd7-4018-a217-9068b3656c87', statehood_is_objective_fact_not_political_grant, conventional).
narrative_ontology:cs_axiom('e0323485-5dd7-4018-a217-9068b3656c87', foundational, recognition_is_merely_evidentiary_not_constitutive).
narrative_ontology:cs_axiom_status(recognition_is_merely_evidentiary_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('e0323485-5dd7-4018-a217-9068b3656c87', recognition_is_merely_evidentiary_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('e0323485-5dd7-4018-a217-9068b3656c87', montevideo_1933_objective_criteria_codification).
narrative_ontology:cs_drift_state('e0323485-5dd7-4018-a217-9068b3656c87', post_cold_war_secession_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e0323485-5dd7-4018-a217-9068b3656c87', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_meeting_criteria).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, secessionist_movements_with_territorial_control).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, international_law_scholars_of_positivist_tradition).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states_facing_secession).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, recognition_dependent_populations).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, unrecognized_states_denied_treaty_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, secessionist_movements_with_territorial_control).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, self_executing_international_legal_personality).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, objective_fact_based_statehood_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls a defined territory, exercises effective government over a permanent population, and claims capacity to enter international relations. Under the declaratory reading, this entity IS a state in law the moment the four criteria are met, regardless of whether any other state extends recognition. It can point to the criteria to claim treaty rights, UN forums, and legal standing even while facing near-universal non-recognition — but in practice still cannot access most international institutions that gate membership on recognition, so the declared status is legally real but practically thin.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_meeting_criteria, beneficiary,
    moderate, generational, constrained, national).

% Have fought for or otherwise achieved effective control over a breakaway territory. The declaratory reading gives their claim to statehood a legal foothold independent of the parent state's consent or of great-power politics — they need only demonstrate the criteria, not win a recognition campaign. They pay for this in the parent state's escalated countermeasures, since the parent state can no longer treat recognition-withholding as dispositive.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, secessionist_movements_with_territorial_control, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, secessionist_movements_with_territorial_control, payer).

% Lose a central lever: under the constitutive reading they could deny a breakaway region legal existence simply by refusing recognition and rallying allies to do the same. Under the declaratory reading that lever is gone in law (though not always in practice) — the breakaway region's statehood is asserted as a fact independent of what the parent state or its allies do. This is a genuine structural cost even though parent states retain diplomatic, economic, and military tools outside the recognition channel.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states_facing_secession, payer,
    institutional, generational, mobile, national).

% Live inside a territory whose declared statehood is legally asserted but widely unrecognized (e.g., contested breakaway regions). They bear the day-to-day cost of the gap between legal fact and practical fact: no consular protection abroad, no access to international courts, no participation in treaties that require UN membership, while their own authorities claim full statehood is already achieved. The declaratory reading tells them their state exists; the world outside often treats it as if it does not.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, recognition_dependent_populations, payer,
    powerless, biographical, trapped, local).

% Meet the objective criteria by any honest measure but remain excluded from most treaty regimes, international organizations, and dispute-resolution fora because those institutions gate access on recognition or UN membership, not on satisfaction of the Montevideo criteria alone. The declaratory reading grants them legal personhood on paper while the actual architecture of international cooperation still runs on the constitutive logic they are trying to escape — a gap between the doctrine and the institutions that administer it.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, unrecognized_states_denied_treaty_access, payer,
    moderate, generational, constrained, global).

% Advance and defend the declaratory reading as doctrinally coherent and normatively preferable — it removes statehood from the arbitrariness of great-power politics and grounds it in verifiable fact. Their professional and intellectual investment is in this reading's continued authority within scholarship and, where possible, in state practice and adjudication (e.g., citations in arbitral awards, ICJ opinions).
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_law_scholars_of_positivist_tradition, beneficiary,
    analytical, civilizational, analytical, global).

% As a body, existing states retain enormous practical control over whether a declared state can function internationally (UN membership, embassy exchange, trade agreements) even if the declaratory reading denies them a formal veto over legal existence. They are structurally sidelined by the declaratory doctrine's premise but not functionally disempowered — they would object that the doctrine ignores how much recognition still matters in practice, but that objection operates outside the legal-fact frame the declaratory reading asserts.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, existing_states_collectively, excluded,
    organized, generational, arbitrage, global).

% Adjudicate disputes that sometimes turn on whether an entity is a state. Courts and tribunals cite the Montevideo criteria as the applicable test but in practice blend declaratory language with attention to recognition, UN membership, and political context — producing an uneven jurisprudential record that neither cleanly vindicates nor cleanly rejects the pure declaratory reading.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, verifiable, objective test for statehood so that international legal personality does not depend entirely on the shifting political preferences of existing powers — an entity that controls territory, governs a population, and can conduct foreign relations should be able to know, and have others know, what its legal status is without waiting on contested recognition politics.
% TRANSFER_FUNCTION: Moves the power to determine legal statehood away from the collective discretion of existing recognized states and toward the entity claiming to meet the criteria itself (and toward scholars/adjudicators who interpret the criteria) — a transfer of adjudicative authority from political consensus to self-asserted factual demonstration.
% ABSENT_VOICES: Existing states as a body are structurally excluded from having a formal veto under this reading's own premise, even though they retain immense practical leverage; their objection — that legal existence divorced from recognition is hollow if no institution will treat the entity as a member — is heard constantly in practice but has no formal purchase within the declaratory doctrine's own logic.
% DISAPPEARANCE_RATIONALE: If the declaratory reading disappeared as a live doctrine, entities meeting the Montevideo criteria but lacking recognition would lose their strongest legal argument for existing at all in international law; secessionist movements would need to build coalitions for recognition from the outset rather than pointing to objective facts on the ground; parent states would regain a fully consensus-gated veto over breakaway legitimacy; and decades of scholarly and arbitral citation to the declaratory test would need to be reworked around a purely constitutive framework.
% FOUNDING_PROBLEM: Early 20th-century international law needed a principled way to determine when a new political entity had become a state, rather than leaving the question entirely to the political convenience of existing powers who might withhold recognition for reasons unconnected to the entity's actual capacity to govern and engage internationally — the 1933 Montevideo Convention codified objective criteria partly in reaction to recognition being used as a tool of political leverage rather than legal assessment.
% FOUNDING_PROBLEM_CORROBORATION: Positivist international law scholars and some arbitral tribunals attest the founding problem remains live — recognition politics still distorts legal outcomes (e.g., Kosovo, Taiwan, Somaliland). Foreign ministries of major recognized states and constitutive-reading scholars attest that state practice has never actually abandoned recognition as functionally decisive, so the declaratory doctrine solves a problem on paper that international institutions never stopped gating on political consensus in practice — corroboration for the 'still live' claim comes disproportionately from entities that would benefit from the declaratory reading being true, which is itself a signal to weigh.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).
:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42, rising slowly over the interval as the doctrine has been repeatedly invoked by breakaway and long-unrecognized entities (Somaliland, Transnistria, various post-Soviet unrecognized states) whose claims the international system has largely continued to route through recognition politics anyway — the doctrine transfers formal authority away from the state community without transferring practical authority, generating a persistent extraction of legitimacy-claims that the doctrine cannot cash out. Suppression is moderate (0.38): the doctrine is not coercively enforced against dissenters, but existing states retain substantial practical leverage (non-membership in international bodies, non-participation in treaties) that functions as a soft suppressive counterweight to the doctrine's formal claims. Theater ratio is low-moderate (0.28) reflecting that the doctrine performs real adjudicative work in some arbitral and scholarly contexts even as its practical bite is often symbolic. Accessibility collapse is moderate (0.45) — alternative framings (constitutive, hybrid) remain fully live and contested, unlike a true mountain where alternatives have collapsed. Resistance is fairly high (0.62): the doctrine is actively contested by states, by constitutive-reading scholars, and by the practical behavior of international institutions that have never fully implemented it.
 *
 * DIRECTIONALITY LOGIC:
 *   De facto authorities meeting the criteria and secessionist movements with territorial control are structural beneficiaries — the doctrine hands them a legal argument they would not otherwise have, independent of political consensus. Positivist scholars benefit intellectually and professionally from the doctrine's continued authority. Parent states facing secession are payers — they lose a formal veto they held under the constitutive framework, even though they retain substantial informal leverage. Recognition-dependent populations and unrecognized states meeting the criteria are payers in a distinct sense: the doctrine promises them legal existence but the institutional architecture around them does not honor that promise, producing a gap between promised and delivered status that they bear the cost of living inside. Existing states collectively are excluded from formal input but not functionally disempowered — this asymmetry (formally sidelined, practically still dominant) is the central structural tension of the declaratory reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — recognition being weaponized as pure political leverage divorced from actual capacity to govern — remains genuinely live in specific cases (Kosovo, Taiwan) where the objective criteria are unambiguously met but recognition is withheld for reasons entirely unconnected to governance capacity. This argues against treating the doctrine as pure mandatrophy. But the doctrine has not solved the problem it names: recognition politics continues to determine practical statehood outcomes regardless of what the declaratory test says, which means the doctrine's mandate (make statehood a self-executing fact) is only partially fulfilled and the gap between its promise and its delivery is exactly what falls on recognition-dependent populations and unrecognized states. Classifying this as tangled_rope rather than snare or mountain captures both halves: there is a genuine coordination function (an objective, verifiable test reduces arbitrariness) and a genuine extraction dynamic (the doctrine's beneficiaries get a legal argument while its costs land on populations whose practical situation the doctrine cannot actually improve).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaratory_versus_constitutive_empirical_status,
    'Does state practice actually operate on the declaratory model, or does it operate on the constitutive model while citing declaratory language as legitimating cover?',
    'Systematic study of cases where entities meet the Montevideo criteria but are denied UN membership, treaty access, or diplomatic relations — if practical statehood outcomes track recognition rather than criteria-satisfaction, the declaratory doctrine is largely rhetorical even where formally invoked.',
    'If state practice is really constitutive despite declaratory rhetoric, the declaratory reading''s coordination claim (objective, verifiable, self-executing statehood) is substantially overstated, and the extraction this story documents (promised status not delivered) is closer to structural than incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_versus_constitutive_empirical_status, empirical, 'Whether the declaratory doctrine describes actual international practice or merely its own aspirational self-description.').

omega_variable(
    kernel_reading_selection_criterion,
    'What determines which of the three kernel readings (declaratory, constitutive, hybrid) governs a given statehood dispute, and is that selection itself principled or political?',
    'Comparative analysis of which reading tribunals, foreign ministries, and scholars invoke in specific disputes (Kosovo, Somaliland, Taiwan, South Sudan, Catalonia) and whether the selection correlates with the political interests of the invoking party rather than with a principled jurisprudential commitment.',
    'If reading-selection tracks interest rather than principle, all three kernel readings function partly as post-hoc legitimating vocabularies for outcomes reached on other grounds, which would recharacterize the entire kernel contest as itself a site of extraction rather than a genuine doctrinal disagreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_criterion, conceptual, 'Whether selection among the three kernel readings is principled or strategically deployed.').

omega_variable(
    self_executing_legal_fact_versus_constructed_status,
    'Is ''meeting the criteria establishes statehood as a legal fact'' a genuine discovery of an objective legal reality, or is the criteria-based test itself a constructed convention that happens to favor entities capable of demonstrating territorial and governmental control by force or endurance?',
    'Historical analysis of why these four criteria (rather than others, e.g. population consent, historical continuity, or normative legitimacy) were selected in 1933, and whose interests that selection served at the time.',
    'If the criteria were selected to favor incumbent-capable entities (those already exercising effective control, often through force) over popular-sovereignty or consent-based alternatives, the declaratory reading''s claim to be merely describing objective fact is itself a beneficiary-serving framing, strengthening the tangled_rope classification over a purer coordination account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_executing_legal_fact_versus_constructed_status, conceptual, 'Whether the objective-criteria test is a neutral discovery or a construction favoring entities with effective territorial control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 1933, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1933, 0.15).
narrative_ontology:measurement(mont_tr_t1950, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(mont_tr_t1975, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(mont_tr_t1991, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1991, 0.24).
narrative_ontology:measurement(mont_tr_t2008, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2008, 0.26).
narrative_ontology:measurement(mont_tr_t2025, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1933, 0.22).
narrative_ontology:measurement(mont_be_t1950, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement(mont_be_t1975, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1975, 0.33).
narrative_ontology:measurement(mont_be_t1991, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1991, 0.37).
narrative_ontology:measurement(mont_be_t2008, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement(mont_be_t2025, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1933, 0.2).
narrative_ontology:measurement(mont_su_t1950, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(mont_su_t1975, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement(mont_su_t1991, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1991, 0.34).
narrative_ontology:measurement(mont_su_t2008, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2008, 0.36).
narrative_ontology:measurement(mont_su_t2025, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'the Montevideo statehood criteria' per the ε-invariance principle: montevideo_statehood_criteria__declaratory_reading (this file), montevideo_statehood_criteria__constitutive_reading, and montevideo_statehood_criteria__hybrid_reading. Each reading has a distinct beneficiary/victim structure and a distinct ε — the declaratory reading benefits de facto authorities and disadvantages parent states and recognition-dependent populations; the constitutive reading inverts much of this by restoring parent-state/existing-state leverage; the hybrid reading adds normative-legitimacy gatekeeping that produces yet another beneficiary/victim configuration. All three are linked bidirectionally in network.affects_constraints because a shift in state practice or scholarly consensus toward any one reading structurally pressures the operative legitimacy of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
