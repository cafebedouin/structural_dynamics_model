% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__contraction_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Honor Satisfaction Mechanism (Contraction Reading): Dueling as Cognitive Impossibility
 *   domain: social/normative/historical
 *
 * SUMMARY:
 *   This reading treats dueling as a cognitive category — a conceptually
 *   available mechanism for honor satisfaction — that contracted into
 *   impossibility rather than being suppressed through policy or enforcement.
 *   The contraction reading's core claim is that by the early 20th century,
 *   dueling had become literally unthinkable as a practice, not because of
 *   law enforcement or moral persuasion, but because the entire framework of
 *   honor satisfaction that made dueling intelligible had undergone a
 *   structural shift. What was once a rational response to an honor offense
 *   became a category error: one simply could not understand oneself as
 *   engaging in dueling in the post-industrial social order. The constraint
 *   here is the category itself — the availability of dueling as a conceptual
 *   possibility. This reading does not model dueling as an extractive
 *   institution with suppression costs; it models the disappearance of the
 *   category as a structural transformation in what counts as a thinkable
 *   response to honor injury. The founding problem (credible honor
 *   satisfaction via lethal-risk arbitration) becomes unrecognizable, not
 *   forbidden. Alternative readings (decline, composite) model different
 *   mechanisms entirely: persistent-but-declining practice, state monopoly on
 *   violence, insurance systems, norm shifts. This reading is unique in
 *   treating the phenomenon as a category-level cognitive evacuation.
 *
 * KEY AGENTS:
 *   - aristocratic_honor_practitioners: Historical actors (18th–19th century) who understood dueling as an available mechanism for honor satisfaction
 *   - industrial_bourgeoisie: Emerging professional and commercial classes who operated under different honor framings (reputation, contract enforcement, insurance)
 *   - analytical_observer: The historian or sociologist who observes that the category became unthinkable, not that it was suppressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.0).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.0).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Honor Satisfaction Mechanism (Contraction Reading): Dueling as Cognitive Impossibility").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "social/normative/historical").

domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, 'dbbd1aea-b29f-4b7b-8ccd-175435de3eb7').
narrative_ontology:cs_kernel_codification('dbbd1aea-b29f-4b7b-8ccd-175435de3eb7', distributed).
narrative_ontology:cs_authority_grounding('dbbd1aea-b29f-4b7b-8ccd-175435de3eb7', practice).
narrative_ontology:cs_interpretation_layer_present('dbbd1aea-b29f-4b7b-8ccd-175435de3eb7').
narrative_ontology:cs_reading_relation('dbbd1aea-b29f-4b7b-8ccd-175435de3eb7', honor_satisfaction_mechanism__decline_reading, influences).
narrative_ontology:cs_reading_relation('dbbd1aea-b29f-4b7b-8ccd-175435de3eb7', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('dbbd1aea-b29f-4b7b-8ccd-175435de3eb7', foundational, honor_satisfaction_requires_lethal_risk_credibility).
narrative_ontology:cs_axiom_status(honor_satisfaction_requires_lethal_risk_credibility, overridden).
narrative_ontology:cs_axiom_grounding('dbbd1aea-b29f-4b7b-8ccd-175435de3eb7', honor_satisfaction_requires_lethal_risk_credibility, instrumental).
narrative_ontology:cs_axiom('dbbd1aea-b29f-4b7b-8ccd-175435de3eb7', foundational, dueling_cognitive_category_evacuation).
narrative_ontology:cs_axiom_status(dueling_cognitive_category_evacuation, holdable).
narrative_ontology:cs_axiom_grounding('dbbd1aea-b29f-4b7b-8ccd-175435de3eb7', dueling_cognitive_category_evacuation, conventional).
narrative_ontology:cs_reference_frame('dbbd1aea-b29f-4b7b-8ccd-175435de3eb7', honor_satisfaction_through_lethal_risk_arbitration).
narrative_ontology:cs_drift_state('dbbd1aea-b29f-4b7b-8ccd-175435de3eb7', early_twentieth_century, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('dbbd1aea-b29f-4b7b-8ccd-175435de3eb7', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stakeholders authored EMPTY (Pattern-5: an explicit assertion that no
% entity's arrangements depend on this constraint — paired with the
% world_unchanged verdict below, enforced by the schema).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this reading treats dueling not as a coordination mechanism but as a cognitive category that ceased to be thinkable.
% TRANSFER_FUNCTION: No transfer occurs in this reading; dueling is not modeled as an extractive arrangement but as a possibility that was evacuated from the conceptual space of honor satisfaction.
% ABSENT_VOICES: Practitioners of dueling (historical: aristocrats, military officers) are conceptually excluded once the category dissolves. In contemporary frames, they have no voice because the behavior is unthinkable, not because they are suppressed.
% DISAPPEARANCE_RATIONALE: If dueling as a cognitive category had never contracted (remained thinkable and practice-viable), the world's institutional facts would differ — alternative dispute mechanisms might not have developed, honor satisfaction would operate through different channels. The contraction reading treats this as a category-level shift in what is conceptually available, not as a policy-level suppression that could be reversed. The disappearance of the cognitive category is what this reading IS ABOUT.
% FOUNDING_PROBLEM: Honor disputes required a mechanism for satisfaction that was credible to participants and society: dueling served this function by making lethal risk the arbitrating consequence, which allegedly proved the sincerity of one's honor claim.
% FOUNDING_PROBLEM_CORROBORATION: Historians of honor systems (e.g., Kiernan, Freeman-Grenville, Redding) and historians of law enforcement (e.g., on the transition to professional policing and insurance-based dispute settlement) attest that the founding problem — credible honor satisfaction — no longer exists as a live problem in post-industrial societies. The mechanism is dead because the problem it solved is categorically unavailable, not because enforcement or policy changed.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   In this reading, extractiveness and suppression are both 0.0 because the constraint (dueling as a cognitive category) is not modeled as extractive or suppressive — it is modeled as a category of possibility that ceased to exist. Accessibility collapse is high (0.95) because once the conceptual category is unavailable, there is no option to access dueling; it is not suppressed from the choice set, it is absent from the possibility space. Resistance is near-zero (0.05) not because enforcement is weak but because there is no organized resistance to a category that no longer exists as thinkable. The measurement series are flat because the contraction reading does not track enforcement intensity or extraction accumulation — it models a structural shift in conceptual availability. The constraint is claimed as mountain because it describes a structural feature of post-industrial cognition: dueling is not available as a category, the way a category is not available in mathematics. This is not a natural law (the contraction is contingent on social transformation), but it functions like one in the post-contraction era — it is an unchangeable feature of the landscape once the shift occurs. The structural delta from sibling readings is the critical feature: the decline reading models persistent practice at declining frequency; the composite reading models multiple distinct mechanisms (state monopoly, insurance, norm migration). This reading alone posits category-level evacuation.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no stakeholders in this reading because dueling is not modeled as an arrangement with parties. No one benefits from dueling's disappearance as a category, and no one is oppressed by it. The contraction is a structural fact about conceptual availability, not a distributional fact about who gains and who loses. Historical practitioners (aristocrats) would have experienced suppression if dueling had been banned; but this reading treats the phenomenon as the opposite — not suppression but evacuation from the space of thinkable options. The reading does not authorize directionality analysis because there are no extractive or coordinating relationships to measure.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution is not applicable here. The founding problem (honor satisfaction via dueling) is dead not because the mandate outlived its function, but because the function itself became cognitively unavailable. A mandatrophy case would involve a constraint that persists theatrically after its founding problem is solved — e.g., a licensing ritual that no longer serves quality assurance but persists through bureaucratic inertia. Dueling's contraction is the opposite: the problem is unthinkable, so the mechanism that addressed it is unthinkable too. There is no zombie constraint, no performance, no gap between function and persistence. The constraint simply ceased to exist as a cognitive category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_suppression_ambiguity,
    'Is the unavailability of dueling as a cognitive category the result of genuine category-level contraction (the framework of honor satisfaction itself shifted to make dueling unthinkable), or is it the result of effective suppression that created the appearance of category-level shift?',
    'Textual and archival analysis of how late-19th and early-20th century practitioners conceptualized honor disputes: did they describe attempts to duel that were legally or socially blocked (suppression framing), or did they describe the concept itself as no longer making sense (contraction framing)? Analysis of counterfactual reasoning: did the absence of dueling persist because the option was unavailable, or because available alternatives better served the same function?',
    'If suppression interpretation is correct, the constraint should be reclassified as a snare (dueling was suppressed through law and social pressure, but the desire to duel persisted). If contraction interpretation is correct, the classification as mountain (cognitive impossibility) holds. The difference is whether dueling became unthinkable (contraction) or merely unenforceable (suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_suppression_ambiguity, conceptual, 'Whether dueling''s disappearance was cognitive contraction or effective suppression that created the appearance of contraction.').

omega_variable(
    honor_framework_simultaneity,
    'Did the honor framework that made dueling intelligible contract as a single conceptual shift, or did the shift occur across different social classes and communities at different times, with the appearance of simultaneity being historiographical aggregation?',
    'Fine-grained historical analysis of honor practices across class and geographic boundaries: trace how long dueling remained intelligible to aristocratic, military, and professional communities, and when each community abandoned the category. Compare rates of adoption of alternative honor mechanisms (reputation systems, insurance, legal remedies, formal apology rituals).',
    'If the shift was simultaneous and structural, the mountain classification holds (a category-level transformation). If the shift was gradual and differential across communities, the decline reading becomes more plausible — dueling did not become unthinkable universally but became fringe and unthinkable within dominant institutional spaces. The contraction reading requires a relatively sharp transition; a prolonged differential shift would support composite or decline framings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_framework_simultaneity, empirical, 'Whether the category-shift was simultaneous across social structures or differential and gradual.').

omega_variable(
    cognitive_category_as_constraint_referent,
    'Is a cognitive category (dueling as a thinkable option) a valid referent for the constraint framework''s definition of a constraint? Or does treating cognitive availability as a constraint reframe the entire system in a way that conflicts with the framework''s grounding in institutional and material relationships?',
    'Meta-level: review the framework''s definition of constraints (arrangements of benefit, cost, coordination, extraction, enforcement). Dueling in the contraction reading fits none of these — it is a pure negation (unavailability). The resolution involves either accepting that constraint scope extends to cognitive-category availability, or reclassifying the phenomenon as outside the constraint framework''s domain (a historical transformation, not a constraint).',
    'If cognitive categories are valid constraint referents, the contraction reading is coherent. If constraints must model institutional or material relationships, the phenomenon is better captured by the decline or composite readings, which model actual practices and enforcement mechanisms. This is a preference-level question about the scope of the framework, not an empirical or conceptual dispute about the historical facts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_category_as_constraint_referent, preference, 'Whether cognitive-category availability is a valid constraint referent or whether constraints must model institutional/material relationships.').

omega_variable(
    sibling_reading_foreclosure,
    'Do the core premises of the contraction reading logically foreclose the decline and composite readings, or do all three readings remain simultaneously viable framings of the historical evidence?',
    'Logical analysis: the contraction reading claims dueling became unthinkable (category-level evacuation). The decline reading claims dueling persisted at declining frequency (practice-level suppression). The composite reading claims multiple mechanisms (state monopoly, norms, insurance, category shift) combined. Either contraction forecloses the other two (if category unavailability rules out persistent practice), or the readings are compatible (if category unavailability and persistent-but-marginal practice can coexist, e.g., among fringe populations). This determines the reading_relations classification.',
    'The reading_relations classification in cs_structure depends on this resolution. Foreclosure is rare and structurally strong; coexistence is common when readings represent different factions or observational frames; influence is intermediate. This omega directly determines the schema output for reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between contraction reading''s premise and sibling readings'' premises.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 1700, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1700, 0.0).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1800, 0.0).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1850, 0.0).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(hono_tr_t1950, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1950, 0.0).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1700, 0.0).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1800, 0.0).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1850, 0.0).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1900, 0.0).
narrative_ontology:measurement(hono_be_t1950, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1950, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1700, 0.0).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1800, 0.0).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1850, 0.0).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1900, 0.0).
narrative_ontology:measurement(hono_su_t1950, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1950, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, information_standard).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the honor_satisfaction_mechanism kernel family. The kernel is the standing commitment to what counts as a legitimate way to satisfy an honor offense. The contraction_reading (this story) models dueling as a cognitive category that became unthinkable. The decline_reading models dueling as a practice that persisted at declining frequency under enforcement and norm pressure. The composite_reading models multiple distinct causal mechanisms (state monopoly, insurance systems, bourgeois norms, category-shift) operating together. All three stories share the kernel but instantiate different ε values and different mechanisms. The contraction reading's ε is 0.0 because the constraint (cognitive availability of dueling) is not extractive; the decline reading's ε would be higher (dueling was suppressed practice); the composite reading's ε would model extraction by the state monopoly or insurance systems. Link these stories via network.affects_constraints and treat the contraction reading as upstream (category-level shift explains why decline and composite mechanisms operated); the decline and composite readings are downstream (they model the empirical details of how suppression and alternative mechanisms took over).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
