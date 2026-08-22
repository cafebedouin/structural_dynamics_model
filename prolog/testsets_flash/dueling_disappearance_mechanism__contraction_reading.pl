% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity Culture's Displacement of Dueling
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint describes the cultural shift where dueling became
 *   unthinkable due to the ascendance of 'dignity culture' over 'honor
 *   culture'. In this reading, dignity culture acts as a fundamental, almost
 *   'natural law' substrate, making the very concept of dueling illegitimate.
 *   The constraint is claimed as a Mountain because the cultural shift is
 *   presented as irreversible and foundational, making dueling's
 *   disappearance a consequence of a changed social ontology rather than mere
 *   institutional or legal prohibition. The beneficiaries are those who
 *   thrive in a dignity-based society, while the victims are those whose
 *   honor-bound identities became culturally illegible.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.05).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.95).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity Culture's Displacement of Dueling").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, 'a845489f-3c69-4b69-b80c-602451b3b1bf').
narrative_ontology:cs_kernel_codification('a845489f-3c69-4b69-b80c-602451b3b1bf', implicit).
narrative_ontology:cs_authority_grounding('a845489f-3c69-4b69-b80c-602451b3b1bf', practice).
narrative_ontology:cs_interpretation_layer_present('a845489f-3c69-4b69-b80c-602451b3b1bf').
narrative_ontology:cs_reading_relation('a845489f-3c69-4b69-b80c-602451b3b1bf', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_reading_relation('a845489f-3c69-4b69-b80c-602451b3b1bf', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('a845489f-3c69-4b69-b80c-602451b3b1bf', foundational, individual_inherent_worth).
narrative_ontology:cs_axiom_status(individual_inherent_worth, holdable).
narrative_ontology:cs_axiom_grounding('a845489f-3c69-4b69-b80c-602451b3b1bf', individual_inherent_worth, deontological).
narrative_ontology:cs_axiom('a845489f-3c69-4b69-b80c-602451b3b1bf', secondary, state_monopoly_on_violence).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence, holdable).
narrative_ontology:cs_axiom_grounding('a845489f-3c69-4b69-b80c-602451b3b1bf', state_monopoly_on_violence, conventional).
narrative_ontology:cs_reference_frame('a845489f-3c69-4b69-b80c-602451b3b1bf', dignity_culture_ascendant).
narrative_ontology:cs_drift_state('a845489f-3c69-4b69-b80c-602451b3b1bf', contemporary_globalized_society, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a845489f-3c69-4b69-b80c-602451b3b1bf', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and institutions whose moral framework emphasizes inherent individual worth, legal rights, and self-restraint, making dueling unthinkable. They benefit from a social order where disputes are resolved through legal and civil means, not violence.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents, beneficiary,
    institutional, generational, analytical, national).

% Individuals who historically adhered to a code where personal honor was paramount and required violent defense against perceived slights. Their framework became culturally illegible and socially sanctioned, forcing them to abandon core identity tenets or face ostracization.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerless, biographical, identity_locked, local).

% The formal institutions that codified anti-dueling laws and enforced them, reflecting and reinforcing the shift towards dignity culture. They provide alternative dispute resolution mechanisms.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% Academics who study the historical evolution of cultural norms, honor codes, and the mechanisms of social change. They analyze the interplay between cultural shifts and institutional changes.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, social_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interactions around a shared understanding of individual worth and the appropriate means of dispute resolution, replacing a system based on personal honor and violent redress.
% TRANSFER_FUNCTION: Transfers the right to violent self-redress from individuals to the state's legal system, and shifts social capital from those who defend honor violently to those who adhere to dignity norms.
% ABSENT_VOICES: The 'ghosts' of honor-culture adherents, whose entire social and moral framework was rendered obsolete and illegitimate. They would argue for the necessity of personal honor defense in a world where state protection was insufficient or dishonorable.
% DISAPPEARANCE_RATIONALE: This constraint describes a fundamental cultural substrate. If the 'dignity culture' framework were to disappear overnight, it would imply a collapse of modern legal and social systems, leading to a complete societal reorganization, not merely a return to dueling. The constraint itself is the new 'natural' state.
% FOUNDING_PROBLEM: Honor cultures generated cycles of violence and instability, where personal slights escalated into deadly encounters, undermining state authority and civil order.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists widely corroborate that the problem of honor-based violence was a significant driver for the rise of state-backed legal systems and the cultural shift towards dignity. Contemporary legal frameworks and social norms attest to the problem being largely resolved, with dueling now seen as an anachronism.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.05) because dignity culture, in this reading, is not primarily about extracting resources but about establishing a new, less violent social order. Suppression is extremely high (0.95) because the honor-culture framework was not merely regulated but actively suppressed and delegitimized, making alternatives (like dueling) culturally unthinkable. Accessibility collapse is high (0.9) as the very conceptual space for dueling vanished. Resistance is low (0.05) because the shift was so profound that active resistance to the new cultural paradigm became marginal. The temporal measurements show a steady increase in suppression as dignity culture solidified its dominance, and a decrease in extractiveness as the 'cost' of the cultural shift became less about active extraction and more about the 'natural' state of affairs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of dignity culture adherents, the constraint is a natural evolution towards a more civilized society, a Mountain. From the perspective of honor culture practitioners, it was a profound and violent suppression of their way of life, a Snare. This story adopts the 'Mountain' framing of the dominant dignity culture, with the high suppression and victim declarations reflecting the cost to the displaced honor culture.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity culture adherents are beneficiaries (d near 0.0) as the new cultural framework aligns with their values and provides a stable social order. Honor culture practitioners are victims (d near 1.0) because their core identity and means of redress were rendered illegitimate and suppressed. Legal systems act as agenda-setters, codifying and enforcing the new cultural norms, benefiting from increased state authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_institutional_causality,
    'Was the decline of dueling primarily a cultural shift (dignity displacing honor) or an institutional one (legal systems and courts displacing dueling as a dispute resolution mechanism)?',
    'Comparative historical analysis of societies with similar legal reforms but different cultural trajectories, or vice versa. Examining the timing of cultural shifts relative to legal enforcement.',
    'If primarily institutional, this constraint would be reclassified as a Tangled Rope or Snare, with legal systems as agenda-setters actively extracting the right to violence. If primarily cultural, the Mountain classification holds, with legal systems as reflections of the cultural shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_institutional_causality, conceptual, 'Ambiguity in the primary causal mechanism for dueling''s disappearance.').

omega_variable(
    naturalness_of_dignity_culture,
    'Is ''dignity culture'' truly a natural, irreversible substrate (a Mountain), or is it a constructed and contingent social arrangement that could be challenged or reversed (a Snare or Tangled Rope)?',
    'Long-term historical observation of cultural resilience and reversibility, or cross-cultural comparison with societies that have resisted or reversed similar shifts. Analysis of active enforcement mechanisms still required to maintain dignity norms.',
    'If dignity culture is found to be a contingent construct, the ''Mountain'' classification would be a false summit, reclassifying to a Tangled Rope or Snare, with higher extractiveness and suppression attributed to its maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_dignity_culture, conceptual, 'The naturalness and irreversibility of dignity culture as a social substrate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1820, 0.1).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1840, 0.08).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1860, 0.06).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1880, 0.05).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(duel_su_t1820, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1820, 0.78).
narrative_ontology:measurement(duel_su_t1840, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1840, 0.85).
narrative_ontology:measurement(duel_su_t1860, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1860, 0.9).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1880, 0.93).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
