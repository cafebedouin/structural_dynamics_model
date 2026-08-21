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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Dueling's Cultural Unthinkability (Dignity-Culture Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint is the 'contraction_reading' of the
 *   'dueling_disappearance_mechanism' kernel. It posits that dueling became
 *   culturally unthinkable due to the fundamental displacement of
 *   honor-culture axioms by the rise of dignity culture. Sibling readings
 *   include 'institutional_displacement_reading' (emphasizing legal/state
 *   action) and 'overdetermined_composite_reading' (arguing for multiple
 *   simultaneous causes). The claimed type is 'mountain' because the dignity
 *   culture is presented as an irreversible cultural substrate, making
 *   dueling unthinkable rather than merely illegal.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.85).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.9).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dueling's Cultural Unthinkability (Dignity-Culture Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '490b0ebd-4975-4af0-b66c-e0e80dc7f1f6').
narrative_ontology:cs_kernel_codification('490b0ebd-4975-4af0-b66c-e0e80dc7f1f6', implicit).
narrative_ontology:cs_authority_grounding('490b0ebd-4975-4af0-b66c-e0e80dc7f1f6', practice).
narrative_ontology:cs_interpretation_layer_present('490b0ebd-4975-4af0-b66c-e0e80dc7f1f6').
narrative_ontology:cs_reading_relation('490b0ebd-4975-4af0-b66c-e0e80dc7f1f6', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('490b0ebd-4975-4af0-b66c-e0e80dc7f1f6', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('490b0ebd-4975-4af0-b66c-e0e80dc7f1f6', foundational, individual_intrinsic_worth).
narrative_ontology:cs_axiom_status(individual_intrinsic_worth, holdable).
narrative_ontology:cs_axiom_grounding('490b0ebd-4975-4af0-b66c-e0e80dc7f1f6', individual_intrinsic_worth, deontological).
narrative_ontology:cs_axiom('490b0ebd-4975-4af0-b66c-e0e80dc7f1f6', foundational, violence_as_illegitimate_dispute_resolution).
narrative_ontology:cs_axiom_status(violence_as_illegitimate_dispute_resolution, holdable).
narrative_ontology:cs_axiom_grounding('490b0ebd-4975-4af0-b66c-e0e80dc7f1f6', violence_as_illegitimate_dispute_resolution, conventional).
narrative_ontology:cs_reference_frame('490b0ebd-4975-4af0-b66c-e0e80dc7f1f6', honor_culture_legitimacy_framework).
narrative_ontology:cs_drift_state('490b0ebd-4975-4af0-b66c-e0e80dc7f1f6', post_dignity_culture_ascendance, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('490b0ebd-4975-4af0-b66c-e0e80dc7f1f6', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, state_legal_systems).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and groups who embrace the intrinsic worth of all persons, regardless of social standing, and for whom violence as a means of dispute resolution is morally and socially unacceptable. They benefit from a more stable and less violent social order.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents, beneficiary,
    powerful, generational, mobile, universal).

% Individuals and groups whose social standing and self-worth were tied to a system of honor that required public defense, often through dueling. The rise of dignity culture rendered their framework illegible and their practices unthinkable, effectively extracting their social capital and means of redress.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerless, biographical, identity_locked, regional).

% Government and judicial bodies that historically sought to suppress dueling through legal prohibition. The cultural shift to dignity culture reinforced their authority and reduced the social pressure to tolerate or ignore dueling, benefiting from a more orderly society.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, state_legal_systems, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__contraction_reading, state_legal_systems, beneficiary).

% Academics and researchers who analyze the historical and cultural forces that led to the decline of dueling, seeking to understand the mechanisms of social change and the interplay between cultural norms and legal frameworks.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dignity culture coordinates social interactions and dispute resolution by establishing a universal baseline of intrinsic human worth, making honor-based challenges and violence culturally unacceptable and providing alternative, legal avenues for redress.
% TRANSFER_FUNCTION: Transfers social legitimacy and authority over dispute resolution from individual honor-based systems to collective, legal, and dignity-based frameworks. It extracts the social currency of honor-based violence from practitioners and reallocates it to state-sanctioned justice.
% ABSENT_VOICES: The voices of those deeply embedded in honor culture, for whom personal reputation and public challenge were paramount, became increasingly marginalized and unheard as the dignity framework solidified. Their grievances, once legitimate, lost their social recognition.
% DISAPPEARANCE_RATIONALE: The shift from an honor-based to a dignity-based culture fundamentally reordered social relations, legal systems, and individual self-conception. If this cultural shift had not occurred, society would operate on entirely different principles of conflict, justice, and personal value.
% FOUNDING_PROBLEM: The inherent instability, violence, and social disruption caused by a system of honor that often required lethal combat to resolve perceived slights or defend reputation.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and cultural anthropologists corroborate the problem of violence and social instability in honor cultures. Contemporary legal systems and human rights frameworks continue to address the problem of violence, albeit in different forms, reinforcing the ongoing relevance of the underlying issue.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

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
 *   The high extractiveness (0.85) and suppression (0.9) reflect the complete displacement of the honor-culture framework, rendering its practices socially impossible and extracting its legitimacy. Accessibility collapse is near total (0.95) as dueling became 'unthinkable.' Resistance is low (0.2) because, while there was initial pushback, the cultural shift was ultimately overwhelming. The theater ratio is very low (0.05) as this is a fundamental cultural transformation, not a performative maintenance of an atrophied function. The measurements show a clear upward trend in extractiveness and suppression, reflecting the increasing dominance of dignity culture over the specified historical interval.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of dignity culture adherents, the decline of dueling is a natural and beneficial social evolution, a 'mountain' of moral progress. From the perspective of honor culture practitioners, it was a profound loss of a legitimate social framework, an 'extraction' of their way of life. The engine's classification will reflect this asymmetry based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity culture adherents and state legal systems are beneficiaries, as the new cultural substrate aligns with their values and strengthens legal authority. Honor culture practitioners are victims, as their entire social and moral framework was rendered obsolete and their means of redress suppressed. Their 'identity_locked' exit option reflects the deep personal and social investment in the honor system, making exit from its logic extremely difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'mandate' to prevent dueling is resolved, not by institutional atrophy, but by a fundamental cultural shift that made the practice unthinkable. The constraint's persistence is not due to inertia, but to the deep entrenchment of dignity-culture axioms. The founding problem (violence from honor culture) is still 'live' in a broader sense, but its specific manifestation (dueling) has been culturally superseded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fsm_dignity_culture_naturalness,
    'Is the dignity culture''s displacement of honor culture a genuine natural evolution of social norms (Mountain), or a constructed cultural hegemony that benefits identifiable groups (False Summit)?',
    'Comparative historical analysis of other cultural shifts, examining the role of power dynamics and elite interests in promoting dignity-based norms versus spontaneous, bottom-up emergence.',
    'If found to be a constructed hegemony, the constraint would reclassify from Mountain to a more extractive type (e.g., Tangled Rope or Snare), highlighting the beneficiaries of the new cultural order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_dignity_culture_naturalness, conceptual, 'Ambiguity regarding the naturalness vs. constructedness of dignity culture''s rise.').

omega_variable(
    reading_institutional_displacement_delta,
    'How would the classification change if the ''institutional_displacement_reading'' were adopted, emphasizing legal and state action over cultural shift?',
    'Re-evaluating the primary causal mechanism based on historical evidence, focusing on the direct impact of legal prohibitions and the rise of alternative dispute resolution mechanisms.',
    'The constraint might shift from Mountain to a more actively enforced type (e.g., Tangled Rope or Snare), with state legal systems as the primary agenda-setters and beneficiaries, and a higher ''requires_active_enforcement'' flag.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_institutional_displacement_delta, conceptual, 'Impact of emphasizing institutional changes over cultural shifts in dueling''s decline.').

omega_variable(
    reading_overdetermined_composite_delta,
    'How would the classification change if the ''overdetermined_composite_reading'' were adopted, arguing for multiple simultaneous causes?',
    'Synthesizing evidence for legal, institutional, and cultural factors, and assessing their relative contributions to dueling''s decline.',
    'This reading would likely lead to a more complex constraint family, with multiple linked constraints representing different causal pathways, each with its own classification, rather than a single dominant mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_overdetermined_composite_delta, conceptual, 'Impact of adopting a multi-causal explanation for dueling''s decline.').

omega_variable(
    causal_mechanism_location_of_disagreement,
    'What is the primary causal mechanism for dueling''s decline: cultural displacement, institutional substitution, or a composite of factors?',
    'Further historical and sociological research, potentially using counterfactual analysis or comparative case studies to isolate the impact of different causal pathways.',
    'The choice of primary mechanism fundamentally alters the constraint''s structural properties, particularly its ''claimed_type'', ''extractiveness'', and ''suppression'' values, and the identification of key beneficiaries and victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_mechanism_location_of_disagreement, empirical, 'Disagreement over the primary causal mechanism for dueling''s disappearance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1700, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(duel_tr_t1733, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1733, 0.08).
narrative_ontology:measurement(duel_tr_t1766, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1766, 0.07).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1800, 0.06).
narrative_ontology:measurement(duel_tr_t1833, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1833, 0.05).
narrative_ontology:measurement(duel_tr_t1866, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1866, 0.05).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.05).

% Extraction over time
narrative_ontology:measurement(duel_be_t1700, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1700, 0.4).
narrative_ontology:measurement(duel_be_t1733, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1733, 0.55).
narrative_ontology:measurement(duel_be_t1766, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1766, 0.7).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1800, 0.8).
narrative_ontology:measurement(duel_be_t1833, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1833, 0.83).
narrative_ontology:measurement(duel_be_t1866, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1866, 0.84).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1700, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement(duel_su_t1733, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1733, 0.5).
narrative_ontology:measurement(duel_su_t1766, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1766, 0.7).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement(duel_su_t1833, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1833, 0.88).
narrative_ontology:measurement(duel_su_t1866, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1866, 0.89).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
