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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Dignity Culture's Displacement of Dueling (Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint represents the cultural shift by which dueling became
 *   unthinkable, framed as the displacement of honor-culture axioms by
 *   dignity-culture axioms. It is a 'contraction reading' of the dueling
 *   disappearance mechanism kernel. The constraint is claimed as a Mountain
 *   because dignity culture, once established, acts as an irreversible
 *   cultural substrate, making dueling culturally illegible rather than
 *   merely illegal. The 'victims' are not actively extracted from in the
 *   present, but represent the historical agents whose entire framework for
 *   social interaction and self-worth was rendered obsolete and illegitimate
 *   by this cultural shift.
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
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity Culture's Displacement of Dueling (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '9c457bb7-3298-4a19-a581-6fe362756d02').
narrative_ontology:cs_kernel_codification('9c457bb7-3298-4a19-a581-6fe362756d02', implicit).
narrative_ontology:cs_authority_grounding('9c457bb7-3298-4a19-a581-6fe362756d02', practice).
narrative_ontology:cs_interpretation_layer_present('9c457bb7-3298-4a19-a581-6fe362756d02').
narrative_ontology:cs_reading_relation('9c457bb7-3298-4a19-a581-6fe362756d02', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c457bb7-3298-4a19-a581-6fe362756d02', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('9c457bb7-3298-4a19-a581-6fe362756d02', foundational, individual_dignity_is_inherent).
narrative_ontology:cs_axiom_status(individual_dignity_is_inherent, holdable).
narrative_ontology:cs_axiom_grounding('9c457bb7-3298-4a19-a581-6fe362756d02', individual_dignity_is_inherent, deontological).
narrative_ontology:cs_axiom('9c457bb7-3298-4a19-a581-6fe362756d02', foundational, state_monopoly_on_violence_is_legitimate).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('9c457bb7-3298-4a19-a581-6fe362756d02', state_monopoly_on_violence_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('9c457bb7-3298-4a19-a581-6fe362756d02', dignity_culture_ascendant).
narrative_ontology:cs_drift_state('9c457bb7-3298-4a19-a581-6fe362756d02', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9c457bb7-3298-4a19-a581-6fe362756d02', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, state_legal_monopoly).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and institutions whose moral framework emphasizes inherent individual worth, equal protection under law, and the state's monopoly on legitimate violence. They benefit from the cultural illegibility of dueling as a legitimate dispute resolution mechanism.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents, beneficiary,
    institutional, generational, analytical, national).

% The legal and political apparatus of the state, which asserts and benefits from its exclusive right to adjudicate disputes and apply force. The cultural shift away from dueling reinforces its authority.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, state_legal_monopoly, beneficiary,
    institutional, civilizational, analytical, national).

% Individuals who, in an earlier era, would have relied on dueling to defend their honor and social standing. Their entire framework for self-worth and dispute resolution became culturally unintelligible and legally proscribed, leaving them without a recognized means to address perceived slights within their own value system.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Academics who study the historical evolution of cultural norms and legal systems, analyzing the mechanisms by which practices like dueling became obsolete. They interpret historical evidence to reconstruct the cultural shifts.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_sociologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates social interactions by establishing a universal framework of individual dignity and legal recourse, replacing honor-based challenges with state-sanctioned dispute resolution.
% TRANSFER_FUNCTION: It transfers the right to adjudicate grievances and apply retributive force from individuals to the state, and shifts the basis of social standing from honor (defended by violence) to dignity (protected by law).
% ABSENT_VOICES: The 'voice' of the honor culture itself, as a coherent system of meaning, is absent from contemporary discourse. Its practitioners, if they could speak from their historical context, would argue for the necessity of dueling for maintaining social order and personal integrity within their framework.
% DISAPPEARANCE_RATIONALE: If the cultural constraint against dueling vanished overnight, the world would remain largely unchanged because the underlying dignity-culture substrate is so deeply embedded. Dueling would not spontaneously reappear as a legitimate practice; its cultural foundations have eroded too completely.
% FOUNDING_PROBLEM: The problem of unchecked violence and private justice inherent in honor cultures, where personal slights could escalate into deadly encounters outside state control.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars corroborate that the problem of private violence was a significant driver for the development of state legal monopolies and the promotion of dignity-based social norms. The state's continued efforts to prevent vigilantism attest to the ongoing nature of this problem.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.05) because dignity culture, in this reading, is not primarily an extractive mechanism but a foundational shift in moral ontology. Suppression is very high (0.95) because the cultural norms of dignity culture actively suppress the very conceptual space for dueling to exist as a legitimate practice. Theater ratio is zero (0.0) as there is no performative maintenance; the constraint operates as a deep cultural background. Accessibility collapse is near total (0.98) as the cultural alternatives (honor-based dispute resolution) have collapsed. Resistance is negligible (0.02) because the shift is so fundamental that active resistance to the dignity-culture framework itself is rare.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of dignity-culture adherents, the constraint is a natural and beneficial evolution, a Mountain of moral progress. From the historical perspective of honor-culture practitioners, it represents the complete collapse of their social world, a form of cultural suppression that renders their very identity illegible. The engine's classification as Mountain reflects the current, dominant cultural perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity culture adherents and the state legal monopoly are beneficiaries, as the cultural shift reinforces their moral and legal frameworks. Honor culture practitioners are 'victims' in a historical sense; their identity was tied to a system that became culturally foreclosed, leaving them identity_locked to an obsolete framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_institutional_causality,
    'To what extent was the decline of dueling primarily a cultural shift (dignity displacing honor) versus an institutional displacement (courts, libel law outcompeting dueling)?',
    'Comparative historical analysis across societies with differing rates of institutional modernization but similar cultural shifts, or vice versa. Quantitative historical sociology correlating legal changes with cultural indicators.',
    'If primarily cultural, this ''contraction_reading'' as a Mountain is strengthened. If primarily institutional, the ''institutional_displacement_reading'' (likely a Tangled Rope or Snare) gains explanatory power, and this constraint''s ''emerges_naturally'' claim would be weakened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_vs_institutional_causality, empirical, 'Distinguishing cultural vs. institutional drivers of dueling''s decline.').

omega_variable(
    irreversibility_of_dignity_culture,
    'Is the displacement of honor-culture axioms by dignity-culture axioms truly irreversible, making this a Mountain, or could a societal crisis lead to a resurgence of honor-based practices?',
    'Longitudinal cultural studies tracking responses to severe societal stress (e.g., state collapse, prolonged civil conflict) for evidence of honor-culture revival. Theoretical analysis of cultural phase transitions.',
    'If reversible, the ''Mountain'' classification is too strong, and the constraint might be reclassified as a deeply embedded Rope or even a Snare (if maintained by active suppression of honor-based alternatives). If irreversible, the Mountain classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_of_dignity_culture, conceptual, 'Assessing the true irreversibility of dignity culture''s dominance.').

omega_variable(
    victimhood_of_honor_culture_practitioners,
    'Is it appropriate to classify historical ''honor_culture_practitioners'' as victims, given that the shift to dignity culture is widely seen as moral progress?',
    'This is a conceptual question. Resolution depends on the adopted ethical framework: whether ''victim'' status can apply to those whose cultural framework is rendered obsolete by a ''progressive'' shift, or if it implies active, unjust extraction.',
    'If ''victim'' status is rejected, the constraint''s beneficiary/victim structure changes, potentially altering its classification from a Mountain with FSM potential to a pure Mountain (if no other beneficiaries are identified) or a Rope. If accepted, it highlights the costs of cultural evolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victimhood_of_honor_culture_practitioners, preference, 'Conceptual debate on applying ''victim'' status to historical cultural displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1800, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1800, 0.0).
narrative_ontology:measurement(duel_tr_t1825, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1825, 0.0).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1850, 0.0).
narrative_ontology:measurement(duel_tr_t1875, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1875, 0.0).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(duel_tr_t1925, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1925, 0.0).
narrative_ontology:measurement(duel_tr_t1950, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1950, 0.0).

% Extraction over time
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(duel_be_t1825, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1825, 0.1).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1850, 0.08).
narrative_ontology:measurement(duel_be_t1875, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1875, 0.06).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(duel_be_t1925, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1925, 0.05).
narrative_ontology:measurement(duel_be_t1950, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1950, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(duel_su_t1825, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1825, 0.75).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1850, 0.8).
narrative_ontology:measurement(duel_su_t1875, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1875, 0.88).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.92).
narrative_ontology:measurement(duel_su_t1925, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1925, 0.95).
narrative_ontology:measurement(duel_su_t1950, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1950, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dueling_disappearance_mechanism' kernel. This 'contraction_reading' emphasizes the cultural displacement of honor by dignity, while 'institutional_displacement_reading' focuses on legal/economic substitution, and 'overdetermined_composite_reading' posits multiple simultaneous causes. All three are distinct but related explanations for the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
