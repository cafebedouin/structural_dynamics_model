% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Laws: Sacral Fidelity Reading
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'sacral fidelity' reading of the Lycurgan
 *   laws, where they are understood as divinely ordained, unchangeable, and
 *   requiring absolute adherence. From this perspective, the laws are a
 *   'mountain' – an irreducible, natural feature of Spartan existence, not a
 *   human construct. Spartan decline is attributed to external pressures or
 *   citizen moral failings, never to the laws' inherent rigidity. This
 *   reading emphasizes the laws' role in creating a stable, martial society,
 *   with any costs seen as necessary for this divine order. The metrics
 *   reflect this internal perspective: minimal extraction (as the laws are
 *   for the common good), high suppression (as adherence is absolute and
 *   unquestionable), and low theater (as the laws are genuinely believed and
 *   followed).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.05).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.95).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws: Sacral Fidelity Reading").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, '1b037c68-c7a8-4c13-8c71-4897519b6ac3').
narrative_ontology:cs_kernel_codification('1b037c68-c7a8-4c13-8c71-4897519b6ac3', fixed_text).
narrative_ontology:cs_authority_grounding('1b037c68-c7a8-4c13-8c71-4897519b6ac3', lineage).
narrative_ontology:cs_reading_relation('1b037c68-c7a8-4c13-8c71-4897519b6ac3', lycurgan_laws__demographic_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b037c68-c7a8-4c13-8c71-4897519b6ac3', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('1b037c68-c7a8-4c13-8c71-4897519b6ac3', foundational, laws_are_divinely_ordained).
narrative_ontology:cs_axiom_status(laws_are_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('1b037c68-c7a8-4c13-8c71-4897519b6ac3', laws_are_divinely_ordained, theological).
narrative_ontology:cs_axiom('1b037c68-c7a8-4c13-8c71-4897519b6ac3', foundational, laws_are_immutable_and_perfect).
narrative_ontology:cs_axiom_status(laws_are_immutable_and_perfect, holdable).
narrative_ontology:cs_axiom_grounding('1b037c68-c7a8-4c13-8c71-4897519b6ac3', laws_are_immutable_and_perfect, deontological).
narrative_ontology:cs_reference_frame('1b037c68-c7a8-4c13-8c71-4897519b6ac3', divine_immutable_constitution).
narrative_ontology:cs_drift_state('1b037c68-c7a8-4c13-8c71-4897519b6ac3', spartan_decline_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('1b037c68-c7a8-4c13-8c71-4897519b6ac3', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_citizens).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_state).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, divine_mandate_theory).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, constitutional_immutability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adhere absolutely to the laws, believing them divinely ordained and the source of Spartan strength and stability. Their identity is fused with this adherence; questioning the laws is unthinkable and would mean losing their place in society. They benefit from the perceived order and martial prowess the laws enable.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_citizens, beneficiary,
    moderate, generational, identity_locked, local).

% Administers the laws as a sacred trust, enforcing absolute adherence. The state's legitimacy is entirely derived from the laws' divine origin and immutability. Any deviation would undermine its foundational authority. It benefits from the stability and obedience the laws command.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_state, agenda_setter,
    institutional, civilizational, trapped, national).

% Are enslaved by the Spartan system, which the Lycurgan laws uphold. They have no voice in the system and are subject to its most brutal extractions. Their perspective is entirely absent from the sacral fidelity reading, which does not acknowledge their suffering as a systemic consequence.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helots, excluded,
    powerless, generational, trapped, local).

% Analyze the Lycurgan system from a historical and theoretical distance, often interpreting its decline through the lens of its rigid structure. This reading attributes decline to external factors or moral decay, not the laws themselves.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, external_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rigid social and military order, ensuring absolute obedience and unity of purpose among Spartan citizens, believed to be divinely sanctioned.
% TRANSFER_FUNCTION: Transfers absolute authority and social stability to the Spartan state and its citizens, in exchange for individual liberty and adherence to a strict, unchanging code of conduct.
% ABSENT_VOICES: The Helots, whose subjugation is a direct consequence of the Lycurgan system, are entirely absent. They would articulate the immense human cost of the laws' 'stability' and 'order'.
% DISAPPEARANCE_RATIONALE: If the belief in the divine, unchangeable nature of the Lycurgan laws vanished, the entire Spartan social, political, and military structure would collapse. The state's legitimacy would evaporate, and citizens would immediately seek to alter or abandon the rigid system, leading to a complete societal reorganization.
% FOUNDING_PROBLEM: Sparta faced internal strife, social inequality, and military weakness, requiring a radical, divinely sanctioned reform to establish lasting order and martial supremacy.
% FOUNDING_PROBLEM_CORROBORATION: Ancient Spartan sources and traditional historians (e.g., Plutarch) corroborate the founding problem and its ongoing relevance for Spartan identity. Modern historians, while acknowledging the historical context, often contest the 'divine' aspect and the 'live' status of the problem in its original form, viewing the laws as a human construct.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects the belief that the laws serve the collective good of Sparta, with any individual sacrifices seen as part of a divinely mandated order rather than extraction. The high suppression (0.95) and accessibility collapse (0.9) are due to the absolute, unquestionable nature of the laws, enforced by social and political structures that permit no deviation or alternative. Resistance is near zero (0.05) because, from this reading, citizens internalize the laws as sacred. The low theater ratio (0.05) indicates that the adherence is genuine, not performative, rooted in deep belief. The constant metric values over time reflect the reading's premise of immutability and unchanging divine order.
 *
 * PERSPECTIVAL GAP:
 *   This reading presents the laws as a mountain, an unchangeable divine ordinance. Other readings (e.g., 'demographic trap' or 'adaptive fiction') would classify the laws as a snare or tangled rope, highlighting their extractive and coercive aspects, and attributing Spartan decline to their structural rigidity. The divergence is fundamental: this reading sees divine order, others see human construction and its consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Spartan citizens are beneficiaries (d near 0.0) as their identity and societal stability are derived from the laws. The Spartan state is also a beneficiary/agenda-setter (d near 0.0) as its legitimacy and power are entirely grounded in the laws. Helots are excluded and bear the full cost of the system, but this reading does not acknowledge their perspective as relevant to the laws' function. External observers are analytical (d near 0.5) and can assess the system without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   From the sacral fidelity reading, mandatrophy is not applicable because the laws' mandate is divine and eternal, thus incapable of outliving its function. Any perceived 'decline' is attributed to external factors or human failing, not a flaw in the laws themselves. This classification prevents mislabeling a divinely ordained, immutable structure as a human construct with an obsolescent mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_origin_vs_human_construct,
    'Are the Lycurgan laws truly of divine origin and immutable, or are they a human construct designed to serve specific political and social ends?',
    'Archaeological and historical evidence for the actual historical development of Spartan law, comparative analysis with other ancient legal systems, and critical textual analysis of primary sources to identify human agency in their formulation.',
    'If human-made, the constraint would reclassify from Mountain to a constructed type (e.g., Snare or Tangled Rope), with significantly higher extractiveness and suppression, as its persistence would depend on coercion rather than naturalness. The beneficiaries would then be seen as those who benefit from the human-designed system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_origin_vs_human_construct, empirical, 'Ambiguity regarding the true origin and nature of the Lycurgan laws.').

omega_variable(
    spartan_decline_causality,
    'Was the decline of Sparta primarily due to external pressures and moral decay (as this reading suggests), or was it an inevitable consequence of the inherent rigidity and unrevisability of the Lycurgan laws (as other readings suggest)?',
    'Counterfactual historical analysis, comparative studies of states with rigid vs. adaptive constitutional structures, and demographic modeling of the Spartan citizen body under the laws'' constraints.',
    'If the laws'' rigidity caused decline, the constraint would be reclassified as a Snare or Tangled Rope, with its ''immutability'' seen as a design flaw rather than a virtue, and its long-term costs becoming apparent. This would also imply a higher, accumulating extractiveness over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spartan_decline_causality, empirical, 'Causal attribution for Sparta''s historical decline.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of the ''lycurgan_laws'' kernel. What are the structural implications of adopting a sibling reading, such as ''demographic_trap_reading'' or ''adaptive_fiction_reading''?',
    'Comparative analysis of the structural properties (extractiveness, suppression, beneficiary/victim sets) and classifications generated by each sibling reading.',
    'Adopting a sibling reading would fundamentally alter the classification from Mountain to a constructed type (Snare, Tangled Rope), shift the extractiveness and suppression metrics significantly upward, and redefine the beneficiaries and victims, highlighting the costs of the system rather than its divine order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural differences between this reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(lycu_tr_t50, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(lycu_tr_t150, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 150, 0.05).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 200, 0.05).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(lycu_be_t50, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 100, 0.05).
narrative_ontology:measurement(lycu_be_t150, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 150, 0.05).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 200, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(lycu_su_t50, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 50, 0.95).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 100, 0.95).
narrative_ontology:measurement(lycu_su_t150, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 150, 0.95).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 200, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
