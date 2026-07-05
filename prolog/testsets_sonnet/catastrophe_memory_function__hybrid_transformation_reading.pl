% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Passover as Hybrid Mourning-and-Survival Ritual (Bitter Herbs + Seder Performance)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This story instantiates the hybrid_transformation_reading of the
 *   catastrophe_memory_function kernel: the Passover seder is read as a
 *   single ritual structure that fuses two functions usually analyzed
 *   separately — mourning-practice (D1/D4: bitter herbs, recitation of
 *   affliction, boundary-marking memorial obligation) and survival-competence
 *   transmission (D5: seder performance as rehearsal of decentralized
 *   leadership, resource improvisation, and continuity without central
 *   institutions). The claim is that these are not two rituals bundled by
 *   convenience but one structure whose elements do double duty — the bitter
 *   herb is simultaneously grief-marker and a lesson in enduring privation;
 *   the questions-and-answers format is simultaneously memorial catechism and
 *   a rehearsed template for oral transmission under institutional loss. This
 *   is a distinct constraint from the mourning_practice_reading (which sees
 *   only D1/D4 function and treats any adaptive content as incidental) and
 *   the survival_competence_reading (which sees only D5 function and treats
 *   the grief content as a delivery vehicle rather than a genuine end in
 *   itself). Each reading has a different epsilon and different
 *   beneficiary/victim structure because each locates the ritual's core
 *   function differently; this story does not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.28).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Passover as Hybrid Mourning-and-Survival Ritual (Bitter Herbs + Seder Performance)").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, 'a92f44fc-9c8b-415c-89ca-9d1ac02d593b').
narrative_ontology:cs_kernel_codification('a92f44fc-9c8b-415c-89ca-9d1ac02d593b', fixed_text).
narrative_ontology:cs_authority_grounding('a92f44fc-9c8b-415c-89ca-9d1ac02d593b', lineage).
narrative_ontology:cs_interpretation_layer_present('a92f44fc-9c8b-415c-89ca-9d1ac02d593b').
narrative_ontology:cs_reading_relation('a92f44fc-9c8b-415c-89ca-9d1ac02d593b', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('a92f44fc-9c8b-415c-89ca-9d1ac02d593b', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('a92f44fc-9c8b-415c-89ca-9d1ac02d593b', foundational, ritual_elements_are_functionally_dual).
narrative_ontology:cs_axiom_status(ritual_elements_are_functionally_dual, holdable).
narrative_ontology:cs_axiom_grounding('a92f44fc-9c8b-415c-89ca-9d1ac02d593b', ritual_elements_are_functionally_dual, empirically_contingent).
narrative_ontology:cs_axiom('a92f44fc-9c8b-415c-89ca-9d1ac02d593b', secondary, memorial_and_adaptive_functions_are_inseparable_in_practice).
narrative_ontology:cs_axiom_status(memorial_and_adaptive_functions_are_inseparable_in_practice, holdable).
narrative_ontology:cs_axiom_grounding('a92f44fc-9c8b-415c-89ca-9d1ac02d593b', memorial_and_adaptive_functions_are_inseparable_in_practice, conventional).
narrative_ontology:cs_reference_frame('a92f44fc-9c8b-415c-89ca-9d1ac02d593b', dual_encoded_founding_narrative).
narrative_ontology:cs_drift_state('a92f44fc-9c8b-415c-89ca-9d1ac02d593b', contemporary_diaspora_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a92f44fc-9c8b-415c-89ca-9d1ac02d593b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_transmission_authorities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, younger_generation_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, younger_generation_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, ritual_dual_encoding_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practice the seder annually across dispersed and often precarious settings. The ritual gives them a portable, household-scale structure that both preserves grief-memory of enslavement/catastrophe and rehearses the practical competencies (leadership rotation, improvisation under scarcity, decentralized continuity without a central temple) that have let communities re-form after displacement. Exit from the practice is possible but costly to identity continuity across generations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, diaspora_communities, beneficiary,
    organized, civilizational, constrained, global).

% Codify and interpret the Haggadah text, deciding which elements are read as memorial obligation versus pedagogical/adaptive content. They have long time horizons and multiple interpretive traditions to draw from, giving them flexibility that ordinary practitioners lack; they shape how the dual function is taught.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_transmission_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Encounter the ritual as inherited practice — the four questions, the afikoman, the bitter herbs — which simultaneously teaches historical grief and models how a household improvises structure, roles, and resource-sharing under uncertainty. Some experience this as a genuine skill-and-meaning transmission; others experience the time and obligation as a cost with declining perceived relevance, and largely retain mobility to disengage without severe sanction.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, younger_generation_participants, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, younger_generation_participants, payer).

% Have left or never engaged with ritual observance. They are not represented in the interpretive process that decides what the ritual means, though many retain informal ties. If present, some would argue the mourning content has become inert symbolism disconnected from the transmitted competence, or that the competence-transmission function no longer requires ritual form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, secular_and_disaffiliated_descendants, excluded,
    moderate, biographical, mobile, national).

% Study the seder comparatively to other catastrophe-response rituals, documenting how the same structure carries both an affective/memorial layer and a procedural/adaptive layer, and debate whether these are separable functions or genuinely fused in the ritual's design.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual solves two coordination problems in one recurring structure: it synchronizes collective grief-expression (so loss is not privately dissolved or forgotten) and it rehearses transferable competencies for decentralized survival (leaderless continuity, resource-constrained hospitality, oral transmission without central infrastructure) — stated without evaluating whether the fusion is optimal or incidental.
% TRANSFER_FUNCTION: The arrangement transfers narrative content, procedural competence, and affective obligation across generations, from those who currently hold ritual authority and memory to those who will need both the memory and the competence when the community's institutional continuity is again disrupted.
% ABSENT_VOICES: Secular and disaffiliated descendants who no longer practice are not part of the interpretive process that maintains the dual reading; if consulted they might argue the mourning layer has become theatrical for many practitioners even as the competence layer persists in secularized form (family gatherings, storytelling), suggesting the two functions may be dissociating rather than remaining fused.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, both the specific historical memory of the catastrophe and the embedded procedural template for decentralized continuity would lose their primary transmission vehicle; communities would need to reconstruct memorial practice and adaptive-competence transmission through separate, less efficient channels (formal education for history, ad hoc mentorship for competence), and the empirical claim that the two were ever fused would become untestable.
% FOUNDING_PROBLEM: A dispersed community facing recurring catastrophe and loss of centralized institutions needed both to keep faith with what was lost and to retain the practical know-how for surviving without the institutions that were destroyed.
% FOUNDING_PROBLEM_CORROBORATION: Practicing communities and rabbinic authorities attest both functions remain live. Independent ritual theorists and historians of diaspora institutions note the competence-transmission function has partly migrated to secular civic and educational structures outside the ritual, while the memorial function persists more robustly inside it — suggesting the founding fusion may be loosening rather than a stable permanent structure, corroborated from outside the practicing community.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.22) because participation is voluntary, low-suppression, and produces genuine dual value for most participants rather than one-sided rent capture. Suppression is moderate-low (0.28): social and familial pressure to participate exists but formal sanction for non-participation is largely absent in most communities today. Theater ratio starts low but rises modestly over the interval (0.15 to 0.32), reflecting a real dynamic in modern diaspora practice: as literal survival-competence transmission (e.g., actual skills for surviving displacement) becomes less immediately necessary in stable host societies, more of the ritual's performance shifts toward symbolic/commemorative theater even while retaining its dual-encoding form. Accessibility collapse is moderate (0.4) — alternative ways to mourn or transmit adaptive competence exist and are used by disaffiliated descendants, so the ritual's alternatives have not fully collapsed the way a mountain's would.
 *
 * PERSPECTIVAL GAP:
 *   Rabbinic authorities and long-practicing community members are likely to see the fused function as intact and functioning; disaffiliated descendants and some ritual theorists are more likely to see the two functions as already dissociating, with mourning persisting more robustly than competence-transmission. The engine's per-seat computation should reflect this: the agenda_setter and beneficiary seats read low extraction and functioning coordination; the excluded seat, if it were made structurally present, would likely read the arrangement as increasingly symbolic.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities and younger participants are coded near the beneficiary end because the ritual's declared dual function serves their own continuity interests, and exit, while costly to identity, is not blocked by external coercion. Rabbinic transmission authorities sit as agenda-setters with the most interpretive flexibility (arbitrage-like exit) because they can adapt the reading across generations without losing standing. No victim group is declared: the hybrid reading, as authored, does not identify anyone who is extractively targeted by the ritual's operation — the closest candidate, secular descendants, are better modeled as excluded voices whose absence shapes interpretive consensus rather than as parties who bear costs from the ritual's persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem question is genuinely contested here: the historical catastrophe (slavery, exile) that motivated the mourning content is not live in the literal sense, but the ritual is claimed to have transformed its function — the competence-transmission layer remains live because diaspora communities continue to face displacement and institutional discontinuity in new forms. This is precisely the case the hybrid reading exists to capture: a ritual whose original occasion has receded but whose encoded competence has found new application, rather than a pure zombie mandate. The rising theater_ratio is the signal to watch — if it continues rising while the competence-transmission claim goes uncorroborated by any use outside pure performance, the hybrid reading would need re-evaluation toward a piton trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_vs_dissociated_function,
    'Is the dual mourning-and-competence function genuinely fused in the ritual''s structure (as this reading claims), or have the two functions become dissociated over time, with mourning persisting as the dominant live function while competence-transmission has migrated to secular institutions?',
    'Comparative ethnographic study of practicing households: do participants report using ritual-derived competencies (decentralized leadership, resource improvisation, oral transmission skill) in contexts outside the ritual itself? A strong correlation would support genuine fusion; absence of transfer would support dissociation and favor the mourning_practice_reading as the more accurate description of current function.',
    'If dissociation is confirmed, this hybrid_transformation_reading''s claimed_type and metrics would need revision toward the mourning_practice_reading''s profile, and the theater_ratio trajectory would be expected to continue rising as the unfused competence claim becomes purely symbolic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_vs_dissociated_function, empirical, 'Whether the ritual''s dual function is structurally fused or has drifted apart into separable, unequally-live components.').

omega_variable(
    kernel_reading_selection_basis,
    'What justifies selecting the hybrid reading over the mourning-only or competence-only readings as the operative description of this specific ritual''s structure, rather than treating the kernel itself as genuinely underdetermined?',
    'Cross-reference textual analysis of the Haggadah''s structural elements (which components serve which function) against practitioner self-report on which function they experience as primary; convergence of textual and phenomenological evidence toward dual-encoding would support the hybrid reading''s naturalness as a description rather than an interpretive imposition.',
    'If the hybrid reading is better understood as one interpretive tradition''s framing rather than a structural fact about the ritual, its status as a distinct ε-invariant constraint (versus a contested overlay on the other two readings) weakens, though it would remain valid as one community''s lived understanding of the practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether the hybrid framing describes a structural fact about the ritual or reflects one interpretive tradition among the three sibling readings.').

omega_variable(
    false_summit_beneficiary_check,
    'Do the declared beneficiaries (diaspora communities, transmission authorities, younger participants) benefit from a ritual whose dual-function claim is naturalized as timeless tradition, when the fusion may actually be a historically contingent interpretive choice that serves institutional continuity interests?',
    'Historical analysis of when and how the dual-function reading became dominant in rabbinic interpretation relative to competing single-function readings, to establish whether the hybrid framing itself has an identifiable institutional origin and beneficiary.',
    'If the hybrid reading was historically promoted by transmission authorities specifically because it maximizes the ritual''s perceived indispensability (harder to argue for discontinuing a ritual that does two jobs than one), the claimed_type may be closer to a mild tangled_rope (coordination cover for institutional self-perpetuation) than a clean rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_beneficiary_check, conceptual, 'Whether the dual-function reading is itself naturalized rhetoric serving the interpretive authorities who promote it, distinct from the ritual''s underlying structural properties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 100, 0.32).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 60, 0.19).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 80, 0.21).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 100, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_function__hybrid_transformation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__hybrid_transformation_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% Part of the catastrophe_memory_function kernel family (3 readings). This story (hybrid_transformation_reading) claims the ritual fuses D1/D4 mourning and D5 competence-transmission in one structure. mourning_practice_reading claims only D1/D4 function is genuinely operative (competence content is incidental). survival_competence_reading claims only D5 function is genuinely operative (grief content is instrumental packaging for competence transfer). Each is authored as a separate ε-invariant constraint per the ε-invariance principle; they are linked here rather than merged because they make incompatible claims about what the ritual's core function actually is.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
