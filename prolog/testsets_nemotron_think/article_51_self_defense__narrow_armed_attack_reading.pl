% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Self-Defense: Narrow Armed Attack Reading
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint story instantiates the narrow armed attack reading of
 *   Article 51: self-defense is lawful only in response to an actual or
 *   imminent armed attack by a state, attributable under international law.
 *   The reading treats the UN Charter's use-of-force regime as a fixed
 *   textual compromise (the San Francisco settlement) that establishes a high
 *   threshold for unilateral force. The constraint operates as a tangled
 *   rope: it performs a genuine coordination function (channeling force
 *   through the Security Council, stabilizing expectations) while
 *   asymmetrically extracting strategic freedom from powerful states, which
 *   must forego preemptive and non-state-actor self-defense claims. The
 *   constraint requires active enforcement through ICJ jurisprudence, UNSC
 *   practice, and state opinio juris to hold against persistent pressure for
 *   expansion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.45).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.6).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Self-Defense: Narrow Armed Attack Reading").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, '49848371-05ab-4f3e-8049-4aa077cac55b').
narrative_ontology:cs_kernel_codification('49848371-05ab-4f3e-8049-4aa077cac55b', fixed_text).
narrative_ontology:cs_authority_grounding('49848371-05ab-4f3e-8049-4aa077cac55b', lineage).
narrative_ontology:cs_interpretation_layer_present('49848371-05ab-4f3e-8049-4aa077cac55b').
narrative_ontology:cs_reading_relation('49848371-05ab-4f3e-8049-4aa077cac55b', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_reading_relation('49848371-05ab-4f3e-8049-4aa077cac55b', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('49848371-05ab-4f3e-8049-4aa077cac55b', foundational, armed_attack_threshold_required).
narrative_ontology:cs_axiom_status(armed_attack_threshold_required, holdable).
narrative_ontology:cs_axiom_grounding('49848371-05ab-4f3e-8049-4aa077cac55b', armed_attack_threshold_required, conventional).
narrative_ontology:cs_axiom('49848371-05ab-4f3e-8049-4aa077cac55b', foundational, state_attribution_required).
narrative_ontology:cs_axiom_status(state_attribution_required, holdable).
narrative_ontology:cs_axiom_grounding('49848371-05ab-4f3e-8049-4aa077cac55b', state_attribution_required, conventional).
narrative_ontology:cs_reference_frame('49848371-05ab-4f3e-8049-4aa077cac55b', san_francisco_compromise).
narrative_ontology:cs_drift_state('49848371-05ab-4f3e-8049-4aa077cac55b', post_9_11_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('49848371-05ab-4f3e-8049-4aa077cac55b', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, prohibition_on_use_of_force).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, state_sovereignty).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, collective_security).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constraint limiting powerful states' unilateral resort to force; depend on the collective security system and the armed attack threshold for protection against intervention; lack independent military capacity to exit the constraint's protection.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    moderate, generational, constrained, global).

% The UN Security Council, ICJ, and General Assembly administer and interpret the constraint; their authority to authorize force and adjudicate self-defense claims is preserved by the narrow reading; they set the agenda for what counts as an armed attack and attribution.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Bear the cost of constrained strategic freedom; cannot lawfully invoke self-defense against non-state actors without attribution to a host state; must seek UNSC authorization for preventive action; face political and legal costs when stretching the armed attack threshold.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_states, payer,
    powerful, biographical, constrained, global).

% Operate in weak or failed states where host state control is absent; their attacks do not trigger Article 51 under this reading unless attributable to a state; have no standing in the interpretation or application of the constraint; are the objects of force, not subjects of the law.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, non_state_actors, excluded,
    organized, biographical, trapped, regional).

% Produce the interpretive discourse that shapes opinio juris; debate the armed attack threshold, attribution standards, and the constraint's evolution; their analyses feed into ICJ judgments, UNSC resolutions, and state legal advisories.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_lawyers_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates peaceful interstate relations by establishing a clear, high threshold for lawful use of force, preventing unilateral military action from becoming a routine tool of statecraft and preserving the Security Council's primary responsibility for international peace.
% TRANSFER_FUNCTION: Moves strategic freedom and unilateral military initiative from powerful states to the collective security system; the cost of foregone preemptive options is borne by powerful states, the benefit of stability and non-intervention accrues to weaker states and the multilateral order.
% ABSENT_VOICES: Populations in weak or failed states where non-state actor threats originate but host states cannot control them; they would argue for a right of defense against non-state threats but have no standing in the interpretation of Article 51. Non-state actors themselves are structurally excluded from the legal conversation.
% DISAPPEARANCE_RATIONALE: If the armed attack requirement vanished overnight, powerful states would immediately invoke self-defense against non-state actors and emerging threats without attribution to a host state, fundamentally altering the use-of-force landscape, undermining the collective security architecture, and removing the primary legal barrier to unilateral preventive war.
% FOUNDING_PROBLEM: The UN Charter was built to prevent the unilateral resort to force that characterized the interwar period; Article 51's armed attack threshold was the negotiated compromise allowing limited self-defense while preserving the Security Council's primary responsibility for peace and the prohibition on force in Article 2(4).
% FOUNDING_PROBLEM_CORROBORATION: The drafting history (San Francisco Conference records) and ICJ jurisprudence (Nicaragua v. USA, Oil Platforms) corroborate the narrow reading as the original understanding. Powerful states and some scholars contest this, arguing state practice has evolved; the UN High-level Panel report (2004) and subsequent state practice reviews document the contested status from outside the beneficiary set.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).
:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint's primary function is coordination (limiting war) but it distributes costs asymmetrically — powerful states lose unilateral options, weaker states gain protection. Suppression is significant (0.6) because the constraint's persistence depends on active legal and institutional enforcement (ICJ rulings, UNSC resolutions, state practice) against powerful states' tendency to stretch the threshold. Theater is low-moderate (0.25) — the legal framework is genuinely operationalized in ICJ cases and UNSC debates, though some compliance is performative. Accessibility collapse (0.6) reflects that alternative interpretations (expansive, unable/unwilling) remain live in state practice but are legally marginalized. Resistance (0.7) is high because powerful states consistently contest the constraint in practice (post-9/11, counter-ISIS operations, anticipatory self-defense claims).
 *
 * PERSPECTIVAL GAP:
 *   From the powerful state seat (payer), the constraint appears as a snare-like extraction of strategic autonomy, enforced by institutions they dominate but cannot fully control. From the weaker state seat (beneficiary), it appears as a rope — genuine coordination preventing domination. From the multilateral institution seat (agenda_setter), it appears as the institutional mandate they administer. The engine computes this divergence from the structural data; the authored claim (tangled_rope) captures the hybrid reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The narrow reading's structural beneficiaries are weaker states and multilateral institutions: weaker states gain protection from intervention (d near beneficiary end), multilateral institutions gain preserved authority (d near beneficiary end). Powerful states are the structural payers: they bear the cost of constrained strategic freedom and must seek collective authorization (d near target end). Non-state actors are excluded — they are neither coordinated nor extracted from directly; they are the threats the constraint regulates. International lawyers/scholars occupy the analytical seat (d=0.5). The directionality derivation from beneficiary/victim declarations plus power/exit captures this: powerful states have high power but constrained exit (cannot leave the treaty regime), yielding high d; weaker states have moderate power and constrained exit, yielding low d as beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing unilateral force) remains contested — the original threat (interwar unilateralism) has mutated into new forms (non-state actor threats, WMD proliferation, cyber operations). The constraint has not atrophied into a piton because the coordination function is actively maintained and the extraction is structurally necessary to the coordination (the threshold IS the coordination). Mandatrophy is not resolved; the constraint is in active contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_standard_ambiguity,
    'Is the ''attributable to a state'' standard (effective control vs. overall control vs. mere harboring) a clear legal threshold or a manipulable continuum that powerful states can exploit?',
    'ICJ clarification in a contentious case involving non-state actor attribution, or UNSC resolution defining the standard; comparative analysis of state practice in attribution claims.',
    'If the standard is manipulable, the constraint''s coordination function degrades — powerful states can engineer attribution to bypass the threshold, converting the tangled rope toward snare. If clear, the constraint maintains its coordination integrity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_standard_ambiguity, conceptual, 'Whether the state attribution requirement operates as a genuine barrier or a permeable filter.').

omega_variable(
    kernel_reading_identity,
    'Does this constraint instantiate a distinct reading of the article_51 kernel, or is it a restatement of the kernel''s core that the other readings deviate from?',
    'Comparative structural analysis of all three readings'' ε, beneficiary/victim structures, and directional profiles; if ε differs substantially across readings, they are distinct constraints per ε-invariance.',
    'If distinct constraints, each carries its own classification and the kernel is a family. If one reading is the kernel''s core, the others are deviations measured against it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: whether the narrow reading is the kernel''s reference frame or one sibling among equals.').

omega_variable(
    collective_security_gap,
    'Does the narrow reading create a genuine protection gap for states facing non-state actor threats from unwilling/unable host states, and if so, does the Security Council reliably fill it?',
    'Empirical study of UNSC responsiveness to non-state actor threats where host state is unwilling/unable; case analysis of authorization delays/denials.',
    'If a gap exists and the UNSC fails to fill it, the constraint''s coordination function is incomplete — it coordinates at the cost of leaving victims unprotected, strengthening the case for the unable/unwilling reading as a necessary supplement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_security_gap, empirical, 'Whether the constraint''s coordination benefit is fully realized or creates a structural vacuum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art51_narrow_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(art51_narrow_tr_t1960, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(art51_narrow_tr_t1975, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(art51_narrow_tr_t1990, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(art51_narrow_tr_t2005, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(art51_narrow_tr_t2024, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(art51_narrow_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.3).
narrative_ontology:measurement(art51_narrow_be_t1960, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(art51_narrow_be_t1975, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1975, 0.4).
narrative_ontology:measurement(art51_narrow_be_t1990, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(art51_narrow_be_t2005, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(art51_narrow_be_t2024, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(art51_narrow_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(art51_narrow_su_t1960, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1960, 0.45).
narrative_ontology:measurement(art51_narrow_su_t1975, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(art51_narrow_su_t1990, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(art51_narrow_su_t2005, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(art51_narrow_su_t2024, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the article_51_self_defense kernel. The narrow reading (this story) and the expansive_preventive_reading are in a forecloses relation — their core premises (armed attack by state required vs. preventive force against non-state actors allowed) cannot coexist in a single legal framework. The unable_unwilling_doctrine_reading occupies an intermediate position influenced by the narrow reading's denial of its legal basis but not logically foreclosed as a political practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__narrow_armed_attack_reading, institutional, 0.1).
constraint_indexing:directionality_override(article_51_self_defense__narrow_armed_attack_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
