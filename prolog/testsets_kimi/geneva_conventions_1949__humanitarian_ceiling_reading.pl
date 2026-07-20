% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions 1949 Humanitarian Ceiling Reading
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   The Geneva Conventions of 1949, read as a humanitarian ceiling, impose
 *   absolute minimum standards on state conduct in armed conflict regardless
 *   of adversary behavior or reciprocity. This reading suppresses
 *   security-maximization and conditional-reciprocity interpretations,
 *   expanding protections to civilians, detainees, and irregular combatants
 *   while placing an asymmetric operational burden on state militaries. It is
 *   claimed as a coordination mechanism to prevent total war and civilian
 *   catastrophe, but the structural asymmetryâstate militaries must comply
 *   while adversaries often do notâcreates a genuine extraction of
 *   operational freedom. The kernel is contested: the same treaty text
 *   supports sibling readings that condition protections on reciprocity or
 *   subordinate them to operational necessity. This story instantiates ONLY
 *   the humanitarian ceiling reading.
 *
 * KEY AGENTS:
 *   - state_militaries: Primary payer (institutional/national/constrained) â bears the asymmetric operational burden and legal risk
 *   - civilian_populations_in_conflict_zones: Primary beneficiary (powerless/local/trapped) â receives targeting and displacement protections
 *   - detainees_in_state_custody: Primary beneficiary (powerless/local/trapped) â receives humane treatment and due process guarantees
 *   - irregular_combatants: Secondary beneficiary (powerless/local/trapped) â retains basic protections without POW status or reciprocity
 *   - icrc_and_treaty_bodies: Agenda setter (institutional/global/constrained) â administers interpretation and monitors compliance
 *   - security_maximization_advocates: Excluded voice (organized/national/analytical) â argues for operational necessity override, structurally suppressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.62).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.75).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 Humanitarian Ceiling Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, '4ca7a01c-1096-4673-bdea-5f6fa5f9c93e').
narrative_ontology:cs_kernel_codification('4ca7a01c-1096-4673-bdea-5f6fa5f9c93e', formalized).
narrative_ontology:cs_authority_grounding('4ca7a01c-1096-4673-bdea-5f6fa5f9c93e', lineage).
narrative_ontology:cs_interpretation_layer_present('4ca7a01c-1096-4673-bdea-5f6fa5f9c93e').
narrative_ontology:cs_reading_relation('4ca7a01c-1096-4673-bdea-5f6fa5f9c93e', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ca7a01c-1096-4673-bdea-5f6fa5f9c93e', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('4ca7a01c-1096-4673-bdea-5f6fa5f9c93e', foundational, absolute_humanitarian_minimums_non_derogable).
narrative_ontology:cs_axiom_status(absolute_humanitarian_minimums_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('4ca7a01c-1096-4673-bdea-5f6fa5f9c93e', absolute_humanitarian_minimums_non_derogable, deontological).
narrative_ontology:cs_axiom('4ca7a01c-1096-4673-bdea-5f6fa5f9c93e', foundational, irregular_combatant_basic_protections_unconditional).
narrative_ontology:cs_axiom_status(irregular_combatant_basic_protections_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('4ca7a01c-1096-4673-bdea-5f6fa5f9c93e', irregular_combatant_basic_protections_unconditional, conventional).
narrative_ontology:cs_reference_frame('4ca7a01c-1096-4673-bdea-5f6fa5f9c93e', post_1949_humanitarian_order).
narrative_ontology:cs_drift_state('4ca7a01c-1096-4673-bdea-5f6fa5f9c93e', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4ca7a01c-1096-4673-bdea-5f6fa5f9c93e', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detainees_in_state_custody).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the operational burden of the humanitarian ceiling: restricted targeting options, obligations to detainees and irregular combatants, and legal exposure for commanders. Cannot exit the constraint without state denunciation of the Geneva Conventions, which is politically prohibitive and legally complex. Experience the constraint as asymmetric because adversaries often operate outside its bounds.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, payer,
    institutional, biographical, constrained, national).

% Receive the protective effect of the ceiling: prohibitions on direct targeting, indiscriminate attack, and displacement absent imperative military necessity. They cannot exit conflict zones and depend entirely on the constraint's enforcement for physical security.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, immediate, trapped, local).

% Benefit from minimum humane treatment standards, due process guarantees, and protection from torture regardless of status or the conduct of their forces. Exit is impossible while in custody; protections are fully dependent on state compliance.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, detainees_in_state_custody, beneficiary,
    powerless, immediate, trapped, local).

% Retain basic humanitarian protections under Common Article 3 and customary law even when denied prisoner-of-war status and regardless of their own forces' compliance with the Conventions. This reading shields them from summary execution and torture, though they remain exposed to lawful targeting.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants, beneficiary,
    powerless, immediate, trapped, local).

% Promote and monitor compliance with the humanitarian ceiling through interpretive guidance, field operations, and legal advocacy. They depend on state consent for access and lack direct enforcement power, but shape the normative environment that defines permissible state conduct.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, icrc_and_treaty_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Argue that operational necessity and state security should override humanitarian minimums in asymmetric conflict. Their positions are structurally suppressed in the humanitarian ceiling reading, which treats security rationales as legally subordinate. They remain vocal in domestic political and military discourse but are marginalized in treaty interpretation forums.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, security_maximization_advocates, excluded,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a baseline of humanity in armed conflict: protecting non-combatants, regulating detention, and preventing total war by restraining state violence even when adversaries do not reciprocate.
% TRANSFER_FUNCTION: Transfers the burden of restraint from unconstrained military necessity to state militaries, who must absorb operational risk and tactical disadvantage in exchange for civilian and detainee protections.
% ABSENT_VOICES: Security-maximization strategists and conditional-reciprocity advocates are structurally marginalized in the ceiling reading; non-state adversaries who violate IHL are present as factual triggers but their legal positions are excluded from the interpretive framework.
% DISAPPEARANCE_RATIONALE: If the humanitarian ceiling vanished, state militaries would expand targeting authority, reduce detention standards, and treat irregular combatants as unlawful combatants subject to summary execution; civilian protection frameworks would collapse and the character of asymmetric conflict would shift toward annihilation.
% FOUNDING_PROBLEM: The Second World War demonstrated that unconstrained state violence against civilians, prisoners, and irregular combatants produces humanitarian catastrophe and undermines post-war political settlement.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC and UN treaty bodies attest the problem remains live, citing ongoing conflicts. State militaries and security-maximization advocates argue the founding problem has mutated into asymmetric non-state threats that the 1949 framework was not designed to address; independent conflict-data researchers corroborate that civilian harm remains endemic but dispute whether the ceiling reading is the operative constraint preventing it.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the ceiling significantly constrains targeting, detention, and interrogation options for state militaries, especially in asymmetric conflicts where adversaries ignore IHL. Suppression (0.75) is high because the reading structurally subordinates security and reciprocity rationales; states must comply even when adversaries do not. Theater_ratio (0.40) reflects growing performative complianceâlegal memos justifying drone strikes as 'targeted killing' while claiming IHL adherence, black sites hidden from monitorsâwithout fully abandoning the constraint's functional core. Resistance (0.60) captures persistent state military and political pushback. Accessibility_collapse (0.50) acknowledges that while the text is fixed, alternative readings (reciprocity, security maximization) remain intellectually available and politically powerful, preventing full collapse.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (civilians, detainees, irregular combatants) experience the constraint as protective coordination; their d values sit near the beneficiary end, yielding low effective extraction. The payer seat (state militaries) experiences the same constraint as an operational straitjacket that adversaries ignore; its d sits near the target end, amplifying effective extraction. The agenda setter seat (ICRC/treaty bodies) experiences it as a generational coordination project with constrained exit. These divergences are structurally derived from the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as civilian_populations_in_conflict_zones, detainees_in_state_custody, and irregular_combatants: they receive protections without paying operational costs, placing their directionality near the full-beneficiary pole. The victim is state_militaries: they bear the asymmetric burden of compliance, restricted tactics, and legal exposure, placing their directionality near the full-target pole. The high suppression value reflects the active subordination of security rationales that would otherwise lower state military directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by maintaining a genuine coordination function (preventing total war, protecting non-combatants) alongside clear asymmetric extraction (state military operational burden). Without the beneficiary declarations, the engine might compute a snare; without the victim declaration, it might compute a rope. The Tangled Rope classification is structurally warranted by the coexistence of coordination and extraction. The founding problemâWWII atrocitiesâremains contested rather than dead, which prevents a piton reading: the function has not fully atrophied, though theater_ratio indicates partial drift toward performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_rationale_suppression_mechanism,
    'Is the suppression of security and reciprocity rationales structural (enforceable legal sanction) or internalized (professional military identity and doctrine)?',
    'Comparative case study: measure compliance rates in conflicts with weak vs strong international legal oversight, controlling for military professionalization.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest and persists even when legal enforcement is absent; if purely structural, enforcement gaps will produce immediate degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_rationale_suppression_mechanism, empirical, 'Whether suppression is legal or identity-based').

omega_variable(
    asymmetric_burden_reciprocity_tension,
    'Does the asymmetric burden on state militaries persist only because adversary non-compliance is structurally rewarded by other readings, or is the ceiling independently stable?',
    'Track state military compliance rates across conflicts with varying adversary compliance levels; test whether ceiling-reading adherence correlates with adversary behavior or with third-party enforcement presence.',
    'If burden depends on adversary behavior, the ceiling reading is a Tangled Rope with reciprocal pressure underneath; if independent, it is a more robust coordination-with-extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_burden_reciprocity_tension, conceptual, 'Stability of asymmetric burden independent of reciprocity').

omega_variable(
    operational_necessity_residual,
    'To what extent does the ''absolute'' humanitarian ceiling already contain implicit operational-necessity exceptions that functionally approximate the security_maximization_reading?',
    'Forensic legal analysis of state military targeting decisions, detention practices, and official justifications to identify unstated necessity exceptions.',
    'If substantial implicit exceptions exist, the effective extraction is lower than the doctrinal reading suggests, and the constraint approaches a Scaffold or Rope; if exceptions are minimal, extraction remains high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_necessity_residual, empirical, 'Gap between doctrinal absolute ceiling and operational practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(gene_tr_t45, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 45, 0.28).
narrative_ontology:measurement(gene_tr_t60, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(gene_tr_t75, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 75, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(gene_be_t45, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 45, 0.48).
narrative_ontology:measurement(gene_be_t60, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(gene_be_t75, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(gene_su_t45, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement(gene_su_t60, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(gene_su_t75, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 75, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, security_maximization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the geneva_conventions_1949 kernel. The kernel decomposes into three structurally distinct claims: conditional_reciprocity_reading (protections conditional on adversary compliance), humanitarian_ceiling_reading (absolute minimums regardless of reciprocity), and security_maximization_reading (protections yield to operational necessity). Each reading has a different epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
