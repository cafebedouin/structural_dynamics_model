% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Orthodox Literal Reading: Caste Duty and Dharmic War Legitimation
 *   domain: religious_studies/hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the orthodox literal reading of the Bhagavad
 *   Gita's Kurukshetra discourse. Under this reading, the text is a direct
 *   divine command (Krishna to Arjuna) mandating performance of caste-bound
 *   duty (svadharma) without attachment to outcomes, and it legitimates
 *   lethal violence when undertaken by the Kshatriya varna as detached duty.
 *   The kernel is a fixed Sanskrit text; authority is grounded in Brahminical
 *   commentarial lineage (sampradaya). The constraint operates as a
 *   commitment system: the kernel is treated as immutable, interpretation
 *   absorbs drift, and the authority structure extracts deference and labor
 *   from lower varnas while legitimating war.
 *
 * KEY AGENTS:
 *   - brahmin_interpreter_class: Primary agenda-setter (institutional/identity_locked) â controls hermeneutic monopoly and textual authority
 *   - kshatriya_warrior_class: Primary beneficiary (powerful/identity_locked) â receives violence legitimation and social dominance
 *   - shudra_servile_class: Primary target (powerless/trapped) â bears caste-based extraction and blocked mobility
 *   - populations_subject_to_dharmic_warfare: Secondary target (powerless/trapped) â bears the lethal cost of dharmic war legitimation
 *   - gandhian_allegorist: Excluded voice (moderate/constrained) â represents the absent allegorical reading
 *   - critical_historian: Analytical observer (analytical/analytical) â external structural analyst
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.62).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.75).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Orthodox Literal Reading: Caste Duty and Dharmic War Legitimation").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious_studies/hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '0dde29e6-8f90-4a78-9052-4ba7c46b937a').
narrative_ontology:cs_kernel_codification('0dde29e6-8f90-4a78-9052-4ba7c46b937a', fixed_text).
narrative_ontology:cs_authority_grounding('0dde29e6-8f90-4a78-9052-4ba7c46b937a', lineage).
narrative_ontology:cs_interpretation_layer_present('0dde29e6-8f90-4a78-9052-4ba7c46b937a').
narrative_ontology:cs_reading_relation('0dde29e6-8f90-4a78-9052-4ba7c46b937a', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_reading_relation('0dde29e6-8f90-4a78-9052-4ba7c46b937a', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('0dde29e6-8f90-4a78-9052-4ba7c46b937a', foundational, svadharma_is_varna_bound).
narrative_ontology:cs_axiom_status(svadharma_is_varna_bound, holdable).
narrative_ontology:cs_axiom_grounding('0dde29e6-8f90-4a78-9052-4ba7c46b937a', svadharma_is_varna_bound, theological).
narrative_ontology:cs_axiom('0dde29e6-8f90-4a78-9052-4ba7c46b937a', foundational, dharmic_violence_absolves_blood_guilt).
narrative_ontology:cs_axiom_status(dharmic_violence_absolves_blood_guilt, holdable).
narrative_ontology:cs_axiom_grounding('0dde29e6-8f90-4a78-9052-4ba7c46b937a', dharmic_violence_absolves_blood_guilt, theological).
narrative_ontology:cs_reference_frame('0dde29e6-8f90-4a78-9052-4ba7c46b937a', varnashrama_dharmic_order).
narrative_ontology:cs_drift_state('0dde29e6-8f90-4a78-9052-4ba7c46b937a', contemporary_postcolonial_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0dde29e6-8f90-4a78-9052-4ba7c46b937a', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpreter_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_servile_class).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, populations_subject_to_dharmic_warfare).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, varnashrama_dharma).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_ahimsa_exemption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the authoritative commentary tradition (sampradaya) that determines correct svadharma for each varna. Their hermeneutic monopoly binds the text to caste performance and ritual prerogative. Exit means abandoning the lineage that constitutes their social and epistemic identity.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpreter_class, agenda_setter,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpreter_class, beneficiary).

% Receives theological legitimation for kingship and warfare; the text absolves blood-guilt when killing is performed as detached caste duty. Their social dominance depends on the continued credibility of this legitimation. Exit would mean surrendering the warrior identity and its associated territorial authority.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, beneficiary,
    powerful, generational, identity_locked, national).

% Assigned servile labor as svadharma and structurally barred from Vedic study and the moksha-paths reserved for twice-born varnas. Mobility is blocked by ritual purity rules and the threat of social ostracism. Exit requires concealment of caste or conversion out of the dharmic universe, both extremely hazardous.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_servile_class, payer,
    powerless, generational, trapped, local).

% Communities and kingdoms designated as adharmic or enemy in wars legitimated by the text. They bear conquest, dispossession, and death framed as cosmic necessity. Their exit is blocked because the aggressor's theological calculus, not their own conduct, determines their status.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, populations_subject_to_dharmic_warfare, payer,
    powerless, immediate, trapped, regional).

% Would read the battlefield as internal spiritual conflict and reject caste-bound duty in favor of universal nonviolence. Excluded from the orthodox interpretive arena; their readings are treated as politically motivated distortion rather than valid hermeneutics.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, gandhian_allegorist, excluded,
    moderate, generational, constrained, national).

% Analyzes the text as a product of its socio-political context, tracing how the discourse stabilizes varna hierarchy and Kshatriya authority. They do not participate in the theological economy but document its extractions from outside.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, critical_historian, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__orthodox_literal_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__orthodox_literal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates cosmic and social order by assigning each varna a fixed duty (svadharma); resolves the existential crisis of action-in-the-world by embedding individual conduct within a divinely ordained framework where performance of caste duty without attachment to fruits yields liberation.
% TRANSFER_FUNCTION: Transfers deference, labor surplus, and interpretive authority from lower varnas to Brahmin interpreters and Kshatriya rulers; transfers the moral burden and mortal risk of warfare to the warrior class while legitimating it; transfers the lethal cost of war to populations designated adharmic or enemy.
% ABSENT_VOICES: Gandhian allegorists, universalist bhaktas, Buddhist critics of varna metaphysics, and Shudra theologians are structurally absent from the orthodox literal interpretive arena; their objections to caste-bound duty and literal battlefield violence are ruled out by Brahminical hermeneutic gatekeeping.
% DISAPPEARANCE_RATIONALE: If the orthodox literal reading vanished overnight, the varna-based social order would lose its primary divine sanction; Kshatriya rulers would lose the legitimation for ritualized war; Brahmin interpretive monopoly would collapse; lower-caste communities would no longer be theologically bound to servile duty; the political theology of dharmic violence would reorganize around alternative readings or secular frameworks.
% FOUNDING_PROBLEM: How to act decisively in a world of cosmic conflict without accumulating karmic bondage; how to stabilize social order across generations in the absence of centralized territorial state authority; how to legitimate the violence necessary for kingship and territorial defense.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by Brahminical commentarial tradition (Shankara, Ramanuja, Madhva) and Kshatriya epic literature. Contested from outside by Buddhist texts rejecting varna metaphysics, modern critical historians, and Gandhian ethicists who assert the founding problem was never literal warfare but social cohesion and internal moral struggle. No neutral corroboration exists; the problem statement is itself a theological claim.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderately high because the constraint extracts lifetime labor-deference from Shudras and lethal extraction from war-target populations, though it also provides a genuine soteriological framework. Suppression (0.75) is high because alternatives (rejecting caste, refusing war, allegorical reading) are theologically foreclosed and socially sanctioned. Theater ratio (0.58) reflects that in the modern period the text is performed ritually and cited politically more than it functionally governs state conduct, though social caste persistence keeps it partly functional. Accessibility collapse (0.82) is high: once the text is accepted as divine revelation and Brahmin commentary as authoritative, structural alternatives nearly vanish. Resistance (0.50) is moderate: Bhakti movements, Buddhism, colonial modernity, and Dalit politics have historically contested the arrangement, but the orthodox literal reading has never lacked institutional defenders.
 *
 * PERSPECTIVAL GAP:
 *   The Brahmin and Kshatriya seats experience the constraint as cosmic order and personal duty; the Shudra seat experiences it as a locked identity with no upward path; the war-target seat experiences it as fatal designation by another's theology. The agenda-setter seat computes coordination (preserving dharma); the payer seats compute extraction (locked labor, designated death). The engine derives this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin interpreters and Kshatriya warriors are declared beneficiaries: they collect interpretive authority and political legitimation respectively, placing their directionality near the beneficiary pole (low d). Shudra servile class and populations subject to dharmic warfare are declared victims: they bear the costs of hierarchy and legitimated violence, placing their directionality near the target pole (high d). No override is needed; the derivation chain captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled rope rather than snare because the soteriological coordination (liberation through nishkama karma) is structurally integral to the text, not merely a cover story. A pure snare would not require an elaborate metaphysics of detached action; the metaphysics is the coordination function that holds the arrangement together. However, the asymmetric cost-bearing â Shudras locked in servitude, populations designated for war â prevents classification as pure rope. The reading is not a scaffold because it carries no sunset clause and is justified as steady-state cosmic order, not transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_ambiguity,
    'Does the orthodox literal reading exhaust the structural possibilities of the Gita kernel, or do the sibling readings represent structurally distinct constraints with equal hermeneutic legitimacy?',
    'Comparative theological and historical analysis of commentarial reception across sampradayas and modern critical scholarship.',
    'If sibling readings are structurally distinct constraints, the orthodox reading''s classification as tangled rope does not generalize to the kernel itself; the kernel would be decomposed into a constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_ambiguity, conceptual, 'Whether this reading is one of multiple structurally distinct constraints from the same kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of Shudra exit structural (legal and social sanction) or internalized (identity fusion with servile dharma)?',
    'Post-exit suppression trajectory study: whether caste-based constraints persist for individuals and communities after structural barriers are formally removed (e.g., post-Independence legal equality).',
    'If internalized, the constraint''s effective suppression exceeds the structural measure; the constraint operates partly as cognitive capture and identity coordination, pushing classification toward higher extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for caste-bound identity.').

omega_variable(
    war_legitimation_scope,
    'Does the constraint''s legitimation of violence apply only to the specific Kurukshetra narrative, or does it generalize to any war framed as dharmic by Brahminical authority?',
    'Historical analysis of wars claimed under Gita sanction versus text-internal limitations (Krishna''s specific command to Arjuna).',
    'If generalizable, extractiveness and victim scope are larger than the text alone suggests; if limited, the war legitimation is a narrower extractive spike confined to the epic narrative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(war_legitimation_scope, conceptual, 'Scope of dharmic war legitimation beyond the textual narrative.').

omega_variable(
    coordination_as_genuine_soteriology,
    'Is the soteriological coordination (detached action as moksha-path) a genuine metaphysical function of the constraint, or is it an epiphenomenon of the power structure it sustains?',
    'Phenomenological study of practitioner experience versus sociological mapping of caste persistence and benefit concentration.',
    'If purely epiphenomenal, the constraint computes closer to snare; if genuine, tangled rope is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_as_genuine_soteriology, conceptual, 'Whether soteriological coordination is genuine or a cover story for power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_orth_lit_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gita_orth_lit_tr_t400, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 400, 0.35).
narrative_ontology:measurement(gita_orth_lit_tr_t800, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 800, 0.45).
narrative_ontology:measurement(gita_orth_lit_tr_t1200, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1200, 0.55).
narrative_ontology:measurement(gita_orth_lit_tr_t1600, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1600, 0.6).
narrative_ontology:measurement(gita_orth_lit_tr_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 2000, 0.58).

% Extraction over time
narrative_ontology:measurement(gita_orth_lit_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gita_orth_lit_be_t400, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 400, 0.55).
narrative_ontology:measurement(gita_orth_lit_be_t800, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 800, 0.65).
narrative_ontology:measurement(gita_orth_lit_be_t1200, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1200, 0.72).
narrative_ontology:measurement(gita_orth_lit_be_t1600, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1600, 0.7).
narrative_ontology:measurement(gita_orth_lit_be_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 2000, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gita_orth_lit_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gita_orth_lit_su_t400, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 400, 0.58).
narrative_ontology:measurement(gita_orth_lit_su_t800, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 800, 0.68).
narrative_ontology:measurement(gita_orth_lit_su_t1200, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1200, 0.75).
narrative_ontology:measurement(gita_orth_lit_su_t1600, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement(gita_orth_lit_su_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 2000, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% This constraint is the orthodox literal reading of the Gita Kurukshetra kernel, which decomposes into three structurally distinct claims: literal caste-mandate and war-legitimation (this story), internal allegorical nonviolent struggle (gandhian_allegorical_reading), and caste-transcendent devotional surrender (universalist_devotional_reading). Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
