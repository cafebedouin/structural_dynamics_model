% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Ritual Encodes Intergenerational Trauma as Warning System
 *   domain: religious/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint instantiates the trauma_encoding_reading of the
 *   catastrophe_memory_kernel: ritual mourning practices in communities
 *   shaped by historical catastrophe (genocide, pogrom, dispossession) encode
 *   traumatic affect into subsequent generations, treating that trauma as an
 *   early-warning survival system. The kernel is contested â the same
 *   ritual complex is read by others as boundary maintenance, symbolic
 *   continuity, or survival-competence transmission. Here, the ritual is
 *   modeled strictly as a trauma-transmission mechanism whose coordination
 *   output is collective threat-vigilance and whose extraction is the
 *   psychological burden imposed on descendants. The claim is tangled_rope
 *   because the vigilance function is genuine but the cost falls
 *   asymmetrically on individuals who cannot exit without identity rupture.
 *
 * KEY AGENTS:
 *   - Ritual elders (agenda_setter / organized / identity_locked): maintain and enforce the mourning ritual, deriving authority from continuity with the founding catastrophe.
 *   - Descendant community (beneficiary / organized / identity_locked): gains collective threat-vigilance and cohesion from the ritual; the group is the seat of coordination benefit.
 *   - Burdened descendants (payer / moderate / identity_locked): bear the psychological costs of transmitted trauma; their exit is blocked by identity fusion.
 *   - Secular descendants (excluded / moderate / constrained): reject trauma transmission but are marginalized within communal decision-making.
 *   - Trauma researcher (observer / analytical / analytical): evaluates the psychological and functional effects of the transmission mechanism from outside the ritual system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Ritual Encodes Intergenerational Trauma as Warning System").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, '0b3d1959-762b-4ff6-88f2-1c15d886f75b').
narrative_ontology:cs_kernel_codification('0b3d1959-762b-4ff6-88f2-1c15d886f75b', implicit).
narrative_ontology:cs_authority_grounding('0b3d1959-762b-4ff6-88f2-1c15d886f75b', lineage).
narrative_ontology:cs_interpretation_layer_present('0b3d1959-762b-4ff6-88f2-1c15d886f75b').
narrative_ontology:cs_reading_relation('0b3d1959-762b-4ff6-88f2-1c15d886f75b', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b3d1959-762b-4ff6-88f2-1c15d886f75b', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('0b3d1959-762b-4ff6-88f2-1c15d886f75b', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('0b3d1959-762b-4ff6-88f2-1c15d886f75b', foundational, intergenerational_trauma_duty).
narrative_ontology:cs_axiom_status(intergenerational_trauma_duty, holdable).
narrative_ontology:cs_axiom_grounding('0b3d1959-762b-4ff6-88f2-1c15d886f75b', intergenerational_trauma_duty, deontological).
narrative_ontology:cs_axiom('0b3d1959-762b-4ff6-88f2-1c15d886f75b', foundational, psychological_burden_prophylactic).
narrative_ontology:cs_axiom_status(psychological_burden_prophylactic, holdable).
narrative_ontology:cs_axiom_grounding('0b3d1959-762b-4ff6-88f2-1c15d886f75b', psychological_burden_prophylactic, instrumental).
narrative_ontology:cs_reference_frame('0b3d1959-762b-4ff6-88f2-1c15d886f75b', ancestral_vigilance_state).
narrative_ontology:cs_drift_state('0b3d1959-762b-4ff6-88f2-1c15d886f75b', contemporary_secularized_generation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0b3d1959-762b-4ff6-88f2-1c15d886f75b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, descendant_community).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, burdened_descendants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain, perform, and teach mourning rituals that re-enact ancestral catastrophe. They interpret the trauma for younger generations and enforce participation through social sanction and religious authority. Their standing depends on continuity with the founding catastrophe.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, ritual_elders, agenda_setter,
    organized, generational, identity_locked, national).

% Receives collective threat-vigilance and group cohesion encoded in ritual calendar and narrative. Members are socialized to read current events through the lens of ancestral catastrophe, treating vigilance as a sacred survival asset.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_community, beneficiary,
    organized, generational, identity_locked, national).

% Bear intrusive anxiety, hypervigilance, and constricted life-choices transmitted through ritual mourning and catastrophe-narrative. Leaving the ritual framework risks severing family ties and communal belonging because trauma and identity are fused.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, burdened_descendants, payer,
    moderate, biographical, identity_locked, national).

% Reject trauma transmission but face social marginalization and loss of communal standing. They would advocate for memorialization without psychological burden but are excluded from ritual governance and their dissent is treated as betrayal of memory.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, secular_descendants, excluded,
    moderate, biographical, constrained, national).

% Studies the intergenerational transmission of trauma, distinguishing functional threat-perception from pathological anxiety. Documents psychological outcomes in descendant populations and evaluates whether vigilance tracks actual threat levels.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, trauma_researcher, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves collective memory of catastrophe across generations so that the community does not lose vigilance against recurrence; it solves the problem of generational forgetting that would leave the group unprepared for renewed threat.
% TRANSFER_FUNCTION: Moves traumatic affect, hypervigilance, and catastrophe-narrative from the ritual-performing generation to subsequent generations, extracting psychological burden in exchange for collective threat-detection capacity.
% ABSENT_VOICES: Descendants who experience the trauma as purely pathological, mental health professionals outside the tradition, and secular memorialists who would separate remembrance from anxiety transmission are present in the broader society but excluded from ritual authority.
% DISAPPEARANCE_RATIONALE: If the ritual trauma encoding vanished, the community would lose its primary early-warning social technology; descendant anxiety levels would likely fall, but collective threat-preparedness would shift to other institutions or dissipate; communal identity would reorganize around non-traumatic anchors.
% FOUNDING_PROBLEM: A founding catastrophe (genocide, pogrom, famine, or dispossession) created an existential threat that required intergenerational memory to prevent recurrence.
% FOUNDING_PROBLEM_CORROBORATION: Historians and genocide scholars attest the founding catastrophe was real. Clinical psychologists and intergenerational trauma researchers outside the ritual community attest that the current transmission mechanism produces pathology disproportionate to contemporary threat levels, corroborating the contested status.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 (moderate-to-high) because descendants pay substantial psychological costs â hypervigilance, anxiety, narrowed opportunity â that are not fully internalized by the collective beneficiary. Suppression is 0.58 because persistence relies on social enforcement and identity-lock rather than physical coercion; alternatives (secular memorialization, therapeutic processing) are partially accessible but socially punished. Theater_ratio rises to 0.45 because as the founding catastrophe recedes temporally, a growing share of ritual activity serves performance of remembrance rather than functional threat-monitoring. Accessibility_collapse is 0.60: within the community, non-traumatic alternatives to memory collapse once the ritual is understood as the only legitimate form of loyalty. Resistance is 0.42: secular descendants and mental health advocates push back, but identity-lock dampens organized resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the ritual-elders' seat, the constraint is a sacred survival technology preserving the group against recurrence; from the burdened-descendants' seat, it is an inherited anxiety system they cannot refuse without exile. The engine computes this divergence from the same structural data: the elders see coordination (they maintain the warning system), while the locked-in payers see extraction (they carry the trauma tax). The secular_descendants seat, with constrained exit and excluded role, registers the suppressed alternative.
 *
 * DIRECTIONALITY LOGIC:
 *   The descendant_community sits near the beneficiary pole (low d) because it gains the coordination surplus of collective vigilance. Burdened_descendants sit near the target pole (high d) because they bear the trauma cost without commensurate individual benefit. Ritual_elders are agenda_setters whose authority is sustained by the constraint; they are not named in beneficiaries because the vigilance surplus accrues to the community, not personally to them. Their derived directionality is moderately low by virtue of institutional power and exit options, but the structural surplus does not flow to them as extracted rent.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents two errors: (1) mislabeling it as pure rope would ignore the asymmetric trauma extraction on descendants who cannot exit; (2) mislabeling it as pure snare would ignore the genuine coordination function of intergenerational threat-vigilance. The founding problem â survival after catastrophe â is contested in status, and the measurements show extraction accumulation over generational time, which keeps the tangled_rope classification stable against drift toward pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the trauma-encoding reading the only structurally coherent reading of this ritual, or do the sibling readings (symbol continuity, survival competence, boundary maintenance) describe equally valid constraints instantiated by the same practice?',
    'Comparative analysis of the four sibling constraints against empirical data: if the ritual''s operational features align with one reading''s predictions and contradict another''s, the readings are distinct constraints under Îµ-invariance.',
    'If sibling readings are equally valid, the trauma-encoding reading''s extractiveness is only one face of a multi-functional practice; if this reading uniquely fits, the extraction is structurally central.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Position of this reading within the catastrophe_memory_kernel family').

omega_variable(
    trauma_function_vs_pathology,
    'Does the encoded trauma produce functional threat-detection that improves descendant survival outcomes, or does it produce pathological anxiety that degrades decision-making and wellbeing?',
    'Longitudinal studies comparing threat-response accuracy and mental health outcomes in descendant populations with high versus low ritual exposure, controlling for actual threat environment.',
    'If pathological, the coordination story is cover and the constraint reclassifies toward snare; if functional, the tangled rope classification holds but the balance of coordination to extraction shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trauma_function_vs_pathology, empirical, 'Whether transmitted trauma is adaptive vigilance or pathological anxiety').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternatives structural (social exile for non-participants) or internalized (descendants believe they owe the trauma to ancestors and cannot conceive of exit)?',
    'Post-exit trajectory study: if psychological suppression persists after physical and social exit, reclassify as partially internalized; if it drops sharply, suppression is primarily structural.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s hold on payers is stronger than visible enforcement suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in ritual transmission').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_kernel, decomposed per the Îµ-invariance principle because the kernel's structural claims differ across readings. The trauma_encoding reading isolates the trauma-transmission/warning-system claim with its own Îµ, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
