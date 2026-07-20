% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Ritual: Symbolic Continuity Reading
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This constraint story instantiates the symbol_continuity_reading of the
 *   catastrophe_memory_kernel. The kernel is the shared ritual
 *   mourning-practice after collective catastrophe; this reading claims the
 *   ritual's primary structural role is preserving symbolic continuity and
 *   collective identity across generations, rather than encoding survival
 *   competence, intergenerational trauma, or group boundaries. It is one of
 *   four sibling readings; per Rule 1, this file contains ONLY this reading
 *   as a clean Îµ-invariant constraint. The constraint coordinates collective
 *   identity but extracts rigidity costs from community members who seek
 *   adaptive modification. The claim/metric independence principle is
 *   observed: the claimed type is tangled_rope (hybrid
 *   coordination/extraction), while the authored metrics describe low
 *   extractiveness and moderate suppression consistent with symbolic
 *   transmission rather than material extraction.
 *
 * KEY AGENTS:
 *   - collective_identity_bearers: Primary beneficiary (moderate/identity_locked) â receive mnemonic stability and group belonging through ritual participation.
 *   - adaptation_seekers: Primary payer (moderate/constrained) â bear social costs when seeking ritual modification.
 *   - ritual_guardians: Agenda-setter (organized/constrained) â administer and enforce ritual correctness, derive status from continuity.
 *   - assimilated_descendants: Excluded voice (moderate/mobile) â absent from communal deliberation about ritual change.
 *   - memory_scholars: Analytical observer (analytical/analytical) â document the trade-off between continuity and adaptation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.2).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.35).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Catastrophe Memory Ritual: Symbolic Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, '0efcbba6-a4f7-416a-9acd-3d784f15be1b').
narrative_ontology:cs_kernel_codification('0efcbba6-a4f7-416a-9acd-3d784f15be1b', fixed_text).
narrative_ontology:cs_authority_grounding('0efcbba6-a4f7-416a-9acd-3d784f15be1b', lineage).
narrative_ontology:cs_interpretation_layer_present('0efcbba6-a4f7-416a-9acd-3d784f15be1b').
narrative_ontology:cs_reading_relation('0efcbba6-a4f7-416a-9acd-3d784f15be1b', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('0efcbba6-a4f7-416a-9acd-3d784f15be1b', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('0efcbba6-a4f7-416a-9acd-3d784f15be1b', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('0efcbba6-a4f7-416a-9acd-3d784f15be1b', foundational, symbolic_continuity_as_ritual_telos).
narrative_ontology:cs_axiom_status(symbolic_continuity_as_ritual_telos, holdable).
narrative_ontology:cs_axiom_grounding('0efcbba6-a4f7-416a-9acd-3d784f15be1b', symbolic_continuity_as_ritual_telos, deontological).
narrative_ontology:cs_axiom('0efcbba6-a4f7-416a-9acd-3d784f15be1b', foundational, identity_precedence_over_adaptation).
narrative_ontology:cs_axiom_status(identity_precedence_over_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('0efcbba6-a4f7-416a-9acd-3d784f15be1b', identity_precedence_over_adaptation, conventional).
narrative_ontology:cs_reference_frame('0efcbba6-a4f7-416a-9acd-3d784f15be1b', communal_mourning_origin).
narrative_ontology:cs_drift_state('0efcbba6-a4f7-416a-9acd-3d784f15be1b', contemporary_secularization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0efcbba6-a4f7-416a-9acd-3d784f15be1b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, collective_identity_bearers).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptation_seekers).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, collective_identity_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in mourning rituals to maintain temporal continuity with ancestral catastrophe and future descendants; receive mnemonic stability and group belonging; exit would require assimilating out of the collective, which is experienced as identity loss.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, collective_identity_bearers, beneficiary,
    moderate, generational, identity_locked, national).

% Seek to modify ritual forms or commemorative practice to fit contemporary contexts; bear costs of social exclusion, shame, or self-censorship when deviating from established ritual; alternatives within the community are limited by communal enforcement of correctness.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptation_seekers, payer,
    moderate, biographical, constrained, national).

% Administer and interpret ritual practice, enforce normative correctness, and certify legitimate commemoration; derive status and role-legitimacy from maintaining unbroken tradition; their authority depends on the ritual's continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_guardians, agenda_setter,
    organized, generational, constrained, national).

% Have left the communal ritual framework entirely through assimilation or secularization; absent from deliberations about ritual change; their absence means the cost of total exit is underrepresented in communal discourse.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, assimilated_descendants, excluded,
    moderate, biographical, mobile, national).

% Study the ritual's social and mnemonic function from an analytical distance; document the trade-off between symbolic continuity and adaptive flexibility; neither collect from nor pay into the ritual constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, memory_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__symbol_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, repeatable practice that links past catastrophe, present community, and future descendants, solving the collective-action problem of maintaining group identity across generational turnover without relying on continuous individual remembering.
% TRANSFER_FUNCTION: Moves social legitimacy and mnemonic stability from those who would adapt or abandon the ritual to the collective identity-bearers, exacting rigidity costs from adaptation-seekers in the form of social exclusion or self-censorship.
% ABSENT_VOICES: Assimilated descendants and secularized community members who have exited the ritual framework are absent from communal deliberation; their absence suppresses testimony about the cost of total exit and the viability of non-ritual memorialization.
% DISAPPEARANCE_RATIONALE: If the ritual practice vanished overnight, the communal calendar would lose its mnemonic anchor, intergenerational transmission of catastrophe memory would fragment, and the collective identity that depends on shared practice would undergo substantial reorganization toward individual or secular memorial forms.
% FOUNDING_PROBLEM: Catastrophe-induced rupture in collective selfhood: how to prevent identity dissolution and maintain temporal continuity when the social and physical world that sustained the group has been destroyed or radically altered.
% FOUNDING_PROBLEM_CORROBORATION: Memory scholars and historians attest to the founding rupture. Some ritual guardians and community elders corroborate that the problem remains live. However, adaptation-seekers and some external sociologists argue the original rupture has healed and the ritual now serves guardian authority more than communal survival; no unanimous corroboration exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.2, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.20) because the constraint's primary yield is symbolic and mnemonic, not material; it moves identity-resources, not wealth. Suppression is moderate (0.35) because enforcement operates through social sanction, communal pressure, and identity-lock rather than violence or law. Theater ratio is moderate (0.35): ritual is inherently performative, but a significant portion of current practice has drifted toward performance for communal display rather than lived mnemonic engagement. Accessibility collapse is moderate (0.45): secular memorial alternatives exist in the broader society, but within the communal framework alternatives to the specific ritual collapse once the identity-marker function is accepted. Resistance is moderate-low (0.30): adaptation-seekers push against rigidity but are fragmented and face identity costs for open resistance. Temporal measurements track gradual performative drift and slightly hardening enforcement over a 50-year generational interval.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (collective_identity_bearers) experiences the constraint as identity-giving and world-stabilizing; the payer seat (adaptation_seekers) experiences the same constraint as socially costly and creatively limiting. The agenda-setter seat (ritual_guardians) occupies an intermediate position: they enforce the constraint and derive status from it, yet they are themselves constrained by their role as guardians of an unbroken form. The engine computes this divergence from structural data rather than authoring it.
 *
 * DIRECTIONALITY LOGIC:
 *   Collective_identity_bearers are declared beneficiaries with identity_locked exit, placing them near the full-beneficiary end of directionality (low d). Their participation is subsidized by the constraint in the form of mnemonic stability. Adaptation_seekers are declared victims with constrained exit, placing them near the target end (high d); they bear the rigidity cost. Ritual_guardians are agenda-setters with constrained exit; their directionality is intermediate because they both gain status from the constraint and are bound by its form. No override is needed because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling in both directions. Against a pure rope reading: the presence of identifiable victims (adaptation_seekers bearing rigidity costs) and active social enforcement prevents classification as pure coordination. Against a pure snare reading: the low extractiveness, genuine mnemonic function, and absence of a concentrated capturer of extraction prevent classification as pure extraction. The founding problem (identity dissolution after catastrophe) is contested but not dead, so the constraint is not a piton; it is a live tangled rope with low extraction intensity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_function_ambiguity,
    'Does the ritual primarily serve symbolic continuity, or has its function been captured by boundary-maintenance or trauma-encoding dynamics?',
    'Ethnographic study of participant intentionality and generational transmission patterns; if younger participants understand the ritual primarily as identity-marker rather than warning or boundary-enforcer, the symbolic continuity reading holds.',
    'If captured by other functions, this constraint should be reclassified to a different kernel reading or its extractiveness revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_function_ambiguity, conceptual, 'Contested functional ambiguity between symbolic continuity and sibling readings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of adaptive modification structural (communal sanctions and guardian enforcement) or internalized (participants fuse collective identity with ritual form so that deviation feels like self-betrayal)?',
    'Post-exit trajectory of adaptation-seekers: if guilt and pressure persist after leaving the communal framework, suppression is partially internalized; if pressure ceases, it was structural.',
    'Internalized suppression raises effective extraction above the structural measure because the target carries the suppression beyond the communal context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of ritual adaptation').

omega_variable(
    secularization_drift,
    'Does advancing secularization render the symbolic continuity reading incoherent by severing the ritual from its metaphysical grounding, or does the reading survive as purely cultural practice?',
    'Comparative analysis across diaspora communities with varying secularization levels to observe whether ritual participation decouples from identity claims or strengthens as a cultural marker.',
    'If the reading survives only by becoming a boundary-maintenance mechanism, the constraint family may need reconfiguration and theater_ratio should be revised upward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secularization_drift, conceptual, 'Secularization pressure on the symbolic continuity reading''s coherence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.19).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 50, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 30, 0.31).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 40, 0.33).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 50, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is the symbol_continuity_reading of the catastrophe_memory_kernel. Sibling readings instantiate structurally distinct constraints from the same kernel: survival_competence_reading (adaptive capacity), trauma_encoding_reading (intergenerational trauma warning), and boundary_maintenance_reading (group boundary enforcement). Each carries its own Îµ, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
