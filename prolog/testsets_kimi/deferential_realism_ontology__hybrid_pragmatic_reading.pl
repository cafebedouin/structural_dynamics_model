% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Ontology â Hybrid Pragmatic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The Deferential Realism constraint typology, under its hybrid pragmatic
 *   reading, asserts a fixed epistemic core where mountains and ropes are
 *   grounded in physical and coordination constraints, while admitting a
 *   contested normative periphery where tangled ropes and snares depend on
 *   judgments about legitimate beneficiaries. This reading positions the
 *   framework as both an observational instrument and a normative tool,
 *   creating a two-tier structure: core classifications command
 *   cross-community assent, while peripheral classifications remain sites of
 *   open political contestation. The arrangement generates seat divergence
 *   between those whose objects of study are sheltered in the stable core and
 *   those whose institutional targets are exposed to contested framing.
 *
 * KEY AGENTS:
 *   - ontology_operators (agenda_setter / institutional / analytical exit): maintain the DR classification system and adjudicate core-periphery boundaries
 *   - physical_invariant_community (beneficiary / organized / mobile exit): researchers whose objects are classified as observational core, gaining epistemic shelter
 *   - periphery_subjects (payer / moderate / constrained exit): institutions and mechanisms subject to contested peripheral classifications they do not control
 *   - normative_arbiters (beneficiary / organized / analytical exit): theorists who render legitimate-beneficiary judgments in the contested periphery
 *   - excluded_constructivists (excluded / moderate / constrained exit): scholars who reject the physical-grounding claim of the core entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.58).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.55).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Ontology â Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '3274a66c-f836-4077-8c1c-88ff6ffcd1cd').
narrative_ontology:cs_kernel_codification('3274a66c-f836-4077-8c1c-88ff6ffcd1cd', formalized).
narrative_ontology:cs_authority_grounding('3274a66c-f836-4077-8c1c-88ff6ffcd1cd', expertise).
narrative_ontology:cs_interpretation_layer_present('3274a66c-f836-4077-8c1c-88ff6ffcd1cd').
narrative_ontology:cs_reading_relation('3274a66c-f836-4077-8c1c-88ff6ffcd1cd', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('3274a66c-f836-4077-8c1c-88ff6ffcd1cd', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('3274a66c-f836-4077-8c1c-88ff6ffcd1cd', foundational, core_physical_grounding).
narrative_ontology:cs_axiom_status(core_physical_grounding, holdable).
narrative_ontology:cs_axiom_grounding('3274a66c-f836-4077-8c1c-88ff6ffcd1cd', core_physical_grounding, empirically_contingent).
narrative_ontology:cs_axiom('3274a66c-f836-4077-8c1c-88ff6ffcd1cd', foundational, periphery_normative_construction).
narrative_ontology:cs_axiom_status(periphery_normative_construction, holdable).
narrative_ontology:cs_axiom_grounding('3274a66c-f836-4077-8c1c-88ff6ffcd1cd', periphery_normative_construction, deontological).
narrative_ontology:cs_reference_frame('3274a66c-f836-4077-8c1c-88ff6ffcd1cd', observational_core_constructed_periphery).
narrative_ontology:cs_drift_state('3274a66c-f836-4077-8c1c-88ff6ffcd1cd', contemporary_institutional_design_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3274a66c-f836-4077-8c1c-88ff6ffcd1cd', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, ontology_operators).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, physical_invariant_community).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, normative_arbiters).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, periphery_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the DR classification system, adjudicate borderline cases between core and periphery, and train practitioners in applying the typology. Their authority derives from analytical expertise and the framework's perceived track record in institutional design.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, ontology_operators, agenda_setter,
    institutional, generational, analytical, global).

% Researchers studying physical laws and genuine coordination mechanisms benefit from the core's stability. Their objects are treated as observationally grounded, insulating them from political contestation and granting their findings high epistemic status within the framework.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, physical_invariant_community, beneficiary,
    organized, biographical, mobile, global).

% Social institutions and regulatory mechanisms classified at the periphery bear the cost of contested framing. Their classification as tangled_ropes or snares depends on normative judgments about beneficiary legitimacy that they do not control, exposing them to analytical delegitimation.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, periphery_subjects, payer,
    moderate, biographical, constrained, national).

% Philosophers, policy theorists, and institutional designers who apply the contested periphery classifications. They gain epistemic authority from being the seats that render legitimate-beneficiary judgments, though they do not formally administer the typology.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, normative_arbiters, beneficiary,
    organized, generational, analytical, national).

% Scholars and critics who hold that all constraint classifications are normative constructions and reject the physical-grounding claim of the core. They are not in the conversation where hybrid pragmatism is taught, funded, or applied.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, excluded_constructivists, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary for distinguishing unchangeable physical constraints from negotiable social arrangements, enabling coordination across interpretive communities on what is contestable versus what is not.
% TRANSFER_FUNCTION: Moves epistemic authority from peripheral subjects to core definers and normative arbiters â the power to classify what is natural versus extractive shifts from those being classified to those applying the typology.
% ABSENT_VOICES: Subjects of peripheral classifications who would contest their framing as snares or tangled ropes but are not in the room where normative judgments are rendered; also pure constructivists who reject any physical grounding and are structurally excluded from DR-trained institutions.
% DISAPPEARANCE_RATIONALE: If the hybrid pragmatic ontology vanished overnight, institutional-design communities would lose their coordinating distinction between the observably fixed and the politically contested; peripheral classifications would fragment into pure political dispute without the framework's epistemic shelter, while physical scientists would continue largely unchanged.
% FOUNDING_PROBLEM: The problem of distinguishing genuine natural limits from socially constructed extraction mechanisms so that critique and design target the right objects and do not waste energy on immutable constraints.
% FOUNDING_PROBLEM_CORROBORATION: Physical scientists outside the DR community corroborate that some constraints are natural-law invariant; critical theorists outside the hybrid framework corroborate that peripheral classifications carry political baggage. Neither group attests the hybrid partition itself, leaving the core-periphery boundary without full external corroboration.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is authored at medium-high because the contested periphery channels epistemic authority â the power to classify an institution as snare or tangled rope â into the hands of normative arbiters who make beneficiary-legitimacy judgments. Suppression (0.55) is medium: the core's stability is not actively enforced, but the peripheral classifications are defended through exclusion of constructivist dissent and gatekeeping in institutional-design discourse. Theater ratio (0.42 at interval end) reflects that the core coordination (genuine analytical distinction) is real, while a growing share of peripheral activity is performative contestation over legitimacy rather than discovery. Accessibility collapse (0.65) is high for core alternatives but moderate overall because periphery alternatives remain visible. Resistance (0.45) is moderate, generated primarily by those contesting their peripheral classification and by constructivists challenging the core's physical grounding.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (ontology operators) experiences the framework as a productive coordination device that correctly partitions the knowable from the contestable. The payer seat (periphery subjects) experiences the same framework as an extractive mechanism that exposes their institutions to delegitimation through normative judgments they do not control. The beneficiary seat (physical invariant community) experiences stability and epistemic privilege. The engine computes this divergence from the structural data; the authored claim of tangled rope does not adjudicate the divergence but names it.
 *
 * DIRECTIONALITY LOGIC:
 *   Ontology operators and normative arbiters sit near the beneficiary end of directionality: they define the framework and render the contested judgments that classify peripheral targets. Physical invariant communities are incidental beneficiaries â their directionality is low because the core shelters them from contestation. Periphery subjects sit near the full-target end: their classification is the object of extraction (epistemic delegitimation), and their exit options are constrained by the framework's dominance in institutional-design discourse. Excluded constructivists are trapped outside the conversation entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by keeping the coordination function (core) and extraction mechanism (periphery) analytically separable. If the core were to atrophy â if mountains and ropes became contested â the framework would degrade toward a pure snare or rhetorical scaffold. Conversely, if the periphery stabilized observationally, the framework would approach a pure diagnostic instrument (rope or mountain). The hybrid reading's value is its explicit partitioning; its risk is that the boundary itself becomes the site of covert extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    core_periphery_boundary_stability,
    'Is the boundary between the observational core and the normative periphery stable, or does it shift under political and institutional pressure?',
    'Comparative historical analysis of how borderline cases (e.g., market mechanisms, biological sex categories) have migrated between mountain/rope and snare/tangled_rope across DR application contexts.',
    'If the boundary shifts systematically toward the core under pressure, the framework is more extractive than its hybrid claim suggests; if stable, the hybrid partition is structurally sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_periphery_boundary_stability, empirical, 'Whether the core-periphery boundary is stable or migrates under pressure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (exclusion of dissenting voices from DR-trained institutions and journals) or internalized (analysts self-censoring to preserve the core''s stability claims)?',
    'Exit-interview and citation-network analysis of scholars who leave DR-informed institutions: if suppression persists after exit, it is internalized; if it drops sharply, it was structural.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because analysts carry the framework''s boundaries with them; if structural, reform can target gatekeeping institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in epistemic communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(defe_tr_t7, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 7, 0.3).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(defe_tr_t22, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 22, 0.38).
narrative_ontology:measurement(defe_tr_t30, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(defe_be_t7, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 7, 0.51).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(defe_be_t22, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 22, 0.56).
narrative_ontology:measurement(defe_be_t30, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(defe_su_t7, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 7, 0.5).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(defe_su_t22, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 22, 0.54).
narrative_ontology:measurement(defe_su_t30, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_pragmatic_reading of the deferential_realism_ontology kernel, instantiating a framework whose core is treated as observational and periphery as normatively constructed. Sibling readings treat the same kernel as purely observational (immutable_diagnostic) or purely rhetorical (rhetorical_scaffold).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
