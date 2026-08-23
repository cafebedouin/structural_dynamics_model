% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Transmission — Symbolic Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   After a catastrophe that destroyed the community's material continuity —
 *   lands, texts, institutions, and most elders — the surviving ritual form
 *   became the sole vessel of identity and survival memory. The
 *   symbol_continuity_reading holds that preserving this symbolic form *as
 *   transmitted* is itself the survival mechanism: the community survives *as
 *   itself* only by maintaining ritual fidelity. This reading instantiates a
 *   tangled_rope constraint: it coordinates identity continuity (genuine
 *   coordination function, beneficiaries: tradition_keepers, communal_elders)
 *   while extracting adaptive capacity from those who would modify the form
 *   for current conditions (victims: adaptive_practitioners,
 *   younger_generations, crisis_responders). Active enforcement is required —
 *   deviations are sanctioned as betrayal of the ancestors and the
 *   catastrophe's lessons. The claimed type (tangled_rope) and metrics are
 *   authored independently: the reading claims coordination, the metrics
 *   describe extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.72).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission — Symbolic Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, 'dbc65bef-de3d-455c-bd51-c3d4dc595c6c').
narrative_ontology:cs_kernel_codification('dbc65bef-de3d-455c-bd51-c3d4dc595c6c', distributed).
narrative_ontology:cs_authority_grounding('dbc65bef-de3d-455c-bd51-c3d4dc595c6c', practice).
narrative_ontology:cs_interpretation_layer_present('dbc65bef-de3d-455c-bd51-c3d4dc595c6c').
narrative_ontology:cs_reading_relation('dbc65bef-de3d-455c-bd51-c3d4dc595c6c', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbc65bef-de3d-455c-bd51-c3d4dc595c6c', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('dbc65bef-de3d-455c-bd51-c3d4dc595c6c', foundational, symbolic_continuity_is_survival).
narrative_ontology:cs_axiom_status(symbolic_continuity_is_survival, holdable).
narrative_ontology:cs_axiom_grounding('dbc65bef-de3d-455c-bd51-c3d4dc595c6c', symbolic_continuity_is_survival, deontological).
narrative_ontology:cs_axiom('dbc65bef-de3d-455c-bd51-c3d4dc595c6c', foundational, ritual_fidelity_outranks_adaptation).
narrative_ontology:cs_axiom_status(ritual_fidelity_outranks_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('dbc65bef-de3d-455c-bd51-c3d4dc595c6c', ritual_fidelity_outranks_adaptation, deontological).
narrative_ontology:cs_reference_frame('dbc65bef-de3d-455c-bd51-c3d4dc595c6c', catastrophe_origin_symbolic_transmission).
narrative_ontology:cs_drift_state('dbc65bef-de3d-455c-bd51-c3d4dc595c6c', contemporary_crisis_response, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dbc65bef-de3d-455c-bd51-c3d4dc595c6c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, tradition_keepers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_elders).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, younger_generations).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, crisis_responders).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__symbol_continuity_reading, ritual_as_identity_anchor).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__symbol_continuity_reading, symbolic_form_as_survival_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold recognized authority to define correct ritual form after the catastrophe. Their status derives from fidelity to the transmitted symbolic sequence. They determine what counts as authentic preservation versus corruption. They benefit from the constraint because their authority and the community's identity continuity depend on the ritual remaining unchanged. Exit would mean abandoning the role that constitutes their communal identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, tradition_keepers, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, tradition_keepers, beneficiary).

% Survived the catastrophe and carry the living memory of pre-catastrophe practice. They experience the ritual as the primary vessel of communal survival and identity. Their psychological and social coherence depends on the ritual's unchanged transmission. They benefit from the constraint's insistence on fidelity because it validates their survival narrative. They cannot exit the constraint without fracturing their self-understanding as bearers of the tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, communal_elders, beneficiary,
    moderate, biographical, identity_locked, local).

% Seek to adapt ritual elements to address new ecological, social, or technological realities after the catastrophe. They bear the cost of the fidelity constraint: their innovations are labeled corruption, their practical knowledge is excluded from the transmission chain, and they are marginalized in communal decision-making. They could leave the community but would lose kinship networks, material support, and cultural belonging. Their exit is constrained by relational and material dependency.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_practitioners, payer,
    moderate, biographical, constrained, regional).

% Inherit a ritual system that purports to ensure survival but shows diminishing practical efficacy in changed conditions. They bear the adaptive capacity cost: they must learn and perform forms that do not address current threats (resource scarcity, climate shifts, new social structures). Their exit options are constrained by age, dependency on communal resources, and identity formation within the tradition. Some eventually leave, but the constraint extracts their formative years first.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, younger_generations, payer,
    powerless, biographical, constrained, local).

% Responsible for immediate material survival (food, water, shelter, defense) in the post-catastrophe environment. They experience the ritual fidelity constraint as a diversion of time, attention, and labor from urgent adaptive tasks. The constraint extracts their operational capacity by requiring participation in high-fidelity performances when improvisation would better serve survival. They have relatively better exit options (skills transferable to other groups) but leave behind communal bonds.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, crisis_responders, payer,
    moderate, immediate, mobile, regional).

% Study the ritual system as a case of cultural transmission under stress. They see the full structural pattern: identity preservation achieved through symbolic fidelity, adaptive capacity sacrificed, enforcement through social sanction and identity threat. They neither collect nor pay within the constraint; their analytical position allows them to trace the coordination-extraction hybrid across all seats.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, external_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains communal identity continuity after catastrophe by enforcing high-fidelity transmission of symbolic ritual form, preventing fragmentation of the group's self-understanding and survival narrative.
% TRANSFER_FUNCTION: Moves adaptive capacity — practical innovation, ecological responsiveness, improvisational problem-solving — from younger generations, adaptive practitioners, and crisis responders to tradition keepers and communal elders, who receive the authority and identity-validation that comes from controlling the authentic symbolic form.
% ABSENT_VOICES: Those who would integrate new survival knowledge into ritual practice — ecological observers, technical innovators, trauma-informed caregivers — are structurally excluded because their contributions require modifying the symbolic form. They are absent from the transmission chain because the constraint defines their input as corruption rather than evolution.
% DISAPPEARANCE_RATIONALE: If the high-fidelity ritual constraint vanished overnight, the community would lose its primary identity anchor and shared survival narrative. The tradition keepers' authority would collapse. Younger generations and adaptive practitioners would rapidly improvise new practices, but the communal cohesion that enabled collective action would fracture. The world rearranges because the constraint currently holds the identity-structure together, even as it extracts adaptive capacity.
% FOUNDING_PROBLEM: After the catastrophe destroyed material continuity (lands, texts, institutions, elders), how could the community preserve its identity and survival knowledge when only symbolic forms remained transmissible?
% FOUNDING_PROBLEM_CORROBORATION: Oral historians from neighboring communities (outside the beneficiary set) attest that the founding problem was real: the catastrophe did destroy material continuity, and symbolic ritual was the only transmissible residue. However, contemporary ecologists and resilience researchers (also outside beneficiaries) attest that the founding problem's conditions have shifted — material conditions now permit adaptive innovation, but the constraint persists as if the catastrophe were ongoing. No single external source corroborates either status exclusively; the contest is live.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the substantial adaptive capacity diverted to ritual fidelity — time, labor, cognitive bandwidth, and innovation foregone. Suppression (0.72) is high because the constraint's persistence depends on active social sanction (shunning, loss of status, identity threat) against those who propose adaptation, not merely on participant preference. Theater ratio (0.41) is moderate and rising: early post-catastrophe performances were functionally necessary for cohesion; later performances increasingly serve to demonstrate fidelity rather than generate survival-relevant coordination. Accessibility collapse (0.63) is significant but not total: alternative adaptive practices exist and are known, but adopting them requires exiting the identity-structure. Resistance (0.58) is substantial: adaptive practitioners and younger generations actively contest the fidelity requirement, but their resistance is fragmented and lacks institutional leverage.
 *
 * PERSPECTIVAL GAP:
 *   The tradition_keepers and communal_elders experience the constraint as genuine coordination — it preserves the identity that makes them a community. The adaptive_practitioners, younger_generations, and crisis_responders experience it as extraction — it takes their adaptive labor and gives them identity-validation they increasingly do not need or trust. The external_anthropologists see both: a coordination function that was real at T=0 but has been overlaid with extraction as material conditions changed. The engine computes this seat divergence from the structural data (power, exit_options, beneficiary/victim declarations).
 *
 * DIRECTIONALITY LOGIC:
 *   Tradition_keepers and communal_elders are declared beneficiaries — they collect authority and identity-validation from the constraint. Their identity_locked exit and organized/moderate power place them near the beneficiary end of directionality (low d). Adaptive_practitioners, younger_generations, and crisis_responders are declared victims — they bear the adaptive capacity costs. Their constrained/mobile exit and moderate/powerless power place them near the target end (high d). The engine derives d from these declarations plus exit modulation: identity_locked beneficiaries get damped χ; constrained victims get amplified χ.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving identity when only symbolic forms remain transmissible) was real at T=0. By T=50, material conditions have recovered enough that adaptive innovation is possible and survival-relevant. The constraint persists because the tradition_keepers' authority depends on denying that the founding problem has changed — acknowledging change would undermine their legitimacy. This is mandatrophy: the mandate (preserve identity through symbolic fidelity) has outlived its survival function but persists because the authority structure extracts benefit from denying obsolescence. The classification prevents mislabeling: the coordination function was genuine (not a cover story), but the extraction overlay is real and growing. The tangled_rope type captures this hybrid truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the symbol_continuity_reading a structurally distinct constraint from its sibling readings, or a perspectival slice of a single constraint?',
    'Test ε-invariance: if measuring extraction via identity-preservation metrics yields a different ε than measuring via survival-outcome metrics, the readings are distinct constraints. Compare base_extractiveness authored in each reading''s story.',
    'If distinct constraints, each gets its own classification and the kernel is a family linked by network.affects_constraints. If one constraint, the readings are observer frames and the engine''s perspectival classification handles the divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel''s readings instantiate separate constraints per ε-invariance principle').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (social sanction, material dependency) or internalized (identity-fusion making adaptation feel like self-betrayal)?',
    'Post-exit suppression trajectory: track adaptive_practitioners who leave the community — if suppression (guilt, identity fragmentation) persists after structural exit, reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent. This would increase χ for identity_locked victims beyond the structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in identity-locked communal constraints').

omega_variable(
    coordination_extraction_boundary,
    'At what point does the coordination function (identity preservation) become a cover for the extraction function (authority maintenance)?',
    'Longitudinal tracking of tradition_keepers'' decisions: when presented with survival-relevant adaptations that do not threaten core identity symbols, do they permit integration or suppress? Pattern of suppression without identity-threat indicates extraction dominance.',
    'If extraction dominates, the constraint may reclassify from tangled_rope to snare at later time points. The temporal measurements already show rising extractiveness and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the tangled_rope classification holds over the full interval or transitions to snare').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmt_scr_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cmt_scr_tr_t10, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(cmt_scr_tr_t20, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(cmt_scr_tr_t30, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(cmt_scr_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(cmt_scr_tr_t50, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(cmt_scr_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cmt_scr_be_t10, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cmt_scr_be_t20, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(cmt_scr_be_t30, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(cmt_scr_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(cmt_scr_be_t50, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cmt_scr_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cmt_scr_su_t10, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(cmt_scr_su_t20, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(cmt_scr_su_t30, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 30, 0.67).
narrative_ontology:measurement(cmt_scr_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(cmt_scr_su_t50, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__symbol_continuity_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the kernel 'catastrophe_memory_transmission' by isolating the symbolic-continuity claim from the operational-competence and hybrid claims. The ε values differ: this reading authors ε=0.68 (substantial extraction of adaptive capacity); operational_competence_reading would author lower ε (ritual as practical rehearsal); hybrid_embedded_reading would author intermediate ε with different beneficiary/victim structure. All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__symbol_continuity_reading, organized, 0.15).
constraint_indexing:directionality_override(catastrophe_memory_transmission__symbol_continuity_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
