% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Sacrifice Commitment — Performance-Only Reading (Dormant Husk)
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This story instantiates the performance_only reading of the sacrifice
 *   commitment kernel: the position that sacrificial commandments require
 *   material instantiation (altar, functioning priesthood, offerings) and
 *   that textual study of sacrificial law in their absence is preservation of
 *   the record, not occupation of the commandment itself. Under this reading
 *   the commitment is a dormant husk — a legal category kept alive by
 *   scholarship and doctrinal memory, but not currently fulfilled by anyone.
 *   This is structurally distinct from the sibling readings:
 *   study_as_exercise holds that study itself IS the performance (a
 *   different, much lower-suppression, near-Rope constraint of intellectual
 *   engagement); hybrid_preparatory holds an intermediate
 *   suspended-occupation state; symbolic_transformation holds that prayer and
 *   study are an authorized substitute, not a placeholder. Each of those is a
 *   separate constraint with its own ε and stakeholder structure — this file
 *   covers only performance_only.
 *
 * KEY AGENTS:
 *   - study_house_scholars: beneficiary of continued institutional and scholarly relevance under a dormant-commitment framing
 *   - rabbinic_authorities_performance_only: agenda_setter administering the material-instantiation doctrine
 *   - study_as_exercise_adherents: excluded voice holding the rival occupation-by-study position
 *   - future_restoration_advocates: excluded/latent stakeholder whose preparatory activity gains meaning under this reading but whose ethical accountability is deferred
 *   - halakhic_tradition_observer: analytical seat tracking the reading's structural properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.08).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.12).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, piton).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Sacrifice Commitment — Performance-Only Reading (Dormant Husk)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/halakhic_tradition/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '110673dd-52b3-4f7d-a94a-3c072d8093e9').
narrative_ontology:cs_kernel_codification('110673dd-52b3-4f7d-a94a-3c072d8093e9', fixed_text).
narrative_ontology:cs_authority_grounding('110673dd-52b3-4f7d-a94a-3c072d8093e9', lineage).
narrative_ontology:cs_interpretation_layer_present('110673dd-52b3-4f7d-a94a-3c072d8093e9').
narrative_ontology:cs_reading_relation('110673dd-52b3-4f7d-a94a-3c072d8093e9', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('110673dd-52b3-4f7d-a94a-3c072d8093e9', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('110673dd-52b3-4f7d-a94a-3c072d8093e9', temple_sacrifice_commitment__symbolic_transformation, influences).
narrative_ontology:cs_axiom('110673dd-52b3-4f7d-a94a-3c072d8093e9', foundational, material_instantiation_is_necessary_condition).
narrative_ontology:cs_axiom_status(material_instantiation_is_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('110673dd-52b3-4f7d-a94a-3c072d8093e9', material_instantiation_is_necessary_condition, conventional).
narrative_ontology:cs_axiom('110673dd-52b3-4f7d-a94a-3c072d8093e9', foundational, study_without_altar_is_preservation_not_fulfillment).
narrative_ontology:cs_axiom_status(study_without_altar_is_preservation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('110673dd-52b3-4f7d-a94a-3c072d8093e9', study_without_altar_is_preservation_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('110673dd-52b3-4f7d-a94a-3c072d8093e9', second_temple_material_sacrificial_order).
narrative_ontology:cs_drift_state('110673dd-52b3-4f7d-a94a-3c072d8093e9', post_destruction_diaspora_present, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('110673dd-52b3-4f7d-a94a-3c072d8093e9', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, study_house_scholars).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, sacrifice_requires_material_altar).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, temple_reconstruction_precondition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Continue the traditional textual study of sacrificial law (masechet kodashim and related tractates) as a scholarly discipline. Under this reading, their study does not occupy the commitment itself — it preserves the record of a defunct practice for the sake of readiness, not as a live fulfillment. They gain intellectual and institutional continuity, prestige within the tradition, and a coherent account of why study continues despite the commitment being dormant, but they do not claim to be satisfying the sacrificial obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, study_house_scholars, beneficiary,
    organized, generational, identity_locked, national).

% Adjudicate that the sacrificial commandments are suspended, not fulfilled, absent a functioning altar, priesthood in service, and Temple structure. They administer this reading by directing communal energy toward messianic anticipation and altar-adjacent halakha (purity, priestly lineage verification) rather than treating study as sufficient. Their authority rests on continuity with classical legal reasoning that ties the commandment to its material conditions.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, rabbinic_authorities_performance_only, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Hold the sibling reading that textual study itself occupies the commandment. Under the performance_only reading they are not in error so much as talking about a different commitment structure; this reading denies their study the status of occupation, framing it instead as archival maintenance. They would object that this demotes centuries of devotional textual practice to mere record-keeping.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, study_as_exercise_adherents, excluded,
    organized, generational, identity_locked, national).

% Groups actively preparing physical and political conditions for renewed sacrificial practice (priestly genealogy registries, ritual object reconstruction, site access advocacy). Under performance_only their preparatory activity is validated as meaningful in a way study alone is not, but this reading also implies that until material conditions are met, no one — including them — currently occupies the commitment. They are not currently victims, but the ethical content of any future restoration (animal welfare, contested sacred-site access) is left unresolved by this reading, creating a latent future-victim class.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, future_restoration_advocates, excluded,
    moderate, civilizational, trapped, regional).

% Analyzes how the sacrifice commitment persists as a legal category despite two millennia without a functioning Temple, and how different readings of the same kernel structure what counts as fulfillment, preparation, or mere memory.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, halakhic_tradition_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the arrangement coordinates almost nothing presently active — it preserves a legal category (the sacrificial commandments) as a matter of record and identity continuity, ensuring the law is not forgotten and remains available for reactivation if material conditions (Temple, altar, priesthood) are restored.
% TRANSFER_FUNCTION: Very little currently transfers: no resources move to any performing party because there is no performance. What is 'transferred' is symbolic and institutional — scholarly prestige and communal identity capital accrue to those who maintain the textual tradition, at essentially no cost to anyone else.
% ABSENT_VOICES: Adherents of study_as_exercise and symbolic_transformation readings would object that this reading strips their devotional practice of occupying status; they are aware of and actively contest this reading rather than being silenced by it. Future generations who might inherit an unreformed sacrificial practice (were restoration ever attempted) are absent from the current conversation entirely — they cannot object to conditions not yet in force.
% DISAPPEARANCE_RATIONALE: If this specific reading (performance_only) were abandoned tomorrow in favor of a sibling reading, current daily life would not visibly change — no altar exists, no sacrifice occurs under any reading. What would shift is purely doctrinal: whether ongoing study is described as fulfillment or as archiving. The material world of practice remains identical across all four readings until and unless a Temple is rebuilt.
% FOUNDING_PROBLEM: The commandment to offer sacrifices presumes a functioning Temple, altar, and priesthood; when the Second Temple was destroyed, the tradition needed a legal account of what happens to a commandment whose material precondition has vanished — is it suspended, transformed, or satisfiable by other means?
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish law outside the rabbinic authority structure (academic scholars of halakhic development, e.g. specialists in post-Destruction legal adaptation) corroborate that the material-instantiation requirement is a genuine, long-standing structural feature of classical sacrificial law, not a post-hoc rationalization invented by any single interested party — though they also document that all four readings emerged as competing responses to the same crisis, none possessing exclusive historical priority.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, world_unchanged).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).
:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because under this reading nothing is currently being extracted from anyone — there is no functioning sacrificial economy, no priesthood collecting offerings, no compelled performance. Suppression is low (0.12) because no one is coerced into or barred from a practice that does not currently operate; the main constraint is doctrinal, not coercive. Theater ratio is notably high and rising (0.40 to 0.65 over the interval) because the doctrinal apparatus around this reading — commemorative practices, fast days, liturgical references to restoration, scholarly emphasis on precise sacrificial procedure absent any application — has grown as a proportion of total religious-legal activity even as material performance remains permanently absent; this is consistent with piton dynamics (a formerly live Rope whose primary function atrophied, now sustained partly through performative maintenance). Accessibility collapse is moderate (0.35), not high, because rival readings (study_as_exercise, symbolic_transformation) remain fully live and contested within the tradition — this reading has not foreclosed the alternatives, it merely occupies one legitimate position among several.
 *
 * DIRECTIONALITY LOGIC:
 *   Study_house_scholars derive modest institutional benefit (continuity, purpose, prestige) from being the custodians of a preserved-but-dormant legal category, so their directionality sits toward the beneficiary end, though the benefit is thin — this is not a rich extraction, more a low-stakes identity and status good. Rabbinic authorities administering this reading are agenda-setters whose institutional legitimacy is partly bound up in correctly characterizing the commitment's status, giving them a stake in the doctrine's stability without personally extracting material resources from anyone. Future_restoration_advocates are not currently harmed by this reading; their exposure is entirely prospective — if restoration is attempted under this reading's framework without addressing the ethical questions (animal welfare, contested site access) that classical sacrificial law never resolved, a future victim class could emerge. No current victim group exists, which is why victims[] is empty and this story is authored as a piton rather than snare or tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this doctrinal position as either a live extraction mechanism or a pure coordination good. It is neither actively extracting (no snare) nor coordinating an active shared function with clean benefit (not a rope in steady operation) — it is a legal commitment whose founding problem (how to characterize an unperformable commandment) remains live in a formal sense but whose practical content has been essentially inert for two millennia, with theatrical/commemorative maintenance substituting for the original function. Piton captures this: extraction is diffuse and thin, no concentrated beneficiary profits meaningfully, and the cost of resolving the doctrinal question (formally declaring transformation or permanent suspension) exceeds what any single administering party is willing to bear, so the husk persists by inertia and ritual repetition rather than active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_victim_class_on_restoration,
    'If sacrificial practice were ever materially restored under the performance_only doctrine''s own terms, without independent ethical re-examination, would this create a new victim class (animals subject to slaughter, populations affected by contested sacred-site access) that the current dormant-husk framing never had to confront?',
    'Track whether any restoration-preparation body operating under this reading has formally engaged animal-welfare or site-access ethics as a precondition of restoration, versus treating the classical procedural rules as sufficient and unrevisable.',
    'If no ethical re-examination occurs prior to any restoration attempt, the currently victimless piton would convert to a tangled_rope or snare upon reactivation, with concentrated costs falling on newly identifiable victim groups; this omega is why the story documents ''no current victim set'' rather than ''no possible victim set.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_victim_class_on_restoration, preference, 'Whether restoration under this reading''s own terms would generate victims absent in the current dormant state.').

omega_variable(
    doctrinal_versus_political_motivation,
    'Is the performance_only reading held primarily because it is the most textually defensible position within classical halakhic reasoning, or because it conveniently defers any obligation to act on restoration (sparing adherents the political and practical difficulty of actually rebuilding a Temple)?',
    'Compare the reasoning given by adherents of this position across historical periods when restoration was more versus less politically feasible; if the doctrine hardens specifically when restoration becomes politically plausible, that supports a motivated-deferral reading.',
    'If primarily doctrinal, the low theater_ratio-adjacent extraction reading is accurate; if primarily motivated deferral, some of what is scored as low-suppression piton drift is better understood as low-grade institutional self-protection, nudging the classification toward a thin tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_versus_political_motivation, conceptual, 'Whether the reading''s persistence is doctrinally motivated or functions as convenient obligation-deferral.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the more analytically important framing the doctrinal dispute over occupation-versus-archiving (as modeled here), or the underlying institutional question of who currently controls the legitimacy of declaring the commandment fulfilled, suspended, or transformed?',
    'Examine whether rabbinic authority structures that adjudicate this reading derive institutional power specifically from being the arbiters of commitment-status, independent of which reading wins.',
    'If authority over adjudication itself is the more load-bearing structure, all four sibling readings might be better modeled as surface variation atop a single higher-order tangled_rope (control over doctrinal adjudication), which would require a fifth constraint story at that level rather than four parallel reading-stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the reading-level decomposition captures the load-bearing structure or whether adjudicative authority itself is the more important kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__performance_only, theater_ratio, 20, 0.48).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__performance_only, theater_ratio, 40, 0.55).
narrative_ontology:measurement(temp_tr_t60, temple_sacrifice_commitment__performance_only, theater_ratio, 60, 0.58).
narrative_ontology:measurement(temp_tr_t80, temple_sacrifice_commitment__performance_only, theater_ratio, 80, 0.62).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__performance_only, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__performance_only, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__performance_only, base_extractiveness, 40, 0.06).
narrative_ontology:measurement(temp_be_t60, temple_sacrifice_commitment__performance_only, base_extractiveness, 60, 0.07).
narrative_ontology:measurement(temp_be_t80, temple_sacrifice_commitment__performance_only, base_extractiveness, 80, 0.08).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__performance_only, base_extractiveness, 100, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__performance_only, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the single natural-language label 'the sacrifice commitment kernel.' Each reading (performance_only, study_as_exercise, hybrid_preparatory, symbolic_transformation) is authored as a separate constraint with its own ε, stakeholder set, and claimed_type, per the ε-invariance principle — the readings are not observer-relative measurements of one constraint but structurally distinct claims about what the commitment currently requires and who occupies it. performance_only (this file) is authored as the lowest-ε, most piton-flavored reading (dormant husk, thin theatrical maintenance); study_as_exercise is expected to be a low-epsilon rope (active coordination via study-as-fulfillment); hybrid_preparatory sits between as a suspended-state constraint; symbolic_transformation reframes the entire commitment as already-transformed rather than dormant or suspended.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
