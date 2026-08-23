% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Material Performance Requirement for Temple Sacrifice (Dormant)
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The performance_only reading of the temple_sacrifice_commitment kernel
 *   holds that the biblical mandate for animal sacrifice in the Jerusalem
 *   Temple requires material instantiation — actual animals, actual altar,
 *   actual priestly service. Study of sacrifice law (kodashim), liturgical
 *   commemoration, and Temple Institute preparations are, on this reading,
 *   archival preservation of a defunct practice. They do not 'occupy' the
 *   commitment; they preserve its memory. The constraint (material
 *   performance requirement) is a piton: its primary function atrophied with
 *   the Temple's destruction (70 CE), but it persists through institutional
 *   inertia and theatrical maintenance via study. The study practice itself
 *   is a separate, low-epsilon rope coordinating readiness for a postulated
 *   messianic restoration. No current victim set exists because the
 *   constraint cannot be enforced materially, but a future victim set
 *   (kohanim obligated to perform, animals slaughtered, political conflict
 *   over Temple Mount) would emerge if restoration were attempted without
 *   ethical evolution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.05).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.05).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, piton).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Material Performance Requirement for Temple Sacrifice (Dormant)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious/halakhic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, 'ff1d3581-db32-4c40-88ca-18a6965006c0').
narrative_ontology:cs_kernel_codification('ff1d3581-db32-4c40-88ca-18a6965006c0', fixed_text).
narrative_ontology:cs_authority_grounding('ff1d3581-db32-4c40-88ca-18a6965006c0', lineage).
narrative_ontology:cs_interpretation_layer_present('ff1d3581-db32-4c40-88ca-18a6965006c0').
narrative_ontology:cs_reading_relation('ff1d3581-db32-4c40-88ca-18a6965006c0', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('ff1d3581-db32-4c40-88ca-18a6965006c0', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('ff1d3581-db32-4c40-88ca-18a6965006c0', temple_sacrifice_commitment__symbolic_transformation, forecloses).
narrative_ontology:cs_axiom('ff1d3581-db32-4c40-88ca-18a6965006c0', foundational, material_instantiation_required).
narrative_ontology:cs_axiom_status(material_instantiation_required, holdable).
narrative_ontology:cs_axiom_grounding('ff1d3581-db32-4c40-88ca-18a6965006c0', material_instantiation_required, deontological).
narrative_ontology:cs_axiom('ff1d3581-db32-4c40-88ca-18a6965006c0', foundational, study_is_archival_not_occupational).
narrative_ontology:cs_axiom_status(study_is_archival_not_occupational, holdable).
narrative_ontology:cs_axiom_grounding('ff1d3581-db32-4c40-88ca-18a6965006c0', study_is_archival_not_occupational, deontological).
narrative_ontology:cs_reference_frame('ff1d3581-db32-4c40-88ca-18a6965006c0', temple_service_operative).
narrative_ontology:cs_drift_state('ff1d3581-db32-4c40-88ca-18a6965006c0', post_destruction_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ff1d3581-db32-4c40-88ca-18a6965006c0', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, traditionalist_authorities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, identity_maintaining_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__performance_only, identity_maintaining_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, divine_command_eternal).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, temple_restoration_certainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic authorities who maintain the halakhic requirement for material sacrifice performance. They administer the study tradition (kodashim tractates, Temple service liturgy) as preparation for restoration. Their interpretive authority derives from the claim that the divine command remains binding in its original material form. They benefit from the constraint's persistence as it anchors their authority in an unchangeable divine mandate.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, traditionalist_authorities, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__performance_only, traditionalist_authorities, beneficiary).

% Lay communities (particularly Haredi and Religious Zionist) for whom the sacrifice commitment functions as an identity boundary. They invest time in study, liturgy, and Temple Institute activities. The constraint's persistence provides continuity with ancestral practice and distinguishes them from liberal movements. They bear the cost of maintaining a practice whose material fulfillment is currently impossible, including cognitive dissonance and opportunity cost of study directed toward a dormant telos.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, identity_maintaining_community, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__performance_only, identity_maintaining_community, payer).

% Progressive halakhic voices, ethical critics, and liberal denominations who argue the sacrifice commitment has been superseded or transformed. They would object to the performance_only reading's claim that material instantiation remains obligatory, and to the potential future restoration without ethical evolution (animal welfare, gender egalitarianism). They are structurally excluded from the authoritative interpretive chain.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, reformist_voices, excluded,
    moderate, biographical, mobile, global).

% Future kohanim who would be obligated to perform sacrifices if restoration occurred. They have no current voice but would bear the full material burden (ritual impurity management, animal slaughter, professional specialization). Their exclusion is structural — they do not yet exist — but the constraint binds them prospectively. The omega variable 'future_victim_set_if_restoration' tracks this.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, potential_future_priests, excluded,
    powerless, generational, trapped, global).

% External scholar of commitment systems, religious law, or halakhic theory. Sees the full structure: a constraint that was once a coordination mechanism (Temple service) now persists as a dormant husk (piton) while generating a low-epsilon coordination rope (study) pointed at a future that may never arrive or may arrive transformed.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically coordinated the centralized Temple sacrificial system — a complex resource-allocation and identity-coordination mechanism requiring priestly rotations, animal supply chains, purity maintenance, and pilgrim management. Currently coordinates a study practice (kodashim learning, Temple Institute activity) that maintains readiness and communal identity toward a postulated future restoration.
% TRANSFER_FUNCTION: Transfers interpretive authority and communal loyalty to traditionalist authorities who hold the performance_only reading. Transfers identity-maintenance costs (study time, cognitive commitment, Temple Institute donations) to the identity_maintaining_community. Transfers prospective material burden to potential_future_priests. No current material transfer occurs because the constraint cannot be materially satisfied.
% ABSENT_VOICES: Reformist halakhic voices who read the commitment as transformed (symbolic_transformation reading) or superseded; animal welfare ethicists who would challenge restoration; feminist critics who would challenge gendered priestly roles; secular Israelis who would bear political costs of Temple Mount contention. These voices are excluded from the authoritative interpretive chain (traditionalist_authorities) that maintains the performance_only reading.
% DISAPPEARANCE_RATIONALE: If the performance requirement constraint vanished overnight, the study rope (kodashim learning, Temple Institute, liturgical references) would lose its teleological anchor — the 'toward-what' of its coordination. The traditionalist_authorities would lose the unchangeable divine mandate that anchors their interpretive authority. The identity_maintaining_community would lose a core differentiator from liberal movements. The world of halakhic practice and Jewish communal politics would rearrange significantly.
% FOUNDING_PROBLEM: How to maintain the divine service obligation after the Temple's destruction (70 CE) made material performance impossible, while preserving the claim that the obligation itself remains binding and unabrogated.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist authorities (Rambam Hilkhot Beit HaBechirah, contemporary poskim) attest the problem remains live — the obligation is eternal, only conditions prevent fulfillment. Historical critics (Josephus, early Christian writers, modern scholars) attest the problem was 'solved' by transformation (prayer substituting for sacrifice, symbolic_transformation reading). The corroboration split mirrors the kernel contest itself — no external neutral arbiter exists.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is near-zero (0.05) because no material performance occurs and study costs are low/voluntary. Suppression is near-zero (0.05) because no one is coerced into study or prevented from dissenting (exit is mobile for reformists). Theater ratio is high (0.85) because the study/liturgy apparatus performs the constraint's former function theatrically — maintaining the appearance of occupancy while the material referent is absent. Accessibility collapse is moderate (0.3) because alternative readings (symbolic_transformation, study_as_exercise) exist and are live in other communities. Resistance is low (0.1) because the constraint's dormancy makes it unthreatening; resistance would target a restoration attempt, not the dormant requirement. The measurement series shows the historical trajectory: high extraction/suppression when Temple stood (T=0), collapsing after destruction, with theater rising as study replaced performance.
 *
 * PERSPECTIVAL GAP:
 *   From the traditionalist_authorities seat, the constraint is a living divine mandate temporarily in abeyance — a mountain-like obligation. From the identity_maintaining_community seat, it is a coordination rope (low cost, high identity value). From the analytical_observer seat, it is a piton — a degraded coordination mechanism persisting theatrically. The reformist_voices seat sees a snare-in-waiting. The engine computes these divergences from the structural data; the claimed_type (piton) represents the analytical observer's structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditionalist_authorities are beneficiaries (d ~ 0.1) — they collect interpretive authority and communal loyalty from the constraint's persistence. Identity_maintaining_community are symmetric beneficiaries/payers (d ~ 0.5) — they gain identity coherence but pay study costs. Reformist_voices are excluded (d not computed) — they would be targets if restoration occurred. Potential_future_priests are excluded but prospectively trapped (d ~ 1.0 if restoration) — they would bear full material burden with zero exit. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (divine command for material sacrifice) has outlived its material conditions. The performance_only reading explicitly diagnoses this as mandatrophy: the arrangement persists (piton) while its founding problem (how to perform divine service) is contested — traditionalists say live, critics say dead. The study rope is the mandatrophic residue: a coordination mechanism detached from its telos, maintained because the authority structure (traditionalist_authorities) extracts legitimacy from denying the mandate's abrogation. The classification prevents mislabeling this as pure extraction (snare) because current extraction is negligible, and prevents mislabeling as coordination (rope) because the coordination target (material performance) is absent. Piton captures the theatrical persistence of a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'This constraint is one reading (performance_only) of the contested kernel ''temple_sacrifice_commitment''. What are the structural consequences of treating the performance requirement as a dormant piton versus a live mandate?',
    'Compare classification outputs across all four kernel readings. If performance_only computes as piton while study_as_exercise computes as rope, the kernel contest maps to a type contest. Track whether the engine''s per-seat classification diverges across readings for the same agents.',
    'If the kernel contest is structurally a type contest (different readings → different constraint types), the framework validates kernel decomposition. If all readings compute the same type despite different ε, the contest is evaluative not structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Commiter-frame omega: structural mapping of kernel contest to type contest').

omega_variable(
    future_victim_set_if_restoration,
    'If Temple restoration were attempted, would the performance_only reading generate a victim set (kohanim, animals, political actors) that reclassifies the constraint from piton to snare/tangled_rope?',
    'Counterfactual modeling: specify restoration scenario (messianic, political, gradual) and compute χ for potential_future_priests, animal_welfare_subjects, palestinian_jerusalem_residents. If χ exceeds snare threshold for any group, the dormant piton contains a latent snare.',
    'Latent snare detection would mean the piton classification is temporally indexed — currently piton, potentially snare. This affects mandatrophy resolution: a constraint with latent victims has not truly resolved its mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_victim_set_if_restoration, empirical, 'Latent victim set in restoration counterfactual').

omega_variable(
    theater_vs_genuine_coordination,
    'Is the study rope (kodashim learning, Temple Institute) genuine low-epsilon coordination for a plausible future, or is it entirely theatrical maintenance of the piton?',
    'Measure study community''s actual preparation behaviors (Temple Institute vessel production, priestly training, architectural planning) vs. purely liturgical engagement. If material preparation exceeds symbolic commemoration, coordination function is non-zero.',
    'If study is genuine coordination (rope), the piton has a functional symbiote. If study is purely theatrical, the piton stands alone — higher theater_ratio, stronger mandatrophy signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_vs_genuine_coordination, empirical, 'Whether the study practice is functional coordination or pure theater').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Traditionalist authorities claim the performance requirement is divine law (mountain-like naturalness). Analytical observers see a constructed halakhic category. Is the piton classification stable against the mountain claim?',
    'False summit mountain (FSM) test: if traditionalist authorities are declared beneficiaries on a mountain claim, FSM triggers reclassification to tangled_rope. Here we claim piton, not mountain, so FSM does not apply directly. But the ambiguity remains: does the authority structure''s mountain-claim function as extraction cover?',
    'If the mountain claim is cover for authority extraction, the piton''s theater_ratio understates the constraint''s extractive history. The mandate''s ''divine'' framing may be the extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the divine-law framing is structural cover for authority extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsc_perf_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tsc_perf_tr_t500, temple_sacrifice_commitment__performance_only, theater_ratio, 500, 0.4).
narrative_ontology:measurement(tsc_perf_tr_t1000, temple_sacrifice_commitment__performance_only, theater_ratio, 1000, 0.65).
narrative_ontology:measurement(tsc_perf_tr_t1500, temple_sacrifice_commitment__performance_only, theater_ratio, 1500, 0.78).
narrative_ontology:measurement(tsc_perf_tr_t1950, temple_sacrifice_commitment__performance_only, theater_ratio, 1950, 0.85).

% Extraction over time
narrative_ontology:measurement(tsc_perf_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(tsc_perf_be_t500, temple_sacrifice_commitment__performance_only, base_extractiveness, 500, 0.15).
narrative_ontology:measurement(tsc_perf_be_t1000, temple_sacrifice_commitment__performance_only, base_extractiveness, 1000, 0.08).
narrative_ontology:measurement(tsc_perf_be_t1500, temple_sacrifice_commitment__performance_only, base_extractiveness, 1500, 0.06).
narrative_ontology:measurement(tsc_perf_be_t1950, temple_sacrifice_commitment__performance_only, base_extractiveness, 1950, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tsc_perf_su_t0, temple_sacrifice_commitment__performance_only, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(tsc_perf_su_t500, temple_sacrifice_commitment__performance_only, suppression_requirement, 500, 0.2).
narrative_ontology:measurement(tsc_perf_su_t1000, temple_sacrifice_commitment__performance_only, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(tsc_perf_su_t1500, temple_sacrifice_commitment__performance_only, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(tsc_perf_su_t1950, temple_sacrifice_commitment__performance_only, suppression_requirement, 1950, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__performance_only, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, kodashim_study_rope).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_institute_coordination).

% DUAL FORMULATION NOTE:
% This constraint (performance_only) and its three siblings form the temple_sacrifice_commitment kernel family. Each reading instantiates a different constraint with different ε: performance_only (piton, ε≈0.05), study_as_exercise (rope, ε≈0.1), hybrid_preparatory (scaffold?, ε≈0.2), symbolic_transformation (mountain?, ε≈0.0). The performance_only reading forecloses symbolic_transformation (material vs. symbolic instantiation are mutually exclusive in a single framework) but coexists_with study_as_exercise and hybrid_preparatory (different parties hold them simultaneously).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__performance_only, institutional, 0.15).
constraint_indexing:directionality_override(temple_sacrifice_commitment__performance_only, organized, 0.5).
constraint_indexing:directionality_override(temple_sacrifice_commitment__performance_only, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
