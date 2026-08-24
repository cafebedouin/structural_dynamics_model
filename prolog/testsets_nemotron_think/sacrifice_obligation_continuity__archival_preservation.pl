% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__archival_preservation, []).

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
 *   constraint_id: sacrifice_obligation_continuity__archival_preservation
 *   human_readable: Sacrifice Law Archival Preservation (Study as Cultural Memory)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   The archival_preservation reading of the sacrifice_obligation_continuity
 *   kernel holds that the biblical sacrifice obligation ceased to be binding
 *   when the Second Temple was destroyed (70 CE), rendering its ritual
 *   performance materially impossible. Study of the sacrificial laws
 *   continues, but purely as cultural-historical preservation — textual
 *   scholarship, liturgical memory, and identity transmission — with zero
 *   normative force. This reading instantiates a constraint (the voluntary
 *   cultural practice of textual preservation) with zero extractiveness, zero
 *   suppression, and no enforcement. It stands in structural opposition to
 *   three sibling readings that maintain the obligation's normative
 *   persistence in varying forms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__archival_preservation, 0.0).
domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_continuity__archival_preservation, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrifice Law Archival Preservation (Study as Cultural Memory)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, '9e73fa7f-cd15-454e-b694-94240dd0255e').
narrative_ontology:cs_kernel_codification('9e73fa7f-cd15-454e-b694-94240dd0255e', fixed_text).
narrative_ontology:cs_authority_grounding('9e73fa7f-cd15-454e-b694-94240dd0255e', lineage).
narrative_ontology:cs_interpretation_layer_present('9e73fa7f-cd15-454e-b694-94240dd0255e').
narrative_ontology:cs_reading_relation('9e73fa7f-cd15-454e-b694-94240dd0255e', sacrifice_obligation_continuity__messianic_suspension, forecloses).
narrative_ontology:cs_reading_relation('9e73fa7f-cd15-454e-b694-94240dd0255e', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_reading_relation('9e73fa7f-cd15-454e-b694-94240dd0255e', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_axiom('9e73fa7f-cd15-454e-b694-94240dd0255e', foundational, sacrifice_obligation_lapsed).
narrative_ontology:cs_axiom_status(sacrifice_obligation_lapsed, holdable).
narrative_ontology:cs_axiom_grounding('9e73fa7f-cd15-454e-b694-94240dd0255e', sacrifice_obligation_lapsed, conventional).
narrative_ontology:cs_axiom('9e73fa7f-cd15-454e-b694-94240dd0255e', foundational, textual_study_as_cultural_not_nominal).
narrative_ontology:cs_axiom_status(textual_study_as_cultural_not_nominal, holdable).
narrative_ontology:cs_axiom_grounding('9e73fa7f-cd15-454e-b694-94240dd0255e', textual_study_as_cultural_not_nominal, conventional).
narrative_ontology:cs_reference_frame('9e73fa7f-cd15-454e-b694-94240dd0255e', sacrificial_cult_operative).
narrative_ontology:cs_drift_state('9e73fa7f-cd15-454e-b694-94240dd0255e', post_temple_destruction, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('9e73fa7f-cd15-454e-b694-94240dd0255e', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, textual_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, cultural_institutions).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, community_memory_keepers).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, textual_preservation_as_cultural_continuity).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, ritual_law_historical_not_nominal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study and preserve sacrifice law texts as historical, philological, and cultural artifacts. Their professional work depends on the texts' survival and accessibility. No normative obligation compels them; they participate voluntarily. Exit is straightforward — they can shift to other textual corpora.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, textual_scholars, beneficiary,
    moderate, biographical, mobile, global).

% Universities, libraries, museums, and research centers that house, catalog, digitize, and teach the sacrifice law corpus. They benefit from the cultural capital and funding attached to preserving this heritage. Their mandate is preservation and access, not ritual observance. They can reallocate resources to other collections if needed.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, cultural_institutions, beneficiary,
    institutional, generational, mobile, global).

% Communal organizations, educators, and families who transmit the textual tradition as cultural identity. They experience the texts as constitutive of collective memory. Exit is constrained by identity — abandoning the texts feels like abandoning heritage — but no enforcement mechanism compels participation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, community_memory_keepers, beneficiary,
    organized, generational, constrained, regional).

% Communities and authorities who hold the messianic_suspension, performance_only, or study_as_performance readings. They experience the sacrifice obligation as normatively live. From the archival_preservation frame, they are excluded from the constraint's coordination function because they reject its premise (that the obligation has lapsed). Their identity is fused with the obligation's persistence.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, adherents_of_binding_obligation_readings, excluded,
    organized, biographical, identity_locked, global).

% Scholar of comparative religion, legal history, or anthropology analyzing the kernel's four readings as a structural dispute. Sees the full field: the archival reading's zero-extractiveness claim, the other readings' persistent obligation claims, and the institutional stakes each reading serves. No personal stake in any reading's victory.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the textual corpus and interpretive tradition of sacrifice law as cultural memory after the ritual practice became materially impossible (Temple destruction, 70 CE). Coordinates scholarly attention, educational curricula, and communal identity around a shared textual heritage without requiring ritual performance.
% TRANSFER_FUNCTION: Moves scholarly labor, institutional funding, and communal attention from ritual performance (now impossible) to textual study, preservation, and transmission. No material resources are extracted from unwilling participants; the transfer is voluntary allocation of cultural attention.
% ABSENT_VOICES: Adherents of the three binding-obligation readings (messianic_suspension, performance_only, study_as_performance) who would object to the claim that the obligation has 'exited constraint space.' They are present in the broader discourse but excluded from this reading's coordination function because they reject its foundational premise.
% DISAPPEARANCE_RATIONALE: If the archival preservation practice vanished overnight, the textual corpus would degrade, scholarly expertise would dissipate, and a major strand of Jewish cultural memory (and its reception in Christianity, Islam, and Western scholarship) would fracture. The obligation itself is already gone on this reading; what would be lost is the cultural infrastructure that keeps the texts alive as heritage.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), the sacrificial cult — the central axis of Israelite religion — became materially impossible. The founding problem was how to preserve the sacrificial law texts and their interpretive tradition when their ritual referent no longer existed.
% FOUNDING_PROBLEM_CORROBORATION: Historians (e.g., Seth Schwartz, Martin Goodman), archaeologists, and textual scholars outside the tradition corroborate that the Temple's destruction ended the sacrificial cult materially. The rabbinic tradition itself (Mishnah, Talmud) records the shift from performance to study as the post-destruction adaptation. No non-beneficiary source claims the ritual obligation remained practically performable.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__archival_preservation, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__archival_preservation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__archival_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__archival_preservation, 0.0, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__archival_preservation_tests).
:- end_tests(sacrifice_obligation_continuity__archival_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.0 because no agent is compelled to study, fund, or transmit these texts; participation is entirely voluntary and exit is costless for scholars/institutions, moderately constrained only by identity for community_memory_keepers. Suppression is 0.0 because alternative readings are not silenced — they operate in parallel communities. Theater_ratio is 0.0 because the practice performs exactly its stated function (preservation) with no gap between ritual and reality. Accessibility_collapse is low (0.15) because ignoring the texts carries no penalty; alternatives (secular scholarship, other cultural practices) remain fully open. Resistance is 0.0 because no one resists a voluntary practice.
 *
 * PERSPECTIVAL GAP:
 *   From the archival_preservation seat, the constraint is a benign cultural rope. From the excluded seat (adherents of binding readings), the archival claim itself looks like a snare — an attempt to naturalize the obligation's disappearance and delegitimize their practice. The engine will compute per-seat types from the structural data; the divergence between the beneficiary seats (rope/mountain) and the excluded seat's experience (which the engine does not classify as a seat under this constraint) is the measure of the kernel's contestation.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared stakeholders are beneficiaries (textual_scholars, cultural_institutions, community_memory_keepers) or excluded (adherents_of_binding_obligation_readings). No stakeholder bears costs from this constraint — the 'payer' role is empty. The excluded group experiences this constraint as irrelevant to their normative world, not as extractive. The analytical_observer sees the full field. Directionality for all seated agents is at the beneficiary end (d ≈ 0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving texts after ritual impossibility) is dead — the Temple is not returning in any material sense — yet the preservation practice persists robustly. This is not mandatrophy (a constraint whose mandate outlived its function) because the practice's current function (cultural memory) is live and valued by its beneficiaries. The original mandate transformed rather than atrophied. The constraint is a genuine rope, not a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status,
    'Is the archival_preservation reading a descriptively accurate account of the sacrifice obligation''s current legal status in halakha, or a revisionist construction?',
    'Survey of contemporary poskim (decisors) across denominations: if a consensus holds that the obligation is technically binding but practically inoperative, the archival reading''s ''exited constraint space'' claim is a category error — the obligation persists in potentia. If consensus holds it is fully lapsed, the reading is descriptively accurate.',
    'If the obligation is technically binding (even if inoperative), extractiveness > 0 for those who accept halakhic authority — the constraint would reclassify from rope to tangled_rope or snare for the identity_locked seat. The zero-extractiveness claim holds only if the obligation is fully lapsed in the internal legal logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status, empirical, 'Whether the archival reading''s zero-extractiveness claim matches the internal legal consensus.').

omega_variable(
    coordination_extraction_boundary,
    'Does the cultural preservation practice covertly extract resources (communal funding, educational time, identity commitment) from participants who would exit if the obligation''s lapsed status were fully transparent?',
    'Comparative study of communities that explicitly teach ''obligation lapsed'' vs. those that teach ''obligation suspended'' — measuring voluntary participation rates, funding allocation, and exit behavior when the archival premise is made explicit.',
    'If preservation depends on participants believing the obligation persists (i.e., the archival framing is a elite/scholarly construct not shared by the community_memory_keepers), then the constraint has hidden extractiveness and the beneficiaries include institutional actors maintaining the practice under false premises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the cultural practice''s voluntary appearance masks normative extraction via identity.').

omega_variable(
    reading_relations_structure,
    'Does the archival_preservation reading''s core premise (obligation fully lapsed) logically foreclose the sibling readings in any single coherent framework, or do they coexist as live interpretive options?',
    'Formal analysis of the kernel''s logical space: can a single halakhic framework simultaneously hold ''obligation lapsed'' (archival) and ''obligation suspended/persistent'' (siblings)? If mutual exclusion is logical, relation = forecloses; if frameworks can compartmentalize, relation = coexists_with.',
    'Forecloses relations would mean the kernel cannot stably host all four readings — adoption of one logically displaces others. Coexists_with means the kernel sustains permanent structural contestation. This determines whether the kernel is a site of resolved succession (archival won) or permanent fracture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_structure, conceptual, 'Logical relationship between archival_preservation and its three sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_obl_arch_tr_t0, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0, 0.0).
narrative_ontology:measurement(sac_obl_arch_tr_t500, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 500, 0.0).
narrative_ontology:measurement(sac_obl_arch_tr_t1000, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(sac_obl_arch_tr_t1500, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(sac_obl_arch_tr_t2000, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 2000, 0.0).

% Extraction over time
narrative_ontology:measurement(sac_obl_arch_be_t0, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(sac_obl_arch_be_t500, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(sac_obl_arch_be_t1000, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(sac_obl_arch_be_t1500, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(sac_obl_arch_be_t2000, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 2000, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(sac_obl_arch_su_t0, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(sac_obl_arch_su_t500, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 500, 0.0).
narrative_ontology:measurement(sac_obl_arch_su_t1000, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 1000, 0.0).
narrative_ontology:measurement(sac_obl_arch_su_t1500, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 1500, 0.0).
narrative_ontology:measurement(sac_obl_arch_su_t2000, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 2000, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__archival_preservation, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_continuity kernel decomposes into four constraint stories, one per reading. This story (archival_preservation) claims the obligation exited constraint space (extractiveness = 0.0). The three sibling stories each maintain the obligation's normative persistence with varying extractiveness profiles. The epsilon-invariance principle requires separate stories because each reading's ε differs structurally: archival = 0.0; messianic > 0 (suspended but binding); performance_only > 0 (binding but impossible); study_as_performance > 0 (binding and fulfilled through study).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
