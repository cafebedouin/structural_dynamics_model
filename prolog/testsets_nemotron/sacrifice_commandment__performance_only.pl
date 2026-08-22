% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment: Physical Execution Only (Study Does Not Fulfill)
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint story captures one reading of the contested kernel
 *   'sacrifice_commandment': the performance_only reading, which holds that
 *   the biblical commandment to offer sacrifices requires physical execution
 *   in the Temple and is therefore suspended (not fulfilled, not abrogated)
 *   during the Temple's absence. This reading structures the halakhic
 *   universe by making the study of sacrificial law an obligatory preparation
 *   for a future that has not arrived for 1,900 years. The structural delta
 *   described in the prompt — high extractiveness, scholarly attention
 *   diverted from living law — is authored here as the constraint's actual
 *   operation. The claim/metric gap is deliberate: the reading claims to be a
 *   rope (coordination of memory and messianic hope), but the metrics
 *   describe a tangled_rope with substantial extraction from scholars and
 *   practitioners of living law. The engine computes this divergence; do not
 *   reconcile.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.78).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.65).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.78).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment: Physical Execution Only (Study Does Not Fulfill)").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/halakhic").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '7c63fe83-a2b2-49ea-8012-7bd492aa384f').
narrative_ontology:cs_kernel_codification('7c63fe83-a2b2-49ea-8012-7bd492aa384f', fixed_text).
narrative_ontology:cs_authority_grounding('7c63fe83-a2b2-49ea-8012-7bd492aa384f', lineage).
narrative_ontology:cs_interpretation_layer_present('7c63fe83-a2b2-49ea-8012-7bd492aa384f').
narrative_ontology:cs_reading_relation('7c63fe83-a2b2-49ea-8012-7bd492aa384f', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('7c63fe83-a2b2-49ea-8012-7bd492aa384f', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('7c63fe83-a2b2-49ea-8012-7bd492aa384f', foundational, physical_execution_irreducible).
narrative_ontology:cs_axiom_status(physical_execution_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('7c63fe83-a2b2-49ea-8012-7bd492aa384f', physical_execution_irreducible, deontological).
narrative_ontology:cs_axiom('7c63fe83-a2b2-49ea-8012-7bd492aa384f', foundational, suspension_preserves_obligation).
narrative_ontology:cs_axiom_status(suspension_preserves_obligation, holdable).
narrative_ontology:cs_axiom_grounding('7c63fe83-a2b2-49ea-8012-7bd492aa384f', suspension_preserves_obligation, deontological).
narrative_ontology:cs_reference_frame('7c63fe83-a2b2-49ea-8012-7bd492aa384f', biblical_sacrificial_obligation).
narrative_ontology:cs_drift_state('7c63fe83-a2b2-49ea-8012-7bd492aa384f', contemporary_post_temple_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7c63fe83-a2b2-49ea-8012-7bd492aa384f', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, temple_restoration_movements).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, sacrificial_literature_institutions).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, messianic_eschatology_tradition).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, halakhic_scholars).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, yeshiva_students).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, living_law_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, temple_centrality_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, physical_mitzvah_irreducibility).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, suspension_not_abrogation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations and movements (e.g., Temple Institute, various messianic groups) that organize around the conviction that the Temple will be rebuilt and physical sacrifice resumed. They benefit from the performance-only reading because it preserves the sacrificial system as a live, unfulfilled obligation whose restoration validates their entire project. Their identity is fused to the Temple's return — exit from this commitment would dissolve their organizational purpose.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, temple_restoration_movements, beneficiary,
    organized, generational, identity_locked, global).

% Yeshivas, kollels, and scholarly institutions whose curriculum and authority structure depend heavily on the study of sacrificial law (Seder Kodashim, relevant Talmudic tractates, Rambam's Hilchot Beit HaBechirah). The performance-only reading justifies devoting massive scholarly resources to unperformable law — it makes the study obligatory (as preparation) rather than elective. They set the agenda of what counts as serious halakhic learning.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, sacrificial_literature_institutions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__performance_only, sacrificial_literature_institutions, agenda_setter).

% The broad theological framework that reads history as moving toward a restored Temple and resumed sacrificial order. The performance-only reading is structurally necessary to this tradition: if study fulfilled the commandment, the messianic horizon would collapse into the present. They benefit by maintaining the gap between now and redemption as the site of religious meaning.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, messianic_eschatology_tradition, beneficiary,
    institutional, civilizational, identity_locked, universal).

% Scholars who devote career-defining portions of their intellectual labor to mastering sacrificial law — tractates Zevachim, Menachot, Tamid, Middot, Keritot, Meilah, and the commentarial tradition — knowing this law cannot be practiced. Their exit is constrained: leaving this specialization means abandoning the core of the traditional curriculum, losing institutional position, and forfeiting the prestige that attaches to 'complete' halakhic mastery.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, halakhic_scholars, payer,
    organized, biographical, constrained, global).

% Students who spend years studying sacrificial law as a required component of ordination and scholarly formation. They bear the opportunity cost directly: time that could go to living law (Shabbat, kashrut, family law, business ethics, medical ethics) goes instead to memorizing procedures for a Temple that has not stood for 1,900 years. Their exit is constrained by the curriculum — they cannot opt out without failing the system that credentials them.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, yeshiva_students, payer,
    moderate, biographical, constrained, global).

% Poskim, dayanim, and communal rabbis who must allocate finite interpretive bandwidth. The performance-only reading creates a structural diversion: the most prestigious and institutionally rewarded scholarly labor flows to unperformable law, while novel questions in technology, medicine, economics, and social organization receive less elite attention. They can exit by shifting focus, but the prestige economy pulls talent toward the sacrificial corpus.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, living_law_practitioners, payer,
    organized, generational, mobile, global).

% Scholars and thinkers (historically: some Rishonim like Rambam in certain readings; modern: academic Talmud scholars, feminist halakhic voices, liberal Orthodox thinkers) who argue that the sacrificial system belongs to a past dispensation or that study-as-fulfillment should be normative. They are excluded from the agenda-setting tables of major yeshivas and poskim circles; their objection would restructure the curriculum if heard.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, critical_halakhic_voices, excluded,
    moderate, biographical, mobile, global).

% Scholars of religion, law, and intellectual history who analyze the sacrificial system as a cultural and legal phenomenon. They see the full structure: the coordination function (preserving a collective memory of the Temple), the extraction function (diverting elite cognitive labor from living law), and the identity-lock mechanisms that sustain both.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, academic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the technical and conceptual integrity of the sacrificial system across 1,900 years of exile so that if the Temple is rebuilt, the knowledge of how to operate it survives intact. Coordinates collective memory and messianic orientation across dispersed communities.
% TRANSFER_FUNCTION: Moves elite scholarly attention, curriculum space, institutional prestige, and career-defining intellectual labor from living halakhic domains (Shabbat, kashrut, family law, medical ethics, business ethics, technology ethics) to the study of sacrificial law that cannot be practiced.
% ABSENT_VOICES: Critical halakhic voices who would argue that the sacrificial commandment is either abrogated or fulfilled through study are structurally excluded from the curriculum-setting and ordination-granting institutions. They exist in academic departments, liberal Orthodox margins, and isolated scholarly works — not in the rooms where the yeshiva curriculum is decided.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished overnight and study-as-performance became normative, the yeshiva curriculum would radically shift: Seder Kodashim would move from core requirement to elective specialization; living law domains would receive the elite scholarly attention they currently lack; temple restoration movements would lose their halakhic justification; the messianic horizon would restructure around present fulfillment rather than future restoration.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the rabbinic leadership faced a crisis: how to preserve the sacrificial system's technical knowledge and theological centrality when its physical performance was impossible. The performance-only reading answered by declaring the commandment suspended — not fulfilled, not abrogated — thereby maintaining the obligation as a live claim on future reality.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Talmudic sages themselves (e.g., Yoma 5b, Menachot 110a) and by Rambam (Hilchot Beit HaBechirah 1:1) — these are inside the tradition. Outside corroboration comes from historians of religion (e.g., Jacob Neusner on the formation of rabbinic Judaism, Jonathan Klawans on sacrifice theory) who document the rabbinic strategy of 'virtual sacrifice' and the deliberate preservation of Temple law as a structural response to catastrophe. No neutral observer attests that the founding problem remains live in the same way; the Temple's non-rebuilding for 1,900 years is a historical fact that changes the problem's status.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint directs ~1,900 years of elite cognitive labor toward unperformable acts — the opportunity cost is the living law that goes under-developed. Suppression (0.65) is substantial: the constraint persists through institutional enforcement (curriculum mandates, ordination requirements, social pressure within the yeshiva world) and through identity-lock mechanisms (scholarly identity constituted through mastery of the full corpus including Kodashim). Theater ratio (0.42) is moderate: the coordination function (preserving knowledge for Temple restoration) is real but increasingly performative as the horizon recedes; a growing share of the labor maintains the system's internal coherence rather than serving a plausible restoration timeline. Accessibility collapse (0.58) reflects that alternatives (study-as-fulfillment, abrogation) are cognitively available but institutionally blocked. Resistance (0.35) is modest: most resistance is quiet (scholars privately prioritizing living law) rather than open challenge to the reading.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, this is a rope: genuine coordination of collective memory across exile, preserving the possibility of restoration. From the payer seats, this is a snare: extraction of career-defining labor for a Temple that shows no sign of returning. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) acknowledges both functions coexist.
 *
 * DIRECTIONALITY LOGIC:
 *   The three beneficiary groups (temple_restoration_movements, sacrificial_literature_institutions, messianic_eschatology_tradition) occupy the beneficiary end of directionality: they collect institutional purpose, curriculum authority, and theological coherence from the constraint. The three payer groups (halakhic_scholars, yeshiva_students, living_law_practitioners) occupy the target end: they bear the opportunity cost of diverted attention. The excluded voice (critical_halakhic_voices) would occupy a target position if admitted — their objection is precisely that the extraction is unjustified. The observer seat sees the full structure. Exit options differentiate the payer seats: scholars are identity_locked (mastery of Kodashim constitutes the 'complete posek'), students are constrained (curriculum mandate), practitioners are mobile (can shift focus but face prestige penalties).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving sacrificial knowledge for imminent restoration) was live in 70 CE. By ~500 CE, the 'imminent' horizon had stretched beyond any individual lifespan. By 1900 CE, the constraint persists primarily because the institutions it created (yeshiva curriculum, sacrificial literature apparatus, messianic theology) now depend on it for their own coherence. The mandate has atrophied — the coordination function survives as institutional self-preservation. This is not a piton (the constraint still has active beneficiaries who would fight to keep it) but a tangled_rope where the extraction has grown while the coordination justification has thinned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_identity,
    'Is the sacrifice_commandment kernel a single commitment with three readings, or are these three distinct constraints that share vocabulary?',
    'Test ε-invariance: if measuring extractiveness from the performance_only reading gives ~0.78 but the study_as_performance reading gives ~0.15 (study fulfills, no extraction), they are different constraints by the ε-invariance principle. The kernel_id is a linguistic convenience, not a structural unity.',
    'If they are distinct constraints, each gets its own story and classification. The kernel frame becomes a map of the discourse, not a structural object. The engine''s inferred_coupling_protocol would detect coupling between them as separate constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_identity, conceptual, 'Whether the kernel frame names one constraint or three.').

omega_variable(
    suspension_vs_abrogation_boundary,
    'Does ''suspended not fulfilled'' (performance_only) structurally differ from ''abrogated until Temple'' (a fourth reading not listed), or is the distinction theological rather than structural?',
    'Compare victim sets: if both readings divert scholarly labor to unperformable law, they have the same extraction profile and are the same constraint. If ''abrogated'' releases the labor (no obligation to study), they differ structurally.',
    'If the distinction is structural, a fourth story is needed. If theological only, it is one constraint with internal theological variance — the engine classifies the constraint, not the theology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_abrogation_boundary, conceptual, 'Whether the suspension/abrogation distinction changes the constraint''s extraction structure.').

omega_variable(
    extraction_accumulation_trajectory,
    'Does the rising extractiveness trajectory (0.45 → 0.78 over 1,900 years) reflect actual historical intensification, or is it an artifact of measuring from the present backward?',
    'Historical curriculum analysis: what fraction of yeshiva curriculum was Kodashim in 200 CE, 1000 CE, 1500 CE, 1800 CE? If the fraction grew, extraction accumulated. If it was always ~30%, the trajectory is a measurement artifact.',
    'If extraction accumulated, the constraint shows mandatrophy drift (coordination → extraction). If stable, the constraint was always extractive at this level — the founding problem was never purely coordinative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_accumulation_trajectory, empirical, 'Whether the temporal measurements reflect historical drift or presentist projection.').

omega_variable(
    cs_framing_underdetermination,
    'Does this constraint instantiate a commitment system with a fixed kernel (the biblical sacrificial commandments) and an authoritative interpreter (the rabbinic tradition), or is the ''kernel'' itself a rabbinic construction?',
    'Compare the biblical text''s sacrificial legislation (Leviticus 1–7, Numbers 28–29) with the rabbinic sacrificial law (Seder Kodashim). If the latter vastly exceeds the former in technical specification, the ''kernel'' is largely rabbinic — the authority grounds itself in a text it largely produced.',
    'If the kernel is rabbinic construction, the cs_structure authority_grounding shifts from ''lineage'' to ''extraction'' — the authority extracts benefit from preventing revision of a kernel it authored. This would change the CS pattern classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel is received or constructed by the authority that claims to interpret it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_perf_only_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sac_perf_only_tr_t475, sacrifice_commandment__performance_only, theater_ratio, 475, 0.28).
narrative_ontology:measurement(sac_perf_only_tr_t950, sacrifice_commandment__performance_only, theater_ratio, 950, 0.35).
narrative_ontology:measurement(sac_perf_only_tr_t1425, sacrifice_commandment__performance_only, theater_ratio, 1425, 0.39).
narrative_ontology:measurement(sac_perf_only_tr_t1900, sacrifice_commandment__performance_only, theater_ratio, 1900, 0.42).

% Extraction over time
narrative_ontology:measurement(sac_perf_only_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sac_perf_only_be_t475, sacrifice_commandment__performance_only, base_extractiveness, 475, 0.58).
narrative_ontology:measurement(sac_perf_only_be_t950, sacrifice_commandment__performance_only, base_extractiveness, 950, 0.68).
narrative_ontology:measurement(sac_perf_only_be_t1425, sacrifice_commandment__performance_only, base_extractiveness, 1425, 0.73).
narrative_ontology:measurement(sac_perf_only_be_t1900, sacrifice_commandment__performance_only, base_extractiveness, 1900, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sac_perf_only_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(sac_perf_only_su_t475, sacrifice_commandment__performance_only, suppression_requirement, 475, 0.51).
narrative_ontology:measurement(sac_perf_only_su_t950, sacrifice_commandment__performance_only, suppression_requirement, 950, 0.58).
narrative_ontology:measurement(sac_perf_only_su_t1425, sacrifice_commandment__performance_only, suppression_requirement, 1425, 0.62).
narrative_ontology:measurement(sac_perf_only_su_t1900, sacrifice_commandment__performance_only, suppression_requirement, 1900, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__performance_only, 0.08).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, living_law_neglect).

% DUAL FORMULATION NOTE:
% This story is one of three in the sacrifice_commandment constraint family. The performance_only reading (this story) has ε=0.78 and classifies as tangled_rope. The study_as_performance reading has ε≈0.15 (study fulfills, minimal extraction) and likely classifies as rope or mountain. The archive_maintenance reading has ε≈0.35 (preservation labor without messianic urgency) and likely classifies as scaffold or rope. The three stories are linked by network.affects_constraints because they share the same referent (the sacrificial commandment) but instantiate different constraints with different ε values — per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__performance_only, organized, 0.2).
constraint_indexing:directionality_override(sacrifice_commandment__performance_only, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
