% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Ritual Dual-Register Operation in Catastrophe Memory Survival
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story captures the hybrid_encoding_reading of the
 *   catastrophe_memory_survival kernel: ritual operates on dual registers —
 *   symbolic boundary-maintenance AND embedded practical knowledge — with
 *   community survival depending on both registers remaining integrated in a
 *   single practice. The reading claims this dual-register structure is a
 *   structural feature of how ritual enables catastrophe survival (low ε,
 *   high accessibility collapse, negligible suppression), not a human
 *   arrangement that extracts from participants. Beneficiaries are
 *   catastrophe-survivor communities who maintain both registers without
 *   theoretical resolution; victims are analysts who force a binary
 *   classification and suffer analytical failure as a result. The constraint
 *   is framed as a Mountain: it would persist regardless of whether anyone
 *   theorizes it correctly, and no party collects rents from its operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.12).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.08).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.07).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, mountain).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Ritual Dual-Register Operation in Catastrophe Memory Survival").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:emerges_naturally(catastrophe_memory_survival__hybrid_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '684b1ae0-1359-4236-ac9f-b5827ff0a98f').
narrative_ontology:cs_kernel_codification('684b1ae0-1359-4236-ac9f-b5827ff0a98f', distributed).
narrative_ontology:cs_authority_grounding('684b1ae0-1359-4236-ac9f-b5827ff0a98f', practice).
narrative_ontology:cs_interpretation_layer_present('684b1ae0-1359-4236-ac9f-b5827ff0a98f').
narrative_ontology:cs_reading_relation('684b1ae0-1359-4236-ac9f-b5827ff0a98f', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('684b1ae0-1359-4236-ac9f-b5827ff0a98f', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('684b1ae0-1359-4236-ac9f-b5827ff0a98f', foundational, survival_requires_dual_register_integration).
narrative_ontology:cs_axiom_status(survival_requires_dual_register_integration, holdable).
narrative_ontology:cs_axiom_grounding('684b1ae0-1359-4236-ac9f-b5827ff0a98f', survival_requires_dual_register_integration, empirically_contingent).
narrative_ontology:cs_reference_frame('684b1ae0-1359-4236-ac9f-b5827ff0a98f', integrated_ritual_practice).
narrative_ontology:cs_drift_state('684b1ae0-1359-4236-ac9f-b5827ff0a98f', contemporary_analytical_fragmentation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('684b1ae0-1359-4236-ac9f-b5827ff0a98f', '2026-07-25T14:30:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_survivor_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, binary_forcing_analysts).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__hybrid_encoding_reading, integrated_ritual_survival_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that have endured catastrophe and maintain ritual practices integrating symbolic boundary-markers (identity, belonging, sacred order) with embedded practical knowledge (resource timing, kinship protocols, ecological reading). Their survival depends on neither register being reduced to the other; the ritual complex carries both without theoretical resolution. Exit from this integrated practice is identity-locked — abandoning either register dissolves the community's catastrophe-survival capacity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_survivor_communities, beneficiary,
    organized, generational, identity_locked, local).

% Scholars and theorists who impose a binary classification on ritual — either 'symbolic/identity' OR 'practical/competence' — and treat the excluded register as epiphenomenal. They pay the cost of analytical failure: their models cannot explain why communities that lose one register collapse, or why ritual persistence correlates with dual-register integrity. Their exit is constrained by disciplinary incentives that reward clean theoretical oppositions over messy integration.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, binary_forcing_analysts, payer,
    moderate, biographical, constrained, global).

% Elders, officiants, and knowledge-holders who enact and transmit the ritual complex. They do not theorize the dual-register structure; they enact it. Their authority derives from the practice's demonstrated survival efficacy. They administer the constraint by deciding which elements are transmitted, when, and to whom — but they cannot separate the registers without destroying the practice's survival function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practitioners, agenda_setter,
    moderate, biographical, identity_locked, local).

% Researchers who study catastrophe memory and ritual without committing to either the binary-forcing or hybrid readings. They document the empirical correlation between dual-register integrity and community survival, and track how analytical fragmentation in the field mirrors the fragmentation they observe in stressed communities.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, memory_studies_scholars, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual integrates symbolic boundary-maintenance (who we are, what we hold sacred, where we belong) with embedded practical knowledge (when to plant, how to allocate scarce resources, how to structure kinship under stress) into a single transmission vehicle that survives catastrophe because neither register alone is sufficient — the symbolic gives the practical its authority and memorability; the practical gives the symbolic its verification and adaptive flexibility.
% TRANSFER_FUNCTION: Moves survival-critical information and identity-coherence from one generation to the next through a single ritual complex, avoiding the fragility of separate transmission channels. The cost is the cognitive and performative burden of maintaining integration; the gain is survival of the community itself.
% ABSENT_VOICES: Communities that have already lost one register and consequently collapsed or assimilated — they cannot testify because they no longer exist as distinct catastrophe-survivor communities. Also absent: analysts who once held binary positions but abandoned them after fieldwork; their conversion stories are rarely published.
% DISAPPEARANCE_RATIONALE: If the dual-register integration vanished overnight — if ritual became purely symbolic or purely practical — catastrophe-survivor communities would lose the integrated transmission vehicle that has historically enabled their persistence. Symbol-only ritual would lack adaptive verification; practice-only transmission would lack identity-coherence and memorability under stress. The historical record shows communities losing one register tend to disappear within 2-3 generations.
% FOUNDING_PROBLEM: How to transmit both identity-boundary integrity and practical survival knowledge through the same bottleneck — ritual performance — without either register colonizing or displacing the other, across catastrophe events that destroy written records, institutions, and explicit pedagogy.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic records from Holocaust-survivor communities, Armenian genocide descendants, Indigenous Australian songline-keepers, and Pacific Islander navigation-ritual practitioners all attest that the founding problem remains live: each generation must re-integrate the registers anew under changed conditions. No community reports the problem as solved; the integration is the ongoing work. The binary-forcing analysts are the only voices claiming the problem is pseudo- or dissolved.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_memory_survival__hybrid_encoding_reading),
    narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the dual-register integration is not a mechanism that transfers value from one party to another — it is a structural description of what ritual must do to enable survival. Suppression is negligible (0.08) because the constraint does not require enforcement; communities that fail to maintain integration simply don't survive as distinct groups. Theater ratio is near zero (0.05) because there is no performative maintenance of a degraded function — the integration is the function. Accessibility collapse is very high (0.92) because once the dual-register necessity is understood, alternative single-register models are recognized as empirically inadequate. Resistance is low (0.07) because the constraint is not imposed; it is recognized by practitioners as the condition of their survival.
 *
 * PERSPECTIVAL GAP:
 *   The binary-forcing analyst seat experiences the constraint as a snare (their theoretical framework is suppressed by the phenomenon's refusal to binarize), while the practitioner seat experiences it as a mountain (the integration is just how ritual works). The engine computes this divergence from the structural data. The hybrid reading itself does not adjudicate — it describes the structural reality that produces the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Catastrophe survivor communities are beneficiaries (d ≈ 0.1) — the dual-register structure subsidizes their survival. Binary-forcing analysts are payers (d ≈ 0.8) — they bear the cost of analytical failure when they impose a false binary on the phenomenon. Ritual practitioners are agenda_setters with identity_locked exit (d ≈ 0.5) — they administer the practice but cannot exit without losing their role and the community's survival capacity. Memory studies scholars are observers (d = 0.5, analytical exit). The engine will compute effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmitting both registers through ritual's bottleneck) remains live — no community reports it solved. The mandate has not atrophied; the dual-register integration is actively maintained because survival still depends on it. Mandatrophy is not resolved; the constraint's function is current, not vestigial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the dual-register integration a genuine structural necessity for catastrophe survival (natural law of collective memory), or a constructed theoretical framework that benefits communities by legitimizing their integrated practice?',
    'Cross-cultural comparative study of catastrophe-survivor communities that lost one register: if all such communities collapsed or assimilated within 2-3 generations regardless of cultural context, the dual-register necessity is structural. If some communities survived with single-register rituals, the claim is constructed.',
    'If structural, the Mountain classification holds and FSM does not trigger. If constructed, FSM would reclassify as tangled_rope (coordination + extraction from analysts forced into binary error).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, empirical, 'False summit mountain ambiguity: natural-law status of dual-register integration vs. constructed theoretical framework.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural commitment to dual-register integration relate to the sibling readings of the same kernel?',
    'Map the structural disagreement: symbol_survival_reading locates survival in symbolic continuity alone (ε near zero for symbolic register, competence register treated as epiphenomenal); competence_transmission_reading locates survival in practical knowledge alone (ε near zero for competence register, symbolic register treated as epiphenomenal); hybrid_encoding_reading locates survival in the integration itself (low ε for integration, high ε for forced separation). The disagreement is located on the ontological status of the integration — is it the survival mechanism itself, or an analytical overlay?',
    'If integration is the mechanism, the hybrid reading''s low ε is accurate and the siblings are structurally incomplete. If integration is an overlay, the siblings capture the real (separate) mechanisms and the hybrid reading inflates ε by treating separation as extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer frame: this reading instantiates hybrid_encoding_reading of catastrophe_memory_survival kernel; siblings are symbol_survival_reading and competence_transmission_reading; disagreement located on whether survival requires integration or only one register.').

omega_variable(
    analyst_victim_status_legitimacy,
    'Are binary-forcing analysts genuine victims of the constraint (harmed by the phenomenon''s structure), or are they self-inflicting analytical error that the constraint merely reveals?',
    'Trace whether analysts who adopt the hybrid reading avoid the analytical failures of binary-forcing analysts. If hybrid-reading analysts produce better predictive models of community survival, the binary-forcing analysts'' victim status is self-inflicted. If even hybrid-reading analysts face structural barriers to publishing integrated models (disciplinary incentives, reviewer demands for clean theory), the victim status has structural force.',
    'If self-inflicted, the victim declaration overstates extraction and the constraint is purer Mountain. If structurally enforced, the constraint has a snare-like edge for analysts embedded in binary-forcing disciplines.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(analyst_victim_status_legitimacy, empirical, 'Whether analyst victimhood is structural or self-inflicted analytical error.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_mem_surv_hybrid_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cat_mem_surv_hybrid_tr_t25, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 25, 0.04).
narrative_ontology:measurement(cat_mem_surv_hybrid_tr_t50, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(cat_mem_surv_hybrid_tr_t75, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 75, 0.06).
narrative_ontology:measurement(cat_mem_surv_hybrid_tr_t100, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(cat_mem_surv_hybrid_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cat_mem_surv_hybrid_be_t25, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 25, 0.11).
narrative_ontology:measurement(cat_mem_surv_hybrid_be_t50, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(cat_mem_surv_hybrid_be_t75, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 75, 0.13).
narrative_ontology:measurement(cat_mem_surv_hybrid_be_t100, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 100, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(cat_mem_surv_hybrid_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(cat_mem_surv_hybrid_su_t25, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 25, 0.07).
narrative_ontology:measurement(cat_mem_surv_hybrid_su_t50, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 50, 0.08).
narrative_ontology:measurement(cat_mem_surv_hybrid_su_t75, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 75, 0.09).
narrative_ontology:measurement(cat_mem_surv_hybrid_su_t100, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__hybrid_encoding_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_encoding_reading of the catastrophe_memory_survival kernel. The kernel decomposes into three structurally distinct constraints: symbol_survival_reading (symbolic register only, ε ≈ 0.05, Mountain), competence_transmission_reading (competence register only, ε ≈ 0.05, Mountain), and hybrid_encoding_reading (dual-register integration, ε ≈ 0.12, Mountain with FSM candidate). The hybrid reading's ε is higher because it treats forced separation as extractive for analysts; the sibling readings treat the excluded register as negligible (ε ≈ 0 for that register). All three share the referent 'ritual enables catastrophe survival' but instantiate different constraints with different ε, beneficiary/victim structures, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__hybrid_encoding_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
