% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Hybrid Transformation Reading of Catastrophe Memory Ritual (Passover-type)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint story models the hybrid_transformation_reading of the
 *   catastrophe_memory_function kernel — the reading that ritual
 *   simultaneously encodes mourning-practice (D1/D4: bitter herbs,
 *   loss-memory, boundary norms) AND survival-competence (D5: seder
 *   performance, adaptive rehearsal, decentralized continuity). The Passover
 *   seder is the paradigmatic instance: the same ritual structure carries
 *   both the commemorative obligation to remember slavery (mourning) and the
 *   performative rehearsal of liberation (survival). This reading claims the
 *   dual function is structurally integrated, not accidental. The constraint
 *   is the standing ritual arrangement that enforces this dual encoding
 *   across generations.
 *
 * KEY AGENTS:
 *   - religious_leadership: agenda_setter (institutional/biographical/arbitrage/global) — sets liturgy, authorizes transmission, collects legitimacy
 *   - community_elders: beneficiary (organized/generational/constrained/global) — hold interpretive authority, receive status from transmission role
 *   - cultural_transmission_institutions: beneficiary (institutional/generational/arbitrage/global) — schools, museums, archives that institutionalize the ritual
 *   - younger_generations: payer (moderate/biographical/constrained/global) — bear participation costs, time, and identity demands; limited exit
 *   - marginalized_community_members: payer (powerless/biographical/trapped/local) — bear disproportionate costs (gendered labor, economic burden), minimal voice
 *   - ritual_practitioners_without_institutional_affiliation: excluded (moderate/biographical/mobile/regional) — would innovate or adapt but lack authorization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Hybrid Transformation Reading of Catastrophe Memory Ritual (Passover-type)").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__hybrid_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, '0e58c0bf-6218-454c-a31b-c104d7fbb65b').
narrative_ontology:cs_kernel_codification('0e58c0bf-6218-454c-a31b-c104d7fbb65b', fixed_text).
narrative_ontology:cs_authority_grounding('0e58c0bf-6218-454c-a31b-c104d7fbb65b', lineage).
narrative_ontology:cs_interpretation_layer_present('0e58c0bf-6218-454c-a31b-c104d7fbb65b').
narrative_ontology:cs_reading_relation('0e58c0bf-6218-454c-a31b-c104d7fbb65b', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e58c0bf-6218-454c-a31b-c104d7fbb65b', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('0e58c0bf-6218-454c-a31b-c104d7fbb65b', foundational, mourning_and_survival_structurally_integrated).
narrative_ontology:cs_axiom_status(mourning_and_survival_structurally_integrated, holdable).
narrative_ontology:cs_axiom_grounding('0e58c0bf-6218-454c-a31b-c104d7fbb65b', mourning_and_survival_structurally_integrated, deontological).
narrative_ontology:cs_axiom('0e58c0bf-6218-454c-a31b-c104d7fbb65b', secondary, dual_function_prevents_paralysis_and_amnesia).
narrative_ontology:cs_axiom_status(dual_function_prevents_paralysis_and_amnesia, holdable).
narrative_ontology:cs_axiom_grounding('0e58c0bf-6218-454c-a31b-c104d7fbb65b', dual_function_prevents_paralysis_and_amnesia, instrumental).
narrative_ontology:cs_reference_frame('0e58c0bf-6218-454c-a31b-c104d7fbb65b', exodus_sinai_dual_covenant).
narrative_ontology:cs_drift_state('0e58c0bf-6218-454c-a31b-c104d7fbb65b', contemporary_diaspora_condition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e58c0bf-6218-454c-a31b-c104d7fbb65b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, religious_leadership).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, community_elders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, cultural_transmission_institutions).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, younger_generations).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, marginalized_community_members).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, ritual_practitioners_without_institutional_affiliation).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, ritual_as_dual_function_memory_system).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, commemorative_practice_transmits_adaptive_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the liturgical text, authorizes ritual leaders, and determines which elements are obligatory vs. optional. Collects institutional legitimacy, resource flows (donations, endowments), and authority from maintaining the dual-function ritual. Can modify the ritual but faces institutional inertia and constituent expectations. Exit is arbitrage-grade — they can reform from within or join other traditions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, religious_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold interpretive authority and transmission responsibility. Receive status, respect, and relational centrality from their role in the ritual. Bear some costs (time, preparation) but net beneficiaries. Exit is constrained — stepping down means loss of identity and community position, but physical exit is possible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, community_elders, beneficiary,
    organized, generational, constrained, global).

% Schools, museums, archives, and media organizations that curate, teach, and institutionalize the ritual. Capture funding, cultural capital, and relevance from maintaining the tradition. Can pivot to other cultural forms if this one declines — exit is arbitrage-grade.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, cultural_transmission_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the primary participation costs: time (multi-day observance), material (special foods, travel), identity (public performance of belonging), and opportunity (work/school absence). The dual function doubles the burden — mourning elements demand emotional labor; survival elements demand skill acquisition. Exit is constrained: leaving means breaking with family, community, and identity framework, but is physically possible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, younger_generations, payer,
    moderate, biographical, constrained, global).

% Bear disproportionate costs: gendered domestic labor (preparation, cleaning), economic burden (special foods, time off work), and identity policing (strict observance as membership test). Have minimal voice in ritual shaping. Exit is trapped — leaving means total social and material severance from the only support network available.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, marginalized_community_members, payer,
    powerless, biographical, trapped, local).

% Independent ritual leaders, scholars, or innovators who would adapt the ritual (e.g., feminist seders, climate-adaptation seders, trauma-informed mourning) but lack authorization. Their exclusion is structural — the constraint's enforcement machinery (leadership control, communal policing) keeps innovation unofficial. They are mobile — can practice elsewhere or create parallel structures.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_practitioners_without_institutional_affiliation, excluded,
    moderate, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the dual problem of (1) maintaining group identity and boundary norms through shared catastrophic memory (mourning) and (2) transmitting adaptive capacities for survival under regime change, displacement, and persecution (survival rehearsal). The ritual integrates these so that mourning motivates survival preparation, and survival rehearsal prevents mourning from becoming despair.
% TRANSFER_FUNCTION: Moves time, labor, material resources, and identity commitment from younger_generations and marginalized_members to religious_leadership and cultural_institutions (legitimacy, continuity, funding). Community_elders receive status and relational centrality. The transfer is bidirectional in experience (participants also receive identity, community, skills) but structurally asymmetric — the agenda_setters capture institutional benefits while payers bear the costs.
% ABSENT_VOICES: Unaffiliated ritual practitioners and reform-minded community members who would adapt the ritual to contemporary catastrophes (climate displacement, digital surveillance, gender violence) are excluded by the authorization structure. They would object to the freezing of the ritual's adaptive dimension into a fixed performance. Their absence is maintained by leadership control of liturgical authority and communal policing of innovation.
% DISAPPEARANCE_RATIONALE: If the hybrid ritual vanished overnight, the community would lose both its primary mourning technology (structured communal grief with boundary maintenance) and its primary survival rehearsal (decentralized continuity practice). Secular commemoration would partially replace mourning but lacks the identity-binding force. No secular equivalent exists for the survival rehearsal function — the community would become more vulnerable to disruption. The institutional structures (leadership, schools, archives) built around the ritual would face legitimacy crisis.
% FOUNDING_PROBLEM: Post-catastrophe group survival: how to maintain identity and continuity when the catastrophe (enslavement, exile, genocide) threatens both physical existence and collective memory. The ritual was built to solve this by encoding the catastrophe memory in a form that simultaneously mourns the loss and rehearses the adaptive behaviors needed for survival.
% FOUNDING_PROBLEM_CORROBORATION: Religious leadership and community elders attest the founding problem is live — ongoing persecution and assimilation threats make survival rehearsal necessary. Younger generations and marginalized members attest it is dead — the original catastrophe is historical, survival is achieved, and the ritual now serves institutional maintenance. Independent historians and anthropologists (outside the beneficiary set) corroborate that the ritual's adaptive function has shifted from concrete survival skills to symbolic identity performance, supporting the 'contested' status.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects real but moderate transfer: the ritual demands significant time, material resources, and identity commitment from participants, particularly younger generations and marginalized members, while leadership and institutions capture legitimacy and continuity benefits. Suppression (0.38) is present but not total: alternatives exist (secular commemoration, individualized practice) but are discouraged through communal pressure and identity framing. Theater ratio (0.28) indicates modest performative drift — some ritual elements persist as theater rather than functional transmission, but core adaptive rehearsal remains active. Accessibility collapse (0.52) and resistance (0.45) are moderate: the ritual's dual function means alternatives only partially collapse (secular mourning exists, but not secular survival rehearsal), and resistance manifests as quiet non-participation rather than open revolt.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (religious_leadership) experiences this as genuine coordination — they built and maintain a system that solves both memorial and adaptive problems. The payer seats (younger_generations, marginalized_members) experience it as extraction with coercive maintenance — they pay the costs but did not choose the dual function. The excluded seat (unaffiliated_practitioners) sees a foreclosed innovation space. The engine computes these divergent seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious_leadership and cultural_institutions are structural beneficiaries: they set the agenda, control interpretation, and capture institutional legitimacy (d ~ 0.15). Community_elders are secondary beneficiaries with constrained exit (d ~ 0.25). Younger_generations are primary targets: they bear participation costs across both mourning and survival dimensions with limited exit (d ~ 0.75). Marginalized_members are full targets: trapped by identity and material dependence (d ~ 0.9). Unaffiliated_practitioners are excluded — their structural position is not on the beneficiary/target axis but outside the constraint's coordination function entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophe survival and group continuity) is contested — some argue it is live (ongoing threats), others that it is dead (sovereignty achieved) but the arrangement persists. The hybrid reading prevents mislabeling: if read as pure mourning, it looks like a snare (extraction via guilt); if read as pure survival, it looks like a scaffold (transitional coordination). The dual function makes it a genuine tangled_rope — both coordination and extraction are structurally real and inseparable in the current arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_hybrid_vs_pure_mourning,
    'Does the hybrid_transformation_reading foreclose the mourning_practice_reading, or do they coexist as distinct commitments within the same tradition?',
    'Comparative liturgical analysis across communities: if communities holding the hybrid reading explicitly reject pure mourning as insufficient, forecloses; if both readings are held by different factions without mutual exclusion, coexists_with.',
    'If forecloses, the hybrid reading structurally displaces the mourning-only reading within a single framework; if coexists_with, both remain live positions in the kernel''s reading space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_hybrid_vs_pure_mourning, conceptual, 'Structural relationship between hybrid and mourning-practice readings of the catastrophe memory kernel').

omega_variable(
    kernel_reading_hybrid_vs_pure_survival,
    'Does the hybrid_transformation_reading foreclose the survival_competence_reading, or do they coexist?',
    'Institutional ethnography: if survival-competence advocates treat the hybrid reading as dilution of adaptive focus, forecloses; if both are treated as complementary emphases, coexists_with.',
    'Determines whether the kernel''s reading space contains mutually exclusive positions or a spectrum of compatible emphases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_hybrid_vs_pure_survival, conceptual, 'Structural relationship between hybrid and survival-competence readings of the catastrophe memory kernel').

omega_variable(
    extraction_boundary_mourning_vs_survival,
    'Where does the extractive component of this constraint lie — in the mourning obligation (D1/D4) or the survival rehearsal (D5), or both?',
    'Participant cost-accounting: measure time, material, and opportunity costs attributed to mourning elements vs. survival-transmission elements; correlate with reported coercion.',
    'If extraction concentrates in mourning, the constraint is a snare with survival cover; if in survival, it is a scaffold with mourning cover; if split, tangled_rope is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_boundary_mourning_vs_survival, empirical, 'Attribution of extractive burden across the constraint''s dual functions').

omega_variable(
    identity_lock_in_ritual_transmission,
    'Are younger_generations identity_locked to this ritual through internalized identity fusion, or do they remain structurally constrained but mobile?',
    'Longitudinal cohort study tracking ritual participation, identity measures, and exit behavior across generations; compare communities with high vs. low institutional enforcement.',
    'If identity_locked, effective extraction amplifies toward full-target for that seat; if constrained, standard directionality derivation applies. Changes per-seat classification for the primary victim group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_ritual_transmission, empirical, 'Whether the primary victim group''s exit is structurally blocked or identity-fused').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_mem_hybrid_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cat_mem_hybrid_tr_t50, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 50, 0.16).
narrative_ontology:measurement(cat_mem_hybrid_tr_t100, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 100, 0.2).
narrative_ontology:measurement(cat_mem_hybrid_tr_t150, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 150, 0.23).
narrative_ontology:measurement(cat_mem_hybrid_tr_t200, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 200, 0.26).
narrative_ontology:measurement(cat_mem_hybrid_tr_t250, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 250, 0.28).

% Extraction over time
narrative_ontology:measurement(cat_mem_hybrid_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cat_mem_hybrid_be_t50, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(cat_mem_hybrid_be_t100, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(cat_mem_hybrid_be_t150, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 150, 0.38).
narrative_ontology:measurement(cat_mem_hybrid_be_t200, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 200, 0.4).
narrative_ontology:measurement(cat_mem_hybrid_be_t250, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 250, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cat_mem_hybrid_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(cat_mem_hybrid_su_t50, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 50, 0.28).
narrative_ontology:measurement(cat_mem_hybrid_su_t100, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 100, 0.32).
narrative_ontology:measurement(cat_mem_hybrid_su_t150, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 150, 0.35).
narrative_ontology:measurement(cat_mem_hybrid_su_t200, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 200, 0.37).
narrative_ontology:measurement(cat_mem_hybrid_su_t250, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 250, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__hybrid_transformation_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the catastrophe_memory_function kernel family. The hybrid reading integrates D1/D4 (mourning) and D5 (survival) into a single ritual structure. The mourning_practice_reading isolates the commemorative function; the survival_competence_reading isolates the adaptive function. All three readings share the same referent (the standing ritual arrangement) but author different ε values and beneficiary/victim structures. The hybrid reading's ε (0.42) sits between the mourning reading's lower extraction (commemorative coordination) and the survival reading's higher extraction (adaptive rehearsal as institutional maintenance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__hybrid_transformation_reading, moderate, 0.75).
constraint_indexing:directionality_override(catastrophe_memory_function__hybrid_transformation_reading, powerless, 0.9).
constraint_indexing:directionality_override(catastrophe_memory_function__hybrid_transformation_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
