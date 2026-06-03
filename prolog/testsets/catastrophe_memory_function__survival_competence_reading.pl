% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Catastrophe Memory Function: Survival-Competence Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   The catastrophe memory function constraint models how ritual preserves
 *   and transmits survival-competence — adaptive knowledge for responding to
 *   institutional collapse, dispersal, and organized violence. This
 *   constraint story instantiates ONE READING of a contested kernel about
 *   ritual's function in collective memory. The survival-competence reading
 *   frames the ritual (exemplified by Passover) as encoding specific adaptive
 *   behaviors: how to organize under decentralized conditions, how to
 *   maintain identity across forced dispersal, how to recognize and respond
 *   to recurring hazards, how to sustain institutional capacity when central
 *   authority fails. This reading distinguishes itself from the
 *   mourning-practice reading (which emphasizes ritual's function in
 *   boundary-maintenance and loss-memory) and from the hybrid-transformation
 *   reading (which holds both functions simultaneously). The
 *   survival-competence reading isolates the transmission of adaptive
 *   institutional knowledge as the primary legitimating function of the
 *   ritual, not as a secondary or emergent effect. The constraint is
 *   tangled_rope because it simultaneously coordinates genuine survival
 *   knowledge (legitimate coordination function) while extracting memory
 *   labor from performers and operational burden from decentralized nodes
 *   (asymmetric extraction). The extractiveness trajectory over the
 *   measurement interval (0.18 → 0.32) reflects institutional routinization
 *   of the ritual: as performance becomes standardized and formalized, the
 *   adaptive knowledge becomes more accessible but also more subject to
 *   institutional control, increasing the extraction component.
 *
 * KEY AGENTS:
 *   - Individual Ritual Performer: Primary victim (powerless/trapped) — bears embodied memory labor with no exit option
 *   - Decentralized Community Node: Secondary victim (moderate/constrained) — maintains local infrastructure under resource constraint and social obligation
 *   - Institutional Knowledge Custodian: Primary beneficiary (institutional/arbitrage) — captures symbolic capital and authority through knowledge stewardship
 *   - Organized Memory Collective: Secondary beneficiary-victim (organized/constrained) — benefits from distributed knowledge while constrained by coordination burden
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing institutional control of distributed knowledge as inevitable feature of collective memory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.32).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.48).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Catastrophe Memory Function: Survival-Competence Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '34160000-db0a-4b31-bdbb-e683115fecd4').
narrative_ontology:cs_kernel_codification('34160000-db0a-4b31-bdbb-e683115fecd4', fixed_text).
narrative_ontology:cs_authority_grounding('34160000-db0a-4b31-bdbb-e683115fecd4', lineage).
narrative_ontology:cs_interpretation_layer_present('34160000-db0a-4b31-bdbb-e683115fecd4').
narrative_ontology:cs_reading_relation('34160000-db0a-4b31-bdbb-e683115fecd4', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('34160000-db0a-4b31-bdbb-e683115fecd4', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('34160000-db0a-4b31-bdbb-e683115fecd4', foundational, adaptive_knowledge_primary_function).
narrative_ontology:cs_axiom_status(adaptive_knowledge_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('34160000-db0a-4b31-bdbb-e683115fecd4', adaptive_knowledge_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('34160000-db0a-4b31-bdbb-e683115fecd4', foundational, survival_capacity_requires_embodied_rehearsal).
narrative_ontology:cs_axiom_status(survival_capacity_requires_embodied_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('34160000-db0a-4b31-bdbb-e683115fecd4', survival_capacity_requires_embodied_rehearsal, empirically_contingent).
narrative_ontology:cs_reference_frame('34160000-db0a-4b31-bdbb-e683115fecd4', distributed_survival_knowledge_transmission).
narrative_ontology:cs_drift_state('34160000-db0a-4b31-bdbb-e683115fecd4', contemporary_institutional_standardization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34160000-db0a-4b31-bdbb-e683115fecd4', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, institutional_knowledge_inheritors).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, post_catastrophe_adaptive_communities).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, ritual_performers_memory_labor).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, decentralized_nodes_organizational_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL RITUAL PERFORMER (SNARE) — Trapped in the performance of survival-competence transmission. Carries the embodied knowledge and must execute the ritual year after year without exit. Experiences this as obligation, not as coordination benefit. The performer bears the memory labor cost (emotional, temporal, cognitive) while the adaptive knowledge is extracted and held institutionally. No alternative means to transmit; no ability to stop participating without severing community membership.
constraint_indexing:constraint_classification(catastrophe_memory_function__survival_competence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DECENTRALIZED COMMUNITY NODE (TANGLED ROPE) — Bears operational burden of maintaining local ritual infrastructure (space, coordination, continuity across disruptions). Constrained by resource requirements and social expectation. But also genuinely benefits: the ritual transmits adaptive knowledge for surviving dispersal, economic disruption, and institutional breakdown — knowledge that kept communities viable through multiple catastrophes. Coordination and extraction coexist: the node benefits from receiving survival-competence encoded in ritual while being extracted through the work of maintaining and performing it.
constraint_indexing:constraint_classification(catastrophe_memory_function__survival_competence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL KNOWLEDGE CUSTODIAN (ROPE) — Benefits from arbitrage over the ritual's two functions: can extract symbolic capital (legitimacy, authority, historical continuity) while the survival-competence function continues without institutional control. The institutional actor experiences the ritual as coordination of a dispersed memory commons — they see themselves as stewards of collective knowledge. The asymmetry is real: institutional actors can walk away into other domains; they have optionality. But from their perspective, they are performing a service (knowledge stewardship) and the constraint is fundamentally coordinative.
constraint_indexing:constraint_classification(catastrophe_memory_function__survival_competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED MEMORY COLLECTIVE (TANGLED ROPE) — Organized agents (diaspora networks, archive projects, educational institutions) see the ritual as a distributed coordination mechanism for survival knowledge. They have some agency: can formalize curriculum, digitize practices, standardize transmission methods. But they remain constrained by the need to maintain embodied practice — the survival-competence function depends on humans doing the ritual, not on abstract knowledge capture. They benefit from access to distributed knowledge; they are extracted through the labor of coordination and standardization.
constraint_indexing:constraint_classification(catastrophe_memory_function__survival_competence_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal, civilizational view, ritual preservation of survival-competence appears as an immutable feature of human adaptive capacity: communities that preserve catastrophe-response knowledge through embodied practice survive dispersal and collapse better than those that rely on inscription alone. This perspective risks naturalizing a contingent institutional arrangement (who controls the knowledge encoding, who bears the performance burden, who benefits from institutional continuity) as an inevitable law of collective memory. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(catastrophe_memory_function__survival_competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_function__survival_competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_function__survival_competence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The ritual does transmit genuinely adaptive knowledge — survival strategies for dispersed communities are not fictitious benefits. But extraction occurs through two mechanisms: (1) institutional actors capture and formalize knowledge they did not create, establishing themselves as custodians and legitimacy sources, and (2) individual performers bear the memory labor cost while institutional actors capture the symbolic capital. The 0.32 value reflects that the coordination benefit is real (knowledge transmission works) but so is the extraction (control and labor asymmetry). The trajectory shows rising extractiveness as institutional standardization increases — the ritual becomes more theatricalized, less about actual survival skill transmission and more about institutional authority maintenance. Suppression (0.48): Moderate-high. Barriers include: strong social obligation to perform (cannot exit without community severance), tacit embodied knowledge that resists institutional capture (need to keep performing to transmit), distributed infrastructure that makes coordination difficult (suppresses alternatives like archive-based transmission), and institutional control of interpretation (suppresses alternative readings of the ritual's function). Theater ratio (0.55): Rising. Over the interval, institutional standardization increases the performative component relative to adaptive function. Early-interval ritual teaching focuses on actual survival strategies; later-interval ritual emphasizes proper form, institutional continuity, and symbolic correctness. The rise from 0.38 to 0.55 marks the transition from survival-practice to memorial-performance. Current state reflects moderate theater — still transmits real knowledge, but performance correctness is increasingly decoupled from adaptive utility.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal in this constraint. Individual performers see a snare (pure extraction without option to exit). Decentralized nodes see tangled rope (genuine survival benefit mixed with operational burden). Institutional custodians see rope (pure coordination of knowledge transmission). Organized collectives see tangled rope from a different position (benefits from knowledge access while constrained by coordination demands). The analytical observer risks seeing a mountain (ritual as inevitable feature of how collective memory works) when the structural data reveals extraction and theatrical drift. The gap reflects the fundamental asymmetry: those who perform and maintain the ritual see it as extractive obligation; those who benefit from institutionalization see it as coordination service; those who transmit knowledge see themselves as stewards, not extractors. This reading isolates the survival-competence function as the primary meaning, which makes the institutional control of that knowledge the salient extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional custodians derive d ≈ 0.15 (beneficiary with arbitrage exit options), experiencing negative effective extraction (they capture capital). Individual performers derive d ≈ 0.95 (victims with trapped exit), experiencing maximum effective extraction (they bear memory labor with no alternative). Decentralized nodes derive d ≈ 0.58 (moderate position — genuinely benefit from survival knowledge but constrained by operational burden). Organized collectives derive d ≈ 0.52 (mixed position — have some agency through formalization but remain resource-constrained). The analytical observer derives d ≈ 0.72 (external to the extraction flow, but at risk of naturalizing institutional arrangements). Suppression is not scaled by directionality — it remains 0.48 as a structural property of the constraint (strong obligation, tacit knowledge, distributed coordination difficulty).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by isolating survival-competence transmission as the constraint's primary function. The mourning_practice_reading would resolve it differently, prioritizing boundary-maintenance and identity preservation. The hybrid_transformation_reading would hold both functions equally. By declaring survival-competence as primary and routing the mourning-practice vs survival-competence question to an omega variable (reading_kernel_ambiguity), this story documents the structural source of the classification indeterminacy: the kernel itself is ambiguous (a contested ritual code that can be read as emphasizing either function). The tangled_rope classification is appropriate to this reading because it acknowledges both the genuine coordination value (survival knowledge does protect communities) and the extraction (institutional control, performance burden, labor asymmetry). If the reading_kernel_ambiguity omega is resolved to show mourning-practice as primary, this story should be reclassified or the mourning_practice_reading should replace it as the primary analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    embodied_vs_inscribed_sufficiency,
    'Does the survival-competence encoded in ritual transmission genuinely require embodied performance, or could it be captured in written/digital form with equal adaptive efficacy?',
    'Comparative analysis: communities that switched to archive-based knowledge transmission vs those maintaining embodied practice; post-catastrophe adaptive outcomes measured against pre-catastrophe encoded knowledge.',
    'If embodied required: the ritual''s extraction mechanism is legitimate coordination cost (Rope perspective strengthens). If inscription sufficient: the performance burden is unnecessary extraction (Snare perspective strengthens). This resolves whether the extraction is true cost-of-coordination or artificial scarcity creation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_vs_inscribed_sufficiency, empirical, 'Whether survival-competence requires embodied ritual or can be transmitted through inscription').

omega_variable(
    institutional_extraction_vs_knowledge_stewardship,
    'When institutional actors control ritual knowledge (canonicalization, standardization, curriculum), are they performing stewardship or extracting symbolic/political capital from distributed knowledge?',
    'Analysis of who controls interpretation changes over time; whether institutional modifications improve or degrade adaptive utility of transmitted knowledge; whether institutional actors have alternative value sources if they relinquish knowledge custodianship.',
    'If stewardship: institutional perspective''s rope classification is accurate (genuine coordination). If extraction: rope should downgrade to snare (institutional actors capture knowledge commons for capital). Determines baseline extractiveness and whether this is a false summit (mountain naturalization of extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_extraction_vs_knowledge_stewardship, empirical, 'Whether institutional knowledge control represents stewardship or extractive appropriation').

omega_variable(
    catastrophe_frequency_threshold,
    'What catastrophe frequency threshold determines whether survival-competence knowledge actually saves lives vs. becomes performative tradition disconnected from real adaptive value?',
    'Historical frequency analysis of actual catastrophes matching ritual-encoded scenarios; correlation between ritual knowledge and post-catastrophe community resilience in conditions where catastrophes occurred.',
    'If frequent (< 50 years): survival-competence is genuinely adaptive (tangled_rope justified). If rare (> 200 years): ritual becomes performative memorial without adaptive function (piton classification from multiple perspectives). This addresses whether the constraint''s current form is matched to current hazard environment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophe_frequency_threshold, empirical, 'Catastrophe frequency determining whether encoded knowledge remains adaptive').

omega_variable(
    reading_kernel_ambiguity,
    'Does this ritual kernel genuinely transmit survival-competence (D5 reading) or primarily encode mourning-practice and boundary-norms (D1/D4 reading), with survival-competence as secondary effect?',
    'Ethnographic comparison: which aspects of ritual practice are emphasized in transmission, what knowledge is actually retained and used post-catastrophe, what functions ritual serves during non-catastrophe periods.',
    'If D5 primary: this reading''s constraint story is correct. If D1/D4 primary with D5 secondary: the mourning_practice_reading is the more accurate reading of the same kernel; this reading commits a reading misidentification error. If both equally primary: hybrid_transformation_reading is more structurally accurate than either single reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Kernel reading identification — survival-competence vs mourning-practice primacy').

omega_variable(
    decentralization_vs_institutional_control_tradeoff,
    'Do institutional standardization and control mechanisms enhance or degrade the decentralized adaptive capacity that gives this ritual survival value?',
    'Analysis of dispersed community outcomes under high institutional control vs low institutional control; whether standardization improves knowledge retention or rigidifies adaptive responses. Compare communities with strong institutional transmission infrastructure vs those with distributed peer-learning networks.',
    'If institutional control enhances decentralization: institutional actors'' rope perspective is justified (coordination beneficial). If institutional control undermines decentralization: the constraint creates false institutional ownership of distributed knowledge (higher extractiveness, institutional actor should see snare not rope). Determines directionality for institutional actor and affects baseline extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_vs_institutional_control_tradeoff, empirical, 'Whether institutional control enhances or degrades decentralized adaptive capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_survival_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(catmem_survival_tr_t3, catastrophe_memory_function__survival_competence_reading, theater_ratio, 3, 0.47).
narrative_ontology:measurement(catmem_survival_tr_t6, catastrophe_memory_function__survival_competence_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(catmem_survival_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(catmem_survival_be_t3, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(catmem_survival_be_t6, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 6, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(catmem_survival_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(catmem_survival_su_t3, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(catmem_survival_su_t6, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, attachment_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, institutional_knowledge_commons_appropriation).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_function is a contested kernel with multiple readings, each isolating different structural elements. This story (survival_competence_reading) models the constraint when survival-competence transmission is the primary function. The mourning_practice_reading models the constraint when boundary-maintenance and memorial obligation are primary. The hybrid_transformation_reading models the constraint when both are held equally. Each story has its own epsilon and classification. They are not different observations of the same constraint — they are different readings of the same contested kernel that generate genuinely different constraints (different mechanisms, different beneficiaries, different extraction patterns) depending on which reading is instantiated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__survival_competence_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
