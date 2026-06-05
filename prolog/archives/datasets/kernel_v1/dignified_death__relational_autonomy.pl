% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__relational_autonomy, []).

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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Dignified Death: Relational Autonomy (distributed decision authority with procedural safeguards)
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The relational autonomy reading of the dignified death kernel locates
 *   dignity not in individual self-determination alone but in the quality of
 *   relationships and the processes through which decision-making unfolds.
 *   This reading instantiates a procedurally-bounded coordination mechanism
 *   in which the patient retains final authority but operates within a triad
 *   (patient-family-clinician) structured by professional and ethical
 *   safeguards. The constraint is a ROPE: it solves a genuine coordination
 *   problem (how to honor the dying person's values while respecting family
 *   continuity and protecting clinician conscience) with moderate extractive
 *   overhead. The theater_ratio (0.55) reflects that procedural safeguards
 *   carry significant performative content — ethics consults, family
 *   conferences, and documentation requirements consume resources and time
 *   that may be partially theater. However, the coordination function is
 *   genuine: the relational framework provides information-sharing pathways,
 *   moral deliberation spaces, and institutional safe harbors that the pure
 *   autonomy reading lacks. The constraint is one reading of a contested
 *   kernel about human dignity at end-of-life. The competing readings —
 *   autonomy_primary (dignity resides in self-determination) and
 *   sanctity_primary (dignity resides in life's intrinsic value) — each
 *   establish different victim sets and beneficiary structures. The
 *   relational_autonomy reading produces a distinctive configuration: its
 *   victims are those excluded from the decision process (isolated patients,
 *   marginalized families, clinicians denied moral participation), while its
 *   beneficiaries are the relational networks and clinical institutions that
 *   gain standing and moral authority through the framework.
 *
 * KEY AGENTS:
 *   - Patient: Moderate power, constrained exit — retains decision authority but embedded in relational obligations and procedural requirements
 *   - Family members: Powerless individually but integrated into decision structure with genuine moral standing — constrained exit (relational and moral costs to declining participation)
 *   - Clinical institution: Institutional actor with arbitrage options — benefits from explicit protocols reducing liability and distributing moral burden
 *   - Ethics committees/bioethics professionals: Organized advocates for relational framework — constrain procedural structure and educate about relational values
 *   - Relational ethics advocates: Coordinated moral and intellectual movement — shape policy and professional standards around relational autonomy principle
 *   - Isolated or marginalized patients: Powerless and potentially trapped if family unavailable or hostile — may be victimized by relational framework if it overweights family voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.38).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.42).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Dignified Death: Relational Autonomy (distributed decision authority with procedural safeguards)").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, 'bd59b831-5b6b-4c36-af68-7274e6b7bd3c').
narrative_ontology:cs_kernel_codification('bd59b831-5b6b-4c36-af68-7274e6b7bd3c', formalized).
narrative_ontology:cs_authority_grounding('bd59b831-5b6b-4c36-af68-7274e6b7bd3c', lineage).
narrative_ontology:cs_interpretation_layer_present('bd59b831-5b6b-4c36-af68-7274e6b7bd3c').
narrative_ontology:cs_reading_relation('bd59b831-5b6b-4c36-af68-7274e6b7bd3c', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('bd59b831-5b6b-4c36-af68-7274e6b7bd3c', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('bd59b831-5b6b-4c36-af68-7274e6b7bd3c', foundational, dignity_is_relational).
narrative_ontology:cs_axiom_status(dignity_is_relational, holdable).
narrative_ontology:cs_axiom_grounding('bd59b831-5b6b-4c36-af68-7274e6b7bd3c', dignity_is_relational, deontological).
narrative_ontology:cs_axiom('bd59b831-5b6b-4c36-af68-7274e6b7bd3c', foundational, decision_authority_distributed_across_triad).
narrative_ontology:cs_axiom_status(decision_authority_distributed_across_triad, holdable).
narrative_ontology:cs_axiom_grounding('bd59b831-5b6b-4c36-af68-7274e6b7bd3c', decision_authority_distributed_across_triad, conventional).
narrative_ontology:cs_reference_frame('bd59b831-5b6b-4c36-af68-7274e6b7bd3c', relational_dignity_and_embeddedness).
narrative_ontology:cs_drift_state('bd59b831-5b6b-4c36-af68-7274e6b7bd3c', contemporary_bioethics_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bd59b831-5b6b-4c36-af68-7274e6b7bd3c', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, relational_network).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, clinical_collaboration).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, family_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT WITHIN RELATIONAL NETWORK (ROPE) — The patient's authority is genuine but bounded by relational obligations and procedural requirements. Not trapped (can exit via refusal or jurisdictional change, at cost), but constrained by the requirement to engage family and clinician perspectives. The coordination function is real: the triadic process genuinely improves decision quality through information-sharing and moral deliberation. Moderate power, constrained exit.
constraint_indexing:constraint_classification(dignified_death__relational_autonomy, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: CLINICAL INSTITUTION (ROPE) — Healthcare system benefits from explicit procedural protocols that reduce liability risk, provide clinical guidance, and distribute moral burden across the triad rather than concentrating it on individual clinicians. The constraint coordinates genuine institutional interests (safe harbor from prosecution, shared decision-making reducing clinician moral distress) with patient/family interests. Net beneficiary through arbitrage — can shift to different jurisdictional regimes or practice models.
constraint_indexing:constraint_classification(dignified_death__relational_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FAMILY MEMBER (ROPE) — Family is integrated into the decision structure, conferring standing and obligation simultaneously. Constrained exit (can decline to participate, but carries relational and moral cost). Powerless individually but not victimized — the relational autonomy model treats family voice as morally authoritative, not subordinate. The coordination function is genuine: family provides continuity, understands patient's values in lived context, and bears ongoing relational consequences.
constraint_indexing:constraint_classification(dignified_death__relational_autonomy, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RELATIONAL ETHICS COALITION (ROPE) — Organized actors (bioethics institutions, family advocacy groups, relational philosophy networks) see the constraint as a solved coordination problem at the generational level. The procedural safeguards are the solution itself — they embed relational values into law and practice. Constrained exit (must work within existing health law frameworks) but high agency (can influence policy, standards, education).
constraint_indexing:constraint_classification(dignified_death__relational_autonomy, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / FALSE SUMMIT CANDIDATE (MOUNTAIN) — From a civilizational perspective, relational interdependence appears as an immutable fact of human existence — dignity is inherently relational because persons are constituted through relational webs. However, this perspective risks naturalizing what is actually a contested reading of the kernel. The engine will flag this as a false summit because identifiable beneficiaries (clinical institutions, family structures) extract institutional and relational value from the 'natural' framing. This perspective instantiates the oracle gap: the analyst cannot see from within the relational frame that the frame itself is a contested axiom.
constraint_indexing:constraint_classification(dignified_death__relational_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__relational_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dignified_death__relational_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dignified_death__relational_autonomy, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(dignified_death__relational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The relational autonomy reading extracts value for clinical institutions (safe harbor, reduced clinician moral distress, institutional coordination) and for family structures (moral standing, ongoing relational investment). However, extractiveness is not high because the patient retains genuine decision authority and the coordination function is real — the triadic process improves information quality and moral deliberation. The beneficiaries are identified and legitimate; the extraction is not coercive. Suppression (0.42): Moderate. The constraint includes procedural requirements that suppress some alternatives — a patient cannot unilaterally request assisted dying without family and clinician participation, and in some jurisdictions, isolation or family opposition creates practical barriers to decision-making. However, suppression is not severe — exit options exist (change jurisdiction, decline participation and accept consequences, pursue refusal). Theater ratio (0.55): Moderate. Procedural safeguards carry significant performative content (ethics consults often occur after clinical decisions are made, family conferences may serve legitimation rather than genuine deliberation), but the coordination function is real enough to prevent classification as high theater. As implementation matures (rising from 0.48 to 0.55 over interval), procedural requirements become more routinized and less connected to genuine moral deliberation — modest theater drift upward.
 *
 * PERSPECTIVAL GAP:
 *   The patient within relational context experiences coordination (Rope) — the triadic process genuinely improves decision-making. The clinical institution experiences arbitrage-based beneficiary status (Rope) — explicit protocols reduce liability and moral burden. The family experiences both integration and constraint (Rope) — moral standing but also obligation. The relational ethics coalition experiences solved coordination (Rope) — procedural safeguards embed relational values. The analytical observer risks seeing natural law (Mountain) — relational interdependence as immutable human fact — but the engine flags this as false summit because clinical institutions and family structures benefit from the 'natural' framing. The perspectival gap between autonomy_primary reading (which sees the patient as primary victim excluded from pure self-determination) and relational_autonomy reading (which sees isolated patients as victims of relational framework when family unavailable) reveals the kernel contest. Different readings define victim sets differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Patient directionality (d ≈ 0.55): Moderate target. The patient is a victim of the relational constraint in the sense that their decision authority is bounded by procedural requirement and family involvement. However, the patient is also a beneficiary (gains information, moral deliberation, family continuity consideration). The moderate power level and constrained exit produce d in the mid-range. Family directionality (d ≈ 0.40): Moderate beneficiary. Family gains moral standing and voice in the decision process, deriving authority from relational positioning rather than patient delegation. Constrained exit means family cannot easily opt out of either the relational obligation or the procedural requirement. Clinical institution directionality (d ≈ 0.20): Beneficiary with arbitrage. Institutional actor gains safe harbor and coordination benefits with low cost — can exit to different jurisdictions or practice models (arbitrage exit). No override needed; derived d is accurate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_versus_relational_framing,
    'Is dignity grounded in individual self-determination (sibling: autonomy_primary) or in relational interdependence (this reading)?',
    'Longitudinal case analysis: do patients whose relational network strongly opposes their stated wishes achieve dignity through the decision process? Do isolated patients with no family achieve dignity through pure autonomy? Legal/clinical outcomes tracking recognition of dignity in each framing.',
    'If autonomy-primary correct: relational_autonomy is paternalistic masking; constraint type shifts toward tangled_rope (mixed coordination + extraction of autonomous choice). If relational correct: autonomy_primary is atomistic misreading; constraint type remains rope. If both are live: the kernel is genuinely contested (coexists_with relation holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_versus_relational_framing, conceptual, 'Whether dignity grounds in individual autonomy or relational interdependence').

omega_variable(
    procedural_sufficiency_for_dignity,
    'Do procedural safeguards (ethics consults, family conferences, documentation requirements) substantively protect dignity, or do they constitute theater masking underlying power asymmetries?',
    'Empirical analysis of case outcomes: do procedural interventions change decision trajectories? Do patients/families report moral agency in the process? Comparison of theater_ratio across jurisdictions with varying procedural intensity.',
    'If substantive: theater_ratio ≤ 0.50, constraint remains rope. If theater: theater_ratio ≥ 0.65, constraint shifts toward piton (degraded ritual) or tangled_rope (procedural theater masking extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_sufficiency_for_dignity, empirical, 'Whether procedural safeguards substantively protect dignity or constitute theater').

omega_variable(
    family_voice_authority_grounding,
    'Is family voice authority grounded in family''s epistemic access to patient''s values (knowledge authority), or in family''s relational standing (moral authority independent of patient''s known preferences)?',
    'Analysis of case law and ethics committee decisions: how do tribunals justify family authority when family preferences diverge from patient''s documented wishes? What grounding do they invoke?',
    'If knowledge-based: family serves coordination function (legitimate epistemic contribution). If relational-standing-based: family serves extraction function (institutional delegation to intimate relationships). Different resolutions produce different ε estimates and different effections on mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_voice_authority_grounding, conceptual, 'Authority grounding for family voice: epistemic access vs. relational standing').

omega_variable(
    contested_kernel_reading_status,
    'Is the relational_autonomy reading an established alternative to autonomy_primary and sanctity_primary, or is it a new reading being constructed through bioethics scholarship and case law?',
    'Genealogy of relational autonomy concept in bioethics: when did it emerge as distinct reading? How much institutional/legal uptake exists? Is it coexisting live alternative or emerging challenger to establishment readings?',
    'If established alternative: coexists_with relation holds for both siblings. If emerging: reading_relations may shift to influences relation (relational_autonomy pushing on autonomy_primary practices without fully foreclosing autonomy_primary claims). Status affects confidence in axiom durability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_kernel_reading_status, conceptual, 'Relational autonomy reading status: established alternative vs. emerging challenger').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_rel_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.48).
narrative_ontology:measurement(dign_rel_tr_t5, dignified_death__relational_autonomy, theater_ratio, 5, 0.52).
narrative_ontology:measurement(dign_rel_tr_t10, dignified_death__relational_autonomy, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(dign_rel_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dign_rel_be_t5, dignified_death__relational_autonomy, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(dign_rel_be_t10, dignified_death__relational_autonomy, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dign_rel_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(dign_rel_su_t5, dignified_death__relational_autonomy, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(dign_rel_su_t10, dignified_death__relational_autonomy, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, attachment_coordination).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, clinical_conscience_protection).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, family_surrogate_authority).

% DUAL FORMULATION NOTE:
% The dignified_death kernel decomposes into three constraint stories corresponding to three readings: autonomy_primary (ε ≈ 0.20, rope), relational_autonomy (ε ≈ 0.38, rope), sanctity_primary (ε ≈ 0.15, mountain). Each reading produces different victim sets and beneficiary structures. The stories are linked via network.affects_constraints as siblings competing for institutional and legal authority. Upstream constraints (clinical_conscience_protection, family_surrogate_authority) establish supporting institutional frameworks that different readings leverage differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
