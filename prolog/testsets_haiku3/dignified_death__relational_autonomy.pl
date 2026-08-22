% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational Autonomy in End-of-Life Decision-Making
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   Relational autonomy in end-of-life care is ONE READING of a contested
 *   kernel about the nature of dignity and the authority to determine the
 *   timing and method of death. This story instantiates the reading that
 *   dignity emerges from relational context — that authority over life-ending
 *   decisions is neither located in the isolated individual (autonomy_primary
 *   reading) nor vested entirely in transcendent moral law (sanctity_primary
 *   reading), but distributed across a procedurally structured triad of
 *   patient, family, and clinician. The relational reading treats exclusion
 *   from this deliberative process as the core harm; it claims the
 *   coordination function solves problems of isolated decision-making and
 *   clinical paternalism through procedural embedding. Yet the same
 *   procedural machinery can suppress voices outside the triad and burden
 *   patients without strong relational ties. The authored metrics reflect
 *   modest extractiveness (0.38 terminal) because the constraint coordinates
 *   real deliberation while simultaneously extracting gatekeeping authority
 *   to clinicians and procedural compliance burden to patients.
 *
 * KEY AGENTS:
 *   - Patient: individual with decision-making capacity, legally empowered but procedurally constrained to relational deliberation
 *   - Family members: formal consultative seat with documented voice and veto capacity in cases of incapacity or inconsistency
 *   - Clinician team: institutional agenda-setter that administers relational framework and retains medical gatekeeping authority
 *   - Bioethics committee: observer/arbiter seat that legitimizes and reviews procedural compliance
 *   - Patients lacking capacity: structurally dependent on substituted judgment within the relational frame; bear extraction cost
 *   - Marginalized patients (weak relational ties): trapped by procedural safeguards that paradoxically become gatekeeping mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.38).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.42).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Autonomy in End-of-Life Decision-Making").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, 'acdfef81-83a3-4bea-97b5-194bee79e390').
narrative_ontology:cs_kernel_codification('acdfef81-83a3-4bea-97b5-194bee79e390', distributed).
narrative_ontology:cs_authority_grounding('acdfef81-83a3-4bea-97b5-194bee79e390', lineage).
narrative_ontology:cs_interpretation_layer_present('acdfef81-83a3-4bea-97b5-194bee79e390').
narrative_ontology:cs_reading_relation('acdfef81-83a3-4bea-97b5-194bee79e390', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('acdfef81-83a3-4bea-97b5-194bee79e390', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('acdfef81-83a3-4bea-97b5-194bee79e390', foundational, dignity_emerges_from_relational_context).
narrative_ontology:cs_axiom_status(dignity_emerges_from_relational_context, holdable).
narrative_ontology:cs_axiom_grounding('acdfef81-83a3-4bea-97b5-194bee79e390', dignity_emerges_from_relational_context, deontological).
narrative_ontology:cs_axiom('acdfef81-83a3-4bea-97b5-194bee79e390', foundational, decision_authority_distributed_not_concentrated).
narrative_ontology:cs_axiom_status(decision_authority_distributed_not_concentrated, holdable).
narrative_ontology:cs_axiom_grounding('acdfef81-83a3-4bea-97b5-194bee79e390', decision_authority_distributed_not_concentrated, conventional).
narrative_ontology:cs_reference_frame('acdfef81-83a3-4bea-97b5-194bee79e390', relational_autonomy_procedural_framework).
narrative_ontology:cs_drift_state('acdfef81-83a3-4bea-97b5-194bee79e390', contemporary_bioethics_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('acdfef81-83a3-4bea-97b5-194bee79e390', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, relational_network).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, medical_system_coordination).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, excluded_decision_voices).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patients_denied_autonomy).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, families_excluded_consultation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, patient).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, family_members).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patients_lacking_capacity).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, marginalized_patients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A person facing death with decision-making capacity intact. Under this reading, the patient retains formal decision authority but must exercise it within a relational triad framework, with their choice subject to procedural review by clinicians and family consultations. Their agency is not absolute but embedded in consultation requirements and safeguard protocols. They cannot unilaterally opt out of the relational process.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patient, agenda_setter,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, patient, beneficiary).

% Hold a formal seat in the decision process, with defined consultation rights and veto capacity in cases where the patient lacks capacity or the team identifies concerning inconsistencies with previously expressed values. They participate in procedural safeguards that frame the patient's autonomy as relational rather than isolated. The constraint requires their engagement; they cannot simply defer to the patient's stated preference.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, family_members, beneficiary,
    moderate, biographical, constrained, local).

% Administers and enforces the relational decision framework. Conducts capacity assessments, facilitates triad consultations, documents procedural compliance, and retains the authority to refuse intervention on grounds of medical conscience or clinical judgment about patient/family coherence. They hold veto power cloaked as professional gatekeeping. The constraint's enforcement machinery is embedded in their clinical workflows.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, clinician_team, agenda_setter,
    institutional, biographical, mobile, regional).

% Reviews cases when relational consensus breaks down or clinician refusal is contested. Provides procedural legitimacy and conflict-resolution capacity. Sits outside the immediate triad but judges whether the triad operated properly. Their role normalizes and standardizes relational decision-making as the authoritative framework.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, bioethics_committee, observer,
    institutional, generational, analytical, regional).

% Would argue that certain forms of life-ending violate transcendent moral principles and should never be authorized, regardless of relational consensus. They are structurally excluded from the formal triad except where they hold family membership. Their objections are treated as inputs to family consultation rather than as constraints on permissible outcomes.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, religious_community_representatives, excluded,
    organized, generational, constrained, regional).

% Cannot voice their own preferences and depend entirely on the relational substituted-judgment framework. The constraint extracts from them by embedding their life-ending decision in a process they cannot directly control, even though that process is justified as protecting their dignity. They bear the full cost of the relational machinery.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patients_lacking_capacity, payer,
    powerless, immediate, identity_locked, local).

% Those with weak family ties, minimal social networks, or cultural/language barriers face heightened procedural burden and gatekeeping risk. The relational requirement becomes an additional suppressive mechanism: a patient with strong relational ties has exit and voice; a patient without them is trapped by the same safeguard regime that is supposed to protect them.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, marginalized_patients, payer,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__relational_autonomy, clinician_team).
narrative_ontology:fixing_cost_class(dignified_death__relational_autonomy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of who holds binding authority over end-of-life decisions: distributes authority across patient-family-clinician rather than concentrating it in any single seat, and establishes procedural safeguards (capacity assessment, conflict resolution, ethics review) to ensure the decision reflects relational deliberation rather than isolated preference or clinical paternalism.
% TRANSFER_FUNCTION: Moves decision-making power from the patient (pure autonomy reading) or from the medical system (pure sanctity reading) into a relational framework where authority is bundled with obligation to consult, justify, and submit to procedural review. Transfers from individual to network; from isolated choice to embedded deliberation.
% ABSENT_VOICES: Those excluded from relational membership (isolated patients, those without family capacity to advocate) cannot challenge the framework that disadvantages them. Religious and philosophical traditions that reject the relational/autonomy premise entirely are present only through individual family members, not as formal stakeholders in the decision process.
% DISAPPEARANCE_RATIONALE: If this relational constraint vanished, decision authority would collapse toward either pure patient autonomy (autonomy_primary reading) or pure medical/institutional gatekeeping (sanctity_primary reading). The entire field of bioethics would reorganize around that binary rather than the current triadic distribution. Institutional protocols, ethics committees, family law, and clinician training would fundamentally shift.
% FOUNDING_PROBLEM: Early medical autonomy frameworks treated end-of-life decisions as purely individual choice, stripping them from family knowledge and clinical expertise. Pure autonomy created the problem of isolated, potentially uninformed, or cognitively compromised decisions. Pure medical paternalism created the problem of clinician-imposed outcomes divorced from patient and family values. Relational autonomy was constructed to bridge both problems.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists and clinicians advocate for the relational framing, citing evidence that triadic consultation reduces decisional regret and improves coherence with patient values. Disability-rights advocates and some patients contest the framing, arguing that 'relational safeguards' become a mechanism to override patient autonomy when family or clinician disagree. No external third party (independent researchers, patient advocacy organizations outside the medical establishment) has conducted neutral empirical validation of the founding problem.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__relational_autonomy_tests).
:- end_tests(dignified_death__relational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.32) because the relational constraint does coordinate a real deliberative problem — it genuinely distributes authority across three seats and requires genuine consultation. It is not pure extraction. But extractiveness drifts upward through the interval (peaking at 0.41 at t=15) as clinical gatekeeping hardens and ethics review becomes more standardized and less reflexive about its own procedural assumptions. The theater ratio is low-to-moderate (0.28 terminal), indicating that the procedural machinery does functional work (ethics review is not purely ceremonial) but a growing share reflects normalization of relational embedding rather than substantive deliberation. Suppression (0.42 terminal) is moderate because the constraint suppresses three categories of challenge: pure autonomy advocates (excluded from formal process), sanctity advocates (present only through family dissent), and patients without relational capacity to work the system. The measurement trajectory shows slight inflation and then stabilization, suggesting the constraint reached a procedural equilibrium after initial expansion of ethics committee review.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter clinician seat and the patient/family seats experience this constraint very differently. From the clinician's view, the relational framework is genuine coordination — it distributes responsibility and provides procedural legitimacy for difficult decisions. From the patient/family seats, especially those without strong relational ties or those whose views diverge from clinician consensus, the framework operates as constrained gatekeeping: required consultation that masks institutional power. From the pure-autonomy or pure-sanctity philosophical seats, the entire relational frame is a compromise that satisfies no coherent principle. The engine computes per-seat directionality from the structural data: clinician seats derive low d (beneficiary of legitimacy coordination); patients with strong family networks derive moderate d (they benefit from consultation); patients without family capacity derive high d (trapped by procedural requirements they cannot influence).
 *
 * DIRECTIONALITY LOGIC:
 *   Patient (with strong relational support): d ≈ 0.5 (symmetric cost-benefit; genuine deliberation but also procedural constraint). Family members: d ≈ 0.3 (beneficiary of formal voice and veto seat). Clinician team: d ≈ 0.15 (beneficiary of institutional legitimacy and retained gatekeeping power). Patients lacking capacity: d ≈ 0.85 (full target; depend entirely on others' substituted judgment and bear burden of procedural review). Marginalized patients: d ≈ 0.90 (trapped by the same safeguard designed to protect them; weaker relational network means higher procedural burden). The beneficiary set is the 'relational network' as an abstract entity — the distribution of authority to the triad rather than concentration in any single seat — plus the medical system's coordination function (clinical consensus-making, ethics committee legitimacy). The victim set is those excluded from meaningful participation: those without family voice, those whose autonomy is overridden by family/clinician consensus, and philosophical/religious advocates positioned outside the formal framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the mandatrophy trap that pure autonomy or pure sanctity framings fall into. Pure autonomy can calcify into a mandate divorced from the relational realities of decision-making (isolated choices without deliberation, no integration of family knowledge or clinical expertise). Pure sanctity can calcify into a mandate that persists after its founding function (protecting vulnerable people) is accomplished, becoming instead an institutional veto on life-ending regardless of consent. Relational autonomy, by design, is reflexive about its own embedding: it acknowledges that authority is always distributed and that procedural coordination is legitimate. However, the constraint carries its own mandatrophy risk: the procedural machinery can become so standardized that it functions theatrically (ethics committees reviewing predetermined decisions, relational consultation as box-ticking) while retaining suppressive force against those outside the triad. The theater ratio climb (0.18 → 0.30 → 0.28) suggests this risk was partially realized, then stabilized. The constraint remains functional but with growing ceremonial overlay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_legitimacy_vs_gatekeeping,
    'Does the procedural relational framework genuinely distribute decision authority, or does it function as institutional gatekeeping disguised as shared deliberation?',
    'Empirical study: compare cases where patient, family, and clinician reach consensus versus cases where they diverge. Track whose preference prevails and the decision-pathway (clinician conviction → family agreement → patient informed, versus patient preference → ethics review → institutional refusal). If consensus-building is genuine, divergence should be rare and documented as substantive deliberation. If gatekeeping is primary, divergence should follow institutional hierarchy regardless of relational framing.',
    'If genuine deliberation: the constraint is a true rope (coordination with procedural overhead). If gatekeeping: reclassify toward tangled_rope or snare (coordination framing masks institutional extraction of authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_legitimacy_vs_gatekeeping, empirical, 'Whether relational decision authority is genuine or procedural cover for institutional control.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the constraint''s suppression of non-relational frameworks structural (exclusionary rules keep those voices out) or internalized (patients and families internalize the relational norm as legitimate even when it constrains their own preferences)?',
    'Longitudinal qualitative study: interview patients and families pre-decision (before exposure to relational framework) and post-decision (after procedural engagement) about their understanding of legitimate decision authority. Track shifts in perceived legitimacy. Also measure: do patients report feeling protected by the relational framework, or constrained? Does family consultation increase or decrease decisional regret?',
    'If structural: the suppression is external gatekeeping and the constraint is more extractive (higher effective χ). If internalized: the suppression is normalized and the constraint''s effective extraction may be lower because targets accept the framing. The distinction affects whether exit options are truly constrained or only perceived as such.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of autonomy-primary and sanctity-primary framings is structural or internalized.').

omega_variable(
    relational_capacity_as_hidden_gate,
    'Does the constraint''s dependence on ''strong relational ties'' function as a hidden capacity gate that excludes marginalized patients who lack family/community support?',
    'Comparative outcomes study: track end-of-life decision processes across patients stratified by relational support (strong family network, weak network, isolated). Measure procedural burden, time to decision, clinician refusal rate, ethics review rate, and patient/family satisfaction by stratum. If relational-capacity becomes a de facto requirement for dignified death authority, the constraint systematically extracts from the marginalized patient cohort.',
    'If relational capacity is a hidden gate: the constraint is not truly rope (coordination available to all) but tangled_rope (coordination for the relationally embedded, extraction from the isolated). Reclassify per-stakeholder: relational-support patients compute as rope-beneficiary; isolated patients compute as snare-target.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(relational_capacity_as_hidden_gate, empirical, 'Whether relational-autonomy framework inadvertently excludes or extracts from patients with weak relational networks.').

omega_variable(
    reading_foreclosure_autonomy_vs_relational,
    'Does the relational_autonomy reading''s core premise (dignity emerges from relational context, not individual choice) logically foreclose the autonomy_primary reading (dignity resides in self-determination), or do both remain coherent in different frameworks?',
    'Philosophical analysis: a relational autonomy theorist can argue that autonomy itself is relational — the capacity for self-determination is constituted through relationships — making the autonomy_primary reading a mis-specified account of autonomy rather than a competing account. Under that analysis, autonomy_primary is foreclosed. Alternatively, autonomy_primary could hold that dignity resides in a core individual capacity that precedes or is independent of relational constitution — a non-relational autonomy. Under that analysis, the readings are genuinely divergent and coexist.',
    'If foreclosed: the reading_relations entry for autonomy_primary should be ''forecloses'' rather than ''coexists_with''. If coexistent: multiple readings remain live (current classification). This affects how the engine models the kernel''s internal coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_autonomy_vs_relational, conceptual, 'Whether relational autonomy logically forecloses pure autonomy or both remain live frameworks.').

omega_variable(
    sanctity_reading_coexistence_test,
    'Does the sanctity_primary reading (dignity resides in life''s intrinsic value; intentional life-termination violates transcendent moral law) remain genuinely live as an alternative, or has the relational reading structurally displaced it within the institutional framework?',
    'Institutional audit: track the presence/absence of sanctity-primary advocates in bioethics committees, clinical protocols, and formal policy across time. Measure whether sanctity objections are treated as substantive deliberation inputs or as sectarian dissent to be managed procedurally. If sanctity is formally excluded or relegated to ''conscience clause'' exceptions, the relational reading has effectively foreclosed it institutionally even if it coexists philosophically.',
    'If sanctity has been institutionally displaced: the reading relation may be ''influences'' (relational reading creates structural conditions that marginalize sanctity) rather than pure ''coexists_with''. This affects how the engine models institutional capture of the kernel debate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctity_reading_coexistence_test, empirical, 'Whether the relational reading has institutionally displaced the sanctity reading despite philosophical coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(dign_tr_t0, observed).
narrative_ontology:measurement(dign_tr_t5, dignified_death__relational_autonomy, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(dign_tr_t5, observed).
narrative_ontology:measurement(dign_tr_t10, dignified_death__relational_autonomy, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(dign_tr_t10, observed).
narrative_ontology:measurement(dign_tr_t15, dignified_death__relational_autonomy, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(dign_tr_t15, observed).
narrative_ontology:measurement(dign_tr_t20, dignified_death__relational_autonomy, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(dign_tr_t20, observed).
narrative_ontology:measurement(dign_tr_t25, dignified_death__relational_autonomy, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(dign_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(dign_be_t0, observed).
narrative_ontology:measurement(dign_be_t5, dignified_death__relational_autonomy, base_extractiveness, 5, 0.35).
narrative_ontology:measurement_basis(dign_be_t5, observed).
narrative_ontology:measurement(dign_be_t10, dignified_death__relational_autonomy, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(dign_be_t10, observed).
narrative_ontology:measurement(dign_be_t15, dignified_death__relational_autonomy, base_extractiveness, 15, 0.41).
narrative_ontology:measurement_basis(dign_be_t15, observed).
narrative_ontology:measurement(dign_be_t20, dignified_death__relational_autonomy, base_extractiveness, 20, 0.39).
narrative_ontology:measurement_basis(dign_be_t20, observed).
narrative_ontology:measurement(dign_be_t25, dignified_death__relational_autonomy, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(dign_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(dign_su_t0, observed).
narrative_ontology:measurement(dign_su_t5, dignified_death__relational_autonomy, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(dign_su_t5, observed).
narrative_ontology:measurement(dign_su_t10, dignified_death__relational_autonomy, suppression_requirement, 10, 0.41).
narrative_ontology:measurement_basis(dign_su_t10, observed).
narrative_ontology:measurement(dign_su_t15, dignified_death__relational_autonomy, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(dign_su_t15, observed).
narrative_ontology:measurement(dign_su_t20, dignified_death__relational_autonomy, suppression_requirement, 20, 0.43).
narrative_ontology:measurement_basis(dign_su_t20, observed).
narrative_ontology:measurement(dign_su_t25, dignified_death__relational_autonomy, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(dign_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, attachment_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__relational_autonomy, 0.12).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% The dignified_death kernel has three structurally distinct readings, each with different ε values, beneficiary/victim structures, and classifications. This story (relational_autonomy) instantiates the middle reading — neither pure autonomy (ε low, individual beneficiary) nor pure sanctity (ε high, institutional veto). The relational reading derives moderate ε from its hybrid character: genuine coordination function but also procedural gatekeeping. All three stories are linked via network.affects_constraints to represent the kernel's internal competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignified_death__relational_autonomy, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
