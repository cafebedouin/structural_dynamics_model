% ============================================================================
% CONSTRAINT STORY: domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_domain_partition_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: domain_partition_reading
 *   human_readable: Domain Partition Reading: Kami-Buddha Jurisdictional Separation
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   The domain partition reading interprets the kami-buddha relationship in
 *   Japanese religious practice as a jurisdictional coordination mechanism:
 *   kami govern this-worldly life-cycle events (birth, marriage, harvest,
 *   purification), while buddhas govern death and the afterlife (funerals,
 *   ancestor veneration, posthumous salvation). This reading emphasizes
 *   functional separation rather than theological integration. The partition
 *   solves a genuine coordination problem: how to allocate ritual labor and
 *   religious authority across the full range of human concerns when two
 *   distinct ritual systems coexist in the same population. The partition is
 *   not a formal doctrine but an enacted practice — households participate in
 *   both shrine and temple rituals without experiencing contradiction because
 *   the rituals address non-overlapping domains. This constraint story models
 *   the partition as a Rope (pure coordination) with low extraction and
 *   suppression, benefiting local ritual autonomy. The partition is one of
 *   three contested readings of the kami-buddha kernel; sibling readings
 *   interpret the relationship as syncretic fusion or pragmatic incoherence.
 *
 * KEY AGENTS:
 *   - Village Households: Primary beneficiaries (moderate/mobile) — the partition provides clear ritual allocation across life-cycle events without theological burden
 *   - Local Shrine Networks: Organized beneficiaries (organized/constrained) — the partition preserves shrine jurisdictional autonomy and protects against Buddhist institutional encroachment
 *   - Buddhist Temple Institutions: Institutional beneficiaries (institutional/constrained) — the partition secures temple death-ritual monopoly and funerary income streams
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the partition as a coordination equilibrium with minimal extractive overhead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(domain_partition_reading, 0.18).
domain_priors:suppression_score(domain_partition_reading, 0.22).
domain_priors:theater_ratio(domain_partition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(domain_partition_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(domain_partition_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(domain_partition_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(domain_partition_reading, rope).
narrative_ontology:human_readable(domain_partition_reading, "Domain Partition Reading: Kami-Buddha Jurisdictional Separation").
narrative_ontology:topic_domain(domain_partition_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(domain_partition_reading, '999f913f-0d1f-4ba2-b763-ad4db39fe960').
narrative_ontology:cs_kernel_codification('999f913f-0d1f-4ba2-b763-ad4db39fe960', distributed).
narrative_ontology:cs_authority_grounding('999f913f-0d1f-4ba2-b763-ad4db39fe960', practice).
narrative_ontology:cs_reading_relation('999f913f-0d1f-4ba2-b763-ad4db39fe960', domain_partition_reading__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('999f913f-0d1f-4ba2-b763-ad4db39fe960', domain_partition_reading__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('999f913f-0d1f-4ba2-b763-ad4db39fe960', foundational, jurisdictional_non_overlap).
narrative_ontology:cs_axiom_status(jurisdictional_non_overlap, holdable).
narrative_ontology:cs_axiom_grounding('999f913f-0d1f-4ba2-b763-ad4db39fe960', jurisdictional_non_overlap, conventional).
narrative_ontology:cs_axiom('999f913f-0d1f-4ba2-b763-ad4db39fe960', secondary, dual_participation_coherence).
narrative_ontology:cs_axiom_status(dual_participation_coherence, holdable).
narrative_ontology:cs_axiom_grounding('999f913f-0d1f-4ba2-b763-ad4db39fe960', dual_participation_coherence, conventional).
narrative_ontology:cs_reference_frame('999f913f-0d1f-4ba2-b763-ad4db39fe960', edo_period_ritual_equilibrium).
narrative_ontology:cs_drift_state('999f913f-0d1f-4ba2-b763-ad4db39fe960', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('999f913f-0d1f-4ba2-b763-ad4db39fe960', '').
narrative_ontology:cs_kernel_id(domain_partition_reading, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(domain_partition_reading, local_ritual_practitioners).
narrative_ontology:constraint_beneficiary(domain_partition_reading, village_shrine_networks).
narrative_ontology:constraint_beneficiary(domain_partition_reading, household_ritual_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(domain_partition_reading, village_households).
narrative_ontology:constraint_beneficiary(domain_partition_reading, local_shrine_networks).
narrative_ontology:constraint_beneficiary(domain_partition_reading, buddhist_temple_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Village households participate in both shrine rituals (for birth, marriage, harvest, purification) and temple rituals (for funerals, ancestor veneration). The partition provides clear ritual allocation across life-cycle events without requiring theological commitment to either system. Households can adjust their ritual participation based on need and preference — exit is low-cost.
narrative_ontology:constraint_stakeholder(domain_partition_reading, village_households, beneficiary,
    moderate, biographical, mobile, local).

% Shrine networks (village shrines, regional shrine associations) benefit from the partition because it preserves their jurisdictional autonomy over this-worldly rituals. The partition protects shrines from Buddhist institutional encroachment and secures their role in community life-cycle events. Exit is constrained — shrines are embedded in regional ritual economies and cannot easily abandon the partition — but the constraint is experienced as beneficial coordination.
narrative_ontology:constraint_stakeholder(domain_partition_reading, local_shrine_networks, beneficiary,
    organized, generational, constrained, regional).

% Buddhist temples benefit from the partition because it secures their monopoly on death rituals and ancestor veneration. Funerary Buddhism is a major revenue stream for temples, and the partition guarantees this income by allocating death-related rituals exclusively to the Buddhist domain. Exit is constrained — temples depend on funerary income and cannot abandon the partition without losing their economic base — but the constraint is experienced as mutually beneficial coordination.
narrative_ontology:constraint_stakeholder(domain_partition_reading, buddhist_temple_institutions, beneficiary,
    institutional, generational, constrained, national).

% The Meiji state formalized the kami-buddha partition through the shinbutsu bunri (kami-buddha separation) policy, which legally separated shrines and temples and suppressed syncretic practices. The state's intervention increased suppression (time_point 6: 0.28) and extraction (time_point 6: 0.18) by imposing legal boundaries on previously fluid ritual practices. The state's role is agenda-setting rather than beneficiary — the state used the partition to consolidate Shinto as a national ideology, not to collect rents from the partition itself.
narrative_ontology:constraint_stakeholder(domain_partition_reading, meiji_state_apparatus, agenda_setter,
    institutional, biographical, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The partition solves the problem of ritual allocation across the full range of human concerns when two distinct ritual systems coexist in the same population. Without the partition, households would face ambiguity about which rituals to perform for which life events, and shrine/temple institutions would face jurisdictional conflict.
% TRANSFER_FUNCTION: The partition moves ritual labor and religious authority from an ambiguous shared domain to clearly separated domains: kami rituals for this-worldly events, Buddhist rituals for death and afterlife. Money flows from households to shrines (for life-cycle rituals) and from households to temples (for funerary rituals), but the flows are reciprocal payments for valued services rather than asymmetric extraction.
% ABSENT_VOICES: Practitioners who experienced the Meiji state's shinbutsu bunri policy as violent suppression of syncretic practices are largely absent from the historical record. The partition reading emphasizes coordination and mutual benefit, but the Meiji formalization involved state coercion and the destruction of syncretic temple-shrine complexes. These voices would contest the 'pure coordination' framing.
% DISAPPEARANCE_RATIONALE: If the partition disappeared, households would face ambiguity about ritual allocation, and shrine/temple institutions would face jurisdictional conflict. The partition is not a natural law — it is a historically contingent coordination mechanism — and its disappearance would require renegotiation of ritual boundaries. The world would rearrange itself, not stay the same.
% FOUNDING_PROBLEM: The founding problem was the coexistence of two distinct ritual systems (indigenous kami worship and imported Buddhism) in the same population without a clear allocation of ritual labor across life-cycle events. The partition emerged as a solution to this coordination problem, allocating kami rituals to this-worldly concerns and Buddhist rituals to death and afterlife.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live in contemporary Japan: households continue to participate in both shrine and temple rituals, and the partition continues to allocate ritual labor across life-cycle events. Ethnographic studies (Reader & Tanabe 1998, Nelson 2000) document ongoing household participation in both systems. The problem is live, not dead.
narrative_ontology:disappearance_verdict(domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(domain_partition_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE HOUSEHOLD (ROPE) — Experiences the partition as pure coordination: kami rituals for birth, marriage, harvest; Buddhist rituals for death and ancestors. No extraction — the partition solves the genuine problem of ritual allocation across life-cycle events. Mobile exit: households can and do adjust their ritual participation without penalty.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: LOCAL SHRINE NETWORK (ROPE) — Organized shrine practitioners see the partition as coordination that preserves their jurisdictional autonomy. Constrained exit: shrine networks are embedded in regional ritual economies and cannot easily abandon the partition, but they experience it as beneficial coordination rather than extraction. The partition protects shrine ritual space from Buddhist institutional encroachment.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUDDHIST TEMPLE INSTITUTION (ROPE) — Institutional Buddhist temples see the partition as coordination that secures their death-ritual monopoly. Constrained exit: temples depend on funerary income and cannot abandon the partition without losing their economic base, but they experience it as mutually beneficial coordination. The partition guarantees temple revenue streams.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the domain partition is a coordination solution to the problem of integrating two distinct ritual systems without theological subsumption. Low extraction: the partition allows both systems to coexist without either dominating the other. The partition is not a natural law (it is historically contingent and varies regionally) but a stable coordination equilibrium with minimal extractive overhead.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(domain_partition_reading_tests).
:- end_tests(domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The partition involves some extraction — Buddhist temples collect funerary fees, shrines collect ritual offerings — but the extraction is modest and reciprocal. Households pay for services they value (life-cycle rituals, ancestor care) rather than bearing asymmetric costs. The slight increase over the interval (0.12 → 0.18) reflects the Meiji state's formalization of the partition and the postwar commercialization of funerary Buddhism, which increased extractive pressure. Suppression (0.22): Low. The partition does not require active enforcement in most contexts — households participate voluntarily because the ritual allocation is functionally useful. The spike at time_point 6 (Meiji period, 0.28) reflects the state's shinbutsu bunri policy, which imposed legal separation and suppressed syncretic practices. Postwar suppression declined (0.22) as state enforcement relaxed. Theater ratio (0.15): Low. The partition is not primarily performative — the rituals genuinely address the concerns they claim to address (birth, death, harvest, ancestors). The slight increase over time reflects the gradual attenuation of ritual efficacy beliefs and the rise of ritual-as-custom rather than ritual-as-necessity, but the partition remains functionally grounded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all four perspectives classify as Rope. The village household, the shrine network, the temple institution, and the analytical observer all experience the partition as coordination with low extraction. The uniformity is diagnostic: when a constraint classifies as Rope from all perspectives, it is likely a genuine coordination mechanism rather than a naturalized extraction arrangement. The absence of a Snare or Tangled Rope perspective suggests that the partition does not create identifiable victims. The main perspectival difference is in exit options (mobile for households, constrained for institutions) and time horizon (biographical for households, generational for institutions), but these differences do not change the classification type.
 *
 * DIRECTIONALITY LOGIC:
 *   All three agent groups (village households, shrine networks, temple institutions) are beneficiaries of the partition. The partition solves a real coordination problem for each: households get clear ritual allocation, shrines get jurisdictional protection, temples get economic security. No agent group is a victim — the partition does not extract from one group to benefit another. The analytical observer sees the same coordination structure. Directionality values are low across all perspectives (d ≈ 0.1–0.2), producing low or negative effective extraction (chi). The partition is a genuine coordination equilibrium, not a disguised extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The domain partition reading resolves the mandatrophy by showing that the kami-buddha relationship can be understood as pure coordination (Rope) rather than extraction (Snare) or hybrid (Tangled Rope). The partition is not a natural law (Mountain) — it is historically contingent and varies regionally — but it is also not extractive. The partition's low extractiveness and suppression distinguish it from alternative readings: if the relationship were syncretic fusion (sibling reading), the constraint might involve theological subsumption and higher extraction; if the relationship were pragmatic incoherence (sibling reading), practitioners might experience cognitive dissonance and higher suppression. The domain partition reading claims that the relationship is best understood as functional separation with mutual benefit, not as fusion or incoherence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the kami-buddha relationship best understood as jurisdictional partition (this reading), syncretic fusion (sibling reading), or pragmatic incoherence (sibling reading)?',
    'Historical analysis of ritual practice variation across regions and time periods; ethnographic data on practitioner self-understanding; textual analysis of doctrinal claims vs. lived practice',
    'If partition: coordination with minimal extraction (Rope). If fusion: single integrated system with different classification. If incoherence: practitioners navigate contradictory commitments, potentially higher extraction or suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which reading of the kami-buddha kernel best captures the structural relationship').

omega_variable(
    regional_variation_threshold,
    'At what degree of regional variation does the ''domain partition'' reading cease to be a single constraint and become multiple distinct local arrangements?',
    'Comparative ethnography across Japanese regions; identification of core vs. peripheral features of the partition; analysis of which elements are invariant and which are locally negotiated',
    'If high invariance: single constraint with regional implementation details. If low invariance: the ''partition'' is a scholarly abstraction over heterogeneous local practices, and this constraint story over-unifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_variation_threshold, empirical, 'Whether regional variation undermines the partition as a unified constraint').

omega_variable(
    meiji_restoration_discontinuity,
    'Did the Meiji state''s shinbutsu bunri (kami-buddha separation) policy create the partition reading, or did it formalize a pre-existing practice?',
    'Historical analysis of pre-Meiji ritual practice; comparison of Edo-period and Meiji-period ritual allocation patterns; assessment of whether the partition is a Tokugawa-era equilibrium or a Meiji-era construction',
    'If pre-existing: the partition is a long-duration coordination equilibrium. If Meiji-created: the partition is a state-imposed arrangement with potentially higher suppression and extraction than this reading acknowledges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_restoration_discontinuity, empirical, 'Whether the partition predates or postdates Meiji state intervention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(domain_partition_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(domain_part_theater_edo_early, domain_partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(domain_part_theater_edo_mid, domain_partition_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(domain_part_theater_meiji, domain_partition_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(domain_part_theater_postwar, domain_partition_reading, theater_ratio, 9, 0.18).

% Extraction over time
narrative_ontology:measurement(domain_part_extract_edo_early, domain_partition_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(domain_part_extract_edo_mid, domain_partition_reading, base_extractiveness, 3, 0.14).
narrative_ontology:measurement(domain_part_extract_meiji, domain_partition_reading, base_extractiveness, 6, 0.18).
narrative_ontology:measurement(domain_part_extract_postwar, domain_partition_reading, base_extractiveness, 9, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(domain_part_suppress_edo_early, domain_partition_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(domain_part_suppress_meiji, domain_partition_reading, suppression_requirement, 6, 0.28).
narrative_ontology:measurement(domain_part_suppress_postwar, domain_partition_reading, suppression_requirement, 9, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(domain_partition_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% The domain partition reading is one of three readings of the kami_buddha_ontology kernel. The other readings (syncretic_fusion_reading, pragmatic_incoherence_reading) are separate constraint stories with different epsilon values and different beneficiary/victim structures. The three readings are linked via network.affects_constraints in their respective JSON files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
