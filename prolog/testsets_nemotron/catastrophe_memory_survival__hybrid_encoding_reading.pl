% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Hybrid Encoding of Catastrophe Memory in Ritual Practice
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_encoding_reading of the
 *   catastrophe_memory_survival kernel. The kernel is the claim that ritual
 *   transmits catastrophe memory across generations. Three readings contend:
 *   (1) symbol_survival_reading — ritual preserves identity/boundary-norms
 *   through symbolic experience; (2) competence_transmission_reading — ritual
 *   encodes practical survival knowledge; (3) hybrid_encoding_reading (this
 *   story) — ritual operates on BOTH registers simultaneously, and survival
 *   depends on their fusion. The hybrid reading asserts that the symbolic and
 *   the practical are not merely co-present but structurally inseparable in
 *   the ritual's operation — the boundary-maintenance FUNCTION IS the
 *   practical transmission mechanism, and vice versa. Analysts who force a
 *   binary classification are the victims; communities maintaining the fused
 *   practice are the beneficiaries. Extraction is low because the arrangement
 *   serves the community's continuity; suppression is low but nonzero because
 *   external forces (colonization, missionization, state policy, academic
 *   categorization) have historically pressured communities to abandon or
 *   bifurcate the practice.
 *
 * KEY AGENTS:
 *   - survivor_communities: Primary beneficiary (moderate/constrained) — maintains fused practice
 *   - ritual_practitioners: Beneficiary/agenda_setter (organized/identity_locked) — embodies both registers
 *   - intergenerational_transmitters: Beneficiary (moderate/constrained) — teaches fused inheritance
 *   - binary_classifying_analysts: Victim (analytical/analytical) — bears cost of theoretical incoherence
 *   - theoretically_purist_scholars: Victim (analytical/analytical) — bears epistemic cost of framework threat
 *   - comparative_ritual_scholars: Observer (analytical/analytical) — maps cross-cultural pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.12).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.25).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Hybrid Encoding of Catastrophe Memory in Ritual Practice").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '0fb01a3b-5f09-4215-b141-407aa535d439').
narrative_ontology:cs_kernel_codification('0fb01a3b-5f09-4215-b141-407aa535d439', distributed).
narrative_ontology:cs_authority_grounding('0fb01a3b-5f09-4215-b141-407aa535d439', practice).
narrative_ontology:cs_interpretation_layer_present('0fb01a3b-5f09-4215-b141-407aa535d439').
narrative_ontology:cs_reading_relation('0fb01a3b-5f09-4215-b141-407aa535d439', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fb01a3b-5f09-4215-b141-407aa535d439', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('0fb01a3b-5f09-4215-b141-407aa535d439', foundational, ritual_registers_inseparable_in_survival).
narrative_ontology:cs_axiom_status(ritual_registers_inseparable_in_survival, holdable).
narrative_ontology:cs_axiom_grounding('0fb01a3b-5f09-4215-b141-407aa535d439', ritual_registers_inseparable_in_survival, empirically_contingent).
narrative_ontology:cs_axiom('0fb01a3b-5f09-4215-b141-407aa535d439', secondary, binary_classification_extracts_from_communities).
narrative_ontology:cs_axiom_status(binary_classification_extracts_from_communities, holdable).
narrative_ontology:cs_axiom_grounding('0fb01a3b-5f09-4215-b141-407aa535d439', binary_classification_extracts_from_communities, deontological).
narrative_ontology:cs_reference_frame('0fb01a3b-5f09-4215-b141-407aa535d439', fused_ritual_practice_as_survival_architecture).
narrative_ontology:cs_drift_state('0fb01a3b-5f09-4215-b141-407aa535d439', contemporary_academic_categorization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fb01a3b-5f09-4215-b141-407aa535d439', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, survivor_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practitioners).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, intergenerational_transmitters).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, binary_classifying_analysts).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, theoretically_purist_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that have endured catastrophe and maintain ritual practices encoding both symbolic boundary-maintenance (identity, mourning, collective meaning) and embedded practical knowledge (resource locations, seasonal timing, family protocols, adaptation strategies). They do not separate these registers analytically; the ritual's survival value depends on their fusion. Exit from the ritual means loss of both identity continuity and practical transmission simultaneously.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, survivor_communities, beneficiary,
    moderate, generational, constrained, regional).

% Elders, officiants, and knowledge-holders who enact and transmit the ritual. Their authority derives from embodying both registers — they are the living archive of symbolic meaning AND practical competence. Professional identity is fused with the practice; leaving the role means abandoning a self-concept constituted through the ritual. They set the agenda for how the ritual is performed but are also constrained by the community's expectation of fidelity to both registers.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practitioners, beneficiary,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practitioners, agenda_setter).

% Parents, grandparents, and community educators who teach the ritual to younger generations. They transmit the fused practice without theoretical decomposition — children learn the movements, words, and timings as a single integrated inheritance. Their stake is the survival of the whole; analytical separation would undermine the transmission mechanism they steward.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, intergenerational_transmitters, beneficiary,
    moderate, generational, constrained, regional).

% Scholars and theorists (anthropologists, religious studies scholars, cognitive scientists) who impose a binary classification on ritual: either it is 'symbolic/expressive' OR 'functional/instrumental'. They are victims of their own analytical framework — the constraint of the hybrid reading extracts from them the cost of theoretical incoherence when they encounter practices that refuse the binary. Their professional incentives reward clean categorization; the hybrid reality forces either misclassification or theoretical innovation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, binary_classifying_analysts, payer,
    analytical, biographical, analytical, global).

% Researchers committed to either the symbol_survival_reading or competence_transmission_reading as exhaustive accounts. They experience the hybrid reading as a threat to their theoretical coherence — it renders their purist frameworks incomplete. The extraction they bear is the epistemic cost of either expanding their framework or dismissing countervailing evidence. Exit means abandoning a career-defining theoretical commitment.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, theoretically_purist_scholars, payer,
    analytical, biographical, analytical, global).

% Scholars who study ritual across traditions without committing to a single reading. They observe the hybrid pattern recurring — Jewish Passover, Indigenous potlatch, Mesoamerican calendar rituals, Japanese ancestral rites — and treat the dual-register thesis as a cross-cultural hypothesis. They neither benefit nor pay; they map the structural terrain.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of transmitting both collective identity AND practical survival knowledge across generations without requiring the community to maintain separate institutions for each. The ritual is a single integrated vehicle for 'who we are' and 'how we survive' — the fusion is the coordination mechanism.
% TRANSFER_FUNCTION: Moves embodied practice, narrative memory, and practical protocols from elders to youth, from past to future, in a single ceremonial act. The transfer is not extractive — no party accumulates surplus; the arrangement sustains the community's continuity.
% ABSENT_VOICES: Communities whose rituals were disrupted by colonization, missionization, or state suppression — they would object to any reading that treats their practices as either 'mere symbolism' or 'mere utility'. Their exclusion from the scholarly conversation is structural; the academy's categories were built on the erasure of their epistemic authority.
% DISAPPEARANCE_RATIONALE: If the hybrid encoding constraint vanished — if communities were forced to separate symbolic ritual from practical knowledge transmission — survivor communities would lose the integrated mechanism that has sustained them through catastrophe. Identity rituals would become hollow performance; practical knowledge would become brittle technique without meaning. The community's survival architecture would fracture.
% FOUNDING_PROBLEM: How does a community transmit both its identity-as-survivors and the concrete knowledge that enabled survival, across generations, without writing, without centralized institutions, and under conditions of recurring threat?
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic records from multiple traditions (Jewish, Indigenous Americas, West African, Tibetan, Japanese) document rituals that simultaneously encode historical trauma, collective identity, and practical protocols for resource management, seasonal timing, and social coordination. The corroboration comes from the communities themselves — their continued practice is the attestation — and from comparative scholars who observe the pattern independently of any single theoretical school.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).
:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12) is low because the ritual's operation primarily serves the community's survival — no party extracts surplus from another. The slight rise over the interval (0.05→0.12) reflects increasing external pressure (academic categorization, heritage tourism, institutional co-optation) that treats the ritual as an object of study or display rather than a living practice. Theater ratio (0.18) is low but rising — some performances for external audiences (museums, festivals, media) are performative in the extractive sense, but the core practice remains functional. Suppression (0.25) reflects historical and ongoing pressure to separate symbol from competence: missionaries demanding 'religion' without 'superstition', states demanding 'culture' without 'politics', scholars demanding 'meaning' without 'function'. Accessibility collapse (0.35) is moderate — alternatives (separate institutions for identity and survival knowledge) exist but are fragile and historically unsuccessful for stateless/minoritized communities. Resistance (0.4) is moderate — communities resist both external suppression and internal theoretical decomposition.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (survivor_communities, ritual_practitioners, intergenerational_transmitters) experience the constraint as a mountain-like necessity — the fused practice IS their survival architecture. The victim seats (binary_classifying_analysts, theoretically_purist_scholars) experience it as a snare-like theoretical trap — their analytical tools cannot grasp the fusion without breaking. The observer seat (comparative_ritual_scholars) sees the structural pattern across traditions. The engine will compute per-seat types from this asymmetry: beneficiaries near mountain/rope, victims near snare/tangled_rope (theoretical extraction), observer near mountain (pattern recognition).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the communities and practitioners who live the fused practice — they receive the coordination benefit (survival continuity) with minimal cost. The ritual's persistence is not enforced upon them; they maintain it because it works. Victims are analysts whose theoretical frameworks require binary classification — the hybrid reality extracts epistemic cost from them (incoherence, framework threat, career investment risk). Their exit is analytical (they could change frameworks) but identity-locked for purist scholars (career-defining commitment). Directionality for beneficiaries is near 0.0 (full beneficiary); for analyst victims, d is elevated by identity_locked exit and analytical power that makes framework change costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmitting identity + practical knowledge under threat) remains live — climate catastrophe, displacement, and cultural erasure make it newly urgent. The arrangement has not atrophied; its function has expanded. No mandatrophy: the constraint is not a vestigial form maintained by inertia. The rising theater_ratio and suppression_requirement reflect external pressures, not internal decay. The hybrid reading prevents mislabeling: a purist symbol reading would miss the practical coordination function (false mountain); a purist competence reading would miss the identity-binding function (false rope). The fusion is the real coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (hybrid_encoding_reading) of the contested kernel catastrophe_memory_survival. What structural elements would change if a sibling reading (symbol_survival_reading or competence_transmission_reading) were instantiated instead?',
    'Compare the three readings'' beneficiary/victim sets, extractiveness profiles, and coordination functions. The hybrid reading has low ε and victims = analysts forcing binary classification. The symbol_survival_reading would have victims = communities whose practical knowledge is erased by symbolic reduction. The competence_transmission_reading would have victims = communities whose identity continuity is erased by instrumental reduction.',
    'If the kernel is real (catastrophe memory survives through ritual), the three readings are not observationally equivalent — they make different predictions about which communities survive, which practices persist, and where extraction occurs. The reading choice changes the constraint''s ε, victim set, and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer frame: this reading''s structural distinctness from sibling readings of the same kernel').

omega_variable(
    symbol_competence_inseparability,
    'Are the symbolic and practical registers genuinely inseparable in the ritual''s operation, or is their fusion an analytical artifact of the hybrid reading?',
    'Ethnographic test: when communities are forced (by suppression, migration, institutional capture) to separate the registers — e.g., performing ''cultural heritage'' rituals stripped of practical protocols, or transmitting practical skills without the narrative frame — does survival continuity degrade faster than when the fusion is maintained? Longitudinal comparison of communities with fused vs. bifurcated practice.',
    'If inseparable, the hybrid reading''s low ε is structurally accurate — the fusion IS the coordination mechanism. If separable, the hybrid reading overstates integration and the purist readings capture real but partial truths. Classification shifts: inseparable → rope (genuine coordination); separable → tangled_rope (coordination + extraction of one register by the other).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbol_competence_inseparability, empirical, 'Whether the dual-register fusion is structurally necessary or analytically imposed').

omega_variable(
    analyst_victim_status,
    'Are binary_classifying_analysts and theoretically_purist_scholars genuine victims of extraction, or does labeling them as victims reify the academic contest as a structural constraint?',
    'Trace whether the analytical binary is imposed on communities (extraction from communities) or whether communities impose hybridity on analysts (extraction from analysts). If communities are forced to choose symbolic OR practical framing for funding/recognition/legitimacy, the extraction flows from analysts to communities — analysts are agenda_setters, not victims. If analysts merely suffer theoretical discomfort, victim status is metaphorical.',
    'If analysts extract from communities (via grant requirements, heritage designations, publication gatekeeping), the victim/beneficiary assignment flips: communities become victims of analytical extraction, analysts become beneficiaries/agenda_setters. The constraint reclassifies from rope to snare or tangled_rope. If analysts only bear epistemic cost, the current assignment stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analyst_victim_status, conceptual, 'Direction of extraction between communities and analysts').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.25) structural (external forces: colonization, missionization, state policy) or internalized (communities themselves bifurcating the practice under pressure)?',
    'Post-pressure suppression trajectory: if communities that regain autonomy spontaneously restore the fused practice, suppression was primarily structural. If the bifurcation persists after external pressure lifts, internalization has occurred — the community has absorbed the binary classification.',
    'If internalized, effective suppression is higher than the structural measure suggests — the community carries the suppression internally. This would raise the constraint''s effective extraction for beneficiary seats (they now pay the cost of their own bifurcation) and could shift classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in communities under external pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_tr_t25, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_tr_t50, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_tr_t75, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 75, 0.17).
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_tr_t100, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_be_t25, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 25, 0.08).
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_be_t50, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 50, 0.1).
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_be_t75, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 75, 0.11).
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_be_t100, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 100, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_su_t25, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_su_t50, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_su_t75, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 75, 0.22).
narrative_ontology:measurement(catastrophe_memory_survival__hybrid_encoding_reading_su_t100, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 100, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__hybrid_encoding_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_survival kernel decomposes into three readings with distinct ε profiles: symbol_survival_reading (ε~0.05, victims=communities losing practical knowledge), competence_transmission_reading (ε~0.08, victims=communities losing identity continuity), hybrid_encoding_reading (ε~0.12, victims=analysts forcing binary). All three share the referent (catastrophe memory transmission via ritual) but differ in what they treat as the coordination function and where extraction falls. The hybrid reading's ε is higher because it incorporates the extraction cost of theoretical incoherence for analysts — a cost the purist readings externalize.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__hybrid_encoding_reading, analytical, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
