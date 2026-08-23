% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Mourning Practice and Boundary-Norms Commemorative Reading
 *   domain: religious studies / ritual theory / collective memory
 *
 * SUMMARY:
 *   This constraint story models Tisha B'Av and comparable
 *   catastrophe-commemoration rituals as a commemorative (D1/D4) reading:
 *   ritual functions purely to preserve mourning-practice and boundary-norms,
 *   maintaining group identity through memorial obligation with no
 *   survival-competence transmission (D5). The kernel is contested among
 *   three readingsâpure commemorative, pure survival-competence, and
 *   hybridâbut this JSON instantiates only the commemorative reading as a
 *   clean, Îµ-invariant constraint per DP-001. The constraint extracts
 *   compliance and emotional labor from obligated members under active social
 *   enforcement, while coordinating the temporal synchronization of
 *   collective memory across generations.
 *
 * KEY AGENTS:
 *   - communal_authority: agenda_setter/beneficiary (institutional/identity_locked/global) â custodians who set mourning norms and derive legitimacy from ritual continuity
 *   - obligated_member: payer/beneficiary (moderate/identity_locked/national) â community members compelled to perform grief who receive belonging but bear compliance costs
 *   - ritual_dissident: excluded (moderate/constrained/national) â rejecters of the obligation excluded from norm-setting discourse
 *   - memory_scholar: observer (analytical/analytical/global) â academic analyst tracking functional divergence across kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Mourning Practice and Boundary-Norms Commemorative Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious studies / ritual theory / collective memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, '5405c802-3979-47c7-8341-193fac2c4db9').
narrative_ontology:cs_kernel_codification('5405c802-3979-47c7-8341-193fac2c4db9', fixed_text).
narrative_ontology:cs_authority_grounding('5405c802-3979-47c7-8341-193fac2c4db9', lineage).
narrative_ontology:cs_interpretation_layer_present('5405c802-3979-47c7-8341-193fac2c4db9').
narrative_ontology:cs_reading_relation('5405c802-3979-47c7-8341-193fac2c4db9', catastrophe_memory_function__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('5405c802-3979-47c7-8341-193fac2c4db9', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('5405c802-3979-47c7-8341-193fac2c4db9', foundational, ritual_as_pure_boundary_maintenance).
narrative_ontology:cs_axiom_status(ritual_as_pure_boundary_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('5405c802-3979-47c7-8341-193fac2c4db9', ritual_as_pure_boundary_maintenance, conventional).
narrative_ontology:cs_axiom('5405c802-3979-47c7-8341-193fac2c4db9', foundational, memorial_obligation_constitutive).
narrative_ontology:cs_axiom_status(memorial_obligation_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('5405c802-3979-47c7-8341-193fac2c4db9', memorial_obligation_constitutive, deontological).
narrative_ontology:cs_reference_frame('5405c802-3979-47c7-8341-193fac2c4db9', classical_rabbinic_communal_order).
narrative_ontology:cs_drift_state('5405c802-3979-47c7-8341-193fac2c4db9', modern_secular_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5405c802-3979-47c7-8341-193fac2c4db9', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, communal_authority).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, obligated_member).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, obligated_member).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, durkheimian_boundary_function).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, collective_memory_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodians of the liturgical calendar and halakhic norms who set the parameters of mandated mourning (e.g., Tisha B'Av fast, lamentation liturgy, prohibited activities). They derive legitimacy and deference from preserving the catastrophe memory and policing its boundaries. Their institutional identity is fused with the ritual's continuity; exit would mean surrendering their authoritative role.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, communal_authority, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, communal_authority, beneficiary).

% Community members compelled to perform annual mourning regardless of personal emotional distance from the historical catastrophe. They bear the costs of complianceâfasting, restricted conduct, public grief performance, and suppression of personal mourning rhythms. They receive group belonging and identity continuity in return, but cannot opt out without social penalty or self-concept rupture.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, obligated_member, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, obligated_member, beneficiary).

% Individuals who reject the memorial obligation or question its contemporary form. They are structurally excluded from communal discourse on ritual meaning; their non-participation marks them as boundary-violators, and they face social exclusion without a voice in setting the norms.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, ritual_dissident, excluded,
    moderate, biographical, constrained, national).

% Academic analysts of religion and ritual who study the constraint's function in group persistence without participating in its enforcement or obligation. They observe the divergence between commemorative, survival-competence, and hybrid readings.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, memory_scholar, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__mourning_practice_reading, communal_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__mourning_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the temporal synchronization of communal grief across a dispersed population, ensuring that collective memory of catastrophe is annually reactivated and transmitted across generations.
% TRANSFER_FUNCTION: Moves emotional labor, compliance time, and deference from obligated members to communal authorities and to the abstract maintenance of group boundaries.
% ABSENT_VOICES: Ritual dissidents who reject the obligation, secular historians who would deprioritize theological framing, and members for whom the catastrophe is genealogically distant but are compelled to perform grief.
% DISAPPEARANCE_RATIONALE: Without the memorial obligation, the annual re-synchronization of collective memory would cease; communal authority would lose a major site of legitimation; group boundaries would weaken as the compulsory distinction between mourners and non-mourners dissolved.
% FOUNDING_PROBLEM: The destruction of the Temple and subsequent exiles threatened collective identity dissolution and assimilation; ritual mourning was instituted to preserve group cohesion and prevent disappearance.
% FOUNDING_PROBLEM_CORROBORATION: Communal authorities and traditional texts attest the problem is live, citing ongoing assimilation and intermarriage. Secular historians and sociologists outside the benefiting authority structure attest the founding conditions have transformed and the ritual now persists partially by inertia; some corroborate the identity-threat, others dispute its current severity.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderate-high because the ritual extracts emotional labor and compliance from members whose personal grief may not match the communal schedule, amplified by identity-locked exit. Suppression (0.6) reflects active social enforcement: exclusion of dissidents, stigma against non-observance, and institutional boundary-policing. Theater_ratio rises from 0.15 to 0.45 over the interval because modernization weakens organic grief connection, increasing the share of performative compliance relative to spontaneous mourning. Accessibility_collapse (0.55) captures that alternatives (secular commemoration, individualized grief) exist but carry heavy social costs. Resistance (0.3) is moderate: modernist and secular challenges exist but are fragmented.
 *
 * PERSPECTIVAL GAP:
 *   The communal_authority seat experiences the constraint as sacred custodianship and necessary identity preservation; the obligated_member seat experiences it as a compulsory emotional regimen that may mismatch personal experience. The engine computes this divergence from the structural asymmetry in exit options (authority is identity_locked to its role; members are identity_locked to communal belonging) and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal_authority sits near the beneficiary end (low d): they collect deference, legitimacy, and institutional continuity from the constraint. Obligated_members sit near the target end (high d): they bear the compliance costs, identity-lock amplifying their effective extraction. Ritual_dissidents would sit even higher if fully in the system, but their exclusion redirects the extraction mechanism back toward compliant members. Memory_scholar occupies the analytical seat with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure coordination (rope) because the active enforcement, identity-locked exit, and compulsory performance create identifiable extraction. It also prevents mislabeling as pure extraction (snare) because the coordination functionâgenuine group identity maintenance and cross-generational memory synchronizationâis structurally real and acknowledged even by resistant members.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (social exclusion of dissidents) or internalized (members believe they owe the grief regardless of social monitoring)?',
    'Post-exit trajectory observation: if obligated members who leave the community continue to feel compelled to mourn, the suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measureâmembers carry the constraint after exit, raising extractiveness for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in communal mourning obligation').

omega_variable(
    coordination_extraction_separability,
    'Can the group-identity coordination function be separated from the compliance extraction, or does the boundary-maintenance necessarily require the obligation?',
    'Comparative analysis of communities with voluntary commemoration versus obligatory mourning: if identity persists without enforcement, the functions are separable.',
    'If separable, the obligation is extractive overhead on genuine coordination; if inseparable, the measured extraction is the necessary price of boundary maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable in ritual obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_mourning_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(catastrophe_mourning_tr_t20, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(catastrophe_mourning_tr_t40, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(catastrophe_mourning_tr_t60, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(catastrophe_mourning_tr_t80, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(catastrophe_mourning_tr_t100, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(catastrophe_mourning_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(catastrophe_mourning_be_t20, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(catastrophe_mourning_be_t40, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(catastrophe_mourning_be_t60, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(catastrophe_mourning_be_t80, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(catastrophe_mourning_be_t100, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_mourning_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(catastrophe_mourning_su_t20, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(catastrophe_mourning_su_t40, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(catastrophe_mourning_su_t60, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(catastrophe_mourning_su_t80, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(catastrophe_mourning_su_t100, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_function kernel, decomposed per the Îµ-invariance principle because each reading instantiates a structurally distinct functional claim (pure D1/D4, pure D5, or hybrid) with different Îµ profiles and stakeholder asymmetries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
