% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Catastrophe Memory Function â Hybrid Transformation Reading
 *   domain: religious/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint is the hybrid_transformation_reading of the
 *   catastrophe_memory_function kernel. It models the Passover seder as a
 *   single ritual structure that simultaneously encodes mourning-practice
 *   (D1/D4: bitter herbs, affliction narrative) and survival-competence (D5:
 *   seder performance as adaptive rehearsal, pedagogical transmission). The
 *   kernel is contested between three readings: mourning_practice_reading
 *   (D1/D4 only), survival_competence_reading (D5 only), and this hybrid
 *   reading (co-original coupling). The ritual operates as a tangled rope: it
 *   genuinely coordinates intergenerational memory and group continuity, but
 *   it also extracts through identity-locked performance obligation,
 *   foreclosure of alternative mourning, and institutional capture of
 *   interpretive authority by ritual custodians.
 *
 * KEY AGENTS:
 *   - ritual_custodians: Primary agenda-setter (institutional/constrained) â administers normative interpretation and derives legitimacy from performance
 *   - commemorating_community: Primary beneficiary (organized/identity_locked) â receives coordination of memory and identity continuity
 *   - individual_participants: Primary target (moderate/identity_locked) â bears performance costs and foreclosure of alternative practices
 *   - alternative_mourners: Excluded voice (moderate/constrained) â would mourn or commemorate outside ritual structure
 *   - ritual_theorists: Analytical observer (analytical/analytical) â sees hybrid structure across sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.6).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Catastrophe Memory Function â Hybrid Transformation Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__hybrid_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, '4a2f622a-483c-45ca-ab61-678eca9800c1').
narrative_ontology:cs_kernel_codification('4a2f622a-483c-45ca-ab61-678eca9800c1', fixed_text).
narrative_ontology:cs_authority_grounding('4a2f622a-483c-45ca-ab61-678eca9800c1', lineage).
narrative_ontology:cs_interpretation_layer_present('4a2f622a-483c-45ca-ab61-678eca9800c1').
narrative_ontology:cs_reading_relation('4a2f622a-483c-45ca-ab61-678eca9800c1', catastrophe_memory_function__mourning_practice_reading, influences).
narrative_ontology:cs_reading_relation('4a2f622a-483c-45ca-ab61-678eca9800c1', catastrophe_memory_function__survival_competence_reading, influences).
narrative_ontology:cs_axiom('4a2f622a-483c-45ca-ab61-678eca9800c1', foundational, mourning_and_survival_are_co_original_in_ritual).
narrative_ontology:cs_axiom_status(mourning_and_survival_are_co_original_in_ritual, holdable).
narrative_ontology:cs_axiom_grounding('4a2f622a-483c-45ca-ab61-678eca9800c1', mourning_and_survival_are_co_original_in_ritual, conventional).
narrative_ontology:cs_reference_frame('4a2f622a-483c-45ca-ab61-678eca9800c1', commemorative_lineage_praxis).
narrative_ontology:cs_drift_state('4a2f622a-483c-45ca-ab61-678eca9800c1', modern_secular_transformation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4a2f622a-483c-45ca-ab61-678eca9800c1', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, commemorating_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, ritual_custodians).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, individual_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbis and religious authorities who interpret, authorize, and enforce correct seder performance. They maintain the ritual's normative structure, adjudicate proper commemorative practice, and derive institutional legitimacy from the community's continued performance. Their authority is tied to the lineage of transmission; leaving the interpretive role means abandoning the tradition's custodial chain.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_custodians, agenda_setter,
    institutional, generational, constrained, global).

% Jewish communities worldwide who perform the seder and receive group identity continuity, intergenerational cohesion, and the coordinated preservation of catastrophe memory. The ritual binds them across diaspora through shared enactment, but their participation is structured by inherited obligation rather than individual choice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, commemorating_community, beneficiary,
    organized, generational, identity_locked, global).

% Individual Jews who bear the costs of ritual performance: time, material resources for the seder, emotional labor of engaging with traumatic narrative, and the foreclosure of alternative mourning or identity practices. For many, participation is not optional but constitutive of family and communal belonging; exit would mean social severance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, individual_participants, payer,
    moderate, biographical, identity_locked, local).

% Secular Jews, transformative theologians, and private mourners who would commemorate catastrophe outside the ritualized seder structure. They are excluded from normative Jewish commemorative space by the expectation that authentic memory work happens through the ritual form; their alternative grief practices are delegitimized as insufficient or assimilationist.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, alternative_mourners, excluded,
    moderate, biographical, constrained, national).

% Academic scholars of religion and anthropologists who analyze the seder's dual function as both mourning practice and survival rehearsal. They observe the structural coupling of D1/D4 and D5 elements without being bound by the ritual's normative demands.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves collective memory of catastrophic loss (D1/D4) while transmitting practical competence for group survival and adaptation (D5) through annual enacted ritual performance.
% TRANSFER_FUNCTION: Moves obligation to perform commemorative acts from ancestral mandate and religious authority to individual participants; transfers extracted legitimacy and interpretive authority to ritual custodians; moves adaptive knowledge and group boundary maintenance across generations.
% ABSENT_VOICES: Secular memorializers, private mourners outside ritual structure, and transformative theologians who would decouple catastrophe from identity are structurally excluded; their absence creates the appearance of unanimous consensus on ritual necessity.
% DISAPPEARANCE_RATIONALE: Jewish communal identity would lose a primary mechanism for intergenerational catastrophe memory transmission; mourning would fragment into private practice, survival competence would require alternative pedagogical channels, and the custodial authority of ritual interpreters would erode.
% FOUNDING_PROBLEM: How to maintain group cohesion, identity boundaries, and practical survival capacity after catastrophic loss (exile, destruction, persecution) in the absence of territorial or political sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Historians and biblical scholars attest the origin of Passover in historical catastrophe and pre-state memory preservation. Sociologists of religion and secular Jewish studies scholars contest that the contemporary seder remains primarily responsive to survival-threat versus serving institutional continuity and identity performance. Academic ritual theorists outside the benefiting community provide corroboration for both the historical origin and the functional-shift reading.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.60 over the interval as the ritual's survival functions become increasingly symbolic and its performative dimensions dominate practical pedagogy. Theater ratio climbs from 0.20 to 0.55, indicating that a growing share of ritual activity is symbolic performance rather than direct competence transmission. Suppression requirement begins high (0.70) in pre-modern contexts with strong communal enforcement, dips to 0.58 as secularization relaxes formal coercion, then ticks up to 0.62 as contemporary identity politics re-intensify soft enforcement through social belonging rather than institutional sanction. Accessibility collapse (0.60) reflects that alternatives to ritualized commemoration are technically available but socially delegitimized within the identity framework. Resistance is low (0.30) because the constraint is largely internalized; overt opposition is rare and atomized.
 *
 * PERSPECTIVAL GAP:
 *   From the custodial seat, the seder is sacred coordination preserving a people against annihilation; from the individual participant seat, it is an inherited obligation whose costs are borne personally while benefits accrue diffusely to group continuity; from the excluded alternative mourner seat, it is a suppression mechanism that monopolizes legitimate grief. The engine computes these divergences from the structural data without requiring the author to reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual custodians and the commemorating community sit near the beneficiary end: custodians collect institutional legitimacy and interpretive authority, while the community receives coordination benefits of identity and memory. Individual participants sit near the target end: their identity_locked exit options mean their effective extraction is amplified by the constraint's scope and their structural position. Alternative mourners are excluded entirely, experiencing the constraint as pure suppression. The engine will compute divergent per-seat classifications: custodians near rope, participants near tangled_rope or snare boundary depending on exit modulation.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled rope rather than rope prevents masking the extraction that rides on genuine coordination: the ritual does solve a real collective-action problem (memory transmission without state infrastructure), but it also imposes identity-locked costs and suppresses alternatives. Classifying it as snare would be incorrect because the coordination function is not cover â the memory and competence transmission are structurally real. The hybrid reading makes this especially salient: mourning and survival are genuinely coupled, not a pretext. Tangled rope captures the simultaneity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_originality_empirical_basis,
    'Does the hybrid coupling of mourning and survival competence describe an inherent structural feature of the ritual, or an interpretive frame projected by the analyst?',
    'Comparative ethnography of catastrophe rituals across cultures: if hybrid D1/D4+D5 structures appear independently where similar survival pressures exist, co-originality is structurally robust; if the hybrid reading only emerges in specific interpretive traditions, it is projection.',
    'If projection, the constraint''s claimed coordination function fragments into separate mourning and survival mechanisms, reducing cohesiveness and potentially reclassifying the hybrid structure as a post-hoc justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_originality_empirical_basis, empirical, 'Whether mourning-survival hybridity is intrinsic or interpretive').

omega_variable(
    identity_lock_vs_voluntary_coordination,
    'Do participants experience seder performance as freely chosen coordination or as identity-locked obligation?',
    'Post-exit trajectory study: measuring psychological cost, social severance, and alternative mourning uptake among individuals who cease ritual participation.',
    'If identity-locked, effective extraction is higher than structural metrics suggest; the constraint operates closer to the snare boundary. If voluntary, the constraint leans toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_voluntary_coordination, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    kernel_reading_containment,
    'Does the hybrid reading''s co-originality claim foreclose the sibling pure readings, or merely influence them?',
    'Analysis of whether a single party can consistently hold the hybrid reading while also asserting that mourning or survival competence alone is the ritual''s sufficient reason.',
    'If foreclosing, the kernel generates logical tension that raises the constraint''s internal resistance; if merely influencing, the readings are separate constraints in a family linked by network edges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_containment, conceptual, 'Logical relationship between hybrid and pure readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 80, 0.48).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 100, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_function kernel decomposes into three structurally distinct constraint stories per the epsilon-invariance principle: this hybrid reading (D1/D4 + D5), a mourning-only reading (D1/D4), and a survival-only reading (D5). Each reading has a different epsilon, different stakeholder directionalities, and different classification. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
