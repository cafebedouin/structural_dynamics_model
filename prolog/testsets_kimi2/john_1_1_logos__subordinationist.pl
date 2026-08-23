% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__subordinationist, []).

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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Subordinationist Logos Reading (John 1:1)
 *   domain: theology/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   The prologue of John's Gospel (John 1:1) is a contested kernel in
 *   Christian theology. This constraint story instantiates the
 *   subordinationist reading: the Logos is the first and highest creation of
 *   the Father, a real personal agent but not co-eternal or consubstantial.
 *   When held as normative, this reading constrains worship (veneration
 *   permitted, latria forbidden) and undermines high-church sacramental
 *   authority that depends on Christ's full divinity. It functions as
 *   doctrinal coordination for subordinationist communities and as asymmetric
 *   extraction from Nicene high-church traditions.
 *
 * KEY AGENTS:
 *   - high_church_traditions: Primary target (institutional/identity_locked) â bears authority loss from the denied full divinity of the Logos
 *   - subordinationist_communities: Primary beneficiary (moderate/identity_locked) â receives monotheistic coordination and worship clarity
 *   - subordinationist_leadership: Agenda-setter (organized/identity_locked) â enforces the doctrinal boundary and worship distinction
 *   - critical_biblical_scholars: Analytical observer (analytical/analytical) â evaluates textual evidence without confessional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.45).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.6).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.45).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Subordinationist Logos Reading (John 1:1)").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, 'dc76da93-e773-4b95-bc3b-4f4fd4d13a2c').
narrative_ontology:cs_kernel_codification('dc76da93-e773-4b95-bc3b-4f4fd4d13a2c', fixed_text).
narrative_ontology:cs_authority_grounding('dc76da93-e773-4b95-bc3b-4f4fd4d13a2c', lineage).
narrative_ontology:cs_interpretation_layer_present('dc76da93-e773-4b95-bc3b-4f4fd4d13a2c').
narrative_ontology:cs_reading_relation('dc76da93-e773-4b95-bc3b-4f4fd4d13a2c', john_1_1_logos__orthodox_christological, forecloses).
narrative_ontology:cs_reading_relation('dc76da93-e773-4b95-bc3b-4f4fd4d13a2c', john_1_1_logos__non_incarnational_monotheist, forecloses).
narrative_ontology:cs_axiom('dc76da93-e773-4b95-bc3b-4f4fd4d13a2c', foundational, logos_is_created_being).
narrative_ontology:cs_axiom_status(logos_is_created_being, holdable).
narrative_ontology:cs_axiom_grounding('dc76da93-e773-4b95-bc3b-4f4fd4d13a2c', logos_is_created_being, theological).
narrative_ontology:cs_axiom('dc76da93-e773-4b95-bc3b-4f4fd4d13a2c', foundational, father_alone_unoriginate).
narrative_ontology:cs_axiom_status(father_alone_unoriginate, holdable).
narrative_ontology:cs_axiom_grounding('dc76da93-e773-4b95-bc3b-4f4fd4d13a2c', father_alone_unoriginate, theological).
narrative_ontology:cs_reference_frame('dc76da93-e773-4b95-bc3b-4f4fd4d13a2c', strict_monotheist_communion).
narrative_ontology:cs_drift_state('dc76da93-e773-4b95-bc3b-4f4fd4d13a2c', contemporary_trinitarian_majority, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('dc76da93-e773-4b95-bc3b-4f4fd4d13a2c', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_communities).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_traditions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They articulate and enforce the doctrinal boundary that the Logos is a created, subordinate divine agent. They preside over worship regulations that permit veneration but prohibit full divine worship (latria) of the Son, and they exclude Trinitarian teachings as incompatible with strict monotheism.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_leadership, agenda_setter,
    organized, generational, identity_locked, global).

% They receive a coherent monotheistic framework that distinguishes them from both polytheism and Nicene orthodoxy. Their worship practices, community boundaries, and scriptural reading are coordinated around the subordination of the Logos. Exit means abandoning a community identity fused with this Christology.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_communities, beneficiary,
    moderate, biographical, identity_locked, global).

% Their authority to administer sacraments, ordain clergy, and define orthodoxy rests on the claim that the Logos is fully divine and consubstantial with the Father. The subordinationist reading undermines this foundation, forcing them to expend theological and institutional resources defending Nicene orthodoxy against the charge of tri-theism.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, high_church_traditions, payer,
    institutional, civilizational, identity_locked, global).

% They analyze the lexical, textual, and historical evidence for John 1:1 without confessional commitment to either Nicene or subordinationist dogma. Their work is cited by both sides but they do not bear the constraint's costs or collect its coordinative benefits.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, critical_biblical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the tension between strict Jewish-Hellenistic monotheism and the prologue of John's Gospel by positing a divine agent who is preeminent yet created, allowing veneration of the Logos without violating the absolute uniqueness of the Father.
% TRANSFER_FUNCTION: Moves liturgical authority and sacramental legitimacy away from high-church traditions grounded in Nicene consubstantiality, and transfers coordinative power to communities that read John 1:1 as created subordination.
% ABSENT_VOICES: Trinitarian theologians and high-church liturgists are absent from subordinationist councils; their full-divinity arguments are ruled out a priori. Non-incarnational readings that treat Logos as mere metaphor are also excluded from the debate as reductive.
% DISAPPEARANCE_RATIONALE: Subordinationist communities organize their worship, baptismal practice, and ecclesial boundaries around the premise that the Logos is not fully divine. Removing the reading would dissolve the doctrinal boundary between them and Nicene orthodoxy, forcing rearrangement of liturgy and authority.
% FOUNDING_PROBLEM: How to reconcile John 1:1's apparent personalization of the Logos with an uncompromised monotheism that reserves absolute divinity for the Father alone.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by extra-Nicene historical sources and by modern non-Trinitarian biblical scholars who attest the problem as unresolved within a strict monotheist framework. High-church traditions attest the problem is solved by the Nicene Creed, but they are inside the benefiting/victim set; independent textual historians and comparative religion scholars outside both confessions provide the external corroboration.
narrative_ontology:disappearance_verdict(john_1_1_logos__subordinationist, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__subordinationist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__subordinationist, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__subordinationist_tests).
:- end_tests(john_1_1_logos__subordinationist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.45) is moderate: the reading denies a foundational premise of high-church authority but does not directly confiscate resources. Suppression (0.60) reflects the active enforcement of worship boundaries and exclusion of Trinitarian teaching. Theater ratio (0.25) is relatively low because the theological commitment is held sincerely; however, it rises during periods of political weakness where the constraint is maintained performatively. Accessibility collapse (0.55) captures the identity-locked nature of the communities: once the subordinationist frame is adopted, Nicene orthodoxy appears as tri-theism, collapsing the epistemic accessibility of alternatives. Resistance (0.75) is high because Nicene high-church traditions have defended their authority against this reading for centuries through conciliar anathemas and catechesis.
 *
 * PERSPECTIVAL GAP:
 *   The subordinationist community seat computes the constraint as protective coordination (a rope that keeps monotheism pure), while the high-church tradition seat computes it as an authority-snare that undermines sacramental validity. The agenda-setter leadership experiences a hybrid seat: they enforce coordination but also bear the cost of marginalization. The engine's per-seat classification will diverge accordingly.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (subordinationist_communities) are identity-locked adherents with low power; their directionality sits near the beneficiary end, reducing effective extraction. Victims (high_church_traditions) are institutional actors also identity-locked to Nicene orthodoxy; their directionality sits near the target end, amplifying effective extraction. The agenda_setter (subordinationist_leadership) is organized and identity-locked, deriving moderate directionality. Scope is global, amplifying extraction slightly for all seats due to verification difficulty across dispersed communities.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope is warranted because the constraint simultaneously solves a genuine coordination problem (how to read John 1:1 without abandoning monotheism) and performs asymmetric extraction (denying high-church authority). If we ignored the coordination function, we would misclassify as a snare; if we ignored the authority extraction, we would misclassify as a rope. The active enforcement requirement (worship boundaries, exclusion of Trinitarian teaching) prevents reading the arrangement as a benign mountain or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_suppression_mechanism,
    'Is the suppression of Nicene alternatives within subordinationist communities achieved through structural excommunication or through internalized identity fusion that makes Trinitarianism unthinkable?',
    'Post-exit belief trajectory: if individuals leaving subordinationist communities continue to reject Trinitarianism due to cognitive schema rather than social penalty, suppression is partially internalized.',
    'If internalized, effective suppression exceeds structural measures, raising the constraint''s extractive impact on high-church traditions by sealing members against missionary or liturgical reintegration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_suppression_mechanism, empirical, 'Structural vs internalized suppression in doctrinal enforcement').

omega_variable(
    textual_underdetermination,
    'Does the Johannine prologue grammatically and lexically underdetermine the ontology of the Logos, making the subordinationist reading an interpretation rather than a textual necessity?',
    'Comprehensive syntactic and discourse analysis of John 1:1-18 across Koine Greek corpora, assessing whether theos in 1:1c and egeneto in 1:14 commit to a specific ontological category.',
    'If underdetermined, the constraint''s base extractiveness is entirely generated by the interpretive layer (high theater ratio), pushing classification toward piton or snare; if determined, it functions as a more honest coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_underdetermination, conceptual, 'Whether the kernel text fixes the reading or permits alternatives').

omega_variable(
    authority_transfer_mechanism,
    'Does the subordinationist reading directly capture and redirect authority from high-church traditions, or does it merely expose a pre-existing legitimacy deficit in their sacramental claims?',
    'Comparative historical analysis of high-church institutional resilience in contexts with and without subordinationist competition, controlling for state patronage.',
    'If direct transfer, gain_flow should name a capturer; if exposure of deficit, the constraint is more akin to a diagnostic scaffold that reveals instability rather than extracting value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_transfer_mechanism, conceptual, 'Whether extraction is active capture or passive revelation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t0, john_1_1_logos__subordinationist, theater_ratio, 0, 0.15).
narrative_ontology:measurement(john_tr_t6, john_1_1_logos__subordinationist, theater_ratio, 6, 0.2).
narrative_ontology:measurement(john_tr_t12, john_1_1_logos__subordinationist, theater_ratio, 12, 0.35).
narrative_ontology:measurement(john_tr_t18, john_1_1_logos__subordinationist, theater_ratio, 18, 0.45).
narrative_ontology:measurement(john_tr_t24, john_1_1_logos__subordinationist, theater_ratio, 24, 0.3).
narrative_ontology:measurement(john_tr_t30, john_1_1_logos__subordinationist, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(john_be_t0, john_1_1_logos__subordinationist, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(john_be_t6, john_1_1_logos__subordinationist, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(john_be_t12, john_1_1_logos__subordinationist, base_extractiveness, 12, 0.3).
narrative_ontology:measurement(john_be_t18, john_1_1_logos__subordinationist, base_extractiveness, 18, 0.2).
narrative_ontology:measurement(john_be_t24, john_1_1_logos__subordinationist, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(john_be_t30, john_1_1_logos__subordinationist, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t0, john_1_1_logos__subordinationist, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(john_su_t6, john_1_1_logos__subordinationist, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(john_su_t12, john_1_1_logos__subordinationist, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(john_su_t18, john_1_1_logos__subordinationist, suppression_requirement, 18, 0.25).
narrative_ontology:measurement(john_su_t24, john_1_1_logos__subordinationist, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(john_su_t30, john_1_1_logos__subordinationist, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the John 1:1 kernel. The orthodox and non-incarnational readings are separate constraints linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
