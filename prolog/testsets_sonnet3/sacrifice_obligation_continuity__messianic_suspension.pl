% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__messianic_suspension, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Suspended Sacrifice Obligation Pending Messianic Restoration
 *   domain: religious/legal/textual_tradition
 *
 * SUMMARY:
 *   This constraint models one reading of a contested kernel about the status
 *   of Biblical/rabbinic sacrifice law after the Temple's destruction: the
 *   position that the obligation is neither fulfilled nor abrogated, but
 *   suspended pending messianic restoration, with textual study serving as a
 *   maintenance protocol that keeps the community 'ready' for reactivation.
 *   This is distinct from the sibling readings that treat study itself as
 *   fulfillment (study_as_performance), treat the obligation as still
 *   requiring literal future performance with no interim substitute
 *   (performance_only), or treat the entire law as historically superseded
 *   (archival_preservation). Those are separate constraints with separate ε
 *   values, linked here only by kernel identity, not merged into this one.
 *
 * KEY AGENTS:
 *   - rabbinic_scholarly_class: Primary agenda-setter and beneficiary (institutional/identity_locked) — administers readiness criteria, derives authority from custodianship
 *   - lay_observant_community: Primary bearer of the readiness burden (moderate/constrained) — carries liturgical and psychological weight without resolution
 *   - diaspora_communal_identity: Beneficiary (organized/identity_locked) — gains cohesive identity from the suspension frame
 *   - restorationist_movements and reform_reconstructionist_communities: Excluded voices whose positions would collapse or accelerate the suspension frame
 *   - textual_tradition_scholars: Analytical observer of the doctrine's structural function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.38).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.42).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.38).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Suspended Sacrifice Obligation Pending Messianic Restoration").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious/legal/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:has_sunset_clause(sacrifice_obligation_continuity__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, '02a10b07-7670-4aae-9132-9ecda30a1898').
narrative_ontology:cs_kernel_codification('02a10b07-7670-4aae-9132-9ecda30a1898', fixed_text).
narrative_ontology:cs_authority_grounding('02a10b07-7670-4aae-9132-9ecda30a1898', lineage).
narrative_ontology:cs_interpretation_layer_present('02a10b07-7670-4aae-9132-9ecda30a1898').
narrative_ontology:cs_reading_relation('02a10b07-7670-4aae-9132-9ecda30a1898', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('02a10b07-7670-4aae-9132-9ecda30a1898', sacrifice_obligation_continuity__performance_only, influences).
narrative_ontology:cs_reading_relation('02a10b07-7670-4aae-9132-9ecda30a1898', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('02a10b07-7670-4aae-9132-9ecda30a1898', foundational, obligation_remains_binding_but_dormant).
narrative_ontology:cs_axiom_status(obligation_remains_binding_but_dormant, holdable).
narrative_ontology:cs_axiom_grounding('02a10b07-7670-4aae-9132-9ecda30a1898', obligation_remains_binding_but_dormant, deontological).
narrative_ontology:cs_axiom('02a10b07-7670-4aae-9132-9ecda30a1898', foundational, restoration_condition_is_external_and_awaited).
narrative_ontology:cs_axiom_status(restoration_condition_is_external_and_awaited, holdable).
narrative_ontology:cs_axiom_grounding('02a10b07-7670-4aae-9132-9ecda30a1898', restoration_condition_is_external_and_awaited, theological).
narrative_ontology:cs_axiom('02a10b07-7670-4aae-9132-9ecda30a1898', secondary, study_constitutes_readiness_not_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_readiness_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('02a10b07-7670-4aae-9132-9ecda30a1898', study_constitutes_readiness_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('02a10b07-7670-4aae-9132-9ecda30a1898', temple_era_normative_performance).
narrative_ontology:cs_drift_state('02a10b07-7670-4aae-9132-9ecda30a1898', post_destruction_rabbinic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('02a10b07-7670-4aae-9132-9ecda30a1898', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholarly_class).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, religious_institutional_continuity).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, diaspora_communal_identity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, lay_observant_community).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, lay_observant_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, divine_law_immutability).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, temple_restoration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the interpretive apparatus that holds the sacrifice obligation as suspended rather than abrogated. Determines what counts as adequate 'study readiness,' trains successive generations in the relevant tractates, and derives professional standing and communal authority from being the custodians of a law that cannot currently be tested against practice. Cannot exit the framework without dissolving the basis of their own religious authority.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholarly_class, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholarly_class, beneficiary).

% Bears the ongoing communal burden of maintaining the suspended obligation's cognitive and liturgical presence: liturgical recitations referencing the sacrifices, holiday observances shaped around the absent Temple service, and a general orientation of readiness that structures calendar and law without ever culminating in physical performance. Gains communal identity and continuity from the arrangement but pays in the form of unresolved ritual incompleteness carried across generations.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, lay_observant_community, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, lay_observant_community, beneficiary).

% The suspension frame gives a dispersed, Temple-less community a coherent theological account of why observance continues in modified form rather than collapsing entirely. It receives cohesion and a shared eschatological horizon without needing physical infrastructure it does not possess.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, diaspora_communal_identity, beneficiary,
    organized, civilizational, identity_locked, global).

% Groups actively working toward practical Temple rebuilding or ritual reactivation (e.g., preparing implements, training priestly lineages) sit uneasily inside the suspension frame — their activity presses toward ending the suspension, which the interpretive mainstream generally treats as premature or halachically fraught. They are rarely given authoritative voice in the interpretive apparatus that governs when suspension ends.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, restorationist_movements, excluded,
    powerless, generational, trapped, regional).

% Communities that have functionally exited the suspension frame by treating sacrifice law as historically superseded are not part of the interpretive conversation that maintains the messianic-suspension reading; their departure is treated as a lapse from the tradition rather than incorporated as a competing account.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, reform_and_reconstructionist_communities, excluded,
    moderate, generational, mobile, global).

% Academic and comparative-religion scholars analyze how the suspension doctrine functions structurally — as a device that keeps a law formally binding and psychologically present while indefinitely deferring the conditions for its performance. They take no side in whether restoration will occur.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, textual_tradition_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, stable account of why an obligation regarded as still divinely binding is not currently performed, allowing a dispersed community without a Temple to maintain religious-legal continuity, calendar structure, and identity across many generations without either declaring the law void or declaring itself in perpetual violation.
% TRANSFER_FUNCTION: Moves interpretive authority and communal deference toward the rabbinic scholarly class, who administer what counts as adequate study and readiness; moves psychological and liturgical labor (recitation, study, calendrical orientation) from the community onto itself, in exchange for a resolved theological status (suspended, not violated) rather than an unresolved one.
% ABSENT_VOICES: Restorationist movements pressing for practical reactivation are marginalized as premature; communities that have functionally treated the law as superseded (reform/reconstructionist) are excluded from the conversation that maintains the suspension frame, since their position would collapse the reading entirely.
% DISAPPEARANCE_RATIONALE: If the suspension framing were abandoned, observant communities would have to choose between an archival-preservation reading (no normative force, memory only) or a performance-only reading (openly living in indefinite violation), either of which reorganizes daily liturgy, the content of religious education, the psychological orientation of holiday observance, and the theological status of the rabbinic class's authority over 'readiness.'
% FOUNDING_PROBLEM: After the Temple's destruction, the community faced a stark theological problem: physical performance of a divinely commanded sacrificial obligation had become structurally impossible, threatening either the law's binding character or the community's standing as compliant. The suspension doctrine was constructed to hold the obligation's validity intact without requiring impossible performance.
% FOUNDING_PROBLEM_CORROBORATION: The rabbinic scholarly class attests the founding problem (impossibility of Temple service) remains live and the suspension is theologically necessary pending restoration. Comparative-religion scholars and historians of Jewish law, writing from outside the interpretive tradition's own authority structure, note that the doctrine also performs an ongoing institutional function — sustaining rabbinic interpretive authority and communal cohesion — independent of whether restoration is imminent or even coherent as a near-term prospect; this is corroboration of a shifted or dual function, not of the problem's death.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).
:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) rather than high because there is no active, enforced victim set bearing concrete costs — the reading's own structural delta is precisely that violation-guilt is absent; what is extracted instead is diffuse cognitive/liturgical labor and deference to interpretive authority, which is real but mild. Theater ratio is elevated and rising (0.40 to 0.55) because an increasing share of the study-as-readiness activity is symbolic maintenance of a status (halachically 'ready') rather than functional preparation toward an operationally near restoration — this is authored honestly as the doctrine's most persistent critique from outside its own tradition. Suppression (0.42) reflects real but moderate pressure against declaring the law void or performing sacrifice prematurely, not coercive extraction. Resistance is low (0.3) because the reading is broadly stable within its tradition and meets little organized internal challenge, though excluded readings exist at the margins.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic scholarly class's seat, this is a coherent scaffold: temporary by its own declared logic (sunset = messianic restoration), coordinating continuity until conditions change. From an external comparative-religion seat, the 'temporary' framing has persisted for nearly two millennia with no operational criteria for when suspension ends, which is exactly the profile a scaffold takes when threatened with becoming a piton — the sunset clause is real in doctrine but has never been triggered, and the engine's computed classification for that seat may diverge sharply from the doctrinal self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic scholarly class sits nearest the beneficiary end: it authors and administers the readiness criteria and derives durable institutional authority from being sole custodian of a law that can never presently be tested. The lay community sits closer to symmetric-to-target: it carries the liturgical and psychological cost of unresolved obligation but also receives identity and communal coherence, hence dual role (payer + beneficiary). No group is declared a victim because the reading's defining structural feature is the absence of an active extraction target — nothing is currently taken from anyone by force; what is extracted is diffuse readiness-labor, which is why extractiveness is moderate rather than high and victims is empty.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification requires an honest sunset clause (messianic restoration) and a coordination function (continuity of law and identity absent the Temple). Because no operational trigger condition for the sunset has been specified or approached in the doctrine's multi-millennium history, this story flags — via omega and via the rising theater_ratio series — the live possibility that the engine's computed type diverges from the claimed scaffold toward piton at some seats: a scaffold whose sunset never approaches proximately functions descriptively like inertial maintenance, even while its own framework insists it remains transitional. The claim (scaffold) and the metrics (rising theater, moderate but non-zero extraction) are authored independently and are not reconciled to each other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_versus_piton_drift,
    'Is the messianic-suspension doctrine still functioning as a genuine transitional scaffold awaiting an identifiable restoration condition, or has multi-millennium non-approach toward that condition converted it descriptively into inertial (piton-like) maintenance dressed in scaffold language?',
    'Track whether any operational, non-symbolic criteria for restoration proximity have been articulated or approached across the doctrine''s history (e.g., concrete institutional preparation with realistic near-term prospects, versus purely devotional/theoretical readiness maintenance with no proximate trigger).',
    'If no such criteria have ever been articulated or approached, several stakeholder seats (particularly the rabbinic scholarly class''s administrative seat) may compute closer to piton than scaffold despite the doctrinally claimed sunset clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_versus_piton_drift, conceptual, 'Whether the doctrinal sunset clause is operative or purely rhetorical after millennia of non-triggering.').

omega_variable(
    kernel_reading_selection_evidence,
    'What textual or communal signals justify treating messianic_suspension as the dominant reading for this story rather than study_as_performance or performance_only, given all three remain live positions within rabbinic literature?',
    'Comparative analysis of normative legal codes (e.g., Maimonides'' treatment of hilkhot beit ha-bechirah and korbanot) against liturgical practice and communal self-description, to establish which reading dominates in which communities and periods.',
    'If study_as_performance dominates in a given community''s self-understanding, the effective constraint operating on that community is structurally different (obligation actively discharged through study, not merely maintained-in-readiness) — a different ε and a different victim/beneficiary structure would apply, requiring a separate story rather than reclassification of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Under-determination among sibling kernel readings and the basis for selecting this one.').

omega_variable(
    readiness_burden_measurement,
    'How should the diffuse psychological and liturgical cost borne by lay observant communities under indefinite suspension be measured against the benefit of communal identity and continuity the same suspension provides?',
    'Ethnographic or survey-based study of observant communities'' self-reported relationship to sacrifice-related liturgy (aninut over the Temple''s absence, holiday practices referencing suspended offerings) versus reported communal cohesion benefits.',
    'A finding of substantial unresolved psychological burden with declining communal benefit would push extractiveness upward for the lay-community seat; a finding of net positive identity benefit with low burden would support the moderate-extraction reading authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readiness_burden_measurement, empirical, 'Empirical uncertainty in weighing diffuse readiness costs against communal identity benefits for lay stakeholders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 20, 0.44).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 40, 0.47).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 60, 0.5).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 80, 0.53).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 60, 0.34).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 80, 0.36).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 100, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__messianic_suspension, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__messianic_suspension, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This story is one of four linked readings of the sacrifice_obligation_continuity kernel. Each reading is authored as its own ε-invariant constraint per the ε-invariance principle: messianic_suspension (this story, moderate extraction, no active victims, scaffold claim), study_as_performance (obligation actively discharged through study, likely different extraction profile centered on scholarly-class gatekeeping of what counts as valid study), performance_only (obligation persists unfulfilled, potentially higher tension/resistance from the unresolved-violation framing), and archival_preservation (lowest extraction, no ongoing normative claim, closest to rope or piton). The readings are not merged; they are linked here for contamination/network propagation analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
