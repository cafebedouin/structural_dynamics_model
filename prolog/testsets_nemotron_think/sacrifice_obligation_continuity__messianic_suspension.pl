% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Sacrifice Obligation Suspended Pending Messianic Restoration; Study Maintains Readiness
 *   domain: religious/legal/textual_tradition
 *
 * SUMMARY:
 *   After the Second Temple's destruction (70 CE), rabbinic Judaism faced a
 *   crisis: the central commandments — sacrifices, priestly service,
 *   pilgrimage — were physically impossible. The messianic_suspension reading
 *   resolves this by declaring the obligation suspended (not violated, not
 *   abrogated) pending messianic restoration of the Temple. Study of
 *   sacrificial law (seder kodashim) becomes the maintenance protocol: the
 *   community stays 'ready' through textual engagement. No current victim set
 *   exists because the obligation is not operationally enforced; the
 *   extraction is the readiness burden — daily study, institutional support,
 *   cognitive orientation toward a future that recedes. The constraint is a
 *   piton: the original coordination function (actual sacrifice as communal
 *   worship) has atrophied, but the study regime persists through
 *   institutional inertia and theological maintenance. The claimed type
 *   (piton) and metrics (moderate extractiveness, rising theater, low
 *   suppression) are authored independently; the engine will compute per-seat
 *   classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.42).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.25).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.42).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, piton).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Sacrifice Obligation Suspended Pending Messianic Restoration; Study Maintains Readiness").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious/legal/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, '2f363984-344d-44ac-9a7e-c3cd7c325f9c').
narrative_ontology:cs_kernel_codification('2f363984-344d-44ac-9a7e-c3cd7c325f9c', fixed_text).
narrative_ontology:cs_authority_grounding('2f363984-344d-44ac-9a7e-c3cd7c325f9c', lineage).
narrative_ontology:cs_interpretation_layer_present('2f363984-344d-44ac-9a7e-c3cd7c325f9c').
narrative_ontology:cs_reading_relation('2f363984-344d-44ac-9a7e-c3cd7c325f9c', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('2f363984-344d-44ac-9a7e-c3cd7c325f9c', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('2f363984-344d-44ac-9a7e-c3cd7c325f9c', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('2f363984-344d-44ac-9a7e-c3cd7c325f9c', foundational, obligation_suspended_not_violated).
narrative_ontology:cs_axiom_status(obligation_suspended_not_violated, holdable).
narrative_ontology:cs_axiom_grounding('2f363984-344d-44ac-9a7e-c3cd7c325f9c', obligation_suspended_not_violated, theological).
narrative_ontology:cs_axiom('2f363984-344d-44ac-9a7e-c3cd7c325f9c', foundational, study_maintains_readiness).
narrative_ontology:cs_axiom_status(study_maintains_readiness, holdable).
narrative_ontology:cs_axiom_grounding('2f363984-344d-44ac-9a7e-c3cd7c325f9c', study_maintains_readiness, theological).
narrative_ontology:cs_reference_frame('2f363984-344d-44ac-9a7e-c3cd7c325f9c', sinaitic_obligation_continuity).
narrative_ontology:cs_drift_state('2f363984-344d-44ac-9a7e-c3cd7c325f9c', post_temple_destruction, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f363984-344d-44ac-9a7e-c3cd7c325f9c', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, halakhic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, observant_practitioners).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, observant_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, sinaitic_commandment_perpetuity).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, messianic_restoration_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, oral_torah_transmission_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and administer the suspension doctrine: the sacrifice obligation remains binding but is in abeyance until Temple restoration. They authorize the study curriculum that constitutes readiness, adjudicate edge cases, and maintain the interpretive framework that prevents the obligation from lapsing into archival status. Their authority rests on the claim of unbroken transmission from Sinai.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, rabbinic_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Produce the textual labor that sustains the readiness protocol — commentaries, novellae, practical simulations of Temple service. Their professional standing and institutional positions depend on the obligation's continued vitality as a live halakhic category, not a dead letter. They benefit from the interpretive monopoly but are constrained by the tradition's internal logic.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, halakhic_scholars, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, halakhic_scholars, agenda_setter).

% Bear the readiness burden: daily study of sacrificial law (seder kodashim), financial support for yeshivot that maintain the curriculum, and the cognitive load of orienting life around a Temple that does not exist. They receive identity continuity, communal coherence, and the assurance that the covenant remains intact. Exit means leaving the observant framework entirely — a high identity cost.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, observant_practitioners, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, observant_practitioners, beneficiary).

% Advocate for immediate resumption of sacrifice on the Temple Mount, rejecting the suspension doctrine as a quietist accommodation. They are structurally excluded from the halakhic decision-making bodies that define the constraint. Their presence would challenge the suspension's legitimacy; their absence is maintained by the same authority structure that administers the suspension.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, temple_restoration_activists, excluded,
    moderate, immediate, trapped, regional).

% Study the tradition as a cultural-historical phenomenon: how a text-centered community maintains a commandment's vitality for two millennia without physical performance. They neither collect nor pay; they document the constraint's operation from outside its normative frame.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, secular_academic_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the sacrifice obligation as a live, binding category across the destruction of its physical substrate (the Temple), preserving communal identity, textual continuity, and the theological claim that the covenant has not been abrogated — only deferred.
% TRANSFER_FUNCTION: Moves interpretive authority and study labor from the practitioner base to the rabbinic-scholarly apparatus. The obligation's operational cost (animal offerings, pilgrimage, priestly gifts) is replaced by a cognitive-institutional cost (study, curriculum maintenance, deference to authority). No material transfer to a beneficiary class occurs; the extraction is the readiness burden itself.
% ABSENT_VOICES: Temple Mount activists who would perform sacrifice now (excluded by rabbinic consensus and state enforcement); former observant practitioners who experienced the study burden as hollow theater and left (their departure is invisible to the constraint's self-accounting); non-Orthodox Jewish movements that treat sacrificial law as purely historical (they are not participants in the halakhic conversation).
% DISAPPEARANCE_RATIONALE: If the suspension doctrine and its study regime vanished overnight, the sacrificial commandment would either revert to an active demand (politically impossible, religiously chaotic) or collapse into archival memory (severing the continuity claim that anchors Orthodox identity). The communal structure organized around 'we are ready' would lose its orienting horizon.
% FOUNDING_PROBLEM: How to maintain the Sinaitic sacrifice obligation as a binding, living commandment after the Roman destruction of the Second Temple (70 CE) made its physical performance impossible, without conceding that God's law can be abrogated by historical circumstance.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record of the Temple's destruction and the immediate rabbinic response (Mishnah, Tosefta, Talmud) which explicitly constructs the suspension/readiness framework. The corroboration comes from the historical fact of the destruction itself — an external event no party controls — and the contemporaneous documentary record of the rabbinic innovation. No beneficiary group disputes that the destruction occurred; the dispute is over the proper response.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) reflects the sustained cognitive-institutional burden of maintaining readiness for a restoration with no timeline. Theater_ratio (0.48) captures the growing gap between the study regime's elaboration and its operational payoff — the curriculum expands (Mishnah → Talmud → codes → commentaries → yeshiva curricula) while the Temple remains absent. Suppression (0.25) is low: the constraint persists through authority and identity, not coercion; dissenters leave rather than being silenced. Accessibility_collapse (0.55) is moderate: alternative framings (archival, performance-only, study-as-performance) exist but are marginalized within the halakhic conversation. Resistance (0.15) is minimal internally; external challenge (activists, reform movements) does not register as resistance within the constraint's own frame.
 *
 * PERSPECTIVAL GAP:
 *   From the authority seat, the constraint is a rope: genuine coordination preserving covenantal continuity across catastrophe. From the practitioner seat, it approaches a piton: a degraded former obligation where the study burden persists without the original function, maintained by identity lock and institutional inertia. The engine computes this divergence from the structural data; the authored claim (piton) reflects the analyst's structural reading, not the authority's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and scholars sit near the beneficiary end (d ~ 0.2): they collect interpretive authority, professional standing, and institutional control from the suspension doctrine. Observant practitioners sit near the target end (d ~ 0.7): they bear the study burden and cognitive orientation cost with no operational return, and their exit is identity-constrained. Temple activists are excluded (trapped exit) — their exclusion is the enforcement object. Secular observers are analytical (d = 0.5 by definition). The derivation follows from beneficiary declarations (authorities, scholars) + payer declarations (practitioners) + exit modulation (practitioners = constrained/identity_locked).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction making sacrifice impossible) remains live — the physical impossibility persists. The arrangement (suspension + study readiness) was built for this problem and still addresses it. However, the mandate has thickened: what began as a minimal holding pattern (Mishnah's skeletal sacrificial law) became an ever-expanding textual edifice. The mandatrophy question is whether the study regime's current scale and complexity still serve the founding problem or have become self-justifying. The corridor between 'readiness maintenance' and 'textual empire' is the extraction gradient.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the messianic_suspension reading a distinct constraint from its siblings, or a strategic framing within a single halakhic discourse?',
    'Compare the beneficiary/victim structures and extraction profiles across readings. If each reading produces a different ε and different stakeholder roles when assessed by its own lights, they are distinct constraints. If they share ε and roles but differ only in self-description, they are framings of one constraint.',
    'If distinct constraints, each gets its own classification and the kernel is a family. If one constraint, the ''readings'' are observer positions on a single structure — the engine''s per-seat computation already captures this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel''s declared readings are structurally distinct constraints or observer framings.').

omega_variable(
    readiness_burden_nature,
    'Is the study burden a genuine coordination cost (maintaining communal readiness) or extractive theater (elaborating a curriculum for a restoration that recedes)?',
    'Measure the correlation between study intensity and restoration-preparedness indicators (e.g., priestly lineage preservation, Temple vessel reconstruction, halakhic consensus on procedural details). If study tracks preparedness, it is coordination cost. If study expands while preparedness stagnates, it is theater.',
    'If coordination cost, the constraint leans rope/scaffold. If theater, it leans piton/snare. The current theater_ratio (0.48) suggests the latter but the measurement is contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readiness_burden_nature, empirical, 'Whether the readiness burden is functional preparation or performative maintenance.').

omega_variable(
    messianic_horizon_credibility,
    'Does the messianic restoration horizon function as a genuine expected future or a perpetual deferral mechanism that stabilizes the suspension?',
    'Track the community''s practical investments in restoration-readiness (land purchases, priestly training, vessel fabrication, political advocacy) vs. purely textual study. If material preparation accompanies textual study, the horizon is credited. If only textual study exists, the horizon is a deferral device.',
    'If genuine horizon, the constraint is a scaffold (transitional with distant sunset). If deferral device, it is a piton (inertial persistence). The absence of a sunset clause and the indefinite timeline favor piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_horizon_credibility, conceptual, 'Whether messianic restoration is a believed future or a stabilizing fiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 70, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(socms_tr_t70, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 70, 0.2).
narrative_ontology:measurement(socms_tr_t500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 500, 0.3).
narrative_ontology:measurement(socms_tr_t1100, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1100, 0.4).
narrative_ontology:measurement(socms_tr_t1500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1500, 0.45).
narrative_ontology:measurement(socms_tr_t1800, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1800, 0.47).
narrative_ontology:measurement(socms_tr_t2025, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(socms_be_t70, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 70, 0.15).
narrative_ontology:measurement(socms_be_t500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 500, 0.25).
narrative_ontology:measurement(socms_be_t1100, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1100, 0.35).
narrative_ontology:measurement(socms_be_t1500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1500, 0.38).
narrative_ontology:measurement(socms_be_t1800, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(socms_be_t2025, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(socms_su_t70, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 70, 0.3).
narrative_ontology:measurement(socms_su_t500, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 500, 0.25).
narrative_ontology:measurement(socms_su_t1100, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1100, 0.2).
narrative_ontology:measurement(socms_su_t1500, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1500, 0.22).
narrative_ontology:measurement(socms_su_t1800, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(socms_su_t2025, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 2025, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__messianic_suspension, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the sacrifice_obligation_continuity kernel family. The four readings (messianic_suspension, study_as_performance, performance_only, archival_preservation) instantiate different constraints from the same textual kernel. They differ in ε (extractiveness), victim sets, and claimed types: messianic_suspension claims piton (moderate ε, no active victims); study_as_performance claims rope (low ε, study=fulfillment); performance_only claims tangled_rope (coordination + extraction, active enforcement against activists); archival_preservation claims mountain (ε≈0, no binding force). Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_continuity__messianic_suspension, institutional, 0.15).
constraint_indexing:directionality_override(sacrifice_obligation_continuity__messianic_suspension, organized, 0.25).
constraint_indexing:directionality_override(sacrifice_obligation_continuity__messianic_suspension, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
