% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study-as-Performance Reading of the Sacrificial Commandment
 *   domain: religious_studies/halakhic_theory/commitment_system
 *
 * SUMMARY:
 *   After the destruction of the Second Temple, the rabbinic tradition taught
 *   that engagement with the laws of sacrifice constitutes the exercise of
 *   the sacrificial commandments themselves — grounded in Hosea 14:3 ('we
 *   shall render the bulls of our lips') and codified in talmudic dicta that
 *   study of the offerings is accounted as if they were brought. This story
 *   authors ONE reading of the sacrifice_commandment kernel: the
 *   study_as_performance reading, under which intellectual engagement
 *   fulfills the divine obligation now, in the absence of any altar. The
 *   arrangement is deliberately authored as near-pure coordination: the
 *   scholar-worshipper bears the labor of study and receives the discharge
 *   and the intrinsic good in the same act; no victim set exists; no seat
 *   captures the gains. KEY AGENTS (by structural relationship): -
 *   scholar_worshippers: Primary participant-beneficiary
 *   (moderate/identity_locked) — performs the commandment through study,
 *   receives discharge and intrinsic value - rabbinic_leadership:
 *   Agenda-setter (institutional/identity_locked) — administers the teaching
 *   and adjudicates fulfillment - rabbinic_academies: Secondary beneficiary
 *   (organized/constrained) — carry the curricula, accrue continuity -
 *   post_temple_communities: Collective beneficiary (moderate/constrained) —
 *   retain covenantal practice without an altar -
 *   temple_restoration_movements: Excluded critic (organized/trapped) — deny
 *   study's sufficiency, unable to perform - academic_historians_of_religion:
 *   Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.08).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.12).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.08).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study-as-Performance Reading of the Sacrificial Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious_studies/halakhic_theory/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, 'd7835037-c6d2-498b-9f33-80fb782b14d7').
narrative_ontology:cs_kernel_codification('d7835037-c6d2-498b-9f33-80fb782b14d7', fixed_text).
narrative_ontology:cs_authority_grounding('d7835037-c6d2-498b-9f33-80fb782b14d7', lineage).
narrative_ontology:cs_interpretation_layer_present('d7835037-c6d2-498b-9f33-80fb782b14d7').
narrative_ontology:cs_reading_relation('d7835037-c6d2-498b-9f33-80fb782b14d7', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('d7835037-c6d2-498b-9f33-80fb782b14d7', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('d7835037-c6d2-498b-9f33-80fb782b14d7', foundational, study_of_laws_equals_offering).
narrative_ontology:cs_axiom_status(study_of_laws_equals_offering, holdable).
narrative_ontology:cs_axiom_grounding('d7835037-c6d2-498b-9f33-80fb782b14d7', study_of_laws_equals_offering, theological).
narrative_ontology:cs_axiom('d7835037-c6d2-498b-9f33-80fb782b14d7', secondary, no_suspension_of_divine_obligation).
narrative_ontology:cs_axiom_status(no_suspension_of_divine_obligation, holdable).
narrative_ontology:cs_axiom_grounding('d7835037-c6d2-498b-9f33-80fb782b14d7', no_suspension_of_divine_obligation, deontological).
narrative_ontology:cs_reference_frame('d7835037-c6d2-498b-9f33-80fb782b14d7', always_dischargeable_sacrificial_obligation).
narrative_ontology:cs_drift_state('d7835037-c6d2-498b-9f33-80fb782b14d7', contemporary_post_destruction_millennia, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d7835037-c6d2-498b-9f33-80fb782b14d7', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshippers).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, rabbinic_academies).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, post_temple_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_commandment__study_as_performance, scholar_worshippers).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, rabbinic_interpretive_authority).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, verbal_worship_equivalence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Devote daily study hours to the sacrificial codes — Leviticus, the talmudic tractates on offerings — and understand that engagement itself as the discharge of the sacrificial commandments. They receive the standing of having fulfilled the obligation together with the intrinsic value of the learning itself; the effort spent is the worship, not a fee for it. Leaving the practice would mean relinquishing a self built around lifelong study; the practice and the person are not readily separable.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshippers, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__study_as_performance, scholar_worshippers, payer).

% Teach, ordain, and answer what the sacrificial commandments require now that the altar is gone; maintain the teaching that study counts as offering, set curricula, and rule on edge cases such as whether the equivalence covers all offerings or only some. Their authority rests on an unbroken chain of transmission back to the sages who first articulated the equivalence, so the teaching and their office stand or fall together.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, rabbinic_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% House the curricula through which the equivalence is taught; enroll students, transmit the texts, and accrue continuity and standing from the practice persisting across centuries. They did not originate the arrangement, but their institutional life is bound to its continuation, and their canon gives them little room to repudiate it.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, rabbinic_academies, beneficiary,
    organized, generational, constrained, continental).

% Diaspora communities keeping covenantal life going without an altar. The equivalence lets every member stand in fulfillment rather than suspension: the commandments concerning offerings remain addressable through the study table instead of waiting on a restored cult. Membership is inherited, and stepping away carries family and communal costs.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, post_temple_communities, beneficiary,
    moderate, generational, constrained, global).

% Groups — from ancient sects to modern activist organizations — holding that only physical offering discharges the commandment and that study is at best preparation for restoration. They cannot offer sacrifices: no altar exists and the site is restricted, so they petition, agitate, and study the rites themselves while denying that such study fulfills anything. Their critique finds little traction inside the academies' frame, and their own predicament traps them in the very activity they disparage.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, temple_restoration_movements, excluded,
    organized, civilizational, trapped, regional).

% Trace how the equivalence emerged after the destruction of 70 CE, compare it with substitution doctrines in other traditions, and document its institutional carriers. They take no position on its validity and observe the whole structure from outside.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, academic_historians_of_religion, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__study_as_performance, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of maintaining continuous covenantal obligation-performance when the sacrificial apparatus is unavailable: it gives every worshipper an always-accessible mode of addressing the offering commandments, and keeps the sacrificial legal corpus alive as practiced devotion rather than dormant statute.
% TRANSFER_FUNCTION: Moves study-labor — time and attention of scholars — into the sacrificial textual tradition, and returns to those same scholars the standing of a discharged obligation plus the intrinsic goods of learning; secondarily it sustains institutional continuity and curricular purpose for the academies that teach the equivalence.
% ABSENT_VOICES: Performance-only adherents — priestly lineages historically, temple-restoration activists today — would object that study discharges nothing and that calling it fulfillment consoles rather than complies. They exist and speak, but their objection is structurally muted inside this reading's frame, since the frame's core premise is precisely the falsity of theirs; the consensus that study suffices arises partly because the dissenting seat was never granted adjudicating standing.
% DISAPPEARANCE_RATIONALE: If the equivalence vanished overnight, post-Temple covenantal life would lose its central fulfillment mechanism: obligation-talk would flip to the suspended register (the performance_only world), academies would lose the organizing purpose of their sacrificial curricula, and communities would face a standing, unanswerable gap between binding text and impossible rite — a wholesale rearrangement of how the tradition relates to its own commandments.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE removed the entire sacrificial apparatus while the Torah's sacrificial commandments remained binding text; the arrangement was built to answer how the covenant's offering obligations could be honored without an altar.
% FOUNDING_PROBLEM_CORROBORATION: The founding event is attested by sources outside any benefiting party — Roman historical records and Josephus document the destruction — and the problem's continuing liveness is corroborated by the publicly verifiable absence of any functioning altar, and by performance-only adherents who, standing outside this reading's beneficiary set, attest the problem is unresolved by awaiting restoration.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).
:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits near the coordination floor (0.08 at interval end): the only cost the arrangement imposes is the study-labor itself, which the reading's own lights count as the benefit, and no seat collects a surplus from anyone else's participation. Suppression is low (0.12) and predominantly internalized-normative rather than structural — communal expectation that a serious Jew studies, and the identity cost of ceasing — with essentially no coercive machinery attached; nothing forbids alternative devotional modes (prayer, charity), which the tradition itself endorses alongside study. Theater is low (0.15): the activity is the function; learning demonstrably occurs and knowledge transmits, though a slow creep across nineteen centuries reflects ritualization at the margins of a very old practice. Accessibility_collapse is low (0.20) because the arrangement collapses no alternatives — it adds a fulfillment mode rather than closing others. Resistance (0.30) registers the live kernel-level contest: performance-only holders have disputed the equivalence since antiquity, though within the academies' operating environment the reading faces little organized opposition. The measurement series run on one shared time grid (70, 200, 500, 1000, 1500, 2026) with both tracked metrics authored at every point; trajectories are nearly flat with a mild medieval institutionalization bump in extractiveness and slow theater creep — no oscillation, no enforcement-capacity dynamics worth a suppression_requirement series, so none is authored.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the scholar-worshipper's position the arrangement is near-pure benefit: the act, the cost, and the reward coincide. From the temple_restoration_movements' position the same structure appears as managed deprivation — a consolation doctrine that relabels inability as fulfillment, with the suppression they experience coming from the frame's definitional exclusion of their premise rather than from any coercive apparatus. The academies experience institutional sustenance; the historian sees an inert, describable structure. The engine computes these divergent per-seat classifications from power, exit, and role data; the authored rope claim describes the arrangement as a whole and does not adjudicate the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party derives a low directionality from the beneficiary declarations: scholar_worshippers, rabbinic_academies, and post_temple_communities all sit near the beneficiary end, so effective extraction lands at or below the identity_coordination floor. There is no victim set — the restoration movements are excluded critics, not extraction targets; their grievance is discursive (their premise is ruled out of the frame), not a flow of goods taken from them, so no directionality override is warranted. Global spatial scope nominally amplifies effective extraction through verification difficulty, but with epsilon at the floor the amplification has nothing to amplify.
 *
 * MANDATROPHY ANALYSIS:
 *   The principal mislabeling risk for this arrangement is age-based: a nineteen-century-old substitutive practice invites a piton reading (old, seemingly vestigial, theatrically maintained). The classification resists that error because the founding problem remains live — no altar exists, so the fulfillment mechanism the arrangement provides is still doing its original work, not performing the memory of work. Symmetrically, the analysis prevents the opposite error: because the restoration movements experience the arrangement as deprivation-management, a snare-flavored reading from their seat is computable; but the structural data (no victim set, no captured gains, no suppressed exits) do not support it beyond the excluded-critic dynamic already recorded. Mandatrophy is not resolved: the mandate has not outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (study_as_performance) of the sacrifice_commandment kernel; which structural facts of the arrangement change under the sibling readings?',
    'Comparative adoption analysis across communities and eras: which reading governs obligation-talk in each community, and what each community''s practice implies about fulfillment status.',
    'Under performance_only, the arrangement loses its fulfillment function entirely — study becomes preparation, the obligation stands suspended, and the beneficiary structure empties into waiting. Under archive_maintenance, study''s value becomes instrumental to a future restoration rather than intrinsically worshipful, shifting the arrangement toward transitional scaffolding. Classification is reading-indexed; this file''s verdict holds only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading-indexed classification of the sacrificial-commandment kernel; siblings instantiate different constraints.').

omega_variable(
    equivalence_sincerity,
    'Is the ''as if offered'' equivalence lived by practitioners as genuine fulfillment, or carried as consolation for a lost rite?',
    'Analysis of devotional literature, sermonic corpora, and practitioner testimony across eras for the phenomenology of study-as-offering: fulfillment language versus compensatory language.',
    'If consolation dominates, the theater_ratio understates symbolic substitution and the arrangement drifts toward maintained performance of a loss despite intact function; if fulfillment language dominates, the low theater reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equivalence_sincerity, empirical, 'Whether the equivalence is experienced as worship or as consolation.').

omega_variable(
    curricular_steering,
    'Does the equivalence direct scholarly labor toward sacrificial texts more than unconstrained preference would, and do the academies accrue disproportionate benefit from that steering?',
    'Curriculum-share analysis against counterfactual interest distributions, plus examination of enrollment and endowment flows around sacrificial-law tracks in yeshiva curricula.',
    'Material steering with institutional capture would introduce a mild asymmetry pushing effective extraction above the coordination floor and complicate the pure-coordination picture; negligible steering leaves the arrangement at the floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curricular_steering, empirical, 'Whether the arrangement steers scholarly labor for institutional benefit beyond intrinsic devotional demand.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_commandment__study_as_performance, theater_ratio, 70, 0.04).
narrative_ontology:measurement(sacr_tr_t200, sacrifice_commandment__study_as_performance, theater_ratio, 200, 0.07).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__study_as_performance, theater_ratio, 500, 0.09).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__study_as_performance, theater_ratio, 1000, 0.11).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__study_as_performance, theater_ratio, 1500, 0.13).
narrative_ontology:measurement(sacr_tr_t2026, sacrifice_commandment__study_as_performance, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_commandment__study_as_performance, base_extractiveness, 70, 0.05).
narrative_ontology:measurement(sacr_be_t200, sacrifice_commandment__study_as_performance, base_extractiveness, 200, 0.07).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__study_as_performance, base_extractiveness, 500, 0.09).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__study_as_performance, base_extractiveness, 1000, 0.09).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__study_as_performance, base_extractiveness, 1500, 0.1).
narrative_ontology:measurement(sacr_be_t2026, sacrifice_commandment__study_as_performance, base_extractiveness, 2026, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the sacrifice_commandment kernel per the epsilon-invariance principle: the colloquial label 'the sacrificial commandments after the Temple' conflates three structurally distinct arrangements. study_as_performance (this file) authors a near-zero-extraction fulfillment mechanism with a beneficiary-only structure; performance_only authors a suspended-obligation arrangement whose structure is defined by an unfulfillable requirement; archive_maintenance authors an instrumental-preparation arrangement oriented to a future restoration. Each carries its own epsilon, beneficiary/victim structure, and classification. The upstream claim common to the family — that the sacrificial legislation remains covenantally binding text after 70 CE — is what makes all three readings live; this reading's network edges link it to its siblings for contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
