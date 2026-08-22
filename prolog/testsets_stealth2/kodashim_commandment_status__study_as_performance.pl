% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Study-as-Performance Fulfillment of the Sacrificial Commandments
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   The arrangement under contest is the halakhic equation that studying the
 *   laws of sacrifices (seder kodashim) fulfills the sacrificial commandments
 *   themselves: the kernel — the commandment's binding force — remains
 *   occupied through intellectual engagement rather than altar service. The
 *   equation is rooted in Hosea 14:3 ('we will render for bulls the offering
 *   of our lips') and codified in talmudic discussion (Menahot 110a: one who
 *   engages in the study of the burnt-offering is as though he offered it).
 *   This story instantiates ONE reading of the kodashim_commandment_status
 *   kernel. Its sibling readings are separate constraints with their own
 *   epsilon values and victim structures: performance_only (the commandment
 *   is contingent on Temple existence and stands as a suspended husk —
 *   nothing currently in force to extract through) and messianic_deferral
 *   (the commandment is temporally suspended but not obsolete; study
 *   maintains readiness — low extraction, but study's function is
 *   preparatory, not discharging). This reading's extractiveness sits at the
 *   coordination floor because the performance gap that would extract under a
 *   suspended-commandment frame is closed here: study IS the performance, the
 *   obligation is discharged daily, and no one bears an undischarged cost.
 *   The epsilon referent is the standing arrangement itself — the
 *   study-fulfillment equation as it operates — assessed by this reading's
 *   own lights; the sibling readings' endorsed arrangements are other files,
 *   not this referent. The expected structural delta (zero extractiveness
 *   from the performance gap; empty victim set) is authored in the metrics
 *   and structure below. The claimed type and the metrics are independent
 *   authored facts.
 *
 * KEY AGENTS:
 *   - torah_study_community: Primary beneficiary (moderate/identity_locked) — discharges the commandments through study; the fulfillment flows to the studiers themselves
 *   - covenantal_community: Beneficiary (organized/constrained) — holds the covenant's obligations complete across the altar's absence
 *   - halakhic_authorities: Agenda-setter and beneficiary (institutional/constrained) — administer the equation's application and exercise their interpretive role through it
 *   - rabbinic_academies: Beneficiary (institutional/constrained) — embody the curriculum through which the equation operates
 *   - kohen_priestly_families: Excluded voice (moderate/identity_locked) — hereditary altar vocation left unoccupied by the study-based fulfillment
 *   - non_scholarly_lay_jews: Excluded voice (powerless/constrained) — fulfillment path gated by access to the scholarly tradition
 *   - academic_historians_of_halakha: Analytical observer (analytical/analytical) — traces the equation's sources and career without holding a reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.08).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.1).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.08).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Study-as-Performance Fulfillment of the Sacrificial Commandments").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic/commitment-system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '64ebd8a9-e69f-447a-ab1c-10dd902ce4f5').
narrative_ontology:cs_kernel_codification('64ebd8a9-e69f-447a-ab1c-10dd902ce4f5', fixed_text).
narrative_ontology:cs_authority_grounding('64ebd8a9-e69f-447a-ab1c-10dd902ce4f5', lineage).
narrative_ontology:cs_interpretation_layer_present('64ebd8a9-e69f-447a-ab1c-10dd902ce4f5').
narrative_ontology:cs_reading_relation('64ebd8a9-e69f-447a-ab1c-10dd902ce4f5', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_reading_relation('64ebd8a9-e69f-447a-ab1c-10dd902ce4f5', kodashim_commandment_status__messianic_deferral, forecloses).
narrative_ontology:cs_axiom('64ebd8a9-e69f-447a-ab1c-10dd902ce4f5', foundational, study_discharges_sacrificial_obligation).
narrative_ontology:cs_axiom_status(study_discharges_sacrificial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('64ebd8a9-e69f-447a-ab1c-10dd902ce4f5', study_discharges_sacrificial_obligation, theological).
narrative_ontology:cs_axiom('64ebd8a9-e69f-447a-ab1c-10dd902ce4f5', secondary, commandment_force_unsuspended).
narrative_ontology:cs_axiom_status(commandment_force_unsuspended, holdable).
narrative_ontology:cs_axiom_grounding('64ebd8a9-e69f-447a-ab1c-10dd902ce4f5', commandment_force_unsuspended, theological).
narrative_ontology:cs_reference_frame('64ebd8a9-e69f-447a-ab1c-10dd902ce4f5', study_sustained_commandment_force).
narrative_ontology:cs_drift_state('64ebd8a9-e69f-447a-ab1c-10dd902ce4f5', contemporary_mass_study_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('64ebd8a9-e69f-447a-ab1c-10dd902ce4f5', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, torah_study_community).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, covenantal_community).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, rabbinic_academies).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, halakhic_authorities).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, torah_study_sacrificial_efficacy).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, covenantal_continuity_through_engagement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engages the talmudic orders of sacrifices in academies, study partnerships, and daily learning cycles; under the arrangement that engagement is itself the commandment's performance, so each session of study discharges the obligation it addresses. The practice is constitutive of scholarly identity — leaving it would mean abandoning a path of fulfillment they hold fully valid, not escaping a burden.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, torah_study_community, beneficiary,
    moderate, generational, identity_locked, global).

% The trans-generational community within which the commandments bind. The equation lets it hold that no divine obligation stands undischarged despite the altar's absence, keeping the covenant's ledger complete across centuries of dispersion. Its members collectively sustain the study institutions that make the fulfillment possible; the alternative — declaring a binding commandment permanently unfulfillable — is not a position the community's self-understanding admits.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, covenantal_community, beneficiary,
    organized, civilizational, constrained, global).

% Institutions that organize the sacrificial-orders curriculum, train its teachers, and publish the commentaries through which the equation operates. Their vocation and institutional purpose are channeled into the corpus whose study counts as sacrifice; the arrangement is the frame within which their work has its meaning.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, rabbinic_academies, beneficiary,
    institutional, generational, constrained, global).

% Decisors and tradition-bearers who rule on the equation's application: which laws must be studied, at what depth, and how the fulfillment holds if performance conditions return. They administer the arrangement by interpreting its textual sources from within it — their own study also falls under the equation — rather than standing outside it.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__study_as_performance, halakhic_authorities, beneficiary).

% Hereditary bearers of the altar service the commandments originally address. Under the equation, fulfillment of those commandments passes to any student of the texts — a domain in which priestly descent confers no role. They stand outside the academies where the equation is maintained; the service they would render is not currently required of anyone, so their vocation is left unoccupied rather than taken from them.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, kohen_priestly_families, excluded,
    moderate, generational, identity_locked, global).

% Jews without practical access to the textual tradition — for reasons of education, language, or the gendered structure of advanced study in traditional frameworks — for whom the equation's fulfillment path is hard to enter. No obligation of theirs goes undischarged by this fact, but the arrangement ties fulfillment to scholarly engagement and they are not present where that tie is set and maintained.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, non_scholarly_lay_jews, excluded,
    powerless, biographical, constrained, global).

% Scholars of rabbinic literature and religious history who trace the equation's sources and its career across communities and centuries. They describe the arrangement and its sibling readings without holding any of them, and their analyses are available to every seat.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, academic_historians_of_halakha, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__study_as_performance, diffuse).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__study_as_performance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of a commandment-centered community holding divine obligations whose performance conditions are absent: the equation converts fulfillment into a portable, universally available practice, keeping the sacrificial corpus in active communal memory and the commandment's force discharged across dispersion and across generations without altar infrastructure.
% TRANSFER_FUNCTION: Moves nothing material between seats. Each participant's own study time and intellectual effort are converted into discharged-obligation status for that participant; the community's collective attention is directed into the sacrificial corpus; fulfillment is relocated from the altar to the study hall. No goods, money, or labor flow from any seat to another.
% ABSENT_VOICES: The priestly families whose hereditary altar service the commandments originally address — their vocation is left unoccupied by the study-based fulfillment and they are not seated in the academies where the equation is maintained. Also Jews without practical access to advanced textual study, for whom the fulfillment path is gated by education, language, and in traditional frameworks the gendered structure of learning. Both would qualify the arrangement's universality; neither is harmed under the reading's own lights, which is why they are excluded voices rather than victims.
% DISAPPEARANCE_RATIONALE: If the equation vanished overnight — if study no longer discharged the sacrificial commandments — the commandment's status would flip from fulfilled to suspended: the kodashim curriculum would lose its performative force and become historical or preparatory material, the studiers' daily practice would discharge nothing, and the community's claim that its obligations stand complete would collapse into one of the sibling readings. The arrangement's participants would immediately rearrange their practice around performance_only or messianic_deferral.
% FOUNDING_PROBLEM: After the Temple's destruction (and for communities always distant from the altar), Jews remained bound to sacrificial commandments they could not perform — an obligation with no discharge path, threatening the covenant's completeness. The equation, drawn from Hosea 14:3 and codified in Menahot 110a, was built to solve exactly that: to give the unperformable commandment a real fulfillment path.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside this reading's beneficiary set: the sibling readings attest the founding problem while contesting the remedy (both performance_only and messianic_deferral agree the performance conditions are absent); the historical record of the Temple's destruction and the liturgy's standing petitions for its restoration attest the absence independently; academic historians of rabbinic literature document the equation's emergence as a response to unperformable commandments. The problem's liveness is not attested by the benefiting parties alone.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).
:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.08 — the attachment_coordination floor — because under this reading the performance gap extracts nothing: the obligation is discharged by the very act of study, so the only cost the arrangement imposes is the real time and effort of study itself, which is inherent coordination cost rather than extractive overhead. Suppression is 0.10, authored as a raw structural property (the engine scales only extractiveness, by directionality and scope): the obligation's normative pull exists, but discharge is self-executed, no enforcement machinery compels study, and the alternative readings remain live rather than suppressed. Theater_ratio is 0.12: study is the arrangement's real function under this reading, not a proxy for it; the small performative share is liturgical recitation of sacrifice passages undertaken without engagement, which crept up slightly as the practice scaled institutionally and then held flat. Accessibility_collapse is 0.30 — workable alternatives persist (not studying, or holding a sibling reading), so alternatives are contested rather than collapsed. Resistance is 0.15: within communities holding the equation it is near-uncontested (Menahot 110a is cited routinely); the contest lives between readings, not inside this one. The claimed type is rope: a genuine collective-action problem (an unperformable commandment would leave the covenant's obligations standing undischarged) solved with minimal coercive overhead, participants as net beneficiaries, and no suppressed alternatives. The measurement series run on one shared decade grid (a 1965-2025 mapping of mass institutionalized kodashim study) and are deliberately flat: six decades show no extraction accumulation and no Goodhart drift. gain_flow is 'diffuse' as an affirmative checked claim: the arrangement's gains — discharged obligation and covenantal continuity — accrue to the studiers themselves and the community collectively; no named seat captures gains from the others. fixing_cost is 'prohibitive': the authorities who could in principle re-rule face a removal whose cost — the commandment's only current fulfillment path collapses — exceeds any benefit, because nothing in the arrangement is malfunctioning; the prohibitive cell here reflects functional indispensability, not institutional inertia, and is consistent with a rope whose beneficiaries actively maintain it because it works.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is mild here because extraction sits at the floor, but it is real. From the studier's seat and the academy's seat, the arrangement is pure fulfillment: what they voluntarily do is what the commandment asks. From the excluded seats the same equation reads differently: the priestly families see a fulfillment path that bypasses their hereditary vocation entirely, and study-inaccessible lay Jews see fulfillment tied to scholarship they cannot reach. Neither excluded seat is harmed under the reading's own lights — no obligation goes undischarged that study could discharge, and the priestly service is not currently required of anyone — which is why the victim set is empty and the divergence is a difference in standing, not a difference in extracted cost. The engine computes per-seat classifications from the power and exit atoms; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared beneficiary derives a directionality near the beneficiary end: the studiers receive discharged-obligation status for effort they undertake willingly; the covenantal community receives continuity; the academies receive vocation and curriculum purpose; the halakhic authorities exercise their interpretive role through the arrangement and also study under it. With an empty victim set there is no high-directionality seat, so effective extraction stays near zero for every party even after scope amplification (the arrangement is global and diaspora-wide, but amplification from a floor-level base remains negligible). The excluded stakeholders are neither beneficiaries nor victims and do not feed the derivation — they mark standing asymmetries that the study_access_asymmetry omega tracks. Suppression is authored as a raw, unscaled structural property; only extractiveness is engine-scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification guards against two mislabelings. A critic outside the reading would call study-as-substitute theater — sacrifice performed in the mind — and the arrangement an inertial relic: ritual maintained after its function died. The classification resists both because the function has not died under this reading's own lights: the commandment's force is maintained by the study, which is the performance, so the theater ratio stays near the floor and the maintenance is functional, not theatrical. Mandatrophy is not resolved because the founding problem — commandments whose performance conditions are absent — is live, not dead: the Temple has not been rebuilt, and the equation continues to do the work it was built for. The genuine obsolescence horizon belongs to the sibling readings' territory: if performance conditions returned, this equation's dominance would sunset into auxiliary status — a transition this constraint does not itself carry and which would be a different story (a successor with a declared sunset), not a reclassification of this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_current_status_contestation,
    'Is the sacrificial commandment''s force currently discharged through study (this reading) or temporally suspended pending performance conditions (the sibling readings of kodashim_commandment_status)?',
    'Halakhic-theoretical adjudication among the readings; the framework''s sibling stories carry the alternatives. The disagreement is located in a single structural element — the commandment''s current force status — and cannot be resolved by data internal to this story.',
    'Adopting a sibling reading changes the structure materially: performance_only renders the commandment a suspended husk with no fulfillment path in force; messianic_deferral converts study''s function from discharge to readiness-maintenance, making the kernel''s occupation instrumental rather than constitutive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_current_status_contestation, conceptual, 'This constraint is the study_as_performance reading of the kodashim_commandment_status kernel; the sibling readings contest the commandment''s current force status.').

omega_variable(
    study_access_asymmetry,
    'Does the equation''s fulfillment path — intellectual engagement — distribute access evenly enough that the empty-victim-set claim holds, or does differential access to advanced study (education, language, and in traditional frameworks the gendered structure of learning) create an excluded class whose standing the reading does not register?',
    'Empirical study of who actually studies kodashim across communities and whether non-studiers experience undischarged obligation; survey of curricular access and its gates.',
    'If access asymmetry is load-bearing, the empty-victim-set claim weakens, extractiveness rises above the coordination floor, and the arrangement drifts toward a hybrid profile — coordinated studiers, effectively excluded non-studiers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_access_asymmetry, empirical, 'Whether the study-based fulfillment path is universally accessible enough to sustain the empty victim set.').

omega_variable(
    fulfillment_force_equivalence,
    'Is study''s fulfillment force identical to the commandment''s performance (full discharge), or an ''as if'' equivalence graciously credited by the tradition (the Menahot 110a comparison) — and does the boundary between those two claims sit inside this reading or mark where it ends?',
    'Conceptual analysis of the talmudic sources and their reception history; the line between ''study is the fulfillment'' and ''study counts as the fulfillment'' marks the border with the deferral sibling.',
    'If the equivalence is merely credited rather than constitutive, the kernel''s occupation is attenuated and this reading converges toward messianic_deferral; the full-force claim is this reading''s distinguishing axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fulfillment_force_equivalence, conceptual, 'Whether study constitutes the commandment''s fulfillment or is credited as equivalent to it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t10, kodashim_commandment_status__study_as_performance, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(koda_tr_t10, observed).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__study_as_performance, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(koda_tr_t20, observed).
narrative_ontology:measurement(koda_tr_t30, kodashim_commandment_status__study_as_performance, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(koda_tr_t30, observed).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__study_as_performance, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(koda_tr_t40, observed).
narrative_ontology:measurement(koda_tr_t50, kodashim_commandment_status__study_as_performance, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(koda_tr_t50, observed).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__study_as_performance, theater_ratio, 60, 0.12).
narrative_ontology:measurement_basis(koda_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t10, kodashim_commandment_status__study_as_performance, base_extractiveness, 10, 0.08).
narrative_ontology:measurement_basis(koda_be_t10, observed).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__study_as_performance, base_extractiveness, 20, 0.08).
narrative_ontology:measurement_basis(koda_be_t20, observed).
narrative_ontology:measurement(koda_be_t30, kodashim_commandment_status__study_as_performance, base_extractiveness, 30, 0.08).
narrative_ontology:measurement_basis(koda_be_t30, observed).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__study_as_performance, base_extractiveness, 40, 0.08).
narrative_ontology:measurement_basis(koda_be_t40, observed).
narrative_ontology:measurement(koda_be_t50, kodashim_commandment_status__study_as_performance, base_extractiveness, 50, 0.08).
narrative_ontology:measurement_basis(koda_be_t50, observed).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__study_as_performance, base_extractiveness, 60, 0.08).
narrative_ontology:measurement_basis(koda_be_t60, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, attachment_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sacrificial commandments after the destruction of the Temple' decomposes into three structurally distinct constraints — readings of the kodashim_commandment_status kernel — because the readings assign different current force to the commandment and therefore different epsilon values and victim sets. This story is the study_as_performance reading (extractiveness at the coordination floor; empty victim set; study discharges the obligation). Its siblings: kodashim_commandment_status__performance_only (the commandment is contingent on Temple existence and stands as a suspended husk — no fulfillment path in force) and kodashim_commandment_status__messianic_deferral (the commandment is temporally suspended; study maintains readiness for restoration — study is preparatory, not discharging). The readings are mutually exclusive on the current-status premise within any single halakhic framework, which is why this reading forecloses both; all three are linked here per the constraint-family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
