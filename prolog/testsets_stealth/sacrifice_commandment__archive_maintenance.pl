% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrificial-Law Archive Maintenance Mandate
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   After the Temple's destruction, the sacrificial code became a body of law
 *   with no object: hundreds of pages of species, disqualifications, and
 *   procedure governing a service that cannot be performed. The
 *   archive_maintenance reading holds that the standing study mandate is
 *   justified as preservation — the technical knowledge must survive the
 *   interregnum so a future restoration-generation would not start from zero
 *   — and that present study therefore has preparatory, not worship, value.
 *   The arrangement is transitional by design: its entire warrant is the gap
 *   it bridges, and its terminal condition (restoration) is declared, which
 *   is why the scaffold claim carries an explicit sunset clause alongside
 *   active curricular enforcement. Receipt surface: the gains of the
 *   extracted study-effort accrue to the seat that holds the accumulating
 *   archive — the rabbinic academy network — which converts student labor
 *   into institutional continuity and a restoration-readiness claim; the
 *   designated ultimate consumer is a generation that cannot yet collect.
 *   Fixing cost is prohibitive from the agenda-setter's seat: reweighting the
 *   curriculum away from the archive would require the academies to dissolve
 *   commitments constitutive of their own authority, not merely adjust an
 *   allocation. This story is one member of a three-story constraint family
 *   decomposing the colloquial label 'observance of the sacrifice commandment
 *   without a Temple'; the decomposition follows the epsilon-invariance
 *   principle and is documented in network.dual_formulation_note. KEY AGENTS
 *   (by structural relationship): - future_temple_generation: Primary
 *   beneficiary (powerless/trapped) — designated inheritor of the archive;
 *   exercises no present agency - present_torah_students: Primary target
 *   (moderate/identity_locked) — bears the study burden now; exit means
 *   leaving the community - rabbinic_academy_network: Agenda setter and
 *   secondary beneficiary (institutional/constrained) — sets curriculum,
 *   administers the archive, draws continuity from it -
 *   lay_community_supporters: Secondary payer (organized/constrained) — funds
 *   the apparatus, receives identity goods - practical_restoration_activists:
 *   Excluded voice (organized/constrained) — demand present-tense
 *   preparation, barred from curriculum-setting -
 *   academic_historians_of_religion: Analytical observer
 *   (analytical/analytical) — documents transmission dynamics from outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.48).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.33).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.48).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.33).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, scaffold).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrificial-Law Archive Maintenance Mandate").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).
narrative_ontology:has_sunset_clause(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, 'cad6642f-8d1b-4d11-a76c-3be7c391aacf').
narrative_ontology:cs_kernel_codification('cad6642f-8d1b-4d11-a76c-3be7c391aacf', fixed_text).
narrative_ontology:cs_authority_grounding('cad6642f-8d1b-4d11-a76c-3be7c391aacf', lineage).
narrative_ontology:cs_interpretation_layer_present('cad6642f-8d1b-4d11-a76c-3be7c391aacf').
narrative_ontology:cs_reading_relation('cad6642f-8d1b-4d11-a76c-3be7c391aacf', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('cad6642f-8d1b-4d11-a76c-3be7c391aacf', sacrifice_commandment__performance_only, influences).
narrative_ontology:cs_axiom('cad6642f-8d1b-4d11-a76c-3be7c391aacf', foundational, study_preserves_restoration_competence).
narrative_ontology:cs_axiom_status(study_preserves_restoration_competence, holdable).
narrative_ontology:cs_axiom_grounding('cad6642f-8d1b-4d11-a76c-3be7c391aacf', study_preserves_restoration_competence, instrumental).
narrative_ontology:cs_axiom('cad6642f-8d1b-4d11-a76c-3be7c391aacf', foundational, present_study_lacks_fulfillment_status).
narrative_ontology:cs_axiom_status(present_study_lacks_fulfillment_status, holdable).
narrative_ontology:cs_axiom_grounding('cad6642f-8d1b-4d11-a76c-3be7c391aacf', present_study_lacks_fulfillment_status, deontological).
narrative_ontology:cs_reference_frame('cad6642f-8d1b-4d11-a76c-3be7c391aacf', standing_temple_service_order).
narrative_ontology:cs_drift_state('cad6642f-8d1b-4d11-a76c-3be7c391aacf', post_destruction_exilic_interval, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('cad6642f-8d1b-4d11-a76c-3be7c391aacf', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_temple_generation).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, rabbinic_academy_network).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_torah_students).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, lay_community_supporters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, lay_community_supporters).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, messianic_restoration_certainty).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, transmission_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A cohort that does not yet exist: the community that would restore the Temple service if the messianic restoration comes. Everything the study mandate produces is addressed to them — species lists, disqualification rules, altar procedure — transmitted forward so they would not start from zero. They exercise no present choice about receiving the inheritance and cannot decline it; whether they ever arrive to use it is outside anyone's present control.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_temple_generation, beneficiary,
    powerless, generational, trapped, global).

% Advanced students in academies where the sacrificial orders form a standing part of the curriculum. They spend years mastering material whose stated use lies beyond their own lifetimes, at the opportunity cost of other mastery. Leaving the study framework altogether would mean leaving the community and the self-concept that studying constitutes; remaining inside it, they cannot skip the sacrificial tractates without visible deviation from the expected course.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, present_torah_students, payer,
    moderate, biographical, identity_locked, global).

% The heads and faculties of the transmission academies. They set the curriculum that assigns sacrificial law its place, certify mastery, and decide how much institutional energy the archive receives. They justify the allocation by the restoration rationale and draw institutional continuity, a distinctive subject matter, and a claim on communal support from administering it. They could reweight the curriculum, though doing so against the restoration rationale would cost them standing inside the tradition they lead.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, rabbinic_academy_network, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, rabbinic_academy_network, beneficiary).

% Households and donors who fund the academies and send them their children. They bear the direct financial cost of the transmission apparatus and the indirect cost of the curriculum's shape. In return they receive communal identity, continuity, and the standing that support confers; few of them engage the sacrificial material directly themselves.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, lay_community_supporters, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, lay_community_supporters, beneficiary).

% Groups that want restoration readiness now: vessels fabricated, priests trained, site access asserted. They regard archive-only study as deferral dressed as fidelity and argue for hands-on preparation in the present tense. They sit outside the academies' curriculum-setting conversation; their proposals are treated as fringe by the mainstream transmission network, and they lack standing to alter what is taught.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, practical_restoration_activists, excluded,
    organized, biographical, constrained, regional).

% Scholars who document how ritual knowledge survives institutional rupture, comparing post-destruction Jewish transmission with other traditions that lost or kept procedural religious knowledge. They observe the system from outside its normative life, take no part in it, and publish analyses that neither the academies nor the activists are bound to heed.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, academic_historians_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__archive_maintenance, rabbinic_academy_network).
narrative_ontology:fixing_cost_class(sacrifice_commandment__archive_maintenance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves specialized sacrificial-law knowledge — species, disqualifications, ritual procedure — across generations without a functioning Temple, so that the technical competence a restoration would require survives the gap. Stated without evaluation of whether the restoration will occur.
% TRANSFER_FUNCTION: Moves present study-time, curricular capacity, and communal funding from current students and supporters into a maintained knowledge archive whose designated consumer is a future generation; institutional continuity accrues to the academies that hold the archive in the meantime.
% ABSENT_VOICES: Practical restoration activists would object that readiness should be built now rather than archived; secular members of the funding communities would object to the eschatological premise the budget rests on; and the designated beneficiary — the future generation — cannot speak at all, its interests voiced only by proxies (the academies) whose present interests are served by the representation. Commentary-grade only: these absences inform the consensus-provenance check, not classification.
% DISAPPEARANCE_RATIONALE: If the archival study mandate vanished overnight, curricula would restructure within a generation, sacrificial expertise would thin to a handful of specialists and then to texts without living transmitters, the restoration-readiness claim would lapse, and the academies would lose a distinctive pillar of identity and funding appeal. Rebuilding the competence later would take centuries — the world the arrangement organizes would visibly rearrange.
% FOUNDING_PROBLEM: The Temple's destruction severed practice from knowledge: a detailed service code faced total loss with no institution left to execute it. The arrangement was built to prevent that loss across an indefinite interregnum.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of religion corroborate both the founding rupture and the transmission response — the redaction of the sacrificial orders in the Mishnah and Talmud is documented independently of the benefiting parties, and comparative cases of communities losing ritual competence after discontinuing transmission establish that the preservation problem is real. Whether the problem remains LIVE at present cost, however, is attested only by the academies themselves; no source outside the beneficiary set confirms that the current scale of investment is warranted, and the activists dispute it from the opposite direction.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48): real, ongoing cost — years of advanced-study capacity — is borne by present students for a payoff contingent on an event outside anyone's control, but the offsetting function (genuine preservation of procedural knowledge) is real, which caps epsilon below snare territory. Suppression (0.33) is authored as a raw structural property and is deliberately NOT scaled by power or scope — the engine owns any scaling; it reflects curricular compulsion plus the identity cost of opting out, tempered by the fact that alternatives within the study world (other tractates, other disciplines) remain fully legitimate. Theater ratio (0.25): the core transmission work is functional, but a commemorative layer (celebrated tractate completions, published volumes displayed as achievement, symbolic seder elements) grows as living memory of practice recedes. Accessibility collapse (0.35): understanding the constraint does not eliminate alternatives — a student may master other domains — so collapse is partial. Resistance (0.20): little active resistance; occasional curricular-relevance debates, no organized opposition to the mandate itself. The claimed type is scaffold: the arrangement is authored as transitional support with a declared terminal condition, and because it is actively enforced (mandated curriculum, examinations, certification), the schema's sunset-clause requirement is satisfied honestly rather than nominally. The measurement series run on one shared time grid (points 0, 6, 12, 18, 24, 30 — approximately decades since the mid-twentieth-century systematization of Kodshim curricula); trajectories are gradual and monotonic, with no oscillation, so no cyclical machinery is invoked. Suppression_requirement is tracked because the story specifically traces enforcement-capacity change: as living memory of the Temple faded, the machinery sustaining the mandate (formalized curricula, standardized testing, celebratory completion rites) hardened, raising the active force needed to hold the arrangement in place.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very different types from identical structural data. From the present student's seat — identity_locked exit, full incidence of the time cost, benefit deferred beyond any reachable horizon — the arrangement reads as enforced deferral approaching pure extraction. From the academy seat, the same structure reads as stewardship: administration of a trust, with continuity flowing back. From the future generation's seat, it is pure subsidy — costless inheritance. The engine computes these per-seat classifications from the declared positions; the scaffold claim records the aggregate design intent and does not adjudicate the divergence. The divergence is the finding: a constraint that is a scaffold at the level of design intent can still operate as a snare at the seat where exit is locked and the sunset condition is unverifiable.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: future_temple_generation sits at the full-beneficiary end (d near 0.0 — the constraint subsidizes it entirely, at zero present cost); present_torah_students and lay_community_supporters sit at the target end (d near 1.0 — they bear the transfer with constrained or identity-locked exit). One override is declared: the derivation from the academy network's beneficiary listing would place the institutional seat near pure beneficiary (d roughly 0.05-0.1), but the academies also bear real administration costs and legitimacy exposure — they spend institutional capital maintaining the archive and would spend more defending it — netting their position closer to d = 0.2. The override targets the institutional power atom, which in this story only the academy network occupies. Scope amplification applies modestly at global scope, making verification of the preservation claim harder and effective extraction slightly higher for target seats than base epsilon suggests.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the mandate as pure extraction ignores the genuine coordination function — procedural knowledge really does decay without deliberate transmission, and comparative cases show communities losing ritual competence within generations of discontinuing practice. Reading it as pure rope ignores that the present payers cannot collect: the students who bear the cost are constitutionally barred from the benefit, which flows to a cohort represented only by proxies. The scaffold claim keeps the transition itself in view: the arrangement's warrant is the gap, so the decisive question is whether the sunset condition ever fires. The restoration_horizon_uncertainty omega tracks exactly this — if the beneficiary never materializes, the scaffold decays toward a piton profile (maintenance continuing past function, theater ratio climbing) without any single seat profiting enough to fix it, while the academy seat's continued collection would push it toward snare instead. The R5 mismatch check runs clean: founding_problem_status is contested (not dead) and disappearance_verdict is world_rearranges, so no zombie flag fires — the founding problem's liveness is precisely what the parties dispute, and the corpus should register that dispute rather than resolve it by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the archive_maintenance reading of the sacrifice_commandment kernel. Which reading governs the standing arrangement, and what would the sibling readings (study_as_performance, performance_only) change structurally?',
    'Adoption history within the interpreting community: curricular rationales issued by the academies, responsa on why sacrificial law is studied without a Temple, and which justification dominates official syllabi and public defense of the curriculum.',
    'If study_as_performance governed, the beneficiary relocates to the present practitioner, study becomes self-justifying worship, and epsilon falls sharply. If performance_only governed, study loses commandment-status entirely, the mandate''s normative force collapses into voluntary scholarship, and the enforcement machinery loses its warrant. Each sibling is a separate constraint file with its own epsilon and beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is one of three readings of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    restoration_horizon_uncertainty,
    'Does the designated beneficiary — the generation that restores the Temple — ever materialize within a horizon that rationalizes the present cost of maintenance?',
    'Only the restoration event itself, or the transmitting community''s abandonment of restoration-expectation, can resolve this; no near-term observational data bears on it.',
    'If the beneficiary never arrives, the arrangement''s benefit claim fails: extraction loses its offsetting function, the sunset condition never fires, and the classification drifts toward inertial or theatrical maintenance from the payer seats while the archive-keeping continues.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_horizon_uncertainty, empirical, 'Eschatological contingency of the constraint''s beneficiary.').

omega_variable(
    preparation_vs_fulfillment_boundary,
    'Is the line between preparation for worship and worship itself stable enough to sustain this reading''s core denial of present fulfillment-value in study?',
    'Analysis of how the tradition itself classifies borderline practices: study undertaken expressly to qualify teachers of priests, ceremonial reenactments, commemorative Passover-lamb rites — do these get counted as fulfillment or as preparation?',
    'If the boundary blurs in practice, this reading converges toward study_as_performance and measured extraction falls (study compensates its own cost). If the boundary holds sharp, the present generation''s cost stands formally uncompensated and epsilon holds at its authored level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preparation_vs_fulfillment_boundary, conceptual, 'Conceptual stability of the preparation/fulfillment distinction on which the reading''s epsilon depends.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t6, sacrifice_commandment__archive_maintenance, theater_ratio, 6, 0.16).
narrative_ontology:measurement_basis(sacr_tr_t6, observed).
narrative_ontology:measurement(sacr_tr_t12, sacrifice_commandment__archive_maintenance, theater_ratio, 12, 0.19).
narrative_ontology:measurement_basis(sacr_tr_t12, observed).
narrative_ontology:measurement(sacr_tr_t18, sacrifice_commandment__archive_maintenance, theater_ratio, 18, 0.21).
narrative_ontology:measurement_basis(sacr_tr_t18, observed).
narrative_ontology:measurement(sacr_tr_t24, sacrifice_commandment__archive_maintenance, theater_ratio, 24, 0.23).
narrative_ontology:measurement_basis(sacr_tr_t24, observed).
narrative_ontology:measurement(sacr_tr_t30, sacrifice_commandment__archive_maintenance, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(sacr_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t6, sacrifice_commandment__archive_maintenance, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(sacr_be_t6, observed).
narrative_ontology:measurement(sacr_be_t12, sacrifice_commandment__archive_maintenance, base_extractiveness, 12, 0.44).
narrative_ontology:measurement_basis(sacr_be_t12, observed).
narrative_ontology:measurement(sacr_be_t18, sacrifice_commandment__archive_maintenance, base_extractiveness, 18, 0.45).
narrative_ontology:measurement_basis(sacr_be_t18, observed).
narrative_ontology:measurement(sacr_be_t24, sacrifice_commandment__archive_maintenance, base_extractiveness, 24, 0.47).
narrative_ontology:measurement_basis(sacr_be_t24, observed).
narrative_ontology:measurement(sacr_be_t30, sacrifice_commandment__archive_maintenance, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(sacr_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t6, sacrifice_commandment__archive_maintenance, suppression_requirement, 6, 0.24).
narrative_ontology:measurement_basis(sacr_su_t6, observed).
narrative_ontology:measurement(sacr_su_t12, sacrifice_commandment__archive_maintenance, suppression_requirement, 12, 0.26).
narrative_ontology:measurement_basis(sacr_su_t12, observed).
narrative_ontology:measurement(sacr_su_t18, sacrifice_commandment__archive_maintenance, suppression_requirement, 18, 0.28).
narrative_ontology:measurement_basis(sacr_su_t18, observed).
narrative_ontology:measurement(sacr_su_t24, sacrifice_commandment__archive_maintenance, suppression_requirement, 24, 0.31).
narrative_ontology:measurement_basis(sacr_su_t24, observed).
narrative_ontology:measurement(sacr_su_t30, sacrifice_commandment__archive_maintenance, suppression_requirement, 30, 0.33).
narrative_ontology:measurement_basis(sacr_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, information_standard).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the kernel 'sacrifice_commandment'. The colloquial label 'how the sacrifice commandment operates without a Temple' conflates three structurally distinct claims that fail the epsilon-invariance test if merged: (1) archive_maintenance (this file) — study as knowledge preservation for restoration, moderate epsilon, beneficiary in the future; (2) study_as_performance — study as present fulfillment, low epsilon, beneficiary is the present practitioner; (3) performance_only — no fulfillment without execution, study carries no commandment-status, mandate collapses to voluntary scholarship. Each story carries its own epsilon, beneficiaries, and claimed type; they are linked here because the upstream readings are cited as warrant for the downstream arrangements. This reading sits between the siblings structurally: it accepts performance_only's premise that execution is what ultimately counts (hence the archive has a point) while rejecting its corollary that interim study is worthless, and it directly contradicts study_as_performance's premise that study already fulfills.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__archive_maintenance, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
