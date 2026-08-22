% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study-as-Performance Reading of the Sacrifice Commandment
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the sacrifice-commandment kernel:
 *   study_as_performance, the Talmudic teaching (Menachot 110a, invoking
 *   Hosea 14:3) that engaging with the laws of sacrifice IS the exercise of
 *   the commandment, so that intellectual engagement discharges the divine
 *   obligation in the absence of altar and Temple. Per Rule 1 the constraint
 *   is generated clean: the standing arrangement under contest is the
 *   obligation to engage sacrificial law as present-tense worship, and
 *   epsilon is authored for that arrangement by this reading's own lights,
 *   near-zero, because the participant receives the value of the practice
 *   directly and no party transfers anything away. The sibling readings
 *   (performance_only, archive_maintenance) are separate constraints in
 *   separate files, linked through the network section. The manifest's
 *   expected structural delta (zero extractiveness; beneficiary is the
 *   scholar-worshipper) is confirmed for the practitioner seat and refined in
 *   two directions: rabbinic academies collect institutional benefits as a
 *   second beneficiary, and God is seated as covenant counterparty per the
 *   tradition's own ontology. Two further seats, women denied textual
 *   education and laborers without study-leisure, document that the
 *   advertised universality is nominal; they are handled through an omega and
 *   a directionality override rather than by contaminating the reading's own
 *   epsilon. Claim and metrics are independent: the type is claimed as rope
 *   on structural grounds, and the metrics are authored descriptively.
 *
 * KEY AGENTS:
 *   - scholar_worshippers: practitioners who discharge the obligation through regular study of the sacrificial codes (moderate/mobile) — receive fulfillment and merit directly; nothing flows out
 *   - rabbinic_academies: yeshivot and kollelim administering curricula, certification, and publication (institutional/constrained) — collect tuition, endowments, and prestige; keep the reading operative
 *   - god_covenant_counterparty: the covenant's other party within the tradition's ontology (powerful/universal/bound by oath) — receives devotion and fidelity, nothing material
 *   - women_in_traditional_communities: exempted from time-bound study and historically denied advanced education (powerless/trapped) — would object that universal accessibility is nominal
 *   - poor_laboring_jews: subsistence workers without study-leisure across the centuries (powerless/constrained) — would object that the open door presumed leisure
 *   - comparative_religion_analysts: academic observers of the reading's emergence and function (analytical) — see the kernel, the sibling readings, and the institutional carriers from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.12).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.08).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.12).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study-as-Performance Reading of the Sacrifice Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, '45d3d0ab-03cc-4d42-8b64-3837a9318482').
narrative_ontology:cs_kernel_codification('45d3d0ab-03cc-4d42-8b64-3837a9318482', fixed_text).
narrative_ontology:cs_authority_grounding('45d3d0ab-03cc-4d42-8b64-3837a9318482', lineage).
narrative_ontology:cs_interpretation_layer_present('45d3d0ab-03cc-4d42-8b64-3837a9318482').
narrative_ontology:cs_reading_relation('45d3d0ab-03cc-4d42-8b64-3837a9318482', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('45d3d0ab-03cc-4d42-8b64-3837a9318482', sacrifice_commandment__archive_maintenance, influences).
narrative_ontology:cs_axiom('45d3d0ab-03cc-4d42-8b64-3837a9318482', foundational, study_fulfills_sacrificial_obligation_directly).
narrative_ontology:cs_axiom_status(study_fulfills_sacrificial_obligation_directly, holdable).
narrative_ontology:cs_axiom_grounding('45d3d0ab-03cc-4d42-8b64-3837a9318482', study_fulfills_sacrificial_obligation_directly, theological).
narrative_ontology:cs_axiom('45d3d0ab-03cc-4d42-8b64-3837a9318482', foundational, textual_engagement_is_present_worship_not_preparation).
narrative_ontology:cs_axiom_status(textual_engagement_is_present_worship_not_preparation, holdable).
narrative_ontology:cs_axiom_grounding('45d3d0ab-03cc-4d42-8b64-3837a9318482', textual_engagement_is_present_worship_not_preparation, deontological).
narrative_ontology:cs_reference_frame('45d3d0ab-03cc-4d42-8b64-3837a9318482', sacrificial_service_transposed_to_study).
narrative_ontology:cs_drift_state('45d3d0ab-03cc-4d42-8b64-3837a9318482', contemporary_post_reconstruction_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('45d3d0ab-03cc-4d42-8b64-3837a9318482', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshippers).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, rabbinic_academies).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, god_covenant_counterparty).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, talmudic_equivalence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Devote themselves to regular study of the sacrificial codes, the Talmudic orders of Zevachim and Menachot and the Temple-service chapters of Maimonides, as an act of worship in its own right. What flows to them: discharge of what they understand as a standing divine obligation, merit they believe accrues before God, and the intellectual and spiritual satisfaction of the texts themselves. Nothing material flows out; the time invested returns to them as the worship itself. Exit looks like simply stopping: no tribunal penalizes cessation, though teachers and peers would notice and grieve the lapse.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshippers, beneficiary,
    moderate, biographical, mobile, global).

% Yeshivot and kollelim organize their curricula around these tractates, train and ordain teachers, publish commentaries, and certify mastery. They collect tuition, endowments, state support in Israel, and the prestige that attaches to institutions guarding the practice; they administer the schedule, standards, and canon of study that keep the reading operative from generation to generation. Exit would mean dismantling their core curriculum and surrendering their chartering purpose.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, rabbinic_academies, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__study_as_performance, rabbinic_academies, agenda_setter).

% Within the tradition's own account, the other party to the covenant, who receives the words of study as He once received offerings, having declared acceptance of the fruit of lips in place of bulls. Nothing material reaches this seat; what flows is devotion, attention, and fidelity. The tradition holds this seat bound to Israel by oath and incapable of withdrawing from the relationship.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, god_covenant_counterparty, beneficiary,
    powerful, civilizational, trapped, universal).

% In most traditional communities they are exempted from time-bound positive commandments, a category that includes scheduled study, and historically were denied advanced textual education. The practice's advertised openness, that anyone can fulfill the obligation through learning, presupposes literacy, Hebrew competence, and hours of leisure that communal structures did not extend to them. From where they stand, leaving the community carries total social cost, and participation was gated by norms they did not set.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, women_in_traditional_communities, excluded,
    powerless, biographical, trapped, global).

% Subsistence workers across the centuries, artisans, peddlers, day laborers, lacked the leisure that sustained study assumes. Formal accessibility never translated into practical accessibility; the great study houses were staffed by men supported by merchants, dowries, or communal stipends. They would object that a worship described as open to all was in practice open to the leisured. Exit meant abandoning observant communal life entirely, a cost few could pay.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, poor_laboring_jews, excluded,
    powerless, immediate, constrained, global).

% Academic historians and scholars of religion study the reading's emergence after the destruction of 70 CE, its function in preserving textual mastery and communal cohesion, and its rivalry with the readings that demand physical performance or treat study as archival preparation. They hold no stake in its truth; their seat sees the kernel, the competing readings, and the institutional carriers from outside the tradition.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, comparative_religion_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-Temple worship-continuity problem: converts a physically impossible sacrificial obligation into a formally universal intellectual practice, keeping the community's covenantal practice, liturgy, and calendar coherent without altar or priesthood, and keeping the sacrificial corpus of law a living discipline rather than a dead letter.
% TRANSFER_FUNCTION: Moves devotional attention, time, and intellectual labor from the individual worshipper into the sacred texts and, in the tradition's account, toward God; no money, goods, or services move between human parties; merit and fulfillment accrue to the one who studies.
% ABSENT_VOICES: Women excluded from advanced textual education and poor laborers without study-leisure would object that the universal-accessibility claim is nominal, since the practice concentrated among leisured males; they sit outside the academies where the reading is administered. Partisans of physical performance also object, but they are present in the dispute as a sibling reading, not absent from it.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, if study ceased to count as fulfillment, the daily obligation would revert to suspended debt, the sacrificial passages woven through the liturgy would lose their operative meaning, yeshiva curricula would reclassify the tractates as archival, and the community would face anew the crisis the reading dissolved: a covenant whose central worship is unperformable.
% FOUNDING_PROBLEM: After the Second Temple's destruction in 70 CE made sacrifice physically impossible, how could Israel continue to discharge the sacrificial commandments and sustain covenantal worship without altar, priesthood, or pilgrimage?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Josephus records the post-70 trauma over interrupted sacrifices; early Christian polemic such as Justin Martyr's Dialogue with Trypho acknowledges the ceased cult as a live Jewish grievance; modern academic historians of rabbinic Judaism independently document the worship crisis and the substitution strategies developed in response. Even communities skeptical of the equivalence equation continue reciting the sacrificial passages daily, attesting the felt persistence of the underlying condition.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.12, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is low (0.12) because the practice is self-consuming in the benign sense: the participant's time returns to them as the worship itself, and the small residual reflects the professionalization overlay (stipends, rank, prestige economies) and the leisure gradient documented in the omegas. Suppression is very low (0.08): there is no enforcement machinery, only normative expectation; nobody is compelled to study and nobody is punished for stopping. Theater is low (0.15): the overwhelming share of activity is the function itself, with a modest status-performance margin. Accessibility collapse is low (0.18): understanding this reading does not eliminate the alternatives, since both sibling readings remain fully live positions held by real parties. Resistance is low (0.12): occasional literalist and secular objections, no organized opposition. The measurement series run on one shared grid, interval 0-75 mapping roughly 1948-2023 CE (the post-Holocaust reconstruction of the yeshiva world through the contemporary era), with both metrics authored at every point; the gentle rise in both series tracks credentialization and the growth of a status economy around study. Suppression_requirement is deliberately untracked: the enforcement picture is static (normative only, no capacity build-up or decay), so the scalar in base_properties carries the whole story per the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   There is no payer seat in this reading, so the classical payer-versus-agenda-setter divergence does not arise. The divergences that do arise: the practitioner seat experiences the arrangement as pure gift (obligation discharged by an act that is itself the reward), the excluded seats experience the same arrangement as a gate (a worship nominally open to all, practically open to the leisured and literate), and the analyst seat sees the equivalence as an interpretive construction solving a historical crisis rather than a metaphysical fact. The tradition's internal view and the external view disagree about what kind of thing the equivalence is; the engine computes per-seat classifications from the structural data, and the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Scholar_worshippers are declared beneficiaries with mobile exit: derivation places them near the beneficiary end, so effective extraction computes near zero or negative, matching the manifest's expected zero-extraction delta. Rabbinic_academies are beneficiaries with constrained exit and a genuine dual position: they collect institutional resources and simultaneously administer the arrangement, giving them low but nonzero effective extraction. God is declared a beneficiary within the tradition's own ontology and likewise derives near the beneficiary end; the analytical observer seat brackets the question of that seat's agency while honoring the story's internal structure. The two excluded seats carry no beneficiary or victim declarations, so structural derivation has no data for them and would fall through to a canonical fallback keyed on the powerless atom, which would misread exclusion as targeting; a directionality override sets the powerless atom to 0.6, encoding that these seats neither receive the practice's subsidy nor pay it a transfer, but bear foregone benefit and second-class standing. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Snare and tangled_rope are excluded because there is no victim set and no enforcement machinery: nobody pays through this structure. Mountain is excluded because the reading is a constructed interpretive position, not a natural feature: emerges_naturally is false, and the equivalence was adopted historically to solve a concrete crisis. Scaffold is excluded because there is no sunset clause and the founding problem is live: the arrangement presents itself as steady-state worship, not transitional support. Piton is excluded because the function has not atrophied: the theater ratio is low and the practice still does what it claims. Rope captures the structure: a genuine coordination function (worship continuity for a community whose central rite became impossible), participants who are net beneficiaries, minimal coercive overhead, and alternatives that remain unsuppressed. The principal drift vector to watch is instrumentalization: if the status economy around study grows faster than the devotional substance, theater_ratio climbs and the arrangement decays toward performance without function, which the temporal series is positioned to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading of the sacrifice-commandment kernel (reading: study_as_performance). What structural changes would adoption of a sibling reading produce?',
    'Compare against the sibling files: performance_only converts the obligation into suspended debt borne by the whole community until restoration, creating a deficit structure with a universal bearer; archive_maintenance strips present-tense worship value and reclassifies study as instrumental preparation for a future Temple, shifting the beneficiary logic from the worshipper to the future restoration project.',
    'Under performance_only the arrangement acquires a deficit structure resembling a universal burden with materially higher effective weight on every obligated party; under archive_maintenance extraction stays low but the coordination function becomes messianic preparation, changing the Boltzmann profile and demoting the practitioner from worshipper to custodian.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: which sibling reading would change what, and where the disagreement is located (the fulfillment-mode of the obligation).').

omega_variable(
    universality_leisure_gradient,
    'Is fulfillment of the study-obligation practically universal, or does it track literacy, Hebrew competence, and leisure, concentrating fulfillment among leisured males?',
    'Historical literacy and time-use evidence; participation demographics in traditional communities; comparison of the formally universal obligation against actual study hours across class and gender lines.',
    'If concentrated, the near-zero extraction assessment understates a distributive asymmetry: the obligation stands over everyone while fulfillment capacity is concentrated, raising the effective burden experienced by excluded seats above the authored baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_leisure_gradient, empirical, 'Whether the reading''s advertised universality is practically real or structurally gated.').

omega_variable(
    intrinsic_value_vs_status_economy,
    'Is study''s value to participants intrinsic worship, as the reading claims, or substantially instrumental: status, communal standing, stipends, institutional reproduction?',
    'Observe study behavior where worship-status and material incentives are detached: do learners sustain sacrificial-code study when it confers no rank or support? Compare secularized descendants of intensive study families and voluntary lay study circles outside credentialing tracks.',
    'If largely instrumental, theater_ratio rises and the arrangement drifts from worship toward credentialing, a live-coordination practice decaying toward performance without its function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_value_vs_status_economy, empirical, 'Intrinsic-devotional versus instrumental-status motivation mix among practitioners.').

omega_variable(
    equivalence_grounding_frame,
    'Does the study-equals-offering equivalence bind as metaphysical fact (God''s asserted acceptance of words in place of bulls) or operate only as conventional interpretive authority (binding because the tradition accepts its own authorization)?',
    'No empirical resolution; depends on adopted ontology. Framing under-determination: the obvious framing grounds the equivalence in divine declaration (Hosea 14:3 as read in Menachot 110a); the less obvious framing sees rabbinic authority constructing the equivalence to solve a post-destruction pastoral crisis, with the prooftext as ratification rather than source.',
    'If metaphysical, the obligation approaches fixity within the committed frame and behaves mountain-like for insiders; if conventional, its force is contingent on communal acceptance and revisable by authority, remaining rope-like throughout.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equivalence_grounding_frame, conceptual, 'Commitment-system framing under-determination: theological versus conventional grounding of the equivalence claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t15, sacrifice_commandment__study_as_performance, theater_ratio, 15, 0.07).
narrative_ontology:measurement_basis(sacr_tr_t15, observed).
narrative_ontology:measurement(sacr_tr_t30, sacrifice_commandment__study_as_performance, theater_ratio, 30, 0.09).
narrative_ontology:measurement_basis(sacr_tr_t30, observed).
narrative_ontology:measurement(sacr_tr_t45, sacrifice_commandment__study_as_performance, theater_ratio, 45, 0.11).
narrative_ontology:measurement_basis(sacr_tr_t45, observed).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_commandment__study_as_performance, theater_ratio, 60, 0.13).
narrative_ontology:measurement_basis(sacr_tr_t60, observed).
narrative_ontology:measurement(sacr_tr_t75, sacrifice_commandment__study_as_performance, theater_ratio, 75, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.06).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t15, sacrifice_commandment__study_as_performance, base_extractiveness, 15, 0.07).
narrative_ontology:measurement_basis(sacr_be_t15, observed).
narrative_ontology:measurement(sacr_be_t30, sacrifice_commandment__study_as_performance, base_extractiveness, 30, 0.08).
narrative_ontology:measurement_basis(sacr_be_t30, observed).
narrative_ontology:measurement(sacr_be_t45, sacrifice_commandment__study_as_performance, base_extractiveness, 45, 0.09).
narrative_ontology:measurement_basis(sacr_be_t45, observed).
narrative_ontology:measurement(sacr_be_t60, sacrifice_commandment__study_as_performance, base_extractiveness, 60, 0.1).
narrative_ontology:measurement_basis(sacr_be_t60, observed).
narrative_ontology:measurement(sacr_be_t75, sacrifice_commandment__study_as_performance, base_extractiveness, 75, 0.12).
narrative_ontology:measurement_basis(sacr_be_t75, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the sacrifice commandment' decomposes into three structurally distinct readings of one kernel, each with its own epsilon, beneficiary structure, and classification. This member (study_as_performance) is the low-extraction present-worship reading; performance_only generates a suspended-debt structure with a universal bearer; archive_maintenance generates an instrumental-preparation structure oriented to a future restoration. This reading influences archive_maintenance: the worship-status this reading conferred on sacrifice study is what funded the academies and curricula through which the technical knowledge was preserved, so the archival function rides on infrastructure this reading built. This reading coexists with performance_only: the two are held by different parties in a live dispute, and many authorities hold them sequentially (study now, performance upon restoration) without contradiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__study_as_performance, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
