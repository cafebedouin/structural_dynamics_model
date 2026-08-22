% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Messianic Suspension of the Sacrificial Obligation with Study-Based Readiness
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This story instantiates the messianic_suspension reading of the
 *   sacrifice_obligation_continuity kernel. After the destruction of the
 *   Second Temple, the sacrificial commandments are held under this reading
 *   to be neither lapsed nor dischargeable through substitutes, but
 *   suspended: their operative force is paused pending messianic restoration,
 *   and sustained study of the sacrificial corpus serves as a maintenance
 *   protocol that keeps the system resumable. The epsilon referent is the
 *   standing suspension-and-study arrangement as this reading assesses it: a
 *   real but moderate readiness burden (daily recitation, curriculum time,
 *   the discipline of treating unperformable commandments as live
 *   commitments) borne without guilt mechanisms. There is no current victim
 *   set because nothing is taken that anyone is structurally denied the means
 *   to decline within the frame; the burden is distributed individual time
 *   and attention, not seized transfers. Constraint family: the colloquial
 *   question 'what happened to the sacrifice commandments?' decomposes into
 *   four structurally distinct claims about the obligation's present status,
 *   instantiated as four linked stories (this one plus archival_preservation,
 *   performance_only, study_as_performance); each carries its own epsilon,
 *   beneficiary structure, and type. Claim and metrics are authored
 *   independently: the scaffold claim rests on the arrangement's declared
 *   termination condition, while the metrics describe its observed operation.
 *
 * KEY AGENTS:
 *   - individual_observant_jews: primary burden-bearer (moderate/identity_locked) — carries the daily readiness levy of recitation and study; exit means leaving the covenantal community
 *   - rabbinic_scholarly_establishment: administrator and concentrated recipient (institutional/constrained) — sets curriculum and liturgical standard; the study mandate flows through its academies
 *   - observant_jewish_community: collective beneficiary (organized/identity_locked) — receives continuity, shared practice, and messianic orientation
 *   - reform_and_secular_jews: excluded voice (organized/mobile) — hold the lapsed reading; exited the framework rather than dissenting within it
 *   - animal_ethics_advocates: excluded voice (organized/mobile) — contest the restoration endpoint the sunset clause keeps permanently open
 *   - academic_historians_of_religion: analytical observer (analytical/analytical) — documents the arrangement's history without a normative seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.42).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.18).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.42).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Messianic Suspension of the Sacrificial Obligation with Study-Based Readiness").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious_law/ritual_studies/textual_tradition").

narrative_ontology:has_sunset_clause(sacrifice_obligation_continuity__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, 'f27dd3a9-2f53-4f1d-8a48-101c5e0dd304').
narrative_ontology:cs_kernel_codification('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304', fixed_text).
narrative_ontology:cs_authority_grounding('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304', lineage).
narrative_ontology:cs_interpretation_layer_present('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304').
narrative_ontology:cs_reading_relation('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_reading_relation('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_axiom('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304', foundational, sacrificial_obligation_binding_but_suspended).
narrative_ontology:cs_axiom_status(sacrificial_obligation_binding_but_suspended, holdable).
narrative_ontology:cs_axiom_grounding('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304', sacrificial_obligation_binding_but_suspended, deontological).
narrative_ontology:cs_axiom('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304', foundational, study_maintains_readiness_not_fulfillment).
narrative_ontology:cs_axiom_status(study_maintains_readiness_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304', study_maintains_readiness_not_fulfillment, instrumental).
narrative_ontology:cs_reference_frame('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304', covenantal_readiness_pending_restoration).
narrative_ontology:cs_drift_state('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304', contemporary_post_emancipation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f27dd3a9-2f53-4f1d-8a48-101c5e0dd304', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, observant_jewish_community).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholarly_establishment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, individual_observant_jews).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, individual_observant_jews).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, messianic_restoration_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, halakhic_corpus_completeness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recite the korbanot passages in the daily liturgy and allocate regular study time to the sacrificial corpus — Mishnah, Talmudic orders, halakhic codes. Bears the recurring readiness burden: time, attention, and the discipline of treating commandments that cannot currently be performed as live commitments. Receives in return the meaning, rhythm, and covenantal belonging the practice structures. Stepping away from the practice would mean stepping outside the community's shared normative life — a personal and social cost that does not reduce to convenience.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, individual_observant_jews, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, individual_observant_jews, beneficiary).

% Poskim, roshei yeshiva, and teachers who define the curriculum of sacrificial-law study, standardize the daily recitation, and adjudicate how the suspension is taught. The study mandate channels students, engagement, and institutional continuity through their academies, publications, and interpretive offices. Their authority rests on administering the tradition's response to the unperformable; abandoning the suspension framing would dissolve the institutional role built around it, which makes formal revision costly even though doctrinally available to them.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholarly_establishment, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholarly_establishment, beneficiary).

% Holds the collective goods the arrangement produces: covenantal continuity across nineteen centuries without a Temple, a shared daily practice linking dispersed communities, and an oriented hope — restoration — that structures the calendar's prayers. The community as a whole pays little directly; the burden is distributed onto individual members. Its collective identity is bound to the suspension framing: declaring the obligation lapsed would redefine the community's entire self-understanding.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, observant_jewish_community, beneficiary,
    organized, generational, identity_locked, global).

% Hold that the sacrificial commandments lapsed with the Temple and that the suspension framing refuses an honest conclusion. Already outside the observant conversation — they exited the framework rather than contesting it internally, and their parallel institutions embody the archival alternative. Would object that 'suspended' preserves as live an obligation they hold void, and that the daily readiness levy is maintained partly because dissenting voices are no longer in the room.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, reform_and_secular_jews, excluded,
    organized, biographical, mobile, global).

% Contest the arrangement's endpoint rather than its present operation: the declared termination condition resolves into resumed animal sacrifice, and they object to industrial-scale ritual slaughter being held permanently open as a live option by an indefinite deferral. Present in public bioethics and animal-welfare discourse but with no seat in the halakhic process that maintains the readiness protocol.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, animal_ethics_advocates, excluded,
    organized, generational, mobile, global).

% Study the post-70 CE transformation of Judaism into a text-and-practice community: the history of sacrificial-law study, liturgical substitution, messianic expectation surges and deflations, and the rival readings' institutional careers. Document the arrangement's evolution and corroborate the founding problem's historical reality without holding any normative position on the obligation's status.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, academic_historians_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholarly_establishment).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the covenantal community's continuity and coherence across a period in which its central ritual institution is unavailable: a shared daily practice (korbanot recitation, sacrificial-law study) synchronizes dispersed communities, transmits the legal corpus intact across generations, and keeps the community's orientation fixed on restoration rather than closure.
% TRANSFER_FUNCTION: Moves time and attention from individual observant Jews into the study and recitation of sacrificial law, and through that channel into the academies, texts, and interpretive offices that teach it; simultaneously moves the obligation itself forward in time — deferring its discharge to a future restoration instead of cancelling or transferring it.
% ABSENT_VOICES: Those who hold the obligation lapsed (Reform and secular Jews) are outside the conversation — they exited rather than dissenting internally, so the framework's unanimity is partly an artifact of their absence. Animal-ethics perspectives on the restoration endpoint are likewise unrepresented in the halakhic process. Within the community, members who privately find the recitation rote rarely voice it, since doubt about restoration shades into doubt about the framework itself.
% DISAPPEARANCE_RATIONALE: If the suspension-and-study arrangement vanished overnight, the community would face an immediate fork with no default: either declare the obligation lapsed (rupturing nineteen centuries of self-understanding and converging on the archival position) or treat the commandments as standing and unmet (importing permanent deficiency into daily life). The daily liturgy would lose its korbanot core, yeshiva curricula would shed a defining track, and the messianic orientation structuring the calendar's prayers would lose its object. The world of the observant community rearranges.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), a covenant whose core commandments required a functioning altar faced the problem of how unperformable commandments remain binding: cancelling them would break covenantal continuity, performing them was impossible, and silently ignoring them would erode the legal corpus — the tradition needed a fourth category for commanded-but-unperformable obligations.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic historians of religion and Jewish studies document the post-70 CE crisis of practice and the emergence of substitution strategies as a real historical problem, independent of any confessional commitment. Rival movements corroborate it implicitly — Reform's declaration that the sacrificial system is obsolete presupposes that a binding-obligation problem existed to resolve. What is disputed between the parties is the problem's present status (live versus resolved), not its historical reality.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.42, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon 0.42 is reading-indexed over the fixed referent: scored by this reading's own lights, the arrangement's extraction is the readiness burden itself — recurring daily time and attention, a curriculum commitment, liturgical load — real and cumulative but moderate, guilt-free, and framed as discipline rather than loss. Suppression 0.18 reflects the post-emancipation dissolution of enforcement machinery: persistence is now identity-based rather than coerced, with residual social pressure only. Theater 0.28: within the frame most study is functional (the corpus must stay known for resumption), while a growing minority of recitation is rote habit. Accessibility_collapse 0.65: inside the committed framework the alternatives are doctrinally foreclosed (the obligation cannot lapse, study cannot fulfill it), but exit exists and rival movements embody live alternatives externally, so collapse is substantial but incomplete. Resistance 0.30: intra-community resistance is thin, but the arrangement has met sustained external resistance (movements founded on declaring the obligation lapsed) and periodic internal discomfort with the restoration prospect. Identity-lock dynamics: the binding mechanism is relational-covenantal — members' self-concept is constituted through membership in a community whose continuity this practice maintains, so exit is experienced as self-estrangement and family rupture rather than a policy choice; if that identity frame broke, exits would open, the burden would need repricing, and the arrangement would likely collapse toward the archival or a voluntary-coordination shape. Cyclical pattern: the series is not monotone. Restoration-expectation surges (exemplified by the Sabbatean episode around T=1590-1666) briefly raise the readiness burden and LOWER theatricality — fervent engagement is earnest — while raising conformity pressure; each deflation normalizes the burden and lets rote recitation accumulate. The oscillation functions as intermittent reinforcement: each fervor cycle re-earnests the practice and each deflation re-embeds it as habit, which is part of how engagement has been sustained across nineteen centuries. All three series run on one shared ten-point grid; base scalars reflect the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the rabbinic_scholarly_establishment seat, the arrangement is a living preparatory discipline it administers: coordination-forward, low effective extraction, its own institutional purpose vindicated. From the individual_observant_jews seat — moderate power, identity_locked exit — the same structure operates as a standing levy on time and attention that the member did not individually negotiate, moderated by the fact that the framework prices the levy as privilege. From the excluded seats (reform_and_secular_jews), the entire arrangement reads as a refusal to close accounts: an elaborate deferral of an admission they consider overdue. The engine computes this per-seat divergence from the structural data; the authored scaffold claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (observant_jewish_community, rabbinic_scholarly_establishment) drive low directionality for those seats: the community receives continuity and orientation collectively; the establishment additionally administers the arrangement. The burden-bearers (individual_observant_jews, moderate power, identity_locked) receive a directionality override to 0.55: with no victims declared, the structural derivation would seat moderate-power agents near symmetric, but the readiness burden lands specifically and daily on them, making them net cost-bearers relative to the collective beneficiaries — while stopping well short of full-target because the burden is willingly framed, guilt-free, and paired with received meaning and belonging. Scope is global (a diaspora-spanning practice), which modestly amplifies effective extraction through verification difficulty. The receipt surface is separable from benefit: the study mandate's yield — engagement, students, institutional continuity — demonstrably accrues to the rabbinic_scholarly_establishment seat, which is why gain_flow names it, while the collective continuity good remains diffuse.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification discipline cuts both ways here. Calling this a snare would require identifiable victims being held against interest — under this reading none exist: no transfers are seized, no one is denied an available alternative they want, and the burden is borne inside a valued identity frame. Calling it a rope would erase the arrangement's defining feature: it is explicitly transitional, oriented to its own termination (restoration), which steady-state coordination is not. Scaffold captures the structure: genuine coordination function (identity and textual continuity across a Temple-less era), a declared sunset condition, moderate transitional burden. The mandatrophy risk runs the other direction: if restoration expectation dies entirely while study continues, the sunset clause becomes decorative and the arrangement drifts toward piton — theatrical readiness maintained by inertia. The theater_ratio series and the sunset_clause_falsifiability omega watch for exactly that drift. On the genealogy interview, founding_problem_status=live paired with disappearance_verdict=world_rearranges is the consistent pairing (no mismatch flag): the founding problem (unperformable commandments in a living covenant) still exists, and the arrangement's disappearance would force an unresolved fork. Fixing cost: formal revision — lapsing the obligation or converting study into fulfillment — is doctrinally available to the establishment that administers the arrangement, but the cost (rupture with the transmitted self-understanding, schism risk, dissolution of the institutional role built around the suspension) is prohibitive relative to the benefit of relieving a moderate, willingly-framed burden; hence fixing_cost=prohibitive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (messianic_suspension) of the sacrifice_obligation_continuity kernel; the sibling readings (archival_preservation, performance_only, study_as_performance) instantiate structurally different constraints from the same kernel. Which reading governs a given community is contested, and the disagreement is located in the obligation''s present normative status: lapsed, discharged-through-text, standing-unmet, or suspended.',
    'Observable in each community''s stated doctrine, liturgical practice, and curriculum: whether korbanot study is framed as memory (archival), preparation (performance_only), fulfillment (study_as_performance), or readiness-maintenance (this reading). Movement-level adoption is public and documented.',
    'Adopting archival_preservation collapses epsilon toward zero (memory-only study, no binding force, no burden); adopting study_as_performance removes the deferral cost (the burden becomes fulfillment); adopting performance_only raises suppression (a standing unmet demand with no suspension category). This story''s epsilon of 0.42 is valid only under the suspension reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a four-way contested kernel; sibling adoption would change the victim set, epsilon, and type.').

omega_variable(
    sunset_clause_falsifiability,
    'Is the messianic restoration trigger a genuine termination condition, or is it unfalsifiable in practice such that the arrangement can defer indefinitely without ever failing its own test?',
    'Behavioral test: does the community treat candidate signs of restoration as actionable (curriculum shifts toward practical service law, Temple-institute activity gaining mainstream standing), or does every candidate sign get absorbed without operational consequence? Twentieth-century religious Zionism and Temple Mount activism are the live test cases.',
    'If the sunset clause is functionally permanent, the arrangement''s transitional justification hollows out and it drifts from scaffold toward rope (steady-state identity coordination wearing a transitional costume) or, if study also atrophies into rote, toward piton. If the trigger is treated as real and near, the scaffold classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_falsifiability, conceptual, 'Whether the declared sunset condition is operationally live or decorative.').

omega_variable(
    readiness_functionality,
    'Does sustained study of the sacrificial corpus actually maintain operational readiness (a system that could resume), or is the readiness symbolic?',
    'Compare communities with intensive korbanot curricula against those with minimal engagement on demonstrated resumption-capacity claims (practical halakhic competence in service law, supply-chain and site contingencies studied seriously); assess whether any credible resumption plan exists anywhere in the tradition''s institutions.',
    'If readiness is symbolic, the theater_ratio understates performative maintenance and the arrangement carries latent piton symptoms (theatrical upkeep of an atrophied function); if readiness is real, the study mandate is functional maintenance and the scaffold claim is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(readiness_functionality, empirical, 'Operational versus symbolic character of the study-maintains-readiness protocol.').

omega_variable(
    burden_voluntariness,
    'Is the readiness burden borne voluntarily within the community''s value frame (a cherished discipline), or is it identity-locked such that refusal carries costs members cannot freely decline?',
    'Compare burden experience and attrition across populations with different exit costs: baalei teshuva (who chose entry as adults) versus lifelong members; communities with strong social sanction versus weak; trajectories of those who quietly stop the daily recitation.',
    'If the burden is substantially identity-locked, effective extraction on individuals is higher than the willing-participant frame suggests, individual directionality shifts toward the target end, and the low suppression score partially reflects internalized rather than absent pressure. If genuinely voluntary, the moderate extraction reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_voluntariness, empirical, 'Voluntary discipline versus identity-locked obligation in the readiness burden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 0, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sacr_tr_t200, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 200, 0.11).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 500, 0.15).
narrative_ontology:measurement(sacr_tr_t900, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 900, 0.18).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1200, 0.21).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1500, 0.23).
narrative_ontology:measurement(sacr_tr_t1660, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1660, 0.19).
narrative_ontology:measurement(sacr_tr_t1750, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1750, 0.27).
narrative_ontology:measurement(sacr_tr_t1870, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1870, 0.27).
narrative_ontology:measurement(sacr_tr_t1955, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1955, 0.28).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sacr_be_t200, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 200, 0.33).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 500, 0.35).
narrative_ontology:measurement(sacr_be_t900, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 900, 0.36).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1200, 0.37).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1500, 0.38).
narrative_ontology:measurement(sacr_be_t1660, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1660, 0.46).
narrative_ontology:measurement(sacr_be_t1750, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1750, 0.39).
narrative_ontology:measurement(sacr_be_t1870, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1870, 0.41).
narrative_ontology:measurement(sacr_be_t1955, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1955, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(sacr_su_t200, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 200, 0.2).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 500, 0.3).
narrative_ontology:measurement(sacr_su_t900, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 900, 0.38).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1200, 0.45).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1500, 0.46).
narrative_ontology:measurement(sacr_su_t1660, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1660, 0.5).
narrative_ontology:measurement(sacr_su_t1750, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1750, 0.4).
narrative_ontology:measurement(sacr_su_t1870, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1870, 0.3).
narrative_ontology:measurement(sacr_su_t1955, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1955, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__archival_preservation).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__study_as_performance).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the sacrifice obligation after the Temple' covers four structurally distinct claims about the obligation's present status, each with its own stable epsilon, beneficiary structure, and type. This reading (messianic_suspension) authors epsilon 0.42 over the suspension-and-study arrangement; archival_preservation authors near-zero epsilon over a memory-only arrangement; study_as_performance authors low epsilon over a fulfilled-through-text arrangement; performance_only authors higher suppression over a standing-unmet arrangement. The stories are linked bidirectionally through affects_constraints; the shared kernel is the fixed Pentateuchal sacrificial text, with drift migrating into each reading's interpretation layer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_continuity__messianic_suspension, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
