% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation â Messianic Suspension Reading
 *   domain: religious/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the messianic_suspension reading of
 *   the temple_sacrifice_obligation kernel: the halakhic claim that the
 *   biblical commandment of Temple sacrifice is not fulfilled, not violated,
 *   and not permanently terminated, but suspended in a state of legal
 *   abeyance pending messianic restoration and rebuilding of the Temple. The
 *   rabbinic authority structure administers this deferral, interpreting the
 *   boundaries of the suspension. Study of sacrificial law is framed as
 *   maintenance of knowledge-in-waiting rather than as compliance or
 *   preparation. The constraint coordinates the Jewish community around a
 *   shared default of non-practice without abandoning the textual obligation.
 *   Claimed as rope: the arrangement solves a coordination problem (what to
 *   do about sacrifice without a Temple) with negligible extraction and
 *   minimal enforcement.
 *
 * KEY AGENTS:
 *   - rabbinic_courts_and_poskim: Agenda-setter (institutional/constrained) â administers the suspension through legal interpretation and maintains the deferral framework.
 *   - observant_jewish_community: Beneficiary (organized/constrained) â coordinates practice around the suspension, receives clarity and communal unity.
 *   - temple_institute_activists: Excluded (moderate/mobile) â rejects suspension, advocates immediate restoration, marginalized from halakhic consensus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.05).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.12).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation â Messianic Suspension Reading").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, '81834374-7035-42ec-859a-9f01648f2446').
narrative_ontology:cs_kernel_codification('81834374-7035-42ec-859a-9f01648f2446', fixed_text).
narrative_ontology:cs_authority_grounding('81834374-7035-42ec-859a-9f01648f2446', lineage).
narrative_ontology:cs_interpretation_layer_present('81834374-7035-42ec-859a-9f01648f2446').
narrative_ontology:cs_reading_relation('81834374-7035-42ec-859a-9f01648f2446', temple_sacrifice_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_reading_relation('81834374-7035-42ec-859a-9f01648f2446', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('81834374-7035-42ec-859a-9f01648f2446', foundational, temple_destruction_suspends_obligation).
narrative_ontology:cs_axiom_status(temple_destruction_suspends_obligation, holdable).
narrative_ontology:cs_axiom_grounding('81834374-7035-42ec-859a-9f01648f2446', temple_destruction_suspends_obligation, deontological).
narrative_ontology:cs_axiom('81834374-7035-42ec-859a-9f01648f2446', foundational, study_not_substitute_for_sacrifice).
narrative_ontology:cs_axiom_status(study_not_substitute_for_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('81834374-7035-42ec-859a-9f01648f2446', study_not_substitute_for_sacrifice, deontological).
narrative_ontology:cs_reference_frame('81834374-7035-42ec-859a-9f01648f2446', active_obligation_under_suspension).
narrative_ontology:cs_drift_state('81834374-7035-42ec-859a-9f01648f2446', contemporary_diaspora, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('81834374-7035-42ec-859a-9f01648f2446', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, observant_jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the halakhic framework that interprets the biblical Temple sacrifice obligation as suspended; issue rulings on what study and practice are permissible during the suspension; maintain the legal continuity that defers active obligation to a future messianic restoration event. Bound by the weight of Talmudic precedent and communal expectations, they cannot unilaterally declare the obligation active or terminated without undermining their own authority.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, rabbinic_courts_and_poskim, agenda_setter,
    institutional, generational, constrained, global).

% Coordinates daily practice around the shared legal norm that sacrifice is suspended; benefits from clarity on ritual status and from a coherent communal narrative that preserves covenantal identity without demanding an impossible practice. Exit from the constraint means leaving the halakhic framework entirely or adopting a marginal alternative reading, both of which carry high social and identity costs.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, observant_jewish_community, beneficiary,
    organized, biographical, constrained, global).

% Reject the indefinite suspension and advocate for immediate restoration of sacrifice and/or construction of the Temple. They are marginalized by the dominant rabbinic interpretation and excluded from halakhic consensus; their position is treated as non-normative or dangerous rather than as a legitimate legal opinion within the mainstream.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, temple_institute_activists, excluded,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates post-Temple Jewish practice around a shared legal status for the sacrificial obligation, preventing sectarian fragmentation by establishing collective deferral rather than unilateral abandonment or rogue restoration.
% TRANSFER_FUNCTION: Transfers normative authority from the defunct Temple priesthood to the rabbinic interpretive class to adjudicate the boundary between suspended and active commandments; extracts no significant material transfer.
% ABSENT_VOICES: Temple-movement activists and sectarians who reject rabbinic suspension and advocate immediate restoration are structurally excluded from the halakhic consensus; they would argue for active obligation or alternative calendrical practice if admitted.
% DISAPPEARANCE_RATIONALE: If the suspension vanished overnight â if the obligation were declared fully active without Temple or messianic restoration â the Jewish community would face an immediate crisis of practice (how to sacrifice legitimately), authority (who may perform it), and theology. The deferral is load-bearing for current arrangements.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the only legitimate site for commanded sacrifice, creating a crisis of covenantal continuity: how to remain in commanded relationship with the divine without the central ritual mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of Second Temple Judaism and archaeologists attest to the Temple's destruction and the post-70 CE religious crisis. The specific halakhic solution of messianic suspension is attested within the rabbinic textual tradition; no external corroboration exists for the normative claim that obligation is suspended rather than terminated.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.05, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).
:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.05 because the suspended obligation extracts essentially nothing from those it governs: there is no current duty to sacrifice, no penalty for non-sacrifice, and no material rent collected in its name. Suppression is low (0.12) because the suspension is the dominant default; only marginal groups resist it, and the primary mechanism is social consensus rather than coercion. Theater ratio is modest (0.18) because while study of sacrifice law has some ceremonial or commemorative character, its primary function under this reading is genuine legal maintenance. Accessibility collapse is moderate (0.35): alternative readings (study as occupation, immediate restoration) are visible and held by identifiable groups, so alternatives have not collapsed. Resistance is very low (0.08) because the suspension is widely accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic agenda-setter seat, the suspension is a necessary and faithful legal response to historical catastrophe, preserving the obligation's integrity while protecting the community from impossible practice. From the excluded Temple activist seat, the same structure appears as an illegitimate indefinite deferral that consolidates rabbinic authority at the expense of priestly/cultic restoration. The engine will compute divergent seat classifications from this structural asymmetry: the beneficiary seat experiences rope-level coordination, while the excluded seat experiences the constraint as a barrier (higher effective extraction due to exclusion and marginalization).
 *
 * DIRECTIONALITY LOGIC:
 *   The observant community is the declared beneficiary: the suspension subsidizes their current practice by removing an impossible obligation and providing a coherent legal narrative. Their directionality d is near the beneficiary end (low d). The rabbinic authority is agenda_setter; while they accrue legitimacy from administering the system, the low extractiveness means they do not capture significant rents from this specific constraint â their d is near neutral or slightly beneficiary. Temple activists are excluded; if they were included they would be targets of the constraint's marginalizing force, so their derived d would be high (target). No victim group is declared because the constraint, in its suspended state, does not systematically extract from any named party.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy mislabeling by distinguishing the suspension's coordination function (preventing sectarian chaos after the Temple's destruction) from extraction. Because the obligation is genuinely suspended rather than secretly enforced, there is no hidden extraction. The constraint does not claim the founding problem (Temple destruction) is solved; it admits the problem persists and the suspension is a holding pattern. This prevents the 'dead founding problem + world rearranges' piton signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the messianic suspension reading a genuine legal derivation from the fixed kernel, or a post-hoc historiographical reconstruction to legitimize rabbinic authority after the Temple''s destruction?',
    'Comparative analysis of pre-destruction legal texts and Second Temple sectarian literature to determine whether suspension doctrines predate 70 CE or emerge from the rabbinic reduction.',
    'If the suspension is demonstrably a post-hoc reconstruction, the constraint''s claimed rope status as coordination around a fixed legal status weakens; it may recompute as a commitment-system scaffold or piton maintaining authority through narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the suspension reading derives from the kernel or reconstructs it.').

omega_variable(
    suspension_vs_termination,
    'Does the obligation''s suspension imply its latent persistence (awaiting restoration) or has it effectively terminated, with study serving as a memorial rather than a legal placeholder?',
    'Textual analysis of the halakhic sources on whether suspended commandments retain full legal personality or are downgraded to theoretical knowledge; theological survey of whether restoration would reactivate or reissue the obligation.',
    'If termination, the constraint is an identity_coordination rope around a memory; if genuine suspension, it is a legal deferral mechanism with a live trigger condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_termination, conceptual, 'Whether suspension masks termination or preserves live obligation.').

omega_variable(
    authority_legitimacy_drift,
    'If messianic restoration occurred, would the rabbinic class that administered the suspension retain authority over sacrificial practice, or would priestly lineage supersede it?',
    'Analysis of messianic/halakhic texts on authority transfer upon restoration; sociological study of how deferred legal systems reactivate.',
    'If priestly authority supersedes rabbinic authority upon restoration, the current rabbinic maintenance of the suspension may be read as preserving a system that will displace them â complicating the beneficiary structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_legitimacy_drift, empirical, 'Who holds authority if the suspension ends.').

omega_variable(
    committer_frame_boundary,
    'Does the messianic suspension reading foreclose the study-as-occupation reading within a single halakhic framework, or can they be held as complementary opinions by different authorities?',
    'Mapping the logical structure of the sibling readings: if study is ''neither compliance nor preparation'' then ''legitimate occupation'' is a contradiction in terms within the same framework; if the framework permits plural legal fictions, they coexist.',
    'If foreclosed, the kernel is logically partitioned and the engine should treat the readings as mutually exclusive attractors; if coexisting, the kernel permits bounded pluralism and the classification should reflect coordination across disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_boundary, conceptual, 'Logical relationship between suspension and occupation readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.15).
narrative_ontology:measurement(temp_tr_t5, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 5, 0.16).
narrative_ontology:measurement(temp_tr_t10, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 10, 0.18).
narrative_ontology:measurement(temp_tr_t15, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 15, 0.18).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 20, 0.18).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t5, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(temp_be_t10, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 10, 0.06).
narrative_ontology:measurement(temp_be_t15, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 15, 0.06).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 20, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(temp_su_t5, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 5, 0.11).
narrative_ontology:measurement(temp_su_t10, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(temp_su_t15, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 15, 0.12).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 20, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the temple_sacrifice_obligation kernel, which decomposes the colloquial label 'Temple sacrifice obligation' into structurally distinct claims: messianic_suspension (obligation live but suspended), study_as_occupation (study fulfills the obligation), and study_as_archiving (study preserves knowledge). Each reading has a distinct epsilon, beneficiary structure, and normative status for study.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
