% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__relational_autonomy, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational Autonomy Triad in End-of-Life Decision-Making
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   End-of-life medicine in the shared-decision-making era runs on an
 *   arrangement this story calls the relational-autonomy triad: dignity at
 *   the deathbed is treated as a property of the relational context, and
 *   decision authority over withdrawal of treatment, palliation, and
 *   contested refusals is distributed across the dying patient, the family,
 *   and the clinical team, governed by procedural safeguards — capacity
 *   assessment, documented deliberation, mandatory ethics consultation for
 *   disputed cases. The arrangement replaced two prior unilateral regimes
 *   (clinician paternalism and de facto family veto) and is now the
 *   mainstream bioethics position, codified in institutional policy,
 *   professional guidelines, and much surrogate-consent law. This file is ONE
 *   READING of the dignified_death kernel (see commentary.kernel_context and
 *   the kernel_reading_position omega); its epsilon is authored for the
 *   standing triadic arrangement as the relational reading itself assesses it
 *   — never for the arrangements the sibling readings would put in place. The
 *   interval 0-30 maps to roughly 1990-2020, the institutionalization era of
 *   shared decision-making, from the post-Cruzan formalization of ethics
 *   consultation through the spread of aid-in-dying statutes.
 *
 * KEY AGENTS:
 *   - supported_dying_patients: protected party (powerless/trapped) — dying patients whose decisions get a structured hearing, capacity assessment, and documented voice inside the triad; they cannot exit the process that governs their own death
 *   - patient_family_members: primary beneficiary (moderate/constrained) — recognized standing, procedural weight for their objections, and a moral burden shared rather than imposed or stolen
 *   - attending_clinicians: secondary beneficiary (institutional/mobile) — clinical judgment integrated into the decision, moral weight distributed across the group, legal position buffered by documented process
 *   - healthcare_institutions: agenda-setter and concentrated beneficiary (institutional/constrained) — write and administer the safeguards; collect the liability shield and absorb family conflicts that would otherwise land on clinicians or courts
 *   - hospital_ethics_committees: procedural administrator (organized/constrained) — convene deliberation, mediate disputes, issue recommendations on contested cases; chartered and restructurable by the institution
 *   - competent_patients_with_overridden_wishes: primary target (powerless/trapped) — undisputed decision-making capacity, diluted final authority; the same procedure that protects others converts their wish into one input among several
 *   - socially_isolated_patients: secondary target (powerless/trapped) — no relational network for the procedure's standing assignments; interests carried by clinician proxy and default institutional policy
 *   - disability_rights_advocates: excluded voice (organized/constrained) — object from outside that capacity assessment and family-weighted deliberation systematically discount disabled self-determination
 *   - academic_bioethicists: analytical observer (analytical/analytical) — author the doctrine the procedure operationalizes, audit case-level outcomes, document the gap between deliberative ideal and bedside practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.38).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.28).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Autonomy Triad in End-of-Life Decision-Making").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, '04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e').
narrative_ontology:cs_kernel_codification('04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e', distributed).
narrative_ontology:cs_authority_grounding('04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e', expertise).
narrative_ontology:cs_interpretation_layer_present('04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e').
narrative_ontology:cs_reading_relation('04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e', foundational, dignity_is_relationally_constituted).
narrative_ontology:cs_axiom_status(dignity_is_relationally_constituted, holdable).
narrative_ontology:cs_axiom_grounding('04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e', dignity_is_relationally_constituted, deontological).
narrative_ontology:cs_axiom('04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e', foundational, decision_authority_must_be_distributed_with_safeguards).
narrative_ontology:cs_axiom_status(decision_authority_must_be_distributed_with_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e', decision_authority_must_be_distributed_with_safeguards, instrumental).
narrative_ontology:cs_reference_frame('04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e', triadic_deliberative_dignity).
narrative_ontology:cs_drift_state('04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e', contemporary_bedside_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('04bd96b1-8789-4bb7-9b66-a9b3f1b9b59e', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, supported_dying_patients).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, patient_family_members).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, attending_clinicians).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, healthcare_institutions).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, competent_patients_with_overridden_wishes).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, socially_isolated_patients).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, relational_autonomy_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, shared_decision_making_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are dying patients whose end-of-life decisions proceed through the triadic process with family and clinicians present. The procedure gives their wishes a structured hearing: capacity is assessed, options are explained, their stated preferences are documented and carried into the decision unless formally overridden. What flows to them is decisional support and a record of their voice; what can flow away is final say, when the group's deliberation lands elsewhere. Exit does not exist in the ordinary sense — the process governs decisions about their own death, and they cannot take those decisions outside the institutional frame while in it.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, supported_dying_patients, beneficiary,
    powerless, immediate, trapped, local).

% Sit at the bedside with recognized standing: the procedure requires that they be heard, gives their objections procedural weight, and shares with them a moral burden that unilateral patient authority or clinician authority would place elsewhere. They bear the emotional cost of deliberation and, where the process overrides the patient's wish or their own, live with the outcome. They cannot leave the process while their person is dying inside it; disengagement is possible but carries relational and moral cost.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patient_family_members, beneficiary,
    moderate, biographical, constrained, local).

% Carry out the medical side of end-of-life care and sit as the third leg of the triad. The procedure integrates their clinical judgment, distributes the moral weight of withdrawal and palliation decisions across the group, and buffers them legally when a contested decision is later questioned. They bear the time cost of deliberation, documentation, and ethics consultation, and the moral distress of cases where the process lands against their judgment. They can change specialties, institutions, or leave practice; in most jurisdictions they cannot be compelled to perform interventions their conscience rejects.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, attending_clinicians, beneficiary,
    institutional, biographical, mobile, national).

% Write and administer the policies that constitute the triadic process — when ethics consultation is mandatory, what documentation is required, how disagreement escalates. They collect the liability shield: a documented, procedurally regular decision is far harder to litigate than a unilateral one, and the process absorbs family conflicts that would otherwise land on individual clinicians or the courts. They bear the cost of staffing committees, training staff, and the delay the process adds. They cannot exit the norm while operating in regulated healthcare markets, but they shape it.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, healthcare_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Run the case-level machinery: convene family meetings, assess capacity disputes, mediate between clinicians and families, issue recommendations on contested withdrawals and refusals. Most consultations resolve live conflicts; a growing share produce documentation that exists to demonstrate the process was followed. Members typically serve part-time alongside clinical duties; the committee can be disbanded or restructured by the institution that charters it.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, hospital_ethics_committees, agenda_setter,
    organized, generational, constrained, local).

% Are patients with undisputed decision-making capacity whose stated end-of-life wishes are diluted or reversed by the triadic process — a family's objection, a clinician's refusal, an ethics committee's recommendation, each given procedural weight equal to or greater than the patient's own voice. What flows from them is authority they would exercise alone under a patient-sovereignty rule; what flows to them is the assurance that no single actor decided alone. They cannot exit: the process governs the decision, and appeal outside it runs through the same institutional machinery.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, competent_patients_with_overridden_wishes, payer,
    powerless, immediate, trapped, local).

% Are dying patients with no family at the bedside and no advocate — estranged, outliving their kin, or institutionalized long-term. The procedure presupposes a relational network: its safeguards are calibrated to conflicts among present parties, and its standing assignments have no seat for absence. In practice their interests are carried by clinicians holding partial proxy authority and by default institutional policy. They cannot exit into a process built for them; the alternative is the default pathway the process was built to replace.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, socially_isolated_patients, payer,
    powerless, immediate, trapped, local).

% Organize outside the clinical encounter to object that procedural distribution of decision authority devalues disabled self-determination — that capacity assessments and family-weighted deliberation systematically discount the wishes of patients with cognitive and physical disabilities. They litigate, submit regulatory comment, and publish, but hold no seat in the triad the procedure constitutes; their access runs through the same institutions whose practice they contest.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Study the arrangement from outside practice: they author the doctrine the procedure operationalizes, audit its case-level outcomes, and document the gap between the deliberative ideal and bedside practice. They hold no decision authority in any individual case; their influence runs through journals, guidelines, and the training of the clinicians and ethicists who do sit in the triad.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, academic_bioethicists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__relational_autonomy, healthcare_institutions).
narrative_ontology:fixing_cost_class(dignified_death__relational_autonomy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: End-of-life decisions bind parties with distinct legitimate stakes — the dying person, those who will live with the loss, and the clinicians whose hands carry the decision out. The arrangement solves the problem of legitimating a decision none of the parties can rightly make alone: it structures who must be heard, what must be documented, and how disagreement is escalated, so that a contested death can proceed without any party's unilateral override.
% TRANSFER_FUNCTION: Moves decision authority — and with it moral burden and legal responsibility — from whichever party would otherwise hold it alone (the clinician under paternalism, the family under de facto veto, the patient under sovereignty norms) into the shared procedure; moves recognition and standing to family members and clinicians as decision participants; and, in cases where deliberation overrides a competent patient's stated wish, moves the patient's final authority to the group. It also moves documentation: every contested decision generates a record whose primary downstream holder is the institution's legal position.
% ABSENT_VOICES: The socially isolated patient is the clearest absent voice: the procedure presupposes a relational network and has no seat for its absence, so the people with the weakest advocacy are the ones its safeguards were not designed around. Disability rights advocates object from outside that capacity assessment and family-weighted deliberation discount disabled self-determination; patient-sovereignty advocates object that a competent person's wish should not be negotiable; both stand outside the triad the procedure constitutes, with access only through the institutions they are contesting.
% DISAPPEARANCE_RATIONALE: If the distributed-authority procedure and its safeguards vanished overnight, end-of-life decisions would not stop happening — they would reorganize around the strongest unilateral claimant: families would veto withdrawals they oppose, clinicians would resume paternalist management of dying, and in jurisdictions with aid-in-dying statutes the patient's unilateral request would stand alone without the deliberative buffer. Liability would migrate to whoever acted. The relational network's recognized standing would evaporate, and the documented-voice protections now attached to patient preferences would disappear with the documentation requirement.
% FOUNDING_PROBLEM: Unmanaged conflict at the deathbed: families and clinicians overriding patients, patients making unsupported requests in states of crisis, disputes ending in litigation or unilateral action — at the moment when the parties' relationships are most morally loaded and least revisable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the pre-arrangement litigation record (the Quinlan and Cruzan line and the hospital-ethics movement they triggered) documents the deathbed-conflict problem the arrangement was built to solve; disability rights organizations and patient-sovereignty advocates — both outside the relational network the arrangement benefits — attest the underlying conflict is real while disputing the arrangement's solution; clinical ethics consultation case registries independently record the continuing volume of live conflicts. No source inside the benefiting parties is relied on for the status claim.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__relational_autonomy_tests).
:- end_tests(dignified_death__relational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.38: the arrangement genuinely coordinates — it replaced unilateral override with structured deliberation and gives every present party a hearing — but it systematically dilutes competent patients' final authority and leaves patients without relational networks with weaker advocacy than the procedure's design assumes. Suppression is 0.28: the arrangement blocks unilateral decision pending process, enforced through institutional policy and professional norms rather than heavy coercion; the suppression_requirement series shows the enforcement apparatus maturing over the interval (mandatory consults, documentation standards, legal deference to process) — this is a story about enforcement infrastructure being built, which is why the series is authored rather than left to the scalar. Theater is 0.18 and rising slowly: most ethics consultations resolve live conflicts, but a growing share of procedural activity is documentation produced to demonstrate compliance rather than to decide. Accessibility_collapse is 0.45: alternatives persist and are used — advance directives restore unilateral patient authority, competent refusal of treatment remains legally protected in most jurisdictions, and aid-in-dying statutes in a growing set of jurisdictions route around the triad entirely — but the contested middle of deathbed decision-making is captured by the procedural frame. Resistance is 0.30: disability rights advocates, patient-sovereignty advocates, and conscientious objectors contest the arrangement from outside without displacing it. Claimed type is rope, per the reading's coordination character and the manifest's expected delta; note that the structural data (declared victims plus active enforcement) gives the engine everything it needs to compute tangled_rope — if it does, that divergence is the measurement, not an error. Boltzmann coordination_type is attachment_coordination: the primary function coordinated is the caregiving relationship at the end of life — the triad is a small-group relational mechanism requiring continuous maintenance, with the procedural safeguards as its maintenance machinery; the type default floor (0.08) applies and no override is authored. The attachment framing is not a cover story: family presence and the clinical relationship at the deathbed are coordination goods the arrangement actually produces, and the declared victim set is what keeps that framing honest. The measurement series run on one shared time grid (points 0, 6, 12, 18, 24, 30) with every tracked metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats compute differently from the same structure. From the institution's seat the triad is legitimacy infrastructure it built and staffs: the same procedure that delays decisions also absorbs the conflicts that would otherwise become litigation. From a competent overridden patient's seat the identical procedure is the machinery that converted their final word into one input among several. Same-level divergence is sharpest between two dying patients in the same hospital: one experiences the deliberative buffer as protection (a family pressing toward a decision they do not want), the other as dilution (a wish the triad will not ratify). What differentiates their situations is not global power — both are powerless, both trapped — but capacity status and relational presence, the two variables the procedure is calibrated to. The engine derives this divergence from the declared beneficiary/victim structure and exit atoms; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (supported patients, families, clinicians, institutions) derive low directionality — the arrangement subsidizes them with standing, burden-sharing, integrated judgment, and the liability shield. Declared victims (overridden competent patients, isolated patients) derive high directionality — they pay in decision authority and advocacy, and both are trapped: a dying patient cannot exit the process that governs their death. The one structural subtlety the derivation handles without overrides: supported dying patients are beneficiaries with trapped exit, which sits them nearer the target end than their beneficiary declaration alone would suggest — the protection is real but they cannot arbitrage away from the arrangement's costs. No directionality_overrides are authored: the beneficiary/victim declarations plus the exit atoms place every seat where the structure puts it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline prevents two opposite mislabels. Reading the arrangement as pure extraction would erase the coordination function that is really there: the procedure did replace family veto and clinician paternalism with structured deliberation, and the founding problem — unmanaged deathbed conflict — is still live. Reading it as costless coordination would erase the identified victims: competent patients whose authority the procedure dilutes and isolated patients for whom it has no seat. The mandatrophy question for this reading is whether the procedural apparatus still solves the founding problem or has begun to perpetuate itself as liability management; founding_problem_status is live, theater_ratio is still low (0.18), and the mandate is not yet outlived. The early-warning signal in the measurement series is the slow theater rise: if documentation-for-compliance overtakes decision-resolving activity (theater_ratio above 0.5), the arrangement has drifted toward inertial procedural performance and should be re-read as a piton. The rising suppression_requirement series is the second watch item: enforcement machinery that keeps maturing after the norm has normalized would indicate the arrangement holding by administration rather than by agreement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the dignified_death kernel — the relational_autonomy reading, in which dignity is relationally constituted and decision authority is distributed across the patient-family-clinician triad. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Author the sibling stories (dignified_death__autonomy_primary, dignified_death__sanctity_primary) and compare victim sets, epsilon, and enforcement structure across the kernel. The disagreement with autonomy_primary is located in the locus of final decision authority (individual vs distributed); the disagreement with sanctity_primary is located in whether intentional termination is permissible at all — a prior question this reading''s procedure operationalizes but cannot itself settle.',
    'If the load-bearing contest is authority allocation, this reading stands or falls as a direct rival to autonomy_primary and the foreclosure edge is the structural fact; if the load-bearing contest is termination permissibility, this reading is downstream procedural bookkeeping over a prior moral verdict and its epsilon should be re-derived relative to that verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which structural element the sibling readings of dignified_death contest, and what this reading presupposes.').

omega_variable(
    isolated_patient_structural_status,
    'Is the socially isolated patient''s disadvantage intrinsic to the triadic arrangement (the procedure has no seat for absent relations), or an artifact of under-resourced implementation (professional advocates and ombuds services could fill the empty seat)?',
    'Compare institutions that fund professional patient advocates for unrepresented patients against those relying on ad hoc clinician proxy: if supported isolation closes the outcome gap, the disadvantage is implementational; if it persists, it is structural to the triad.',
    'If structural, the victim set is permanent and the arrangement''s cost to that group is a fixed feature of the reading (pushing the computed type toward tangled_rope); if implementational, the victim set is contingent and the rope classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isolated_patient_structural_status, empirical, 'Whether the isolated-patient victim set is intrinsic to the triad or fixable by implementation.').

omega_variable(
    protection_dilution_separability,
    'The same procedural machinery that shields vulnerable patients from family pressure and clinician paternalism also dilutes competent patients'' unilateral authority — is the measured extraction the price of the protection, or separable from it?',
    'Case-level analysis separating decisions where the patient''s capacity is contested (protection domain) from decisions where capacity is undisputed (dilution domain): if triadic overrides concentrate in the contested domain, the cost tracks protection; if overrides appear in the undisputed domain, the dilution is separable.',
    'If the cost tracks protection, epsilon should be read down as coordination overhead; if separable, the dilution component is genuine asymmetric extraction riding on the coordination frame — supporting a tangled_rope reading over the authored rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_dilution_separability, empirical, 'Whether the arrangement''s cost to competent patients is separable from its protective function.').

omega_variable(
    suppression_mechanism_character,
    'Is the arrangement''s suppression structural (institutional rules that block unilateral decision pending procedure) or internalized (participants have absorbed the norm that solo death decisions are illegitimate, so coercion is rarely needed)?',
    'Observe behavior where enforcement lapses: in institutions without active ethics-committee enforcement, do unilateral decisions reappear (structural suppression dominant) or does the norm self-sustain (internalized)?',
    'If internalized, measured suppression understates the arrangement''s hold — removing the enforcement machinery would not restore unilateral authority; if structural, enforcement decay would rapidly return the pre-procedural equilibrium of family veto and clinician paternalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_character, empirical, 'Structural vs internalized character of the arrangement''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(dign_tr_t0, observed).
narrative_ontology:measurement(dign_tr_t6, dignified_death__relational_autonomy, theater_ratio, 6, 0.12).
narrative_ontology:measurement_basis(dign_tr_t6, observed).
narrative_ontology:measurement(dign_tr_t12, dignified_death__relational_autonomy, theater_ratio, 12, 0.14).
narrative_ontology:measurement_basis(dign_tr_t12, observed).
narrative_ontology:measurement(dign_tr_t18, dignified_death__relational_autonomy, theater_ratio, 18, 0.15).
narrative_ontology:measurement_basis(dign_tr_t18, observed).
narrative_ontology:measurement(dign_tr_t24, dignified_death__relational_autonomy, theater_ratio, 24, 0.17).
narrative_ontology:measurement_basis(dign_tr_t24, observed).
narrative_ontology:measurement(dign_tr_t30, dignified_death__relational_autonomy, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(dign_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(dign_be_t0, observed).
narrative_ontology:measurement(dign_be_t6, dignified_death__relational_autonomy, base_extractiveness, 6, 0.32).
narrative_ontology:measurement_basis(dign_be_t6, observed).
narrative_ontology:measurement(dign_be_t12, dignified_death__relational_autonomy, base_extractiveness, 12, 0.34).
narrative_ontology:measurement_basis(dign_be_t12, observed).
narrative_ontology:measurement(dign_be_t18, dignified_death__relational_autonomy, base_extractiveness, 18, 0.35).
narrative_ontology:measurement_basis(dign_be_t18, observed).
narrative_ontology:measurement(dign_be_t24, dignified_death__relational_autonomy, base_extractiveness, 24, 0.37).
narrative_ontology:measurement_basis(dign_be_t24, observed).
narrative_ontology:measurement(dign_be_t30, dignified_death__relational_autonomy, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(dign_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(dign_su_t0, observed).
narrative_ontology:measurement(dign_su_t6, dignified_death__relational_autonomy, suppression_requirement, 6, 0.16).
narrative_ontology:measurement_basis(dign_su_t6, observed).
narrative_ontology:measurement(dign_su_t12, dignified_death__relational_autonomy, suppression_requirement, 12, 0.2).
narrative_ontology:measurement_basis(dign_su_t12, observed).
narrative_ontology:measurement(dign_su_t18, dignified_death__relational_autonomy, suppression_requirement, 18, 0.24).
narrative_ontology:measurement_basis(dign_su_t18, observed).
narrative_ontology:measurement(dign_su_t24, dignified_death__relational_autonomy, suppression_requirement, 24, 0.26).
narrative_ontology:measurement_basis(dign_su_t24, observed).
narrative_ontology:measurement(dign_su_t30, dignified_death__relational_autonomy, suppression_requirement, 30, 0.28).
narrative_ontology:measurement_basis(dign_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, attachment_coordination).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% The dignified_death kernel decomposes into three structurally distinct constraints, linked here and not merged: this relational-autonomy reading (authority distributed across the triad; victims are those the procedure overrides or excludes; moderate epsilon), the autonomy_primary reading (patient holds final authority; a different victim set and enforcement structure), and the sanctity_primary reading (termination impermissible; the suffering denied release are its victims). Each story carries its own epsilon, beneficiaries, and claimed type; this file's epsilon is authored for the standing triadic arrangement as the relational reading itself assesses it. The upstream/downstream structure runs from the doctrinal level (this reading operationalizes relational autonomy doctrine) toward the case-level procedural constraints the siblings would govern differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
