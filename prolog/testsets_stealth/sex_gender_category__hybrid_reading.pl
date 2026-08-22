% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__hybrid_reading, []).

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
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Medical Gatekeeping Model of Sex/Gender Category Membership
 *   domain: social_ontology/legal_classification/identity_politics
 *
 * SUMMARY:
 *   Mid-twentieth-century medicine answered a question no other institution
 *   would touch: when may a person change sex/gender category? The settlement
 *   it built — the medical gatekeeping model — makes category membership a
 *   determination of biology plus supervised transition: a diagnosis
 *   establishes the condition, a clinical program (hormones, often surgery, a
 *   monitored real-life period) executes the change, and the clinic's
 *   certification is what legal and institutional recognition rides on. The
 *   model solved a real assignment problem — registries, prisons, sport,
 *   employers all needed a rule — and it built the gate that administers the
 *   rule, and both facts live in the same structure: the profession that
 *   verifies also collects (fees, treatment revenue, diagnostic jurisdiction
 *   over transness itself), the applicants who are verified also pay
 *   (narrative compliance, waiting years, refusal risk, pathologized legal
 *   status), and those who do not or cannot pass through the clinic are
 *   outside the category entirely. This story instantiates ONE reading of the
 *   contested sex_gender_category kernel — the hybrid reading; the
 *   biology_reading and identity_reading siblings are separate constraints
 *   with their own epsilon values and victim sets, linked through the
 *   network. The epsilon authored here is referent-fixed (the standing
 *   gatekeeping arrangement) and reading-indexed (assessed by this reading's
 *   own lights, which endorse the gate and count its verification function as
 *   legitimate while conceding its burdens). The claimed type and the metrics
 *   are authored independently: the claim is what I believe structurally
 *   true, the metrics what I believe descriptively true of the arrangement's
 *   operation.
 *
 * KEY AGENTS:
 *   - gender_clinics_medical_establishment: agenda-setter and primary beneficiary (institutional / arbitrage) — writes the Standards of Care, staffs the gate, collects assessment fees, treatment revenue, and diagnostic jurisdiction over transness
 *   - civil_status_authorities: secondary agenda-setter (institutional / constrained) — enforces the medical standard at the document counter; gains a politically safe answer to a contested question
 *   - conditionally_recognized_trans_individuals: conditional beneficiary and payer (moderate / identity_locked) — crossed the gate, holds recognition contingent on the medical record, bears paid and ongoing costs
 *   - gatekept_trans_individuals: primary target (powerless / trapped) — waits, performs the diagnostic narrative, risks refusal; no parallel road to recognition
 *   - non_transitioning_trans_individuals: excluded target (powerless / identity_locked) — outside the category entirely under this model's criteria; never had a seat where the criteria were written
 *   - general_public: diffuse beneficiary (moderate / mobile) — receives a verifiable membership standard for the institutions it staffs and uses
 *   - trans_advocacy_organizations: analytical observer (organized / analytical) — documents gatekeeping harms, litigates, campaigns for the identity reading; holds no administrative seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.58).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.62).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Medical Gatekeeping Model of Sex/Gender Category Membership").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social_ontology/legal_classification/identity_politics").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, '2add7bc2-92e1-4103-bc8e-8e4b2c5c4526').
narrative_ontology:cs_kernel_codification('2add7bc2-92e1-4103-bc8e-8e4b2c5c4526', formalized).
narrative_ontology:cs_authority_grounding('2add7bc2-92e1-4103-bc8e-8e4b2c5c4526', extraction).
narrative_ontology:cs_interpretation_layer_present('2add7bc2-92e1-4103-bc8e-8e4b2c5c4526').
narrative_ontology:cs_reading_relation('2add7bc2-92e1-4103-bc8e-8e4b2c5c4526', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('2add7bc2-92e1-4103-bc8e-8e4b2c5c4526', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_axiom('2add7bc2-92e1-4103-bc8e-8e4b2c5c4526', foundational, category_membership_changeable_through_transition).
narrative_ontology:cs_axiom_status(category_membership_changeable_through_transition, holdable).
narrative_ontology:cs_axiom_grounding('2add7bc2-92e1-4103-bc8e-8e4b2c5c4526', category_membership_changeable_through_transition, empirically_contingent).
narrative_ontology:cs_axiom('2add7bc2-92e1-4103-bc8e-8e4b2c5c4526', foundational, genuine_transition_requires_clinical_verification).
narrative_ontology:cs_axiom_status(genuine_transition_requires_clinical_verification, holdable).
narrative_ontology:cs_axiom_grounding('2add7bc2-92e1-4103-bc8e-8e4b2c5c4526', genuine_transition_requires_clinical_verification, empirically_contingent).
narrative_ontology:cs_reference_frame('2add7bc2-92e1-4103-bc8e-8e4b2c5c4526', medical_certification_membership_standard).
narrative_ontology:cs_drift_state('2add7bc2-92e1-4103-bc8e-8e4b2c5c4526', post_self_id_legislation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2add7bc2-92e1-4103-bc8e-8e4b2c5c4526', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, gender_clinics_medical_establishment).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, conditionally_recognized_trans_individuals).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, general_public).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, gatekept_trans_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, civil_status_authorities).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, conditionally_recognized_trans_individuals).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, gender_dysphoria_diagnostic_framework).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, medical_supervision_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Psychiatric, endocrinological, and surgical institutions that write the Standards of Care, run the assessment clinics, and issue the diagnoses and certifications that legal sex change requires. They set how long assessment takes, what narrative an applicant must give, what treatment is prerequisite, and who is refused. Assessment fees, treatment revenue, publication fields, and professional jurisdiction over transness all flow through the gate they staff. If the certification standard were abandoned, their role would shrink to service provision on patient request; they retain the option to reposition as care providers, since the clinical skills are theirs regardless of who holds the gate.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, gender_clinics_medical_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Registries, courts, and government departments that require medical evidence — diagnosis, treatment history, often surgery — before amending legal sex on documents. They did not write the diagnostic criteria but enforce them at the document counter, and they gain from the delegation: the contested question of who counts is answered by an authority that carries clinical legitimacy, sparing them the political cost of deciding it themselves.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, civil_status_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, civil_status_authorities, beneficiary).

% Trans people who completed the required assessments, treatments, and waiting periods and now hold legal recognition of their gender. Their recognition is contingent on the medical record that produced it: it can be contested, and in some jurisdictions it carries ongoing requirements. They paid the full toll — assessment narratives, treatment timelines, fees, years of disclosure to clinicians — and many remain in medical follow-up to keep their status secure. Leaving the arrangement behind is not available to them; their legal identity is bound to the certification they underwent.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, conditionally_recognized_trans_individuals, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, conditionally_recognized_trans_individuals, payer).

% Trans people currently seeking recognition through the medical route: waiting years for first appointments, assembling the diagnostic narrative the clinic expects, undergoing assessments that can refuse them, and accepting treatments the protocol requires in order to qualify. There is no parallel path to legal recognition in jurisdictions that run this model; refusal at the gate leaves them with the status they sought to change.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, gatekept_trans_individuals, payer,
    powerless, biographical, trapped, national).

% Trans and non-binary people who do not undergo, or do not complete, medical transition — because they do not want it, cannot access it, cannot afford it, or their profile does not fit the diagnostic script. Under this model they are outside the category entirely, regardless of how they live or what recognition they claim. The criteria were written by clinician committees they were never part of; their objection that the standard measures compliance rather than identity has no formal venue.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, excluded).

% The broad population whose institutions — schools, sports bodies, prisons, employers, registries — need a workable rule for which category a person belongs to. The medical standard gives them a verifiable answer administered by a trusted profession, and they pay almost nothing for it directly.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, general_public, beneficiary,
    moderate, biographical, mobile, national).

% Campaign groups and community organizations that document gatekeeping harms, litigate against requirements they view as degrading, and campaign for declaration-based recognition. They hold no administrative role; their seat is analytical and adversarial — they produce the counter-record that the clinics' assessments are contested.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_advocacy_organizations, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__hybrid_reading, gender_clinics_medical_establishment).
narrative_ontology:fixing_cost_class(sex_gender_category__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns category membership for legal and institutional purposes (documents, records, facilities, sport) through a single verifiable standard — medical certification of transition — solving the coordination problem of who counts as a man or woman when birth assignment is contested, in a form mid-century institutions would accept.
% TRANSFER_FUNCTION: Moves decision authority over category membership from individuals to medical institutions; moves fees, diagnostic compliance labor, waiting time, and pathologized legal status from trans individuals to the medical system; moves the power to define trans experience from trans communities to psychiatry.
% ABSENT_VOICES: Non-transitioning trans people and those whose histories do not fit the diagnostic script (non-binary people, those without dysphoria narratives) had no seat when the Standards of Care and diagnostic criteria were written — clinician-dominated committees set the gate's terms, and the people gated were objects of assessment, not participants in the rule-making.
% DISAPPEARANCE_RATIONALE: If the medical gatekeeping standard vanished overnight, every jurisdiction using it would need an immediate replacement rule for legal sex change — declaration or birth biology — medical gender services would reorganize around consent-based care, the conditionally recognized would hold their status under a different warrant, and the excluded would gain or lose standing depending on which sibling reading filled the gap. The arrangement's disappearance forces the kernel contest to a resolution; the world does not stay put.
% FOUNDING_PROBLEM: Mid-20th century: people seeking to live in a gender role other than their assigned sex needed hormones, surgery, legal documents, and social recognition, and no institution had an accepted standard for when such a transition was legitimate. Medicine supplied the only framework both patients and the broader society of the era would accept — at the price of pathologization.
% FOUNDING_PROBLEM_CORROBORATION: The founding access problem is corroborated from outside the benefiting parties by historical scholarship on mid-century gender medicine (Meyerowitz's archival work), trans community archives documenting the demand for recognition, and contemporaneous clinical literature stating the problem. Its status is disputed: medical institutions attest the verification problem is still live; trans advocacy outside the medical establishment attests the access need is live but the gatekeeping solution obsolete; and the legislative records of self-ID jurisdictions record an explicit judgment that certification is no longer necessary.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 — this is the hybrid reading's own assessment of the arrangement it endorses: the reading counts as legitimate the verification costs of a medical standard (assessment, treatment prerequisites) but concedes as burden the parts of the gate that decouple from any verification function — years-long waits, arbitrary clinic-to-clinic variability, refusals uncorrelated with clinical indicators, and the diagnostic jurisdiction the profession maintains around transness itself. Suppression (0.62) is the raw structural force keeping alternative paths to recognition foreclosed in hybrid jurisdictions; it is unscaled — only extractiveness is scaled by directionality and scope downstream. Theater (0.34) reflects the share of gate activity that tests narrative compliance rather than clinical need: a share that grew as the criteria formalized (Standards of Care 1979, the GID diagnosis 1980) and community knowledge of the 'correct' presentation spread, then eased as informed-consent models and depathologized framing reduced narrative testing. Accessibility collapse (0.58) is partial: inside a hybrid jurisdiction the legal alternatives are foreclosed, but the collapse is jurisdictional rather than total — self-ID regimes operate visibly elsewhere, and that visibility is itself pressure on the gate. Resistance (0.68) is high and continuous: six decades of community organizing, depathologization campaigns, and litigation. All three tracked metrics run on one shared eight-point grid (1950-2020, decade steps). The story tracks suppression_requirement because enforcement capacity is a central dynamic here — formalization (Standards of Care, diagnosis codes, surgical-requirement case law) followed by erosion (informed-consent clinics, SOC flexibility, depathologized framing) — not a static enforcement picture. The trajectories are non-cyclical: rise, plateau, partial erosion, with a slight re-hardening at the interval's end as backlash politics re-entrench medical authority in some jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same arrangement. From the establishment seat the structure is a professional function it built, staffs, and legitimately administers — near-full beneficiary directionality, low experienced burden, and the gate reads as the price of a standard. From the gatekept and non-transitioning seats the same structure reads as a toll booth with no parallel road: high directionality toward the target end, trapped or identity-locked exit, and the burden is the arrangement's product, not its price. The conditionally recognized seat sits between — they paid the toll and collect the recognition, and their legal identity is now bound to the certification, which tends to make them defenders of the gate they resented passing through. The engine computes these per-seat classifications from the structural data; this story does not adjudicate which seat's experience is 'the' constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. gender_clinics_medical_establishment is declared beneficiary and holds the agenda-setter seat with arbitrage-grade exit — directionality near the beneficiary end, with effective extraction damping toward subsidy: the gate pays them. general_public is declared beneficiary with mobile exit — low directionality, diffuse mild benefit. conditionally_recognized_trans_individuals are declared in BOTH arrays because their structural position is genuinely dual — they bear the gate's costs (paid and ongoing) and collect its benefit (recognition); the dual declaration plus identity-locked exit should derive a mid-range directionality rather than the low value a beneficiary-only declaration would produce. gatekept_trans_individuals are declared victims with trapped exit — near-full-target directionality, amplified effective extraction. non_transitioning_trans_individuals are declared victims with identity-locked exit — the highest directionality in the story: the arrangement forecloses recognition for them entirely, and they cannot exit their position because the position is their identity relative to the category. civil_status_authorities hold the agenda-setter seat with a secondary beneficiary role — low-to-mid directionality; they enforce and mildly gain. No directionality overrides are authored: the declarations plus exit atoms produce the honest values, so the derivation chain is left intact.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no accepted standard for when a category transition is legitimate — is still live in its access form (people still need documents and care), but the founding solution's monopoly is contested, so the R5 status is 'contested' rather than 'dead'. The arrangement is not inertial: its function has not atrophied — the gate still gates, the certification still certifies. Nor is it pure extraction: the coordination function is real, because institutions need assignable, verifiable category standards and some standard must exist. The tangled-rope classification preserves both truths and blocks the two symmetrical mislabelings: reading the arrangement as pure coordination hides the gatekeeping rents (fees, jurisdiction, narrative compliance) that the profession collects through the same structure that solves the assignment problem; reading it as pure extraction hides the genuine assignment problem any replacement — sibling readings included — must still solve. The mandate has not outlived its function; it has lost its monopoly. The measurement series shows the erosion is real but incomplete, which is why no mandatrophy resolution is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the sex_gender_category kernel — the hybrid medical-gatekeeping reading. What structurally changes under the sibling readings, and where exactly is the disagreement located?',
    'The sibling stories themselves (biology_reading, identity_reading): each authors its own epsilon, beneficiary/victim sets, and classification over the same referent; comparing the three isolates which structural elements — victim set, authority location, gate costs — are reading-relative.',
    'Under biology_reading the victim set expands to all trans people (membership immutable) and the medical establishment loses its gate; under identity_reading the victim set contracts to those subjected to verification regimes and the gate''s rents vanish; this story''s classification holds only for the hybrid reading''s structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    gatekeeping_cost_vs_verification_value,
    'Is the gate''s burden a verification cost that buys a standard institutions can rely on, or rent decoupled from any verification the standard performs?',
    'Compare recognition robustness (document reliability, institutional function, abuse rates) in hybrid-model jurisdictions against self-ID jurisdictions with comparable institutions; if function holds without the gate, the burden''s verification value is low and the decoupled share is rent.',
    'If the gate''s burden exceeds its verification value, most of the arrangement''s extractiveness is overhead riding on a coordination function a cheaper standard provides; if the burden tracks real verification value, much of the authored epsilon is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_cost_vs_verification_value, empirical, 'Whether gatekeeping costs track verification value or institutional rent.').

omega_variable(
    diagnostic_narrative_theater_share,
    'What share of gate activity tests narrative compliance (does the applicant present as the diagnosis expects) versus clinical need?',
    'Audit assessment criteria and approval patterns across clinics: if narrative conformity predicts approval better than clinical indicators do, the performative share dominates; informed-consent clinics provide the comparison arm.',
    'A high narrative-compliance share raises the true theater ratio above the authored 0.34 and shifts the arrangement toward enforcement of a script rather than assessment of a condition; a low share supports the reading''s assessment-function claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_narrative_theater_share, empirical, 'How much of the gate is script enforcement versus clinical assessment.').

omega_variable(
    non_transitioner_exclusion_status,
    'Is the exclusion of non-transitioning trans people a boundary condition of THIS reading''s category — the category is partly constituted by the transition, so non-transitioners sit outside it as cis people do — or a suppression of alternatives, barring people who would qualify under a different reading from a status they claim?',
    'Conceptual: fix this reading''s own definition and ask whether the excluded party asserts a claim the definition even addresses; empirical supplement: whether excluded individuals experience the exclusion as denial of something they claim or as irrelevance.',
    'If exclusion is definitional, the victim set for this reading is narrower than a sibling reading would author and the arrangement''s epsilon rests on the gatekept rather than the excluded; if it is suppression, the non-transitioning seat is a first-class extraction target and epsilon rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_transitioner_exclusion_status, conceptual, 'Whether the excluded seat is outside the category''s question or suppressed by its answer.').

omega_variable(
    medical_authority_ground,
    'Does the establishment''s gatekeeping authority track clinical expertise about transition care, or institutional self-interest in maintaining diagnostic jurisdiction over transness?',
    'Trace the profession''s own revision record: resistance to depathologization, the economics of assessment services, and whether gate strictness correlates with clinical evidence or with jurisdictional stakes.',
    'If self-interest dominates, the extraction-grounded authority reading is confirmed and the gate''s persistence is better modeled as enforced rent; if expertise dominates, part of the arrangement''s enforcement is epistemic division of labor rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_authority_ground, empirical, 'Expertise versus self-interest as the ground of gatekeeping authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sgc_hybrid_reading_tr_t1950, sex_gender_category__hybrid_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(sgc_hybrid_reading_tr_t1960, sex_gender_category__hybrid_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(sgc_hybrid_reading_tr_t1970, sex_gender_category__hybrid_reading, theater_ratio, 1970, 0.26).
narrative_ontology:measurement(sgc_hybrid_reading_tr_t1980, sex_gender_category__hybrid_reading, theater_ratio, 1980, 0.33).
narrative_ontology:measurement(sgc_hybrid_reading_tr_t1990, sex_gender_category__hybrid_reading, theater_ratio, 1990, 0.36).
narrative_ontology:measurement(sgc_hybrid_reading_tr_t2000, sex_gender_category__hybrid_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(sgc_hybrid_reading_tr_t2010, sex_gender_category__hybrid_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(sgc_hybrid_reading_tr_t2020, sex_gender_category__hybrid_reading, theater_ratio, 2020, 0.34).

% Extraction over time
narrative_ontology:measurement(sgc_hybrid_reading_be_t1950, sex_gender_category__hybrid_reading, base_extractiveness, 1950, 0.44).
narrative_ontology:measurement(sgc_hybrid_reading_be_t1960, sex_gender_category__hybrid_reading, base_extractiveness, 1960, 0.52).
narrative_ontology:measurement(sgc_hybrid_reading_be_t1970, sex_gender_category__hybrid_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(sgc_hybrid_reading_be_t1980, sex_gender_category__hybrid_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(sgc_hybrid_reading_be_t1990, sex_gender_category__hybrid_reading, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(sgc_hybrid_reading_be_t2000, sex_gender_category__hybrid_reading, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement(sgc_hybrid_reading_be_t2010, sex_gender_category__hybrid_reading, base_extractiveness, 2010, 0.59).
narrative_ontology:measurement(sgc_hybrid_reading_be_t2020, sex_gender_category__hybrid_reading, base_extractiveness, 2020, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sgc_hybrid_reading_su_t1950, sex_gender_category__hybrid_reading, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(sgc_hybrid_reading_su_t1960, sex_gender_category__hybrid_reading, suppression_requirement, 1960, 0.66).
narrative_ontology:measurement(sgc_hybrid_reading_su_t1970, sex_gender_category__hybrid_reading, suppression_requirement, 1970, 0.73).
narrative_ontology:measurement(sgc_hybrid_reading_su_t1980, sex_gender_category__hybrid_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(sgc_hybrid_reading_su_t1990, sex_gender_category__hybrid_reading, suppression_requirement, 1990, 0.74).
narrative_ontology:measurement(sgc_hybrid_reading_su_t2000, sex_gender_category__hybrid_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(sgc_hybrid_reading_su_t2010, sex_gender_category__hybrid_reading, suppression_requirement, 2010, 0.66).
narrative_ontology:measurement(sgc_hybrid_reading_su_t2020, sex_gender_category__hybrid_reading, suppression_requirement, 2020, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, identity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'what determines sex/gender category membership' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — one per reading of the sex_gender_category kernel. This story is the hybrid (medical-gatekeeping) reading: its epsilon (0.58) is authored over the standing gatekeeping arrangement by this reading's own lights. The biology_reading sibling authors epsilon over the same referent from the premise that birth biology decides and the gate's premise (transition changes membership) is itself the error; the identity_reading sibling authors epsilon from the premise that verification regimes are the extraction. The readings differ in victim set (all trans people / gatekept and non-transitioning trans people / those subjected to verification), in authority location (nature / the clinic / the individual), and in beneficiary structure. Genealogy: the hybrid reading differentiated out of the biology default by granting medicine a revision power the biology reading denies, and the identity reading emerged as repudiation of the hybrid gate — each sibling is linked through affects_constraints, and each sibling story should link back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
