% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Categorical Prohibition on Intentional Life-Ending (Sanctity-of-Life Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint models the sanctity-of-life reading of the end-of-life
 *   authority kernel: the position that intentional life-ending is
 *   categorically prohibited regardless of individual preference, grounded in
 *   the intrinsic and inviolable value of human life rather than in a
 *   calculation of the patient's own welfare or wishes. This is one of three
 *   structurally distinct readings of the same underlying contested kernel
 *   (end_of_life_authority). The autonomy_reading treats the same standing
 *   arrangement as an unjustified restriction on a liberty interest; the
 *   slippery_slope_mechanism reading treats permissive alternatives as
 *   empirically prone to scope expansion. Each reading is authored as its own
 *   constraint with its own epsilon, per the ε-invariance principle — this
 *   file does not average across them or hedge its extraction value to
 *   accommodate the sibling positions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.58).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.71).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Categorical Prohibition on Intentional Life-Ending (Sanctity-of-Life Reading)").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '622ed2f3-e19e-4ecc-9212-6701fdc0c99b').
narrative_ontology:cs_kernel_codification('622ed2f3-e19e-4ecc-9212-6701fdc0c99b', distributed).
narrative_ontology:cs_authority_grounding('622ed2f3-e19e-4ecc-9212-6701fdc0c99b', distributed).
narrative_ontology:cs_reading_relation('622ed2f3-e19e-4ecc-9212-6701fdc0c99b', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('622ed2f3-e19e-4ecc-9212-6701fdc0c99b', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('622ed2f3-e19e-4ecc-9212-6701fdc0c99b', foundational, human_life_possesses_intrinsic_inviolable_value).
narrative_ontology:cs_axiom_status(human_life_possesses_intrinsic_inviolable_value, holdable).
narrative_ontology:cs_axiom_grounding('622ed2f3-e19e-4ecc-9212-6701fdc0c99b', human_life_possesses_intrinsic_inviolable_value, deontological).
narrative_ontology:cs_axiom('622ed2f3-e19e-4ecc-9212-6701fdc0c99b', secondary, medical_role_categorically_excludes_intentional_life_ending).
narrative_ontology:cs_axiom_status(medical_role_categorically_excludes_intentional_life_ending, holdable).
narrative_ontology:cs_axiom_grounding('622ed2f3-e19e-4ecc-9212-6701fdc0c99b', medical_role_categorically_excludes_intentional_life_ending, conventional).
narrative_ontology:cs_reference_frame('622ed2f3-e19e-4ecc-9212-6701fdc0c99b', categorical_life_preservation_mandate).
narrative_ontology:cs_drift_state('622ed2f3-e19e-4ecc-9212-6701fdc0c99b', contemporary_assisted_dying_legalization_wave, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('622ed2f3-e19e-4ecc-9212-6701fdc0c99b', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_and_disability_advocacy_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, medical_licensing_bodies).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, competent_terminally_ill_patients_seeking_death).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, chronic_intractable_suffering_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, elderly_disabled_and_economically_disadvantaged_populations).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, intrinsic_human_dignity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Facing a terminal or unbearably burdensome condition, judged mentally competent, they request assistance in ending their life on their own terms. The prohibition forecloses this option entirely regardless of their stated wishes; their remaining paths are continued suffering, unassisted self-harm (often more violent and less certain), palliative sedation, or travel to a permissive jurisdiction if they have the means and mobility to do so.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, competent_terminally_ill_patients_seeking_death, payer,
    powerless, immediate, trapped, national).

% Living with non-terminal but intractable suffering (severe chronic pain, progressive degenerative disease), they are further from the margins of any conceivable exception and are governed by the same categorical bar. They have no legal exit within the jurisdiction and bear the extraction as prolonged suffering with no sanctioned relief.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, chronic_intractable_suffering_patients, payer,
    powerless, biographical, trapped, national).

% Structurally vulnerable to being steered, subtly or overtly, toward ending their lives by family burden, cost pressures, or systemic undervaluation of their lives. The categorical bar is the mechanism the sanctity reading offers as their protection — under this reading they benefit from a prohibition that forecloses an exit others would want, because the same door that offers relief to a confident autonomous patient offers coercion to a pressured one. Whether they in fact benefit or are merely denied an option they too might want is exactly the reading's central contested claim.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, elderly_disabled_and_economically_disadvantaged_populations, beneficiary,
    powerless, generational, trapped, national).

% Lobby for and defend the categorical prohibition in legislatures and courts, framing it as protection of the vulnerable and affirmation of inherent human worth. They shape statutory language, provide expert testimony, and mobilize public opinion. Their institutional standing and mission legitimacy are partly constituted by successfully holding this line; a change in the law would not physically harm them but would represent a defeat of their core normative project.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_and_disability_advocacy_institutions, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, religious_and_disability_advocacy_institutions, beneficiary).

% Bound by professional codes and licensing law to a life-preservation mandate; participating in intentional life-ending, even at a competent patient's explicit request, risks license revocation and criminal liability. Their professional identity and legal exposure are structured entirely around the preservation mandate, which the sanctity reading enforces through licensing boards and medical association ethics codes.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, physicians_and_medical_licensing_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicate challenges to the prohibition, weigh competing constitutional claims (liberty interest vs. state interest in preserving life), and can alter the arrangement through statute or constitutional ruling. They receive testimony from all sides and are the mechanism through which the kernel's readings actually compete for legal supremacy.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, legislatures_and_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, uniformly enforceable rule that removes case-by-case discretion over who may authorize an intentional death, which in principle protects against error, coercion, and misjudgment in irreversible decisions, and coordinates medical practice around a single unambiguous mandate (preserve life) rather than a contestable case-by-case weighing.
% TRANSFER_FUNCTION: Moves the burden of unrelieved suffering from the state and medical system (which would otherwise have to build, fund, and defend a case-by-case authorization apparatus) onto the individual patient who wants assistance dying; simultaneously moves a claimed protective benefit toward vulnerable populations at large by foreclosing the coercion pathway for everyone, including those who were never at coercion risk.
% ABSENT_VOICES: Competent patients currently suffering are represented in litigation and advocacy but are frequently dead or too incapacitated to testify by the time cases resolve; disabled and elderly people who WOULD want the option (as opposed to those the reading claims to protect) are largely absent from advocacy institutions that claim to speak for the whole vulnerable population — the sanctity reading's beneficiary class and its self-appointed representatives are not identical.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, jurisdictions would default to whatever framework replaced it (most plausibly an autonomy-reading regime with eligibility screening); medical licensing bodies would need new protocols, disability and religious advocacy organizations would lose a central organizing cause, and some competent terminally-ill patients would gain access to assisted death while advocates warn of downstream expansion risk to non-terminal and incompetent populations.
% FOUNDING_PROBLEM: Historically built to prevent the medical profession and the state from ever being positioned as arbiters of which lives are worth continuing — a bright-line response to eugenics-era and disability-devaluing precedents where 'quality of life' judgments by authorities became instruments of exclusion and harm.
% FOUNDING_PROBLEM_CORROBORATION: Disability rights scholars and historians of eugenics-era medicine (a constituency outside the beneficiary set of religious advocacy institutions, though overlapping with disability advocacy) corroborate that the founding problem — professional and state authority over which lives merit continuation — remains structurally live wherever cost pressures on healthcare systems persist; competent-patient advocacy groups and several jurisdictions' legislative review commissions dispute that a categorical bar is still the necessary or least-restrictive response to that problem, citing decades of operating permissive regimes with eligibility safeguards short of an absolute prohibition.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is authored as moderate-high: from the sanctity reading's own lights, the arrangement extracts real welfare from competent suffering patients who are denied an outcome they affirmatively want, in service of a protective function for a different population. Suppression (0.71) is high because the prohibition is backed by criminal law, licensing sanctions, and active enforcement against physicians and patients who attempt to circumvent it — this is not a passive default but an actively policed boundary. Theater ratio is low-moderate (0.22) because the enforcement machinery (licensing review, criminal prosecution) performs real functional work rather than mere performance, though some of the compliance apparatus (ethics review boards that never authorize exceptions) drifts toward ritual over the measured interval. accessibility_collapse (0.62) reflects that once the prohibition is enforced, patients genuinely have no legal path to the outcome they seek within the jurisdiction — alternatives (unassisted self-harm, palliative sedation, cross-border travel) are real but categorically inferior or inaccessible to most. resistance (0.55) reflects sustained, organized litigation and advocacy pressure against the prohibition from patient-autonomy movements, which is real and escalating but has not (in most jurisdictions modeled here) succeeded in overturning the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent terminally-ill patients and chronic-suffering patients are declared victims: the constraint extracts directly from them by categorically foreclosing an outcome they want, with no meaningful exit (trapped) since the prohibition is jurisdiction-wide and criminal in character. Religious and disability advocacy institutions and medical licensing bodies are declared beneficiaries/agenda-setters: they do not bear the extraction and their institutional legitimacy is partly constituted by maintaining the rule. Elderly, disabled, and economically disadvantaged populations are declared beneficiaries under this reading's own logic (the rule is claimed to protect them from coercion) — this is the single most contested directionality claim in the story, since the same population that is claimed to benefit also contains people who are denied an exit they too would want; this tension is deliberately not resolved in this constraint's metrics (that resolution belongs to the omega below and to the sibling slippery_slope_mechanism reading).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing medical/state authorities from making life-worth judgments that echo eugenics-era harms) is coded contested rather than resolved: disability-rights historians outside the beneficiary set corroborate the problem's continued relevance, while competent-patient advocates and legislative review commissions argue the categorical form of the response is no longer the least-restrictive available means, given decades of permissive regimes operating with eligibility safeguards short of absolute prohibition. The tangled_rope classification captures this precisely: there is a genuine coordination function (protecting against coercion of the vulnerable, providing bright-line clarity for medical practice) operating through the same structure that extracts from a different population (competent patients denied their preference) — this is not a costume over pure extraction, nor is it costless coordination; it is both at once, which is exactly what tangled_rope is built to represent and what a pure snare or pure rope classification would mislabel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vulnerable_population_benefit_or_denial,
    'Does the categorical prohibition genuinely protect elderly, disabled, and economically disadvantaged people from coercion, or does it merely deny them an option that some of them would also want, using their vulnerability as the justification for a rule that primarily protects the rule-makers'' institutional position?',
    'Comparative empirical study of jurisdictions with permissive-but-safeguarded assisted-dying regimes: measure whether documented coercion incidents among vulnerable populations are higher there than in prohibition jurisdictions, controlling for reporting differences and baseline social support levels.',
    'If coercion rates are not measurably higher under safeguarded permissive regimes, the sanctity reading''s core protective claim for this population weakens substantially and the beneficiary declaration for elderly/disabled/economically disadvantaged groups becomes harder to sustain, pushing the classification toward a purer extraction (snare) profile. If coercion rates are meaningfully higher, the tangled_rope coordination function is empirically vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_benefit_or_denial, empirical, 'Whether the prohibition''s protective claim for vulnerable populations is empirically supported or a justificatory cover.').

omega_variable(
    sanctity_reading_kernel_disambiguation,
    'Is the sanctity reading a distinct normative framework from the autonomy reading and slippery-slope mechanism, or are all three better understood as different empirical predictions about the consequences of a single underlying permissive-vs-prohibitive policy choice?',
    'Track whether sanctity-reading proponents shift their argument in response to empirical safeguard data (suggesting it is actually a consequentialist/empirical claim in disguise) or hold the categorical position regardless of empirical outcomes (confirming it is a genuinely distinct deontological framework).',
    'If the sanctity reading is empirical-in-disguise, it should be merged analytically with the slippery_slope_mechanism reading rather than treated as an independently grounded normative position, though per the ε-invariance principle these remain separately authored constraint files regardless.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctity_reading_kernel_disambiguation, conceptual, 'Whether the sanctity reading is a genuinely distinct deontological kernel-reading or a restated empirical/consequentialist claim.').

omega_variable(
    physician_role_identity_lock,
    'Is the physician''s life-preservation mandate under this reading better understood as externally imposed suppression (licensing law and criminal liability) or as an internalized professional identity that would persist even if the legal prohibition were lifted?',
    'Survey physicians in newly permissive jurisdictions on participation rates in assisted-dying programs where legally available versus opt-out rates, to determine whether the preservation mandate is held internally independent of legal requirement.',
    'If largely internalized, removing the legal prohibition would produce far less behavioral change among physicians than the suppression metric alone would predict, since much of the constraint''s force operates through professional identity rather than external enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physician_role_identity_lock, empirical, 'Structural (legal/licensing) versus internalized (professional identity) suppression mechanism for physicians.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(end__tr_t8, end_of_life_authority__sanctity_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(end__tr_t16, end_of_life_authority__sanctity_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(end__tr_t24, end_of_life_authority__sanctity_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(end__tr_t32, end_of_life_authority__sanctity_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__sanctity_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(end__be_t8, end_of_life_authority__sanctity_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(end__be_t16, end_of_life_authority__sanctity_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(end__be_t24, end_of_life_authority__sanctity_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(end__be_t32, end_of_life_authority__sanctity_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__sanctity_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(end__su_t8, end_of_life_authority__sanctity_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(end__su_t16, end_of_life_authority__sanctity_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(end__su_t24, end_of_life_authority__sanctity_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(end__su_t32, end_of_life_authority__sanctity_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__sanctity_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the end_of_life_authority kernel. autonomy_reading treats the identical standing arrangement as an unjustified extraction on patient liberty (high epsilon from the target's perspective, victim set = suffering patients denied choice). slippery_slope_mechanism treats permissive alternatives to this arrangement as the object of concern, evaluating empirical scope-creep risk rather than the prohibition's own legitimacy. Each reading is authored with its own epsilon and its own beneficiary/victim structure per the ε-invariance principle; none is a measurement of the others taken from a different angle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
