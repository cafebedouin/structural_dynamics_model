% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Health Intervention Legitimacy — Proportionality Reading (Threat-Calibrated Coercion)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates one reading of the kernel governing when coercive
 *   health measures are legitimate: the proportionality reading, under which
 *   an intervention is legitimate when its severity tracks the demonstrated
 *   threat — transmissibility, case-fatality, controllability — with
 *   population harm and individual autonomy each carrying real but
 *   threat-weighted value. The constraint operates as a conditional license:
 *   health authorities issue measures sized to a judged threat; review bodies
 *   test the sizing; measures are supposed to retract as threat recedes.
 *   Because the license is conditional, the burden structure rotates — a
 *   severe pathogen licenses broad measures whose burdens track genuine
 *   protection for a small victim set, while a mild-pathogen context licenses
 *   little, so any measure that persists or oversizes there is nearly pure
 *   burden borne by a broad victim set. The authored epsilon (0.47) is the
 *   cycle-average value of that conditional structure under observed
 *   behavior: genuine protection delivered during well-matched periods,
 *   overshoot during emergencies, and residual mandates persisting into calm.
 *   KEY AGENTS (by structural relationship): public_health_authorities:
 *   agenda-setting enforcer (institutional/constrained) — sizes, issues, and
 *   retains measures; immunocompromised_and_high_risk_residents: primary
 *   intended beneficiary (organized/trapped); hourly_low_wage_workers:
 *   primary recurring burden bearer (powerless/constrained);
 *   liberty_objecting_residents: contested payers who litigate sizing
 *   (moderate/constrained); resource_poor_detained_individuals: excluded
 *   burden bearers (powerless/trapped); general_compliant_public:
 *   dual-positioned beneficiary-and-cost-bearer (organized/constrained);
 *   delegating_elected_legislatures: indirect beneficiary via blame
 *   delegation (institutional/constrained); constitutional_review_judiciary:
 *   analytical observer adjudicating individual sizings
 *   (institutional/analytical).
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda-setting enforcer (institutional/constrained) — declares threat levels, issues and lifts measures, retains unwithdrawn instruments; collects discretion and emergency budget
 *   - immunocompromised_and_high_risk_residents: primary intended beneficiary (organized/trapped) — protection arrives only through others' compliance; cannot exit vulnerability
 *   - hourly_low_wage_workers: primary recurring payer (powerless/immediate-horizon/constrained) — compliance costs scale inversely with income; secondary beneficiary during matched severe outbreaks
 *   - liberty_objecting_residents: contested payers (moderate/biographical/constrained) — bear fines, job actions, and social friction under orders they judge oversized; partially vindicated through litigation
 *   - resource_poor_detained_individuals: excluded burden bearers (powerless/local/trapped) — confined with least access to counsel or appeal; least present in the calibration conversation
 *   - general_compliant_public: dual-positioned (organized/constrained) — receives protection, absorbs diffuse costs, sets the political ceiling on stringency
 *   - delegating_elected_legislatures: indirect beneficiary (institutional/biographical/constrained) — statutory framers who offload contested sizing calls and their electoral costs
 *   - constitutional_review_judiciary: analytical observer (institutional/generational/analytical) — upholds or strikes individual orders case-by-case; rulings accumulate into binding doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.47).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.49).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.49).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Health Intervention Legitimacy — Proportionality Reading (Threat-Calibrated Coercion)").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '86c56884-92e2-419a-ae86-35ee9cf1a39d').
narrative_ontology:cs_kernel_codification('86c56884-92e2-419a-ae86-35ee9cf1a39d', formalized).
narrative_ontology:cs_authority_grounding('86c56884-92e2-419a-ae86-35ee9cf1a39d', distributed).
narrative_ontology:cs_reading_relation('86c56884-92e2-419a-ae86-35ee9cf1a39d', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('86c56884-92e2-419a-ae86-35ee9cf1a39d', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('86c56884-92e2-419a-ae86-35ee9cf1a39d', foundational, coercion_legitimate_only_when_proportional_to_threat).
narrative_ontology:cs_axiom_status(coercion_legitimate_only_when_proportional_to_threat, holdable).
narrative_ontology:cs_axiom_grounding('86c56884-92e2-419a-ae86-35ee9cf1a39d', coercion_legitimate_only_when_proportional_to_threat, deontological).
narrative_ontology:cs_axiom('86c56884-92e2-419a-ae86-35ee9cf1a39d', secondary, least_restrictive_effective_means_required).
narrative_ontology:cs_axiom_status(least_restrictive_effective_means_required, holdable).
narrative_ontology:cs_axiom_grounding('86c56884-92e2-419a-ae86-35ee9cf1a39d', least_restrictive_effective_means_required, instrumental).
narrative_ontology:cs_reference_frame('86c56884-92e2-419a-ae86-35ee9cf1a39d', jacobson_era_balanced_police_powers).
narrative_ontology:cs_drift_state('86c56884-92e2-419a-ae86-35ee9cf1a39d', contemporary_post_emergency_review_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('86c56884-92e2-419a-ae86-35ee9cf1a39d', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, immunocompromised_and_high_risk_residents).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, general_compliant_public).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, delegating_elected_legislatures).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, hourly_low_wage_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, liberty_objecting_residents).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, resource_poor_detained_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, hourly_low_wage_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, general_compliant_public).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, least_restrictive_effective_means_principle).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, necessity_and_severity_balance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run surveillance, declare outbreak thresholds, and issue or lift orders — isolation directives, closure ranges, vaccination requirements — sized to the judged threat. Their discretion is bounded by review: orders can be challenged, and challenge outcomes feed back into future calibration. During declared emergencies their staffing and budgets expand sharply; afterward they retain whatever instruments were not explicitly withdrawn, and professional identity rewards retaining capability.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Depend on others' compliance with protective measures they cannot personally effect. When issued measures match the threat, they can work, shop, and gather; when measures are lifted early or never issued, their exposure is set by strangers' choices. They cannot exit their vulnerability, and although advocacy organizations speak for them effectively, no organization can shield them body-by-body.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, immunocompromised_and_high_risk_residents, beneficiary,
    organized, biographical, trapped, national).

% Face compliance costs that scale inversely with income: an isolation directive costs wages, a school closure costs childcare, a workplace mandate can cost the job. During severe outbreaks the same measures protect them and their households; when measures outlast the threat or exceed it, the costs continue without the offsetting protection. Switching employers does not escape the orders, only changes which supervisor enforces them.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, hourly_low_wage_workers, payer,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, hourly_low_wage_workers, beneficiary).

% Judge particular orders disproportionate to the demonstrated threat and refuse, petition, organize, or litigate. Some obtain exemptions or win legal challenges; most comply under protest. Their costs — fines, employment actions, social friction — attach to the specific order they contest, and their success rate tracks the quality of the threat evidence behind each measure, which they often cannot inspect before complying.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, liberty_objecting_residents, payer,
    moderate, biographical, constrained, national).

% Are confined under isolation or quarantine directives with the least access to counsel, communication, or appeal. Whatever review process exists formally, they rarely reach it before release; their experience of an order is a closed room, a lost paycheck, and a date they did not choose. They would object with specificity about conditions and duration if the calibration conversation ever reached them; it generally reaches them as a completed act.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, resource_poor_detained_individuals, excluded,
    powerless, immediate, trapped, local).

% Follow issued measures as asked, gaining protection during severe outbreaks and absorbing diffuse costs otherwise — taxes funding enforcement, accumulated inconvenience, and the gradual normalization of being managed. Their aggregate compliance is what makes any measure function, and their tolerance sets the political ceiling on how stringent any order can get.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, general_compliant_public, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, general_compliant_public, payer).

% Write the statutory frames that authorize health orders, then step back from individual sizing decisions. Delegation routes contested calls to technical bodies and courts; when an order misfires, the electoral cost lands there too. Amending the frame to tighten or loosen the criterion is always procedurally available and almost never chosen, because owning the trade-off openly is more expensive than renting it out.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, delegating_elected_legislatures, beneficiary,
    institutional, biographical, constrained, national).

% Hear challenges to specific orders, compare the asserted threat against the imposed severity under precedent, and uphold or strike. They administer nothing and collect nothing; their rulings accumulate into the doctrine that disciplines future sizing. Case-by-case posture means they see the tail of the distribution — the contested orders — and not the routine operation that never reaches them.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, constitutional_review_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives officials, courts, and publics a shared, reviewable rule for when coercive measures are warranted and how large they may be: severity must track demonstrated threat magnitude, with both population harm and personal autonomy carrying real but disease-weighted value. It converts an unbounded 'protect the public at any cost' mandate and an absolute 'never coerce' prohibition into a decidable per-pathogen, per-moment question with a dispute mechanism attached.
% TRANSFER_FUNCTION: Moves coercive burden onto whoever falls inside a licensed measure's reach at the prevailing threat level: confinement, movement restriction, occupational mandates, and closure orders transfer liberty and income from constrained residents toward collective protection, while enforcement and adjudication costs transfer to taxpayers and courts. Which residents pay rotates with disease characteristics: marginal-risk minorities pay during severe outbreaks; broad publics pay when measures outlive or outrun the threat.
% ABSENT_VOICES: Confined individuals without counsel, hourly workers whose compliance costs are ruinous, and residents of under-protected regions during under-response phases each object from positions the sizing conversation rarely reaches; their objections surface late — usually as litigation after harm is done — and the excluded detainee seat is structurally silent for the duration of the very orders it would contest.
% DISAPPEARANCE_RATIONALE: Overnight removal of the proportionality criterion leaves the authorization question unanswered: jurisdictions drift toward whichever remaining premise they already lean on — protective coercion without bound where population benefit is treated as sovereign, or categorical refusal where autonomy is treated as sovereign — and the current mixed practice of calibrated, reviewable, retractable orders dissolves into one pole or the other. High-risk residents lose either their protection or their protections' limits; objectors lose the doctrinal handle that makes their contests winnable.
% FOUNDING_PROBLEM: Historical quarantine and sanitary regimes were simultaneously indispensable against epidemic catastrophe and routinely abused against the poor, the foreign, and the politically inconvenient; constitutional settlement required a rule that preserved the coercive power while making every exercise of it contestable after the fact.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: court decisions in both directions (upholding demonstrably matched orders, striking overbroad ones), official post-emergency inquiries documenting miscalibration and retraction failure, and bioethics and constitutional scholarship independent of the operating agencies all attest both the founding problem and its continued liveness. No attestation originating inside the health bureaucracy is counted as dispositive, since the agencies benefit from the criterion's persistence.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.47, the late-cycle value: the criterion delivers genuine protection during matched threat periods (pulling epsilon down), but observed operation adds three extraction channels — emergency overshoot, asymmetric retraction speed, and regressive compliance costs (pushing it up). Suppression 0.49 reflects enforcement that is real (fines, confinement, employment consequences, police powers) but bounded by least-restrictive-means expectations; it is authored as a raw structural property and left unscaled — the engine scales only extraction by directionality and scope. Theater 0.34 captures visible-compliance ritual and residual symbolic mandates between crises. Accessibility collapse 0.45: the criterion itself demands preserved alternatives (exemption processes, judicial appeal, sunset reviews), so alternatives narrow but do not close. Resistance 0.6: persistent objection movements and litigation contest specific sizings rather than the framework's existence. CYCLICAL PATTERN: the ten-point series spans one full threat cycle (baseline, emergence, crisis peak, overshoot, retreat, recalibration, residual-linger, normalization, minor resurgence). The oscillator is driven by threat cycles plus institutional response lag plus liability asymmetry; crucially, the oscillation itself functions as an extraction mechanism — measures deploy in weeks but retract in years, so each cycle nets a ratchet of retained instruments and normalized practices (intermittent imposition with slow release). Scalars are measured at t=18, early in a minor resurgence after recalibration, hence the mid-band values rather than crisis peaks. Claim/metric independence: claimed_type states the structure believed true; metrics state the operation believed descriptive; neither was tuned toward the other or toward a predicted engine verdict.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structure. From the authority seat the arrangement is a disciplined discretionary toolkit whose abuses are reviewable exceptions; from the high-risk seat it is the difference between participating in public life and house arrest by others' choices; from the hourly-worker seat it is uncompensated exposure whose timing never matches payroll; from the objecting resident's seat it is an arbitrary line drawn through their life by a threat estimate they cannot inspect. SAME-LEVEL DYNAMICS: two institutionally powerful seats diverge sharply — the judiciary holds analytical exit (decides case-by-case, administers nothing, accumulates doctrine), while legislatures hold nominally superior amendment power they rarely exercise because delegation insulates them electorally; equal global standing, radically different lived constraint, driven by exit structure rather than power. COALITION NOTE: the payer seats are disjoint in ordinary politics but episodically align — labor organizations and civil-liberties litigators converged against specific overbroad orders during the last cycle — which is the principal channel through which powerless payers convert into effective resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to real structure: high-risk residents and the compliant public derive low directionality (subsidized — protection flows to them), legislatures derive low-moderate d (indirect blame-insurance benefit), while hourly workers, liberty objectors, and detained individuals derive high d (they bear confinement, income loss, and fines). Dual-positioned seats (workers with secondary beneficiary status; the compliant public with secondary payer status) sit midband, which is why their situations describe both flows. Authorities combine agenda-setter and beneficiary positions — derivation would place them near the subsidy end, but review pressure genuinely caps their discretion, so their effective position is beneficiary-with-overhead. Suppression is declared raw and unscaled; extraction scales with directionality and scope. Scope note: most seats sit at national scope (modest amplification); the detained seat sits at local scope, and the engine's scope damping likely understates their effective burden — their situation is verification-opaque precisely because it is localized and unwitnessed. Receipt surface: the arrangement's surplus — retained instruments, expanded emergency capacity, delegated insulation — demonstrably accrues to the authorities' seat, so gain_flow names public_health_authorities; legislatures collect blame-insurance but the primary accrual seat is the agencies. Fixing cost is prohibitive: perfecting calibration (real-time threat assessment, automated sunsets, compensation funds) is technically uncertain and would force legislatures to re-own contested calls and agencies to surrender discretion — costs borne by the fixers exceed what fixing returns to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabelings. Reading this as pure coordination would erase the rotating victim set, the regressive burden distribution, and the retraction ratchet; reading it as pure extraction would erase the genuine epidemic-control function that protects the high-risk seat and that no alternative arrangement replicates. Tangled rope keeps both faces load-bearing: coordination function (a shared, reviewable rule converting an unbounded mandate and an absolute prohibition into a decidable per-pathogen question) plus asymmetric extraction (whoever sits inside an oversized or persisted measure pays through the same structure that protects others). Mandatrophy status is clean at the frame level: the founding problem — bounding epidemic coercion while preserving it — is live, corroborated externally, and not obsolete. The transient piton-shaped signature appears WITHIN the cycle, not at the frame level: during the residual-linger phase (t=10–14) theater rises to 0.46 while function atrophies, and the seats that could retract measures bear less cost from keeping them than from the political effort of withdrawal — classic cost-asymmetry. That phase resolves through recalibration rather than mandate death, which is why mandatrophy_resolved is not declared: the mandate outlives only its per-episode instances, not its governing function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'This story instantiates the proportionality reading of the legitimate_health_intervention kernel; which structural elements would change if a sibling reading governed instead?',
    'Comparative generation of the sibling stories against the same referent: if bodily_autonomy_primary governed, the proportional band disappears and all coercion becomes burden-bearing; if public_health_primary governed, the autonomy term drops out and coercion is licensed wherever population benefit is positive. The disagreement''s location is the weighting function between population harm and autonomy, and the seat that sets the weights.',
    'Sibling governance changes the victim set and epsilon wholesale: categorical-autonomy governance maximizes burden on the unprotected; population-primacy governance maximizes burden on refusing individuals. This reading''s classification is valid only for the proportionality instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer-frame positioning: one reading of a contested kernel, with sibling readings as separate constraints.').

omega_variable(
    epsilon_conditionality_by_pathogen_profile,
    'Does the burden profile of proportionality-governed intervention vary systematically with pathogen characteristics (transmissibility, case-fatality, controllability) enough that the single authored epsilon misrepresents the pole cases?',
    'Stratified audit of archived order sets computing imposed severity minus threat-justified severity per pathogen profile; if stratum values diverge beyond noise, decompose into per-regime stories linked by network edges.',
    'At the severe-disease pole the regime approaches pure coordination (small victim set, high justification); at the mild-disease pole with persisted measures the burden approaches pure imposition. The mid-band value stands only as a cycle-average; decomposition would yield different classifications per stratum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_conditionality_by_pathogen_profile, empirical, 'Whether the conditional constraint structure hides multiple epsilons behind one averaged value.').

omega_variable(
    calibration_error_direction_bias,
    'Do real-world proportionality systems err systematically toward over-intervention (institutional self-protection, liability avoidance) or toward under-intervention?',
    'Retrospective comparison of order stringency against ex-post threat estimates across episodes, controlling for the information available at decision time.',
    'Systematic over-intervention concentrates effective burden on liberty-objecting and hourly-worker seats; systematic under-intervention concentrates mortality on high-risk seats — flipping which seats compute as victims and moving the classification along the extraction spectrum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_error_direction_bias, empirical, 'Directional bias in threat-to-severity calibration.').

omega_variable(
    weighting_function_authority,
    'Who sets the population-harm versus individual-autonomy weights inside proportionality judgments — technical agencies, legislatures, or courts?',
    'Trace statutory delegation chains and judicial deference patterns across jurisdictions; identify whether weight-setting is captured by the operating agencies or held by deliberative bodies.',
    'Agency-set weights raise effective extraction (the operator grades its own homework); legislature- or court-set weights distribute the legitimacy load and lower it. The receipt-of-gain attribution depends materially on this answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weighting_function_authority, preference, 'Political locus of the weighting function inside the proportionality criterion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lhi_proportionality_tr_t0, legitimate_health_intervention__proportionality_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(lhi_proportionality_tr_t2, legitimate_health_intervention__proportionality_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(lhi_proportionality_tr_t4, legitimate_health_intervention__proportionality_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(lhi_proportionality_tr_t6, legitimate_health_intervention__proportionality_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(lhi_proportionality_tr_t8, legitimate_health_intervention__proportionality_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(lhi_proportionality_tr_t10, legitimate_health_intervention__proportionality_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(lhi_proportionality_tr_t12, legitimate_health_intervention__proportionality_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(lhi_proportionality_tr_t14, legitimate_health_intervention__proportionality_reading, theater_ratio, 14, 0.46).
narrative_ontology:measurement(lhi_proportionality_tr_t16, legitimate_health_intervention__proportionality_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(lhi_proportionality_tr_t18, legitimate_health_intervention__proportionality_reading, theater_ratio, 18, 0.34).

% Extraction over time
narrative_ontology:measurement(lhi_proportionality_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lhi_proportionality_be_t2, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(lhi_proportionality_be_t4, legitimate_health_intervention__proportionality_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(lhi_proportionality_be_t6, legitimate_health_intervention__proportionality_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(lhi_proportionality_be_t8, legitimate_health_intervention__proportionality_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(lhi_proportionality_be_t10, legitimate_health_intervention__proportionality_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(lhi_proportionality_be_t12, legitimate_health_intervention__proportionality_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(lhi_proportionality_be_t14, legitimate_health_intervention__proportionality_reading, base_extractiveness, 14, 0.46).
narrative_ontology:measurement(lhi_proportionality_be_t16, legitimate_health_intervention__proportionality_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement(lhi_proportionality_be_t18, legitimate_health_intervention__proportionality_reading, base_extractiveness, 18, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(lhi_proportionality_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lhi_proportionality_su_t2, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2, 0.4).
narrative_ontology:measurement(lhi_proportionality_su_t4, legitimate_health_intervention__proportionality_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(lhi_proportionality_su_t6, legitimate_health_intervention__proportionality_reading, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(lhi_proportionality_su_t8, legitimate_health_intervention__proportionality_reading, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(lhi_proportionality_su_t10, legitimate_health_intervention__proportionality_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(lhi_proportionality_su_t12, legitimate_health_intervention__proportionality_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(lhi_proportionality_su_t14, legitimate_health_intervention__proportionality_reading, suppression_requirement, 14, 0.44).
narrative_ontology:measurement(lhi_proportionality_su_t16, legitimate_health_intervention__proportionality_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(lhi_proportionality_su_t18, legitimate_health_intervention__proportionality_reading, suppression_requirement, 18, 0.49).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Constraint family: legitimate_health_intervention decomposes into three structurally distinct constraints, one per reading of the kernel. Each member carries its own epsilon, beneficiary/victim structure, and claimed type over the same referent (standing coercive health-intervention practice): this proportionality story authors mid-band conditional extraction with a rotating victim set; the public_health_primary story authors a near-coordination profile (its endorsed criterion licenses what it observes); the bodily_autonomy_primary story authors uniformly high extraction (every coercive instrument is burden by its lights). Upstream/downstream: public_health_primary supplies the evidentiary infrastructure (surveillance, burden estimates) that proportionality consumes as input, so its story links downward into this one; bodily_autonomy_primary supplies the rights vocabulary proportionality's autonomy term borrows. The colloquial label 'legitimate health intervention' conflates the three; the epsilon differences between them are the signal that the label, not the mathematics, was doing the merging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
