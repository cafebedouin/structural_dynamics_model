% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__public_safety_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__public_safety_coordination, []).

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
 *   constraint_id: licensing_statute_mandate__public_safety_coordination
 *   human_readable: Statutory Credential Requirements as Consumer-Harm Prevention (Public-Safety Coordination Reading)
 *   domain: economic/political/regulatory
 *
 * SUMMARY:
 *   State credential statutes require practitioners in dozens of occupations
 *   to pass examinations, complete approved training, and maintain renewal
 *   and continuing-education compliance before serving the public lawfully.
 *   This story instantiates ONE reading of that arrangement — the
 *   public_safety_coordination reading of the licensing_statute_mandate
 *   kernel — under which the statutes solve a real information-asymmetry
 *   problem: consumers cannot evaluate competence before purchase, so a
 *   shared, state-verifiable quality threshold lets them trust any license
 *   holder and gives competent practitioners a portable signal. Per the
 *   epsilon-invariance principle, the sibling readings
 *   (rent_seeking_suppression, graduated_access_filter) are separate
 *   constraint files linked through network.affects_constraints; their
 *   epsilon values differ because they assess the same statutes through
 *   different structural lenses, and no averaging across readings occurs
 *   here. Epsilon's referent throughout is the standing arrangement — the
 *   credential statutes as they operate — assessed by this reading's own
 *   lights: genuine coordination carrying moderate threshold costs, not the
 *   endorsed alternative of pure reputation markets. KEY AGENTS (by
 *   structural relationship): - state_legislatures: Agenda setter
 *   (institutional/mobile) — enacts, amends, and sunsets credential statutes;
 *   holds the fix lever - state_licensing_boards: Administrator
 *   (institutional/constrained) — runs exams, discipline, and fee collection;
 *   funded by the arrangement it polices - licensed_incumbent_practitioners:
 *   Primary beneficiary (organized/identity_locked) — collects the trust
 *   premium and reduced competition; supplies board majorities -
 *   service_consumers: Intended beneficiary (moderate/mobile) — buys
 *   competence assurance at a price premium - threshold_screened_out_workers:
 *   Primary target (powerless/mobile) — barred from lawful practice by the
 *   threshold - informal_unlicensed_providers: Excluded actor
 *   (powerless/trapped) — serves the shadow market under enforcement risk -
 *   consumer_safety_researchers: Analytical observer (analytical/analytical)
 *   — measures harm and price effects across jurisdictions
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__public_safety_coordination, 0.38).
domain_priors:suppression_score(licensing_statute_mandate__public_safety_coordination, 0.42).
domain_priors:theater_ratio(licensing_statute_mandate__public_safety_coordination, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, extractiveness, 0.38).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__public_safety_coordination, rope).
narrative_ontology:human_readable(licensing_statute_mandate__public_safety_coordination, "Statutory Credential Requirements as Consumer-Harm Prevention (Public-Safety Coordination Reading)").
narrative_ontology:topic_domain(licensing_statute_mandate__public_safety_coordination, "economic/political/regulatory").

domain_priors:requires_active_enforcement(licensing_statute_mandate__public_safety_coordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__public_safety_coordination, 'f5e7810b-8346-44b4-8064-a3913e12090f').
narrative_ontology:cs_kernel_codification('f5e7810b-8346-44b4-8064-a3913e12090f', formalized).
narrative_ontology:cs_authority_grounding('f5e7810b-8346-44b4-8064-a3913e12090f', expertise).
narrative_ontology:cs_interpretation_layer_present('f5e7810b-8346-44b4-8064-a3913e12090f').
narrative_ontology:cs_reading_relation('f5e7810b-8346-44b4-8064-a3913e12090f', licensing_statute_mandate__rent_seeking_suppression, influences).
narrative_ontology:cs_reading_relation('f5e7810b-8346-44b4-8064-a3913e12090f', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('f5e7810b-8346-44b4-8064-a3913e12090f', foundational, incompetence_harm_preventable_by_ex_ante_screening).
narrative_ontology:cs_axiom_status(incompetence_harm_preventable_by_ex_ante_screening, holdable).
narrative_ontology:cs_axiom_grounding('f5e7810b-8346-44b4-8064-a3913e12090f', incompetence_harm_preventable_by_ex_ante_screening, empirically_contingent).
narrative_ontology:cs_axiom('f5e7810b-8346-44b4-8064-a3913e12090f', secondary, information_asymmetry_warrants_collective_gatekeeping).
narrative_ontology:cs_axiom_status(information_asymmetry_warrants_collective_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('f5e7810b-8346-44b4-8064-a3913e12090f', information_asymmetry_warrants_collective_gatekeeping, instrumental).
narrative_ontology:cs_reference_frame('f5e7810b-8346-44b4-8064-a3913e12090f', minimum_competence_consumer_protection).
narrative_ontology:cs_drift_state('f5e7810b-8346-44b4-8064-a3913e12090f', contemporary_deregulation_review_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f5e7810b-8346-44b4-8064-a3913e12090f', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, service_consumers).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, licensed_incumbent_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, threshold_screened_out_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, state_licensing_boards).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__public_safety_coordination, minimum_competence_threshold_doctrine).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__public_safety_coordination, information_asymmetry_market_failure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and amend credential statutes in response to harm incidents, professional association campaigns, and reform pressure. Hold the sunset-review and delicensing levers and can restructure or abolish any threshold at negligible fiscal cost; bear little direct cost from the arrangement either way, which makes their attention dependent on which coalition organizes most effectively.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, state_legislatures, agenda_setter,
    institutional, generational, mobile, national).

% Administer examinations, investigate complaints, discipline licensees, and collect the fees that fund their own operations. Staffed heavily by licensed incumbents appointed from the professions they regulate. Their budgets, staffing, and jurisdictional reach depend on the statutes they enforce, so the arrangement's continuation is also their institutional continuation.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, state_licensing_boards, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__public_safety_coordination, state_licensing_boards, beneficiary).

% Hold the credential that gates lawful practice and collect the trust premium and reduced competition the threshold produces. Serve as board majorities, write exam blueprints, and set continuing-education requirements. Their career investment and professional self-conception are bound to the credential, so they defend the arrangement's terms even where its compliance costs fall on them.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, licensed_incumbent_practitioners, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__public_safety_coordination, licensed_incumbent_practitioners, agenda_setter).

% Rely on the license as a low-cost signal of minimum competence when hiring doctors, electricians, contractors, and cosmetologists, and pay somewhat higher prices as the cost of that assurance. Can substitute do-it-yourself effort, informal providers, or unlicensed alternatives for many lower-stakes services, which bounds how much of the premium they must absorb.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, service_consumers, beneficiary,
    moderate, biographical, mobile, national).

% Cannot meet or cannot afford the training-hour, examination, apprenticeship, or fee requirements and are therefore barred from lawful paid practice in the occupation. Lose the income and status of the trade and bear the transition cost privately, typically re-entering adjacent unregulated work or retraining. Their exclusion is the threshold operating as designed, whatever its calibration.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, threshold_screened_out_workers, payer,
    powerless, biographical, mobile, national).

% Provide services outside the legal market under persistent complaint-driven enforcement risk, unable to advertise openly or build formal reputations. Would argue for competency-based assessment or small-operator exemptions if seated in rulemaking, but notice-and-comment processes reach few of them; their exclusion is what the enforcement machinery maintains.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, informal_unlicensed_providers, excluded,
    powerless, immediate, trapped, local).

% Study harm rates, disciplinary action volumes, and price effects across licensed and unlicensed jurisdictions, publishing the comparisons that inform sunset reviews and reform legislation. Hold no stake in market access and no vote in standard-setting; their findings enter the political process only when a legislature chooses to read them.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, consumer_safety_researchers, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__public_safety_coordination, licensed_incumbent_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__public_safety_coordination, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the information-asymmetry problem between service consumers and practitioners: consumers cannot cheaply evaluate competence before purchase, so a shared, state-verifiable quality threshold lets them trust any license holder without individual vetting and gives competent practitioners a portable, credible signal across jurisdictions.
% TRANSFER_FUNCTION: Moves lawful market access from workers without the credential to workers holding it; moves compliance costs (tuition, supervised hours, examination and renewal fees) from entrants to boards and public treasuries; and moves a measurable price premium from consumers to license holders as the price of assured minimum competence.
% ABSENT_VOICES: Screened-out workers and informal providers are absent from board rulemaking: boards are composed mainly of license holders, and notice-and-comment channels reach few of the excluded. Their objection — that thresholds exceed demonstrated harm risk and price out low-income entrants — enters the record mainly through litigation and academic studies rather than the rooms where standards are written.
% DISAPPEARANCE_RATIONALE: Overnight repeal would force rapid reconstruction: insurers, platforms, and trade associations would build reputation, bonding, and warranty substitutes within months; prices in licensed trades would fall as suppressed supply entered; and some share of consumers would be harmed by genuinely incompetent providers before substitutes matured. The size of that harm share is precisely what the sibling readings dispute, but no party contends the service economy would simply continue unchanged.
% FOUNDING_PROBLEM: Late nineteenth- and early twentieth-century markets for medicine, pharmacy, engineering, and the skilled trades were plagued by practitioners whose incompetence caused death, injury, and property loss invisible to buyers until too late — patent-medicine poisoning, unsafe wiring, collapsed structures — and no private signal available at the point of sale could separate competent from incompetent providers.
% FOUNDING_PROBLEM_CORROBORATION: Public-health agencies and product-safety investigators attest that incompetent practice still causes measurable harm (malpractice payouts, electrical-fire investigations, adverse-event reporting); economic historians corroborate the pre-licensure harm record independently of the professional associations that lobby for licensure. No party outside the beneficiary set attests the problem is fully solved, though reform economists outside the beneficiary set attest that in many low-risk occupations the problem is now smaller than the arrangement's side costs.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__public_safety_coordination, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__public_safety_coordination, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__public_safety_coordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__public_safety_coordination, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__public_safety_coordination, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__public_safety_coordination_tests).
:- end_tests(licensing_statute_mandate__public_safety_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.38: this reading sees the price uplift and access restriction that licensing measurably produces, but reads most of it as the cost of maintaining a verified threshold rather than as rent — the number is deliberately not tuned to guarantee a rope verdict, and the engine's per-seat computation may disagree. Suppression is 0.42: the legal bar on unlicensed practice is real coercion, but narrowly targeted at market entry rather than pervasive over conduct. Theater is 0.22: examination and discipline are functional adjudication, with a minority of activity (renewal paperwork, some continuing-education box-checking) performing compliance rather than assuring competence. Accessibility_collapse is 0.50 — workable alternatives persist (self-service, informal providers, substitute goods, relocation to lighter-regime states) even though formal-market entry is legally closed. Resistance is 0.40 — braiding and telehealth litigation, sunset commissions, and universal-recognition reforms are real but episodic, blunted by broad public trust in licensure. The temporal series run on one shared grid (all three metrics at all seven points) so no metric inherits another's end-state value. The suppression_requirement series is included because the story specifically traces enforcement-capacity change: boards were thin complaint bureaus in 1900, professionalized into full investigative and disciplinary machinery by 1980 (continuing-education mandates, fingerprinting, interstate discipline networks), then eased modestly after 2000 as reciprocity and universal-recognition compacts reduced per-state enforcement burden. Extraction peaks at 1980 with the great scope expansion into low-harm occupations and partially retreats thereafter under reform pressure. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the consumer seat the arrangement is a subsidy: assurance purchased at a tolerable premium, with mobile exit into substitutes keeping the relationship favorable. From the screened-out worker seat the same statute is a hard gate that confiscates a livelihood path, and mobile exit (leaving the trade) is cold comfort. From the incumbent seat the credential is both income floor and professional identity, so the seat defends the arrangement even where its own compliance costs bite. From the board seat the statutes are simply the administrative reality the office executes. The engine derives these divergent per-seat classifications from the declared power, exit, and beneficiary/victim structure; the authored rope claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Service consumers are declared beneficiaries with mobile exit, placing them near the beneficiary end of directionality — the constraint subsidizes them with assurance. Licensed incumbents are declared beneficiaries but their identity_locked exit and board seats deepen their stake in maintenance rather than in suffering extraction; they derive low-to-moderate d. Threshold-screened-out workers are declared victims: the gate extracts their market access directly, placing them near the full-target end, with mobile exit damping but not reversing the extraction they bear. Informal providers are excluded rather than coordinated — the enforcement machinery exists to keep them outside, so their exposure is high even though they are not the threshold's designed target. Boards and legislatures administer rather than collect personally; boards' fee revenue is fiduciary, funding the enforcement function itself. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct qualitative ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against two opposite errors. Against the extraction-only error: reading modern scope creep (cosmetology, floristry) as proof that the whole arrangement was always a snare erases the documented pre-licensure harm record that motivated the founding statutes. Against the coordination-only error: treating the founding problem's continued existence as proof the current thresholds still serve it ignores threshold drift away from harm evidence. The R5 interview finds the founding problem LIVE — incompetent practice still causes measurable, corroborated harm — so mandatrophy is not resolved and no sunset is declared. The drift vector to watch is piton-shaped: if harm-evidence tracking lapses entirely while boards continue administering frozen requirements (rising theater, diffuse costs borne by entrants, an administrator that could recalibrate but does not), the arrangement would decay from rope toward piton without any capturer necessary. The theater_ratio series (peaking at 0.26, settling at 0.22) shows no Goodhart crossing of 0.5, consistent with a still-functional coordination core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the public-safety coordination reading the best explanation of the licensing statutes'' persistence, or do the sibling readings (rent_seeking_suppression, graduated_access_filter) fit the same statutory record better?',
    'Comparative institutional analysis: correlate each scope expansion and fee increase with contemporaneous harm evidence versus incumbent board composition; run natural experiments from delicensing episodes and universal-recognition adoptions to see which reading predicts the observed outcomes.',
    'If the rent-seeking reading fits better, this constraint reclassifies toward tangled_rope or snare with incumbents as capturers and the safety frame as cover; if the public-safety reading fits, the rope classification stands and the siblings remain minority explanations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame omega: this file instantiates one reading of the licensing_statute_mandate kernel; the same observable record underdetermines which reading is true.').

omega_variable(
    threshold_calibration_vs_harm_evidence,
    'Are current examination, training-hour, and renewal requirements calibrated to demonstrated harm risk in each occupation, or systematically over-inclusive relative to the harm they prevent?',
    'Occupational risk stratification: regress licensure stringency against documented harm rates per occupation, controlling for training substitutes and insurance availability.',
    'Over-inclusive thresholds raise measured extraction above the coordination-cost floor and lend partial support to the graduated_access_filter observation without adopting its full frame; well-calibrated thresholds confirm the rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_vs_harm_evidence, empirical, 'Whether the quality threshold tracks harm or exceeds it.').

omega_variable(
    counterfactual_substitute_effectiveness,
    'How much of the consumer protection the license provides could reputation systems, bonding, warranties, and liability insurance reproduce absent the statutory gate?',
    'Cross-jurisdiction comparison of harm rates in the same occupations under licensed versus unlicensed regimes, and platform-era data on rating-and-bonding substitutes.',
    'High substitutability would mean the coordination function is not load-bearing, raising effective extraction and weakening the rope justification; low substitutability confirms the threshold solves a problem private signals cannot.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_substitute_effectiveness, empirical, 'Whether private-order substitutes could replace the statutory quality signal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__public_safety_coordination, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(licensing_public_safety_tr_t1900, licensing_statute_mandate__public_safety_coordination, theater_ratio, 1900, 0.15).
narrative_ontology:measurement_basis(licensing_public_safety_tr_t1900, observed).
narrative_ontology:measurement(licensing_public_safety_tr_t1935, licensing_statute_mandate__public_safety_coordination, theater_ratio, 1935, 0.18).
narrative_ontology:measurement_basis(licensing_public_safety_tr_t1935, observed).
narrative_ontology:measurement(licensing_public_safety_tr_t1960, licensing_statute_mandate__public_safety_coordination, theater_ratio, 1960, 0.22).
narrative_ontology:measurement_basis(licensing_public_safety_tr_t1960, observed).
narrative_ontology:measurement(licensing_public_safety_tr_t1980, licensing_statute_mandate__public_safety_coordination, theater_ratio, 1980, 0.26).
narrative_ontology:measurement_basis(licensing_public_safety_tr_t1980, observed).
narrative_ontology:measurement(licensing_public_safety_tr_t2000, licensing_statute_mandate__public_safety_coordination, theater_ratio, 2000, 0.24).
narrative_ontology:measurement_basis(licensing_public_safety_tr_t2000, observed).
narrative_ontology:measurement(licensing_public_safety_tr_t2015, licensing_statute_mandate__public_safety_coordination, theater_ratio, 2015, 0.23).
narrative_ontology:measurement_basis(licensing_public_safety_tr_t2015, observed).
narrative_ontology:measurement(licensing_public_safety_tr_t2025, licensing_statute_mandate__public_safety_coordination, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(licensing_public_safety_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(licensing_public_safety_be_t1900, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement_basis(licensing_public_safety_be_t1900, observed).
narrative_ontology:measurement(licensing_public_safety_be_t1935, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 1935, 0.34).
narrative_ontology:measurement_basis(licensing_public_safety_be_t1935, observed).
narrative_ontology:measurement(licensing_public_safety_be_t1960, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement_basis(licensing_public_safety_be_t1960, observed).
narrative_ontology:measurement(licensing_public_safety_be_t1980, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 1980, 0.46).
narrative_ontology:measurement_basis(licensing_public_safety_be_t1980, observed).
narrative_ontology:measurement(licensing_public_safety_be_t2000, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement_basis(licensing_public_safety_be_t2000, observed).
narrative_ontology:measurement(licensing_public_safety_be_t2015, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement_basis(licensing_public_safety_be_t2015, observed).
narrative_ontology:measurement(licensing_public_safety_be_t2025, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 2025, 0.38).
narrative_ontology:measurement_basis(licensing_public_safety_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(licensing_public_safety_su_t1900, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 1900, 0.25).
narrative_ontology:measurement_basis(licensing_public_safety_su_t1900, observed).
narrative_ontology:measurement(licensing_public_safety_su_t1935, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 1935, 0.32).
narrative_ontology:measurement_basis(licensing_public_safety_su_t1935, observed).
narrative_ontology:measurement(licensing_public_safety_su_t1960, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement_basis(licensing_public_safety_su_t1960, observed).
narrative_ontology:measurement(licensing_public_safety_su_t1980, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement_basis(licensing_public_safety_su_t1980, observed).
narrative_ontology:measurement(licensing_public_safety_su_t2000, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 2000, 0.47).
narrative_ontology:measurement_basis(licensing_public_safety_su_t2000, observed).
narrative_ontology:measurement(licensing_public_safety_su_t2015, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement_basis(licensing_public_safety_su_t2015, observed).
narrative_ontology:measurement(licensing_public_safety_su_t2025, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(licensing_public_safety_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__public_safety_coordination, information_standard).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, rent_seeking_suppression).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, graduated_access_filter).

% DUAL FORMULATION NOTE:
% The colloquial label 'occupational licensing' decomposes into three structurally distinct claims about the same statutes: harm-prevention coordination (this file, epsilon 0.38), incumbent rent extraction (rent_seeking_suppression, substantially higher epsilon with incumbents as capturers), and class-sorted access filtering (graduated_access_filter, epsilon indexed to regressive barrier incidence). Each member of the family carries its own epsilon, beneficiary/victim structure, and classification; they are linked because each cites the same statutory record as evidence, and the upstream safety frame's legitimacy feeds the downstream readings' explanatory targets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
