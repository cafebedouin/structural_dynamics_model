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
 *   human_readable: Statutory Credential Requirements — Public-Safety Coordination Reading
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   Statutory credential requirements condition entry into licensed
 *   occupations on demonstrated minimum competence: examinations, approved
 *   training, background checks, renewals, and criminal penalties for
 *   unlicensed practice. This story instantiates the public-safety
 *   coordination reading of that arrangement: the statutes solve a
 *   consumer-side information problem in markets where quality cannot be
 *   inspected before purchase and failure is dangerous or irreversible,
 *   giving every consumer a common competence signal and giving competent
 *   practitioners protection from being pooled with below-threshold
 *   substitutes. The epsilon referent is the standing licensing arrangement
 *   as it exists — fees, boards, scope, and all — assessed by this reading's
 *   own lights, never the reading's endorsed alternative; per the
 *   epsilon-invariance principle the sibling readings
 *   (rent_seeking_suppression, graduated_access_filter) are separate stories
 *   with their own epsilon, linked here through the network, not averaged
 *   into this one. The claim and the metrics are independent authored facts:
 *   the reading claims rope; the metrics describe what the arrangement's
 *   operation looks like from this seat, including drift this reading
 *   concedes.
 *
 * KEY AGENTS:
 *   - service_consumers: primary beneficiary (moderate / constrained) — receives the competence signal and disciplinary backstop; absorbs compliance costs indirectly through prices
 *   - licensed_competent_practitioners: secondary beneficiary (organized / constrained) — holds the credential, benefits from distinguishability and peer discipline, has sunk career capital in the threshold
 *   - incompetent_practitioners: primary payer (powerless / constrained) — bears exclusion from lawful practice, receives none of the trust flow
 *   - state_licensing_boards: agenda setter and fee recipient (institutional / constrained) — administers examinations, scopes, and discipline; funded by the fees it collects
 *   - state_legislatures: agenda setter (institutional / arbitrage) — enacts, expands, narrows, or repeals the statutes; can sunset the whole arrangement
 *   - professional_associations: secondary beneficiary (organized / mobile) — supplies board members, administers examinations, sells continuing education; could pivot to voluntary certification
 *   - competent_uncertified_aspirants: excluded voice (moderate / constrained) — claims threshold-exceeding competence without the credential; no seat in board rulemakings
 *   - regulatory_economists: analytical observer (analytical / analytical) — measures wage premiums, prices, and harm rates against stringency; holds no enforcement seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__public_safety_coordination, 0.3).
domain_priors:suppression_score(licensing_statute_mandate__public_safety_coordination, 0.5).
domain_priors:theater_ratio(licensing_statute_mandate__public_safety_coordination, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, extractiveness, 0.3).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__public_safety_coordination, rope).
narrative_ontology:human_readable(licensing_statute_mandate__public_safety_coordination, "Statutory Credential Requirements — Public-Safety Coordination Reading").
narrative_ontology:topic_domain(licensing_statute_mandate__public_safety_coordination, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__public_safety_coordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__public_safety_coordination, 'a9dcdb08-fb1e-4b16-864a-7b3ef39f119d').
narrative_ontology:cs_kernel_codification('a9dcdb08-fb1e-4b16-864a-7b3ef39f119d', formalized).
narrative_ontology:cs_authority_grounding('a9dcdb08-fb1e-4b16-864a-7b3ef39f119d', expertise).
narrative_ontology:cs_interpretation_layer_present('a9dcdb08-fb1e-4b16-864a-7b3ef39f119d').
narrative_ontology:cs_reading_relation('a9dcdb08-fb1e-4b16-864a-7b3ef39f119d', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_reading_relation('a9dcdb08-fb1e-4b16-864a-7b3ef39f119d', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('a9dcdb08-fb1e-4b16-864a-7b3ef39f119d', foundational, minimum_competence_standards_prevent_consumer_harm).
narrative_ontology:cs_axiom_status(minimum_competence_standards_prevent_consumer_harm, holdable).
narrative_ontology:cs_axiom_grounding('a9dcdb08-fb1e-4b16-864a-7b3ef39f119d', minimum_competence_standards_prevent_consumer_harm, empirically_contingent).
narrative_ontology:cs_axiom('a9dcdb08-fb1e-4b16-864a-7b3ef39f119d', secondary, harm_prevention_justifies_entry_restriction).
narrative_ontology:cs_axiom_status(harm_prevention_justifies_entry_restriction, holdable).
narrative_ontology:cs_axiom_grounding('a9dcdb08-fb1e-4b16-864a-7b3ef39f119d', harm_prevention_justifies_entry_restriction, instrumental).
narrative_ontology:cs_reference_frame('a9dcdb08-fb1e-4b16-864a-7b3ef39f119d', harm_calibrated_expert_thresholding).
narrative_ontology:cs_drift_state('a9dcdb08-fb1e-4b16-864a-7b3ef39f119d', contemporary_deregulation_scrutiny_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a9dcdb08-fb1e-4b16-864a-7b3ef39f119d', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, service_consumers).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, licensed_competent_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, professional_associations).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, incompetent_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, state_licensing_boards).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__public_safety_coordination, police_power_consumer_protection_doctrine).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__public_safety_coordination, adverse_selection_correction_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchase services — medical care, electrical work, legal advice, cosmetology — whose quality they cannot inspect before consumption and whose failure is costly or irreversible. The license gives them a common, state-backed competence signal and a disciplinary body to complain to. They cannot lawfully hire unlicensed providers in licensed fields, and they absorb part of the compliance burden indirectly through higher prices. Their choice set is which licensed provider to use, or informal substitutes of uncertain legality.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, service_consumers, beneficiary,
    moderate, biographical, constrained, national).

% Met the threshold and hold the credential. The license distinguishes them from below-threshold substitutes in the customer's eyes, and the disciplinary process polices quality among peers. They paid for the credential in training time, examination fees, and renewal compliance, and their professional standing is bound to the credential's value; leaving the occupation forfeits that sunk investment.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, licensed_competent_practitioners, beneficiary,
    organized, biographical, constrained, national).

% Practitioners whose skills fall below the statutory threshold. The statute bars them from lawful practice; working without a license carries fines and criminal exposure. Their options are retraining to meet the threshold (costly, and for some infeasible), informal practice outside the law, or leaving the trade. They bear the exclusion directly and receive none of the trust flow the arrangement generates.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, incompetent_practitioners, payer,
    powerless, biographical, constrained, national).

% Administer the statute: set examination content, define scope-of-practice rules, issue and renew licenses, and discipline violators. They are funded principally by the license and renewal fees they collect, and their rulemakings are staffed largely by licensees. The board's continuing existence and budget depend on the statute it administers; it cannot exit the arrangement without its own dissolution.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, state_licensing_boards, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__public_safety_coordination, state_licensing_boards, beneficiary).

% Enact and amend the credentialing statutes, decide which occupations require licenses, and can expand, narrow, or repeal them. They face organized professional associations on one side and diffuse consumer interests on the other, and in several jurisdictions they sweep licensing fee revenue into general funds. Their exit is genuine: any statute can be rewritten or sunset by the same process that created it.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, state_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).

% Trade and professional bodies that participate in standard-setting, administer examinations under contract, sell continuing-education coursework, and supply board members. The statutory regime gives their standards the force of law and their educational products a near-captive market. If the statutes were repealed they could pivot to voluntary certification; their exit from the arrangement is comparatively cheap.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, professional_associations, beneficiary,
    organized, generational, mobile, national).

% People who claim — and in some cases can demonstrate — competence at or above the safety-necessary level but who lack the credential: self-taught tradespeople, foreign-trained professionals, out-of-state licensees, career changers unwilling to repeat training they consider redundant. They hold no seat in board rulemakings, which are composed of licensees; their objection that the threshold exceeds the safety-necessary level has no procedural home.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, competent_uncertified_aspirants, excluded,
    moderate, biographical, constrained, national).

% Researchers measuring the arrangement's effects: wage premiums, service prices, harm rates, and barrier stringency by field and by entrant demographic. They publish the dose-response and cross-state comparisons any party can cite, hold no enforcement seat, and bear none of the arrangement's costs or benefits directly.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, regulatory_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__public_safety_coordination, state_licensing_boards).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__public_safety_coordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a consumer-side information problem: in services where quality cannot be inspected before purchase and failure is dangerous or irreversible, a shared statutory threshold gives every consumer a common competence signal and a disciplinary backstop, and spares competent practitioners the price competition of being pooled with below-threshold substitutes.
% TRANSFER_FUNCTION: Moves training time, examination and license fees, and continuing-education spending from practitioners — all of them, competent or not — into the credentialing apparatus and its vendors; moves below-threshold labor out of licensed markets; and moves trust in the opposite direction, reducing consumer search costs and expected harm.
% ABSENT_VOICES: Competent-but-uncertified aspirants and would-be informal providers have no seat in board rulemakings, which are composed of licensees; consumer representatives appear at legislative hearings but not in the standard-setting process where thresholds are actually calibrated.
% DISAPPEARANCE_RATIONALE: If the statutes vanished overnight, every licensed market would lose its shared competence signal at once: consumers would fall back on trial-and-error and reputation in fields where failure is dangerous, competent practitioners would invest in private certification and bonding to rebuild distinguishability, and below-threshold practitioners would re-enter immediately. The rearrangement would be real and costly — which is this reading's core claim that the arrangement is doing work.
% FOUNDING_PROBLEM: Demonstrable, hard-to-attribute consumer harm from incompetent practitioners — quack medicine, building collapses, electrical fires — in markets where buyers could not verify competence ex ante and harm was severe and irreversible.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by insurance actuarial data on malpractice and worksite harm, historical mortality records from pre-licensure markets (patent-medicine deaths, tenement fires), and the spontaneous persistence of voluntary certification demand in fields the statutes never reached. Critics corroborate the founding problem's historical reality while contesting its current scope — the dispute is over which present-day fields still carry the harm profile, not over whether it existed.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__public_safety_coordination, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__public_safety_coordination, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__public_safety_coordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__public_safety_coordination, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__public_safety_coordination, 0.3, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.30: from this seat the arrangement's charges — examination and license fees, training time, continuing-education spending — are the price of the threshold, and the reading holds that price justified by the harm it prevents; the residual concedes what is visible from inside: fee growth above administration cost, scope creep into fields with thin harm profiles, and compliance spending that functions as vendor revenue. Suppression is 0.50: unlicensed practice is criminalized and boards do enforce, but the coercion aims at practice-without-credential rather than at consumers' alternatives, and licensed substitutes remain plentiful. Theater is 0.25: examinations and discipline are functional, while continuing-education hour accumulation and renewal paperwork carry a growing performative share. Accessibility collapse is 0.40: voluntary certification, reputation systems, bonding, and insurance remain workable in adjacent unlicensed fields, so the statutory route dominates without annihilating alternatives. Resistance is 0.40: deregulation campaigns, economic-liberty litigation, and informal practice press the arrangement persistently, though diffuse consumer support blunts them. The measurement series share one grid (points 0 through 60) so no metric is sampled against another's end-state; suppression_requirement is tracked because the interval spans the maturation of board enforcement capacity, and its gentle rise models professionalized discipline machinery rather than a ratchet. The receipt surface records that the fee stream lands on the administering board — receipt of the charge, not a capture finding; whether that receipt constitutes capture is exactly the question the sibling readings press, and it is routed to the kernel omega rather than settled here. Fixing cost is prohibitive: repeal would force simultaneous rebuilding of trust infrastructure across every licensed field at once, and this reading holds the benefit of removal below that cost.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute differently and do so structurally: an incompetent practitioner meets the statute as a wall — exclusion with criminal exposure and no offsetting flow — while a consumer meets it as a floor — someone has already been filtered. The board seat experiences the arrangement as its mandate and its budget simultaneously, which is why the fee-funding fact matters: the administrator's continuation is funded by the charge it sets. The legislature seat alone holds genuine arbitrage — it can redraw or erase the threshold — and is the only seat for which fixing is cheap in mechanism though costly in consequence. The excluded aspirant seat sees the threshold from outside the conversation that sets it, which is why its position is recorded as excluded rather than as payer: this reading disputes that its members are competent, and that dispute is empirical, not structural. The engine computes per-seat classifications from the structural data; this story's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: consumers sit near the beneficiary end (protection flows to them; their costs are indirect price effects), licensed practitioners sit low-but-not-zero (net beneficiaries under this reading — the trust signal and peer discipline outweigh their fee and compliance burden), and professional associations sit low (they collect examination and coursework revenue while bearing no compliance cost). The victim declaration drives high directionality: incompetent practitioners sit near the full-target end — they bear the exclusion and receive none of the trust flow. The board seat derives near-symmetric with a mild beneficiary tilt: the fee stream passes through it into its own administration, which is why gain_flow names it while the reading denies capture. No directionality overrides are used: the beneficiary/victim declarations plus exit options separate every seat the derivation needs to separate, and the one seat the derivation cannot place — the excluded aspirant, who appears in neither declaration — is commentary-grade by design, since an authored absence must not drive classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim and the drift series do opposite protective work. Against the rent sibling, the rope claim preserves the distinction between a constraint whose charges are the price of coordination and one whose coordination story is cover: the founding problem is corroborated by harm data from outside the beneficiary set, so dismantling the arrangement wholesale would destroy a real signal. Against complacency, the rising theater and extractiveness series refuse the arrangement the immunity of a natural fact — it must keep re-earning its classification against field-level harm evidence, and the field-heterogeneity omega marks where this reading expects itself to fail first (low-stakes fields). Mandatrophy is not resolved: the founding problem is live, so the risk this reading tracks is not persistence past the problem but expansion beyond it — thresholds set above the safety-necessary level would convert the excluded from the incompetent into the merely uncredentialed, moving the arrangement toward the graduated_access_filter sibling's description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story is one reading of the licensing_statute_mandate kernel. Do the same statutory arrangements track consumer harm (this reading), incumbent rent (rent_seeking_suppression), or class-sorted access (graduated_access_filter) — and where does the evidence actually fall?',
    'Field-level dose-response studies correlating requirement stringency with (a) consumer harm rates, (b) incumbent wage premiums, and (c) barrier stringency by entrant demographics; cross-state natural experiments where requirements were relaxed or tightened.',
    'If harm rates do not track stringency while wages do, this reading loses its empirical grounding and the arrangement''s effective extraction rises sharply toward the sibling readings'' assessments; if harm tracks stringency in high-stakes fields only, the reading survives narrowed to those fields.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Which of the three kernel readings the statutory evidence supports.').

omega_variable(
    threshold_calibration,
    'Is the competence threshold in each licensed field set at the safety-necessary level, or above it — and does a substantial class of would-be practitioners exist who are excluded without any corresponding protection gain?',
    'Compare harm rates and service quality in jurisdictions that lowered requirements against matched controls; audit examination content and training-hour mandates against task-level risk analysis.',
    'If thresholds exceed the safety-necessary level, part of the excluded population is competent under a correct calibration — the victim set shrinks in fact, the arrangement''s suppression and extraction rise, and this reading''s clean coordination claim degrades toward the graduated_access_filter sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration, empirical, 'Whether the statutory threshold tracks safety-necessary competence or exceeds it.').

omega_variable(
    field_harm_heterogeneity,
    'The arrangement spans fields with radically different harm profiles — from surgery to hair braiding. Does the harm-prevention justification hold field-by-field, or does it hold only for a high-stakes core while low-stakes fields persist on inertia?',
    'Stratify the measurement series and the beneficiary/victim structure by field-level harm externality; test whether low-stakes fields show the theater and fee signatures the high-stakes core lacks.',
    'If low-stakes fields are carried by inertia, the aggregate rope classification masks a field-level split — high-stakes core as rope, low-stakes periphery drifting toward piton or snare behavior — and this reading''s epsilon is understated for the periphery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(field_harm_heterogeneity, empirical, 'Whether the harm-prevention justification holds uniformly across licensed fields.').

omega_variable(
    enforcement_legitimacy_framing,
    'The same enforcement facts — criminalized unlicensed practice, licensee-dominated boards, fee-funded administration — are read by this reading as legitimate threshold enforcement and by the sibling readings as competition suppression and capture. Is the disagreement empirical (resolvable by calibration data) or conceptual (the same facts under different framings)?',
    'Test whether enforcement intensity tracks harm risk (this reading''s prediction) or market entry rates (the siblings'' prediction); if it tracks neither, the disagreement is conceptual and the framing choice itself carries the classification.',
    'If enforcement tracks entry rather than harm, this reading''s suppression is mislabeled as enforcement and the story reclassifies toward the rent sibling; if enforcement tracks harm, the sibling readings lose their suppression claim for the fields where it holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_legitimacy_framing, conceptual, 'Whether the enforcement-facts disagreement across kernel readings is empirical or framing-bound.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__public_safety_coordination, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__public_safety_coordination, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__public_safety_coordination, theater_ratio, 10, 0.12).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__public_safety_coordination, theater_ratio, 20, 0.15).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__public_safety_coordination, theater_ratio, 30, 0.18).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__public_safety_coordination, theater_ratio, 40, 0.2).
narrative_ontology:measurement(lice_tr_t50, licensing_statute_mandate__public_safety_coordination, theater_ratio, 50, 0.23).
narrative_ontology:measurement(lice_tr_t60, licensing_statute_mandate__public_safety_coordination, theater_ratio, 60, 0.25).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 30, 0.26).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(lice_be_t50, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 50, 0.29).
narrative_ontology:measurement(lice_be_t60, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 60, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(lice_su_t50, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 50, 0.49).
narrative_ontology:measurement(lice_su_t60, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 60, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__public_safety_coordination, identity_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate__rent_seeking_suppression).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% The natural-language label 'occupational licensing' covers at least three structurally distinct claims about the same statutory arrangement: that it prevents consumer harm through competence thresholds (this story), that it restricts labor supply to extract incumbent rents (licensing_statute_mandate__rent_seeking_suppression), and that it filters market access by class and prior resources (licensing_statute_mandate__graduated_access_filter). Per the epsilon-invariance principle these are authored as separate stories, each with its own epsilon, beneficiary/victim structure, and claimed type, linked here as a constraint family. This reading is the upstream official justification; the siblings cite its arrangement as the object they re-describe. Epsilon differs by construction: this reading assesses the standing arrangement by harm-prevention lights (low extraction); the siblings assess the same arrangement by rent and access lights (high extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
