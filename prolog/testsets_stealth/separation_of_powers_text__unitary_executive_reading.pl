% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Reading of the Article II Vesting Clause
 *   domain: constitutional law/political theory/administrative law
 *
 * SUMMARY:
 *   Article II opens: 'The executive Power shall be vested in a President.'
 *   The unitary executive reading takes the grant as exclusive — whatever
 *   executive Power includes, Congress may neither fragment it among
 *   collegial bodies nor shield its holders from presidential removal. On
 *   this reading the independent regulatory agencies (FTC, NLRB, SEC, FCC),
 *   the for-cause removal protections of Humphrey's Executor, and the
 *   balancing test of Morrison are all unconstitutional; the Federal
 *   Reserve's insulation is the hardest case. The reading matured from an
 *   academic position in the 1980s through Department of Justice adoption,
 *   then partial judicial adoption in Free Enterprise Fund (2010) and Seila
 *   Law (2020), with actual commissioner removals following. Per the
 *   epsilon-invariance discipline this file instantiates ONE reading of the
 *   separation_of_powers_text kernel and does not average across its
 *   siblings: the formalist reading (impermeable delegation boundaries) and
 *   the functionalist reading (flexible overlap, intelligible-principle
 *   delegation) are separate constraints with their own victim sets and
 *   epsilon values, linked through network.affects_constraints. The claimed
 *   type and the metrics are authored independently: the claim is
 *   tangled_rope — a genuine accountability-coordination function joined to
 *   an asymmetric transfer of control, held in place by active judicial and
 *   presidential enforcement — while the metric values record the
 *   arrangement's operation as descriptively observed. The expected
 *   structural delta is honored: the agencies and the Fed sit in the losing
 *   set, removal power trends toward absoluteness, and the executive branch
 *   gains at the judiciary's expense as much as at Congress's.
 *
 * KEY AGENTS:
 *   - - incumbent_presidency: Primary beneficiary and agenda-setter (institutional/arbitrage) — collects command of execution; advances the reading through removals, OLC opinions, and judicial appointments
 *   - - white_house_policy_apparatus: Secondary beneficiary (institutional/mobile) — converts the doctrine into operational control of rulemaking and enforcement
 *   - - independent_regulatory_agencies: Primary targets (institutional/trapped) — multimember commissions whose for-cause removal protection the reading declares void
 *   - - federal_reserve_system: High-stakes target (institutional/trapped) — monetary independence hangs on the removal question
 *   - - congress: Structural loser (institutional/constrained) — loses the power to insulate administration from presidential control
 *   - - federal_judiciary: Dual-positioned loser and adjudicator (institutional/constrained) — dismantles Humphrey's Executor while absorbing defiance pressure
 *   - - career_civil_servants: Diffuse targets (moderate/constrained) — merit insulation erodes as removal protection narrows
 *   - - regulated_industries: Incidental beneficiaries (powerful/arbitrage) — gain a political access channel to formerly insulated regulators
 *   - - regulation_reliant_citizens: Excluded voice (powerless/trapped) — depend on stable nonpartisan administration; enter only post hoc through amicus briefs and elections
 *   - - constitutional_law_scholars: Analytical observers (analytical/analytical) — map the stakes and supply both sides' arguments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.66).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.52).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Reading of the Article II Vesting Clause").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional law/political theory/administrative law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, '61149a19-f68c-4ba6-a725-bc3ffffdeae9').
narrative_ontology:cs_kernel_codification('61149a19-f68c-4ba6-a725-bc3ffffdeae9', fixed_text).
narrative_ontology:cs_authority_grounding('61149a19-f68c-4ba6-a725-bc3ffffdeae9', lineage).
narrative_ontology:cs_interpretation_layer_present('61149a19-f68c-4ba6-a725-bc3ffffdeae9').
narrative_ontology:cs_reading_relation('61149a19-f68c-4ba6-a725-bc3ffffdeae9', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('61149a19-f68c-4ba6-a725-bc3ffffdeae9', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('61149a19-f68c-4ba6-a725-bc3ffffdeae9', foundational, executive_power_exclusively_vested_in_president).
narrative_ontology:cs_axiom_status(executive_power_exclusively_vested_in_president, holdable).
narrative_ontology:cs_axiom_grounding('61149a19-f68c-4ba6-a725-bc3ffffdeae9', executive_power_exclusively_vested_in_president, conventional).
narrative_ontology:cs_axiom('61149a19-f68c-4ba6-a725-bc3ffffdeae9', secondary, congressional_removal_restrictions_invalid).
narrative_ontology:cs_axiom_status(congressional_removal_restrictions_invalid, holdable).
narrative_ontology:cs_axiom_grounding('61149a19-f68c-4ba6-a725-bc3ffffdeae9', congressional_removal_restrictions_invalid, conventional).
narrative_ontology:cs_reference_frame('61149a19-f68c-4ba6-a725-bc3ffffdeae9', exclusive_vesting_single_executive).
narrative_ontology:cs_drift_state('61149a19-f68c-4ba6-a725-bc3ffffdeae9', contemporary_administrative_state, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('61149a19-f68c-4ba6-a725-bc3ffffdeae9', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, incumbent_presidency).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, white_house_policy_apparatus).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, regulated_industries).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_regulatory_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_reserve_system).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congress).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, career_civil_servants).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_judiciary).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, regulated_industries).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, unitary_executive_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, article_two_exclusive_vesting_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets removal policy for executive officers, obtains Office of Legal Counsel opinions treating for-cause removal limits as void, nominates judges receptive to the reading, and removes commission members whenever courts permit. Gains direct command of regulatory enforcement priorities across the government. Its levers span litigation, appointments, and personnel action, so it pursues the reading through whichever channel opens next.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, incumbent_presidency, agenda_setter,
    institutional, biographical, arbitrage, national).

% The Executive Office staff, budget office, and counsel's office convert the reading into day-to-day control: clearing agency rules, steering enforcement through budget and personnel leverage, and drafting the legal memoranda that operationalize presidential direction. Collects the gains of unified command without individually bearing removal disputes.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, white_house_policy_apparatus, beneficiary,
    institutional, biographical, mobile, national).

% Firms subject to commission regulation gain a new access channel when agency leadership answers to the White House: policy relief obtainable through political proximity rather than adjudication. They still bear whatever requirements the now-more-political agency imposes, so their position cuts both ways.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, regulated_industries, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, regulated_industries, payer).

% Multimember commissions such as the FTC and NLRB operate under statutes protecting their members from removal except for cause. Under the reading their defining feature is void, their members serve at the President's pleasure, and their policy continuity depends on whoever occupies the White House. They cannot leave the constitutional order they inhabit; their persistence rests on judicial validation they no longer control.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_regulatory_agencies, payer,
    institutional, generational, trapped, national).

% Conducts monetary policy through a board whose governors historically enjoyed removal protection. A successful removal challenge would expose interest-rate decisions to presidential displeasure, with immediate consequences for inflation expectations and global dollar markets. Legally embedded in statute and international financial commitments, it has nowhere else to conduct monetary policy.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_reserve_system, payer,
    institutional, generational, trapped, global).

% Wrote the removal protections and bipartisan commission structures now under constitutional attack. Every adverse ruling narrows its power to design administration insulated from presidential control. Its counters — new statutes, confirmation leverage, impeachment, funding conditions — run through veto points and courts that increasingly side with the reading.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress, payer,
    institutional, generational, constrained, national).

% Merit-system employees in regulatory agencies built careers on the assumption that expertise, not political loyalty, determines retention. As removal protection narrows to the constitutional minimum, reassignment and dismissal risk rises; the realistic exit is leaving public service for the private sector.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, career_civil_servants, payer,
    moderate, biographical, constrained, national).

% Adjudicates the removal cases that decide the reading's reach, and in doing so dismantles the Humphrey's Executor framework that previously upheld congressional structuring. Life tenure shields individual judges, but the institution absorbs the pressure of enforcement-defiance scenarios and loses doctrinal tools it relied on for a century. It observes the whole structure from the only seat empowered to redraw it.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_judiciary, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, federal_judiciary, observer).

% Households, retirees, and small firms that depend on stable, nonpartisan administration — predictable safety rules, steady monetary policy, durable benefit programs. Their interest in insulation enters the process only through amicus briefs and elections after removals have begun; no seat in the interpretive conversation represents them directly.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, regulation_reliant_citizens, excluded,
    powerless, generational, trapped, national).

% Academics and commentators who map the doctrinal stakes, publish competing accounts of the Vesting Clause, and supply the arguments both sides litigate. They bear none of the arrangement's costs and collect none of its control; their influence runs through citation networks and clerkship pipelines.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__unitary_executive_reading, incumbent_presidency).
narrative_ontology:fixing_cost_class(separation_of_powers_text__unitary_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the accountability-attribution problem of a large administrative apparatus: by vesting all executive authority in one elected officer with unrestricted removal power, every act of execution is traceable to a single locus voters can reward or punish, and execution gains the decisiveness Hamilton attributed to unified command.
% TRANSFER_FUNCTION: Moves control over the execution of federal law — appointment, removal, supervision, and enforcement priorities across the regulatory state — from Congressionally-insulated multimember commissions and merit-protected career officials to the President and the White House staff apparatus.
% ABSENT_VOICES: Those who depend on insulated, nonpartisan administration — market participants pricing Fed independence, career experts, and citizens relying on stable regulation — appear only as amici after removals begin; the interpretive conversation runs among judges, executive-branch lawyers, and aligned academics. Future presidents of the opposing party, who would inherit the concentrated control, are absent until they hold it.
% DISAPPEARANCE_RATIONALE: If the exclusive-vesting requirement vanished overnight, Humphrey's Executor-style protection would stand undisturbed, removed commissioners would retain their offices, and the presidency's newly acquired direction of the regulatory state would evaporate — the mid-century administrative settlement would reconstitute itself, which is precisely what the reading's adherents fear and its opponents expect.
% FOUNDING_PROBLEM: The 1787 framers needed an executor energetic enough to enforce national law promptly — decision, activity, secrecy, dispatch, in Hamilton's catalogue — without recreating monarchy, and they needed responsibility locatable: a plural executive or legislatively-controlled councils would diffuse blame beyond electoral reach.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: Federalist 70 states the energy-and-accountability problem before any modern presidency existed; twentieth-century defenders of independent agencies nonetheless conceded the 'headless fourth branch' accountability deficit; contemporary administrative-law scholarship across the spectrum acknowledges diffuse responsibility in the administrative state while disputing whether exclusive vesting is the cure. No serious participant denies the founding problem; the contest is over the remedy.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66 because the reading, where enforced, relocates command of the entire regulatory apparatus to one office: agencies lose policy continuity, Congress loses structuring power, and the judiciary loses a century-old framework — a real transfer, though one paired with an accountability function that even opponents concede headless commissions serve imperfectly. Suppression (0.52) reflects enforcement through court majorities and personnel action rather than broad coercion; rival readings remain publishable and arguable, so alternatives are narrowed, not closed. Theater_ratio (0.33) prices the growing gap between originalist justification and interest-driven application — the core operations (removals, rule clearance) are real, but rhetorical production increasingly outruns textual work. Accessibility_collapse is low (0.30): the functionalist and formalist readings remain fully available to any judge willing to adopt them. Resistance is high (0.65): four-justice dissents, bipartisan defense of Fed independence, and congressional counter-legislation document sustained pushback. The three temporal series share one grid (t=0..40 at steps of 8) so every metric is authored at every examined point; the suppression_requirement series is included because this story's central dynamic is enforcement-capacity build-up — fringe academic position, through OLC adoption, to Supreme Court majorities and executed removals — which is exactly the intensification the series exists to trace. Fixing is authored prohibitive: the seat positioned to reverse course (the Court) would need to overrule fresh precedent and then survive probable executive noncompliance, a cost exceeding the institutional benefit it would capture, while the seat that could accommodate the change is the seat collecting the gains.
 *
 * PERSPECTIVAL GAP:
 *   From the presidency's seat the arrangement restores a promise the Constitution made: one accountable officer answers for execution, and voters can locate blame. From the agencies' and Congress's seats the identical structure dispossesses them — statutes they wrote are voided, members they confirmed become removable at will, and policy swings with each election. The seats share the same nominal institutional power atom, so differentiation comes from directionality and exit: the presidency holds arbitrage-grade flexibility across litigation, appointments, and personnel, while the agencies are trapped inside the constitutional order whose terms the reading fixes. The judiciary sits astride the divide — it authors the displacement and is displaced by it, gaining doctrinal control of the boundary while losing the framework that defined its role for a century.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (incumbent_presidency, white_house_policy_apparatus, regulated_industries) drive those seats toward the beneficiary end: the presidency collects control directly, the apparatus collects operational command, industries collect political access. Victim declarations (independent_regulatory_agencies, federal_reserve_system, congress, career_civil_servants, federal_judiciary) drive those seats toward the target end, with trapped exit options holding the agencies and the Fed nearest the full-target position. No directionality_overrides are authored: the derivation from declarations plus exit options captures the structure, and the override mechanism keys on power atoms, which would smear one value across the many institutional seats whose directionalities deliberately diverge.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — energetic, locatably accountable execution — remains live, so no mandatrophy resolution is declared and the R5 mismatch check (dead status combined with world-rearranges) does not fire. The classification discipline matters in both directions here: reading the arrangement as pure extraction would erase the genuine coordination it performs (accountability attribution and unified command are real functions the prior settlement served badly); reading it as pure coordination would erase the asymmetric transfer it performs (agencies, Congress, and the judiciary pay; the presidency collects). The tangled-rope structure holds both halves, and the temporal series shows which half is growing: extraction and enforcement intensity rise together while the accountability payoff remains asserted rather than demonstrated — the open question the accountability_efficacy omega carries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the separation_of_powers_text kernel — would instantiating a sibling reading (formalist or functionalist) change the victim set and the computed classification?',
    'Generate the sibling stories and compare computed per-seat classifications: the formalist reading shifts the losing seats toward delegatees and regulated private parties, while the functionalist reading shrinks the losing set toward zero.',
    'Under the functionalist instantiation the same constitutional order computes as near-rope (stable practice, minimal victims); under the formalist instantiation extraction redistributes rather than vanishes. This story''s classification holds only for the unitary instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading-contest structure of the separation-of-powers kernel').

omega_variable(
    federal_reserve_carve_out,
    'Does monetary-policy independence survive even within unitary-executive logic (as Seila Law dicta hints), removing the Federal Reserve from the losing set?',
    'Litigation over removal of Fed governors; whether courts articulate a monetary exceptionalism distinct from Humphrey''s Executor rather than extending removal power to the Board.',
    'If carved out, the highest-stakes losing seat exits the set, measured extraction falls materially, and the accountability-coordination story strengthens correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_reserve_carve_out, empirical, 'Whether the Federal Reserve sits inside or outside the constraint''s reach').

omega_variable(
    sincerity_vs_partisan_advantage,
    'Is the doctrine applied as textual fidelity or as partisan advantage — would its adherents accept it binding against their own policy allies?',
    'Cross-alignment test: observe whether administrations invoke removal power against commissioners whose policy they support, and whether the theory''s advocates defend symmetric application across parties.',
    'If application tracks political alignment rather than text, theater_ratio rises above the authored trajectory and the constraint drifts toward performative maintenance of a power transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_vs_partisan_advantage, empirical, 'Motivation ambiguity behind doctrinal enforcement').

omega_variable(
    judicial_compliance_defiance,
    'Will the executive branch comply with adverse removal rulings, or does enforcement enter a defiance regime?',
    'Observe post-ruling behavior: reinstatement of removed commissioners, backpay, restoration of protected status, or renewed removal attempts despite orders.',
    'Defiance would push suppression well beyond the authored trajectory and drive the computed type toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_compliance_defiance, empirical, 'Compliance uncertainty in enforcement of the constraint').

omega_variable(
    accountability_efficacy_claim,
    'Does concentrated presidential control actually deliver the accountability and energy benefits that constitute the constraint''s coordination function?',
    'Comparative institutional analysis of policy responsiveness, blame attribution, and error correction under insulated versus presidentially-directed agencies.',
    'If no measurable accountability gain exists, the coordination half of the structure is cover and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_efficacy_claim, empirical, 'Reality check on the coordination-function claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__unitary_executive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(sepa_tr_t0, observed).
narrative_ontology:measurement(sepa_tr_t8, separation_of_powers_text__unitary_executive_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement_basis(sepa_tr_t8, observed).
narrative_ontology:measurement(sepa_tr_t16, separation_of_powers_text__unitary_executive_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement_basis(sepa_tr_t16, observed).
narrative_ontology:measurement(sepa_tr_t24, separation_of_powers_text__unitary_executive_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(sepa_tr_t24, observed).
narrative_ontology:measurement(sepa_tr_t32, separation_of_powers_text__unitary_executive_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement_basis(sepa_tr_t32, observed).
narrative_ontology:measurement(sepa_tr_t40, separation_of_powers_text__unitary_executive_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement_basis(sepa_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(sepa_be_t0, observed).
narrative_ontology:measurement(sepa_be_t8, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement_basis(sepa_be_t8, observed).
narrative_ontology:measurement(sepa_be_t16, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement_basis(sepa_be_t16, observed).
narrative_ontology:measurement(sepa_be_t24, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement_basis(sepa_be_t24, observed).
narrative_ontology:measurement(sepa_be_t32, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(sepa_be_t32, observed).
narrative_ontology:measurement(sepa_be_t40, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(sepa_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(sepa_su_t0, observed).
narrative_ontology:measurement(sepa_su_t8, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 8, 0.22).
narrative_ontology:measurement_basis(sepa_su_t8, observed).
narrative_ontology:measurement(sepa_su_t16, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 16, 0.3).
narrative_ontology:measurement_basis(sepa_su_t16, observed).
narrative_ontology:measurement(sepa_su_t24, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 24, 0.38).
narrative_ontology:measurement_basis(sepa_su_t24, observed).
narrative_ontology:measurement(sepa_su_t32, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 32, 0.46).
narrative_ontology:measurement_basis(sepa_su_t32, observed).
narrative_ontology:measurement(sepa_su_t40, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(sepa_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__functionalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'separation of powers' decomposes into at least three structurally distinct constraints sharing one kernel text. This story (unitary_executive_reading) binds the REMOVAL and STRUCTURING dimension — its victims are the independent agencies, the Fed, Congress's design power, and the judiciary's framework. The formalist sibling binds the DELEGATION dimension (victims: delegatees and the administrative state's rulemaking output); the functionalist sibling binds almost nothing (near-zero victims, stable practice). The upstream formalist claim is frequently cited as support for this reading (shared originalist method), which is why the family edges run through it; the functionalist reading is the direct logical competitor on the removal question. Each member carries its own epsilon; none averages across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
