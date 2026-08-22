% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__outcomes_based_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: Outcomes-Based Equivalence Standard for Autonomous Engagement Systems (IHL Distinction/Proportionality)
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the outcomes_based_reading of the contested
 *   kernel ihl_distinction_proportionality: the claim that IHL's distinction
 *   and proportionality obligations are satisfied whenever an autonomous
 *   engagement system demonstrates distinction/proportionality performance
 *   equal to or exceeding a human-operator baseline, with law governing
 *   outcomes rather than means. The standing arrangement under assessment is
 *   the compliance regime this reading produces — equivalence benchmarks,
 *   certification-gated procurement, and fielded autonomy justified by
 *   demonstrated scores. Two sibling readings instantiate different
 *   constraints from the same kernel and are authored separately: the
 *   human_agency_reading (irreducible human judgment constitutive of
 *   compliance; higher epsilon because the victim set expands to include the
 *   non-delegation interest) and the categorical_proportionality_reading's
 *   neighbor categorical_prohibition_reading (no performance can justify
 *   machine-decided killing; the permission structure collapses entirely).
 *   Those files carry their own epsilon, beneficiaries, and victims; this
 *   file links them through network.affects_constraints. The claim/metric
 *   split is deliberate: claimed_type records the structure I believe true
 *   (tangled_rope — a real coordination function with asymmetric incidence),
 *   while the metrics describe observed operation independently. KEY AGENTS
 *   (by structural relationship): - national_legal_advisers: Agenda-setter
 *   (institutional/constrained) — writes the equivalence test, signs fielding
 *   clearances - defense_contractors: Primary beneficiary
 *   (powerful/arbitrage) — certification-gated market access -
 *   adopting_militaries: Beneficiary with payer exposure
 *   (institutional/constrained) — capability gain, command responsibility
 *   retained - benchmark_certification_bodies: Beneficiary and de facto
 *   agenda-setter (organized/mobile) — holds the operative definition of
 *   legality - civilian_populations_conflict_zones: Primary target
 *   (powerless/trapped) — absorbs residual risk, no consent, no exit -
 *   ihl_interpretive_custodians: Target of authority displacement
 *   (organized/identity_locked) - martens_clause_advocacy_coalitions:
 *   Excluded voice (organized/constrained) - un_gge_diplomatic_community:
 *   Analytical observer (institutional/analytical)
 *
 * KEY AGENTS:
 *   - national_legal_advisers: agenda-setter (institutional/constrained) — authors the equivalence test and signs clearances; career legitimacy invested in prior determinations
 *   - defense_contractors: primary beneficiary (powerful/arbitrage) — certification converts a contested category into revenue; can exit a closing jurisdiction
 *   - adopting_militaries: beneficiary with secondary payer exposure (institutional/constrained) — capability and force multiplication against retained command responsibility
 *   - benchmark_certification_bodies: beneficiary and secondary agenda-setter (organized/mobile) — the test suite is the operative legality definition
 *   - civilian_populations_conflict_zones: primary target (powerless/trapped) — bears the benchmark-to-battlefield gap without consent or exit
 *   - ihl_interpretive_custodians: target of interpretive displacement (organized/identity_locked) — professional identity constituted through the authority being relocated
 *   - martens_clause_advocacy_coalitions: excluded (organized/constrained) — present in diplomacy, absent from threshold-setting
 *   - un_gge_diplomatic_community: observer (institutional/analytical) — compiles positions, could crystallize or supersede the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.58).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.52).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "Outcomes-Based Equivalence Standard for Autonomous Engagement Systems (IHL Distinction/Proportionality)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, 'c1c90d91-9da8-4529-9648-8e9974af7076').
narrative_ontology:cs_kernel_codification('c1c90d91-9da8-4529-9648-8e9974af7076', fixed_text).
narrative_ontology:cs_authority_grounding('c1c90d91-9da8-4529-9648-8e9974af7076', expertise).
narrative_ontology:cs_interpretation_layer_present('c1c90d91-9da8-4529-9648-8e9974af7076').
narrative_ontology:cs_reading_relation('c1c90d91-9da8-4529-9648-8e9974af7076', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('c1c90d91-9da8-4529-9648-8e9974af7076', ihl_distinction_proportionality__categorical_prohibition_reading, forecloses).
narrative_ontology:cs_axiom('c1c90d91-9da8-4529-9648-8e9974af7076', foundational, outcome_performance_exhausts_ihl_compliance).
narrative_ontology:cs_axiom_status(outcome_performance_exhausts_ihl_compliance, holdable).
narrative_ontology:cs_axiom_grounding('c1c90d91-9da8-4529-9648-8e9974af7076', outcome_performance_exhausts_ihl_compliance, empirically_contingent).
narrative_ontology:cs_axiom('c1c90d91-9da8-4529-9648-8e9974af7076', secondary, technological_neutrality_of_legal_obligations).
narrative_ontology:cs_axiom_status(technological_neutrality_of_legal_obligations, holdable).
narrative_ontology:cs_axiom_grounding('c1c90d91-9da8-4529-9648-8e9974af7076', technological_neutrality_of_legal_obligations, conventional).
narrative_ontology:cs_reference_frame('c1c90d91-9da8-4529-9648-8e9974af7076', outcome_equivalence_sufficiency).
narrative_ontology:cs_drift_state('c1c90d91-9da8-4529-9648-8e9974af7076', contemporary_gge_negotiations, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c1c90d91-9da8-4529-9648-8e9974af7076', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, adopting_militaries).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, benchmark_certification_bodies).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_conflict_zones).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, ihl_interpretive_custodians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, adopting_militaries).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, technology_neutral_law_doctrine).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, comparative_performance_compliance_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Serve as the legal offices that conduct weapons reviews and decide whether an autonomous engagement system may be fielded. They write the equivalence test, commission the evaluation evidence, and sign the clearance. Their professional standing is bound to the review frameworks they build; once a clearance regime exists, revisiting its premises means reopening their own prior determinations. Leaving the frame would mean conceding that past clearances lacked a defensible basis.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, national_legal_advisers, agenda_setter,
    institutional, biographical, constrained, national).

% Build autonomous targeting, sentry, and loitering-munition systems whose market access depends on passing the equivalence demonstration. Certification converts a contested product category into a lawful one and channels procurement budgets toward whoever clears the bar. Vendors can redirect engineering teams, pursue export licenses in other jurisdictions, or repackage capabilities as human-supervised tools if one market's standard closes.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    powerful, biographical, arbitrage, global).

% Armed forces adopting the reading gain persistent-strike capacity, reduced exposure of their own personnel, and engagement tempo beyond unaided human reaction times. The same forces carry command responsibility for what certified systems do, hand adversaries propaganda value in any civilian-harm incident, and inherit procurement lines that are costly to unwind. Capability competition with peer militaries narrows their room to decline the category altogether.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, adopting_militaries, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, adopting_militaries, payer).

% Testing laboratories, standards institutes, and contractor-affiliated evaluation teams design the scenarios, metrics, and statistical thresholds that operationalize 'equivalent performance.' Whoever holds the test suite holds the de facto definition of legality. The role brings funding, authority, and agenda control over what counts as a representative engagement; the bodies can relocate or rebrand if a particular regime loses legitimacy.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, benchmark_certification_bodies, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, benchmark_certification_bodies, agenda_setter).

% People living where certified systems operate absorb the difference between laboratory conditions and war: sensor degradation, adversary spoofing, crowd complexity, and the long tail of cases no test suite anticipated. They did not consent to the risk transfer, have no seat in the working groups that set thresholds, and cannot leave the battlespace. When a misclassification happens, the harm lands on them and the remedy runs through courts they rarely reach.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_conflict_zones, payer,
    powerless, biographical, trapped, regional).

% The ICRC, humanitarian-law academies, and treaty-body experts hold mandates to interpret distinction and proportionality. The reading relocates the decisive questions — what counts as adequate care, feasible precaution, reasonable certainty — into technical benchmarks outside their training and institutions. Their professional identity is constituted by interpretive authority over these very terms; abandoning the contest would dissolve the function they exist to perform, so they contest rather than exit.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, ihl_interpretive_custodians, payer,
    organized, generational, identity_locked, global).

% Campaign networks and civil-society coalitions argue that public conscience constrains the means of violence irrespective of measured outcomes. They hold speaking slots in diplomatic forums but no seat in the technical working groups where equivalence thresholds are drafted, and no vote in national review processes. Their leverage is agenda-shaping at conferences, not entry into the certification pipeline.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, martens_clause_advocacy_coalitions, excluded,
    organized, biographical, constrained, global).

% The Group of Governmental Experts on lethal autonomous weapons compiles state positions, commissions working papers, and keeps a negotiating record. It takes testimony from every other seat, tracks accumulating practice, and could convert accumulated positions into treaty text that would supersede or entrench the reading.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, un_gge_diplomatic_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__outcomes_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces indeterminate 'adequate human judgment' assessments with a shared, measurable equivalence test, letting states, reviewers, and vendors coordinate on a common legality criterion for autonomous engagement systems.
% TRANSFER_FUNCTION: Moves engagement decisions from trained human operators to certified machine systems; moves interpretive authority over distinction and proportionality from humanitarian-law custodians to benchmark designers; moves residual misidentification risk onto civilians in conflict zones; moves procurement revenue to defense contractors.
% ABSENT_VOICES: Civilian populations of conflict zones — the parties who absorb metric failure — have no seat in benchmark design or review boards. Martens Clause constituencies arguing that public conscience constrains means regardless of outcomes are present in diplomatic forums but excluded from the technical working groups where 'equivalent performance' is operationalized.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, fielded-autonomy programs would lose their legal basis and pause pending an alternative; procurement would reorganize around human-in-the-loop architectures; contractors would lose the certification-gated market; and interpretive authority over the core terms would revert to custodial institutions. Deployment practice, budget lines, and legal review all currently hang on this standard.
% FOUNDING_PROBLEM: Existing IHL rules were drafted for human decision-makers; 'what counts as adequate care in attack' resisted straightforward application to machine engagement, leaving states without an objective test for lawful fielding of autonomous systems.
% FOUNDING_PROBLEM_CORROBORATION: UN GGE on LAWS session records and statements by non-aligned and African Group delegations attest that the applicability gap remains unresolved; academic international-law commentary from outside both vendor and military communities documents the same indeterminacy. The corroboration attests the founding problem's liveness, not the adequacy of this reading's solution — humanitarian custodians dispute that metrics can solve it at all, and no corroborating source outside the beneficiary set attests that equivalence testing closes the gap.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: the reading creates a genuine permission structure that did not previously exist — before it, fielding autonomous engagement systems required contorting rules written for human decision-makers — and it attaches a real demonstration burden. But the standard's costs fall disproportionately on parties who never agreed to it: civilians absorb the gap between benchmark conditions and field conditions, and interpretive authority over the core IHL terms migrates from custodial institutions to whoever designs the test suite. Suppression (0.52) is a raw structural property, unscaled by power or scope: it is mostly juridical foreclosure — within an adopting jurisdiction a commander cannot invoke the human-agency requirement as a matter of law — plus the total absence of exit for civilians in the battlespace; there is little coercive machinery aimed at persons. Theater_ratio (0.38) reflects vendor-run demonstrations on curated scenarios: a meaningful fraction of verification activity persuades rather than measures, and the share grows as marketing stakes rise. Accessibility_collapse (0.42): alternative readings remain legally live internationally, so understanding this reading does not collapse the alternatives — but inside an adopting state's review process the alternatives are foreclosed for the officers operating within it. Resistance (0.65) is sustained and organized: ICRC positions, campaign coalitions, and a majority bloc of GGE delegations contest the reading's adequacy. The temporal series run on one shared grid (two-year steps across the twelve-year interval): extraction, theater, and suppression ratchet upward together as informal adoption precedes any binding treaty — procurement lock-in hardening ahead of legal settlement. Identity-lock note: the custodian seat's exit is identity_locked — professional self-concept constituted through interpretive authority over the very terms being relocated; the legal-adviser seat shows a milder fusion (prior clearances become their own justification). Coalition check: the powerless civilian seat cannot coalition effectively (dispersed, wartime), but custodian-plus-advocacy coalitions supply the organized resistance the resistance metric records.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently, and the engine computes that divergence from the structural data. From the adopting-military and contractor seats the arrangement presents as rope-like: a clear, shared standard that converts an ungovernable category into a lawful one and rewards demonstrated care. From the civilian seat the same structure operates as unconsented risk imposition with no exit — a snare-flavored experience. The custodian seat experiences authority expropriation: the questions they exist to answer are answered elsewhere, by methods they cannot audit. The legal-adviser seat sits between: administering a framework whose premises they authored, they experience custodian dissent as bad faith and civilian-harm incidents as edge cases — the administrator's view is stabilized by having signed the clearances.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Defense contractors (arbitrage exit) sit nearest the beneficiary pole: certification converts a contested category into revenue, and they can leave if a jurisdiction closes. Adopting militaries benefit through capability but retain command responsibility and reputational exposure, damping their subsidy below the pure-beneficiary level. Benchmark bodies collect authority and funding — beneficiaries whose d is low but whose agenda-setting secondary role gives them structural influence beyond their receipts. Civilian populations are full targets: trapped, unconsenting, absorbing residual risk with no compensating flow. Interpretive custodians are targets of a less tangible extraction — authority displacement — and their identity_locked exit keeps them at the high-d end rather than allowing exit-driven damping. National legal advisers are administrators rather than declared beneficiaries; their d derives from administration plus career investment in prior clearances, placing them mildly toward the constraint rather than symmetric. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the intended ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — IHL rules drafted for human decision-makers confronting a novel category of engagement — is still live, so this is not a mandatrophy case today. The classification work the type performs is preventive: calling this a rope would erase the asymmetric incidence (externalized residual risk, displaced interpretive authority) that distinguishes it from pure coordination; calling it a snare would erase the genuine indeterminacy problem it solves and the demonstration burden it imposes on would-be fielders. Tangled rope holds both facts. The reading carries no sunset clause and presents itself as steady-state law rather than transition. If the benchmark-field-validity omega resolves badly, expect drift toward snare — a permission structure persisting under false assurance. If a binding treaty supersedes the reading, expect piton decay — a benchmark apparatus maintained ceremonially while practice follows the new instrument. Both trajectories are foreshadowed in the measurement series' joint upward ratchet.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of kernel ihl_distinction_proportionality (this file instantiates outcomes_based_reading). Which reading ultimately governs — outcomes-equivalence, irreducible human agency (human_agency_reading), or categorical prohibition (categorical_prohibition_reading)?',
    'Consolidated state practice, a binding treaty instrument on autonomous weapons, or authoritative judicial clarification settling whether means carry independent legal weight under distinction and proportionality.',
    'Under human_agency_reading, certified autonomous engagements lacking moment-of-force human judgment are unlawful regardless of metrics — the victim set expands to include the non-delegation dignity interest, epsilon rises, and this reading''s permission structure converts into cover for unlawful practice. Under categorical_prohibition_reading the permission structure collapses entirely and the constraint reads as a snare dressed as neutrality. If this reading prevails, the authored structure stands as written.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest over which reading of distinction/proportionality governs autonomous systems.').

omega_variable(
    benchmark_field_validity,
    'Do benchmark conditions under which equivalence is demonstrated predict field performance under distribution shift, adversary adaptation, degraded communications, and mixed human-machine teaming?',
    'Longitudinal audits comparing certified-system field incident rates against benchmark scores, with incident reporting decoupled from vendor and military incentives.',
    'Low validity means the permission structure externalizes risk under false assurance — extraction concentrates on civilians and the constraint drifts toward snare; high validity bounds the extraction to a genuine, disclosed tradeoff.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benchmark_field_validity, empirical, 'Whether laboratory equivalence demonstrations track battlefield distinction/proportionality performance.').

omega_variable(
    human_baseline_selection,
    '''Equal to or exceeding human operators'' — which operator population, under what fatigue, time pressure, and information conditions, defines the comparison baseline?',
    'Comparative empirical studies of human performance distributions in representative combat conditions, adjudicated through a transparent multilateral process rather than vendor-selected reference cohorts.',
    'A weak baseline renders the threshold trivially passable and inflates extraction; a strong baseline makes the standard more demanding than much current human practice, shrinking the permission space and lowering effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_baseline_selection, conceptual, 'Baseline selection determines whether the equivalence threshold is stringent or nominal.').

omega_variable(
    residual_risk_permissibility,
    'May a legality standard knowingly accept quantified residual civilian risk from metric blind spots in exchange for military efficiency, and if so, who is entitled to consent to that trade?',
    'Normative analysis within IHL doctrine (proportionality''s precaution logic, feasibility standards) plus diplomatic settlement; not resolvable by measurement alone.',
    'If the trade is impermissible, the reading''s justification fails wholesale and the constraint is extraction wearing neutrality as a cover story; if permissible within articulated bounds, the authored moderate extraction stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_risk_permissibility, preference, 'Whether accepting quantified residual civilian risk for efficiency is a legitimate IHL tradeoff at all.').

omega_variable(
    interpretive_displacement_effect,
    'Does relocating distinction/proportionality determination from humanitarian-law custodians to technical benchmark bodies degrade protection outcomes, or does measurable standardization improve accountability relative to opaque after-action human judgment?',
    'Comparative incident analysis across jurisdictions adopting versus rejecting the reading, controlling for conflict intensity and system maturity.',
    'If outcomes improve, the custodian victim seat is partly status grievance and net extraction falls; if outcomes degrade, authority displacement is a substantive harm channel and extraction rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_displacement_effect, empirical, 'Whether the authority transfer harms protection outcomes or improves them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ihl__tr_t2, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2, 0.23).
narrative_ontology:measurement(ihl__tr_t4, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ihl__tr_t6, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement(ihl__tr_t8, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 12, 0.38).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ihl__be_t2, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(ihl__be_t4, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(ihl__be_t6, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(ihl__be_t8, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ihl__su_t2, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2, 0.33).
narrative_ontology:measurement(ihl__su_t4, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 4, 0.37).
narrative_ontology:measurement(ihl__su_t6, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 6, 0.41).
narrative_ontology:measurement(ihl__su_t8, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(ihl__su_t12, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the kernel ihl_distinction_proportionality decomposes into three readings per the epsilon-invariance principle — the colloquial question 'does IHL permit autonomous weapons?' covers three structurally distinct constraints with different epsilon, victim sets, and failure modes. This file is the outcomes_based_reading (moderate epsilon; permission conditioned on demonstrated performance equivalence). The siblings — human_agency_reading and categorical_prohibition_reading — are separate stories linked here; each authors its own epsilon over the same referent (the standing arrangement of autonomous-engagement governance). Values differ because the readings weigh means-relevance differently, not because they measure different things.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
