% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_absolutist_reading, []).

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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Near-Absolute Speech Protection Boundary (Absolutist Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   A constitutional settlement fixes the boundary of state power over
 *   expression near the maximal-protection pole: government may not suppress
 *   speech on account of its content or viewpoint, and the sole harm
 *   exception reaches only advocacy directed to inciting imminent lawless
 *   action likely to produce it. The arrangement is maintained actively —
 *   courts invalidate a continuous stream of statutes regulating hateful,
 *   harassing, or extremist expression short of that line — and its costs
 *   land asymmetrically: the communities targeted by protected harmful
 *   expression absorb fear, exclusion, and defensive labor as an externality
 *   priced outside the doctrinal analysis, while speakers, provocateurs, and
 *   intermediary platforms collect protection and monetized reach. This file
 *   instantiates one reading of the speech_protection_boundary kernel; the
 *   harm_limited and balancing readings are separate constraint files with
 *   their own epsilon values, victim sets, and classifications, linked
 *   through the network section. Per the epsilon-invariance principle, the
 *   colloquial label 'speech protection' decomposes into these structurally
 *   distinct claims rather than one observable-dependent story. The claimed
 *   type is stated independently of the metrics below. KEY AGENTS (by
 *   structural relationship): - federal_judiciary: Agenda setter
 *   (institutional/identity_locked) — administers the boundary and strikes
 *   down encroaching regulation - dissenting_speakers_and_press: Primary
 *   beneficiary (organized/mobile) — shielded from content-based suppression
 *   - extremist_provocateur_speakers: Marginal beneficiary (moderate/mobile)
 *   — operates up to the incitement line at zero legal price -
 *   online_platform_intermediaries: Concentrated beneficiary
 *   (powerful/arbitrage) — hosts protected harmful expression without
 *   liability; captures the monetized gains -
 *   minoritized_targeted_communities: Primary payer (powerless/trapped) —
 *   absorbs the aggregate externality - individual_harassment_targets:
 *   Secondary payer (moderate/constrained) — partial recourse only -
 *   state_legislative_majorities: Institutional payer
 *   (institutional/constrained) — regulatory authority foreclosed -
 *   first_amendment_scholarship: Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.46).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.6).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Near-Absolute Speech Protection Boundary (Absolutist Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '8e134051-8be6-45cb-aeb2-efc3ebbd78fe').
narrative_ontology:cs_kernel_codification('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', fixed_text).
narrative_ontology:cs_authority_grounding('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', lineage).
narrative_ontology:cs_interpretation_layer_present('8e134051-8be6-45cb-aeb2-efc3ebbd78fe').
narrative_ontology:cs_reading_relation('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', foundational, dignitary_harm_no_suppression_basis).
narrative_ontology:cs_axiom_status(dignitary_harm_no_suppression_basis, holdable).
narrative_ontology:cs_axiom_grounding('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', dignitary_harm_no_suppression_basis, deontological).
narrative_ontology:cs_axiom('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', foundational, imminent_lawless_action_sole_exception).
narrative_ontology:cs_axiom_status(imminent_lawless_action_sole_exception, holdable).
narrative_ontology:cs_axiom_grounding('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', imminent_lawless_action_sole_exception, conventional).
narrative_ontology:cs_axiom('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', secondary, counterspeech_remedies_harm).
narrative_ontology:cs_axiom_status(counterspeech_remedies_harm, holdable).
narrative_ontology:cs_axiom_grounding('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', counterspeech_remedies_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', brandenburg_categorical_protection).
narrative_ontology:cs_drift_state('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', contemporary_post_counterman_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8e134051-8be6-45cb-aeb2-efc3ebbd78fe', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, dissenting_speakers_and_press).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, extremist_provocateur_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, online_platform_intermediaries).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_targeted_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, individual_harassment_targets).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, state_legislative_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The federal courts, culminating in the Supreme Court, decide which categories of expression fall outside protection and strike down statutes that reach inside the protected zone. Every term brings petitions asking the Court to widen or narrow the line — new communications technology, new harassment patterns, new national-security arguments. The justices cannot delegate the line-drawing to anyone else, and the institution's standing rests on being seen as the faithful guardian of the speech guarantee; abandoning that role would cost it the authority it exercises everywhere else. Leaving the role is unavailable to a court: the only exits are doctrinal reversal, which carries heavy legitimacy costs, or passive drift, which invites the other branches to fill the vacuum.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Journalists, protesters, whistleblowers, dissident political movements, and unpopular religious or ideological speakers operate under a guarantee that their expression cannot be suppressed because of its content or viewpoint. What flows to them is a durable shield: they publish, assemble, and advocate without first clearing their message with officials. Their exposure is residual — prosecution remains possible at the margins for leaks, true threats, or coordination with violence — but the ordinary instruments of suppression are off the table. Exit is unnecessary: the protection follows them wherever they speak within the jurisdiction.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, dissenting_speakers_and_press, beneficiary,
    organized, generational, mobile, national).

% Agitators, provocateurs, and extremist organizers exploit the widest possible protected set: they demonize groups, harass opponents, and organize rage, stopping precisely short of the point where advocacy becomes direct incitement to imminent violence. What flows to them is operational space — rallies, viral reach, recruitment pipelines — at zero legal price. Their activity depends on the line staying where it is; a narrower unprotected set would expose their core methods. They can relocate, rebrand, or go quiet at will.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, extremist_provocateur_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Social media companies and hosting services carry user-generated expression, including defamatory, harassing, and extremist material, without liability for most of it, because the same near-absolute protection that shields speakers shields the conduits. What flows to them is engagement — attention, data, advertising revenue — monetized without the filtering costs a stricter regime would impose. They operate across many legal systems and calibrate per jurisdiction: strictest rules where local law demands, maximal latitude where the American standard applies. Their exit is portfolio management across regimes.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, online_platform_intermediaries, beneficiary,
    powerful, biographical, arbitrage, global).

% Religious, racial, ethnic, and sexual minorities absorb the cumulative weight of expression the arrangement places beyond legal reach: demonstrations outside their places of worship, coordinated harassment campaigns, dehumanizing mass messaging, recruitment propaganda. None of it, in the typical case, crosses the incitement line, so none of it is actionable. What flows from them is the cost — fear, exclusion, defensive expenditure, the labor of counterspeech — while the decision about how much of this they must absorb is made elsewhere. Relocation does not help; the speech environment is the country itself.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_targeted_communities, payer,
    powerless, generational, trapped, national).

% Private individuals caught in viral pile-ons, workplace or campus harassment campaigns, and sustained defamatory attacks find that the expressive components of their ordeal are largely beyond legal remedy unless a narrow category — a true threat, or defamation meeting strict standards — happens to fit. What flows from them is reputational and psychological damage with only partial recourse. They can sometimes change schools, jobs, or cities, but the recorded speech persists and follows them.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, individual_harassment_targets, payer,
    moderate, biographical, constrained, national).

% State legislatures repeatedly pass statutes addressing hateful displays, harassing protests, nonconsensual imagery, and algorithmic amplification, and watch them struck down in whole or in part. What flows from them is the loss of a regulatory instrument their constituents demand; each failed statute consumes political capital and litigation expense. They cannot opt out of the federal limitation, and their successive attempts supply the enforcement occasions that keep the boundary polished.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, state_legislative_majorities, payer,
    institutional, biographical, constrained, regional).

% Constitutional scholars, comparative-law researchers, and civil-liberties historians document how the line is drawn, track the accumulating categories of unprotected expression, and compare the American settlement with dignity-based regimes abroad. They collect nothing from the arrangement and bear none of its direct costs; their output shapes the terms in which future courts and legislatures argue about it.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, first_amendment_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__absolutist_reading, online_platform_intermediaries).
narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the credible-commitment problem of dissent protection: by drawing a bright, judicially policed line that removes case-by-case discretion, the arrangement assures every potential dissenter that today's majority cannot weaponize speech law against tomorrow's opposition, and assures speakers they need not pre-clear messages with officials. It also economizes on adjudication: fixed categories replace open-ended harm inquiries in the vast run of cases.
% TRANSFER_FUNCTION: Moves the costs of harmful-but-protected expression — fear, exclusion, reputational injury, harassment burden, defensive counterspeech labor — from speakers and intermediaries onto the people and groups the expression targets; and moves regulatory authority over expression from legislative majorities to the courts, permanently.
% ABSENT_VOICES: The targets of protected harmful speech had no seat where the line was drawn: doctrinal formation occurred in cases litigated by speakers and the state, with targets appearing only as incidental plaintiffs who almost always lost. Comparative jurisdictions and international human-rights bodies that treat dignitary-harm regulation as routine stand wholly outside the American conversation. Legislative majorities speak, but only to lose.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, every queued restriction would enact within months: hate-speech statutes, harassment codes, protest-buffer laws, amplification rules. Investigative journalism, protest movements, and the platform economy would reorganize around a permission-based regime, and political coalitions would form around capturing whoever controlled the new licensing power. Nothing about the current arrangement is self-enforcing — it is maintained against constant legislative pressure and would not survive its guardian's withdrawal.
% FOUNDING_PROBLEM: Government suppression of political dissent: seditious-libel prosecutions, wartime loyalty prosecutions, and loyalty-oath regimes demonstrated that majorities and executives predictably criminalize opposition speech whenever given discretion. The arrangement was built to remove that discretion categorically.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary coalition: the historical record of Espionage Act prosecutions and loyalty-oath dismissals; the continuing use of leak prosecutions and subpoena threats against journalists; and — most tellingly — the civil-rights movement's own reliance on the doctrine, attested by the very communities that now bear the externality costs. Cross-national evidence that democracies lacking such a boundary prosecute dissenters more readily corroborates the problem's persistence. No serious participant disputes that the founding problem exists; the dispute is over what solving it now costs.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).
:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.46 from this reading's own lights over the standing arrangement (near-absolute protection with the Brandenburg line as the sole harm exception): the reading concedes that the arrangement imposes real, recurring, asymmetrically distributed costs on targeted communities — that concession is built into the doctrine itself, which draws the incitement line precisely because some expression causes actionable harm — but prices those costs as the necessary charge for a structure whose primary function is protecting dissent against majoritarian suppression. Sibling readings assessing the same referent would author materially higher values; this file does not average them in. Suppression (0.60) records the arrangement's coercive overhead: the boundary is not self-enforcing — it is maintained by courts striking down a continuous stream of popular legislation, and it simultaneously forecloses the legal-remedy path for targets of protected harmful speech. Suppression is authored as a raw structural property; only extractiveness is scaled downstream. Theater (0.22) is low-moderate: marketplace-of-ideas rhetoric is partly ceremonial, but the doctrine performs substantive work in nearly every term. Accessibility_collapse (0.38): alternatives persist for most seats — counterspeech, private platform moderation, narrow torts, relocation — though they collapse almost completely for trapped target communities. Resistance (0.62): the arrangement meets persistent, organized resistance — campus-code movements, anti-harassment ordinances, comparative-regime pressure, recurring legislative attempts — and survives it. All three tracked series share one six-point grid (T=0..55, roughly 1969–2024); end-state values match the scalar base_properties. The slow rise in base_extractiveness tracks the migration of harmful expression onto monetized platforms, which enlarged the externality without moving the line; the rising suppression_requirement tracks the intensifying enforcement workload as legislative attempts accumulate.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the dissenting-speaker and platform seats the arrangement is a shield they never paid for — coordination at negligible personal cost. From the trapped target-community seat the same structure operates as enforced cost-bearing: the decision about how much harm they absorb is made by others, and exit does not exist. From the judiciary's seat the arrangement is stewardship of a founding-level commitment. The engine computes these divergent per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidy end: dissenting speakers (mobile exit, generational stake) and provocateurs (mobile, immediate horizon) derive pure protection; platforms (arbitrage-grade exit across jurisdictions) sit nearest the beneficiary pole and monetize the widest protected set. Declared victims sit near the target end: minoritized communities are trapped — no exit from the domestic speech environment — and bear the aggregate externality; individual harassment targets are constrained; state legislative majorities are institutional payers whose regulatory authority is permanently transferred to the courts. The judiciary declares no beneficiary or victim position; it enters through its agenda-setting role, and its legitimacy income from guarding the boundary is noted qualitatively rather than forced through the derivation. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms produce the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — majoritarian and executive suppression of dissent — is live, corroborated from outside the beneficiary coalition by the prosecution record and by the targeted communities' own historical reliance on the doctrine. Disappearance would rearrange the world immediately. The classification therefore resists two symmetrical errors: reading the genuine victims as proof of pure extraction (which would erase the coordination function that historically saved the civil-rights movement and still shields every unpopular speaker), and reading the genuine coordination as proof of universal benefit (which would erase the uncompensated externality the structural delta names). The mandate has not outlived its function; no sunset applies; theatrical maintenance is minimal. Coordination and extraction run through the same line, which is the honest structural claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the speech_protection_boundary kernel; what would change structurally if a sibling reading (harm_limited_reading or balancing_reading) displaced it?',
    'Track doctrinal displacement events: a Supreme Court majority adopting dignitary-harm exceptions, a constitutional amendment, or sustained circuit-level divergence would signal reading replacement; monitor the composition of the unprotected set across decisions.',
    'Under harm_limited_reading, minoritized_targeted_communities migrate from payer toward protected seat, the victim set contracts to narrowly defined categories, and epsilon over the same referent rises sharply; under balancing_reading, epsilon becomes case-indexed and the bright-line coordination function dissolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed structure of the speech protection kernel').

omega_variable(
    externality_pricing_dispute,
    'Does the counterspeech remedy actually compensate the aggregate harm borne by targeted communities, or is the externality uncompensated?',
    'Longitudinal studies of counterspeech outcomes against coordinated harassment campaigns; cross-national comparison of minority-welfare indicators under dignity-based versus near-absolute regimes.',
    'If counterspeech systematically fails, the arrangement''s coordination claim weakens and the payer seats'' effective extraction approaches full-target levels, pushing computed classifications toward snare-flavored verdicts; if it succeeds, the arrangement sits nearer pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_pricing_dispute, empirical, 'Whether the externality on targets is compensated by the remedy the arrangement provides').

omega_variable(
    historical_beneficiary_inversion,
    'The communities now bearing the externality were historically the doctrine''s principal rescued beneficiaries (civil-rights litigation, NAACP v. Button); does current cost-bearing negate the coordination function, or mark a distributional shift within a working structure?',
    'Cohort analysis of which groups invoked the doctrine successfully across eras, versus which groups bear its costs in the current period.',
    'If the inversion is permanent, the beneficiary structure has rotated and the coordination justification weakens for the current cohort; if the protection remains fully available to targeted groups when they speak, the structure retains its coordination core and the costs are distributional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_beneficiary_inversion, conceptual, 'Rotation of beneficiary and cost-bearing populations across the doctrine''s life').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_absolutist_tr_t0, speech_protection_boundary__absolutist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(speech_absolutist_tr_t0, observed).
narrative_ontology:measurement(speech_absolutist_tr_t11, speech_protection_boundary__absolutist_reading, theater_ratio, 11, 0.16).
narrative_ontology:measurement_basis(speech_absolutist_tr_t11, observed).
narrative_ontology:measurement(speech_absolutist_tr_t22, speech_protection_boundary__absolutist_reading, theater_ratio, 22, 0.18).
narrative_ontology:measurement_basis(speech_absolutist_tr_t22, observed).
narrative_ontology:measurement(speech_absolutist_tr_t33, speech_protection_boundary__absolutist_reading, theater_ratio, 33, 0.19).
narrative_ontology:measurement_basis(speech_absolutist_tr_t33, observed).
narrative_ontology:measurement(speech_absolutist_tr_t44, speech_protection_boundary__absolutist_reading, theater_ratio, 44, 0.21).
narrative_ontology:measurement_basis(speech_absolutist_tr_t44, observed).
narrative_ontology:measurement(speech_absolutist_tr_t55, speech_protection_boundary__absolutist_reading, theater_ratio, 55, 0.22).
narrative_ontology:measurement_basis(speech_absolutist_tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(speech_absolutist_be_t0, speech_protection_boundary__absolutist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(speech_absolutist_be_t0, observed).
narrative_ontology:measurement(speech_absolutist_be_t11, speech_protection_boundary__absolutist_reading, base_extractiveness, 11, 0.41).
narrative_ontology:measurement_basis(speech_absolutist_be_t11, observed).
narrative_ontology:measurement(speech_absolutist_be_t22, speech_protection_boundary__absolutist_reading, base_extractiveness, 22, 0.43).
narrative_ontology:measurement_basis(speech_absolutist_be_t22, observed).
narrative_ontology:measurement(speech_absolutist_be_t33, speech_protection_boundary__absolutist_reading, base_extractiveness, 33, 0.44).
narrative_ontology:measurement_basis(speech_absolutist_be_t33, observed).
narrative_ontology:measurement(speech_absolutist_be_t44, speech_protection_boundary__absolutist_reading, base_extractiveness, 44, 0.45).
narrative_ontology:measurement_basis(speech_absolutist_be_t44, observed).
narrative_ontology:measurement(speech_absolutist_be_t55, speech_protection_boundary__absolutist_reading, base_extractiveness, 55, 0.46).
narrative_ontology:measurement_basis(speech_absolutist_be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(speech_absolutist_su_t0, speech_protection_boundary__absolutist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(speech_absolutist_su_t0, observed).
narrative_ontology:measurement(speech_absolutist_su_t11, speech_protection_boundary__absolutist_reading, suppression_requirement, 11, 0.52).
narrative_ontology:measurement_basis(speech_absolutist_su_t11, observed).
narrative_ontology:measurement(speech_absolutist_su_t22, speech_protection_boundary__absolutist_reading, suppression_requirement, 22, 0.54).
narrative_ontology:measurement_basis(speech_absolutist_su_t22, observed).
narrative_ontology:measurement(speech_absolutist_su_t33, speech_protection_boundary__absolutist_reading, suppression_requirement, 33, 0.56).
narrative_ontology:measurement_basis(speech_absolutist_su_t33, observed).
narrative_ontology:measurement(speech_absolutist_su_t44, speech_protection_boundary__absolutist_reading, suppression_requirement, 44, 0.58).
narrative_ontology:measurement_basis(speech_absolutist_su_t44, observed).
narrative_ontology:measurement(speech_absolutist_su_t55, speech_protection_boundary__absolutist_reading, suppression_requirement, 55, 0.6).
narrative_ontology:measurement_basis(speech_absolutist_su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% The colloquial concept 'speech protection' decomposes into three structurally distinct constraints sharing one kernel (the First Amendment speech clause): this absolutist reading (protected set maximal; unprotected set = incitement to imminent lawless action; targeted communities bear aggregate harm as externality), the harm_limited_reading (protection conditional on absence of dignitary/equality harm; victim set contracts accordingly), and the balancing_reading (case-indexed protection; epsilon becomes adjudication-dependent). Pressure runs from this reading outward: because it currently governs, it defines the baseline against which the siblings argue and supplies the precedents they must displace. Each file carries its own stable epsilon; none averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
