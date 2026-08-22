% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__absolutist_reading, []).

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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Near-Absolute Speech Protection (Absolutist Reading of the Speech-Harm Boundary)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   A constitutional speech regime operates under a near-absolute protection
 *   rule: government may not restrict speech on the basis of its content or
 *   the harms it causes, except within a fixed, narrow set of unprotected
 *   categories (incitement to imminent lawless action, true threats,
 *   defamation, obscenity). Constitutional courts administer the rule,
 *   striking down content-based regulation and refining the unprotected
 *   categories case by case. This story instantiates the absolutist reading
 *   of the speech-harm boundary kernel: the arrangement under contest is the
 *   near-absolute protection regime itself, and epsilon is authored for that
 *   arrangement as the absolutist reading assesses it — the reading concedes
 *   that targets of harmful-but-protected speech bear its costs without
 *   remedy, and the authored epsilon records the magnitude of that cost
 *   allocation honestly while the reading contests its evaluation. The
 *   claim/metric gap is deliberate and structural: the reading presents the
 *   arrangement as universal coordination (every speaker protected by the
 *   same rule), while the authored structural claim is tangled_rope — the
 *   same bright line that coordinates all speakers' protection is the
 *   instrument that allocates harm costs to identifiable target seats. The
 *   engine computes per-seat classifications from the structural data; the
 *   divergence between the reading's self-presentation and the authored claim
 *   is part of the measurement the corpus exists to take. KEY AGENTS (by
 *   structural relationship): - constitutional_courts: Agenda-setter
 *   (institutional/constrained) — administers the boundary, collects
 *   interpretive authority with each enforcement act -
 *   political_and_media_speakers: Primary beneficiary (powerful/mobile) —
 *   near-absolute autonomy and immunity at mass reach -
 *   online_platform_operators: Primary beneficiary and receipt seat
 *   (institutional/arbitrage) — monetizes protected speech including the
 *   harmful share - general_speaking_public: Dual-positioned
 *   (beneficiary/payer) — protected speech, absorbed exposure -
 *   targets_of_hate_speech: Target seat (moderate/constrained) — bears
 *   dignity harms without remedy - targeted_harassment_victims: Target seat
 *   (moderate/trapped) — bears coordinated-harassment costs; exit is
 *   self-silencing - disinformation_targets: Target seat
 *   (moderate/constrained) — bears false-speech costs outside defamation -
 *   speech_regulation_proponents: Excluded (organized/constrained) — the
 *   foreclosed regulatory voice - legal_scholars: Analytical observer
 *   (moderate/analytical) — maps costs and tests premises, no decision power
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.68).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.66).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Near-Absolute Speech Protection (Absolutist Reading of the Speech-Harm Boundary)").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, 'da3dd7d5-4833-4352-9502-fcfa888511f0').
narrative_ontology:cs_kernel_codification('da3dd7d5-4833-4352-9502-fcfa888511f0', fixed_text).
narrative_ontology:cs_authority_grounding('da3dd7d5-4833-4352-9502-fcfa888511f0', lineage).
narrative_ontology:cs_interpretation_layer_present('da3dd7d5-4833-4352-9502-fcfa888511f0').
narrative_ontology:cs_reading_relation('da3dd7d5-4833-4352-9502-fcfa888511f0', speech_harm_boundary__harm_balancing_reading, forecloses).
narrative_ontology:cs_reading_relation('da3dd7d5-4833-4352-9502-fcfa888511f0', speech_harm_boundary__dignity_reading, forecloses).
narrative_ontology:cs_axiom('da3dd7d5-4833-4352-9502-fcfa888511f0', foundational, no_content_harm_balancing).
narrative_ontology:cs_axiom_status(no_content_harm_balancing, holdable).
narrative_ontology:cs_axiom_grounding('da3dd7d5-4833-4352-9502-fcfa888511f0', no_content_harm_balancing, deontological).
narrative_ontology:cs_axiom('da3dd7d5-4833-4352-9502-fcfa888511f0', foundational, fixed_narrow_unprotected_categories).
narrative_ontology:cs_axiom_status(fixed_narrow_unprotected_categories, holdable).
narrative_ontology:cs_axiom_grounding('da3dd7d5-4833-4352-9502-fcfa888511f0', fixed_narrow_unprotected_categories, conventional).
narrative_ontology:cs_axiom('da3dd7d5-4833-4352-9502-fcfa888511f0', secondary, counter_speech_sufficiency).
narrative_ontology:cs_axiom_status(counter_speech_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('da3dd7d5-4833-4352-9502-fcfa888511f0', counter_speech_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('da3dd7d5-4833-4352-9502-fcfa888511f0', near_absolute_speech_protection).
narrative_ontology:cs_drift_state('da3dd7d5-4833-4352-9502-fcfa888511f0', contemporary_digital_attention_economy, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('da3dd7d5-4833-4352-9502-fcfa888511f0', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, political_and_media_speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, online_platform_operators).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, general_speaking_public).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_hate_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targeted_harassment_victims).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, disinformation_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, targets_of_hate_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, general_speaking_public).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, viewpoint_neutrality_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, counter_speech_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, marketplace_of_ideas_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which speech restrictions survive constitutional review and which fall. Strike down content-based speech laws, maintain the fixed list of unprotected categories, and refine the categories' edges case by case. Each strike-down widens the judiciary's authority over speech governance. Stepping off the doctrine would mean overturning their own precedents at high legitimacy cost, so they administer the boundary rather than redesign it.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, constitutional_courts, beneficiary).

% Campaign actors, officeholders, and press organizations speak to mass audiences under near-absolute protection: no right of reply, no fairness obligation, broad immunity for attack speech and coverage. Harms their speech causes others carry no legal consequence for them. The protection follows them everywhere in the jurisdiction; giving it up would mean surrendering the immunity, which no heavy speaker does.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, political_and_media_speakers, beneficiary,
    powerful, biographical, mobile, national).

% Run the channels where public discourse now happens, hosting and monetizing user speech including the harmful-but-protected share. The rule shields both what users post and the platforms' own curation decisions. Engagement economics convert provocative and harmful speech into advertising revenue. The protection is portable across jurisdictions, so operators can relocate or re-scope moderation without losing its value.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, online_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Hold the same categorical protection as the powerful: their speech cannot be suppressed for its content or viewpoint, which is the rule's promise to everyone. The exposure runs in reverse too — they absorb everyone else's harmful-but-protected speech without recourse, and their replies compete against voices with far larger reach. Most never litigate either side of the rule; they live inside both.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, general_speaking_public, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, general_speaking_public, payer).

% Members of groups targeted by dignity-denying speech that falls outside the fixed unprotected categories. They carry the costs — social exclusion, threat climate, withdrawal from public discussion — with no legal claim, while their own speech stays protected by the same rule. They cannot exit their identity or their visibility; the remedy the framework offers is to answer the speech with more speech.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_hate_speech, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, targets_of_hate_speech, beneficiary).

% Individuals subjected to coordinated harassment — doxxing, pile-ons, sustained abuse — that does not meet the true-threat or incitement thresholds. The campaigns follow them across platforms and into offline life. The framework's remedies (counter-speech, private moderation, blocking) are the resources the harassment is designed to exhaust; leaving the platforms means self-silencing, which is the campaign's goal.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targeted_harassment_victims, payer,
    moderate, biographical, trapped, national).

% People and institutions harmed by false speech about them that falls outside narrow defamation doctrine — public-figure status, matters of public concern, opinion privilege. They bear reputational and epistemic damage with no cause of action; correction travels slower than the falsehood in an attention market built for reach.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, disinformation_targets, payer,
    moderate, biographical, constrained, national).

% Legislative majorities, city governments, and civil-society coalitions that try to regulate hate speech, harassment, and disinformation. Their statutes are struck down or never introduced because the outcome is known in advance. They hold no seat in the framework except as losing litigants; keeping this seat empty is what the doctrine's enforcement exists to do.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speech_regulation_proponents, excluded,
    organized, biographical, constrained, national).

% Constitutional scholars and comparative-law researchers who document the rule's cost incidence, test its empirical premises, and design alternative boundary arrangements. They hold no decision power; their work enters the system only through briefs, citations, and the occasional judicial conversion.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, legal_scholars, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the censor-selection problem: any rule that lets the state restrict some harmful speech must decide who identifies the harm, and that decision power is historically captured by incumbents to suppress opposition. The categorical rule removes the state's harm-identification discretion entirely — every speaker holds the same protection, no one must trust the state's judgment about which viewpoints count as harmful, and dissent is protected as a side effect of protecting everything.
% TRANSFER_FUNCTION: Moves the costs of harmful-but-protected speech — dignity injury, harassment burden, reputational and epistemic damage, participation chilling — from speakers to targets with no compensating transfer; moves speech-governance authority from legislatures to courts; confers near-absolute immunity from speech-based liability on speakers, press, and platforms.
% ABSENT_VOICES: Targets whose harms fall outside the fixed unprotected set are present in the doctrine only as losing claimants; the framework's categories define which harms count, so the dignity-based objection has no seat inside the boundary's administration. Legislative and civil-society proponents of hate-speech, harassment, and disinformation regulation are structurally excluded — their proposals are struck down or deterred pre-enactment, and their exclusion is the enforcement object. Comparative-jurisdiction experience with dignity-protective boundary designs enters the doctrine's self-understanding only as foreign and dismissible.
% DISAPPEARANCE_RATIONALE: If the near-absolute rule vanished overnight, speech law would rearrange within a single legislative session: content-based regulation of harassment, hate speech, and disinformation would pass in most jurisdictions, platform liability and moderation incentives would shift immediately, and political actors would lose the immunity that structures campaign attack speech. The anti-censorship coordination function would have to be rebuilt under whatever boundary design replaced it — the rearrangement is total, which is why the beneficiary seats defend the arrangement at constitutional scale.
% FOUNDING_PROBLEM: State suppression of dissent: seditious libel, prior restraint, viewpoint-based prosecution of political opposition — the historical record the categorical rule was built to make categorically unavailable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the sedition-prosecution record (Alien and Sedition Acts, wartime prosecutions, the loyalty-program era) is documented by historians with no stake in the speech interest, and ongoing content-based regulatory attempts are litigated by adverse parties and recorded in case law. The corroboration covers the founding problem's liveness, not the claim that the present harm-cost allocation remains necessary — that necessity claim is attested only from within the beneficiary set; dignity-focused constitutional scholarship and comparative constitutional practice dispute it, and no seat outside the beneficiary set corroborates it.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) but not snare-grade because the extracting structure and the coordinating structure are the same rule: targets of harmful speech are themselves protected speakers under the same bright line, so the arrangement damps its own extraction at every seat that also holds the speaker benefit. Suppression (0.66) is structural, not internalized — this is an institutional constraint, and the suppression consists of the enforcement machinery actively nullifying regulatory alternatives (statutes struck down, legislative proposals deterred pre-enactment) plus the foreclosure of the target's legal-remedy channel; it is a raw structural property, unscaled by power or scope in the engine's arithmetic. Theater (0.30) reflects a real daily doctrinal workload with a growing performative share — absolutist identity ritual, and selective invocation of the free-speech principle by institutions that carve themselves exceptions. Accessibility collapse (0.58): once the doctrine is understood, most regulatory alternatives collapse categorically, but margins persist — the unprotected set itself, time-place-manner regulation, private platform moderation, and tort boundaries outside the doctrine. Resistance (0.62): continuous legislative re-attempts, sustained scholarly repudiation, comparative-jurisdiction divergence, recurring dissents. All three temporal series share one grid (T=0..55, roughly decade steps from the 1969 doctrinal settlement); extractiveness and suppression rise as the attention economy scales the reach of harmful-but-protected speech, dating the extraction accumulation to the digital-media era. Receipt surface: the material gains of the harm-cost transfer concentrate in the platform seat, which converts protected harmful speech into revenue at zero liability — gain_flow names that seat; the media and political seats hold the autonomy share, which is constitutive rather than monetized receipt. Fixing cost is prohibitive: the fixer (the courts) could only fix by categorical re-architecture of the boundary, since the bright line's design forbids incremental repair; precedent and legitimacy costs, plus the risk of re-arming the censor-selection problem, exceed the benefit of fixing for the seat that could do it.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and payer seats compute different types from identical doctrine. From the platform and mass-speaker seats the arrangement is the load-bearing wall of a free polity: categorical protection is what makes dissent, journalism, and opposition speech possible, and the harm costs are the visible price of a rule that cannot be made discretionary without being captured. From the harassment-target and hate-speech-target seats the same rule is an enforced cost allocation: the harm is foreseeable, concentrated, and remedy-less, and the offered remedy (counter-speech) is the resource the harm is designed to exhaust. The dual-positioned seats (general_speaking_public; targets_of_hate_speech with secondary beneficiary status) show the seat-dependence within a single agent: the same person is subsidized when speaking and extracted-from when targeted. The agenda-setter seat (constitutional_courts) experiences the arrangement as doctrinal craftsmanship and institutional role; the excluded seat (speech_regulation_proponents) experiences it as a locked door whose locking is the point.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: online_platform_operators sit nearest the beneficiary pole (arbitrage-grade exit — the protection is jurisdiction-portable and monetizable), political_and_media_speakers near them (mobile, subsidized wherever they speak within the jurisdiction), general_speaking_public near-symmetric (dual role: protected speech, absorbed exposure). Victim declarations map to high directionality: targeted_harassment_victims sit nearest the full-target pole (trapped — the campaigns follow them, and exit is self-silencing, the harm's objective); targets_of_hate_speech high but damped by their own protected-speaker status (constrained exit, secondary beneficiary role); disinformation_targets high with partial damping. The agenda-setter (constitutional_courts) carries a mild beneficiary tilt via authority accrual — each enforcement act concentrates interpretive power in the court, which the secondary beneficiary role records. Scope: the doctrine is national in codification but its extraction scales through global platform distribution; the larger effective scope raises verification difficulty and amplifies effective extraction on the target seats. Coalition note: the target seats' coalition potential (civil-society counter-mobilization, collective documentation, coordinated rebuttal) is the collective form of the counter-speech remedy and is the main force damping their effective directionality below the full-target pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state suppression of dissent through seditious libel, prior restraint, and viewpoint prosecution — is live: governments continuously attempt content-based regulation, and the doctrine is invoked against real suppression attempts on a rolling basis. The constraint is therefore not mandatrophy-resolved and not a piton candidate: its coordination function is exercised, not performed. The tangled_rope classification does double preventive work: it blocks the absolutist self-presentation (pure universal coordination, everyone a net beneficiary) from erasing the asymmetric extraction onto target seats, and it blocks the dignity critic's counter-framing (pure speaker-class extraction) from erasing the anti-censorship function that target seats themselves consume as speakers. The drift risk runs in the opposite direction from atrophy: if censorship pressure ever collapsed while the arrangement persisted, it would drift piton-ward (function atrophied, maintenance theatrical); the rising extractiveness series shows the opposite trajectory — extraction accumulating on a live function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'This constraint is one reading of kernel speech_harm_boundary (absolutist_reading): what structurally changes under the sibling readings speech_harm_boundary__harm_balancing_reading and speech_harm_boundary__dignity_reading?',
    'Author the sibling stories and compare victim sets, unprotected-category sets, and epsilon: the harm-balancing sibling makes the unprotected set elastic to demonstrated harm (targets gain remedies, speakers lose categorical immunity); the dignity sibling adds personhood-denying speech to the unprotected set (hate-speech targets gain remedies; administration moves from categorical to substantive).',
    'The disagreement is located in the harm-override threshold''s height, the fixity of the unprotected set, and the administration mode; adopting a sibling reading re-sits the boundary and redistributes extraction between speaker and target seats — this story''s classification holds only for the absolutist instantiation and must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Committer structure: one of three readings of the speech-harm boundary kernel; sibling readings instantiate different constraints.').

omega_variable(
    extraction_necessity_ambiguity,
    'Is the harm-cost allocation borne by targets a necessary price of the anti-censorship coordination function, or asymmetric extraction that narrower tailoring could reduce without sacrificing the function?',
    'Comparative natural experiment: in jurisdictions with dignity-protective or balancing boundary designs, does categorical protection produce measurably stronger dissent protection net of other institutions, and do expanded unprotected categories measurably increase state suppression of dissent?',
    'If the coordination benefit survives expanded categories, the extraction component is contingent and the constraint drifts snare-ward; if not, the extraction is the price of the coordination and the tangled_rope classification is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_necessity_ambiguity, empirical, 'Whether the target-borne harm costs are constitutive of the coordination function or separable extraction.').

omega_variable(
    counter_speech_remedy_adequacy,
    'Is counter-speech — the remedy the framework offers targets — adequate against asymmetric-reach harms (viral harassment, coordinated brigading, algorithmic amplification), or does the remedy gap make the measured suppression understated?',
    'Longitudinal participation data for targeted individuals and groups: does targeted speech produce durable participation chilling that counter-speech fails to offset, and does platform amplification systematically outrun rebuttal?',
    'If counter-speech is structurally inadequate for high-reach harms, the arrangement''s effective suppression of targets is higher than the structural measure and the target seats sit nearer the full-target pole than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_speech_remedy_adequacy, empirical, 'Whether the framework''s offered remedy actually offsets the harm costs it allocates to targets.').

omega_variable(
    category_fixity_under_digital_harms,
    'Can the fixed unprotected set absorb digital-era harm vectors (coordinated harassment, synthetic media, algorithmic amplification) through category refinement, or does fixity itself become the mechanism by which harm costs scale onto targets?',
    'Track doctrinal refinement cases against measured harm incidence: if refined categories (true-threat mens rea requirements, harassment-adjacent torts) keep pace with the harm vectors, fixity is stable doctrine; if incidence grows while the set stays fixed, fixity is doing allocative work.',
    'If fixity is the scaling mechanism, the rising extractiveness series dates a drift toward snare from the digital-media inflection; if categories keep pace, the tangled_rope classification is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_fixity_under_digital_harms, empirical, 'Whether fixed category boundaries scale harm costs in the digital environment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__absolutist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t11, speech_harm_boundary__absolutist_reading, theater_ratio, 11, 0.18).
narrative_ontology:measurement_basis(spee_tr_t11, observed).
narrative_ontology:measurement(spee_tr_t22, speech_harm_boundary__absolutist_reading, theater_ratio, 22, 0.21).
narrative_ontology:measurement_basis(spee_tr_t22, observed).
narrative_ontology:measurement(spee_tr_t33, speech_harm_boundary__absolutist_reading, theater_ratio, 33, 0.24).
narrative_ontology:measurement_basis(spee_tr_t33, observed).
narrative_ontology:measurement(spee_tr_t44, speech_harm_boundary__absolutist_reading, theater_ratio, 44, 0.27).
narrative_ontology:measurement_basis(spee_tr_t44, observed).
narrative_ontology:measurement(spee_tr_t55, speech_harm_boundary__absolutist_reading, theater_ratio, 55, 0.3).
narrative_ontology:measurement_basis(spee_tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__absolutist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t11, speech_harm_boundary__absolutist_reading, base_extractiveness, 11, 0.48).
narrative_ontology:measurement_basis(spee_be_t11, observed).
narrative_ontology:measurement(spee_be_t22, speech_harm_boundary__absolutist_reading, base_extractiveness, 22, 0.53).
narrative_ontology:measurement_basis(spee_be_t22, observed).
narrative_ontology:measurement(spee_be_t33, speech_harm_boundary__absolutist_reading, base_extractiveness, 33, 0.58).
narrative_ontology:measurement_basis(spee_be_t33, observed).
narrative_ontology:measurement(spee_be_t44, speech_harm_boundary__absolutist_reading, base_extractiveness, 44, 0.63).
narrative_ontology:measurement_basis(spee_be_t44, observed).
narrative_ontology:measurement(spee_be_t55, speech_harm_boundary__absolutist_reading, base_extractiveness, 55, 0.68).
narrative_ontology:measurement_basis(spee_be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__absolutist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t11, speech_harm_boundary__absolutist_reading, suppression_requirement, 11, 0.54).
narrative_ontology:measurement_basis(spee_su_t11, observed).
narrative_ontology:measurement(spee_su_t22, speech_harm_boundary__absolutist_reading, suppression_requirement, 22, 0.57).
narrative_ontology:measurement_basis(spee_su_t22, observed).
narrative_ontology:measurement(spee_su_t33, speech_harm_boundary__absolutist_reading, suppression_requirement, 33, 0.6).
narrative_ontology:measurement_basis(spee_su_t33, observed).
narrative_ontology:measurement(spee_su_t44, speech_harm_boundary__absolutist_reading, suppression_requirement, 44, 0.63).
narrative_ontology:measurement_basis(spee_su_t44, observed).
narrative_ontology:measurement(spee_su_t55, speech_harm_boundary__absolutist_reading, suppression_requirement, 55, 0.66).
narrative_ontology:measurement_basis(spee_su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the speech-harm boundary' decomposes into three structurally distinct constraints — the absolutist, harm-balancing, and dignity readings of the same kernel — each with its own epsilon, unprotected-category set, and victim structure. The readings are not observables of one constraint: measuring the boundary by the balancing standard yields a different epsilon than measuring it by the categorical standard, so per the epsilon-invariance principle they are separate stories linked by network edges. The absolutist settlement structurally influences both siblings: it raises the adoption cost of balancing or dignity designs within the jurisdiction (constitutional amendment or doctrinal rupture is required), which is what these edges record. This story links to both family members; neither sibling story is described or classified here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
