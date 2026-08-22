% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: Harm-Limited Reading of Speech Protection: Immunity Yields on Demonstrated Unconsented-To Harm
 *   domain: constitutional law/political philosophy/speech regulation
 *
 * SUMMARY:
 *   This story instantiates the harm-limited reading of the First Amendment
 *   speech-protection kernel: expressive protection is the default condition,
 *   but it yields when expression causes demonstrable, unconsented-to harm,
 *   at which point regulation and liability become constitutionally
 *   permissible. The arrangement solves a real boundary problem for a
 *   tolerant polity while imposing real, enforced losses on the speakers it
 *   reaches and concentrating line-drawing authority in the courts. Per the
 *   epsilon-invariance principle, the colloquial label 'First Amendment
 *   protection' decomposes into three structurally distinct constraints; this
 *   file authors only the harm-limited reading, with the absolutist and
 *   categorical-balancing readings as separate linked stories. The claim and
 *   the metrics are independent authored facts: the constraint is CLAIMED as
 *   tangled_rope, and the metrics describe moderately extractive, actively
 *   enforced operation whose intensity has grown as the harm concept
 *   broadened. KEY AGENTS (by structural relationship): -
 *   constitutional_courts: Agenda setter (institutional/constrained) —
 *   administers the harm test, accrues adjudicative authority with each
 *   finding - elected_legislatures: Co-agenda setter
 *   (institutional/constrained) — enacts the statutes the test permits -
 *   vulnerable_minorities: Primary beneficiary (powerless/trapped) — gain
 *   remedy access unavailable through counter-speech - harassment_targets:
 *   Secondary beneficiary (moderate/trapped) — their documented damage is the
 *   test's evidentiary object - general_listening_public: Diffuse beneficiary
 *   with indirect costs (organized/mobile) - harm_causing_speakers: Primary
 *   target (moderate/constrained) — lose immunity upon demonstrated harm -
 *   boundary_zone_speakers: Marginal target (moderate/constrained) — bear
 *   chilling costs at the adjudicated edge - platform_content_moderators:
 *   Dual-positioned enforcer (powerful/arbitrage) — pay compliance costs,
 *   collect liability shield - absolutist_civil_libertarians: Excluded critic
 *   (organized/constrained) — categorical objection has no entry point in the
 *   test - constitutional_scholars: Analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.48).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.58).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.21).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.21).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "Harm-Limited Reading of Speech Protection: Immunity Yields on Demonstrated Unconsented-To Harm").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional law/political philosophy/speech regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, '864881b2-401c-4b5a-91e0-2704222c1de0').
narrative_ontology:cs_kernel_codification('864881b2-401c-4b5a-91e0-2704222c1de0', fixed_text).
narrative_ontology:cs_authority_grounding('864881b2-401c-4b5a-91e0-2704222c1de0', lineage).
narrative_ontology:cs_interpretation_layer_present('864881b2-401c-4b5a-91e0-2704222c1de0').
narrative_ontology:cs_reading_relation('864881b2-401c-4b5a-91e0-2704222c1de0', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('864881b2-401c-4b5a-91e0-2704222c1de0', first_amendment_speech_protection__categorical_balancing_reading, influences).
narrative_ontology:cs_axiom('864881b2-401c-4b5a-91e0-2704222c1de0', foundational, expression_immunity_yields_to_demonstrated_unconsented_harm).
narrative_ontology:cs_axiom_status(expression_immunity_yields_to_demonstrated_unconsented_harm, holdable).
narrative_ontology:cs_axiom_grounding('864881b2-401c-4b5a-91e0-2704222c1de0', expression_immunity_yields_to_demonstrated_unconsented_harm, deontological).
narrative_ontology:cs_axiom('864881b2-401c-4b5a-91e0-2704222c1de0', foundational, demonstrability_requirement_as_censorship_guardrail).
narrative_ontology:cs_axiom_status(demonstrability_requirement_as_censorship_guardrail, holdable).
narrative_ontology:cs_axiom_grounding('864881b2-401c-4b5a-91e0-2704222c1de0', demonstrability_requirement_as_censorship_guardrail, instrumental).
narrative_ontology:cs_reference_frame('864881b2-401c-4b5a-91e0-2704222c1de0', harm_principle_bounded_expression).
narrative_ontology:cs_drift_state('864881b2-401c-4b5a-91e0-2704222c1de0', contemporary_platform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('864881b2-401c-4b5a-91e0-2704222c1de0', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, harassment_targets).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, general_listening_public).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, harm_causing_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, boundary_zone_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, platform_content_moderators).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, general_listening_public).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, platform_content_moderators).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, mill_harm_principle).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, demonstrable_harm_evidentiary_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide, case by case, whether a specific expression caused demonstrable harm to an identifiable person without their consent, and whether regulation or liability may therefore proceed. Each finding refines the standard future speakers and regulators will be held to. The court cannot decline the question once properly presented, and its authority over the speech boundary grows with each line it draws.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Enact the statutes the harm test permits: defamation rules, harassment statutes, incitement bans, calibrated to what courts will sustain. Electoral pressure after salient incidents pushes toward broader harm findings; judicial review pushes back. Their regulatory permission exists only inside the boundary the courts maintain.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, elected_legislatures, agenda_setter,
    institutional, immediate, constrained, national).

% Bear the brunt of harassing, defamatory, and inciting expression directed at them and typically lack the resources to answer it with counter-speech. The harm test gives them a legal pathway, through lawsuits and statutory remedies, that does not depend on winning a shouting match. They cannot leave the discourse environment in which expression about them circulates.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    powerless, biographical, trapped, national).

% Individuals subjected to sustained targeted expression, including stalking messages, coordinated pile-ons, and defamatory campaigns, that produces documented psychological, economic, or physical damage. Their remedy runs entirely through persuading an adjudicator that the damage is real and was not consented to.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, harassment_targets, beneficiary,
    moderate, biographical, trapped, national).

% Inhabits a discourse environment from which demonstrably damaging expression can be removed, and funds the enforcement apparatus through taxes and attention. Pays indirectly when speakers near the boundary grow cautious and the range of available expression narrows. Can disengage from any particular controversy but not from the ambient climate.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, general_listening_public, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, general_listening_public, payer).

% Publish or utter expression that demonstrably injures identifiable others, including defaming reputations, coordinating harassment, and inciting violence, and thereby lose the immunity other speakers retain. Liability, injunctions, and removal follow. Recasting claims as opinion or relocating venues offers partial escape but not immunity for what was already done.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, harm_causing_speakers, payer,
    moderate, biographical, constrained, national).

% Work deliberately close to the line: satirists, polemicists, investigative journalists, edgy performers, academics. For them the difference between protected provocation and actionable harm is exactly what is in dispute, and each adverse finding anywhere in their category raises their own exposure. Most keep speaking; some trim; a few are professionally defined by refusing to trim.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, boundary_zone_speakers, payer,
    moderate, biographical, constrained, national).

% Operate the infrastructure where most harmful expression now circulates. They enforce harm standards at scale through removal, demotion, and account termination, gaining a liability shield for good-faith enforcement while bearing compliance costs, appeal volumes, and accusations of bias from every direction. Their standards routinely run ahead of what courts would sustain.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, platform_content_moderators, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, platform_content_moderators, beneficiary).

% Organizations and jurists holding that any harm-based elasticity in speech protection will be exploited until it swallows the guarantee, citing the historical record of sedition prosecutions and dissent suppression. The harm test's evidentiary frame gives their categorical argument no entry point: they can litigate individual outcomes but cannot contest the standard itself from inside the proceeding.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, absolutist_civil_libertarians, excluded,
    organized, generational, constrained, national).

% Map the doctrine's evolution, publish critiques and defenses, and supply the theoretical vocabulary, including the harm principle, the chilling effect, and viewpoint neutrality, that judges and litigants borrow. They neither collect the proceeds nor bear the liabilities; their standing comes from describing the structure accurately enough that partisans on all sides cite them.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__harm_limited_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, adjudicable criterion marking where expressive immunity ends, so that speakers, targets, regulators, and platforms can predict which expression carries legal exposure without having to resolve the underlying social value of any particular utterance.
% TRANSFER_FUNCTION: Moves legal immunity away from speakers whose expression is found to cause demonstrable unconsented-to harm, and moves corresponding remedy rights to the targets of that expression; incidentally moves adjudicative authority to the courts that draw and redraw the line.
% ABSENT_VOICES: Absolutist civil libertarians would object that the elasticity of 'demonstrable harm' makes the exception swallow the rule, and boundary-zone speakers would object that the harm determination is made about them without a seat in the standard-setting process; both stand outside the harm test's evidentiary frame, which admits proof of damage but not categorical liberty arguments.
% DISAPPEARANCE_RATIONALE: If the harm yield vanished overnight and protection became categorical, harassment and defamation targets would lose every legal recourse short of counter-speech they cannot win, legislatures would lose the constitutional permission structure for every protective statute from libel law onward, platforms would face unmanageable liability in both directions, and the expression economy would reorganize around total speaker immunity.
% FOUNDING_PROBLEM: Strong free-speech commitments repeatedly collided with cases where expression inflicted serious, provable damage on identifiable people, including reputation-destroying defamation, targeted harassment, and incitement producing violence, with no principled stopping point short of total immunity; the harm limitation was built to supply that stopping point.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: courts' own published opinions document recurring concrete harms through measured defamation damages and documented harassment injuries; communications-science research quantifies psychological and economic damage from targeted expression; legal historians attest the pre-doctrine liability gap. Absolutist scholars corroborate that the underlying problem exists while disputing this particular solution, so corroboration of the problem does not rest on the parties who benefit from the remedy.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48: the withdrawal of immunity is formally bounded to demonstrated harm, but the effective burden on speakers exceeds the formal footprint because chilling effects extend into fully protectable territory and litigation risk falls on anyone near the line. Suppression is 0.58: enforcement is the constraint's operating mode (liability, injunctions, platform removal), yet it is confined to findings of demonstrated harm rather than generalized censorship; suppression is authored as a raw structural property and is not scaled by context, whereas the engine scales only extractiveness by directionality and scope. Theater is 0.21: adjudication is predominantly functional, with a growing minority of strategic harm-inflation rhetoric attached to it. Accessibility collapse is 0.35: alternatives persist, since the protected core of expression remains vast and reframing or relocation is often available. Resistance is 0.60: organized civil-liberties opposition, absolutist scholarship, and speaker-side pushback are persistent and occasionally successful at narrowing the doctrine. The temporal series run on one shared grid (points 0-60) so every tracked metric is authored at every examined time point; all three rise monotonically, tracking the historical broadening of the harm concept (from defamation and incitement toward emotional distress, hostile-environment theories, and platform-scale harm moderation) and the maturing of the enforcement infrastructure that applies it. No cyclical pattern is asserted: the drift is directional, driven by doctrinal accretion rather than oscillating external shocks.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute materially different constraints from the same doctrine. From the payer seats, the structure is an enforced withdrawal of immunity: a speaker on the wrong side of a harm finding experiences state-backed extraction, and boundary-zone speakers experience a moving line they did not draw. From the beneficiary seats, the same structure is remedy access and environmental safety that counter-speech alone cannot provide. From the agenda-setter seat, it is a manageable adjudicative docket and a stewardship role over the tolerance boundary. From the excluded seat, it is a category error: any elasticity is already the rule's defeat. Same-level divergence is sharpest between harm_causing_speakers and boundary_zone_speakers, who are nominally similar actors sorted not by power but by whether harm against their targets is demonstrable; the demonstrability line, not status, differentiates their exits. A subset of boundary-zone speakers carries an identity-lock dynamic: provocateurs and investigative journalists whose professional self-concept is constituted by transgressive expression cannot reframe without ceasing to be what they are, so for them the suppression is partly internalized and would persist after any formal barrier was removed; the class-level exit rating remains constrained because most members are not so fused.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for vulnerable_minorities, harassment_targets, and the general listening public: the constraint subsidizes them with remedy access and a scrubbed discourse environment, with the public near-symmetric since it also pays through narrowed expression and funded enforcement. Victim declarations drive high directionality for harm_causing_speakers and boundary_zone_speakers, amplified by their constrained exits: they cannot recover immunity for past expression and face a standard that tightens with each adverse finding elsewhere in their category. The agenda setters derive near-beneficiary directionality because the arrangement routes adjudicative authority to them; the receipt surface records this accrual explicitly. Platform moderators sit mid-range: they pay compliance costs but collect the liability shield, and their arbitrage-grade exit (global infrastructure, portable operations) damps their effective burden. No directionality overrides are authored: the beneficiary/victim declarations plus exit options reproduce the true structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents both standard mislabels. Reading the arrangement as pure coordination (a rope serving everyone) erases the real, enforced losses borne by speakers and the adjudicative power that accrues to the bench with every line drawn. Reading it as pure extraction (a censorship snare wearing a harm costume) erases the genuine coordination achievement: a shared, adjudicable tolerance boundary that keeps the overwhelming majority of expression immune while giving damaged targets a path that does not require winning a shouting match. The founding problem remains live, so no mandatrophy resolution is declared; the slowly rising theater ratio signals early proxy-drift, as harm rhetoric is increasingly deployed strategically by parties who would regulate disliked expression regardless of demonstration, and is the trend worth watching for a future transition rather than a present one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the first_amendment_speech_protection kernel; would instantiating a sibling reading (absolutist or categorical_balancing) change the beneficiary/victim structure and epsilon?',
    'Author the absolutist and categorical-balancing readings as separate constraint stories and compare computed types, epsilon, and seat structures across the family.',
    'Under the absolutist reading the harm yield disappears entirely: harm-causing speakers leave the victim set, vulnerable minorities lose remedy access, and epsilon falls toward coordination-cost levels. Under categorical balancing the demonstrability threshold dissolves into case-by-case weighing, making epsilon observer-relative and the victim set unpredictable in advance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the speech-protection kernel is instantiated, and what the siblings would change.').

omega_variable(
    demonstrable_harm_boundary,
    'Where does ''demonstrable unconsented-to harm'' end: does subjectively experienced offense count as harm, or only objectively verifiable damage?',
    'Longitudinal coding of adjudicated harm findings: track the ratio of objective-damage findings (economic loss, documented injury) to subjective-offense findings across the interval.',
    'A boundary drifting toward subjective offense sharply expands the regulated set, raising effective extraction on boundary-zone speakers and pushing the constraint toward snare-flavored suppression of disliked expression; a firm objective-damage boundary keeps the arrangement rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrable_harm_boundary, empirical, 'Elasticity of the harm concept that bounds the protected set.').

omega_variable(
    adjudicator_capture_direction,
    'Do harm determinations track demonstrable damage to vulnerable targets, or the sensibilities of the adjudicating majority, such that the harm test could be turned against dissident or minority speech itself?',
    'Historical and continuing audit of harm findings by speaker position: compare rates at which politically dissident, minority, and mainstream speakers lose protection on harm grounds, including the historical sedition-era record.',
    'If capture is real, the beneficiary structure inverts, with vulnerable minorities becoming targets of the enforcement machinery, and the classification flips from tangled_rope toward snare; if not, the protective function dominates and the current structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjudicator_capture_direction, empirical, 'Whether the harm standard serves its declared beneficiaries or their opponents.').

omega_variable(
    chilling_effect_extent,
    'How far beyond the formal harm boundary does the deterrent effect extend: how much fully protectable expression is never uttered because of uncertainty about harm adjudication?',
    'Survey and behavioral data on self-censorship among boundary-zone speakers (journalists, satirists, academics) correlated with enforcement salience and adverse-findings news cycles.',
    'Large chilling effects raise effective extraction well above the formal footprint and increase the internalized share of suppression, meaning the burden would persist after formal barriers were removed; negligible chilling keeps measured extraction close to formal yields.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_extent, empirical, 'Gap between the formal harm boundary and the behavioral deterrence boundary.').

omega_variable(
    consent_in_open_discourse,
    'What does ''unconsented-to'' mean in an open discourse commons where listeners cannot opt out of encountering expression: is mere exposure sufficient to establish non-consent?',
    'Doctrine-tracking conceptual analysis: determine whether adjudicators require targeting, foreseeability, and a realistic avoidance opportunity before finding non-consent, or accept exposure alone.',
    'Exposure-as-non-consent dramatically widens the regulated set, since any unwelcome expression would qualify; targeting-plus-foreseeability requirements keep it narrow. This single conceptual choice moves epsilon more than any observed metric drift in the series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_in_open_discourse, conceptual, 'The meaning of non-consent that gates the entire harm yield.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(firs_tr_t0, observed).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(firs_tr_t10, observed).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement_basis(firs_tr_t20, observed).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement_basis(firs_tr_t30, observed).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(firs_tr_t40, observed).
narrative_ontology:measurement(firs_tr_t50, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement_basis(firs_tr_t50, observed).
narrative_ontology:measurement(firs_tr_t60, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement_basis(firs_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(firs_be_t0, observed).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(firs_be_t10, observed).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(firs_be_t20, observed).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(firs_be_t30, observed).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement_basis(firs_be_t40, observed).
narrative_ontology:measurement(firs_be_t50, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 50, 0.47).
narrative_ontology:measurement_basis(firs_be_t50, observed).
narrative_ontology:measurement(firs_be_t60, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement_basis(firs_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(firs_su_t0, observed).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(firs_su_t10, observed).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement_basis(firs_su_t20, observed).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(firs_su_t30, observed).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(firs_su_t40, observed).
narrative_ontology:measurement(firs_su_t50, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement_basis(firs_su_t50, observed).
narrative_ontology:measurement(firs_su_t60, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(firs_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'First Amendment speech protection' conflates three structurally distinct claims with different epsilon values, beneficiary sets, and failure modes. The absolutist reading (categorical immunity, narrow historical exclusions) has near-zero extraction from speakers and no harm-boundary victims; the categorical-balancing reading (case-by-case weighing) has an observer-relative epsilon and an unpredictable victim set; this harm-limited reading has a stable, adjudicable epsilon with named beneficiaries and victims. The kernel text is upstream of all three; the harm-limited reading supplies the evidentiary threshold that categorical-balancing frameworks increasingly import, which is why this story links to both siblings. Any measurement of 'speech protection' that shifts epsilon with the observable used is measuring a different member of this family, not a different angle on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
