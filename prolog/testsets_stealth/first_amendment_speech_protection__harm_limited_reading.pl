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
 *   human_readable: Harm-Limited Reading of First Amendment Speech Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested constitutional kernel.
 *   The kernel is the First Amendment speech guarantee; this file authors the
 *   harm_limited_reading: protection yields when speech causes demonstrable,
 *   unconsented-to harm. Per the epsilon-referent rule, the authored
 *   extractiveness describes the harm-limited arrangement AS IT STANDS AND
 *   OPERATES: its doctrinal tests, its application history from the Espionage
 *   Act era through hostile-environment harassment law to platform-era threat
 *   regulation. It does not describe the absolutist alternative this reading
 *   competes with, nor an idealized perfectly-calibrated version of itself.
 *   KEY AGENTS (by structural relationship): federal_courts: agenda setter
 *   (institutional/constrained) — draws and polices the harm boundary,
 *   captures adjudicative centrality from each new controversy;
 *   vulnerable_minorities: primary beneficiary (moderate/trapped) — gain
 *   recourse against targeting, cannot exit the spaces where it occurs;
 *   ordinary_speakers: secondary beneficiary (moderate/mobile) — retain broad
 *   protection far from the boundary; disfavored_speakers: primary payer
 *   (moderate/constrained) — bear litigation risk and chill near the
 *   boundary; political_dissenters: payer (organized/constrained) —
 *   prosecuted under harm rationales in crisis periods;
 *   state_and_local_regulators: beneficiary with agenda-setting reach
 *   (institutional/arbitrage) — selective enforcement leverage;
 *   chilled_silent_speakers: excluded voice (powerless/trapped) — invisible
 *   to the record; first_amendment_scholars: analytical observer
 *   (analytical/analytical). The claim/metric gap is deliberate: claimed_type
 *   records the structure I believe true; the metrics record descriptive
 *   operation; the engine computes per-seat classifications from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - federal_courts: Agenda setter (institutional/constrained) — administers the harm boundary; captures adjudicative centrality from each new harm controversy
 *   - vulnerable_minorities: Primary beneficiary (moderate/trapped) — gain recourse against threatening and harassing expression; cannot exit the spaces where targeting occurs
 *   - ordinary_speakers: Secondary beneficiary (moderate/mobile) — retain broad protection; far from the boundary
 *   - disfavored_speakers: Primary payer (moderate/constrained) — provocateurs, offensive artists, controversial academics near the boundary; bear litigation risk and chill
 *   - political_dissenters: Payer (organized/constrained) — movements prosecuted under harm rationales in crisis periods
 *   - state_and_local_regulators: Beneficiary with agenda-setting reach (institutional/arbitrage) — gain enforceable authority once harm is credited; select theories and cases
 *   - chilled_silent_speakers: Excluded voice (powerless/trapped) — self-censor into invisibility; never enter the record
 *   - first_amendment_scholars: Analytical observer (analytical/analytical) — map the boundary's movement; supply arguments to all factions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.42).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.38).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "Harm-Limited Reading of First Amendment Speech Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, '6ab65a68-1abf-4a4f-8fa3-f82353899a86').
narrative_ontology:cs_kernel_codification('6ab65a68-1abf-4a4f-8fa3-f82353899a86', fixed_text).
narrative_ontology:cs_authority_grounding('6ab65a68-1abf-4a4f-8fa3-f82353899a86', lineage).
narrative_ontology:cs_interpretation_layer_present('6ab65a68-1abf-4a4f-8fa3-f82353899a86').
narrative_ontology:cs_reading_relation('6ab65a68-1abf-4a4f-8fa3-f82353899a86', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ab65a68-1abf-4a4f-8fa3-f82353899a86', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('6ab65a68-1abf-4a4f-8fa3-f82353899a86', foundational, harm_demonstration_limits_protection).
narrative_ontology:cs_axiom_status(harm_demonstration_limits_protection, holdable).
narrative_ontology:cs_axiom_grounding('6ab65a68-1abf-4a4f-8fa3-f82353899a86', harm_demonstration_limits_protection, deontological).
narrative_ontology:cs_axiom('6ab65a68-1abf-4a4f-8fa3-f82353899a86', secondary, harm_must_be_demonstrable_not_merely_offense).
narrative_ontology:cs_axiom_status(harm_must_be_demonstrable_not_merely_offense, holdable).
narrative_ontology:cs_axiom_grounding('6ab65a68-1abf-4a4f-8fa3-f82353899a86', harm_must_be_demonstrable_not_merely_offense, empirically_contingent).
narrative_ontology:cs_reference_frame('6ab65a68-1abf-4a4f-8fa3-f82353899a86', harm_bounded_presumptive_protection).
narrative_ontology:cs_drift_state('6ab65a68-1abf-4a4f-8fa3-f82353899a86', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6ab65a68-1abf-4a4f-8fa3-f82353899a86', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, ordinary_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, disfavored_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, political_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, state_and_local_regulators).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, millian_harm_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the speech guarantee, decide when a showing of harm justifies regulating expression, and bind lower courts and reviewing institutions to the resulting boundary. Each new harm controversy enlarges the judiciary's docket and its centrality to social conflict. The bench is bound by precedent, the case-or-controversy requirement, and appointment politics; it cannot simply step outside the framework it administers.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Disproportionately targeted by threatening, harassing, and intimidatory expression directed at who they are. The harm boundary gives them enforceable recourse they would lack under a categorically absolute guarantee. They cannot exit the discursive spaces where targeting occurs, and they typically act through proxy organizations because individual suits are expensive.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    moderate, biographical, trapped, national).

% The large majority whose expression never approaches the harm boundary. They retain broad protection for political, religious, artistic, and commercial expression, and they can adjust their conduct trivially if a particular mode of expression comes into question. They benefit from the stability of a settlement that keeps government suppression exceptional.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, ordinary_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Provocateurs, offensive artists, controversial academics, and critics of powerful institutions whose expression is plausibly classifiable as harmful by someone. They bear litigation risk, defense costs, and the pressure to self-censor. Their exit is silence, which for them means abandoning the work or the identity the expression expresses; they cannot easily stop speaking about what they know.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, disfavored_speakers, payer,
    moderate, biographical, constrained, national).

% Movements challenging state policy: war opponents, labor organizers, civil-rights demonstrators. In crisis periods their speech has repeatedly been prosecuted under harm rationales (espionage and sedition statutes, breach-of-peace and fighting-words arrests). Organization gives them partial defense capacity, but their dissent is precisely what the harm rationale is invoked against when the state is threatened.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, political_dissenters, payer,
    organized, biographical, constrained, national).

% Gain enforceable authority over expression once a harm showing is credited: prosecutors, human-rights commissions, university disciplinary bodies, workplace regulators. They choose which harm theories to advance and which cases to bring, so they hold selective-enforcement leverage unavailable to any other seat. They also absorb losses when courts reject their theories as insufficiently demonstrable.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, state_and_local_regulators, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, state_and_local_regulators, agenda_setter).

% People who decide not to speak, publish, protest, or teach because of what happened to visible speakers near the boundary. They generate no cases, no record, and no statistics; the body of doctrine is built entirely from disputes brought by those who spoke anyway. Their interests enter the system only through speculation and proxy claims.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, chilled_silent_speakers, excluded,
    powerless, biographical, trapped, national).

% Map the boundary's movement, publish critiques of its coherence and its applications, and supply the arguments that litigating factions deploy. They hold no direct stake in outcomes and no enforcement role; their influence runs through the quality of the arguments available to the other seats.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, first_amendment_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__harm_limited_reading, federal_courts).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a broad anti-suppression settlement among rival religious, political, and cultural factions: each accepts limits on its power to silence opponents in exchange for reciprocal protection of its own expression. The harm boundary supplies targeted recourse sufficient that those injured by unprotected expression retain a stake in the settlement instead of organizing to dismantle it.
% TRANSFER_FUNCTION: Moves regulatory authority over expression from speakers to public institutions whenever a factfinder credits a showing of demonstrable, unconsented-to harm; moves enforceable recourse and remedial power to targets of harmful expression; moves adjudicative centrality, caseload, and doctrinal agenda control to the courts.
% ABSENT_VOICES: Chilled speakers who self-censored never became cases and so never entered the record from which the boundary is drawn; the doctrine is structurally blind to the silenced. Targets of harmful expression without resources to litigate are present only through proxy organizations. The former would object to the boundary's instability; the latter to the expense and difficulty of demonstrating harm.
% DISAPPEARANCE_RATIONALE: If the harm limitation vanished overnight, the speech-regulation order would reorganize around whichever pole filled the vacuum: under a categorical rule, threat, incitement, harassment, and stalking law would collapse along with their administrative machinery; under an unbounded rule, regulation would expand without the demonstration discipline that currently checks it. Thousands of statutes, workplace regimes, and campus codes are built on the boundary's current placement.
% FOUNDING_PROBLEM: Reconcile a categorical textual guarantee ('no law ... abridging the freedom of speech') with the recognition that expression can operate as an instrument of injury: threats, fraud, incitement, harassment, defamation. The harm-limited formulation was built to hold both ends: presumptively broad protection for dissent and difference, with a disciplined yield at demonstrated, unconsented-to injury, so that neither majorities nor injured parties would abandon the settlement.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by a century of case law in which ideologically opposed majorities (the Holmes-Brandeis lineage, the Warren Court, the Rehnquist and Roberts Courts) each treated some harm-yield as unavoidable; by legal historians' documentation of the pre-doctrinal repression (Espionage Act prosecutions, postal censorship) that made the protective settlement necessary; and by the legislative findings accompanying threat, stalking, and harassment statutes. No wholly disinterested arbiter exists; the strongest external attestation is the cross-ideological convergence of opposing benches on the necessity of some harm boundary.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).
:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42): most governed speakers are untouched by the boundary, but speakers near it bear real, recurring costs, and the historical record shows the standard operating severely in crisis periods. Suppression (0.38) reflects the standing enforcement machinery — courts, agencies, disciplinary systems — that the boundary requires, plus the coercive restriction it licenses once triggered; suppression is authored as a raw structural property and is not scaled by power or scope (only extractiveness is scaled, by directionality and scope, in the engine's computation). Theater (0.28) reflects the growing share of multi-factor balancing that is ritualized and outcome-tracking rather than genuinely decision-guiding, alongside a core function that remains real. Accessibility collapse (0.50): the sibling readings remain live, and speakers can adapt expression, but once a tribunal accepts a harm theory, retreat options narrow sharply for the affected speakers. Resistance (0.62): a sustained, organized free-speech bar and academy contests every expansion. CYCLICAL PATTERN: the series oscillates rather than drifting monotonically — war and security crises spike extraction and enforcement (1917-1920, 1949-1957, 2001-2008), liberalizing counter-movements trough them (1930s-40s doctrinal tightening, the 1960s-70s peak-protection era), and harassment-law expansion ratchets the baseline upward between crises. The oscillation is partly an extraction mechanism in itself: each spike teaches boundary-adjacent speakers that their protection is unstable, producing durable self-censorship that persists after the spike recedes. The 2023 mens-rea tightening for true threats produces the slight terminal decline. All three tracked metrics run on one shared ten-point grid (1919-2025); endpoints match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the federal_courts seat the arrangement is a principled calibration the bench has refined for a century — low experienced extraction, high legitimacy. From the vulnerable_minorities seat it is overdue recourse, and its evidentiary expense reads as friction, not extraction. From the disfavored_speakers and political_dissenters seats the same boundary operates as unstable immunity: protection that can be withdrawn by whoever defines harm, with history showing the withdrawal falling hardest during political crises. From the state_and_local_regulators seat it is an expandable grant of authority. The chilled_silent_speakers seat experiences the arrangement entirely from outside its record. The engine computes this divergence from power, exit, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for vulnerable_minorities and ordinary_speakers (recourse and retained protection respectively; the latter near the beneficiary extreme given mobile exit). Victim declarations drive high directionality for disfavored_speakers and political_dissenters, amplified by constrained exit — they cannot stop being the people whose expression is at issue. State_and_local_regulators derive low-to-moderate directionality from their beneficiary role, tempered by their exposure to losing test cases. Federal_courts carry no beneficiary or victim declaration; their position is administrative, with mild institutional gain from doctrinal centrality. NO DIRECTIONALITY OVERRIDES are authored, deliberately: overrides key on power atoms, and this story's institutional seats (courts versus regulators) diverge internally in structural position, so an atom-level override would misfire across both. The derivation chain from beneficiary/victim data plus exit options captures the relationships accurately; the residual nuance is documented here and in the administrator_threshold_endogeneity omega.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: expression continues to operate as an instrument of injury at new scales, and the settlement continues to need a boundary. Mandatrophy is therefore NOT resolved, and no sunset or inertia story applies. The classification guards against mislabeling in both directions: the genuine, load-bearing coordination function (the anti-suppression settlement that nearly all speakers and factions still draw on daily) prevents reading the arrangement as pure extraction despite its real asymmetric component; the documented asymmetric application (crisis-period prosecution of dissent, administrator discretion over the harm threshold) prevents reading it as pure coordination. Theater at 0.28 sits below the degraded-performance signal, and the arrangement has concentrated institutional maintainers, which distinguishes it from an inertial leftover nobody profits from. The R5 mismatch consumer sees founding_problem_status=live paired with disappearance_verdict=world_rearranges: a consistent profile, no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_displacement,
    'This story instantiates the harm_limited_reading of the first_amendment_speech_protection kernel; the absolutist_reading and categorical_balancing_reading siblings instantiate different constraints with different protected sets, different victim sets, and different epsilon values. Which reading''s boundary rule governs, and what changes structurally if a sibling displaces this one?',
    'Track doctrinal dominance: the Supreme Court majority''s stated methodology in speech cases, circuit adoption rates for each boundary rule, and the outcomes of test cases inviting categorical protection or open balancing.',
    'Under the absolutist sibling the victim set empties (no harm-yielding is permitted) and measured extraction drops toward irrelevance; under the categorical-balancing sibling epsilon becomes case-indexed and unstable, with no fixed boundary to measure. This story''s metrics are valid only while this reading''s boundary rule governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_displacement, conceptual, 'Committer structure: one reading of a contested kernel; sibling displacement changes the constraint''s identity, not just its evaluation.').

omega_variable(
    harm_category_objectivity_drift,
    'Does ''demonstrable unconsented-to harm'' remain anchored to objectively verifiable injury (threats, incitement, defamation, concrete interference), or drift toward subjective offense and discomfort (expansive hostile-environment theories, viewpoint-coded harassment findings)?',
    'Longitudinal coding of successful regulations and liability findings by harm theory across the interval: objective-injury bases versus subjective-offense bases.',
    'Objective anchoring keeps the arrangement''s costs tied to demonstrated injury; subjective drift transfers the boundary to administrator judgment, raising effective extraction sharply and pushing the arrangement toward majority-suppression dynamics dressed as harm prevention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_category_objectivity_drift, empirical, 'Whether the harm boundary stays objective or drifts subjective.').

omega_variable(
    administrator_threshold_endogeneity,
    'The evidentiary threshold for ''demonstrable'' is administered by institutions (courts, agencies, university tribunals) whose incentives favor recognizing more harm; is the boundary''s placement a property of the written standard or of its administrators?',
    'Compare boundary placement across administrator types with different incentive structures (Article III courts versus administrative bodies versus internal disciplinary panels) on matched fact patterns.',
    'If placement tracks administrator identity rather than the stated standard, the arrangement''s effective shape is administrative, and changing it requires targeting administrator selection and structure rather than doctrinal text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrator_threshold_endogeneity, conceptual, 'Whether the boundary lives in the standard or in the incentives of those who apply it.').

omega_variable(
    chill_invisibility_in_measurements,
    'Measured costs come from litigated losses; speakers deterred into silence generate no record. Is true extraction higher than the measured series shows?',
    'Survey-based and natural-experiment studies of chilling effects surrounding salient enforcement episodes (prosecutions, high-profile liability findings, disciplinary waves).',
    'If measurable self-censorship substantially exceeds litigated loss, the arrangement''s real extraction exceeds the authored series, and any assessment computed from the visible record understates it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chill_invisibility_in_measurements, empirical, 'Unlitigated self-censorship is invisible to case-derived measures.').

omega_variable(
    minority_speaker_position_reversal,
    'Vulnerable minorities are declared beneficiaries as targets of harmful expression, yet the historical enforcement record shows the same communities bearing the sharpest costs when their own protest speech was prosecuted under harm rationales (sedition prosecutions, breach-of-peace arrests of civil-rights demonstrators). Is the beneficiary declaration stable across contexts?',
    'Disaggregate enforcement and liability data by speaker identity and harm theory across the interval; test whether costs concentrate on minority speakers during periods of political contestation.',
    'If the same population sits on both sides depending on the political moment, the recourse story fails for them specifically: the arrangement operates toward minority speakers as enforced silence in contested periods even while functioning as protection in others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_speaker_position_reversal, empirical, 'The declared beneficiary population has historically occupied the paying position under crisis enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 1919, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1919, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1919, 0.1).
narrative_ontology:measurement_basis(firs_tr_t1919, observed).
narrative_ontology:measurement(firs_tr_t1931, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1931, 0.14).
narrative_ontology:measurement_basis(firs_tr_t1931, observed).
narrative_ontology:measurement(firs_tr_t1951, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1951, 0.24).
narrative_ontology:measurement_basis(firs_tr_t1951, observed).
narrative_ontology:measurement(firs_tr_t1969, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1969, 0.17).
narrative_ontology:measurement_basis(firs_tr_t1969, observed).
narrative_ontology:measurement(firs_tr_t1977, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement_basis(firs_tr_t1977, observed).
narrative_ontology:measurement(firs_tr_t1986, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1986, 0.24).
narrative_ontology:measurement_basis(firs_tr_t1986, observed).
narrative_ontology:measurement(firs_tr_t1995, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1995, 0.29).
narrative_ontology:measurement_basis(firs_tr_t1995, observed).
narrative_ontology:measurement(firs_tr_t2003, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2003, 0.27).
narrative_ontology:measurement_basis(firs_tr_t2003, observed).
narrative_ontology:measurement(firs_tr_t2015, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2015, 0.31).
narrative_ontology:measurement_basis(firs_tr_t2015, observed).
narrative_ontology:measurement(firs_tr_t2025, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(firs_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(firs_be_t1919, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1919, 0.68).
narrative_ontology:measurement_basis(firs_be_t1919, observed).
narrative_ontology:measurement(firs_be_t1931, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1931, 0.55).
narrative_ontology:measurement_basis(firs_be_t1931, observed).
narrative_ontology:measurement(firs_be_t1951, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1951, 0.63).
narrative_ontology:measurement_basis(firs_be_t1951, observed).
narrative_ontology:measurement(firs_be_t1969, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1969, 0.33).
narrative_ontology:measurement_basis(firs_be_t1969, observed).
narrative_ontology:measurement(firs_be_t1977, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1977, 0.31).
narrative_ontology:measurement_basis(firs_be_t1977, observed).
narrative_ontology:measurement(firs_be_t1986, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1986, 0.4).
narrative_ontology:measurement_basis(firs_be_t1986, observed).
narrative_ontology:measurement(firs_be_t1995, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement_basis(firs_be_t1995, observed).
narrative_ontology:measurement(firs_be_t2003, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2003, 0.48).
narrative_ontology:measurement_basis(firs_be_t2003, observed).
narrative_ontology:measurement(firs_be_t2015, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2015, 0.46).
narrative_ontology:measurement_basis(firs_be_t2015, observed).
narrative_ontology:measurement(firs_be_t2025, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(firs_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1919, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1919, 0.7).
narrative_ontology:measurement_basis(firs_su_t1919, observed).
narrative_ontology:measurement(firs_su_t1931, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1931, 0.5).
narrative_ontology:measurement_basis(firs_su_t1931, observed).
narrative_ontology:measurement(firs_su_t1951, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1951, 0.64).
narrative_ontology:measurement_basis(firs_su_t1951, observed).
narrative_ontology:measurement(firs_su_t1969, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1969, 0.28).
narrative_ontology:measurement_basis(firs_su_t1969, observed).
narrative_ontology:measurement(firs_su_t1977, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1977, 0.24).
narrative_ontology:measurement_basis(firs_su_t1977, observed).
narrative_ontology:measurement(firs_su_t1986, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1986, 0.33).
narrative_ontology:measurement_basis(firs_su_t1986, observed).
narrative_ontology:measurement(firs_su_t1995, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1995, 0.37).
narrative_ontology:measurement_basis(firs_su_t1995, observed).
narrative_ontology:measurement(firs_su_t2003, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2003, 0.39).
narrative_ontology:measurement_basis(firs_su_t2003, observed).
narrative_ontology:measurement(firs_su_t2015, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2015, 0.41).
narrative_ontology:measurement_basis(firs_su_t2015, observed).
narrative_ontology:measurement(firs_su_t2025, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(firs_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'First Amendment protection.' The label conflates three structurally distinct claims with different epsilons, victim sets, and failure modes: the absolutist_reading (upstream textual anchor; near-zero extraction; categorical), this harm_limited_reading (fixed demonstration boundary; moderate extraction with crisis spikes), and the categorical_balancing_reading (open weighing; case-indexed, unstable extraction). The absolutist reading is upstream: both limited readings cite the text's breadth as the baseline they depart from, and absolutist victories shrink the space in which the other two operate. Every success of this reading's harm boundary exerts downstream pressure on the categorical-balancing sibling by shrinking the territory balancing must govern, and political pressure on the absolutist sibling by normalizing exceptions. All three files link one another via affects_constraints; each carries its own stable epsilon per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
