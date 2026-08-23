% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Harm-Balancing Reading of the Speech-Harm Boundary
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This story instantiates the harm_balancing_reading of the
 *   speech_harm_boundary kernel: speech protection operates presumptively but
 *   yields where harm is demonstrated, with restriction calibrated by
 *   proportionality. The standing arrangement under contest — the referent
 *   for every metric here — is that balancing regime itself as it actually
 *   operates: constitutional and statutory harm categories (incitement, hate
 *   speech, group libel, harassment, online harms), tribunals and commissions
 *   administering them, and platform moderation architectures citing them.
 *   The reading's own lights assess this arrangement as justified in
 *   principle; the metrics below measure the arrangement's actual operation,
 *   not its justification. Claimed type and metrics are authored
 *   independently: I claim tangled_rope because the regime demonstrably
 *   coordinates (a shared adjudicable harm standard replacing private
 *   retaliation and official whim) while demonstrably extracting (restriction
 *   costs fall on speakers, and the demonstrated-harm determination is
 *   controlled by institutions whose mandates grow with each accepted
 *   category). Sibling readings — absolutist_reading (near-absolute
 *   protection, extreme override threshold) and dignity_reading (protection
 *   subordinate to dignity, categorical exclusion) — are separate constraints
 *   with their own epsilon values; see network.dual_formulation_note. KEY
 *   AGENTS (by structural relationship): - balancing_adjudicators:
 *   agenda-setter and principal beneficiary (institutional/arbitrage) —
 *   determines when harm is demonstrated, collects adjudicative authority
 *   with each recognized category - targets_of_harmful_speech: primary
 *   intended beneficiary (moderate/constrained) — gains recourse contingent
 *   on institutional validation - restricted_speakers: primary payer
 *   (moderate/constrained) — bears removal, sanction, and penalty costs -
 *   heterodox_minority_speakers: asymmetric payer (powerless/trapped) —
 *   disproportionately classified harmful, least able to contest -
 *   general_public_audience: dual-positioned beneficiary-payer
 *   (organized/mobile) — cleaner discourse environment versus narrowed public
 *   sphere - civil_liberties_advocates: observer (organized/analytical) —
 *   contests category expansion from outside the benefit flow -
 *   chilled_would_be_speakers: excluded (powerless/trapped) — self-censors
 *   before any proceeding; never registers as a party
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.46).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.52).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Harm-Balancing Reading of the Speech-Harm Boundary").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '9a73bcfb-989f-45d4-994b-3b7b326d55b3').
narrative_ontology:cs_kernel_codification('9a73bcfb-989f-45d4-994b-3b7b326d55b3', fixed_text).
narrative_ontology:cs_authority_grounding('9a73bcfb-989f-45d4-994b-3b7b326d55b3', lineage).
narrative_ontology:cs_interpretation_layer_present('9a73bcfb-989f-45d4-994b-3b7b326d55b3').
narrative_ontology:cs_reading_relation('9a73bcfb-989f-45d4-994b-3b7b326d55b3', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a73bcfb-989f-45d4-994b-3b7b326d55b3', speech_harm_boundary__dignity_reading, influences).
narrative_ontology:cs_axiom('9a73bcfb-989f-45d4-994b-3b7b326d55b3', foundational, liberty_presumption_yields_to_demonstrated_harm).
narrative_ontology:cs_axiom_status(liberty_presumption_yields_to_demonstrated_harm, holdable).
narrative_ontology:cs_axiom_grounding('9a73bcfb-989f-45d4-994b-3b7b326d55b3', liberty_presumption_yields_to_demonstrated_harm, instrumental).
narrative_ontology:cs_axiom('9a73bcfb-989f-45d4-994b-3b7b326d55b3', foundational, proportionality_disciplines_all_restriction).
narrative_ontology:cs_axiom_status(proportionality_disciplines_all_restriction, holdable).
narrative_ontology:cs_axiom_grounding('9a73bcfb-989f-45d4-994b-3b7b326d55b3', proportionality_disciplines_all_restriction, conventional).
narrative_ontology:cs_axiom('9a73bcfb-989f-45d4-994b-3b7b326d55b3', secondary, harm_categories_track_evidence).
narrative_ontology:cs_axiom_status(harm_categories_track_evidence, holdable).
narrative_ontology:cs_axiom_grounding('9a73bcfb-989f-45d4-994b-3b7b326d55b3', harm_categories_track_evidence, empirically_contingent).
narrative_ontology:cs_reference_frame('9a73bcfb-989f-45d4-994b-3b7b326d55b3', presumptive_liberty_proportionate_override).
narrative_ontology:cs_drift_state('9a73bcfb-989f-45d4-994b-3b7b326d55b3', platform_scale_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a73bcfb-989f-45d4-994b-3b7b326d55b3', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, targets_of_harmful_speech).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, balancing_adjudicators).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, general_public_audience).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, restricted_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, heterodox_minority_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, general_public_audience).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, demonstrated_harm_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutional courts, human rights commissions, equality bodies, and platform oversight boards that decide when expressive harm is sufficiently demonstrated to justify restriction and calibrate the response. Each accepted harm category enlarges their docket, staffing, and doctrinal footprint; their determinations define where the boundary sits in practice. Exit is effectively unlimited: they operate across the whole case stream and their accumulated authority travels with them.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, balancing_adjudicators, agenda_setter,
    institutional, generational, arbitrage, national).

% People subjected to harassment campaigns, group-defamatory falsehoods, or incitement-adjacent targeting. The regime gives them a formal channel — complaint, takedown request, tribunal petition — that converts their injury into a legally cognizable claim. The benefit is real but conditional: it materializes only when an adjudicator accepts their harm demonstration, which favors articulate, resourced, and sympathetic claimants. Leaving the discourse space entirely forfeits the benefit.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, targets_of_harmful_speech, beneficiary,
    moderate, biographical, constrained, national).

% Speakers whose expression lands in an unprotected category — removed posts, sanctioned accounts, prosecuted statements. They lose reach, standing, or liberty proportional to the restriction imposed. Alternatives exist but cost: pseudonymity, offshore platforms, smaller audiences, or litigation they usually cannot fund.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, restricted_speakers, payer,
    moderate, biographical, constrained, national).

% Dissidents, religious minorities, and political outsiders whose speech is the most readily recast as harmful by majority-staffed adjudication — historically the blasphemer, the seditionist, the obscenity publisher; contemporarily the radical critic and the unpopular campaigner. They bear restriction at rates exceeding their share of harmful conduct and lack the resources to contest classification. Coalition across ideological lines is rare, which keeps them individually weak despite collective numerosity.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, heterodox_minority_speakers, payer,
    powerless, biographical, trapped, national).

% Everyone else in the discourse environment. They receive a moderated commons with less harassment and group vilification, and they pay diffusely: a narrower range of audible positions, platform friction, and the precedent that expression is conditional. Their exit is high — they can and do migrate venues — so their stake on either side is attenuated.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, general_public_audience, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, general_public_audience, payer).

% Free-expression organizations, absolutist-leaning jurists, and academic commentators who contest each category expansion. They collect nothing from the regime and bear little of its direct cost; their function is adversarial scrutiny — litigating boundary cases, documenting chilling effects, arguing the presumption downward. They sit outside the benefit and cost flows but inside the argument.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, global).

% People who never speak because they anticipate the regime: the employee who will not post, the researcher who softens findings, the minority member who stays silent after watching similar speakers sanctioned. They appear in no docket, file no complaint, and register in no statistic — their absence is the regime's quietest product.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, chilled_would_be_speakers, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__harm_balancing_reading, balancing_adjudicators).
narrative_ontology:fixing_cost_class(speech_harm_boundary__harm_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a procedurally uniform, adjudicable test for when expression leaves the protected sphere — replacing private retaliation, mob response, and ad hoc official whim with a shared standard (demonstrated harm, proportionately calibrated) that speakers, targets, and institutions can all address.
% TRANSFER_FUNCTION: Moves expressive liberty and audience reach from speakers classified as harmful to the targets of that speech (as protection and recourse), and moves adjudicative discretion and mandate to the institutions operating the balancing; the currencies are speech rights, attention, reputational standing, and institutional authority.
% ABSENT_VOICES: Chilled would-be speakers (role: excluded) never enter any proceeding, so the record systematically understates the regime's cost side. Absolutist jurists object that any demonstrated-harm override corrodes the presumption, but their seat is structurally outvoted inside balancing forums, which are staffed by the balancing's own operators. Targets who fail to convince adjudicators are likewise absent from the recorded beneficiary count.
% DISAPPEARANCE_RATIONALE: If the harm-balancing apparatus vanished overnight, targets of harassment and group defamation would lose their formal recourse channel and revert to private counter-speech or retaliation; adjudicating institutions would lose mandates built over decades; platforms would lose the legal-policy template their moderation rules cite; legislatures would face immediate pressure to build a successor standard. The speech environment would reorganize around whichever substitute each jurisdiction adopted — absolutist tolerance or dignity-categorical exclusion — and the transition itself would be fought.
% FOUNDING_PROBLEM: The post-war settlement confronted twin failures: unrestricted expression had fed incitement and group vilification culminating in atrocity, while unrestricted suppression was the signature of the totalitarian states just defeated. The arrangement was built to solve the problem of protecting persons and groups from demonstrably harmful expression without reconstructing censorship machinery.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: ICCPR drafting records and mid-century constitutional commentary attest the founding problem's formulation; contemporary civil-liberties scholarship attests it remains live while disputing the current solution's scope; victim-advocacy groups attest liveness from the opposite direction. The adjudicating institutions also attest liveness, but they are benefiting parties and their attestation is discounted accordingly.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).
:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.46) rather than low because restriction costs are real and asymmetrically distributed: the speaker side pays in removed reach, sanction, and prosecution, while the demonstrated-harm gate is controlled by institutions with organizational stakes in category acceptance. It stops short of snare levels because the regime delivers genuine, reachable protection to targets and remains subject to appeal and review. Suppression (0.52) reflects hard enforcement machinery — dedicated tribunals, notice-and-takedown pipelines, criminal hate-speech statutes — bounded by judicial review and the persistent availability of counter-speech and venue migration. Theater (0.28) is low-moderate: most balancing is operative case-work, but high-salience proceedings increasingly ratify determinations made upstream, and the ritual language of proportionality sometimes launders predetermined outcomes. Accessibility collapse (0.42): within the framework's logic the unregulated-speech alternative collapses substantially — treaty obligations, platform uniformity, and doctrinal path-dependence close it — yet sibling readings remain live and speakers retain costly partial exits, so collapse is partial. Resistance (0.58) is sustained and recurring: litigation challenges, legislative repeal attempts, platform-policy reversals, and periodic backlash against each expansion wave. The measurement series runs on one shared grid (1948-2026, eight points) so every tracked metric is authored at every examined time point; all three series rise, modeling category expansion, mandate growth, and enforcement hardening rather than stabilization.
 *
 * PERSPECTIVAL GAP:
 *   The adjudicator seat computes the arrangement as legitimate craft: case-by-case weighing performed by accountable institutions, each restriction individually defensible. The target seat computes it as indispensable protection whose value depends entirely on institutional recognition — a benefit that vanishes precisely for the least credible claimants. The payer seats compute the same structure as asymmetric gatekeeping: a presumption that yields exactly when the strongest institutions say it should. The heterodox-minority seat adds the historical record — blasphemy, sedition, and obscenity were each, in turn, demonstrated harm — and expects to be next. One structure, four experienced constraints; the engine derives this divergence from the declared positions, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: balancing_adjudicators sit nearest the beneficiary end (they collect authority with each case and hold arbitrage-grade exit across the whole docket); targets_of_harmful_speech sit low but not minimal (their benefit is contingent on validation they do not control); general_public_audience sits near-symmetric with a net-beneficiary tilt (cleaner commons received, narrowed sphere paid diffusely, mobile exit attenuating both sides). Victim declarations map to high directionality: restricted_speakers bear the transfer directly under constrained exit; heterodox_minority_speakers sit nearest the full-target end — powerless, trapped, and bearing restriction at rates exceeding their share of harmful conduct. National-scale administration amplifies effective extraction modestly: the standard itself is set at scale, where verification of proportionality is weakest even though individual cases are verified closely. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct ordering without correction. On coalition power: the heterodox seat is numerous but ideologically fragmented, and the regime's case-by-case procedure fragments them further — each contest is individualized, which is precisely why numerosity does not convert into leverage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so this is not a resolved mandatrophy case — the arrangement still addresses a real, externally corroborated problem. The classification work is preventive: holding tangled_rope keeps both halves visible. Reading the regime as pure coordination would erase the payer seats and let category expansion pass as protection; reading it as pure extraction would erase the target seats' genuine recourse and make abolition look costless. The drift vector to watch is category expansion: if demonstrated harm becomes self-referential — harm defined as whatever adjudicators already restrict — the coordination half atrophies while enforcement persists, and the regime slides toward snare or, if enforcement also ritualizes, toward inertial performance. The measurement series tracks exactly this: extractiveness and suppression rising together with theater creeping upward. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges — no zombie flag — but the series slope is the early-warning signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the harm_balancing_reading of the speech_harm_boundary kernel; would instantiating the absolutist_reading or dignity_reading instead change the structural classification?',
    'Generate the sibling stories (speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading) with their own epsilon, beneficiary/victim sets, and metrics, then compare computed types across the family.',
    'Under the absolutist reading the unprotected-category set contracts toward incitement alone and epsilon drops well below this file''s 0.46; under the dignity reading the victim set re-centers on speakers of personhood-denying expression and epsilon rises categorically. This file''s classification holds only for the balancing reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    harm_determination_capture_risk,
    'Does the demonstrated-harm threshold track independently measurable harm, or the organizational interests and prevailing moral panics of the adjudicating institutions?',
    'Compare classification outcomes across independent adjudicative bodies and across time against external harm baselines (victimization surveys, documented incitement outcomes, replication of harm claims).',
    'Significant capture means effective extraction on heterodox speakers is understated here and the regime trends toward snare; tight tracking supports the tangled_rope reading as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_determination_capture_risk, empirical, 'Whether the harm gate is epistemically anchored or institutionally self-serving.').

omega_variable(
    chilled_speaker_undercount,
    'How much restriction cost is borne by speakers who never enter any record because they self-censor before the regime is invoked?',
    'Behavioral and survey measurement of self-censorship across differently regulated venues; natural experiments where regulation changes abruptly.',
    'Authored epsilon counts adjudicated cases only; heavy chilling places true extraction above 0.46 and shifts the suppressed seats'' computed classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilled_speaker_undercount, empirical, 'Unrecorded pre-procedural suppression concentrated in the excluded seat.').

omega_variable(
    category_expansion_trajectory,
    'Is the historical expansion of unprotected categories converging on a stable boundary or ratcheting without a limiting principle?',
    'Fit the authored measurement series for saturation; test whether each expansion wave ties to a specific evidentiary trigger or to adjudicating-body mandate growth independent of harm data.',
    'An unbounded ratchet supports reclassification pressure toward snare; convergence supports the stable tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_expansion_trajectory, empirical, 'Whether the regime''s growth is bounded by anything other than institutional appetite.').

omega_variable(
    proportionality_genuineness,
    'Is proportionality balancing a real constraint that ever reverses an initial harm determination, or a legitimating form that ratifies decisions made upstream?',
    'Code high-salience cases for whether the balancing stage reverses or materially reshapes the initial determination; a near-zero reversal rate indicates theatrical balancing.',
    'High theater would lift theater_ratio above the authored 0.28 and push mature jurisdictions toward performance-maintained operation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_genuineness, conceptual, 'Functional versus performative share of the balancing procedure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1948, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1948, 0.12).
narrative_ontology:measurement_basis(spee_tr_t1948, observed).
narrative_ontology:measurement(spee_tr_t1965, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement_basis(spee_tr_t1965, observed).
narrative_ontology:measurement(spee_tr_t1982, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1982, 0.18).
narrative_ontology:measurement_basis(spee_tr_t1982, observed).
narrative_ontology:measurement(spee_tr_t1995, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1995, 0.21).
narrative_ontology:measurement_basis(spee_tr_t1995, observed).
narrative_ontology:measurement(spee_tr_t2008, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2008, 0.24).
narrative_ontology:measurement_basis(spee_tr_t2008, observed).
narrative_ontology:measurement(spee_tr_t2016, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement_basis(spee_tr_t2016, observed).
narrative_ontology:measurement(spee_tr_t2021, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2021, 0.27).
narrative_ontology:measurement_basis(spee_tr_t2021, observed).
narrative_ontology:measurement(spee_tr_t2026, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(spee_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t1948, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1948, 0.28).
narrative_ontology:measurement_basis(spee_be_t1948, observed).
narrative_ontology:measurement(spee_be_t1965, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1965, 0.31).
narrative_ontology:measurement_basis(spee_be_t1965, observed).
narrative_ontology:measurement(spee_be_t1982, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1982, 0.35).
narrative_ontology:measurement_basis(spee_be_t1982, observed).
narrative_ontology:measurement(spee_be_t1995, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement_basis(spee_be_t1995, observed).
narrative_ontology:measurement(spee_be_t2008, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2008, 0.41).
narrative_ontology:measurement_basis(spee_be_t2008, observed).
narrative_ontology:measurement(spee_be_t2016, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2016, 0.43).
narrative_ontology:measurement_basis(spee_be_t2016, observed).
narrative_ontology:measurement(spee_be_t2021, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2021, 0.45).
narrative_ontology:measurement_basis(spee_be_t2021, observed).
narrative_ontology:measurement(spee_be_t2026, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2026, 0.46).
narrative_ontology:measurement_basis(spee_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1948, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement_basis(spee_su_t1948, observed).
narrative_ontology:measurement(spee_su_t1965, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1965, 0.34).
narrative_ontology:measurement_basis(spee_su_t1965, observed).
narrative_ontology:measurement(spee_su_t1982, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1982, 0.4).
narrative_ontology:measurement_basis(spee_su_t1982, observed).
narrative_ontology:measurement(spee_su_t1995, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1995, 0.44).
narrative_ontology:measurement_basis(spee_su_t1995, observed).
narrative_ontology:measurement(spee_su_t2008, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2008, 0.47).
narrative_ontology:measurement_basis(spee_su_t2008, observed).
narrative_ontology:measurement(spee_su_t2016, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2016, 0.5).
narrative_ontology:measurement_basis(spee_su_t2016, observed).
narrative_ontology:measurement(spee_su_t2021, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2021, 0.51).
narrative_ontology:measurement_basis(spee_su_t2021, observed).
narrative_ontology:measurement(spee_su_t2026, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2026, 0.52).
narrative_ontology:measurement_basis(spee_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the speech-harm boundary' decomposes into three structurally distinct constraints — one per reading of the shared kernel. They differ in epsilon (absolutist lowest, dignity highest, balancing moderate), in victim sets (this reading: speakers classified harmful; dignity reading: speakers of personhood-denying expression; absolutist reading: nearly none), and in enforcement burden. This file is the balancing member and links to both siblings. Upstream/downstream structure: the balancing reading's proportionality methodology exerts structural pressure on dignity-system adjudication (declared as an influences edge in cs_structure.reading_relations), while coexisting with the absolutist reading as rival live positions held by different legal cultures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
