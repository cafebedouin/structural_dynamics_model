% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Speech-Harm Boundary — Proportionality Balancing Reading
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This story instantiates the harm-balancing reading of the speech-harm
 *   boundary kernel: speech protection is presumptive, but courts apply
 *   proportionality tests weighing severity, intent, and available
 *   alternatives to determine when demonstrated harm overrides the
 *   presumption. This reading sits structurally between the absolutist
 *   reading (near-absolute protection, extremely high override threshold) and
 *   the dignity reading (categorical unprotection of personhood-denying
 *   speech) — it authors neither extreme, but a moving, fact-specific
 *   threshold that expands the unprotected category (hate speech, group
 *   libel, harassment) relative to the absolutist reading while stopping
 *   short of the dignity reading's categorical exclusions. Its ε (0.42) is
 *   moderate by design: real coordination function (adjudicating genuine
 *   liberty/harm conflicts) coexists with real extraction (unpredictable
 *   chilling effect falling disproportionately on under-resourced speakers).
 *
 * KEY AGENTS:
 *   - courts_administering_balancing_tests: institutional agenda-setter administering the fact-specific threshold
 *   - targeted_minority_groups and harassment_complainants: beneficiaries who gain recourse for demonstrated harm
 *   - controversial_speakers, advocacy_organizations_using_charged_rhetoric, satirists_and_provocateurs: payers who bear unpredictable liability risk
 *   - platform_operators: institutional agenda-setter translating the standard into content moderation at scale
 *   - absolutist_free_speech_advocates: excluded from setting the operative standard despite participating in litigation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.42).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.48).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech-Harm Boundary — Proportionality Balancing Reading").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '8fa3109b-da03-4518-a8b5-cac3ef2aefe1').
narrative_ontology:cs_kernel_codification('8fa3109b-da03-4518-a8b5-cac3ef2aefe1', distributed).
narrative_ontology:cs_authority_grounding('8fa3109b-da03-4518-a8b5-cac3ef2aefe1', practice).
narrative_ontology:cs_interpretation_layer_present('8fa3109b-da03-4518-a8b5-cac3ef2aefe1').
narrative_ontology:cs_reading_relation('8fa3109b-da03-4518-a8b5-cac3ef2aefe1', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8fa3109b-da03-4518-a8b5-cac3ef2aefe1', speech_harm_boundary__dignity_reading, influences).
narrative_ontology:cs_axiom('8fa3109b-da03-4518-a8b5-cac3ef2aefe1', foundational, harm_must_be_demonstrated_not_presumed).
narrative_ontology:cs_axiom_status(harm_must_be_demonstrated_not_presumed, holdable).
narrative_ontology:cs_axiom_grounding('8fa3109b-da03-4518-a8b5-cac3ef2aefe1', harm_must_be_demonstrated_not_presumed, conventional).
narrative_ontology:cs_axiom('8fa3109b-da03-4518-a8b5-cac3ef2aefe1', foundational, proportionality_over_categorical_rules).
narrative_ontology:cs_axiom_status(proportionality_over_categorical_rules, holdable).
narrative_ontology:cs_axiom_grounding('8fa3109b-da03-4518-a8b5-cac3ef2aefe1', proportionality_over_categorical_rules, instrumental).
narrative_ontology:cs_reference_frame('8fa3109b-da03-4518-a8b5-cac3ef2aefe1', presumptive_protection_with_case_by_case_override).
narrative_ontology:cs_drift_state('8fa3109b-da03-4518-a8b5-cac3ef2aefe1', post_digital_harassment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8fa3109b-da03-4518-a8b5-cac3ef2aefe1', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, targeted_minority_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, harassment_complainants).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, courts_administering_balancing_tests).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, controversial_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, advocacy_organizations_using_charged_rhetoric).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, satirists_and_provocateurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, platform_operators).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, demonstrated_harm_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate case-by-case whether speech crosses from protected expression into demonstrated harm, applying multi-factor proportionality tests (severity, intent, context, alternative channels). Sets the operative boundary through accumulated precedent rather than a bright line, and can shift the threshold up or down over time through how it weighs factors.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, courts_administering_balancing_tests, agenda_setter,
    institutional, generational, analytical, national).

% Receive legal recourse against group libel, harassment, and hate speech once harm is demonstrated through evidence of psychological, reputational, or social injury. Their exit from harmful speech environments is otherwise costly (relocation, withdrawal from public discourse); the balancing standard gives them a route to remedy without requiring them to simply absorb the harm.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, targeted_minority_groups, beneficiary,
    moderate, biographical, constrained, national).

% Individuals subject to targeted, sustained speech-based harassment (workplace, campus, online) who can invoke the harm standard to obtain injunctions or damages. Often lack resources to simply relocate away from the harasser, making the legal remedy their primary practical exit.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, harassment_complainants, beneficiary,
    powerless, immediate, trapped, local).

% Speakers whose provocative, offensive, or politically charged statements are litigated or chilled under the harm standard. Bear the cost of legal uncertainty — the proportionality test is fact-specific and unpredictable ex ante, so they must self-censor or accept litigation risk. Cannot easily know in advance which side of the line a given statement falls on.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, controversial_speakers, payer,
    moderate, biographical, constrained, national).

% Political and social movement organizations that rely on strong, sometimes inflammatory rhetoric to mobilize and provoke. Face injunctions, defamation suits, or reputational costs when their rhetoric is found to cross into demonstrated group harm; must recalibrate messaging strategy around an unstable legal boundary.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, advocacy_organizations_using_charged_rhetoric, payer,
    organized, biographical, constrained, national).

% Individual comedians, artists, and commentators whose exaggeration or provocation is core to their craft. Lack institutional legal support; a single adverse ruling can be financially and professionally ruinous. Cannot exit the field without abandoning their vocation, and cannot predict in advance which provocations will be deemed harmful.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, satirists_and_provocateurs, payer,
    powerless, immediate, trapped, national).

% Implement the balancing standard through content moderation policy, translating judicial and regulatory harm thresholds into removal and demotion rules at scale. Benefit from a workable liability shield when they can show good-faith balancing; also administer the boundary in the first instance for billions of speech acts.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, platform_operators, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, platform_operators, beneficiary).

% Argue the proportionality approach imports a moving, manipulable standard into what should be a near-categorical protection. Participate in litigation and public debate but do not control the doctrinal framework; their preferred bright-line rule is not the operative standard under this reading.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, absolutist_free_speech_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__harm_balancing_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_harm_boundary__harm_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for adjudicating genuine conflicts between expressive liberty and demonstrable harm to identifiable others, allowing courts to weigh context, severity, and alternatives rather than applying an all-or-nothing rule that would either permit unlimited harm or suppress all provocative speech.
% TRANSFER_FUNCTION: Moves the cost of ambiguity from harmed parties (who would otherwise bear injury with no remedy under an absolutist rule) to speakers whose expression sits near the shifting harm threshold — the risk of unpredictable liability is transferred onto controversial and marginal speakers, especially those without institutional legal backing.
% ABSENT_VOICES: Absolutist free speech advocates are present in litigation but do not set the operative standard; some silenced-by-chilling-effect speakers never surface in the record at all because they self-censor before any case arises — their objection is structurally invisible to the balancing process itself.
% DISAPPEARANCE_RATIONALE: If the harm-balancing standard vanished and a pure absolutist standard took its place, targeted minority groups and harassment complainants would lose recourse for a wide category of currently-actionable speech; conversely if it collapsed toward the dignity reading, a much larger swath of currently-permitted charged rhetoric would become categorically unprotected. Litigation dockets, platform moderation policy, and advocacy messaging strategy would all restructure around whichever boundary replaced it.
% FOUNDING_PROBLEM: Neither a pure liberty-maximizing rule (which leaves demonstrable, serious harms to identifiable groups and individuals without remedy) nor a pure harm-avoidance rule (which would suppress vast amounts of legitimate but uncomfortable political and artistic expression) was administrable or normatively acceptable on its own; courts needed a mechanism to adjudicate genuine cases where the two values conflict.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights litigators and harassment-law scholars attest the problem remains live — new forms of coordinated online harassment continually test the boundary. Free speech scholars outside the beneficiary set (First Amendment absolutists, some civil libertarian organizations) attest that the balancing framework has itself become the primary source of unpredictability and chilling effect, arguing the cure has become a freestanding cost independent of the harm it addresses.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.42) reflects genuine but moderate cost transfer: the standard does real coordination work (resolving cases an all-or-nothing rule could not resolve well) while imposing real, unevenly distributed costs on speakers near the threshold. Suppression (0.48) is meaningfully lower than a categorical-exclusion regime because the presumption of protection remains the starting point and much speech never reaches the balancing test at all. Accessibility collapse (0.35) is moderate — alternative expressive channels and legal defenses remain available, unlike a bright-line ban. Resistance (0.55) is substantial because absolutist advocates, media organizations, and civil libertarian groups actively contest the standard's unpredictability in courts and public discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the courts' seat, this looks like principled, case-sensitive coordination resolving genuine value conflicts. From an individual satirist's seat with no institutional legal backing, the identical structure looks like exposure to ruinous, unpredictable liability for engaging in exactly the kind of provocative expression the presumption is supposed to protect. The engine should compute these as structurally different experiences of the same doctrine, driven by asymmetric exit options and resources, not by disagreement about the doctrine's text.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted minority groups and harassment complainants are structural beneficiaries — the standard exists to give them recourse they would lack under a pure liberty-maximizing rule, so their directionality sits toward the beneficiary end despite their otherwise-low institutional power (their power is compensated by the doctrine's direct design purpose). Controversial speakers, advocacy organizations, and especially individual satirists sit toward the target end: they bear the ex ante unpredictability cost even when never formally sanctioned, because the chilling effect operates on anticipated liability, not just realized judgments. Courts and platform operators occupy dual positions — administering the standard while also being scoped by it (courts through precedent constraint, platforms through liability exposure).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine unresolved conflict between expressive liberty and demonstrable harm — remains live in the sense that new speech technologies (algorithmic amplification, coordinated online harassment) continually generate fresh instances the doctrine was built to handle. But there is a live secondary claim that the balancing apparatus itself has become a freestanding source of unpredictability and chilling effect independent of the harms it addresses, which is why founding_problem_status is authored as contested rather than simply live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_test_manipulability,
    'Is the proportionality test a genuine multi-factor adjudicative process, or does its indeterminacy function as a mechanism by which courts can reach predetermined outcomes while appearing principled?',
    'Longitudinal analysis of case outcomes against announced factors: if factor-weighting is consistent and predictable across similar fact patterns, the test is functioning as genuine adjudication; if outcomes correlate more with judge identity or political climate than with the announced factors, the test is functioning as a discretion-laundering mechanism.',
    'If the test is substantially manipulable, effective extraction is higher than the authored ε suggests because unpredictability itself is the extractive mechanism (chilling effect operates through uncertainty, not through any specific rule). If the test is reliably principled, the moderate ε is a fair characterization of a working coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_manipulability, empirical, 'Whether case-by-case balancing is genuinely adjudicative or functions as outcome-laundering discretion.').

omega_variable(
    kernel_reading_selection_basis,
    'Is the harm-balancing reading a stable, independently-justified middle position, or is it simply the reading that currently commands the most institutional votes among three genuinely incommensurable value framings (absolutist, dignity, harm-balancing)?',
    'Comparative doctrinal history: track whether jurisdictions cycle between the three readings over time in response to political shifts (suggesting no principled median exists) or whether the harm-balancing reading has independent normative grounding that survives political change.',
    'If the reading is merely the current political center of gravity rather than a principled synthesis, its claimed_type and ε may be more contingent and less stable than authored here, and the sibling readings (absolutist, dignity) are better understood as competing attractors than as genuinely foreclosed alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether the harm-balancing reading is a principled synthesis or a contingent political equilibrium among three live framings.').

omega_variable(
    differential_harm_visibility,
    'Does the demonstrated-harm requirement systematically favor harms that are easy to litigate and document (reputational, economic) over harms that are diffuse or hard to prove (psychological harm from sustained low-grade hostility, dignitary harm to groups without organized legal capacity)?',
    'Compare successful harm claims by harm type and by claimant''s access to legal resources; if well-resourced claimants with easily-documented harms succeed disproportionately relative to under-resourced claimants with diffuse harms, the standard''s neutral proportionality framing masks a resource-access filter.',
    'If confirmed, the beneficiary group is narrower in practice than the doctrine''s stated purpose suggests, and the coordination story (protecting genuinely harmed parties) partially obscures an access-to-litigation filter that functions as quiet extraction from under-resourced would-be claimants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(differential_harm_visibility, empirical, 'Whether the harm standard''s neutral framing masks unequal access to demonstrating harm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__harm_balancing_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__harm_balancing_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__harm_balancing_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__harm_balancing_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 32, 0.46).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__harm_balancing_reading, 0.12).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the speech_harm_boundary kernel, decomposed per the ε-invariance principle because the natural-language concept 'free speech protection' conflates structurally distinct claims about where the harm-override threshold sits. The absolutist_reading authors a near-zero override threshold and correspondingly lower ε for the standing protective arrangement; the dignity_reading authors categorical exclusion of personhood-denying speech regardless of proportionality, with a differently-structured victim set (speakers of that category are categorically, not contingently, unprotected). This harm_balancing_reading occupies the structural middle: moderate ε, case-by-case adjudication, broader-than-absolutist but narrower-than-dignity unprotected categories. Each reading is authored with its own stable ε assessed by that reading's own lights, per the kernel-reading ε referent rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
