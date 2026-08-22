% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__categorical_balancing_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Protection via Judicial Category-Balancing (Categorical Balancing Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the categorical-balancing reading of the First
 *   Amendment speech-protection kernel: the doctrine that protection is
 *   defined by judicially maintained categories (obscenity, incitement, true
 *   threats, fighting words, etc.) whose boundaries are drawn through
 *   case-by-case weighing of speech value against asserted harm, rather than
 *   by the text's categorical command ('no law') or by a general harm/consent
 *   test. This is a distinct constraint from its siblings — the absolutist
 *   reading (constraint_id: first_amendment_absolutist_reading) and the
 *   harm-limited reading (constraint_id:
 *   first_amendment_harm_limited_reading) — each of which authors its own ε
 *   and its own beneficiary/victim structure per the ε-invariance principle.
 *   Under this reading, the standing arrangement under contest is the
 *   balancing methodology itself: its beneficiary is the judiciary that
 *   retains ongoing interpretive discretion and the professional-academic
 *   apparatus that elaborates the resulting tests, and its cost falls on
 *   speakers whose expression sits near a contested boundary and cannot
 *   predict in advance which side of the line a court will place them on.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda_setter (institutional/analytical) — draws and redraws category boundaries
 *   - constitutional_law_academy: beneficiary (organized/arbitrage) — produces doctrinal scaffolding that sustains ongoing contestability
 *   - speakers_in_contested_categories: payer (powerless/trapped) — bears unpredictability of after-the-fact line-drawing
 *   - political_dissidents_and_minorities: payer (powerless/constrained) — disproportionately test cases for incitement/true-threats boundaries
 *   - absolutist_and_harm_limited_advocates: excluded (organized/constrained) — argue for replacing the balancing method entirely, neither controls doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.58).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.52).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Protection via Judicial Category-Balancing (Categorical Balancing Reading)").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, 'e524eeaa-6086-437f-a8ee-a906ec283156').
narrative_ontology:cs_kernel_codification('e524eeaa-6086-437f-a8ee-a906ec283156', fixed_text).
narrative_ontology:cs_authority_grounding('e524eeaa-6086-437f-a8ee-a906ec283156', lineage).
narrative_ontology:cs_interpretation_layer_present('e524eeaa-6086-437f-a8ee-a906ec283156').
narrative_ontology:cs_reading_relation('e524eeaa-6086-437f-a8ee-a906ec283156', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e524eeaa-6086-437f-a8ee-a906ec283156', first_amendment_speech_protection__harm_limited_reading, influences).
narrative_ontology:cs_axiom('e524eeaa-6086-437f-a8ee-a906ec283156', foundational, speech_value_admits_judicial_weighing_against_harm).
narrative_ontology:cs_axiom_status(speech_value_admits_judicial_weighing_against_harm, holdable).
narrative_ontology:cs_axiom_grounding('e524eeaa-6086-437f-a8ee-a906ec283156', speech_value_admits_judicial_weighing_against_harm, conventional).
narrative_ontology:cs_axiom('e524eeaa-6086-437f-a8ee-a906ec283156', secondary, categorical_exclusions_are_judicially_revisable).
narrative_ontology:cs_axiom_status(categorical_exclusions_are_judicially_revisable, holdable).
narrative_ontology:cs_axiom_grounding('e524eeaa-6086-437f-a8ee-a906ec283156', categorical_exclusions_are_judicially_revisable, instrumental).
narrative_ontology:cs_reference_frame('e524eeaa-6086-437f-a8ee-a906ec283156', textual_absolutism_baseline).
narrative_ontology:cs_drift_state('e524eeaa-6086-437f-a8ee-a906ec283156', post_brandenburg_incitement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e524eeaa-6086-437f-a8ee-a906ec283156', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, constitutional_law_academy).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, established_media_institutions).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, speakers_in_contested_categories).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, political_dissidents_and_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, lower_court_litigants).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, living_constitutionalism_doctrine).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, balancing_test_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and redraws the boundaries of unprotected categories (obscenity, incitement, true threats, fighting words) case by case, weighing asserted speech value against asserted harm. Retains ongoing interpretive authority because no fixed rule ever forecloses relitigating a boundary; each new case is an occasion to restate or shift the line.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Produces the doctrinal frameworks (strict scrutiny tiers, categorical tests, ad hoc balancing formulas) that give the judiciary's line-drawing intellectual cover. Careers, casebooks, and clerkship pipelines depend on the categories remaining contestable and therefore requiring ongoing scholarly elaboration.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, constitutional_law_academy, beneficiary,
    organized, generational, arbitrage, national).

% Have the legal resources to litigate close cases and typically fall comfortably within categories courts protect (institutional press, established political speech). Benefit from a doctrine flexible enough to accommodate their interests while its unpredictability burdens smaller or less-resourced speakers.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, established_media_institutions, beneficiary,
    powerful, generational, mobile, national).

% Individuals whose speech falls near a contested boundary (protest speech characterized as incitement, artistic or sexual expression characterized as obscenity, threatening rhetoric characterized as a true threat) cannot know in advance which side of the line a court will place them on. Bear prosecution, civil liability, or platform/employer sanction while the category is litigated after the fact.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, speakers_in_contested_categories, payer,
    powerless, immediate, trapped, local).

% Disproportionately represented among defendants in incitement and true-threats cases historically and today, because the harm side of the balancing test is more readily found credible when the speaker is already viewed as dangerous or marginal. Cannot exit the jurisdiction whose courts draw these lines; their speech is the recurring test case that redraws the boundary for everyone else.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, political_dissidents_and_minorities, payer,
    powerless, biographical, constrained, national).

% Trial and appellate parties below the Supreme Court must apply an unsettled, multi-factor balancing standard without a bright-line rule, producing inconsistent outcomes across circuits and jurisdictions. Their exposure to liability or conviction depends heavily on which circuit's balancing tradition happens to govern their case.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, lower_court_litigants, payer,
    moderate, immediate, constrained, regional).

% Textualist absolutists and harm-based reformers both argue the balancing approach is illegitimate — one because it abandons the categorical command of the text, the other because it protects speech that causes demonstrable harm without requiring proof of consent or damage. Neither camp controls the doctrine; both litigate around its edges rather than replacing it.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, absolutist_and_harm_limited_advocates, excluded,
    organized, generational, constrained, national).

% Study the accumulated case law, track circuit splits, and assess whether the balancing framework produces coherent, predictable results over time. Their scholarship both critiques and legitimizes the ongoing case-by-case method.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, constitutional_scholars_and_appellate_judges, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__categorical_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for adjudicating genuinely hard cases where speech's value and its harm are both real and contested (e.g., true threats, incitement to imminent violence) without requiring a single bright-line rule that would either over-protect dangerous speech or under-protect legitimate expression.
% TRANSFER_FUNCTION: Moves interpretive authority and litigation risk from the legislature and the speaker to the judiciary: the judiciary retains ongoing power to define the boundaries of protection, while individual speakers near the boundary bear the unpredictability and cost of finding out, after the fact, which side of the line their speech fell on.
% ABSENT_VOICES: Absolutist textualists (who would remove the balancing discretion entirely) and harm-limited reformers (who would substitute a consent/harm test) are both present in legal discourse but neither controls doctrine formation; genuinely marginalized speakers whose cases become the test vehicles for redrawing category lines rarely have a voice in how the resulting rule will apply to future, differently situated speakers.
% DISAPPEARANCE_RATIONALE: If categorical balancing vanished overnight in favor of either a strict textual absolutism or a harm-limited test, entire bodies of doctrine (obscenity law, true-threats doctrine, incitement's imminence requirement, defamation's actual-malice standard) would need to be rebuilt on different premises; litigation strategy, First Amendment scholarship, and the judiciary's institutional role as ongoing arbiter would all restructure substantially.
% FOUNDING_PROBLEM: Courts needed a way to handle speech cases where a flat 'no law abridging' rule seemed to produce absurd or dangerous results (e.g., true threats, child pornography, incitement to imminent lawless action) without abandoning First Amendment protection altogether — the founding problem was reconciling textual absolutism with the practical necessity of some exceptions.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and mainstream constitutional scholarship attest the founding problem remains live — genuinely hard cases with real harm-value tradeoffs continue to arise. Textualist critics (originalist scholars, some sitting justices in concurrences and dissents) and harm-based reformers attest from outside the judiciary's own beneficiary position that the balancing framework has drifted from resolving genuinely hard cases into a general-purpose discretionary tool that lets courts reach preferred outcomes while claiming principled constraint; empirical studies of circuit-split inconsistency in incitement and true-threats cases corroborate the predictability critique from outside all three advocacy camps.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects that the balancing method transfers real value — the judiciary's retained interpretive authority and the academy's elaboration industry — while imposing real, unevenly distributed costs on marginal speakers who cannot know the rule in advance. Suppression (0.52) is moderate: speakers are not flatly barred from expression, but the credible threat of after-the-fact liability chills speech near contested boundaries, and this has strengthened somewhat over the measured interval as more categories (true threats, incitement, non-obscene sexual expression) have accumulated additional balancing sub-tests. Theater ratio (0.41) captures that a substantial share of the doctrinal apparatus (multi-factor tests, tiers of scrutiny) functions partly as legitimating performance for outcomes the judiciary could reach more directly — but the coordination function (resolving genuinely hard cases) is real, not fully performative, which is why this does not cross into piton territory. Accessibility collapse (0.47) and resistance (0.62) reflect that alternatives (textual absolutism, harm-limited tests) remain live, actively argued positions rather than foreclosed — this is a contested doctrinal space, not a settled natural fact.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary sits nearest the beneficiary end: it authored the balancing method, retains the discretion to revise it, and bears essentially no downside from doctrinal instability — instability is, if anything, the source of its ongoing relevance. The constitutional law academy is a secondary beneficiary: its professional and reputational capital depends on the categories remaining genuinely contestable enough to require sustained scholarly elaboration. Established media institutions benefit incidentally — they typically litigate from a position of resource advantage and tend to fall within protected categories, so doctrinal flexibility rarely threatens them and sometimes helps them (e.g., broad 'newsworthiness' balancing). Speakers in contested categories and political dissidents/minorities sit at the target end: trapped or constrained exit, immediate exposure to liability determined retroactively by a standard they could not have applied in advance to their own conduct. Lower court litigants are moderate targets: the unpredictability compounds through circuit splits that create geography-dependent outcomes for facially similar speech.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuinely hard cases where textual absolutism produces intolerable results — has not disappeared, which prevents mislabeling this constraint as a pure snare with no coordination function; some true-threats and incitement cases really do require weighing competing values that a flat rule cannot resolve. But the founding_problem_status is authored as contested rather than simply live, because independent circuit-split data and originalist critique from outside the judiciary's own seat corroborate that the balancing apparatus has expanded well past the narrow set of genuinely hard cases into a general-purpose discretionary tool. This is the tangled-rope signature: a real coordination function (resolving genuinely hard cases) persists alongside asymmetric extraction (retained judicial discretion, academic elaboration industry) that the coordination function alone does not require and that requires active enforcement (ongoing litigation, circuit court rulings, contempt/injunction machinery) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_as_principled_or_discretionary,
    'Is case-by-case categorical balancing a principled method for resolving genuinely hard cases, or a discretionary tool that lets courts reach preferred outcomes while claiming doctrinal constraint?',
    'Longitudinal analysis of whether balancing outcomes correlate more strongly with the stated multi-factor tests or with extralegal factors (speaker identity, political salience, judicial ideology) across a large sample of incitement, true-threats, and obscenity cases.',
    'If outcomes track extralegal factors more than the stated tests, the coordination story is substantially cover for outcome-driven adjudication, pushing this reading further toward snare; if outcomes track the stated factors reliably, the coordination function is more genuinely load-bearing, pushing toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_as_principled_or_discretionary, empirical, 'Whether the balancing method is principled adjudication or outcome-driven discretion dressed as doctrine.').

omega_variable(
    which_reading_is_the_true_kernel,
    'Is the categorical-balancing reading the doctrinally correct interpretation of the First Amendment kernel, or is it one contestable reading among the absolutist and harm-limited alternatives, with no reading holding a privileged claim to correctness?',
    'This is a conceptual/interpretive question that cannot be resolved by empirical data alone; it depends on contested theories of constitutional interpretation (originalism, living constitutionalism, textualism) that themselves have no neutral adjudicator.',
    'If categorical balancing is treated as simply correct, its extraction is invisible by definition (the standard IS the baseline); if it is one contestable reading among several, its extraction becomes visible as the cost of choosing this interpretive method over the alternatives that different parties actively advocate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_true_kernel, conceptual, 'Whether categorical balancing is the privileged reading of the kernel or one contested reading among equals — the committer-frame question underlying this whole story.').

omega_variable(
    disproportionate_impact_on_minorities,
    'Is the disproportionate representation of political dissidents and minorities in incitement/true-threats case law a structural feature of how the harm side of the balancing test is applied, or an artifact of who happens to engage in the specific conduct courts are asked to evaluate?',
    'Comparative case analysis controlling for underlying conduct type across speaker demographics and political affiliation, to isolate whether courts apply the harm-finding differently based on speaker identity holding conduct constant.',
    'If the harm-finding is applied asymmetrically by speaker identity, the victim declaration for political_dissidents_and_minorities is a structural feature of the constraint rather than a downstream correlation; if not, the disproportion is better explained by other factors and the victim framing should be narrowed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disproportionate_impact_on_minorities, empirical, 'Whether disproportionate impact on dissidents/minorities is built into the balancing method''s application or is an artifact of case selection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(firs_tr_t50, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(firs_tr_t60, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 60, 0.41).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement(firs_be_t50, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement(firs_be_t60, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(firs_su_t50, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 50, 0.51).
narrative_ontology:measurement(firs_su_t60, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__categorical_balancing_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the first_amendment_speech_protection kernel, decomposed per the ε-invariance principle because the natural-language label 'First Amendment protection' covers structurally distinct interpretive commitments with different ε values and different beneficiary/victim structures. absolutist_reading treats 'no law' as categorical with narrow historical exceptions (near-mountain, low extraction, judiciary as rule-follower). harm_limited_reading conditions protection on absence of demonstrable unconsented-to harm (beneficiary/victim structure centers on harm-claimants vs. alleged harm-causers). This story, categorical_balancing_reading, treats protection as defined through ongoing judicial line-drawing (moderate-to-substantial extraction, judiciary and legal academy as primary beneficiaries of retained interpretive discretion). All three should be read as competing accounts of the same underlying constitutional text, not as measurements of one constraint from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
