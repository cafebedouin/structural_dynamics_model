% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Categorical Balancing of Speech Value Against Harm
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The First Amendment's command that 'Congress shall make no law...
 *   abridging the freedom of speech' has been interpreted by the federal
 *   judiciary through a categorical balancing framework in which courts
 *   define protected and unprotected speech classes and apply case-by-case
 *   balancing of speech value against competing harms. This reading
 *   instantiates one commitment-system kernel contested by absolutist ('no
 *   law means no law') and harm-limited ('protection yields to demonstrated
 *   harm') readings. Under the categorical balancing reading, the
 *   institutional judiciary is the primary beneficiary, capturing
 *   interpretive supremacy over democratic legislatures, while minorities
 *   whose expression sits near category boundaries and litigants seeking
 *   predictable rules bear the costs. The coordination functionâenabling
 *   government to regulate genuine harms without legislative carte
 *   blancheâis real, but the extraction of interpretive control and the
 *   suppression of alternative constitutional methodologies make this a
 *   tangled rope rather than a pure coordination mechanism.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Primary agenda-setter and beneficiary (institutional/arbitrage) â captures interpretive control and the power to define constitutional speech categories
 *   - minorities_in_protected_categories: Primary target (powerless/identity_locked) â nominally protected but find their speech devalued in balancing against state or majoritarian interests
 *   - democratic_legislatures: Secondary target (organized/constrained) â democratic authority over speech regulation is preempted by judicial review
 *   - litigants_seeking_predictability: Secondary target (moderate/constrained) â bear costs of doctrinal uncertainty under balancing tests
 *   - free_speech_absolutists: Excluded voice (moderate/constrained) â structurally excluded from judicial doctrine because their position rejects the balancing premise
 *   - constitutional_scholars_observer: Analytical observer (analytical/analytical) â documents doctrinal drift and distributional effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.62).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.58).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Categorical Balancing of Speech Value Against Harm").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, '9b429156-fd30-45d5-83e8-d5e3d1a6bf49').
narrative_ontology:cs_kernel_codification('9b429156-fd30-45d5-83e8-d5e3d1a6bf49', fixed_text).
narrative_ontology:cs_authority_grounding('9b429156-fd30-45d5-83e8-d5e3d1a6bf49', lineage).
narrative_ontology:cs_interpretation_layer_present('9b429156-fd30-45d5-83e8-d5e3d1a6bf49').
narrative_ontology:cs_reading_relation('9b429156-fd30-45d5-83e8-d5e3d1a6bf49', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b429156-fd30-45d5-83e8-d5e3d1a6bf49', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('9b429156-fd30-45d5-83e8-d5e3d1a6bf49', foundational, judicial_balancing_defines_speech_protection).
narrative_ontology:cs_axiom_status(judicial_balancing_defines_speech_protection, holdable).
narrative_ontology:cs_axiom_grounding('9b429156-fd30-45d5-83e8-d5e3d1a6bf49', judicial_balancing_defines_speech_protection, conventional).
narrative_ontology:cs_reference_frame('9b429156-fd30-45d5-83e8-d5e3d1a6bf49', evolving_common_law_speech_doctrine).
narrative_ontology:cs_drift_state('9b429156-fd30-45d5-83e8-d5e3d1a6bf49', contemporary_culture_war_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9b429156-fd30-45d5-83e8-d5e3d1a6bf49', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minorities_in_protected_categories).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, litigants_seeking_predictability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, democratic_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains interpretive control over First Amendment doctrine by creating and applying categorical balancing tests that define which speech is protected and which is not. Reviews and strikes down or upholds federal and state speech regulations based on these judicially created categories.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary, beneficiary).

% Are nominally protected by First Amendment categories but frequently find their speech balanced against competing interests such as public order, majoritarian comfort, or national security and deemed unprotected or less valuable. Their exit options are limited because the speech central to their identity and political participation is the object of balancing.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, minorities_in_protected_categories, payer,
    powerless, biographical, identity_locked, national).

% Enact speech regulations to address local harms including obscenity, incitement, and harassment but face judicial preemption under the balancing framework. Their democratic authority to set community standards is constrained by federal judicial interpretation.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, democratic_legislatures, payer,
    organized, biographical, constrained, national).

% Face highly uncertain legal outcomes because balancing tests provide less categorical guidance than rule-based approaches. The cost of legal uncertainty falls on speakers, platforms, and publishers who cannot know in advance whether their expression will survive judicial review.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, litigants_seeking_predictability, payer,
    moderate, immediate, constrained, national).

% Hold the position that the First Amendment prohibits nearly all content-based speech regulation. Are systematically excluded from judicial doctrine because the balancing framework presupposes that some speech can be regulated. Their arguments are raised in briefs but rarely prevail.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, free_speech_absolutists, excluded,
    moderate, generational, constrained, national).

% Analyze and critique the development of speech doctrine from outside the judiciary. Document the drift from categorical rules toward ad hoc balancing and the distributional effects on different speaker classes.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, constitutional_scholars_observer, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__categorical_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves conflicts between expressive freedom and competing social interests such as public order, privacy, and dignity by creating judicial categories that permit government to regulate genuinely harmful expression while preventing legislative overreach against dissent.
% TRANSFER_FUNCTION: Moves interpretive authority over speech regulation from democratic legislatures to the federal judiciary; moves the cost of legal uncertainty to litigants and vulnerable speakers whose expression sits near category boundaries.
% ABSENT_VOICES: Free speech absolutists who reject any content-based balancing; minority communities whose speech is routinely devalued in the balancing calculus; legislative majorities who would set different harm thresholds. These voices are structurally excluded from the judicial doctrine that frames the constitutional conversation.
% DISAPPEARANCE_RATIONALE: If the categorical balancing framework vanished overnight, legislatures would regain primary authority to set speech and harm boundaries, existing obscenity and incitement doctrines would lack their constitutional grounding, and the judiciary would lose its supervisory role over speech regulation. First Amendment jurisprudence would reorganize around legislative choice, textual absolutism, or alternative constitutional methodologies.
% FOUNDING_PROBLEM: How to protect expressive freedom while permitting government to regulate harms like incitement, obscenity, and true threats without empowering legislatures to suppress political and religious dissent.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians attest the problem was genuine in the early twentieth century when speakers faced prosecution for anti-war and labor advocacy. Contemporary free speech absolutists and constitutional scholars outside the judiciary contest that judicial balancing was the necessary solution, arguing the categories became vehicles for judicial preference. Corroboration from legislative historians and comparative constitutionalists supports the view that alternative institutional arrangements were structurally available.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) reflects the substantial but not total extraction inherent in judicial supremacy over speech regulation: the judiciary captures interpretive control and the power to legitimate or delegitimate democratic choices, while minorities and litigants bear the costs of unpredictable, ideologically variable outcomes. Suppression (0.58) measures the degree to which alternative constitutional methodologiesâabsolutist textualism, legislative majoritarianism, European-style proportionalityâare foreclosed or marginalized within U.S. doctrine. Theater ratio (0.42) captures the performative dimension of judicial claims to neutral, principled categorization that often masks ad hoc political balancing. Accessibility collapse (0.68) is high because once a society accepts that courts must balance speech value against harm, legislative alternatives and textualist absolutism collapse as live constitutional options. Resistance (0.52) is moderate: absolutists, textualist scholars, and some legislators resist, but the framework has been dominant for decades. Temporal measurements show rising extractiveness and theater from the start of the interval to the end as doctrine shifted from relatively firm categories toward increasingly ad hoc, ideology-inflected balancing.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary experiences this constraint as genuine coordination: it solves the problem of protecting speech while permitting necessary regulation, and the judiciary sees itself as the neutral arbiter performing this function. Minorities in protected categories and democratic legislatures experience it as extraction: the former find their speech systematically devalued when balanced against order or majoritarian comfort, while the latter find their democratic authority usurped. Litigants experience unpredictability as a diffuse tax on expression. The engine computes this divergence from the structural dataâlow directionality for the beneficiary agenda-setter, high directionality for the trapped and constrained payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the federal_judiciary, which collects interpretive control and institutional legitimacy. Victim declarations map to minorities_in_protected_categories and litigants_seeking_predictability, who bear the costs of balancing unpredictability and categorical exclusion. Democratic_legislatures are also payers though not listed in the victims array. The federal judiciary's power (institutional) and exit (analytical) place it near the beneficiary end; minorities' power (powerless) and exit (identity_locked) place them near the full-target end, amplifying effective extraction. Free speech absolutists are excluded, receiving no directionality benefit but also not directly taxed by the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophy mislabeling because it preserves a genuine coordination function: without some framework for distinguishing protected from unprotected speech, legislatures might overregulate or underregulate expression. The classification as tangled rope rather than snare depends on that genuine function. However, the coordination story does not justify the level of interpretive control captured by the judiciary or the suppression of alternative methodological frameworks. If the coordination function were the whole story, the constraint would be a rope with diffuse benefits and no victims; the presence of identifiable victims and concentrated beneficiaries establishes the asymmetric extraction that makes it tangled. A piton classification would require the coordination function to have atrophied entirely, leaving only performative maintenance; while theater is rising, the coordination function remains active, so piton is not warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_neutrality_or_preference,
    'Does the categorical balancing framework produce principled, predictable categories, or does it mask ad hoc judicial preferences about which speech is valuable?',
    'Systematic empirical analysis of balancing outcomes controlling for judicial ideology, speech content, and litigant identity; comparison with outcomes under alternative frameworks such as European proportionality or textual absolutism.',
    'If outcomes track judicial ideology more than principed categories, extraction is higher than structurally claimed and the constraint functions partly as a snare of judicial preference; if neutral, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_neutrality_or_preference, empirical, 'Whether balancing is neutral or masks judicial preference.').

omega_variable(
    kernel_reading_contest,
    'Is the categorical balancing reading the only defensible interpretation of the First Amendment speech kernel, or do the absolutist and harm-limited readings capture structural possibilities that this reading suppresses?',
    'Comparative constitutional analysis and historical linguistics of the ''no law'' text; assessment of whether the categorical balancing framework has foreclosed structurally viable alternatives through path dependence rather than constitutional necessity.',
    'If the sibling readings are structurally viable, the categorical balancing constraint''s accessibility_collapse is higher than warranted and its coordination story is partially cover for judicial supremacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether sibling readings are structurally viable alternatives.').

omega_variable(
    protected_category_minority_experience,
    'Do minorities in categories deemed ''protected'' by the judiciary actually experience the constraint as protective, or does balancing systematically devalue their speech when weighed against majority comfort or state interest?',
    'Empirical studies of speech restriction enforcement patterns across racial, religious, and sexual minorities; analysis of which categories of minority speech most frequently lose in balancing.',
    'If systematically devalued, minorities are victims of extraction rather than beneficiaries of coordination, and the directionality for this seat should be higher (more target-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protected_category_minority_experience, empirical, 'Whether protected-category minorities experience protection or devaluation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(firs_tr_t14, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 14, 0.24).
narrative_ontology:measurement(firs_tr_t28, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 28, 0.3).
narrative_ontology:measurement(firs_tr_t42, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 42, 0.34).
narrative_ontology:measurement(firs_tr_t56, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 56, 0.38).
narrative_ontology:measurement(firs_tr_t70, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 70, 0.42).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(firs_be_t14, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 14, 0.42).
narrative_ontology:measurement(firs_be_t28, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 28, 0.5).
narrative_ontology:measurement(firs_be_t42, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 42, 0.55).
narrative_ontology:measurement(firs_be_t56, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 56, 0.6).
narrative_ontology:measurement(firs_be_t70, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 70, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(firs_su_t14, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 14, 0.4).
narrative_ontology:measurement(firs_su_t28, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 28, 0.48).
narrative_ontology:measurement(firs_su_t42, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 42, 0.52).
narrative_ontology:measurement(firs_su_t56, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 56, 0.56).
narrative_ontology:measurement(firs_su_t70, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 70, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the first_amendment_speech_protection kernel, decomposed per the Îµ-invariance principle from the absolutist_reading and harm_limited_reading because the structural claims, beneficiary-victim profiles, and coordination/extraction balances differ across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
