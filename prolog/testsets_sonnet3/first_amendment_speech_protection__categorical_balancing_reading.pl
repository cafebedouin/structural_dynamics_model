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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Categorical Balancing Doctrine
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the categorical balancing reading of the First
 *   Amendment speech-protection kernel: the view that constitutional text
 *   does not itself resolve which speech is protected, and that courts
 *   legitimately develop categories (obscenity, incitement, fighting words,
 *   true threats) through case-by-case weighing of speech value against harm.
 *   This is a distinct constraint from the absolutist reading (protection is
 *   categorical except for narrow historical exclusions) and the harm-limited
 *   reading (protection yields to demonstrable unconsented harm) — those are
 *   separate stories with their own ε values, beneficiary/victim structures,
 *   and classifications, linked here only by network reference. Under this
 *   reading, the coordination function (adapting an old text to new
 *   circumstances) is real, but the same mechanism that performs that
 *   coordination also concentrates interpretive authority in the judiciary
 *   and imposes unpredictability costs on the speakers least able to absorb
 *   them.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda_setter (institutional/arbitrage) — administers and continuously redefines the categorical boundary
 *   - supreme_court_justices: beneficiary (institutional/arbitrage) — gains durable interpretive relevance from ongoing case-by-case adjudication
 *   - unpopular_minority_speakers: payer (powerless/trapped) — bears disproportionate risk of adverse categorization
 *   - litigants_facing_unpredictable_categorization: payer (moderate/constrained) — bears the cost of not knowing ex ante whether speech is protected
 *   - civil_liberties_organizations: excluded/observer (organized/constrained) — litigates within the framework but cannot challenge its structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.58).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.52).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Categorical Balancing Doctrine").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, 'b513fd8a-53e1-45ad-a76b-029635587bb5').
narrative_ontology:cs_kernel_codification('b513fd8a-53e1-45ad-a76b-029635587bb5', fixed_text).
narrative_ontology:cs_authority_grounding('b513fd8a-53e1-45ad-a76b-029635587bb5', lineage).
narrative_ontology:cs_interpretation_layer_present('b513fd8a-53e1-45ad-a76b-029635587bb5').
narrative_ontology:cs_reading_relation('b513fd8a-53e1-45ad-a76b-029635587bb5', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('b513fd8a-53e1-45ad-a76b-029635587bb5', first_amendment_speech_protection__harm_limited_reading, influences).
narrative_ontology:cs_axiom('b513fd8a-53e1-45ad-a76b-029635587bb5', foundational, judicial_categorization_is_legitimate_interpretive_method).
narrative_ontology:cs_axiom_status(judicial_categorization_is_legitimate_interpretive_method, holdable).
narrative_ontology:cs_axiom_grounding('b513fd8a-53e1-45ad-a76b-029635587bb5', judicial_categorization_is_legitimate_interpretive_method, conventional).
narrative_ontology:cs_axiom('b513fd8a-53e1-45ad-a76b-029635587bb5', foundational, speech_value_is_commensurable_with_harm_for_case_by_case_weighing).
narrative_ontology:cs_axiom_status(speech_value_is_commensurable_with_harm_for_case_by_case_weighing, holdable).
narrative_ontology:cs_axiom_grounding('b513fd8a-53e1-45ad-a76b-029635587bb5', speech_value_is_commensurable_with_harm_for_case_by_case_weighing, instrumental).
narrative_ontology:cs_reference_frame('b513fd8a-53e1-45ad-a76b-029635587bb5', clear_and_present_danger_era_balancing).
narrative_ontology:cs_drift_state('b513fd8a-53e1-45ad-a76b-029635587bb5', post_1969_multifactor_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b513fd8a-53e1-45ad-a76b-029635587bb5', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, constitutional_law_scholarship_industry).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, unpopular_minority_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, litigants_facing_unpredictable_categorization).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, lower_court_bound_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Creates and maintains the taxonomy of unprotected categories (obscenity, incitement, true threats, fighting words) and performs the ad hoc balancing that decides which new speech falls inside or outside protection. Because the categories are judicially defined and continuously refined case-by-case, the judiciary retains ongoing interpretive authority over the scope of the Amendment rather than ceding that authority to a fixed textual rule.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Individually and collectively gain durable relevance and discretionary power from the balancing framework: every new communications technology or speech controversy becomes a fresh occasion for the Court to declare where the line sits, rather than a mechanical application of settled text. Their institutional prestige and doctrinal legacy depend on continued case-by-case adjudication.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, supreme_court_justices, beneficiary,
    institutional, generational, arbitrage, national).

% Law reviews, casebooks, CLE programs, and academic careers are built on parsing and criticizing the ever-shifting balancing tests. The doctrine's inherent instability generates a continuous supply of scholarly and pedagogical material that a bright-line rule would foreclose.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, constitutional_law_scholarship_industry, beneficiary,
    organized, generational, mobile, national).

% Speech by politically marginal, racially targeted, or otherwise disfavored groups is disproportionately vulnerable to being swept into an unprotected category (e.g., 'fighting words,' 'true threats') because the balancing test invites judges to weigh the social value of speech against perceived harm, and marginal speakers' speech is systematically valued lower by dominant cultural assumptions. They cannot litigate their way out of an adverse categorization and often cannot afford to try.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, unpopular_minority_speakers, payer,
    powerless, biographical, trapped, local).

% Individuals and organizations planning speech (protesters, publishers, platform users) cannot know in advance whether their expression will be classified as protected until a court rules, often years after the speech occurred and any chilling effect has already done its work. They bear the cost of the doctrine's unpredictability in the form of self-censorship or expensive post hoc litigation.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, litigants_facing_unpredictable_categorization, payer,
    moderate, biographical, constrained, national).

% Speakers in jurisdictions where circuit splits or unresolved balancing tests leave the boundary of protection genuinely unsettled face materially different outcomes depending on which circuit hears their case — the same speech act can be protected in one region and punishable in another, with no realistic option to relocate before speaking.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, lower_court_bound_speakers, payer,
    powerless, immediate, trapped, regional).

% Groups like the ACLU litigate individual cases within the balancing framework but have no mechanism to force adoption of a categorical, predictable rule; their institutional strategy is confined to arguing at the margins of categories the judiciary alone controls, so their objection to the doctrine's unpredictability is heard case-by-case but never as a structural challenge to the framework itself.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, civil_liberties_organizations, excluded,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, civil_liberties_organizations, observer).

% Elected bodies that might prefer clearer statutory speech rules are preempted from doing so wherever the judiciary has already occupied the field with constitutional balancing tests; legislative attempts to draw brighter lines are themselves subject to judicial review under the same balancing framework, keeping ultimate line-drawing authority with the courts.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legislatures, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__categorical_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for adapting speech protection to novel circumstances (new technologies, new harms) without requiring constitutional amendment, allowing the doctrine to address cases the founding text's drafters could not have anticipated.
% TRANSFER_FUNCTION: Moves interpretive authority over the boundary of a fundamental right from the legislature and from a fixed textual rule to the judiciary, and moves the cost of doctrinal unpredictability from institutional actors (who can absorb litigation costs and shape precedent) to individual speakers who cannot.
% ABSENT_VOICES: Legislatures that might prefer bright-line statutory rules are structurally excluded from setting the boundary; civil liberties organizations can litigate individual cases but not challenge the balancing framework itself; ordinary speakers whose expression is chilled by unpredictability rarely appear in court at all because the chilling effect prevents the speech act that would generate standing.
% DISAPPEARANCE_RATIONALE: If categorical balancing disappeared and were replaced by either a strict textual absolutism or a fixed harm-threshold rule, the judiciary would lose ongoing discretionary authority over speech classification, an entire genre of constitutional scholarship would lose its subject matter, and legal predictability for speakers would increase substantially — but some contested speech acts (true threats, incitement) would need new fixed rules to resolve cases the old balancing test currently absorbs into judicial discretion.
% FOUNDING_PROBLEM: The First Amendment's text does not itself specify whether categories like obscenity, defamation, or incitement fall inside or outside 'freedom of speech,' and early courts needed some mechanism to resolve genuinely hard cases (e.g., wartime sedition, libel, fighting words) without either banning all restriction or permitting unlimited restriction.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and legal academy attest the balancing framework remains necessary to handle genuinely novel speech technologies and harms (e.g., deepfakes, algorithmic amplification). Legal predictability scholars and several sitting and retired judges (writing extrajudicially) attest the founding problem of resolving genuinely hard cases has calcified into open-ended judicial discretion that exceeds what the original hard cases required, citing the proliferation of multi-factor tests with no textual anchor as evidence the framework now serves institutional control rather than the founding necessity.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.58 — meaningfully extractive but well below a snare-level reading — because the balancing framework does perform genuine coordination work (translating an 18th-century text onto unanticipated speech technologies and harms) even as it also concentrates discretionary power in the judiciary and imposes real costs on unpredictability-exposed speakers. Suppression sits near the midpoint (0.52) because the mechanism does not categorically forbid speech the way a snare would; it creates a standing risk that any given speech act might retroactively be reclassified as unprotected, which chills without directly banning. Theater ratio (0.4) reflects that a substantial share of the doctrinal apparatus — multi-factor tests, extensive line-drawing opinions — functions partly as legitimating performance for what is, in practice, judicial discretion dressed in the language of principled balancing. Accessibility collapse is moderate (0.45): unlike a mountain, workable alternative doctrinal architectures (textual absolutism, fixed harm thresholds) visibly exist and are actively argued by named parties in this very kernel contest, so alternatives have not collapsed once the doctrine is understood — if anything, understanding the doctrine reveals its alternatives more clearly. Resistance is elevated (0.62) because critics from across the political spectrum (free-speech absolutists, harm-reduction advocates, legal-predictability scholars) actively contest the framework, unlike a genuinely settled coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary and Supreme Court justices sit near the beneficiary end: they set and continuously revise the categories, retaining interpretive authority that a fixed rule would foreclose, and their institutional standing is partly constituted by ongoing exercise of that authority. The constitutional law scholarship industry benefits similarly through secondary effects — instability generates scholarly material — though its exit options (mobile) are better than the judiciary's identity-bound relationship to the doctrine. Unpopular minority speakers and lower-court-bound speakers sit near the target end: trapped exit options, immediate/biographical time horizons, and structurally elevated risk of adverse categorization because the balancing test explicitly invites valuation of speech's social worth, a valuation historically disfavoring marginal viewpoints. Litigants facing unpredictable categorization occupy an intermediate position — moderate power, constrained exit — bearing diffuse unpredictability costs rather than concentrated targeting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resolving genuinely hard cases the constitutional text does not itself answer) was real in 1919 and remains partly live for genuinely novel cases (algorithmic speech, deepfakes). This prevents a flat 'pure extraction' verdict — there is a real coordination function under the doctrine, which is why this story claims tangled_rope rather than snare. But the corroboration record shows the founding problem's status is contested: critics attest that what began as case-by-case resolution of hard cases has calcified into a self-perpetuating apparatus of multi-factor tests whose primary function is now judicial discretion-preservation rather than resolution of genuinely novel circumstances. The tangled_rope classification holds both truths at once rather than forcing a choice between 'legitimate doctrine' and 'raw power grab.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_discretion_capture,
    'Is the categorical balancing framework a genuinely necessary coordination mechanism for resolving cases the constitutional text cannot itself decide, or has it become a self-perpetuating mechanism for preserving judicial discretion beyond what resolving hard cases actually requires?',
    'Comparative analysis of jurisdictions/eras where narrower, more rule-bound speech doctrines operate (e.g., certain state constitutional interpretations, or the doctrine''s own earlier and more categorical phases) against outcomes under the modern multi-factor balancing tests, isolating whether case outcomes track genuinely novel circumstances or track expansion of discretionary factors untethered from the founding hard cases.',
    'If the framework tracks genuinely novel circumstances, the tangled_rope classification''s coordination component is well-supported. If the framework has expanded well beyond resolving genuinely novel cases into general-purpose discretion, the classification would drift toward snare with the judiciary as concentrated beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_discretion_capture, conceptual, 'Whether the balancing doctrine still tracks its founding coordination problem or has become self-perpetuating judicial discretion.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the correct unit of analysis the balancing methodology itself (this story) or the specific category-by-category outputs (obscenity doctrine, incitement doctrine, true-threats doctrine) treated as separate sub-constraints with potentially different ε values?',
    'Decompose each category (obscenity, incitement, fighting words, true threats) into its own constraint story and compare ε values; if they diverge substantially, the aggregate 0.58 in this story is a lossy average masking sharper extraction in specific categories (e.g., true-threats doctrine applied to minority political speech) diluted by more settled categories (e.g., long-stable obscenity doctrine).',
    'If sub-category ε values diverge widely, this story should itself be decomposed per the ε-invariance principle rather than treated as a single unified reading; the current single-file treatment is a simplification chosen for tractability given the kernel-contest framing already required by the manifest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the categorical balancing reading should itself decompose into per-category sub-constraints.').

omega_variable(
    judicial_beneficiary_naturalness,
    'Is judicial interpretive authority over speech categorization a beneficiary relationship (the judiciary gains discretionary power it would not otherwise hold) or is it simply the necessary structural consequence of any judiciary empowered to adjudicate constitutional claims at all — i.e., is ''benefiting from discretion'' distinguishable from ''performing the judicial function as designed''?',
    'Compare the scope of discretion exercised under speech balancing against the scope of discretion the same courts exercise in other constitutional domains with more textually determinate standards, to isolate whether speech doctrine confers unusually broad discretion relative to the judiciary''s baseline constitutional role.',
    'If speech balancing confers discretion in excess of the judiciary''s baseline constitutional role, the beneficiary declaration for federal_judiciary and supreme_court_justices is well-grounded. If the discretion is comparable to ordinary constitutional adjudication generally, the beneficiary framing overstates what is simply judicial role performance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_beneficiary_naturalness, conceptual, 'Whether judicial discretion under speech balancing is an extractive beneficiary relationship or ordinary constitutional role performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 1919, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1919, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1919, 0.2).
narrative_ontology:measurement(firs_tr_t1950, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1950, 0.28).
narrative_ontology:measurement(firs_tr_t1969, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1969, 0.32).
narrative_ontology:measurement(firs_tr_t1990, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(firs_tr_t2010, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(firs_be_t1919, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1919, 0.35).
narrative_ontology:measurement(firs_be_t1950, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(firs_be_t1969, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1969, 0.48).
narrative_ontology:measurement(firs_be_t1990, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(firs_be_t2010, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1919, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1919, 0.4).
narrative_ontology:measurement(firs_su_t1950, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(firs_su_t1969, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1969, 0.45).
narrative_ontology:measurement(firs_su_t1990, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement(firs_su_t2010, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__categorical_balancing_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the first_amendment_speech_protection kernel. The absolutist_reading treats 'no law' as near-categorical protection with only narrow historical exclusions, yielding a low-extraction near-Mountain or Rope profile with different (or absent) beneficiary/victim structure. The harm_limited_reading conditions protection on demonstrable unconsented harm, producing yet another beneficiary/victim map (protecting harm-claimants, potentially burdening speakers differently than this reading does). Each reading is authored as an independently ε-stable constraint per the ε-invariance principle; they are linked here via affects_constraints because judicial adoption or erosion of one reading structurally changes the legitimacy conditions and doctrinal resources available to the others (e.g., strengthening the harm_limited_reading in practice would erode the categorical_balancing_reading's exclusive claim to define which harms count).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
