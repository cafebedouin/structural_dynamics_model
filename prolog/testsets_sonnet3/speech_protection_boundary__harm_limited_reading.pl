% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Harm-Limited Reading of the Speech Protection Boundary
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the harm-limited reading of the speech protection
 *   boundary kernel: speech is protected unless it works significant harm to
 *   dignity, equality, or freedom from harassment. Under this reading, the
 *   protected set is deliberately narrower than the absolutist
 *   Brandenburg-imminence standard and structurally different from the ad hoc
 *   balancing reading — this reading fixes harm-to-protected-status
 *   categories (dignity, equality, harassment-freedom) as the operative gate
 *   rather than either near-absolute protection or open case-by-case
 *   weighing. The state becomes an active gatekeeper adjudicating what counts
 *   as dignitary or equality harm, which is the reading's central structural
 *   feature and its central risk: the same gatekeeping apparatus that
 *   protects targeted groups from organized hate speech can be captured by
 *   majoritarian actors to suppress dissident or heterodox speech
 *   recharacterized as 'harm.' The extractiveness trend over time models
 *   increasing invocation of the standard against a widening range of speech
 *   as the doctrine matures and gatekeepers develop broader harm taxonomies.
 *
 * KEY AGENTS:
 *   - state_speech_gatekeepers: courts and tribunals administering the harm test (institutional/analytical)
 *   - targeted_minority_groups: primary intended beneficiaries of the harm-limited standard (organized/constrained)
 *   - dissident_political_speakers: primary bearers of the doctrine's chilling and liability risk (powerless/trapped)
 *   - civil_libertarian_critics: analytical observers tracking abuse patterns (organized/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.58).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.62).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Harm-Limited Reading of the Speech Protection Boundary").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '054f5a98-c35f-4b8a-9f58-e83711f1d0ce').
narrative_ontology:cs_kernel_codification('054f5a98-c35f-4b8a-9f58-e83711f1d0ce', distributed).
narrative_ontology:cs_authority_grounding('054f5a98-c35f-4b8a-9f58-e83711f1d0ce', distributed).
narrative_ontology:cs_reading_relation('054f5a98-c35f-4b8a-9f58-e83711f1d0ce', speech_protection_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('054f5a98-c35f-4b8a-9f58-e83711f1d0ce', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('054f5a98-c35f-4b8a-9f58-e83711f1d0ce', foundational, equality_and_dignity_are_coequal_constitutional_values).
narrative_ontology:cs_axiom_status(equality_and_dignity_are_coequal_constitutional_values, holdable).
narrative_ontology:cs_axiom_grounding('054f5a98-c35f-4b8a-9f58-e83711f1d0ce', equality_and_dignity_are_coequal_constitutional_values, deontological).
narrative_ontology:cs_axiom('054f5a98-c35f-4b8a-9f58-e83711f1d0ce', foundational, speech_can_itself_function_as_subordination_mechanism).
narrative_ontology:cs_axiom_status(speech_can_itself_function_as_subordination_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('054f5a98-c35f-4b8a-9f58-e83711f1d0ce', speech_can_itself_function_as_subordination_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('054f5a98-c35f-4b8a-9f58-e83711f1d0ce', harm_limited_equality_primacy_framework).
narrative_ontology:cs_drift_state('054f5a98-c35f-4b8a-9f58-e83711f1d0ce', contemporary_platform_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('054f5a98-c35f-4b8a-9f58-e83711f1d0ce', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, targeted_minority_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, equality_rights_advocates).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, state_speech_gatekeepers).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, dissident_political_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, unpopular_ideological_minorities).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, satirists_and_provocateurs).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, dignitarian_equality_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, substantive_equality_as_constitutional_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts, tribunals, and administrative bodies apply the harm-limited standard, deciding case by case whether speech crosses into unprotected territory because it materially damages dignity, equality standing, or freedom from harassment. They administer the line, draft the tests, and their discretion determines who gets prosecuted or enjoined. They bear no personal cost from the standard's operation and gain institutional authority and legitimacy from being the arbiter of harm.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_speech_gatekeepers, agenda_setter,
    institutional, generational, analytical, national).

% Historically marginalized groups subjected to hate speech, harassment campaigns, and dehumanizing rhetoric gain a legal mechanism to suppress or penalize speech that degrades their standing and safety. They cannot exit the harms of unregulated speech (they live in the society where it circulates), so the constraint functions as protection they could not otherwise secure through market or social means alone.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, targeted_minority_groups, beneficiary,
    organized, biographical, constrained, national).

% Civil rights organizations, litigators, and scholars who advance the doctrine that equality and dignity are constitutional values co-equal with expression. They gain legal tools, funding, and institutional standing from the doctrine's adoption and expansion, and actively litigate to extend its reach.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, equality_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Speakers whose political criticism, satire, or minority viewpoint is recharacterized as dignitary harm or harassment by an offended target or a risk-averse gatekeeper. They have no forum shopping option — the standard applies wherever they speak nationally — and face civil liability, deplatforming, or criminal exposure for speech that would be protected under a narrower standard.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, dissident_political_speakers, payer,
    powerless, biographical, trapped, national).

% Holders of unpopular, heterodox, or offensive views (religious dissenters, fringe political movements, provocative artists) whose expression is disproportionately captured by harm-based tests because their speech is, by design, discomforting to some audience. They cannot easily predict in advance which of their statements will be deemed harmful, producing a chilling effect on speech that never reaches adjudication.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, unpopular_ideological_minorities, payer,
    powerless, biographical, trapped, national).

% Comedians, cartoonists, and cultural critics who use exaggeration, mockery, and provocation as their craft. Harm-based standards struggle to distinguish satire from genuine harassment, and the ambiguity forces self-censorship or costly legal defense even when their intent was not to degrade but to critique.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, satirists_and_provocateurs, payer,
    moderate, biographical, constrained, national).

% Politicians and movements with sufficient institutional power to shape which harms get recognized may capture the gatekeeping function itself, defining their opponents' rhetoric as harassment while their own comparable rhetoric escapes scrutiny. Their capacity to do this is not visible in the doctrine's text and is not part of the formal debate over the standard, though it structurally shapes enforcement.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, majoritarian_political_actors, excluded,
    powerful, generational, mobile, national).

% Free-speech organizations and scholars who track enforcement patterns, document chilling effects, and testify about the doctrine's abuse potential without themselves being targets. They supply the empirical record used to evaluate whether the standard is applied evenhandedly.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, civil_libertarian_critics, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for weighing expressive freedom against the concrete harms speech can inflict on the dignity, equal standing, and physical/psychological security of targeted individuals and groups — solving the real problem that pure non-interference leaves victims of concerted harassment or dehumanizing rhetoric without recourse.
% TRANSFER_FUNCTION: Moves the burden of proof and the risk of liability from would-be victims of harmful speech onto speakers whose expression touches contested harm categories; transfers discretionary power to adjudicate what counts as harm from speakers and the public to courts and administrative gatekeepers.
% ABSENT_VOICES: Dissident and heterodox speakers whose speech is later deemed harmful rarely have a voice in defining the standard before it is applied to them; majoritarian actors who could weaponize the gatekeeping function against political opponents are not named as a category in the doctrine's justificatory discourse, though their structural capacity to do so is real.
% DISAPPEARANCE_RATIONALE: If the harm-limited boundary vanished overnight, currently-actionable hate speech, harassment campaigns, and dignitary attacks would lose a distinct legal handle (defaulting to whatever a narrower or looser sibling standard governs instead); targeted groups would lose a specific recourse mechanism, while currently-chilled dissident and provocative speech would immediately expand, since the anticipatory self-censorship the doctrine produces would no longer have a legal peg to fear.
% FOUNDING_PROBLEM: Unregulated 'more speech is the remedy' doctrines left members of historically subordinated groups exposed to organized harassment, dehumanizing propaganda, and identity-based intimidation that operated as a practical suppression of their own speech and civic participation, without any legal recognition that expression itself could function as a mechanism of subordination.
% FOUNDING_PROBLEM_CORROBORATION: Equality scholars and international human rights bodies (e.g., comparative constitutional courts in Germany, Canada, and South Africa) attest the founding problem remains live, citing ongoing documented harms from organized hate speech. Civil libertarian organizations and some empirical First Amendment scholars, situated outside the doctrine's direct beneficiary set, attest that the harm categories have proven elastic in practice and are increasingly invoked against political dissent rather than the narrow harassment scenarios that motivated the doctrine, making the founding problem's current instantiation contested rather than settled.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (rising from 0.32) because the harm categories, while narrower than open balancing, are elastic enough in practice that their application has drifted from core harassment scenarios toward broader political and ideological speech — this is the reading's own internal trajectory, not a comparison to the sibling readings. Suppression is authored at 0.62 because the doctrine's operation depends on state adjudication with real liability consequences, which produces measurable chilling effects beyond the cases actually litigated. Accessibility collapse is moderate (0.5): the protected/unprotected line remains contestable and litigated, not fully closed off, distinguishing this from a mountain-type total foreclosure. Resistance is high (0.7) because both civil libertarian critics and dissident speakers actively contest applications of the standard in courts and public discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted minority groups and equality rights advocates are coded as beneficiaries because the doctrine was built to give them a legal handle against organized dehumanization they could not otherwise counter — their exit options are constrained (they cannot simply leave the speech environment) which is precisely the vulnerability the doctrine addresses. Dissident speakers, ideological minorities, and satirists are coded as victims/payers because the same harm categories that protect the first group are the instrument that captures their expression when reinterpreted broadly; their exit options are trapped or constrained because the standard applies nationally and they cannot forum-shop around it. State gatekeepers are the agenda-setters: they administer the line and bear none of its costs directly, gaining institutional authority instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (organized dehumanizing speech functioning as a practical suppression mechanism against subordinated groups) remains partially live — hence 'contested' rather than 'dead' — but the doctrine's application has plausibly outrun its founding scope, capturing political dissent and satire that the founding rationale did not contemplate. This is exactly the mismatch pattern the founding_problem/disappearance_verdict pairing is designed to surface: status is contested (not clearly dead) while disappearance produces world_rearranges (real dependents exist on both sides), which is the honest reading rather than either a clean vindication or a clean debunking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_category_elasticity,
    'Do the dignity/equality/harassment harm categories in this reading have a stable, principled boundary, or do they expand predictably to capture speech beyond their founding scope as gatekeepers accumulate precedent?',
    'Longitudinal tracking of adjudicated cases against the founding-era paradigm cases (organized hate speech, targeted harassment campaigns) to measure category drift toward political dissent, satire, and viewpoint discrimination.',
    'If the categories are stable, this reading functions closer to a genuine tangled rope with a real, bounded coordination function; if they expand predictably, the reading drifts toward snare as the state gatekeeping function increasingly captures speech unrelated to the founding harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_category_elasticity, empirical, 'Whether harm categories under this reading remain bounded or expand to capture disfavored political speech.').

omega_variable(
    gatekeeper_capture_risk,
    'Can the state speech-gatekeeping apparatus created by this reading be captured by majoritarian political actors to suppress minority or dissident viewpoints under the guise of harm prevention?',
    'Comparative analysis of jurisdictions that have adopted harm-limited speech standards, tracking whether enforcement asymmetrically targets politically weaker speakers relative to politically dominant ones making comparable claims.',
    'High capture risk would suggest the reading''s protective function for targeted minorities is structurally unstable and convertible into a tool against different minorities depending on who controls the gatekeeping institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_capture_risk, empirical, 'Whether the gatekeeping function is vulnerable to majoritarian capture against its founding beneficiaries.').

omega_variable(
    reading_choice_as_framing,
    'Is the choice to read the speech protection boundary as harm-limited (rather than absolutist or balancing) itself a contestable framing decision, or does it follow necessarily from prioritizing equality and dignity as co-equal constitutional values?',
    'Comparative constitutional analysis of how different legal systems (US First Amendment jurisprudence vs. German Grundgesetz vs. Canadian Charter) have resolved this framing choice, and whether the choice tracks deeper commitments about the relationship between negative and positive liberty.',
    'If the reading choice is itself a contestable framing rather than a logical entailment, then classifying this constraint as tangled_rope (rather than snare, if capture dominates, or rope, if the coordination function is genuinely clean) depends on priors about which sibling reading is correct — a conceptual dependency this story cannot resolve internally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_choice_as_framing, conceptual, 'Whether the harm-limited framing is a necessary implication of equality-as-value or a contestable jurisprudential choice among several defensible framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__harm_limited_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t8, speech_protection_boundary__harm_limited_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(spee_tr_t16, speech_protection_boundary__harm_limited_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(spee_tr_t24, speech_protection_boundary__harm_limited_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(spee_tr_t32, speech_protection_boundary__harm_limited_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__harm_limited_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__harm_limited_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(spee_be_t8, speech_protection_boundary__harm_limited_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(spee_be_t16, speech_protection_boundary__harm_limited_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(spee_be_t24, speech_protection_boundary__harm_limited_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(spee_be_t32, speech_protection_boundary__harm_limited_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__harm_limited_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__harm_limited_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(spee_su_t8, speech_protection_boundary__harm_limited_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(spee_su_t16, speech_protection_boundary__harm_limited_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(spee_su_t24, speech_protection_boundary__harm_limited_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(spee_su_t32, speech_protection_boundary__harm_limited_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__harm_limited_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__harm_limited_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the speech_protection_boundary kernel, each instantiated as a separate constraint story per the ε-invariance principle: absolutist_reading (near-absolute protection, harm exception limited to Brandenburg imminent-lawless-action standard), balancing_reading (open case-by-case weighing of expressive interests against other constitutional values), and this harm_limited_reading (protection conditional on absence of significant dignity/equality/harassment harm). Each reading produces a different protected/unprotected speech set and different victim/beneficiary structure, hence different ε and classification. They are linked here as a constraint family; contamination or legitimacy shifts in one reading's adoption or judicial reception structurally pressure the others by changing which reading courts and legislatures treat as the operative default.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
