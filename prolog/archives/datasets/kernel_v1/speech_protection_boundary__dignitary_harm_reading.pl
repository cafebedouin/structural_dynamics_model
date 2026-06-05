% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__dignitary_harm_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__dignitary_harm_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_protection_boundary__dignitary_harm_reading
 *   human_readable: Speech Restriction on Dignitary Harm Grounds (Contested Reading)
 *   domain: constitutional_law/free_speech/dignitary_protection
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested
 *   speech-protection-boundary kernel — the dignitary harm reading. This
 *   reading asserts that speech causing tangible dignitary harm (bigotry,
 *   harassment constructing systemic oppression) can be legitimately
 *   restricted when the harm is demonstrable and non-speculative. The
 *   dignitary harm reading is in active dispute with two sibling readings:
 *   the near-absolutist reading (which treats 'no law' as categorical except
 *   for imminent physical harm) and the balancing reading (which treats
 *   speech protection and competing interests as requiring case-by-case
 *   weighing). This constraint represents the dignitary harm reading's
 *   structural instantiation — how the doctrine operates in practice when
 *   this reading is applied by adjudicators and institutions. The constraint
 *   is a tangled rope: it coordinates protection of vulnerable minorities
 *   against epistemic injustice (genuine coordination function) while
 *   simultaneously extracting from speakers whose expression is restricted on
 *   dignitary grounds (asymmetric extraction). The extractiveness has
 *   increased over the interval (0.30 → 0.52) as the dignitary harm threshold
 *   has broadened, reducing predictability and increasing speaker chilling
 *   effect. The suppression requirement has also increased (0.35 → 0.58) as
 *   enforcement of dignitary harm standards has required more active
 *   institutional capacity. Theater ratio is low (0.34) because dignitary
 *   harm adjudication involves real substantive judgment about epistemic
 *   harms and systemic impact, not mere performative ritual — though as the
 *   omega on threshold demarcation notes, the standard's opacity introduces
 *   performative elements when adjudicators apply it flexibly.
 *
 * KEY AGENTS:
 *   - Vulnerable Minority Groups: Primary beneficiary (powerless/trapped) — protected from epistemic injustice and systemic bigotry; bear zero extraction cost in this reading
 *   - Speakers Restricted on Dignitary Grounds: Primary victim (varies by speaker position; moderate-to-powerful) — bear extraction cost through speech restriction and chilling effect
 *   - Anti-Bigotry Enforcement Coalition: Secondary beneficiary (institutional/arbitrage) — coordinate protection of equality norms; benefit from doctrine legitimating dignitary harm restrictions
 *   - Speech Protection Doctrine: Institutional artifact (institutional/constrained) — both benefits from legitimation through dignitary harm framing and bears extraction through reduced predictability
 *   - Adjudicators and Courts: Enforcement agents (institutional/constrained) — required to operationalize vague dignitary harm threshold; bear cost of complexity and potential legitimacy challenges
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the dignitary harm reading as immutable principle rather than contingent judicial interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__dignitary_harm_reading, 0.52).
domain_priors:suppression_score(speech_protection_boundary__dignitary_harm_reading, 0.58).
domain_priors:theater_ratio(speech_protection_boundary__dignitary_harm_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__dignitary_harm_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(speech_protection_boundary__dignitary_harm_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__dignitary_harm_reading, theater_ratio, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__dignitary_harm_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__dignitary_harm_reading, "Speech Restriction on Dignitary Harm Grounds (Contested Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__dignitary_harm_reading, "constitutional_law/free_speech/dignitary_protection").

domain_priors:requires_active_enforcement(speech_protection_boundary__dignitary_harm_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__dignitary_harm_reading, 'ad83af2e-7265-4def-a495-9fcab042881b').
narrative_ontology:cs_kernel_codification('ad83af2e-7265-4def-a495-9fcab042881b', formalized).
narrative_ontology:cs_authority_grounding('ad83af2e-7265-4def-a495-9fcab042881b', lineage).
narrative_ontology:cs_interpretation_layer_present('ad83af2e-7265-4def-a495-9fcab042881b').
narrative_ontology:cs_reading_relation('ad83af2e-7265-4def-a495-9fcab042881b', speech_protection_boundary__near_absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('ad83af2e-7265-4def-a495-9fcab042881b', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('ad83af2e-7265-4def-a495-9fcab042881b', foundational, demonstrable_dignitary_harm_as_sufficient_restriction_ground).
narrative_ontology:cs_axiom_status(demonstrable_dignitary_harm_as_sufficient_restriction_ground, holdable).
narrative_ontology:cs_axiom_grounding('ad83af2e-7265-4def-a495-9fcab042881b', demonstrable_dignitary_harm_as_sufficient_restriction_ground, deontological).
narrative_ontology:cs_axiom('ad83af2e-7265-4def-a495-9fcab042881b', foundational, epistemic_injustice_as_cognizable_harm).
narrative_ontology:cs_axiom_status(epistemic_injustice_as_cognizable_harm, holdable).
narrative_ontology:cs_axiom_grounding('ad83af2e-7265-4def-a495-9fcab042881b', epistemic_injustice_as_cognizable_harm, deontological).
narrative_ontology:cs_reference_frame('ad83af2e-7265-4def-a495-9fcab042881b', dignitary_harm_as_restriction_ground).
narrative_ontology:cs_drift_state('ad83af2e-7265-4def-a495-9fcab042881b', contemporary_anti_discrimination_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ad83af2e-7265-4def-a495-9fcab042881b', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__dignitary_harm_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__dignitary_harm_reading, vulnerable_minority_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__dignitary_harm_reading, targets_of_systemic_bigotry).
narrative_ontology:constraint_victim(speech_protection_boundary__dignitary_harm_reading, speakers_restricted_on_dignitary_grounds).
narrative_ontology:constraint_victim(speech_protection_boundary__dignitary_harm_reading, speech_protection_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED MINORITY (SNARE) — Trapped in epistemic injustice: speech constructing systemic oppression creates cumulative dignitary harm with no exit option. The targeted group bears full extraction cost (dignity loss, epistemic credibility damage, material consequences of stereotype activation). From this position, restriction of harmful speech appears as legitimate protection, not suppression. Zero exit capacity; maximum experienced harm.
constraint_indexing:constraint_classification(speech_protection_boundary__dignitary_harm_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SPEECH ADVOCATE (TANGLED ROPE) — Constrained by institutional role and career risk of opposing minority protection. Benefits from speech protection doctrine (institutional function) while also bearing extraction cost (complicity in epistemic injustice, erosion of doctrine legitimacy). Mixed position: coordination of speech freedom exists alongside extraction of minority groups. Moderate power with constrained exit — can shift institutional position but at reputational cost.
constraint_indexing:constraint_classification(speech_protection_boundary__dignitary_harm_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANTI-BIGOTRY COALITION (ROPE) — Benefits from dignitary harm reading: coordination mechanism for protecting equality norms without fully collapsing speech rights. Views restriction as solving genuine collective action problem (how to coordinate against systemic bigotry). Arbitrage position: can exit to alternative jurisdictions or to pure near-absolutist reading if enforcement costs rise. Experiences constraint as predominantly coordinating, not extractive.
constraint_indexing:constraint_classification(speech_protection_boundary__dignitary_harm_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MAJORITY-POSITION SPEAKER (SNARE) — Despite high power and mobility, experiences the constraint as extractive when dignitary harm standard is applied to majoritarian speech. Paradox: powerful speaker can exit locally (jurisdiction-shop) but cannot exit the constraint's reach at scale. High power does not prevent classification as snare because the suppression mechanism (dignitary harm threshold) is epistemically opaque — speaker cannot know in advance whether speech will be classified as harmful. Uncertainty + power concentration in adjudicators = snare from this perspective.
constraint_indexing:constraint_classification(speech_protection_boundary__dignitary_harm_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SPEECH PROTECTION DOCTRINE (TANGLED ROPE) — The doctrine itself is both beneficiary and victim. Benefits from dignitary harm reading because the reading reframes speech protection as conditional on harm levels rather than absolute — this legitimates the doctrine's institutional role (balancing authority). But the doctrine also bears extraction: if dignitary harm standard is applied flexibly, the doctrine's predictability erodes, reducing its institutional coordination function. Trapped in its own legitimacy crisis.
constraint_indexing:constraint_classification(speech_protection_boundary__dignitary_harm_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, some readings treat free speech as a natural law with no exception for dignitary harm — 'no law' means no law, period. This perspective sees the dignitary harm reading as a violation of an immutable principle, not as legitimate balancing. However, this perspective is CONTESTED: the false summit detection engine will flag this as naturalization of a contingent judicial reading, not a discovered natural law.
constraint_indexing:constraint_classification(speech_protection_boundary__dignitary_harm_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__dignitary_harm_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speech_protection_boundary__dignitary_harm_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speech_protection_boundary__dignitary_harm_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__dignitary_harm_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__dignitary_harm_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The dignitary harm reading creates real extraction from speakers whose expression is restricted: lost communication capacity, reduced expressive freedom, chilling effect on similar speech. However, the extraction is justified by legitimate protection of vulnerable minorities from epistemic injustice — it is not pure rent-seeking. The value reflects that genuine coordination (protection of equality and dignity) coexists with real restriction of speaker freedom. The trajectory (0.30 → 0.52) shows increasing extraction as the dignitary harm threshold has expanded and judicial application has become more aggressive, increasing speaker uncertainty and deterrence. Suppression (0.58): Moderate-high. The constraint operates through suppression because speakers cannot ex-ante predict whether their speech will be classified as causing demonstrable dignitary harm — the standard is epistemically opaque and jurisdiction-dependent. This creates conformity pressure and self-censorship (suppression of alternatives). However, suppression is not total because speakers can engage in careful framing, context-provision, and engagement with the harm threshold rather than complete silence. Theater ratio (0.34): Low. Dignitary harm adjudication involves substantive judgment about epistemic harms, systemic impact, and speaker intent rather than mere performative ritual. Courts and institutions must engage with the actual content and consequences of speech, not just ceremonial review. The low theater reflects that this reading prioritizes real substantive protection over procedural theater — though the omega on threshold demarcation notes that opacity in applying the standard introduces some performative elements.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental disagreement about what the speech protection boundary means. The targeted minority sees snare (pure extraction of epistemic injustice with no exit). The speech advocate sees tangled rope (coordination of dignity with extraction of free expression). The anti-bigotry coalition sees rope (coordination of equality protection). The powerful speaker sees snare (discretionary suppression). The doctrine itself sees tangled rope (legitimated but predictably eroded). The analytical observer risks seeing mountain (immutable principle of free speech) but this is a false summit — the dignitary harm reading is a contingent institutional interpretation, not a discovered natural law. The perspectival gaps between these positions reveal that the kernel contest is structural, not merely political. The near-absolutist reading and the dignitary harm reading are not compatible within any single framework: one says 'no law,' the other says 'law restricting demonstrable dignitary harm.' This suggests a FORECLOSES relation rather than COEXISTS_WITH, unless we treat the readings as held by genuinely separate institutional authorities with no unified jurisdiction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d value) is derived from the agent's structural relationship to the constraint and their exit options. Vulnerable minorities are full beneficiaries with no exit (d ≈ 0.05) — they benefit from dignitary harm protection and cannot escape its protections. Speakers restricted on dignitary grounds are full targets: if they have mobile exit (powerful actor who can relocate or shift platforms), d ≈ 0.85 (high experienced extraction); if they have constrained exit (moderate speaker), d ≈ 0.65. The speech protection doctrine is trapped between its own coordination function and its erosion: beneficiary status (legitimated by dignitary harm framing) and victim status (reduced predictability) exist simultaneously, yielding d ≈ 0.50. Anti-bigotry institutions are beneficiaries with arbitrage exit: d ≈ 0.15. The analytical observer's potential false-summit classification would rest on d ≈ 0.72, treating the reading as a powerless agent with analytical constraints rather than as a defensible institutional position. The perspectival gaps reveal that this reading is not false-summit territory — the constraint genuinely coordinates dignitary protection alongside restriction, making tangled_rope the correct classification rather than mountain.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignitary_harm_threshold_demarcation,
    'What operationally distinguishes demonstrable dignitary harm from speculative or attenuated offense?',
    'Longitudinal analysis of court decisions applying dignitary harm standard; identification of consistent criteria (measurable stereotype activation, documented discriminatory outcomes, epistemic credibility loss); comparison across jurisdictions with different thresholds',
    'If threshold is coherent and consistently applied: constraint functions as rule-based coordination (lower snare risk). If threshold is vague or jurisdiction-dependent: constraint functions as discretionary extraction (higher snare risk, speech chilling effect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignitary_harm_threshold_demarcation, empirical, 'Operationalization of ''demonstrable'' dignitary harm threshold').

omega_variable(
    epistemic_injustice_scope_ambiguity,
    'Does dignitary harm standard protect against epistemic injustice (credibility damage, silencing) or only against material discrimination?',
    'Discourse analysis of court opinions: do they recognize epistemic harms (testimonial injustice, hermeneutical injustice)? Comparison of cognizable vs non-cognizable harms in case law; analysis of whether pure credibility damage (absent material consequence) triggers restriction.',
    'If epistemic harms are recognized: constraint scope expands significantly (more speech restricted, broader victim protection). If only material discrimination: constraint scope is narrower (coordination focus). Determines whether this reading FORECLOSES or COEXISTS_WITH balancing_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_injustice_scope_ambiguity, conceptual, 'Whether dignitary harm includes epistemic injustice or only material discrimination').

omega_variable(
    vulnerable_minority_identification,
    'Which groups qualify as ''vulnerable minorities'' subject to dignitary harm protection? Is vulnerability tied to historical subordination, current structural inequality, or epistemic status?',
    'Comparative constitutional law analysis across jurisdictions (Canada, South Africa, EU); identification of criteria for group vulnerability; test case analysis of groups at the boundary (religious minorities, political minorities, occupational groups)',
    'If vulnerability is narrowly defined (only historically subordinated racial/ethnic groups): constraint applies to a discrete set of speakers/topics, reducing chilling effect. If vulnerability is expansively defined: constraint applies broadly, increasing speaker uncertainty and chilling effect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vulnerable_minority_identification, conceptual, 'Definition of ''vulnerable minority'' for dignitary harm protection').

omega_variable(
    systemic_oppression_vs_incidental_harm,
    'Does dignitary harm threshold require the speech to be part of a systematic oppression pattern, or is single-instance dignitary harm sufficient for restriction?',
    'Jurisprudential analysis: do courts look for systemic patterns or single incidents? Analysis of whether repeated offenses accumulate or standalone incidents trigger restriction; comparison of doctrinal approaches in balancing_reading vs dignitary_harm_reading',
    'If systemic pattern required: constraint is narrower, more predictable, less chilling. If single instance sufficient: constraint is broader, less predictable, higher speaker uncertainty. Affects whether this reading COEXISTS_WITH balancing_reading (both can exist) or INFLUENCES it (dignitary standard changes adjudication patterns).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_oppression_vs_incidental_harm, empirical, 'Whether systemic oppression pattern is required or single incident suffices for restriction').

omega_variable(
    near_absolutist_rebuttal_mechanism,
    'Can the near_absolutist_reading be rebutted once dignitary harm is demonstrated and accepted as a legitimate restriction ground, or does it foreclose this reading entirely?',
    'Jurisprudential meta-analysis: does accepting dignitary harm as a restriction ground logically entail abandoning near-absolutist first principles? Can a reading simultaneously hold ''no law'' absolutism and dignitary harm exceptions? Examination of whether these positions are genuinely incompatible or merely politically opposed.',
    'If positions are logically incompatible: FORECLOSES relation to near_absolutist_reading. If positions are politically opposed but logically compatible: COEXISTS_WITH. This determines the engine''s computation of path foreclosure in the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(near_absolutist_rebuttal_mechanism, conceptual, 'Logical compatibility of dignitary harm reading and near-absolutist reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__dignitary_harm_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_dign_tr_t0, speech_protection_boundary__dignitary_harm_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(speech_dign_tr_t10, speech_protection_boundary__dignitary_harm_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(speech_dign_tr_t20, speech_protection_boundary__dignitary_harm_reading, theater_ratio, 20, 0.34).

% Extraction over time
narrative_ontology:measurement(speech_dign_be_t0, speech_protection_boundary__dignitary_harm_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(speech_dign_be_t10, speech_protection_boundary__dignitary_harm_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(speech_dign_be_t20, speech_protection_boundary__dignitary_harm_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(speech_dign_su_t0, speech_protection_boundary__dignitary_harm_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(speech_dign_su_t10, speech_protection_boundary__dignitary_harm_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(speech_dign_su_t20, speech_protection_boundary__dignitary_harm_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__dignitary_harm_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__dignitary_harm_reading, speech_protection_boundary__balancing_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__dignitary_harm_reading, speech_protection_boundary__near_absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__dignitary_harm_reading, epistemic_injustice_silencing_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__dignitary_harm_reading, hate_speech_doctrine_enforcement).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary is a contested kernel with three distinct readings. This file represents the dignitary harm reading. The near_absolutist reading and balancing reading are separate constraint stories with different ε values, different victim/beneficiary structures, and different perspectives. All three readings affect the same downstream constraints (epistemic injustice, hate speech enforcement) but through different mechanisms. The readings are linked bidirectionally in network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
