% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Harm-Limitation Reading
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   The harm-limitation reading of First Amendment protection instantiates
 *   one coherent constitutional interpretation within a contested doctrinal
 *   field. This reading holds that First Amendment protection is not
 *   categorical but yields when speech causes demonstrable unconsented-to
 *   harm — establishing a boundary condition that permits regulation of
 *   harmful expression while protecting speech that does not meet the harm
 *   threshold. The reading differs from the absolutist reading (which treats
 *   'no law' as categorical except for narrow historical exclusions like
 *   incitement to imminent violence) and from the categorical-balancing
 *   reading (which treats protected/unprotected categories as products of
 *   ongoing case-by-case balancing of speech value against various state
 *   interests). The harm-limitation reading attempts to provide a limiting
 *   principle: harm is the specific interest that can justify regulation, not
 *   general welfare, morals, economic benefit, or political preference.
 *   Vulnerable minorities are the beneficiaries of this reading — their
 *   access to remedy when targeted by harmful speech expands. Speakers whose
 *   expression causes harm are the victims — their protected speech zone
 *   contracts when causation is established. The extractiveness trajectory
 *   (rising from 0.38 to 0.58 over the interval) reflects category expansion:
 *   early application focuses on narrow, high-confidence harm cases
 *   (defamation, incitement, harassment with clear causation); later
 *   application expands to include psychological injury, dignity harm, and
 *   group-based targeting (where causation becomes harder to establish but
 *   claims expand). The suppression trajectory (rising from 0.40 to 0.52)
 *   reflects the judicial gatekeeping burden rising as harm categories expand
 *   — more speakers face legal exposure, more speech undergoes scrutiny, and
 *   more uncertainty about which expressions will meet the harm threshold.
 *
 * KEY AGENTS:
 *   - Vulnerable Minorities: Primary beneficiary (powerless/trapped) — gain access to harm-based regulation when targeted by expression that causes demonstrable injury
 *   - Speakers Causing Harm: Primary victim (moderate/constrained) — face speech constraints and legal exposure when expression meets harm threshold
 *   - Civil Rights Enforcement Bodies: Secondary beneficiary (institutional/arbitrage) — mandate expands, institutional capacity grows through harm-based speech regulation
 *   - Powerful Speech Producers (Media): Secondary victim (powerful/mobile) — face regulatory pressure but retain arbitrage options across markets and corporate structures
 *   - Courts: Mixed actor (institutional/constrained) — benefit from expanded jurisdiction but constrained by doctrinal precedent and causation requirements
 *   - Speech Maximalist Coalition: Organized opposition (organized/constrained) — view reading as temporary compromise with sunset horizon
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a chosen interpretation as fundamental constraint on rights coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.58).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.52).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Harm-Limitation Reading").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, 'f200dcd3-e501-4203-9ea8-b7f6cd69bbe8').
narrative_ontology:cs_kernel_codification('f200dcd3-e501-4203-9ea8-b7f6cd69bbe8', formalized).
narrative_ontology:cs_authority_grounding('f200dcd3-e501-4203-9ea8-b7f6cd69bbe8', lineage).
narrative_ontology:cs_interpretation_layer_present('f200dcd3-e501-4203-9ea8-b7f6cd69bbe8').
narrative_ontology:cs_reading_relation('f200dcd3-e501-4203-9ea8-b7f6cd69bbe8', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f200dcd3-e501-4203-9ea8-b7f6cd69bbe8', first_amendment_speech_protection__categorical_balancing_reading, influences).
narrative_ontology:cs_axiom('f200dcd3-e501-4203-9ea8-b7f6cd69bbe8', foundational, speech_harm_causation_limiting_principle).
narrative_ontology:cs_axiom_status(speech_harm_causation_limiting_principle, holdable).
narrative_ontology:cs_axiom_grounding('f200dcd3-e501-4203-9ea8-b7f6cd69bbe8', speech_harm_causation_limiting_principle, deontological).
narrative_ontology:cs_axiom('f200dcd3-e501-4203-9ea8-b7f6cd69bbe8', foundational, harm_demonstrability_requirement).
narrative_ontology:cs_axiom_status(harm_demonstrability_requirement, holdable).
narrative_ontology:cs_axiom_grounding('f200dcd3-e501-4203-9ea8-b7f6cd69bbe8', harm_demonstrability_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('f200dcd3-e501-4203-9ea8-b7f6cd69bbe8', harm_demonstrated_regulation_permitted).
narrative_ontology:cs_drift_state('f200dcd3-e501-4203-9ea8-b7f6cd69bbe8', contemporary_harm_category_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f200dcd3-e501-4203-9ea8-b7f6cd69bbe8', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, harm_victims).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speakers_causing_harm).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speech_producers_subject_to_harm_regulation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE MINORITIES TARGETED BY HARMFUL SPEECH (SNARE) — Cannot exit or escape speech directed at them; bear full cost of harm without remedy under absolutist reading. Under harm-limited reading, have access to regulation but enforcement is uncertain and slow. Structural reality: high suppression (fear, self-silencing), high extraction (psychological/material harm), minimal coordination function. The reading offers nominal protection but faces judicial skepticism regarding harm causation burden.
constraint_indexing:constraint_classification(first_amendment_speech_protection__harm_limited_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SPEAKERS SUBJECT TO HARM-BASED REGULATION (TANGLED ROPE) — Face barriers to speech (legal exposure, chilling effect) but also benefit from the regulatory framework's legitimacy and specificity (harm-based, not content-based, in principle). Extraction exists (constrained speech opportunity, litigation cost) but coordination function exists too (harm definition, predictable regulatory standard, alternative speech modes remain available). Neither maximal extraction nor minimal.
constraint_indexing:constraint_classification(first_amendment_speech_protection__harm_limited_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL RIGHTS ENFORCEMENT INSTITUTIONS (ROPE) — Benefit from harm-limited reading's expansion of their mandate and capacity to regulate speech. Experience the constraint as coordination: establishing harm standards, enforcing regulations, enabling vulnerable groups. Net beneficiary with arbitrage options (institutional flexibility in enforcement approach). Low extraction experience — they see their power expanding through legitimate rights-enforcement.
constraint_indexing:constraint_classification(first_amendment_speech_protection__harm_limited_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POWERFUL SPEECH PRODUCERS (SNARE) — Despite mobile exit options (corporate restructuring, market-based speech platforms), face regulatory pressure and reputational harm under harm-limited reading. Extraction mechanism: regulatory cost and attention. However, power level and mobility moderate experienced extraction — they can exit to different markets, use corporate structures for insulation, or engage in arbitrage across jurisdictions. Classification reflects their structural capacity to bear regulation, not immobilization.
constraint_indexing:constraint_classification(first_amendment_speech_protection__harm_limited_reading, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL SYSTEM ADMINISTERING HARM-BASED STANDARD (TANGLED ROPE) — Must define 'demonstrable harm,' adjudicate causation, balance speech interests against injury. Coordination function: creating predictable legal standard, enabling rights-enforcement. Extraction function: judicial gatekeeping determines who can claim harm and under what conditions. Courts benefit from expanded jurisdiction (institutional capacity growth) but constrained by precedent and jurisdictional principles. Mixed beneficiary and victim.
constraint_indexing:constraint_classification(first_amendment_speech_protection__harm_limited_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SPEECH MAXIMALIST COALITION (SCAFFOLD) — Organized advocates for broad speech protection see harm-limitation as a temporary accommodation (sunset: when harm standards are sufficiently precise and consistently applied, they expect pushback). View the reading as a transitory compromise that will either solidify into doctrine or collapse under its own contradictions (when harm becomes too broadly interpreted). Low theater ratio for this perspective — they see clear exit path through constitutional amendment or doctrinal reversal.
constraint_indexing:constraint_classification(first_amendment_speech_protection__harm_limited_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW PERSPECTIVE (MOUNTAIN) — From a civilizational/universal view, harm-prevention is a fundamental constraint on any rights claim — no right can be absolute when its exercise causes severe unconsented-to injury. This appears as a brute fact about rights coherence. However, the structural data contradicts this: the harm-limitation reading is a chosen interpretive strategy grounded in political commitment, not a natural law. The false-summit signal reveals how rights frameworks naturalize contingent boundaries.
constraint_indexing:constraint_classification(first_amendment_speech_protection__harm_limited_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(first_amendment_speech_protection__harm_limited_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(first_amendment_speech_protection__harm_limited_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising. The harm-limitation reading creates a regulatory framework that expands the justification for limiting speech. Base extractiveness starts at 0.38 (early application to clear-causation cases like defamation) and rises to 0.58 (expansion to psychological harm, dignity harm, group targeting where causation is contested and categories broaden). The trajectory reflects not a change in the reading's structure but a change in how it is applied — as courts interpret 'harm' more expansively, more speech is regulated, and more speakers experience constraint. This is not the constraint becoming more extractive in principle but becoming more extractive in practice as its boundary conditions blur. Suppression (0.52): Moderate-high, stable. The reading requires active judicial enforcement (courts must determine harm, causation, and scope). Speakers face legal uncertainty — what counts as harm is partially codified but partially context-dependent. The suppression trajectory (rising from 0.40 to 0.52) reflects increasing legal exposure as harm categories expand. Theater ratio (0.48): Moderate, declining. Early harm-limitation application (clear defamation, straightforward causation) has lower theater — the regulatory logic is transparent. As harm categories expand and causation becomes harder to establish, more of the regulation becomes performative (appearing to regulate harm while actually gatekeeping based on category assumptions, speaker identity, or viewpoint proximity). The declining trajectory (0.55 → 0.48) suggests that the reading's functional clarity is modest and stable — it is neither highly performative (like piton) nor highly functional (like rope), but genuinely mixed.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits perspectival divergence across agent positions. Vulnerable minorities see the harm-limitation reading as a tangled_rope or nascent rope — it coordinates access to remedy and establishes their harms as legally cognizable, but enforcement is uncertain and slow (constrained exit, bounded remedy). Speakers subject to harm regulation see a tangled_rope or snare — their speech is constrained, legal exposure increases, but they retain alternative speech modes (constrained rather than trapped). Powerful speech producers see modest regulation (snare classification reflects their power to absorb regulatory cost, not immobilization). Civil rights institutions see a rope or scaffold — the reading expands their mandate and provides clear coordination function (establishing harm standards). Courts see a tangled_rope — they benefit from doctrinal clarity but are constrained by precedent and evidentiary requirements. The analytical observer risks seeing a mountain (harm-prevention as fundamental constraint) when the reading is actually a chosen political commitment. The perspectival gap is structurally healthy: no single type dominates, and the variance reflects genuine disagreement about what the harm-limitation reading accomplishes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from structural relationship to the constraint: beneficiary vs victim status, power level, and exit options. Vulnerable minorities are beneficiaries with trapped exit (no way to avoid targeted speech) — this produces low d → negative χ from their perspective, meaning the constraint relieves rather than extracts from them. Speakers subject to harm regulation are victims with constrained exit (can change expression but at cost) — this produces higher d (0.55–0.65 range) → moderate χ. Powerful speech producers are victims but with mobile exit — this produces lower d due to arbitrage capacity, yielding moderate extraction despite victim status. Civil rights institutions are beneficiaries with arbitrage options — low d, negative χ. Courts are mixed (both beneficiary and victim) with constrained exit — middle-range d. The beneficiary/victim declarations drive these computations; the engine derives d and applies the sigmoid f(d) = -0.20 + 1.70/(1+e^(-6*(d-0.50))). The tangled_rope classification emerges from the conjunction of genuine beneficiary group (vulnerable minorities), genuine victim group (speakers), and active enforcement requirement — all three gates satisfied.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: This constraint exhibits tension between coordination function and extraction function that cannot be resolved within a single classification. The harm-limitation reading DOES coordinate: it establishes a legal standard (harm must be demonstrable, not speculative), creates procedural rules (who can claim, what burden of proof), and enables a previously powerless group (targeted minorities) to seek remedy. But it ALSO extracts: it constrains speakers, gatekeeps expression based on harm judgments, and shifts regulatory power to enforcement institutions. The tangled_rope classification acknowledges both functions and the tension between them. The mandatrophy is resolved by noting that the tension IS the reading's fundamental structure — the reading cannot eliminate extraction while preserving coordination function because the coordination function (establishing who gets regulated) IS the extraction mechanism. The reading attempts to make extraction legitimate by grounding it in a limiting principle (harm, not viewpoint; causation, not mere potential). Whether extraction is actually limited depends on how harm categories expand and how causation thresholds are applied. The omega variables (particularly 'harm_causation_threshold' and 'speech_harm_category_expansion') directly address whether the limiting principle holds in practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_causation_threshold,
    'What causal and evidentiary threshold establishes ''demonstrable unconsented-to harm'' in speech contexts?',
    'Empirical analysis of judicial harm determinations; comparison of standards across jurisdictions; assessment of false-positive rates (speech regulated when causation is speculative) vs false-negative rates (harm-causing speech protected due to causation burden)',
    'If threshold is high (near-absolute causation required): reading collapses toward absolutist. If threshold is low (plausible harm suffices): reading expands toward categorical balancing. Classification moves between tangled_rope, snare, and rope depending on effective threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_causation_threshold, empirical, 'Evidentiary threshold for establishing demonstrable harm in speech regulation').

omega_variable(
    speech_harm_category_expansion,
    'Does ''harm'' category remain bounded (physical injury, economic loss, direct defamation) or expands to include psychological injury, dignity harm, group reputation, systemic exclusion?',
    'Historical case-law analysis tracking category expansion; comparison with how harm is defined in tort, criminal, and civil-rights law; assessment of whether expansion tracks evidence of causal harm or reflects value commitments',
    'If bounded: harm-limitation reading remains workable coordination mechanism (Rope/Tangled Rope). If expanded: boundary becomes performative theater (Piton); regulation becomes content-disguised as harm (Snare); or reading becomes substantively indistinguishable from categorical balancing (collapses into sibling reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speech_harm_category_expansion, conceptual, 'Scope of speech-related harm included in regulation').

omega_variable(
    consent_and_unavoidability,
    'What conditions establish ''unconsented-to'' harm for speech that is geographically or informationally unavoidable (public speech, internet reach, group identity targeting)?',
    'Doctrinal analysis of consent standards; assessment of whether avoidance is practically possible; comparison with duty-of-care standards in adjacent law (privacy, defamation, harassment)',
    'If consent is narrowly defined (explicit opt-in): few speech acts count as unconsented-to harm. If consent is broadly inferred (default exclusion unless speaker accommodates): most targeted speech counts. Narrow consent pushes reading toward absolutist (more speech protected); broad consent pushes toward categorical balancing (more speech regulated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_and_unavoidability, conceptual, 'Definition of ''unconsented-to'' in speech harm contexts').

omega_variable(
    kernel_reading_ambiguity,
    'Is ''demonstrable unconsented-to harm'' a limiting principle on First Amendment protection, or a framework for rebalancing protected categories?',
    'Textual analysis of how harm-limitation is applied: (a) as a hard boundary (certain harm classes always justify regulation), (b) as a rebalancing factor (harm enters into case-by-case balancing), or (c) as a categorical reclassification (harm-causing speech moves from protected to unprotected). Real-world application determines which reading the constraint instantiates.',
    'If (a): maintains structural independence from categorical balancing; harm-limitation reading is coherent. If (b) or (c): reading collapses into categorical balancing sibling; the ''harm-limited'' label becomes misdescriptive. Classification shifts from tangled_rope to either rope (if harm is treated as coordinate factor) or snare (if harm overrides other factors).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether harm-limitation is a limiting principle or a rebalancing framework').

omega_variable(
    alternative_remedial_adequacy,
    'Do non-speech remedies (damages, injunctions, institutional accommodation) adequately address speech-caused harm, or does preventing the speech itself become necessary?',
    'Empirical assessment of remedy effectiveness: do damage awards reverse harm? Do injunctions prevent future harm? Do institutional changes (accessibility, moderation, counter-speech) reduce impact? Comparison across remedy types.',
    'If remedial: harm-limitation reading can remain focused on genuine coordination (who pays, how is harm addressed); extraction component is lower. If non-remedial (harm is irremediable): reading must include speech prevention as primary mechanism; extraction component rises (speakers bear full prevention burden); reading drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_remedial_adequacy, empirical, 'Adequacy of non-speech remedies for addressing speech harm').

omega_variable(
    institutional_power_asymmetry,
    'How do power asymmetries between regulated speakers and harm victims affect the coherence of harm-limitation as a neutral standard?',
    'Structural analysis of who has capacity to litigate harm claims, who can afford speech regulations, whose harms are judicially recognizable. Assessment of whether harm-limitation standard systematically favors powerful speakers or powerful targets.',
    'If asymmetric toward powerful speakers: reading becomes a snare for vulnerable speakers (high suppression, high extraction). If asymmetric toward vulnerable targets: reading becomes primarily a rope for civil rights (coordination function dominates). Measurement of realized power distribution determines actual classification vs nominal tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_power_asymmetry, empirical, 'Power asymmetries between speakers and harm claimants').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fa_harm_theater_t0, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(fa_harm_theater_t5, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(fa_harm_theater_t10, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(fa_harm_extract_t0, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fa_harm_extract_t5, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fa_harm_extract_t10, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fa_harm_supp_t0, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fa_harm_supp_t5, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(fa_harm_supp_t10, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% The harm_limited_reading is one of three structurally distinct readings of the first_amendment_speech_protection kernel. The absolutist_reading and categorical_balancing_reading are separate constraints with different epsilon values, different beneficiary/victim structures, and different perspectival profiles. All three share the same textual grounding (First Amendment) but interpret 'speech protection' and 'permissible regulation' differently. The harm_limited_reading (ε≈0.58, tangled_rope) establishes harm-causation as the limiting principle; the absolutist_reading (ε≈0.15, mountain/rope) treats protection as categorical; the categorical_balancing_reading (ε≈0.48, tangled_rope/rope) treats protection as emerging from ongoing balancing. Network edges represent mutual influence: if the harm-limitation standard is applied expansively (harm categories broaden), the absolutist position becomes structurally harder to maintain; if the absolutist position gains institutional power, harm-limitation claims face higher evidentiary burdens.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__harm_limited_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
