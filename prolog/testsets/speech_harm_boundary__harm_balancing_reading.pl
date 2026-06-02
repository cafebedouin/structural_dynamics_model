% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Protection with Harm-Balancing Restriction (Constitutional Reading)
 *   domain: constitutional_law/free_speech/proportionality_doctrine
 *
 * SUMMARY:
 *   This constraint instantiates the HARM-BALANCING READING of the
 *   speech-harm boundary kernel — one of three live alternative readings in
 *   constitutional law. Under this reading, speech receives presumptive
 *   protection, but that protection yields when an agent demonstrates that
 *   speech causes concrete, measurable harm exceeding some threshold. The
 *   reading establishes a proportionality doctrine: restrictions are
 *   justified only when harm is demonstrated AND the restriction is
 *   proportional to the harm. This reading is distinct from the absolutist
 *   reading (which rejects all harm-based restriction) and the dignity
 *   reading (which permits restriction for dignitary harm and group libel
 *   without requiring empirical harm demonstration). The harm-balancing
 *   reading occupies a moderate position: narrower than the dignity reading's
 *   protected categories, broader than absolutism's near-universal
 *   protection. The constraint exhibits Tangled Rope structure: it provides a
 *   genuine coordination function (the proportionality doctrine offers
 *   predictable framework for settlement) while also extracting costs from
 *   speakers whose speech crosses the harm threshold and from marginalized
 *   groups who must bear burden of proving harm. The extractiveness has
 *   increased over time (0.38 → 0.52) as the burden-of-proof mechanism has
 *   clarified and as enforcement institutions have developed more
 *   sophisticated means of harm measurement, making the doctrine more
 *   operationally demanding for marginalized groups to activate.
 *
 * KEY AGENTS:
 *   - Marginalized groups subject to hate speech: Primary victims (powerless/trapped) — face exposure to speech and bear epistemic burden to prove harm sufficient for restriction
 *   - Protected speakers (general category): Primary beneficiaries (institutional/arbitrage) — receive baseline speech protection under presumption that benefits from predictable legal framework
 *   - Restricted speakers (demonstrable harm case): Secondary actors (moderate/constrained) — face restriction costs when harm is demonstrated; also benefit from transparency of harm-balancing threshold
 *   - State enforcement institutions (courts, administrative bodies): Institutional beneficiaries (institutional/arbitrage) — benefit from doctrine's coordination function providing legitimate framework for decision-making
 *   - Powerful speakers with institutional platforms: Secondary beneficiaries (powerful/mobile) — have resources to contest harm determinations and navigate legal process; benefit from protection while bearing modest restriction costs
 *   - Civil society advocacy coalition: Organized agents (organized/constrained) — monitor doctrine implementation and push for clearer shared harm standards through public deliberation
 *   - Legal doctrine itself (as institution): Piton perspective — harm-balancing formulae maintained as performative ritual; actual outcomes increasingly driven by political composition of enforcement bodies rather than by doctrine
 *   - Analytical observer: Civilizational perspective — risks naturalizing contingent institutional arrangement (specific balance point, burden-of-proof placement) as universal feature of all communication systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.52).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.48).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Protection with Harm-Balancing Restriction (Constitutional Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/free_speech/proportionality_doctrine").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, 'reading_harm_balancing_kernel_speech_harm_boundary').
narrative_ontology:cs_kernel_codification('reading_harm_balancing_kernel_speech_harm_boundary', fixed_text).
narrative_ontology:cs_authority_grounding('reading_harm_balancing_kernel_speech_harm_boundary', lineage).
narrative_ontology:cs_interpretation_layer_present('reading_harm_balancing_kernel_speech_harm_boundary').
narrative_ontology:cs_reading_relation('reading_harm_balancing_kernel_speech_harm_boundary', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('reading_harm_balancing_kernel_speech_harm_boundary', speech_harm_boundary__dignity_reading, influences).
narrative_ontology:cs_axiom('reading_harm_balancing_kernel_speech_harm_boundary', foundational, demonstrated_harm_justifies_restriction).
narrative_ontology:cs_axiom_status(demonstrated_harm_justifies_restriction, holdable).
narrative_ontology:cs_axiom_grounding('reading_harm_balancing_kernel_speech_harm_boundary', demonstrated_harm_justifies_restriction, empirically_contingent).
narrative_ontology:cs_axiom('reading_harm_balancing_kernel_speech_harm_boundary', foundational, proportionality_limits_restriction_scope).
narrative_ontology:cs_axiom_status(proportionality_limits_restriction_scope, holdable).
narrative_ontology:cs_axiom_grounding('reading_harm_balancing_kernel_speech_harm_boundary', proportionality_limits_restriction_scope, deontological).
narrative_ontology:cs_reference_frame('reading_harm_balancing_kernel_speech_harm_boundary', liberal_constitutional_speech_baseline).
narrative_ontology:cs_drift_state('reading_harm_balancing_kernel_speech_harm_boundary', contemporary_era_of_epistemic_fragmentation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('reading_harm_balancing_kernel_speech_harm_boundary', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, protected_speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, state_enforcement_capacity).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_of_harmful_content).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, marginalized_groups_subject_to_hate_speech).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED GROUP SUBJECT TO HATE SPEECH (SNARE) — Trapped by both the speech that targets them AND by the requirement to prove 'demonstrated harm' to trigger restriction. No exit from exposure to speech; burden of proof shifts extraction toward the target. High extraction, suppression through epistemic barriers to harm demonstration.
constraint_indexing:constraint_classification(speech_harm_boundary__harm_balancing_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RESTRICTED SPEAKER FACING DEMONSTRABLE HARM THRESHOLD (TANGLED ROPE) — Faces real restriction costs when harm is demonstrated, but also benefits from the coordination function of proportionality doctrine: clear (if demanding) rules about when restriction is legitimate. Not maximally trapped — has exit path through legal process and burden of proof. Experiences both coordination (transparent balancing) and extraction (restriction when threshold met).
constraint_indexing:constraint_classification(speech_harm_boundary__harm_balancing_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ENFORCEMENT INSTITUTION (ROPE) — Benefits from clear proportionality doctrine that enables consistent application and public legitimacy. Sees the constraint as coordination mechanism: harm-balancing provides institutional framework for settling speech disputes without total prohibition or total permission. Arbitrage exit — can switch enforcement standards if doctrine changes.
constraint_indexing:constraint_classification(speech_harm_boundary__harm_balancing_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POWERFUL SPEAKER WITH INSTITUTIONAL PLATFORM (TANGLED ROPE) — Has resources to contest harm determinations and navigate legal process; also benefits from baseline speech protection. Experiences the constraint as mixed: gains coordination benefit of predictable legal framework but bears modest restriction costs when harm is demonstrated. Mobile exit — can relocate speech to jurisdictions with weaker harm-balancing doctrine.
constraint_indexing:constraint_classification(speech_harm_boundary__harm_balancing_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZED CIVIL SOCIETY / ADVOCACY COALITION (SCAFFOLD) — Sees harm-balancing doctrine as a temporary coordination solution with sunset logic: as social norms evolve and shared epistemic standards for 'harm' mature, the need for judicial intervention declines. Organized agents pushing for clearer harm standards and public deliberation. Constraints enforcement through social scrutiny rather than legal restriction.
constraint_indexing:constraint_classification(speech_harm_boundary__harm_balancing_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FORMAL LEGAL DOCTRINE AS INSTITUTIONAL ACTOR (PITON) — The harm-balancing framework itself has become largely performative: courts apply harm-balancing formulae as ritual (the 'proportionality test'), but actual outcomes depend more on political composition of the bench and public sentiment than on the formal doctrine. Theater ratio is moderate-high because doctrine provides appearance of rational constraint on political speech enforcement, but the doctrine is honored in form while outcomes are driven by power.
constraint_indexing:constraint_classification(speech_harm_boundary__harm_balancing_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / UNIVERSAL VIEW (MOUNTAIN) — From a civilizational/universal perspective, some balance between protection and restriction is inherent to any communication system: absolute prohibition silences necessary voices; absolute protection enables organized silencing through speech itself. The constraint appears as an immutable feature of social coordination. However, the structural data indicates false summit: the specific balance point, burden of proof placement, and harm definitions are contingent institutional choices, not natural laws.
constraint_indexing:constraint_classification(speech_harm_boundary__harm_balancing_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speech_harm_boundary__harm_balancing_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speech_harm_boundary__harm_balancing_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, TR),
    TR >= 0.70.

:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate, trending upward. The harm-balancing reading imposes real restriction costs on speakers whose speech demonstrably causes harm, but those costs are bounded by the proportionality requirement and the burden of proof. The extractiveness is not as high as pure snare (which would have ε > 0.66) because the doctrine offers genuine coordination benefit — clear (if demanding) rules about when restriction is legitimate. However, the measurement trajectory shows increasing extractiveness over the 30-year interval as enforcement institutions have become more sophisticated in measuring and demonstrating harm, and as marginalized groups have developed better documentation of cumulative harassment effects. This rising trend suggests the constraint is drifting toward snare territory as the harm threshold becomes more operable. Suppression (0.48): Moderate. There are genuine barriers to demonstrating harm (epistemic burden, difficulty capturing cumulative effects, power asymmetries in who can present evidence), but these are not total barriers — legal process exists and some marginalized groups have successfully met the threshold. Theater ratio (0.38): Low-moderate. The harm-balancing doctrine is substantive rather than performative — proportionality analysis genuinely constrains enforcement outcomes more than pure ritual would. However, the doctrine still contains performative elements (proportionality tests applied formulaically, outcomes diverging from doctrine based on political factors), explaining the non-trivial theater value.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival gaps that reveal the reading's asymmetries. Marginalized groups see a snare: they are trapped by speech exposure and trapped again by burden of proof. State enforcement sees a rope: coordination mechanism that provides legitimate framework. The restricted speaker (moderate power) sees tangled rope: mixed extraction and coordination. The powerful speaker sees near-rope (minor restriction costs offset by baseline protection and ability to contest determinations). The analytical observer risks seeing mountain (universal feature of communication systems), but the structural data reveals false summit: the specific burden-of-proof placement and harm threshold are institutional choices, not natural laws. The largest gap is between the marginalized group perspective (snare) and the institutional perspective (rope): the same doctrine appears as constraint vs. coordination depending on who must activate it and who benefits from its application.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by their structural position: whether they benefit or bear costs from this reading's specific balance point and burden-of-proof placement. Marginalized groups subject to hate speech experience high d (close to 1.0 — full targets of extraction) because they bear both the primary speech harm AND the secondary epistemic burden of demonstrating harm. Powerful speakers with institutional platforms experience low d because they benefit from baseline protection and have resources to navigate the burden-of-proof system. State enforcement institutions experience low d because they benefit from the coordination framework that legitimizes their decisions. The tangled rope classification at moderate power reflects that restricted speakers in successful harm-demonstration cases experience mixed effects: real restriction costs but also genuine coordination benefit from predictable framework. The piton classification reflects that the doctrine's actual constraint on enforcement outcomes has weakened as political factors have become dominant in determining which cases succeed.
 *
 * MANDATROPHY ANALYSIS:
 *   The harm-balancing reading resolves mandatrophy through explicit acknowledgment of mixed function: the doctrine genuinely coordinates speech disputes (marginalizing groups and state enforcement need predictable framework) while also extracting costs (speakers bear restriction costs, marginalized groups bear burden of proof). The mandate is not 'pure protection' or 'pure restriction' but 'predictable balancing.' The mandatrophy surfaces as the gap between the doctrine's stated function (proportionality-based coordination) and its actual operation (burden of proof heavily weighted against those seeking restriction, enforcement outcomes determined by political factors rather than doctrinal coherence). This gap is captured in the piton perspective: doctrine maintained for legitimacy while actual constraint comes from power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_demonstration_standard_ambiguity,
    'What standard of evidence constitutes ''demonstrated harm'' sufficient to justify speech restriction under this reading?',
    'Comparative analysis of jurisdictions with different harm thresholds; empirical measurement of impact on marginalized groups under strict vs. permissive harm standards; tracking of legal outcomes across cases with similar speech but different harm-proof strategies',
    'High threshold (empirically verified direct violence, measurable economic loss): narrow unprotected category, favors speakers, requires marginalized groups to prove severe harm. Low threshold (reasonable fear, documented harassment patterns, community testimony): broad unprotected category, favors marginalized groups, requires state to show proportionality. Determines whether this reading is closer to absolutist (high threshold) or dignity (low threshold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_demonstration_standard_ambiguity, empirical, 'Evidentiary standard for ''demonstrated harm'' determining restriction threshold').

omega_variable(
    proportionality_calculus_instability,
    'Is the proportionality balance between restriction costs and harm prevention stable across different power configurations, or does it depend on the political position of enforcement institutions?',
    'Longitudinal analysis of harm-balancing application across administrations with different political compositions; comparison of identical speech types treated as protected vs. restricted under different governments; measurement of doctrine-drift correlation with institutional power shifts',
    'If stable: proportionality doctrine functions as genuine rule of law constraint. If unstable: doctrine is piton (performative ritual maintained by power structures, not functional constraint). Determines whether piton perspective is diagnostic or misclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calculus_instability, empirical, 'Stability of proportionality doctrine across political configurations').

omega_variable(
    social_epistemic_standard_evolution,
    'As social understanding of harm evolves (e.g., recognition of algorithmic amplification effects, cumulative harassment, epistemic injustice), does the harm-balancing doctrine evolve to track shared standards, or does it lag behind evolving consensus?',
    'Historical comparison of harm standards across decades; tracking of gap between academic/activist definitions of harm and doctrinal definitions; survey of convergence/divergence between public understanding and legal standards over time',
    'If doctrine tracks social consensus: scaffold perspective is accurate — sunset logic works as norms mature. If doctrine lags: gap persists, marginalized groups cannot access updated harm standards through legal process, extraction mechanism continues. Determines whether harm-balancing is temporary coordination or stable extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_epistemic_standard_evolution, empirical, 'Doctrinal evolution in response to evolving social understanding of harm').

omega_variable(
    false_summit_naturalization,
    'Is the claim that ''all communication systems require some speech-harm balance'' a universal natural law, or a contingent institutional arrangement that naturalizes specific political choices?',
    'Cross-cultural and historical comparison: do societies with radically different speech protection frameworks (absolute prohibition, absolute protection, communal consensus models) all achieve successful communication? If yes: balance is contingent, not natural. If no: balance point may be natural but specific location (this reading''s harm threshold) is not.',
    'If natural law: mountain classification is correct, harm-balancing is universal feature of human communication. If contingent: mountain is false summit, this reading instantiates a specific political choice presented as natural inevitability. Determines whether the analytical observer''s perspective is genuine insight or naturalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether speech-harm balance is universal natural law or contingent institutional choice').

omega_variable(
    sibling_reading_foreclosure_status,
    'Does harm-balancing doctrine logically foreclose the absolutist reading, or do they coexist as live alternative positions held by different parties?',
    'Examination of whether a single legal framework could hold both: can a jurisdiction maintain harm-balancing as the law while recognizing absolutist arguments as legally respectable? Comparison with jurisdictions that have formally rejected harm-balancing (what grounds do they use?). Assessment of whether the readings differ only in policy preference or in fundamental commitments.',
    'If forecloses: absolutist reading''s core premise is logically impossible within this reading''s framework. If coexists: both readings remain live, present as competing values within pluralist frameworks. Determines reading_relations type and grounds for this reading''s legitimacy claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_status, conceptual, 'Logical relationship between harm-balancing and absolutist readings of speech protection').

omega_variable(
    burden_of_proof_asymmetry_origin,
    'Is the burden of proof placed on marginalized groups (to demonstrate harm) or on state enforcement institutions (to justify restriction) a deliberate design choice of this reading, or a contingent outcome of institutional power?',
    'Analysis of alternative burden-placement regimes: what if burden were on speakers to justify absence of harm? What if burden were shared? Comparison of how different jurisdictions place burden and outcomes for marginalized groups. Assessment of whether burden placement follows from harm-balancing principle or from independent power structure.',
    'If design choice: reading is internally coherent and burden could be rebalanced without abandoning harm-balancing. If contingent: burden placement reflects power dynamics, not doctrine, and represents disguised extraction. Determines whether reading is coherent Tangled Rope or whether snare perspective more accurately captures structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_proof_asymmetry_origin, empirical, 'Origins and contingency of burden-of-proof placement in harm demonstration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_harm_bal_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(speech_harm_bal_tr_t15, speech_harm_boundary__harm_balancing_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(speech_harm_bal_tr_t30, speech_harm_boundary__harm_balancing_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(speech_harm_bal_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(speech_harm_bal_be_t15, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(speech_harm_bal_be_t30, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(speech_harm_bal_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(speech_harm_bal_su_t15, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(speech_harm_bal_su_t30, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, hate_speech_epistemic_burden).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, proportionality_doctrine_enforcement).

% DUAL FORMULATION NOTE:
% The speech-harm boundary constraint family contains three structurally distinct readings of the same kernel: absolutist_reading (ε≈0.15, Mountain for most perspectives, near-universal protection), dignity_reading (ε≈0.68, Snare for speakers, high restriction costs without empirical harm requirement), and harm_balancing_reading (ε=0.52, Tangled Rope, moderate restriction costs bounded by proportionality). Each reading has different ε, different beneficiary/victim structures, and different operational mechanisms. They are not the same constraint viewed from different angles — they are genuinely different interpretations of the constitutional kernel that produce different outcomes. Network links these three readings as family members competing for institutional adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__harm_balancing_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
