% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__marketplace_reading, []).

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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Speech Protection as Marketplace Truth-Discovery (Marketplace Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   The marketplace reading of speech protection grounds the constraint in
 *   epistemic justification rather than individual autonomy or democratic
 *   participation. The core claim: unrestricted speech serves truth-discovery
 *   because errors are best corrected through counter-speech and open debate,
 *   not through institutional suppression of content. False or harmful speech
 *   is countered by more speech — better arguments, corrections,
 *   counter-narratives — which refines the marketplace toward truth. This
 *   reading rejects content-based restrictions as distorting the
 *   truth-discovery process (institution cannot reliably distinguish true
 *   from false; restriction prevents the market from testing claims;
 *   suppression creates information asymmetries favoring the suppressing
 *   party). The structural consequence: the constraint protects speakers'
 *   freedom to participate in the market but does not protect listeners from
 *   false speech, subordinated groups from coordinated denigration, or the
 *   epistemic commons from pollution by institutional falsehoods. The
 *   reading's beneficiaries are institutional speakers and the speech market
 *   architecture itself; its victims are targets of false speech and
 *   subordinated groups whose capacity to participate as epistemic equals is
 *   constrained by the market asymmetries the reading refuses to remedy.
 *
 * KEY AGENTS:
 *   - Individual Target of False Speech (powerless/trapped) — bears extraction through lack of equal amplification capacity; must correct at own expense without institutional support
 *   - Subordinated Group (powerless/trapped) — bears durable epistemic harm from coordinated false speech about group characteristics; marketplace refuses content-based protective intervention
 *   - Fact-Checking and Counter-Speech Communities (moderate/constrained) — benefit from marketplace logic legitimizing their corrective role; constrained by asymmetric resources and attention scarcity relative to false-speech production
 *   - Institutional Media and Technology Platforms (institutional/arbitrage) — primary beneficiaries; capture arbitrage from amplification asymmetries, algorithmic advantages, institutional voice; protected from content-based moderation by marketplace logic
 *   - Civil Rights and Democratic Protection Advocates (organized/constrained) — constrained by the marketplace reading's rejection of protective intervention; benefit from speech protection enabling their own advocacy; face suppression from neutral application of marketplace logic to their speech
 *   - Legal Doctrine and Institutional Implementation (institutional/arbitrage) — maintains marketplace doctrine while carving exceptions that contradict it; high theater ratio as courts invoke market logic while acknowledging its asymmetries
 *   - Analytical Observer (analytical/analytical) — risks naturalizing market asymmetries as epistemic features; may see pure coordination where structural extraction is occurring
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.38).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.42).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Speech Protection as Marketplace Truth-Discovery (Marketplace Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '2f787f9e-8198-47a9-b0ce-55de22145e1c').
narrative_ontology:cs_kernel_codification('2f787f9e-8198-47a9-b0ce-55de22145e1c', formalized).
narrative_ontology:cs_authority_grounding('2f787f9e-8198-47a9-b0ce-55de22145e1c', lineage).
narrative_ontology:cs_interpretation_layer_present('2f787f9e-8198-47a9-b0ce-55de22145e1c').
narrative_ontology:cs_reading_relation('2f787f9e-8198-47a9-b0ce-55de22145e1c', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f787f9e-8198-47a9-b0ce-55de22145e1c', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f787f9e-8198-47a9-b0ce-55de22145e1c', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('2f787f9e-8198-47a9-b0ce-55de22145e1c', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('2f787f9e-8198-47a9-b0ce-55de22145e1c', foundational, truth_discovery_justification).
narrative_ontology:cs_axiom_status(truth_discovery_justification, holdable).
narrative_ontology:cs_axiom_grounding('2f787f9e-8198-47a9-b0ce-55de22145e1c', truth_discovery_justification, empirically_contingent).
narrative_ontology:cs_axiom('2f787f9e-8198-47a9-b0ce-55de22145e1c', foundational, market_correction_sufficiency).
narrative_ontology:cs_axiom_status(market_correction_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('2f787f9e-8198-47a9-b0ce-55de22145e1c', market_correction_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('2f787f9e-8198-47a9-b0ce-55de22145e1c', unrestricted_marketplace_of_ideas).
narrative_ontology:cs_drift_state('2f787f9e-8198-47a9-b0ce-55de22145e1c', digital_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f787f9e-8198-47a9-b0ce-55de22145e1c', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, institutional_truth_discovery_process).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, institutional_speech_market_participants).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targets_of_false_or_harmful_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, epistemic_commons_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGET OF FALSE SPEECH (SNARE) — Trapped by the marketplace logic. No exit from the constraint: the reading positions counter-speech as the remedy, but a powerless target cannot command equal amplification or resources to mount an effective counter-narrative. The constraint extracts by requiring the victim to do additional epistemic labor (fact-correction, narrative reconstruction) while the false speech has already diffused. No suppression of the false speech itself — the market ideology prevents protective intervention, leaving the target structurally defenseless.
constraint_indexing:constraint_classification(speech_protection_kernel__marketplace_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUBORDINATED GROUP BEARING DURABLE EPISTEMIC HARM (SNARE) — Trapped in the market by coordinated false speech that naturalizes their subordination. The marketplace reading rejects content-based intervention ('more speech, not enforced silence'), which means systematic falsehoods about capacity, trustworthiness, or humanity persist as market-traded beliefs. The group cannot counter with equal force — the market asymmetry IS the extraction mechanism. Exit options are none: they cannot leave the epistemic commons, and the commons treats their denigration as a tradeable speech commodity.
constraint_indexing:constraint_classification(speech_protection_kernel__marketplace_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FACT-CHECKING AND COUNTER-SPEECH COMMUNITIES (TANGLED ROPE) — Constrained by resource limits and attention economics, but also benefit from the market logic. The reading positions them as the functional correction mechanism ('more speech'), which gives their work institutional and moral legitimacy. They coordinate on debunking and counter-narrative production — genuine coordination function. But extraction is embedded: the market requires them to work without enforced restrictions on the original false speech; they must catch up to diffusion; they operate under resource constraints (funding, volunteer effort) while false speech production is often institutionally or commercially resourced. Mixed experience of genuine enabling coordination and asymmetric extraction burden.
constraint_indexing:constraint_classification(speech_protection_kernel__marketplace_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL MARKET BENEFICIARIES (ROPE) — Platforms, media companies, and institutional speakers benefit from the marketplace reading's protection of unrestricted speech. The constraint is experienced as coordination: they facilitate communication among all parties; the market logic justifies their refusal to moderate based on content (they are neutral conduits, not censors); they capture arbitrage from the volume of speech (engagement metrics, ad revenue, attention asymmetries favor institutional voices with resources to speak loudly). Suppression is low for them — their market position is secured by the constraint, not limited by it.
constraint_indexing:constraint_classification(speech_protection_kernel__marketplace_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEMOCRATIC PROTECTION ADVOCATES (TANGLED ROPE) — Organized but constrained. The marketplace reading constrains their toolkit: they cannot advocate for content-based restrictions or institutional removal of harmful speech without contradicting the reading's core epistemic justification. Yet they benefit from the general speech protection framework — it enables their own advocacy, organizing, and counter-speech. They face real suppression from institutional interpretation of the marketplace logic (platforms treating political speech as content-neutral, moderating critique more than harm). Mixed experience: genuine protection benefits alongside structural constraints on protective remedies.
constraint_indexing:constraint_classification(speech_protection_kernel__marketplace_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DOCTRINE AND INSTITUTIONAL IMPLEMENTATION (PITON) — The marketplace reading has become substantially performative in doctrine. Courts invoke the marketplace logic (more speech counters bad speech) while simultaneously acknowledging that market conditions are systematically asymmetric (institutional speakers vs. powerless targets, algorithmic amplification, attention scarcity). The doctrine persists as the canonical U.S. framework despite mounting evidence that undisputed facts (false speech spreads faster than corrections, resource asymmetries prevent equal counter-speech, vulnerable populations cannot market themselves adequately). The theater ratio is high: the legal doctrine performs the marketplace fiction while courts implicitly recognize its failure (limited exceptions carved out for fraud, defamation, incitement — exceptions that contradict the marketplace premise but continue because practice requires them).
constraint_indexing:constraint_classification(speech_protection_kernel__marketplace_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PURE COORDINATION VIEW (ROPE) — From a purely epistemic standpoint, the marketplace reading coordinates all parties around a single rule (unrestricted speech) that enables distributed truth-discovery. The analytical view sees low-friction coordination: everyone speaks, everyone listens, errors are corrected through debate, system converges toward truth. This perspective treats extraction as absent — the constraint is a pure coordination mechanism. However, structural data contradicts this: victims are trapped, subordinated groups bear durable harm, institutional beneficiaries arbitrage their market positions, and doctrine is largely performative. The analytical observer risks naturalizing market asymmetries as epistemic features.
constraint_indexing:constraint_classification(speech_protection_kernel__marketplace_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speech_protection_kernel__marketplace_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speech_protection_kernel__marketplace_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, TR),
    TR >= 0.70.

:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reading justifies protection of speech that benefits institutional speakers and harms powerless targets. The extractiveness is not maximal (0.46+) because genuine coordination functions exist — the marketplace does enable distributed truth-seeking, counter-speech does sometimes correct falsehoods, and unrestricted speech enables vulnerable groups' own advocacy. However, extraction is embedded in the reading's refusal to remedy market asymmetries: powerless targets cannot command equal counter-speech resources; institutional speakers can amplify falsehoods at scale with minimal correction cost; subordinated groups' denigration is treated as a legitimate market commodity. The extractiveness has risen over time (0.22 → 0.38) as platform technologies amplified the resource asymmetries (institutional voices reach billions; powerless counter-speech reaches thousands) and made algorithmic moderation choices visible, revealing that 'neutral facilitation' is actually active market architecture. Suppression (0.42): Moderate. The reading does not suppress false speech directly, but it suppresses protective responses (content-based moderation, institutional correction, removal of denigrating speech) through the marketplace logic ('more speech, not enforced silence'). For powerless targets and subordinated groups, this means suppression of remedies available in other readings. Institutional voices face low suppression — they can speak freely with full market amplification. Theater ratio (0.58): Moderate. The doctrine performs a marketplace fiction: courts invoke market logic while carving exceptions (fraud, incitement, defamation) that acknowledge the market fails. The theater has increased as empirical evidence of market failure accumulated (false information spreads faster than corrections; resource asymmetries prevent equal counter-speech; algorithmic amplification contradicts neutrality claims) but doctrine formally retained the marketplace frame. The exceptions are essentially admissions that 'more speech' doesn't actually counter harmful false speech in practice — yet courts continue invoking marketplace logic while authorizing protective exceptions.
 *
 * PERSPECTIVAL GAP:
 *   The marketplace reading produces a stark perspectival gap between beneficiaries and victims. Institutional speakers and platforms experience it as pure coordination (Rope) — the rule enables their speech with minimal friction, and the market logic justifies their refusal to suppress competing speech. Fact-checkers and civil rights advocates experience mixed coordination and extraction (Tangled Rope) — they benefit from speech protection enabling their own work but are constrained by the reading's rejection of protective remedies. Powerless targets and subordinated groups experience pure extraction (Snare) — the marketplace offers them counter-speech as the remedy, but they lack the resources to mount it, making them structurally defenseless. The legal doctrine itself experiences its own degradation (Piton) — courts invoke marketplace logic while simultaneously carving exceptions that contradict it, suggesting the doctrine persists through institutional inertia rather than epistemic conviction. The analytical observer risks misclassifying the whole structure as pure coordination (Rope) — seeing the marketplace as a neutral epistemic mechanism rather than an architecture that asymmetrically extracts from powerless agents and subordinated groups.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across perspectives based on each agent's structural position within the speech market. Institutional speakers are net beneficiaries of the marketplace reading: they speak with full amplification, face minimal suppression, and experience low extraction — their d approaches 0.0 (institutional/arbitrage → canonical d ≈ 0.0). Powerless targets are pure victims: they cannot command counter-speech, face full extraction through asymmetric amplification, experience maximum suppression of remedies — their d approaches 1.0 (powerless/trapped → canonical d ≈ 1.0). Moderate and organized agents occupy intermediate positions: they benefit from speech protection enabling their own advocacy but are constrained by the reading's refusal to remedy market asymmetries — their d ≈ 0.50-0.70 (moderate/constrained or organized/constrained). The analytical observer's d ≈ 0.73 (canonical fallback for analytical power atom) reflects neither net benefit nor net victimization — the observer position is neutral but at risk of missing the asymmetric extraction masked by the marketplace logic.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING INSTANTIATION: This constraint's mandatrophy is resolved not by proving the marketplace reading is 'correct' but by documenting how it functions as one reading among multiple competitors. The reading is neither purely coordination (Rope) nor purely extraction (Snare) — it is tangled (benefits institutional speakers and enables counter-speech, while extracting from powerless targets and subordinated groups). The piton perspective (legal doctrine) reveals that courts have implicitly recognized the reading's failure — the exception doctrine contradicts marketplace premises but persists. The snare perspectives (powerless targets) reveal that the reading's remedy ('more speech') does not function when resource asymmetries prevent equal counter-speech. The rope perspective (institutional beneficiary) reveals that the reading does enable their coordination role. All perspectives are holding structural truths — the constraint manifests differently to different agents because the marketplace itself is asymmetric. The mandatrophy is resolved by accepting that the marketplace reading is ONE READING, currently institutionalized in U.S. doctrine and platform policy, but in structural tension with dignity and harm-threshold readings. The omega variables document the unresolved contests: Does the marketplace actually converge to truth? Can powerless targets mount effective counter-speech? Does the marketplace reading foreclose dignity-based protections? These are empirical, conceptual, and normative questions that no single indexical classification can settle — they are the substance of the ongoing kernel contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_asymmetry_vs_market_efficiency,
    'Does the marketplace of ideas actually converge toward truth despite asymmetric resources, or do structural inequalities in voice and amplification create stable false-belief equilibria?',
    'Empirical analysis of belief convergence: do false claims about subordinated groups, scientific matters, or political figures eventually get corrected at scale, or do they persist in subcommunities? Comparison of correction rates across resource-symmetric vs. asymmetric speech domains. Analysis of cascade effects (early distribution advantage persists regardless of later correction).',
    'If asymmetries prevent convergence: the marketplace reading is aspirational rather than structural — the constraint is not pure rope but tangled rope or snare, requiring protective intervention. If convergence occurs despite asymmetries: the reading holds and protection through speech alone is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_asymmetry_vs_market_efficiency, empirical, 'Whether marketplace of ideas converges to truth or settles into false-belief equilibria').

omega_variable(
    counter_speech_capacity_equity,
    'Can powerless targets (individuals, subordinated groups) mount effective counter-speech against institutional falsehoods without external amplification support or content-based moderation?',
    'Comparative analysis of counter-speech effectiveness: does fact-checking and counter-narrative production by powerless agents achieve parity with institutional false-speech distribution? What resource ratios are required for counter-speech to reach equivalent audiences? Do platforms'' neutral facilitation policies systematically disadvantage counter-speech (algorithmic amplification, attention scarcity)?',
    'If counter-speech requires orders-of-magnitude greater resources: the marketplace reading fails its own coordination claim — ''more speech'' is not a functional remedy. Classification shifts toward snare. If parity is achievable at reasonable resource ratios: the reading''s structural logic holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counter_speech_capacity_equity, empirical, 'Whether counter-speech by powerless agents can achieve parity with institutional false speech').

omega_variable(
    subordination_via_speech_markets,
    'Does the marketplace reading''s protection of harmful stereotypes, systematic falsehoods, and denigrating speech function as structural subordination of target groups, making this reading incompatible with equal dignity and equal participation?',
    'Analysis of whether coordinated false or denigrating speech about a group (whether intentional or emergent from market dynamics) produces measurable constraints on that group''s ability to participate as equals in the epistemic commons (epistemic injustice, silencing, reduced credibility). Does the reading''s no-intervention principle foreclose protective responses that would enable equal participation?',
    'If systematic: the marketplace reading coexists with dignity and democratic participation readings (different parties'' commitments), but cannot coexist within a single framework that prioritizes equal participation or equal dignity. Framework choice becomes binding. If incidental: the readings can coexist within frameworks that deemphasize participation equality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_via_speech_markets, conceptual, 'Whether marketplace speech protection reproduces structural subordination of target groups').

omega_variable(
    doctrine_vs_practice_divergence,
    'Why does the legal doctrine invoke marketplace logic while simultaneously carving exceptions (fraud, incitement, defamation) that contradict the logic? Is the doctrine performing the marketplace fiction while practice requires protective intervention?',
    'Historical analysis of exception doctrine: do exceptions expand when empirical evidence of market failure accumulates? Do courts explicitly acknowledge that exceptions contradict marketplace premises? Do jurisdictions that adopt stronger protective doctrines (dignity, harm-threshold) show different market outcomes?',
    'If exceptions are expanding due to market failure evidence: the marketplace reading is being implicitly abandoned in doctrine while formally retained, indicating piton degradation. If exceptions remain stable: doctrine may be stable despite acknowledging asymmetries. If protective doctrines show better empirical outcomes: the marketplace reading''s epistemic justification is falsified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_vs_practice_divergence, empirical, 'Whether exception doctrine reveals covert abandonment of marketplace logic').

omega_variable(
    kernel_contest_foreclosure,
    'Does the marketplace reading''s core premise (speech protection serves truth-discovery; false speech is best countered by more speech) logically foreclose any of the sibling readings, or do all readings remain live normative positions?',
    'Philosophical analysis: can a framework hold both the marketplace reading AND the dignity reading (which rejects speech that functions as subordination)? Can it hold both the marketplace reading AND a harm-threshold reading that justifies protective intervention? Where are the logical breaking points?',
    'If the marketplace reading forecloses dignity or harm-threshold: the sibling readings cannot coexist in one framework; adoption is zero-sum. If readings coexist: different parties can hold different readings and negotiate the gap. Framework choice determines whether readings compete or cooperate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Whether marketplace reading logically forecloses sibling readings').

omega_variable(
    platform_neutrality_and_market_structure,
    'Do social media platforms'' claims of content-neutral facilitation (justified by the marketplace reading) constitute accurate representation of their market role, or do algorithmic choice, moderation asymmetries, and amplification decisions make them active architects of the speech market?',
    'Empirical analysis of platform behavior: are algorithmic recommendations and moderation policies actually content-neutral, or do they systematically amplify certain speech types? Do platforms'' public statements about neutrality match their actual design choices? What visibility do platforms give to false speech vs. corrections?',
    'If platforms are genuinely neutral: the marketplace reading''s justification for non-intervention applies to them. If platforms actively shape the market: they are not neutral facilitators but architects whose choices determine market structure — the reading''s non-intervention principle is misapplied, and platforms become responsible for market outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_neutrality_and_market_structure, empirical, 'Whether platform neutrality claims match actual algorithmic and moderation behavior').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spkm_tr_t0, speech_protection_kernel__marketplace_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(spkm_tr_t5, speech_protection_kernel__marketplace_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(spkm_tr_t10, speech_protection_kernel__marketplace_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(spkm_be_t0, speech_protection_kernel__marketplace_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(spkm_be_t5, speech_protection_kernel__marketplace_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(spkm_be_t10, speech_protection_kernel__marketplace_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(spkm_su_t0, speech_protection_kernel__marketplace_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(spkm_su_t5, speech_protection_kernel__marketplace_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(spkm_su_t10, speech_protection_kernel__marketplace_reading, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, information_standard).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (speech_protection_kernel). The five readings — marketplace, absolutist, harm-threshold, dignity, democratic-participation — are separate constraint stories linked by the kernel contest. Each reading has its own beneficiaries, victims, epsilon value, and classification profile. The marketplace reading is characterized by moderate extractiveness (0.38) arising from market asymmetries; the absolutist reading would have lower extractiveness (emphasis on individual autonomy trumps all structural concerns); the harm-threshold reading would have higher extractiveness of the false-speech harm itself (reading rejects the marketplace's no-intervention premise); the dignity reading would reframe subordination as the primary victim; the democratic reading would prioritize political speech over market efficiency. These are not different perspectives on one constraint — they are different constraints instantiating the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
