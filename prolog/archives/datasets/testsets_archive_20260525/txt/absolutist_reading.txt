% ============================================================================
% CONSTRAINT STORY: absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_absolutist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: absolutist_reading
 *   human_readable: Absolutist Speech Protection (Near-Categorical, Harm-Agnostic)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The absolutist reading of speech protection establishes a
 *   near-categorical rule: speech receives maximal legal protection, and
 *   listener harm is explicitly excluded as a grounds for restriction. This
 *   constraint operates in constitutional democracies (particularly the US
 *   post-Brandenburg v. Ohio) and claims principled status: speech rights are
 *   foundational; harm is too contested and too easily weaponized to serve as
 *   a restriction criterion; protecting unpopular and offensive speech is the
 *   price of protecting dissent. From a Deferential Realism perspective, the
 *   absolutist reading is one interpretation of the contested speech
 *   protection kernel, alongside harm-threshold, marketplace, dignity-based,
 *   and democratic-participation readings. Each reading instantiates a
 *   different constraint with different extraction profiles,
 *   victim/beneficiary structures, and classifications. This JSON generates
 *   ONLY the absolutist reading as a clean, ε-invariant constraint. The
 *   sibling readings are separate constraint files linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Targeted Communities: Primary victims (powerless/trapped) — structurally exposed to speech harm with no legal recourse; high suppression and no exit
 *   - Institutional Speakers: Primary beneficiaries (institutional/arbitrage) — high-power speakers and media outlets maximize reach under absolutist protection; low extraction experience
 *   - Vulnerable Speakers: Secondary actors (moderate/constrained) — activists and academics benefit from protection but bear costs of harassment and targeting they cannot legally restrict
 *   - Free Speech Coalition: Organized agents (organized/mobile) — civil liberties unions and digital rights groups see absolutism as temporary coordination solution with sunset through technological intermediation
 *   - Constitutional Courts: Institutional maintainers (institutional/arbitrage) — maintain the categorical rule through performative application while creating exceptions for workability; piton dynamic
 *   - Analytical Observer: Civilizational position (analytical/analytical) — recognizes the snare structure: absolutism extracts toward speaker liberty and institutional power while suppressing remedy-seeking by harm-bearers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(absolutist_reading, 0.58).
domain_priors:suppression_score(absolutist_reading, 0.72).
domain_priors:theater_ratio(absolutist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(absolutist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(absolutist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(absolutist_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(absolutist_reading, snare).
narrative_ontology:human_readable(absolutist_reading, "Absolutist Speech Protection (Near-Categorical, Harm-Agnostic)").
narrative_ontology:topic_domain(absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(absolutist_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(absolutist_reading, high_power_speakers).
narrative_ontology:constraint_beneficiary(absolutist_reading, institutional_communicators).
narrative_ontology:constraint_victim(absolutist_reading, harm_bearers).
narrative_ontology:constraint_victim(absolutist_reading, targeted_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED COMMUNITY (SNARE) — Structurally exposed to speech harm with no legal recourse. The absolutist reading denies listener harm as a grounds for restriction, trapping the target. High suppression: the legal rule itself prevents retaliation, counter-speech platforms, or institutional remedy. No exit option — the constraint applies nationwide and the target cannot avoid exposure.
constraint_indexing:constraint_classification(absolutist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL BENEFICIARY (ROPE) — High-power speakers (media outlets, politicians, corporations) experience the absolutist rule as pure coordination: maximum speech freedom enables their communication goals. They benefit from the widest possible protection boundary. Arbitrage exit option: can speak or refrain as strategic interest dictates. Low experienced extraction — the constraint enables rather than restrains them.
constraint_indexing:constraint_classification(absolutist_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: VULNERABLE SPEAKER (TANGLED ROPE) — Moderate-power speakers (academics, activists, community organizers) experience mixed effects. The absolutist protection enables their speech but also exposes them to harassment and retaliation they cannot restrict through legal channels. High suppression of counter-speech harassment; constrained exit (can speak but at cost of exposure). Coordination benefit (speech freedom) + extraction cost (unprotected targeting).
constraint_indexing:constraint_classification(absolutist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: FREE SPEECH COALITION (SCAFFOLD) — Organized actors (civil liberties unions, digital rights groups) see the absolutist framework as a temporary coordination solution with eventual sunset. They believe technological intermediation (content moderation, algorithmic filtering, counter-speech platforms) will eventually replace categorical legal prohibition, allowing fine-grained harm reduction without state censorship. Mobile exit: these groups can shift platforms, jurisdictions, and institutional affiliations. Theater ratio moderate: the coalition's advocacy is substantive, not performative.
constraint_indexing:constraint_classification(absolutist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSTITUTIONAL COURT (PITON) — Judicial institutions maintain the absolutist framework through performative application: they announce categorical protection while creating doctrinal exceptions (true threats, incitement, defamation, harassment). The categorical rule persists through inertia despite extensive exception-creation. Theater ratio: high — the exceptions substantially reduce the rule's force, but the categorical framing is maintained for institutional legitimacy and historical continuity.
constraint_indexing:constraint_classification(absolutist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the absolutist reading operates as a snare masquerading as a principle. The framework claims near-categorical protection but relies on continuous exception-creation to remain workable. The analytical observer sees the rule as extractive toward harm-bearers (maximizing speaker autonomy at cost to targeted communities) while maintaining the myth of categorical protection. High suppression: the legal category itself prevents remedy-seeking. Moderate extractiveness: the rule genuinely does expand speaker protection, but asymmetrically favors institutional speakers.
constraint_indexing:constraint_classification(absolutist_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(absolutist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(absolutist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(absolutist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(absolutist_reading, TR),
    TR >= 0.70.

:- end_tests(absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The absolutist rule genuinely expands speaker protection for all speakers, but the protection is asymmetrical: institutional and powerful speakers can leverage the wide boundary; vulnerable speakers gain protection but cannot use it without exposure to retaliatory speech they cannot restrict. The extraction mechanism is not prohibition (the rule is permissive) but suppression of remedy. The trajectory from 0.35 to 0.58 reflects increasing recognition that the absolutist boundary, while principled, systematically favors powerful speakers and enables coordinated harassment of vulnerable groups. Suppression (0.72): High. The legal rule explicitly forecloses harm as a grounds for remedy — this is not a side effect but the rule's core content. Targeted communities face high barriers: they cannot restrict attackers through legal channels, and the categorical rule itself delegitimizes harm-based claims (if the law says harm is not grounds for restriction, victims internalize this as a reason their claims lack standing). Theater ratio (0.45): Moderate-low. The absolutist reading is substantively argued and legally enforced; it is not purely performative. But the theater has increased over the interval as doctrinal exceptions have accumulated (Brandenburg true-threats test, harassment law, defamation law) without formally revising the categorical principle. The rule maintains categorical framing while exceptions absorb most complexity.
 *
 * PERSPECTIVAL GAP:
 *   The absolutist reading produces sharp perspectival divergence. Institutional beneficiaries classify the constraint as Rope (pure coordination of their speech rights) because they experience low or negative χ. Targeted communities classify it as Snare (pure extraction with no coordination benefit) because they experience high χ with no exit. Vulnerable speakers see Tangled Rope (mixed coordination and extraction) because they gain protection but bear harassment costs. Organized agents see Scaffold (temporary coordination with eventual sunset through technology and norms) because they perceive mobile exit and believe alternatives will emerge. Courts see Piton (degraded categorical rule maintained through exceptions) because they recognize the categorical framing has decayed into standards. The analytical observer sees Snare (the systematic extraction toward institutional power) because they can measure asymmetry across perspectives. These are not measurement errors or uncertainty — they are genuine differences in how the constraint operates from different structural positions. The perspectival gap is the entire point: the same legal rule appears as principled coordination to the powerful and as suppressive extraction to the vulnerable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by agent. Institutional beneficiaries (high-power speakers) have d ≈ 0.05 — they are full beneficiaries with arbitrage exit, experiencing negative effective extraction (the rule subsidizes their speech). Targeted communities have d ≈ 0.95 — they are full targets with trapped exit, experiencing maximum extraction (no legal remedy, no alternative mechanism). Vulnerable speakers have d ≈ 0.70 (moderate/constrained) — they are partly beneficiaries (speech protection) and partly targets (harassment suppression). Organized agents (free speech coalition) have d ≈ 0.45 (mobile exit, mixed beneficiary/victim status). The analytical observer has d ≈ 0.80 (analytical position observing asymmetric extraction) — they see the snare structure clearly but cannot dissolve it from a position of pure observation. The sigmoid f(d) transformation applies, amplifying the experienced extraction for powerless agents (d ≈ 0.95) and dampening it for beneficiaries (d ≈ 0.05). Scope modifier σ(national) = 1.0 applies without adjustment. χ = ε × f(d) × σ(S) produces experienced extractiveness that exceeds base ε for targets and falls below it for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading resolves mandatrophy by treating the constraint as a snare with legitimacy grounding in a principle (speaker liberty) that blinds observers to its extraction mechanism (suppression of remedy). The mandate (speech must be maximally protected) is genuine: the rule does serve important coordination functions for diverse speakers and prevents government censorship. But the mandate's enforcement (harm is categorically excluded as grounds for restriction) creates extraction at the cost of harm-bearers. The mandatrophy is not resolved by declaring the rule illegitimate — it is resolved by recognizing that principled coordination mechanisms can simultaneously extract from excluded parties. The absolutist reading does not deny this; it claims that the extraction (suppression of remedy) is an acceptable cost of preventing worse outcomes (government censorship, harm-based suppression of dissent). That is a values judgment, not a classification error. The snare classification is structurally correct: high extraction (suppression of remedy), high suppression (rule forecloses remedy-seeking), χ ≥ 0.66 (from the powerless perspective). The beneficiary's rope classification is also correct from their perspective: coordination of speech rights with low extraction cost to them. Mandatrophy resolves by accepting that both are accurate from their respective structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_exception_load,
    'At what exception-density does a categorical rule collapse into a standards-based test? Has the absolutist reading already crossed that threshold?',
    'Empirical doctrinal analysis: count exceptions (true threats, incitement per Brandenburg, defamation, false advertising, harassment, doxxing, copyright infringement, child safety material) as fraction of total speech cases. Cross-jurisdictional comparison (US vs European vs common-law approaches to exception-creation).',
    'If exception-load > 60% of speech cases: the absolutist rule is already a piton (functional degradation through exception-creation). If < 40%: categorical protection remains structurally coherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_vs_exception_load, empirical, 'Proportion of speech cases resolved through exceptions to categorical rule').

omega_variable(
    harm_definition_contestation,
    'Is ''harm'' sufficiently contested that harm-agnostic categoricalism is the only coherent rule, or does harm contestation reflect deeper disagreement about whose interests count?',
    'Normative philosophy + empirical discourse analysis: document competing harm definitions (psychological harm vs tangible harm vs civic silencing vs community trauma) and trace whose interests each definition protects. Cross-cultural comparison: how other democracies define actionable harm.',
    'If harm definition is genuinely indeterminate: absolutism is pragmatically justified (harm-based tests are incoherent). If harm is operationalizable but values-laden: absolutism is a value choice that favors speaker liberty over harm-bearer protection — reclassifies from principled mountain to extractive snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_definition_contestation, conceptual, 'Whether harm can be operationally defined or is fundamentally contested').

omega_variable(
    institutional_speaker_asymmetry,
    'Does the absolutist rule operate symmetrically across institutional and non-institutional speakers, or does it systematically amplify powerful speakers'' reach while powerless speakers gain protection they cannot monetize?',
    'Empirical: measure speech amplification (reach, platform distribution, algorithmic boost) by speaker power category under absolutist rule. Track litigation outcomes (who can afford to defend speech claims). Document self-censorship rates by speaker power (powerful speakers self-censor less).',
    'If asymmetric: extractiveness should increase to 0.72+; snare classification is correct. If symmetric: extractiveness drops to ~0.35; rope classification becomes viable for all perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_speaker_asymmetry, empirical, 'Whether absolutist rule operates symmetrically across speaker power levels').

omega_variable(
    kernel_reading_contestation,
    'Is the absolutist reading itself contestable, or is it one stable interpretation among several coherent alternatives?',
    'This is the committer structure. The absolutist reading is one of five sibling readings of the speech_protection_kernel: (1) absolutist_reading (this constraint), (2) harm_threshold_reading, (3) marketplace_reading, (4) dignity_reading, (5) democratic_participation_reading. Each reading instantiates a different constraint with different ε, beneficiary/victim structure, and type.',
    'Recognition that the ''absolutist principle'' is a reading choice, not a law of nature, routes through omega and commentary.kernel_context rather than affecting the classification of this constraint. Each reading is generated as a clean, ε-invariant constraint in its own JSON file, linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'The absolutist reading is one interpretation of the contested speech protection kernel').

omega_variable(
    suppression_mechanism_internalization,
    'Does high suppression (0.72) reflect external legal barriers or internalized framing? Do targeted communities self-censor because the law forbids their claims or because they''ve internalized the ''harm is not grounds for restriction'' principle?',
    'Discourse analysis + empirical: document instances where targeted communities invoke the absolutist principle to defend their attackers; measure self-censorship by group before/after exposure to the absolutist framing vs after exposure to alternative framings (harm-threshold, dignity-based).',
    'If largely internalized: suppression persists even when legal barriers are removed; constraint''s force is cognitive rather than structural. If external: removal of the legal rule would substantially reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or partly internalized through exposure to absolutist principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(absolutist_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abso_tr_t0, absolutist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(abso_tr_t3, absolutist_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(abso_tr_t5, absolutist_reading, theater_ratio, 5, 0.45).

% Extraction over time
narrative_ontology:measurement(abso_be_t0, absolutist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(abso_be_t2, absolutist_reading, base_extractiveness, 2, 0.46).
narrative_ontology:measurement(abso_be_t5, absolutist_reading, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(absolutist_reading, harm_threshold_reading).
narrative_ontology:affects_constraint(absolutist_reading, marketplace_reading).
narrative_ontology:affects_constraint(absolutist_reading, dignity_reading).
narrative_ontology:affects_constraint(absolutist_reading, democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The absolutist reading is one interpretation of the speech_protection_kernel. It is decomposed from that kernel into a separate constraint file to enable structural analysis of each reading independently. The five readings (absolutist, harm-threshold, marketplace, dignity, democratic-participation) are linked as a constraint family via network.affects_constraints. Each reading instantiates a different constraint with different extractiveness, beneficiary/victim structure, and classification. The absolutist reading claims ε ≈ 0.58 (moderate-high extractiveness from the powerless perspective due to suppression of remedy); sibling readings will show different ε values reflecting their different empirical contestation levels and operational mechanisms. This decomposition follows ε-invariance principle: each reading measures the constraint differently and produces different ε, so each is a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
