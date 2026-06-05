% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_categorical_balancing, []).

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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Categorical Balancing: Protected/Unprotected Speech Categories via Judicial Case-by-Case Determination
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The categorical balancing reading of the First Amendment construes
 *   protection as emerging from case-by-case judicial determinations of the
 *   speech value of a particular utterance against the magnitude of harm it
 *   risks. Rather than treating the Amendment's text ('Congress shall make no
 *   law abridging freedom of speech') as categorical immunity, this reading
 *   treats it as establishing a method: courts balance competing interests to
 *   determine which categories of speech receive protection and which do not.
 *   Obscenity, incitement to imminent lawless action, true threats, and a
 *   narrowing set of other categories are deemed unprotected based on
 *   judicial findings that speech value is low and harm prevention interest
 *   is high. This constraint instantiates one interpretation of a contested
 *   kernel — the First Amendment text itself — and should be understood as
 *   distinct from absolutist categorical readings (which claim the text
 *   forecloses balancing) and harm-limited readings (which subordinate
 *   categorical analysis to a pure harm principle). The extractiveness value
 *   (0.58) reflects that the categorical balancing framework concentrates
 *   interpretive power in the judiciary (beneficiary: institutional judiciary
 *   maintaining control over category boundaries) while imposing costs on
 *   marginalized speakers and legal predictability (victims: disfavored
 *   speech groups and actors needing legal certainty). The measurements show
 *   increasing extraction over the interval: as new fact patterns emerge, the
 *   courts rebalance categories, expanding suppression and increasing theater
 *   (performative deference to balancing language while enforcement proceeds
 *   under settled category assumptions).
 *
 * KEY AGENTS:
 *   - Institutional Judiciary: Primary beneficiary (institutional/arbitrage) — maintains unilateral control over category boundaries through case-by-case balancing; benefits from discretionary power to resolve disputes
 *   - Marginalized Speakers (radical political, extremist, hate speech communities): Primary victims (powerless/trapped) — their speech falls into disfavored categories; cannot exit or appeal category assignment without risking prosecution
 *   - Mainstream Press and Established Publishers: Secondary agents (moderate/constrained) — benefit from stability of established protected categories but constrained by ongoing rebalancing uncertainty
 *   - Civil Liberties Coalition: Organized agents (organized/constrained) — see categorical balancing as temporary framework with sunset potential; pursuing doctrinal evolution toward broader protection or harm-based standards
 *   - Law Enforcement and Security Apparatus: Institutional actor (institutional/arbitrage) — uses categorical balancing language to justify enforcement that occurs through largely performative deference to settled categories
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the categorical framework as inherent to speech regulation rather than recognizing it as contingent institutional arrangement with identifiable beneficiaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.58).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.62).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Categorical Balancing: Protected/Unprotected Speech Categories via Judicial Case-by-Case Determination").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, '2d9f313f-51f7-4446-add9-327680a05712').
narrative_ontology:cs_kernel_codification('2d9f313f-51f7-4446-add9-327680a05712', fixed_text).
narrative_ontology:cs_authority_grounding('2d9f313f-51f7-4446-add9-327680a05712', lineage).
narrative_ontology:cs_interpretation_layer_present('2d9f313f-51f7-4446-add9-327680a05712').
narrative_ontology:cs_reading_relation('2d9f313f-51f7-4446-add9-327680a05712', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d9f313f-51f7-4446-add9-327680a05712', first_amendment_speech_protection__harm_limited_reading, influences).
narrative_ontology:cs_axiom('2d9f313f-51f7-4446-add9-327680a05712', foundational, judicial_case_by_case_balancing_proper_method).
narrative_ontology:cs_axiom_status(judicial_case_by_case_balancing_proper_method, holdable).
narrative_ontology:cs_axiom_grounding('2d9f313f-51f7-4446-add9-327680a05712', judicial_case_by_case_balancing_proper_method, conventional).
narrative_ontology:cs_axiom('2d9f313f-51f7-4446-add9-327680a05712', secondary, speech_value_legitimately_weighable_against_harm).
narrative_ontology:cs_axiom_status(speech_value_legitimately_weighable_against_harm, holdable).
narrative_ontology:cs_axiom_grounding('2d9f313f-51f7-4446-add9-327680a05712', speech_value_legitimately_weighable_against_harm, deontological).
narrative_ontology:cs_reference_frame('2d9f313f-51f7-4446-add9-327680a05712', categorical_balancing_doctrine).
narrative_ontology:cs_drift_state('2d9f313f-51f7-4446-add9-327680a05712', contemporary_speech_ecosystem, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2d9f313f-51f7-4446-add9-327680a05712', '2026-02-27T15:33:00Z').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, established_speech_categories).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, disfavored_speech_groups).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, marginalized_speakers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISFAVORED SPEECH GROUP (SNARE) — Speakers whose utterances fall into judicially disfavored categories (e.g., hate speech, radical political speech, speech challenging state security) face categorical suppression with minimal recourse. Trapped by the category assignment itself; cannot appeal the category boundary without risking prosecution. The category system provides zero coordination benefit to this agent — it is pure extraction disguised as neutral legal categorization.
constraint_indexing:constraint_classification(first_amendment_speech_protection__categorical_balancing_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAINSTREAM PRESS (TANGLED ROPE) — Benefits from the stability of established protected categories (journalists have presumptive protection for newsgathering, editorial judgment). But constrained by the doctrine's ongoing case-by-case balancing: new factual circumstances trigger new judicial balancing that can narrowly alter category boundaries, creating uncertainty. Genuine coordination function (defining reliable sphere of protection) combined with asymmetric extraction (courts retain unilateral power to redefine categories on a fact-specific basis).
constraint_indexing:constraint_classification(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL JUDICIARY (ROPE) — The primary beneficiary of categorical balancing doctrine. Courts maintain interpretive control over the boundary between protected and unprotected speech through case-by-case determinations. This arrangement provides genuine coordination: the judiciary resolves disputes about speech boundaries. The judiciary experiences the constraint as enabling its institutional function, not as extraction. Arbitrage exit (courts can choose which cases to hear, how to frame balancing tests) and immediate time horizon (decisions are made quickly within the system's preferred timeframe).
constraint_indexing:constraint_classification(first_amendment_speech_protection__categorical_balancing_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL LIBERTIES COALITION (SCAFFOLD) — Organized agents (ACLU, First Amendment scholars, speech-rights organizations) see categorical balancing as a temporary framework vulnerable to being sunset by doctrinal evolution. These advocates pursue structural change toward categorical protection (moving toward absolutist reading) or harm-only standards (moving toward harm_limited reading). They experience the constraint as having a sunset: as legal doctrine evolves and case law accumulates, the categorical balancing framework can be formally superseded. Current trajectory shows pressure toward expanding protected categories.
constraint_indexing:constraint_classification(first_amendment_speech_protection__categorical_balancing_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LAW ENFORCEMENT (PITON) — Police and security agencies maintain categorical balancing doctrine through largely performative deference to judicial decisions. The constraint is institutionally inert: law enforcement conducts surveillance and suppression of disfavored speech categories (radical political speech, extremist content) and then post-hoc justifies it through categorical balancing language. The doctrine provides cover for enforcement that would happen regardless. Theater ratio high because the judicial balancing is genuinely difficult and legitimate, but enforcement occurs under the premise that certain categories (incitement, true threats) are already settled, reducing the actual balancing work to theater.
constraint_indexing:constraint_classification(first_amendment_speech_protection__categorical_balancing_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED LIMITS VIEW (MOUNTAIN) — From a universalist/civilizational standpoint, some boundaries between protected and unprotected speech appear inherent to any speech system: incitement to imminent lawless action, true threats of violence, defamation — these appear to be natural limits on speech itself, not contingent institutional creations. This perspective sees categorical balancing as discovering these natural boundaries. However, the structural data reveals this as a false summit: the beneficiaries are institutional actors maintaining interpretive control, and victims are marginalized speakers and legal predictability — both pointing to extraction, not natural law.
constraint_indexing:constraint_classification(first_amendment_speech_protection__categorical_balancing_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(first_amendment_speech_protection__categorical_balancing_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(first_amendment_speech_protection__categorical_balancing_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, TR),
    TR >= 0.70.

:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The categorical balancing framework creates asymmetric power relations: the judiciary determines category boundaries unilaterally, and speakers face suppression without direct input into the balancing process. However, extractiveness is not maximal (0.72+) because the doctrine includes genuine coordination function (establishing some categories as clearly protected, enabling predictable speech conduct) and because the beneficiary (judiciary) is using balancing language that references legitimate interests (harm prevention) rather than openly asserting extraction. The measurement trajectory shows extraction increasing over the interval as new fact patterns require rebalancing, expanding suppression of emerging speech forms. Suppression (0.62): High. Barriers to suppressed speech include legal prohibition (disfavored categories are unlawful), enforcement through prosecution, and category assignment system that places burden of legal risk on marginal speakers. Speakers have no formal mechanism to challenge category membership except through appellate litigation, which is costly and risky. Theater ratio (0.68): Moderate-high. Judicial balancing language performs neutrality and reasoned deliberation, yet enforcement apparatus often operates under the assumption that certain categories (incitement, true threats, obscenity) are already settled, reducing the actual balancing work. Law enforcement conducts surveillance and suppression justified post-hoc through categorical reasoning. The performative element has increased as the balancing framework has matured — courts refine the tests, but enforcement proceeds on settled-category assumptions.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives reveal a full DR spectrum from a single set of base properties. The institutional judiciary sees pure coordination (Rope) — the balancing framework enables dispute resolution within their preferred institutional logic. The mainstream press sees mixed coordination and extraction (Tangled Rope) — they benefit from established protected categories but face uncertainty from ongoing rebalancing. Marginalized speakers see pure extraction (Snare) — their speech is categorically suppressed with minimal recourse. The organized civil liberties coalition sees a temporary framework under pressure (Scaffold) — legal doctrine is evolving toward different standards. Law enforcement sees an inert institutional framework (Piton) — the categorical balancing provides performative cover for enforcement that would occur on settled-category assumptions. The analytical observer risks seeing natural boundaries (Mountain) — incitement and true threats appear inherent to any speech system — but the structural data (identifiable beneficiaries in judiciary, clear victims in marginalized speakers) reveals this as a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are computed from each agent's structural position relative to the constraint. The institutional judiciary (beneficiary + arbitrage exit) derives low d, producing negative effective extraction (they benefit). Marginalized speakers (victim + trapped exit) derive high d, producing maximum effective extraction (they bear the full cost). Mainstream press (mixed position + constrained exit) derives moderate d, producing moderate extraction. Organized civil liberties advocates (active agent + constrained exit) derive moderate-high d but with some benefit from the coalition's ability to influence doctrine, reducing pure extraction. The analytical observer derives d from the presumption of non-participation, producing a moderate-high value. The false-summit detector will flag the mountain classification because beneficiaries and victims are present — the 'natural law' framing naturalizes what is actually an institutional distribution of interpretive power.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through committer structure: the categorical_balancing_reading is ONE valid interpretation of the contested kernel. The reading is internally coherent — it describes actual Supreme Court doctrine accurately. The tension between readings (categorical balancing vs. absolutist vs. harm-limited) is not a sign that the reading is wrong; it is evidence that the kernel is genuinely contested and that different parties hold different readings. The extracted information is: (1) which reading is instantiated here (categorical_balancing); (2) what structural consequences follow from this reading (judiciary benefits, marginalized speakers lose, legal predictability suffers); (3) what alternative readings would change (absolutist would flip beneficiaries toward speakers and marginalized groups; harm-limited would eliminate category system entirely). The engine's classification of this reading as Tangled Rope (rather than Mountain or pure Rope) reflects that the reading combines genuine judicial coordination function with asymmetric extraction of interpretive power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_boundary_stability,
    'Are the boundaries between protected and unprotected speech categories stable doctrinal settlements or continuously destabilized by new fact patterns and judicial rebalancing?',
    'Longitudinal analysis of Supreme Court speech doctrine: mapping category boundary shifts over decades; counting reversals or narrowings of established categories; documenting new categories created through novel balancing tests',
    'If stable: the categories function as coordinate system for speakers (Rope from mainstream speaker perspective). If continuously destabilized: speakers face perpetual uncertainty about which category applies to new circumstances, and the judiciary retains unilateral rebalancing power (Tangled Rope or Snare from marginalized speaker perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_boundary_stability, empirical, 'Stability of protected/unprotected category boundaries over time').

omega_variable(
    categorization_neutrality_myth,
    'Is case-by-case balancing genuinely content-neutral, or do the balancing factors systematically weight toward suppressing disfavored political speech?',
    'Statistical analysis of Supreme Court balancing outcomes: coding the direction of balance (protection or suppression) by speech content category (political, religious, commercial, expressive conduct); controlling for harm magnitude; testing for systematic bias in weight assignment to ''speech value'' vs ''harm prevention''',
    'If genuinely neutral: balancing is coordination mechanism (Rope, Scaffold). If systematically biased: balancing is extraction disguised as neutral doctrine (Snare from disfavored speaker perspective; False Summit from analytical perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorization_neutrality_myth, empirical, 'Whether categorical balancing is content-neutral or systematically biased').

omega_variable(
    judicial_review_predictability,
    'Can speakers predict with reasonable confidence whether speech falling in a borderline category will be deemed protected or unprotected, or does case-by-case balancing produce unpredictable outcomes?',
    'Empirical legal analysis: Supreme Court amicus briefs and lower court decisions in speech cases; interviews with legal practitioners; measurement of consistency in outcome prediction for novel borderline cases; citation patterns showing reliance on balancing tests for category-membership determination',
    'If predictable: the categorical system provides legal certainty (Rope, Tangled Rope). If unpredictable: speakers and lower courts face perpetual uncertainty, and the judiciary retains discretionary power (evidence for Snare and False Summit classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_review_predictability, empirical, 'Predictability of categorical balancing outcomes for speakers').

omega_variable(
    doctrinal_evolution_trajectory,
    'Is the categorical balancing framework evolving toward broader protection (absolutism), narrower protection (harm-based approach), or remaining in stable dynamic equilibrium?',
    'Bibliometric analysis of Supreme Court doctrine and legal scholarship: tracking citation patterns of categorical balancing cases; measuring shift in center of gravity of doctrinal discussion; identifying emerging sub-categories and their trajectory; analyzing whether recent cases expand or contract protected categories',
    'If moving toward absolutism: categorical balancing is being sunset by doctrinal drift (supports Scaffold perspective). If moving toward harm-based approach: the categorical system is being replaced by a different constraint (structural transition). If stable equilibrium: the constraint persists (supports ongoing classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_evolution_trajectory, empirical, 'Direction and magnitude of doctrinal evolution of categorical balancing framework').

omega_variable(
    reading_underdetermination,
    'Which reading of the First Amendment kernel best describes the current U.S. legal doctrine: categorical balancing, absolutist categorical protection, or harm-limited exception?',
    'Doctrinal analysis of Supreme Court precedent: identify which reading best explains the most recent decisions; determine whether the Court applies categorical balancing, invokes categorical protection, or uses harm-only standards as the primary framework; note whether doctrine is internally coherent or contains unresolved tensions between readings',
    'If categorical balancing is the best fit: this reading''s classification stands. If absolutist reading better explains the doctrine: reclassify as Mountain (natural rights reading). If harm-limited reading dominates: reclassify as Snare (harm principle dominates protection). The actual doctrinal state reveals whether readings coexist or one forecloses another in practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_underdetermination, conceptual, 'Which reading of the First Amendment kernel best fits current Supreme Court doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fa_cat_bal_theater_t0, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(fa_cat_bal_theater_t25, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 25, 0.62).
narrative_ontology:measurement(fa_cat_bal_theater_t50, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(fa_cat_bal_extract_t0, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fa_cat_bal_extract_t25, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(fa_cat_bal_extract_t50, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fa_cat_bal_suppress_t0, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(fa_cat_bal_suppress_t25, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(fa_cat_bal_suppress_t50, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested First Amendment kernel. The three readings (categorical_balancing, absolutist, harm_limited) represent different structural interpretations of the same legal text. Each reading is instantiated as a separate constraint story with its own ε value and perspectives. The categorical_balancing reading shows ε=0.58 (Tangled Rope); the absolutist reading is expected to show ε ≈ 0.15 (Mountain/Rope); the harm_limited reading is expected to show ε ≈ 0.65 (Snare/Tangled Rope). The readings coexist in current doctrine, with the categorical_balancing reading dominant in Supreme Court precedent. Links in network.affects_constraints point to sibling readings and enable the engine to model doctrinal interaction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__categorical_balancing_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
