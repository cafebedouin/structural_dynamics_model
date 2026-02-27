% ============================================================================
% CONSTRAINT STORY: sapir_whorf_hypothesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sapir_whorf_hypothesis, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sapir_whorf_hypothesis
 *   human_readable: Sapir-Whorf Hypothesis (Linguistic Relativity)
 *   domain: social/cognitive
 *
 * SUMMARY:
 *   The Sapir-Whorf hypothesis proposes that the structure of a language
 *   affects its speakers' worldview and cognition. Originally articulated by
 *   Edward Sapir and Benjamin Whorf in the early-to-mid 20th century, the
 *   hypothesis has functioned as a constraint on linguistic and cognitive
 *   science research, policy justification, and cross-cultural understanding.
 *   The constraint exhibits the full perspectival range: minority language
 *   speakers experience it as extraction justified by cognitive determinism
 *   (Snare); cross-cultural researchers experience it as a useful explanatory
 *   framework with epistemological risks (Tangled Rope); academic linguists
 *   benefit from it as an organizing principle (Rope); language preservation
 *   movements have instrumentally invoked it with an implicit sunset clause
 *   as psychology advances (Scaffold); educational policy systems maintain it
 *   through institutional inertia despite weak empirical support (Piton); and
 *   from a civilizational analytical view, it risks naturalizing a contingent
 *   empirical claim as immutable (Mountain). The extractiveness value (0.52)
 *   reflects that the hypothesis functions as both a coordination mechanism
 *   for comparative linguistics and an extraction mechanism for justifying
 *   language loss and educational assimilation policies. The theater ratio
 *   (0.65) indicates that contemporary invocations of Sapir-Whorf in policy
 *   and pedagogy are substantially performative — the hypothesis is cited as
 *   justification but the actual causal mechanism remains empirically
 *   unsupported.
 *
 * KEY AGENTS:
 *   - Linguistic Relativists: Beneficiaries (institutional/arbitrage) — maintain the hypothesis as canonical framework, secure research funding and theoretical authority through its perpetuation
 *   - Language Minorities: Primary victims (powerless/trapped) — language loss and assimilation policies justified by cognitive determinism framing; cannot exit the constraint through individual linguistic choice
 *   - Linguistic Universalists: Secondary victims (moderate/constrained) — their empirical findings (cognitive universals, universal semantic features) are reinterpreted within relativity frameworks; constrained in theoretical space
 *   - Cross-Cultural Researchers: Mixed position (moderate/constrained) — benefit from the hypothesis as explanatory framework but constrained by unfalsifiability and confirmation bias risks
 *   - Academic Linguistics Establishment: Institutional beneficiary (institutional/arbitrage) — reproduces the hypothesis through textbooks, curricula, funding prioritization; arbitrage exits enable alternatives to emerge without institutional commitment
 *   - Language Preservation Movement: Organized agents (organized/constrained) — initially relied on Sapir-Whorf justification; now transitioning to cultural heritage, ecosystem, and epistemic pluralism justifications (sunset in progress)
 *   - Educational Policy Systems: Institutional piton (institutional/arbitrage) — maintains Sapir-Whorf embedded assumptions in curricula despite weak empirical support; theater persists through cultural identity and institutional continuity rather than functional cognitive mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sapir_whorf_hypothesis, 0.52).
domain_priors:suppression_score(sapir_whorf_hypothesis, 0.48).
domain_priors:theater_ratio(sapir_whorf_hypothesis, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sapir_whorf_hypothesis, extractiveness, 0.52).
narrative_ontology:constraint_metric(sapir_whorf_hypothesis, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sapir_whorf_hypothesis, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sapir_whorf_hypothesis, tangled_rope).
narrative_ontology:human_readable(sapir_whorf_hypothesis, "Sapir-Whorf Hypothesis (Linguistic Relativity)").
narrative_ontology:topic_domain(sapir_whorf_hypothesis, "social/cognitive").

domain_priors:requires_active_enforcement(sapir_whorf_hypothesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sapir_whorf_hypothesis, linguistic_relativists).
narrative_ontology:constraint_beneficiary(sapir_whorf_hypothesis, language_planning_institutions).
narrative_ontology:constraint_victim(sapir_whorf_hypothesis, linguistic_universalists).
narrative_ontology:constraint_victim(sapir_whorf_hypothesis, language_minorities).
narrative_ontology:constraint_victim(sapir_whorf_hypothesis, cross_cultural_understanding).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LANGUAGE MINORITY SPEAKER (SNARE) — Speakers of minority languages face the claim that their language constrains their cognition. This creates a self-fulfilling extraction mechanism: policy decisions to abandon minority languages are justified by the hypothesis itself, while the loss of linguistic diversity is framed as inevitable cognitive limitation rather than structural extraction. The speaker cannot exit this constraint through linguistic choice — the hypothesis follows them across languages.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CROSS-CULTURAL RESEARCHER (TANGLED ROPE) — Benefits from the hypothesis as an explanatory framework for cognitive variation (coordination function) but also constrained by its unprovable claims and the risk of confirmation bias in interpretation. The researcher experiences both coordination (comparative linguistics provides explanatory structure) and extraction (pressure to find linguistic determinism to validate theories).
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ACADEMIC LINGUISTICS ESTABLISHMENT (ROPE) — Benefits from the hypothesis as a canonical framework that structures linguistic research programs, funding allocation, and theory development. Sees the constraint primarily as coordination: the hypothesis provides a unifying concept for comparative linguistics and enables institutional reproduction through textbooks and curricula.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LANGUAGE PRESERVATION MOVEMENT (SCAFFOLD) — Organized agents (UNESCO, linguistic archives, indigenous language councils) initially invoked the Sapir-Whorf hypothesis to justify language preservation — the unique cognitive perspective in each language justified documentation efforts. This represents a sunset clause: as empirical psychology and neuroscience advance, the preservation justification shifts from cognitive determinism to cultural heritage, ecosystem value, and epistemic pluralism. The constraint's extraction mechanism weakens as alternatives to linguistic relativity emerge.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EDUCATIONAL POLICY FRAMEWORK (PITON) — Language education policy has embedded Sapir-Whorf assumptions (linguistic structure shapes thought, so native language instruction is essential) despite weak empirical support. The policy persists through institutional inertia: it is cited in curricula and training frameworks even as cognitive science evidence contradicts it. The theater is high because the policy's justification has become performative — maintained for reasons of cultural identity and institutional continuity rather than because the cognitive mechanism actually works as claimed.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some constraint between language structure and cognition might appear inevitable: humans think through language, language has structure, therefore language structure must affect thought. This perspective risks naturalizing a contingent empirical claim as an immutable feature of cognition itself. However, the structural data contradicts the mountain classification — the hypothesis's enforcement, beneficiaries, and victims reveal it as a social/institutional constraint, not a natural law.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sapir_whorf_hypothesis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sapir_whorf_hypothesis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sapir_whorf_hypothesis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sapir_whorf_hypothesis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sapir_whorf_hypothesis, TR),
    TR >= 0.70.

:- end_tests(sapir_whorf_hypothesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The hypothesis extracts from minority language communities by justifying language loss through cognitive determinism (the claim that losing a language loses a unique cognitive perspective). It also extracts through the enforcement of assimilation policies framed as cognitive necessity. However, extractiveness has declined over the interval (0.65 → 0.52) as empirical psychology and neuroscience have accumulated evidence for cognitive universals and reduced the hypothesis's explanatory power. Suppression (0.48): Moderate. The hypothesis suppresses through: (1) academic gatekeeping (alternative universalist perspectives are marginalized in linguistics curricula), (2) framing language loss as inevitable due to cognitive constraints rather than policy choices, and (3) embedding the assumption in policy and pedagogy where it is difficult to challenge directly. But suppression is not total — empirical counterevidence exists, alternative theoretical frameworks are available, and organized preservation movements have carved out institutional space. Theater ratio (0.65): High and rising. Contemporary invocations of Sapir-Whorf in educational policy, language preservation advocacy, and popular cognitive science are substantially performative. The hypothesis is cited because it provides an appealing narrative (linguistic diversity = cognitive diversity) rather than because the causal mechanism is empirically validated. Policy justifications invoke the hypothesis without updating for decades of cognitive science challenging its strong form.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps reflect conflicting structural positions relative to the hypothesis's extraction mechanisms. The linguistic establishment sees the hypothesis as coordination (Rope) — it structures research programs, enables comparative analysis, and produces publishable work. Minority language speakers see it as extraction (Snare) — it justifies policies that eliminate their languages while naturalizing the elimination as cognitive inevitability. Cross-cultural researchers see mixed coordination and extraction (Tangled Rope) — the hypothesis enables comparative work but constrains them through unfalsifiability. The preservation movement sees a temporary constraint with a sunset (Scaffold) — alternative justifications for preservation (cultural heritage, ecosystem value, epistemic pluralism) have matured enough to carry the burden without the hypothesis. Educational policy systems see a degraded ritual (Piton) — the hypothesis persists in curricula despite weak empirical support because institutions don't cost the shift to alternatives. The civilizational analytical view risks treating the hypothesis as natural law (Mountain) — 'language shapes thought' can appear inevitable — but the structural data reveals it as contingent institutional constraint enforced by academic and policy gatekeeping.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the hypothesis's extraction flows. Beneficiaries of the hypothesis (academic linguists, institutional gatekeepers) experience low directionality (d ≈ 0.1-0.2) because they benefit from its perpetuation and can arbitrage (shift to other explanatory frameworks if needed). Victims of the hypothesis (language minorities, universalists constrained in theoretical space) experience high directionality (d ≈ 0.8-0.95) because they bear the costs of its enforcement and have limited exit options. Cross-cultural researchers experience moderate directionality (d ≈ 0.5) because they both benefit from the explanatory framework and are constrained by its empirical weaknesses. The open science coalition (language preservation movement) has engineered a sunset by decoupling preservation justification from the hypothesis's empirical claims, reducing their experienced directionality over time. The piton perspective (educational policy) maintains inertial directionality despite declining functional basis.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the Sapir-Whorf hypothesis functions simultaneously as coordination (comparative linguistics framework) and extraction (justification for language loss and assimilation). The tangled rope classification captures this hybrid: the hypothesis genuinely enables some comparative linguistics research (coordination) while simultaneously justifying the elimination of minority languages (extraction). The tension between these functions explains the perspectival gaps and the declining extractiveness over the interval — as cognitive science accumulated evidence for universals and as language preservation movements built alternative justifications, the extraction mechanism weakened while the coordination function remained useful. The theater ratio increase (0.35 → 0.65) reflects Goodhart drift: as the hypothesis's empirical basis eroded, its use in policy became increasingly performative — cited for its intuitive appeal and cultural resonance rather than scientific validity. The mountain perspective is identified as a false summit through the structural data: the claim that 'language shapes thought' naturalizes what is actually a contingent institutional choice about how to frame language policy and cognitive science. The hypothesis is not a law of nature but an extractive framework supported by institutional gatekeeping, now in decline as empirical psychology matured and policy alternatives emerged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    weak_vs_strong_formulation,
    'Does the Sapir-Whorf hypothesis assert linguistic determinism (strong: language determines thought) or merely linguistic influence (weak: language influences thought)? Are these the same constraint or two structurally distinct constraints?',
    'Textual analysis of original Sapir and Whorf writings; separation of deterministic vs correlational claims; empirical testing of each formulation independently',
    'Strong formulation: ε ≈ 0.60, Snare from minority speakers'' perspective. Weak formulation: ε ≈ 0.25, Rope from comparative linguists'' perspective. If these are two separate constraints, the constraint family should decompose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(weak_vs_strong_formulation, conceptual, 'Whether strong vs weak formulations are distinct constraints').

omega_variable(
    empirical_falsifiability,
    'What experimental evidence would falsify the Sapir-Whorf hypothesis at either strength level? Is the hypothesis empirically testable or does it remain unfalsifiable because language and thought are entangled by definition?',
    'Meta-analysis of cognitive psychology experiments (color perception, spatial reasoning, temporal reasoning) testing linguistic influence; identification of what evidence would be accepted as falsification by proponents and critics',
    'If unfalsifiable: ε ≈ 0.70 (high extraction through unfalsifiability mechanism). If testable: ε ≈ 0.35 (moderate constraint dependent on empirical outcomes). Theater ratio reflects unfalsifiability directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_falsifiability, empirical, 'Whether the hypothesis is empirically testable').

omega_variable(
    universality_of_refutation,
    'Do cognitive universals (Pinker''s language instinct, universal semantic features across languages, domain-specific modules in cognition) definitively refute the Sapir-Whorf hypothesis, or can weak formulations coexist with universalist findings?',
    'Examination of whether cognitive universals and linguistic influence are mutually exclusive or complementary; literature review of contemporary cognitive science positions',
    'If mutually exclusive and universals are confirmed: hypothesis is refuted, extractiveness drops to 0.15 (residual institutional inertia only — piton status). If compatible: hypothesis persists in weak form, ε ≈ 0.35.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universality_of_refutation, empirical, 'Whether universals refute the hypothesis').

omega_variable(
    policy_decoupling_timeline,
    'How quickly will language policy institutions decouple the Sapir-Whorf justification for language preservation from the actual preservation efforts? Does language preservation require the hypothesis to be true?',
    'Analysis of UNESCO, linguistic archive, and indigenous language council policy documents; tracking shifts in stated justifications for preservation over time; surveys of language preservation advocates on whether cognitive determinism is necessary',
    'If rapid decoupling: scaffold sunset clause is real and imminent (hypothesis loses extractive force in policy within 5-10 years). If slow/incomplete: hypothesis remains embedded in institutional justification indefinitely (piton status becomes permanent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_decoupling_timeline, empirical, 'Decoupling speed of policy justification from empirical hypothesis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sapir_whorf_hypothesis, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sw_tr_t0, sapir_whorf_hypothesis, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sw_tr_t20, sapir_whorf_hypothesis, theater_ratio, 20, 0.5).
narrative_ontology:measurement(sw_tr_t50, sapir_whorf_hypothesis, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(sw_be_t0, sapir_whorf_hypothesis, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(sw_be_t20, sapir_whorf_hypothesis, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(sw_be_t50, sapir_whorf_hypothesis, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sapir_whorf_hypothesis, information_standard).
narrative_ontology:affects_constraint(sapir_whorf_hypothesis, linguistic_assimilation_policy).
narrative_ontology:affects_constraint(sapir_whorf_hypothesis, cognitive_universals_hypothesis).

% DUAL FORMULATION NOTE:
% The Sapir-Whorf hypothesis decomposes into two structurally distinct constraints: (1) weak linguistic relativity (language influences thought) ε ≈ 0.25, Rope; (2) strong linguistic determinism (language determines thought) ε ≈ 0.60, Snare. The current story models the institutional constraint that enforces rhetorical invocation of both formulations. The decomposed constraints should track the empirical refutation of the strong form and the evolution of the weak form as a legitimate research program.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sapir_whorf_hypothesis, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
