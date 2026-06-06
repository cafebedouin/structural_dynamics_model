% ============================================================================
% CONSTRAINT STORY: necessity_ambiguity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_necessity_ambiguity, []).

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
 *   constraint_id: necessity_ambiguity
 *   human_readable: Necessity Ambiguity in Dirty Hands Justification
 *   domain: political_philosophy/normative_ethics/applied_ethics
 *
 * SUMMARY:
 *   The necessity ambiguity in dirty hands justification represents a
 *   structural tension in political ethics: the concept of 'necessity' must
 *   be flexible enough to accommodate genuine moral dilemmas while
 *   constrained enough to prevent post-hoc rationalization of unjustified
 *   harms. From Hobbes's self-preservation necessity (near-deterministic,
 *   minimal constraint) through Walzer's supreme emergency (restrictive, high
 *   threshold) to contemporary debates over targeted killing and torture, the
 *   ambiguity has persisted across 350+ years of philosophical development.
 *   This constraint exhibits tangled rope structure: it genuinely coordinates
 *   moral discourse across incommensurable frameworks (deontology,
 *   consequentialism, virtue ethics all engage with necessity claims) while
 *   simultaneously enabling extraction (expansive applications that undermine
 *   restrictive criteria). The theater ratio (0.48) reflects moderate
 *   performativity: necessity claims involve genuine moral reasoning but also
 *   strategic invocation of ambiguous criteria to justify preferred actions.
 *   The constraint's extractiveness and suppression have increased over the
 *   interval as the scope of state action expanded and necessity claims
 *   migrated from existential threats (Hobbes's state of nature, Walzer's
 *   WWII) to routine security operations (contemporary counterterrorism).
 *
 * KEY AGENTS:
 *   - Victims of Premature Necessity Claims: Primary victims (powerless/trapped) — individuals subjected to rights violations justified by expansive necessity interpretations; cannot exit or contest the determination
 *   - Restrictive Criteria Advocates: Secondary victims (moderate/constrained) — philosophers and legal theorists advocating narrow necessity definitions; benefit from coordination function but bear extraction through ambiguity's enabling of expansive applications
 *   - Executive Discretion Advocates: Primary beneficiaries (institutional/arbitrage) — state actors and political realists who benefit from interpretive flexibility; can shift between necessity framings contextually
 *   - International Human Rights Coalition: Organized victims (organized/constrained) — attempting to codify restrictive criteria in international law; benefit from necessity discourse but bear extraction through state violations
 *   - Codification Movement: Organized agents with sunset logic (organized/mobile) — legal theorists working toward operationalizable criteria; see ambiguity as temporary coordination failure
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees irreducible tension between coordination function and extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(necessity_ambiguity, 0.58).
domain_priors:suppression_score(necessity_ambiguity, 0.62).
domain_priors:theater_ratio(necessity_ambiguity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(necessity_ambiguity, extractiveness, 0.58).
narrative_ontology:constraint_metric(necessity_ambiguity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(necessity_ambiguity, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(necessity_ambiguity, tangled_rope).
narrative_ontology:human_readable(necessity_ambiguity, "Necessity Ambiguity in Dirty Hands Justification").
narrative_ontology:topic_domain(necessity_ambiguity, "political_philosophy/normative_ethics/applied_ethics").

domain_priors:requires_active_enforcement(necessity_ambiguity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(necessity_ambiguity, expansive_dirty_hands_application).
narrative_ontology:constraint_beneficiary(necessity_ambiguity, executive_discretion_advocates).
narrative_ontology:constraint_beneficiary(necessity_ambiguity, realist_political_theorists).
narrative_ontology:constraint_victim(necessity_ambiguity, restrictive_criteria_advocates).
narrative_ontology:constraint_victim(necessity_ambiguity, deontological_constraint_frameworks).
narrative_ontology:constraint_victim(necessity_ambiguity, victims_of_premature_necessity_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VICTIM OF PREMATURE NECESSITY CLAIMS (SNARE) — Individual subjected to rights violations justified by expansive necessity claims has no exit from the constraint. Cannot contest the necessity determination ex ante, cannot escape the harm, and faces suppression of alternative framings. Experiences maximum extraction: the ambiguity enables harm that would not survive stricter necessity criteria.
constraint_indexing:constraint_classification(necessity_ambiguity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RESTRICTIVE CRITERIA ADVOCATE (TANGLED ROPE) — Philosopher or legal theorist advocating for narrow necessity definitions (Walzer's supreme emergency, threshold deontology). Benefits from the necessity discourse as a coordination mechanism (shared vocabulary for moral constraint) but bears extraction through the ambiguity's enabling of expansive applications that undermine restrictive frameworks. Constrained exit: can argue within the discourse but cannot escape the ambiguity's structural effects on policy.
constraint_indexing:constraint_classification(necessity_ambiguity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE DISCRETION ADVOCATE (ROPE) — State actors and political realists who benefit from expansive necessity interpretations. The ambiguity functions as coordination: provides shared justificatory vocabulary while preserving flexibility. Arbitrage exit: can shift between necessity framings (deterministic, best-available, strong-preference) depending on context. Experiences the constraint as net beneficial coordination with minimal extraction.
constraint_indexing:constraint_classification(necessity_ambiguity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL HUMAN RIGHTS COALITION (TANGLED ROPE) — Organized actors (Amnesty International, Human Rights Watch, ICC) attempting to codify restrictive necessity criteria in international law. Benefits from necessity discourse as coordination mechanism (enables cross-border moral claims) but bears extraction through the ambiguity's enabling of state violations justified as 'necessary.' Constrained exit: can advocate for codification but cannot escape the ambiguity's persistence in state practice.
constraint_indexing:constraint_classification(necessity_ambiguity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CODIFICATION MOVEMENT (SCAFFOLD) — Subset of legal theorists and international law scholars working toward operationalizable necessity criteria (proportionality tests, imminence requirements, least-restrictive-means analysis). Sees the ambiguity as temporary coordination failure with sunset logic: as case law accumulates and criteria crystallize, the ambiguity's extraction mechanism loses force. Mobile exit: can shift to other normative frameworks if codification fails.
constraint_indexing:constraint_classification(necessity_ambiguity, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/universal perspective, the necessity ambiguity serves genuine coordination function (enables moral discourse across incommensurable frameworks) while simultaneously enabling extraction (expansive applications that would not survive disambiguation). The ambiguity is not resolvable through better philosophy alone — it reflects irreducible tension between ex ante constraint and ex post justification, between rule-following and consequentialist override. Tangled rope at analytical level: the constraint genuinely coordinates AND extracts.
constraint_indexing:constraint_classification(necessity_ambiguity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(necessity_ambiguity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(necessity_ambiguity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(necessity_ambiguity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(necessity_ambiguity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(necessity_ambiguity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The ambiguity enables rights violations that would not survive stricter necessity criteria, but the extraction is not maximal because some necessity claims are genuine (Walzer's WWII cases) and the discourse does constrain some actors. The value reflects substantial but not total extraction. Increased from 0.45 (Hobbes era, when necessity claims were mostly existential) to 0.58 (contemporary, when necessity migrated to routine operations). Suppression (0.62): Moderate-high. Victims of necessity claims face significant barriers to contesting determinations: epistemic asymmetry (state has superior information), temporal pressure (necessity claims are urgent), institutional capture (courts defer to executive on security), and conceptual ambiguity itself (no clear criteria to invoke). But suppression is not total — some necessity claims are successfully challenged ex post, and restrictive criteria advocates have institutional platforms. Increased from 0.50 to 0.62 as state capacity and security discourse expanded. Theater ratio (0.48): Moderate. Necessity claims involve genuine moral reasoning (actors do face dilemmas, alternatives are genuinely constrained in some cases) but also strategic invocation of ambiguous criteria. The performative element is substantial but not dominant. Increased from 0.35 to 0.48 as necessity discourse became more institutionalized and necessity claims more routine.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same ambiguity serves different structural functions from different positions. Executive discretion advocates experience rope (beneficial coordination with flexibility). Restrictive criteria advocates experience tangled rope (coordination function undermined by extraction through expansive application). Victims experience snare (trapped in harm justified by ambiguous criteria they cannot contest). The codification movement sees scaffold (temporary problem with sunset as criteria crystallize). The analytical observer sees tangled rope at civilizational scale (irreducible tension between coordination and extraction). The gap is not resolvable by better philosophy alone — it reflects structural positions relative to the ambiguity's dual function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Victims of premature necessity claims are full targets (d near 1.0): trapped exit, no beneficiary status, bear direct harm from expansive applications. Restrictive criteria advocates are moderate targets (d around 0.6): constrained exit, mixed position (benefit from coordination, harmed by extraction), can argue but cannot escape structural effects. Executive discretion advocates are beneficiaries (d near 0.2): arbitrage exit, primary beneficiaries of interpretive flexibility, experience constraint as enabling rather than limiting. International human rights coalition is moderate target (d around 0.5): organized but constrained, benefit from necessity discourse but bear extraction through state violations. Codification movement is low target (d around 0.3): mobile exit, see ambiguity as temporary, can shift frameworks if codification fails. Analytical observer has analytical directionality (d context-dependent): sees both coordination and extraction functions.
 *
 * MANDATROPHY ANALYSIS:
 *   The necessity ambiguity resolves mandatrophy by demonstrating that tangled rope classification at the analytical level is not a failure of disambiguation but a structural feature. The ambiguity genuinely coordinates (enables moral discourse across frameworks, provides shared vocabulary for constraint) AND genuinely extracts (enables expansive applications, suppresses restrictive criteria, harms victims of premature claims). Attempts to resolve the ambiguity face a trilemma: (1) deterministic necessity is too restrictive (rules out genuine dilemmas), (2) optimality necessity is empirically contested and epistemically demanding, (3) strong-preference necessity loses constraining force. The ambiguity persists because each resolution path has unacceptable costs. The constraint's mandate (enable moral reasoning about necessity) has not outlived its function, but the function itself is dual: coordination and extraction are inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    determinism_vs_optionality,
    'Does ''necessity'' require causal determinism (no alternative was possible), optimality (no better alternative existed), or merely strong preference (this was the preferred option among available alternatives)?',
    'Conceptual analysis of necessity claims across historical cases; identification of which interpretation best predicts when actors and observers accept necessity justifications',
    'If determinism required: most dirty hands claims fail (very few situations have literally no alternatives). If optimality required: necessity becomes empirically testable but highly contested. If strong preference sufficient: necessity loses constraining force and becomes post-hoc rationalization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(determinism_vs_optionality, conceptual, 'What threshold constitutes genuine necessity').

omega_variable(
    ex_ante_vs_ex_post_asymmetry,
    'Is necessity properly evaluated ex ante (what alternatives appeared available to the actor at decision time) or ex post (what alternatives were actually available given full information)?',
    'Analysis of legal doctrine (reasonable person standard vs objective standard); philosophical debate between epistemic and metaphysical necessity',
    'If ex ante: necessity becomes subjective and difficult to falsify (actor''s claimed perception is dispositive). If ex post: necessity becomes objective but potentially unfair (judges actor by information they could not have had).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ex_ante_vs_ex_post_asymmetry, conceptual, 'Temporal frame for necessity evaluation').

omega_variable(
    codification_feasibility,
    'Can necessity criteria be operationalized into clear legal tests, or does the concept''s utility depend on retaining interpretive flexibility?',
    'Empirical study of jurisdictions that have attempted necessity codification (proportionality tests in European human rights law, imminence requirements in self-defense doctrine); comparison of false positive and false negative rates',
    'If codifiable: scaffold perspective confirmed — ambiguity is temporary and solvable through legal development. If inherently flexible: ambiguity is structural feature, not bug, and extraction is unavoidable cost of the concept''s coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(codification_feasibility, empirical, 'Whether necessity can be operationalized without losing function').

omega_variable(
    walzer_threshold_coherence,
    'Is Walzer''s ''supreme emergency'' threshold (imminent threat to community survival) a coherent limiting principle, or does it collapse into expansive application under pressure?',
    'Historical analysis of supreme emergency invocations (Churchill''s WWII decisions, Israeli targeted killings, post-9/11 torture debates); identification of scope creep patterns',
    'If coherent: restrictive criteria are viable and the ambiguity is resolvable. If incoherent: even the most restrictive formulations fail under real-world pressure, suggesting the ambiguity is structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(walzer_threshold_coherence, empirical, 'Whether restrictive necessity criteria remain stable under application').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(necessity_ambiguity, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nec_amb_theater_hobbes_era, necessity_ambiguity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nec_amb_theater_walzer_era, necessity_ambiguity, theater_ratio, 150, 0.42).
narrative_ontology:measurement(nec_amb_theater_contemporary, necessity_ambiguity, theater_ratio, 200, 0.48).

% Extraction over time
narrative_ontology:measurement(nec_amb_extract_hobbes_era, necessity_ambiguity, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nec_amb_extract_walzer_era, necessity_ambiguity, base_extractiveness, 150, 0.52).
narrative_ontology:measurement(nec_amb_extract_contemporary, necessity_ambiguity, base_extractiveness, 200, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(nec_amb_suppress_hobbes_era, necessity_ambiguity, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(nec_amb_suppress_walzer_era, necessity_ambiguity, suppression_requirement, 150, 0.58).
narrative_ontology:measurement(nec_amb_suppress_contemporary, necessity_ambiguity, suppression_requirement, 200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(necessity_ambiguity, identity_coordination).
narrative_ontology:affects_constraint(necessity_ambiguity, dirty_hands_doctrine).
narrative_ontology:affects_constraint(necessity_ambiguity, supreme_emergency_threshold).
narrative_ontology:affects_constraint(necessity_ambiguity, proportionality_principle).

% DUAL FORMULATION NOTE:
% The necessity ambiguity is upstream of specific dirty hands doctrines (Walzer's supreme emergency, Nagel's moral costs, Coady's consequentialist override) but represents a distinct structural constraint. The downstream constraints have their own extractiveness values reflecting their specific threshold criteria; the necessity ambiguity has its own extractiveness reflecting the conceptual under-determination that enables expansive application across all downstream doctrines.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
