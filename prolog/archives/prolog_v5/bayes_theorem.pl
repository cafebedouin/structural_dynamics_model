% ============================================================================
% CONSTRAINT STORY: bayes_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bayes_theorem, []).

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
 *   constraint_id: bayes_theorem
 *   human_readable: Bayesian Reasoning Bias (Base Rate Neglect)
 *   domain: social/cognitive
 *
 * SUMMARY:
 *   Base rate neglect is a cognitive bias where individuals systematically
 *   underweight or ignore statistical base rate information (prior
 *   probabilities) when making judgments, relying instead on salient case
 *   information or vivid exemplars. This constraint operates at the
 *   intersection of individual cognition and institutional exploitation.
 *   Individual reasoners cannot escape the bias through awareness alone — it
 *   persists even in trained statisticians when cognitive load is high or
 *   information is presented in low-salience formats. Simultaneously,
 *   institutional actors (marketers, risk communicators, political
 *   persuaders, insurance companies) navigate the bias strategically —
 *   sometimes exploiting it for persuasion, sometimes fighting it through
 *   redesigned communication formats. The constraint exhibits a perspectival
 *   spread: individual reasoners experience it as an inescapable snare;
 *   persuaders experience it as coordination opportunity (rope); debiasing
 *   institutions see a temporary coordination problem solvable through
 *   scaffolding; formal logic establishments maintain ceremonial commitment
 *   to Bayesian reasoning while actual practice remains heuristic-driven
 *   (piton); and from a civilizational view, the bias might be a rational
 *   adaptation to ancestral information environments rather than a defect
 *   (false mountain). The theater ratio has increased over the measured
 *   interval as institutional persuasion has become more sophisticated at
 *   exploiting base rate neglect while simultaneously debiasing interventions
 *   have proliferated.
 *
 * KEY AGENTS:
 *   - Individual Reasoners: Primary victims (powerless/trapped) — cannot escape the bias through awareness; require external support
 *   - Institutional Persuaders: Primary beneficiaries (institutional/arbitrage) — exploit the bias for marketing, political messaging, and risk communication persuasion
 *   - Risk Communicators: Mixed position (moderate/constrained) — public health, insurance, and regulatory professionals must work within the bias while attempting to overcome it
 *   - Debiasing Institutions: Organized agents (organized/constrained) — educational programs, decision support systems, expert institutions providing scaffolding solutions
 *   - Formal Logic Establishment: Institutional actor (institutional/arbitrage) — maintains normative commitment to Bayesian reasoning; actual constraint-solving function degraded
 *   - Analytical Observer: Civilizational view (analytical/analytical) — can see the bias as either a defect or a rational adaptation to ancestral environments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bayes_theorem, 0.52).
domain_priors:suppression_score(bayes_theorem, 0.65).
domain_priors:theater_ratio(bayes_theorem, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bayes_theorem, extractiveness, 0.52).
narrative_ontology:constraint_metric(bayes_theorem, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bayes_theorem, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bayes_theorem, tangled_rope).
narrative_ontology:human_readable(bayes_theorem, "Bayesian Reasoning Bias (Base Rate Neglect)").
narrative_ontology:topic_domain(bayes_theorem, "social/cognitive").

domain_priors:requires_active_enforcement(bayes_theorem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bayes_theorem, persuaders_exploiting_bias).
narrative_ontology:constraint_beneficiary(bayes_theorem, institutional_gatekeepers).
narrative_ontology:constraint_victim(bayes_theorem, individual_reasoners).
narrative_ontology:constraint_victim(bayes_theorem, epistemic_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL REASONER (SNARE) — Cognitive architecture produces systematic neglect of base rates regardless of education or training. Cannot exit the bias through awareness alone; requires constant external scaffolding (decision aids, Bayesian training). Trapped in a structural cognitive constraint with no easy exit.
constraint_indexing:constraint_classification(bayes_theorem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL PERSUADER (ROPE) — Benefits from base rate neglect through framing flexibility. Marketing, risk communication, and political messaging coordinate around the bias — using salient cases rather than statistics. Net beneficiary with arbitrage options (can frame information advantageously or switch messaging strategies).
constraint_indexing:constraint_classification(bayes_theorem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: RISK COMMUNICATOR (TANGLED ROPE) — Public health, insurance, and regulatory professionals must work within the bias constraint while also trying to overcome it. Constrained by the cognitive limitation of their audience (cannot make base rates salient through willpower alone) but also benefits from coordination tools (standardized risk formats, visual aids, decision trees). Mixed extraction and coordination — bears cost of imperfect transmission but also gains coordination function.
constraint_indexing:constraint_classification(bayes_theorem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEBIASING INTERVENTION (SCAFFOLD) — Educational programs, decision support systems, and deliberative institutions (citizen assemblies, expert panels) provide temporary scaffolding to bypass base rate neglect. These interventions have a sunset: as individuals acquire metacognitive awareness and institutions develop Bayesian literacy, the constraint's grip weakens. Theater ratio low (actual functional content: teaching explicit calculation) because scaffolding directly addresses the mechanism.
constraint_indexing:constraint_classification(bayes_theorem, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL LOGIC ESTABLISHMENT (PITON) — Normative decision theory prescribes Bayesian updating, but this prescription is ceremonial for most human contexts. Formal education teaches Bayes' theorem; professional standards invoke Bayesian principles; but actual decision-making in organizations relies on heuristics and salient cases. The formal framework persists through institutional inertia (science requires formal probability language) but the actual constraint-solving function has atrophied.
constraint_indexing:constraint_classification(bayes_theorem, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EVOLUTIONARY PERSPECTIVE (MOUNTAIN) — From an analytical view, base rate neglect may be unavoidable given computational constraints and ancestral environments where local frequency information was more reliable than abstract statistics. The bias could be seen as a rational adaptation to information scarcity rather than a defect. However, this perspective risks naturalizing what is actually a contingent mismatch between modern information environments and evolved heuristics.
constraint_indexing:constraint_classification(bayes_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bayes_theorem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bayes_theorem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bayes_theorem, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bayes_theorem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bayes_theorem, TR),
    TR >= 0.70.

:- end_tests(bayes_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The bias enables significant value extraction by institutional persuaders who frame information to exploit base rate neglect (underestimating risk frequencies, amplifying salient case information). However, the extraction is not as severe as a pure snare because: (1) debiasing interventions create competitive pressure on persuaders, (2) some institutional actors (risk communicators, public health) bear costs trying to overcome the bias, and (3) individual reasoners retain some capacity to notice and correct for the bias under deliberate reflection. The value has increased over the interval as persuasion techniques have become more sophisticated. Suppression (0.65): High. Multiple barriers prevent escape: cognitive architecture constraints, information format limitations, time pressure under which heuristics dominate, institutional incentives to maintain salient (rather than Bayesian) communication, and lack of accessible debiasing tools in most decision contexts. However, suppression is not total — formal education, decision support systems, and deliberative institutions can reduce the bias's force. Theater ratio (0.58): Moderate-high and rising. Formal statistical education teaches Bayes' theorem but most reasoning occurs in heuristic mode. Institutional risk communication employs scientific framing (percentages, probabilities) while still relying on salient exemplars. Debiasing programs teach Bayesian logic but their actual effect on naturalistic reasoning is limited.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates different structural experiences across observation contexts. Individual reasoners trapped in the bias see a snare with no escape (except through constant external support). Institutional persuaders see the same bias as a coordination tool — a lever they can use to align audience beliefs with their messaging. Risk communicators see mixed extraction and coordination: they are both constrained by the audience's bias and enabled by it (they can use salience strategically for public health messaging). Debiasing institutions see a temporary problem with a sunset: training, decision support, and institutional redesign gradually reduce the bias's grip. The formal logic establishment sees a ceremonial framework that persists despite mismatch with actual practice. The analytical observer risks seeing the bias as a natural law of evolved cognition rather than a defect in modern information environments — this is a false mountain, naturalizing a contingent mismatch.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by institutional position. Individual reasoners are trapped victims with no exit options — they bear full cost of the bias (misestimation, poor decisions, exploitation). Persuaders have arbitrage options — they can frame information advantageously and switch messaging strategies. Risk communicators are constrained but not trapped: they can partially overcome the bias through format redesign, though not completely. Debiasing institutions have constrained options (dependent on funding, institutional access, individual motivation to learn) but can improve outcomes. The piton classification derives from the theater ratio and the atrophy of the formal logic framework's actual function. The mountain classification is perspectival — from a civilizational view, the bias appears inevitable given cognitive constraints, but this naturalizes what is actually a changeable institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is UNRESOLVED in this constraint. The classification hinges on whether base rate neglect is primarily an individual cognitive defect (suggesting snare/piton) or a structural mismatch between cognitive heuristics and modern information environments (suggesting tangled_rope with debiasing scaffolds). The institutional exploitation is real (high suppression, high extractiveness in persuasion contexts) but the bias also has legitimate coordination functions (salience heuristics often provide quick approximations that work in many natural contexts). The constraint is labeled tangled_rope to reflect this ambiguity: there is genuine extraction (persuaders benefit from the bias persisting), but the constraint also enables coordination (shared heuristics allow rapid group decision-making). Full mandatrophy resolution would require: (1) empirical determination of whether base rate neglect persists after optimal debiasing intervention, (2) measurement of institutional intentionality in maintaining the bias, and (3) clarification of whether the coordination function (salience heuristics as efficient approximation) is genuinely valuable or merely serves power asymmetries. Until these empirical questions resolve, the classification remains in the tangled_rope band.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accessibility_of_base_rates,
    'Is base rate neglect a constraint on reasoning itself, or a constraint on information access and salience?',
    'Experimental variation of base rate presentation (explicit frequency statements vs implicit proportions vs visual representations) and measurement of reasoning accuracy across presentation formats',
    'If information access: constraint is lower ε (mountain/rope). If reasoning constraint: constraint is higher ε (snare/tangled_rope). Different interventions required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accessibility_of_base_rates, empirical, 'Whether the bias reflects computational constraint or information format').

omega_variable(
    domain_specificity_of_bias,
    'Does base rate neglect persist uniformly across all reasoning domains, or is it domain-dependent (worse for low-frequency events, worse for abstract categories)?',
    'Meta-analysis of base rate neglect experiments across domains (medical diagnosis, criminal justice, weather prediction, social statistics); systematic variation of event frequency, category concreteness, and agent expertise',
    'If universal: single constraint applies globally. If domain-dependent: separate constraint stories for high-frequency vs low-frequency reasoning; different ε values per domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_specificity_of_bias, empirical, 'Whether base rate neglect is domain-specific or universal').

omega_variable(
    training_permanence,
    'Does Bayesian training produce durable debiasing or merely temporary compliance?',
    'Longitudinal follow-up studies of trained reasoners; measurement of reasoning accuracy weeks, months, years after training; comparison of trained vs untrained performance in naturalistic decision-making contexts',
    'If durable: scaffold perspective confirmed. If temporary: training is theater (piton perspective confirmed). Affects both the sunset clause logic and the mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_permanence, empirical, 'Whether debiasing interventions produce lasting change').

omega_variable(
    institutional_exploitation_intent,
    'Is base rate neglect actively maintained by institutional actors (persuaders deliberately avoiding Bayesian formats), or merely exploited opportunistically?',
    'Analysis of institutional practices in marketing, risk communication, and political messaging; measurement of base rate salience in public communication before and after regulatory intervention',
    'If actively maintained: constraint is snare with enforcement. If exploited: constraint is snare with lower suppression and lower extractiveness. Affects classification and directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_exploitation_intent, empirical, 'Whether base rate neglect is institutionally enforced or naturally persisting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bayes_theorem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bayes_tr_t0, bayes_theorem, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bayes_tr_t5, bayes_theorem, theater_ratio, 5, 0.5).
narrative_ontology:measurement(bayes_tr_t10, bayes_theorem, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(bayes_be_t0, bayes_theorem, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bayes_be_t5, bayes_theorem, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(bayes_be_t10, bayes_theorem, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bayes_theorem, information_standard).
narrative_ontology:affects_constraint(bayes_theorem, availability_heuristic).
narrative_ontology:affects_constraint(bayes_theorem, representativeness_bias).
narrative_ontology:affects_constraint(bayes_theorem, conjunction_fallacy).

% DUAL FORMULATION NOTE:
% Base rate neglect overlaps with availability heuristic and representativeness bias but is structurally distinct: it specifically concerns the underweighting of prior probabilities relative to case information. The network links represent empirical co-occurrence in the same cognitive domain and institutional exploitation patterns. A full constraint family would decompose into cognitive mechanism (ε ≈ 0.15, mountain for pure reasoning constraint) vs institutional exploitation (ε ≈ 0.65, snare for persuasion context). This story focuses on the institutional extraction version.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bayes_theorem, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
