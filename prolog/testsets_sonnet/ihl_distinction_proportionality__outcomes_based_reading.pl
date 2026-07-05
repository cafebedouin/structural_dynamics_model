% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__outcomes_based_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: Outcomes-Based Compliance Reading of IHL Distinction/Proportionality for Autonomous Weapons
 *   domain: international_law/military_technology
 *
 * SUMMARY:
 *   This story instantiates the outcomes-based reading of the contested IHL
 *   distinction/proportionality kernel governing autonomous weapons systems:
 *   the claim that legal obligations are satisfied whenever a system
 *   demonstrably matches or exceeds human-operator performance on distinction
 *   and proportionality metrics, regardless of the decision architecture used
 *   to achieve that performance. This is one of three structurally distinct
 *   readings of the same kernel (the others being a categorical prohibition
 *   reading and a human-agency reading, generated as separate constraint
 *   stories). The outcomes-based reading has its own beneficiary/victim
 *   structure and its own extraction profile — it is not a hedge across the
 *   three positions but a specific legal-political claim with real
 *   institutional backers (defense industry, capable-state militaries) and
 *   real institutional losers (IHL custodial bodies, under-resourced states,
 *   civilians in conflict zones where benchmarks fail).
 *
 * KEY AGENTS:
 *   - defense_contractors: primary beneficiary — control test protocols and profit from continued procurement
 *   - military_efficiency_planners: agenda_setter — administer doctrine and adjust compliance thresholds
 *   - states_with_advanced_autonomous_weapons_programs: beneficiary — strategic advantage from being able to claim compliance
 *   - humanitarian_law_custodians: payer — displaced interpretive authority
 *   - civilian_populations_in_conflict_zones: payer — bear the cost if metrics fail
 *   - states_without_autonomous_weapons_capability: payer — rule-takers without verification capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.52).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.44).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "Outcomes-Based Compliance Reading of IHL Distinction/Proportionality for Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_law/military_technology").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, 'ce742557-f272-41b3-bc3f-a27a556e9e63').
narrative_ontology:cs_kernel_codification('ce742557-f272-41b3-bc3f-a27a556e9e63', distributed).
narrative_ontology:cs_authority_grounding('ce742557-f272-41b3-bc3f-a27a556e9e63', distributed).
narrative_ontology:cs_reading_relation('ce742557-f272-41b3-bc3f-a27a556e9e63', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('ce742557-f272-41b3-bc3f-a27a556e9e63', ihl_distinction_proportionality__categorical_prohibition_reading, forecloses).
narrative_ontology:cs_axiom('ce742557-f272-41b3-bc3f-a27a556e9e63', foundational, outcome_equivalence_satisfies_legal_obligation).
narrative_ontology:cs_axiom_status(outcome_equivalence_satisfies_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ce742557-f272-41b3-bc3f-a27a556e9e63', outcome_equivalence_satisfies_legal_obligation, instrumental).
narrative_ontology:cs_axiom('ce742557-f272-41b3-bc3f-a27a556e9e63', foundational, law_governs_means_agnostic_outcomes_not_decision_architecture).
narrative_ontology:cs_axiom_status(law_governs_means_agnostic_outcomes_not_decision_architecture, holdable).
narrative_ontology:cs_axiom_grounding('ce742557-f272-41b3-bc3f-a27a556e9e63', law_governs_means_agnostic_outcomes_not_decision_architecture, conventional).
narrative_ontology:cs_reference_frame('ce742557-f272-41b3-bc3f-a27a556e9e63', human_judgment_centered_targeting_law).
narrative_ontology:cs_drift_state('ce742557-f272-41b3-bc3f-a27a556e9e63', post_autonomous_weapons_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ce742557-f272-41b3-bc3f-a27a556e9e63', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_efficiency_planners).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, states_with_advanced_autonomous_weapons_programs).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, states_without_autonomous_weapons_capability).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, technology_neutrality_of_ihl).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, outcome_equivalence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell autonomous targeting systems. The outcomes-based reading lets them market compliance as a certification/benchmarking exercise rather than face a categorical ban; they control the test protocols, datasets, and performance claims used to demonstrate equal-or-better distinction/proportionality performance, and profit from continued procurement.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    organized, generational, arbitrage, global).

% Set doctrine and procurement policy around autonomous weapons, championing the outcomes-based standard because it lets deployment scale with battlefield tempo and reduces operator risk. They administer whatever verification regime exists and can adjust the compliance threshold as systems mature.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_efficiency_planners, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, military_efficiency_planners, beneficiary).

% Gain a strategic and diplomatic advantage: the outcomes-based reading lets them field autonomous systems as legally compliant while less-resourced states, lacking equivalent testing infrastructure, cannot make the same demonstrable claims and are pressured to either adopt the tech or accept a normative framework built around it.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, states_with_advanced_autonomous_weapons_programs, beneficiary,
    institutional, generational, arbitrage, global).

% International Committee of the Red Cross, UN special rapporteurs, and IHL scholars who have historically interpreted distinction and proportionality as requiring situated human judgment, not just statistical equivalence. The outcomes-based reading displaces their interpretive authority with a technical metrics regime they do not control and cannot easily contest once benchmark protocols are institutionalized by military and industry actors.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians, payer,
    moderate, civilizational, constrained, global).

% Bear the direct cost if the performance metrics used to certify compliance fail to capture edge cases, novel environments, or adversarial spoofing that autonomous targeting systems were not tested against. They have no voice in the certification process and no exit from being present when systems are deployed.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, regional).

% Face a legal and normative framework built around a compliance standard they cannot independently verify or contest, since they lack the technical infrastructure to run comparable performance benchmarks; they are effectively rule-takers in a regime authored by the states and firms that benefit from it.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, states_without_autonomous_weapons_capability, payer,
    moderate, generational, constrained, global).

% Analyze whether the technology-neutral, outcomes-based framing is a coherent extension of existing IHL doctrine or a substantive reinterpretation that shifts the locus of legal judgment from battlefield commanders to system designers and test engineers.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, technology-neutral standard that lets militaries adopt new targeting technologies without requiring a new legal instrument for every generation of hardware — compliance is judged by measurable outcome parity rather than by the means used to achieve it, which is a genuine coordination benefit across a rapidly changing technology base.
% TRANSFER_FUNCTION: Moves interpretive authority over what counts as lawful targeting from a body of accumulated human-judgment case law and customary practice (curated by IHL institutions) to whichever party designs, controls, and reports the performance benchmarks — typically the fielding state and its contractors — and moves risk from system operators onto civilian populations in the event the benchmarks fail to capture real combat conditions.
% ABSENT_VOICES: Civilian populations in prospective conflict zones have no seat in setting or auditing the compliance thresholds; less-resourced states lacking benchmarking infrastructure are present only as rule-takers; IHL custodial bodies participate in commentary but do not control certification once states and industry standardize their own testing regimes.
% DISAPPEARANCE_RATIONALE: If the outcomes-based reading were repudiated overnight in favor of a categorical or human-agency standard, current and planned autonomous weapons deployments would face immediate legal exposure, procurement programs would be paused or restructured around mandated human control points, and the diplomatic leverage currently held by advanced-capability states would erode substantially.
% FOUNDING_PROBLEM: IHL treaty text was drafted before autonomous targeting was technically feasible, and existing distinction/proportionality obligations reference the judgment of a human decision-maker; the outcomes-based reading was constructed to answer the question of how those obligations apply when a machine, not a human, makes the immediate targeting decision.
% FOUNDING_PROBLEM_CORROBORATION: Defense ministries and contractor-aligned policy institutes attest the outcomes-based standard resolves the founding problem faithfully by preserving the law's purpose (protecting civilians) while remaining technology-neutral. Independent international law scholars, ICRC commentary, and several UN Human Rights Council special rapporteur reports attest from outside the beneficiary set that the reading substitutes a verifiable-but-narrow technical proxy for the broader judgment-based standard the law actually requires, and that no consensus verification methodology yet exists to make the equivalence claim meaningfully falsifiable.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.52 at interval end) rather than high: the reading does provide a genuine, technology-neutral coordination function (letting the law track fast-evolving hardware without perpetual re-legislation), so it is not pure extraction — but the benchmark-setting and verification apparatus is controlled almost entirely by the parties who benefit from favorable results, which is the extractive component. Suppression is moderate (0.44): the mechanism does not physically bar dissent, but institutionalized certification regimes make alternative (categorical or human-agency) readings progressively harder to assert once procurement and doctrine lock in around the metrics. Theater ratio rises across the interval (0.22 to 0.41) reflecting a drift where compliance demonstrations increasingly serve to legitimate pre-decided procurement rather than to genuinely test performance equivalence — a Goodhart-style substitution as the benchmarks become the target rather than the proxy.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense contractors and capable-state militaries sit near the beneficiary end: they set the tests, control the reported results, and capture the procurement and strategic advantage that flows from certified compliance. Humanitarian law custodians and under-resourced states sit toward the target end: their interpretive or verification authority is structurally displaced by a technical regime they do not administer. Civilian populations in conflict zones are the most extreme target case — trapped, immediate time horizon, zero voice in certification — because they bear the tail-risk cost (metric failure in an untested scenario) without any of the institutional benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (applying pre-autonomy-era IHL text to autonomous decision systems) is only partially live: it is real in the sense that some legal gap genuinely exists, but the outcomes-based resolution to that gap has been substantially captured by the parties who profit from a permissive resolution. This is exactly the tangled-rope signature: a genuine coordination function (technology-neutral law that doesn't need constant amendment) is bundled with asymmetric extraction (control of the verification apparatus by the beneficiary class) and requires active enforcement (procurement policy, doctrine, and diplomatic pressure) to hold against the competing categorical and human-agency readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    outcome_equivalence_measurability,
    'Can distinction/proportionality performance be measured with enough fidelity, across enough combat scenarios, for ''equal to or exceeding human operators'' to be a meaningful empirical claim rather than a benchmark-gaming target?',
    'Independent, adversarially-designed testing regimes run by parties without a procurement stake, including deliberate out-of-distribution and contested-environment scenarios; compare pre-deployment benchmark claims against post-deployment incident review.',
    'If performance parity claims are not robustly measurable outside the vendor''s own test conditions, the outcomes-based reading functions as a compliance fiction rather than a genuine technology-neutral standard, pushing the classification toward snare; if independently verifiable, the coordination function is stronger and the classification sits closer to a genuine (if still asymmetric) tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outcome_equivalence_measurability, empirical, 'Whether the core performance-parity claim is independently falsifiable.').

omega_variable(
    interpretive_displacement_reversibility,
    'Once benchmark-based certification becomes institutionalized in procurement and doctrine, can IHL custodial bodies (ICRC, UN rapporteurs, treaty bodies) recover interpretive authority, or does the technical regime become self-reinforcing?',
    'Track whether any state or international body has successfully imposed a human-agency or categorical standard AFTER an outcomes-based procurement regime was already operational, versus only before.',
    'If displacement is empirically difficult to reverse once fielded systems exist, the extraction from humanitarian law custodians is closer to permanent capture; if reversible via treaty or judicial action, the current extraction is more contingent and time-bound.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_displacement_reversibility, empirical, 'Whether institutionalization of the outcomes-based standard is reversible.').

omega_variable(
    kernel_framing_choice,
    'Is the choice to treat this as a ''technology-neutral, outcomes-based'' reading itself a framing that already presupposes the answer to the underlying moral question (whether machine-mediated killing is categorically different from human-mediated killing)?',
    'Compare this reading''s premises against the Martens Clause ''principles of humanity and dictates of public conscience'' language directly — assess whether outcome-equivalence is a legitimate operationalization of that language or a substitution of a different (consequentialist) ethical framework for the one the treaty text invokes.',
    'If outcome-equivalence is found to substitute a different ethical framework rather than operationalize the existing one, this reading''s claim to being a faithful continuation of IHL doctrine (rather than a substantive reinterpretation favoring capable states) weakens considerably, which would raise the story''s proper extractiveness score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the outcomes-based framing pre-decides the moral question it claims only to operationalize.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ihl__tr_t4, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(ihl__tr_t8, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(ihl__tr_t16, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ihl__be_t4, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(ihl__be_t8, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(ihl__be_t16, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ihl__su_t4, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(ihl__su_t8, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(ihl__su_t12, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(ihl__su_t16, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 20, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__outcomes_based_reading, 0.1).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% Part of a three-story kernel family on IHL distinction/proportionality obligations for autonomous weapons. outcomes_based_reading (this story) is the most permissive reading with the clearest concentrated beneficiary class; human_agency_reading requires irreducible human judgment at the point of lethal force; categorical_prohibition_reading bars autonomous lethal decisions per se under Martens Clause reasoning regardless of performance. All three are linked bidirectionally in commentary and via affects_constraints; each carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
