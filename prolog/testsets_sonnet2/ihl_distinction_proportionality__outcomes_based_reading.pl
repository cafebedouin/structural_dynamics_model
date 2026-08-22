% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This story authors the outcomes-based reading of the contested IHL
 *   distinction/proportionality kernel: the claim that autonomous weapons
 *   systems satisfy IHL obligations if they demonstrably match or exceed
 *   human-operator performance on distinction and proportionality metrics,
 *   regardless of the decision-making architecture used to achieve that
 *   performance. This is a technology-neutral, performance-gated compliance
 *   doctrine actively promoted by states with advanced autonomy programs and
 *   the contractors building the relevant systems. It is one of three
 *   structurally distinct readings of the same kernel (the others being a
 *   categorical prohibition and a human-agency requirement); this story does
 *   not adjudicate between them or average their positions — it authors only
 *   this reading's own structural claim, beneficiary/victim map, and ε.
 *
 * KEY AGENTS:
 *   - military_procurement_commands: agenda_setter, sets certification thresholds
 *   - defense_contractors: beneficiary, market depends on the reading's adoption
 *   - ihl_interpretive_custodians: payer, loses interpretive authority
 *   - civilian_populations_in_conflict_zones: payer, bears harm if metrics fail to transfer
 *   - states_without_autonomy_capacity: excluded, sidelined in treaty negotiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.52).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.48).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "Outcomes-Based Compliance Reading of IHL Distinction/Proportionality for Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '95d77853-8e24-4ebd-b98b-38a6fa342a9e').
narrative_ontology:cs_kernel_codification('95d77853-8e24-4ebd-b98b-38a6fa342a9e', distributed).
narrative_ontology:cs_authority_grounding('95d77853-8e24-4ebd-b98b-38a6fa342a9e', distributed).
narrative_ontology:cs_reading_relation('95d77853-8e24-4ebd-b98b-38a6fa342a9e', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('95d77853-8e24-4ebd-b98b-38a6fa342a9e', ihl_distinction_proportionality__categorical_prohibition_reading, forecloses).
narrative_ontology:cs_axiom('95d77853-8e24-4ebd-b98b-38a6fa342a9e', foundational, outcome_equivalence_satisfies_legal_obligation).
narrative_ontology:cs_axiom_status(outcome_equivalence_satisfies_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('95d77853-8e24-4ebd-b98b-38a6fa342a9e', outcome_equivalence_satisfies_legal_obligation, instrumental).
narrative_ontology:cs_axiom('95d77853-8e24-4ebd-b98b-38a6fa342a9e', foundational, decision_architecture_is_legally_irrelevant).
narrative_ontology:cs_axiom_status(decision_architecture_is_legally_irrelevant, holdable).
narrative_ontology:cs_axiom_grounding('95d77853-8e24-4ebd-b98b-38a6fa342a9e', decision_architecture_is_legally_irrelevant, conventional).
narrative_ontology:cs_reference_frame('95d77853-8e24-4ebd-b98b-38a6fa342a9e', human_operator_baseline_standard).
narrative_ontology:cs_drift_state('95d77853-8e24-4ebd-b98b-38a6fa342a9e', post_autonomous_weapons_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('95d77853-8e24-4ebd-b98b-38a6fa342a9e', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_procurement_commands).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, states_with_advanced_autonomy_programs).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, ihl_interpretive_custodians).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, human_operators_and_commanders).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, human_operators_and_commanders).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, technology_neutrality_doctrine).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, outcome_equivalence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and certify the technical performance thresholds that determine whether an autonomous targeting system is deemed IHL-compliant. Control the test protocols, the benchmark datasets, and the classification of what counts as 'equal to or exceeding' human operator performance. Their procurement timelines and doctrine depend on this reading being accepted.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_procurement_commands, agenda_setter,
    institutional, generational, arbitrage, global).

% Sell autonomous targeting and weapons-review systems whose lawfulness is validated by demonstrating a performance metric rather than by satisfying a categorical human-judgment requirement. A metrics-based compliance gate is a market they can engineer toward and certify against; a categorical human-agency requirement would foreclose the product category entirely.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    powerful, biographical, arbitrage, global).

% International committees, tribunals, and legal scholars whose institutional function is interpreting distinction and proportionality. An outcomes-based reading displaces their interpretive authority onto engineering benchmarks set by the systems' own developers and operators, hollowing out the space in which their expertise is dispositive. They can litigate, publish commentary, and lobby treaty bodies, but cannot unilaterally block state adoption of the reading.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, ihl_interpretive_custodians, payer,
    organized, civilizational, constrained, global).

% Bear the consequences if the performance metrics are gamed, measured in low-fidelity test environments, or fail to transfer to real combat conditions. Have no voice in defining the compliance threshold and no exit from the conflict zones where the systems operate; a false-positive metric converts directly into unlawful harm they cannot contest in advance.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, regional).

% Benefit from reduced personal legal and psychological exposure when a certified autonomous system, rather than their own judgment, makes the targeting decision. Simultaneously bear command-responsibility risk if the delegated system's real-world performance diverges from its certified benchmark, since accountability may still attach to the deploying commander.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, human_operators_and_commanders, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, human_operators_and_commanders, payer).

% Push for the outcomes-based reading in treaty negotiations and doctrine documents because it legalizes systems they are already developing and gives them a first-mover advantage over states without comparable technical capacity to demonstrate compliance.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, states_with_advanced_autonomy_programs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, states_with_advanced_autonomy_programs, beneficiary).

% Lack the technical infrastructure to independently verify performance-benchmark claims made by autonomy-leading states, and lack comparable systems of their own. Would prefer either a categorical prohibition (leveling the field) or a strong verification regime, but have limited leverage in the treaty bodies where the reading is being normalized.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, states_without_autonomy_capacity, excluded,
    moderate, generational, constrained, global).

% Analyzes whether the outcomes-based reading is doctrinally coherent with existing IHL jurisprudence and the Martens Clause, and tracks divergence between claimed compliance and battlefield outcomes.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, international_humanitarian_law_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__outcomes_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a technology-neutral, verifiable standard that lets states adopt autonomous targeting systems without amending the text of IHL, by translating distinction and proportionality into measurable performance benchmarks comparable across systems and operators.
% TRANSFER_FUNCTION: Moves adjudicative authority over lawful killing from human-judgment-in-the-moment and from IHL's interpretive custodians (courts, ICRC commentary, customary law bodies) to the entities that design, run, and certify the performance benchmarks — chiefly the fielding state and its contractors. Where certified performance is later shown not to transfer to real combat conditions, the resulting harm transfers to civilian populations who had no part in setting or verifying the benchmark.
% ABSENT_VOICES: Civilian populations in prospective conflict zones have no seat in benchmark design. States lacking autonomy programs are structurally sidelined in treaty negotiations dominated by states with fielded or near-fielded systems. IHL interpretive custodians participate but cannot compel adoption of a stricter standard once technologically advanced states coordinate around the outcomes-based reading.
% DISAPPEARANCE_RATIONALE: If the outcomes-based reading were abandoned tomorrow in favor of a categorical or human-agency standard, currently planned and partially fielded autonomous targeting programs would face immediate legal exposure, procurement contracts would need renegotiation or cancellation, and the interpretive authority currently ceded to technical certification would revert to human-judgment requirements and traditional IHL adjudication bodies.
% FOUNDING_PROBLEM: IHL's distinction and proportionality obligations were articulated for human decision-makers; as autonomous targeting systems became technically feasible, states and industry needed a doctrine that would not categorically foreclose deployment while still claiming fidelity to the law's substantive protections for civilians.
% FOUNDING_PROBLEM_CORROBORATION: Military legal advisors and defense-industry ethics boards (proponents) attest the reading solves a genuine gap — IHL's text does not mention autonomy and a technology-neutral outcomes test is the most faithful application of existing principles to new means. Independent IHL scholars, ICRC commentary, and several UN CCW delegations (outside the beneficiary set) attest instead that the reading substitutes an unverifiable engineering claim for the substantive human-judgment element the law was understood to require, and that no state has yet produced benchmark data credibly demonstrating superiority to trained human operators under real combat variance.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.52) rather than high because the reading has a genuine coordination function — a workable, technology-neutral standard that avoids freezing IHL doctrine against all technological change — but it also enables a real transfer of adjudicative authority away from IHL's traditional interpretive institutions toward benchmark-setting entities with a commercial or military stake in a favorable result. Suppression (0.48) reflects the structural difficulty non-adopting states and interpretive custodians face in contesting a standard once it becomes embedded in doctrine and procurement, without amounting to outright coercion. Theater ratio (0.40) captures that a portion of 'compliance demonstration' activity is calibrated to pass a benchmark rather than to guarantee real-world civilian protection, and this share is treated as rising over the interval as certification regimes mature and gaming incentives accumulate.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of military procurement commands and defense contractors, this reading looks like principled technology-neutral law-following coordination: a fair, verifiable standard applied equally regardless of decision architecture. From the seat of IHL interpretive custodians and civilian populations, the same structure looks like an extraction of adjudicative authority — a technical fig leaf that launders lethal-force delegation through a metric the beneficiaries themselves design and certify. The engine should compute these as different per-seat classifications from the identical structural data; that divergence, not a hedge between them, is the analytical content.
 *
 * DIRECTIONALITY LOGIC:
 *   Military procurement commands and states with advanced autonomy programs set the benchmark and its terms — directionality places them near the beneficiary end. Defense contractors profit directly from the existence of a technical pass/fail gate rather than a categorical prohibition — also beneficiary-side. IHL interpretive custodians and civilian populations bear the cost: custodians lose interpretive centrality, civilians bear the tail risk of benchmark-to-battlefield performance gaps, with no meaningful exit for either — custodians are constrained to advocacy, civilians are trapped in conflict geography. Human operators/commanders occupy a mixed position: they benefit from reduced immediate decision burden but retain downstream accountability exposure, captured by the dual role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — IHL text silent on autonomous means — remains genuinely live in the sense that some technology-neutral doctrine is needed; what is contested is whether THIS particular doctrine (outcomes-based, self-certified performance) still serves that founding problem or has become a vehicle for legitimizing deployment ahead of independently verified capability. The status is authored as contested rather than dead precisely because credible corroboration exists on both sides — this prevents the classification from collapsing into either 'pure extraction dressed as coordination' or 'settled, uncontested coordination.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    benchmark_validity_ambiguity,
    'Do laboratory or simulation performance benchmarks for autonomous distinction/proportionality reliably predict real-combat performance, or do they measure a proxy that diverges under battlefield variance (weather, adversarial countermeasures, civilian pattern-of-life ambiguity)?',
    'Independent, adversarial third-party testing under contested/adversarial conditions rather than developer-controlled test suites; post-deployment incident audits comparing certified benchmark performance to real-world outcomes.',
    'If benchmarks do not transfer, the outcomes-based reading''s coordination claim collapses into theater — ''compliance'' would be demonstrated on paper while extraction of interpretive authority and civilian risk continues unchanged. If benchmarks do transfer robustly, the reading''s coordination function is substantially validated and ε should be revised downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(benchmark_validity_ambiguity, empirical, 'Whether certified benchmark performance predicts real-world civilian-protection outcomes.').

omega_variable(
    self_certification_conflict_of_interest,
    'Can a compliance standard remain a genuine legal safeguard when the entities most incentivized to pass it (fielding states and their contractors) substantially control the design and administration of the test?',
    'Comparative institutional analysis: track whether certification regimes converge toward independent third-party verification bodies over time, or remain internal to the fielding state/contractor relationship.',
    'If certification stays internal to the beneficiary parties, the tangled-rope classification is reinforced (coordination function real but captured); external, independently administered verification would push the constraint toward a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_certification_conflict_of_interest, empirical, 'Whether the performance-certification process is structurally captured by its own beneficiaries.').

omega_variable(
    kernel_framing_alternative_selection,
    'Is ''technology neutrality'' the correct framing axiom for this reading, or does technology neutrality itself already presuppose that outcome-equivalence is the right unit of legal analysis (rather than the human_agency_reading''s claim that the decision-making architecture is itself a distinct object of legal concern, independent of measured outcomes)?',
    'Doctrinal and philosophical analysis of whether IHL''s structure of obligations attaches to acts, actors, or outcomes; comparative study of how other legal domains (e.g., product liability vs. professional negligence standards) resolve analogous architecture-vs-outcome disputes.',
    'If technology neutrality is itself a contested framing choice rather than a neutral premise, this reading''s claimed coherence with existing IHL doctrine is weaker than presented, and the categorical/human-agency readings'' objections gain force independent of any benchmark validity question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_selection, conceptual, 'Whether outcome-equivalence is a neutral or a contested unit of legal analysis relative to the sibling readings.').


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
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ihl__tr_t16, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ihl__be_t4, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(ihl__be_t8, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(ihl__be_t16, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ihl__su_t4, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(ihl__su_t8, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(ihl__su_t12, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(ihl__su_t16, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__outcomes_based_reading, 0.1).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the ihl_distinction_proportionality kernel. outcomes_based_reading (this story) authors moderate extraction with a genuine but captured coordination function. human_agency_reading and categorical_prohibition_reading are separate stories authoring different ε and beneficiary/victim structures for the same underlying text and Martens Clause dispute. All three must remain cross-linked via affects_constraints; none averages or hedges against the others per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
