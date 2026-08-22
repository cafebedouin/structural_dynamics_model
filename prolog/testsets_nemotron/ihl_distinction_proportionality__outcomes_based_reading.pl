% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: Outcomes-Based IHL Compliance for Autonomous Weapons
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the outcomes-based reading of the IHL
 *   distinction/proportionality kernel: an autonomous weapon system is lawful
 *   if it demonstrably achieves distinction and proportionality performance
 *   equal to or exceeding human operators. The reading is technology-neutral
 *   — law governs outcomes, not means. It functions as a tangled rope: it
 *   coordinates by providing a determinate compliance standard (benefiting
 *   military operators and defense contractors who gain a legal authorization
 *   pathway) while extracting by transferring interpretive authority from
 *   humanitarian law custodians and transferring risk to civilian populations
 *   when metrics diverge from operational reality. Active enforcement is
 *   required — the compliance thresholds must be defined, tested, and
 *   certified by institutional authorities who are structurally aligned with
 *   the beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.48).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.35).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "Outcomes-Based IHL Compliance for Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '87bc24b5-dfb2-4680-be45-d637580c4373').
narrative_ontology:cs_kernel_codification('87bc24b5-dfb2-4680-be45-d637580c4373', fixed_text).
narrative_ontology:cs_authority_grounding('87bc24b5-dfb2-4680-be45-d637580c4373', lineage).
narrative_ontology:cs_interpretation_layer_present('87bc24b5-dfb2-4680-be45-d637580c4373').
narrative_ontology:cs_reading_relation('87bc24b5-dfb2-4680-be45-d637580c4373', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('87bc24b5-dfb2-4680-be45-d637580c4373', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('87bc24b5-dfb2-4680-be45-d637580c4373', foundational, technology_neutral_compliance_standard).
narrative_ontology:cs_axiom_status(technology_neutral_compliance_standard, holdable).
narrative_ontology:cs_axiom_grounding('87bc24b5-dfb2-4680-be45-d637580c4373', technology_neutral_compliance_standard, conventional).
narrative_ontology:cs_axiom('87bc24b5-dfb2-4680-be45-d637580c4373', foundational, measurable_performance_satisfies_ihl_obligations).
narrative_ontology:cs_axiom_status(measurable_performance_satisfies_ihl_obligations, holdable).
narrative_ontology:cs_axiom_grounding('87bc24b5-dfb2-4680-be45-d637580c4373', measurable_performance_satisfies_ihl_obligations, instrumental).
narrative_ontology:cs_reference_frame('87bc24b5-dfb2-4680-be45-d637580c4373', ihl_governs_outcomes_not_means).
narrative_ontology:cs_drift_state('87bc24b5-dfb2-4680-be45-d637580c4373', contemporary_aws_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('87bc24b5-dfb2-4680-be45-d637580c4373', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_operators).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, autonomous_systems_developers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, autonomous_systems_developers).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, technology_neutral_compliance_standard).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, measurable_performance_justifies_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain operational flexibility, reduced personnel risk, and force multiplication from deploying autonomous systems that meet performance thresholds. They define and certify the compliance metrics, control the testing environments, and benefit from expanded mission envelopes. Exit means forgoing a strategic capability advantage, but they hold the institutional power to shape the standards.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Capture procurement contracts and sustain revenue streams by delivering systems that pass the compliance thresholds. They influence metric design through lobbying, standards bodies, and embedded personnel. Their exit is commercial — they can pivot to other programs — but the autonomous weapons market is a primary growth vector.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    powerful, biographical, mobile, global).

% Engineer the systems to meet thresholds; their professional standing and funding depend on successful certification. They bear the R&D cost of meeting evolving standards and the career risk of failed tests. Exit means leaving the subfield, which is constrained by specialized expertise and security clearances.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, autonomous_systems_developers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, autonomous_systems_developers, payer).

% Interpret and guard IHL's normative core — the ICRC, UN special rapporteurs, academic specialists. They lose interpretive authority when compliance reduces to technical metrics, and their warnings about metric gaming, context collapse, and accountability gaps are structurally marginalized. Their identity is fused to the humanitarian project; exit means abandoning the vocation that constitutes their professional self.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians, payer,
    organized, generational, identity_locked, global).

% Bear the consequences when metrics diverge from reality — false positives in distinction, proportionality calculus that misses cumulative harm, edge cases where testing environments don't match operational chaos. They have no voice in standard-setting, no exit from the battlespace, and no recourse when systems fail. Their situation is the ground truth the metrics claim to represent but cannot fully capture.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Design, administer, and certify the performance thresholds (e.g., military test ranges, NATO standardization agencies, national certification bodies). They set the pass/fail criteria, control the test scenarios, and decide what counts as 'equal to or exceeding human operators.' They are institutionally incentivized to produce deployable systems; their structural position aligns with military operators.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, compliance_testing_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Analyze the constraint's coherence with IHL's object and purpose, the Martens Clause, and state practice. They map the drift from normative judgment to technical compliance, document the accountability gap, and trace how the reading reshapes the legal architecture. Their exit is analytical — they can always observe — but their influence on state practice is indirect and contested.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, verifiable standard for authorizing autonomous weapons systems: replaces the indeterminacy of 'human judgment' with measurable performance thresholds, enabling states to field new capabilities while claiming legal compliance.
% TRANSFER_FUNCTION: Transfers interpretive authority from humanitarian law custodians (who guard normative thresholds) to military operators and testing authorities (who define and certify technical metrics). Transfers risk from military personnel (removed from the kill chain) to civilian populations (exposed to system failures at the metric boundary). Transfers procurement revenue to defense contractors and developers who meet the thresholds.
% ABSENT_VOICES: Civilian populations in conflict zones are structurally excluded from standard-setting and compliance certification. Affected communities have no seat at the table where distinction/proportionality metrics are defined, test scenarios are chosen, or pass/fail thresholds are calibrated. Their representatives (local NGOs, community leaders) are absent from the technical review boards and treaty meetings where this reading is operationalized.
% DISAPPEARANCE_RATIONALE: If this reading vanished, states could no longer claim IHL compliance for autonomous weapons solely by citing technical metrics. They would need to either (a) revert to the human_agency_reading requiring human judgment at the point of lethal force, constraining deployment; (b) adopt the categorical_prohibition_reading, banning such systems; or (c) operate in a declared legal grey zone. The autonomous weapons programs of major military powers would lose their primary legal authorization pathway, reorganizing procurement, doctrine, and arms control negotiations.
% FOUNDING_PROBLEM: The indeterminacy of applying IHL's distinction and proportionality rules to autonomous systems created a legal vacuum: states wanted to develop and deploy AWS but lacked a clear compliance standard. Human operators are fallible and variable; a technology-neutral, outcomes-based standard promised objective, verifiable compliance that could keep pace with technical progress.
% FOUNDING_PROBLEM_CORROBORATION: Military operators and defense contractors attest the problem remains live: human performance is the only existing benchmark, and technical standards are the only path to lawful deployment. Humanitarian law custodians (ICRC, UNIDIR, Article 36) and legal scholars attest the founding problem was misdiagnosed: IHL requires contextual moral judgment, not statistical performance, and the 'problem' was constructed to legitimize a capability trajectory. The ICRC's 2021 position paper and the 2023 UN Secretary-General's policy brief corroborate the shifted-function reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.48) reflects the moderate but structurally significant transfer: the reading permits autonomy where metrics pass, extracting interpretive authority from humanitarian custodians and risk protection from civilians. Suppression (0.35) is moderate — the constraint operates through standard-setting and certification rather than overt coercion, but the structural exclusion of affected populations and the institutional capture of testing authorities create effective suppression. Theater ratio (0.22) is low-moderate: the compliance testing apparatus has real functional content, but a growing share of its activity serves to legitimate deployment rather than validate performance. Accessibility collapse (0.42) is moderate — alternatives (human judgment, categorical prohibition) remain conceptually available but are institutionally marginalized. Resistance (0.68) is high — humanitarian law custodians, legal scholars, and civil society actively contest the reading's coherence with IHL's object and purpose.
 *
 * PERSPECTIVAL GAP:
 *   From the military operator seat, the constraint is genuine coordination — it solves the legal authorization problem for a strategic capability. From the humanitarian custodian seat, it is extraction dressed as coordination — the metrics are a cover for delegating life/death decisions to machines. From the civilian seat, it is a snare — they bear the downside of metric failure with no voice in the standard. The engine computes this divergence; the claimed_type (tangled_rope) captures the structural hybridity.
 *
 * DIRECTIONALITY LOGIC:
 *   Military operators and testing authorities are structural beneficiaries (d ~ 0.15-0.25): they set the metrics, control certification, and gain deployment authorization. Defense contractors and developers are beneficiaries with cost exposure (d ~ 0.3): they profit but bear R&D and certification risk. Humanitarian law custodians are identity-locked payers (d ~ 0.85): their professional identity fuses to the humanitarian project, and the reading structurally displaces their interpretive role. Civilian populations are trapped payers (d ~ 0.95): zero exit, zero voice, full exposure to metric failure. The engine computes per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legal vacuum for AWS authorization) is contested: beneficiaries say it's live; custodians say it was constructed to legitimize a capability trajectory. The constraint persists because it solves a real coordination problem for powerful actors (legal certainty for deployment) while its extraction falls on actors with no structural power to resist (civilians) or whose resistance is institutionally contained (custodians). Mandatrophy is not resolved — the arrangement's function has shifted from 'enabling lawful innovation' to 'legitimizing deployment at the metric boundary,' but the coordination cover remains effective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_validity_gap,
    'Do the compliance metrics (distinction accuracy, proportionality calculus) actually capture the normative content of IHL''s distinction and proportionality obligations, or do they measure a proxy that diverges systematically in operational conditions?',
    'Operational data from deployed systems compared against independent humanitarian assessment of the same engagements; red-teaming of test scenarios by humanitarian law custodians; longitudinal tracking of civilian harm rates in AWS vs. human-operated engagements.',
    'If metrics systematically diverge from normative content, the constraint''s coordination function is illusory and its extraction is unchecked — classification shifts toward snare. If metrics track normative content within acceptable bounds, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metric_validity_gap, empirical, 'Whether technical compliance metrics are valid proxies for IHL''s normative obligations').

omega_variable(
    interpretive_authority_displacement,
    'Is the transfer of interpretive authority from humanitarian custodians to military testing authorities a structural feature of this reading, or a contingent institutional capture that could be corrected by procedural reform?',
    'Institutional analysis of certification bodies: governance structure, independence requirements, custodian participation rights, and track record of metric revision in response to custodian critique. Comparison with other dual-use technology governance regimes (nuclear, biological, cyber).',
    'If structural, the tangled_rope''s asymmetric extraction is inherent — the coordination function requires the displacement. If contingent, the extraction could be reduced without losing coordination, moving the constraint toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_displacement, conceptual, 'Whether interpretive authority displacement is inherent to the outcomes-based compliance model').

omega_variable(
    civilian_risk_at_metric_boundary,
    'When systems operate at or near the compliance threshold, does the residual risk concentrate on civilian populations in ways that the metric framework cannot capture (cumulative harm, psychological effects, infrastructural degradation, community trust)?',
    'Mixed-methods field research in conflict zones where AWS have been deployed: civilian harm monitoring, community perception surveys, longitudinal health and displacement tracking correlated with system engagement logs.',
    'If residual risk systematically falls on civilians at the metric boundary, the constraint''s extraction is more severe than the base metrics suggest — the victims array is structurally incomplete. This would increase effective extraction for the civilian payer seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_risk_at_metric_boundary, empirical, 'Whether civilian harm at the compliance threshold is systematically unmeasured').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''IHL distinction/proportionality for autonomous systems'' admit only these three readings, or is there a fourth framing (e.g., a distributed authority model where human judgment and machine performance are jointly required) that changes the structural relations?',
    'Genealogical analysis of CCW/GGE discussions, ICRC working papers, and state position papers to identify whether alternative framings were considered and suppressed, or never articulated. Mapping of the argumentative space across diplomatic, legal, and technical forums.',
    'If a suppressed fourth framing exists, the current tripartite kernel map is incomplete and the reading_relations (especially coexists_with) may misrepresent the structural field. A joint human-machine judgment framing would create a distinct constraint with different beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the declared kernel readings exhaust the defensible framings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 2010, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2010, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(ihl__tr_t2015, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(ihl__tr_t2020, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(ihl__tr_t2025, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement(ihl__tr_t2030, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2030, 0.22).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2010, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(ihl__be_t2015, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement(ihl__be_t2020, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(ihl__be_t2025, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2025, 0.45).
narrative_ontology:measurement(ihl__be_t2030, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2030, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2010, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(ihl__su_t2015, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(ihl__su_t2020, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2020, 0.28).
narrative_ontology:measurement(ihl__su_t2025, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2025, 0.32).
narrative_ontology:measurement(ihl__su_t2030, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2030, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, autonomous_weapons_proliferation_regime).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ccw_gge_negotiations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ihl_distinction_proportionality kernel. The outcomes_based_reading declares lawful compliance via technical metrics; the human_agency_reading requires human judgment; the categorical_prohibition_reading bans autonomous weapons per se. The ε values differ substantially: outcomes_based (moderate ~0.48), human_agency (low ~0.2, coordination-dominant), categorical_prohibition (near-zero extraction but high suppression for would-be developers). They are linked via network.affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__outcomes_based_reading, institutional, 0.18).
constraint_indexing:directionality_override(ihl_distinction_proportionality__outcomes_based_reading, powerful, 0.32).
constraint_indexing:directionality_override(ihl_distinction_proportionality__outcomes_based_reading, organized, 0.35).
constraint_indexing:directionality_override(ihl_distinction_proportionality__outcomes_based_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
