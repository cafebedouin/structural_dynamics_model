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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   domain: international_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint story represents the outcomes-based reading of the IHL
 *   distinction/proportionality kernel. It holds that autonomous weapons
 *   satisfy IHL obligations if they demonstrably achieve distinction and
 *   proportionality performance equal to or exceeding human operators. The
 *   law governs outcomes, not means. This reading enables LAWS deployment
 *   under a verification regime. It has moderate extractiveness: it permits
 *   autonomy where measurable performance justifies it, benefiting military
 *   efficiency and defense contractors, while extracting interpretive
 *   authority from humanitarian law custodians and imposing risk on civilian
 *   populations if metrics fail in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.48).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.35).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "Outcomes-Based IHL Compliance for Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, 'd04edcf7-749b-49a2-b15b-7bcba3ead9bd').
narrative_ontology:cs_kernel_codification('d04edcf7-749b-49a2-b15b-7bcba3ead9bd', formalized).
narrative_ontology:cs_authority_grounding('d04edcf7-749b-49a2-b15b-7bcba3ead9bd', lineage).
narrative_ontology:cs_interpretation_layer_present('d04edcf7-749b-49a2-b15b-7bcba3ead9bd').
narrative_ontology:cs_reading_relation('d04edcf7-749b-49a2-b15b-7bcba3ead9bd', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('d04edcf7-749b-49a2-b15b-7bcba3ead9bd', ihl_distinction_proportionality__categorical_prohibition_reading, influences).
narrative_ontology:cs_axiom('d04edcf7-749b-49a2-b15b-7bcba3ead9bd', foundational, technology_neutral_outcome_standard).
narrative_ontology:cs_axiom_status(technology_neutral_outcome_standard, holdable).
narrative_ontology:cs_axiom_grounding('d04edcf7-749b-49a2-b15b-7bcba3ead9bd', technology_neutral_outcome_standard, conventional).
narrative_ontology:cs_axiom('d04edcf7-749b-49a2-b15b-7bcba3ead9bd', foundational, verifiable_performance_satisfies_ihl).
narrative_ontology:cs_axiom_status(verifiable_performance_satisfies_ihl, holdable).
narrative_ontology:cs_axiom_grounding('d04edcf7-749b-49a2-b15b-7bcba3ead9bd', verifiable_performance_satisfies_ihl, empirically_contingent).
narrative_ontology:cs_reference_frame('d04edcf7-749b-49a2-b15b-7bcba3ead9bd', geneva_conventions_additional_protocol_i).
narrative_ontology:cs_drift_state('d04edcf7-749b-49a2-b15b-7bcba3ead9bd', autonomous_weapons_debate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d04edcf7-749b-49a2-b15b-7bcba3ead9bd', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, state_armed_forces).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, technical_verification_bodies).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, technology_neutral_ihl_application).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, verifiable_performance_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets rules of engagement and procurement requirements for autonomous weapons. Gains operational flexibility and reduced personnel risk by delegating targeting to verified autonomous systems. Controls the verification regimes that certify compliance. Can shift between human-operated and autonomous systems based on tactical assessment.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, state_armed_forces, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, state_armed_forces, beneficiary).

% Develop and sell autonomous weapons systems certified under the outcomes-based standard. Revenue depends on the reading's acceptance as the governing compliance framework. Can pivot to other markets if this reading is rejected, but have invested heavily in autonomy R&D predicated on this legal pathway.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    organized, biographical, mobile, global).

% ICRC, international legal scholars, UN human rights mechanisms, and judicial bodies that interpret and guard IHL. Their interpretive authority erodes as technical metrics replace legal judgment. They cannot exit the constraint because their mandate requires engaging with state practice, but their voice is marginalized in verification bodies dominated by military and technical experts.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians, payer,
    institutional, generational, constrained, global).

% Bear the physical consequences when autonomous systems fail distinction/proportionality in practice. Metrics certified in testing environments may not generalize to complex urban warfare. Have no voice in defining verification standards, no ability to opt out of being targeted, and no recourse when metric failures cause harm.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones, payer,
    powerless, biographical, trapped, local).

% Communities in conflict zones who would object to machine-decided killing if consulted. Their exclusion is structural: verification regimes are designed by states and technical bodies without civilian participation. They experience the constraint's effects but are not seated at the table where 'equal or exceeding human performance' is defined.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, affected_civilian_communities, excluded,
    powerless, biographical, trapped, local).

% Analyze the constraint's compatibility with IHL principles, treaty interpretation rules, and the Martens Clause. Provide the epistemic infrastructure for contestation but hold no enforcement power. Their analyses inform but do not determine state practice or verification standards.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% Testing laboratories, standards organizations, and military certification authorities that define and administer the performance metrics. Gain institutional standing and funding from being the arbiters of 'demonstrable equivalence.' Their authority depends on the outcomes-based reading remaining the governing framework.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, technical_verification_bodies, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, technical_verification_bodies, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a technology-neutral, measurable standard for distinction and proportionality compliance, replacing subjective human judgment with verifiable performance metrics that can be tested, certified, and audited across platforms and conflicts.
% TRANSFER_FUNCTION: Transfers interpretive authority from humanitarian law custodians (ICRC, legal scholars, judicial bodies) to technical verification bodies (testing labs, certification authorities); transfers risk from military operators to civilian populations when metrics fail to generalize from test environments to complex warfare; transfers procurement revenue and R&D direction to defense contractors building certified autonomous systems.
% ABSENT_VOICES: Civilian populations in conflict zones who bear the consequences of metric failures; future generations affected by the precedent of delegating life/death decisions to machines; non-state armed groups not party to verification regimes; states without technical capacity to develop or verify autonomous systems.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the legal framework would revert to requiring irreducible human judgment at the moment of lethal force application. LAWS development programs would lose their primary legal pathway to deployment. Procurement pipelines would shift back to human-operated systems. The verification regime infrastructure would lose its mandate. The entire autonomy ecosystem built around 'demonstrable equivalence' would restructure.
% FOUNDING_PROBLEM: The problem of ensuring distinction and proportionality compliance in increasingly complex, high-speed warfare where human operators may be overwhelmed by data volume, decision speed requirements, and cognitive fatigue, leading to errors that autonomous systems could theoretically reduce.
% FOUNDING_PROBLEM_CORROBORATION: Military doctrine documents (e.g., US DoD Directive 3000.09, NATO autonomy strategies) attest the problem is live and growing. ICRC, UN Special Rapporteurs on extrajudicial killings, and the International Committee of the Red Cross attest the problem is mischaracterized: the core issue is not human performance limits but the delegation of moral agency to machines, which no performance metric can resolve.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness is moderate (0.48) because the constraint enables a transfer of authority and risk without obviously concentrating wealth — the extraction is structural (interpretive authority, risk distribution) rather than purely financial. Suppression is moderate (0.35) because the constraint operates through technical certification regimes that marginalize legal interpretation, not through direct coercion of civilians (though civilians bear the downside risk). Theater ratio is significant (0.42) because verification regimes involve performative testing in controlled environments that may not reflect battlefield complexity. Accessibility collapse is moderate (0.45) because alternatives (human judgment, categorical prohibition) remain conceptually available but are structurally disadvantaged in procurement and policy venues. Resistance is moderate-high (0.55) because humanitarian law custodians and civil society actively contest the reading in diplomatic forums.
 *
 * PERSPECTIVAL GAP:
 *   From the state armed forces seat, this is genuine coordination: a measurable standard solves the problem of inconsistent human performance. From the humanitarian law custodian seat, this is extraction of their core function (legal interpretation) by a technical regime they cannot control. From the civilian population seat, this is a snare: they are bound by metrics they had no role in defining, with no exit and no recourse. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State armed forces and defense contractors sit at the beneficiary end (d ~0.2): they gain operational flexibility, reduced personnel risk, and revenue. Humanitarian law custodians sit at the target end (d ~0.8): their interpretive authority is structurally displaced by technical metrics they do not control. Civilian populations are trapped targets (d ~0.95): they bear the physical consequences of metric failures with zero exit. Technical verification bodies are dual-positioned: they set the agenda (define metrics) and benefit (institutional standing, funding). International legal scholars are analytical observers (d=0.5): they analyze but do not collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (human performance limits in complex warfare) remains live per military doctrine, but humanitarian law custodians argue it misstates the problem (delegation of moral agency, not performance). This mismatch — status=contested, disappearance=world_rearranges — flags potential mandatrophy: the arrangement may persist because it serves military/industrial interests even if the humanitarian justification is contested. The constraint is not a piton because it is actively maintained and expanded, not theatrically preserved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_adequacy_for_proportionality,
    'Can quantitative performance metrics (precision, recall, false positive rates) genuinely capture the proportionality calculus, which requires context-sensitive weighing of military advantage against anticipated civilian harm?',
    'Empirical analysis of whether test-environment metrics correlate with battlefield outcomes in complex urban warfare; legal analysis of whether proportionality is reducible to quantitative thresholds.',
    'If metrics cannot capture proportionality, the coordination function is illusory and the constraint collapses toward snare (extraction of humanitarian law''s core function without genuine replacement). If metrics can, the tangled_rope classification holds with genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metric_adequacy_for_proportionality, conceptual, 'Whether technical metrics can substitute for legal proportionality judgment.').

omega_variable(
    generalization_gap_risk_transfer,
    'Does the risk transfer to civilian populations from metric generalization failure constitute extraction, or is it an acceptable residual risk of any weapons system?',
    'Comparative analysis of civilian harm rates: human-operated vs. autonomous systems in comparable conflicts; legal assessment of whether verification regimes create a duty of care that shifts liability.',
    'If risk transfer is extractive (systematic underperformance in deployment vs. test), extraction score rises and constraint trends toward snare. If risk is comparable or lower, coordination function is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generalization_gap_risk_transfer, empirical, 'Whether civilian risk from metric failure is structural extraction or acceptable residual risk.').

omega_variable(
    reading_relations_foreclosure,
    'Does the outcomes-based reading practically foreclose the human_agency_reading in states that adopt it, or do they coexist as live policy options?',
    'Track state practice: do states that adopt metric-based verification for some systems retain human-judgment requirements for others, or does adoption of this reading eliminate the human_agency_reading from their legal framework?',
    'If foreclosure occurs in practice, the relation should be ''forecloses'' not ''coexists_with'', changing the kernel''s structural dynamics. If coexistence, current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_foreclosure, conceptual, 'Whether adoption of outcomes-based reading eliminates human_agency_reading as a live option within a state''s framework.').

omega_variable(
    suppression_mechanism_civilian_marginalization,
    'Is the marginalization of civilian voices in verification standard-setting structural (institutional exclusion) or internalized (civilian acceptance of expert authority)?',
    'Analyze whether affected communities have formal participation channels in weapons review processes (Article 36 reviews); survey whether civilian populations in conflict zones view technical certification as legitimate.',
    'If structural, suppression is higher than measured and civilians are more trapped. If internalized, the constraint''s persistence depends partly on manufactured consent, suggesting higher theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_civilian_marginalization, empirical, 'Structural vs. internalized suppression of civilian participation in verification governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 2010, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2010, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(ihl__tr_t2015, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(ihl__tr_t2018, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2018, 0.33).
narrative_ontology:measurement(ihl__tr_t2021, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(ihl__tr_t2024, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement(ihl__tr_t2027, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2027, 0.41).
narrative_ontology:measurement(ihl__tr_t2030, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2030, 0.42).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2010, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(ihl__be_t2015, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2015, 0.32).
narrative_ontology:measurement(ihl__be_t2018, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement(ihl__be_t2021, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2021, 0.43).
narrative_ontology:measurement(ihl__be_t2024, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2024, 0.46).
narrative_ontology:measurement(ihl__be_t2027, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2027, 0.48).
narrative_ontology:measurement(ihl__be_t2030, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2030, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2010, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement(ihl__su_t2015, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2015, 0.22).
narrative_ontology:measurement(ihl__su_t2018, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2018, 0.28).
narrative_ontology:measurement(ihl__su_t2021, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2021, 0.32).
narrative_ontology:measurement(ihl__su_t2024, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2024, 0.34).
narrative_ontology:measurement(ihl__su_t2027, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2027, 0.35).
narrative_ontology:measurement(ihl__su_t2030, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2030, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__outcomes_based_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, autonomous_weapons_verification_regime).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, article_36_weapons_review_process).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ihl_distinction_proportionality kernel. The outcomes_based_reading instantiates a technology-neutral, metric-based compliance standard. The human_agency_reading requires irreducible human judgment. The categorical_prohibition_reading prohibits autonomous weapons per se. The three readings have different ε values, different beneficiary/victim structures, and different claimed types. They are linked via network.affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__outcomes_based_reading, institutional, 0.15).
constraint_indexing:directionality_override(ihl_distinction_proportionality__outcomes_based_reading, organized, 0.25).
constraint_indexing:directionality_override(ihl_distinction_proportionality__outcomes_based_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
