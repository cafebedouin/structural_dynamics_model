% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail-Risk Dominance in Nuclear Energy Risk Assessment
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'catastrophic_tail_dominant' reading
 *   of the contested kernel 'acceptable_risk_for_energy'. The kernel is the
 *   question: what risk framework should govern nuclear energy acceptability?
 *   This reading asserts that low-probability, high-consequence events (core
 *   damage, large early release, repository failure over geological time)
 *   must dominate the calculus because their irreversibility and
 *   intergenerational burden cannot be compensated by expected-value gains.
 *   The sibling readings — 'expected_value_dominant' (probability ×
 *   consequence optimization) and 'comparative_risk_dominant' (nuclear vs.
 *   coal/climate trade-offs) — are distinct constraints with different ε,
 *   different victim sets, and different structural dynamics. This story
 *   instantiates ONLY the catastrophic_tail_dominant reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.82).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.78).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.82).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail-Risk Dominance in Nuclear Energy Risk Assessment").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, 'fb7c8d23-9657-4e0d-8f30-af417aa75db8').
narrative_ontology:cs_kernel_codification('fb7c8d23-9657-4e0d-8f30-af417aa75db8', formalized).
narrative_ontology:cs_authority_grounding('fb7c8d23-9657-4e0d-8f30-af417aa75db8', lineage).
narrative_ontology:cs_interpretation_layer_present('fb7c8d23-9657-4e0d-8f30-af417aa75db8').
narrative_ontology:cs_reading_relation('fb7c8d23-9657-4e0d-8f30-af417aa75db8', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('fb7c8d23-9657-4e0d-8f30-af417aa75db8', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('fb7c8d23-9657-4e0d-8f30-af417aa75db8', foundational, irreversibility_aversion_overrides_expected_value).
narrative_ontology:cs_axiom_status(irreversibility_aversion_overrides_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('fb7c8d23-9657-4e0d-8f30-af417aa75db8', irreversibility_aversion_overrides_expected_value, deontological).
narrative_ontology:cs_axiom('fb7c8d23-9657-4e0d-8f30-af417aa75db8', foundational, intergenerational_equity_requires_non_compensatory_thresholds).
narrative_ontology:cs_axiom_status(intergenerational_equity_requires_non_compensatory_thresholds, holdable).
narrative_ontology:cs_axiom_grounding('fb7c8d23-9657-4e0d-8f30-af417aa75db8', intergenerational_equity_requires_non_compensatory_thresholds, deontological).
narrative_ontology:cs_reference_frame('fb7c8d23-9657-4e0d-8f30-af417aa75db8', post_war_radiological_protection_consensus).
narrative_ontology:cs_drift_state('fb7c8d23-9657-4e0d-8f30-af417aa75db8', contemporary_decarbonization_imperative, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fb7c8d23-9657-4e0d-8f30-af417aa75db8', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, radiation_protection_regulators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_liability_insurers).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, waste_management_contractors).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_ethics_institutions).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_operators).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, ratepayers_in_nuclear_jurisdictions).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_intensive_industries).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, decarbonization_policy_architects).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_principle_as_governing_standard).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_equity_requires_irreversibility_aversion).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_assessment_is_inadequate_for_catastrophic_tails).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set dose limits, siting criteria, and emergency planning zones based on ALARA and linear no-threshold models. Their authority derives from statutory mandate and international harmonization (ICRP, IAEA). They collect budget authority and institutional legitimacy from maintaining the precautionary regime. Exit for them means career transition to consultancy or international bodies — high option value.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, radiation_protection_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate in a market where state-backed liability caps (Price-Anderson, Paris/Brussels conventions) limit their exposure while the tail-risk framing justifies high premiums and limited competition. They benefit from the constraint's suppression of probabilistic trade-offs that would expose the cap structure. Exit is mobile — they can reallocate capital to other lines.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_liability_insurers, beneficiary,
    organized, biographical, mobile, global).

% Receive multi-decadal contracts for interim storage, repository development, and decommissioning funded by ratepayer surcharges and government appropriations. The tail-risk framing transforms waste from an engineering optimization problem into a perpetual guardianship mandate, securing revenue streams. Exit is constrained by specialized assets and regulatory capture of the licensing pathway.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, waste_management_contractors, beneficiary,
    organized, generational, constrained, national).

% Academic centers, NGOs, and advisory bodies that produce the ethical frameworks justifying irreversibility aversion. Their funding and relevance depend on the tail-risk narrative dominating policy. Exit is identity-locked — their professional self-concept is constituted by this frame; abandoning it dissolves their institutional reason for being.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_ethics_institutions, beneficiary,
    moderate, civilizational, identity_locked, global).

% Bear the full cost of compliance with dose limits, security requirements, and waste fees that embed tail-risk premiums. They cannot exit the regulatory regime without ceasing operations; divestment is constrained by asset specificity, licensing non-transferability, and decommissioning liabilities that follow the license. The constraint extracts via cost escalation and stranded asset risk.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_operators, payer,
    powerful, biographical, constrained, national).

% Pay embedded regulatory costs, waste surcharges, and decommissioning fees through electricity rates. In regulated markets they cannot choose an alternative supplier; in competitive markets the costs are baked into wholesale prices. Exit is constrained — geographic mobility is the only real option, and the burden is regressive.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, ratepayers_in_nuclear_jurisdictions, payer,
    moderate, biographical, constrained, regional).

% Face higher electricity costs from nuclear's regulatory burden, reducing competitiveness against jurisdictions with less tail-averse regimes. They can relocate production (mobile exit), which the constraint's advocates treat as feature not bug. Their exit option disciplines the constraint's spatial scope but not its structural logic.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_intensive_industries, payer,
    powerful, biographical, mobile, global).

% Need nuclear as a firm low-carbon baseload to meet net-zero targets, but the tail-risk framing makes nuclear deployment slow, expensive, and politically fragile. They bear the opportunity cost of foregone decarbonization speed. Exit is identity-locked — their professional mandate is decarbonization; admitting nuclear's necessity under a different risk frame creates cognitive dissonance with the precautionary commitments that animate their coalition.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, decarbonization_policy_architects, payer,
    institutional, generational, identity_locked, national).

% Argue that tail-risk aversion in wealthy nations shifts fossil burden to the Global South and delays just transition. They are excluded from nuclear licensing proceedings and radiation protection standard-setting. Their exit is trapped — the institutional venues are structurally closed to their framing.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_justice_movements, excluded,
    organized, generational, trapped, global).

% Develop PSA Level 3 methods that quantify tail risks in probability-consequence space. Their work is cited to justify the constraint but their methodological commitment to quantification is suppressed by the constraint's insistence on non-compensatory thresholds. They observe from the analytical seat; their exit is analytical — they can change research focus.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_assessment_community, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal consensus on an acceptable boundary for radiological risk by establishing a non-negotiable floor of protection that prevents a race-to-the-bottom in safety standards across jurisdictions and operator generations.
% TRANSFER_FUNCTION: Moves compliance costs, waste liabilities, and opportunity costs of foregone nuclear deployment from the beneficiaries (regulators, insurers, waste contractors, ethics institutions) to the payers (operators, ratepayers, energy-intensive industries, decarbonization architects). The transfer is mediated through rate structures, liability caps, and licensing gatekeeping.
% ABSENT_VOICES: Communities hosting waste facilities (often Indigenous or economically marginalized) who bear localized burden without consent; Global South populations whose energy access is constrained by export of tail-risk aversion via financing and technology transfer restrictions; future generations who inherit both the waste guardianship burden and the climate damage from delayed decarbonization — neither present in the rooms where dose limits and repository standards are set.
% DISAPPEARANCE_RATIONALE: If the tail-risk dominance constraint vanished overnight, nuclear licensing would shift to comparative or expected-value frameworks within 5-10 years. New builds would accelerate in OECD nations; waste policy would pivot to engineered optimization (deep boreholes, partitioning/transmutation) rather than perpetual guardianship. Liability caps would face pressure to align with actuarial risk. Decarbonization pathways would re-optimize. The world rearranges because the constraint actively suppresses alternative risk frames that have live institutional constituencies.
% FOUNDING_PROBLEM: Post-WWII radiological protection needed a simple, defensible standard to prevent the horrors of early radiation exposure (radium dial painters, Hiroshima/Nagasaki, early weapons testing) from recurring in civilian nuclear power. The linear no-threshold model and ALARA provided a bright line that could be enforced without requiring probabilistic sophistication from regulators or trust in operator self-regulation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by ICRP Publication 1 (1959) and the legislative history of the Atomic Energy Act amendments. Radiation protection regulators and ethics institutions attest it remains live (ongoing low-dose uncertainty, new exposure pathways). Nuclear operators, PSA practitioners, and decarbonization architects attest it is substantially solved for civilian power (operational dose records, Gen III+ passive safety, empirical health physics from 70+ reactor-years) and the constraint now serves rent extraction and coalition maintenance. Independent corroboration: UNSCEAR epidemiological reviews, NRC SOARCA studies, and IPCC mitigation pathway literature all document the shift.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint transfers massive compliance costs and opportunity costs to payers while beneficiaries capture institutional rents, liability protection, and perpetual waste contracts. Suppression (0.78) is high because the constraint actively excludes probabilistic trade-off framings from licensing proceedings (NRC's 'adequate protection' standard forbids cost-benefit on safety margins) and marginalizes comparative risk arguments. Theater ratio (0.45) is substantial and rising — the safety review function is real but an increasing share of regulatory activity performs guardianship theater (documentation of defense-in-depth for scenarios already physically implausible) rather than reducing measurable risk. Accessibility collapse (0.72) is high: once the tail-risk frame is accepted, alternatives (expected-value, comparative) are structurally excluded from the licensing logic, not merely disfavored. Resistance (0.55) is moderate: operators and decarbonization advocates push back but within the frame (seeking exemptions, not frame change).
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (regulators) and beneficiary seats experience this as genuine coordination — a necessary floor preventing regulatory capture by operators. The payer seats (operators, ratepayers, decarbonization architects) experience it as enforced extraction — costs escalating without measurable risk reduction, alternatives suppressed. The engine computes this divergence from the structural data: beneficiaries have low directionality (d near 0), payers have high directionality (d near 1), excluded have trapped exit amplifying their effective extraction. The claimed_type (tangled_rope) reflects the authoring seat's judgment that BOTH coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Radiation protection regulators are agenda_setters with institutional power and arbitrage exit — they set the rules and can monetize their expertise globally. Nuclear liability insurers and waste contractors are beneficiaries with organized power; insurers have mobile exit (capital reallocation), contractors have constrained exit (specialized assets). Intergenerational ethics institutions are beneficiaries with identity_locked exit — their professional identity fuses with the frame. Nuclear operators, ratepayers, and energy-intensive industries are payers; operators and ratepayers have constrained exit (asset specificity, geographic lock-in), industries have mobile exit (relocation). Decarbonization architects are payers with identity_locked exit — their professional mandate creates cognitive lock-in. Climate justice movements are excluded with trapped exit — venues structurally closed. PSA community are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing early-era radiological horrors) is contested: regulators and ethics institutions say it remains live; operators, PSA practitioners, and decarbonization architects say it is substantially solved for modern designs. The constraint persists because the beneficiary coalition (regulators + insurers + waste contractors + ethics institutions) has institutional inertia and the payer coalition is fragmented (operators constrained, ratepayers diffuse, industries mobile, decarbonization architects identity-locked). This is mandatrophy: the original coordination function (simple bright-line standard for a nascent industry) has atrophied, but the constraint persists through coalition maintenance and identity fusion. The theater ratio rise tracks the shift from functional coordination to performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the catastrophic_tail_dominant reading a distinct constraint with its own ε, or a perspectival lens on a single underlying constraint?',
    'Apply the ε-invariance test: if changing the observable (LNT vs. threshold dose-response; waste isolation vs. engineered optimization; probabilistic vs. non-compensatory decision rules) changes the measured extractiveness for the SAME stakeholder constellation, they are distinct constraints. Empirical test: compare ε for operators under each reading using the same cost data.',
    'If distinct, each reading gets its own constraint story with independent classification; if single, the framework must model observable-dependent classification (which DP-001 forbids). This omega guards the kernel decomposition discipline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel decomposition into three readings satisfies ε-invariance.').

omega_variable(
    suppression_mechanism_tail_risk,
    'Is the suppression of probabilistic and comparative risk framings structural (regulatory forbearance, statutory prohibition) or internalized (professional socialization, identity fusion among decarbonization architects)?',
    'Post-exit trajectory analysis: track decarbonization architects who leave the field — do they adopt comparative/expected-value framings, or carry the suppression with them? Compare with regulators who rotate to industry.',
    'If internalized, effective suppression is higher than structural measure — the constraint''s victims participate in their own suppression. This would increase χ for identity-locked payers beyond the structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_tail_risk, empirical, 'Structural vs. internalized suppression in the tail-risk dominance constraint.').

omega_variable(
    waste_guardianship_vs_optimization_boundary,
    'Is the perpetual guardianship mandate for nuclear waste a genuine coordination function (solving intergenerational trust) or extraction cover (securing contractor revenue)?',
    'Natural experiment: jurisdictions pursuing deep geological repositories with retrievability (Finland, Sweden) vs. those mandating monitored retrievable storage indefinitely (US). If retrievability reduces cost without increasing measured risk, the guardianship mandate has extractive overhead.',
    'If guardianship has extractive overhead, the waste_management_contractor beneficiary declaration is validated and the tangled_rope classification is strengthened. If genuine coordination, the constraint may be more rope-like than tangled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(waste_guardianship_vs_optimization_boundary, empirical, 'Whether waste disposal''s transformation from engineering problem to guardianship constraint is functional or extractive.').

omega_variable(
    decarbonization_architect_identity_lock,
    'Is the identity_locked exit of decarbonization policy architects a genuine professional identity fusion, or a strategic positioning that would dissolve under political pressure?',
    'Counterfactual probe: if a major climate policy institution (IEA, IPCC WGIII) formally endorsed comparative_risk_dominant framing, how many architects would switch vs. defect? Track citation networks and personnel flows.',
    'If strategic, their directionality is overstated — they are mobile, not identity_locked. This would reduce effective extraction for that seat and weaken the tangled_rope''s payer coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decarbonization_architect_identity_lock, preference, 'Whether decarbonization architects'' identity lock is structural or contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 1955, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1955, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(acce_tr_t1970, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1970, 0.22).
narrative_ontology:measurement(acce_tr_t1979, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1979, 0.31).
narrative_ontology:measurement(acce_tr_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1986, 0.38).
narrative_ontology:measurement(acce_tr_t2000, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(acce_tr_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2011, 0.44).
narrative_ontology:measurement(acce_tr_t2025, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(acce_be_t1955, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1955, 0.35).
narrative_ontology:measurement(acce_be_t1970, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement(acce_be_t1979, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1979, 0.62).
narrative_ontology:measurement(acce_be_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1986, 0.71).
narrative_ontology:measurement(acce_be_t2000, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(acce_be_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2011, 0.81).
narrative_ontology:measurement(acce_be_t2025, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1955, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1955, 0.45).
narrative_ontology:measurement(acce_su_t1970, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(acce_su_t1979, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1979, 0.68).
narrative_ontology:measurement(acce_su_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1986, 0.74).
narrative_ontology:measurement(acce_su_t2000, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2000, 0.76).
narrative_ontology:measurement(acce_su_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2011, 0.78).
narrative_ontology:measurement(acce_su_t2025, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_guardianship_mandate).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, price_anderson_liability_cap).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, alara_regulatory_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'acceptable_risk_for_energy' kernel. The expected_value_dominant reading (probability × consequence optimization) has lower extractiveness (~0.45) and lower suppression (~0.35) — it coordinates via market mechanisms. The comparative_risk_dominant reading (nuclear vs. coal/climate) has moderate extractiveness (~0.55) and suppression (~0.50) — it coordinates via policy trade-off forums. This reading has the highest extractiveness and suppression because it structurally excludes the other two framings from the licensing logic. All three form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, institutional, 0.15).
constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, powerful, 0.85).
constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, moderate, 0.75).
constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, organized, 0.25).
constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
