% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine (Diminution-of-Value Reading of the Takings Clause)
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint instantiates the regulatory-takings reading of the
 *   contested Takings Clause kernel: the claim that regulations diminishing a
 *   property's economic value 'too far' — even absent any physical seizure or
 *   occupation — trigger the Fifth Amendment's compensation requirement.
 *   Traced from Pennsylvania Coal v. Mahon through Penn Central
 *   Transportation Co. v. New York City, this reading rejects the narrower
 *   physical-appropriation view and instead subjects most land-use and
 *   environmental regulation to an ad hoc, multi-factor balancing test with
 *   no bright-line threshold. This is a genuinely distinct constraint from
 *   its siblings: the physical-appropriation reading has a much lower ε
 *   (compensation triggers only on possession, a bright-line,
 *   low-litigation-uncertainty rule) and the categorical-takings reading sits
 *   between the two (per se rules for total elimination, Penn Central factors
 *   for everything else). Decomposing rather than blending observables is
 *   required by the ε-invariance principle — merging the three readings into
 *   one story would average away the structurally different victim sets,
 *   enforcement mechanisms, and uncertainty profiles that distinguish them.
 *
 * KEY AGENTS:
 *   - affected_property_owners: primary beneficiaries of the doctrine, use it to obtain compensation for non-physical value destruction
 *   - real_estate_developers: organized beneficiaries who use the doctrine strategically against land-use and environmental regulation
 *   - takings_litigation_bar: professional beneficiaries whose practice depends on the doctrine's inherent uncertainty
 *   - municipalities_and_states: primary payers, bear compensation liability and litigation costs
 *   - environmental_regulatory_agencies: payers whose regulatory ambition is chilled by unpredictable exposure
 *   - neighboring_property_owners_seeking_land_use_protection: excluded third parties who rely on the regulations being challenged
 *   - judiciary_applying_penn_central: agenda-setters administering the ad hoc balancing test case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.58).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.42).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine (Diminution-of-Value Reading of the Takings Clause)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, 'ddd1de6d-b643-4717-ad00-a3c2da62dae5').
narrative_ontology:cs_kernel_codification('ddd1de6d-b643-4717-ad00-a3c2da62dae5', fixed_text).
narrative_ontology:cs_authority_grounding('ddd1de6d-b643-4717-ad00-a3c2da62dae5', lineage).
narrative_ontology:cs_interpretation_layer_present('ddd1de6d-b643-4717-ad00-a3c2da62dae5').
narrative_ontology:cs_reading_relation('ddd1de6d-b643-4717-ad00-a3c2da62dae5', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('ddd1de6d-b643-4717-ad00-a3c2da62dae5', takings_clause_boundary__categorical_takings_reading, influences).
narrative_ontology:cs_axiom('ddd1de6d-b643-4717-ad00-a3c2da62dae5', foundational, regulatory_diminution_can_functionally_equal_seizure).
narrative_ontology:cs_axiom_status(regulatory_diminution_can_functionally_equal_seizure, holdable).
narrative_ontology:cs_axiom_grounding('ddd1de6d-b643-4717-ad00-a3c2da62dae5', regulatory_diminution_can_functionally_equal_seizure, conventional).
narrative_ontology:cs_axiom('ddd1de6d-b643-4717-ad00-a3c2da62dae5', secondary, ad_hoc_multifactor_balancing_is_legitimate_adjudicative_method).
narrative_ontology:cs_axiom_status(ad_hoc_multifactor_balancing_is_legitimate_adjudicative_method, holdable).
narrative_ontology:cs_axiom_grounding('ddd1de6d-b643-4717-ad00-a3c2da62dae5', ad_hoc_multifactor_balancing_is_legitimate_adjudicative_method, instrumental).
narrative_ontology:cs_reference_frame('ddd1de6d-b643-4717-ad00-a3c2da62dae5', pennsylvania_coal_diminution_doctrine).
narrative_ontology:cs_drift_state('ddd1de6d-b643-4717-ad00-a3c2da62dae5', post_penn_central_modern_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ddd1de6d-b643-4717-ad00-a3c2da62dae5', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, affected_property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, real_estate_developers).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, takings_litigation_bar).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, municipalities_and_states).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, environmental_regulatory_agencies).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, neighboring_property_owners_seeking_land_use_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own land or improvements whose economic value is severely diminished by a new land-use, environmental, or zoning regulation without any physical taking of the parcel. Under this reading they can sue for compensation by showing the regulation goes 'too far,' converting a regulatory burden into a compensable taking. Their exit is constrained by sunk investment in the land; litigation is their primary lever.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, affected_property_owners, beneficiary,
    moderate, biographical, constrained, national).

% Hold portfolios of development-stage land and use the diminution-of-value theory strategically to challenge zoning, wetlands, and coastal regulations that block or reduce planned projects. More mobile than individual owners — can shift capital across jurisdictions but use takings claims as leverage to extract settlements or regulatory concessions.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, real_estate_developers, beneficiary,
    organized, biographical, mobile, national).

% Specialized attorneys and advocacy organizations whose practice depends on the existence and expansion of the ad hoc balancing test. They benefit directly from the doctrinal uncertainty this reading creates, since every diminution-of-value claim requires case-specific litigation rather than bright-line resolution.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, takings_litigation_bar, beneficiary,
    organized, generational, arbitrage, national).

% Enact land-use, environmental, and public-welfare regulations and must now budget for potential compensation liability or withdraw regulations rather than risk takings litigation. Cannot exit the constraint — they are bound by the doctrine whenever they regulate land use, and litigation costs and settlement payouts come from public budgets.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, municipalities_and_states, payer,
    institutional, generational, trapped, regional).

% Draft wetlands protections, coastal setback rules, and endangered-species habitat restrictions that this doctrine exposes to takings challenges whenever they substantially reduce a parcel's development value. Regulatory ambition is chilled by the unpredictability of the ad hoc balancing test, which has no bright-line percentage threshold.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, environmental_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Rely on the same zoning and environmental regulations being challenged to protect their own property values, viewsheds, and environmental quality. They are not parties to the takings litigation and have no direct voice in whether a regulation survives a diminution-of-value challenge, even though its invalidation or chilling directly affects them.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, neighboring_property_owners_seeking_land_use_protection, excluded,
    powerless, biographical, trapped, local).

% Courts apply the ad hoc, multi-factor balancing test (economic impact, interference with investment-backed expectations, character of the government action) case by case to decide whether a regulation has gone 'too far.' They administer the doctrine's boundary and could in principle sharpen or abandon it, but each case is decided on its own facts, producing persistent doctrinal uncertainty.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, judiciary_applying_penn_central, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__regulatory_takings_reading, diffuse).
narrative_ontology:fixing_cost_class(takings_clause_boundary__regulatory_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism to prevent government from using regulation to achieve what would otherwise require formal condemnation and compensation — coordinating the boundary between legitimate police-power regulation and de facto expropriation, so that the public does not free-ride on privately borne regulatory costs that in fairness should be spread across taxpayers.
% TRANSFER_FUNCTION: When triggered, moves compensation payments from public treasuries (municipalities, states, regulatory agencies) to owners of regulated property. Independent of that transfer, the doctrine's mere existence moves regulatory drafting resources and legal risk-management costs from agencies to their legal departments and insurers, and shifts litigation fees to the specialized takings bar.
% ABSENT_VOICES: Neighboring property owners, environmental beneficiaries of the regulation, and the diffuse public who benefit from wetlands protection, coastal management, or zoning stability have no standing in the takings claim itself — the litigation is bilateral (owner vs. government) even though the regulation's value was triable in the first place because of external, third-party effects the excluded parties experience directly.
% DISAPPEARANCE_RATIONALE: If the regulatory-takings reading vanished and only physical appropriation triggered compensation, regulatory agencies would face dramatically reduced compensation exposure and could enact aggressive land-use, environmental, and zoning restrictions without take-clause liability; landowners would lose their primary tool for challenging severe non-physical value diminution, and the specialized takings litigation practice would substantially contract.
% FOUNDING_PROBLEM: Courts recognized as early as Pennsylvania Coal v. Mahon (1922) that government could functionally destroy the value of property through regulation just as thoroughly as through physical seizure, and that treating only physical appropriation as compensable would let government evade the Fifth Amendment's compensation requirement by regulatory means — the doctrine was built to close that evasion route.
% FOUNDING_PROBLEM_CORROBORATION: Property-rights scholars and takings claimants attest the evasion risk remains live, citing modern land-use and environmental regulations that approach total value elimination. Land-use planners, environmental law scholars, and several state supreme courts (writing outside the beneficiary set) attest that the doctrine has drifted from closing an evasion loophole to functioning as a standing veto threat over ordinary environmental and zoning regulation, chilling regulation whose value-destruction is far short of the founding concern.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that a substantial share of the doctrine's operation now consists of leveraging litigation risk to extract settlements or regulatory concessions well beyond the founding concern (functional condemnation-by-regulation), rather than compensating owners for genuine near-total value elimination. Suppression (0.42) is moderate: the doctrine does not suppress regulatory alternatives outright, but it does chill regulatory drafting through unpredictable liability exposure, which is a real if indirect suppressive force on the payer side. Theater ratio (0.30) captures that a meaningful fraction of takings litigation activity is now oriented toward establishing negotiating leverage rather than genuinely vindicating the founding evasion-prevention concern. Accessibility collapse is low-moderate (0.35) because regulators retain real alternatives (narrower drafting, phased implementation, transferable development rights) even under this doctrine — it has not foreclosed regulatory options, only raised their cost and uncertainty. Resistance is high (0.68): regulatory agencies, land-use planners, and environmental law scholars actively contest the doctrine's scope in litigation and legislative drafting, which is inconsistent with a settled natural-law-like arrangement and consistent with a contested, actively-defended doctrinal structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Affected property owners, developers, and the takings bar sit near the beneficiary end: the doctrine's existence and its inherent ad hoc uncertainty are what generate their gains (compensation, leverage, or litigation fees). Municipalities, states, and environmental agencies sit near the target end: they are institutional but trapped or constrained — they cannot exit the doctrine while retaining any regulatory function, and its costs land on public budgets they administer but do not control. Neighboring property owners are excluded rather than positioned on the beneficiary/victim axis directly — they experience downstream effects of the doctrine's operation without being a party to its transactions, which is why they are marked excluded rather than payer despite bearing real costs when protective regulation is chilled or invalidated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing regulatory evasion of the compensation requirement through pretextual 'regulation' that functions as seizure) remains partially live — genuine near-total value elimination by regulation still occurs and the evasion concern is not obsolete. But the doctrine's classification as tangled_rope rather than a clean rope or snare captures that it retains a real coordination function (preventing genuine evasion) while having accumulated a substantial extraction layer (strategic litigation against ordinary regulation whose value impact falls well short of the founding concern). Classifying this reading as tangled_rope rather than collapsing it into either pure coordination or pure extraction prevents both a mislabeling of legitimate compensation claims as pure rent-seeking and a mislabeling of the doctrine's now-substantial strategic-litigation function as untainted coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_takings_kernel_reading_choice,
    'Is the diminution-of-value theory the correct reading of the Takings Clause''s original evasion-prevention purpose, or does it overextend the clause beyond its founding concern into ordinary regulatory friction?',
    'This is the committer-level disagreement located at the threshold question in Penn Central: whether ''investment-backed expectations'' and ''economic impact'' should be measured against the whole parcel or a segmented portion, and how severe the diminution must be before it triggers compensation. The physical_appropriation_reading resolves this by rejecting the diminution theory outright; the categorical_takings_reading resolves it by confining per se treatment to total elimination and applying ad hoc balancing only to partial diminution. This story''s reading resolves it by treating severe partial diminution itself as potentially compensable through the balancing test.',
    'If courts adopted the physical_appropriation_reading, the victim set here (municipalities, environmental agencies) would lose most of their compensation exposure and the doctrine''s extractiveness would collapse toward the physical-appropriation constraint''s much lower ε. If courts adopted the categorical_takings_reading, exposure would narrow to near-total-elimination cases, reducing extraction while still leaving a partial-diminution balancing test at a lower intensity than this reading''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_takings_kernel_reading_choice, conceptual, 'Committer-level disagreement over which reading of the Takings Clause correctly locates the evasion-prevention boundary; routes the kernel contest to an omega rather than folding sibling readings into this story''s classification.').

omega_variable(
    ad_hoc_balancing_predictability,
    'Does the Penn Central ad hoc balancing test provide enough predictive structure for regulators and owners to anticipate outcomes, or is its unpredictability itself the mechanism generating strategic-litigation extraction?',
    'Empirical analysis of takings litigation outcomes across jurisdictions and time: if outcomes cluster predictably around identifiable factor-weightings, the test functions closer to a genuine coordination mechanism; if outcomes are highly variable and settlement-driven, the unpredictability itself is generating the extraction this story measures.',
    'High measured unpredictability would support a higher extraction score and validate the tangled_rope classification''s extraction component; demonstrated predictability would push the classification toward a cleaner rope, since the coordination function would dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ad_hoc_balancing_predictability, empirical, 'Whether the balancing test''s unpredictability is itself a component of the doctrine''s extractive mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(taki_tr_t20, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(taki_tr_t60, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement(taki_tr_t80, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 80, 0.29).
narrative_ontology:measurement(taki_tr_t100, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(taki_be_t20, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(taki_be_t60, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(taki_be_t80, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement(taki_be_t100, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(taki_su_t20, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(taki_su_t60, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 60, 0.38).
narrative_ontology:measurement(taki_su_t80, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 80, 0.4).
narrative_ontology:measurement(taki_su_t100, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__regulatory_takings_reading, 0.1).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, categorical_takings_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'Takings Clause boundary' concept, per the ε-invariance principle. physical_appropriation_reading has the lowest ε (bright-line, possession-triggered only). categorical_takings_reading sits intermediate (per se rules for total elimination, Penn Central balancing otherwise). regulatory_takings_reading (this story) has the highest ε because its diminution-of-value theory subjects the widest range of ordinary regulation to compensation exposure through an inherently unpredictable balancing test. All three share the same underlying kernel (the Fifth Amendment's compensation requirement and its boundary) but instantiate structurally distinct constraints with different victim sets, different enforcement mechanisms, and different degrees of doctrinal uncertainty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
