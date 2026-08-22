% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Regulatory Takings Doctrine: Value Diminution as Compensable Taking
 *   domain: constitutional/legal
 *
 * SUMMARY:
 *   The Fifth Amendment's Takings Clause provides: 'nor shall private
 *   property be taken for public use, without just compensation.' This
 *   constraint story instantiates ONE reading of this contested kernel: the
 *   regulatory takings reading holds that regulations diminishing property
 *   value 'too far' constitute compensable takings, even absent physical
 *   appropriation. Under this reading, regulatory agencies must justify
 *   severe value diminutions through the Penn Central three-factor test
 *   (economic impact, interference with reasonable expectations, character of
 *   government action); if they cannot, compensation is due. The reading
 *   provides property owners broader protection against non-physical
 *   extraction but creates uncertainty in the regulatory space and imposes
 *   fiscal/administrative costs on government. The constraint is authored as
 *   a tangled rope because it coordinates genuine property protection (the
 *   coordination function) while simultaneously extracting compensation
 *   obligations from a diffuse set of payers (regulatory agencies, taxpayers,
 *   capacity-constrained governments). The claim/metric independence
 *   principle is in force: the constraint is CLAIMED as tangled_rope (genuine
 *   coordination + asymmetric extraction + active enforcement all present),
 *   and the metrics describe the empirical extractiveness and suppression the
 *   reading instantiates.
 *
 * KEY AGENTS:
 *   - property_owners_facing_severe_value_diminution: The expanded victim set this reading recognizes — owners whose property value is diminished 60% or more by regulation, absent physical occupation. They benefit from the doctrine (expanded compensation eligibility) but face uncertainty about which losses qualify.
 *   - regulatory_agencies: The primary payers and enforcer-administrators. They must justify regulations through the Penn Central test or face compensation liability. Their capacity to regulate is constrained by fiscal exposure and doctrinal uncertainty.
 *   - taxpayers: The diffuse payers who fund compensation awards; they bear the extracted cost without negotiation or visibility.
 *   - courts_applying_penn_central: The institutional seats that mediate the constraint through case-by-case balancing. Their role is adjudicative, not extractive, but the uncertainty in their doctrine-application feeds the constraint's extractiveness.
 *   - sibling_reading_advocates: Justices, scholars, and advocates defending the physical-only reading; they remain live within the kernel contest but are structurally secondary to this reading's dominance.
 *   - future_regulatory_capacity: An abstract but material victim — the reading's extraction via fiscal and doctrinal burden reduces the state's capacity for future regulation in adjacent domains.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.62).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.58).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine: Value Diminution as Compensable Taking").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional/legal").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, 'b9455238-b409-422a-8a93-5ed8b55ca17f').
narrative_ontology:cs_kernel_codification('b9455238-b409-422a-8a93-5ed8b55ca17f', fixed_text).
narrative_ontology:cs_authority_grounding('b9455238-b409-422a-8a93-5ed8b55ca17f', lineage).
narrative_ontology:cs_interpretation_layer_present('b9455238-b409-422a-8a93-5ed8b55ca17f').
narrative_ontology:cs_reading_relation('b9455238-b409-422a-8a93-5ed8b55ca17f', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9455238-b409-422a-8a93-5ed8b55ca17f', takings_clause_boundary__categorical_takings_reading, influences).
narrative_ontology:cs_axiom('b9455238-b409-422a-8a93-5ed8b55ca17f', foundational, value_diminution_is_taking).
narrative_ontology:cs_axiom_status(value_diminution_is_taking, holdable).
narrative_ontology:cs_axiom_grounding('b9455238-b409-422a-8a93-5ed8b55ca17f', value_diminution_is_taking, deontological).
narrative_ontology:cs_axiom('b9455238-b409-422a-8a93-5ed8b55ca17f', secondary, penn_central_balancing_is_appropriate).
narrative_ontology:cs_axiom_status(penn_central_balancing_is_appropriate, holdable).
narrative_ontology:cs_axiom_grounding('b9455238-b409-422a-8a93-5ed8b55ca17f', penn_central_balancing_is_appropriate, conventional).
narrative_ontology:cs_reference_frame('b9455238-b409-422a-8a93-5ed8b55ca17f', property_protection_through_compensation).
narrative_ontology:cs_drift_state('b9455238-b409-422a-8a93-5ed8b55ca17f', contemporary_regulatory_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b9455238-b409-422a-8a93-5ed8b55ca17f', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners_facing_severe_value_diminution).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, regulatory_agencies).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, taxpayers_funding_compensation).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, future_regulatory_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, regulated_industries).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, taxpayers).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, regulated_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own property subject to regulations that diminish value by 60% or more without physical occupation (wetland restrictions, development prohibitions, zoning changes). Under the regulatory takings reading, they can sue for compensation if the Penn Central test is not satisfied. They benefit from the expanded protection but face uncertainty about which losses qualify; litigation is expensive and outcomes unpredictable.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners_facing_severe_value_diminution, beneficiary,
    moderate, biographical, constrained, national).

% Draft and enforce regulations (environmental, land-use, public health) that may incidentally diminish property values. Under the regulatory takings reading, they must justify severe value diminutions through the Penn Central test or face compensation liability. They set the regulatory agenda but bear the extraction burden through litigation costs and compensation awards. They operate in doctrinal uncertainty about which regulations will withstand takings scrutiny.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__regulatory_takings_reading, regulatory_agencies, payer).

% Fund compensation awards through general tax revenues. The regulatory takings reading increases the class of compensable losses, raising aggregate compensation liability that flows through state and federal budgets. Taxpayers have no voice in specific takings determinations and bear diffuse costs without negotiation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, taxpayers, payer,
    powerless, biographical, trapped, national).

% Apply the Penn Central three-factor test case-by-case, evaluating whether regulations constitute takings. They mediate the constraint through doctrine but do not themselves extract or pay; their role is adjudicative and legitimating.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, courts_applying_penn_central, observer,
    institutional, generational, analytical, national).

% Many face value diminutions through environmental, labor, or safety regulations. They benefit from takings claims (leverage against agencies, compensation eligibility) but also face the uncertainty of the Penn Central test. Large, resourced industries can absorb litigation costs and relocate if needed; smaller operators cannot.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, regulated_industries, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__regulatory_takings_reading, regulated_industries, payer).

% Justices and scholars who argue that the Fifth Amendment applies only to physical appropriations, not value diminutions. They are excluded from the regulatory reading's framing but remain live in dissents and academic debate. They would argue the regulatory reading goes too far in constraining regulatory capacity.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, advocate_for_physical_only_reading, excluded,
    institutional, generational, trapped, national).

% An abstract victim representing the state's diminished capacity for future regulation in adjacent domains (climate, public health, labor) due to fiscal exposure and doctrinal precedent established by the regulatory takings reading. Not an agent, but a material outcome stream that the reading affects.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, future_regulatory_capacity, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(takings_clause_boundary__regulatory_takings_reading, future_regulatory_capacity).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__regulatory_takings_reading, regulatory_agencies).
narrative_ontology:fixing_cost_class(takings_clause_boundary__regulatory_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates property protection and governmental accountability: the Takings Clause solves the problem of ensuring that when government diminishes private property rights for public purposes, it bears the fiscal and political cost. The regulatory reading extends this coordination from physical appropriations to severe value diminutions, providing ex post compensation for non-physical extraction.
% TRANSFER_FUNCTION: Moves fiscal liability (compensation awards, litigation costs, administrative burden) from property owners to regulatory agencies and taxpayers when regulations cause severe value diminutions (60%+ losses). The reading also transfers doctrinal authority: property owners gain standing to contest regulations through takings claims; courts gain authority to second-guess regulatory justifications through Penn Central balancing.
% ABSENT_VOICES: Advocates for the physical-only reading are excluded from the regulatory reading's frame — they would argue that limiting takings to physical appropriations is correct and that extending to value diminutions goes too far. Future generations and regulated industries with moderate power are partially excluded from compensation design — compensation is determined ex post through litigation rather than through negotiated regulatory design.
% DISAPPEARANCE_RATIONALE: If the regulatory takings reading disappeared, regulatory agencies would face far fewer takings liability threats; they could regulate more aggressively without compensation exposure. Property owners' protection against non-physical extraction would shrink to zero (physical appropriations only). Fiscal liability would drop; regulatory capacity would expand. The distribution of protection and extraction would shift dramatically toward agencies.
% FOUNDING_PROBLEM: The Fifth Amendment's Takings Clause was written to prevent unlimited government seizure of private property without compensation. The founding problem is: how to ensure that when government acts for public purposes, it compensates those who bear the cost. The regulatory takings reading extends that concern from overt seizures to regulatory takings — cases where government action diminishes property value so severely it amounts to an uncompensated taking.
% FOUNDING_PROBLEM_CORROBORATION: The regulatory reading's understanding of the founding problem is defended by the majority of contemporary constitutional scholars and the Supreme Court majority (Penn Central, Lucas, Tahoe-Sierra). The physical-only reading's alternative account (the founding problem was only about physical seizures, not value diminutions) is defended by Justice Thomas and a dissenting tradition but has not been endorsed by the current Court majority. Corroboration from outside the benefiting parties: legal historians debate whether the Framers intended takings to cover value diminutions, with scholarly consensus divided but trending toward accepting the regulatory reading as a legitimate interpretation of the Clause's purposes.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness of 0.62 reflects the reading's structural effect: property owners gain a broadened compensation claim (low d for them, near beneficiary), but regulatory agencies and taxpayers face exposure to claims previously uncompensable (high d for them, near target). The expanded victim set is the reading's core structural innovation — it shifts costs from owners to payers in ways that were not present under the physical-only reading. Suppression at 0.58 reflects moderate active enforcement: the Penn Central test is neither fully determinate nor fully opaque; property owners mount challenges with some probability of success, creating real resistance. Theater ratio at 0.41 captures the balancing test's partial performance: the test is genuinely doctrinal (not purely theatrical), but growing judicial skepticism about regulatory justifications introduces performative elements (e.g., agencies staging environmental reviews they do not expect to withstand takings scrutiny). The measurement series shows modest upward drift in extractiveness and theater ratio over the interval, reflecting the doctrine's maturation and growing fiscal awareness among states — as the doctrine ages, agencies and courts internalize the extraction more deeply and the performance of justification becomes more refined. All metrics authored on a single shared time grid (t ∈ {0, 10, 20, 30}) to avoid misalignment.
 *
 * PERSPECTIVAL GAP:
 *   A property owner facing a severe value-diminution regulation experiences the constraint as property protection (the reading enables compensation claims). A regulatory agency experiences the same constraint as a fiscal and doctrinal burden (the reading expands litigation risk). A taxpayer experiences it as a diffuse tax (funding compensation awards without visibility or consent). The engine computes these divergent classifications from the structural data: the owner's beneficiary status yields low d and a type-computation favoring rope/coordination; the agency's high d and target status yield type-computations favoring extraction/snare. The asymmetry is the constraint's signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners whose value is severely diminished are structural beneficiaries under this reading (d approaches 0.0): the doctrine expands their compensation eligibility and provides protection against extraction via regulation. Regulatory agencies and taxpayers are structural targets (d approaches 1.0): they bear the cost of justifying regulations and funding compensation. Courts are neutral adjudicators (d near 0.5), neither collecting nor paying but mediating the distribution. The directionality derivation flows from the beneficiary/victim declarations: owners are named as beneficiaries (the reading protects them), agencies and taxpayers are named as victims (the reading imposes costs on them). No overrides are necessary — the structural data derive the correct d without exception.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is genuine and live: unregulated takings (unlimited government seizure of property) was a real constitutional concern in 1791. The regulatory takings reading extends that concern to value diminutions. The mandatrophy question is whether the Penn Central test still solves that founding problem or has become a barrier itself. The reading does not claim to have solved takings entirely — it claims to have extended the protection to non-physical cases. Agency resistance and fiscal pressure (measured at 0.72 resistance, 0.58 suppression) indicate the constraint persists despite genuine contestation, not performance or decay. The doctrine is live and enforced, not theatrical maintenance of a solved problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Fifth Amendment''s Takings Clause best read as compensating severe economic losses from regulation (''regulatory takings'' frame), or only as prohibiting direct physical appropriations (''physical takings'' frame)?',
    'This is a committed doctrinal choice: the reading selected shapes which victim set is recognized and what compensation burden falls on the state. Historical case law from Penn Central onward instantiates the regulatory reading; the physical-only reading remains live as a jurisprudential position (Justice Thomas, Justices in dissent) without foreclosure.',
    'The regulatory reading expands the compensable victim set to include severe value-diminished owners, making regulatory takings doctrine a constraint on state policy space (tangled_rope structure: coordination of property protection + extraction from fiscal and regulatory capacity). The physical reading would narrow victims to physical possessors only, reducing state fiscal exposure but narrowing property protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest: regulatory vs. physical takings framing.').

omega_variable(
    penn_central_balancing_uncertainty,
    'Does the Penn Central three-factor test (economic impact + interference with reasonable expectations + character of government action) produce stable, predictable results, or does it introduce irreducible uncertainty that defeats the doctrine''s coordination function?',
    'Doctrinal analysis of circuit split outcomes and Supreme Court reversals under Penn Central; comparative study of how different jurisdictions apply the test; empirical measurement of regulatory takings claim success rates.',
    'If the test is highly uncertain, the constraint''s extractiveness is substantially higher (property owners cannot rely on the rule; regulatory agencies cannot rely on the rule; both operate in fog). If the test is stable, extractiveness drops and the coordination function strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_balancing_uncertainty, empirical, 'Stability and predictability of the Penn Central balancing test.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.58) of property owner challenges primarily structural (legal barriers, burden of proof, litigation cost) or internalized (property owners believe the loss is just, uncompensable by nature, or accept regulatory authority''s characterization)?',
    'Post-litigation behavioral studies: do property owners who lose takings claims continue to accept the regulatory characterization, or do they pursue political/legislative remedies? Do they identify as expropriated or as bearing a legitimate social cost?',
    'If suppression is internalized, the constraint operates with lower actual coercive pressure than the structural measures suggest — the constraint persists because owners have accepted the frame. If structural, the constraint persists despite resistance and would collapse without active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Structural vs. internalized suppression of regulatory takings challenges.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the regulatory reading''s expanded victim set logically foreclose the physical-only reading in the same constitutional framework, or can both coexist as live jurisprudential options?',
    'Doctrinal analysis: if accepting the regulatory reading commits one to rejecting the physical-only reading''s core premise (that the Fifth Amendment is silent on value diminution), foreclosure holds. If both can coexist (each party holding one within the same text), coexistence holds.',
    'Foreclosure would mean the regulatory reading dominates; coexistence means the kernel remains genuinely contested and the sibling readings are not reduced to minority positions. Current jurisprudence shows coexistence: the regulatory reading is the majority doctrine, but the physical-only reading remains live in dissents and lower-court debate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical foreclosure vs. doctrinal coexistence of takings readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0, 0.31).
narrative_ontology:measurement(taki_tr_t10, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(taki_tr_t20, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(taki_tr_t30, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 30, 0.41).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(taki_be_t10, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(taki_be_t20, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(taki_be_t30, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(taki_su_t10, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(taki_su_t20, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(taki_su_t30, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__regulatory_takings_reading, 0.14).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__categorical_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, regulatory_permitting_constraint).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, environmental_regulation_extractiveness).

% DUAL FORMULATION NOTE:
% Part of the takings_clause_boundary kernel family. The regulatory_takings_reading expands the victim set relative to the physical_appropriation_reading and creates doctrinal uncertainty that influences sibling readings. All three readings of the kernel should be decomposed as separate constraint stories linked via affects_constraints; they are not alternative measurements of one constraint but structurally distinct readings instantiating different victim sets and extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
