% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Proportionality-Conditioned Vaccine Mandate Legitimacy
 *   domain: public_health/constitutional_law
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda_setter/beneficiary (institutional/arbitrage) — administers the three-prong test, gains legitimacy when applied well
 *   - immunocompromised_populations: beneficiary (powerless/trapped) — depends on honestly-applied mandates for severe pathogens
 *   - low_severity_pathogen_mandate_targets: payer (moderate/constrained) — bears cost of proportionality test stretched to marginal cases
 *   - vaccine_injured_minority: payer (powerless/trapped) — tail-risk cost the population-level calculus discounts
 *   - courts_and_ethics_review_bodies: observer (institutional/analytical) — enforces or exposes pretextual application of the test
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.42).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.5).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Proportionality-Conditioned Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health/constitutional_law").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, 'a97044b4-dede-47f4-9559-c3a7eeb6875b').
narrative_ontology:cs_kernel_codification('a97044b4-dede-47f4-9559-c3a7eeb6875b', distributed).
narrative_ontology:cs_authority_grounding('a97044b4-dede-47f4-9559-c3a7eeb6875b', practice).
narrative_ontology:cs_interpretation_layer_present('a97044b4-dede-47f4-9559-c3a7eeb6875b').
narrative_ontology:cs_reading_relation('a97044b4-dede-47f4-9559-c3a7eeb6875b', mandate_legitimacy_scope__public_health_primary, influences).
narrative_ontology:cs_reading_relation('a97044b4-dede-47f4-9559-c3a7eeb6875b', mandate_legitimacy_scope__bodily_autonomy_primary, influences).
narrative_ontology:cs_axiom('a97044b4-dede-47f4-9559-c3a7eeb6875b', foundational, legitimacy_is_pathogen_conditional).
narrative_ontology:cs_axiom_status(legitimacy_is_pathogen_conditional, holdable).
narrative_ontology:cs_axiom_grounding('a97044b4-dede-47f4-9559-c3a7eeb6875b', legitimacy_is_pathogen_conditional, instrumental).
narrative_ontology:cs_axiom('a97044b4-dede-47f4-9559-c3a7eeb6875b', secondary, less_restrictive_alternative_exhaustion_required).
narrative_ontology:cs_axiom_status(less_restrictive_alternative_exhaustion_required, holdable).
narrative_ontology:cs_axiom_grounding('a97044b4-dede-47f4-9559-c3a7eeb6875b', less_restrictive_alternative_exhaustion_required, empirically_contingent).
narrative_ontology:cs_reference_frame('a97044b4-dede-47f4-9559-c3a7eeb6875b', jacobson_era_rational_basis_review).
narrative_ontology:cs_drift_state('a97044b4-dede-47f4-9559-c3a7eeb6875b', post_covid_mandate_litigation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a97044b4-dede-47f4-9559-c3a7eeb6875b', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, school_age_children).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, low_severity_pathogen_mandate_targets).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, vaccine_injured_minority).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, workers_under_marginal_indication_mandates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines which pathogens meet the severity/efficacy/alternatives threshold and issues or lifts mandates accordingly. Gains legitimacy and institutional capacity when the proportionality test is applied credibly (measles-tier mandates hold up in court); loses legitimacy when it stretches the framework to marginal cases (annual flu mandates for low-risk workers) and courts or public trust push back.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, public_health_agencies, beneficiary).

% Cannot be vaccinated themselves and depend on herd-level coverage for protection from high-severity, high-transmissibility diseases like measles. The proportionality framework, applied honestly to a genuinely severe pathogen, is the mechanism that protects them; they have no exit from exposure risk if coverage collapses.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Attend settings where high-severity communicable disease spreads fastest. Mandates keyed to genuinely severe, well-characterized pathogens with strong vaccine safety data protect this population with minimal proportionate burden on their families, who retain exemption pathways in most jurisdictions.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, school_age_children, beneficiary,
    powerless, biographical, constrained, regional).

% Subject to mandates for pathogens whose severity, transmissibility, or vaccine efficacy profile is contested or marginal (e.g., seasonal flu in low-risk adults). The proportionality test, when applied loosely or by institutional habit rather than rigorous re-assessment, extends mandate coercion to populations for whom the coordination case is genuinely weak — this is where the reading's own logic identifies illegitimate extraction.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, low_severity_pathogen_mandate_targets, payer,
    moderate, biographical, constrained, national).

% A small population experiences genuine adverse events; the proportionality reading's efficacy/safety prong is supposed to weigh this cost, but the aggregate risk-benefit calculus at the population level structurally discounts identifiable individual harm. Compensation and exemption mechanisms are typically slow, underfunded, or contested, leaving this group to absorb the tail risk that the framework's math treats as acceptable at scale.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vaccine_injured_minority, payer,
    powerless, biographical, trapped, national).

% Employer- or institution-imposed mandates sometimes outrun the pathogen's actual severity/efficacy/alternatives profile, using the general legitimacy of proportionality reasoning as cover for policies that would fail rigorous re-application of the same test. Exit means job loss or relocation, not a genuine alternative within the employment relationship.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, workers_under_marginal_indication_mandates, payer,
    moderate, biographical, constrained, regional).

% Adjudicate whether a specific mandate satisfies the three-prong test in a given case, comparing the claimed pathogen severity, vaccine safety/efficacy data, and availability of less restrictive alternatives (testing, isolation, voluntary uptake incentives) against the mandate actually imposed. Their rulings are the mechanism by which the reading's internal standard is enforced or exposed as pretextual.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, courts_and_ethics_review_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__proportionality_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes mandates that solve a genuine, severe collective-action problem (a highly transmissible, high-severity disease with a well-characterized safe and effective vaccine and no comparably effective less-restrictive alternative) from mandates that borrow the legitimacy of that category without meeting its conditions.
% TRANSFER_FUNCTION: Where the test is satisfied, the arrangement transfers a bounded bodily-autonomy cost from mandate targets to the pool of vulnerable non-vaccinated persons who gain herd protection. Where the test is stretched to marginal cases, it transfers a bodily-autonomy and economic cost from marginal-indication mandate targets to institutional actors (employers, agencies) who avoid the harder task of case-specific proportionality assessment.
% ABSENT_VOICES: Vaccine-injured individuals and workers under marginal-indication mandates rarely have a direct seat in the policy-setting process; their costs are aggregated into population-level risk-benefit ratios that structurally underweight identifiable minority harm. Pathogen-specific epidemiological dissent (researchers who dispute a given pathogen's severity classification) is also frequently absent from the enforcement-facing conversation.
% DISAPPEARANCE_RATIONALE: If the proportionality standard vanished, high-severity mandates (measles-tier) would likely be replaced by either unconditional public-health-primary mandates or contested bodily-autonomy-primary refusals — the underlying disease dynamics remain, but the legitimacy test that currently sorts strong from weak mandate cases would disappear, and courts would have to invent a new standard or default to one of the sibling readings. Public health agencies dispute this would matter (they claim to already apply proportionality informally); civil liberties advocates and marginal-mandate targets say its formal disappearance would remove their only current legal lever against overreach.
% FOUNDING_PROBLEM: Courts and public health law needed a mechanism to distinguish genuinely necessary compulsory vaccination from opportunistic or precautionary overreach, without either banning all mandates (ignoring real epidemic risk) or permitting all mandates (ignoring bodily autonomy) — the three-prong test (severity, safety/efficacy, alternatives) was built as that sorting mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and epidemiologists outside both the mandate-issuing agencies and the plaintiff bar broadly attest the sorting problem remains live (pathogens genuinely differ in severity and vaccine profiles differ in evidentiary maturity). But independent public-health-law reviewers and litigation outcomes data suggest the test is inconsistently applied in practice — agencies invoke it to justify marginal mandates as often as courts invoke it to strike them down, which is the corroborated basis for the contested status rather than a claim from either the agencies or the mandate targets themselves.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, contested).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).
:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at moderate (0.42) rather than high because the proportionality reading's structural design, when honestly applied, is genuinely responsive to pathogen-specific facts — this is not a pretextual mandate regime. But it is not low because the same three-prong language can be and is invoked for marginal-indication mandates (workplace flu mandates for low-risk staff, for instance) where a rigorous re-application of the same test would likely fail. Suppression sits at moderate (0.5) reflecting real enforcement machinery (exemption denial, employment consequences) but also genuine exemption pathways that keep suppression short of the coercive maximum. Theater ratio is modest (0.22) and rising slowly — some administrative proportionality review is genuinely substantive, some is procedural box-checking that doesn't actually re-examine severity/efficacy/alternatives data on a rolling basis.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies sit near the beneficiary end: they administer the test and gain institutional legitimacy from cases where it holds up, with arbitrage-grade exit (they can retreat to a different pathogen's mandate if one is struck down). Immunocompromised populations and school-age children are structural beneficiaries of honestly-applied high-severity mandates — low d, the constraint subsidizes their safety. Low-severity-pathogen mandate targets and marginal-indication workers are targets specifically where the test is stretched past its own conditions — high d, real extraction. Vaccine-injured minority is the hardest case: they are structurally harmed even under a rigorously-applied test, because population-level risk-benefit math discounts individual-level tail harm by design, not by misapplication — this is the reading's own residual cost, not a corruption of it.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading exists precisely to prevent Mandatrophy in both directions: it resists collapsing into public_health_primary (which would legitimate every mandate regardless of whether the pathogen or vaccine profile actually warrants compulsion) and resists collapsing into bodily_autonomy_primary (which would delegitimate even the measles-tier mandate that genuinely protects trapped, powerless beneficiaries). The classification as tangled_rope rather than rope reflects that the coordination function is real and severity-conditional but the same structure is used, in marginal cases, to extract compliance without meeting its own stated conditions — the enforcement apparatus doesn't distinguish rigorous from lazy invocation of the test at the point of compulsion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Does the proportionality reading''s per-pathogen conditionality genuinely coexist with the public_health_primary and bodily_autonomy_primary readings as live alternative frameworks, or does its explanatory success at sorting hard cases (measles vs. flu) functionally displace both siblings in practice even though no single ruling formally forecloses either?',
    'Track citation patterns in appellate mandate litigation over a multi-decade window: if courts increasingly resolve cases using the three-prong test rather than invoking either sibling''s categorical premise, that is evidence of de facto (not de jure) displacement.',
    'If displacement is occurring, the coexists_with relations declared here understate this reading''s structural dominance; if not, the readings genuinely persist as competing live frameworks as declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Whether proportionality reading''s practical success constitutes informal foreclosure of its siblings despite formal coexistence.').

omega_variable(
    severity_threshold_construction,
    'Who determines the severity/efficacy/alternatives thresholds that make a given pathogen mandate ''proportionate,'' and is that determination itself insulated from the same institutional incentives (agency legitimacy, avoiding liability, administrative convenience) that this reading is designed to check?',
    'Compare threshold-setting procedures across pathogens with contested severity classifications (e.g., seasonal flu, COVID-19 boosters for low-risk groups) for evidence of agency self-interest in the threshold determination versus independent epidemiological input.',
    'If threshold-setting is captured by the same agencies that benefit from mandate issuance, the proportionality test''s internal check is compromised and ε should be revised upward; if threshold-setting is genuinely independent, the moderate ε and tangled_rope classification (rather than snare) are well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_threshold_construction, empirical, 'Whether the proportionality test''s threshold-setting process is independent of the agencies it constrains.').

omega_variable(
    aggregate_versus_individual_harm_weighting,
    'Is the vaccine-injured minority''s harm a flaw in this reading''s application, or a structural feature of any population-level proportionality calculus that trades identifiable minority harm for aggregate majority benefit?',
    'Examine whether compensation and exemption mechanisms for vaccine injury are funded and administered independently of the mandate-issuing agency, and whether their adequacy tracks improvements in the proportionality test''s rigor or remains constant regardless.',
    'If compensation adequacy is structurally decoupled from test rigor, the vaccine-injured minority''s payer status is intrinsic to the reading itself, not a symptom of lazy application — this would argue for authoring a higher floor ε even under ideal administration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aggregate_versus_individual_harm_weighting, conceptual, 'Whether minority vaccine-injury harm is a fixable application flaw or an intrinsic feature of population-level proportionality reasoning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(mand_tr_t4, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mand_be_t4, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(mand_su_t4, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 24, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the mandate_legitimacy_scope kernel. public_health_primary treats mandate legitimacy as fixed by state authority to protect the vulnerable, regardless of pathogen-specific severity/efficacy/alternatives analysis — its ε and victim set do not vary by pathogen. bodily_autonomy_primary treats non-consensual medical intervention as categorically impermissible regardless of collective benefit — under that reading every mandate is extractive by definition, and its ε is authored high and pathogen-invariant. This proportionality_reading is the only one of the three where ε and the victim set are conditional on disease parameters: it authors moderate ε (0.42) because it is designed to legitimate high-severity mandates while flagging marginal ones, producing a victim set (low_severity_pathogen_mandate_targets, workers_under_marginal_indication_mandates) that would not exist under either sibling's uniform treatment. All three are linked here as a constraint family; none averages or hedges across the others per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
