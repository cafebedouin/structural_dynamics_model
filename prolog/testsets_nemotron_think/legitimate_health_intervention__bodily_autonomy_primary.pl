% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: State Medical Mandate Authority (Bodily Autonomy Primary Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story evaluates the state medical mandate authority (the
 *   standing arrangement allowing government to compel medical interventions)
 *   from the bodily_autonomy_primary reading of the
 *   legitimate_health_intervention kernel. The reading holds that legitimacy
 *   requires informed consent and that state coercion violates bodily
 *   integrity regardless of public benefit. From this reading's structural
 *   perspective, the mandate arrangement operates as a snare: it extracts
 *   compliance from individuals through employment and access leverage,
 *   actively suppresses alternatives (exemptions, alternative protections),
 *   and its persistence depends on coercion rather than voluntary
 *   coordination. The claimed_type is snare; the metrics describe the mandate
 *   arrangement's actual operation during the 2020-2024 period.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.72).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.85).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "State Medical Mandate Authority (Bodily Autonomy Primary Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '514485f2-a2c0-459a-8faa-a7a24df32981').
narrative_ontology:cs_kernel_codification('514485f2-a2c0-459a-8faa-a7a24df32981', formalized).
narrative_ontology:cs_authority_grounding('514485f2-a2c0-459a-8faa-a7a24df32981', extraction).
narrative_ontology:cs_interpretation_layer_present('514485f2-a2c0-459a-8faa-a7a24df32981').
narrative_ontology:cs_reading_relation('514485f2-a2c0-459a-8faa-a7a24df32981', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('514485f2-a2c0-459a-8faa-a7a24df32981', legitimate_health_intervention__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('514485f2-a2c0-459a-8faa-a7a24df32981', foundational, bodily_integrity_inviolable).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('514485f2-a2c0-459a-8faa-a7a24df32981', bodily_integrity_inviolable, deontological).
narrative_ontology:cs_axiom('514485f2-a2c0-459a-8faa-a7a24df32981', foundational, informed_consent_absolute).
narrative_ontology:cs_axiom_status(informed_consent_absolute, holdable).
narrative_ontology:cs_axiom_grounding('514485f2-a2c0-459a-8faa-a7a24df32981', informed_consent_absolute, deontological).
narrative_ontology:cs_reference_frame('514485f2-a2c0-459a-8faa-a7a24df32981', pre_mandate_common_law_consent).
narrative_ontology:cs_drift_state('514485f2-a2c0-459a-8faa-a7a24df32981', covid_mandate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('514485f2-a2c0-459a-8faa-a7a24df32981', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, state_governance).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, employment_access_gatekeepers).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, public_health_advocates).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, employment_access_gatekeepers).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, bodily_integrity_principle).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, informed_consent_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce medical mandates (vaccination, testing, quarantine) under emergency and routine public health statutes. Justify mandates as necessary for population health. Control the definition of public health emergency and the scope of mandated interventions. Collect institutional legitimacy and budgetary authority from mandate administration.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Face employment termination, educational exclusion, travel restrictions, and civil penalties for refusing mandated medical interventions. Medical exemptions are narrowly drawn and administratively difficult to obtain. Religious or philosophical exemptions are eliminated or severely restricted in many jurisdictions. The cost of refusal is loss of livelihood and social participation.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals, payer,
    moderate, biographical, constrained, national).

% Employers, schools, and venue operators enforce mandates as a condition of access. They gain liability protection and regulatory compliance by enforcing state mandates, but bear implementation costs and workforce disruption. Their cooperation is compelled by threat of license revocation, fines, or closure orders.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, employment_access_gatekeepers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, employment_access_gatekeepers, payer).

% Adjudicate challenges to mandates under constitutional and statutory frameworks. Apply varying standards of review (rational basis, strict scrutiny, Jacobson deference). Their rulings shape the operational boundary of mandate authority but do not bear the costs of compliance or non-compliance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, judicial_review_courts, observer,
    institutional, generational, analytical, national).

% Advocate for expansive mandate authority as essential to population health. Receive funding, institutional access, and policy influence from alignment with state public health authorities. Their professional standing depends on the mandate framework's legitimacy.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_advocates, beneficiary,
    organized, generational, mobile, national).

% Argue that mandates violate fundamental bodily integrity and informed consent rights. Are structurally excluded from mandate design processes and emergency rulemaking. Their litigation and legislative efforts face Jacobson-derived deference doctrines that treat their claims as settled against state police power.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, bodily_autonomy_advocates, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level disease containment through uniform medical interventions, replacing individual risk assessment with centralized directive.
% TRANSFER_FUNCTION: Moves bodily autonomy and medical decision-making authority from individuals to state; moves compliance costs (employment loss, educational exclusion, medical risk) to coerced individuals; moves legitimacy and administrative control to state authorities.
% ABSENT_VOICES: Individuals with medical contraindications not recognized by narrow exemption criteria; religious objectors whose beliefs are not accommodated; those with prior natural immunity or low risk profiles who are mandated identically to high-risk groups; children and dependents whose guardians refuse mandates on their behalf.
% DISAPPEARANCE_RATIONALE: If state mandate authority vanished overnight, individuals would regain medical decision-making authority; disease containment would shift to voluntary uptake, targeted protection, and non-coercive measures; state would lose a primary coercive lever over bodily integrity; employment and educational access would decouple from medical status.
% FOUNDING_PROBLEM: Historical epidemic control required coordinated population-wide interventions (smallpox, polio) where individual refusal created collective risk due to sterilizing vaccines and high transmission dynamics.
% FOUNDING_PROBLEM_CORROBORATION: Public health historians attest the founding problem was live for smallpox and polio with sterilizing vaccines and universal susceptibility. Bodily autonomy scholars and epidemiologists outside state public health institutions attest the founding problem is not analogous for non-sterilizing interventions with age-stratified risk profiles and widespread pre-existing immunity. No consensus exists outside the benefiting authorities.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72) because the mandate transfers medical decision authority and compliance costs to individuals while state authorities collect legitimacy and administrative control. Suppression is very high (0.85) because the arrangement's persistence depends on actively eliminating alternatives: narrow exemptions, penalty regimes, and Jacobson-derived judicial deference. Theater ratio is low (0.22) because the coercion is functionally real — the mandate achieves its stated public health aim (coverage) through compulsion, not performance. Accessibility collapse is high (0.82) because once a mandate issues, alternatives (informed refusal, medical exemption, religious accommodation) collapse administratively. Resistance is moderate (0.48) because litigation and non-compliance exist but are structurally constrained by deference doctrines.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state authorities) experiences the arrangement as coordination (rope/tangled_rope) — they see mandate authority solving a collective action problem. The payer seat (coerced individuals) experiences it as extraction (snare) — they see bodily integrity violated for a public benefit they do not consent to provide. The engine computes this divergence from the structural data: same constraint, different effective extraction per seat. The authored claim (snare) reflects the payer seat's structural reality; the agenda-setter seat would claim rope.
 *
 * DIRECTIONALITY LOGIC:
 *   State public health authorities are structural beneficiaries (d ~ 0.15): they set mandates, control exemptions, and collect institutional authority. Mandate-coerced individuals are structural targets (d ~ 0.85): they bear compliance costs, face penalties for refusal, and have constrained exit (employment/education access tied to compliance). Employment gatekeepers are dual-positioned: they benefit from liability protection but pay implementation costs (d ~ 0.45). Courts are analytical observers (d ~ 0.5). Public health advocates are beneficiaries (d ~ 0.2). Bodily autonomy advocates are excluded — their structural position would be target but they are kept out of the mandate design process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (smallpox/polio-style epidemics requiring sterilizing vaccines) is contested as live. If dead, the mandate authority persists as mandate without its original justification — a candidate for mandatrophy. However, state authorities claim the problem is live (novel pathogens, waning immunity). The contested status means mandatrophy is not resolved; the arrangement may be a scaffold whose sunset clause (emergency expiration) was not honored, or a snare whose founding problem was never the true justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint story is one reading (bodily_autonomy_primary) of the contested kernel ''legitimate_health_intervention''. Sibling readings are public_health_primary and proportionality_reading. What structural elements do the readings disagree on?',
    'Compare the three readings'' beneficiary/victim structures, claimed types, and epsilon values. The disagreement is located in: (1) whether mandate-coerced individuals are victims (this reading) or beneficiaries of protection (public_health_primary) or conditional payers (proportionality_reading); (2) whether state mandate authority is extraction (this reading) or coordination (public_health_primary); (3) whether epsilon is high (this reading) or low (public_health_primary).',
    'If the kernel is treated as a single constraint, epsilon becomes observer-relative and classification becomes unstable. Decomposition into three constraint stories with distinct epsilon values is required by epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment kernel decomposition: this story is one reading of legitimate_health_intervention kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.85) structural (employment termination, legal penalties, access denial) or internalized (individuals believing refusal is morally wrong, self-censoring dissent)?',
    'Post-mandate longitudinal study: if suppression behaviors (compliance without belief) persist after mandate removal, internalized component is significant. Survey experimental designs measuring compliance intent with and without enforcement threat.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after formal exit. This would increase effective extraction for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in mandate compliance.').

omega_variable(
    extraction_referent_boundary,
    'Does epsilon (0.72) measure the mandate policy itself, or the enforcement apparatus (mandate + penalties + exemption denial)? The reading''s epsilon referent is the standing arrangement under contest — but the arrangement''s boundaries are contested.',
    'Decompose the arrangement: (a) the statutory authority to mandate; (b) the specific mandate orders; (c) the penalty/enforcement regime; (d) the exemption adjudication process. Measure epsilon for each component. The reading''s claim targets the integrated arrangement (a+b+c+d).',
    'If epsilon is high only for the enforcement apparatus (c+d) but low for the statutory authority (a), the constraint family may need further decomposition. This reading treats the integrated arrangement as the referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_referent_boundary, conceptual, 'Boundary of the epsilon referent: mandate policy vs. enforcement apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 2020, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t2020, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(legi_tr_t2021, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 2021, 0.18).
narrative_ontology:measurement(legi_tr_t2022, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 2022, 0.25).
narrative_ontology:measurement(legi_tr_t2023, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 2023, 0.28).
narrative_ontology:measurement(legi_tr_t2024, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(legi_be_t2020, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(legi_be_t2021, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 2021, 0.75).
narrative_ontology:measurement(legi_be_t2022, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 2022, 0.78).
narrative_ontology:measurement(legi_be_t2023, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 2023, 0.68).
narrative_ontology:measurement(legi_be_t2024, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t2020, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(legi_su_t2021, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 2021, 0.9).
narrative_ontology:measurement(legi_su_t2022, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 2022, 0.88).
narrative_ontology:measurement(legi_su_t2023, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 2023, 0.82).
narrative_ontology:measurement(legi_su_t2024, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__bodily_autonomy_primary, 0.1).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the legitimate_health_intervention constraint family (kernel). The three readings decompose the kernel into structurally distinct constraints with different epsilon values, beneficiary/victim structures, and claimed types. This reading (bodily_autonomy_primary) classifies the mandate arrangement as snare (epsilon 0.72). The public_health_primary reading classifies it as rope (epsilon ~0.15). The proportionality_reading classifies it as tangled_rope (epsilon ~0.45). The epsilon values differ because the referent arrangement's extraction is assessed differently: this reading counts mandate coercion as extraction; public_health_primary counts disease prevention as coordination benefit; proportionality_reading counts both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__bodily_autonomy_primary, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
