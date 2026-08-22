% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Categorical Prohibition on Non-Consensual Medical Intervention (Bodily Autonomy Primary Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the coercion_legitimacy_boundary kernel. The reading asserts that medical
 *   intervention without consent is categorically impermissible regardless of
 *   collective benefit — a mountain claim grounded in post-Nuremberg medical
 *   ethics and constitutional privacy doctrine. The kernel is contested: the
 *   proportionality_reading conditions legitimacy on disease
 *   severity/transmission; the public_health_primary reading permits
 *   compulsion when collective harm-prevention outweighs autonomy. This
 *   reading's ε (0.32) reflects the moderate extraction from
 *   immunocompromised individuals and non-consenting patients who bear
 *   disproportionate risk when mandates are blocked. The beneficiaries
 *   (mandate_enforcers, public_health_authorities) are counterintuitive —
 *   they benefit from being stripped of coercive power because mandates
 *   generate legitimacy costs, resistance, and political backlash that exceed
 *   their disease-control value. The constraint shows rising
 *   suppression_requirement over the interval as judicial enforcement of the
 *   prohibition hardened (Jacobson → Buck → modern strict scrutiny), while
 *   extractiveness accumulated as the epidemiological stakes of non-mandate
 *   regimes became clearer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.32).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.68).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.32).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, mountain).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Categorical Prohibition on Non-Consensual Medical Intervention (Bodily Autonomy Primary Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:emerges_naturally(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, 'c5a88047-68f6-4345-a2dd-4228131baf7d').
narrative_ontology:cs_kernel_codification('c5a88047-68f6-4345-a2dd-4228131baf7d', formalized).
narrative_ontology:cs_authority_grounding('c5a88047-68f6-4345-a2dd-4228131baf7d', lineage).
narrative_ontology:cs_interpretation_layer_present('c5a88047-68f6-4345-a2dd-4228131baf7d').
narrative_ontology:cs_reading_relation('c5a88047-68f6-4345-a2dd-4228131baf7d', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('c5a88047-68f6-4345-a2dd-4228131baf7d', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('c5a88047-68f6-4345-a2dd-4228131baf7d', foundational, bodily_integrity_categorically_inviolable).
narrative_ontology:cs_axiom_status(bodily_integrity_categorically_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('c5a88047-68f6-4345-a2dd-4228131baf7d', bodily_integrity_categorically_inviolable, deontological).
narrative_ontology:cs_axiom('c5a88047-68f6-4345-a2dd-4228131baf7d', foundational, collective_benefit_never_justifies_nonconsensual_intervention).
narrative_ontology:cs_axiom_status(collective_benefit_never_justifies_nonconsensual_intervention, holdable).
narrative_ontology:cs_axiom_grounding('c5a88047-68f6-4345-a2dd-4228131baf7d', collective_benefit_never_justifies_nonconsensual_intervention, deontological).
narrative_ontology:cs_reference_frame('c5a88047-68f6-4345-a2dd-4228131baf7d', nuremberg_helsinki_consent_framework).
narrative_ontology:cs_drift_state('c5a88047-68f6-4345-a2dd-4228131baf7d', contemporary_pandemic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c5a88047-68f6-4345-a2dd-4228131baf7d', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, non_consenting_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_hesitant_populations).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_integrity_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, informed_consent_requirement).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, katz_v_united_states_privacy_penumbra).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot safely receive vaccines or face heightened risk from them; depend on population-level immunity for protection. When mandates are blocked by this reading, their exposure risk increases without their consent. No individual exit from the immunological commons.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, biographical, trapped, national).

% Subject to medical interventions without consent under countervailing public health orders (quarantine, forced treatment, involuntary commitment). This reading protects them categorically; its absence leaves them structurally exposed to state-compelled intervention.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, non_consenting_patients, payer,
    powerless, immediate, trapped, local).

% Public health agencies, school districts, employers who administer mandate regimes. When this reading prevails, their enforcement authority is constrained — they lose the coercive tool but avoid the legitimacy costs and resistance that mandates generate. They benefit from the reading's restriction on their own power.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers, beneficiary,
    institutional, generational, arbitrage, national).

% Set policy frameworks; this reading binds their agenda by removing the most effective coercive lever. They retain persuasion, incentivization, and access-expansion tools. The reading forces innovation in non-coercive strategies — a structural benefit to institutional legitimacy long-term.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, agenda_setter).

% Would refuse vaccination if given the choice; this reading guarantees that choice categorically. Their exit from vaccination is structurally protected. They bear no direct cost from the reading; the cost is externalized to immunocompromised individuals and outbreak risk.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_hesitant_populations, beneficiary,
    moderate, biographical, constrained, national).

% Analyze the constraint's coherence, its collision with collective harm-prevention, and its genealogical roots in post-Nuremberg medical ethics. Do not bear costs or collect benefits from the constraint's operation.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, bioethics_scholars, observer,
    analytical, civilizational, analytical, universal).

% Adjudicate clashes between bodily autonomy claims and police power. Their rulings instantiate which reading of the kernel governs. They do not personally bear the constraint's extraction but their institutional role is shaped by it.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bright-line rule that medical decisions require consent, eliminating the coordination problem of case-by-case balancing between individual autonomy and collective benefit. Provides a stable normative anchor for medical practice, research ethics, and patient trust.
% TRANSFER_FUNCTION: Transfers the burden of disease risk from non-consenting individuals (who would be compelled) to the collective (which absorbs higher outbreak risk) and to immunocompromised individuals (who bear disproportionate exposure). Transfers enforcement authority away from public health institutions to the individual veto.
% ABSENT_VOICES: Future generations who would inherit the epidemiological consequences of categorical refusal (e.g., endemic measles, polio resurgence). They are structurally excluded from the current consent calculus. Also absent: those who would consent to intervention but are denied access because the reading's framing treats all mandates as suspect, chilling voluntary programs.
% DISAPPEARANCE_RATIONALE: If this categorical prohibition vanished overnight, mandate regimes would expand immediately across jurisdictions — school entry, healthcare employment, travel, outbreak containment. The institutional infrastructure for compulsion exists; the reading is the primary brake. Its removal rearranges the consent landscape entirely.
% FOUNDING_PROBLEM: Post-Nuremberg recognition that state-compelled medical intervention was the primary vector of 20th-century atrocities (eugenics, forced sterilization, unethical experimentation). The constraint was built to make 'never again' structurally enforceable by removing the collective-benefit exception that had licensed abuse.
% FOUNDING_PROBLEM_CORROBORATION: The Nuremberg Code and Declaration of Helsinki (external to state beneficiaries) attest the founding problem as live. Public health authorities and constitutional courts in multiple jurisdictions attest it as dead or superseded by modern police power doctrine. No consensus outside the benefiting parties.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, ExtMetricName, E),
    domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(coercion_legitimacy_boundary__bodily_autonomy_primary),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) is moderate: the constraint extracts risk-bearing from immunocompromised individuals and autonomy from non-consenting patients, but does not transfer resources to a concentrated beneficiary. Suppression (0.68) is high: the categorical prohibition requires active judicial enforcement against state police power — it does not persist naturally. Theater (0.15) is low: the prohibition is genuinely operative in courts and ethics boards, not performative. Accessibility_collapse (0.42) is moderate: alternatives (incentives, persuasion, access expansion) exist but are less effective for disease control. Resistance (0.75) is high: public health institutions, legislatures, and segments of the public continuously contest the categorical reading. The claimed_type is mountain — the reading presents itself as a natural law of medical ethics — but beneficiaries exist, triggering FSM evaluation via omegas.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (immunocompromised, non-consenting) experience this as a snare — extraction without recourse, high suppression. The beneficiary seats (enforcers, authorities) experience it as a rope — coordination of medical ethics without coercive overhead. The observer seat (bioethics scholars) sees a mountain — a foundational norm. The agenda_setter seat (courts) sees a tangled_rope — must balance competing kernel readings. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised individuals and non-consenting patients are structural payers (d near 1.0): they bear the risk and autonomy costs of the prohibition. Mandate enforcers and public health authorities are structural beneficiaries (d near 0.0): they are relieved of the coercive burden and its legitimacy costs. Vaccine-hesitant populations are incidental beneficiaries (d ~ 0.3): they gain protection from mandates but do not drive the constraint. Constitutional courts are agenda_setters (analytical d): they instantiate which reading governs but do not personally bear extraction. The directionality derivation from beneficiary/victim declarations + exit options produces the expected d-values; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing state-compelled medical atrocities) remains live but contested. The constraint has not atrophied — its enforcement has intensified (rising suppression_requirement). However, the extraction profile has shifted: originally near-zero (1905), now moderate (0.32) as epidemiological externalities became measurable. This is not mandatrophy (degraded function); it is a mountain accumulating extraction as the world changes around it. The T17 mountain_extraction_accumulation trigger would fire at warning severity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the categorical prohibition on non-consensual intervention a genuine natural law (mountain) or a constructed constraint that benefits identifiable institutional actors?',
    'Cross-jurisdictional comparison: if the prohibition holds identically across radically different legal traditions and state capacities, it tracks a natural law; if it varies with institutional interests, it is constructed.',
    'If constructed, the mountain claim is a false summit — FSM signature would reclassify to tangled_rope. If natural, the mountain classification holds and the beneficiaries are incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the bodily_autonomy_primary reading instantiates a natural law or a constructed constraint with identifiable beneficiaries.').

omega_variable(
    mandate_enforcer_benefit_paradox,
    'Do mandate enforcers genuinely benefit from the categorical prohibition, or is their listed beneficiary status an artifact of the reading''s framing?',
    'Counterfactual: compare enforcement costs, legitimacy erosion, and political backlash under mandate regimes vs. non-mandate regimes. If enforcers face lower total cost without mandates, the beneficiary declaration is structurally valid.',
    'If enforcers are net harmed by the prohibition (lost disease control, political blame for outbreaks), the beneficiary declaration is inverted — they are payers, not beneficiaries. This flips the directionality logic for institutional seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_enforcer_benefit_paradox, empirical, 'Whether institutional actors who administer mandates are net beneficiaries or net payers under the categorical prohibition.').

omega_variable(
    immunocompromised_as_victims_of_reading,
    'Are immunocompromised individuals victims of THIS reading (bodily_autonomy_primary) or victims of the ABSENCE of the sibling reading (public_health_primary)?',
    'Trace the causal chain: immunocompromised risk rises when vaccination rates fall. Vaccination rates fall when mandates are blocked. The reading blocks mandates. But the reading does not compel non-vaccination — it permits it. The victim attribution depends on whether permission-to-refuse is treated as causally equivalent to compulsion-to-expose.',
    'If immunocompromised are victims of the reading, the constraint extracts from them (extraction > 0). If they are victims of the reading''s absence, this reading has near-zero extraction from them. This is the core ε-invariance test for kernel readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immunocompromised_as_victims_of_reading, conceptual, 'Attribution of victim status across kernel readings — whether the harm to immunocompromised individuals is extracted by this reading or by the failure of its sibling.').

omega_variable(
    suppression_mechanism_in_autonomy_constraints,
    'Is the suppression measured (0.68) structural (legal barriers to mandates) or internalized (professional norms, chilling effects on voluntary programs)?',
    'Post-decisional tracking: if a court strikes down a mandate and voluntary uptake remains stable, suppression is primarily structural. If voluntary programs atrophy because providers fear legal challenge, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure — the chilling effect extends beyond the legal prohibition. This would increase the constraint''s computed extractiveness for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_in_autonomy_constraints, empirical, 'Structural vs. internalized suppression in the categorical prohibition on non-consensual intervention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 1905, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clb_bap_tr_t1905, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1905, 0.02).
narrative_ontology:measurement(clb_bap_tr_t1947, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(clb_bap_tr_t1964, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1964, 0.08).
narrative_ontology:measurement(clb_bap_tr_t1976, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(clb_bap_tr_t1990, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(clb_bap_tr_t2005, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(clb_bap_tr_t2020, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(clb_bap_tr_t2025, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(clb_bap_be_t1905, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1905, 0.05).
narrative_ontology:measurement(clb_bap_be_t1947, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1947, 0.12).
narrative_ontology:measurement(clb_bap_be_t1964, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1964, 0.18).
narrative_ontology:measurement(clb_bap_be_t1976, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1976, 0.22).
narrative_ontology:measurement(clb_bap_be_t1990, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(clb_bap_be_t2005, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(clb_bap_be_t2020, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2020, 0.32).
narrative_ontology:measurement(clb_bap_be_t2025, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2025, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(clb_bap_su_t1905, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1905, 0.15).
narrative_ontology:measurement(clb_bap_su_t1947, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1947, 0.35).
narrative_ontology:measurement(clb_bap_su_t1964, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1964, 0.45).
narrative_ontology:measurement(clb_bap_su_t1976, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(clb_bap_su_t1990, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(clb_bap_su_t2005, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(clb_bap_su_t2020, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(clb_bap_su_t2025, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.08).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_mandate_enforcement).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, involuntary_treatment_law).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, research_ethics_oversight).

% DUAL FORMULATION NOTE:
% The coercion_legitimacy_boundary kernel decomposes into three constraint stories, each a distinct reading with its own ε, beneficiaries, victims, and claimed_type. This reading (bodily_autonomy_primary) claims mountain with moderate ε. The public_health_primary reading claims rope/tangled_rope with low ε for pandemic pathogens. The proportionality_reading claims tangled_rope with variable ε by disease. They form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
