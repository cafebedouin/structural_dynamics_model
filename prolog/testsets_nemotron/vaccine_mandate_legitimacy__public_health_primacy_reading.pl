% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Public Health Primacy Vaccine Mandate Authority
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the public_health_primacy_reading of
 *   the contested vaccine_mandate_legitimacy kernel. The reading holds that
 *   the state's duty to prevent collective harm justifies mandatory
 *   vaccination authority, treating unvaccinated status as a negative
 *   externality that the state may coercively internalize. The constraint
 *   operates as a tangled rope: it performs a genuine coordination function
 *   (achieving population immunity thresholds that voluntary uptake cannot
 *   reliably reach) while simultaneously extracting bodily autonomy and
 *   compliance costs from refusers and autonomy advocates, with the public
 *   health bureaucracy as the primary institutional beneficiary of expanded
 *   authority. The claim/metric divergence is deliberate: the reading claims
 *   tangled_rope (coordination + asymmetric extraction) while the metrics
 *   describe substantial extractiveness and suppression — the engine computes
 *   the per-seat classification from structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.68).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.82).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Public Health Primacy Vaccine Mandate Authority").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, '758367e4-bd0f-4ed0-99c2-f436ae8603e0').
narrative_ontology:cs_kernel_codification('758367e4-bd0f-4ed0-99c2-f436ae8603e0', formalized).
narrative_ontology:cs_authority_grounding('758367e4-bd0f-4ed0-99c2-f436ae8603e0', lineage).
narrative_ontology:cs_interpretation_layer_present('758367e4-bd0f-4ed0-99c2-f436ae8603e0').
narrative_ontology:cs_reading_relation('758367e4-bd0f-4ed0-99c2-f436ae8603e0', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('758367e4-bd0f-4ed0-99c2-f436ae8603e0', vaccine_mandate_legitimacy__risk_stratification_reading, coexists_with).
narrative_ontology:cs_axiom('758367e4-bd0f-4ed0-99c2-f436ae8603e0', foundational, state_duty_to_prevent_collective_harm_justifies_bodily_intrusion).
narrative_ontology:cs_axiom_status(state_duty_to_prevent_collective_harm_justifies_bodily_intrusion, holdable).
narrative_ontology:cs_axiom_grounding('758367e4-bd0f-4ed0-99c2-f436ae8603e0', state_duty_to_prevent_collective_harm_justifies_bodily_intrusion, deontological).
narrative_ontology:cs_axiom('758367e4-bd0f-4ed0-99c2-f436ae8603e0', foundational, unvaccinated_status_constitutes_negative_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_constitutes_negative_externality, holdable).
narrative_ontology:cs_axiom_grounding('758367e4-bd0f-4ed0-99c2-f436ae8603e0', unvaccinated_status_constitutes_negative_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('758367e4-bd0f-4ed0-99c2-f436ae8603e0', jacobson_v_massachusetts_authority).
narrative_ontology:cs_drift_state('758367e4-bd0f-4ed0-99c2-f436ae8603e0', post_covid_mandate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('758367e4-bd0f-4ed0-99c2-f436ae8603e0', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, bodily_autonomy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, general_population).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, general_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, promulgates, and enforces vaccine mandates through regulatory rulemaking, emergency orders, and guidance documents. Gains expanded institutional authority, budget authority for enforcement infrastructure, and normative legitimacy as the designated guardian of collective health. Can redirect resources to mandate compliance programs. Exit is arbitrage-grade: the institution persists regardless of any specific mandate's fate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, beneficiary).

% Gain indirect protection through herd immunity effects when mandates raise population vaccination rates. Their medical vulnerability makes them structurally dependent on others' vaccination choices. Exit is constrained: they cannot individually produce herd immunity and have limited ability to relocate to higher-coverage jurisdictions. Their benefit is real but diffuse and mediated through population-level dynamics.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_populations, beneficiary,
    organized, biographical, constrained, national).

% Bear the direct costs of mandate compliance (vaccination against conscience, medical exemption navigation costs) or non-compliance (employment loss, educational exclusion, mobility restrictions, social stigmatization). Their refusal is often fused with identity — religious conviction, parental autonomy, anti-institutional worldview — making exit identity-locked rather than merely constrained. The suppression they experience is structural (legal penalties) and internalized (moral injury from coerced violation of conscience).
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers, payer).

% Organize legal challenges, legislative advocacy, and public campaigns against mandate authority. Bear costs of litigation, political mobilization, and opportunity cost of diverted advocacy resources. Their position is that the constraint's premise (state duty to prevent collective harm overrides bodily integrity) creates a precedent that threatens all medical self-determination. Exit is mobile at the organizational level — they can shift jurisdiction, forum, or strategy — but the precedent effect is structural.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, bodily_autonomy_advocates, payer,
    organized, generational, mobile, national).

% Receive the coordination benefit of reduced disease transmission, healthcare system stability, and economic continuity. Simultaneously bear diffuse costs: tax funding for enforcement infrastructure, marginal liberty restrictions (verification systems, access controls), and the social friction of mandate politics. Exit is constrained: the benefits are collective and non-excludable; the costs are distributed and unavoidable while the mandate persists.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, general_population, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, general_population, payer).

% Adjudicates constitutional challenges to mandates under varying standards of review (Jacobson v. Massachusetts rational basis, strict scrutiny for fundamental rights, hybrid approaches). Their rulings determine the operational boundaries of mandate authority. They do not collect extraction nor bear its direct costs; their institutional role is to resolve the structural tension between the other seats.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, judicial_branch, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of achieving population immunity thresholds that no individual can achieve alone: free-rider dynamics, information asymmetry about vaccine risk/benefit, and coordination failure in voluntary uptake.
% TRANSFER_FUNCTION: Moves bodily autonomy decision-rights and compliance costs from individuals (especially refusers) to the state apparatus; moves protection-from-infection benefits to vulnerable populations and the general public; moves institutional authority and enforcement resources to public health bureaucracy.
% ABSENT_VOICES: Children and adolescents subject to school mandates without consent capacity; undocumented populations who face enforcement without political representation; future generations who inherit the precedent of state bodily intrusion authority; vaccine-injured individuals whose claims are channeled into a separate compensation system (CICP/VICP) with low awards and no judicial review.
% DISAPPEARANCE_RATIONALE: If mandate authority vanished overnight, vaccination rates would drop measurably (observed in jurisdictions lifting mandates), disease incidence would rise, immunocompromised individuals would lose their primary structural protection, public health bureaucracy would lose its central enforcement lever and associated resource flows, and the constitutional precedent for state bodily intrusion would be substantially weakened — though not erased, as Jacobson remains.
% FOUNDING_PROBLEM: Recurrent epidemic disease (smallpox, polio, measles) that voluntary vaccination could not control due to free-rider dynamics, access barriers, and community-level coordination failure; state needed a reliable tool to achieve herd immunity thresholds.
% FOUNDING_PROBLEM_CORROBORATION: Public health historians (e.g., Colgrove, Conis) document that mandates achieved eradication/control where voluntary programs failed. Vaccine skeptics and bodily autonomy advocates (e.g., CHD, NVIC) contest whether the founding problem persists given modern vaccine technology, disease ecology, and less restrictive alternatives. The CDC and WHO attest the problem remains live for novel pathogens; legal scholars (e.g., Gostin, Jacobson) are split on whether the original justification maps to current mandate architectures.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects that the mandate transfers significant decision-rights and compliance costs from individuals to the state, with the transfer concentrated on a minority (refusers) while benefits are diffuse. Suppression (0.82) is high because the constraint's persistence depends on active enforcement — school exclusion, employment mandates, mobility restrictions, legal penalties — and on suppressing exit alternatives (medical exemptions narrowly construed, religious exemptions contested, homeschooling/alternative schooling barriers). Theater ratio (0.28) is moderate: the coordination function (disease control) is real and documented, but a growing share of enforcement activity serves institutional self-preservation and precedent-setting rather than marginal public health gain. Accessibility collapse (0.72) is high because the constraint's logic (externality internalization) makes alternatives structurally difficult — once the premise is accepted that unvaccinated status is a regulable externality, the scope of regulable behavior expands. Resistance (0.75) is high and rising, reflecting organized legal, legislative, and cultural pushback.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (public health bureaucracy) experiences the constraint as genuine coordination infrastructure it builds and maintains. The payer/victim seats (refusers, autonomy advocates) experience the same structure as enforced extraction with identity-locking suppression. The beneficiary seats (immunocompromised, general public) experience it as a net-positive but imperfect coordination mechanism with diffuse costs. The engine computes this divergence from the structural data authored here.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health bureaucracy sits at the beneficiary end (d ~0.15): it sets the agenda, collects authority/resources, and has arbitrage-grade exit. Immunocompromised populations and general population are near-symmetric beneficiaries with constrained exit (d ~0.4-0.5): they gain real but diffuse protection while bearing diffuse costs. Vaccine refusers are at the target end (d ~0.9): identity-locked exit, concentrated costs, structural suppression. Bodily autonomy advocates are targets with mobile exit at organizational level (d ~0.75): they bear advocacy costs and precedent risk but can shift forum. Judicial branch is analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epidemic disease control via coordination failure) is contested as live vs. solved vs. transformed. If dead (eradication achieved, less restrictive alternatives available), the mandate persists as piton/zombie — maintained by institutional inertia and precedent value. If live (novel pathogens, waning immunity, coverage gaps), it remains tangled rope. If contested, the mandate's legitimacy depends on which framing controls the adjudicating institutions. The mandatrophy risk is real: the constraint's coordination function may have atrophied relative to its extraction function, but the institutional beneficiary has strong incentives to maintain and expand it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_reading_boundary,
    'Is the public_health_primacy_reading a single stable constraint, or does it conceal multiple constraints with different ε values (e.g., childhood school mandates vs. adult employer mandates vs. healthcare worker mandates vs. COVID-19 emergency mandates)?',
    'Decompose by mandate scope, target population, and enforcement mechanism; measure extractiveness and suppression separately for each. If ε varies significantly across sub-constraints, the reading fractures into a constraint family.',
    'If the reading fractures, the single ε authored here is a composite that masks structural variation. The engine would classify each sub-constraint independently, potentially yielding different types for school mandates (rope?) vs. emergency mandates (snare?). This is the ε-invariance principle applied to the reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_reading_boundary, conceptual, 'Whether this reading is one ε-invariant constraint or a family requiring decomposition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression measured here primarily structural (legal penalties, exclusion) or substantially internalized (moral injury, identity fusion, social stigmatization that persists after formal mandates lift)?',
    'Post-mandate longitudinal studies of refuser populations: if suppression effects (employment avoidance, healthcare avoidance, social withdrawal) persist after legal mandates are removed, the internalized component is significant.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after formal exit. This would amplify χ for identity-locked agents beyond what the structural derivation captures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for vaccine refusers.').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination function (population immunity achievement) be separated from the extraction function (bodily autonomy transfer to state) without degrading the coordination outcome?',
    'Natural experiments: jurisdictions with robust voluntary programs + targeted mandates for high-risk settings vs. blanket mandates. If coordination outcomes (coverage, disease incidence) are comparable, the functions are separable and the blanket mandate''s extraction is not coordination-necessary.',
    'If separable, the constraint''s extraction component is not the price of coordination but a separable layer — moving it toward snare. If inseparable, the extraction is the coordination cost, supporting tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction components are structurally separable.').

omega_variable(
    mandatrophy_precedent_value,
    'Does the public health bureaucracy''s primary benefit from this constraint come from its disease-control function or from the constitutional precedent it establishes for state bodily intrusion authority?',
    'Analyze institutional behavior: does the bureaucracy advocate for mandates in settings where disease-control justification is weak but precedent value is high? Do they resist less-restrictive alternatives that achieve the same coverage?',
    'If precedent value dominates, the constraint is a scaffold for institutional power masquerading as coordination — the sunset clause (disease control achieved) never triggers because the real function is precedent maintenance. This would reclassify toward piton or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_precedent_value, preference, 'Whether institutional beneficiary motivation is coordination or precedent capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 1905, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vaccine_mandate_phpr_tr_t1905, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 1905, 0.1).
narrative_ontology:measurement(vaccine_mandate_phpr_tr_t1955, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 1955, 0.12).
narrative_ontology:measurement(vaccine_mandate_phpr_tr_t1977, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(vaccine_mandate_phpr_tr_t1998, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(vaccine_mandate_phpr_tr_t2015, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(vaccine_mandate_phpr_tr_t2020, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement(vaccine_mandate_phpr_tr_t2024, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(vaccine_mandate_phpr_be_t1905, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 1905, 0.35).
narrative_ontology:measurement(vaccine_mandate_phpr_be_t1955, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 1955, 0.42).
narrative_ontology:measurement(vaccine_mandate_phpr_be_t1977, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 1977, 0.48).
narrative_ontology:measurement(vaccine_mandate_phpr_be_t1998, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 1998, 0.52).
narrative_ontology:measurement(vaccine_mandate_phpr_be_t2015, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(vaccine_mandate_phpr_be_t2020, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(vaccine_mandate_phpr_be_t2024, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vaccine_mandate_phpr_su_t1905, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 1905, 0.55).
narrative_ontology:measurement(vaccine_mandate_phpr_su_t1955, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 1955, 0.6).
narrative_ontology:measurement(vaccine_mandate_phpr_su_t1977, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 1977, 0.65).
narrative_ontology:measurement(vaccine_mandate_phpr_su_t1998, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 1998, 0.7).
narrative_ontology:measurement(vaccine_mandate_phpr_su_t2015, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(vaccine_mandate_phpr_su_t2020, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(vaccine_mandate_phpr_su_t2024, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_emergency_powers).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, school_vaccination_requirements).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, healthcare_worker_vaccine_mandates).

% DUAL FORMULATION NOTE:
% Part of the vaccine_mandate_legitimacy constraint family. This reading (public_health_primacy) claims tangled_rope with high extractiveness/suppression. The bodily_autonomy_primacy reading would claim snare (pure extraction, no genuine coordination). The risk_stratification reading would claim scaffold or rope (targeted coordination with sunset/proportionality). All three share the kernel but instantiate different constraints with different ε, different victim/beneficiary structures, and different types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__public_health_primacy_reading, moderate, 0.85).
constraint_indexing:directionality_override(vaccine_mandate_legitimacy__public_health_primacy_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
