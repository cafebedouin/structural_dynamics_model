% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__developmental_state_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__developmental_state_reading, []).

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
 *   constraint_id: flexible_employment_legitimacy__developmental_state_reading
 *   human_readable: Flexible Employment as State-Managed Transitional Form
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   A developmental state frames flexible employment as a transitional form
 *   requiring active management toward 2027 formalization targets. The
 *   12-point plan reasserts state authority over platform labor markets,
 *   presenting wage growth as evidence of managed transition rather than
 *   market efficiency. This reading claims the constraint is a genuine
 *   scaffold: coordination function (formalization pathway) with declared
 *   sunset (2027 targets), active enforcement (regulatory compliance), and
 *   transitional justification. The constraint's legitimacy depends on
 *   whether formalization actually delivers protections to precarious workers
 *   or merely legitimizes platform extraction under state cover.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, 0.42).
domain_priors:suppression_score(flexible_employment_legitimacy__developmental_state_reading, 0.38).
domain_priors:theater_ratio(flexible_employment_legitimacy__developmental_state_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__developmental_state_reading, scaffold).
narrative_ontology:human_readable(flexible_employment_legitimacy__developmental_state_reading, "Flexible Employment as State-Managed Transitional Form").
narrative_ontology:topic_domain(flexible_employment_legitimacy__developmental_state_reading, "economic/political/social").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__developmental_state_reading).
narrative_ontology:has_sunset_clause(flexible_employment_legitimacy__developmental_state_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__developmental_state_reading, '13062c1d-e28e-4c93-a4a9-9794faeb8854').
narrative_ontology:cs_kernel_codification('13062c1d-e28e-4c93-a4a9-9794faeb8854', formalized).
narrative_ontology:cs_authority_grounding('13062c1d-e28e-4c93-a4a9-9794faeb8854', extraction).
narrative_ontology:cs_interpretation_layer_present('13062c1d-e28e-4c93-a4a9-9794faeb8854').
narrative_ontology:cs_reading_relation('13062c1d-e28e-4c93-a4a9-9794faeb8854', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('13062c1d-e28e-4c93-a4a9-9794faeb8854', flexible_employment_legitimacy__precarity_extraction_reading, influences).
narrative_ontology:cs_axiom('13062c1d-e28e-4c93-a4a9-9794faeb8854', foundational, state_managed_transition_necessary).
narrative_ontology:cs_axiom_status(state_managed_transition_necessary, holdable).
narrative_ontology:cs_axiom_grounding('13062c1d-e28e-4c93-a4a9-9794faeb8854', state_managed_transition_necessary, instrumental).
narrative_ontology:cs_axiom('13062c1d-e28e-4c93-a4a9-9794faeb8854', foundational, formalization_as_public_good).
narrative_ontology:cs_axiom_status(formalization_as_public_good, holdable).
narrative_ontology:cs_axiom_grounding('13062c1d-e28e-4c93-a4a9-9794faeb8854', formalization_as_public_good, deontological).
narrative_ontology:cs_axiom('13062c1d-e28e-4c93-a4a9-9794faeb8854', secondary, id_2027_standardization_target_binding).
narrative_ontology:cs_axiom_status(id_2027_standardization_target_binding, holdable).
narrative_ontology:cs_axiom_grounding('13062c1d-e28e-4c93-a4a9-9794faeb8854', id_2027_standardization_target_binding, conventional).
narrative_ontology:cs_reference_frame('13062c1d-e28e-4c93-a4a9-9794faeb8854', developmental_state_formalization_mandate).
narrative_ontology:cs_drift_state('13062c1d-e28e-4c93-a4a9-9794faeb8854', post_12_point_plan_implementation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('13062c1d-e28e-4c93-a4a9-9794faeb8854', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, state_development_agencies).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, platform_companies).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, formalizing_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, precarious_gig_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, informal_sector_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, formalizing_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce the 12-point formalization plan targeting 2027 standardization. They control regulatory frameworks, social protection extensions, and enforcement mechanisms. Their legitimacy rests on demonstrating measurable formalization progress. Exit means policy failure and loss of developmental mandate.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, state_development_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Operate flexible employment platforms under state tolerance while formalization proceeds. They gain regulatory certainty and continued access to labor pools during transition. Their exit options include jurisdictional arbitrage, lobbying for slower formalization, or adapting business models to formalized standards. They capture efficiency gains from flexible matching while externalizing transition costs.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_companies, beneficiary,
    powerful, biographical, arbitrage, global).

% Workers currently in flexible arrangements who gain formal protections (social security, labor rights, wage floors) through the transition. They pay through reduced flexibility, potential job loss during formalization, and contribution deductions. Exit means returning to informality or accepting worse conditions; their constrained exit reflects dependence on the very platforms being formalized.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, formalizing_workers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, formalizing_workers, payer).

% Workers in the most precarious flexible arrangements who bear transition costs without guaranteed formalization benefits. They face algorithmic control, income volatility, and no social protection during the extended transition. Their trapped exit reflects structural dependence on platform income and lack of alternative livelihoods. They experience the constraint as extraction without coordination.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, precarious_gig_workers, payer,
    powerless, immediate, trapped, national).

% Workers outside platform structures but affected by formalization spillovers — increased regulatory scrutiny, competition from formalizing workers, and potential displacement. They pay compliance costs without platform-mediated benefits. Their trapped exit reflects total dependence on informal earnings and zero bargaining power in formalization negotiations.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, informal_sector_workers, payer,
    powerless, immediate, trapped, national).

% Study the formalization trajectory, measure wage growth attribution (managed transition vs market forces), and evaluate whether 2027 targets represent genuine stabilization or moving goalposts. Their analytical seat sees the full structural asymmetry between state commitments and worker outcomes.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, labor_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of transitioning a massive informal/platform labor force into formal protections without collapsing employment — provides sequenced regulatory pathway, social protection extension, and enforcement capacity that individual actors cannot achieve alone.
% TRANSFER_FUNCTION: Moves formalization costs (compliance, contributions, reduced flexibility) from state/platforms onto workers (especially precarious ones), while moving protections and wage floors from future promise to present reality. Platforms transfer regulatory risk to state; state transfers fiscal burden to workers via contributions; workers transfer flexibility for security.
% ABSENT_VOICES: Workers in unregistered platforms, cross-border gig workers excluded from national formalization, and future cohorts who will inherit the formalized structure without having shaped it. They are structurally excluded because formalization negotiations occur between state, platforms, and currently-registered workers only.
% DISAPPEARANCE_RATIONALE: If the state-managed transition vanished overnight, platforms would revert to unregulated flexibility, formalization gains would reverse, and 12-point plan enforcement would collapse — but the underlying labor market would not return to pre-transition state because worker expectations and partial formalization have shifted the equilibrium.
% FOUNDING_PROBLEM: How to extend formal labor protections to platform and informal workers without destroying the employment generation that flexible arrangements enable — the developmental state's core dilemma of inclusion without stagnation.
% FOUNDING_PROBLEM_CORROBORATION: International Labour Organization (ILO) studies on formalization pathways corroborate the live status; platform companies' own policy submissions acknowledge the problem persists; independent labor economists outside state agencies confirm the coordination gap remains. No single benefiting party monopolizes the attestation.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__developmental_state_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__developmental_state_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__developmental_state_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(flexible_employment_legitimacy__developmental_state_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__developmental_state_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__developmental_state_reading_tests).
:- end_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects real but partial extraction: workers gain protections but pay transition costs; platforms gain certainty but face formalization mandates. Suppression (0.38) is moderate — enforcement targets non-compliant platforms and informal operators, not workers directly. Theater ratio (0.28) captures performative compliance: platforms adopt formalization optics while algorithmic control persists; state agencies hit intermediate targets while 2027 deadline looms. Accessibility collapse (0.45) and resistance (0.52) reflect that alternatives (pure informality, full formalization) exist but are structurally difficult. The measurement series shows extractiveness plateauing as formalization stalls, theater rising as performative compliance replaces substantive change, suppression increasing as enforcement hardens against resistant platforms.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, this is a genuine scaffold: coordination with sunset. From precarious workers' seat, it operates as a snare: extraction without meaningful coordination. From platforms' seat, it's a tangled rope: they are coordinated (regulatory certainty) but also pay (formalization costs) while extracting (continued surplus capture). The engine computes this divergence; the authored claim (scaffold) represents the state's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   State agencies are agenda_setters with analytical exit — they define the transition but face policy legitimacy risks. Platform companies are beneficiaries with arbitrage exit — they capture transition gains while retaining exit leverage. Formalizing workers are dual-role (beneficiary/payer) with constrained exit — they gain protections but depend on platforms for income. Precarious and informal workers are payers with trapped exit — they bear costs without guaranteed benefits. The engine will compute per-seat classifications from this structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by declaring its transitional nature explicitly (has_sunset_clause: true) and naming both coordination beneficiaries (formalizing workers) and extraction victims (precarious workers). If formalization stalls past 2027 without sunset activation, mandatrophy triggers: the scaffold becomes a piton (theatrical maintenance of transition rhetoric) or snare (extraction legitimized by permanent transition). The founding_problem_status=live and corroboration from outside beneficiaries (ILO, independent economists) test whether the transition remains genuine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalization_genuineness,
    'Is the 2027 formalization target a genuine stabilization endpoint or a moving goalpost that perpetuates transitional extraction?',
    'Post-2027 observation: if formalization metrics (coverage, compliance, worker protections) plateau without new targets, the scaffold was genuine; if new transition narratives emerge with extended deadlines, the constraint was a false scaffold masking extraction.',
    'If moving goalpost, reclassifies from scaffold to piton (theatrical transition maintenance) or snare (permanent extraction legitimized by permanent transition rhetoric).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalization_genuineness, empirical, 'Whether the declared sunset clause represents real transition endpoint or extractive deferral.').

omega_variable(
    wage_growth_attribution,
    'Is observed wage growth in flexible employment driven by state-managed formalization (this reading) or by market tightening/platform competition (market_efficiency_reading)?',
    'Counterfactual analysis comparing wage trajectories in jurisdictions with/without active formalization plans, controlling for platform competition intensity and labor market tightness.',
    'If market-driven, the developmental state''s claimed coordination function is overstated — the constraint''s coordination benefit is partly spurious, supporting precarity_extraction_reading''s claim that state management legitimizes market outcomes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wage_growth_attribution, conceptual, 'Causal attribution of wage growth — managed transition vs market forces.').

omega_variable(
    commitment_system_framing,
    'Does the developmental state reading instantiate a commitment-system constraint where the kernel (flexible employment legitimacy) is grounded in state authority, or is it a policy program misidentified as a kernel reading?',
    'Assess whether the 12-point plan functions as a kernel codification (formalized authority structure with interpretive layer) or as a standard policy instrument. Check for authority_grounding in extraction (state extracts legitimacy from preventing kernel revision) vs expertise/practice.',
    'If not a genuine CS constraint, the cs_structure fields misrepresent a policy program as a kernel reading — the reading_relations and axioms would be analytical impositions, not structural features.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_system_framing, conceptual, 'Whether this reading genuinely operates as a commitment-system reading of a contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__developmental_state_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fel_dev_state_tr_t0, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fel_dev_state_tr_t20, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(fel_dev_state_tr_t40, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(fel_dev_state_tr_t60, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(fel_dev_state_tr_t80, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(fel_dev_state_tr_t100, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement(fel_dev_state_tr_t120, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 120, 0.28).

% Extraction over time
narrative_ontology:measurement(fel_dev_state_be_t0, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fel_dev_state_be_t20, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(fel_dev_state_be_t40, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(fel_dev_state_be_t60, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 60, 0.41).
narrative_ontology:measurement(fel_dev_state_be_t80, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 80, 0.42).
narrative_ontology:measurement(fel_dev_state_be_t100, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(fel_dev_state_be_t120, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 120, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(fel_dev_state_su_t0, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(fel_dev_state_su_t20, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(fel_dev_state_su_t40, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 40, 0.33).
narrative_ontology:measurement(fel_dev_state_su_t60, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(fel_dev_state_su_t80, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(fel_dev_state_su_t100, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 100, 0.38).
narrative_ontology:measurement(fel_dev_state_su_t120, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 120, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__developmental_state_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__developmental_state_reading, 0.18).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, platform_algorithm_control).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, social_protection_extension).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, labor_market_formalization).

% DUAL FORMULATION NOTE:
% Part of flexible_employment_legitimacy kernel family. This reading (developmental_state) claims scaffold with sunset; market_efficiency_reading claims rope (coordination without extraction); precarity_extraction_reading claims snare (extraction without coordination). All three share the referent (flexible employment arrangements) but author different ε, beneficiaries, victims, and structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flexible_employment_legitimacy__developmental_state_reading, powerful, 0.25).
constraint_indexing:directionality_override(flexible_employment_legitimacy__developmental_state_reading, moderate, 0.55).
constraint_indexing:directionality_override(flexible_employment_legitimacy__developmental_state_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
