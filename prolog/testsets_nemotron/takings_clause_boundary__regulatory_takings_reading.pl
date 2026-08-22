% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine (Pennsylvania Coal v. Mahon / Penn Central)
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   The regulatory takings reading of the Takings Clause holds that
 *   regulations diminishing property value 'too far' require just
 *   compensation, per Pennsylvania Coal v. Mahon (1922) and the Penn Central
 *   ad hoc balancing test (1978). This reading expands the victim set beyond
 *   physical appropriation to include severe economic diminution, introduces
 *   a multi-factor balancing test that creates doctrinal uncertainty, and
 *   provides property protection against non-physical extraction. The
 *   constraint is claimed as tangled_rope because it performs a genuine
 *   coordination function (calibrating the boundary between regulation and
 *   taking) while also extracting: property owners facing severe diminution
 *   are coordinated with the regulatory state, but the uncertainty of the
 *   test and the compensation transfers extract from both property owners
 *   (litigation costs, unpredictability) and the regulatory state
 *   (compensation liability, chilled regulation). Taxpayers bear diffuse
 *   compensation costs. Active enforcement is required — courts must
 *   adjudicate each balancing test, and the doctrine's persistence depends on
 *   judicial willingness to find takings in novel contexts.
 *
 * KEY AGENTS:
 *   - property_owners_severe_diminution: Primary target AND beneficiary (powerless/identity_locked) — bears extraction from uncertainty, benefits from compensation when takings found
 *   - land_use_regulators: Agenda setter (institutional/constrained) — administers regulations subject to takings review; constrained by doctrine but sets regulatory agenda
 *   - judicial_branch: Agenda setter (institutional/analytical) — adjudicates balancing test, defines 'too far'; analytical exit but institutional power
 *   - taxpayers_general: Victim (powerless/trapped) — bears compensation costs diffusely; no exit from tax liability
 *   - regulatory_state_legitimacy: Victim (institutional/identity_locked) — chilled regulation, doctrinal uncertainty undermines governance capacity; identity-locked to regulatory mission
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.42).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.35).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine (Pennsylvania Coal v. Mahon / Penn Central)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, 'e0c575f5-b560-4b0c-b851-967087720233').
narrative_ontology:cs_kernel_codification('e0c575f5-b560-4b0c-b851-967087720233', fixed_text).
narrative_ontology:cs_authority_grounding('e0c575f5-b560-4b0c-b851-967087720233', lineage).
narrative_ontology:cs_interpretation_layer_present('e0c575f5-b560-4b0c-b851-967087720233').
narrative_ontology:cs_reading_relation('e0c575f5-b560-4b0c-b851-967087720233', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0c575f5-b560-4b0c-b851-967087720233', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_axiom('e0c575f5-b560-4b0c-b851-967087720233', foundational, regulation_can_be_taking_without_physical_seizure).
narrative_ontology:cs_axiom_status(regulation_can_be_taking_without_physical_seizure, holdable).
narrative_ontology:cs_axiom_grounding('e0c575f5-b560-4b0c-b851-967087720233', regulation_can_be_taking_without_physical_seizure, deontological).
narrative_ontology:cs_axiom('e0c575f5-b560-4b0c-b851-967087720233', foundational, ad_hoc_balancing_calibrates_just_compensation).
narrative_ontology:cs_axiom_status(ad_hoc_balancing_calibrates_just_compensation, holdable).
narrative_ontology:cs_axiom_grounding('e0c575f5-b560-4b0c-b851-967087720233', ad_hoc_balancing_calibrates_just_compensation, conventional).
narrative_ontology:cs_reference_frame('e0c575f5-b560-4b0c-b851-967087720233', mahon_too_far_principle).
narrative_ontology:cs_drift_state('e0c575f5-b560-4b0c-b851-967087720233', penn_central_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e0c575f5-b560-4b0c-b851-967087720233', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners_severe_diminution).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, land_use_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, judicial_branch).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, property_owners_severe_diminution).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, taxpayers_general).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, regulatory_state_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owners whose property suffers severe economic diminution from regulation (e.g., 90%+ value loss from wetlands designation, historic preservation, zoning downzoning). They bear the uncertainty of the Penn Central test — cannot predict if their loss crosses 'too far.' When takings are found, they receive compensation (beneficiary). When not, they absorb the loss with no recourse (payer). Exit is identity-locked: family land, unique location, generational attachment make sale or abandonment existentially costly. Spatial scope is local (the regulation hits their specific parcel), but the doctrine's reach is national.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners_severe_diminution, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__regulatory_takings_reading, property_owners_severe_diminution, beneficiary).

% State and local agencies (planning departments, environmental regulators, zoning boards) that promulgate regulations affecting property value. They set the regulatory agenda but operate under the shadow of takings liability. Cannot exit the regulatory function (constrained exit) — the political mandate to regulate is institutional. Their power is institutional (coercive authority), but they are constrained by the doctrine's uncertainty: regulations are drafted defensively, ambitious programs abandoned or scaled back to avoid takings claims.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, land_use_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Courts (especially SCOTUS) that articulate and apply the Penn Central factors. They adjudicate 'too far' case by case, creating the doctrine's content. Analytical exit: judges can reason to different outcomes without personal cost. Institutional power: their decisions bind all regulators and property owners. They benefit from the doctrine's flexibility (judicial discretion) but bear legitimacy costs when outcomes appear result-oriented.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, judicial_branch, agenda_setter,
    institutional, generational, analytical, national).

% General taxpayers who fund compensation awards when takings are found. Payments come from general revenues or agency budgets, diffused across the tax base. No exit from tax liability (trapped). No direct benefit from the regulation or the taking — purely extractive from this seat. The cost per taxpayer is small but aggregate compensation liability is substantial (billions annually across jurisdictions).
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, taxpayers_general, payer,
    powerless, biographical, trapped, national).

% The regulatory state's capacity to govern effectively. Takings uncertainty chills regulation: agencies avoid innovative or ambitious rules, settle claims to avoid precedent, and lose policy flexibility. This is not a human agent but an institutional capacity — identity-locked because the state cannot 'exit' its governance function. The extraction is the foregone regulatory value: public goods not provided, externalities not internalized, because the takings risk made the regulation too costly to pursue.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, regulatory_state_legitimacy, payer,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_non_agent(takings_clause_boundary__regulatory_takings_reading, regulatory_state_legitimacy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decision procedure for distinguishing regulation from taking when government action diminishes property value without physical seizure. Solves the coordination problem of allocating loss between property owners and the public when regulation destroys value: the balancing test (economic impact, investment-backed expectations, character of government action) is the mechanism.
% TRANSFER_FUNCTION: Moves compensation liability from the public fisc to property owners when regulation is deemed 'too far' (government pays owner), and moves the cost of uncertainty and foregone regulation from the regulatory state to the public when regulation is chilled (public loses regulatory benefits). The ad hoc test creates a litigation market that transfers wealth to attorneys and experts.
% ABSENT_VOICES: Future generations who bear the cost of chilled environmental/land-use regulation (climate adaptation, habitat preservation, floodplain management not undertaken due to takings risk). Non-property-owning residents of regulated areas who benefit from regulation but have no standing in takings claims. Indigenous nations whose land-use sovereignty is constrained by the same doctrine but were not consulted in its creation.
% DISAPPEARANCE_RATIONALE: If the regulatory takings doctrine vanished overnight, regulations that severely diminish value could be enacted without compensation liability. Property owners would lose the compensation backstop but gain regulatory predictability (no 'too far' line to litigate). Regulators would gain freedom to pursue ambitious policies but lose the constraint that forces internalization of regulatory costs. The litigation industry around Penn Central would collapse. The allocation of regulatory loss would shift to the political process.
% FOUNDING_PROBLEM: How to distinguish legitimate regulation (which may diminish value) from a taking (which requires compensation) when government acts through law rather than physical seizure. Pennsylvania Coal v. Mahon (1922) recognized that regulation could go 'too far' and become a taking, but provided no clear line.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as live by all three institutional seats (regulators, courts, property owners) — each claims the line-drawing problem persists. However, the regulatory state's corroboration is self-interested (it wants clearer rules to reduce liability). Independent corroboration comes from legislative history: Congress has repeatedly considered but never enacted a statutory takings standard, suggesting the problem resists legislative solution. Scholars across the ideological spectrum (Epstein, Sax, Michelman, Krier) agree the line-drawing problem is genuine, though they disagree on the solution.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).
:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) reflects the compensation transfer plus the cost of doctrinal uncertainty — the ad hoc test creates litigation markets, strategic behavior, and regulatory chill that exceed the coordination value. Suppression (0.35) is moderate: the constraint does not forbid regulation outright but raises its cost and risk, suppressing some regulatory alternatives. Theater ratio (0.25) captures the performative invocation of 'balancing' where outcomes often track judicial philosophy rather than factor application. Accessibility collapse (0.45) is partial: property owners can sometimes avoid diminution (mobile exit for some asset classes), but identity-locked owners (family land, unique resources) face near-total collapse. Resistance (0.55) is substantial: regulated entities litigate aggressively, legislatures craft workarounds, and scholars contest the doctrine's coherence.
 *
 * PERSPECTIVAL GAP:
 *   The property owner seat diverges most: from the owner's perspective, the doctrine is a snare when uncertainty chills use of their property, but a rope when compensation protects them. The regulator seat experiences it as a tangled rope — genuine coordination need (knowing the boundary) but extractive overhead (unpredictable liability). The judicial seat sees coordination (providing a decision procedure) with modest extraction (adjudication costs). The taxpayer seat sees pure extraction (snare-like) — diffuse cost, no benefit. The engine computes these per-seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners with severe diminution are structurally dual-positioned: they are targets of the uncertainty and litigation costs (d → 0.7), but beneficiaries when compensation is awarded (d → 0.2). The engine derives this from the dual beneficiary/victim declaration. Land use regulators are agenda setters with constrained exit (they cannot abandon regulation; d ~0.4). The judicial branch is an agenda setter with analytical exit (d ~0.15). Taxpayers are trapped victims (d → 0.9). Regulatory state legitimacy is identity-locked (d → 0.8) — the state cannot exit its regulatory function, and the doctrine's uncertainty is internalized as institutional drag.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (calibrating regulation vs. taking) remains live, but the ad hoc test has accumulated extraction layers: litigation industry, strategic behavior, compensation as regulatory tax. The doctrinal form persists (mandatrophy unresolved) because no constituency benefits enough to reform it — property owners want stronger protection, regulators want clearer rules, courts want manageable standards, but the coalition for a per se replacement fractures on where the line falls. This is the tangled rope dynamic: coordination function alive, extraction layered on top.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the regulatory_takings_reading a distinct constraint from the categorical_takings_reading and physical_appropriation_reading, or a single constraint evaluated through different observables?',
    'The ε-invariance principle: if the three readings yield different ε values for the same kernel, they are distinct constraints. This reading''s ε=0.42 reflects substantial extraction through the ad hoc balancing test''s uncertainty and compensation transfers. The categorical reading''s ε is lower (per se rules constrain extraction). The physical reading''s ε is near-zero (only direct seizure triggers compensation). Three ε values → three constraints linked by network.affects_constraints.',
    'Confirms this story must be authored independently with its own metrics, stakeholders, and classification. The kernel context is recorded in commentary.kernel_context and cs_structure.reading_relations/axioms; it does not merge the readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this reading instantiates one ε-invariant constraint, not a measurement variant of a single constraint.').

omega_variable(
    diminution_threshold_ambiguity,
    'What quantum of value diminution constitutes ''too far'' — and is the threshold a structural property of the constraint or a judicial construction that varies by decision-maker?',
    'Longitudinal analysis of Penn Central factor application across circuits and decades: if outcomes cluster around a stable threshold, the constraint has structural coherence; if outcomes are predictably divergent based on judicial philosophy, the threshold is a construction masking extraction.',
    'If the threshold is a judicial construction, the constraint''s extractiveness is higher than measured (the balancing test is a mechanism for discretionary extraction). If structural, the coordination function is genuine and the ad hoc test is the price of that coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diminution_threshold_ambiguity, empirical, 'Whether ''too far'' is a discoverable line or a cover for discretionary extraction.').

omega_variable(
    regulatory_uncertainty_as_extraction,
    'Does the uncertainty created by the ad hoc balancing test function as an extraction mechanism (chilling regulation, enabling strategic litigation) or as a genuine coordination cost of calibrating property protection?',
    'Measure regulatory chill: track regulation abandonment or modification rates attributable to takings litigation risk. Compare to jurisdictions with per se rules. If chill exceeds what calibration requires, uncertainty is extractive.',
    'If uncertainty is extractive, the constraint''s suppression is under-measured (it suppresses regulatory alternatives, not just property uses). If coordination cost, the current metrics are descriptively accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_uncertainty_as_extraction, empirical, 'Whether doctrinal uncertainty is a feature or a bug.').

omega_variable(
    taxpayer_victim_structure,
    'Are taxpayers a genuine victim class bearing the cost of compensation awards, or is this a diffuse political grievance that does not map to the directionality engine''s victim concept?',
    'Trace compensation payments from specific regulatory programs to general revenue or program budgets. If payments come from general funds, taxpayers bear diffuse costs; if from program fees, regulated parties bear them. Compare fiscal incidence to the directionality derivation for ''taxpayers_general''.',
    'If taxpayers are genuine victims, the victim set expands and the constraint''s extraction is more widely distributed. If not, the victim set narrows to property owners and the regulatory state itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taxpayer_victim_structure, conceptual, 'Whether taxpayer burden is a structural victim relationship or political rhetoric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 1922, 2019).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_tr_t1922, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1922, 0.1).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_tr_t1950, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_tr_t1978, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_tr_t1992, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1992, 0.23).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_tr_t2005, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_tr_t2019, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2019, 0.25).

% Extraction over time
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_be_t1922, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1922, 0.15).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_be_t1950, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_be_t1978, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1978, 0.38).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_be_t1992, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1992, 0.41).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_be_t2005, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_be_t2019, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2019, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_su_t1922, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1922, 0.2).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_su_t1950, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_su_t1978, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_su_t1992, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_su_t2005, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(takings_clause_boundary__regulatory_takings_reading_su_t2019, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2019, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__regulatory_takings_reading, 0.12).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__categorical_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, inverse_condemnation_doctrine).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, penn_central_factors_application).

% DUAL FORMULATION NOTE:
% Takings Clause Boundary constraint family: three readings of the same constitutional text with divergent ε. Regulatory takings reading (this story) has highest ε due to ad hoc balancing test uncertainty and expanded victim set. Categorical reading uses per se rules to constrain extraction. Physical appropriation reading has near-zero ε by limiting takings to physical seizure. The upstream physical/categorical readings are often cited as limiting principles for the downstream regulatory reading, creating institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__regulatory_takings_reading, institutional, 0.35).
constraint_indexing:directionality_override(takings_clause_boundary__regulatory_takings_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
