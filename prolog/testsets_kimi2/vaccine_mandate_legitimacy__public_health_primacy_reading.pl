% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy â Public Health Primacy Reading
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the public_health_primacy_reading of the
 *   vaccine_mandate_legitimacy kernel. It models the claim that the state's
 *   duty to prevent collective infectious harm justifies coercive vaccination
 *   mandates, treating unvaccinated status as a negative externality. Under
 *   this reading, the public health bureaucracy gains enforceable authority
 *   over individual medical decisions, while unvaccinated refusers bear the
 *   concentrated costs of exclusion and penalty. The sibling
 *   readingsâbodily_autonomy_primacy (absolute medical self-sovereignty)
 *   and risk_stratification (actuarial proportionality)âare structurally
 *   distinct constraints linked through the same kernel. This story authors
 *   high extractiveness and suppression because the mandate's persistence
 *   depends on actively penalizing non-compliance and suppressing alternative
 *   (voluntary) public health strategies; the coordination function (disease
 *   prevention) is genuine but asymmetrically distributed.
 *
 * KEY AGENTS:
 *   - public_health_bureaucracy: Agenda-setter (institutional/mobile) â administers mandates and gains authority
 *   - unvaccinated_refusers: Primary target (powerless/constrained) â bear extraction via penalties and exclusion
 *   - community_at_large: Secondary beneficiary (moderate/constrained) â receives herd immunity benefit
 *   - civil_liberties_advocates: Excluded voice (organized/mobile) â challenges framing but absent from policy table
 *   - constitutional_courts: Analytical observer (institutional/analytical) â adjudicates legitimacy limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.85).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.92).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Legitimacy â Public Health Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, 'a6f48a94-e510-4262-91ce-8e2e0bd34d83').
narrative_ontology:cs_kernel_codification('a6f48a94-e510-4262-91ce-8e2e0bd34d83', formalized).
narrative_ontology:cs_authority_grounding('a6f48a94-e510-4262-91ce-8e2e0bd34d83', lineage).
narrative_ontology:cs_interpretation_layer_present('a6f48a94-e510-4262-91ce-8e2e0bd34d83').
narrative_ontology:cs_reading_relation('a6f48a94-e510-4262-91ce-8e2e0bd34d83', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('a6f48a94-e510-4262-91ce-8e2e0bd34d83', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('a6f48a94-e510-4262-91ce-8e2e0bd34d83', foundational, collective_harm_trumps_bodily_autonomy).
narrative_ontology:cs_axiom_status(collective_harm_trumps_bodily_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('a6f48a94-e510-4262-91ce-8e2e0bd34d83', collective_harm_trumps_bodily_autonomy, deontological).
narrative_ontology:cs_axiom('a6f48a94-e510-4262-91ce-8e2e0bd34d83', foundational, unvaccinated_status_constitutes_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_constitutes_externality, holdable).
narrative_ontology:cs_axiom_grounding('a6f48a94-e510-4262-91ce-8e2e0bd34d83', unvaccinated_status_constitutes_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('a6f48a94-e510-4262-91ce-8e2e0bd34d83', police_power_collective_welfare).
narrative_ontology:cs_drift_state('a6f48a94-e510-4262-91ce-8e2e0bd34d83', post_pandemic_mandate_contest, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a6f48a94-e510-4262-91ce-8e2e0bd34d83', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, community_at_large).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_refusers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces vaccination mandates through legal orders, employment regulations, and exclusion policies. Gains institutional authority, budgetary capacity, and political capital from the expanded mandate to manage population health. Frames non-compliance as a negative externality requiring state intervention and coordinates enforcement across agencies.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, agenda_setter,
    institutional, generational, mobile, national).

% Bear the direct costs of mandate enforcement: loss of employment, exclusion from public spaces and transport, fines, and social censure. Their unvaccinated status is classified as a negative externality justifying state coercion. Exit requires medical compliance, geographic relocation, or acceptance of severe economic and social penalties.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_refusers, payer,
    powerless, immediate, constrained, national).

% Receives the coordination benefit of reduced disease transmission and the public good of healthcare system protection where the vaccine blocks transmission or severe outcomes. Does not directly pay the mandate's coercive costs but lives in a society where medical status is politicized, surveilled, and regulated by the state.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, community_at_large, beneficiary,
    moderate, biographical, constrained, national).

% Challenge mandate constitutionality and advocate for bodily autonomy rights in litigation and public discourse. They are structurally excluded from the public health policy table where the externality framing is established; their objections are treated as epistemically illegitimate or anti-social within the public health primacy framework.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, civil_liberties_advocates, excluded,
    organized, generational, mobile, national).

% Review the constitutional limits of mandate authority, assessing whether the state duty to prevent collective harm overrides individual rights under police power doctrine. Their rulings can validate or invalidate the enforcement machinery and define the boundaries of the constraint.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents infectious disease outbreaks by achieving high vaccination coverage, protecting those who cannot be vaccinated, and reducing healthcare system overload through collective immunity.
% TRANSFER_FUNCTION: Moves authority to coerce medical compliance, along with associated budgetary and enforcement resources, from the general polity and unvaccinated individuals to the public health bureaucracy, while transferring epidemiological risk reduction to the vaccinated community.
% ABSENT_VOICES: Civil liberties advocates and unvaccinated refusers are structurally excluded from the policy table where the externality framing is set; their objections are treated as epistemically illegitimate or anti-social within the public health primacy framework.
% DISAPPEARANCE_RATIONALE: If the mandate authority disappeared overnight, unvaccinated individuals would re-enter workplaces and public spaces without penalty, the public health bureaucracy would lose a major enforcement lever and stream of institutional authority, and the legal scope of police power in the biomedical domain would contract.
% FOUNDING_PROBLEM: Contagious disease outbreaks that overwhelm healthcare systems and kill vulnerable populations, where voluntary vaccination uptake is insufficient to achieve population-level protection.
% FOUNDING_PROBLEM_CORROBORATION: Public health officials assert the problem remains live. Civil liberties advocates and some independent epidemiologists attest that the problem was overstated or that alternative measures were adequate; retrospective all-cause mortality and transmission studies offer mixed corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.85, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness peaks at 0.85 because the constraint transfers severe economic and social penalties to unvaccinated refusers. Suppression reaches 0.92 because persistence requires active enforcement (employment bans, fines, QR systems) and the suppression of voluntary-alternative narratives. Theater rises to 0.50 as enforcement increasingly serves to display state capacity and bureaucratic authority rather than to manage transmission that vaccines may not fully block. Accessibility collapse is high (0.80) because legal alternatives to vaccination are narrowed to narrow medical exemptions. Resistance is substantial (0.75) due to widespread protests and litigation. The measurement grid is shared across metrics to prevent misaligned drift detection.
 *
 * PERSPECTIVAL GAP:
 *   The public health bureaucracy experiences the constraint as legitimate coordination for collective welfare, with low directionality. Unvaccinated refusers experience it as high-directionality extraction of bodily autonomy and economic standing. The community at large sits near symmetric: diffuse epidemiological benefit, but diffuse social polarization cost. Constitutional courts occupy an analytical seat where the divergence is visible but not experienced as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the public health bureaucracy (authority, budget, institutional growth) and the community at large (risk reduction). Victim declarations map to unvaccinated refusers (direct penalties, exclusion). The engine will derive low d for the bureaucracy and high d for the refusers, producing the seat divergence that characterizes tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) by preserving the genuine coordination function (disease prevention), while preventing mislabeling as pure coordination (rope) by declaring the asymmetric victimization of refusers. If the disease threat recedes and the mandate persists by bureaucratic inertia, the constraint would drift toward piton; if the externality claim is falsified, it collapses toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vaccine_mandate_kernel_frame,
    'Which reading of the vaccine mandate legitimacy kernel correctly captures the structural relationship between state authority and individual medical status?',
    'Comparative constitutional analysis across jurisdictions and empirical outcome evaluation of mandate efficacy versus voluntary uptake.',
    'Determines whether the constraint is classified as tangled_rope, snare, or rope depending on which reading''s structural data is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vaccine_mandate_kernel_frame, conceptual, 'Kernel reading under-determination for mandate legitimacy.').

omega_variable(
    externality_empirical_contingency,
    'Is unvaccinated status a genuine and significant externality for all pathogens and vaccines, or is this claim empirically contingent on specific transmission dynamics and sterilizing immunity?',
    'Epidemiological meta-analysis of transmission contribution by vaccination status, stratified by pathogen and vaccine type.',
    'If the externality claim is not empirically robust, the coordination function weakens and the constraint drifts toward pure extraction (snare); if robust, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_empirical_contingency, empirical, 'Whether the externality justification is empirically general or contingent.').

omega_variable(
    authority_grounding_framing,
    'Is the mandate authority better framed as constitutional police power (lineage) or as public health bureaucratic expansion (extraction)?',
    'Historical institutional analysis of public health authority growth versus continuity in constitutional police power doctrine.',
    'A lineage framing supports the coordination legitimacy of the constraint; an extraction framing recasts the bureaucracy as a rent-seeking authority and shifts directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Alternative commitment-system framings of mandate authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vaccine_mandate_ph_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(vaccine_mandate_ph_tr_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(vaccine_mandate_ph_tr_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(vaccine_mandate_ph_tr_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(vaccine_mandate_ph_tr_t16, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(vaccine_mandate_ph_tr_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement(vaccine_mandate_ph_tr_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 24, 0.5).

% Extraction over time
narrative_ontology:measurement(vaccine_mandate_ph_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(vaccine_mandate_ph_be_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 4, 0.35).
narrative_ontology:measurement(vaccine_mandate_ph_be_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(vaccine_mandate_ph_be_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(vaccine_mandate_ph_be_t16, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 16, 0.8).
narrative_ontology:measurement(vaccine_mandate_ph_be_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(vaccine_mandate_ph_be_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 24, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vaccine_mandate_ph_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(vaccine_mandate_ph_su_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(vaccine_mandate_ph_su_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(vaccine_mandate_ph_su_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 12, 0.82).
narrative_ontology:measurement(vaccine_mandate_ph_su_t16, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 16, 0.88).
narrative_ontology:measurement(vaccine_mandate_ph_su_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(vaccine_mandate_ph_su_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 24, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
