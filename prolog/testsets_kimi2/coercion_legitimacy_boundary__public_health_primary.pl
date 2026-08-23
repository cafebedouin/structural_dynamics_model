% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: State Compelled Medical Intervention (Public Health Primary Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint is the public_health_primary reading of the
 *   coercion_legitimacy_boundary kernel. It instantiates the claim that the
 *   state may compel medical intervention when collective harm-prevention
 *   outweighs individual autonomy. Under this reading, unvaccinated
 *   individuals enter the victim set as coerced subjects, while
 *   immunocompromised populations exit the victim set to become protected
 *   beneficiaries. The enforcement apparatus generates high epsilon. Sibling
 *   readings include bodily_autonomy_primary (categorical prohibition on
 *   non-consensual intervention) and proportionality_reading (coercion scales
 *   with disease severity).
 *
 * KEY AGENTS:
 *   - state_public_health_authority: Agenda-setter (institutional/constrained) â designs, mandates, and enforces intervention policy
 *   - unvaccinated_individuals: Primary target (powerless/constrained) â bear coerced medical intervention and exclusion costs
 *   - immunocompromised_population: Protected beneficiary (powerless/trapped) â receive reduced exposure risk without agency in the policy
 *   - community_health_beneficiaries: Diffuse beneficiary (organized/constrained) â benefit from herd effects but do not direct the constraint
 *   - civil_liberties_organizations: Excluded voice (organized/analytical) â would contest coercion framing but are marginalized in emergency policy discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.78).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.75).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "State Compelled Medical Intervention (Public Health Primary Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '581dd19d-cfe1-4d75-9dec-c009d4836d88').
narrative_ontology:cs_kernel_codification('581dd19d-cfe1-4d75-9dec-c009d4836d88', formalized).
narrative_ontology:cs_authority_grounding('581dd19d-cfe1-4d75-9dec-c009d4836d88', lineage).
narrative_ontology:cs_interpretation_layer_present('581dd19d-cfe1-4d75-9dec-c009d4836d88').
narrative_ontology:cs_reading_relation('581dd19d-cfe1-4d75-9dec-c009d4836d88', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('581dd19d-cfe1-4d75-9dec-c009d4836d88', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('581dd19d-cfe1-4d75-9dec-c009d4836d88', foundational, collective_health_over_individual_autonomy).
narrative_ontology:cs_axiom_status(collective_health_over_individual_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('581dd19d-cfe1-4d75-9dec-c009d4836d88', collective_health_over_individual_autonomy, conventional).
narrative_ontology:cs_reference_frame('581dd19d-cfe1-4d75-9dec-c009d4836d88', public_health_police_power_authority).
narrative_ontology:cs_drift_state('581dd19d-cfe1-4d75-9dec-c009d4836d88', post_pandemic_legal_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('581dd19d-cfe1-4d75-9dec-c009d4836d88', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_population).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, community_health_beneficiaries).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are subject to state-mandated medical intervention and face legal penalties, exclusion from employment, or loss of access to public spaces if they refuse. Their bodily autonomy is overridden by the collective harm-prevention calculus.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    powerless, biographical, constrained, national).

% Cannot mount effective immune responses and depend on high community vaccination coverage to avoid severe outcomes. They benefit from reduced pathogen circulation but have no direct control over the coercive policies that produce it.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_population, beneficiary,
    powerless, biographical, trapped, national).

% Benefit from herd immunity and reduced outbreak risk as a diffuse public good. They do not administer the policy and cannot easily opt out of the public health system, but they also do not bear the direct coercive burden.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, community_health_beneficiaries, beneficiary,
    organized, biographical, constrained, national).

% Designs, mandates, and enforces medical intervention policies using police power and public health statutes. It collects expanded authority and compliance, and is constrained by legal precedent, political incentives, and institutional mission.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, state_public_health_authority, agenda_setter,
    institutional, generational, constrained, national).

% Contest the legitimacy of compelled medical intervention on constitutional and human-rights grounds. They are routinely sidelined in emergency public health deliberations and their objections are framed as threats to collective safety.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, civil_liberties_organizations, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, state_public_health_authority).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents epidemic transmission by achieving vaccination coverage beyond voluntary uptake levels, thereby protecting those who cannot be immunized and reducing pathogen circulation across the population.
% TRANSFER_FUNCTION: Moves the burden of medical risk and compliance from the diffuse community and immunocompromised populations to specific unvaccinated individuals, while transferring decision authority over bodily intervention from the individual to the state apparatus.
% ABSENT_VOICES: Bodily-autonomy absolutists and libertarian medical-ethics advocates are structurally excluded; their absence is engineered by framing dissent as a public health threat rather than a legitimate rights claim.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, unvaccinated individuals would regain medical decision autonomy, the state would lose its claimed monopoly on coercive health intervention, and immunocompromised populations would face elevated exposure risk, forcing reliance on voluntary uptake and non-coercive mitigation.
% FOUNDING_PROBLEM: Contagious disease outbreaks that exceed voluntary containment capacity, producing externalized harm to non-consenting third parties who cannot protect themselves through individual action alone.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and public health historians corroborate that historical outbreaks have exceeded voluntary response capacity. Civil liberties organizations and medical ethicists contest that current coercion levels remain proportionate to present risk, attesting from outside the beneficiary set.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint authorizes involuntary bodily intervention, overriding individual autonomy. Suppression (0.75) is high because persistence depends on legal penalties, employment exclusion, and social participation barriers for non-compliant individuals. Theater ratio (0.40) reflects that while the epidemiological justification is genuine, a substantial fraction of enforcement activity performs state authority and compliance theater rather than marginal risk reduction. Accessibility collapse (0.72) is high because legal and medical exemptions are narrow and stigmatized. Resistance (0.60) captures organized legal challenges, political mobilization, and non-compliance movements. The temporal series show a ratchet: extraction and suppression rise as mandates expand from targeted populations to broader populations and enforcement hardens, while theater creeps upward as epidemiological rationale thins.
 *
 * PERSPECTIVAL GAP:
 *   The unvaccinated seat experiences the constraint as direct bodily coercion with high extraction and high suppression; the state seat experiences it as legitimate coordination with low extraction and necessary enforcement; the immunocompromised seat experiences it as protective subsidy with negative effective extraction. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals are declared victims and have constrained exit, yielding high directionality (near full target). Immunocompromised and community beneficiaries are declared beneficiaries with limited exit (trapped/constrained), yielding low directionality (near subsidy). The state agenda-setter is not a declared beneficiary in base_properties but is the capturer of extracted authority per gain_flow; its institutional power and constrained exit place its derived directionality in the mid-low range, reflecting its role as orchestrator rather than target.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists mandatrophy mislabeling because the coordination function (herd immunity, outbreak containment) is structurally real and historically vindicated, while the extraction function (coerced compliance, state authority expansion) is equally real and borne by a distinct victim set. Abolishing the constraint would not leave the founding problem unsolvedâpersuasion and voluntary uptake remain alternativesâso the coordination story is not post-hoc cover, but it is not the whole story either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'This constraint is the public_health_primary reading of the coercion_legitimacy_boundary kernel. How would the beneficiary/victim structure change under the bodily_autonomy_primary or proportionality_reading siblings?',
    'Cross-reading comparison via the sibling constraints in the same kernel family.',
    'The bodily_autonomy_primary reading would remove unvaccinated individuals from the victim set and reclassify the state as aggressor; the proportionality_reading would make victim status contingent on empirical disease severity thresholds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Structural position of this reading within the contested kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, exclusion from public space) or internalized (social stigma, medical paternalism norms)?',
    'Post-exit suppression trajectory: if unvaccinated individuals who relocate to non-mandate jurisdictions still experience social or professional penalties, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggestsâthe target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    coercion_threshold_risk_scaling,
    'Is the coercion threshold empirically tethered to current pathogen risk, or has it decoupled into a standing authorization for compelled intervention regardless of severity?',
    'Compare mandate activation and maintenance thresholds against real-time reproduction numbers, hospitalization rates, and seroprevalence; observe whether mandates persist after risk declines.',
    'If decoupled, base_extractiveness is higher than the coordination story justifies and the constraint drifts toward snare; if tethered, the extraction is proportionate to the genuine coordination need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_threshold_risk_scaling, empirical, 'Whether coercion thresholds track actual epidemiological risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(coer_tr_t4, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 4, 0.25).
narrative_ontology:measurement(coer_tr_t8, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 8, 0.3).
narrative_ontology:measurement(coer_tr_t12, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 12, 0.35).
narrative_ontology:measurement(coer_tr_t16, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 16, 0.38).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(coer_be_t4, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(coer_be_t8, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(coer_be_t12, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 12, 0.71).
narrative_ontology:measurement(coer_be_t16, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(coer_su_t4, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(coer_su_t8, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(coer_su_t12, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(coer_su_t16, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 16, 0.73).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
