% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Public Health Mandate Supersedes Individual Consent (Lethal Exposure Reading)
 *   domain: public_health/constitutional_law
 *
 * SUMMARY:
 *   In the public-health-primary reading of the vaccine-mandate-balance
 *   kernel, collective protection supersedes individual consent when
 *   voluntary compliance fails to achieve herd immunity and vulnerable
 *   populations face lethal exposure risk. The constraint is the necessity
 *   doctrine: when pathogen circulation threatens those who cannot
 *   self-protect through vaccination (immunocompromised, infants, medically
 *   exempt populations), public health authorities are justified in mandating
 *   vaccination for the broader population, subordinating autonomy to
 *   epidemiological necessity. This reading presents a genuine coordination
 *   problem (many individuals' autonomous choice leaves protected populations
 *   exposed) AND asymmetric extraction (coerced individuals bear autonomy
 *   costs; protected populations bear medical risk). The engine computes this
 *   per-seat: from the public health authority's seat, necessity justifies
 *   the mandate and the constraint is genuine coordination. From the coerced
 *   individual's seat, consent subordination is extractive enforcement. From
 *   the protected-but-powerless population's seat, the constraint is
 *   life-preserving. The CLAIM (tangled_rope) and the METRICS (extraction
 *   0.68, suppression 0.71) are authored independently: the constraint's
 *   structure combines real coordination (protecting the coordinationally
 *   excluded) with real extraction (subordinating autonomy). No tuning
 *   reconciles them.
 *
 * KEY AGENTS:
 *   - immunocompromised_exposed_populations: Cannot be vaccinated; depend entirely on population immunity; beneficiary but powerless.
 *   - unvaccinated_coerced_individuals: Forced to accept vaccination against stated preference; their autonomy is subordinated by necessity doctrine; victims.
 *   - religious_objector_groups: Conscientious objectors; identity-locked (exit requires abandoning religious community); victims.
 *   - public_health_authorities: Set mandate scope, adjudicate necessity, define vulnerable-population thresholds; agenda-setters; control necessity-doctrine invocation.
 *   - elected_legislators: Authorize emergency mandate authority; benefit politically from appearing protective while avoiding enforcement blame.
 *   - bodily_autonomy_advocacy_organizations: Excluded from emergency decision-making; would contest necessity doctrine; structurally absent from mandate deliberation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.68).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.71).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public Health Mandate Supersedes Individual Consent (Lethal Exposure Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health/constitutional_law").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, '52d028b0-cb8e-4d8a-bb6e-a6e8784303ea').
narrative_ontology:cs_kernel_codification('52d028b0-cb8e-4d8a-bb6e-a6e8784303ea', formalized).
narrative_ontology:cs_authority_grounding('52d028b0-cb8e-4d8a-bb6e-a6e8784303ea', extraction).
narrative_ontology:cs_interpretation_layer_present('52d028b0-cb8e-4d8a-bb6e-a6e8784303ea').
narrative_ontology:cs_reading_relation('52d028b0-cb8e-4d8a-bb6e-a6e8784303ea', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('52d028b0-cb8e-4d8a-bb6e-a6e8784303ea', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('52d028b0-cb8e-4d8a-bb6e-a6e8784303ea', foundational, necessity_doctrine_supremacy).
narrative_ontology:cs_axiom_status(necessity_doctrine_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('52d028b0-cb8e-4d8a-bb6e-a6e8784303ea', necessity_doctrine_supremacy, deontological).
narrative_ontology:cs_axiom('52d028b0-cb8e-4d8a-bb6e-a6e8784303ea', foundational, collective_protection_duty_over_individual_consent).
narrative_ontology:cs_axiom_status(collective_protection_duty_over_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('52d028b0-cb8e-4d8a-bb6e-a6e8784303ea', collective_protection_duty_over_individual_consent, deontological).
narrative_ontology:cs_reference_frame('52d028b0-cb8e-4d8a-bb6e-a6e8784303ea', voluntary_coordination_framework).
narrative_ontology:cs_drift_state('52d028b0-cb8e-4d8a-bb6e-a6e8784303ea', emergency_mandate_invocation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('52d028b0-cb8e-4d8a-bb6e-a6e8784303ea', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_exposed_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, infants_too_young_for_vaccination).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, medically_exempt_populations).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, vaccine_hesitant_population).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, religious_objector_groups).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction measures 0.68 because mandates impose direct bodily intrusion (vaccination requirement) on non-consenting individuals to benefit protected populations. The coerced population cannot arbitrage or exit through refusal alone—they must accept vaccination, accept exclusion/sanctions, or relocate. This is high extraction with limited exit. Suppression measures 0.71 because mandate enforcement requires active suppression of resistance: employment-based coercion, institutional exclusion, benefits denial, and in some jurisdictions, legal penalties. Alternatives (treating the disease, accepting isolation, alternative vaccines) are available in principle but foreclosed or very costly in practice. Theater measures 0.28 (moderate-low) because the constraint has real coordination function (herd immunity genuinely protects the coordinationally excluded) but increasing share of enforcement activity defends subordination of autonomy rather than actual disease prevention (enforcement against philosophically opposed but compliant individuals, or enforcement against populations with low transmission risk). Accessibility_collapse measures 0.72 because alternatives are substantially foreclosed: consent refusal → employment loss or institutional exclusion is a significant collapse, though not complete (geographic exit remains possible, and legal challenges remain available post-hoc). The measurement series track uptake acceleration early (t0-t12), then plateau (t24-t36) as resistant populations self-identify and enforcement reaches steady state. Suppression requirement rises early as enforcement machinery is built, then plateaus as suppression becomes structural background. Theater ratio rises slowly as enforcement intensity becomes more performative relative to new-infection-prevention value (initial mandates drive uptake; later-stage mandates mainly discipline non-compliers). These are not cyclical dynamics—they reflect institutional escalation to steady state, not oscillating social equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the public health authority seat: the constraint is genuine coordination solving a collective-action problem. Voluntary compliance failed; pathogen circulation threatens those who cannot self-protect; necessity doctrine justifies mandate. Classification should compute as tangled_rope-leaning-rope (coordination dominates, extraction is the price of solving the coordination problem). From the unvaccinated coerced seat: the constraint is enforced extraction subordinating autonomy to a collective benefit they did not agree to. Classification should compute as tangled_rope-leaning-snare (extraction dominates, coercion is the enforcement mechanism, consent is explicitly subordinated). From the immunocompromised seat: the constraint is protective—without herd immunity, they face lethal risk. They benefit from extraction imposed on others. Classification should compute as rope (pure coordination, no extraction from their perspective, though they may feel moral discomfort with the autonomy costs borne by the coerced). The engine computes these per-seat divergences from the structural data—beneficiary/victim designation, power level, exit options. The divergence is the point: this constraint looks different from every seat because it genuinely IS different for every seat. No single classification is 'correct'—the per-seat mapping is the structural finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are near the beneficiary end (d ≈ 0.2): they set the constraint, control necessity adjudication, and benefit from mandate authority. Their exit is analytical (they can choose not to invoke mandates, but institutional incentive structures favor invoking them once authority is granted). Immunocompromised populations are beneficiaries by necessity but their d should be computed as near-zero or negative (they benefit without running the constraint; they have no exit because their vulnerability is irreducible). Unvaccinated coerced individuals are near the target end (d ≈ 0.8-0.9): they bear the constraint, consent is subordinated, exit is highly constrained (accept vaccination, accept exclusion, or relocate at high cost). Religious objectors are identity-locked, which amplifies d (d ≈ 0.85): exit requires reconstructing identity (abandoning religious community or practice), which exceeds typical constrained exit. Vaccine-hesitant individuals have lower d (d ≈ 0.75) because they retain some choice (medical exemption pathways exist even if hard to access; geographic exit is possible). No directionality overrides are needed—the structural data produces accurate d values through the derivation chain: beneficiary/victim + power + exit → d → χ.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is the process by which a constraint's original function (protect the vulnerable from lethal exposure) outlives the conditions that made it necessary, but the constraint persists anyway. This reading does NOT yet exhibit mandatrophy (founding_problem_status is 'contested,' not 'dead,' and disappearance_verdict is 'world_rearranges,' indicating arrangements still depend on the constraint). However, mandatrophy risk is high: as vaccine supply increases, as vulnerable populations access preventive treatments (monoclonal antibodies, early treatments), as population immunity accrues through vaccination+infection, the necessity doctrine becomes less empirically grounded. The constraint could persist through institutional inertia (mandates remain 'because we have mandate authority') or through regulatory capture (institutions benefit from mandate authority and continue invoking necessity). The theater ratio (currently 0.28) is the early warning: if it rises sharply toward 0.50+ while extraction remains high, mandatrophy is setting in. The measurement series should be extended: if theater_ratio rises above 0.40 while base_extractiveness plateaus, the constraint is becoming mostly performative, and mandatrophy classification should be considered. Currently, the constraint is not yet mandatrophy—the necessity doctrine is still contested, not obviously false, and the protected populations still face demonstrable (if declining) risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_failure_threshold_ambiguity,
    'At what voluntary uptake rate can collective mandate authority claim that voluntary means have ''failed''? Is it threshold-driven (e.g., <70% uptake = failure) or conditions-driven (e.g., failure to reach threshold despite sufficient time and trust-building)?',
    'Systematic analysis of uptake trajectories across jurisdictions with different campaign intensities, trust-building timelines, and incentive structures; comparison of actual vs. counterfactual uptake under different conditions; expert panel assessment of what constitutes ''sufficient'' voluntary effort.',
    'A lower threshold favors mandate authority (failure is claimed earlier, mandates activate sooner); a higher threshold favors autonomy-first readings (failure requires demonstrating genuine exhaustion of voluntary means). This ambiguity allows mandate authority to claim failure prematurely and subordinate autonomy before necessity is genuinely established.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_failure_threshold_ambiguity, empirical, 'What constitutes sufficient failure of voluntary means before mandates are justified.').

omega_variable(
    lethal_exposure_vs_manageable_risk,
    'Does the unvaccinated population''s continued circulation constitute ''lethal exposure risk'' to immunocompromised groups, or a manageable risk that can be mitigated through targeted shielding, treatment, and isolation protocols?',
    'Epidemiological modeling comparing lethal outcome rates under mandate vs. non-mandate scenarios; real-world data from jurisdictions that did not implement mandates; treatment efficacy data for exposed immunocompromised populations; cost-effectiveness analysis of targeted protection vs. population-wide mandates.',
    'If risk is demonstrably lethal without mandates, the necessity case for this reading strengthens; if risk is manageable through alternative protections, alternative readings become structurally viable. This omega gates the empirical claim underlying the necessity doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lethal_exposure_vs_manageable_risk, empirical, 'Whether circulatory pathogen exposure to immunocompromised is lethal or manageable without mandates.').

omega_variable(
    consent_subordination_scope_boundary,
    'Does subordination of consent to necessity apply narrowly (only to those in direct contact with protected populations) or broadly (to everyone in the population, regardless of individual risk or exposure likelihood)?',
    'Constitutional analysis of necessity doctrine limits; comparative jurisprudence on emergency powers scope; epidemiological risk stratification (who actually poses transmission risk to vulnerable populations); institutional practice in jurisdictions with bounded vs. universal mandates.',
    'Narrow scope constrains mandate authority (only high-transmission-risk individuals forced into compliance); broad scope maximizes extraction (everyone compelled regardless of individual risk contribution). This ambiguity allows mandate scope to exceed epidemiological necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_subordination_scope_boundary, conceptual, 'How widely the necessity doctrine extends the subordination of consent beyond those directly responsible for vulnerable exposure.').

omega_variable(
    identity_locked_exemption_availability,
    'Are robust religious and conscience exemptions available in practice, or is ''conscience objection'' nominally permitted but structurally foreclosed through administrative burden, bad-faith adjudication, or institutional hostility?',
    'Audit of exemption grant rates by jurisdiction and demographic; documented cases of bad-faith denial; survey data on whether objectors perceive exemption pathways as genuinely available; analysis of institutional trust in exemption adjudication.',
    'If exemptions are genuinely available, identity-locked classification for religious objectors may be incorrect (exit becomes ''constrained'' rather than ''trapped''). If exemptions are procedurally available but administratively or institutionally foreclosed, suppression is higher than coded, and the constraint is more extractive than the metrics indicate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_locked_exemption_availability, empirical, 'Whether conscience and religious exemptions are structurally available or nominally available but practically foreclosed.').

omega_variable(
    kernel_contest_foreclosure,
    'Does the public-health-primary reading logically foreclose the bodily-autonomy-primary reading within a single constitutional framework, or do both readings remain live policy options held by different jurisdictions and constitutional traditions?',
    'Comparative constitutional jurisprudence; examination of whether any legal system holds both readings simultaneously (both entrenched in law); analysis of whether the necessity doctrine permits bodily autonomy exceptions even when public health gains are possible.',
    'If foreclosure is genuine (necessity REQUIRES subordination of consent), the readings are in genuine contradiction. If both can be held simultaneously (some rights are inviolable even when public health gains are possible), the readings coexist and mandate-scope questions become political rather than logical. This omega determines the reading-relation classification in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Whether the necessity doctrine logically forecloses bodily autonomy protections or merely overrides them in policy.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is suppression of mandate resistance primarily structural (coercive barriers: employment loss, institutional exclusion, social benefits denial) or internalized (belief in necessity, acceptance of subordination, internalized shame/obligation)?',
    'Post-mandate analysis in jurisdictions where mandates were lifted: do resistance and hesitancy return to pre-mandate levels (suggesting suppression was structural) or persist (suggesting internalized acceptance)? Survey of mandate opponents on whether barriers prevent them or persuasion prevents them. Comparison of suppression in jurisdictions with different enforcement mechanisms.',
    'If suppression is primarily structural, it decays when enforcement ends. If suppression is internalized, the constraint persists through belief even after coercion is removed. If internalized, effective suppression is higher than measured, and the constraint''s hold is stronger than the base metric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether mandate resistance suppression is structural (external barriers) or internalized (changed beliefs/norms).').

omega_variable(
    reading_instantiation_scope,
    'This JSON instantiates the public-health-primary reading specifically. Does this reading apply to all vaccine-preventable diseases at all severity levels, or only to high-transmissibility pathogens causing demonstrable lethal outcomes in vulnerable populations?',
    'Examine whether public health authorities invoke this reading equally for COVID-19, measles, seasonal influenza, and HPV. Check whether necessity doctrine is constrained by disease severity, transmission rate, or outcome severity in actual mandate decisions. Review historical mandate scope (which diseases triggered mandates under this reading).',
    'A narrow reading (only demonstrable lethal pathogens) preserves some autonomy protection. A broad reading (any communicable disease) maximizes mandate authority and creates path dependency (mandates for low-severity diseases establish precedent for broader future mandates). This omega clarifies the scope of this specific reading instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_instantiation_scope, conceptual, 'The scope of pathogens and disease severities this reading covers in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_balance__public_health_primary, theater_ratio, 4, 0.21).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__public_health_primary, theater_ratio, 8, 0.24).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__public_health_primary, theater_ratio, 12, 0.26).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_balance__public_health_primary, theater_ratio, 18, 0.27).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__public_health_primary, theater_ratio, 24, 0.28).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_balance__public_health_primary, theater_ratio, 30, 0.28).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_balance__public_health_primary, theater_ratio, 36, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__public_health_primary, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__public_health_primary, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__public_health_primary, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_balance__public_health_primary, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__public_health_primary, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_balance__public_health_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_balance__public_health_primary, base_extractiveness, 36, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__public_health_primary, suppression_requirement, 4, 0.56).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__public_health_primary, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__public_health_primary, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_balance__public_health_primary, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__public_health_primary, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_balance__public_health_primary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_balance__public_health_primary, suppression_requirement, 36, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__public_health_primary, 0.18).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vaccine-mandate-balance kernel. The kernel contests whether collective protection or individual consent is primary. The public-health-primary reading (THIS file) subordinates consent to necessity; the bodily-autonomy-primary reading subordinates collective benefit to inviolable consent; the proportionality reading seeks middle ground through strict thresholds and exemptions. Each reading is authored as a separate constraint with its own ε, metrics, and stakeholder structure because the structural logic differs: public-health-primary measures extraction through autonomy subordination, bodily-autonomy-primary measures extraction through the threat of disease treated as autonomy violation, proportionality-reading measures extraction through scope limitations. The three constraints are linked via network.affects_constraints in both directions—each reading's classification influences the others (if bodily autonomy is reclassified as genuinely inviolable, it forecloses the public-health-primary reading's necessity doctrine). The decomposition follows DP-001 (ε-invariance): one constraint, one ε. Here, ε ≈ 0.68 because the public-health-primary reading measures extraction through mandate enforcement coercion. Other readings would produce different ε values reflecting their different structural premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
