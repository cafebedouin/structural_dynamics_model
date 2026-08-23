% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__risk_stratification_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Actuarial Risk-Stratified Vaccine Mandate Legitimacy (Risk-Stratification Reading)
 *   domain: public_health/constitutional/bioethics
 *
 * SUMMARY:
 *   This story instantiates the risk-stratification reading of the
 *   vaccine_mandate_legitimacy kernel: mandate legitimacy is contingent on
 *   actuarial risk thresholds — blanket mandates fail proportionality,
 *   targeted mandates are permissible. The standing arrangement under contest
 *   is the mandate regime as actually deployed across the interval: broadly
 *   uniform requirements spanning risk strata, enforced through employment,
 *   education, and access rules. Epsilon is authored for THAT arrangement as
 *   this reading sees it — proportionality analysis finds the blanket
 *   component coercing low-risk individuals without proportional actuarial
 *   justification, layered over a genuine coordination core that protects
 *   people who cannot protect themselves. The sibling readings
 *   (public_health_primacy_reading, bodily_autonomy_primacy_reading) are
 *   separate constraints with their own epsilon, victim sets, and
 *   classifications; per the epsilon-invariance principle this file does not
 *   average across them or hedge between them. KEY AGENTS (by structural
 *   relationship): - public_health_authorities: Agenda setter
 *   (institutional/arbitrage) — sets thresholds and exemptions, runs
 *   enforcement, collects compliance - immunocompromised_and_elderly: Primary
 *   beneficiary (powerless/trapped) — receives protection it cannot
 *   self-provision - low_actuarial_risk_adults: Primary target
 *   (moderate/constrained) — bears blanket coercion disproportionate to
 *   actuarial contribution - high_exposure_essential_workers: Dual
 *   target-beneficiary (organized/constrained) — legitimately targeted yet
 *   fully coerced - employers_and_institutions: Secondary beneficiary
 *   (powerful/mobile) — collects liability simplification, bears admin costs
 *   - severely_immunocompromised_patients: Excluded voice (powerless/trapped)
 *   — wants broader coverage than thresholds deliver -
 *   unauthorized_frontline_workers: Unrepresented target (powerless/trapped)
 *   — bears enforcement without recourse - constitutional_courts: Analytical
 *   observer (institutional/analytical) — adjudicates proportionality
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda setter (institutional/arbitrage) — sets thresholds, defines exemptions, operates enforcement machinery
 *   - immunocompromised_and_elderly: primary beneficiary (powerless/trapped) — protection purchased by others' compliance
 *   - low_actuarial_risk_adults: primary target (moderate/constrained) — blanket coercion beyond actuarial justification
 *   - high_exposure_essential_workers: dual target-beneficiary (organized/constrained) — paradigm legitimate targets who nonetheless bear the full coercion
 *   - employers_and_institutions: secondary beneficiary (powerful/mobile) — liability simplification and administrative offloading
 *   - severely_immunocompromised_patients: excluded voice (powerless/trapped) — interests favor maximal coverage, unrepresented in threshold-setting
 *   - unauthorized_frontline_workers: unrepresented target (powerless/trapped) — enforcement without exemption access or appeal
 *   - constitutional_courts: analytical observer (institutional/analytical) — proportionality rulings reset legitimacy conditions for the whole family
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.52).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.5).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Actuarial Risk-Stratified Vaccine Mandate Legitimacy (Risk-Stratification Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).
narrative_ontology:has_sunset_clause(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, 'b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080').
narrative_ontology:cs_kernel_codification('b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080', formalized).
narrative_ontology:cs_authority_grounding('b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080', lineage).
narrative_ontology:cs_interpretation_layer_present('b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080').
narrative_ontology:cs_reading_relation('b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080', vaccine_mandate_legitimacy__public_health_primacy_reading, influences).
narrative_ontology:cs_reading_relation('b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080', foundational, mandate_legitimacy_requires_actuarial_proportionality).
narrative_ontology:cs_axiom_status(mandate_legitimacy_requires_actuarial_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080', mandate_legitimacy_requires_actuarial_proportionality, empirically_contingent).
narrative_ontology:cs_axiom('b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080', secondary, coercion_scaled_to_demonstrated_risk).
narrative_ontology:cs_axiom_status(coercion_scaled_to_demonstrated_risk, holdable).
narrative_ontology:cs_axiom_grounding('b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080', coercion_scaled_to_demonstrated_risk, instrumental).
narrative_ontology:cs_reference_frame('b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080', proportionality_gated_mandate_authority).
narrative_ontology:cs_drift_state('b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080', post_acute_phase_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b64a9bb7-e7fb-40d6-bdf3-8bbe4778f080', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_and_elderly).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, employers_and_institutions).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, low_actuarial_risk_adults).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, high_exposure_essential_workers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, unauthorized_frontline_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, high_exposure_essential_workers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, employers_and_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the eligibility rules, exemption categories, and enforcement mechanisms for vaccination requirements across workplaces, schools, and travel. Operate the compliance apparatus: verification systems, exemption review boards, penalty schedules. Collect compliance statistics and expanded administrative reach; when political conditions shift they can retire requirements and redirect budgets toward voluntary campaigns, though retired programs rarely return their funding lines.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Cannot mount strong vaccine responses or safely encounter circulating virus. Their safety depends on the vaccination status of people around them, which they cannot observe or negotiate individually. Requirements on others purchase protection they cannot obtain for themselves; losing those requirements leaves them managing exposure through isolation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_and_elderly, beneficiary,
    powerless, biographical, trapped, national).

% Young and healthy adults for whom severe outcomes were statistically rare. Broad requirements asked them to accept injection, documentation, and potential job or access loss in exchange for marginal reductions in risks they did not meaningfully contribute to or personally face. Compliance was usually cheaper than resistance; resistance meant litigation, relocation, or career disruption.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_actuarial_risk_adults, payer,
    moderate, biographical, constrained, national).

% Hospital staff, transit operators, food-processing workers — occupations where exposure frequency placed them above any reasonable risk threshold, both as people at elevated risk and as vectors toward patients and the public. Requirements fell on them first and hardest; they also gained the most direct workplace protection. Union representation gave them bargaining channels unavailable to unorganized workers.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, high_exposure_essential_workers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, high_exposure_essential_workers, beneficiary).

% Hospitals, universities, and large corporations. A uniform requirement simplified duty-of-care obligations, lowered insurance and liability exposure, and replaced bespoke accommodation decisions with a single rule administered elsewhere. They bore the administrative costs of verification and exemption processing and absorbed reputational damage when enforcement drew public backlash.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, employers_and_institutions, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, employers_and_institutions, payer).

% Transplant recipients, chemotherapy patients, and others whose protection requires community coverage well beyond what narrow risk thresholds deliver. Their strongest interest is maximal coverage, yet threshold-setting deliberations weighted individual liberty and administrative simplicity; no seat at the table represented maximizing their survival odds.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, severely_immunocompromised_patients, excluded,
    powerless, biographical, trapped, national).

% Worked the same high-exposure jobs as documented peers but lacked legal status, benefits, and standing to appeal exemptions or contest terminations. Requirements reached them through employers; recourse did not.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, unauthorized_frontline_workers, payer,
    powerless, immediate, trapped, national).

% Adjudicate challenges to requirement legitimacy: proportionality analyses, exemption-scope disputes, separation-of-powers questions about agency authority. Their rulings raise or lower the bar every future requirement must clear, and several landmark cases turned directly on whether uniform application survived scrutiny against risk-differentiated alternatives.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__risk_stratification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in community immunity: individuals capture protection from others' vaccination without bearing injection risk themselves, so uncoordinated uptake undershoots the level that protects the vulnerable. The threshold-gated form concentrates the corrective on the subpopulations whose behavior actually moves transmission and severe-outcome totals, purchasing protection at the smallest footprint of compelled compliance.
% TRANSFER_FUNCTION: Moves compliance burden and residual disease risk between groups: from the general population onto above-threshold individuals whose vaccination is compelled, and from vulnerable populations onto the immunized surrounding them. Under the blanket form actually deployed, it additionally moved autonomy costs onto low-risk individuals far beyond what their actuarial contribution justified.
% ABSENT_VOICES: Severely immunocompromised patients, whose survival interest favors broader coverage than any liberty-respecting threshold delivers, had no seat in threshold-setting. Unauthorized frontline workers bore enforcement without recourse channels. Future-pandemic planners inherit the resulting trust deficit but were absent when blanket extensions were decided.
% DISAPPEARANCE_RATIONALE: If threshold-contingent legitimacy vanished overnight, mandate politics collapses to one of the two poles: public-health-primacy jurisdictions would impose uniform requirements unchecked by proportionality challenge, autonomy-primacy jurisdictions would strip all requirements including protective ones for the vulnerable, and the litigation dockets, exemption bureaucracies, and employment policies built around risk-differentiated rules would unravel.
% FOUNDING_PROBLEM: Compulsory vaccination has been contested since smallpox-era ordinances: how much coercion may the state apply to bodies that resist it, and on what showing? The risk-stratified answer crystallized once epidemiology made risk measurable per person: coercion should scale to demonstrated actuarial risk rather than apply uniformly.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: constitutional courts' proportionality rulings (European courts striking down indiscriminate measures; U.S. district courts splitting on military and contractor mandates), the Nuffield Council on Bioethics and WHO guidance articulating proportionality tests, and historical scholarship tracing the Jacobson v. Massachusetts lineage. Public health agencies also attest the problem is live, but their attestation is self-interested; the external judicial and bioethics record carries the corroborating weight.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52 end-state) reflects a mixed regime: the blanket component extracted compliance from low-risk adults beyond actuarial justification (peaking near 0.70 during blanket extension), declining as mandates lifted and reverted toward targeted forms this reading deems legitimate. Suppression (0.50) is structural-dominant: enforcement ran through termination, enrollment denial, and access exclusion — material consequences rather than belief management — with a minority internalized component (professional-duty fusion among healthcare staff; see omega). Theater (0.35) tracks enforcement persisting past marginal benefit: booster requirements maintained after widespread infection-acquired immunity, documentation rituals continuing after the emergency ended. The three series share one time grid (T=0..36 at step 6) so no metric borrows another's end-state at earlier points. Trajectories are surge-coupled rather than smoothly monotonic: tightening tracked variant waves, producing a ratchet that advances in surges, and the intermittent rhythm itself — relax, surge, re-tighten — functioned as part of the compliance mechanism rather than as noise. Claim/metric independence: claimed_type=tangled_rope is asserted from structure (genuine coordination function, identifiable payers, active enforcement); the metrics are authored descriptively and were not tuned to land the claim in any category.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From public_health_authorities the arrangement is a necessary instrument they built and calibrated; from low_actuarial_risk_adults the same blanket rules operated as uncompensated coercion — identical documents, opposite lived constraint. High_exposure_essential_workers sit genuinely astride the divide: targeted requirements are this reading's paradigm of justified coercion, yet they are the ones coerced, and their union organization is the only payer-side lever that altered terms. Courts occupy the analytical seat where the divergence becomes adjudicable, and their split rulings are the observable form of the perspectival gap. The engine computes per-seat classifications from the structural data; this commentary explains the asymmetry without adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive directionality toward the subsidized end: immunocompromised_and_elderly receive protection they cannot self-provision with no exit at all (nearest full beneficiary); employers_and_institutions collect liability simplification tempered by real administrative costs; public_health_authorities derive authority, budget, and compliance metrics from the arrangement they administer. Victim declarations drive directionality toward the target end: low_actuarial_risk_adults bear blanket coercion with constrained exit; unauthorized_frontline_workers bear it with no recourse at all (nearest full target); high_exposure_essential_workers are dual-positioned — full coercion borne, substantial protection received — netting moderately above symmetric. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct ordering, and per-power-atom overrides would misfire across the multiple agents sharing each power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem has a resolved half and a live half. The acute-phase problem — suppressing mortality surges before vaccination and infection converged — is dead: by the interval's end, blanket extension persisted mostly as administrative inertia and institutional reluctance to admit miscalibration (theater_ratio peaking at 0.43), which is the mandatrophy signature; mandatrophy_resolved is declared for that blanket-emergency instantiation. The endemic problem — calibrating coercion to whatever the next pathogen's actuarial profile warrants — is alive and will recur, hence founding_problem_status=live. Reading the constraint through this reading prevents two symmetrical mislabels: the public-health frame alone renders blanket persistence as pure coordination (hiding the payers), and the autonomy frame alone renders the whole apparatus as pure extraction (hiding the coordination core that protects the trapped vulnerable). The hybrid classification keeps both halves visible; the open question is whether the threshold mechanism survives contact with the next emergency or collapses into one of the poles (see omegas).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_definition_contingency,
    'Where the actuarial threshold sits — and what counts as ''risk'' (personal severe-outcome risk, transmission risk, population-weighted burden) — determines the size and identity of the coerced set; which threshold definition does this reading''s legitimacy claim rest on?',
    'Comparative jurisdiction analysis: map mandated populations against alternative threshold definitions (age band, comorbidity, occupational exposure, infection-acquired-immunity credit) and observe which definitions survive legal and clinical challenge.',
    'A narrow clinical threshold shrinks the coerced set toward a genuinely targeted regime (coordination-dominant); a broad population-weighted threshold expands it toward blanket coverage, collapsing this reading''s distinction from the public-health-primacy sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_definition_contingency, conceptual, 'Victim set size and classification hinge on threshold definition; sibling-collapse risk.').

omega_variable(
    actuarial_measurement_granularity,
    'Can actuarial risk be measured finely enough at decision time to support targeted mandates, or do available proxies (age bands, crude comorbidity flags) collapse targeting into blanket practice?',
    'Audit exemption and deferral determinations against later-validated individual risk scores; measure discordance between proxy-assigned and true risk strata.',
    'If proxies are too coarse, the reading''s coordination promise fails in operation and the regime collapses into one of the extreme siblings despite this reading''s formal adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_measurement_granularity, empirical, 'Whether risk stratification is operationally implementable or collapses to blanket.').

omega_variable(
    free_riding_asymmetry_status,
    'Below-threshold individuals free-ride on protection purchased by coerced above-threshold compliance — is that asymmetry a fixable design defect of threshold mandates, or inherent to any partial coercion regime?',
    'Counterfactual modeling of coverage and severe-outcome totals under alternative burden-sharing designs (incentive-tiered, insurance-linked, universal-with-opt-out) versus pure threshold gating.',
    'If inherent, the hybrid coordination-plus-asymmetry character is stable across designs; if fixable, a redesigned regime could shed the extraction asymmetry and migrate toward pure coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(free_riding_asymmetry_status, conceptual, 'Whether the extraction asymmetry is intrinsic to threshold-gated mandates.').

omega_variable(
    emergency_endemic_calibration_drift,
    'Were thresholds calibrated for acute-phase mortality suppression misapplied to endemic circulation, driving enforcement past its justified endpoint?',
    'Compare stated threshold rationales at imposition with the evidentiary basis cited at each extension; date the divergence between declared actuarial justification and actual continuation.',
    'Confirms the late-interval theater rise as calibration failure rather than bad faith, and locates the reform point: sunset-and-recalibrate clauses rather than abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_endemic_calibration_drift, empirical, 'Acute-to-endemic threshold miscalibration as driver of late-interval theater.').

omega_variable(
    suppression_composition_structural_internalized,
    'Is the measured suppression predominantly structural (termination, access denial, enrollment loss) or partly internalized (professional-duty identity making exit unthinkable for healthcare staff)?',
    'Post-rollback compliance and attrition trajectories: if mandate-opposed staff exited once rules lifted, suppression was structural; if duty-frame behavior persists after removal, part of the suppression was internalized.',
    'An internalized share raises effective suppression above the structural measure and predicts persistence of compliance norms after formal repeal; a structural share predicts rapid reversion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_composition_structural_internalized, empirical, 'Structural versus internalized composition of enforcement pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 18, 0.37).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 36, 0.35).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 6, 0.66).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 18, 0.71).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 36, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 18, 0.69).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 36, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, resource_allocation).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'vaccine mandates' decomposes into three structurally distinct constraints sharing the vaccine_mandate_legitimacy kernel. public_health_primacy_reading is upstream (established Jacobson-lineage doctrine cited as settled warrant); this risk_stratification_reading mediates (conditional, proportionality-gated legitimacy); bodily_autonomy_primacy_reading exerts downstream counter-pressure (categorical prohibition). Epsilon differs across the family because the victim set differs: the public-health reading treats unvaccinated status as the managed externality; this reading counts blanket-coerced low-risk individuals as payers and above-threshold subjects as legitimate targets; the autonomy reading counts any coerced person as wronged. Each file stands alone with its own stable epsilon; the edges here propagate legitimacy shifts and contamination across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
