% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__prohibition_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Prohibition Reading: Substance Use as Punishable Moral Transgression
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the prohibition_reading of the
 *   substance_control_kernel: the commitment that substance use is a moral
 *   transgression requiring state punishment to protect social order. The
 *   standing arrangement under contest — and the fixed epsilon referent — is
 *   the criminalization regime itself: scheduled substances, punitive
 *   statutes, and the enforcement machinery that executes them. The regime is
 *   presented by its administrators as protection of social order; the
 *   structural record shows a transfer apparatus whose costs concentrate on
 *   users, record holders, and policed communities, and whose gains flow to
 *   the enforcement-carceral complex and to illicit suppliers collecting
 *   scarcity rents. Black-market violence operates as a secondary externality
 *   the regime generates rather than prevents. KEY AGENTS (by structural
 *   relationship): - drug_enforcement_apparatus: agenda-setter and primary
 *   beneficiary (institutional/identity_locked) — administers enforcement,
 *   collects appropriations and forfeited assets -
 *   criminalized_substance_users: principal bearer of costs
 *   (powerless/trapped) — arrest, incarceration, record -
 *   conviction_record_holders: post-contact cost bearers (powerless/trapped)
 *   — collateral consequences persist after sentence ends -
 *   overpoliced_low_income_communities: geographically concentrated cost
 *   bearers (moderate/constrained) - illicit_market_organizations: parasitic
 *   beneficiary (organized/arbitrage) — profits from the risk premium
 *   criminalization creates - carceral_facility_operators: contracted
 *   beneficiary (powerful/mobile) - forfeiture_funded_departments: revenue
 *   beneficiary (organized/arbitrage) - law_and_order_politicians: electoral
 *   beneficiary (powerful/arbitrage) - order_concerned_public: nominal
 *   beneficiary, incidental payer (moderate/constrained) -
 *   harm_reduction_services: excluded voice (organized/constrained) -
 *   public_health_researchers: analytical observer
 *   (institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.72).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.78).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Prohibition Reading: Substance Use as Punishable Moral Transgression").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, 'ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8').
narrative_ontology:cs_kernel_codification('ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8', formalized).
narrative_ontology:cs_authority_grounding('ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8', extraction).
narrative_ontology:cs_interpretation_layer_present('ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8').
narrative_ontology:cs_reading_relation('ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8', substance_control_kernel__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8', foundational, substance_use_is_moral_transgression).
narrative_ontology:cs_axiom_status(substance_use_is_moral_transgression, holdable).
narrative_ontology:cs_axiom_grounding('ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8', substance_use_is_moral_transgression, deontological).
narrative_ontology:cs_axiom('ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8', foundational, punishment_required_to_protect_social_order).
narrative_ontology:cs_axiom_status(punishment_required_to_protect_social_order, holdable).
narrative_ontology:cs_axiom_grounding('ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8', punishment_required_to_protect_social_order, instrumental).
narrative_ontology:cs_reference_frame('ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8', penal_moral_order).
narrative_ontology:cs_drift_state('ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8', contemporary_post_legalization_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee797bd7-8dc6-4ee7-997b-b6c1409aa9b8', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, drug_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, carceral_facility_operators).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, forfeiture_funded_departments).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, law_and_order_politicians).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, illicit_market_organizations).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, order_concerned_public).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, criminalized_substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, conviction_record_holders).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, overpoliced_low_income_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, order_concerned_public).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, expressive_condemnation_theory_of_law).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, punitive_deterrence_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets scheduling priorities, arrest patterns, and prosecution referral through statute and agency practice, and enforces them through a standing police, prosecutorial, and intelligence infrastructure. Receives appropriations, grant funding, and seized assets that scale with enforcement volume. Its mission statements, career ladders, and inter-agency relationships are built around drug suppression; pivoting to other missions would strand decades of specialized capability and personnel identity.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, drug_enforcement_apparatus, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, drug_enforcement_apparatus, beneficiary).

% Operates prisons and jails under contract or appropriation, with occupancy guarantees written into some facility agreements. Bed demand tracks enforcement output; when arrests fall, operators seek backfill populations or fight closures. Contracts are portable across jurisdictions, so operators follow enforcement markets rather than opposing them.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, carceral_facility_operators, beneficiary,
    powerful, generational, mobile, national).

% Seizes cash, vehicles, and property under drug statutes and routes proceeds into equipment, overtime, and training outside normal appropriation channels. Revenue arrives in proportion to enforcement aggressiveness, and equitable-sharing rules let units bypass state-level restrictions. The same units could police other priorities, but the revenue stream decides which priorities get staffed.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, forfeiture_funded_departments, beneficiary,
    organized, biographical, arbitrage, national).

% Converts enforcement posture into campaigns, endorsements, and media visibility; sponsors sentencing enhancements and blocks reform measures. The investment is rhetorical and procedural rather than operational, so commitment tracks polling rather than outcomes, and the issue can be dropped when the electoral weather changes.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, law_and_order_politicians, beneficiary,
    powerful, biographical, arbitrage, national).

% Supplies prohibited substances across borders. Criminalization adds a risk premium to every transaction that functions as profit margin, and enforcement against smaller competitors consolidates market share. Routes and product composition adapt faster than enforcement does; if prohibition ended, many operators would seek licenses in the legal market, as some did after cannabis legalization.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, illicit_market_organizations, beneficiary,
    organized, biographical, arbitrage, global).

% Receives the promised protection of social order and expresses durable support for punishment in the abstract, while paying for the system through taxation and absorbing spillover costs: black-market violence, overdose deaths in their own families, and crowded courts. Information about results arrives mainly through the enforcement institutions themselves.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, order_concerned_public, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, order_concerned_public, payer).

% Uses scheduled substances and carries the risk of arrest, incarceration, forced withdrawal, and a permanent record for conduct that medical frameworks treat as dependence or preference. Addiction, poverty, and the record itself close off relocation, licensing, and credit; desisting does not erase the record. Political voice is suppressed by the same exposure that defines the situation — organizing openly means self-incrimination.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, criminalized_substance_users, payer,
    powerless, biographical, trapped, national).

% Completed sentences but carry background-check exclusions in employment, housing, occupational licensing, and in some jurisdictions voting. The record is administered by thousands of separate employers, landlords, and agencies, so no single act of compliance clears it; expungement exists but is slow, discretionary, and unevenly available.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, conviction_record_holders, payer,
    powerless, biographical, trapped, national).

% Lives where enforcement concentrates: frequent stops, surveillance technology, and street-level market violence displaced from regulated commerce. Residents bear the public-order costs of the illegal market while receiving little of the service spending comparable tax bases receive. Household relocation is possible for some but fragments the community fabric that mutual aid and local politics depend on.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, overpoliced_low_income_communities, payer,
    moderate, generational, constrained, regional).

% Runs syringe exchanges, naloxone distribution, and supervised consumption programs where law permits, and is barred, unfunded, or criminally exposed where it does not. Holds operational evidence about what reduces death and disease but enters policy conversations as a witness rather than a designer, and several program types remain illegal across most prohibitionist jurisdictions.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, harm_reduction_services, excluded,
    organized, biographical, constrained, national).

% Measures use rates, overdose mortality, market violence, and enforcement disparities across jurisdictions and over time. Findings circulate through journals and advisory bodies with no enforcement authority; several commissioning agencies discount findings that threaten appropriation streams.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, drug_enforcement_apparatus).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines and enforces a shared moral boundary between licit and illicit intoxication: schedules substances, mobilizes police, prosecutorial, and carceral capacity against use and supply, and renders state moral authority publicly visible through enforcement activity.
% TRANSFER_FUNCTION: Moves liberty, income, and life-chances from users and the communities policed for them to the enforcement-carceral apparatus (appropriations, forfeiture proceeds, facility contracts, electoral capital) and to illicit suppliers, who collect the scarcity rents criminalization creates.
% ABSENT_VOICES: Active users and people with conviction records are structurally absent from legislative design — disenfranchised in some jurisdictions, stigmatized out of testimony, unable to advocate openly without self-incrimination. Harm-reduction practitioners are admitted as witnesses rather than designers. Affected neighborhoods speak through intermediaries at best. The unanimity behind punitive statutes is therefore consensus among parties who do not bear its costs.
% DISAPPEARANCE_RATIONALE: Enforcement agencies would lose mission and budgets; hundreds of thousands of annual cases would exit courts and facilities; illicit supply chains would collapse or convert as risk premiums vanished; the international treaty architecture and producer-state economies built around suppression would renegotiate; and the stock of criminal records shaping employment and housing would stop growing immediately.
% FOUNDING_PROBLEM: Containing what founding coalitions framed as moral contagion: intoxicant use among disfavored populations — opium fear attached to Chinese immigrant labor, cocaine fear to Black Americans, cannabis fear to Mexican labor and jazz culture — generalized into a doctrine that use itself threatens social order and must be punished.
% FOUNDING_PROBLEM_CORROBORATION: Historians of drug policy (the epidemic-tolerance cycle literature, commission records such as the 1972 Shafer Commission) and the public-health outcome literature attest, from outside the benefiting parties, that the founding moral-contagion framing was factually mistaken and racially constructed. No attestation of the founding problem's continuing validity comes from outside prohibitionist beneficiary and advocacy circles — the arrangement's own defenders are its only remaining witnesses.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__prohibition_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the transfer scales with enforcement capacity rather than with harm: sentencing severity has repeatedly diverged from pharmacological danger (the crack/powder disparity is the canonical case), and the largest cost components — incarceration, record consequences, market violence — are produced by the arrangement rather than by the substances. Suppression is high (0.78) because persistence depends on continuous police, prosecutorial, and carcereal activity and on legislatively blocking alternatives (harm-reduction programs remain illegal in most jurisdictions); the suppression_requirement series is authored deliberately, since this story's dynamic is the build-up and partial decay of enforcement machinery itself. Theater is moderate (0.34): the coercion is functionally real, but a persistent share of activity is spectacle — seizure statistics, bust imagery, pledge ceremonies — that performs resolve rather than altering outcomes. Accessibility_collapse is moderate-low (0.48): the constraint's logic retains grip, but living counterexamples (Portugal, state legalizations, alcohol-prohibition repeal) keep alternatives visibly available, so understanding the arrangement does not close the option space the way a natural law would. Resistance is substantial (0.62): reform movements, ballot initiatives, litigation, and mass everyday noncompliance. The series runs on one shared ten-point grid. The trajectory is cyclical rather than monotonic: a panic-tolerance cycle (epidemic alarm, enforcement surge, fatigue, relaxation, renewed alarm) with an asymmetric ratchet — each tolerance phase floors well above the previous one, so the oscillation itself functions as an accumulation mechanism, not noise. Base properties are measured at the interval end (t=54), a tolerance-phase point that sits far above the t=0 baseline. Extraction accumulation across the middle of the series (t=12 through t=30) is steep enough to trip the mountain-extraction-accumulation abductive trigger were this story claimed as a mountain; it is not, so the trigger's hypothesis lands as corroborating drift evidence instead.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same statutes. From the enforcement seat the arrangement is a legitimate order-protection function the seat itself administers, fused with professional identity — it computes as genuine coordination. From the user and record-holder seats the same structure is an ambient existential exposure that no compliant act terminates — it computes as pure imposed cost. From the illicit-supplier seat the arrangement is a rent-generating moat: the seat's optimal strategy is the constraint's continuation, and its 'exit' from the arrangement's harms is profiting by them. From the research seat it is a policy whose stated goal its own outputs contradict. The divergence is structural, not informational: all seats can read the same data and still compute different types, because directionality and exit options differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (enforcement apparatus, carceral operators, forfeiture-funded departments, politicians, illicit suppliers) derive directionality near the beneficiary end; arbitrage-grade exit keeps the politician, forfeiture, and supplier seats furthest toward subsidy. Declared victims (users, record holders, policed communities) derive directionality near the full-target end; trapped exit pushes users and record holders to the extreme. The order_concerned_public seat derives nominally as a beneficiary but sits near symmetric in fact — it pays taxes and absorbs spillover violence — and that residual asymmetry is left to the engine rather than corrected, because the available override lever keys on the power atom 'moderate', which this seat shares with the policed-community seat whose high directionality must not be disturbed. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already differentiate every seat the derivation needs to distinguish. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — containing moral contagion from disfavored users — is dead as a factual matter, yet the arrangement persists at near-peak enforcement scale. Classifying this as a snare rather than a rope is load-bearing: a rope classification would launder the transfer as the price of order-protection, and the victim declaration is what blocks that laundering. Equally, the genuine residual coordination need (managing addictive-substance externalities) belongs to the sibling readings' arrangements, not to this one; keeping the readings epsilon-invariant prevents that need from being cited inside this file to soften its classification. The persistence mechanism is capture plus identity fusion: the enforcement apparatus has institutionally become its function, so the mandate survives its own factual refutation. Coalition potential among the victim seats exists in principle — users, record holders, and policed communities overlap heavily — but stigma, disenfranchisement, and self-incrimination risk suppress coalition formation, which is why a powerless-class victim set has not converted into correcting power. The R5 interview records the mismatch directly: dead founding problem, world-rearranging disappearance verdict — the signature of an arrangement held up by its beneficiaries rather than by its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the substance_control_kernel — the prohibition_reading. What would change structurally if a sibling reading (harm_reduction_reading or legalization_reading) were instantiated instead?',
    'Nothing empirical resolves this; it is a commitment structure. Instantiating a sibling as its own constraint story shifts the victim set (users exit the criminal category), the beneficiary structure (service providers or regulated markets replace enforcement capture), and epsilon (authored materially lower over the sibling''s own arrangement). The disagreement is located in the normative classification of use itself.',
    'Recording the committer structure prevents cross-reading contamination: this file''s epsilon refers only to the punitive arrangement and is never averaged with sibling arrangements or with the legalization counterfactual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer frame: one reading of substance_control_kernel; sibling readings are separate constraints with separate files.').

omega_variable(
    transgression_status_of_use,
    'Is substance use in fact a moral transgression — a natural moral fact about the act — or is the transgression classification a constructed doctrine whose operation serves enforcement interests?',
    'Cross-cultural and historical-moral analysis: whether condemnation tracks harm caused by the act or marks disfavored user populations; the record of which substances and which users drew prohibition and which did not despite identical pharmacology.',
    'If constructed, the constraint holds no natural-law authority and reads as enforced doctrine maintained by its beneficiaries; if a genuine moral fact, part of its persistence reflects conscience rather than capture, and the victim set narrows to enforcement excess rather than criminalization as such.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transgression_status_of_use, conceptual, 'Natural moral fact versus constructed classification serving enforcement interests.').

omega_variable(
    deterrence_efficacy_vs_black_market_harm,
    'Does punitive enforcement reduce use and disorder net of the black-market violence, overdose, and incarceration harm the enforcement itself generates?',
    'Comparative jurisdiction evidence: Portugal''s 2001 decriminalization, US state cannabis legalizations, the 1920-1933 alcohol prohibition episode and its repeal, supervised-consumption pilot outcomes, and disparity-controlled dose-response studies of enforcement intensity.',
    'Net failure collapses the protective-function cover and confirms the arrangement as enforced doctrine with parasitic illicit markets; net success would establish a residual coordination function and shift the structural reading toward a hybrid coordination/extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_vs_black_market_harm, empirical, 'Whether the claimed protective function survives accounting for enforcement-generated harms.').

omega_variable(
    suppression_internalization_split,
    'How much of users'' political quiescence is structural suppression (arrest exposure, disenfranchisement, record consequences) versus internalized stigma (self-conception as criminal)?',
    'Political-participation trajectories in jurisdictions that removed arrest exposure: if organizing, testimony, and voter turnout among users and record holders surge after decriminalization, the structural share dominated; if quiescence persists unchanged, internalized stigma carries.',
    'If substantially internalized, the constraint''s effective hold persists after statutory reform — measured suppression understates it, and repeal alone under-delivers relative to the structural metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized suppression of the target population''s voice.').

omega_variable(
    panic_cycle_ratchet_trajectory,
    'Has the current tolerance phase established a new permanent floor, or will the next epidemic-scale drug panic re-ratchet enforcement to prior peaks?',
    'Track enforcement appropriations, sentencing legislation, and agency staffing through the next panic cycle; compare trough-to-trough and peak-to-peak values against the series in this story.',
    'Re-ratcheting confirms the oscillation-as-ratchet mechanism and dates the constraint''s operative end-state above current values; a broken ratchet would date a structural transition toward the sibling readings'' terrain and mark this reading as declining rather than stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(panic_cycle_ratchet_trajectory, empirical, 'Whether the panic-tolerance cycle continues functioning as an asymmetric ratchet.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prohibition_reading_tr_t0, substance_control_kernel__prohibition_reading, theater_ratio, 0, 0.26).
narrative_ontology:measurement_basis(prohibition_reading_tr_t0, observed).
narrative_ontology:measurement(prohibition_reading_tr_t6, substance_control_kernel__prohibition_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement_basis(prohibition_reading_tr_t6, observed).
narrative_ontology:measurement(prohibition_reading_tr_t12, substance_control_kernel__prohibition_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement_basis(prohibition_reading_tr_t12, observed).
narrative_ontology:measurement(prohibition_reading_tr_t18, substance_control_kernel__prohibition_reading, theater_ratio, 18, 0.39).
narrative_ontology:measurement_basis(prohibition_reading_tr_t18, observed).
narrative_ontology:measurement(prohibition_reading_tr_t24, substance_control_kernel__prohibition_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(prohibition_reading_tr_t24, observed).
narrative_ontology:measurement(prohibition_reading_tr_t30, substance_control_kernel__prohibition_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(prohibition_reading_tr_t30, observed).
narrative_ontology:measurement(prohibition_reading_tr_t36, substance_control_kernel__prohibition_reading, theater_ratio, 36, 0.35).
narrative_ontology:measurement_basis(prohibition_reading_tr_t36, observed).
narrative_ontology:measurement(prohibition_reading_tr_t42, substance_control_kernel__prohibition_reading, theater_ratio, 42, 0.34).
narrative_ontology:measurement_basis(prohibition_reading_tr_t42, observed).
narrative_ontology:measurement(prohibition_reading_tr_t48, substance_control_kernel__prohibition_reading, theater_ratio, 48, 0.34).
narrative_ontology:measurement_basis(prohibition_reading_tr_t48, observed).
narrative_ontology:measurement(prohibition_reading_tr_t54, substance_control_kernel__prohibition_reading, theater_ratio, 54, 0.34).
narrative_ontology:measurement_basis(prohibition_reading_tr_t54, observed).

% Extraction over time
narrative_ontology:measurement(prohibition_reading_be_t0, substance_control_kernel__prohibition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(prohibition_reading_be_t0, observed).
narrative_ontology:measurement(prohibition_reading_be_t6, substance_control_kernel__prohibition_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement_basis(prohibition_reading_be_t6, observed).
narrative_ontology:measurement(prohibition_reading_be_t12, substance_control_kernel__prohibition_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement_basis(prohibition_reading_be_t12, observed).
narrative_ontology:measurement(prohibition_reading_be_t18, substance_control_kernel__prohibition_reading, base_extractiveness, 18, 0.72).
narrative_ontology:measurement_basis(prohibition_reading_be_t18, observed).
narrative_ontology:measurement(prohibition_reading_be_t24, substance_control_kernel__prohibition_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement_basis(prohibition_reading_be_t24, observed).
narrative_ontology:measurement(prohibition_reading_be_t30, substance_control_kernel__prohibition_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement_basis(prohibition_reading_be_t30, observed).
narrative_ontology:measurement(prohibition_reading_be_t36, substance_control_kernel__prohibition_reading, base_extractiveness, 36, 0.75).
narrative_ontology:measurement_basis(prohibition_reading_be_t36, observed).
narrative_ontology:measurement(prohibition_reading_be_t42, substance_control_kernel__prohibition_reading, base_extractiveness, 42, 0.73).
narrative_ontology:measurement_basis(prohibition_reading_be_t42, observed).
narrative_ontology:measurement(prohibition_reading_be_t48, substance_control_kernel__prohibition_reading, base_extractiveness, 48, 0.72).
narrative_ontology:measurement_basis(prohibition_reading_be_t48, observed).
narrative_ontology:measurement(prohibition_reading_be_t54, substance_control_kernel__prohibition_reading, base_extractiveness, 54, 0.72).
narrative_ontology:measurement_basis(prohibition_reading_be_t54, observed).

% Suppression requirement over time
narrative_ontology:measurement(prohibition_reading_su_t0, substance_control_kernel__prohibition_reading, suppression_requirement, 0, 0.66).
narrative_ontology:measurement_basis(prohibition_reading_su_t0, observed).
narrative_ontology:measurement(prohibition_reading_su_t6, substance_control_kernel__prohibition_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement_basis(prohibition_reading_su_t6, observed).
narrative_ontology:measurement(prohibition_reading_su_t12, substance_control_kernel__prohibition_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement_basis(prohibition_reading_su_t12, observed).
narrative_ontology:measurement(prohibition_reading_su_t18, substance_control_kernel__prohibition_reading, suppression_requirement, 18, 0.86).
narrative_ontology:measurement_basis(prohibition_reading_su_t18, observed).
narrative_ontology:measurement(prohibition_reading_su_t24, substance_control_kernel__prohibition_reading, suppression_requirement, 24, 0.88).
narrative_ontology:measurement_basis(prohibition_reading_su_t24, observed).
narrative_ontology:measurement(prohibition_reading_su_t30, substance_control_kernel__prohibition_reading, suppression_requirement, 30, 0.87).
narrative_ontology:measurement_basis(prohibition_reading_su_t30, observed).
narrative_ontology:measurement(prohibition_reading_su_t36, substance_control_kernel__prohibition_reading, suppression_requirement, 36, 0.83).
narrative_ontology:measurement_basis(prohibition_reading_su_t36, observed).
narrative_ontology:measurement(prohibition_reading_su_t42, substance_control_kernel__prohibition_reading, suppression_requirement, 42, 0.79).
narrative_ontology:measurement_basis(prohibition_reading_su_t42, observed).
narrative_ontology:measurement(prohibition_reading_su_t48, substance_control_kernel__prohibition_reading, suppression_requirement, 48, 0.78).
narrative_ontology:measurement_basis(prohibition_reading_su_t48, observed).
narrative_ontology:measurement(prohibition_reading_su_t54, substance_control_kernel__prohibition_reading, suppression_requirement, 54, 0.78).
narrative_ontology:measurement_basis(prohibition_reading_su_t54, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'drug policy' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing the substance_control_kernel: this prohibition reading (punitive arrangement, epsilon approximately 0.72, users in the victim set, enforcement apparatus as primary beneficiary), the harm_reduction_reading (service-provision arrangement, users as patients, materially lower epsilon), and the legalization_reading (regulated-liberty arrangement, users as rights-holders, lowest epsilon). Prohibition is historically upstream: its enforcement externalities — black-market violence, overdose epidemics, mass incarceration — generate the evidentiary record the downstream readings cite against it. Each file authors its own stable epsilon over its own arrangement; none averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
