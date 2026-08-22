% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: State Vaccine Mandate Authority (Public Health Primacy Reading)
 *   domain: public_health/constitutional/bioethical
 *
 * SUMMARY:
 *   This constraint story instantiates the public health primacy reading of
 *   the vaccine mandate legitimacy kernel — the reading in which state duty
 *   to prevent collective harm justifies mandate authority and unvaccinated
 *   status is classified as externality. Under this reading, the mandate
 *   solves a genuine coordination problem (disease transmission; free-rider
 *   vaccination) through both coordination (information and distribution
 *   infrastructure) and extraction (suppression of refusers as a means to
 *   achieve herd immunity thresholds). The unvaccinated are positioned as
 *   victims because suppression is justified as correcting their imposition
 *   of externality costs on the vaccinated. This is NOT a claim about whether
 *   the reading is empirically correct — it is the structured assumption the
 *   reading makes. Sibling readings (bodily autonomy primacy, risk
 *   stratification) instantiate different frameworks where the constraint's
 *   type, beneficiary/victim structure, and suppression justification differ
 *   fundamentally. The engine computes per-seat classification from this
 *   structural data; divergence across readings is the measurement the corpus
 *   takes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.68).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.79).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "State Vaccine Mandate Authority (Public Health Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health/constitutional/bioethical").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, '3f5047b7-f761-42b9-8087-dae513a1bd40').
narrative_ontology:cs_kernel_codification('3f5047b7-f761-42b9-8087-dae513a1bd40', fixed_text).
narrative_ontology:cs_authority_grounding('3f5047b7-f761-42b9-8087-dae513a1bd40', lineage).
narrative_ontology:cs_interpretation_layer_present('3f5047b7-f761-42b9-8087-dae513a1bd40').
narrative_ontology:cs_reading_relation('3f5047b7-f761-42b9-8087-dae513a1bd40', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('3f5047b7-f761-42b9-8087-dae513a1bd40', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('3f5047b7-f761-42b9-8087-dae513a1bd40', foundational, state_duty_collective_harm_prevention).
narrative_ontology:cs_axiom_status(state_duty_collective_harm_prevention, holdable).
narrative_ontology:cs_axiom_grounding('3f5047b7-f761-42b9-8087-dae513a1bd40', state_duty_collective_harm_prevention, deontological).
narrative_ontology:cs_axiom('3f5047b7-f761-42b9-8087-dae513a1bd40', foundational, unvaccinated_status_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_externality, holdable).
narrative_ontology:cs_axiom_grounding('3f5047b7-f761-42b9-8087-dae513a1bd40', unvaccinated_status_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('3f5047b7-f761-42b9-8087-dae513a1bd40', state_police_power_collective_harm_duty).
narrative_ontology:cs_drift_state('3f5047b7-f761-42b9-8087-dae513a1bd40', endemic_phase_therapeutics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3f5047b7-f761-42b9-8087-dae513a1bd40', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, medical_exemption_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets vaccine mandate scope, exemption criteria, enforcement thresholds, and compliance timelines. Administers the mandate through licensing bodies, school attendance rules, employment regulations, and public facility access. Justifies the mandate as preventing disease externalities and protecting herd immunity thresholds. Gains institutional authority, regulatory budget, surveillance capacity, and political capital from mandate administration.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive protection from disease transmission in aggregate — the coordination function of the mandate. Under this reading, unvaccinated status is externality imposed on them; the mandate redresses that externality. They also incur minor information-gathering costs (compliance documentation, testing access verification) and live under permanent surveillance infrastructure built to enforce the mandate. Low individual pressure; high aggregate mobilization for enforcement.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population, payer).

% Bear the mandate's direct suppression: employment loss, school exclusion, license revocation, facility access denial, and social stigmatization. The reading classifies them as externalizers whose refusal imposes costs on others; suppression is justified as correcting that externality. Their exit options collapse: accepting vaccination (forced choice), relocating to non-mandate jurisdictions (capital-intensive, often infeasible for working-class refusers), or accepting prohibition (economic and social death). No seat in the mandate-setting process; opposition framed as irresponsible imposing costs on others.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers, payer,
    moderate, biographical, constrained, national).

% Individuals with documented medical contraindications to vaccination (immunocompromise, prior severe reaction, rare genetic conditions). This reading's mandate framework treats them as a second victim set: they bear exemption-seeking costs (medical documentation, clinical gatekeeping, bureaucratic processing delay) and simultaneously carry the exclusion cost — unvaccinated status marks them as externalizers even when vaccination is contraindicated. Identity-locked: their medical condition is intrinsic and persistent; exit via vaccination is medically unavailable.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, medical_exemption_seekers, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, medical_exemption_seekers, excluded).

% Public health researchers and policy analysts arguing for actuarial risk-based mandates (targeting high-transmission settings, high-severity disease populations, narrow demographics) rather than blanket coverage. They would argue mandates should narrow to where externalities are largest and suppress least. Excluded from the mandate-setting process by institutional hierarchy; their proposals framed as insufficiently protective rather than engaged with on efficiency grounds.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, risk_stratification_advocates, excluded,
    moderate, biographical, constrained, national).

% Review mandate constitutional validity under competing frameworks: state police power vs. fundamental rights to bodily autonomy and medical freedom. Courts under this reading face pressure to endorse the collective-harm justification or impose narrow tailoring requirements. Their review is framed as adjudicating competing constitutional goods, not questioning whether the externality classification is accurate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Faith communities and conscience-objection advocates arguing vaccine mandates violate sincerely held religious or philosophical beliefs. Under this reading, conscience objections are treated as subjective preference rather than as legitimate competing values. They attempt to secure exemptions through litigation and legislative advocacy but are consistently out-positioned by collective-harm rhetoric.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, religious_liberty_organizations, excluded,
    moderate, biographical, constrained, national).

% Hospital and institutional ethics bodies tasked with applying mandate policy to individual cases. They operate under the collective-harm framework but confront identity-locked agents (medical contraindication cases) where the framework produces perverse results. Their role is implementation, not authority-setting; they can document but not override the institutional mandate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, clinical_ethics_committees, observer,
    moderate, biographical, analytical, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves disease transmission reduction and herd immunity thresholds by requiring vaccination across a population, solving the free-rider problem where individuals decline vaccination while benefiting from others' immunity. Centralizes vaccine distribution and monitoring through state infrastructure.
% TRANSFER_FUNCTION: Moves suppression costs from the unvaccinated refuser to the vaccinated (or to those denied exemptions): employment security, school enrollment, facility access, and social standing. The reading frames this as correcting externalities — imposing costs on those who refuse to bear their share of collective protection. Moves authority and surveillance capacity to the public health bureaucracy.
% ABSENT_VOICES: Individuals whose risk profile (age, prior infection, comorbidity) would make vaccination less beneficial than cost; religious and conscience objectors whose values prioritize bodily autonomy over collective outcomes; risk-stratification advocates who would narrow mandates to high-externality settings; and unvaccinated persons themselves, whose opposition is framed as irresponsibility rather than as a legitimate alternative framework. They are structurally excluded from mandate-setting.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, vaccination rates would drop (estimated 5-15 percentage points), disease incidence would rise, and public health bureaucracy would lose the authority and surveillance infrastructure built to enforce compliance. The coordination function (herd immunity achievement) depends on the mandate's active suppression; voluntary uptake alone would not reach the thresholds this reading asserts are necessary.
% FOUNDING_PROBLEM: Novel infectious disease (SARS-CoV-2) posed acute collective harm risk; free-rider vaccination dynamics left unvaccinated populations as disease reservoirs, creating externalities for the vaccinated. Public health authorities interpreted their mandate as preventing that externality through blanket vaccination requirements.
% FOUNDING_PROBLEM_CORROBORATION: Public health bureaucracy and epidemiologists attesting to ongoing transmission risk corroborate the founding problem's persistence. However, researchers at competing methodological schools (comparing endemic disease burden to mandate era mortality, examining risk stratification, studying vaccination-infection hybrid immunity) contest whether the founding problem persists in the form the reading claims. Legislative testimony from refuser communities and independent bioethicists contest whether unvaccinated status generates the externality the reading asserts. The founding problem's status is litigated, not settled by external corroboration.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 endpoint) because the mandate's persistence depends on redefining refusal as externality, not on voluntary participant consent or reciprocal benefit — suppression sustains the coordination story, not genuine preference alignment. The measurement trajectory shows extraction rising steeply in the early interval (0.41 → 0.61 by T=12) as the mandate scope broadens and enforcement hardened, then stabilizing (0.68 by T=24, flat thereafter). Suppression is higher still (0.79) because mandate persistence requires active exclusion of refusers from employment, education, and facilities — compliance is not incidental but enforced. Theater rises gradually (0.08 → 0.28) as enforcement infrastructure matures and the reading's legitimacy story becomes more elaborate (risk communication, medical exemption gatekeeping, public health theater) while core suppression remains constant. At T=36 onward, extractiveness and suppression plateau: the mandate has achieved maximum institutional embedding and political normalization; further compression of alternatives is marginal. This is tangled_rope: genuine coordination function (disease transmission reduction, herd immunity) + asymmetric extraction (suppression borne by refusers) + active enforcement (employment/facility gatekeeping, exemption denial). The claim and metrics are independent: we author the reading's own framing (coordination rationale) and also the metrics that describe its actual operation (extraction dominance). The divergence is diagnostic — it is what the framework exists to measure.
 *
 * PERSPECTIVAL GAP:
 *   Public health bureaucracy and vaccinated population seats compute the constraint as coordination with justified suppression of externalities (low extraction from their position, beneficiary seats). Vaccine refusers compute it as pure suppression — their exit options collapse identically regardless of any coordination story, and suppression is their experienced reality (high extraction, victim seat, constrained exit). Medical exemption seekers occupy a structurally broken position: classified as externalizers (victim) when vaccination is medically contraindicated (identity-locked, not a choice). Risk-stratification advocates would compute narrower extraction if their favored framing (actuarial proportionality) governed — but they are excluded from agenda-setting, so their alternative is not the operative constraint. The engine derives directionality from power + exit + beneficiary/victim declarations; this structural gap is where seat divergence emerges.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health bureaucracy: d ≈ 0.05 (agenda-setter, institutional power, mobile exit, gains authority and budget). Vaccinated population: d ≈ 0.35 (organized power, mobile exit, collects coordination benefit but also minor cost). Vaccine refusers: d ≈ 0.92 (moderate power, constrained exit, victim designation, employment/facility loss). Medical exemption seekers: d ≈ 0.95 (powerless, identity-locked exit, victim designation, exemption-seeking costs + exclusion marking). Risk-stratification advocates: excluded from the constraint entirely — their alternative framing is not the operative one, so their directionality does not enter the story. Constitutional courts: d ≈ 0.5 (analytical seat, no collection or paying, reviewing framework validity). The operative directionality is asymmetric because the reading's structural assumption is asymmetric: the unvaccinated are externalizers requiring suppression, not parties to coordination. The suppression cost derivation flows directly from that assignment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (novel disease, transmission risk, free-rider vaccination) is contested as to whether it persists in the form the reading claims — endemic phase, waning transmission risk, prior-infection immunity, and therapeutic options have altered the original emergency posture. However, the institutional mandate persists and has deepened (exemption narrowing, scope broadening, surveillance elaboration). This is the mandatrophy signature: the founding problem's status shifts from 'live' (early interval) to 'contested' or 'dead' (later interval) while the constraint persists and even intensifies. The theater_ratio gradient (rising through T=12, then flat) captures this: early on, suppression tracks disease risk messaging (coordination theatre, functional connection); later, suppression carries mostly institutional momentum and political normalization (pure theatre, connection severed). The rising extraction + stable suppression + rising theatre trajectory is the classic mandatrophy pattern: the original justification has decayed but the extraction mechanism has calcified into institutional routine. The tangled_rope classification is accurate for the reading's internal logic — it genuinely coordinates disease transmission reduction — but the mandatrophy measurement flags that the reading's justification may have outlived its referent problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_classification_legitimacy,
    'Is unvaccinated status a genuine externality (imposing costs on others) or a status asymmetry created by the reading''s categorization framework?',
    'Epidemiological analysis of transmission dynamics, vaccination effectiveness waning, and cross-population risk stratification. Compare disease burden in vaccinated vs. unvaccinated cohorts, controlling for prior infection and age-stratified risk. Compare to risk of vaccine adverse effects in lower-risk populations. Test whether the externality persists at all risk thresholds or only in specific demographics.',
    'If unvaccinated status is a genuine externality across demographics, the reading''s victim classification and suppression justification hold. If externality is threshold-dependent (high only above certain age/risk cutoffs), the risk_stratification_reading becomes structurally superior and suppression becomes overreach. If externality is concept-dependent (created by treating vaccination as the only relevant risk measure, ignoring prior infection equivalence), the reading is constructed rather than discovered, and mandatrophy accelerates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_classification_legitimacy, empirical, 'Whether unvaccinated status is empirically externality-generating across all populations the mandate covers, or whether externality is constructed by the reading''s chosen observable.').

omega_variable(
    bodily_autonomy_foreclosure,
    'Does the public health primacy reading logically foreclose the bodily autonomy primacy reading, or do they coexist as incompatible but not mutually exclusive frameworks?',
    'Examine whether accepting the state duty to prevent collective harm (public health primacy axiom) logically entails rejecting medical self-determination as inviolable (bodily autonomy primacy axiom). Test via: (a) whether any party could coherently hold both axioms simultaneously in one legal framework, (b) whether the readings differ only on priority weights (balancing) vs. differ on logical necessity, (c) whether one axiom is nested inside the other or whether they are orthogonal.',
    'If foreclosure is real (genuine logical contradiction), the readings represent incompatible worldviews and one must be eliminated from institutional operation — constitutional courts must choose. If foreclosure is not real (balancing problem, priority dispute, axioms coexist as incompatible-but-jointly-holdable), the readings coexist, courts can impose narrow tailoring as compromise, and both continue as live positions. If coexistence is correct, the constraint''s type shifts: from zero-sum institutional conflict to negotiated boundary maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bodily_autonomy_foreclosure, conceptual, 'Whether public health duty and bodily autonomy are logically incompatible or merely prioritized differently across readings.').

omega_variable(
    mandate_founding_problem_persistence,
    'Does the founding problem (novel disease transmission risk, free-rider vaccination dynamics) persist in the form the reading asserts, or has it been superseded by endemic disease, vaccine availability, and therapeutic options?',
    'Track disease epidemiology (incidence, severity, transmission) as a function of vaccination status and prior infection status, controlling for demographic and geographic confounding. Compare to alternative risk mitigation strategies (prior-infection immunity, targeted prophylaxis, therapeutic access). Assess whether disease burden in the post-mandate interval is consistent with the founding problem persisting or whether it has compressed below the reading''s original threat threshold.',
    'If founding problem persists (live status), the tangled_rope and mandatrophy classification hold. If founding problem has been substantially resolved but mandate persists (dead status), mandatrophy is confirmed and constraint reclassifies toward piton (zombie extraction, theatrical maintenance). If founding problem status is contested but the measurement data is clear, the reading''s legitimacy foundation is undermined and constitutionality challenges gain structural basis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_founding_problem_persistence, empirical, 'Whether the novel-disease threat that justified the mandate''s founding remains live or has been resolved by endemic phase and therapeutic availability.').

omega_variable(
    medical_exemption_incoherence,
    'How does the reading coherently apply victim classification and suppression justification to medically-contraindicated individuals who cannot choose compliance?',
    'Examine the reading''s own logic: if suppression is justified because refusers externalize costs by choosing non-compliance, how is that justification extended to individuals whose medical condition makes compliance impossible? Document the logical move — does it apply the externalizer label despite absence of choice, does it deny the medical contraindication, does it establish a different suppression justification for the identity-locked subset?',
    'If the reading cannot coherently extend suppression to the identity-locked subset, that subset is a falsifying case for the reading''s foundational assumption. The response reveals whether the reading is a systematic classification system (where incoherence is structural flaw) or a political construct (where incoherence is managed via exemption gatekeeping and rhetoric). High incoherence would favor the bodily_autonomy_primacy reading, which applies consistently across all statuses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medical_exemption_incoherence, conceptual, 'Whether the reading''s victim/suppression logic extends coherently to medically-contraindicated individuals or breaks down.').

omega_variable(
    suppression_internalization_trajectory,
    'Is the measured suppression (0.79) purely structural (external enforcement) or partially internalized (refusers accept the externalizer label and internalize their status as deserving exclusion)?',
    'Post-mandate period trajectory study: measure refuser attitudes and self-conception after mandate lift (if it occurs). If suppression was purely structural, suppression should drop sharply and refuser-beneficiary self-positioning should shift. If suppression was partially internalized, suppression would persist (self-exclusion, stigma internalization, persistent deferential behavior) even after external enforcement ends. Survey refuser communities on whether exclusion feels externally imposed or self-deserved.',
    'If internalized: effective suppression cost is higher than the structural measure (individuals enforce it on themselves); constraint is more extractive than metrics suggest; post-mandate recovery time is longer. If purely structural: individuals would rapidly reintegrate once external barriers drop, suggesting the extraction was coercive rather than normatively accepted. Internalization level affects whether the constraint is classified as snare (coercive) vs. tangled_rope (extraction defended by coordination story) — if internalization is high, the coordination story''s power is confirmed; if internalization is low, the reading is pure rationalization over coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'What proportion of the measured suppression is structural (external enforcement) vs. internalized (refusers accept the externalizer narrative).').

omega_variable(
    risk_stratification_alternative_feasibility,
    'Could the reading''s stated coordination goal (achieve herd immunity, reduce disease transmission) be met via risk-stratified, narrower mandates instead of blanket mandates, and if so, why is the blanket approach maintained?',
    'Model disease transmission dynamics under stratified mandate scenarios (mandate for high-transmission settings, elderly, immunocompromised; voluntary uptake elsewhere). Compare herd immunity thresholds achieved, disease burden reduction, and suppression cost across mandate strategies. If stratified mandates achieve equivalent disease control at lower suppression cost, the blanket mandate cannot be defended as minimally extractive; if blanket mandates achieve substantially better outcomes, the reading''s strategy is vindicated.',
    'If stratified mandates are feasible and equivalent-or-superior, the reading''s choice of blanket mandates reveals extraction beyond what the coordination story requires — indicating the constraint is closer to snare than tangled_rope. If blanket mandates are necessary for the stated coordination goal, the reading''s framework is vindicated and the tangled_rope classification holds. If the reading refuses to consider stratification on principle (state duty obligates universal coverage regardless of actuarial threshold), that refusal signals the coordination story is secondary to the authority-expansion motive — tangled_rope with dominance toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_stratification_alternative_feasibility, conceptual, 'Whether the reading''s blanket-mandate approach is minimally extractive given the coordination goal, or whether risk-stratified alternatives would achieve equivalent coordination at lower cost.').

omega_variable(
    kernel_reading_relationship_to_bodily_autonomy,
    'What is the structural relationship between this reading (public health primacy) and the bodily_autonomy_primacy_reading? Do they foreclose each other, coexist, or influence each other?',
    'Logical analysis: Does accepting state duty to prevent collective harm logically require rejecting bodily autonomy as inviolable? (Test foreclosure.) Or can a single party or legal framework hold both, prioritizing them differently based on context? (Test coexistence.) Or does one reading create structural pressure that changes how the other operates (one survives but constrained)? (Test influence.)',
    'If foreclosure: the kernel admits only one reading''s core axiom; institutional choice is zero-sum. If coexistence: both readings remain live, courts can narrow the winning reading via proportionality doctrine, and compromise is possible. If influence: one reading constrains the other (e.g., bodily autonomy reading survives but only as narrow exception to collective-harm authority). The classification affects whether institutional conflict over the mandate is resolvable or fundamental.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_relationship_to_bodily_autonomy, conceptual, 'Whether public health primacy and bodily autonomy primacy readings logically foreclose each other or coexist as incompatible-but-jointly-holdable frameworks.').

omega_variable(
    theater_ratio_function_decay,
    'Is the rising theater_ratio (0.08 → 0.28) evidence of Goodhart drift (the coordination function is decaying and being replaced by performative maintenance), or is it evidence of institutional maturation (better communication, more sophisticated exemption processing, more elaborate legitimacy narrative)?',
    'Decompose theater_ratio growth by component: measure (a) public health communication volume and content drift over time, (b) exemption-processing bureaucratic layers added, (c) actual contact-tracing and disease-surveillance activity, (d) enforcement vs. coordination in enforcement activity composition. If early years show high disease-focused communication and later years show high authority-defense communication, Goodhart drift is present. If exemption gatekeeping activity increases faster than disease risk changes warrant, bureaucratic overgrowth is present.',
    'If Goodhart drift is confirmed, the coordination function''s decay supports mandatrophy diagnosis and suggests the constraint should be reclassifying toward piton. If institutional maturation is the accurate reading, the rising theater reflects normal adaptation and does not signal coordination collapse. If bureaucratic overgrowth is present without disease-responsive justification, the constraint is extractive-plus-performative, closer to snare+piton hybrid than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_function_decay, empirical, 'Whether rising theater_ratio represents Goodhart drift (function decay replaced by performance) or institutional maturation (legitimate adaptation to persistent coordination problem).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 36, 0.28).
narrative_ontology:measurement(vacc_tr_t48, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 48, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.41).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 36, 0.68).
narrative_ontology:measurement(vacc_be_t48, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 48, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 12, 0.76).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 24, 0.79).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 36, 0.79).
narrative_ontology:measurement(vacc_su_t48, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 48, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vaccine_mandate_legitimacy' kernel, decomposed per ε-invariance principle. Each reading instantiates the same institutional arrangement (state-mandated vaccination) from a different normative vantage, producing different ε values, different victim/beneficiary classifications, and different suppression justifications. The public_health_primacy_reading asserts state duty to prevent collective harm as the legitimacy foundation and classifies unvaccinated individuals as externalizers; the bodily_autonomy_primacy_reading denies the externalizer classification and asserts inviolable medical self-determination; the risk_stratification_reading accepts conditional externalizer status but requires actuarial proportionality in mandate scope. Each is a complete constraint story with independent metrics. They are linked via network.affects_constraints to enable family-level analysis: which reading dominates institutional operation, whether readings compete or coexist, how institutional decisions about mandate scope reveal which reading's axioms are operative. Sibling story IDs: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__public_health_primacy_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
