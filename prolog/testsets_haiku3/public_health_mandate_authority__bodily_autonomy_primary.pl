% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate as Bodily Autonomy Violation
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the bodily_autonomy_primary reading of the
 *   public_health_mandate_authority kernel. The reading treats public health
 *   mandates as categorically violating bodily sovereignty: no
 *   epidemiological benefit, no collective health outcome, and no vulnerable
 *   population protection justifies non-consensual medical intervention. The
 *   constraint's structure is a snare—coercion without legitimacy—because the
 *   reading denies that public health authorities possess authority to compel
 *   bodily intervention regardless of pandemic severity. The reading
 *   explicitly EXCLUDES immunocompromised persons from the victim set: their
 *   vulnerability does not generate a duty to protect them via bodily
 *   invasion of others. This structural exclusion is the reading's
 *   distinctive move—it rejects the harm-prevention rationale that would
 *   justify collective protection via mandate.
 *
 * KEY AGENTS:
 *   - unvaccinated_persons: powerless targets bearing coercive bodily intervention under duress
 *   - public_health_authorities: institutional agenda-setters administering coercion without legitimate authority (per this reading)
 *   - religious_objectors: constrained payers forced to violate conscience
 *   - immunocompromised_persons: structurally EXCLUDED from victim set (not protected via bodily invasion of others)
 *   - civil_liberties_advocates: observers documenting the constraint as rights violation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.89).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.78).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.89).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate as Bodily Autonomy Violation").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, 'e909b1fc-1f40-41fd-8598-4d43b0739045').
narrative_ontology:cs_kernel_codification('e909b1fc-1f40-41fd-8598-4d43b0739045', fixed_text).
narrative_ontology:cs_authority_grounding('e909b1fc-1f40-41fd-8598-4d43b0739045', lineage).
narrative_ontology:cs_interpretation_layer_present('e909b1fc-1f40-41fd-8598-4d43b0739045').
narrative_ontology:cs_reading_relation('e909b1fc-1f40-41fd-8598-4d43b0739045', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('e909b1fc-1f40-41fd-8598-4d43b0739045', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('e909b1fc-1f40-41fd-8598-4d43b0739045', foundational, bodily_sovereignty_inviolable).
narrative_ontology:cs_axiom_status(bodily_sovereignty_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('e909b1fc-1f40-41fd-8598-4d43b0739045', bodily_sovereignty_inviolable, deontological).
narrative_ontology:cs_axiom('e909b1fc-1f40-41fd-8598-4d43b0739045', foundational, no_collective_benefit_justifies_bodily_violation).
narrative_ontology:cs_axiom_status(no_collective_benefit_justifies_bodily_violation, holdable).
narrative_ontology:cs_axiom_grounding('e909b1fc-1f40-41fd-8598-4d43b0739045', no_collective_benefit_justifies_bodily_violation, deontological).
narrative_ontology:cs_reference_frame('e909b1fc-1f40-41fd-8598-4d43b0739045', bodily_autonomy_supremacy).
narrative_ontology:cs_drift_state('e909b1fc-1f40-41fd-8598-4d43b0739045', contemporary_pandemic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e909b1fc-1f40-41fd-8598-4d43b0739045', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_persons).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, vaccine_hesitant_populations).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, religious_objectors).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, medical_exemption_seekers).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, bodily_sovereignty_inviolable).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, consent_absolute_precondition).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, collective_benefit_insufficient_justification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face coercive mandates imposing medical intervention without consent. Options are limited: comply under duress, accept employment/education/public access exclusion, or leave jurisdiction. This reading treats their situation as non-negotiable violation of bodily integrity regardless of pandemic severity or collective benefit claims.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_persons, payer,
    powerless, biographical, trapped, national).

% Experience mandate as forced violation of conscience and religious practice. Exemptions are limited and discretionary; denial of exemptions forces choice between faith observance and mandate compliance. No collective health claim justifies this intrusion, per this reading.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, religious_objectors, payer,
    moderate, biographical, constrained, national).

% Bear the extraction of coerced bodily intervention. Their uncertainty or preference against vaccination is overridden by mandate authority. Exit from employment, education, or civic participation becomes the price of refusing the intervention. Per this reading, hesitation itself is treated as irrelevant—the wrong lies in coercion, not in the hesitation's rationality.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, vaccine_hesitant_populations, payer,
    powerless, biographical, trapped, national).

% Those with legitimate medical contraindications face bureaucratic burden and discretionary denial. Exemption authorities operate with asymmetric power and limited transparency. The reading treats medical exemption gatekeeping as additional extraction layered onto the base coercion.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, medical_exemption_seekers, payer,
    moderate, biographical, constrained, national).

% Design, announce, and enforce the mandate. They claim authority from public health necessity and collective benefit. This reading treats their enforcement as illegitimate regardless of epidemiological justification—they administer coercion they have no legitimate authority to impose.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Would benefit from population immunity via vaccination. This reading explicitly EXCLUDES them from the victim set because it rejects the duty-to-protect-via-bodily-invasion premise: their vulnerability does not justify coercing others' bodies. They are structurally separated from the payer victims by the reading's foundational axiom.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_persons, excluded,
    powerless, biographical, trapped, national).

% A non-agent entity standing in for the collective capacity argument. The reading treats 'protecting healthcare infrastructure' as insufficient justification for bodily coercion, so infrastructure is excluded from beneficiary/victim reasoning.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, healthcare_infrastructure, excluded,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(public_health_mandate_authority__bodily_autonomy_primary, healthcare_infrastructure).

% Document and contest the mandate as rights violation. They testify to courts, mount legal challenges, and articulate the reading's foundational claim. As observers with no direct stake in mandate enforcement, they provide external perspective on the constraint's structure.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, civil_liberties_advocates, observer,
    organized, biographical, analytical, national).

% Endorse the mandate on public health grounds and resist legal challenges. This reading treats them as absent from the structural conversation because their support is precisely what would be rejected if bodily autonomy is truly inviolable—they belong in the mandatrophy analysis omega, not in the stakeholder surface.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, political_coalitions_supporting_mandate, excluded,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No coordination function exists under this reading. The reading denies that collective benefit—whether epidemiological, economic, or infrastructural—can justify non-consensual medical intervention. What public health authorities describe as coordination is reframed as coercion without legitimacy.
% TRANSFER_FUNCTION: Transfers bodily integrity and autonomy FROM unvaccinated and hesitant persons TO the public health system's authority to define acceptable risk and intervene without consent. The transfer is the coercion itself: the extraction of choice and bodily control.
% ABSENT_VOICES: Public health advocates and epidemiologists who endorse the mandate on utilitarian grounds are excluded from this reading's seat map. They would argue that collective protection of vulnerable populations requires vaccination mandates and that individual autonomy is not absolute when contagion creates externalities. That argument is what the reading denies at the foundational level—these voices are structurally outside because their core claim (collective benefit can sometimes override individual bodily autonomy) is what the axioms reject.
% DISAPPEARANCE_RATIONALE: If mandate authority disappeared overnight, unvaccinated populations would face the actual epidemiological risks of non-vaccination and make informed decisions about risk acceptance. Vaccine uptake would likely decline significantly in the absence of coercion. Healthcare systems would adapt to infection patterns without mandate-driven immunity assumptions. The world reorganizes around individual choice and market signals rather than collective command. Per this reading, that reorganization would restore legitimacy even if it reduces collective immunity.
% FOUNDING_PROBLEM: The founding problem, per this reading, is not a pandemic's epidemiological danger but the question of what authority legitimately compels bodily intervention: Can the state—or any collective actor—override individual bodily autonomy in the name of public health? The reading's answer is categorical: no.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and bioethicists outside the public health establishment—including those who support vaccine uptake on individual benefit grounds—attest that bodily autonomy is a live jurisprudential problem. Civil liberties organizations document the constraint structure. Courts in multiple jurisdictions have entertained bodily autonomy claims against mandates, indicating the problem is recognized in law. Notably, NO public health authority attests to the bodily autonomy problem because that authority's legitimacy depends on denying it—corroboration comes from independent constitutional and philosophical analysis, not from beneficiaries of mandate authority.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.89) because the constraint takes bodily control from the payer set without offering alternative or compensation; it operates as pure transfer of bodily sovereignty to public health authority with no coordination benefit (the reading denies any benefit justifies it). Suppression is high (0.78) because mandate enforcement depends on legal penalties, employment exclusion, education access denial, and sustained institutional pressure; resistance remains high (0.81) because victims and advocates continuously contest the mandate's legitimacy—this is not normalized acceptance but active dispute. Theater ratio is moderate-low (0.22): the constraint does have a public health function (disease reduction, if effective), but per this reading that function is irrelevant to legitimacy; the theater consists in the authority's justification narrative (public health) deployed to defend coercion the reading treats as unjustifiable. Accessibility_collapse at 0.72 reflects that alternatives (refusing vaccine and accepting exclusion, seeking medical exemption, leaving jurisdiction) exist but are all costly enough to approach unavoidable compliance for most victims. The measurement series tracks rising extractiveness and suppression from t=0 to t=18, then stabilization at t=24–36: as mandate enforcement matures and normalization sets in, the infrastructure of suppression hardens (suppression_requirement plateaus), but the core extraction measure (bodily control transferred) does not increase further—the initial coercive transfer is complete. Resistance remains elevated throughout because the reading's advocates do not accept mandate legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The public_health_primary reading and this reading (bodily_autonomy_primary) experience the same mandate as fundamentally different constraints. From the public health seat, the mandate is rope: genuine coordination problem (preventing disease spread), participant beneficiaries (those protected), and enforcement justified by collective action. From the bodily autonomy seat, the mandate is snare: coercion without legitimacy, pure extraction of bodily control, beneficiaries only if one accepts that collective protection can override individual sovereignty (which this reading denies). The engine will compute per-seat classifications that diverge dramatically: a public health authority agent would compute rope or tangled_rope, while an unvaccinated victim would compute snare. This divergence is the structural fact the constraint exists to expose—the two readings cannot coexist in a single agent's perceptual frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for unvaccinated persons is near 1.0 (full target): they bear the bodily extraction, have trapped or identity_locked exit, and derive zero benefit from the constraint under this reading (the reading denies collective health benefit justifies coercion). Public health authorities are agenda_setters with institutional power and analytical exit—they administer the constraint and are not subject to its coercion; d near 0.0 if we incorrectly treated them as beneficiaries (but they are not beneficiaries in this reading—they are the enforcement seat, deriving authority and control rather than material rents). No beneficiary group is authored in base_properties because the reading denies any party legitimately benefits. The vindicated_propositions array captures what the reading's operation WOULD vindicate IF coercive authority over bodies were legitimate—but the reading denies that legitimacy from the start. This creates a structural inversion: the constraint operates as if public health protection justifies mandates, but the reading asserts that premise is false, so nothing is actually vindicated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem under this reading is not epidemiological (pandemic danger) but constitutional: what authority can legitimately compel bodily intervention? The reading answers: none. The founding_problem_status is LIVE because the constitutional question remains unresolved across jurisdictions and over time. The disappearance_verdict is WORLD_REARRANGES because mandate authority structure is not a natural law but a constructed coercive arrangement—if it vanished, bodily autonomy would be restored and individuals would make vaccine decisions on merit and risk tolerance, not coercion. This misalignment (live founding problem + rearranges_verdict) is not mandatrophy in the classical sense (function dead but structure persists) but rather structural dispute: one reading's founding problem is another reading's legitimate authority. Mandatrophy resolution here requires the kernel contest (omega_kernel_reading_contest) to be decided—if bodily_autonomy_primary is correct, the mandate is never legitimate and persists by coercive force, not function. If public_health_primary is correct, the mandate persists by its continued function (disease control). The reading-level analysis cannot resolve this; the committer structure (which reading is adopted by which political coalition) determines the mandatrophy verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the public health mandate authority kernel is structurally sound: bodily_autonomy_primary (bodily sovereignty is inviolable), public_health_primary (collective protection duty overrides individual bodily autonomy when externalities are severe), or proportionality_reading (legitimacy is contextual, trading off harm magnitude, alternatives, and duration)?',
    'Constitutional jurisprudence over time, empirical examination of mandate outcomes across jurisdictions with different readings, philosophical analysis of autonomy and collective action. No single empirical fact resolves this; resolution requires normative framing alongside evidence.',
    'If bodily_autonomy_primary forecloses the siblings, then ANY mandate is illegitimate regardless of pandemic severity or vulnerable population protection—the snare classification holds. If proportionality_reading is adopted, the constraint becomes tangled_rope (coordination function + asymmetric extraction + enforcement, all justified by sliding scale). If public_health_primary forecloses bodily_autonomy_primary, then unvaccinated persons are no longer structurally victims—they are failure-to-contribute actors, and the classification shifts to rope or tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which kernel reading is correct—what normative framework governs public health mandate authority?').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.78) primarily structural (legal penalties, employment loss, access exclusion) or internalized (victims internalize the mandate as legitimate, normalization of coercion, social shame of non-compliance)?',
    'Post-mandate period: do unvaccinated/hesitant populations continue to report coercion and autonomy violation after mandate lifted, or do they normalize compliance? Do they seek remedy and recognition, or accept the imposed bodily intervention as necessary? Longitudinal surveys of subjective autonomy experience.',
    'If suppression is primarily structural, exit from the constraint (mandate termination) should substantially reduce reported extraction and autonomy harm. If internalized, the victims carry the suppression forward even absent enforcement, indicating the coercion has been colonized into identity and choice-framing. This would suggest the constraint''s persistence derives partly from cognitive capture, not merely legal enforcement—a deeper classification concern for piton decay or identity_locked exit dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether mandate suppression is structural or internalized in victim populations.').

omega_variable(
    inviolability_vs_harm,
    'Can bodily autonomy be inviolable AND yet limited when the intervention prevents severe quantifiable harm to others—or does inviolability entail absolute prohibition on body-protective-of-others mandates?',
    'Jurisprudential clarification: does this reading permit any coercive health interventions to prevent harm (e.g., quarantine of active tuberculosis patients), or does it categorically prohibit all body-invasive mandates? The boundary between bodily autonomy and public harm prevention is contested within the bodily autonomy tradition itself.',
    'If inviolability is absolute, then even quarantine of infectious disease carriers is illegitimate—the reading extends to pure-isolation coercion as well as vaccination. If inviolability has an exception for preventing active harm transmission, then the reading''s categorical claim (''no collective benefit can justify...'') is undermined; the proportionality_reading''s framing becomes more plausible. This omega clarifies whether the reading is internally coherent or contains an unresolved tension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inviolability_vs_harm, conceptual, 'Whether bodily autonomy inviolability admits any exceptions for harm prevention.').

omega_variable(
    mandate_vs_incentive_distinction,
    'Does this reading''s categorical rejection apply equally to hard mandates (legal requirement with penalties) and soft incentives (vaccine requirement for employment/travel but with refusal option, even if costly)? Where is the line between coercion and choice under constraint?',
    'Doctrinal clarification from legal scholars and bioethicists working in this reading''s framework: do employment exclusions, travel restrictions, and education access denials constitute coercion if the person retains the choice to refuse vaccines and accept the consequence?',
    'If soft coercion counts as violation, the constraint applies to a much wider range of policies and the victim set is larger. If only hard mandates with criminal or forcible medical penalties count, the reading''s scope narrows significantly. This affects the measured extractiveness: softer constraints might score lower on extraction if choice (however costly) is preserved; harder mandates score higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_vs_incentive_distinction, conceptual, 'Whether mandate coercion includes soft incentives/exclusions or only hard legal requirements.').

omega_variable(
    reading_sibling_foreclosure,
    'Does bodily_autonomy_primary FORECLOSE public_health_primary, or do they COEXIST as competing readings held by different parties? Can one framework coherently hold both?',
    'Logical analysis: if the core axiom of bodily_autonomy_primary is that no collective benefit justifies bodily violation, does that logically prevent someone from also holding that collective health protection justifies mandates? Or do different political traditions simply endorse different readings without one ruling out the other?',
    'If foreclosure: the constraint is asymmetrically structured—one side''s truth claim rules out the other''s. If coexist: both readings persist as live political positions, suggesting the kernel contest is not logically settled but politically contested. This affects how mandatrophy analysis and contested_kernel_dynamics work: foreclosure suggests one reading is emergent and the other receding; coexistence suggests ongoing political equilibrium.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure, conceptual, 'Whether bodily autonomy and public health readings of mandate authority logically foreclose one another or coexist as competing live positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(publ_tr_t0, observed).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 6, 0.19).
narrative_ontology:measurement_basis(publ_tr_t6, observed).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(publ_tr_t12, observed).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 18, 0.21).
narrative_ontology:measurement_basis(publ_tr_t18, observed).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(publ_tr_t24, observed).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(publ_tr_t30, observed).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 36, 0.22).
narrative_ontology:measurement_basis(publ_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.81).
narrative_ontology:measurement_basis(publ_be_t0, observed).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 6, 0.84).
narrative_ontology:measurement_basis(publ_be_t6, observed).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 12, 0.86).
narrative_ontology:measurement_basis(publ_be_t12, observed).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 18, 0.88).
narrative_ontology:measurement_basis(publ_be_t18, observed).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 24, 0.89).
narrative_ontology:measurement_basis(publ_be_t24, observed).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 30, 0.89).
narrative_ontology:measurement_basis(publ_be_t30, observed).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 36, 0.89).
narrative_ontology:measurement_basis(publ_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(publ_su_t0, observed).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(publ_su_t6, observed).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 12, 0.73).
narrative_ontology:measurement_basis(publ_su_t12, observed).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 18, 0.76).
narrative_ontology:measurement_basis(publ_su_t18, observed).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 24, 0.78).
narrative_ontology:measurement_basis(publ_su_t24, observed).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(publ_su_t30, observed).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 36, 0.78).
narrative_ontology:measurement_basis(publ_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__bodily_autonomy_primary, 0.05).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% The public_health_mandate_authority kernel decomposes into three structurally distinct constraint stories, each with different ε, beneficiary/victim structures, and classifications. bodily_autonomy_primary (this story) treats mandates as snare—pure coercion without legitimacy. public_health_primary treats them as rope or tangled_rope—coordination with asymmetric extraction justified by collective protection. proportionality_reading treats them as contextual—legitimacy depends on sliding scale analysis. The three stories are linked: each reading competes for dominance of the same kernel, but they cannot be unified into one constraint without losing ε-invariance. Each story's ε is computed relative to its own reading's endorsed referent (the standing mandate arrangement as that reading sees it), not relative to a hypothetical 'correct' arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
