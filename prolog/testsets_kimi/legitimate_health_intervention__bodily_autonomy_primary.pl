% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: Coercive Medical Intervention Mandate Regime (Bodily Autonomy Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story models the coercive public health mandate
 *   regimeâstate-compelled medical intervention enforced through employment
 *   and access leverageâthrough the analytical lens of the
 *   bodily_autonomy_primary reading of the legitimate_health_intervention
 *   kernel. In this reading, legitimacy requires informed consent for all
 *   medical interventions, and state coercion violates bodily integrity
 *   regardless of public benefit. The constraint extracts bodily autonomy
 *   from individuals via threats to livelihood and access, while public
 *   health authorities and institutional enforcers benefit from compliance,
 *   operational continuity, and liability shields. The claim (tangled_rope)
 *   and metrics are independently authored: the metrics describe a heavily
 *   extractive, actively enforced regime whose coordination function (disease
 *   control) is increasingly coupled with performative enforcement and
 *   institutionalized coercion.
 *
 * KEY AGENTS:
 *   - public_health_authority: Primary agenda-setter and enforcement architect (institutional/arbitrage) â benefits from expanded authority and legitimacy
 *   - mandate_coerced_individuals: Primary target (powerless/trapped) â bears bodily integrity violation and economic exclusion
 *   - institutional_enforcers: Secondary beneficiary (organized/constrained) â receives liability protection in exchange for policing labor
 *   - autonomy_advocacy_groups: Excluded voice (moderate/constrained) â would object if included in policy design
 *   - judicial_bodies: Analytical observer (institutional/analytical) â evaluates constitutional challenges but often defers to public health expertise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.7).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.76).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "Coercive Medical Intervention Mandate Regime (Bodily Autonomy Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '2b19b1f7-bd23-46bf-b6d5-9189c4a57c87').
narrative_ontology:cs_kernel_codification('2b19b1f7-bd23-46bf-b6d5-9189c4a57c87', formalized).
narrative_ontology:cs_authority_grounding('2b19b1f7-bd23-46bf-b6d5-9189c4a57c87', lineage).
narrative_ontology:cs_interpretation_layer_present('2b19b1f7-bd23-46bf-b6d5-9189c4a57c87').
narrative_ontology:cs_reading_relation('2b19b1f7-bd23-46bf-b6d5-9189c4a57c87', legitimate_health_intervention__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('2b19b1f7-bd23-46bf-b6d5-9189c4a57c87', legitimate_health_intervention__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('2b19b1f7-bd23-46bf-b6d5-9189c4a57c87', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('2b19b1f7-bd23-46bf-b6d5-9189c4a57c87', bodily_integrity_absolute, deontological).
narrative_ontology:cs_reference_frame('2b19b1f7-bd23-46bf-b6d5-9189c4a57c87', bodily_autonomy_sovereignty).
narrative_ontology:cs_drift_state('2b19b1f7-bd23-46bf-b6d5-9189c4a57c87', post_emergency_mandate_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2b19b1f7-bd23-46bf-b6d5-9189c4a57c87', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, public_health_authority).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, institutional_enforcers).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, hospital_systems).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and promulgates coercive medical intervention mandates, sets exemption criteria, and enforces compliance through licensing, regulatory penalties, and emergency powers. Retains authority to modify or lift mandates but gains institutional legitimacy and budgetary reinforcement from maintaining the enforcement apparatus.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Must undergo unwanted medical intervention or forfeit employment, educational enrollment, and access to public accommodations. Economic dependency on employers and state benefit systems prevents exit; they bear the direct cost of bodily autonomy violation and medical risk acceptance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals, payer,
    powerless, biographical, trapped, national).

% Employers, schools, and venue operators mandated to verify compliance; they receive liability protections and operational continuity guarantees in exchange for policing individuals, transferring enforcement labor and social friction away from the state.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, institutional_enforcers, beneficiary,
    organized, biographical, constrained, national).

% Benefit from reduced patient surges that would overwhelm capacity; structurally support mandate regimes to preserve operational viability and staff availability, though they do not set enforcement policy.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, hospital_systems, beneficiary,
    powerful, biographical, constrained, national).

% Civil liberties and medical ethics organizations arguing for absolute informed consent and against coerced intervention; structurally excluded from emergency policy advisory roles when absolutist autonomy positions are dismissed as non-operational or politically untenable.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, autonomy_advocacy_groups, excluded,
    moderate, generational, constrained, national).

% Courts hearing constitutional and human-rights challenges to mandate regimes; they evaluate whether coercion exceeds proportionality limits or violates bodily integrity rights, but frequently defer to public health expertise during declared emergencies.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, judicial_bodies, observer,
    institutional, generational, analytical, national).

narrative_ontology:fixing_cost_class(legitimate_health_intervention__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents infectious disease transmission by ensuring high compliance rates with preventive medical interventions across workplaces, schools, and public accommodations, reducing population-level morbidity and healthcare system overload.
% TRANSFER_FUNCTION: Transfers bodily autonomy and informed consent authority from individuals to state public health authorities and institutional enforcers; transfers compliance enforcement labor and social policing burden from the state to employers and venue operators.
% ABSENT_VOICES: Absolutist medical ethicists and bodily autonomy advocates who reject all coerced intervention regardless of disease severity; individuals with identity-locked religious or philosophical objections who are denied exemptions under narrow criteria.
% DISAPPEARANCE_RATIONALE: If the coercive mandate framework vanished overnight, workplaces and schools would need to reorganize around voluntary measures or alternative safety protocols; public health authorities would lose a primary enforcement lever; compliance rates and disease dynamics would shift, and the legal relationship between institutions and individuals would require renegotiation.
% FOUNDING_PROBLEM: An outbreak of infectious disease threatening to overwhelm healthcare capacity and cause mass mortality, where voluntary compliance rates were deemed insufficient to achieve population-level protective thresholds.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities attest the problem remains live and mandates remain necessary. Civil liberties organizations and dissenting epidemiologists attest the founding threat has diminished or was overstated, and that the arrangement persists beyond its originating justification. Courts have issued split rulings, providing mixed corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.7, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.70 at interval end) reflects the systematic transfer of bodily autonomy from individuals to state authorities under threat of employment loss and access denial. Suppression (0.76) is high because the constraint's persistence depends on actively penalizing non-compliance and collapsing alternatives such as routine testing or remote accommodation. Theater ratio (0.48) indicates substantial Goodhart drift: enforcement rituals (verification systems, compliance portals) have partly decoupled from marginal infection risk. Accessibility collapse (0.68) captures the institutional disappearance of non-pharmaceutical alternatives once the mandate framework was established. Resistance (0.72) reflects sustained legal challenges, political protest, and civil disobedience. The temporal measurements run on one shared grid so every metric is authored at every examined time point, showing extraction and theater rising through the mandate-intensification phase and modestly softening as enforcement matures into institutionalized ritual.
 *
 * PERSPECTIVAL GAP:
 *   The public health authority seat experiences the constraint as necessary coordination for collective survival against an infectious threat; the mandate-coerced individual seat experiences the identical structure as asymmetric extraction of bodily autonomy. The divergence is structural: the authority controls exit (can declare emergency over, can shift policy) while the individual is trapped by economic dependency and identity-locked objections. The proportionality and public-health-primary readings would reclassify the victim seat as a beneficiary of population-level protection; this reading refuses that framing and insists on the absolute priority of individual bodily integrity.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authority and institutional enforcers are structural beneficiaries: they gain compliance, institutional legitimacy, and operational stability (low d, subsidy from the constraint). Hospital systems benefit from reduced surge but do not drive enforcement (moderate-low d). Mandate-coerced individuals are the targets: they bear direct bodily integrity costs and face employment or access exclusion if they resist (high d, amplified extraction). The engine will compute high effective extraction for the powerless, trapped payer seat and low or negative effective extraction for the institutional agenda-setter seat. No directionality override is needed because beneficiary/victim declarations plus exit options correctly map the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was an emergency infectious disease threat threatening healthcare system collapse. The R5 genealogy records this status as contested: civil liberties organizations and dissenting epidemiologists attest the threat has diminished, while public health authorities maintain it remains live. Temporal measurements show base_extractiveness rising from 0.50 to 0.78 and theater_ratio rising to 0.52, indicating significant Goodhart drift. However, because concentrated beneficiaries (public health authority, institutional enforcers) still actively profit from the constraint's persistence and victims continue to mount substantial resistance (0.72), the constraint has not decayed into a piton. It remains a tangled rope whose coordination function is increasingly subordinated to extractive enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the bodily_autonomy_primary reading describe a structurally distinct constraint, or does it merely offer a normative evaluation of the same mandate regime the other readings describe?',
    'Compare structural fingerprints (beneficiary/victim sets, enforcement mechanisms, directionality profiles) across the three reading-generated constraints; if they share identical structural data, the readings are observer-axis differences rather than kernel decompositions.',
    'If structurally identical, merge into one constraint with a preference omega; if structurally distinct (this reading produces a victim set the public_health reading omits), the decomposition is valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether this reading instantiates a distinct constraint or just a normative framing').

omega_variable(
    enforcement_disease_coupling,
    'Is the enforcement severity of the mandate regime tracking actual disease threat levels, or has it decoupled to serve institutional convenience and inertia?',
    'Time-series comparison of enforcement intensity against contemporaneous epidemiological indicators (hospitalization rates, variant severity, excess mortality).',
    'If decoupled, the coordination function is atrophying and the constraint is drifting toward snare; if coupled, the tangled_rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_disease_coupling, empirical, 'Whether enforcement tracks epidemiological need or institutional inertia').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal penalties, access denial, employment termination) or internalized (social stigma, medical ethics reinterpretation, identity fusion with compliance)?',
    'Post-mandate suppression trajectory: if compliance pressure persists after legal repeal primarily through social sanction and professional ostracism, reclassify suppression as partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because the target carries the suppression mechanism with them after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t6, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 6, 0.28).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 12, 0.38).
narrative_ontology:measurement(legi_tr_t18, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 18, 0.48).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 24, 0.52).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 30, 0.5).
narrative_ontology:measurement(legi_tr_t36, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 36, 0.48).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(legi_be_t6, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(legi_be_t18, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 18, 0.78).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(legi_be_t36, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 36, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t6, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(legi_su_t18, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 24, 0.85).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(legi_su_t36, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 36, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one decomposition of the legitimate_health_intervention kernel. The bodily_autonomy_primary reading identifies the coercive mandate regime as extractive, while the sibling readings instantiate structurally distinct constraints from the same domain. Each story carries its own stable epsilon and stakeholder surface.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
