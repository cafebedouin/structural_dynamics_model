% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Proportionality Reading of Health Intervention Legitimacy
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the proportionality reading of the contested
 *   kernel legitimate_health_intervention. It holds that the legitimacy of
 *   public health coercion depends on maintaining proportionality between
 *   intervention severity and disease threat level, with population harm and
 *   individual autonomy both entering the calculus weighted by disease
 *   characteristics (transmissibility, case-fatality rate). The constraint is
 *   actively enforced through public health orders and judicial review,
 *   generating a conditional structure where the victim set and extraction
 *   intensity vary with perceived threat. It is claimed as a coordination
 *   mechanism to prevent both lethal negligence and tyrannical overreach,
 *   while critics note it authorizes substantial autonomy extraction under
 *   the cover of technical balancing.
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda_setter/beneficiary (institutional/constrained) â sets the proportionality calculus and enforces interventions
 *   - individuals_subject_to_intervention: primary payer (powerless/trapped) â bears the autonomy and economic costs of mandated interventions
 *   - epidemiologically_vulnerable_groups: beneficiary (moderate/constrained) â receives protective coordination from reduced transmission
 *   - constitutional_courts: observer (institutional/analytical) â adjudicates whether interventions meet proportionality standards
 *   - civil_liberties_organizations: observer (organized/analytical) â challenges overreach and documents autonomy costs
 *   - marginalized_communities: excluded/payer (powerless/trapped) â disproportionately burdened and absent from the proportionality calculus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.6).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.55).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality Reading of Health Intervention Legitimacy").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '98a5b658-7dfe-46a3-9d70-4741270d93f5').
narrative_ontology:cs_kernel_codification('98a5b658-7dfe-46a3-9d70-4741270d93f5', formalized).
narrative_ontology:cs_authority_grounding('98a5b658-7dfe-46a3-9d70-4741270d93f5', lineage).
narrative_ontology:cs_interpretation_layer_present('98a5b658-7dfe-46a3-9d70-4741270d93f5').
narrative_ontology:cs_reading_relation('98a5b658-7dfe-46a3-9d70-4741270d93f5', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('98a5b658-7dfe-46a3-9d70-4741270d93f5', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('98a5b658-7dfe-46a3-9d70-4741270d93f5', foundational, proportionality_as_legitimacy_condition).
narrative_ontology:cs_axiom_status(proportionality_as_legitimacy_condition, holdable).
narrative_ontology:cs_axiom_grounding('98a5b658-7dfe-46a3-9d70-4741270d93f5', proportionality_as_legitimacy_condition, conventional).
narrative_ontology:cs_axiom('98a5b658-7dfe-46a3-9d70-4741270d93f5', foundational, threat_severity_modulates_coercion_limit).
narrative_ontology:cs_axiom_status(threat_severity_modulates_coercion_limit, holdable).
narrative_ontology:cs_axiom_grounding('98a5b658-7dfe-46a3-9d70-4741270d93f5', threat_severity_modulates_coercion_limit, instrumental).
narrative_ontology:cs_reference_frame('98a5b658-7dfe-46a3-9d70-4741270d93f5', constitutional_proportionality_doctrine).
narrative_ontology:cs_drift_state('98a5b658-7dfe-46a3-9d70-4741270d93f5', post_covid_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('98a5b658-7dfe-46a3-9d70-4741270d93f5', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, epidemiologically_vulnerable_groups).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, individuals_subject_to_intervention).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, marginalized_communities).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, constitutional_proportionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the proportionality calculus by determining disease severity metrics and matching intervention tiers. They issue orders under legal frameworks requiring proportionality justification. Their legitimacy depends on adherence to science and law rather than raw power.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, public_health_authorities, beneficiary).

% Required to submit to quarantine, vaccination, or behavioral restrictions when authorities judge the disease threat sufficient. They bear the physical, psychological, and economic costs of the intervention. Exit incurs legal penalties or social exclusion.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, individuals_subject_to_intervention, payer,
    powerless, immediate, trapped, local).

% Benefit disproportionately from population-level interventions that reduce transmission. They depend on the proportionality framework to justify protective measures that restrict others' behavior. They cannot easily exit the epidemiological risk landscape.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, epidemiologically_vulnerable_groups, beneficiary,
    moderate, biographical, constrained, national).

% Review whether public health interventions meet proportionality standards. They interpret the balance between individual rights and state police power. Their rulings calibrate the constraint's enforcement boundaries.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Challenge disproportionate interventions in court and public discourse. They argue that proportionality review has become a rubber stamp during emergencies. They can exit the debate but remain committed to defending autonomy.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, civil_liberties_organizations, observer,
    organized, generational, analytical, national).

% Bear disproportionate intervention burdens and enforcement exposure while being underrepresented in the proportionality calculus that sets threat thresholds. Their voices are rarely at the table when severity is weighed against autonomy.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, marginalized_communities, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decision framework for legitimizing public health interventions when individual liberty and population welfare conflict, enabling coordinated epidemic response without defaulting to pure coercion or pure voluntarism.
% TRANSFER_FUNCTION: Moves autonomy and bodily decision rights from individuals to public health authorities when the authority judges the disease threat severe enough to warrant the intervention severity; transfers legitimacy from constitutional doctrine to the specific intervention order.
% ABSENT_VOICES: Marginalized communities bearing disproportionate enforcement burdens; individuals with rare adverse-event profiles swept into broad mandates; future generations who inherit the precedent for state bodily control.
% DISAPPEARANCE_RATIONALE: Courts would lose the standard of review for public health orders; authorities would default to unconstrained police power or face legitimacy collapse; vulnerable populations would lose protective coordination; the boundary between tyrannical overreach and lethal negligence would become ungovernable.
% FOUNDING_PROBLEM: How to legitimately exercise state coercive power over individuals during epidemics without collapsing into either tyrannical overreach or lethal negligence.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts and international human rights tribunals attest to the need for limiting principles from outside the public health beneficiary seat; historians of the 1918 influenza and HIV/AIDS policy crises corroborate the coordination failure. Civil liberties organizations attest the problem has been repurposed as cover for permanent emergency infrastructure.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60) is substantial because the constraint authorizes overriding individual autonomy, and the payer bears real costs regardless of proportionality review. It is not higher because the proportionality requirement genuinely limits severity in low-threat contexts. Suppression (0.55) reflects the active enforcement of public health orders backed by legal penalties, tempered by judicial review. Theater ratio (0.32) captures the performative 'following the science' rhetoric that sometimes obscures discretionary political judgments. Accessibility collapse (0.40) is moderate: alternatives (pure consent, pure utilitarianism) are intellectually available and politically advocated. Resistance (0.60) is significant: anti-mandate movements, legal challenges, and civil disobedience indicate active opposition. The temporal series show extraction and theater spiking during acute pandemic phases (time points 4-6) before partial regression, indicating conditional enforcement rather than steady-state operation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (public health authorities) experiences the constraint as a necessary legitimacy framework that prevents both underreaction and overreach. The payer seats (individuals subjected to intervention) experience the same structure as conditional but real extraction of autonomy â the proportionality calculus is run by the authority, not the individual. Marginalized communities experience the constraint as invariant extraction regardless of disease severity because enforcement concentrates in their neighborhoods. The engine computes this divergence from the structural data: same constraint, opposed directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities sit near the beneficiary end (low d): the constraint subsidizes their legitimacy and expands their powers within a legal framework. Epidemiologically vulnerable groups sit near the beneficiary end (low-to-mid d): they receive protective coordination. Individuals subjected to intervention sit near the target end (high d): the constraint extracts autonomy and bodily liberty, with severity modulated by disease characteristics but still extracting. Marginalized communities sit at high d with trapped exit, amplifying effective extraction. Constitutional courts and civil liberties organizations sit at analytical exit with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading prevents mislabeling pure public health extraction as coordination by requiring severity-tracking, and prevents mislabeling pure autonomy absolutism as viable coordination by admitting population harm as a legitimate weight. However, if the proportionality calculus is captured by the public_health_primary reading â always weighting population harm heavily â the constraint decays toward snare. Mandatrophy would be detected by a dead founding_problem_status coupled with a world_rearranges disappearance verdict, signaling that the arrangement persists despite the original problem's resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_proportionality,
    'Does the proportionality reading produce a structurally different constraint from public_health_primary, or do they converge to the same extraction profile in high-severity outbreaks?',
    'Cross-jurisdictional comparison of intervention intensity during equivalent outbreaks under different doctrinal regimes.',
    'If they converge, the proportionality reading is not epsilon-invariant and should merge with public_health_primary as a distinct story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_proportionality, conceptual, 'Whether proportionality maintains independent structural identity from pure public health framing').

omega_variable(
    victim_set_variability,
    'Does the victim set vary systematically with disease severity as the proportionality reading claims, or do marginalized communities bear extraction regardless of severity?',
    'Disaggregated compliance and enforcement data by disease type and demographic.',
    'If victims are invariant to severity, the proportionality calculus is theatrical and the constraint operates as a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_variability, empirical, 'Whether victim set tracks disease severity or remains statically concentrated').

omega_variable(
    autonomy_weight_suppression,
    'Is the disagreement with bodily_autonomy_primary resolved by the proportionality framework, or does it persist as unacknowledged structural suppression?',
    'Examine legal dissent rates and civil disobedience across jurisdictions adopting proportionality versus absolute consent frameworks.',
    'If dissent persists as suppressed resistance, the proportionality framework''s coordination function is overstated and its suppression metric is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_weight_suppression, empirical, 'Whether proportionality absorbs or suppresses autonomy-based objections').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__proportionality_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(legi_tr_t2, legitimate_health_intervention__proportionality_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(legi_tr_t4, legitimate_health_intervention__proportionality_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(legi_tr_t6, legitimate_health_intervention__proportionality_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement(legi_tr_t8, legitimate_health_intervention__proportionality_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(legi_tr_t10, legitimate_health_intervention__proportionality_reading, theater_ratio, 10, 0.32).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t2, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(legi_be_t4, legitimate_health_intervention__proportionality_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(legi_be_t6, legitimate_health_intervention__proportionality_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(legi_be_t8, legitimate_health_intervention__proportionality_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__proportionality_reading, base_extractiveness, 10, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(legi_su_t2, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2, 0.42).
narrative_ontology:measurement(legi_su_t4, legitimate_health_intervention__proportionality_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(legi_su_t6, legitimate_health_intervention__proportionality_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(legi_su_t8, legitimate_health_intervention__proportionality_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__proportionality_reading, suppression_requirement, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is the proportionality reading of the legitimate_health_intervention kernel, decomposed from the colloquial label 'legitimate health intervention' per the epsilon-invariance principle. Sibling readings public_health_primary and bodily_autonomy_primary instantiate structurally distinct constraints from the same kernel. The epsilon values differ: proportionality scales extraction with disease severity, public_health_primary holds higher baseline extraction, and bodily_autonomy_primary holds negligible extraction but sacrifices coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
