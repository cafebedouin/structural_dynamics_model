% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: State Vaccination Mandate Authority — Public-Health-Primary Reading
 *   domain: public_health/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the public-health-primary reading of the
 *   mandate_legitimacy_scope kernel: state authority to compel vaccination is
 *   legitimate when necessary to protect vulnerable populations who cannot
 *   protect themselves. Under this reading, the immunocompromised and
 *   medically-ineligible enter the beneficiary set directly, and the absence
 *   of a mandate is itself treated as the harm-producing condition — this
 *   drives extraction upward over the measured interval as the reading's
 *   logic extends mandate scope (narrowing exemptions, adding compelled
 *   settings) in response to perceived herd-immunity erosion. This is ONE of
 *   three sibling readings of the same kernel (bodily_autonomy_primary,
 *   proportionality_reading); this file does not adjudicate between them,
 *   does not average their ε values, and does not describe the contest — the
 *   contest is routed to omega variables and cs_structure per the
 *   committer-frame rules.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.58).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.62).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "State Vaccination Mandate Authority — Public-Health-Primary Reading").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health/constitutional_law").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, 'b8606506-4e53-4827-8138-004dffb7a8f4').
narrative_ontology:cs_kernel_codification('b8606506-4e53-4827-8138-004dffb7a8f4', distributed).
narrative_ontology:cs_authority_grounding('b8606506-4e53-4827-8138-004dffb7a8f4', distributed).
narrative_ontology:cs_reading_relation('b8606506-4e53-4827-8138-004dffb7a8f4', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('b8606506-4e53-4827-8138-004dffb7a8f4', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('b8606506-4e53-4827-8138-004dffb7a8f4', foundational, protective_duty_to_vulnerable_overrides_individual_consent).
narrative_ontology:cs_axiom_status(protective_duty_to_vulnerable_overrides_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('b8606506-4e53-4827-8138-004dffb7a8f4', protective_duty_to_vulnerable_overrides_individual_consent, deontological).
narrative_ontology:cs_axiom('b8606506-4e53-4827-8138-004dffb7a8f4', secondary, mandate_absence_constitutes_harm_producing_condition).
narrative_ontology:cs_axiom_status(mandate_absence_constitutes_harm_producing_condition, holdable).
narrative_ontology:cs_axiom_grounding('b8606506-4e53-4827-8138-004dffb7a8f4', mandate_absence_constitutes_harm_producing_condition, empirically_contingent).
narrative_ontology:cs_reference_frame('b8606506-4e53-4827-8138-004dffb7a8f4', police_power_communicable_disease_doctrine).
narrative_ontology:cs_drift_state('b8606506-4e53-4827-8138-004dffb7a8f4', post_pandemic_mandate_expansion_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b8606506-4e53-4827-8138-004dffb7a8f4', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, infants_too_young_to_vaccinate).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, elderly_care_residents).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, public_health_agencies).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, vaccine_refusers).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, religious_exemption_seekers).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, medically_uncertain_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, vaccine_manufacturers).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, herd_immunity_threshold_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, state_police_power_over_communicable_disease).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets vaccination requirements for school enrollment, employment in healthcare settings, and public gathering access, and enforces them through exclusion, fines, or licensure conditions. Justifies the mandate as necessary to maintain herd immunity thresholds that protect those who cannot be vaccinated. Gains institutional legitimacy and measurable disease-reduction outcomes from compliance.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__public_health_primary, public_health_agencies, beneficiary).

% Cannot receive certain vaccines themselves due to medical contraindication and depend entirely on the vaccination rate of the surrounding population for protection. Absent a mandate, they bear elevated exposure risk they have no personal means to reduce. Have no direct enforcement power; their protection is entirely mediated through the mandate's coercion of others.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, local).

% Below the age threshold for vaccination against certain diseases and rely on cocooning — vaccination of the surrounding adult population — for protection during the most vulnerable window of their lives. Cannot advocate for themselves and have no exit option whatsoever.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, infants_too_young_to_vaccinate, beneficiary,
    powerless, biographical, trapped, local).

% Object to vaccination on grounds ranging from personal risk assessment to distrust of institutions, and bear the direct cost of the mandate: exclusion from school, workplace termination, or civil penalties. Their exit options are relocation to jurisdictions without mandates, home-schooling, or accepting exclusion from public and economic life — all costly and constrained rather than free.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vaccine_refusers, payer,
    moderate, biographical, constrained, national).

% Hold sincere religious objections to vaccination and seek exemption carve-outs that many jurisdictions under this reading narrow or eliminate. They bear the mandate's costs specifically because the public-health-primary framing treats religious exemption as a threat to herd immunity rather than a protected liberty interest.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, religious_exemption_seekers, payer,
    moderate, biographical, constrained, national).

% Have contraindications or histories that make vaccination genuinely uncertain in their individual case but do not meet the narrow, formally recognized medical exemption criteria. They are compelled or excluded under a mandate calibrated to population-level risk that does not account well for their individual circumstance.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, medically_uncertain_individuals, payer,
    powerless, biographical, trapped, local).

% Receive guaranteed, government-compelled demand for their products when mandates are enacted, along with liability protections in many jurisdictions. They do not administer the mandate but benefit structurally from its existence regardless of the underlying public-health calculus.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vaccine_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).

% Adjudicate challenges to mandate scope, testing them against police-power doctrine and constitutional liberty claims. Their rulings determine how far the public-health-primary reading can be pushed before it collides with bodily autonomy claims in a given jurisdiction.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains population-level immunity thresholds so that communicable disease cannot sustain transmission chains, which protects individuals who cannot be vaccinated themselves (immunocompromised, infants, those with genuine contraindications) and prevents outbreak conditions that would harm the whole community.
% TRANSFER_FUNCTION: Moves bodily autonomy and individual risk-decision authority from vaccine-hesitant and religiously-objecting individuals to the state, and moves epidemiological protection from the general vaccinated population to those who cannot vaccinate themselves — a duty-to-protect is imposed on the former for the benefit of the latter.
% ABSENT_VOICES: Vaccine refusers with sincere but non-religious philosophical objections are frequently excluded from the exemption conversation entirely under this reading, since public-health-primary logic treats non-medical exemptions as a threat vector rather than a legitimate liberty claim to be weighed.
% DISAPPEARANCE_RATIONALE: If mandate authority vanished overnight under this reading's premises, vaccination rates in hesitant communities would fall below herd-immunity thresholds, immunocompromised individuals and infants would face materially higher exposure risk, and outbreak events (e.g., measles resurgence) would become substantially more likely in under-vaccinated pockets — the vulnerable populations this reading exists to protect would bear the reordering directly.
% FOUNDING_PROBLEM: Communicable diseases with high transmissibility and serious harm to vulnerable subpopulations cannot be controlled by individual voluntary choice alone, because unvaccinated individuals impose externalities (transmission risk) on people who have no ability to protect themselves through their own vaccination status.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and epidemiologists (arguably interested parties, given institutional stake in mandate authority) attest the problem remains live, citing measles and pertussis resurgence in under-vaccinated regions. Independent corroboration exists in peer-reviewed outbreak-investigation literature published by researchers outside vaccination-policy agencies, though civil-liberties scholars dispute whether the compulsion mechanism — rather than incentive or education-based alternatives — is what the founding problem actually requires.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.32 to 0.58 across the interval as this reading's operative logic pushes mandate scope outward (fewer exemption categories, more compelled settings, harsher exclusion penalties) whenever measured vaccination rates approach herd-immunity thresholds — the reading treats mandate absence itself as the extraction driver on vulnerable populations, which inverts into extraction on refusers as enforcement tightens. Suppression tracks upward similarly (0.40 to 0.62) as enforcement mechanisms (school exclusion, employment conditions, civil penalties) intensify. Theater ratio stays low and rises only modestly (0.10 to 0.20) because the enforcement machinery under this reading is substantially functional — it does track and does move vaccination rates — rather than performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised individuals, infants, and medically vulnerable populations are the structural beneficiaries under this reading — the mandate exists specifically to protect them, and they bear no coercive cost. Vaccine refusers, religious exemption seekers, and medically-uncertain individuals who fall outside recognized exemption categories are the targets: the mandate's authority is exercised against their bodily-autonomy claims, and their exit options (relocation, exclusion from public life) are costly and constrained, not free. Vaccine manufacturers are beneficiaries through guaranteed demand but are not administrators of the constraint — an arbitrage-grade exit position that keeps their effective extraction low despite structural benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (communicable disease externalities harming those who cannot self-protect) remains partially live — outbreak data in under-vaccinated pockets corroborates it independent of public health agencies' own institutional interest. This prevents the constraint from being dismissed as pure mandatrophy. But the reading's tendency to narrow exemptions specifically in response to herd-immunity math, rather than in response to updated risk-benefit or safety data, is exactly the drift the proportionality_reading sibling exists to check — this file does not resolve that tension, it names it via omega and cs_structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unconditional_vs_conditional_protective_duty,
    'Does the state''s authority to protect vulnerable populations from serious harm hold unconditionally (this reading) or only when calibrated against disease severity, vaccine safety, and less restrictive alternatives (proportionality_reading)?',
    'Comparative case law across jurisdictions with differently calibrated mandate regimes, tracking whether unconditional-duty jurisdictions produce better vulnerable-population outcomes without proportionally higher rights-violation costs than proportionality-calibrated jurisdictions.',
    'If unconditional framing produces materially better outcomes with acceptable cost, it strengthens this reading''s structural legitimacy; if proportionality calibration achieves comparable protection with lower extraction on refusers, this reading''s high ε becomes harder to justify as necessary rather than merely sufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unconditional_vs_conditional_protective_duty, conceptual, 'Whether protective duty to vulnerable populations should be treated as unconditional or proportionality-gated.').

omega_variable(
    bodily_autonomy_foreclosure_question,
    'Does the public-health-primary premise (state authority to compel is legitimate when necessary to protect the vulnerable) logically foreclose the bodily-autonomy-primary premise (non-consensual intervention violates bodily integrity regardless of collective benefit), or can both remain live within different institutional frameworks (e.g., different jurisdictions, different courts)?',
    'Constitutional doctrine tracking: do jurisdictions that adopt public-health-primary compulsion doctrines subsequently and formally reject bodily-autonomy-primary claims as a matter of binding precedent, or do the two persist as live, unresolved tensions litigated case by case?',
    'If foreclosure is doctrinally real within a jurisdiction, that jurisdiction''s mandate authority computes cleanly under this reading with no live rights-based counter-claim; if both persist as live claims, every mandate enforcement action carries unresolved legitimacy risk that shows up as elevated resistance and litigation cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bodily_autonomy_foreclosure_question, conceptual, 'Whether this reading''s core premise logically forecloses the bodily-autonomy-primary sibling within a single legal framework, or the two coexist as ongoing doctrinal tension.').

omega_variable(
    exemption_narrowing_drift_direction,
    'Is the observed narrowing of exemption categories under this reading a proportionate response to genuine herd-immunity erosion, or is it drift toward extraction that outpaces the epidemiological justification?',
    'Track exemption-narrowing events against contemporaneous vaccination-rate and outbreak data; a narrowing event uncorrelated with measurable epidemiological deterioration is a drift signal rather than a proportionate response.',
    'If narrowing consistently tracks real epidemiological deterioration, the rising extractiveness measurements are justified coordination cost; if narrowing outpaces the epidemiological record, the rising ε reflects extraction accumulation disguised as public-health necessity — a T17-relevant pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_narrowing_drift_direction, empirical, 'Whether rising mandate scope tracks genuine epidemiological need or represents extraction drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mand_tr_t4, mandate_legitimacy_scope__public_health_primary, theater_ratio, 4, 0.12).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__public_health_primary, theater_ratio, 8, 0.14).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__public_health_primary, theater_ratio, 12, 0.16).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__public_health_primary, theater_ratio, 16, 0.18).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__public_health_primary, theater_ratio, 20, 0.19).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__public_health_primary, theater_ratio, 24, 0.2).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mand_be_t4, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mand_su_t4, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the mandate_legitimacy_scope kernel. bodily_autonomy_primary holds an unconditional consent-based veto that this reading's foundational axiom directly forecloses within a single legal framework (a jurisdiction cannot simultaneously hold that consent is an absolute bar AND that protective duty overrides it). proportionality_reading calibrates legitimacy against severity/safety/alternatives; this reading's unconditional-duty premise exerts downstream pressure on that calibration (raising the floor of what counts as 'necessary') without logically foreclosing it, since a proportionality analysis could still conclude the protective duty is satisfied by lesser measures in specific cases. Each sibling carries its own ε, its own stakeholder set, and its own classification — do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
