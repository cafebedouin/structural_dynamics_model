% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate (Public Health Primary Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'public health primary' reading of public
 *   health mandates, where the obligation to protect the vulnerable commons
 *   (immunocompromised individuals, healthcare infrastructure) via collective
 *   action is paramount. It frames mandates as a necessary, albeit sometimes
 *   coercive, mechanism for collective well-being. The immunocompromised
 *   enter the victim set when mandates fail, while the unvaccinated are
 *   excluded from the victim set, framed as free-riders imposing an
 *   externality. This reading acknowledges high extractiveness on
 *   mandate-resistant individuals through coercion via employment or service
 *   loss.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.7).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.8).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate (Public Health Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, '4e2f2184-9eb7-4e7c-b204-c15b19220e87').
narrative_ontology:cs_kernel_codification('4e2f2184-9eb7-4e7c-b204-c15b19220e87', formalized).
narrative_ontology:cs_authority_grounding('4e2f2184-9eb7-4e7c-b204-c15b19220e87', lineage).
narrative_ontology:cs_interpretation_layer_present('4e2f2184-9eb7-4e7c-b204-c15b19220e87').
narrative_ontology:cs_reading_relation('4e2f2184-9eb7-4e7c-b204-c15b19220e87', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('4e2f2184-9eb7-4e7c-b204-c15b19220e87', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('4e2f2184-9eb7-4e7c-b204-c15b19220e87', foundational, collective_health_supersedes_individual_autonomy_in_epidemics).
narrative_ontology:cs_axiom_status(collective_health_supersedes_individual_autonomy_in_epidemics, holdable).
narrative_ontology:cs_axiom_grounding('4e2f2184-9eb7-4e7c-b204-c15b19220e87', collective_health_supersedes_individual_autonomy_in_epidemics, deontological).
narrative_ontology:cs_axiom('4e2f2184-9eb7-4e7c-b204-c15b19220e87', foundational, vulnerable_populations_require_active_collective_protection).
narrative_ontology:cs_axiom_status(vulnerable_populations_require_active_collective_protection, holdable).
narrative_ontology:cs_axiom_grounding('4e2f2184-9eb7-4e7c-b204-c15b19220e87', vulnerable_populations_require_active_collective_protection, deontological).
narrative_ontology:cs_reference_frame('4e2f2184-9eb7-4e7c-b204-c15b19220e87', public_health_emergency_powers_doctrine).
narrative_ontology:cs_drift_state('4e2f2184-9eb7-4e7c-b204-c15b19220e87', contemporary_political_polarization, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('4e2f2184-9eb7-4e7c-b204-c15b19220e87', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_systems).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, general_public).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, businesses_facing_compliance_costs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for setting and enforcing public health mandates, justifying them as necessary to protect the collective good and vulnerable populations. They bear the political cost of enforcement but benefit from a healthier populace and reduced strain on healthcare systems.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from mandates that reduce pathogen transmission, as their health and lives are disproportionately at risk. They have no direct power over mandate policy but are the primary intended beneficiaries of the protection.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, local).

% Benefit from reduced patient load during epidemics, preventing system collapse and allowing continued care for other conditions. They advocate for mandates but also bear the cost of implementing and enforcing them within their facilities.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_systems, beneficiary,
    institutional, generational, constrained, national).

% Benefits from reduced disease prevalence, allowing for more normal social and economic activity. They may experience minor inconveniences from mandates but generally support measures that protect collective well-being.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, general_public, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of compliance (e.g., vaccination, masking) or face exclusion from employment, services, or public spaces. They perceive mandates as an infringement on personal liberty and resist enforcement.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals, payer,
    moderate, immediate, constrained, local).

% Incur costs for implementing and enforcing mandates (e.g., checking vaccine status, providing PPE). They may lose customers who resist mandates but also benefit from a safer environment for their employees and patrons.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, businesses_facing_compliance_costs, payer,
    organized, immediate, constrained, local).

% Argue that mandates infringe on fundamental rights and that less restrictive alternatives should be prioritized. While they participate in legal challenges, their core arguments are often sidelined in the public health primary framing.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to reduce pathogen transmission, protecting shared public health resources and vulnerable individuals who cannot protect themselves.
% TRANSFER_FUNCTION: Transfers individual autonomy (e.g., choice over medical interventions, access to public spaces) from mandate-resistant individuals to the collective good of public health and vulnerable populations.
% ABSENT_VOICES: Those who prioritize individual bodily autonomy above collective health outcomes are often excluded from the core decision-making process, framed as free-riders or threats to the commons rather than legitimate stakeholders with competing values.
% DISAPPEARANCE_RATIONALE: If public health mandates and their enforcement vanished during an epidemic, pathogen transmission would increase, overwhelming healthcare systems and disproportionately harming vulnerable populations. Society would be forced to rearrange around widespread illness and death, or implement new, potentially more coercive, measures.
% FOUNDING_PROBLEM: The problem of communicable diseases threatening collective well-being, overwhelming healthcare capacity, and disproportionately harming vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Medical professionals, epidemiologists, and public health organizations universally corroborate that the problem of communicable disease and its threat to public health remains live, citing ongoing outbreaks and the potential for new pandemics. This corroboration comes from outside the direct beneficiaries of mandate enforcement.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because mandates impose significant costs on those who resist, including loss of employment or access to services. Suppression (0.8) is also high, reflecting the active enforcement required to ensure compliance and the limited exit options for those who wish to participate in society. Theater ratio is low (0.1) as the mandates are generally seen as directly functional in achieving public health goals, with little performative overhead. Accessibility collapse is moderate (0.6) as alternatives to mandates (e.g., voluntary measures) are often deemed insufficient by this reading, but not entirely absent. Resistance is high (0.75) due to strong opposition from those who prioritize individual autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and vulnerable populations, the mandate is a necessary coordination mechanism. From the perspective of mandate-resistant individuals, it is a coercive extraction. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, immunocompromised individuals, healthcare systems, and the general public are beneficiaries, as they gain protection and stability. Mandate-resistant individuals and businesses facing compliance costs are payers, bearing the direct burdens. Civil liberties advocates are excluded, as their arguments are not central to this reading's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_efficacy_ambiguity,
    'What is the actual, measurable efficacy of specific mandates (e.g., mask mandates, vaccine passports) in reducing transmission and protecting vulnerable populations, independent of political will?',
    'Large-scale, peer-reviewed epidemiological studies comparing outcomes in jurisdictions with and without mandates, controlling for confounding factors.',
    'If efficacy is lower than assumed, the justification for high extractiveness and suppression weakens, potentially reclassifying the constraint towards a Snare or Piton. If efficacy is higher, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_efficacy_ambiguity, empirical, 'Uncertainty regarding the direct impact of mandates on public health outcomes.').

omega_variable(
    alternative_measures_sufficiency,
    'Are less coercive alternative measures (e.g., voluntary guidelines, improved ventilation, targeted protections) sufficient to achieve comparable public health outcomes without mandates?',
    'Comparative policy analysis across different jurisdictions and longitudinal studies on the effectiveness of non-mandate interventions.',
    'If alternatives are found sufficient, the necessity of mandates (and thus their justification as a coordination mechanism) is undermined, shifting the classification towards a Snare. If insufficient, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_measures_sufficiency, empirical, 'Whether less coercive alternatives could achieve similar public health goals.').

omega_variable(
    framing_of_unvaccinated_as_victim_or_payer,
    'Is the unvaccinated individual primarily a victim of coercive mandates (as per bodily_autonomy_primary reading) or a payer/free-rider imposing externalities (as per public_health_primary reading)?',
    'Conceptual analysis of ethical frameworks regarding individual rights vs. collective duties, and empirical data on the actual burden imposed by non-compliance.',
    'If framed as a victim, the extractiveness of the mandate is re-evaluated as unjust, potentially shifting the constraint towards a Snare. If framed as a payer/free-rider, the current classification as Tangled Rope is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_unvaccinated_as_victim_or_payer, conceptual, 'Ambiguity in the ethical framing of mandate-resistant individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.05).
narrative_ontology:measurement(publ_tr_t5, public_health_mandate_authority__public_health_primary, theater_ratio, 5, 0.08).
narrative_ontology:measurement(publ_tr_t10, public_health_mandate_authority__public_health_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(publ_tr_t15, public_health_mandate_authority__public_health_primary, theater_ratio, 15, 0.09).
narrative_ontology:measurement(publ_tr_t20, public_health_mandate_authority__public_health_primary, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(publ_be_t5, public_health_mandate_authority__public_health_primary, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(publ_be_t10, public_health_mandate_authority__public_health_primary, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(publ_be_t15, public_health_mandate_authority__public_health_primary, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(publ_be_t20, public_health_mandate_authority__public_health_primary, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(publ_su_t5, public_health_mandate_authority__public_health_primary, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(publ_su_t10, public_health_mandate_authority__public_health_primary, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(publ_su_t15, public_health_mandate_authority__public_health_primary, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(publ_su_t20, public_health_mandate_authority__public_health_primary, suppression_requirement, 20, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, healthcare_resource_allocation).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, economic_activity_regulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
