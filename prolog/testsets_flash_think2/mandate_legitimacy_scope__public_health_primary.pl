% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: State Authority to Compel Vaccination (Public Health Primary Reading)
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'public_health_primary' reading of
 *   the 'mandate_legitimacy_scope' kernel. It asserts that state authority to
 *   compel vaccination is legitimate when necessary to protect vulnerable
 *   populations from serious harm. From this perspective, the constraint
 *   functions as a Tangled Rope, coordinating collective health outcomes
 *   while imposing a justified, though significant, extraction on
 *   unvaccinated individuals. The metrics reflect the burden of compulsion
 *   and the active enforcement required, but the claimed type reflects the
 *   reading's internal justification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.6).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.78).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.6).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "State Authority to Compel Vaccination (Public Health Primary Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, 'cab13fa4-0819-483a-99f4-caf2d08a6e62').
narrative_ontology:cs_kernel_codification('cab13fa4-0819-483a-99f4-caf2d08a6e62', formalized).
narrative_ontology:cs_authority_grounding('cab13fa4-0819-483a-99f4-caf2d08a6e62', lineage).
narrative_ontology:cs_interpretation_layer_present('cab13fa4-0819-483a-99f4-caf2d08a6e62').
narrative_ontology:cs_reading_relation('cab13fa4-0819-483a-99f4-caf2d08a6e62', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('cab13fa4-0819-483a-99f4-caf2d08a6e62', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('cab13fa4-0819-483a-99f4-caf2d08a6e62', foundational, collective_health_priority).
narrative_ontology:cs_axiom_status(collective_health_priority, holdable).
narrative_ontology:cs_axiom_grounding('cab13fa4-0819-483a-99f4-caf2d08a6e62', collective_health_priority, deontological).
narrative_ontology:cs_axiom('cab13fa4-0819-483a-99f4-caf2d08a6e62', foundational, individual_duty_to_community).
narrative_ontology:cs_axiom_status(individual_duty_to_community, holdable).
narrative_ontology:cs_axiom_grounding('cab13fa4-0819-483a-99f4-caf2d08a6e62', individual_duty_to_community, deontological).
narrative_ontology:cs_reference_frame('cab13fa4-0819-483a-99f4-caf2d08a6e62', public_health_imperative).
narrative_ontology:cs_drift_state('cab13fa4-0819-483a-99f4-caf2d08a6e62', contemporary_anti_mandate_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('cab13fa4-0819-483a-99f4-caf2d08a6e62', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, public_health_system).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, healthcare_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they invoke and enforce vaccination mandates based on epidemiological evidence and legal precedent. From this reading, their authority is legitimate and necessary for collective well-being.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Individuals who cannot be vaccinated (e.g., infants, immunocompromised) or for whom vaccines are less effective. They are directly protected from serious harm by high community vaccination rates, which this authority aims to secure.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Individuals who are compelled to vaccinate against their will or face restrictions. From this reading, they bear a duty to contribute to collective immunity, and the mandate is a legitimate imposition for the greater good.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals, payer,
    moderate, immediate, constrained, local).

% Administer vaccines and advise on public health measures. They benefit from a healthier population and reduced burden on healthcare systems. They are also agents of enforcement for mandates.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, healthcare_providers, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__public_health_primary, healthcare_providers, beneficiary).

% Argue for individual bodily autonomy and against state compulsion, even for public health. From this reading's perspective, their concerns are secondary to the collective duty to protect the vulnerable, and their arguments are foreclosed by the necessity of the mandate.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a collective action mechanism to achieve high population immunity, preventing the spread of serious infectious diseases and protecting those most vulnerable to severe outcomes.
% TRANSFER_FUNCTION: Transfers the burden of disease risk from vulnerable populations to unvaccinated individuals (by compelling vaccination), and transfers individual autonomy to the state for public health purposes.
% ABSENT_VOICES: Advocates for absolute bodily autonomy are structurally excluded from the core premise of this reading, which prioritizes collective health over individual choice when serious harm to the vulnerable is at stake.
% DISAPPEARANCE_RATIONALE: If the state's authority to compel vaccination vanished, community immunity would decline, leading to increased outbreaks, severe illness, and death among vulnerable populations, fundamentally altering public health outcomes and societal trust in collective action.
% FOUNDING_PROBLEM: The historical and ongoing challenge of controlling infectious diseases that pose a serious threat to public health, especially to those who cannot protect themselves.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists, public health organizations, and medical professionals consistently corroborate the live status of this problem, citing ongoing threats from vaccine-preventable diseases and the emergence of new pathogens.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.60) reflects the significant imposition on individual autonomy, but is not at a 'snare' level because this reading frames it as a legitimate and necessary cost for a greater good. Suppression (0.78) is high due to the coercive nature of mandates and the active enforcement required. Theater ratio (0.15) is low, as the function is genuinely about public health, not performance. Accessibility collapse (0.82) is high for unvaccinated individuals, as the option to remain unvaccinated without consequence is largely removed. Resistance (0.65) is moderate-high, reflecting ongoing public debate and opposition to mandates.
 *
 * PERSPECTIVAL GAP:
 *   The 'public_health_primary' reading frames the constraint as a necessary coordination mechanism, while other readings (e.g., 'bodily_autonomy_primary') would frame it as pure extraction or a snare. The engine's computation of per-seat classification will highlight this divergence, particularly for the 'unvaccinated_individuals' and 'civil_liberties_advocates' seats.
 *
 * DIRECTIONALITY LOGIC:
 *   State public health authorities and healthcare providers are agenda-setters and beneficiaries, as they implement and benefit from a healthier population. Vulnerable populations are primary beneficiaries, directly protected by the constraint. Unvaccinated individuals are the payers, bearing the direct cost of compulsion. Civil liberties advocates are excluded, as their core premise of absolute bodily autonomy is not prioritized by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_and_harm_definition,
    'What constitutes ''necessary'' and ''serious harm'' in practice, and who adjudicates these definitions?',
    'Analysis of judicial rulings, public health guidelines, and legislative debates across different jurisdictions. Empirical data on disease severity and vaccine efficacy.',
    'If definitions are broad or loosely applied, the constraint''s effective extractiveness and suppression increase, potentially shifting its classification towards a Snare. If definitions are narrow and rigorously applied, the ''tangled_rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_and_harm_definition, empirical, 'Ambiguity in the conditions for legitimate state compulsion.').

omega_variable(
    reading_bodily_autonomy_primary_impact,
    'How would the ''bodily_autonomy_primary'' reading structurally alter the constraint?',
    'Analysis of legal frameworks and policy proposals grounded in absolute bodily autonomy. Counterfactual modeling of public health outcomes under such frameworks.',
    'The ''bodily_autonomy_primary'' reading would eliminate the state''s authority to compel, shifting the burden of disease risk entirely to vulnerable populations (making them victims of the absence of the constraint) and reclassifying the current constraint as a Snare due to its coercive nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_bodily_autonomy_primary_impact, conceptual, 'Impact of the bodily_autonomy_primary reading on constraint structure.').

omega_variable(
    reading_proportionality_impact,
    'How would the ''proportionality_reading'' structurally alter the constraint?',
    'Analysis of legal frameworks and policy proposals that incorporate strict proportionality tests for public health interventions. Comparison of outcomes in jurisdictions with such tests.',
    'The ''proportionality_reading'' would introduce additional gates and conditions (e.g., least restrictive means, severity thresholds) that could reduce the constraint''s extractiveness and suppression by limiting its scope and duration, potentially shifting it towards a Rope or a more narrowly defined Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_proportionality_impact, conceptual, 'Impact of the proportionality_reading on constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 1900, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t1900, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(mand_tr_t1930, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1930, 0.12).
narrative_ontology:measurement(mand_tr_t1960, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(mand_tr_t1990, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(mand_tr_t2010, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(mand_tr_t2030, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2030, 0.15).

% Extraction over time
narrative_ontology:measurement(mand_be_t1900, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(mand_be_t1930, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1930, 0.55).
narrative_ontology:measurement(mand_be_t1960, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement(mand_be_t1990, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1990, 0.57).
narrative_ontology:measurement(mand_be_t2010, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2010, 0.59).
narrative_ontology:measurement(mand_be_t2030, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2030, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t1900, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(mand_su_t1930, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1930, 0.72).
narrative_ontology:measurement(mand_su_t1960, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1960, 0.75).
narrative_ontology:measurement(mand_su_t1990, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(mand_su_t2010, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(mand_su_t2030, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2030, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
