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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: Public Health Primary Reading: State Authority to Compel Vaccination
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint represents the 'public_health_primary' reading of the
 *   'mandate_legitimacy_scope' kernel. It asserts that state authority to
 *   compel vaccination is legitimate when necessary to protect vulnerable
 *   populations from serious harm. From this perspective, the absence of
 *   mandates imposes a high cost on the immunocompromised, and unvaccinated
 *   individuals bear a duty to protect the collective. The constraint is
 *   classified as a Tangled Rope because it genuinely coordinates collective
 *   health outcomes (benefiting vulnerable populations and the public health
 *   system) but does so through asymmetric extraction from unvaccinated
 *   individuals and medical autonomy advocates, requiring active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.65).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.75).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "Public Health Primary Reading: State Authority to Compel Vaccination").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, '7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2').
narrative_ontology:cs_kernel_codification('7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2', formalized).
narrative_ontology:cs_authority_grounding('7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2', lineage).
narrative_ontology:cs_interpretation_layer_present('7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2').
narrative_ontology:cs_reading_relation('7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2', foundational, collective_health_priority).
narrative_ontology:cs_axiom_status(collective_health_priority, holdable).
narrative_ontology:cs_axiom_grounding('7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2', collective_health_priority, deontological).
narrative_ontology:cs_axiom('7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2', foundational, duty_to_protect_vulnerable).
narrative_ontology:cs_axiom_status(duty_to_protect_vulnerable, holdable).
narrative_ontology:cs_axiom_grounding('7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2', duty_to_protect_vulnerable, deontological).
narrative_ontology:cs_reference_frame('7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2', public_health_imperative).
narrative_ontology:cs_drift_state('7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2', contemporary_pandemic_response, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7b3ea519-61e2-4dcb-bbbe-cdee0b6536c2', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, public_health_system).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, medical_autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they issue and enforce vaccination mandates, viewing them as a necessary tool to prevent disease spread and protect vulnerable groups. Their legitimacy is grounded in the collective good.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Immunocompromised individuals, infants, and the elderly who cannot be vaccinated or for whom vaccines are less effective. They directly benefit from herd immunity provided by widespread vaccination, as it reduces their risk of severe illness and death.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Individuals who, for various reasons (personal belief, medical contraindication, or skepticism), choose not to vaccinate. They bear the direct cost of mandates through restrictions on travel, employment, or access to public spaces, and are compelled to act against their personal preference.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, local).

% Groups and individuals who prioritize individual bodily integrity and the right to make personal medical decisions without state coercion. They bear the cost of mandates by seeing their core principles overridden and actively resist enforcement through legal and political channels.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, medical_autonomy_advocates, payer,
    organized, generational, constrained, national).

% Benefits from reduced disease burden, fewer hospitalizations, and greater capacity to manage other health crises when vaccination rates are high. Mandates simplify disease control efforts and reduce strain on resources.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, public_health_system, beneficiary,
    institutional, generational, constrained, national).

% Administer vaccines and manage the health consequences of both vaccination and non-vaccination. They operate within the framework set by public health authorities and often face ethical dilemmas balancing individual autonomy with public health directives.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, healthcare_providers, observer,
    organized, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to achieve herd immunity, protecting both vaccinated and unvaccinated individuals from disease, and preventing healthcare system overload. It ensures a baseline level of public health security.
% TRANSFER_FUNCTION: Transfers the burden of disease risk from vulnerable populations to unvaccinated individuals, who are compelled to accept vaccination or face social/economic restrictions. It also transfers decision-making authority from individuals to the state in matters of public health.
% ABSENT_VOICES: Individuals with rare medical conditions that make vaccination genuinely unsafe, or those whose religious beliefs are genuinely incompatible with vaccination, are often marginalized in the public discourse, their specific concerns often subsumed under broader anti-mandate arguments.
% DISAPPEARANCE_RATIONALE: If state authority to compel vaccination vanished, vaccination rates would likely drop, leading to increased outbreaks of vaccine-preventable diseases, greater risk to vulnerable populations, and potential collapse of healthcare systems during epidemics. The social contract around collective health would fundamentally shift.
% FOUNDING_PROBLEM: The problem of highly contagious diseases causing widespread illness, death, and societal disruption, particularly impacting those unable to protect themselves.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations, medical professionals, and epidemiologists universally corroborate that the problem of infectious disease remains live and poses a continuous threat to vulnerable populations and healthcare infrastructure. Historical data on pre-vaccine eras also corroborates the severity of the problem.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.65) is substantial because it compels individuals to undergo medical procedures against their will or face significant social/economic penalties. Suppression (0.75) is high due to the state's coercive power (legal penalties, restrictions on movement/employment) and the limited exit options for those who wish to participate in society. Theater ratio is low (0.1) as the public health justification is largely genuine from this reading's perspective; the enforcement is directly aimed at achieving the stated public health goal, not merely performing it.
 *
 * PERSPECTIVAL GAP:
 *   The state and vulnerable populations perceive this as a necessary, legitimate coordination mechanism, while unvaccinated individuals and autonomy advocates experience it as an oppressive, extractive force. The engine's per-seat classification will reflect this divergence, with beneficiaries computing as Rope-like and targets as Snare-like, even though the overall constraint is a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   State public health authorities and vulnerable populations are clear beneficiaries (d near 0.0), as the constraint directly serves their interests in collective health and personal safety. Unvaccinated individuals and medical autonomy advocates are targets (d near 1.0), bearing the costs of compelled action or restricted freedoms. Healthcare providers are observers, implementing the mandates but not directly benefiting from the extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_duty_scope,
    'To what extent does an individual''s duty to the collective override their bodily autonomy in matters of public health?',
    'Societal consensus through deliberative democracy, or judicial precedent establishing the limits of state power in public health emergencies.',
    'If collective duty is deemed absolute, this reading''s legitimacy is strengthened, potentially increasing its effective extractiveness. If individual autonomy is given more weight, the constraint''s legitimacy weakens, pushing it towards a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_duty_scope, preference, 'The normative boundary between individual rights and collective responsibility.').

omega_variable(
    vulnerability_threshold_definition,
    'How is ''vulnerable population'' defined, and what level of ''serious harm'' is required to trigger legitimate state compulsion?',
    'Epidemiological data, medical consensus on disease severity and population susceptibility, and ethical guidelines for risk assessment.',
    'A broad definition of vulnerability and low harm threshold would expand the scope of legitimate mandates, potentially increasing extraction. A narrow definition would limit mandates, reducing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_threshold_definition, empirical, 'Empirical and ethical criteria for triggering public health mandates.').

omega_variable(
    mandate_legitimacy_framing,
    'Is this constraint a genuine coordination mechanism for public health, or a cover for state overreach and control?',
    'Analysis of the constraint''s persistence after the immediate public health crisis subsides, and comparison with less restrictive alternatives implemented in other jurisdictions.',
    'If it persists beyond necessity, it shifts towards a Snare. If it genuinely dissolves or adapts, it reinforces its coordination function. This omega addresses the core contest between this reading and its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_legitimacy_framing, conceptual, 'The fundamental framing of mandate legitimacy as coordination vs. control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mand_tr_t5, mandate_legitimacy_scope__public_health_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(mand_tr_t10, mandate_legitimacy_scope__public_health_primary, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(mand_be_t5, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(mand_be_t10, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(mand_su_t5, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 5, 0.73).
narrative_ontology:measurement(mand_su_t10, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
