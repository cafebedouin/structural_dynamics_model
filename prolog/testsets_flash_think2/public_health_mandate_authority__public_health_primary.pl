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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate (Public Health Primary Reading)
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'public_health_primary' reading of the
 *   'public_health_mandate_authority' kernel. It frames public health
 *   mandates as a necessary obligation to protect the vulnerable commons
 *   (immunocompromised individuals, healthcare infrastructure) through
 *   collective action. This reading prioritizes collective well-being and
 *   frames resistance as an externality imposed on the system. The high
 *   extractiveness and suppression reflect the coercive nature of mandates
 *   when met with resistance, leading to a 'tangled_rope' classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.75).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.8).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.75).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate (Public Health Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, '3de94591-e19f-4a9c-98e9-07cd0889ea39').
narrative_ontology:cs_kernel_codification('3de94591-e19f-4a9c-98e9-07cd0889ea39', formalized).
narrative_ontology:cs_authority_grounding('3de94591-e19f-4a9c-98e9-07cd0889ea39', expertise).
narrative_ontology:cs_interpretation_layer_present('3de94591-e19f-4a9c-98e9-07cd0889ea39').
narrative_ontology:cs_reading_relation('3de94591-e19f-4a9c-98e9-07cd0889ea39', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('3de94591-e19f-4a9c-98e9-07cd0889ea39', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('3de94591-e19f-4a9c-98e9-07cd0889ea39', foundational, collective_health_priority).
narrative_ontology:cs_axiom_status(collective_health_priority, holdable).
narrative_ontology:cs_axiom_grounding('3de94591-e19f-4a9c-98e9-07cd0889ea39', collective_health_priority, deontological).
narrative_ontology:cs_axiom('3de94591-e19f-4a9c-98e9-07cd0889ea39', foundational, vulnerable_protection_obligation).
narrative_ontology:cs_axiom_status(vulnerable_protection_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3de94591-e19f-4a9c-98e9-07cd0889ea39', vulnerable_protection_obligation, deontological).
narrative_ontology:cs_reference_frame('3de94591-e19f-4a9c-98e9-07cd0889ea39', public_health_emergency_response_framework).
narrative_ontology:cs_drift_state('3de94591-e19f-4a9c-98e9-07cd0889ea39', contemporary_pandemic_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3de94591-e19f-4a9c-98e9-07cd0889ea39', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_systems).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, general_public).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, unvaccinated_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for setting and enforcing public health policies, including mandates, based on scientific consensus. They frame mandates as essential for collective well-being and protection of vulnerable populations.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Directly benefit from reduced pathogen transmission due to mandates, as their health is severely threatened by common infections. They have limited options for self-protection in a high-transmission environment.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, immediate, trapped, local).

% Benefit from mandates by preventing overwhelming surges in patient numbers, preserving capacity for critical care. Without mandates, they face collapse and moral injury to staff.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_systems, beneficiary,
    institutional, biographical, constrained, national).

% Benefits from a healthier society, reduced risk of infection, and stable healthcare access. They bear some inconvenience but generally accept mandates for collective good.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, general_public, beneficiary,
    moderate, biographical, mobile, national).

% Bear the costs of mandates through required compliance (e.g., vaccination, masking) or face penalties such as job loss, exclusion from services, or social stigma. They perceive mandates as infringements on personal liberty.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals, payer,
    moderate, immediate, constrained, national).

% Specifically targeted by vaccine mandates, facing significant social and economic pressure to comply. From this reading's perspective, they are free-riders imposing an externality on the collective, and their resistance is a cost to public health.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, unvaccinated_individuals, payer,
    moderate, immediate, identity_locked, national).

% Monitor and challenge public health mandates on grounds of individual liberty and constitutional rights. They analyze the legal and ethical implications, often representing mandate-resistant individuals.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, constitutional_rights_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To achieve collective immunity and reduce pathogen transmission, thereby protecting vulnerable populations and ensuring the functional capacity of healthcare infrastructure during public health crises.
% TRANSFER_FUNCTION: Transfers the burden of risk from vulnerable populations and the healthcare system to individuals by requiring adherence to public health measures, often enforced through social, economic, or legal penalties.
% ABSENT_VOICES: Those who prioritize individual bodily autonomy above collective health, or those who believe the mandates are disproportionate to the threat, are often marginalized in policy discussions, their concerns framed as selfish or misinformed by the public health primary perspective.
% DISAPPEARANCE_RATIONALE: If public health mandates and their enforcement vanished overnight, pathogen transmission would increase, vulnerable populations would face higher risks, and healthcare systems could be overwhelmed, leading to a significant reorganization of social and economic life around managing widespread illness and its consequences.
% FOUNDING_PROBLEM: Preventing widespread disease, protecting vulnerable populations, and ensuring the functional capacity of healthcare systems during epidemics and pandemics.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations, medical professionals, and epidemiologists consistently attest to the ongoing need for collective action to manage infectious diseases, citing scientific evidence and historical precedent. This corroboration comes from outside the immediate beneficiaries of mandate enforcement.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.75) because mandates impose significant costs on non-compliant individuals, often through loss of employment or access to services. Suppression is also high (0.80) as the constraint's persistence relies on active enforcement and the suppression of alternatives to compliance. Theater ratio is low (0.10) because the mandates are directly functional in their aim to reduce transmission and protect public health, with little performative maintenance. Resistance is moderate (0.60) due to significant opposition from those prioritizing individual autonomy. Accessibility collapse is moderate-high (0.70) as alternatives to compliance are severely limited for those wishing to participate in society.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and vulnerable populations, the mandate is a necessary coordination mechanism. From the perspective of mandate-resistant individuals, it is an extractive and suppressive imposition on their bodily autonomy. The engine's classification will reflect this divergence, computing a beneficial outcome for some seats and a highly extractive one for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, immunocompromised individuals, healthcare systems, and the general public are beneficiaries, as they gain from reduced disease burden and preserved system capacity. Mandate-resistant and unvaccinated individuals are targets (payers), bearing the costs of compliance or exclusion. The directionality for the unvaccinated is particularly high, as this reading frames their non-compliance as an imposition on the collective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_efficacy_ambiguity,
    'What is the precise empirical efficacy of specific mandates (e.g., mask mandates, vaccine passports) in reducing transmission and protecting vulnerable populations, considering compliance rates and pathogen characteristics?',
    'Rigorous epidemiological studies, randomized controlled trials, and comparative analyses across jurisdictions with varying mandate policies.',
    'If efficacy is lower than assumed, the justification for high extractiveness and suppression weakens, potentially shifting the classification towards a Snare or a less justified Tangled Rope. If efficacy is higher, it reinforces the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_efficacy_ambiguity, empirical, 'Uncertainty regarding the actual effectiveness of public health mandates.').

omega_variable(
    individual_rights_collective_good_balance,
    'What is the ethically justifiable balance between individual bodily autonomy and the collective good of public health, particularly when interventions are non-consensual or carry significant individual costs?',
    'Philosophical and bioethical deliberation, public discourse, and legal precedent that explicitly weighs these competing values. This is a conceptual, not empirical, resolution.',
    'A shift in societal consensus towards prioritizing individual autonomy would fundamentally alter the perceived legitimacy and classification of mandates, potentially re-framing them as Snares from a different ethical perspective. This reading explicitly prioritizes the collective good.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_rights_collective_good_balance, conceptual, 'The conceptual tension between individual rights and collective health obligations.').

omega_variable(
    unvaccinated_externality_quantification,
    'What is the precise, quantifiable externality (e.g., healthcare burden, transmission risk) imposed by unvaccinated individuals on the collective, and how does it compare to the costs imposed by mandates?',
    'Detailed epidemiological and economic modeling that isolates the specific contributions of unvaccinated populations to disease spread and healthcare strain, accounting for confounding factors.',
    'If the externality is lower than assumed, the justification for framing unvaccinated individuals as ''victims'' (payers) of the mandate weakens, and the perceived extractiveness of the mandate increases. If higher, it strengthens the ''tangled_rope'' justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unvaccinated_externality_quantification, empirical, 'Quantification of the burden imposed by unvaccinated individuals on public health.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__public_health_primary, theater_ratio, 6, 0.11).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__public_health_primary, theater_ratio, 12, 0.1).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__public_health_primary, theater_ratio, 18, 0.1).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__public_health_primary, theater_ratio, 24, 0.09).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__public_health_primary, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__public_health_primary, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__public_health_primary, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__public_health_primary, base_extractiveness, 18, 0.72).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__public_health_primary, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__public_health_primary, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__public_health_primary, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__public_health_primary, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__public_health_primary, suppression_requirement, 18, 0.78).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__public_health_primary, suppression_requirement, 24, 0.79).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__public_health_primary, suppression_requirement, 30, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
