% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: Emergence of Digital Money (First Held Reading)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint describes the emergence of digital money from the
 *   perspective of when individuals first practically held and used
 *   non-physical monetary instruments as stores of value. This reading
 *   emphasizes the role of technological development, individual adoption,
 *   and the overcoming of practical barriers, leading to a later origin date
 *   than conceptual emergence but prior to formal regulatory recognition. The
 *   constraint is classified as a Rope due to its coordination function in
 *   enabling new forms of value transfer, but with inherent costs and
 *   barriers to entry for certain populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.65).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.55).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Emergence of Digital Money (First Held Reading)").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, 'e918e737-15a2-4f2d-a98a-9eca5b922a4a').
narrative_ontology:cs_kernel_codification('e918e737-15a2-4f2d-a98a-9eca5b922a4a', implicit).
narrative_ontology:cs_authority_grounding('e918e737-15a2-4f2d-a98a-9eca5b922a4a', practice).
narrative_ontology:cs_reading_relation('e918e737-15a2-4f2d-a98a-9eca5b922a4a', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('e918e737-15a2-4f2d-a98a-9eca5b922a4a', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('e918e737-15a2-4f2d-a98a-9eca5b922a4a', foundational, practical_utility_defines_emergence).
narrative_ontology:cs_axiom_status(practical_utility_defines_emergence, holdable).
narrative_ontology:cs_axiom_grounding('e918e737-15a2-4f2d-a98a-9eca5b922a4a', practical_utility_defines_emergence, empirically_contingent).
narrative_ontology:cs_axiom('e918e737-15a2-4f2d-a98a-9eca5b922a4a', secondary, individual_agency_precedes_institutional_legitimacy).
narrative_ontology:cs_axiom_status(individual_agency_precedes_institutional_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e918e737-15a2-4f2d-a98a-9eca5b922a4a', individual_agency_precedes_institutional_legitimacy, conventional).
narrative_ontology:cs_reference_frame('e918e737-15a2-4f2d-a98a-9eca5b922a4a', individual_practical_adoption).
narrative_ontology:cs_drift_state('e918e737-15a2-4f2d-a98a-9eca5b922a4a', contemporary_digital_economy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e918e737-15a2-4f2d-a98a-9eca5b922a4a', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, digital_payment_providers).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, unbanked_populations).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, technologically_excluded).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, traditional_financial_institutions).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, traditional_financial_institutions).
narrative_ontology:constraint_vindicates(digital_money_origin__first_held_reading, network_effect_theory).
narrative_ontology:constraint_vindicates(digital_money_origin__first_held_reading, technological_diffusion_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who first adopted and regularly used non-physical monetary instruments (e.g., electronic funds, early digital currencies) as practical stores of value. They gained convenience, speed, and new capabilities for transactions, but also bore initial risks and learning costs.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Entities (e.g., banks, early tech companies) that developed and operated the infrastructure for digital money. They profited from transaction fees, network effects, and the expansion of financial services, actively shaping the terms of engagement.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, digital_payment_providers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, digital_payment_providers, beneficiary).

% Populations without access to traditional banking services, who were further marginalized by the emergence of digital money due to lack of infrastructure, identification, or trust. They bore the cost of exclusion from new economic opportunities.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_populations, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, unbanked_populations, excluded).

% Individuals lacking the necessary devices, internet access, or digital literacy to participate in the emerging digital money ecosystem. They faced barriers to entry and were effectively excluded from its benefits, relying solely on physical cash or traditional methods.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, technologically_excluded, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, technologically_excluded, excluded).

% Established banks and financial entities that initially faced disruption from digital money but later adapted by integrating digital services. They bore the costs of technological transition and competition but ultimately benefited from expanding their service offerings.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, traditional_financial_institutions, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, traditional_financial_institutions, beneficiary).

% Academics and researchers who study the historical development and impact of monetary systems, including the emergence of digital money. They analyze the structural changes and societal implications without direct participation in the economic flows.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__first_held_reading, digital_payment_providers).
narrative_ontology:fixing_cost_class(digital_money_origin__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled individuals to store and transfer value without physical instruments, facilitating transactions across distance and time, and reducing the friction associated with physical cash.
% TRANSFER_FUNCTION: Transferred convenience, speed, and new forms of value storage to early adopters and digital payment providers, while imposing costs of exclusion and adaptation on unbanked and technologically excluded populations.
% ABSENT_VOICES: The unbanked and technologically excluded populations, who would highlight the exclusionary aspects of this emergence and the digital divide it created. Their voices were often not part of the design or policy discussions.
% DISAPPEARANCE_RATIONALE: If digital money had never emerged, global commerce, individual financial practices, and the entire structure of modern economies would be fundamentally different, relying on slower, less efficient, and less scalable physical systems.
% FOUNDING_PROBLEM: The limitations of physical cash for large-scale, rapid, or remote transactions, and the desire for more efficient, secure, and globally accessible methods of value transfer and storage.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and technology analysts, from outside the direct beneficiaries, corroborate the historical limitations of physical money and the societal push for more efficient digital alternatives.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.65) reflecting the initial costs and risks for early adopters, and later, the network effects that concentrated power and rents with digital payment providers. Suppression is moderate (0.55) due to technical barriers, lack of infrastructure, and digital literacy requirements that limited access for many, rather than active coercion. Theater ratio is low (0.10) as the emergence was a functional, practical development. Accessibility collapse is moderate (0.40) because while digital money offered new capabilities, physical cash remained a viable alternative for those without access. Resistance is low (0.20) as opposition was more passive (non-adoption) than active.
 *
 * PERSPECTIVAL GAP:
 *   The emergence of digital money is viewed as a net positive for efficiency and innovation by its beneficiaries, while victims experience it as a source of exclusion and inequality. The engine's classification will reflect this divergence based on the structural positions of stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and digital payment providers are beneficiaries, gaining convenience and profit respectively. Unbanked and technologically excluded populations are victims, bearing the costs of exclusion and lack of access. Traditional financial institutions initially bore adaptation costs but became beneficiaries by integrating digital services. Monetary historians serve as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_practical_holding,
    'What threshold of use or adoption constitutes ''practical holding'' as a store of value?',
    'Empirical studies establishing minimum frequency, volume, or duration of use for non-physical instruments to be considered a primary store of value by individuals.',
    'A higher threshold would shift the emergence date later, emphasizing more mature digital systems; a lower threshold would push it earlier, including nascent forms of electronic value. This would affect the temporal measurements and potentially the perceived extractiveness/suppression of the early phase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_practical_holding, conceptual, 'Ambiguity in defining the ''practical holding'' criterion for digital money''s emergence.').

omega_variable(
    distinction_from_conceptual_emergence,
    'How distinct is ''practical holding'' from the prior conceptual and technical ''thinkability'' of digital money?',
    'Analysis of historical records to identify the gap between theoretical possibility/early prototypes and widespread individual adoption for practical use. If the gap is minimal, the readings might converge.',
    'If the distinction is weak, this reading''s unique contribution to the kernel''s understanding diminishes, potentially leading to a re-evaluation of its independent constraint status. If strong, it reinforces the importance of implementation barriers and network effects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distinction_from_conceptual_emergence, empirical, 'Clarifying the boundary between conceptual and practical emergence of digital money.').

omega_variable(
    pre_vs_post_regulatory_recognition,
    'To what extent did individual practical holding of digital money precede and influence formal regulatory recognition, versus being a consequence of it?',
    'Comparative historical analysis of jurisdictions where individual adoption outpaced regulation versus those where regulation drove adoption. This would clarify the causal directionality.',
    'If individual holding strongly preceded regulation, it emphasizes bottom-up emergence and the constraint''s ''rope'' nature as a solution to practical problems. If regulation was a strong precursor, it would suggest a more ''tangled rope'' or ''snare'' dynamic driven by institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_vs_post_regulatory_recognition, empirical, 'Causal relationship between individual adoption and regulatory recognition of digital money.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 1970, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1970, digital_money_origin__first_held_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(digi_tr_t1976, digital_money_origin__first_held_reading, theater_ratio, 1976, 0.06).
narrative_ontology:measurement(digi_tr_t1982, digital_money_origin__first_held_reading, theater_ratio, 1982, 0.07).
narrative_ontology:measurement(digi_tr_t1988, digital_money_origin__first_held_reading, theater_ratio, 1988, 0.08).
narrative_ontology:measurement(digi_tr_t1994, digital_money_origin__first_held_reading, theater_ratio, 1994, 0.09).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__first_held_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(digi_be_t1970, digital_money_origin__first_held_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(digi_be_t1976, digital_money_origin__first_held_reading, base_extractiveness, 1976, 0.53).
narrative_ontology:measurement(digi_be_t1982, digital_money_origin__first_held_reading, base_extractiveness, 1982, 0.57).
narrative_ontology:measurement(digi_be_t1988, digital_money_origin__first_held_reading, base_extractiveness, 1988, 0.6).
narrative_ontology:measurement(digi_be_t1994, digital_money_origin__first_held_reading, base_extractiveness, 1994, 0.63).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__first_held_reading, base_extractiveness, 2000, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1970, digital_money_origin__first_held_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(digi_su_t1976, digital_money_origin__first_held_reading, suppression_requirement, 1976, 0.44).
narrative_ontology:measurement(digi_su_t1982, digital_money_origin__first_held_reading, suppression_requirement, 1982, 0.48).
narrative_ontology:measurement(digi_su_t1988, digital_money_origin__first_held_reading, suppression_requirement, 1988, 0.51).
narrative_ontology:measurement(digi_su_t1994, digital_money_origin__first_held_reading, suppression_requirement, 1994, 0.53).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__first_held_reading, suppression_requirement, 2000, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'digital_money_origin' kernel, focusing on the practical adoption by individuals. It is linked to sibling readings that emphasize conceptual emergence and regulatory recognition, as these aspects are causally and structurally related in the overall history of digital money.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
