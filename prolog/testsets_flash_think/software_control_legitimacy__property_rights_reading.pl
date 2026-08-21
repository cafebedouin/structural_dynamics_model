% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right (Property Rights Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'property rights' reading of software
 *   control, asserting creators' legitimate authority to restrict use,
 *   modification, and distribution to protect investment and enable
 *   commercial sustainability. While claimed as a 'rope' (coordination for
 *   innovation), its operation involves substantial extraction from users and
 *   FOSS advocates, and requires active legal and technical enforcement. The
 *   metrics reflect this operational reality, creating a deliberate gap
 *   between the claimed type and the observed behavior, which the engine is
 *   designed to detect.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.68).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.75).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right (Property Rights Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, '473228ea-8100-4777-a5d0-e24cd8800dc8').
narrative_ontology:cs_kernel_codification('473228ea-8100-4777-a5d0-e24cd8800dc8', formalized).
narrative_ontology:cs_authority_grounding('473228ea-8100-4777-a5d0-e24cd8800dc8', extraction).
narrative_ontology:cs_interpretation_layer_present('473228ea-8100-4777-a5d0-e24cd8800dc8').
narrative_ontology:cs_reading_relation('473228ea-8100-4777-a5d0-e24cd8800dc8', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('473228ea-8100-4777-a5d0-e24cd8800dc8', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('473228ea-8100-4777-a5d0-e24cd8800dc8', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('473228ea-8100-4777-a5d0-e24cd8800dc8', foundational, software_is_private_property).
narrative_ontology:cs_axiom_status(software_is_private_property, holdable).
narrative_ontology:cs_axiom_grounding('473228ea-8100-4777-a5d0-e24cd8800dc8', software_is_private_property, conventional).
narrative_ontology:cs_axiom('473228ea-8100-4777-a5d0-e24cd8800dc8', foundational, commercial_incentives_require_exclusivity).
narrative_ontology:cs_axiom_status(commercial_incentives_require_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('473228ea-8100-4777-a5d0-e24cd8800dc8', commercial_incentives_require_exclusivity, empirically_contingent).
narrative_ontology:cs_reference_frame('473228ea-8100-4777-a5d0-e24cd8800dc8', classical_intellectual_property_framework).
narrative_ontology:cs_drift_state('473228ea-8100-4777-a5d0-e24cd8800dc8', contemporary_digital_economy, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('473228ea-8100-4777-a5d0-e24cd8800dc8', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, investors_in_software).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, users_seeking_unrestricted_use).
narrative_ontology:constraint_vindicates(software_control_legitimacy__property_rights_reading, intellectual_property_doctrine).
narrative_ontology:constraint_vindicates(software_control_legitimacy__property_rights_reading, commercial_incentive_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell proprietary software, relying on intellectual property rights to protect their investment and revenue streams. They actively lobby for strong IP enforcement and shape licensing terms.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_vendors, agenda_setter,
    institutional, biographical, mobile, global).

% Provide capital for software development, expecting returns protected by the legal framework of software as property. Their investment decisions are directly influenced by the strength of these rights.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, investors_in_software, beneficiary,
    powerful, biographical, arbitrage, global).

% Promote free and open-source software, often viewing proprietary restrictions as ethically problematic or economically inefficient. They bear the cost of restricted access and face legal challenges when attempting to modify or distribute proprietary code.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, foss_advocates, excluded).

% Desire full control over the software they use, including the ability to modify, share, and understand its inner workings. They pay for proprietary licenses and are legally restricted from exercising these freedoms, often without full awareness of the implications.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, users_seeking_unrestricted_use, payer,
    powerless, immediate, constrained, global).

% Specialize in drafting and enforcing software licenses, litigating infringement cases, and advising on IP strategy. They directly benefit from the complexity and enforcement needs of software property rights.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, intellectual_property_lawyers, agenda_setter,
    institutional, biographical, mobile, national).

% Interpret and enforce intellectual property laws, and create new legislation that defines the scope of software control. They are the ultimate arbiters of this constraint's legal boundaries.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, courts_and_legislatures, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework that incentivizes investment and innovation in software development by granting creators exclusive rights, thereby coordinating commercial activity and market stability.
% TRANSFER_FUNCTION: Transfers control over software use, modification, and distribution from users and the public domain to creators and their commercial entities, in exchange for access to proprietary software products.
% ABSENT_VOICES: Philosophers of technology advocating for digital commons, activists for universal access to knowledge, and users who believe in the fundamental right to control their own computing devices are largely excluded from the legislative and judicial processes that define these rights.
% DISAPPEARANCE_RATIONALE: If software property rights vanished overnight, the commercial software industry as currently structured would collapse. Investment models would shift dramatically, open-source development would likely accelerate, and new economic models for software creation and distribution would emerge, fundamentally reorganizing the digital economy.
% FOUNDING_PROBLEM: The problem of incentivizing the creation of complex software and ensuring commercial viability for developers in an era where digital copying was trivial and widespread.
% FOUNDING_PROBLEM_CORROBORATION: Software vendors and investors consistently attest that the problem of incentivizing innovation and preventing piracy remains live, citing ongoing R&D costs and the ease of unauthorized copying. Independent economic analyses and historical trends corroborate that strong IP protections have historically correlated with commercial software growth, though the necessity of *absolute* control is contested by FOSS economists.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the restrictions on use and modification impose significant costs on users and developers who desire more freedom, and the pricing of proprietary software often captures substantial rents beyond development costs. Suppression is also high (0.75) due to robust legal enforcement (DMCA, licensing agreements) and technical protection measures (DRM) that actively prevent alternatives or circumvention. Theater ratio is low (0.15) as the enforcement mechanisms are generally functional and directly serve the goal of maintaining proprietary control, rather than being purely performative. Accessibility collapse is moderate (0.60) as FOSS alternatives exist but are often less commercially supported or require greater technical expertise, and resistance is moderate (0.55) from organized FOSS movements and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of software vendors and investors, this constraint is a legitimate 'rope' that enables a thriving industry by protecting their intellectual property. From the perspective of FOSS advocates and users seeking unrestricted use, it operates as a 'snare' or 'tangled rope', extracting value and suppressing freedoms under the guise of commercial necessity. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Software vendors and investors are clear beneficiaries (low d) as they directly profit from the exclusive rights. FOSS advocates and users seeking unrestricted use are targets (high d) as they bear the costs of restricted access and legal limitations. Intellectual property lawyers also benefit from the enforcement complexity. Courts and legislatures act as observers and agenda-setters, shaping the legal landscape.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_property_analogy_validity,
    'Is the analogy between physical property rights and software control (digital property) structurally sound, given the non-rivalrous and non-excludable nature of digital goods?',
    'Conceptual analysis comparing the economic and social implications of physical vs. digital property regimes, and empirical studies on the effects of different IP strengths on innovation and access.',
    'If the analogy is weak, the foundational premise of this reading is undermined, potentially reclassifying the constraint as more extractive (snare) due to a lack of genuine coordination justification. If strong, it reinforces the ''rope'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_property_analogy_validity, conceptual, 'Ambiguity regarding the applicability of traditional property rights to software.').

omega_variable(
    incentive_vs_access_balance,
    'What is the optimal balance between intellectual property protection (to incentivize creation) and open access (to foster innovation and public good), and does the current property_rights_reading achieve it?',
    'Longitudinal economic studies comparing innovation rates and market concentration under varying IP regimes (e.g., strong vs. weak patent/copyright enforcement), and sociological studies on the impact of access on digital literacy and societal development.',
    'If the current balance is found to stifle innovation or create undue social costs, the extractiveness metric would be re-evaluated upward, pushing the classification towards snare. If it is found to be optimal, the rope classification would be strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incentive_vs_access_balance, empirical, 'Uncertainty about the empirical effects of strong IP on innovation and public welfare.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''software_control_legitimacy'' kernel. How would the classification change if a sibling reading (e.g., ''freedom_imperative_reading'') were adopted?',
    'Adopting the ''freedom_imperative_reading'' would shift the primary beneficiaries to users and FOSS advocates, and the victims to software vendors, fundamentally inverting the directionality and likely reducing extractiveness to near zero, reclassifying as a mountain or rope.',
    'A reclassification to a less extractive type (mountain/rope) for the ''freedom_imperative_reading'' would highlight the constructed nature and perspectival dependence of the ''property_rights_reading''s'' extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Documents this constraint as the ''property_rights_reading'' of the ''software_control_legitimacy'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1980, software_control_legitimacy__property_rights_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(soft_tr_t1990, software_control_legitimacy__property_rights_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(soft_tr_t2000, software_control_legitimacy__property_rights_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(soft_tr_t2010, software_control_legitimacy__property_rights_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(soft_tr_t2020, software_control_legitimacy__property_rights_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(soft_tr_t2025, software_control_legitimacy__property_rights_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(soft_be_t1980, software_control_legitimacy__property_rights_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(soft_be_t1990, software_control_legitimacy__property_rights_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(soft_be_t2000, software_control_legitimacy__property_rights_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(soft_be_t2010, software_control_legitimacy__property_rights_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(soft_be_t2020, software_control_legitimacy__property_rights_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(soft_be_t2025, software_control_legitimacy__property_rights_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1980, software_control_legitimacy__property_rights_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(soft_su_t1990, software_control_legitimacy__property_rights_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(soft_su_t2000, software_control_legitimacy__property_rights_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(soft_su_t2010, software_control_legitimacy__property_rights_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(soft_su_t2020, software_control_legitimacy__property_rights_reading, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(soft_su_t2025, software_control_legitimacy__property_rights_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel. Each reading instantiates a distinct constraint with its own ε and classification, linked here to show their conceptual relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
