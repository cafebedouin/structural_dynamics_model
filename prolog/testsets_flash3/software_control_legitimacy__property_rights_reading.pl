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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right (Property Rights Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint models the 'property rights' reading of software control,
 *   where creators' authority to restrict use, modification, and distribution
 *   is seen as legitimate and necessary to protect investment and ensure
 *   commercial viability. It is one reading of the broader
 *   'software_control_legitimacy' kernel. The constraint operates as a
 *   Tangled Rope, providing a coordination function for commercial
 *   development while extracting from users and FOSS advocates through
 *   restrictions and licensing fees. The metrics reflect a moderate but
 *   increasing extractiveness and suppression, as enforcement mechanisms for
 *   digital rights management and licensing become more sophisticated.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.45).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.6).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right (Property Rights Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, 'eca0144a-e0f3-4b28-9ae2-550e3fce50d5').
narrative_ontology:cs_kernel_codification('eca0144a-e0f3-4b28-9ae2-550e3fce50d5', formalized).
narrative_ontology:cs_authority_grounding('eca0144a-e0f3-4b28-9ae2-550e3fce50d5', lineage).
narrative_ontology:cs_interpretation_layer_present('eca0144a-e0f3-4b28-9ae2-550e3fce50d5').
narrative_ontology:cs_reading_relation('eca0144a-e0f3-4b28-9ae2-550e3fce50d5', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('eca0144a-e0f3-4b28-9ae2-550e3fce50d5', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('eca0144a-e0f3-4b28-9ae2-550e3fce50d5', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('eca0144a-e0f3-4b28-9ae2-550e3fce50d5', foundational, software_is_property).
narrative_ontology:cs_axiom_status(software_is_property, holdable).
narrative_ontology:cs_axiom_grounding('eca0144a-e0f3-4b28-9ae2-550e3fce50d5', software_is_property, deontological).
narrative_ontology:cs_axiom('eca0144a-e0f3-4b28-9ae2-550e3fce50d5', foundational, investment_requires_exclusivity).
narrative_ontology:cs_axiom_status(investment_requires_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('eca0144a-e0f3-4b28-9ae2-550e3fce50d5', investment_requires_exclusivity, empirically_contingent).
narrative_ontology:cs_reference_frame('eca0144a-e0f3-4b28-9ae2-550e3fce50d5', classical_intellectual_property_framework).
narrative_ontology:cs_drift_state('eca0144a-e0f3-4b28-9ae2-550e3fce50d5', contemporary_digital_commons_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('eca0144a-e0f3-4b28-9ae2-550e3fce50d5', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_investors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, users_seeking_modification).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, intellectual_property_lawyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell proprietary software, relying on intellectual property laws to protect their investment and revenue streams. They actively enforce licensing agreements and advocate for strong copyright and patent protections.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_vendors, agenda_setter,
    institutional, biographical, mobile, global).

% Provide capital to software companies, expecting returns based on the ability to control and monetize software. Their investment decisions are influenced by the strength of intellectual property protections.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Promote free and open-source software, arguing for user freedoms and collaborative development. They bear the cost of proprietary restrictions through limited access, inability to modify, and legal challenges when attempting to circumvent controls.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    organized, generational, constrained, global).

% Individuals or small groups who wish to modify software for personal use, interoperability, or customization. They are restricted by licensing terms and technical protection measures, often facing legal threats for unauthorized alterations.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, users_seeking_modification, payer,
    moderate, immediate, constrained, local).

% Specialize in defending and enforcing software property rights, benefiting from the complexity and litigious nature of intellectual property disputes. They advise vendors on licensing and pursue infringement cases.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, intellectual_property_lawyers, beneficiary,
    organized, biographical, mobile, national).

% Provide the framework for intellectual property rights, adjudicating disputes and enforcing judgments. They are the ultimate arbiter of what constitutes a 'property right' in software and how it can be restricted.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, legal_systems, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates investment and innovation by providing a legal framework that assures creators they can profit from their work, incentivizing the development of complex and costly software.
% TRANSFER_FUNCTION: Transfers control over software use, modification, and distribution from users and potential competitors to the original creators and their investors, in exchange for access to the software itself.
% ABSENT_VOICES: Users who believe software should be inherently free or a public good, and developers who prioritize collaborative, unrestricted innovation, are often marginalized in policy discussions dominated by commercial interests and legal precedent.
% DISAPPEARANCE_RATIONALE: If software property rights vanished overnight, the commercial software industry would collapse, investment models would shift dramatically, and a new ecosystem of open-source or public-funded software would emerge, fundamentally reorganizing the digital economy.
% FOUNDING_PROBLEM: The problem of incentivizing investment in software development when digital goods are easily copied and distributed, leading to a 'free rider' problem that stifles innovation.
% FOUNDING_PROBLEM_CORROBORATION: Software industry associations and legal scholars consistently attest to the ongoing need for intellectual property protection to sustain commercial software development. While FOSS advocates contest the necessity of proprietary models, the economic reality of large-scale commercial software investment largely corroborates the problem's persistence from the perspective of capital allocation.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).
:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while it enables commercial sustainability, it also restricts freedoms that some users and developers value. Suppression is higher (0.6) due to the active legal and technical enforcement required to maintain these restrictions against a culture of sharing and modification. Theater ratio is low (0.1) as the enforcement is largely functional in protecting commercial interests, not merely performative. Accessibility collapse is moderate (0.4) as alternatives (FOSS) exist but are often less feature-rich or require more technical expertise. Resistance is moderate (0.5) from FOSS communities and users seeking greater control.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of software vendors and investors, this constraint is a necessary Rope, enabling a functioning market. From the perspective of FOSS advocates and users, it is a Snare, restricting fundamental freedoms and extracting value. The engine's per-seat classification will reflect this divergence based on the declared power, exit options, and beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Software vendors and investors are clear beneficiaries and agenda-setters, as the constraint directly protects their business model and returns on investment. FOSS advocates and users seeking modification are victims, bearing the costs of restricted access and control. Legal systems act as agenda-setters by providing the framework and enforcement mechanisms. The constraint subsidizes commercial development by externalizing some costs onto users and alternative development models.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_analogy_validity,
    'Is the analogy between physical property rights and software control structurally sound, given the non-rivalrous nature of digital goods?',
    'Conceptual analysis and legal scholarship examining the unique characteristics of digital information and its implications for property theory.',
    'If the analogy is weak, the foundational justification for this reading''s extractiveness is undermined, potentially shifting its classification towards a Snare. If strong, it reinforces the legitimacy of the current structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_analogy_validity, conceptual, 'The conceptual validity of applying physical property rights to software.').

omega_variable(
    innovation_incentive_necessity,
    'Is strong intellectual property protection truly necessary to incentivize software innovation, or do alternative models (e.g., public funding, service-based revenue) suffice?',
    'Empirical studies comparing innovation rates and quality in proprietary vs. open-source ecosystems, and economic analysis of alternative funding models.',
    'If innovation thrives without strong IP, the primary coordination function claimed by this reading is weakened, increasing its effective extractiveness. If IP is shown to be critical, the coordination function is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_necessity, empirical, 'The empirical necessity of IP for software innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__property_rights_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__property_rights_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__property_rights_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__property_rights_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__property_rights_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__property_rights_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__property_rights_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__property_rights_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__property_rights_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__property_rights_reading, base_extractiveness, 40, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__property_rights_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__property_rights_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__property_rights_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__property_rights_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, digital_rights_management_enforcement).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, patent_troll_litigation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
