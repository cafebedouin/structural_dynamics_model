% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__property_rights_reading, []).

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
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Software as Intellectual Property: Creator's Rights Reading
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'property rights' reading of software's
 *   status, asserting that software is intellectual property and creators
 *   have a legitimate right to restrict access and modification. This reading
 *   underpins the proprietary software industry, where licensing restrictions
 *   are seen as legitimate exercises of ownership, and users are primarily
 *   consumers with contractual rights only. The constraint is classified as a
 *   Tangled Rope because it provides a coordination function (incentivizing
 *   creation) but also involves significant asymmetric extraction (from users
 *   and other developers to proprietary companies) maintained through active
 *   enforcement of IP laws.
 *
 * KEY AGENTS:
 *   - Proprietary Software Companies: Agenda-setter (institutional/mobile) — sets and enforces licensing terms, benefits from extraction.
 *   - Individual Software Creators: Beneficiary (moderate/constrained) — benefits from monetization, relies on IP protections.
 *   - Software Users: Payer (powerless/constrained) — bears costs of licenses, limited rights.
 *   - Open Source Developers: Payer (organized/constrained) — affected by proprietary dominance, advocates for alternatives.
 *   - Intellectual Property Lawyers: Beneficiary (organized/mobile) — profits from the legal complexity of IP enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.6).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.7).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Software as Intellectual Property: Creator's Rights Reading").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, '7103504a-3f8c-45cc-821a-2731c6d214ad').
narrative_ontology:cs_kernel_codification('7103504a-3f8c-45cc-821a-2731c6d214ad', formalized).
narrative_ontology:cs_authority_grounding('7103504a-3f8c-45cc-821a-2731c6d214ad', lineage).
narrative_ontology:cs_interpretation_layer_present('7103504a-3f8c-45cc-821a-2731c6d214ad').
narrative_ontology:cs_reading_relation('7103504a-3f8c-45cc-821a-2731c6d214ad', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('7103504a-3f8c-45cc-821a-2731c6d214ad', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('7103504a-3f8c-45cc-821a-2731c6d214ad', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('7103504a-3f8c-45cc-821a-2731c6d214ad', foundational, creator_exclusive_rights_foundational).
narrative_ontology:cs_axiom_status(creator_exclusive_rights_foundational, holdable).
narrative_ontology:cs_axiom_grounding('7103504a-3f8c-45cc-821a-2731c6d214ad', creator_exclusive_rights_foundational, deontological).
narrative_ontology:cs_axiom('7103504a-3f8c-45cc-821a-2731c6d214ad', foundational, software_as_private_property).
narrative_ontology:cs_axiom_status(software_as_private_property, holdable).
narrative_ontology:cs_axiom_grounding('7103504a-3f8c-45cc-821a-2731c6d214ad', software_as_private_property, conventional).
narrative_ontology:cs_reference_frame('7103504a-3f8c-45cc-821a-2731c6d214ad', exclusive_creator_control_framework).
narrative_ontology:cs_drift_state('7103504a-3f8c-45cc-821a-2731c6d214ad', contemporary_open_source_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7103504a-3f8c-45cc-821a-2731c6d214ad', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_software_companies).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, individual_software_creators).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, open_source_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, intellectual_property_lawyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These companies develop and sell software under proprietary licenses, relying on copyright and patent law to restrict access to source code and control modification. They actively lobby for stronger IP protections and enforce licensing agreements.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_software_companies, agenda_setter,
    institutional, generational, mobile, global).

% Individual creators benefit from the ability to monetize their software through sales and licensing, protecting their work from unauthorized copying or modification. Their livelihood often depends on these protections.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, individual_software_creators, beneficiary,
    moderate, biographical, constrained, global).

% Users purchase or license software, gaining access to its functionality but typically without rights to inspect, modify, or redistribute the source code. They bear the cost of licenses and are subject to vendor lock-in.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_users, payer,
    powerless, immediate, constrained, global).

% While often operating outside the proprietary model, open-source developers are affected by the dominance of proprietary IP, facing legal challenges or market pressure when their work interacts with proprietary systems. They advocate for alternative models of software distribution.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, open_source_developers, payer,
    organized, generational, constrained, global).

% Legal professionals specializing in intellectual property law benefit from the complexity and enforcement needs of proprietary software, providing services for licensing, patenting, and litigation.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, intellectual_property_lawyers, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal and economic framework that incentivizes software creation by granting creators exclusive rights, thereby coordinating investment and innovation in software development.
% TRANSFER_FUNCTION: Transfers economic value from software users (through license fees and restricted access) to software creators and companies, in exchange for access to proprietary software functionality.
% ABSENT_VOICES: Advocates for software freedom and digital commons are often marginalized in policy discussions dominated by proprietary interests. They would argue for default open access and user rights to modify software.
% DISAPPEARANCE_RATIONALE: If intellectual property rights for software vanished overnight, the economic model for proprietary software would collapse. Companies would cease to invest in closed-source development, leading to a rapid shift towards open-source models or alternative funding mechanisms, fundamentally reorganizing the software industry.
% FOUNDING_PROBLEM: The problem was how to incentivize the creation of complex software by ensuring creators could recoup their investment and profit from their work, preventing unauthorized copying and free-riding.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary software companies and IP lawyers attest that the problem of incentivizing creation and preventing piracy remains live, citing ongoing challenges in protecting digital assets. Independent economic analyses often corroborate the role of IP in incentivizing certain types of innovation, though the optimal scope of these rights is contested.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the significant portion of value captured by proprietary rights holders, often exceeding the direct cost of development and maintenance. Suppression (0.7) is high due to the legal and technical barriers preventing users and other developers from accessing or modifying source code, with active enforcement through copyright, patent, and licensing agreements. The theater ratio (0.2) is relatively low, as the enforcement mechanisms are genuinely functional in protecting proprietary interests, though some performative aspects exist in public discourse around 'piracy'. Accessibility collapse (0.4) is moderate, as open-source alternatives exist but are often not direct substitutes for dominant proprietary solutions. Resistance (0.5) is also moderate, with ongoing legal challenges and strong advocacy from the open-source community.
 *
 * PERSPECTIVAL GAP:
 *   Proprietary software companies perceive this as a legitimate Rope, essential for innovation and fair compensation. Software users and open-source developers, however, experience it as a Snare or Tangled Rope, where the coordination function is overshadowed by extractive practices and suppressed freedoms. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software companies and individual creators are clear beneficiaries, with directionality skewed towards 0.0. Software users and open-source developers are targets, bearing the costs and restrictions, with directionality towards 1.0. IP lawyers also benefit from the system's complexity, aligning with the beneficiary side.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as a pure Rope (as proprietary interests claim) by highlighting the asymmetric extraction and active suppression. It also avoids mislabeling it as a pure Snare by acknowledging the genuine coordination function of incentivizing software creation. The 'live' status of the founding problem (incentivizing creation) suggests mandatrophy is not fully resolved, but the 'contested' status indicates a debate over whether the current level of extraction is still justified by that original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_ip_scope,
    'What is the optimal scope and duration of intellectual property rights for software to maximize innovation and societal welfare, balancing creator incentives with public access?',
    'Longitudinal empirical studies comparing innovation rates and economic growth under different IP regimes, combined with economic modeling of market structures.',
    'If current IP protections are found to be sub-optimal (e.g., too broad or too long), it would suggest the constraint''s extractiveness is excessive and could be reduced without harming innovation, potentially reclassifying it closer to a Rope or even a Scaffold if temporary adjustments are proposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_ip_scope, empirical, 'Uncertainty regarding the efficiency and welfare effects of current software IP laws.').

omega_variable(
    software_nature_conceptual_ambiguity,
    'Is software fundamentally an ''expression'' (like literature, protected by copyright) or a ''machine'' (like an invention, protected by patent, or a public good)?',
    'A philosophical and legal re-evaluation of software''s ontological status, potentially leading to new legal frameworks that treat software differently from other forms of intellectual property.',
    'If software is re-conceptualized as a machine or public good, the ''property rights'' reading would be conceptually undermined, leading to a reclassification towards a Rope (if coordination is primary) or even a Mountain (if its machine-like nature implies inherent freedoms).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(software_nature_conceptual_ambiguity, conceptual, 'Ambiguity in the fundamental nature of software and its implications for IP law.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''software_source_status'' kernel. How do the structural deltas of the ''freedom_imperative_reading'', ''pragmatic_development_reading'', and ''utilitarian_hybrid_reading'' alter the classification?',
    'Comparative analysis of each sibling reading''s stakeholder structure, extractiveness, and suppression metrics, as authored in their respective constraint stories.',
    'Each sibling reading would likely yield a different classification (e.g., ''freedom_imperative_reading'' might be a Mountain or Rope, ''pragmatic_development_reading'' a Rope, ''utilitarian_hybrid_reading'' a Tangled Rope with different parameters), highlighting the contestability of software''s fundamental status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Documents that this constraint is one specific interpretation within a contested kernel, with other interpretations leading to different structural outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1980, software_source_status__property_rights_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(soft_tr_t1990, software_source_status__property_rights_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__property_rights_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__property_rights_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(soft_tr_t2024, software_source_status__property_rights_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(soft_be_t1980, software_source_status__property_rights_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(soft_be_t1990, software_source_status__property_rights_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(soft_be_t2000, software_source_status__property_rights_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(soft_be_t2010, software_source_status__property_rights_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(soft_be_t2024, software_source_status__property_rights_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1980, software_source_status__property_rights_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(soft_su_t1990, software_source_status__property_rights_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(soft_su_t2000, software_source_status__property_rights_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(soft_su_t2010, software_source_status__property_rights_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(soft_su_t2024, software_source_status__property_rights_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, digital_rights_management_systems).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, open_source_licensing_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
