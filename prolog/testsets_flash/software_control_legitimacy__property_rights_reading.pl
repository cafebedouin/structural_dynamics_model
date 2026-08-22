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
 *   human_readable: Software as Property Right (Property Rights Reading)
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint models the 'software as property right' reading of
 *   software control legitimacy. It asserts that creators have legitimate
 *   authority to restrict use, modification, and distribution to protect
 *   investment and enable commercial sustainability. This reading is
 *   foundational to the proprietary software industry and is actively
 *   enforced through legal and technical means. The constraint is presented
 *   as a tangled rope, acknowledging a genuine coordination function
 *   (incentivizing software creation) alongside significant extraction from
 *   users and competitors.
 *
 * KEY AGENTS:
 *   - software_vendors: Primary agenda-setter and beneficiary (institutional/mobile)
 *   - software_investors: Primary beneficiary (powerful/arbitrage)
 *   - users_seeking_modification: Primary payer (powerless/constrained)
 *   - open_source_advocates: Organized payer (organized/identity_locked)
 *   - competitors_seeking_interoperability: Payer (moderate/constrained)
 *   - legal_systems: Institutional agenda-setter (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.55).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.65).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software as Property Right (Property Rights Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, 'c557a290-c43c-4a3d-901a-0625271900ff').
narrative_ontology:cs_kernel_codification('c557a290-c43c-4a3d-901a-0625271900ff', formalized).
narrative_ontology:cs_authority_grounding('c557a290-c43c-4a3d-901a-0625271900ff', lineage).
narrative_ontology:cs_interpretation_layer_present('c557a290-c43c-4a3d-901a-0625271900ff').
narrative_ontology:cs_reading_relation('c557a290-c43c-4a3d-901a-0625271900ff', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('c557a290-c43c-4a3d-901a-0625271900ff', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('c557a290-c43c-4a3d-901a-0625271900ff', software_control_legitimacy__commons_reading, forecloses).
narrative_ontology:cs_axiom('c557a290-c43c-4a3d-901a-0625271900ff', foundational, intellectual_property_is_natural_right).
narrative_ontology:cs_axiom_status(intellectual_property_is_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('c557a290-c43c-4a3d-901a-0625271900ff', intellectual_property_is_natural_right, deontological).
narrative_ontology:cs_axiom('c557a290-c43c-4a3d-901a-0625271900ff', foundational, exclusive_rights_incentivize_innovation).
narrative_ontology:cs_axiom_status(exclusive_rights_incentivize_innovation, holdable).
narrative_ontology:cs_axiom_grounding('c557a290-c43c-4a3d-901a-0625271900ff', exclusive_rights_incentivize_innovation, empirically_contingent).
narrative_ontology:cs_reference_frame('c557a290-c43c-4a3d-901a-0625271900ff', classical_intellectual_property_framework).
narrative_ontology:cs_drift_state('c557a290-c43c-4a3d-901a-0625271900ff', contemporary_digital_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c557a290-c43c-4a3d-901a-0625271900ff', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_investors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, users_seeking_modification).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, open_source_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, competitors_seeking_interoperability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and distribute proprietary software, relying on intellectual property laws to protect their investments. They actively enforce licensing agreements and digital rights management (DRM) to control use, modification, and distribution, ensuring commercial sustainability.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_vendors, agenda_setter,
    institutional, generational, mobile, global).

% Provide capital to software companies, expecting returns protected by the legal framework of software as property. Their investment decisions are directly influenced by the strength of intellectual property enforcement.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Desire to modify, adapt, or understand the software they use, but are legally and technically restricted by proprietary licenses and DRM. They bear the cost of limited control and dependence on vendors for updates and fixes.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, users_seeking_modification, payer,
    powerless, immediate, constrained, local).

% Promote software freedom and the commons, viewing proprietary restrictions as ethically problematic. They are structurally disadvantaged by the property rights framework, as their model of shared development is directly opposed to exclusive control.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, open_source_advocates, payer,
    organized, generational, identity_locked, global).

% Seek to create compatible or interoperable software, but face legal barriers and technical hurdles due to proprietary control. They bear the cost of reverse engineering or licensing fees, which limits competition and innovation.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, competitors_seeking_interoperability, payer,
    moderate, biographical, constrained, national).

% Provide the legal framework (copyright, patent, trade secret law) that defines software as property and enables its enforcement. They adjudicate disputes and evolve the interpretation of these rights.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, legal_systems, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates investment and innovation in software development by granting creators exclusive rights, thereby incentivizing the creation of complex and valuable software that might not otherwise be produced.
% TRANSFER_FUNCTION: Transfers economic value from users and potential competitors to software creators and investors, in exchange for access to proprietary software products.
% ABSENT_VOICES: Users who believe they have a fundamental right to control their computing devices, and developers who advocate for a software commons, are often marginalized in policy discussions dominated by intellectual property holders. Their arguments for freedom and shared resources are systematically excluded from the property rights framing.
% DISAPPEARANCE_RATIONALE: If the concept of software as a property right vanished overnight, the commercial software industry as we know it would collapse. Investment models would shift dramatically, open-source development would likely accelerate, and the entire digital economy would reorganize around new models of creation and distribution.
% FOUNDING_PROBLEM: Software development requires significant investment, but software is easily copied and distributed. Without legal protection, creators and investors feared they would have no way to recoup costs or profit, leading to underinvestment in software innovation.
% FOUNDING_PROBLEM_CORROBORATION: Software vendors and investors consistently attest that the problem of protecting investment in easily replicable digital goods remains live. Independent economic analyses, while sometimes critical of the extent of protection, generally corroborate the need for some form of intellectual property to incentivize complex software development.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.55) reflects the cost borne by users and competitors for access and restricted use, which is substantial but not absolute. Suppression (0.65) is high due to active legal enforcement (copyright, patent, DRM) and the lack of viable alternatives for many essential software functions. The theater ratio is low (0.1) because the enforcement mechanisms are genuinely functional in protecting proprietary interests, not merely performative. The claimed type is 'tangled_rope' because it genuinely solves a coordination problem (incentivizing creation) but does so with significant asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   Software vendors and investors perceive this as a legitimate and necessary framework for innovation (a rope or even a mountain of economic reality). Users and open-source advocates, however, experience it as an extractive snare that limits their freedom and access. The engine's per-seat classification will reflect this divergence based on their declared power, exit options, and roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Software vendors and investors are clear beneficiaries, as the constraint directly protects their business model and returns on investment. Users, open-source advocates, and competitors are payers, bearing the costs of restricted access, lack of control, and suppressed alternatives. Legal systems act as agenda-setters, providing the framework and enforcement mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (incentivizing software creation) is still live, but its implementation through strong property rights is contested. The high extractiveness and suppression suggest that while the founding problem persists, the solution has evolved into a mechanism that disproportionately benefits the agenda-setters, potentially beyond what is necessary for coordination. This prevents mislabeling it as a pure snare by acknowledging the initial coordination function, but flags the potential for rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_rights_vs_freedom_imperative,
    'Is software control primarily a property right, or is user freedom over computing a more fundamental imperative?',
    'Philosophical and ethical debate, potentially leading to shifts in legal frameworks if a consensus for user freedom gains sufficient political traction.',
    'If user freedom is prioritized, the constraint''s legitimacy would be undermined, leading to reclassification towards a snare or piton from the user''s perspective, and a push for open-source or public-domain software models.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_rights_vs_freedom_imperative, conceptual, 'Fundamental conceptual conflict over the nature of software and digital rights.').

omega_variable(
    property_rights_vs_commons_governance,
    'Is software best governed by exclusive property rights, or as a digital commons requiring collective management?',
    'Empirical comparison of innovation rates and societal benefits under different governance models (proprietary vs. open source vs. hybrid commons models), alongside policy choices regarding public funding and licensing.',
    'If a commons model proves more beneficial, the property rights framework would be seen as an inefficient or extractive mechanism, leading to policy shifts and reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_rights_vs_commons_governance, empirical, 'Debate over optimal governance model for software.').

omega_variable(
    investment_incentive_necessity,
    'To what extent are strong property rights truly necessary to incentivize software investment, versus other models like public funding, patronage, or service-based revenue?',
    'Economic studies comparing investment and innovation in sectors with varying IP protections, and analysis of successful open-source business models.',
    'If strong IP is found to be less critical than claimed, the ''coordination function'' justification for extraction would weaken, pushing the constraint closer to a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investment_incentive_necessity, empirical, 'Empirical necessity of strong IP for software investment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1980, software_control_legitimacy__property_rights_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(soft_tr_t1990, software_control_legitimacy__property_rights_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(soft_tr_t2000, software_control_legitimacy__property_rights_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(soft_tr_t2010, software_control_legitimacy__property_rights_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(soft_tr_t2024, software_control_legitimacy__property_rights_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t1980, software_control_legitimacy__property_rights_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(soft_be_t1990, software_control_legitimacy__property_rights_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(soft_be_t2000, software_control_legitimacy__property_rights_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(soft_be_t2010, software_control_legitimacy__property_rights_reading, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__property_rights_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1980, software_control_legitimacy__property_rights_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(soft_su_t1990, software_control_legitimacy__property_rights_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(soft_su_t2000, software_control_legitimacy__property_rights_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(soft_su_t2010, software_control_legitimacy__property_rights_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(soft_su_t2024, software_control_legitimacy__property_rights_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, digital_rights_management_enforcement).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_patent_litigation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
