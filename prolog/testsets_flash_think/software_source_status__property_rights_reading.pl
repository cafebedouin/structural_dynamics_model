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
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Proprietary Software Licensing as Property Right
 *   domain: intellectual_property/software_development/economic
 *
 * SUMMARY:
 *   This constraint story instantiates the 'property_rights_reading' of the
 *   'software_source_status' kernel. It frames software as a form of
 *   intellectual property, granting creators the legitimate right to restrict
 *   access and modification. This perspective emphasizes ownership and
 *   control as fundamental to incentivizing innovation and commercialization.
 *   The constraint's operation is characterized by active legal and technical
 *   enforcement of licensing terms, leading to significant extraction from
 *   users and developers who desire more freedom or access.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.7).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.78).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Proprietary Software Licensing as Property Right").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "intellectual_property/software_development/economic").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, 'eb4f7841-ea71-4878-a0c4-be390da2b267').
narrative_ontology:cs_kernel_codification('eb4f7841-ea71-4878-a0c4-be390da2b267', formalized).
narrative_ontology:cs_authority_grounding('eb4f7841-ea71-4878-a0c4-be390da2b267', lineage).
narrative_ontology:cs_interpretation_layer_present('eb4f7841-ea71-4878-a0c4-be390da2b267').
narrative_ontology:cs_reading_relation('eb4f7841-ea71-4878-a0c4-be390da2b267', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('eb4f7841-ea71-4878-a0c4-be390da2b267', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb4f7841-ea71-4878-a0c4-be390da2b267', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('eb4f7841-ea71-4878-a0c4-be390da2b267', foundational, software_is_private_property).
narrative_ontology:cs_axiom_status(software_is_private_property, holdable).
narrative_ontology:cs_axiom_grounding('eb4f7841-ea71-4878-a0c4-be390da2b267', software_is_private_property, deontological).
narrative_ontology:cs_axiom('eb4f7841-ea71-4878-a0c4-be390da2b267', secondary, incentive_for_creation_requires_control).
narrative_ontology:cs_axiom_status(incentive_for_creation_requires_control, holdable).
narrative_ontology:cs_axiom_grounding('eb4f7841-ea71-4878-a0c4-be390da2b267', incentive_for_creation_requires_control, instrumental).
narrative_ontology:cs_reference_frame('eb4f7841-ea71-4878-a0c4-be390da2b267', traditional_intellectual_property_framework).
narrative_ontology:cs_drift_state('eb4f7841-ea71-4878-a0c4-be390da2b267', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb4f7841-ea71-4878-a0c4-be390da2b267', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_software_creators).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, software_publishers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, independent_developers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, academic_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or teams who develop software and assert their intellectual property rights, restricting access and modification to control distribution and monetization. They benefit directly from the legal framework that protects their creations.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_software_creators, agenda_setter,
    powerful, biographical, mobile, global).

% Companies that acquire, market, and distribute proprietary software under license. They leverage IP law to build business models around exclusive rights, collecting revenue from sales and subscriptions. Their power derives from market position and legal enforcement.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_publishers, beneficiary,
    institutional, generational, arbitrage, global).

% Individuals and organizations who use proprietary software under restrictive licenses. They pay for access and are legally bound by terms that limit their ability to inspect, modify, or share the software, often without full understanding of the underlying code.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_users, payer,
    moderate, immediate, constrained, global).

% Developers who may wish to build upon, integrate with, or learn from existing software, but are prevented from doing so by proprietary licenses. They face legal barriers to innovation and competition, often forced to 'reinvent the wheel' or work around restrictions.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Researchers who need access to software's internal workings for study, verification, or to build new scientific tools. Proprietary restrictions impede their ability to conduct transparent, reproducible research and contribute to public knowledge.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, academic_researchers, payer,
    moderate, generational, constrained, global).

% Groups and individuals who argue for software freedom as an ethical imperative, advocating for open access and modification rights. They are structurally excluded from the legal and economic frameworks that define proprietary software, operating in an adversarial relationship to it.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, free_software_advocates, excluded,
    organized, civilizational, identity_locked, global).

% The body of laws, courts, and enforcement mechanisms that define and protect intellectual property rights for software. It adjudicates disputes, enforces licenses, and provides the foundational framework for the proprietary software model.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, proprietary_software_creators).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Incentivizes the creation and commercialization of complex software by granting creators exclusive rights, thereby coordinating investment and innovation in a market-driven framework.
% TRANSFER_FUNCTION: Transfers control over software's use, modification, and distribution, as well as economic value (revenue), from users and independent developers to proprietary software creators and publishers.
% ABSENT_VOICES: Free software advocates, users demanding greater control over their digital tools, and academic researchers seeking open access for verification and advancement of knowledge are largely excluded from the policy-making processes that shape IP law.
% DISAPPEARANCE_RATIONALE: If software intellectual property rights vanished overnight, the entire proprietary software industry would collapse. Business models based on licensing, subscriptions, and restricted access would become untenable, leading to a fundamental reorganization of how software is created, distributed, and monetized globally.
% FOUNDING_PROBLEM: To protect the significant investment of time, effort, and capital required to develop complex software, ensuring creators could recoup costs and profit, thereby incentivizing continued innovation in a digital environment where copying is trivial.
% FOUNDING_PROBLEM_CORROBORATION: Industry bodies, legal scholars specializing in IP, and economic studies often corroborate the ongoing need for IP protection to incentivize innovation. However, critics (e.g., free software advocates, some economists) contest the extent to which current IP regimes are optimal for innovation or public welfare, suggesting the problem is 'contested' from other readings.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.70) because the constraint enables creators and publishers to capture substantial economic value by restricting access and modification, often beyond the direct cost of development. Suppression is also high (0.78) due to robust legal frameworks (copyright, patents, trade secrets) and technical measures (DRM, obfuscation) that actively prevent unauthorized use or modification. Theater ratio is low (0.10) as the enforcement mechanisms are genuinely functional in maintaining the proprietary model, not merely performative. Accessibility collapse is moderate-high (0.65) because while open-source alternatives exist, they often lack the market penetration or specific functionalities of proprietary solutions, making exit costly or impractical for many users and developers. Resistance is moderate (0.55) from the active free software movement and ongoing legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of proprietary software creators and publishers, this constraint is a legitimate and necessary 'rope' that coordinates innovation and investment. From the perspective of software users, independent developers, and academic researchers, it operates as a 'snare' or 'tangled rope,' extracting value and restricting fundamental freedoms. The legal system, as an agenda-setter, largely upholds the property rights framing, but faces increasing pressure from competition authorities and public interest groups.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software creators and publishers are clear beneficiaries, directly profiting from the exclusive rights granted by this constraint. Software users, independent developers, and academic researchers are targets, bearing the costs of restricted access, lack of modification rights, and licensing fees. Free software advocates are structurally excluded, their alternative vision directly suppressed by the constraint's enforcement. The legal system acts as an agenda-setter, enforcing the rules that define these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to incentivize creation is still considered 'live' by its beneficiaries, preventing a full mandatrophy resolution. However, the high and increasing extractiveness and suppression, coupled with significant resistance, suggest that the coordination function is increasingly overshadowed by rent-seeking. The 'tangled_rope' classification reflects this hybrid nature, acknowledging the claimed coordination while highlighting the asymmetric extraction and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_rights_vs_public_good,
    'Is software fundamentally a private good subject to exclusive property rights, or does it possess characteristics of a public good that should be freely accessible and modifiable?',
    'Philosophical and legal re-evaluation of software''s ontological status, potentially influenced by shifts in public policy or international treaties that prioritize access over exclusive control.',
    'If reclassified as a public good, the constraint''s extractiveness would be deemed illegitimate, leading to calls for its dismantling or radical restructuring. If reaffirmed as a private good, the current framework''s legitimacy would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_rights_vs_public_good, conceptual, 'Fundamental conceptual framing of software''s nature.').

omega_variable(
    innovation_incentive_efficacy,
    'To what extent do strong proprietary rights genuinely incentivize innovation, versus merely enabling rent extraction and stifling follow-on innovation?',
    'Longitudinal empirical studies comparing innovation rates and quality in proprietary vs. open-source ecosystems, and economic analyses of the optimal duration and scope of IP protection.',
    'If evidence suggests that strong IP rights primarily enable extraction without commensurate innovation, the justification for the constraint weakens, potentially leading to policy reforms that reduce its suppressive and extractive elements. If the incentive effect is robust, the current framework''s rationale is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_efficacy, empirical, 'Empirical effectiveness of IP as an innovation driver.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal barriers, DRM) or internalized (developers'' self-censorship, users'' acceptance of EULAs as inevitable)?',
    'Post-exit suppression trajectory: if developers and users continue to self-restrict even after legal barriers are removed (e.g., in jurisdictions with weaker IP enforcement), it suggests a significant internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit, making reform efforts more challenging. If purely structural, legal changes would be more immediately effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1980, software_source_status__property_rights_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(soft_tr_t1990, software_source_status__property_rights_reading, theater_ratio, 1990, 0.07).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__property_rights_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__property_rights_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(soft_tr_t2024, software_source_status__property_rights_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t1980, software_source_status__property_rights_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(soft_be_t1990, software_source_status__property_rights_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(soft_be_t2000, software_source_status__property_rights_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(soft_be_t2010, software_source_status__property_rights_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(soft_be_t2024, software_source_status__property_rights_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1980, software_source_status__property_rights_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(soft_su_t1990, software_source_status__property_rights_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(soft_su_t2000, software_source_status__property_rights_reading, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(soft_su_t2010, software_source_status__property_rights_reading, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(soft_su_t2024, software_source_status__property_rights_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
