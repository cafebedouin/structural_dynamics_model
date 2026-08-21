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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Proprietary Software Licensing (Property Rights Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'property rights' reading of software's
 *   status, asserting that software is a form of intellectual property and
 *   its creators have legitimate rights to restrict access and modification.
 *   This reading underpins the proprietary software industry, where licensing
 *   restrictions are seen as a natural exercise of ownership. It is one of
 *   several competing readings of the 'software_source_status' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.78).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.7).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Proprietary Software Licensing (Property Rights Reading)").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, '3b9863a3-38c9-4f9c-b157-df8112f601bd').
narrative_ontology:cs_kernel_codification('3b9863a3-38c9-4f9c-b157-df8112f601bd', formalized).
narrative_ontology:cs_authority_grounding('3b9863a3-38c9-4f9c-b157-df8112f601bd', lineage).
narrative_ontology:cs_interpretation_layer_present('3b9863a3-38c9-4f9c-b157-df8112f601bd').
narrative_ontology:cs_reading_relation('3b9863a3-38c9-4f9c-b157-df8112f601bd', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('3b9863a3-38c9-4f9c-b157-df8112f601bd', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b9863a3-38c9-4f9c-b157-df8112f601bd', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('3b9863a3-38c9-4f9c-b157-df8112f601bd', foundational, software_is_private_property).
narrative_ontology:cs_axiom_status(software_is_private_property, holdable).
narrative_ontology:cs_axiom_grounding('3b9863a3-38c9-4f9c-b157-df8112f601bd', software_is_private_property, deontological).
narrative_ontology:cs_axiom('3b9863a3-38c9-4f9c-b157-df8112f601bd', foundational, exclusive_rights_incentivize_creation).
narrative_ontology:cs_axiom_status(exclusive_rights_incentivize_creation, holdable).
narrative_ontology:cs_axiom_grounding('3b9863a3-38c9-4f9c-b157-df8112f601bd', exclusive_rights_incentivize_creation, instrumental).
narrative_ontology:cs_reference_frame('3b9863a3-38c9-4f9c-b157-df8112f601bd', exclusive_rights_framework).
narrative_ontology:cs_drift_state('3b9863a3-38c9-4f9c-b157-df8112f601bd', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3b9863a3-38c9-4f9c-b157-df8112f601bd', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, software_creators).
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

% Individuals and companies who develop software and claim exclusive rights over its distribution, modification, and use. They set licensing terms and benefit directly from sales and restricted access.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_creators, agenda_setter,
    institutional, generational, arbitrage, global).

% Companies that market, distribute, and license software created by others or in-house. They profit from the enforcement of proprietary rights, enabling their business model.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_publishers, beneficiary,
    institutional, biographical, mobile, global).

% Individuals and organizations who purchase or license proprietary software. They pay for access and are restricted by licensing terms from modifying, sharing, or often even fully understanding the software they use.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_users, payer,
    moderate, immediate, constrained, global).

% Developers who wish to build upon, integrate with, or modify existing software. Proprietary restrictions often limit their ability to innovate or compete, forcing them to license or develop from scratch.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Researchers who need access to source code for scientific analysis, verification, or to build new knowledge. Proprietary restrictions often impede their work, forcing them to use less suitable open-source alternatives or navigate complex legal hurdles.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, academic_researchers, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, academic_researchers, excluded).

% The body of laws, courts, and enforcement agencies that define and uphold intellectual property rights for software, including copyright, patent, and trade secret law. They provide the framework and enforcement mechanisms for the constraint.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Organizations and individuals who argue for software freedom and open access to source code. While they operate outside this constraint's direct enforcement, their arguments represent a fundamental challenge to its legitimacy and are actively suppressed in policy debates.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, open_source_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, software_publishers).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal and economic framework that incentivizes investment in software development by granting creators exclusive rights, thereby fostering a commercial software market.
% TRANSFER_FUNCTION: Transfers control over software's use, modification, and distribution from the general public to creators and publishers, and transfers revenue from users to these rights holders.
% ABSENT_VOICES: Users demanding the right to repair or modify their software, independent developers seeking interoperability without licensing burdens, and open-source advocates who view proprietary software as an ethical problem. These voices are often marginalized in policy discussions dominated by industry interests.
% DISAPPEARANCE_RATIONALE: If intellectual property rights for software vanished overnight, the entire commercial software industry, built on licensing and restricted access, would undergo a radical transformation. Business models would collapse, investment incentives would shift dramatically, and the digital economy would reorganize around new paradigms of creation and distribution.
% FOUNDING_PROBLEM: To protect the significant financial and creative investment required for software development, preventing unauthorized copying and modification that would undermine creators' ability to recoup costs and profit.
% FOUNDING_PROBLEM_CORROBORATION: Software industry associations, legal scholars specializing in intellectual property, and venture capitalists consistently attest that strong IP protection remains crucial for incentivizing innovation and investment in the software sector. This corroboration comes from parties whose interests align with the constraint, but is widely accepted in legal and economic discourse.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because the constraint enables creators and publishers to capture significant value through licensing fees and control over functionality, often exceeding the marginal cost of distribution or support. Suppression is also high (0.70) due to robust legal frameworks (copyright, patent, trade secret law) and technical enforcement (DRM, obfuscation) that actively prevent unauthorized access or modification. The theater ratio is low (0.10) as the enforcement mechanisms are highly functional and directly serve the purpose of maintaining proprietary control, with little performative overhead. Accessibility collapse is moderate (0.60) as open-source alternatives exist, but often require significant effort to adopt or lack the specific features of proprietary solutions. Resistance is moderate (0.45) from open-source movements and user rights advocates, but this resistance has not fundamentally altered the core proprietary model.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of software creators and publishers, this constraint is a legitimate and necessary 'rope' that coordinates investment and innovation by protecting their intellectual property. From the perspective of users, independent developers, and academic researchers, it often functions as a 'snare' or 'tangled rope,' extracting value and suppressing freedom to use and modify software, with limited recourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Software creators and publishers are clear beneficiaries, collecting revenue and controlling the market (low directionality). Software users, independent developers, and academic researchers are targets, bearing costs and facing restrictions (high directionality). The legal system acts as an agenda-setter, enforcing the rules that enable this flow. Open-source advocates are largely excluded from the formal mechanisms of this constraint, their arguments often suppressed in favor of proprietary interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_definition_ambiguity,
    'Is ''intellectual property'' an appropriate conceptual framework for non-rivalrous digital goods like software, or does it create artificial scarcity and rent-seeking?',
    'Conceptual analysis comparing the economic characteristics of physical property with digital goods, and empirical studies on the impact of different IP regimes on innovation and access.',
    'If ''property'' is deemed an ill-fitting metaphor, the foundational legitimacy of this reading would be undermined, potentially reclassifying it as a pure ''snare'' built on a conceptual error. If deemed appropriate, its ''tangled_rope'' classification would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_definition_ambiguity, conceptual, 'The conceptual fit of ''property'' to software.').

omega_variable(
    innovation_incentive_efficacy,
    'To what extent do strong proprietary rights genuinely incentivize innovation and investment in software, versus merely enabling market dominance and extraction?',
    'Longitudinal economic studies comparing innovation rates and investment levels in sectors with varying IP protection strengths, and analysis of the impact of open-source models on innovation.',
    'If the incentive effect is weak or outweighed by negative impacts on competition and follow-on innovation, the ''coordination'' aspect of this ''tangled_rope'' would diminish, pushing it closer to a ''snare''. If strong, the coordination function would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_efficacy, empirical, 'The empirical efficacy of IP as an innovation incentive.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal barriers, DRM) or internalized (developers/users accepting proprietary norms as inevitable)?',
    'Post-exit suppression trajectory: if developers/users continue to avoid modifying proprietary software even when legal/technical barriers are removed (e.g., after a patent expires or DRM is cracked), it suggests internalized suppression. If they immediately engage in modification, it''s structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. This would amplify the extractive nature of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1980, software_source_status__property_rights_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(soft_tr_t1990, software_source_status__property_rights_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__property_rights_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__property_rights_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(soft_tr_t2020, software_source_status__property_rights_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(soft_tr_t2025, software_source_status__property_rights_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t1980, software_source_status__property_rights_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(soft_be_t1990, software_source_status__property_rights_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(soft_be_t2000, software_source_status__property_rights_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(soft_be_t2010, software_source_status__property_rights_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(soft_be_t2020, software_source_status__property_rights_reading, base_extractiveness, 2020, 0.77).
narrative_ontology:measurement(soft_be_t2025, software_source_status__property_rights_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1980, software_source_status__property_rights_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(soft_su_t1990, software_source_status__property_rights_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(soft_su_t2000, software_source_status__property_rights_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(soft_su_t2010, software_source_status__property_rights_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(soft_su_t2020, software_source_status__property_rights_reading, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement(soft_su_t2025, software_source_status__property_rights_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, digital_rights_management_enforcement).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_licensing_agreements).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, open_source_licensing_models).

% DUAL FORMULATION NOTE:
% This constraint is the 'property_rights_reading' of the 'software_source_status' kernel. It is one of four distinct readings, each modeled as a separate constraint, linked here to reflect their shared conceptual domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
