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
 *   human_readable: Software as Property: Creator's Rights to Control
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.65).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.7).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software as Property: Creator's Rights to Control").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, 'a6662e8d-e712-4078-814c-7d4aba66cb2a').
narrative_ontology:cs_kernel_codification('a6662e8d-e712-4078-814c-7d4aba66cb2a', formalized).
narrative_ontology:cs_authority_grounding('a6662e8d-e712-4078-814c-7d4aba66cb2a', lineage).
narrative_ontology:cs_interpretation_layer_present('a6662e8d-e712-4078-814c-7d4aba66cb2a').
narrative_ontology:cs_reading_relation('a6662e8d-e712-4078-814c-7d4aba66cb2a', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('a6662e8d-e712-4078-814c-7d4aba66cb2a', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6662e8d-e712-4078-814c-7d4aba66cb2a', software_control_legitimacy__commons_reading, forecloses).
narrative_ontology:cs_axiom('a6662e8d-e712-4078-814c-7d4aba66cb2a', foundational, intellectual_labor_creates_property).
narrative_ontology:cs_axiom_status(intellectual_labor_creates_property, holdable).
narrative_ontology:cs_axiom_grounding('a6662e8d-e712-4078-814c-7d4aba66cb2a', intellectual_labor_creates_property, deontological).
narrative_ontology:cs_axiom('a6662e8d-e712-4078-814c-7d4aba66cb2a', foundational, exclusivity_incentivizes_creation).
narrative_ontology:cs_axiom_status(exclusivity_incentivizes_creation, holdable).
narrative_ontology:cs_axiom_grounding('a6662e8d-e712-4078-814c-7d4aba66cb2a', exclusivity_incentivizes_creation, instrumental).
narrative_ontology:cs_reference_frame('a6662e8d-e712-4078-814c-7d4aba66cb2a', classical_property_doctrine).
narrative_ontology:cs_drift_state('a6662e8d-e712-4078-814c-7d4aba66cb2a', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a6662e8d-e712-4078-814c-7d4aba66cb2a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, investors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, users).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, independent_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Companies that create and sell proprietary software. They rely on intellectual property laws to restrict use, modification, and distribution, protecting their investment and enabling commercial sustainability. They actively lobby for stronger IP protections.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Entities that fund software development. They expect a return on investment, which is largely predicated on the ability of software vendors to control and monetize their creations through property rights.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, investors, beneficiary,
    powerful, generational, mobile, global).

% Individuals who purchase and use software. They are restricted by licenses from modifying, distributing, or sometimes even fully understanding the software they 'own'. They bear the cost of these restrictions in terms of reduced freedom and potential vendor lock-in.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, users, payer,
    powerless, biographical, constrained, global).

% Software developers who operate outside large corporate structures. They often find their ability to innovate, build upon existing code, or compete with established players constrained by proprietary licenses and the legal enforcement of software property rights.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Organizations and individuals who advocate for Free and Open Source Software. They view proprietary control as an ethical and practical impediment to user freedom and collaborative innovation, bearing the cost of a less open digital ecosystem.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    organized, generational, identity_locked, global).

% The legislative and judicial bodies that define, interpret, and enforce intellectual property laws, including those pertaining to software. They provide the formal framework that grants creators the authority to restrict software use.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, legal_systems, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal and economic framework that incentivizes investment in software creation by guaranteeing creators control over their work, thereby enabling commercial sustainability and the production of complex, high-quality software.
% TRANSFER_FUNCTION: Transfers control over software use, modification, and distribution from potential users and developers to the original creators and their investors. This enables the extraction of economic value (e.g., license fees, subscription revenue) from users and downstream developers.
% ABSENT_VOICES: Users demanding full control over their purchased software, developers seeking to freely build upon existing codebases, and communities advocating for software as a public good are often marginalized in policy discussions dominated by IP holders and their legal representatives.
% DISAPPEARANCE_RATIONALE: If software property rights vanished overnight, the commercial software industry as currently structured would collapse. Investment models would shift dramatically, and a new ecosystem based on different incentives (e.g., service models, patronage, public funding) would emerge, fundamentally reorganizing the digital economy.
% FOUNDING_PROBLEM: The core problem this arrangement was built to solve was how to incentivize the creation of complex software, which requires significant upfront investment, in an environment where digital copying is trivial and unauthorized distribution is easy, threatening creators' ability to recoup costs and profit.
% FOUNDING_PROBLEM_CORROBORATION: Software industry associations and investors consistently attest to the ongoing need for strong IP protection to fund innovation and maintain commercial viability. While critics (FOSS advocates, some economists) argue that alternative models exist and the problem is overblown, the fundamental challenge of incentivizing complex digital creation remains a live concern for many stakeholders.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_rights_scope_ambiguity,
    'Is software truly analogous to physical property, or does its digital, non-rivalrous nature require a fundamentally different legal and economic framework for control and ownership?',
    'Philosophical and legal re-evaluation of property theory in the context of digital goods, potentially leading to new legal precedents or legislative frameworks that distinguish digital from physical property.',
    'If software is deemed fundamentally different from physical property, the foundational axioms of this reading would be challenged, potentially leading to a reclassification towards a more ''commons'' or ''freedom'' oriented constraint, with significantly lower extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_rights_scope_ambiguity, conceptual, 'Ambiguity regarding the applicability of traditional property rights to digital software.').

omega_variable(
    incentive_structure_efficacy,
    'Is strong intellectual property protection the *only* or *most efficient* way to incentivize software creation, or do alternative models (e.g., open source, patronage, public funding) offer comparable or superior outcomes for innovation and societal benefit?',
    'Empirical studies comparing innovation rates, software quality, and economic impact across different licensing and funding models (proprietary vs. open source, public vs. private investment) over extended periods.',
    'If empirical evidence strongly supports the efficacy of alternative models, the instrumental axiom ''exclusivity_incentivizes_creation'' would be undermined, weakening the justification for the constraint''s extractiveness and suppression, potentially shifting its classification towards a Snare or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_structure_efficacy, empirical, 'Whether IP protection is the optimal incentive for software creation.').

omega_variable(
    enforcement_cost_vs_benefit,
    'Does the societal cost of enforcing software property rights (e.g., legal battles, DRM development, restricted innovation, vendor lock-in) outweigh the benefits derived from incentivized creation?',
    'Comprehensive economic and social impact assessments that quantify both the direct and indirect costs and benefits of the current IP regime for software, including externalities on innovation and competition.',
    'If the costs are found to outweigh the benefits, the constraint''s overall legitimacy would be severely challenged, leading to strong pressure for reform and a re-evaluation of its claimed coordination function, likely increasing its perceived extractiveness and potentially reclassifying it as a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_cost_vs_benefit, empirical, 'Assessment of the net societal impact of software IP enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1990, software_control_legitimacy__property_rights_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(soft_tr_t1996, software_control_legitimacy__property_rights_reading, theater_ratio, 1996, 0.12).
narrative_ontology:measurement(soft_tr_t2002, software_control_legitimacy__property_rights_reading, theater_ratio, 2002, 0.14).
narrative_ontology:measurement(soft_tr_t2008, software_control_legitimacy__property_rights_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(soft_tr_t2014, software_control_legitimacy__property_rights_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(soft_tr_t2020, software_control_legitimacy__property_rights_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(soft_be_t1990, software_control_legitimacy__property_rights_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(soft_be_t1996, software_control_legitimacy__property_rights_reading, base_extractiveness, 1996, 0.55).
narrative_ontology:measurement(soft_be_t2002, software_control_legitimacy__property_rights_reading, base_extractiveness, 2002, 0.6).
narrative_ontology:measurement(soft_be_t2008, software_control_legitimacy__property_rights_reading, base_extractiveness, 2008, 0.63).
narrative_ontology:measurement(soft_be_t2014, software_control_legitimacy__property_rights_reading, base_extractiveness, 2014, 0.64).
narrative_ontology:measurement(soft_be_t2020, software_control_legitimacy__property_rights_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1990, software_control_legitimacy__property_rights_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(soft_su_t1996, software_control_legitimacy__property_rights_reading, suppression_requirement, 1996, 0.6).
narrative_ontology:measurement(soft_su_t2002, software_control_legitimacy__property_rights_reading, suppression_requirement, 2002, 0.65).
narrative_ontology:measurement(soft_su_t2008, software_control_legitimacy__property_rights_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(soft_su_t2014, software_control_legitimacy__property_rights_reading, suppression_requirement, 2014, 0.69).
narrative_ontology:measurement(soft_su_t2020, software_control_legitimacy__property_rights_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
