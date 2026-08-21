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
 *   human_readable: Software as Proprietary Intellectual Property (Property Rights Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the 'property_rights_reading' of the
 *   'software_source_status' kernel. It posits that software is a form of
 *   intellectual property, granting creators the legitimate right to restrict
 *   access and modification. This reading emphasizes ownership and
 *   contractual agreements, viewing users primarily as consumers. The core of
 *   this constraint is the legal and commercial framework that enables
 *   proprietary software, with its associated licensing restrictions and
 *   enforcement mechanisms. This is one of several competing readings of the
 *   fundamental status of software.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.65).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.7).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Software as Proprietary Intellectual Property (Property Rights Reading)").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, '3be5ee36-ad48-40a9-96ee-a337d8a25cc2').
narrative_ontology:cs_kernel_codification('3be5ee36-ad48-40a9-96ee-a337d8a25cc2', formalized).
narrative_ontology:cs_authority_grounding('3be5ee36-ad48-40a9-96ee-a337d8a25cc2', lineage).
narrative_ontology:cs_interpretation_layer_present('3be5ee36-ad48-40a9-96ee-a337d8a25cc2').
narrative_ontology:cs_reading_relation('3be5ee36-ad48-40a9-96ee-a337d8a25cc2', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('3be5ee36-ad48-40a9-96ee-a337d8a25cc2', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('3be5ee36-ad48-40a9-96ee-a337d8a25cc2', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('3be5ee36-ad48-40a9-96ee-a337d8a25cc2', foundational, software_is_intellectual_property).
narrative_ontology:cs_axiom_status(software_is_intellectual_property, holdable).
narrative_ontology:cs_axiom_grounding('3be5ee36-ad48-40a9-96ee-a337d8a25cc2', software_is_intellectual_property, conventional).
narrative_ontology:cs_axiom('3be5ee36-ad48-40a9-96ee-a337d8a25cc2', foundational, creators_have_exclusive_rights).
narrative_ontology:cs_axiom_status(creators_have_exclusive_rights, holdable).
narrative_ontology:cs_axiom_grounding('3be5ee36-ad48-40a9-96ee-a337d8a25cc2', creators_have_exclusive_rights, deontological).
narrative_ontology:cs_reference_frame('3be5ee36-ad48-40a9-96ee-a337d8a25cc2', classical_intellectual_property_regime).
narrative_ontology:cs_drift_state('3be5ee36-ad48-40a9-96ee-a337d8a25cc2', contemporary_digital_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('3be5ee36-ad48-40a9-96ee-a337d8a25cc2', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_software_companies).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, individual_software_creators).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, independent_developers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, academic_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These companies develop and distribute software under restrictive licenses, asserting full control over their source code. They benefit from exclusive rights to modification, distribution, and commercialization, which underpins their business models. They actively enforce these rights through legal means and technical measures.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_software_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Individual creators who choose to license their software restrictively benefit from the legal framework that protects their intellectual property, allowing them to monetize their work through sales or licensing fees without fear of unauthorized copying or modification. Their ability to benefit depends on effective enforcement.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, individual_software_creators, beneficiary,
    moderate, biographical, mobile, global).

% Users acquire software under terms that typically grant them only the right to use it, not to inspect, modify, or redistribute it. They pay for licenses and are subject to the creator's restrictions, with limited recourse or ability to adapt the software to their own needs. Their exit options are to switch to alternative (often proprietary) software or to do without.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_users, payer,
    powerless, immediate, constrained, global).

% Developers who wish to build upon or integrate with proprietary software often face licensing barriers, fees, or outright prohibitions on reverse engineering or modification. This limits their creative freedom and market opportunities, forcing them to work within the proprietary ecosystem or develop from scratch.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Researchers often need access to source code for verification, replication, and extension of scientific work. Proprietary restrictions hinder this process, forcing them to rely on black-box systems or to re-implement functionality, which impedes scientific progress and transparency.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, academic_researchers, payer,
    moderate, generational, constrained, global).

% These advocates argue against the proprietary model, asserting that software should be free to use, study, modify, and distribute. They are excluded from the legal and commercial frameworks that define proprietary software, operating in an alternative ecosystem and constantly challenging the legitimacy of restrictive licenses.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, free_and_open_source_advocates, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear ownership and control over software, incentivizing creation by granting exclusive rights and providing a legal framework for commercial transactions and investment in software development.
% TRANSFER_FUNCTION: Transfers economic value from users and other developers (through licensing fees, restricted access, and lack of modification rights) to software creators and companies, in exchange for access to functional software.
% ABSENT_VOICES: Advocates for software freedom and open access are structurally excluded from the legal and commercial discourse that defines and enforces proprietary rights. They would argue for a fundamental right to inspect and modify software, challenging the very premise of restrictive licensing.
% DISAPPEARANCE_RATIONALE: If the legal framework for software as intellectual property vanished overnight, the proprietary software industry would collapse. Companies would lose their primary revenue model, and a massive shift towards open-source or alternative funding models would occur, fundamentally reorganizing the software economy.
% FOUNDING_PROBLEM: The problem of incentivizing software creation and protecting creators' investments in a digital world where copying is trivial and costless.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary software companies and many individual creators attest that the problem of incentivizing creation and preventing piracy remains live. Free and open-source advocates, however, argue that alternative models (e.g., service-based, patronage) have proven viable, suggesting the 'problem' is primarily one of business model, not fundamental necessity. Legal scholars and economists outside the benefiting parties offer mixed corroboration, acknowledging the incentive function but questioning the scope and duration of current protections.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the significant economic value captured by proprietary software companies through licensing and restricted access, which often exceeds the marginal cost of distribution. Suppression (0.70) is high due to the active legal and technical enforcement (DRM, EULAs, copyright law) required to maintain these restrictions against a backdrop of easy digital copying. The theater ratio is low (0.10) because the enforcement is genuinely functional in protecting proprietary interests, not merely performative. Accessibility collapse is moderate (0.40) as open-source alternatives exist, but switching costs or feature parity issues can constrain users. Resistance (0.55) is substantial, driven by the free and open-source software movements, legal challenges, and academic critiques.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of proprietary software companies, this constraint is a necessary 'rope' that coordinates innovation and investment by protecting intellectual property. From the perspective of users and independent developers, it functions as a 'snare' or 'tangled rope,' extracting value and limiting freedom through enforced restrictions. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software companies and individual creators are clear beneficiaries, as the constraint directly enables their business models and monetization strategies. Software users, independent developers, and academic researchers are victims, bearing the costs of restricted access, lack of modification rights, and licensing fees. Free and open-source advocates are excluded, as their fundamental premises are antithetical to this reading, and they operate outside its framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (protecting proprietary rights and incentivizing creation) is actively pursued and defended by its beneficiaries. The contest is over the legitimacy and necessity of this mandate, not its atrophy. The classification as a Tangled Rope reflects the ongoing tension between its coordination function (incentivizing creation) and its extractive, suppressive aspects (restricting access and modification).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine instantiation of the ''property_rights_reading'' of the ''software_source_status'' kernel, or is it better understood as a different reading or an independent constraint?',
    'Analysis of the core normative claims and their alignment with the declared axioms of the ''property_rights_reading'' versus other readings (e.g., ''freedom_imperative_reading'', ''pragmatic_development_reading'').',
    'If it aligns with another reading, the classification and stakeholder analysis would shift to reflect that reading''s specific structural properties. If it''s an independent constraint, it would be re-indexed outside the kernel family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Verifies the correct instantiation of a kernel reading.').

omega_variable(
    incentive_vs_monopoly_power,
    'To what extent do current intellectual property protections for software genuinely incentivize innovation, versus merely granting monopoly power that enables extraction?',
    'Comparative economic studies of innovation rates and market concentration in proprietary versus open-source ecosystems, and analysis of the impact of patent and copyright duration on innovation.',
    'If protections primarily grant monopoly power, the extractiveness metric would be further justified, and the coordination function would be seen as a weaker justification for the constraint. If incentives are paramount, the ''rope'' aspect of the Tangled Rope classification would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_vs_monopoly_power, empirical, 'Distinguishes between legitimate incentive mechanisms and rent-seeking behavior.').

omega_variable(
    enforcement_necessity,
    'Is the current level of active enforcement (e.g., DRM, legal action) structurally necessary to maintain the proprietary model, or could it persist with less suppression?',
    'Analysis of historical shifts in enforcement intensity and their impact on piracy rates and proprietary market share, or counterfactual analysis of alternative enforcement regimes.',
    'If less suppression is viable, the ''suppression'' metric would be re-evaluated downwards, potentially shifting the constraint towards a ''rope'' if extraction also decreases, or highlighting the excess cost of current enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_necessity, empirical, 'Assesses the necessity and proportionality of enforcement mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__property_rights_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t10, software_source_status__property_rights_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(soft_tr_t20, software_source_status__property_rights_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(soft_tr_t30, software_source_status__property_rights_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(soft_tr_t40, software_source_status__property_rights_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__property_rights_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(soft_be_t10, software_source_status__property_rights_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(soft_be_t20, software_source_status__property_rights_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(soft_be_t30, software_source_status__property_rights_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(soft_be_t40, software_source_status__property_rights_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__property_rights_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(soft_su_t10, software_source_status__property_rights_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(soft_su_t20, software_source_status__property_rights_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(soft_su_t30, software_source_status__property_rights_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(soft_su_t40, software_source_status__property_rights_reading, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_source_status' kernel. This 'property_rights_reading' defines software as intellectual property, influencing the structural conditions and perceived legitimacy of the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
