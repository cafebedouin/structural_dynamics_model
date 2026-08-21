% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Derivative Work Trigger (Broad Copyleft Reading)
 *   domain: Software Licensing / Copyright Law / Open Source Governance
 *
 * SUMMARY:
 *   This constraint represents the 'broad copyleft reading' of the GPL's
 *   derivative work clause, asserting that linking (even dynamically) to
 *   GPL-licensed code creates a derivative work, thereby triggering the
 *   obligation to disclose source code under GPL terms. This interpretation
 *   is central to the Free Software movement's strategy for expanding the
 *   software commons but is highly contested by proprietary software vendors.
 *   This story instantiates one specific reading of the
 *   'gpl_derivative_work_trigger' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.8).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.9).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Derivative Work Trigger (Broad Copyleft Reading)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "Software Licensing / Copyright Law / Open Source Governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '6528add9-04b2-4e1c-84ed-290a5d9ae7b4').
narrative_ontology:cs_kernel_codification('6528add9-04b2-4e1c-84ed-290a5d9ae7b4', fixed_text).
narrative_ontology:cs_authority_grounding('6528add9-04b2-4e1c-84ed-290a5d9ae7b4', lineage).
narrative_ontology:cs_interpretation_layer_present('6528add9-04b2-4e1c-84ed-290a5d9ae7b4').
narrative_ontology:cs_reading_relation('6528add9-04b2-4e1c-84ed-290a5d9ae7b4', gpl_derivative_work_trigger__narrow_linking_permissive_reading, forecloses).
narrative_ontology:cs_reading_relation('6528add9-04b2-4e1c-84ed-290a5d9ae7b4', gpl_derivative_work_trigger__interface_boundary_reading, forecloses).
narrative_ontology:cs_axiom('6528add9-04b2-4e1c-84ed-290a5d9ae7b4', foundational, linking_creates_derivative_work).
narrative_ontology:cs_axiom_status(linking_creates_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('6528add9-04b2-4e1c-84ed-290a5d9ae7b4', linking_creates_derivative_work, empirically_contingent).
narrative_ontology:cs_axiom('6528add9-04b2-4e1c-84ed-290a5d9ae7b4', foundational, software_freedom_requires_copyleft).
narrative_ontology:cs_axiom_status(software_freedom_requires_copyleft, holdable).
narrative_ontology:cs_axiom_grounding('6528add9-04b2-4e1c-84ed-290a5d9ae7b4', software_freedom_requires_copyleft, deontological).
narrative_ontology:cs_reference_frame('6528add9-04b2-4e1c-84ed-290a5d9ae7b4', fsf_copyleft_doctrine).
narrative_ontology:cs_drift_state('6528add9-04b2-4e1c-84ed-290a5d9ae7b4', contemporary_legal_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6528add9-04b2-4e1c-84ed-290a5d9ae7b4', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_community).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and benefits from the broad interpretation of derivative works, ensuring that code linked to GPL projects remains open. They actively monitor compliance and initiate enforcement actions.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_community, agenda_setter,
    organized, generational, mobile, global).

% Benefit from the increased availability of source code and the freedom to modify and distribute software, as the broad interpretation pulls more code into the open-source commons.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users, beneficiary,
    powerless, biographical, constrained, global).

% Bear significant compliance costs, legal risks, or re-engineering expenses to avoid triggering the GPL's copyleft obligations when linking to GPL-licensed libraries or components. They often seek to minimize the scope of 'derivative work'.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Individual developers or small teams working on commercial projects face the direct burden of understanding and complying with the broad interpretation, often without the legal resources of larger vendors.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_developers, payer,
    moderate, biographical, constrained, global).

% Organizations like the Free Software Foundation (FSF) actively interpret and enforce the GPL, promoting the broad copyleft reading through legal guidance, licensing education, and direct enforcement actions.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_enforcement_organizations, agenda_setter,
    institutional, generational, analytical, global).

% Advise clients on GPL compliance, navigating the complexities of 'derivative work' definitions. Their expertise is in high demand due to the ongoing contestation and high stakes.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, software_lawyers, observer,
    institutional, biographical, mobile, global).

% Argue that clean API boundaries should prevent derivative work status, even with tight coupling. Their position is directly contradicted by this broad reading, effectively excluding their interpretation from being legally viable within this framework.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, interface_boundary_advocates, excluded,
    organized, biographical, constrained, global).

% Advocate for interpretations where linking is generally considered aggregation, not derivation, thus not triggering copyleft. This reading forecloses their preferred interpretation, forcing them to operate under more restrictive terms or avoid GPL code.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, permissive_linking_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that software built upon GPL-licensed code remains free and open, fostering a commons of shared software by requiring source disclosure for linked works.
% TRANSFER_FUNCTION: Transfers the obligation to disclose source code (and the associated intellectual property rights) from proprietary developers to the public domain (or under GPL terms) when linking to GPL code.
% ABSENT_VOICES: Proprietary software vendors and commercial developers who prefer to keep their code closed, and those who advocate for more permissive linking models, are structurally excluded from shaping this interpretation.
% DISAPPEARANCE_RATIONALE: If this broad interpretation vanished overnight, proprietary software would freely link to GPL code without obligation, fragmenting the open-source commons and reducing the availability of source code for users, fundamentally reorganizing the open-source software ecosystem.
% FOUNDING_PROBLEM: Preventing proprietary enclosure of software derived from publicly shared code, ensuring software freedom and user rights by extending copyleft obligations to linked works.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and other open-source advocacy groups consistently attest to the ongoing need for strong copyleft to protect software freedom. Legal scholars and some users corroborate the importance of this interpretation for maintaining the open-source ecosystem against commercial enclosure.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because proprietary developers face substantial costs (re-engineering, legal compliance, or loss of proprietary IP) to comply with this interpretation. Suppression is very high (0.9) due to the legal enforceability of copyright law and the active enforcement efforts by organizations like the FSF. Resistance is also high (0.7) as proprietary interests continuously challenge this interpretation in courts and through lobbying. Accessibility collapse is moderate (0.6) as alternatives exist (e.g., using non-GPL libraries, rewriting code, or acquiring proprietary licenses), but they are often costly. Theater ratio is low (0.1) because the enforcement of this interpretation is genuine and impactful, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the open-source community, this constraint is a vital mechanism for software freedom and coordination, ensuring the commons grows. From the perspective of proprietary developers, it is a highly extractive snare that forces them to choose between abandoning valuable IP or incurring significant costs. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The open-source community and downstream users are clear beneficiaries, gaining access to more source code. Proprietary software vendors and commercial developers are the primary targets, bearing the costs of compliance or avoidance. GPL enforcement organizations act as agenda-setters, actively promoting and enforcing this interpretation. Software lawyers observe and advise, benefiting from the complexity. Advocates for alternative interpretations are excluded, as their views are foreclosed by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_definition_ambiguity,
    'What constitutes a ''derivative work'' under copyright law, specifically concerning software linking, and how does this legal definition align with the GPL''s intent?',
    'Definitive court rulings in major jurisdictions or legislative clarification of copyright law regarding software linking.',
    'If a narrower definition of ''derivative work'' gains legal precedence, the extractiveness and suppression of this reading would decrease, potentially reclassifying it towards a Rope or even a Piton if enforcement becomes theatrical. If the broad reading is universally affirmed, its status as a Tangled Rope would solidify.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_definition_ambiguity, empirical, 'Ambiguity in the legal definition of ''derivative work'' as applied to software linking.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by proprietary developers primarily structural (legal enforcement, re-engineering costs) or internalized (fear of litigation, industry norms)?',
    'Post-litigation behavior analysis: if developers continue to avoid GPL linking even after favorable rulings, internalized suppression is significant. If behavior shifts rapidly with legal changes, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as developers carry the suppression with them even in less restrictive legal environments. This would amplify the effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for proprietary developers.').

omega_variable(
    kernel_reading_validity,
    'Is this ''broad_copyleft_reading'' a valid and consistent interpretation of the GPL''s text and intent, or does it overreach beyond the original scope?',
    'Consensus among legal scholars and open-source practitioners, or a definitive statement from the original authors of the GPL that clarifies the intended scope of ''derivative work'' in linking scenarios.',
    'If deemed an overreach, the legitimacy of this reading would erode, potentially shifting its classification towards a Snare (if still enforced coercively) or a Piton (if enforcement becomes theatrical). If widely affirmed, its legitimacy as a Tangled Rope would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_validity, conceptual, 'Conceptual validity of the broad copyleft reading within the GPL framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 1991, 0.15).
narrative_ontology:measurement(gpl__tr_t1998, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(gpl__tr_t2005, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(gpl__tr_t2012, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2012, 0.09).
narrative_ontology:measurement(gpl__tr_t2018, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(gpl__tr_t2024, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 1991, 0.65).
narrative_ontology:measurement(gpl__be_t1998, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 1998, 0.7).
narrative_ontology:measurement(gpl__be_t2005, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(gpl__be_t2012, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2012, 0.78).
narrative_ontology:measurement(gpl__be_t2018, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2018, 0.79).
narrative_ontology:measurement(gpl__be_t2024, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 1991, 0.7).
narrative_ontology:measurement(gpl__su_t1998, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 1998, 0.78).
narrative_ontology:measurement(gpl__su_t2005, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2005, 0.85).
narrative_ontology:measurement(gpl__su_t2012, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2012, 0.88).
narrative_ontology:measurement(gpl__su_t2018, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2018, 0.89).
narrative_ontology:measurement(gpl__su_t2024, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
