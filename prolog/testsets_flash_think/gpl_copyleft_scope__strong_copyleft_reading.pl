% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__strong_copyleft_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Strong Copyleft Scope (Dynamic Linking / Combined Works)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the 'strong copyleft' reading of GPL Section
 *   2(b), which asserts that any work dynamically linked to or combined with
 *   GPL-licensed code must also be licensed under the GPL. This
 *   interpretation extends the 'derivative work' boundary broadly, aiming to
 *   maximize the scope of free software. It is a reading of the
 *   'gpl_copyleft_scope' kernel, with sibling readings 'narrow_scope_reading'
 *   and 'enforcement_vacuum_reading'. The claimed type is 'snare' because,
 *   from the perspective of proprietary vendors, the coordination function
 *   (expanding free software) is secondary to the coercive extraction of
 *   their intellectual property.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.85).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.9).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Strong Copyleft Scope (Dynamic Linking / Combined Works)").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, 'af8fbe9a-00e1-47bd-a051-cc40aa2a4824').
narrative_ontology:cs_kernel_codification('af8fbe9a-00e1-47bd-a051-cc40aa2a4824', fixed_text).
narrative_ontology:cs_authority_grounding('af8fbe9a-00e1-47bd-a051-cc40aa2a4824', lineage).
narrative_ontology:cs_interpretation_layer_present('af8fbe9a-00e1-47bd-a051-cc40aa2a4824').
narrative_ontology:cs_reading_relation('af8fbe9a-00e1-47bd-a051-cc40aa2a4824', gpl_copyleft_scope__narrow_scope_reading, forecloses).
narrative_ontology:cs_reading_relation('af8fbe9a-00e1-47bd-a051-cc40aa2a4824', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('af8fbe9a-00e1-47bd-a051-cc40aa2a4824', foundational, code_freedom_maximalism).
narrative_ontology:cs_axiom_status(code_freedom_maximalism, holdable).
narrative_ontology:cs_axiom_grounding('af8fbe9a-00e1-47bd-a051-cc40aa2a4824', code_freedom_maximalism, deontological).
narrative_ontology:cs_axiom('af8fbe9a-00e1-47bd-a051-cc40aa2a4824', foundational, viral_copyleft_principle).
narrative_ontology:cs_axiom_status(viral_copyleft_principle, holdable).
narrative_ontology:cs_axiom_grounding('af8fbe9a-00e1-47bd-a051-cc40aa2a4824', viral_copyleft_principle, conventional).
narrative_ontology:cs_reference_frame('af8fbe9a-00e1-47bd-a051-cc40aa2a4824', free_software_maximalism).
narrative_ontology:cs_drift_state('af8fbe9a-00e1-47bd-a051-cc40aa2a4824', contemporary_proprietary_challenge, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('af8fbe9a-00e1-47bd-a051-cc40aa2a4824', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, gpl_licensed_projects).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_community).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, commercial_developers).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, software_freedom_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, copyleft_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary author, interpreter, and enforcer of the GPL. They actively promote the strong copyleft interpretation, provide legal guidance, and pursue enforcement actions to ensure compliance, thereby expanding the scope of free software.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation, agenda_setter,
    institutional, generational, analytical, global).

% Projects whose code is licensed under the GPL benefit from the strong copyleft interpretation by ensuring that any derivative or combined works also become GPL-licensed, protecting their codebase from proprietary enclosure and fostering a larger free ecosystem.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, gpl_licensed_projects, beneficiary,
    organized, generational, constrained, global).

% Developers and users who align with the free software philosophy benefit from the expansion of free code, seeing it as a moral and practical good. Their identity is often tied to the principles upheld by strong copyleft.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_community, beneficiary,
    organized, generational, identity_locked, global).

% Companies that develop and sell proprietary software. They face the choice of either avoiding GPL-licensed components entirely, or releasing their own code under the GPL if they combine or dynamically link with GPL code, which they view as a significant loss of intellectual property.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Individual developers or small teams working on commercial software. They must navigate the complexities of GPL licensing, often incurring legal costs or foregoing the use of desirable GPL components to avoid the strong copyleft obligations.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_developers, payer,
    moderate, biographical, constrained, global).

% Industry groups representing proprietary software interests. While they actively lobby against the broad interpretation and enforcement of copyleft in legal and policy arenas, they are structurally excluded from influencing the GPL's text or the FSF's interpretation of it.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_lobby, excluded,
    institutional, generational, trapped, national).

% Judicial bodies that may be called upon to adjudicate disputes over GPL compliance. Their rulings can either affirm or narrow the scope of the strong copyleft interpretation, but they do not set the terms of the license itself.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that software improvements and derivative works built upon GPL-licensed code remain free and open, fostering a collaborative and non-proprietary software development ecosystem.
% TRANSFER_FUNCTION: Legally compels the release of proprietary source code under the GPL if it is combined with or dynamically linked to GPL-licensed components, transferring intellectual property from proprietary to free domains.
% ABSENT_VOICES: Proprietary software vendors and their legal advocates are present in the broader legal and policy debate, but they are structurally excluded from the interpretive community that defines the GPL's scope within the free software movement. They would argue for a narrower interpretation of 'derivative work' and 'combined work'.
% DISAPPEARANCE_RATIONALE: If the strong copyleft interpretation vanished, proprietary vendors would freely integrate GPL components into their closed-source products without releasing their own code, leading to the enclosure of free software improvements and a fragmentation of the free software ecosystem. The core principle of 'software freedom' would be severely undermined.
% FOUNDING_PROBLEM: To prevent proprietary software companies from taking free software, modifying it, and then distributing the modified version as proprietary software, thereby enclosing the commons and denying users the four essential freedoms.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and numerous free software advocates consistently attest that the threat of proprietary enclosure remains a live and ongoing problem, necessitating the continued enforcement of strong copyleft. This is corroborated by the persistent efforts of proprietary vendors to find loopholes or challenge the GPL's scope.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because proprietary vendors are forced to either forgo using valuable GPL components or release their own code, which they consider a significant cost. Suppression is very high (0.90) due to the legal enforceability of the GPL and the FSF's active pursuit of compliance, which effectively suppresses proprietary alternatives for integration. Theater ratio is low (0.10) because the enforcement actions are genuine and have real consequences, not merely performative. Accessibility collapse is high (0.75) as it severely limits the options for proprietary developers wishing to leverage GPL code. Resistance is high (0.80) from proprietary software vendors and their legal teams, who continuously challenge this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Free Software Foundation and the free software community, this constraint is a vital mechanism for ensuring software freedom and preventing enclosure, potentially appearing as a 'rope' or 'tangled_rope' that coordinates a shared resource. However, from the perspective of proprietary software vendors, it operates as a 'snare' that coercively extracts their intellectual property, with the 'coordination' aspect serving as cover for this extraction. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Free Software Foundation and GPL-licensed projects are clear beneficiaries, as this interpretation expands the reach and ensures the freedom of their code. Proprietary software vendors and commercial developers are the primary targets, as they bear the costs of either compliance (releasing their code) or avoidance (forgoing GPL components). The free software community is also a beneficiary, aligning with their ideological goals. Courts act as observers, adjudicating disputes without directly benefiting or paying.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_ambiguity,
    'What constitutes a ''derivative work'' or ''combined work'' under copyright law, particularly concerning dynamic linking, in jurisdictions where the GPL is enforced?',
    'Definitive judicial precedent from a high court in a major jurisdiction, or legislative clarification of copyright law regarding software coupling.',
    'A narrower legal definition would reduce the constraint''s extractiveness and suppression, potentially reclassifying it towards a ''tangled_rope'' or ''rope'' for proprietary vendors. A broader definition would reinforce its ''snare'' characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, empirical, 'Legal ambiguity of the derivative work boundary for software.').

omega_variable(
    enforcement_capacity_variability,
    'How does the actual enforcement capacity and willingness of the Free Software Foundation or other GPL licensors vary across different jurisdictions and against different types of infringers?',
    'Empirical analysis of GPL enforcement actions, including success rates, settlement terms, and judicial outcomes, disaggregated by jurisdiction and infringer type.',
    'If enforcement capacity is low or inconsistent, the constraint''s effective suppression and extractiveness would be lower than the base metrics suggest, potentially shifting its classification towards a ''piton'' or ''tangled_rope'' in practice. High and consistent enforcement reinforces the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_variability, empirical, 'Variability in GPL enforcement capacity and its impact on effective constraint.').

omega_variable(
    gpl_reading_contest,
    'Is the ''strong_copyleft_reading'' of the GPL a universally accepted interpretation within the open-source community, or is it a contested framing that serves specific ideological or strategic goals?',
    'Analysis of legal scholarship, developer surveys, and community discourse to map the prevalence and arguments for alternative interpretations (e.g., ''narrow_scope_reading'', ''enforcement_vacuum_reading'').',
    'If the strong copyleft reading is widely contested even within the open-source community, it highlights the ''conceptual'' nature of the constraint''s scope, suggesting that its ''snare'' characteristics are partly a function of a specific interpretive choice rather than an inherent property of the license text. This would reinforce the need for careful consideration of the ''conceptual'' omegas.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gpl_reading_contest, conceptual, 'The contestability of the strong copyleft interpretation itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 1991, 0.15).
narrative_ontology:measurement(gpl__tr_t1998, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(gpl__tr_t2005, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(gpl__tr_t2012, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(gpl__tr_t2018, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(gpl__tr_t2024, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 1991, 0.6).
narrative_ontology:measurement(gpl__be_t1998, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 1998, 0.7).
narrative_ontology:measurement(gpl__be_t2005, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement(gpl__be_t2012, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2012, 0.82).
narrative_ontology:measurement(gpl__be_t2018, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2018, 0.84).
narrative_ontology:measurement(gpl__be_t2024, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 1991, 0.65).
narrative_ontology:measurement(gpl__su_t1998, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 1998, 0.75).
narrative_ontology:measurement(gpl__su_t2005, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2005, 0.83).
narrative_ontology:measurement(gpl__su_t2012, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2012, 0.87).
narrative_ontology:measurement(gpl__su_t2018, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2018, 0.89).
narrative_ontology:measurement(gpl__su_t2024, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_development_norms).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_copyleft_scope' kernel. This 'strong_copyleft_reading' focuses on the maximalist interpretation of GPL Section 2(b) regarding derivative works and dynamic linking, contrasting with the 'narrow_scope_reading' (which limits the derivative work boundary) and the 'enforcement_vacuum_reading' (which focuses on practical enforceability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
