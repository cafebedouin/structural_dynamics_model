% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_restriction_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Copyleft as Business Model Restriction
 *   domain: Software Licensing / Intellectual Property / Open Source Governance
 *
 * SUMMARY:
 *   This constraint represents a reading of the GPL's reciprocity obligation
 *   from the perspective of proprietary business models, viewing it primarily
 *   as a restriction. Viral licensing, particularly the GNU General Public
 *   License (GPL), legally mandates that any software derived from
 *   GPL-licensed code must also be licensed under the GPL. From this
 *   'restriction' reading, this 'virality' constrains proprietary business
 *   models by prohibiting them from integrating GPL components into
 *   closed-source products without opening their own code, thereby imposing a
 *   significant cost on their ability to leverage existing open-source
 *   assets.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.75).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.8).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, snare).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Copyleft as Business Model Restriction").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "Software Licensing / Intellectual Property / Open Source Governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'b5dee5ce-cc47-4265-9fe6-5663d82a49a3').
narrative_ontology:cs_kernel_codification('b5dee5ce-cc47-4265-9fe6-5663d82a49a3', fixed_text).
narrative_ontology:cs_authority_grounding('b5dee5ce-cc47-4265-9fe6-5663d82a49a3', practice).
narrative_ontology:cs_interpretation_layer_present('b5dee5ce-cc47-4265-9fe6-5663d82a49a3').
narrative_ontology:cs_reading_relation('b5dee5ce-cc47-4265-9fe6-5663d82a49a3', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5dee5ce-cc47-4265-9fe6-5663d82a49a3', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('b5dee5ce-cc47-4265-9fe6-5663d82a49a3', foundational, proprietary_integration_is_restriction).
narrative_ontology:cs_axiom_status(proprietary_integration_is_restriction, holdable).
narrative_ontology:cs_axiom_grounding('b5dee5ce-cc47-4265-9fe6-5663d82a49a3', proprietary_integration_is_restriction, instrumental).
narrative_ontology:cs_reference_frame('b5dee5ce-cc47-4265-9fe6-5663d82a49a3', unrestricted_proprietary_development).
narrative_ontology:cs_drift_state('b5dee5ce-cc47-4265-9fe6-5663d82a49a3', contemporary_software_economy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b5dee5ce-cc47-4265-9fe6-5663d82a49a3', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_community).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_ecosystem).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_businesses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, rival_proprietary_vendors).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, software_freedom_doctrine).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, reciprocity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These businesses wish to integrate valuable GPL-licensed software components into their proprietary products. The copyleft obligation forces them to either open-source their entire derived work, which conflicts with their business model, or avoid using the GPL code altogether, incurring development costs or competitive disadvantage. Their exit is constrained by the market dominance of some GPL software.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_businesses, payer,
    powerful, biographical, constrained, global).

% The collective of developers and advocates who create and maintain GPL-licensed software. They choose the GPL to ensure that software remains free and open, preventing proprietary enclosure and ensuring that all derived works contribute back to the commons. They actively enforce the license terms through copyright claims.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_community, agenda_setter,
    organized, generational, mobile, global).

% The broader network of open-source projects and users. It benefits from the GPL's reciprocity by ensuring a larger pool of freely available code and preventing proprietary forks that do not contribute back, thus fostering a vibrant and growing commons.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_ecosystem, beneficiary,
    organized, generational, mobile, global).

% These vendors may not use GPL code themselves but benefit when their competitors are constrained from integrating valuable GPL components into their proprietary offerings. This can level the competitive playing field or create a strategic advantage for those who develop proprietary alternatives.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, rival_proprietary_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Academics and legal experts who analyze the legal implications, effectiveness, and economic impact of copyleft licenses on software development, intellectual property, and business models. They provide independent analysis of the constraint's operation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, legal_scholars_ip, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_community).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_restriction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development and distribution of software by establishing a legal framework that ensures all derivative works remain open, fostering a shared software commons.
% TRANSFER_FUNCTION: Transfers the right to create proprietary derivative works from those who would enclose GPL code to the open-source community, ensuring all derived work remains open and accessible under the same terms.
% ABSENT_VOICES: Businesses and developers who advocate for more permissive licensing models or who wish to integrate GPL code into proprietary products without reciprocity obligations are structurally excluded from shaping the GPL's terms. They would argue for greater flexibility and less restrictive integration options.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished overnight, many proprietary forks of existing GPL software would likely emerge. This would fragment the open-source commons, reduce contributions back to the public domain, and fundamentally alter the economic and developmental models of the software ecosystem, leading to a significant rearrangement of the industry.
% FOUNDING_PROBLEM: The problem of proprietary enclosure of software, where code developed collaboratively could be taken, modified, and then locked away behind proprietary licenses, preventing users from exercising the four freedoms (run, study, redistribute, improve).
% FOUNDING_PROBLEM_CORROBORATION: While the GPL community and open-source advocates attest that proprietary enclosure remains a live threat, some proprietary software businesses and legal scholars argue that the founding problem is overstated or that the license's strictness stifles innovation and collaboration. Legislative hearings and economic studies offer mixed corroboration, indicating a contested status.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.75) is high because the GPL's terms impose a substantial cost on proprietary businesses, forcing them to either abandon their proprietary model or forgo using valuable GPL code. Suppression (0.80) is high due to the legally binding nature of copyright law and the active enforcement by copyright holders, which effectively suppresses the alternative of proprietary integration. The theater ratio is low (0.10) as the license's function is direct and legally unambiguous, with little performative overhead. Accessibility collapse (0.60) is moderate; while alternatives (permissive licenses, proprietary development from scratch) exist, the desire to use specific GPL components can make these alternatives less attractive. Resistance (0.70) is significant, manifested in legal challenges, attempts to find loopholes, and the development of alternative licensing strategies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of proprietary software businesses, the GPL is a snare that extracts value by forcing them to choose between their business model and valuable code. From the GPL community's perspective (e.g., 'copyleft_as_freedom_reading'), the same constraint is a rope or scaffold that coordinates contributions and protects user freedoms. The engine's classification will highlight this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software businesses are the primary victims (payers) as they bear the direct cost of the restriction on their business models. The GPL community and the broader open-source ecosystem are beneficiaries, as the constraint ensures the continued growth and purity of the open-source commons. Rival proprietary vendors can also be indirect beneficiaries by gaining a competitive edge when their competitors are constrained. Legal scholars act as observers, analyzing the constraint's impact without direct benefit or cost.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    copyleft_effectiveness_ambiguity,
    'Is copyleft truly effective at preventing proprietary enclosure, or does it merely push businesses to use more permissive licenses or avoid open source altogether, thus limiting overall adoption?',
    'Empirical studies analyzing licensing trends, developer adoption rates of different license types, and the long-term impact on the growth of both copyleft and proprietary software ecosystems.',
    'If copyleft primarily drives developers away from open source or towards permissive licenses, its effective suppression of proprietary integration might be lower than intended, and its overall benefit to the open-source commons could be contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyleft_effectiveness_ambiguity, empirical, 'Uncertainty regarding the actual behavioral impact and long-term effectiveness of copyleft in achieving its goals.').

omega_variable(
    innovation_impact_ambiguity,
    'Does the restriction on proprietary integration stifle innovation by limiting collaboration and the use of valuable components, or does it foster innovation within the open-source ecosystem by ensuring contributions remain open?',
    'Comparative economic analysis of innovation rates and software quality in ecosystems dominated by copyleft versus permissive licenses, controlling for other factors.',
    'If the restriction demonstrably stifles overall innovation, the ''snare'' classification would be reinforced by evidence of broader societal cost. If it fosters innovation within the open-source sphere, the ''beneficiary'' role of the open-source ecosystem would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_impact_ambiguity, empirical, 'Debate over whether copyleft''s restrictions are a net positive or negative for innovation.').

omega_variable(
    framing_underdetermination_gpl,
    'Is the GPL''s reciprocity obligation fundamentally a restriction on business models, a guarantor of user freedom, or a mechanism for commons governance?',
    'Conceptual analysis of the core normative claims and observed structural effects of the license, acknowledging that different framings lead to different classifications. The engine''s output for this and sibling readings will highlight the structural differences.',
    'The classification of the GPL shifts dramatically depending on the adopted framing. This reading (restriction) leads to a Snare, while other readings (freedom, commons) would likely lead to Rope or Tangled Rope. The choice of framing determines the primary beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_gpl, conceptual, 'The GPL''s nature is underdetermined by its text alone; its classification depends on the interpretive frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1989, 0.1).
narrative_ontology:measurement(gpl__tr_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1999, 0.1).
narrative_ontology:measurement(gpl__tr_t2009, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2009, 0.1).
narrative_ontology:measurement(gpl__tr_t2019, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2019, 0.1).
narrative_ontology:measurement(gpl__tr_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1989, 0.65).
narrative_ontology:measurement(gpl__be_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1999, 0.7).
narrative_ontology:measurement(gpl__be_t2009, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2009, 0.73).
narrative_ontology:measurement(gpl__be_t2019, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2019, 0.74).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1989, 0.75).
narrative_ontology:measurement(gpl__su_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1999, 0.78).
narrative_ontology:measurement(gpl__su_t2009, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2009, 0.8).
narrative_ontology:measurement(gpl__su_t2019, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2019, 0.8).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_development_models).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, permissive_open_source_licensing).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% The 'gpl_reciprocity_obligation' is a contested kernel decomposed into multiple constraint stories, each representing a distinct reading. This story focuses on the 'copyleft_as_restriction_reading', while others address 'copyleft_as_freedom_reading' and 'copyleft_as_commons_reading'. Each reading has a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
