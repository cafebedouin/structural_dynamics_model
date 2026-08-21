% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Copyleft as Commons Enclosure Prevention
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint describes the GNU General Public License (GPL) from the
 *   perspective of 'copyleft as commons enclosure prevention.' It views the
 *   GPL's mandatory reciprocity (copyleft) as an institutional technology
 *   designed to protect and grow a shared software commons by preventing
 *   individual actors from privatizing derivative works. This is one reading
 *   of the broader 'gpl_reciprocity_obligation' kernel, which also includes
 *   'copyleft as freedom' and 'copyleft as restriction' readings. The claimed
 *   type is Tangled Rope, reflecting its dual function of coordinating
 *   contributions to the commons while extracting the right to privatize from
 *   those who would enclose it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.65).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.7).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Copyleft as Commons Enclosure Prevention").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'e24bf6ad-7024-41e6-a9c8-fc2909add7b1').
narrative_ontology:cs_kernel_codification('e24bf6ad-7024-41e6-a9c8-fc2909add7b1', fixed_text).
narrative_ontology:cs_authority_grounding('e24bf6ad-7024-41e6-a9c8-fc2909add7b1', lineage).
narrative_ontology:cs_interpretation_layer_present('e24bf6ad-7024-41e6-a9c8-fc2909add7b1').
narrative_ontology:cs_reading_relation('e24bf6ad-7024-41e6-a9c8-fc2909add7b1', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('e24bf6ad-7024-41e6-a9c8-fc2909add7b1', gpl_reciprocity_obligation__copyleft_as_restriction_reading, forecloses).
narrative_ontology:cs_axiom('e24bf6ad-7024-41e6-a9c8-fc2909add7b1', foundational, software_is_a_commons).
narrative_ontology:cs_axiom_status(software_is_a_commons, holdable).
narrative_ontology:cs_axiom_grounding('e24bf6ad-7024-41e6-a9c8-fc2909add7b1', software_is_a_commons, conventional).
narrative_ontology:cs_axiom('e24bf6ad-7024-41e6-a9c8-fc2909add7b1', foundational, reciprocity_prevents_enclosure).
narrative_ontology:cs_axiom_status(reciprocity_prevents_enclosure, holdable).
narrative_ontology:cs_axiom_grounding('e24bf6ad-7024-41e6-a9c8-fc2909add7b1', reciprocity_prevents_enclosure, instrumental).
narrative_ontology:cs_reference_frame('e24bf6ad-7024-41e6-a9c8-fc2909add7b1', perpetual_commons_growth).
narrative_ontology:cs_drift_state('e24bf6ad-7024-41e6-a9c8-fc2909add7b1', contemporary_licensing_landscape, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e24bf6ad-7024-41e6-a9c8-fc2909add7b1', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, open_source_community).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_developers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, exit_maximizers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body of free and open-source software, which is protected from proprietary enclosure by the GPL's mandatory reciprocity. It benefits from the continuous contribution of derivative works under the same license.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons).

% Developers and users who contribute to and benefit from the shared software commons. They rely on the GPL to ensure that their contributions and the work of others remain free and accessible, fostering collaboration and preventing privatization.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, open_source_community, beneficiary,
    organized, generational, constrained, global).

% The Free Software Foundation and other entities responsible for maintaining and enforcing the GPL. They actively monitor compliance and initiate legal action against violations to uphold the copyleft principle and protect the commons.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, fsf_licensing_enforcers, agenda_setter,
    institutional, generational, constrained, global).

% Developers and companies who wish to use GPL-licensed code in their proprietary products but are prevented from doing so without releasing their derivative work under the GPL. They bear the 'cost' of mandatory reciprocity by foregoing proprietary integration.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_developers, payer,
    powerful, biographical, constrained, global).

% Individual developers or small businesses who might prefer to privatize their contributions or derivative works for short-term commercial gain, but are obligated by the GPL to maintain reciprocity, thus 'extracting' their ability to exit the commons.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, exit_maximizers, payer,
    moderate, immediate, constrained, global).

% Advocates for licenses like MIT or Apache, which allow proprietary integration of derivative works. While they can use GPL code, their preferred model of maximizing individual freedom (including the freedom to privatize) is suppressed by the GPL's mandatory reciprocity, leading to a philosophical and practical exclusion from the GPL ecosystem for certain use cases.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, permissive_licensing_advocates, excluded,
    organized, biographical, mobile, global).

% Academics and legal experts who analyze the implications, effectiveness, and legal challenges of the GPL. They provide critical commentary and contribute to the ongoing interpretation of copyleft principles.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that contributions to a shared software codebase remain freely available for all, preventing individual actors from privatizing collective work and fostering a continuously growing software commons.
% TRANSFER_FUNCTION: Transfers the right to privatize derivative works from individual developers back to the collective commons, enforced by legal obligation, thereby maintaining the 'freeness' of the software.
% ABSENT_VOICES: Developers and businesses who prefer permissive licenses or proprietary models are structurally excluded from using GPL-licensed code in proprietary derivative works without complying with copyleft. They would argue for greater flexibility and less 'viral' licensing, prioritizing individual freedom to choose licensing terms over mandatory reciprocity.
% DISAPPEARANCE_RATIONALE: If the GPL's copyleft mechanism and its enforcement vanished overnight, the software commons would be rapidly enclosed by proprietary interests. Developers would privatize derivative works, leading to fragmentation of shared codebases and a significant shift in the open-source ecosystem towards more permissive, less reciprocal models, fundamentally altering the landscape of free software.
% FOUNDING_PROBLEM: The risk of proprietary enclosure of early free software, where contributions could be taken and privatized without giving back to the community, threatening the long-term viability and growth of a shared software commons.
% FOUNDING_PROBLEM_CORROBORATION: The open-source community and many legal scholars (outside the FSF) corroborate the ongoing threat of enclosure and the role of copyleft in mitigating it. While some debate its efficacy or necessity in all contexts, the core problem of maintaining a shared commons against privatizing pressures is widely acknowledged.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is medium-high because the GPL imposes a significant obligation on developers: any derivative work must also be licensed under the GPL, effectively 'extracting' the option to privatize. Suppression (0.70) is high as it actively suppresses proprietary alternatives for derivative works, enforced through legal mechanisms. Theater ratio (0.10) is low, as the GPL is a highly functional legal instrument with clear, enforceable rules, not primarily performative. Accessibility collapse (0.60) is moderate; while developers can choose other licenses for original works, once GPL code is incorporated, the alternatives for that specific project collapse. Resistance (0.55) is ongoing, primarily from proprietary software companies and advocates of more permissive open-source licenses.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the open-source community and the FSF, the GPL is a vital coordination mechanism that protects a public good. From the perspective of proprietary developers, it is a restrictive and extractive legal obligation that limits their business models. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as a Rope or Scaffold, and victims experiencing it as a Snare or Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'software_commons' (as an institutional concept) and the 'open_source_community' are the primary beneficiaries, as the GPL ensures the growth and protection of their shared resource. The 'fsf_licensing_enforcers' act as agenda-setters, actively maintaining the constraint. 'Proprietary_developers' and 'exit_maximizers' are the victims/payers, as they bear the cost of mandatory reciprocity by being unable to privatize derivative works. 'Permissive_licensing_advocates' are excluded, as their preferred model is suppressed by copyleft's viral nature. Legal scholars serve as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    copyleft_framing_ambiguity,
    'Is copyleft primarily an institutional technology for commons protection, a mechanism for user freedom, or a restriction on business models?',
    'Analysis of legal outcomes, developer adoption patterns, and economic impact studies, weighted by the stated intent of license creators and adopters.',
    'If primarily a freedom mechanism, the constraint might lean more towards Rope; if primarily a restriction, it would lean more towards Snare. This reading emphasizes the commons aspect, influencing its Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(copyleft_framing_ambiguity, conceptual, 'Ambiguity in the primary framing of copyleft''s function.').

omega_variable(
    commons_naturalness_ambiguity,
    'Is the ''software commons'' a natural emergent property of collaborative development, or a constructed legal and social institution maintained by constraints like the GPL?',
    'Comparative study of software development ecosystems with and without strong copyleft, observing the long-term trajectory of shared codebases and proprietary forks.',
    'If more natural, the GPL''s role as an ''enclosure prevention'' would be less critical, potentially lowering its perceived extractiveness. If highly constructed, the GPL''s role as an active enforcement mechanism for the commons is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_naturalness_ambiguity, empirical, 'The ontological status of the software commons.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement(gpl__tr_t1999, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1999, 0.08).
narrative_ontology:measurement(gpl__tr_t2009, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2009, 0.1).
narrative_ontology:measurement(gpl__tr_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2019, 0.1).
narrative_ontology:measurement(gpl__tr_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1989, 0.5).
narrative_ontology:measurement(gpl__be_t1999, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1999, 0.58).
narrative_ontology:measurement(gpl__be_t2009, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2009, 0.62).
narrative_ontology:measurement(gpl__be_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2019, 0.64).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1989, 0.55).
narrative_ontology:measurement(gpl__su_t1999, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1999, 0.63).
narrative_ontology:measurement(gpl__su_t2009, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2009, 0.68).
narrative_ontology:measurement(gpl__su_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2019, 0.69).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_reciprocity_obligation' kernel. Each reading presents a distinct structural claim about the GPL's function and impact, leading to different ε values and classifications. They are linked to represent the contested nature of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
