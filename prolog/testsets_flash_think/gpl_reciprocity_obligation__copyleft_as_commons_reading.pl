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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Copyleft Reciprocity Obligation (Commons Stewardship Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint story analyzes the GNU General Public License (GPL)
 *   through the lens of 'copyleft as commons stewardship.' It focuses on how
 *   the GPL's mandatory reciprocity clause functions as an institutional
 *   technology to prevent the enclosure of the open-source software commons.
 *   This is one reading of the 'gpl_reciprocity_obligation' kernel, distinct
 *   from readings emphasizing individual freedom or commercial restriction.
 *   The metrics reflect the perspective that the GPL is a functional,
 *   actively enforced mechanism that extracts the right to privatize from
 *   certain actors to benefit the collective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.65).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.75).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Copyleft Reciprocity Obligation (Commons Stewardship Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '70f46442-7f69-4bd0-8775-97359997cde0').
narrative_ontology:cs_kernel_codification('70f46442-7f69-4bd0-8775-97359997cde0', formalized).
narrative_ontology:cs_authority_grounding('70f46442-7f69-4bd0-8775-97359997cde0', lineage).
narrative_ontology:cs_interpretation_layer_present('70f46442-7f69-4bd0-8775-97359997cde0').
narrative_ontology:cs_reading_relation('70f46442-7f69-4bd0-8775-97359997cde0', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('70f46442-7f69-4bd0-8775-97359997cde0', gpl_reciprocity_obligation__copyleft_as_restriction_reading, forecloses).
narrative_ontology:cs_axiom('70f46442-7f69-4bd0-8775-97359997cde0', foundational, commons_stewardship_is_primary).
narrative_ontology:cs_axiom_status(commons_stewardship_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('70f46442-7f69-4bd0-8775-97359997cde0', commons_stewardship_is_primary, deontological).
narrative_ontology:cs_axiom('70f46442-7f69-4bd0-8775-97359997cde0', secondary, reciprocity_prevents_enclosure).
narrative_ontology:cs_axiom_status(reciprocity_prevents_enclosure, holdable).
narrative_ontology:cs_axiom_grounding('70f46442-7f69-4bd0-8775-97359997cde0', reciprocity_prevents_enclosure, empirically_contingent).
narrative_ontology:cs_reference_frame('70f46442-7f69-4bd0-8775-97359997cde0', perpetual_commons_stewardship).
narrative_ontology:cs_drift_state('70f46442-7f69-4bd0-8775-97359997cde0', contemporary_licensing_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('70f46442-7f69-4bd0-8775-97359997cde0', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, open_source_commons).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_users).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_freedom_doctrine).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_stewardship_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body of software and knowledge maintained under copyleft licenses, benefiting from mandatory contributions and the prevention of proprietary enclosure. Its existence and growth are directly supported by the GPL's reciprocity.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, open_source_commons, beneficiary,
    institutional, civilizational, analytical, global).

% The primary authors and legal stewards of the GPL, actively enforcing its terms and promoting its adoption. They ensure the legal integrity of the license and advocate for its role in protecting software freedom and the digital commons.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, fsf_and_gpl_stewards, agenda_setter,
    institutional, generational, analytical, global).

% Commercial entities that wish to incorporate GPL-licensed code into proprietary products without releasing their modifications. They find their business models constrained by the reciprocity clause, which mandates sharing derived works under the same license.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Individual developers or small businesses who prefer to use open-source components in proprietary projects without the obligation to share their derived work. They perceive the GPL as a restriction on their commercial freedom and ability to maximize individual gain.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers, payer,
    moderate, biographical, constrained, global).

% Users of GPL-licensed software who benefit from the assurance that the software will remain free and open, with access to source code, the right to modify it, and the guarantee that future improvements will also be open.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_users, beneficiary,
    moderate, biographical, mobile, global).

% Advocates for licenses like MIT or Apache, who believe that maximum freedom comes from minimal restrictions, including the freedom to make software proprietary. They are excluded from the GPL's specific reciprocity framework and its vision of commons stewardship.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, permissive_license_advocates, excluded,
    organized, biographical, mobile, global).

% Academics and legal experts who analyze the GPL's legal enforceability, economic impact, and philosophical underpinnings, providing independent commentary on its role in intellectual property and open source governance.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, legal_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates contributions to a shared software commons by ensuring that all derived works remain open, preventing fragmentation and proprietary capture of collective effort. It establishes a legal framework for mandatory reciprocity.
% TRANSFER_FUNCTION: Legally transfers the obligation to share modifications under the same license from the original licensor to any subsequent distributor of derived works. It also transfers the right to privatize derived works from individual developers to the open source commons.
% ABSENT_VOICES: Those who believe all software should be proprietary by default, or those who advocate for purely permissive open-source licenses (e.g., MIT, Apache) that do not impose reciprocity. They are excluded from the GPL's specific framework and its underlying philosophy of commons protection.
% DISAPPEARANCE_RATIONALE: If the GPL and its enforcement vanished overnight, the open-source commons would rapidly fragment. Proprietary integrators would incorporate and privatize improvements without contributing back, leading to enclosure of the digital commons and a significant shift in the software development ecosystem.
% FOUNDING_PROBLEM: The problem of proprietary software vendors taking open-source code, improving it, and then privatizing those improvements, thereby enclosing the collective digital commons and undermining the collaborative spirit of free software.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation (FSF) and numerous open-source legal experts consistently attest that the threat of proprietary enclosure remains live. Legal precedents upholding copyleft, ongoing debates in the software industry, and the continued development of permissive licenses as alternatives all corroborate the persistence of this founding problem, even if its severity is contested by some commercial actors.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is rated at 0.65, reflecting the significant 'cost' imposed on proprietary integrators and individual exit-maximizers who are prevented from privatizing derived works. Suppression is high (0.75) because the GPL actively suppresses alternative business models that rely on proprietary enclosure, requiring legal enforcement to maintain its terms. Theater ratio is low (0.10) as the constraint is highly functional; its legal mechanisms are directly aimed at achieving its stated goal of commons protection, with minimal performative overhead. Accessibility collapse (0.60) indicates that while alternatives to reciprocity exist (e.g., permissive licenses), the path of proprietary integration of GPL code is significantly constrained. Resistance (0.70) is substantial, coming from commercial entities and developers who prefer less restrictive licensing models.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'open_source_commons' and 'fsf_and_gpl_stewards', the GPL is a vital 'rope' or 'scaffold' that coordinates collective action and protects a shared resource. However, from the 'proprietary_integrators' and 'individual_exit_maximizers' seats, the same constraint operates as a 'snare' or 'tangled_rope', extracting value (the right to privatize) and suppressing their preferred business models. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'open_source_commons' and 'downstream_users' are clear beneficiaries (low directionality), as the GPL ensures the continued availability and freedom of software. The 'fsf_and_gpl_stewards' are agenda-setters, benefiting from the constraint's success in fulfilling their mission. 'Proprietary_integrators' and 'individual_exit_maximizers' are targets (high directionality), as the constraint directly extracts from them the ability to privatize derived works. 'Permissive_license_advocates' are excluded, as their philosophy is fundamentally at odds with the GPL's core reciprocity, making them targets of its suppressive function.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gpl_reading_framing_ambiguity,
    'Is the GPL''s primary function best understood as protecting the commons, preserving individual user freedom, or restricting commercial business models?',
    'Analysis of legal outcomes, developer motivations, and economic impacts across different contexts, coupled with philosophical inquiry into the core values prioritized by different stakeholders.',
    'If the ''freedom'' reading is primary, the constraint might be reclassified closer to a ''rope'' for users; if ''restriction'' is primary, it might be a ''snare'' for businesses. This reading asserts ''commons stewardship'' as primary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gpl_reading_framing_ambiguity, conceptual, 'Ambiguity in the primary framing of the GPL''s purpose.').

omega_variable(
    copyleft_as_freedom_vs_commons,
    'How does the ''copyleft_as_freedom_reading'' structurally differ from this ''copyleft_as_commons_reading''?',
    'Comparative analysis of their foundational axioms and declared beneficiaries/victims. The ''freedom'' reading emphasizes individual user rights, while the ''commons'' reading emphasizes collective resource protection.',
    'The ''freedom'' reading would likely have ''individual_users'' as primary beneficiaries and ''proprietary_capture'' as the primary victim, potentially leading to a slightly different extractiveness profile depending on the specific definition of ''freedom''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyleft_as_freedom_vs_commons, conceptual, 'Distinction between freedom-centric and commons-centric interpretations of copyleft.').

omega_variable(
    copyleft_as_restriction_vs_commons,
    'How does the ''copyleft_as_restriction_reading'' structurally differ from this ''copyleft_as_commons_reading''?',
    'Comparative analysis of their foundational axioms and declared beneficiaries/victims. The ''restriction'' reading focuses on the negative impact on commercial actors, while the ''commons'' reading views this as a necessary mechanism for a greater good.',
    'The ''restriction'' reading would likely have ''proprietary_integrators'' as primary victims and potentially a higher extractiveness score from their perspective, leading to a ''snare'' classification from that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyleft_as_restriction_vs_commons, conceptual, 'Distinction between restriction-centric and commons-centric interpretations of copyleft.').

omega_variable(
    gpl_enforcement_efficacy,
    'To what extent is the GPL''s reciprocity truly effective in preventing commons enclosure, given the rise of permissive licenses and alternative business models?',
    'Empirical studies tracking the growth and fragmentation of open-source projects under different licenses, and the long-term economic impact on the software ecosystem.',
    'If enforcement efficacy is found to be low, the ''suppression'' metric might be overstated, and the constraint could drift towards a ''piton'' if its function atrophies despite continued legal existence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gpl_enforcement_efficacy, empirical, 'Empirical effectiveness of GPL in preventing enclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement(gpl__tr_t1995, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1995, 0.07).
narrative_ontology:measurement(gpl__tr_t2001, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2001, 0.08).
narrative_ontology:measurement(gpl__tr_t2007, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2007, 0.09).
narrative_ontology:measurement(gpl__tr_t2013, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2013, 0.09).
narrative_ontology:measurement(gpl__tr_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2019, 0.1).
narrative_ontology:measurement(gpl__tr_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1989, 0.55).
narrative_ontology:measurement(gpl__be_t1995, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(gpl__be_t2001, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2001, 0.6).
narrative_ontology:measurement(gpl__be_t2007, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2007, 0.62).
narrative_ontology:measurement(gpl__be_t2013, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2013, 0.63).
narrative_ontology:measurement(gpl__be_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2019, 0.64).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1989, 0.6).
narrative_ontology:measurement(gpl__su_t1995, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(gpl__su_t2001, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement(gpl__su_t2007, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2007, 0.72).
narrative_ontology:measurement(gpl__su_t2013, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2013, 0.73).
narrative_ontology:measurement(gpl__su_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2019, 0.74).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_development_practices).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, open_source_business_models).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, permissive_licensing_adoption).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_reciprocity_obligation' kernel, focusing on its role in commons stewardship. It is linked to sibling readings that emphasize user freedom and commercial restriction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
