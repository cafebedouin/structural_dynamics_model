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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Reciprocity Obligation (as Business Model Restriction)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the GPL's reciprocity obligation from the
 *   perspective that it restricts business models by prohibiting proprietary
 *   integration. It is one reading of the 'gpl_reciprocity_obligation'
 *   kernel, focusing on the 'copyleft as restriction' interpretation. From
 *   this view, the GPL acts as a snare, extracting the right to proprietary
 *   development from those who wish to use GPL-licensed code, benefiting
 *   proprietary vendors by limiting competition from hybrid models.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.65).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.75).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, snare).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation (as Business Model Restriction)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'ea9add46-a7b0-4125-b713-9853f51c899a').
narrative_ontology:cs_kernel_codification('ea9add46-a7b0-4125-b713-9853f51c899a', fixed_text).
narrative_ontology:cs_authority_grounding('ea9add46-a7b0-4125-b713-9853f51c899a', lineage).
narrative_ontology:cs_interpretation_layer_present('ea9add46-a7b0-4125-b713-9853f51c899a').
narrative_ontology:cs_reading_relation('ea9add46-a7b0-4125-b713-9853f51c899a', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea9add46-a7b0-4125-b713-9853f51c899a', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('ea9add46-a7b0-4125-b713-9853f51c899a', foundational, proprietary_integration_is_a_fundamental_business_right).
narrative_ontology:cs_axiom_status(proprietary_integration_is_a_fundamental_business_right, holdable).
narrative_ontology:cs_axiom_grounding('ea9add46-a7b0-4125-b713-9853f51c899a', proprietary_integration_is_a_fundamental_business_right, deontological).
narrative_ontology:cs_axiom('ea9add46-a7b0-4125-b713-9853f51c899a', secondary, copyleft_stifles_innovation_and_economic_growth).
narrative_ontology:cs_axiom_status(copyleft_stifles_innovation_and_economic_growth, holdable).
narrative_ontology:cs_axiom_grounding('ea9add46-a7b0-4125-b713-9853f51c899a', copyleft_stifles_innovation_and_economic_growth, empirically_contingent).
narrative_ontology:cs_reference_frame('ea9add46-a7b0-4125-b713-9853f51c899a', unrestricted_software_business_models).
narrative_ontology:cs_drift_state('ea9add46-a7b0-4125-b713-9853f51c899a', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('ea9add46-a7b0-4125-b713-9853f51c899a', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_contributors_seeking_proprietary_integration).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, hybrid_business_models).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These vendors benefit from the GPL's restrictive nature, as it prevents competitors from easily integrating GPL-licensed components into proprietary products without 'viral' obligations, thus protecting their market share for fully proprietary solutions. They can choose to avoid GPL components entirely or use them in isolated ways.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, mobile, global).

% These contributors wish to use GPL-licensed code in projects that may have proprietary components or business models. The 'viral' nature of the GPL forces them to either open-source their entire derivative work or avoid GPL components, restricting their business model choices.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_contributors_seeking_proprietary_integration, payer,
    moderate, biographical, constrained, global).

% Companies attempting to build business models that combine open-source components with proprietary extensions find their options severely limited by the GPL. They must either dual-license, use non-GPL open-source, or fully embrace open-source, which may not align with their revenue strategies.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, hybrid_business_models, payer,
    organized, generational, constrained, global).

% Organizations like the Free Software Foundation actively enforce the GPL's terms, ensuring that derivative works are also open-sourced under compatible licenses. From this reading, their enforcement acts as a restriction on business freedom rather than a protection of user freedom.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_enforcement_foundations, agenda_setter,
    institutional, generational, analytical, global).

% These users are often unaware of the underlying licensing constraints. They are excluded from the debate about the GPL's impact on business models, as their primary concern is the functionality and cost of the software they use, regardless of its licensing.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, users_of_proprietary_software, excluded,
    powerless, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The GPL coordinates the development of a shared software commons by mandating reciprocity, ensuring that improvements to the code base remain open and accessible to all under the same terms.
% TRANSFER_FUNCTION: The constraint transfers the right to create proprietary derivative works from developers and businesses to the open-source commons, effectively forcing proprietary business models to either contribute back or avoid GPL-licensed code.
% ABSENT_VOICES: Many developers and businesses who would prefer to integrate GPL-licensed components into proprietary products without 'viral' obligations are effectively silenced by the license's terms. Their business models are simply foreclosed by the license, rather than being part of a negotiation.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished, many proprietary software vendors would immediately integrate existing GPL code into their products without contributing back, leading to a rapid enclosure of the open-source commons and a significant shift in software development economics.
% FOUNDING_PROBLEM: The founding problem was the 'enclosure' of software by proprietary vendors, where code developed collaboratively or freely was then locked down and monetized without giving back to the community, leading to a loss of user freedom and a shrinking commons.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and many open-source advocates attest that the problem of proprietary enclosure remains live, citing ongoing attempts by corporations to leverage open-source without reciprocity. However, proprietary vendors and hybrid business models contest this, arguing that the GPL itself stifles innovation and new business models, rather than protecting a commons.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).

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
 *   The extractiveness (0.65) is high because the GPL demands a significant 'price' (open-sourcing derivative works) for using its licensed code, which is a cost for business models seeking proprietary value. Suppression (0.75) is also high, as the license is legally enforced, and there are few viable alternatives for integrating GPL code without accepting its terms. The theater ratio is low (0.1) because the enforcement is direct and functional, not performative. Accessibility collapse is moderate (0.6) as there are other open-source licenses, but the GPL's ubiquity in certain domains makes it hard to avoid.
 *
 * PERSPECTIVAL GAP:
 *   The GPL enforcement foundations perceive this constraint as a 'rope' or 'mountain' that protects user freedom and the commons. However, from the perspective of businesses seeking proprietary integration, it operates as a 'snare' that extracts their ability to innovate freely. The engine's classification will highlight this divergence based on the declared beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software vendors are beneficiaries (d=0.0-0.2) as the GPL protects their market by restricting hybrid competitors. Open-source contributors seeking proprietary integration and hybrid business models are victims (d=0.8-1.0) as they bear the cost of the reciprocity obligation. GPL enforcement foundations are agenda-setters (d=0.5-0.7) as they actively maintain the constraint, but from this reading, their actions are seen as restrictive.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gpl_impact_on_innovation,
    'Does the GPL''s reciprocity obligation stifle or promote overall software innovation?',
    'Longitudinal studies comparing innovation rates in GPL-heavy vs. permissively licensed ecosystems, and economic analysis of business model diversity.',
    'If it stifles innovation, the constraint''s negative impact on the broader software ecosystem is higher than currently measured; if it promotes, the ''restriction'' reading is less accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gpl_impact_on_innovation, empirical, 'Ambiguity regarding the GPL''s net effect on innovation.').

omega_variable(
    freedom_vs_restriction_framing,
    'Is the GPL''s ''viral'' nature a necessary restriction to ensure user freedom and a vibrant commons, or an unnecessary constraint on business model flexibility?',
    'Conceptual analysis of ''freedom'' in software development, and policy debates on the balance between user rights and developer autonomy.',
    'If framed as necessary for freedom, the constraint shifts towards a ''rope'' or ''mountain'' for users; if framed as an unnecessary restriction, it remains a ''snare'' for businesses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_vs_restriction_framing, conceptual, 'The core conceptual dispute over the GPL''s nature.').

omega_variable(
    proprietary_vendor_beneficiary_ambiguity,
    'Are proprietary software vendors truly beneficiaries of the GPL''s restrictions, or do they also face costs by being unable to leverage GPL-licensed components?',
    'Detailed analysis of proprietary vendors'' strategies, including their use of non-GPL open-source alternatives and their lobbying efforts against copyleft licenses.',
    'If proprietary vendors also face significant costs, their beneficiary status is overstated, and the constraint''s overall extractiveness might be more diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_vendor_beneficiary_ambiguity, empirical, 'Ambiguity in the beneficiary status of proprietary vendors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement(gpl__tr_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1999, 0.08).
narrative_ontology:measurement(gpl__tr_t2009, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2009, 0.1).
narrative_ontology:measurement(gpl__tr_t2019, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2019, 0.1).
narrative_ontology:measurement(gpl__tr_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1989, 0.5).
narrative_ontology:measurement(gpl__be_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1999, 0.58).
narrative_ontology:measurement(gpl__be_t2009, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2009, 0.62).
narrative_ontology:measurement(gpl__be_t2019, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2019, 0.64).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1989, 0.6).
narrative_ontology:measurement(gpl__su_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1999, 0.68).
narrative_ontology:measurement(gpl__su_t2009, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2009, 0.72).
narrative_ontology:measurement(gpl__su_t2019, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2019, 0.74).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, information_standard).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_reciprocity_obligation' kernel. This reading focuses on the GPL as a restriction on business models, while others emphasize freedom or commons protection. The ε values differ significantly across these readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
