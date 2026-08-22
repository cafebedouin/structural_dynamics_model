% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Reciprocity Obligation, read as a restriction on proprietary business models
 *   domain: software_licensing_intellectual_property_open_source_governance
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested GPL reciprocity
 *   kernel: the reading that treats the viral licensing clause primarily as a
 *   constraint on business models, prohibiting proprietary integration of
 *   GPL-licensed code without triggering full-source disclosure obligations.
 *   Under this reading, the clause's practical effect — regardless of its
 *   stated free-software rationale — is to foreclose closed-source
 *   commercialization paths for downstream integrators while creating a
 *   monetizable toll-gate for the copyright holder, who can sell commercial
 *   licenses as an exception to the reciprocity requirement. This is NOT a
 *   claim that copyleft is bad or that the freedom-preserving reading is
 *   wrong; it is a separate, structurally distinct constraint occupying the
 *   same kernel, authored with its own beneficiary/victim structure and its
 *   own epsilon.
 *
 * KEY AGENTS:
 *   - commons_contributors_seeking_commercial_integration: primary target (moderate/constrained) — bears the reciprocity cost when seeking to commercialize
 *   - small_startups_needing_proprietary_extensions: primary target (powerless/trapped) — least able to absorb the compliance-or-pay choice
 *   - proprietary_software_vendors_using_dual_licensing: primary beneficiary (powerful/arbitrage) — monetizes the compliance gap via paid exceptions
 *   - gpl_enforcement_organizations: agenda-setter (institutional/arbitrage) — administers and enforces the clause
 *   - embedded_systems_manufacturers: secondary target (moderate/constrained) — absorbs compliance risk in hardware contexts
 *   - permissive_license_alternatives: excluded comparator — the exit route the clause is designed to make relatively less attractive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.42).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.38).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation, read as a restriction on proprietary business models").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "software_licensing_intellectual_property_open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '3f502112-61d5-4c6d-8ad6-a131e12a80c6').
narrative_ontology:cs_kernel_codification('3f502112-61d5-4c6d-8ad6-a131e12a80c6', fixed_text).
narrative_ontology:cs_authority_grounding('3f502112-61d5-4c6d-8ad6-a131e12a80c6', extraction).
narrative_ontology:cs_interpretation_layer_present('3f502112-61d5-4c6d-8ad6-a131e12a80c6').
narrative_ontology:cs_reading_relation('3f502112-61d5-4c6d-8ad6-a131e12a80c6', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f502112-61d5-4c6d-8ad6-a131e12a80c6', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('3f502112-61d5-4c6d-8ad6-a131e12a80c6', foundational, reciprocity_clause_functions_as_commercial_toll_gate).
narrative_ontology:cs_axiom_status(reciprocity_clause_functions_as_commercial_toll_gate, holdable).
narrative_ontology:cs_axiom_grounding('3f502112-61d5-4c6d-8ad6-a131e12a80c6', reciprocity_clause_functions_as_commercial_toll_gate, empirically_contingent).
narrative_ontology:cs_axiom('3f502112-61d5-4c6d-8ad6-a131e12a80c6', secondary, business_model_foreclosure_is_the_operative_cost).
narrative_ontology:cs_axiom_status(business_model_foreclosure_is_the_operative_cost, holdable).
narrative_ontology:cs_axiom_grounding('3f502112-61d5-4c6d-8ad6-a131e12a80c6', business_model_foreclosure_is_the_operative_cost, instrumental).
narrative_ontology:cs_reference_frame('3f502112-61d5-4c6d-8ad6-a131e12a80c6', single_copyright_holder_commercial_toll_model).
narrative_ontology:cs_drift_state('3f502112-61d5-4c6d-8ad6-a131e12a80c6', post_saas_cloud_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f502112-61d5-4c6d-8ad6-a131e12a80c6', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors_using_dual_licensing).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_enforcement_organizations).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors_seeking_commercial_integration).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, small_startups_needing_proprietary_extensions).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, embedded_systems_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Contributed code under GPL expecting to build a business around it, but discover that any derivative work incorporating proprietary components must itself be released under GPL. They cannot integrate the commons code into a closed product without either open-sourcing the whole stack or negotiating a separate commercial license from the copyright holder. Their exit is to fork under a permissive license or abandon the integration.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors_seeking_commercial_integration, payer,
    moderate, biographical, constrained, global).

% Want to build a proprietary product on top of a GPL library because it is the best or only available implementation. The license forces a choice: release the entire product under GPL (destroying the proprietary business model), pay for a rarely-offered commercial license, or engage in costly reimplementation. Limited capital means limited ability to negotiate or reimplement.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, small_startups_needing_proprietary_extensions, payer,
    powerless, immediate, trapped, national).

% Hold the copyright to a GPL-licensed codebase and dual-license it, selling proprietary licenses to companies that cannot comply with GPL's reciprocity terms. The viral clause manufactures the very commercial demand for their paid exception, turning what looks like community licensing into a captive sales funnel they alone control.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors_using_dual_licensing, beneficiary,
    powerful, generational, arbitrage, global).

% Audit compliance, threaten litigation against violators, and negotiate settlements that sometimes include payment to the enforcing organization or mandated code release. They administer and interpret the viral clause, deciding which violations to pursue and how aggressively, without bearing the commercial cost the clause imposes on downstream integrators.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_enforcement_organizations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_enforcement_organizations, beneficiary).

% Ship devices containing GPL components (often a Linux kernel or GPL toolchain) alongside proprietary firmware and drivers. Compliance requires disclosure and offer of source in ways that expose trade secrets or invite reverse engineering of adjacent proprietary logic. Many operate in a gray-compliance zone, absorbing legal risk rather than fully complying or fully avoiding GPL code.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, embedded_systems_manufacturers, payer,
    moderate, biographical, constrained, global).

% MIT/BSD/Apache-licensed projects exist as an alternative that imposes no reciprocity obligation and permits proprietary integration freely. They compete for contributor attention and adoption but are structurally outside this constraint's story except as the exit route the viral clause is designed to make less attractive by comparison.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, permissive_license_alternatives, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reciprocity clause does solve a real coordination problem for a specific class of actor: it lets a copyright holder monetize a codebase by making the free version commercially unusable for closed integration, creating a paid exception as the release valve. Read this way, the coordination being solved is the vendor's revenue problem, not the commons' preservation problem.
% TRANSFER_FUNCTION: Moves optionality and negotiating leverage from downstream integrators (who must either open-source their whole product or pay) to the upstream copyright holder or license-enforcing body, who converts the compliance threat into commercial license revenue or litigation settlements.
% ABSENT_VOICES: Downstream engineers and product managers at small firms who never contributed to license drafting and have no seat in FSF or enforcement-body governance would object that the clause functions as a tax on integration rather than a freedom-preserving mechanism; they are not present in licensing-body deliberations and mostly encounter the constraint only at contract-review or audit time.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished, dual-licensing vendors would lose their commercial license revenue stream and enforcement organizations would lose their leverage, while startups and embedded manufacturers would gain free integration rights — a real reallocation. Whether the 'world rearranges' or 'world stays roughly the same' depends on which reading's ontology you accept: from this restriction reading, the business-constraining function would disappear and previously blocked proprietary products would ship; from the commons-preservation reading, disappearance would mean enclosure. The two readings disagree about which state is the baseline.
% FOUNDING_PROBLEM: The GPL was drafted to prevent proprietary vendors from taking freely-contributed code, modifying it, and redistributing it as closed, non-reciprocal proprietary software — the 'free-rider capture' problem of early free software.
% FOUNDING_PROBLEM_CORROBORATION: Free Software Foundation historical writing and license text attest the founding problem (proprietary capture of commons code) as still live. Independent software-licensing economists and startup founders outside the FSF and outside dual-licensing vendors attest that the mechanism has, in a substantial share of commercial cases, shifted from preventing capture to manufacturing a toll-gate that specific copyright-holding vendors monetize — this restriction reading treats that shifted-function reading as the operative one for the arrangement's business-model effect.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) because the clause does not extract direct rents from most users — it extracts optionality and forecloses specific business models, which is a real but bounded cost. Suppression (0.38) reflects the enforcement infrastructure (SFC/FSF compliance actions, litigation threats) that gives the clause teeth beyond voluntary compliance; this has grown over the measured interval as enforcement organizations professionalized (e.g., BusyBox litigation era, Software Freedom Conservancy). Theater ratio stays low (0.18) because enforcement activity is substantially functional — real settlements and real code releases occur, not merely performative audits. Accessibility collapse (0.45) is moderate: permissive alternatives exist for many use cases, so the constraint does not universally foreclose all paths, only closed-integration paths specifically. Resistance (0.55) reflects real, organized pushback from industry consortia and standards bodies favoring permissive licensing precisely because of this restriction effect.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors using dual-licensing sit at the beneficiary end: they hold copyright, control the commercial-exception price, and have arbitrage-grade exit (they can always choose not to comply and instead monetize). Enforcement organizations administer and partly benefit through settlement leverage. Commons contributors seeking commercialization, small startups, and embedded manufacturers sit toward the target end: they face a binary choice engineered by the license terms, with constrained or trapped exit depending on capital and technical alternatives. This directionality is specific to THIS reading — the freedom-preserving reading would assign the same enforcement organizations to a coordination-protecting role rather than a toll-collecting one, which is exactly why these are different constraints sharing one kernel.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing proprietary capture of commons contributions) is genuinely contested as live vs. dead in this reading: it remains live for genuinely community-governed projects with diffuse contributor bases, but for single-copyright-holder dual-licensed projects, the mechanism has drifted into a monetization structure whose primary present-day function is generating commercial license revenue rather than protecting a commons that, in these cases, barely exists as a multi-stakeholder entity. Classifying this reading as tangled_rope rather than snare captures that a genuine coordination function (preventing pure free-riding) coexists with the asymmetric extraction (toll-gating commercialization) — collapsing it to snare would erase the real coordination story; collapsing it to rope would erase the real victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_clause_reading_indeterminacy,
    'Is the GPL reciprocity clause''s dominant real-world function today (2024) preventing commons enclosure, preserving user freedom, or constraining proprietary business models via a monetizable toll-gate? These are the three declared readings of the same kernel, and the license text itself is compatible with all three framings.',
    'Empirical audit of dual-licensing revenue structures across a large sample of GPL-licensed commercial codebases, cross-referenced with contributor governance structure (single-copyright-holder vs. diffuse foundation-governed) to determine which reading''s structural predictions hold in which project types.',
    'If dual-licensing revenue dominates in single-copyright-holder projects while diffuse commons-governed projects show negligible commercial toll-gating, that would support treating this restriction reading as accurate primarily for a subset of GPL deployments, not the whole population — narrowing this constraint''s scope rather than falsifying it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_clause_reading_indeterminacy, conceptual, 'Whether the restriction reading is the dominant structural fact across GPL deployments or a subset phenomenon specific to dual-licensed, single-holder codebases.').

omega_variable(
    sibling_reading_foreclosure_or_coexistence,
    'Does authoring the restriction reading (beneficiary = proprietary vendors, victim = commons contributors) logically foreclose the commons-preservation reading (same clause, opposite beneficiary/victim assignment) for a SPECIFIC project, or can both readings coexist as true descriptions of the same license text applied to different projects?',
    'Case-by-case governance analysis: single-copyright-holder dual-licensed projects and diffuse-foundation multi-contributor projects are structurally different populations even though both use GPL text; determine whether the two readings partition cleanly by governance structure or genuinely overlap on the same projects.',
    'If the readings partition cleanly by project governance type, they are not really in tension — they are readings of different sub-populations wearing the same license label, which would argue for further kernel decomposition rather than treating this as pure interpretive disagreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_or_coexistence, conceptual, 'Whether the restriction and commons-preservation readings apply to disjoint project populations or genuinely contest the same cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(gpl__tr_t1998, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(gpl__tr_t2005, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(gpl__tr_t2012, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2012, 0.16).
narrative_ontology:measurement(gpl__tr_t2018, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2018, 0.17).
narrative_ontology:measurement(gpl__tr_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1991, 0.2).
narrative_ontology:measurement(gpl__be_t1998, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement(gpl__be_t2005, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2005, 0.34).
narrative_ontology:measurement(gpl__be_t2012, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2012, 0.38).
narrative_ontology:measurement(gpl__be_t2018, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1991, 0.22).
narrative_ontology:measurement(gpl__su_t1998, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1998, 0.28).
narrative_ontology:measurement(gpl__su_t2005, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2005, 0.32).
narrative_ontology:measurement(gpl__su_t2012, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2012, 0.35).
narrative_ontology:measurement(gpl__su_t2018, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2018, 0.37).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.12).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gpl_reciprocity_obligation kernel. copyleft_as_freedom_reading treats the same clause as beneficiary=downstream users/victim=proprietary capturers (near-inverse beneficiary structure); copyleft_as_commons_reading treats it as beneficiary=commons-as-institution/victim=would-be enclosers. This story (copyleft_as_restriction_reading) inverts both: beneficiary=proprietary dual-licensing vendors, victim=commons contributors and integrators seeking commercialization. All three share the identical license text as their kernel but diverge in ε, beneficiary/victim sets, and claimed_type because each reads a different standing arrangement as the referent under contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
