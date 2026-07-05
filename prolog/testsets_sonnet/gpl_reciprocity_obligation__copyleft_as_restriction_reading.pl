% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: GPL Reciprocity as Business-Model Restriction
 *   domain: software_licensing_intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the restriction reading of the GPL reciprocity
 *   kernel: viral (copyleft) licensing terms are analyzed as a business-model
 *   constraint that prohibits proprietary integration of covered code without
 *   either full reciprocal disclosure or a negotiated commercial exception.
 *   This reading treats the coordination story (preventing enclosure) as real
 *   but foregrounds the asymmetric extraction the same clause enables —
 *   copyright holders who retain dual-licensing rights extract rent from a
 *   restriction that binds everyone else, while commons contributors whose
 *   labor built the asset cannot independently monetize it. This is distinct
 *   from the freedom reading (which measures the same clause as protecting
 *   user liberties) and the commons reading (which measures it as
 *   anti-enclosure institutional technology) — those are separate constraints
 *   in this family, sharing the same kernel text but different ε profiles
 *   because they foreground different beneficiary/victim structures. Per the
 *   ε-invariance principle, each reading is authored as its own file.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.58).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.62).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity as Business-Model Restriction").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "software_licensing_intellectual_property").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'becd2b92-9ef9-45f9-b6bf-bd6020e7973e').
narrative_ontology:cs_kernel_codification('becd2b92-9ef9-45f9-b6bf-bd6020e7973e', formalized).
narrative_ontology:cs_authority_grounding('becd2b92-9ef9-45f9-b6bf-bd6020e7973e', extraction).
narrative_ontology:cs_interpretation_layer_present('becd2b92-9ef9-45f9-b6bf-bd6020e7973e').
narrative_ontology:cs_reading_relation('becd2b92-9ef9-45f9-b6bf-bd6020e7973e', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('becd2b92-9ef9-45f9-b6bf-bd6020e7973e', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('becd2b92-9ef9-45f9-b6bf-bd6020e7973e', foundational, reciprocity_clause_is_a_market_asset_not_a_pure_public_good).
narrative_ontology:cs_axiom_status(reciprocity_clause_is_a_market_asset_not_a_pure_public_good, holdable).
narrative_ontology:cs_axiom_grounding('becd2b92-9ef9-45f9-b6bf-bd6020e7973e', reciprocity_clause_is_a_market_asset_not_a_pure_public_good, empirically_contingent).
narrative_ontology:cs_axiom('becd2b92-9ef9-45f9-b6bf-bd6020e7973e', secondary, rightsholder_retained_licensing_discretion_is_legitimate_regardless_of_contributor_labor_share).
narrative_ontology:cs_axiom_status(rightsholder_retained_licensing_discretion_is_legitimate_regardless_of_contributor_labor_share, holdable).
narrative_ontology:cs_axiom_grounding('becd2b92-9ef9-45f9-b6bf-bd6020e7973e', rightsholder_retained_licensing_discretion_is_legitimate_regardless_of_contributor_labor_share, conventional).
narrative_ontology:cs_reference_frame('becd2b92-9ef9-45f9-b6bf-bd6020e7973e', reciprocity_as_founder_controlled_asset).
narrative_ontology:cs_drift_state('becd2b92-9ef9-45f9-b6bf-bd6020e7973e', post_open_core_business_model_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('becd2b92-9ef9-45f9-b6bf-bd6020e7973e', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_fork_vendors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_licensing_originators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, downstream_commercial_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors_locked_out_of_monetization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds full copyright over the codebase and offers it under GPL to the public while separately selling a proprietary license to companies unwilling to accept the reciprocity terms. Writes and enforces the license terms, and profits precisely from the restriction the GPL creates for others; can exit into arbitrage by selling exceptions it alone controls.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_licensing_originators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_licensing_originators, beneficiary).

% Obtains a permissively-licensed or dual-licensed fork, or negotiates a commercial exception, and builds closed derivative products without reciprocity obligations. Benefits from the restriction's chilling effect on smaller competitors who cannot afford legal review or negotiated exceptions, gaining relative market position.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_fork_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Wants to embed the GPL-licensed component into a proprietary commercial product but cannot without either releasing its own source under the same terms or negotiating a paid exception it may not be able to afford. Faces a binary choice — comply and lose proprietary control, or exit the codebase entirely and re-implement, which is often prohibitively costly. The restriction constrains its business model regardless of whether it wanted to free-ride.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, downstream_commercial_integrators, payer,
    moderate, biographical, constrained, global).

% Contributes code under the GPL in good faith expecting reciprocal openness from all users, but discovers that the copyright holder retains a dual-licensing escape hatch it cannot access itself. Its labor becomes the raw material for a business model (dual licensing, proprietary exceptions) that it does not share in, since only the original rightsholder can sell exceptions to the accumulated commons work.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors_locked_out_of_monetization, payer,
    powerless, biographical, trapped, global).

% Uses whichever product results — GPL-compliant or proprietary-exception — without visibility into the licensing negotiation that shaped what features or transparency they receive. Would prefer more source availability but has no seat in licensing decisions and often no awareness the constraint exists.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, end_users_of_derivative_products, excluded,
    powerless, immediate, constrained, global).

% Studies how license choice shapes venture funding decisions, acquisition structures, and startup viability. Documents cases where GPL virality foreclosed commercialization paths that a permissive license would have allowed, informing this reading's claim that the restriction is the operative fact for business-model design.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, software_industry_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared codebase by ensuring that anyone who redistributes modified versions must also share their modifications — solving the free-rider problem of proprietary capture of collectively improved code.
% TRANSFER_FUNCTION: Moves the option value of proprietary integration away from downstream commercial firms and toward whoever holds original copyright (who can sell exceptions) and toward larger firms who can absorb compliance or negotiation costs, while commons contributors who created the value cannot independently monetize it.
% ABSENT_VOICES: Small commercial integrators who cannot afford to negotiate a dual-license exception, and individual contributors whose patches became part of the asset a rightsholder now licenses commercially, rarely appear in license-drafting or foundation governance discussions where these terms are set.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished, downstream integrators could freely fold the code into proprietary products without negotiation; dual-licensing business models built on the restriction would lose their leverage overnight, and proprietary fork vendors would lose the competitive moat the restriction currently gives large, well-resourced players over smaller ones.
% FOUNDING_PROBLEM: Software developers wanted a legal mechanism to prevent their collectively-improved code from being captured into closed proprietary products without any obligation to share improvements back.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and copyleft-licensing project maintainers attest the reciprocity obligation still serves its original anti-enclosure purpose. Independent industry analysts, venture investors, and antitrust-adjacent commentary from outside the licensing community attest that in practice the restriction now functions primarily as a business-model lever — enabling dual-licensing rent extraction and favoring incumbents who can negotiate exceptions — a function distinct from, and in tension with, the founding anti-enclosure rationale.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58 at interval end) reflects the growing prevalence of dual-licensing and 'open core' business models that convert the reciprocity clause into a monetizable restriction rather than a pure anti-enclosure mechanism — the trajectory rises as this business pattern has become more institutionalized across the software industry since early copyleft adoption. Suppression (0.62) is high because compliance is enforced through copyright litigation and takedown threats, and the alternative to compliance (re-implementation) is often prohibitively costly, effectively trapping downstream integrators. Theater ratio stays low (0.2) because the enforcement machinery (license compliance audits, SFC-style litigation) does real suppressive work rather than performing it — this is not a degraded/inertial constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Dual-licensing originators sit at the beneficiary end: they authored the restriction and hold the unique right to sell exceptions to it, converting the reciprocity clause's constraining power into direct revenue. Proprietary fork vendors with resources to negotiate or acquire permissive alternatives also benefit from the restriction's asymmetric burden on smaller rivals. Downstream commercial integrators and, especially, unpaid commons contributors sit at the target end — their code becomes the substrate for a monetization scheme they cannot access, and their exit options (re-implement, negotiate, or comply) are all costly or foreclosed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than pure snare) is deliberate: there is a genuine coordination function (preventing pure free-riding on collectively improved code) bundled with the asymmetric extraction this reading foregrounds. Treating this purely as extraction would erase the real anti-enclosure work the clause does in other contexts (see the sibling commons reading); treating it purely as coordination would erase the documented rent-extraction pattern dual-licensing firms have built on top of it. The tangled_rope classification holds both facts without collapsing them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restriction_vs_protection_framing,
    'Is the reciprocity clause best characterized as a restriction imposed on downstream business models, or as a protection extended to users and the commons against enclosure — given that both descriptions are true of the same clause depending on which party''s position is centered?',
    'This is inherently a framing question, not empirically resolvable in isolation — resolution comes from examining which reading better predicts actual behavior in specific cases (e.g., whether dual-licensing revenue flows disproportionately to original rightsholders versus whether proprietary capture events actually declined post-adoption).',
    'If the restriction framing dominates empirically (extraction via dual-licensing exceeds anti-enclosure benefit to the commons), this reading''s tangled_rope classification is well-supported. If the protection framing dominates, the sibling commons/freedom readings better describe the operative dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restriction_vs_protection_framing, conceptual, 'Whether restriction or protection is the operative framing for a given instance of the reciprocity clause.').

omega_variable(
    dual_licensing_rent_share,
    'What share of dual-licensing revenue collected by originating rightsholders flows back to the community contributors whose code is part of the licensed asset, versus staying with the rightsholder alone?',
    'Audit of contributor license agreements (CLAs) and revenue-sharing terms across major dual-licensed open-source projects (e.g., MySQL-era, MongoDB, Elastic).',
    'A near-zero contributor share would strongly support the victim characterization of commons_contributors_locked_out_of_monetization; a substantial share would weaken this reading''s extraction claim and shift the balance toward the commons reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_licensing_rent_share, empirical, 'Whether commons contributors share in the rents this reading claims flow to rightsholders.').

omega_variable(
    kernel_reading_selection_evidence,
    'What structural or contextual signals should determine which of the three kernel readings (restriction, freedom, commons) is the operative description for a specific deployment of the GPL?',
    'Case-level analysis: examine whether a dual-licensing structure exists (favors restriction reading), whether the primary observed effect is preventing a specific proprietary capture event (favors commons reading), or whether the analysis centers user rights to modify and redistribute (favors freedom reading).',
    'Choosing the wrong reading for a given case would misattribute beneficiary/victim structure and misclassify the constraint type for that instance — this omega documents that the three readings are not arbitrary alternatives but track genuinely different structural configurations of the same license text.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Selection criteria for which kernel reading applies to a specific GPL deployment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(gpl__tr_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(gpl__tr_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 25, 0.2).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gpl__be_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(gpl__be_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(gpl__be_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gpl__su_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(gpl__su_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(gpl__su_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gpl_reciprocity_obligation kernel, each authored as a separate ε-invariant story per the decomposition principle. copyleft_as_freedom_reading measures the same license clause centering user-freedom preservation (lower extraction, closer to rope). copyleft_as_commons_reading measures it as anti-enclosure institutional technology (coordination-dominant, closer to rope with narrower victim set). This restriction reading foregrounds the dual-licensing/business-model asymmetry (tangled_rope, higher extraction). All three share the identical underlying license text but diverge in which beneficiary/victim structure is foregrounded, hence different ε and different computed types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
