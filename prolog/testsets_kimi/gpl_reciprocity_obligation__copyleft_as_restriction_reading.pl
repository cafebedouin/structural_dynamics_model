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
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Restriction Reading)
 *   domain: software_licensing/intellectual_property
 *
 * SUMMARY:
 *   The GNU General Public License's reciprocity obligation requires that
 *   derivative works of GPL-licensed software be distributed under the same
 *   license terms. In the 'copyleft as restriction' reading, this obligation
 *   functions not merely as a commons-protecting coordination mechanism but
 *   as an active constraint on business models that asymmetrically benefits
 *   proprietary software vendors by quarantining GPL code from the dominant
 *   proprietary ecosystem. The constraint is a contested kernel: other
 *   readings frame it as preserving user freedom or protecting a digital
 *   commons. This story instantiates ONLY the restriction reading, per
 *   Îµ-invariance.
 *
 * KEY AGENTS:
 *   - gpl_stewards (FSF and copyright holders): Institutional agenda-setter with constrained exit â administers the license text and enforcement machinery.
 *   - proprietary_software_vendors: Powerful beneficiary with mobile exit â captures preserved market share from the commons quarantine effect.
 *   - commons_contributors: Moderate payer/victim with constrained exit â bears the cost of isolation from proprietary integration markets.
 *   - enterprise_integrators: Powerful payer with constrained exit â restricted from integrating GPL code into proprietary products without reciprocating.
 *   - permissive_license_advocates: Excluded voice â offers alternative licensing models that would reduce restriction costs.
 *   - software_industry_analysts: Analytical observer â tracks licensing market dynamics and enforcement trends.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.72).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.58).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation (Copyleft as Restriction Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "software_licensing/intellectual_property").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '2de16f36-d3af-40f6-a99e-fffa4a3d84dc').
narrative_ontology:cs_kernel_codification('2de16f36-d3af-40f6-a99e-fffa4a3d84dc', formalized).
narrative_ontology:cs_authority_grounding('2de16f36-d3af-40f6-a99e-fffa4a3d84dc', lineage).
narrative_ontology:cs_interpretation_layer_present('2de16f36-d3af-40f6-a99e-fffa4a3d84dc').
narrative_ontology:cs_reading_relation('2de16f36-d3af-40f6-a99e-fffa4a3d84dc', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('2de16f36-d3af-40f6-a99e-fffa4a3d84dc', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('2de16f36-d3af-40f6-a99e-fffa4a3d84dc', foundational, copyleft_restricts_market_integration).
narrative_ontology:cs_axiom_status(copyleft_restricts_market_integration, holdable).
narrative_ontology:cs_axiom_grounding('2de16f36-d3af-40f6-a99e-fffa4a3d84dc', copyleft_restricts_market_integration, empirically_contingent).
narrative_ontology:cs_axiom('2de16f36-d3af-40f6-a99e-fffa4a3d84dc', secondary, commons_contributors_bear_restriction_cost).
narrative_ontology:cs_axiom_status(commons_contributors_bear_restriction_cost, holdable).
narrative_ontology:cs_axiom_grounding('2de16f36-d3af-40f6-a99e-fffa4a3d84dc', commons_contributors_bear_restriction_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('2de16f36-d3af-40f6-a99e-fffa4a3d84dc', reciprocity_as_market_restriction).
narrative_ontology:cs_drift_state('2de16f36-d3af-40f6-a99e-fffa4a3d84dc', contemporary_cloud_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2de16f36-d3af-40f6-a99e-fffa4a3d84dc', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, enterprise_integrators).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, strong_reciprocity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the GPL license text and enforce its terms through copyright infringement litigation and compliance programs. Derive institutional legitimacy and mission-continuity from the reciprocity obligation's existence. Cannot easily abandon the kernel without dissolving the organization's foundational purpose.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_stewards, agenda_setter,
    institutional, generational, constrained, global).

% Sell proprietary software and services that compete with or substitute for GPL-licensed tools. Benefit from the integration barrier because it prevents commoditization of their product categories by GPL code and preserves customer demand for proprietary licensing. Can route around the constraint by funding clean-room implementations or acquiring proprietary substitutes.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Contribute code under GPL expecting to build a vibrant commons. Bear the cost of the restriction because their contributions are quarantined from the dominant proprietary software ecosystem, limiting adoption, downstream revenue opportunities, and economic sustainability. Exit is constrained because changing a project's license requires agreement from all past contributors.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors, payer,
    moderate, biographical, constrained, global).

% Enterprises seeking to integrate GPL components into proprietary products or SaaS offerings. Must either reciprocate by opening their entire stack, forego the integration, or bear substantial compliance and legal-review costs. Many choose to build proprietary alternatives instead of accepting the reciprocity obligation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, enterprise_integrators, payer,
    powerful, biographical, constrained, global).

% Advocate for MIT, BSD, or Apache-style licensing that permits proprietary integration without reciprocity. Structurally excluded from GPL project governance and licensing decision-making despite offering an alternative model that would reduce restriction costs for integrators and expand adoption for contributors.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, permissive_license_advocates, excluded,
    moderate, biographical, mobile, global).

% Research and analyze the economic and social effects of different licensing regimes. Track adoption rates, contribution patterns, enforcement actions, and market dynamics between proprietary and copyleft ecosystems.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, software_industry_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_restriction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes a commons of software by requiring that derivative works be distributed under the same license terms, creating a self-sustaining pool of reciprocally available source code.
% TRANSFER_FUNCTION: Moves potential market-integration value and downstream adoption opportunities from commons contributors and enterprise integrators to proprietary software vendors by legally quarantining GPL code from proprietary ecosystems, forcing either reciprocation or investment in proprietary alternatives.
% ABSENT_VOICES: Permissive licensing advocates who argue that reciprocity mandates stifle commons growth, and enterprise architects who would integrate GPL components into proprietary stacks if the reciprocity obligation were removable. They are structurally excluded from GPL project governance and licensing decision-making.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished, enterprises would rapidly incorporate GPL code into proprietary products without source release, proprietary vendors would face immediate commoditization pressure from integrated commons code, and the copyleft commons would likely fragment into permissive-licensed and proprietary-enclosed variants.
% FOUNDING_PROBLEM: Software commons suffered from free-rider enclosure: contributors released source code that was incorporated into proprietary products without reciprocation, eroding contributor incentives and commons sustainability.
% FOUNDING_PROBLEM_CORROBORATION: Empirical software engineering researchers and open-source sustainability analysts from outside the proprietary vendor beneficiary set document historical free-riding but contest whether mandatory reciprocity was the appropriate remedy; the Open Source Initiative notes alternative sustainability models (corporate sponsorship, SaaS hosting, foundation governance) that arose without copyleft mandates.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the reciprocity obligation forecloses substantial business-model options for integrators and quarantines commons contributors from the largest software market segments. Suppression (0.58) is moderate-high: the constraint actively suppresses proprietary integration via copyright enforcement, though clean-room alternatives and permissive licenses provide incomplete escape routes. Theater_ratio (0.40) reflects that the 'freedom' rhetoric surrounding GPL has genuine ideological content but also serves to mask the restriction's market-segmentation effects. Resistance (0.75) is high due to decades of license proliferation, GPL avoidance strategies, and corporate lobbying for permissive alternatives. Accessibility_collapse (0.65) captures that while alternative licenses exist, GPL network effects in certain domains (Linux kernel, GNU toolchain) create substantial switching costs. The measurement series run on one shared time grid; suppression shows a rise-and-fall pattern tracking the shift from distribution-based software to SaaS/cloud delivery.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (FSF) experiences the constraint as a necessary institutional technology defending a threatened commons; the proprietary vendor seat experiences it as a welcome market moat that preserves demand for proprietary alternatives; the commons contributor seat experiences it as a noble cage that limits adoption and monetization; the enterprise integrator seat experiences it as a legal minefield that forces expensive architectural workarounds. These divergences are structurally determined by directionalities derived from beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary_software_vendors are declared beneficiaries with powerful/mobile exit, placing derived d near the beneficiary end (low effective extraction, net subsidy in the form of preserved market share). Commons_contributors are declared victims with moderate/constrained exit, placing derived d near the target end (high effective extraction from foregone integration opportunities). Enterprise_integrators are payers with powerful/constrained exit â their global power lowers d somewhat, but their legal constraint and compliance costs keep it elevated. The engine computes seat-specific Ï values reflecting these structural asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by insisting on both coordination and extraction components. Pure coordination (Rope) would require no identifiable victims and no asymmetric extraction; pure extraction (Snare) would require no genuine coordination function. The GPL clearly coordinates a commons (the reciprocal code pool), but this reading demonstrates that the same structure simultaneously extracts by restricting market participation and benefiting proprietary vendors. The Tangled Rope classification captures this hybridity without collapsing into either institutional apologetics or cynical reductionism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gpl_saas_enforcement_gap,
    'Does the SaaS/cloud deployment model (where software is not ''distributed'') functionally nullify the GPL reciprocity obligation''s restriction, or has the extraction shifted to different domains?',
    'Empirical analysis of GPL enforcement actions in the 2015-2025 period: if enforcement targets traditional distribution but ignores SaaS, the constraint''s effective scope has narrowed; if AGPL adoption rates indicate structural adaptation, the restriction has migrated rather than attenuated.',
    'If the SaaS gap is real and unpatched by AGPL, the constraint''s extractiveness is lower than measured for the SaaS sector but remains high for embedded/installed software; this would support a decomposition into separate constraints for distribution versus service models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gpl_saas_enforcement_gap, empirical, 'SaaS loophole impact on GPL restriction effectiveness').

omega_variable(
    intentional_structure_divergence,
    'Is the restriction on proprietary integration an intended feature of copyleft as claimed by its designers, or an unintended structural side effect that diverges from contributor intent?',
    'Contributor surveys and license-choice regression analysis: if contributors systematically misunderstand the GPL''s business-model implications, the victim structure is emergent rather than designed; if contributors knowingly accept the restriction, the extraction is consensual.',
    'If contributors are uninformed victims, the constraint leans toward snare; if informed, the classification remains tangled_rope with consensual payer participation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intentional_structure_divergence, conceptual, 'Divergence between contributor intent and structural restriction effect').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_restrict_tr_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gpl_restrict_tr_t8, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(gpl_restrict_tr_t16, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(gpl_restrict_tr_t24, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(gpl_restrict_tr_t32, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 32, 0.47).
narrative_ontology:measurement(gpl_restrict_tr_t40, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(gpl_restrict_be_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gpl_restrict_be_t8, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(gpl_restrict_be_t16, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(gpl_restrict_be_t24, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(gpl_restrict_be_t32, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 32, 0.71).
narrative_ontology:measurement(gpl_restrict_be_t40, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gpl_restrict_su_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gpl_restrict_su_t8, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(gpl_restrict_su_t16, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(gpl_restrict_su_t24, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(gpl_restrict_su_t32, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(gpl_restrict_su_t40, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'GPL copyleft' decomposes into three structurally distinct constraints per the Îµ-invariance principle. This reading (restriction) treats the reciprocity obligation as asymmetric market extraction; the freedom reading treats it as user-rights preservation with negligible extraction; the commons reading treats it as institutional anti-enclosure technology. They form a constraint family linked by shared kernel but divergent Îµ values and stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
