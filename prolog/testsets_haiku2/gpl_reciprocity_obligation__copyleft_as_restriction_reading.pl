% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: GPL Copyleft Reciprocity as Proprietary Business Model Constraint
 *   domain: intellectual_property/software_licensing/open_source
 *
 * SUMMARY:
 *   GPL copyleft—the requirement that derivative works be distributed under
 *   compatible free-software licenses—is read in this constraint as a
 *   restriction on proprietary vendors' business models. From the restriction
 *   reading, the copyleft obligation functions as a coercive gate:
 *   proprietary firms that wish to use GPL-licensed code must choose between
 *   full source disclosure (destroying proprietary advantage) or forgoing the
 *   code entirely. The constraint's structural form is a snare from the
 *   proprietary vendor's perspective: they are blocked from a valuable
 *   resource (commons code) unless they surrender something essential
 *   (proprietary control). Commons contributors and enforcement communities
 *   maintain the constraint through copyright law and litigation. This is ONE
 *   reading of the GPL reciprocity obligation kernel—other readings frame the
 *   same text as a freedom-preservation mechanism or as commons-institutional
 *   technology. The claim and metrics are independent: the constraint is
 *   CLAIMED as snare (coercive extraction from proprietary vendors) while the
 *   authored metrics describe a gradually-stabilizing enforcement profile
 *   (extractiveness rises then plateaus; suppression falls as voluntary
 *   compliance rises; theater remains low because enforcement is real).
 *
 * KEY AGENTS:
 *   - proprietary_vendors: Powerful institutional actors paying the full cost of the constraint through exclusion from GPL-derivative business models (d near 1.0)
 *   - commons_contributors: Organized beneficiaries maintaining the commons via forced reciprocity; choosing copyleft specifically to prevent vendor capture (d near 0.0)
 *   - dual_licensed_vendors: Agenda-setter; institutional power to interpret GPL terms and manage exemptions; benefit from dual-licensing arbitrage
 *   - license_enforcement_communities: Organized agenda-setter; distributed copyright ownership allows any GPL holder to sue; enforcement machinery maintained ideologically, not for rent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.62).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.48).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, snare).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Copyleft Reciprocity as Proprietary Business Model Constraint").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "intellectual_property/software_licensing/open_source").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '7425be23-ee41-4d5a-98c4-da94979885a1').
narrative_ontology:cs_kernel_codification('7425be23-ee41-4d5a-98c4-da94979885a1', fixed_text).
narrative_ontology:cs_authority_grounding('7425be23-ee41-4d5a-98c4-da94979885a1', lineage).
narrative_ontology:cs_interpretation_layer_present('7425be23-ee41-4d5a-98c4-da94979885a1').
narrative_ontology:cs_reading_relation('7425be23-ee41-4d5a-98c4-da94979885a1', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('7425be23-ee41-4d5a-98c4-da94979885a1', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('7425be23-ee41-4d5a-98c4-da94979885a1', foundational, proprietary_integration_should_be_permitted).
narrative_ontology:cs_axiom_status(proprietary_integration_should_be_permitted, holdable).
narrative_ontology:cs_axiom_grounding('7425be23-ee41-4d5a-98c4-da94979885a1', proprietary_integration_should_be_permitted, deontological).
narrative_ontology:cs_axiom('7425be23-ee41-4d5a-98c4-da94979885a1', secondary, software_property_rights_supersede_code_reuse_obligations).
narrative_ontology:cs_axiom_status(software_property_rights_supersede_code_reuse_obligations, holdable).
narrative_ontology:cs_axiom_grounding('7425be23-ee41-4d5a-98c4-da94979885a1', software_property_rights_supersede_code_reuse_obligations, deontological).
narrative_ontology:cs_reference_frame('7425be23-ee41-4d5a-98c4-da94979885a1', property_rights_commercial_software).
narrative_ontology:cs_drift_state('7425be23-ee41-4d5a-98c4-da94979885a1', contemporary_open_source_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7425be23-ee41-4d5a-98c4-da94979885a1', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, users_accessing_derivative_works).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, embedded_systems_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commercial software firms that wish to incorporate GPL-licensed code into proprietary products face an irreconcilable choice: release their entire product under GPL (losing proprietary control and competitive advantage) or avoid using GPL code entirely (forgoing access to high-quality libraries and components). They experience the constraint as a forced disclosure obligation tied to code reuse, not as a voluntary coordination mechanism. Exit requires either redesigning around GPL dependencies or licensing under compatible terms.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_vendors, payer,
    powerful, biographical, constrained, global).

% Developers who contribute to GPL-licensed projects benefit from mandatory derivative-work disclosure: any vendor who builds on their work must share improvements back to the commons, enlarging the commons-accessible codebase. They have chosen copyleft specifically to prevent proprietary capture. Their power derives from the accumulated utility of GPL projects, not from enforcement machinery—proprietary firms choose to use GPL code despite the constraint.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors, beneficiary,
    organized, generational, mobile, global).

% End-users of software built on GPL foundations gain access to the full source code and modification rights whenever they receive the software, because GPL requires redistribution of source-code equivalents. They benefit from the constraint's enforcement, though they may not be aware they are receiving GPL-encumbered code. Their exit option is to use proprietary alternatives when they exist.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, users_accessing_derivative_works, beneficiary,
    moderate, biographical, mobile, global).

% Companies that maintain the same codebase under both GPL and proprietary licenses (dual licensing) set and enforce the reciprocity obligation through GPL's written terms and community norm enforcement. They benefit from the dual-licensing arbitrage (proprietary licensees pay for exemption) and from commons growth funded by GPL users. They actively maintain the constraint through code-review gatekeeping and license-compliance monitoring.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_licensed_vendors, agenda_setter,
    institutional, biographical, arbitrage, global).

% Manufacturers of appliances, routers, and IoT devices that embed GPL code in firmware face severe disclosure burden: they must distribute source or offer written offers for source, imposing manufacturing and support costs. Many discover GPL obligations only after shipping. Their exclusion from the GPL drafting and maintenance communities means they have no seat in norm-setting, yet bear compliance costs. They experience the constraint as surprise enforcement.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, embedded_systems_vendors, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_restriction_reading, embedded_systems_vendors, excluded).

% Open-source legal organizations and GPL copyright holders that monitor compliance and sue or negotiate settlements for violations are the enforcement machinery. They use cease-and-desist letters, litigation, and public shaming to maintain the constraint. Their power is distributed across many independent actors (any GPL copyright holder can sue); their motivation is ideological (commons preservation) rather than rent-seeking.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, license_enforcement_communities, agenda_setter,
    organized, generational, mobile, global).

% Projects licensed under permissive licenses (MIT, Apache 2.0, BSD) that depend on GPL code are subject to the reciprocity obligation—their code must be relicensed or they must add GPL attribution. They are excluded from GPL's governance but bound by its terms; they have no voice in whether reciprocity standards shift.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, compatible_license_projects, excluded,
    moderate, generational, constrained, global).

% The Free Software Foundation and kernel maintainers (Linus Torvalds, etc.) who interpret GPL language and set de facto standards for what constitutes derivative work and compliance. They publish guidance on linking thresholds, linking exceptions (e.g., the Classpath exception), and enforcement priorities. They are observers because they do not enforce directly; they set the interpretive framework that enforcement communities use.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_drafting_authority, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_restriction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the commons by ensuring derivative works remain accessible to all users—solves the collective-action problem of preventing proprietary capture of collectively-built infrastructure. Without reciprocity, proprietary vendors can incorporate commons code while blocking access to improvements, creating a tragedy-of-the-commons scenario where the commons shrinks.
% TRANSFER_FUNCTION: Moves access and modification rights from proprietary vendors (who would hold them exclusively) to the wider user base and future commons contributors. Proprietary vendors transfer their ability to build proprietary derivatives in exchange for access to high-quality commons code. The transfer is not voluntary—it is a condition of code reuse.
% ABSENT_VOICES: Proprietary software users who want access to source code but see proprietary vendors as the legitimate authority over software they purchase; embedded-systems manufacturers who discover the obligation only after shipping millions of units; device owners who are denied the modification rights the constraint is meant to guarantee (because manufacturers hide GPL code in bootloaders or claim it is 'not software'). These groups would argue for either permissive licensing or narrower definitions of derivative work, but they are not in the GPL governance conversation.
% DISAPPEARANCE_RATIONALE: If GPL reciprocity vanished overnight, proprietary vendors would immediately incorporate high-value commons libraries without contribution obligations. The commons codebase would stagnate (vendor improvements would be proprietary) and fragment into proprietary branches. Within years, the commons would shrink to a maintenance backlog as new work concentrated in proprietary forks. The institutional arrangement that funds the commons through forced reciprocity would collapse.
% FOUNDING_PROBLEM: In the 1980s–1990s, proprietary vendors could legally take open-source software, incorporate it into proprietary products, and block users from accessing improvements—a form of commons enclosure where upstream improvement required community action but downstream benefit accrued to proprietary firms. GPL was designed to prevent this asymmetry by requiring vendors to share back.
% FOUNDING_PROBLEM_CORROBORATION: Commons advocates and GPL copyright holders attest the problem is live: continuous litigation and enforcement campaigns document proprietary violations (Linux kernel in routers, GPL code in set-top boxes). Proprietary vendors and embedded-systems manufacturers attest the problem was real but is substantially solved: source-availability demands are now mainstream, many vendors pay GPL license fees or comply voluntarily, and the market has adapted. Independent software historians note that proprietary capture did occur (e.g., SCO/Linux litigation, embedded routers), but that voluntary compliance now dominates enforcement.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.35 to 0.62 over the interval (t=0 to t=32) as proprietary vendors increasingly discover GPL obligations in supply chains and as litigation costs accumulate. The trajectory plateaus at t=20 onward, indicating the constraint reaches steady-state enforcement and vendor adaptation (many now budget for GPL licenses or compliance). Suppression FALLS from 0.72 to 0.48 as voluntary compliance increases—vendors internalize the obligation and suppliers pre-screen for license compliance, reducing the need for active enforcement machinery. Theater remains low (0.08→0.22) because enforcement is substantive: real source-code releases, real license disputes, real design-around costs for vendors. The constraint IS extractive (proprietary business models are prohibited from GPL-derivative space), but the extraction is not theatrical—it is enforced through durable legal and community mechanisms. The claim/metric gap is intended: proprietary vendors claim snare while commons advocates claim freedom-preservation; the engine computes per-seat classification from the structural data, revealing the divergence.
 *
 * PERSPECTIVAL GAP:
 *   Proprietary vendors and commons advocates are locked in a genuine adversarial relationship over the same constraint. From vendor perspective: 'GPL restricts my business model unfairly; I should have free choice to use or proprietary-ize code I build on.' From commons perspective: 'Reciprocity is freedom itself; without it, my work gets encased in proprietary black boxes and I lose the ability to improve my own software.' Neither perspective is externally 'wrong'—they are incommensurable value systems (property rights vs. commons commons rights). The constraint enforces one value system (commons rights) over the other (property rights), making it a snare from the property-rights perspective and a freedom mechanism from the commons perspective. The engine's per-seat classification captures this: payer seat computes snare; beneficiary seat computes rope or mountain depending on how naturalized the commons-rights frame is.
 *
 * DIRECTIONALITY LOGIC:
 *   From the proprietary-vendor perspective (powerful, constrained exit): the constraint extracts their choice to use GPL code without contribution; they experience d≈0.95 (near full target). From commons-contributor perspective (organized, mobile exit): the constraint protects their commons investment; they experience d≈0.05 (near full beneficiary). From dual-licensed-vendor perspective (institutional, arbitrage exit): the constraint sustains their dual-licensing revenue model; they experience d≈0.3 (moderate beneficiary, some cost to maintain enforcement). From embedded-systems-vendor perspective (powerful, constrained exit but excluded from governance): the constraint imposes surprise compliance costs; they experience d≈0.85 (high target, worse than proprietary-software vendors because they lack legal sophistication). The payer seat (proprietary vendors) and beneficiary seat (commons contributors) compute radically different types from the same structural data—that divergence is what the classification system detects.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint was built to solve the commons-enclosure problem (founding_problem: proprietary vendors incorporate code without contribution). The problem remains contested: commons advocates argue proprietary capture is still a live threat (citing continued GPL violations, embedded systems abuse, weak enforcement in some jurisdictions); proprietary vendors argue the problem is substantially solved (source-disclosure is now routine, many vendors comply, market has adapted). The constraint's mandate—forcing reciprocity to prevent enclosure—is live for commons advocates, contested for vendors. Mandatrophy does NOT apply here: the constraint still actively solves its intended problem (commons preservation through forced reciprocity), even though the founding problem's acuteness has shifted. The measurement trajectory (extractiveness→plateau, suppression→decline) reflects adaptation and normalization, not mandate-death.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_ambiguity,
    'What counts as a ''derivative work'' under GPL? Is dynamic linking derivative? Static linking? Syscall-based integration? This boundary is where the constraint''s enforcement power lives.',
    'Litigation and GPL-enforcer guidance documents establish case-law precedent; case-by-case adjudication (e.g., Jacobsen v. Katzer, Software Freedom Conservancy settlements) gradually clarify the boundary. Natural experiments from GPLv3''s linking exceptions and Classpath exception outcomes.',
    'If the derivative-work definition is narrow (only direct code incorporation is derivative), the constraint''s extractiveness falls sharply because vendors can use GPL libraries without disclosure. If narrow, proprietary_vendors move down in directionality (d drops, exit improves). If the definition is broad (even linking via syscalls or APIs is derivative), the constraint remains highly extractive. Classification can shift materially.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, conceptual, 'The structural question of what triggering condition activates the reciprocity obligation.').

omega_variable(
    enforcement_intensity_vs_voluntary_compliance,
    'Is the constraint maintained primarily through active litigation and enforcement (suppression is structural, external), or through vendor internalization of the norm (suppression is behavioral, internalized)?',
    'Trend analysis of enforcement actions per year; surveys of vendor understanding of GPL obligations; post-litigation behavior of non-compliant vendors (do they comply only under threat, or do they budget for compliance?); emergence of automated license-scanning tools (indicator of vendor-side norm adoption).',
    'If enforcement remains structural/external (litigation-driven), the constraint is more snare-like and the measured suppression is accurate. If compliance becomes internalized (vendors self-screen and budget), the suppression drops further and the constraint begins to look more like rope from vendor perspective—a norms-based coordination mechanism rather than coercive extraction. The trajectory (suppression falling from 0.72→0.48) is consistent with internalization; confirming it changes the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_intensity_vs_voluntary_compliance, empirical, 'Whether GPL reciprocity is maintained through external enforcement machinery or vendor-side norm internalization.').

omega_variable(
    commons_vs_commons_enclosure_reduction_ambiguity,
    'Does GPL succeed in preserving the commons (preventing enclosure), or does it succeed in reducing the rate of enclosure while still allowing substantial proprietary derivative-work creation (via dual licensing, exemption negotiation, or GPL violations)?',
    'Longitudinal analysis of proprietary derivative-work market: trends in proprietary code that incorporates or is derivative from GPL code (measured via code provenance, licensing audit trails, litigation documents). Comparison to pre-GPL landscape (1980s proprietary enclosure patterns) vs. post-GPL landscape.',
    'If GPL prevents enclosure (communes stays commons), the constraint is commons-institutional and the beneficiary seat (commons_contributors) should compute as rope (genuine coordination that works). If GPL reduces but does not eliminate enclosure, the constraint is Tangled Rope at best—it coordinates the commons AND extracts from vendors—or it is misclassified as snare. This omega is the deepest methodological question: the ''restriction reading'' assumes GPL IS restrictive-on-vendors (snare from vendor seat); but if commons enclosure persists substantially despite GPL, the ''restriction'' framing may be false-summit (a claimed snare that is actually ineffective rope, or coordination failure passed off as restriction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_vs_commons_enclosure_reduction_ambiguity, empirical, 'Whether GPL reciprocity achieves its stated goal of commons preservation or is a net-ineffective restriction that does not prevent enclosure.').

omega_variable(
    reading_foreclosure_risk,
    'Do the axioms of the RESTRICTION reading logically foreclose the sibling FREEDOM and COMMONS readings, or do all three readings coexist as incommensurable value systems?',
    'Conceptual analysis: a reading forecloses if accepting its core axiom requires denying the sibling''s core axiom within a single coherent framework. RESTRICTION reading''s axiom: proprietary integration should be permitted (property-rights frame). FREEDOM reading''s axiom: proprietary capture violates user freedoms (freedom-from-control frame). These are NOT logically contradictory—they are *value-system incommensurable*. Both can be true in different normative systems. Foreclosure does not apply; coexistence is correct.',
    'If the readings truly coexist (no foreclosure), the kernel is contested-and-alive, not resolved-by-elimination. The engine''s reader-consensus-averaging path should NOT treat this as a settled matter. The constraint family should carry at least three active readings with no hierarchy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_risk, conceptual, 'Whether sibling readings of the GPL kernel are logically foreclosed or value-system incommensurable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t4, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 4, 0.11).
narrative_ontology:measurement_basis(gpl__tr_t4, observed).
narrative_ontology:measurement(gpl__tr_t8, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement_basis(gpl__tr_t8, observed).
narrative_ontology:measurement(gpl__tr_t12, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(gpl__tr_t12, observed).
narrative_ontology:measurement(gpl__tr_t16, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement_basis(gpl__tr_t16, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t24, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t24, observed).
narrative_ontology:measurement(gpl__tr_t28, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 28, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t28, observed).
narrative_ontology:measurement(gpl__tr_t32, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t4, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement_basis(gpl__be_t4, observed).
narrative_ontology:measurement(gpl__be_t8, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(gpl__be_t8, observed).
narrative_ontology:measurement(gpl__be_t12, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement_basis(gpl__be_t12, observed).
narrative_ontology:measurement(gpl__be_t16, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(gpl__be_t16, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t24, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(gpl__be_t24, observed).
narrative_ontology:measurement(gpl__be_t28, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 28, 0.62).
narrative_ontology:measurement_basis(gpl__be_t28, observed).
narrative_ontology:measurement(gpl__be_t32, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(gpl__be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t4, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement_basis(gpl__su_t4, observed).
narrative_ontology:measurement(gpl__su_t8, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(gpl__su_t8, observed).
narrative_ontology:measurement(gpl__su_t12, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement_basis(gpl__su_t12, observed).
narrative_ontology:measurement(gpl__su_t16, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement_basis(gpl__su_t16, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t24, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement_basis(gpl__su_t24, observed).
narrative_ontology:measurement(gpl__su_t28, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 28, 0.48).
narrative_ontology:measurement_basis(gpl__su_t28, observed).
narrative_ontology:measurement(gpl__su_t32, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 32, 0.48).
narrative_ontology:measurement_basis(gpl__su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.12).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% The GPL reciprocity obligation kernel is decomposed into three constraint stories, one per reading: (1) RESTRICTION reading (this story): GPL as constraint on proprietary business models; beneficiary=commons advocates, victim=proprietary vendors. (2) FREEDOM reading: GPL as protection of user freedoms; beneficiary=users and developers. (3) COMMONS reading: GPL as commons-institutional technology. Each reading instantiates a different constraint with different ε (extractiveness referent), different beneficiary/victim structure, different claimed type. The three readings coexist in software-licensing discourse without logical foreclosure—they are incommensurable value systems (property rights, individual freedoms, commons rights). All three readings remain live; none rules out the others within a single coherent framework. The three files are linked via network.affects_constraints to establish the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
