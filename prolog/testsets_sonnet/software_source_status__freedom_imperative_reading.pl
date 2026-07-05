% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__freedom_imperative_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Proprietary Software Licensing as Categorical Injustice (Freedom Imperative Reading)
 *   domain: software_engineering/political_economy/ethics
 *
 * SUMMARY:
 *   This story instantiates the freedom-imperative reading of the
 *   software_source_status kernel: the claim that software freedom (to run,
 *   study, modify, and redistribute) is a fundamental ethical entitlement,
 *   and that any license restricting source access is a categorical injustice
 *   regardless of context, market conditions, or development-quality
 *   outcomes. Under this reading, EVERY instance of proprietary licensing
 *   enters the victim set structurally, because the wrong is located in the
 *   denial of the freedoms themselves, not in any downstream harm (security
 *   defect, lock-in cost, or repair barrier) that might or might not follow.
 *   This is a different, and more extractive, constraint than the
 *   pragmatic-development reading (which treats freedom as instrumentally
 *   valuable for code quality, not categorically required) or the
 *   property-rights reading (which treats source withholding as a legitimate
 *   exercise of authorial control, with no victims at all). Those are
 *   separate constraints, authored separately, and linked via
 *   network.affects_constraints — this file does not average across them or
 *   hedge its epsilon between them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.71).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.62).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Proprietary Software Licensing as Categorical Injustice (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "software_engineering/political_economy/ethics").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, '478da145-51c7-4bea-aa07-d5982ffe27fa').
narrative_ontology:cs_kernel_codification('478da145-51c7-4bea-aa07-d5982ffe27fa', distributed).
narrative_ontology:cs_authority_grounding('478da145-51c7-4bea-aa07-d5982ffe27fa', distributed).
narrative_ontology:cs_reading_relation('478da145-51c7-4bea-aa07-d5982ffe27fa', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('478da145-51c7-4bea-aa07-d5982ffe27fa', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('478da145-51c7-4bea-aa07-d5982ffe27fa', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('478da145-51c7-4bea-aa07-d5982ffe27fa', foundational, software_freedom_is_inalienable_right).
narrative_ontology:cs_axiom_status(software_freedom_is_inalienable_right, holdable).
narrative_ontology:cs_axiom_grounding('478da145-51c7-4bea-aa07-d5982ffe27fa', software_freedom_is_inalienable_right, deontological).
narrative_ontology:cs_axiom('478da145-51c7-4bea-aa07-d5982ffe27fa', foundational, source_withholding_is_categorically_unjust_regardless_of_outcome).
narrative_ontology:cs_axiom_status(source_withholding_is_categorically_unjust_regardless_of_outcome, holdable).
narrative_ontology:cs_axiom_grounding('478da145-51c7-4bea-aa07-d5982ffe27fa', source_withholding_is_categorically_unjust_regardless_of_outcome, deontological).
narrative_ontology:cs_reference_frame('478da145-51c7-4bea-aa07-d5982ffe27fa', four_freedoms_founding_charter).
narrative_ontology:cs_drift_state('478da145-51c7-4bea-aa07-d5982ffe27fa', contemporary_platform_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('478da145-51c7-4bea-aa07-d5982ffe27fa', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, copyright_holding_corporations).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, end_users_denied_source_access).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, downstream_developers_barred_from_modification).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, repair_and_interoperability_communities).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, software_freedom_as_inalienable_right).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, four_freedoms_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write license terms that withhold source code, prohibit reverse engineering, and forbid redistribution or modification. Enforce these terms through copyright litigation, DRM, and DMCA-style anti-circumvention law. Collect licensing revenue directly from the restriction itself, not merely from the software's use.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, proprietary_software_vendors, beneficiary).

% Hold portfolios of software copyrights and lobby for stronger legal protection of source concealment (extended copyright terms, anti-circumvention statutes, software patents). Benefit from a legal architecture that treats source withholding as a default right rather than a contestable choice.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, copyright_holding_corporations, beneficiary,
    institutional, civilizational, arbitrage, global).

% Run software whose behavior they cannot inspect, audit for malice or defect, or adapt to their own needs. Under this reading their inability to read, modify, and redistribute the code they depend on is a direct denial of a right they are owed, not merely an inconvenience of the market they participate in. Exit to free alternatives is often incomplete due to network effects, file-format lock-in, and hardware dependencies.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, end_users_denied_source_access, payer,
    powerless, biographical, constrained, global).

% Want to fix bugs, extend functionality, or build interoperable tools but are legally and technically barred from viewing or altering the underlying source. Under the freedom-imperative reading, this bar is not a legitimate property boundary but an imposed disability — a constraint that exists only because enforcement (litigation, licensing terms, technical protection measures) makes disobedience costly.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, downstream_developers_barred_from_modification, payer,
    moderate, biographical, constrained, global).

% Attempt to repair devices, build interoperable products, or preserve software against obsolescence, and are blocked by source concealment and anti-circumvention law. They lobby for right-to-repair and interoperability mandates but are structurally outside the license-drafting process entirely — their objections are litigated against, not negotiated with.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, repair_and_interoperability_communities, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, repair_and_interoperability_communities, excluded).

% Articulates the position this constraint story instantiates: that software freedom (to run, study, modify, and share) is a moral entitlement, and that its systematic denial by license architecture is an injustice comparable to other denials of basic liberty. Produces free-licensed alternatives (copyleft software) but cannot compel proprietary vendors to relicense; its voice appears in public argument, standards bodies, and legislative testimony, but rarely inside the license-drafting rooms of dominant vendors.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_movement, excluded,
    organized, civilizational, constrained, global).

% Study the effects of copyright, patent, and anti-circumvention law on software markets, competition, security, and repairability, and testify in legislative and antitrust proceedings without holding a stake in either the vendor or user side.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, legal_and_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At a minimal level, licensing terms coordinate expectations between authors and users about what may be done with a copy of a program, avoiding ad hoc disputes over use. This reading holds that this coordination function could be, and normatively should be, achieved without withholding source — the coordination need does not require the restriction.
% TRANSFER_FUNCTION: Moves control over inspection, modification, and redistribution of software away from everyone who runs it and concentrates it exclusively in the copyright holder, converting what would otherwise be a shared technical commons into a metered, gatekept asset; enforcement (litigation, DRM, anti-circumvention statutes) is the mechanism that keeps this concentration in place against the technical fact that copying and modification are otherwise nearly costless.
% ABSENT_VOICES: Users who cannot audit the software controlling their devices, medical equipment, or personal data are not party to the license terms they are bound by. Independent repair technicians and accessibility developers who could extend or fix software are excluded from the design conversation entirely and encounter the restriction only as an enforcement action.
% DISAPPEARANCE_RATIONALE: If source-withholding licensing and its legal enforcement machinery vanished overnight, the entire proprietary software business model built on selling restricted binaries would collapse or be forced to pivot to services, support, or hardware; a large share of vendor revenue depends specifically on the enforceability of the restriction, not on the software's mere existence.
% FOUNDING_PROBLEM: Early software distribution needed some legal category to let authors control unauthorized commercial copying and to fund continued development; copyright-style exclusivity was adapted from other media to serve that funding need.
% FOUNDING_PROBLEM_CORROBORATION: Vendors and copyright holders attest the restriction remains necessary to fund development. The Free Software Foundation and allied movement voices — external to the vendor beneficiary set — attest that the funding problem has long been solvable through alternative models (service revenue, support contracts, copyleft-compatible commercial licensing, public funding) and that the restriction now persists primarily as rent extraction rather than as the only viable funding mechanism; this corroboration is itself a party to the underlying dispute, so it is offered as an outside-the-beneficiary-set voice, not a neutral one.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 by interval end) because, under this reading, the withholding of source from every user of proprietary software is itself the extraction — a rights denial that scales with every license sold, not merely a cost imposed in edge cases. Suppression is authored substantial but lower than extractiveness (0.62) because the mechanism runs through law (copyright, anti-circumvention statutes, EULA enforcement) rather than direct physical coercion; accessibility_collapse is moderate (0.58) because free-software alternatives exist and are growing but remain incomplete substitutes due to network effects and hardware/format lock-in. Resistance is high (0.74) reflecting an organized, decades-old movement (FSF, GNU project, copyleft licensing, right-to-repair coalitions) actively contesting the arrangement — this is not a quietly-accepted constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor/agenda-setter seat, licensing is a legitimate exercise of authorship and a necessary funding mechanism — this is the property-rights reading's territory, not this file's. From this reading's seat, the same restriction is a categorical wrong regardless of funding necessity, because the entitlement to source access is held to be inalienable. The engine computes the payer seats as facing extraction near the target end and the vendor seats as beneficiaries; the divergence between those computed seats and the vendors' own self-story (funding legitimate development) is exactly the gap this reading exists to name.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors and copyright-holding corporations sit at the beneficiary end: they draft the restriction, enforce it, and collect licensing revenue specifically contingent on the restriction holding. End users, downstream developers, and repair/interoperability communities sit at the target end: under this reading their inability to inspect, modify, or redistribute software is the direct harm the constraint imposes, independent of whether the software itself functions well. Their exit options are constrained rather than trapped because free-software alternatives exist for many use cases, but incomplete substitutability (proprietary formats, hardware drivers, enterprise software with no free equivalent) keeps most users and developers short of genuine mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (funding continued software development) is contested as live or dead: this reading holds that alternative funding models (service contracts, copyleft-compatible commercial licensing, public and foundation funding) have long since solved the funding problem the restriction was originally introduced to address, and that the restriction now persists primarily as rent extraction rather than as the only viable mechanism — a mandatrophy pattern in which the original coordination justification (funding) has been substantially superseded but the restrictive mandate (source withholding) persists via enforcement rather than by continued necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_instrumental_wrong,
    'Is the wrong of proprietary licensing located categorically in the denial of the four freedoms themselves, or only instrumentally in the harms (security opacity, lock-in, repair barriers) that sometimes follow from that denial?',
    'This is fundamentally a normative/conceptual question not resolvable by empirical data alone — it turns on whether one accepts a rights-based framework (freedom as inalienable entitlement) or a consequentialist framework (freedom as instrumentally valuable when it produces better outcomes). The pragmatic_development_reading and property_rights_reading sibling constraints instantiate the alternative framings.',
    'If the categorical framing is rejected in favor of an instrumental one, the victim set collapses to only those proprietary-software instances that actually produce measurable harm, sharply reducing this reading''s authored extractiveness and victim scope; if accepted, every instance of proprietary licensing remains a victim-generating instance regardless of outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_vs_instrumental_wrong, conceptual, 'Whether the wrong is categorical (rights-based) or instrumental (outcomes-based) — the central fault line between this reading and its siblings.').

omega_variable(
    funding_alternative_sufficiency,
    'Are non-restrictive funding models (service revenue, support contracts, copyleft-compatible commercial licensing, public/foundation funding) actually sufficient to sustain the current volume and quality of software development across all domains, or only in some sectors?',
    'Comparative empirical analysis of revenue models and development sustainability across proprietary and free/open-source software sectors, including enterprise, embedded, and safety-critical domains where free-software adoption has been slower.',
    'If alternative funding is broadly sufficient, the founding_problem_status assessment of ''contested'' should shift toward ''dead'' with higher confidence, strengthening the mandatrophy reading; if funding is genuinely insufficient in significant sectors (e.g. capital-intensive safety-critical software), the founding problem retains more force as ''live'' in those sectors specifically, which would argue for a more sector-differentiated constraint than this single global story captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_alternative_sufficiency, empirical, 'Whether alternative funding models are sufficient across all software sectors or only some.').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel here best framed as ''what does the author of software owe the people who run it'' (an obligation-of-creation framing, which this reading adopts) or as ''what property rights does a creator hold over a non-rivalrous good'' (a property-definition framing, closer to the property_rights_reading)? The two framings produce structurally different cs_pattern classifications even though both describe the same licensing texts.',
    'No empirical resolution exists; this is a framing choice that different legal and ethical traditions have made differently (continental droit d''auteur moral-rights traditions lean toward the obligation framing; Anglo-American copyright-as-property traditions lean toward the property framing).',
    'Under the obligation framing (adopted here), authority_grounding reads as extraction (vendors extract benefit from denying an obligation) and the axiom of inalienable software freedom is coherent. Under the property framing, authority_grounding would read closer to a legitimate-property-right pattern with no victims at all, which is exactly the property_rights_reading sibling constraint''s territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framings (obligation-of-creation vs. property-definition) yield different classifications for the same licensing text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__freedom_imperative_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(soft_tr_t8, software_source_status__freedom_imperative_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(soft_tr_t16, software_source_status__freedom_imperative_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(soft_tr_t24, software_source_status__freedom_imperative_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(soft_tr_t32, software_source_status__freedom_imperative_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(soft_tr_t40, software_source_status__freedom_imperative_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__freedom_imperative_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(soft_be_t8, software_source_status__freedom_imperative_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(soft_be_t16, software_source_status__freedom_imperative_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(soft_be_t24, software_source_status__freedom_imperative_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(soft_be_t32, software_source_status__freedom_imperative_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(soft_be_t40, software_source_status__freedom_imperative_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__freedom_imperative_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(soft_su_t8, software_source_status__freedom_imperative_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(soft_su_t16, software_source_status__freedom_imperative_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(soft_su_t24, software_source_status__freedom_imperative_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(soft_su_t32, software_source_status__freedom_imperative_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(soft_su_t40, software_source_status__freedom_imperative_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(software_source_status__freedom_imperative_reading, 0.08).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language label 'the software freedom debate' into structurally distinct claims per the kernel/reading discipline: freedom_imperative_reading (this file — categorical rights violation, maximal victim set), pragmatic_development_reading (instrumental quality claim, smaller victim set gated on actual quality/security harms), property_rights_reading (legitimate property exercise, no victims), and utilitarian_hybrid_reading (context-dependent welfare-maximization, victim set determined case-by-case). Each carries its own epsilon and classification; they are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
