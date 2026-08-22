% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Freedom Reading)
 *   domain: software/intellectual_property/open_source
 *
 * SUMMARY:
 *   The GNU General Public License (GPL) is a free software license that
 *   requires derivative works to be licensed under the same terms. This
 *   reading instantiates the GPL as a technology of freedom: the reciprocity
 *   obligation (the viral clause requiring source code disclosure and freedom
 *   preservation) prevents proprietary vendors from capturing user freedoms
 *   by integrating GPL code into closed products. From this reading's
 *   position, the GPL constrains proprietary integrators not to extract
 *   unfair value from downstream users but to preserve user autonomy and
 *   software auditability. The beneficiaries are downstream users and future
 *   free software developers; the payers are proprietary vendors and
 *   closed-source integrators whose business models depend on restricting
 *   access to source code.
 *
 * KEY AGENTS:
 *   - downstream_users: end-users of GPL software; benefit from the right to inspect, modify, and redistribute; trapped or mobile depending on platform
 *   - free_software_developers: contributors to GPL codebases; benefit from the commons protection and freedom to build on prior work; moderate power, generational horizon
 *   - proprietary_integrators: vendors seeking to incorporate GPL code into closed products; experience the reciprocity obligation as a constraint on their business model; powerful but constrained exit
 *   - licensing_authors: set the GPL terms and enforce them; moderate power, generational time horizon
 *   - open_source_advocates: observe and shape the narrative about copyleft; organized, analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.38).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.72).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation (Copyleft as Freedom Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software/intellectual_property/open_source").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '66a1f56c-784d-4799-841a-e498ceaa3798').
narrative_ontology:cs_kernel_codification('66a1f56c-784d-4799-841a-e498ceaa3798', fixed_text).
narrative_ontology:cs_authority_grounding('66a1f56c-784d-4799-841a-e498ceaa3798', lineage).
narrative_ontology:cs_interpretation_layer_present('66a1f56c-784d-4799-841a-e498ceaa3798').
narrative_ontology:cs_reading_relation('66a1f56c-784d-4799-841a-e498ceaa3798', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('66a1f56c-784d-4799-841a-e498ceaa3798', gpl_reciprocity_obligation__copyleft_as_commons_reading, influences).
narrative_ontology:cs_axiom('66a1f56c-784d-4799-841a-e498ceaa3798', foundational, user_software_freedom_doctrine).
narrative_ontology:cs_axiom_status(user_software_freedom_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('66a1f56c-784d-4799-841a-e498ceaa3798', user_software_freedom_doctrine, deontological).
narrative_ontology:cs_axiom('66a1f56c-784d-4799-841a-e498ceaa3798', foundational, proprietary_capture_as_user_harm).
narrative_ontology:cs_axiom_status(proprietary_capture_as_user_harm, holdable).
narrative_ontology:cs_axiom_grounding('66a1f56c-784d-4799-841a-e498ceaa3798', proprietary_capture_as_user_harm, deontological).
narrative_ontology:cs_reference_frame('66a1f56c-784d-4799-841a-e498ceaa3798', software_freedom_as_user_right).
narrative_ontology:cs_drift_state('66a1f56c-784d-4799-841a-e498ceaa3798', contemporary_cloud_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('66a1f56c-784d-4799-841a-e498ceaa3798', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_developers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, closed_source_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_open_source_firms).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_open_source_firms).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, user_software_freedom_doctrine).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commons_reciprocity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive software under GPL with guaranteed right to inspect source code, modify it for their own use, and redistribute modified versions with the same freedoms intact. The reciprocity obligation ensures that anyone downstream from them maintains these same rights. They can fork, audit, and adapt without permission.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    powerless, biographical, mobile, global).

% Contribute to GPL codebases knowing their work will be protected from proprietary capture. They can build on each other's work freely and know that commercial entities cannot lock their contributions into closed products. The reciprocity obligation creates a commons where contributions remain accessible and free-buildable for future developers.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_developers, beneficiary,
    moderate, generational, mobile, global).

% Cannot incorporate GPL code into proprietary products without open-sourcing their derivative work or avoiding GPL code altogether. Exit options: release proprietary code, restructure the integration, or choose differently licensed components. The suppression is structural: GPL terms are non-negotiable and enforced by copyright law.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Must either license proprietary code separately, avoid GPL entirely, or accept that incorporating GPL code will require opening their source. Their business model—selling closed code—is structurally incompatible with the viral obligation. Exit: redesign architecture, pay for dual licensing, litigate the GPL's enforceability, or switch to permissive licenses.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, closed_source_vendors, payer,
    powerful, biographical, constrained, global).

% Distribute GPL software and earn revenue through support contracts, hosting, consulting, and proprietary add-ons that sit outside the GPL obligation. They benefit from the GPL commons while navigating the constraint by selling services rather than restrictive licensing. The constraint limits proprietary extensions but does not exclude their business model.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_open_source_firms, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_open_source_firms, payer).

% Choose the GPL license for their projects and enforce it through legal action when necessary. They set the terms of the reciprocity obligation and decide what counts as a derivative work. Their power is leveraged through copyright law and community enforcement (cease-and-desist, litigation, forks, reputation damage).
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, licensing_authors, agenda_setter,
    moderate, generational, mobile, global).

% Monitor GPL adoption, document enforcement actions, produce licensing guidance, and argue the case for copyleft as a technology of freedom in policy forums. They take testimony from all seats, conduct legal analysis, and shape the narrative about copyleft—but do not directly control the constraint's operation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_advocates, observer,
    organized, generational, analytical, global).

% Would offer proprietary licensing of the same code for a fee to allow companies to avoid the GPL obligation, but are barred from doing so by GPL authors' refusal to grant proprietary licenses or by the GPL's irrevocability. Their exclusion is structural to the GPL's mechanism.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, dual_licensing_shops, excluded,
    powerful, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of open-source contribution: ensures that once code is contributed to a commons, it cannot be captured by downstream integrators and locked away. Each contributor knows their work will remain free, which removes the fear of contributing to projects later commercialized without permission.
% TRANSFER_FUNCTION: Transfers the right to integrate copyrighted code without restriction. Under proprietary licensing, the integrator pays for that right. Under GPL, the integrator can integrate freely only if they open-source their derivative. The reciprocity obligation transfers freedom-to-fork from proprietary vendors to downstream users and future developers.
% ABSENT_VOICES: Proprietary vendors who would prefer closed integration without reciprocity; firms that would profit from sole proprietary licensing of widely-used components; users of proprietary software who cannot inspect or modify what they depend on would advocate for their inclusion in the GPL commons if they had a seat.
% DISAPPEARANCE_RATIONALE: If GPL reciprocity vanished, proprietary vendors would immediately integrate GPL components into closed products, the commons would fragment as projects were captured, and downstream users would lose the guaranteed right to inspect, modify, and redistribute. The open-source ecosystem would reorganize around permissive licenses (MIT, Apache 2.0) where proprietary capture is faster and easier.
% FOUNDING_PROBLEM: Software copyright enables proprietary lock-in: authors can distribute binaries without source, making code uninspectable and unmodifiable for users. Open-source developers needed a tool to prevent downstream proprietary capture while still allowing commercial use.
% FOUNDING_PROBLEM_CORROBORATION: Free Software Foundation and GPL authors attest that proprietary lock-in remains the governing threat. Commercial open-source companies (Red Hat, Canonical, Elastic) attest that GPL copyleft successfully prevents vendor lock-in and structures their business models. Downstream users and security researchers attest that the ability to inspect and modify GPL code enables audits and patches. Proprietary vendors attest that GPL is a real constraint on their strategy, confirming the founding problem's persistence.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end): the GPL does impose a real constraint on proprietary integrators, but the constraint is framed as preventing extraction rather than performing extraction itself. From the reading's own lights, the GPL protects downstream users FROM extraction by proprietary vendors; the constraint on proprietary vendors is the means, not the end. Suppression is high (0.72) because the constraint's persistence depends on active enforcement through copyright law and community mechanisms (license violations prosecuted, proprietary incorporations blocked by copyright claims, forks and reputation damage for violators). Theater is low-to-moderate (0.22): the functional purpose (preventing proprietary capture) is real and clearly measurable; the theatricality that grows over time reflects increasing investment in license compliance ceremonies and dual-licensing negotiations that do not directly advance the copyleft function itself. The measurement series show a slight rise in extractiveness and theater early (as more copyleft frameworks emerge and licensing complexity increases) before stabilizing, reflecting the constraint's maturation into a stable institutional form.
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary integrator's seat, the constraint operates as a restriction that prevents them from using GPL code without surrendering proprietary control—it is experienced as a cost, not a coordination benefit. From the downstream-user seat, the same constraint is experienced as freedom protection: without it, proprietary vendors would lock away derivative improvements and security patches. The engine computes different types from each seat because the structural relationship to the constraint differs: the proprietary integrator's directionality is high (d near 1.0—they are the target, bearing the cost), while the downstream user's directionality is low (d near 0.0—they are the beneficiary, receiving the protection). This reading claims rope because the coordination function (commons protection) is genuine and both parties understand the trade-off; the computed type for each seat will differ because extraction is asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users and free software developers are structurally beneficiaries: they receive freedom guarantees and access to code that proprietary vendors would otherwise restrict. Their directionality is low (near 0.0) because the constraint subsidizes them relative to proprietary users. Proprietary integrators and closed-source vendors are the targets: they cannot extract monopoly rent from locking down GPL-derived improvements. Their directionality is high (near 0.8–1.0) because the constraint prevents them from capturing value they would otherwise collect. Commercial open-source firms sit between: they benefit from the GPL commons but must navigate the reciprocity obligation, making them partially symmetric (d around 0.4–0.5). Power amplifies directionality: powerful proprietary vendors experience higher effective suppression (they have resources to fight the constraint legally and technically) while powerless downstream users experience lower effective suppression (exit is mobile—they simply use GPL code if available). The constraint's suppression is itself structural (copyright law + community enforcement) and does not scale with power, but its effect does.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy trap by clearly grounding the GPL's continued justification in the persistence of proprietary lock-in as a threat to user freedoms. The founding problem (software copyright enables proprietary lock-in) remains live: proprietary closed-source software is still the dominant form in many markets, users still cannot inspect or modify commercial software, and vendors still restrict access to source code. The constraint's continued operation is ratified by its beneficiaries (downstream users and open-source developers who choose GPL) and resisted by its payers (proprietary vendors who avoid GPL components or litigate the license's enforceability). If the founding problem were dead—if proprietary vendors voluntarily open-sourced all code and granted inspection rights—the GPL would become vestigial. The reading does not inherit mandatrophy because the founding problem's status is live, the constraint continues to prevent the identified harm, and the resistance from proprietary interests confirms that the constraint remains a real friction in their operations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_operationalization_ambiguity,
    'What constitutes ''user freedom'' in software: the right to read and modify source code, or the right to use software under any chosen license (including proprietary)?',
    'Discourse analysis of user behavior and regulatory hearings: do users prioritize code auditability and modifiability (supporting the freedom-as-inspection reading), or do they prioritize the ability to use proprietary software without license restrictions (supporting an alternative reading where copyleft is a restriction on freedom rather than a preservation of it)?',
    'If freedom is understood as inspection + modification rights, the GPL preserves freedom by enforcing reciprocity. If freedom is understood as license-choice, the GPL restricts freedom by forbidding proprietary licensing. The reading''s classification rests on this definitional choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_operationalization_ambiguity, conceptual, 'Whether user freedom in the freedom-reading refers to code auditability or license choice.').

omega_variable(
    proprietary_capture_threat_contingency,
    'Is proprietary lock-in of GPL-derived code a real ongoing threat to downstream users, or is this threat adequately mitigated by the availability of alternative open-source options?',
    'Empirical: surveying integrator attempts to incorporate GPL code into closed products (DMCA exemptions, license violation patterns), monitoring the prevalence of proprietary forks of GPL software, and tracking whether downstream users face practical friction in accessing GPL alternatives.',
    'If proprietary capture remains a live threat (integrators continuously attempt proprietary lock-in, alternatives are scarce in specific domains), the GPL''s founding problem is live and the constraint''s classification as rope (genuine coordination) is supported. If proprietary capture is rare and alternatives abundant, the founding problem has substantially died and the constraint drifts toward mandatrophy (enforcing a dead covenant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_capture_threat_contingency, empirical, 'Whether proprietary lock-in of GPL-derived code remains a live threat or has been substantially mitigated.').

omega_variable(
    commons_vs_freedom_distinction,
    'Does this reading''s core claim (freedom-as-prevention-of-proprietary-capture) differ meaningfully from the commons-reading''s core claim (reciprocity-as-commons-stewardship), or do they describe the same mechanism from different rhetorical angles?',
    'Structural analysis: the freedom-reading prioritizes user autonomy and access rights (individual liberty frame); the commons-reading prioritizes shared resource sustainability and collective stewardship (institutional commons frame). Practical divergence would appear in policy recommendations: freedom-reading supports user-level rights (right to fork, right to audit); commons-reading supports institutional-level rules (mandatory contribution back, anti-enclosure provisions). If both readings endorse identical policies, they are the same constraint described differently; if they endorse different policies (or conflict in edge cases like dual-licensing), they are distinct constraints.',
    'If the readings are genuinely distinct, they are two separate constraint stories with different ε values. If they are the same constraint re-narrated, one should be marked as redundant and consolidated with the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_vs_freedom_distinction, conceptual, 'Whether the freedom-reading and commons-reading are structurally distinct constraints or the same constraint narrated from different positions.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high suppression (0.72) experienced by proprietary integrators primarily structural (copyright law + legal enforcement) or internalized (integrators have internalized the norm that proprietary lock-in is ethically indefensible)?',
    'Post-GPL landscape analysis: if integrators cease to suppress GPL constraints after legal enforcement weakens (copyright exceptions, licensing reforms), the suppression is structural. If they continue to respect the copyleft obligation even under reduced legal enforcement (as happens with norms that have become culturally embedded), the suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is more stable and self-sustaining than legal enforcement alone suggests; if purely structural, enforcement capacity is the critical variable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of proprietary integration is structural or internalized.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the freedom-reading''s core claim (freedom-is-preserved-by-preventing-proprietary-capture) logically foreclose the restriction-reading''s core claim (copyleft-restricts-business-models) or do both claims coexist?',
    'Logical analysis: the freedom-reading says the GPL preserves freedom by constraining proprietary integration. The restriction-reading says the GPL restricts business freedom by forbidding proprietary licensing. These are compatible if freedom is understood differently in each (user-freedom vs. vendor-freedom); they foreclose each other only if freedom is assumed to have a single referent. If the readings agree on the meaning of freedom, one must forecast the other.',
    'If the readings foreclose each other, one is a false alternative reading (a misinterpretation rather than a legitimate alternative). If they coexist, the GPL constraint genuinely admits multiple readings from different seats, and all three stories should be authored as separate constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the freedom-reading and restriction-reading logically foreclose or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t30, observed).
narrative_ontology:measurement(gpl__tr_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 35, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(gpl__be_t25, observed).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(gpl__be_t30, observed).
narrative_ontology:measurement(gpl__be_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement_basis(gpl__be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(gpl__su_t25, observed).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(gpl__su_t30, observed).
narrative_ontology:measurement(gpl__su_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(gpl__su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.12).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_software_licensing).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_business_models).

% DUAL FORMULATION NOTE:
% The GPL reciprocity obligation is a contested kernel with multiple readings. This story instantiates the copyleft-as-freedom reading (viral licensing preserves user freedoms by preventing proprietary capture). Sibling readings—copyleft-as-restriction and copyleft-as-commons—author different ε values and beneficiary/victim structures from the same GPL mechanism, because they attribute different founding problems and different extraction targets. All three are members of the gpl_reciprocity_obligation constraint family and are linked via affects_constraints; each is authored as a self-contained, ε-invariant constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, powerful, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
