% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation (Commons Preservation Reading)
 *   domain: software/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL implements reciprocal obligation as an institutional technology
 *   for commons preservation: code released under GPL may be used freely but
 *   must be returned to the commons under the same GPL terms when
 *   distributed. This reading frames the GPL as a constraint that prevents
 *   proprietary firms from enclosing software commons through integration and
 *   modification without returning improvements. The commons itself (Linux,
 *   GNU toolchain, web infrastructure) is the primary beneficiary. Individual
 *   exit-maximizers and proprietary integration firms bear the constraint
 *   because they cannot convert commons improvements into proprietary
 *   advantage—the reciprocal obligation forces a choice: open-source the
 *   product, pay for proprietary licensing, or use proprietary alternatives.
 *   This is a kernel reading of the GPL's reciprocity obligation; sibling
 *   readings interpret the same legal mechanism as a freedom-preservation
 *   instrument (focusing on user autonomy) or a restriction instrument
 *   (focusing on business constraints). This reading emphasizes institutional
 *   preservation of the commons as a non-enclosable resource.
 *
 * KEY AGENTS:
 *   - software_commons_as_institution: Primary beneficiary; the persistent, coordinated knowledge base preserved through reciprocal obligation (Linux kernel, GNU toolchain, web server software)
 *   - open_source_developers: Beneficiaries; participate in and build on the commons without vendor lock-in
 *   - individual_exit_maximizers: Victims; cannot integrate GPL code into proprietary products without releasing modifications
 *   - proprietary_integration_firms: Victims; powerful but constrained in their ability to capture commons improvements; must license proprietary alternatives or release under GPL
 *   - commercial_dual_licensing_operators: Agenda-setters; maintain GPL projects and enforce reciprocal obligation; benefit from both commons (community contributions) and proprietary licensing
 *   - legal_commons_advocates: Observers; stewards of GPL terms; enforce through litigation
 *   - cloud_platform_providers: Trapped payers; depend on GPL infrastructure but face AGPLv3 constraints on service modifications
 *   - embedded_systems_integrators: Constrained payers; cannot easily release embedded firmware under GPL without exposing internals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.48).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.32).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation (Commons Preservation Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'ff94a0b5-17b7-41d3-b1a5-384c99085934').
narrative_ontology:cs_kernel_codification('ff94a0b5-17b7-41d3-b1a5-384c99085934', fixed_text).
narrative_ontology:cs_authority_grounding('ff94a0b5-17b7-41d3-b1a5-384c99085934', extraction).
narrative_ontology:cs_interpretation_layer_present('ff94a0b5-17b7-41d3-b1a5-384c99085934').
narrative_ontology:cs_reading_relation('ff94a0b5-17b7-41d3-b1a5-384c99085934', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff94a0b5-17b7-41d3-b1a5-384c99085934', gpl_reciprocity_obligation__copyleft_as_restriction_reading, influences).
narrative_ontology:cs_axiom('ff94a0b5-17b7-41d3-b1a5-384c99085934', foundational, commons_as_institutional_beneficiary).
narrative_ontology:cs_axiom_status(commons_as_institutional_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('ff94a0b5-17b7-41d3-b1a5-384c99085934', commons_as_institutional_beneficiary, conventional).
narrative_ontology:cs_axiom('ff94a0b5-17b7-41d3-b1a5-384c99085934', foundational, enclosure_prevention_as_commons_function).
narrative_ontology:cs_axiom_status(enclosure_prevention_as_commons_function, holdable).
narrative_ontology:cs_axiom_grounding('ff94a0b5-17b7-41d3-b1a5-384c99085934', enclosure_prevention_as_commons_function, instrumental).
narrative_ontology:cs_reference_frame('ff94a0b5-17b7-41d3-b1a5-384c99085934', reciprocal_knowledge_preservation).
narrative_ontology:cs_drift_state('ff94a0b5-17b7-41d3-b1a5-384c99085934', contemporary_cloud_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff94a0b5-17b7-41d3-b1a5-384c99085934', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons_as_institution).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integration_firms).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.48) not high: the GPL does enforce a constraint on proprietary integration, but the extraction is structural (lose the option to enclose) rather than rent-seeking (no external party is collecting the transferred value; it flows back to the commons). Suppression is low-moderate (0.32) because firms have legitimate alternatives—they can use permissive licenses, dual-license their code, or pay for proprietary software. The reciprocal obligation is enforced through copyright law and community/legal action, which explains the non-zero suppression, but it is not violent or economically coercive beyond the choice structure itself. Theater is low (0.18) because the mechanism is functionally transparent: firms understand what GPL requires and plan accordingly. The theatrical element rises slightly over time (from 0.08 to 0.18) as the commons becomes so embedded in infrastructure that compliance theater ('we love open source') grows even where firms resist reciprocal obligations. Accessibility collapse is moderate-high (0.71) because once a firm understands GPL terms, the alternatives are largely closed off by technical necessity—GPL infrastructure is superior for most use cases. Resistance is moderate (0.54) because proprietary firms invest significantly in avoiding GPL (developing closed-source alternatives, lobbying for permissive licenses, negotiating exemptions), though they cannot eliminate the constraint entirely. The measurement series is one shared time grid across all metrics, authored at every examined time point from 0 to 35 time units. Extractiveness rises early then plateaus, suggesting the constraint reaches stable operation once firms understand and adapt to GPL terms. Theater rises as compliance rituals accumulate but never approaches high values, indicating functional enforcement is the dominant force.
 *
 * PERSPECTIVAL GAP:
 *   From the open-source developer's seat, GPL is a coordination benefit: they build on the commons, contribute freely, and keep out vendors. From the proprietary integrator's seat, GPL is a constraint: it prevents profitable business models (integration + enclosure). From the commons-as-institution's seat, GPL is a preservation mechanism: it maintains the shared resource against enclosure. These are not different understandings of the same constraint—they are genuinely different structural relationships, computed by the engine from power/exit/beneficiary data. A developer with high mobility, moderate power, and beneficiary role computes a different effective extraction than a powerful firm with trapped exit and victim role. This is not a bug in the classification system; it is the detection of real asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The software commons-as-institution is the beneficiary (d near 0.0 on the beneficiary end): the reciprocal obligation exists to preserve the commons from enclosure. Individual exit-maximizers and proprietary integration firms are victims (d near 1.0): they lose the option to integrate GPL code without returning modifications. However, the directionality is not binary. Open developers are beneficiaries with high exit options and moderate power—they benefit from the commons without paying the extraction cost; their d moves toward 0.1–0.2. Proprietary firms have high power but constrained exit relative to the commons—they depend on GPL infrastructure but cannot freely enclose it; their d is moderate (0.6–0.7). Cloud providers are trapped: they benefit from GPL infrastructure but face increasing suppression from AGPLv3; their d is high (0.75+). Dual-licensing operators sit at the pivot: they enforce GPL (agenda-setters) but also benefit from proprietary licensing; their d is symmetric to slightly asymmetric (0.4–0.5) depending on the revenue split between GPL maintenance and proprietary licensing.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mandatrophy by forcing a genuine coordination tradeoff: the GPL's founding problem (enclosure of commons by proprietary integration) remains live and contested. Proprietary firms continue attempting enclosure; legal cases and cloud-provider resistance to AGPLv3 document ongoing contestation. The constraint is enforced actively (litigation, community shunning) because the commons would not persist without enforcement—firms would enclose it as soon as suppression lapsed. This is the structural definition of Tangled Rope: genuine coordination function (preserve commons, coordinate development) + asymmetric extraction (lose proprietary integration option) + active enforcement. The alternative reading—that GPL is pure restriction (Snare)—misses the coordination benefit; the alternative reading that GPL is pure freedom preservation (Rope) misses the institutional beneficiary (the commons itself), which has interests distinct from any individual developer's interests in autonomy. This commons-reading framing prevents the false summit of treating GPL as natural law ('of course code wants to be free') by making explicit the institutional choice to treat the commons as a beneficiary worthy of preservation through forced reciprocity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_as_agent_or_institution,
    'Is the ''software commons as institution'' a genuine beneficiary (a coordinating entity that collects from the arrangement), or is it a non-agent placeholder for the aggregated benefit to open developers and users?',
    'Examine whether the commons—or formal stewards like the FSF—make autonomous decisions that vary from what individual open developers would choose. If the commons has distinct preferences (e.g., enforcing source-availability even when individual developers would accept proprietary integration for revenue), it is an agent; if it merely summarizes individual benefits, it is a non-agent.',
    'If the commons is a genuine agent-beneficiary, the constraint is Tangled Rope with agent asymmetry (organized commons vs. dispersed payers). If it is a non-agent, the constraint may be better classified as Rope (genuine coordination among open developers, no central extractor). The commons-reading framing depends on treating the commons as an institutional actor with distinct interests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commons_as_agent_or_institution, conceptual, 'Whether the software commons functions as an autonomous agent or as a summary of distributed preferences.').

omega_variable(
    enclosure_without_enforcement,
    'How much GPL enclosure would occur if enforcement mechanisms (litigation, community shunning, licensing denial) ceased but the copyleft license text remained legally binding?',
    'Thought experiment + corporate survey: ask proprietary firms whether they would integrate GPL without restrictions if enforcement stopped. Cross-check against historical GPL violations that went unenforced (firmware in routers, embedded systems) and see whether enclosure actually occurred at scale.',
    'If firms would massively violate GPL without active enforcement, suppression is understated and the constraint is snare-like (enforcement-dependent extraction). If firms would comply even without enforcement due to reputational or business-model concerns, suppression is accurate and the constraint is genuinely coordinative (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enclosure_without_enforcement, empirical, 'The role of active enforcement vs. structural incentive alignment in maintaining reciprocal obligation.').

omega_variable(
    reading_boundary_commons_vs_freedom,
    'In this reading, is the GPL primarily a commons-preservation institution (beneficiary = the shared resource itself, victim = enclosers), or is it primarily a freedom-preservation mechanism (beneficiary = users and developers as individuals, victim = proprietary restrictions)?',
    'Examine the GPL''s stated purpose and enforcement priorities. The FSF emphasizes ''software freedom''; Red Hat and Canonical emphasize shared development. The reading divergence lies in whether the commons is treated as an end in itself (commons-reading) or as a means to individual freedom (freedom-reading).',
    'In the commons-reading, the constraint benefits the commons as an institution and constrains individual exit-maximizers. In the freedom-reading, beneficiaries are individual users and developers, and the constraint protects their freedoms rather than the commons itself. The two readings may produce different victim sets and different compensation structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_commons_vs_freedom, conceptual, 'The primary beneficiary framing: commons-as-institution vs. individuals-as-freedoms.').

omega_variable(
    dual_licensing_capture_risk,
    'Does the dual-licensing model (GPL open source + proprietary commercial licenses) undermine the commons-preservation function by allowing copyright holders to selectively release proprietary exceptions?',
    'Track instances where copyright holders (e.g., MySQL AB selling proprietary licenses while maintaining GPL versions) allow enclosure through proprietary licensing while using GPL enforcement against competitors. Measure whether dual licensing evolves into a de facto proprietary licensing business with GPL as a loss leader.',
    'If dual licensing becomes routine enclosure-enabled-for-money, the constraint shifts from commons-preservation (Tangled Rope) toward regulatory capture (Snare) where the agenda-setter profits from both the commons and exceptions to it. The commons becomes a theater for extracting licensing rents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_licensing_capture_risk, empirical, 'Whether dual licensing preserves or undermines the commons-preservation function of GPL reciprocity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gpl__tr_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(gpl__tr_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(gpl__tr_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 25, 0.17).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(gpl__tr_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 35, 0.18).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gpl__be_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(gpl__be_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(gpl__be_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(gpl__be_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 35, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gpl__su_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 5, 0.27).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(gpl__su_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement(gpl__su_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 25, 0.32).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 30, 0.32).
narrative_ontology:measurement(gpl__su_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 35, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.18).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, open_source_funding_sustainability).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_software_market_structure).

% DUAL FORMULATION NOTE:
% The GPL kernel_id (gpl_reciprocity_obligation) has three constraint readings: copyleft_as_commons_reading (this file, institutional commons preservation), copyleft_as_freedom_reading (user autonomy protection), and copyleft_as_restriction_reading (business-model constraint). Each reading applies to the same legal mechanism but produces different beneficiary structures, victim sets, and type classifications. The three stories are linked as a constraint family via network.affects_constraints; each story must cross-reference the siblings and explain in commentary.kernel_context why the readings diverge. Do not merge readings into one constraint; the ε-invariance principle requires that different beneficiary/victim structures and different type claims yield separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, organized, 0.05).
constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
