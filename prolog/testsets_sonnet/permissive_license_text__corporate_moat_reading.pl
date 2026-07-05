% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Text (Corporate Moat Reading)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the permissive_license_text
 *   kernel: the corporate moat reading, under which the absence of a
 *   reciprocity requirement in permissive licenses (MIT, BSD, Apache-2.0)
 *   structurally enables well-capitalized corporations to build proprietary
 *   derivative products on volunteer-maintained code without any obligation
 *   to compensate, contribute, or even sustain the upstream project. The same
 *   license text supports two other readings, generated as separate
 *   constraint stories: the commons_coordination_reading (which foregrounds
 *   the genuine friction-reduction and universal-implementation benefits of
 *   the same text) and the copyleft_counterfactual_reading (which argues the
 *   absence of reciprocity is the specific defect that a viral-copyleft
 *   alternative like the GPL would correct). This story does not average
 *   across those readings or hedge its epsilon between them — it asserts a
 *   single, moderate-epsilon extraction profile specific to the
 *   corporate-benefit structural claim.
 *
 * KEY AGENTS:
 *   - enterprise_cloud_vendors: primary beneficiary (institutional/arbitrage) — captures uncompensated derivative value at global scope
 *   - individual_maintainers: primary target (powerless/trapped) — bears unpaid maintenance burden with no legal claim on downstream commercial value
 *   - volunteer_led_projects: secondary target (powerless/constrained) — collective version of the same extraction dynamic
 *   - license_drafting_bodies: agenda setter (institutional/analytical) — could add reciprocity terms but generally does not
 *   - open_source_foundations: analytical observer / partially excluded — sees the full structure but lacks enforcement leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.58).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.42).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Text (Corporate Moat Reading)").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "software_licensing/intellectual_property/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, '97812fea-d3a7-4be3-9bf3-dd54f325cda5').
narrative_ontology:cs_kernel_codification('97812fea-d3a7-4be3-9bf3-dd54f325cda5', fixed_text).
narrative_ontology:cs_authority_grounding('97812fea-d3a7-4be3-9bf3-dd54f325cda5', extraction).
narrative_ontology:cs_interpretation_layer_present('97812fea-d3a7-4be3-9bf3-dd54f325cda5').
narrative_ontology:cs_reading_relation('97812fea-d3a7-4be3-9bf3-dd54f325cda5', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('97812fea-d3a7-4be3-9bf3-dd54f325cda5', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('97812fea-d3a7-4be3-9bf3-dd54f325cda5', foundational, uncompensated_derivative_capture_is_the_defining_harm).
narrative_ontology:cs_axiom_status(uncompensated_derivative_capture_is_the_defining_harm, holdable).
narrative_ontology:cs_axiom_grounding('97812fea-d3a7-4be3-9bf3-dd54f325cda5', uncompensated_derivative_capture_is_the_defining_harm, empirically_contingent).
narrative_ontology:cs_axiom('97812fea-d3a7-4be3-9bf3-dd54f325cda5', secondary, license_neutrality_is_a_cover_story_not_a_fact).
narrative_ontology:cs_axiom_status(license_neutrality_is_a_cover_story_not_a_fact, holdable).
narrative_ontology:cs_axiom_grounding('97812fea-d3a7-4be3-9bf3-dd54f325cda5', license_neutrality_is_a_cover_story_not_a_fact, conventional).
narrative_ontology:cs_reference_frame('97812fea-d3a7-4be3-9bf3-dd54f325cda5', friction_minimization_founding_intent).
narrative_ontology:cs_drift_state('97812fea-d3a7-4be3-9bf3-dd54f325cda5', platform_scale_cloud_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('97812fea-d3a7-4be3-9bf3-dd54f325cda5', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_cloud_vendors).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, proprietary_saas_incumbents).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, volunteer_led_projects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, downstream_application_developers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, downstream_application_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Take permissively licensed code (MIT/BSD/Apache-2.0), wrap it in proprietary orchestration, support contracts, and managed hosting, and sell it back to the market at scale. Owe the original maintainers nothing beyond attribution. Because the license imposes no reciprocity requirement, they can fork, close, and monetize without ever contributing code, funding, or maintenance capacity back to the upstream project. Their exit option is total: they can walk away from any upstream dependency and re-host or re-implement without legal jeopardy.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_cloud_vendors, beneficiary,
    institutional, civilizational, arbitrage, global).

% Build core product features on top of permissively licensed libraries and frameworks, capturing the R&D savings as margin while offering no equivalent artifact back to the ecosystem. Use the absence of a reciprocity clause as a legal shield against demands for contribution.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, proprietary_saas_incumbents, beneficiary,
    institutional, generational, arbitrage, global).

% Wrote and maintain the underlying software, often unpaid or under-compensated, and watch companies with orders of magnitude more resources monetize derivative products built on their work. Relicensing retroactively is usually legally impossible once adoption has occurred (dependents would break, and many maintainers lack the collective bargaining position to demand terms). Burnout, unpaid support burden, and the psychological toll of maintaining critical infrastructure without recognition or income are structural features of the position, not incidental.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_maintainers, payer,
    powerless, biographical, trapped, global).

% Governed by loose, under-resourced foundations or informal maintainer groups. Depend on goodwill contributions and cannot compel the corporations extracting value from their code to fund maintenance, triage security issues, or staff support. Some have attempted relicensing to source-available or business-source terms after high-profile extraction incidents, at the cost of community trust and contributor goodwill.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, volunteer_led_projects, payer,
    powerless, biographical, constrained, global).

% Benefit from frictionless reuse of permissively licensed components in their own commercial and non-commercial products, lowering their own development costs. Some are small operators who could not survive if reciprocity requirements forced them to open-source their own proprietary work; others are simply passing along the same extraction pattern one level down.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, downstream_application_developers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__corporate_moat_reading, downstream_application_developers, payer).

% Attempt to broker sustainability solutions (corporate sponsorship tiers, dual-licensing advocacy, maintainer stipends) but have no enforcement mechanism to compel contribution back from downstream commercial users under a permissive license. Their voice in license-drafting decisions is frequently absent from the corporate legal teams that draft the license text maintainers are asked to adopt.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, open_source_foundations, observer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__corporate_moat_reading, open_source_foundations, excluded).

% Legal foundations and corporate open-source offices draft and promote permissive license templates, framing them as maximizing adoption and innovation. They administer the license text itself and could add reciprocity requirements but generally do not, since the entities most influential in drafting are frequently the same enterprises that benefit from the absence of such requirements.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, license_drafting_bodies, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, enterprise_cloud_vendors).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing genuinely lowers legal friction for anyone wanting to reuse, modify, or embed the code, which does solve a real coordination problem: without it, every integration would require bespoke negotiation.
% TRANSFER_FUNCTION: Moves unpaid engineering labor, maintenance burden, and design value from individual maintainers and volunteer-led projects to well-resourced corporations, who convert that labor into proprietary commercial products without any contractual or legal obligation to compensate or contribute back.
% ABSENT_VOICES: Individual maintainers rarely have a seat at the table when license terms are chosen for a project they will spend years maintaining — the license is frequently selected at project founding, before maintenance burden or corporate extraction patterns are visible, and by the time the pattern is visible, relicensing existing contributors' code requires unanimous consent that is often practically unobtainable.
% DISAPPEARANCE_RATIONALE: Corporate beneficiaries would say the world rearranges catastrophically — adoption would slow, legal friction would spike, and innovation would suffer. Maintainer advocates would say the world barely changes for coordination purposes (copyleft and source-available alternatives coordinate reuse too) but changes enormously for compensation: extraction that currently flows uncompensated would either stop or require negotiated terms. The dispute is precisely about which effect is primary.
% FOUNDING_PROBLEM: Early software licensing required negotiated agreements for every reuse, which made lightweight collaboration and broad adoption of shared code prohibitively slow and legally expensive; permissive licenses were built to remove that friction entirely.
% FOUNDING_PROBLEM_CORROBORATION: Corporate legal teams and standards bodies attest the friction-removal problem remains live and central. Independent researchers studying open-source sustainability (e.g., surveys of maintainer burnout, Tidelift and Open Source Collective sustainability reports) and several high-profile maintainers who have publicly relicensed projects after extraction incidents attest, from outside the corporate beneficiary set, that the original friction-removal problem is largely solved while a distinct, unaddressed compensation problem has emerged and calcified in its place.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, contested).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is authored as moderate rather than severe because the coordination function is genuinely real — permissive licenses do lower friction and drive broad adoption, which is not itself the extraction. The extraction is specifically the asymmetric capture of derivative commercial value without any compensating flow back to origin. Suppression (0.42) is moderate: there is no active coercion forcing maintainers to use permissive terms, but switching costs, ecosystem lock-in, and the practical impossibility of retroactive relicensing after wide adoption function as a structural trap once the license choice is made early in a project's life. Theater ratio is low-moderate and rising (0.10 to 0.28) as corporate sponsorship programs and 'open source stewardship' initiatives increasingly perform commitment to sustainability without materially altering the underlying extraction pattern. Accessibility collapse (0.35) is moderate — alternatives exist (copyleft, source-available, dual licensing) but adopting them after the fact requires unanimous contributor consent that is frequently unobtainable, which is why accessibility does not collapse further toward 1.0: the option is visible, just often too late to exercise.
 *
 * PERSPECTIVAL GAP:
 *   From the corporate agenda-setter and beneficiary seats, this is unambiguously rope: minimal friction, maximal adoption, no coercion visible from where they stand because they never experience the maintenance burden side of the ledger. From the individual maintainer seat, the identical structure computes as extraction with a a real, if partial, coordination function riding on top of it — hence the claimed_type of snare here reflects the authoring seat's judgment that the extraction, not the coordination, is now the dominant and defining feature of the arrangement, even though a genuine coordination function is acknowledged to exist.
 *
 * DIRECTIONALITY LOGIC:
 *   Enterprise cloud vendors and proprietary SaaS incumbents sit near the full-beneficiary end: they have arbitrage-grade exit (can substitute libraries, fork, or re-implement at will), institutional power, and directly capture the commercial value of derivative work. Individual maintainers sit near the full-target end: powerless, trapped (relicensing existing contributions requires consent they usually cannot obtain), and bear the maintenance costs the corporate beneficiaries impose without compensating. Downstream application developers are directionally mixed — they benefit from the same friction reduction while some also become payers if their own reuse patterns get further exploited a level down.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (excessive legal friction blocking reuse) is largely solved industry-wide; permissive licensing succeeded at its original goal decades ago. What persists is a distinct, unaddressed problem — uncompensated extraction — that the license text was never designed to solve and that has calcified into the status quo because the parties best positioned to fix it (license drafting bodies, large corporate adopters) are also its principal beneficiaries. Classifying this as tangled_rope would risk crediting an active enforcement/coordination trade that model this constraint does not actually run on (there is no enforcement machinery at all, just an absence of an obligation); classifying it as pure mountain (natural fact) would erase the identifiable, addressable beneficiary/victim asymmetry. Snare captures that a genuine coordination story exists but is being used as cover for a persistent, addressable extraction that has no exit for the party bearing its cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_absence_as_design_choice_or_oversight,
    'Is the absence of a reciprocity requirement in permissive licenses a deliberate design choice reflecting a coherent normative commitment to unrestricted reuse, or an oversight/externality that license drafters failed to price when corporate-scale extraction was not yet a visible pattern?',
    'Archival review of license-drafting deliberations (e.g., OSI mailing lists, BSD/MIT license history) to determine whether reciprocity was considered and rejected, versus never seriously considered given the scale of the software industry at drafting time.',
    'If deliberate and well-reasoned, the corporate_moat_reading''s extraction framing is weaker (the parties consented to the terms with full information). If an unpriced externality, the extraction framing strengthens: the beneficiaries are capturing value the original design never intended to cede.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_absence_as_design_choice_or_oversight, conceptual, 'Whether permissive terms reflect intentional policy or an unpriced historical externality.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the three sibling readings (commons_coordination, copyleft_counterfactual, corporate_moat) disagree — is it about the facts of who captures value, or purely about the normative weight assigned to that capture?',
    'Compare stakeholder situations across all three sibling constraint stories: if beneficiary/victim declarations converge but claimed_type diverges, the disagreement is normative (framing); if the declared parties themselves differ, the disagreement is factual.',
    'A purely normative disagreement means all three readings can coexist indefinitely as live positions (as authored in cs_structure.reading_relations). A factual disagreement about who actually benefits would mean at least one reading has an empirical error that further evidence could correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether the kernel''s contested readings differ in fact or in evaluation.').

omega_variable(
    maintainer_collective_action_capacity,
    'Could individual maintainers, acting collectively (via foundations, license migration campaigns, or coordinated relicensing), meaningfully shift the extraction pattern, or is the powerlessness structural regardless of coordination?',
    'Track outcomes of documented relicensing campaigns (e.g., MongoDB''s SSPL move, Elastic''s license change, HashiCorp''s BSL adoption) for effects on corporate extraction, community trust, and project sustainability.',
    'If collective relicensing meaningfully changes outcomes, the powerless/trapped exit_options declared for individual_maintainers should be revisited toward constrained rather than trapped in future iterations, and the snare classification may weaken toward tangled_rope (genuine, if costly, exit exists).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maintainer_collective_action_capacity, empirical, 'Whether maintainer collective action can alter the extraction pattern this story treats as structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__corporate_moat_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__corporate_moat_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__corporate_moat_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__corporate_moat_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__corporate_moat_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__corporate_moat_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__corporate_moat_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__corporate_moat_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__corporate_moat_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__corporate_moat_reading, base_extractiveness, 25, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(permissive_license_text__corporate_moat_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__corporate_moat_reading, 0.12).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposed from the single natural-language label 'permissive open source licensing' per the ε-invariance principle. Each sibling reading (commons_coordination_reading: rope; copyleft_counterfactual_reading: analysis of the GPL alternative; corporate_moat_reading: this story, snare) shares the same underlying license text but produces a different ε and a different classification because each reading foregrounds a different structural claim about the same text's effect. They are linked bidirectionally via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__corporate_moat_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
