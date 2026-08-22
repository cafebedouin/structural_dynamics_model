% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Commercial-Use Derivative Work Licensing Carveout
 *   domain: intellectual_property/technology_governance/information_economics
 *
 * SUMMARY:
 *   This story instantiates the hybrid_carveout_reading of the
 *   derivative-work-statutory-boundary kernel: the boundary between permitted
 *   transformative use and infringing derivative preparation is drawn not by
 *   the fixity or substantiality of incorporation (the coordination_reading)
 *   nor by a blanket authorization requirement for any use (the
 *   enclosure_reading), but by whether the resulting use is commercial.
 *   Noncommercial transformative use proceeds unencumbered; commercial
 *   exploitation requires licensing. This produces a genuine coordination
 *   function (protecting noncommercial creative culture from licensing
 *   friction) fused with an asymmetric extraction mechanism (commercial
 *   developers, especially small ones, pay licensing costs and bear
 *   boundary-ambiguity risk that well-resourced incumbents can absorb but
 *   marginal creators cannot).
 *
 * KEY AGENTS:
 *   - rightsholder_licensing_offices: administers the commercial/noncommercial line and collects licensing fees
 *   - noncommercial_transformative_users: exempt beneficiaries of the carveout
 *   - commercial_derivative_developers: bear licensing costs and boundary-ambiguity risk
 *   - small_scale_commercial_creators: powerless payers with no negotiating leverage
 *   - established_platform_incumbents: powerful actors who absorb licensing costs as routine overhead and benefit from the barrier it creates for smaller commercial rivals
 *   - courts_and_regulators: analytical observers who adjudicate contested characterizations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.52).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.48).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Commercial-Use Derivative Work Licensing Carveout").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '426cddd5-8b6f-4205-8fad-0e4834e00a7c').
narrative_ontology:cs_kernel_codification('426cddd5-8b6f-4205-8fad-0e4834e00a7c', formalized).
narrative_ontology:cs_authority_grounding('426cddd5-8b6f-4205-8fad-0e4834e00a7c', lineage).
narrative_ontology:cs_interpretation_layer_present('426cddd5-8b6f-4205-8fad-0e4834e00a7c').
narrative_ontology:cs_reading_relation('426cddd5-8b6f-4205-8fad-0e4834e00a7c', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('426cddd5-8b6f-4205-8fad-0e4834e00a7c', derivative_work_statutory_boundary__enclosure_reading, influences).
narrative_ontology:cs_axiom('426cddd5-8b6f-4205-8fad-0e4834e00a7c', foundational, commercial_exploitation_is_the_relevant_trigger).
narrative_ontology:cs_axiom_status(commercial_exploitation_is_the_relevant_trigger, holdable).
narrative_ontology:cs_axiom_grounding('426cddd5-8b6f-4205-8fad-0e4834e00a7c', commercial_exploitation_is_the_relevant_trigger, conventional).
narrative_ontology:cs_axiom('426cddd5-8b6f-4205-8fad-0e4834e00a7c', secondary, noncommercial_transformation_categorically_exempt).
narrative_ontology:cs_axiom_status(noncommercial_transformation_categorically_exempt, holdable).
narrative_ontology:cs_axiom_grounding('426cddd5-8b6f-4205-8fad-0e4834e00a7c', noncommercial_transformation_categorically_exempt, instrumental).
narrative_ontology:cs_reference_frame('426cddd5-8b6f-4205-8fad-0e4834e00a7c', statutory_derivative_work_definition_with_fair_use_backstop).
narrative_ontology:cs_drift_state('426cddd5-8b6f-4205-8fad-0e4834e00a7c', platform_economy_commercialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('426cddd5-8b6f-4205-8fad-0e4834e00a7c', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_offices).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, noncommercial_transformative_users).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, established_platform_incumbents).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, small_scale_commercial_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the boundary by setting licensing terms, negotiating fees with any party whose derivative use is characterized as commercial, and pursuing enforcement against unauthorized commercial exploitation. Collect licensing revenue directly and control the practical line-drawing between exempt transformative use and exploitation requiring authorization.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_offices, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_offices, beneficiary).

% Create fan works, remixes, commentary, and parody without seeking authorization, so long as no commercial exploitation is involved. Benefit from a genuine carveout that lets transformative culture happen without licensing friction; their exit option is simply staying within the noncommercial line, which is usually achievable.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, noncommercial_transformative_users, beneficiary,
    moderate, biographical, mobile, national).

% Build games, software tools, merchandise, or media that transform existing copyrighted works but intend to monetize the result. Must seek authorization and pay licensing fees or royalties, or forgo commercialization entirely, or attempt to launch and risk litigation. The commercial/noncommercial line often turns on ambiguous facts (ad-supported vs. paid, incidental revenue vs. business model), so many face costly legal uncertainty even when they believe they qualify for the carveout.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_developers, payer,
    moderate, biographical, constrained, national).

% Individual creators or micro-studios who cross into modest commercial activity (a Patreon-supported webcomic, a small paid app) lack the legal resources to negotiate licenses or contest boundary determinations. They either abandon monetization, operate in a legally exposed gray zone, or accept licensing terms set unilaterally by rightsholders with no bargaining power of their own.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, small_scale_commercial_creators, payer,
    powerless, biographical, trapped, national).

% Large studios and platforms with existing licensing relationships and legal departments navigate the commercial/noncommercial line efficiently, absorb licensing costs as a normal cost of business, and benefit from a boundary that disadvantages smaller commercial competitors who cannot afford the same compliance or negotiation capacity.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, established_platform_incumbents, beneficiary,
    powerful, generational, arbitrage, global).

% Adjudicate disputed cases where the commercial/noncommercial characterization is contested, and could in principle clarify or shift the boundary through rulemaking or case law, but in practice apply the existing hybrid test case-by-case rather than resolving its structural ambiguity.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_offices).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes low-stakes transformative culture (which proceeds unencumbered) from monetized exploitation of another's expression (which triggers a licensing negotiation), allowing rightsholders to permit broad noncommercial creative activity while still capturing value from commercial derivative markets they consider theirs.
% TRANSFER_FUNCTION: Moves licensing fees and negotiation leverage from anyone attempting to commercialize a transformative work toward the rightsholder controlling the underlying work, while imposing legal-uncertainty costs disproportionately on parties without the resources to define or defend the commercial/noncommercial line for themselves.
% ABSENT_VOICES: Small-scale commercial creators attempting to cross from hobbyist to modestly monetized activity are rarely represented in the licensing negotiations or litigation that define where the line actually falls; the boundary is set through disputes between well-resourced rightsholders and well-resourced commercial defendants, leaving the ambiguous middle to guess at their exposure.
% DISAPPEARANCE_RATIONALE: If the commercial/noncommercial carveout vanished and reverted to a uniform rule (either the enclosure reading's blanket authorization requirement or the coordination reading's narrow substantial-incorporation test), licensing revenue flows would collapse or expand dramatically, noncommercial creative communities would either lose their exemption or gain it as a matter of universal right, and commercial developers would face either uniform liability or uniform freedom rather than a negotiated, contested middle.
% FOUNDING_PROBLEM: Copyright holders wanted to permit the broad cultural practice of noncommercial fan creativity and commentary (which generates goodwill and rarely displaces market value) without surrendering their ability to capture revenue when someone else profits from exploiting their protected expression.
% FOUNDING_PROBLEM_CORROBORATION: Rightsholder licensing offices attest the distinction remains necessary to protect commercial derivative markets. Independent legal scholarship and testimony from small commercial creators in policy hearings attest that the line has become primarily a revenue-extraction and litigation-leverage mechanism against under-resourced commercializers, since large incumbents absorb the licensing cost as routine business while the ambiguous boundary chiefly burdens parties who cannot afford to define their own exposure.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) rather than high because a genuine noncommercial exemption exists and functions as designed for a large population of users — this is not blanket enclosure. But it is well above zero because the commercial trigger creates a real, actively enforced transfer from commercial developers to rightsholders, and the line's ambiguity (ad-supported vs. paid, incidental vs. primary revenue) generates disproportionate cost for parties without legal resources to define or defend their position. Suppression (0.48) reflects active enforcement against unauthorized commercial exploitation plus the chilling effect of boundary ambiguity itself, which discourages some non-infringing commercialization out of risk-aversion. Theater ratio remains low-moderate (0.22): the enforcement is substantively functional, not primarily performative, though litigation over boundary characterization has grown as commercial derivative markets (mods, fan-funded content, AI-training-adjacent works) have expanded.
 *
 * PERSPECTIVAL GAP:
 *   From the rightsholder licensing office's seat, this is a well-functioning tangled rope: it coordinates broad permission for low-stakes creativity while preserving commercial value capture. From the small-scale commercial creator's seat, the same boundary computes closer to a snare — an ambiguous, actively enforced line they cannot afford to litigate or negotiate, where crossing into any monetization exposes them to licensing terms set unilaterally by a more powerful counterparty. The engine's per-seat computation should surface this divergence directly from the differing power/exit declarations rather than from any single authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Rightsholder licensing offices are structural beneficiaries with arbitrage-grade exit (they can choose whom to license and on what terms) — low d. Noncommercial users are beneficiaries of the carveout itself — low d, since the constraint subsidizes their activity by exempting it. Commercial derivative developers and especially small-scale commercial creators are targets: the constraint extracts licensing payments and imposes uncertainty costs, and their exit options (constrained, trapped) push d toward the target end. Established platform incumbents occupy an unusual beneficiary position despite technically being 'commercial' actors: their scale and existing licensing relationships mean the boundary functions as a barrier to entry protecting them from smaller commercial competitors, which is why they are declared a beneficiary rather than a victim despite facing the same nominal rule.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (permitting noncommercial creative culture while preserving commercial value capture) remains partially live — the coordination function for noncommercial users still operates as designed. But the classification as tangled_rope rather than pure rope or pure snare is essential: treating this purely as coordination would erase the real, asymmetric extraction commercial developers face; treating it purely as extraction would erase the genuine and functioning carveout that noncommercial users rely on. The tangled_rope type is the only classification that holds both the real coordination function and the real asymmetric extraction as simultaneously true, which is structurally accurate to how this specific reading of the boundary operates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_noncommercial_line_stability,
    'Is the commercial/noncommercial distinction a stable, administrable line, or does its inherent ambiguity (ad-supported content, incidental revenue, crowdfunded creative work) function as a structural extraction mechanism regardless of anyone''s intent to abuse it?',
    'Empirical survey of litigation and licensing-negotiation outcomes: if outcomes cluster predictably around clear commercial/noncommercial fact patterns, the line is administrable; if outcomes are highly variable for similarly-situated parties, the ambiguity itself is doing extractive work.',
    'If the line is inherently unstable, effective extraction is higher than the categorical carveout suggests, because uncertainty cost falls on all commercializing parties regardless of ultimate legal outcome — this would push the classification toward snare for the affected population even though the doctrine''s stated design is tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_noncommercial_line_stability, empirical, 'Whether the commercial/noncommercial line is administrable or inherently extractive through ambiguity.').

omega_variable(
    sibling_reading_selection_pressure,
    'This story is one of three readings of the same kernel (coordination_reading, enclosure_reading, hybrid_carveout_reading). Which reading actually governs in a given jurisdiction or dispute is itself contested and can shift with litigation and legislative amendment — what determines which reading is operative at a given moment?',
    'Track case law and statutory amendment trends: a shift toward the enclosure_reading would appear as expanding definitions of ''derivative work'' capturing more transformative activity; a shift toward coordination_reading would appear as courts narrowing the derivative work definition to require substantial fixed incorporation regardless of commercial status.',
    'If the doctrine is drifting toward the enclosure_reading, this hybrid_carveout_reading''s moderate ε would understate the trajectory of actual extraction; if drifting toward coordination_reading, current commercial licensing practice would become increasingly vulnerable to legal challenge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_selection_pressure, conceptual, 'Which kernel reading is operative is itself unstable and contested across jurisdictions and time.').

omega_variable(
    incumbent_beneficiary_classification,
    'Should established platform incumbents, who are nominally ''commercial'' actors subject to the same licensing requirement as small creators, be classified as beneficiaries (because the boundary functions as a barrier protecting their market position) or as payers (because they do pay licensing fees)?',
    'Compare relative cost burden: if licensing costs are a rounding error for incumbents'' revenue but existential for small creators, and if incumbents'' market share benefits from smaller competitors'' exclusion, the net effect is beneficiary despite formal payer status.',
    'Reclassifying incumbents as payers rather than beneficiaries would understate the asymmetric-extraction gate''s severity, since the tangled_rope''s defining asymmetry is precisely that similarly-labeled ''commercial'' actors experience radically different effective costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_beneficiary_classification, conceptual, 'Whether nominal commercial payer status or actual competitive benefit should govern incumbent classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(deri_tr_t4, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(deri_tr_t8, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(deri_tr_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(deri_tr_t16, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(deri_be_t4, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(deri_be_t8, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(deri_be_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(deri_be_t16, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(deri_su_t4, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 4, 0.37).
narrative_ontology:measurement(deri_su_t8, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(deri_su_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(deri_su_t16, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.12).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_carveout_reading member of a three-story kernel decomposition of the derivative_work_statutory_boundary contest. The coordination_reading (narrow, substantial-incorporation trigger; low ε, closer to rope) and the enclosure_reading (any use triggers authorization requirement; high ε, closer to snare) are separate constraint files sharing this kernel_id. Each reading has its own stable ε and its own classification; they are linked here for contamination-propagation analysis, not merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
