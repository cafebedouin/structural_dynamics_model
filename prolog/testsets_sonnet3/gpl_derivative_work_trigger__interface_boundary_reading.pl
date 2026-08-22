% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__interface_boundary_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: Interface-Boundary Reading of the GPL Derivative-Work Trigger
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This story instantiates the interface-boundary reading of the GPL
 *   derivative-work trigger: a clean, documented API separating a GPL
 *   component from a proprietary component defeats the derivative-work
 *   classification even when the two are tightly coupled at runtime and
 *   marketed as a single product. This reading enables a scaffold-shaped
 *   arrangement — modular architecture with mixed licensing — that lets
 *   ecosystem integrators build commercial value atop GPL infrastructure. The
 *   reading is deliberately generated as its own constraint, not as a hedge
 *   across the kernel: its ε reflects the interface-boundary arrangement's
 *   own operation, not the broad-copyleft alternative it displaces.
 *
 * KEY AGENTS:
 *   - ecosystem_integrators: primary beneficiary (organized/arbitrage) — captures value from modular composition
 *   - proprietary_plugin_vendors: secondary beneficiary (moderate/mobile) — business model depends on this reading holding
 *   - commercial_platform_operators: agenda setter (institutional/arbitrage) — designs and defends the boundary
 *   - downstream_users_expecting_full_source: primary payer (powerless/trapped) — denied source they reasonably expected
 *   - gpl_original_authors_seeking_reciprocity: secondary payer (moderate/constrained) — reciprocity norm defeated by engineered seam
 *   - software_freedom_advocacy_groups: excluded voice (organized/analytical) — argues the case for absent users
 *   - courts_and_licensing_arbiters: analytical observer (institutional/analytical) — the reading's actual boundary is only as stable as unlitigated case law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.42).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.35).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, scaffold).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "Interface-Boundary Reading of the GPL Derivative-Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:has_sunset_clause(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, '00061a36-5048-47e0-abb1-98dd051d0133').
narrative_ontology:cs_kernel_codification('00061a36-5048-47e0-abb1-98dd051d0133', fixed_text).
narrative_ontology:cs_authority_grounding('00061a36-5048-47e0-abb1-98dd051d0133', distributed).
narrative_ontology:cs_reading_relation('00061a36-5048-47e0-abb1-98dd051d0133', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('00061a36-5048-47e0-abb1-98dd051d0133', gpl_derivative_work_trigger__narrow_linking_permissive_reading, influences).
narrative_ontology:cs_axiom('00061a36-5048-47e0-abb1-98dd051d0133', foundational, interface_stability_defeats_derivation).
narrative_ontology:cs_axiom_status(interface_stability_defeats_derivation, holdable).
narrative_ontology:cs_axiom_grounding('00061a36-5048-47e0-abb1-98dd051d0133', interface_stability_defeats_derivation, conventional).
narrative_ontology:cs_axiom('00061a36-5048-47e0-abb1-98dd051d0133', secondary, functional_coupling_irrelevant_absent_source_sharing).
narrative_ontology:cs_axiom_status(functional_coupling_irrelevant_absent_source_sharing, holdable).
narrative_ontology:cs_axiom_grounding('00061a36-5048-47e0-abb1-98dd051d0133', functional_coupling_irrelevant_absent_source_sharing, instrumental).
narrative_ontology:cs_reference_frame('00061a36-5048-47e0-abb1-98dd051d0133', gpl_v2_linking_ambiguity_era).
narrative_ontology:cs_drift_state('00061a36-5048-47e0-abb1-98dd051d0133', post_saas_platform_commercialization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('00061a36-5048-47e0-abb1-98dd051d0133', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_plugin_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, commercial_platform_operators).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, downstream_users_expecting_full_source).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, gpl_original_authors_seeking_reciprocity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build commercial products that call into GPL-licensed components through documented, stable APIs. Under this reading, they can ship proprietary code alongside the GPL component without disclosing their own source, provided the boundary is a genuine interface rather than a thin wrapper around shared internals. This lets them adopt GPL infrastructure while keeping their differentiating code closed.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators, beneficiary,
    organized, generational, arbitrage, global).

% Sell plugins or extensions that interoperate with GPL host applications strictly through published extension points. They rely on the interface-boundary reading to justify keeping plugin source proprietary; if the boundary reading loses ground to the broad reading, their entire business model requires relicensing or exit.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_plugin_vendors, beneficiary,
    moderate, biographical, mobile, global).

% Design and maintain the API boundary itself, deciding where the interface line is drawn, how stable it is, and how tightly internal data structures leak across it. They administer the architecture that makes the interface-boundary reading credible or pretextual, and can redraw boundaries in response to litigation risk.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, commercial_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive a product built partly on GPL code but cannot obtain source for the proprietary components bundled across the API boundary, even though the two halves are tightly coupled in practice and marketed as a single product. Their reasonable expectation of the four freedoms is not met, but they have no standing to compel disclosure under this reading and no practical way to fork the closed half.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, downstream_users_expecting_full_source, payer,
    powerless, biographical, trapped, global).

% Wrote and license code under GPL specifically to ensure that derivative works remain free, expecting the copyleft to propagate through tight functional integration. Under this reading, vendors route around that expectation by engineering an API seam, capturing the benefit of the author's contribution while returning nothing to the commons. Authors can relicense future versions or litigate, but cannot retroactively compel disclosure of shipped proprietary integrations.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_original_authors_seeking_reciprocity, payer,
    moderate, civilizational, constrained, global).

% Argue publicly that tight functional coupling should trigger derivative-work status regardless of API cleanliness, since users experience a single integrated program. They file amicus briefs and publish position papers but hold no direct enforcement power over any specific license and are not party to the commercial arrangements they critique.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, software_freedom_advocacy_groups, excluded,
    organized, civilizational, analytical, global).

% Adjudicate disputes over whether a given API boundary is genuine (a stable, documented, arm's-length interface) or pretextual (a thin technical seam engineered to evade copyleft). Their rulings determine which projects can safely rely on this reading and which are exposed to reclassification under the broad reading.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, courts_and_licensing_arbiters, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__interface_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a modular software ecosystem to form around a GPL component: parties on either side of a clean, documented interface can independently license, version, and monetize their half without needing to negotiate license compatibility for every integration, enabling composition of free and proprietary code in a single running system.
% TRANSFER_FUNCTION: Moves the benefit of the GPL author's contributed engineering work to commercial integrators and plugin vendors who build proprietary value on top of it through the interface, while the reciprocal obligation the GPL was designed to enforce (source disclosure of the whole combined work) is not transferred back to the commons or to end users of the combined product.
% ABSENT_VOICES: End users who bought or installed the combined product experience one program and would object that they cannot inspect or modify the whole thing; software freedom advocacy groups make this argument on their behalf but have no enforcement standing. Neither group is party to the vendor's licensing decision or the API design that determines which reading applies.
% DISAPPEARANCE_RATIONALE: If the interface-boundary reading disappeared and the broad copyleft reading became uncontested law, every commercial product architected around a GPL-component-plus-proprietary-API pattern would face an immediate compliance crisis: vendors would need to relicense, rearchitect around a non-copyleft dependency, or open their proprietary layer. Entire categories of GPL-adjacent commercial tooling would need to restructure or exit the GPL ecosystem.
% FOUNDING_PROBLEM: Early copyleft litigation and FSF guidance left genuine ambiguity about whether functional coupling through an API, as opposed to source-level derivation, was sufficient to trigger the GPL's derivative-work clause — developers needed a workable line to build modular systems without perpetual legal uncertainty.
% FOUNDING_PROBLEM_CORROBORATION: Commercial platform operators and their counsel attest the interface-boundary line is a necessary and legally sound engineering practice. Independent legal scholars (e.g., academic copyright commentary on the LGPL's origin as a compromise instrument) and software freedom advocacy groups from outside the beneficiary set attest that the boundary is frequently engineered specifically to evade copyleft rather than reflecting a genuine architectural seam, and that courts have never definitively resolved the general question.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).
:- end_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) and rising, reflecting a real but bounded transfer: value flows from the GPL commons to commercial integrators, but the interface-boundary reading also performs a genuine coordination function (enabling modular composition that a strict broad reading would foreclose entirely). Suppression is moderate (0.35) because no party is coerced into using this architecture — vendors choose the interface pattern deliberately, and the GPL author chose the license knowing the ambiguity existed. Theater ratio is low-moderate and rising (0.12 to 0.28) reflecting a documented pattern: some 'interfaces' are engineered specifically as legal seams rather than as genuine architectural boundaries, and this theatrical fraction of interface design grows as the reading becomes commercially load-bearing. Accessibility collapse is moderate (0.4): alternative licensing arrangements and rearchitecture remain available to vendors, distinguishing this from a mountain-like inevitability.
 *
 * PERSPECTIVAL GAP:
 *   From the commercial-platform-operator seat, this looks like a scaffold: temporary, legitimate architectural accommodation permitting modular innovation, defensible while litigation remains unsettled. From the downstream-user and original-author seats, the same structure reads as an engineered evasion — a tangled-rope-flavored extraction wearing coordination's clothing. The engine computes these divergently from the shared structural data; this story authors the interface-boundary reading's own metrics honestly rather than collapsing the two perspectives into one score.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecosystem integrators and plugin vendors sit near the beneficiary end: the reading directly subsidizes their ability to monetize proprietary code atop GPL infrastructure with mobile-to-arbitrage exit (they can rearchitect or relicense if the reading loses ground). Downstream users sit at the target end: trapped exit, no standing to compel disclosure, bearing the cost of an expectation the GPL was written to guarantee. GPL original authors are victims in a different register — moderate power, constrained exit — their reciprocity expectation is defeated by design choices made entirely by third parties they cannot control after release.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold framing is only honest if the boundary-drawing exercise has a genuine sunset condition — i.e., courts eventually settle whether a given seam is real or pretextual, closing the ambiguity. If litigation never resolves the question and the interface-boundary reading persists indefinitely as a permanent workaround rather than a transitional accommodation while case law develops, the sunset clause is theater and the arrangement drifts toward tangled_rope in practice even though it is authored here as scaffold from this reading's own vantage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_versus_pretextual_boundary,
    'Is a given API boundary a genuine, stable, arm''s-length interface reflecting real architectural separation, or is it engineered specifically to route around the GPL''s derivative-work trigger while preserving tight functional coupling?',
    'Case-by-case judicial or expert technical review examining interface stability over time, documentation quality, whether the interface predates the licensing motivation, and whether internal data structures leak across the boundary in practice.',
    'If courts consistently find engineered seams pretextual, this reading''s practical scope narrows toward the narrow_linking_permissive_reading; if courts accept API cleanliness as dispositive regardless of intent, this reading''s scope widens and its extractiveness toward original authors increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_versus_pretextual_boundary, empirical, 'Whether the API boundary in any given case is architecturally genuine or legally pretextual.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the GPL''s own text (and the FSF''s contemporaneous guidance) actually indeterminate on the linking question, or does one reading better reflect the drafters'' original intent even though the text permits multiple readings?',
    'Historical analysis of GPL drafting history, FSF''s own published guidance evolution (including the LGPL''s creation as an explicit compromise instrument), and comparison with how courts in different jurisdictions (US fair-use-adjacent analysis vs. EU/German case law on linking) have resolved analogous disputes.',
    'If the drafters clearly intended the broad reading and the interface-boundary reading is a post-hoc commercial rationalization, this constraint''s coordination-function claim weakens substantially and it drifts toward tangled_rope; if genuine textual indeterminacy existed from the start, the scaffold framing while case law develops is more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel itself was ever determinate, or whether all three readings are post-hoc constructions filling a real textual gap.').

omega_variable(
    sunset_condition_realism,
    'Does this reading actually carry a realistic path to resolution (case law settling the boundary question), or will litigation risk and settlement patterns keep the question perpetually unresolved, making the ''scaffold'' framing a permanent fixture rather than a transitional one?',
    'Track whether major GPL-linking disputes proceed to published appellate rulings versus settling before judgment; a pattern of settlement-before-precedent indicates the ambiguity is structurally self-perpetuating.',
    'If the ambiguity is self-perpetuating, the has_sunset_clause declaration is aspirational rather than descriptive, and the constraint''s true operating mode is closer to tangled_rope with a scaffold cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_condition_realism, empirical, 'Whether the scaffold''s declared transitional character is realistic or theatrical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gpl__tr_t4, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(gpl__tr_t8, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(gpl__tr_t12, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(gpl__tr_t16, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(gpl__tr_t24, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gpl__be_t4, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(gpl__be_t8, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(gpl__be_t12, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(gpl__be_t16, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(gpl__be_t24, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 24, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gpl_derivative_work_trigger__interface_boundary_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__interface_boundary_reading, 0.15).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the gpl_derivative_work_trigger kernel. broad_copyleft_reading treats any linking (dynamic or static) as sufficient to create a derivative work, with high extractiveness authored from the perspective that vendors are evading a clear obligation. narrow_linking_permissive_reading treats linking as pure aggregation, with low extractiveness authored from the perspective that only source modification should trigger obligations. This interface_boundary_reading occupies a structurally distinct middle position — not an average, but its own arrangement with its own beneficiary/victim structure (ecosystem integrators benefit; users expecting full-stack source and reciprocity-seeking authors pay) and its own ε (0.42), reflecting genuine but bounded extraction riding on a genuine but partial coordination function. All three are linked bidirectionally in their respective network.affects_constraints arrays since a ruling or drift in any one reading's legal standing materially changes the practical scope of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
