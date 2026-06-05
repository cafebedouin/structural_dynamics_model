% ============================================================================
% CONSTRAINT STORY: license_reciprocity_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_license_reciprocity_enforcement, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: license_reciprocity_enforcement
 *   human_readable: License Reciprocity Enforcement in Software Ecosystems
 *   domain: software_governance/licensing
 *
 * SUMMARY:
 *   License reciprocity enforcement in software ecosystems represents a
 *   hybrid coordination-extraction mechanism. GPL and similar copyleft
 *   licenses solve a genuine collective action problem: preventing the
 *   enclosure of shared software commons into proprietary derivative works.
 *   But the enforcement mechanism is also extractive — it imposes involuntary
 *   transparency requirements and derivative-work disclosure obligations on
 *   developers who integrate copyleft code into their own projects. The
 *   constraint exhibits all perspectives from the six-type taxonomy.
 *   Proprietary developers see a trap; integration companies see mixed costs
 *   and benefits; copyleft maintainers see pure coordination; permissive
 *   license coalitions see a temporary friction with sunset pathways; the
 *   enforcement apparatus appears degraded (theater-dominant); large
 *   technology companies maintain arbitrage exits unavailable to smaller
 *   developers; and analytical observers risk naturalizing a contingent legal
 *   arrangement as an immutable logical truth. Extractiveness has risen over
 *   20 years (0.35 → 0.52) as license complexity increased and integration
 *   patterns became tighter; theater ratio has also risen (0.22 → 0.48) as
 *   enforcement became more aspirational than actual.
 *
 * KEY AGENTS:
 *   - Proprietary Software Developers: Primary victim (powerless/trapped) — cannot integrate copyleft code without triggering license obligations or rewriting; binary exit cost
 *   - Integration Companies: Secondary victim (moderate/constrained) — face license compatibility analysis costs, engineering labor, legal review; also benefit from mature libraries
 *   - Copyleft Commons Maintainers: Primary beneficiary (institutional/arbitrage) — control orchestrates derivative work contributions back to commons; experiences as pure coordination
 *   - Permissive License Coalition: Organized agent (organized/mobile) — MIT, Apache, BSD projects building exit pathways through dual-licensing and explicit non-copyleft alternatives
 *   - Large Technology Companies: Powerful beneficiary (powerful/arbitrage) — deploy copyleft enforcement against smaller competitors while maintaining proprietary control through cloud architectures and license loopholes
 *   - GPL Enforcement Apparatus: Institutional actor (institutional/mobile) — Software Freedom Conservancy, license compliance audits, litigation threat; sees own apparatus as degraded
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent legal artifact as immutable principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(license_reciprocity_enforcement, 0.52).
domain_priors:suppression_score(license_reciprocity_enforcement, 0.58).
domain_priors:theater_ratio(license_reciprocity_enforcement, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(license_reciprocity_enforcement, extractiveness, 0.52).
narrative_ontology:constraint_metric(license_reciprocity_enforcement, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(license_reciprocity_enforcement, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(license_reciprocity_enforcement, tangled_rope).
narrative_ontology:human_readable(license_reciprocity_enforcement, "License Reciprocity Enforcement in Software Ecosystems").
narrative_ontology:topic_domain(license_reciprocity_enforcement, "software_governance/licensing").

domain_priors:requires_active_enforcement(license_reciprocity_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(license_reciprocity_enforcement, copyleft_software_commons).
narrative_ontology:constraint_beneficiary(license_reciprocity_enforcement, derivative_work_creators).
narrative_ontology:constraint_victim(license_reciprocity_enforcement, proprietary_software_developers).
narrative_ontology:constraint_victim(license_reciprocity_enforcement, closed_source_integrators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPRIETARY DEVELOPER (SNARE) — Trapped by GPL terms if derivative work is created. Cannot commercialize without either paying license conversion costs, rewriting to avoid triggering copyleft, or accepting open-source disclosure. Exit options are costly and binary. No negotiation available once the copyleft trigger fires.
constraint_indexing:constraint_classification(license_reciprocity_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTEGRATION COMPANY (TANGLED ROPE) — Constrained by license compatibility requirements but also benefits from access to mature copyleft libraries. Can invest in license-compliant architecture (conditional cooperation benefit), but faces labor costs for GPL-compliant engineering and legal review. Extraction is real but asymmetric — not maximal as in snare.
constraint_indexing:constraint_classification(license_reciprocity_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COPYLEFT COMMONS MAINTAINER (ROPE) — Institutional beneficiary (organized open-source projects). Experiences copyleft enforcement as pure coordination: it guarantees that derivative works remain available to the commons, enabling contributor networks to accumulate value. No experienced extraction — the constraint funds the commons through mandatory contribution of improvements.
constraint_indexing:constraint_classification(license_reciprocity_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PERMISSIVE LICENSE COALITION (SCAFFOLD) — Organized actors (MIT/Apache-licensed projects, dual-licensing strategies) have built alternative pathways that reduce effective suppression. Permissive licenses maintain coordination benefits without the binary trap. As permissive norms mature and dual-licensing becomes standard, the hard reciprocity wall softens — copyleft becomes one option among many rather than a mandatory extraction mechanism.
constraint_indexing:constraint_classification(license_reciprocity_enforcement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: GPL ENFORCEMENT APPARATUS (PITON) — The legal infrastructure for enforcing GPL terms (Software Freedom Conservancy, litigation threat, license compliance audits) persists through institutional inertia but faces degradation as enforcement becomes costly and unpopular. Many GPL projects no longer actively police derivatives. The apparatus maintains its theatrical presence (licensing threat, compliance documentation) but has lower functional enforcement than past decades. Theater ratio reflects the asymmetry between stated enforcement intensity and actual follow-through.
constraint_indexing:constraint_classification(license_reciprocity_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LARGE TECHNOLOGY COMPANY (TANGLED ROPE) — Powerful actors (Google, Apple, Meta) benefit from copyleft enforcement against competitors while maintaining proprietary control of their own systems through architectural separation, licensing strategies (cloud loopholes), or dual-licensing. They have arbitrage options unavailable to small developers: ability to hire GPL expertise, negotiate exception licenses, or engineer around copyleft triggers. Experienced extraction is moderate — they extract value from the constraint by using it against rivals while maintaining leverage to escape it themselves.
constraint_indexing:constraint_classification(license_reciprocity_enforcement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a purely logical perspective, copyleft enforcement reflects an immutable principle of information law: if software can be copied and modified freely, controlling its use requires legally binding chains. This perspective naturalizes license reciprocity as inherent to copyright mechanics. However, the structural data contradicts the mountain classification — the constraint is contingent on specific legal jurisdictions, licensing strategies, and enforcement capacity.
constraint_indexing:constraint_classification(license_reciprocity_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(license_reciprocity_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(license_reciprocity_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(license_reciprocity_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(license_reciprocity_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(license_reciprocity_enforcement, TR),
    TR >= 0.70.

:- end_tests(license_reciprocity_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts involuntary disclosure and transparency requirements from derivatives. But it is not maximal (0.70+) because: (a) the extraction flows toward a genuine commons benefit, not private capture; (b) derivative creators retain some license choice and architectural options; (c) large actors maintain loopholes (cloud deployment, dual-licensing, organizational separation). The rising trajectory (0.35 → 0.52) reflects integration tightness increasing faster than architectural alternatives mature. Suppression (0.58): Moderate-high. Real barriers to non-compliance exist: legal liability, reputational risk in open-source communities, license audits. But not total — many developers simply ignore GPL terms without consequence, and enforcement is often theatrical. Theater ratio (0.48): Moderate. The apparatus maintains visible enforcement posture (compliance documentation, licensing threat) but actual litigation and enforcement action has declined as a proportion of stated enforcement threat. This reflects Piton dynamics: the institutional apparatus persists through inertia but its functional enforcement has degraded. Claimed type (Tangled Rope): Justified by genuine coordination function (commons protection) PLUS asymmetric extraction (mandatory disclosure). Active enforcement is required, beneficiaries and victims are distinct, and extraction is not maximal.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits strong perspectival divergence. The proprietary developer sees a binary trap (Snare) — no negotiation, no sliding scale, full disclosure required. The integration company sees mixed coordination and extraction (Tangled Rope) — the library is genuinely valuable, but the license cost is real. The copyleft maintainer sees pure coordination (Rope) — the constraint ensures improvements return to the commons. The permissive coalition sees a friction point with a sunset (Scaffold) — as alternatives mature, hard copyleft becomes optional. The enforcement apparatus sees its own degradation (Piton) — the legal threat persists but actual enforcement is patchy. The large tech company sees arbitrage opportunity (Tangled Rope with skew toward beneficiary) — they can enforce against rivals while maintaining loopholes. The analytical observer risks seeing immutable logic (Mountain) — copyright mechanics require control chains — but the structural data reveals contingency: derivative work definitions change across jurisdictions, enforcement capacity varies, architectural patterns shift.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural position. Proprietary developers: victims + trapped exit → high d → high f(d) → high experienced chi. Integration companies: mixed (benefit from libraries + victim of obligations) + constrained exit → moderate d → moderate chi. Copyleft maintainers: beneficiaries + arbitrage → low d → negative/minimal chi. Large tech companies: beneficiaries + arbitrage (plus loophole access) → very low d → minimal chi. The structural derivative is: small/powerless developers experience maximum extraction; organized actors and large companies maintain arbitrage; institutional beneficiaries experience as coordination. This d-based differentiation captures the asymmetric extraction that defines Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   License reciprocity enforcement resolves the mandatrophy by revealing how the same institutional artifact can coordinate commons protection while extracting from individual developers. The Tangled Rope classification prevents collapsing into pure extraction (Snare frame: 'GPL is just a trap') or pure coordination (Rope frame: 'GPL is just voluntary cooperation'). Both frames are perspectivally true. The constraint genuinely solves the enclosure problem (coordination function) AND genuinely imposes involuntary transparency (extraction function). The mandatrophy is resolved by recognizing that institutional classification depends on structural position: from the commons maintainer perspective, it is Rope; from the proprietary developer perspective, it is Snare; from the moderate integration company, it is Tangled Rope. The framework's job is to show all three, not to force one classification onto all contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_trigger_ambiguity,
    'What constitutes a ''derivative work'' under copyleft licensing? Does static linking, dynamic linking, plugin architecture, network interaction, or API binding trigger copyleft obligations?',
    'Case law analysis, license interpretation in different jurisdictions, technical architecture mapping to legal definitions',
    'If trigger is narrow (static linking only): many projects avoid copyleft obligation — suppression drops, constraint shifts toward Rope. If trigger is broad (any meaningful integration): more projects trapped — suppression rises, constraint shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_trigger_ambiguity, conceptual, 'Legal ambiguity in derivative work definition triggers different constraint classifications').

omega_variable(
    enforcement_willingness_trend,
    'Are copyleft projects actually enforcing license violations, or has enforcement become largely theatrical?',
    'Longitudinal data on cease-and-desist letters, license enforcement actions, litigation rate over time; correlation with project size and commercial stakes',
    'If enforcement is active: suppression remains high (0.58+), constraint remains Tangled Rope/Snare. If enforcement is declining: suppression drops to 0.3-0.4, constraint shifts toward Rope or Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_willingness_trend, empirical, 'Actual enforcement rate vs stated enforcement posture').

omega_variable(
    cloud_loophole_viability,
    'Does the ''cloud loophole'' (AGPL carve-out, Tivoization work-around through network delivery) actually circumvent copyleft obligations, or does it merely shift enforcement burden?',
    'AGPL adoption rates, enforcement action against cloud deployments, technical analysis of whether AGPL actually closes the loophole',
    'If loophole is viable: large companies maintain arbitrage, extracted value concentrates, constraint remains asymmetrically extractive (high chi for powerless). If loophole fails: constraint becomes more symmetric, shifts toward pure Rope for all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cloud_loophole_viability, empirical, 'Whether cloud/network deployment architecture escapes copyleft enforcement').

omega_variable(
    permissive_license_substitution_rate,
    'Are permissive licenses (MIT, Apache) actually displacing GPL in practical adoption, or is GPL still the dominant copyleft standard?',
    'GitHub language trends, package manager adoption statistics, enterprise license preference surveys',
    'If permissive licenses dominate: copyleft reciprocity becomes a minority constraint — theater ratio rises, extracted value declines, constraint shifts toward Piton. If GPL dominates: copyleft enforcement remains structurally significant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(permissive_license_substitution_rate, empirical, 'Market displacement of GPL by permissive alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(license_reciprocity_enforcement, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(licrec_tr_t0, license_reciprocity_enforcement, theater_ratio, 0, 0.22).
narrative_ontology:measurement(licrec_tr_t10, license_reciprocity_enforcement, theater_ratio, 10, 0.35).
narrative_ontology:measurement(licrec_tr_t20, license_reciprocity_enforcement, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(licrec_be_t0, license_reciprocity_enforcement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(licrec_be_t10, license_reciprocity_enforcement, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(licrec_be_t20, license_reciprocity_enforcement, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(license_reciprocity_enforcement, information_standard).
narrative_ontology:affects_constraint(license_reciprocity_enforcement, software_commons_enclosure).
narrative_ontology:affects_constraint(license_reciprocity_enforcement, proprietary_software_licensing).
narrative_ontology:affects_constraint(license_reciprocity_enforcement, open_source_governance_capture).

% DUAL FORMULATION NOTE:
% License reciprocity enforcement is downstream of the fundamental software commons preservation problem but represents a distinct structural constraint. It exists to solve commons enclosure but creates extraction asymmetries in the process. The upstream constraint (software_commons_enclosure) has higher theater and lower active extraction; this constraint carries both coordination and extraction functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(license_reciprocity_enforcement, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
