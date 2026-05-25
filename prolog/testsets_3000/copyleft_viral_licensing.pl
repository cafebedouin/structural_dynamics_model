% ============================================================================
% CONSTRAINT STORY: copyleft_viral_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyleft_viral_licensing, []).

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
 *   constraint_id: copyleft_viral_licensing
 *   human_readable: Copyleft Viral Licensing (e.g., GPL)
 *   domain: technological/legal
 *
 * SUMMARY:
 *   Copyleft licensing, exemplified by the GNU General Public License (GPL),
 *   uses copyright law to enforce a reciprocal obligation: any software that
 *   incorporates GPL-licensed code must itself be released under GPL. This
 *   constraint exhibits radically different structural properties depending
 *   on the observer's position. For proprietary software vendors, copyleft is
 *   a snare: they cannot use GPL code without surrendering trade secrets or
 *   facing infringement liability. For open source communities, copyleft is
 *   pure coordination: it ensures contributions remain free and shared. For
 *   commercial firms building on GPL infrastructure, copyleft is a mixed
 *   constraint: they benefit from mature libraries and community
 *   contributions but face forced disclosure and licensing strategy
 *   constraints. The constraint's theater ratio has increased modestly over
 *   20 years (0.25 to 0.35) as legal interpretation has shifted from clear
 *   bright lines (source code distribution) to ambiguous boundaries
 *   (API-wrapped services, cloud computing, embedded systems). Extractiveness
 *   has remained relatively stable (0.35 to 0.38) because the structural
 *   tension — free software advocates enforcing perpetual freedom through
 *   legal constraints — is inherent to the mechanism, not an institutional
 *   drift.
 *
 * KEY AGENTS:
 *   - Open Source Developers: Primary beneficiaries (institutional/arbitrage) — benefit from symmetrical reciprocity and commons preservation
 *   - Open Source Community: Primary beneficiary (institutional/arbitrage) — coordinate through GPL's legal framework with low friction
 *   - Proprietary Software Vendors: Primary victims (powerful/trapped) — face binary choice between source disclosure and legal liability
 *   - Commercial Derivative Developers: Secondary victims (moderate/constrained) — experience mixed extraction (forced disclosure) and coordination (access to code)
 *   - Enterprise Adapter Layer: Organized agents (organized/mobile) — navigate legal ambiguity through service architecture and licensing strategies
 *   - Software Patent System: Institutional actor (institutional/constrained) — maintains patent grant clauses in GPL despite reduced relevance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing copyright law as immutable when it is politically contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyleft_viral_licensing, 0.38).
domain_priors:suppression_score(copyleft_viral_licensing, 0.42).
domain_priors:theater_ratio(copyleft_viral_licensing, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyleft_viral_licensing, extractiveness, 0.38).
narrative_ontology:constraint_metric(copyleft_viral_licensing, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(copyleft_viral_licensing, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyleft_viral_licensing, tangled_rope).
narrative_ontology:human_readable(copyleft_viral_licensing, "Copyleft Viral Licensing (e.g., GPL)").
narrative_ontology:topic_domain(copyleft_viral_licensing, "technological/legal").

domain_priors:requires_active_enforcement(copyleft_viral_licensing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyleft_viral_licensing, open_source_developers).
narrative_ontology:constraint_beneficiary(copyleft_viral_licensing, derivative_work_authors).
narrative_ontology:constraint_beneficiary(copyleft_viral_licensing, software_commons).
narrative_ontology:constraint_victim(copyleft_viral_licensing, proprietary_software_vendors).
narrative_ontology:constraint_victim(copyleft_viral_licensing, closed_source_business_models).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPRIETARY SOFTWARE VENDOR (SNARE) — A company incorporating GPL-licensed code into its product is legally trapped: it must either release source code (surrendering competitive advantage) or face infringement liability. Suppression is high: exit options are narrow (costly compliance or legal risk). No coordination benefit accrues to the vendor. Effective extraction runs from proprietary business model toward the commons.
constraint_indexing:constraint_classification(copyleft_viral_licensing, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMERCIAL DERIVATIVE DEVELOPER (TANGLED ROPE) — A firm building on GPL code experiences both coordination benefits (access to mature libraries, community contributions, peer review) and extraction costs (forced disclosure of improvements, constraint on licensing strategy). Exit is constrained but not impossible: they can avoid GPL components, but at engineering cost. Mixed experience: coordination enables their business model, but asymmetric disclosure requirement extracts value.
constraint_indexing:constraint_classification(copyleft_viral_licensing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPEN SOURCE COMMUNITY (ROPE) — Pure coordination mechanism from community perspective. GPL ensures their contributions remain free and accessible; derivative works are automatically part of the commons. No extraction: benefits are symmetrical. Community members can arbitrage away (use non-GPL alternatives) but choose GPL for its coordination properties. Theater ratio is low: the mechanism is functionally transparent — license text directly specifies rights and obligations.
constraint_indexing:constraint_classification(copyleft_viral_licensing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ENTERPRISE ADAPTER LAYER (SCAFFOLD) — Large enterprises (Google, Microsoft, Meta) have developed organized strategies to benefit from GPL while minimizing disclosure burden: API boundaries, service architecture, cloud deployment that avoids derivative work classification. These are temporary workarounds with a sunset: as courts clarify what constitutes 'derivative work' in service contexts, the loopholes close. Organizations experience suppression (legal uncertainty) but have agency (resources to navigate ambiguity) and clear exit paths (migrate to permissive licenses or GPL compliance). Theater: contract interpretation and license reading are performative labor; actual engineering constraints are minimal.
constraint_indexing:constraint_classification(copyleft_viral_licensing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SOFTWARE PATENT SYSTEM (PITON) — GPL's original purpose was coordination: ensure free software remains free. But in its interaction with patent law, GPL has become partly theatrical: GPL does not revoke patent rights, so a GPL licensor can still sue for patent infringement. The patent system persists as a performative constraint on GPL's freedom guarantee, maintained by institutional inertia rather than functional necessity. Patents are increasingly irrelevant to software practice (most firms use defensive patent portfolios, not offensive suits), but GPL's patent grant language persists to assuage pre-2010 concerns. Theater ratio high: the patent mechanism in GPL text serves a historical purpose that has largely atrophied.
constraint_indexing:constraint_classification(copyleft_viral_licensing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COPYRIGHT LAW VIEW (MOUNTAIN) — From a purely legal-logical perspective, copyleft is an immutable structural feature of copyright law: copyright exists; any copyright holder can impose conditions on derivative works; if conditions are imposed, they bind downstream users by copyright law itself. This perspective sees copyleft as a natural law of intellectual property — a logical consequence of copyright, not a contingent institutional arrangement. However, the structural data contradicts the mountain classification: suppression is moderate (0.42), not near-zero; extractiveness is moderate (0.38), not near-zero; theater is moderate (0.35), not near-zero. This reveals the 'immutable law' framing as naturalization of a contingent legal system (copyright) that could be replaced or reformed.
constraint_indexing:constraint_classification(copyleft_viral_licensing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyleft_viral_licensing_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(copyleft_viral_licensing, TR),
    TR >= 0.70.

:- end_tests(copyleft_viral_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. GPL enforces a reciprocal obligation, but the magnitude depends on the vendor's business model. Service-based companies can sometimes escape through architectural boundaries; embedded systems vendors face stricter constraints. The global average extraction is moderate because avoidance strategies exist (migrate to permissive licenses, re-architect services, use non-GPL alternatives) even though they carry engineering costs. Suppression (0.42): Moderate-high. The main suppressive element is legal uncertainty: ambiguity about what constitutes a 'derivative work' creates compliance friction. Additionally, vendors face career and strategic constraints: disclosing proprietary code violates business norms. However, suppression is not extreme because the rule is known and foreseeable. Theater ratio (0.35): Low-moderate. GPL's mechanism is functionally transparent: license text directly specifies rights and obligations. There is minimal performative labor (reading licenses, legal review) compared to, say, securities regulation. Theater has increased slightly because courts must now interpret edge cases (cloud, APIs, embedded systems) that the original GPL text did not clearly address — interpretation theater has replaced straightforward compliance theater.
 *
 * PERSPECTIVAL GAP:
 *   The proprietary vendor sees a Snare because exit options are genuinely limited (comply, rewrite, or litigate). The open source community sees a Rope because they designed and control the constraint for mutual benefit. The commercial derivative developer sees Tangled Rope because they benefit from GPL code access but are asymmetrically constrained by disclosure requirements. The enterprise adapter sees a Scaffold because large organizations have resources to navigate ambiguity and can migrate to alternatives once legal interpretation clarifies (and it has: AGPL addresses cloud computing). The patent system sees its own Piton status: the patent grant clause in GPL persists despite low utility because history carries institutional weight. The analytical observer risks seeing a Mountain by naturalizing copyright law as a fixed feature of property law, when copyright is actually a contingent political system subject to reform. This perspectival range — from immutable law to temporary scaffold to pure coordination — demonstrates that the same structural mechanism produces all six types.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from structural position. Proprietary vendors are targets (d ≈ 0.85): they bear extraction and have trapped exits. Open source developers are beneficiaries (d ≈ 0.15): they benefit from reciprocity and have arbitrage options (use non-GPL, or accept GPL). Commercial derivatives occupy middle ground (d ≈ 0.55): they benefit from code access (low d) but face extraction (high d) — constrained exit prevents full arbitrage. Enterprises with architectural workarounds (d ≈ 0.40): they have partial agency through legal interpretation loopholes (mobile exit), so experienced extraction is dampened. The patent system's position is ambiguous (d ≈ 0.50): it is formally part of GPL but functionally irrelevant — the constraint has no clear benefit or cost to patent holders, so directional symmetry applies. The analytical perspective (d ≈ 0.72): as observer, they see the full structure but derive no extraction or benefit; their role is analytic detachment.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that GPL is genuinely a Tangled Rope: it has both coordination function (ensures free software remains free, enables community contribution) and asymmetric extraction (proprietary vendors bear costs, open source beneficiaries profit). The core structural property is that GPL uses legal coercion (copyright enforcement) to achieve coordination (perpetual freedom). This hybrid nature is not a classification failure — it is the structural reality. The alternative classifications (Snare for vendors, Rope for communities, Scaffold for enterprises) are perspectival readings of the same base constraint. The mandatrophy is resolved by declaring that the constraint is a Tangled Rope from the global/institutional perspective while acknowledging that each observer has a legitimate partial perspective. GPL is neither pure coordination (open source myth) nor pure extraction (vendor complaints) — it is a hybrid that enforces reciprocity through legal constraints. This is exactly what Tangled Rope captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_definition,
    'What constitutes a ''derivative work'' under GPL in cloud/service architectures where code is not distributed to end users?',
    'Case law clarification on AGPL (Affero GPL) enforcement and API-boundary cases; legislative action on software copyright scope',
    'If narrow (binary: modified source or not): many service-based wrappers escape GPL. If broad (any functional dependency): GPL applies to most cloud software. High uncertainty directly enables the Scaffold perspective workarounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_definition, empirical, 'Definition of derivative work in service/cloud contexts').

omega_variable(
    commons_preservation_mechanism,
    'Is GPL the only legal mechanism that reliably preserves software freedom as commons, or can permissive licenses (MIT, Apache) achieve the same effect through social norm and competition?',
    'Longitudinal analysis of commons fragmentation in MIT vs GPL projects; measurement of derivative work closed-sourcing rates by license type',
    'If GPL is necessary: copyleft is a structural feature of freedom preservation (argues Mountain). If permissive licenses suffice: GPL is one strategy among many (argues against Mountain, supports Rope view).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_preservation_mechanism, conceptual, 'Whether GPL is the only effective commons preservation mechanism').

omega_variable(
    enforcement_effectiveness,
    'How effective is GPL enforcement at preventing proprietary relicensing? What fraction of GPL violations are detected vs undetected?',
    'Survey of known GPL compliance audits and litigation outcomes; analysis of compliance rates in closed-source derivative projects; reverse-engineering detection of GPL code in proprietary products',
    'If enforcement rate > 50%: GPL is a genuine constraint (Snare for vendors). If enforcement rate < 20%: GPL is a suggestion (Rope or weaker). Directly determines whether suppression coefficient should be 0.42 or lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_effectiveness, empirical, 'Effectiveness of GPL enforcement against violations').

omega_variable(
    business_model_substitution,
    'Can proprietary vendors completely exit GPL constraints by adopting service models (SaaS), embedded systems, or permissive alternatives without loss of competitive position?',
    'Market analysis of licensing strategies by sector; comparison of market share trends for proprietary vs AGPL-compliant services; engineering cost analysis of permissive alternative adoption',
    'If exit is fully available: GPL is not a Snare (should be lower extraction). If exit costs are prohibitive: GPL is a genuine Snare. Determines whether the vendor perspective should be classified as constrained (mid-extraction) vs trapped (high extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(business_model_substitution, empirical, 'Availability of vendor exit strategies from GPL constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyleft_viral_licensing, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copyleft_tr_t0, copyleft_viral_licensing, theater_ratio, 0, 0.25).
narrative_ontology:measurement(copyleft_tr_t10, copyleft_viral_licensing, theater_ratio, 10, 0.3).
narrative_ontology:measurement(copyleft_tr_t20, copyleft_viral_licensing, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(copyleft_be_t0, copyleft_viral_licensing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(copyleft_be_t10, copyleft_viral_licensing, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(copyleft_be_t20, copyleft_viral_licensing, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyleft_viral_licensing, enforcement_mechanism).
narrative_ontology:affects_constraint(copyleft_viral_licensing, permissive_open_source_licensing).
narrative_ontology:affects_constraint(copyleft_viral_licensing, software_patent_enforcement).
narrative_ontology:affects_constraint(copyleft_viral_licensing, derivative_work_definition).

% DUAL FORMULATION NOTE:
% Copyleft as coordination mechanism (open source perspective) is a distinct constraint from copyleft as extraction mechanism (proprietary vendor perspective). Both operate via the same legal text but have different ε values and base structural properties. However, unified analysis shows they are the same constraint viewed from different positions. This is not constraint decomposition — it is perspectival differentiation of a single Tangled Rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyleft_viral_licensing, powerful, 0.85).
constraint_indexing:directionality_override(copyleft_viral_licensing, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
