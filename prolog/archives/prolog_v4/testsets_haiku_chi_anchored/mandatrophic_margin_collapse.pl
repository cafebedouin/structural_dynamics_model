% ============================================================================
% CONSTRAINT STORY: mandatrophic_margin_collapse
% ============================================================================
% Version: 2.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandatrophic_margin_collapse, []).

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
 *   constraint_id: mandatrophic_margin_collapse
 *   human_readable: Mandatrophy: The Extraction of Margin for Mandate
 *   domain: institutional/technological
 *
 * SUMMARY:
 *   Mandatrophy is the systemic wasting away of resilience caused by rigid
 *   prioritization of administrative mandates over the operational margins
 *   (buffers, redundancies, adaptive slack) required for survival and
 *   adaptation. The constraint operates across institutional hierarchies —
 *   government agencies, healthcare systems, military organizations,
 *   corporate bureaucracies — wherever top-down directives encounter
 *   bottom-up resource constraints. The mandate-originating authority,
 *   insulated from operational consequences, enforces compliance metrics
 *   (cost reduction, efficiency targets, throughput maximization) that
 *   directly extract slack. Frontline personnel and the system's adaptive
 *   capacity experience this as pure extraction: margin is not a feature but
 *   the very capacity to absorb shocks, respond to novelty, and survive
 *   perturbation. The structure differs from simple resource scarcity:
 *   mandatrophy is not a shortage but an active prohibition of margin through
 *   enforcement. Organizational learning movements (agile, resilience
 *   frameworks, antifragility doctrine) are building alternative governance
 *   pathways that recognize margin as a functional input, not waste. The
 *   constraint exhibits all six DR types: pure extraction (Snare) from the
 *   frontline perspective, mixed extraction-coordination (Tangled Rope) from
 *   management, coordination-only (Rope) from the authority's perspective,
 *   degraded ritual (Piton) in compliance theater, temporary problem with
 *   sunset (Scaffold) from the resilience movement, and — most dangerously —
 *   false natural law (Mountain) when authority naturalizes mandatrophy as
 *   inherent to governance.
 *
 * KEY AGENTS:
 *   - Mandate-Originating Authority: Primary beneficiary (institutional/arbitrage) — benefits from compliance metrics and uniform execution; insulated from margin depletion consequences
 *   - Frontline Personnel: Primary victim (powerless/trapped) — execute mandates with vanishing buffer; no exit; bears full cost of cascading shortages
 *   - System Adaptive Capacity: Primary victim (moderate/constrained) — margin IS resilience; mandatrophy extracts the capacity to adapt, respond, and survive perturbation
 *   - Middle Management: Secondary actor (organized/constrained) — caught between upward mandate enforcement and downward resilience preservation; both enforcer and defender
 *   - Mandatrophy Awareness Movement: Organized response (organized/mobile) — resilience frameworks, agile methodologies, redundancy-preserving governance building alternative pathways with real sunset as adoption spreads
 *   - Compliance Theater System: Institutional degradation (institutional/constrained) — formal audits and metrics reporting have replaced functional oversight; persists through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing mandatrophy as a law of governance rather than a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandatrophic_margin_collapse, 0.68).
domain_priors:suppression_score(mandatrophic_margin_collapse, 0.72).
domain_priors:theater_ratio(mandatrophic_margin_collapse, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandatrophic_margin_collapse, extractiveness, 0.68).
narrative_ontology:constraint_metric(mandatrophic_margin_collapse, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(mandatrophic_margin_collapse, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandatrophic_margin_collapse, snare).
narrative_ontology:human_readable(mandatrophic_margin_collapse, "Mandatrophy: The Extraction of Margin for Mandate").
narrative_ontology:topic_domain(mandatrophic_margin_collapse, "institutional/technological").

domain_priors:requires_active_enforcement(mandatrophic_margin_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandatrophic_margin_collapse, mandate_originating_authority).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse, operational_resilience).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse, frontline_personnel).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse, system_adaptive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE PERSONNEL (SNARE) — Operational staff execute mandates with vanishing margin. No buffer, no slack, no exit. Each directive consumes reserves intended for resilience. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.95. Pure extraction masked as operational efficiency.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SYSTEM ADAPTIVE CAPACITY (SNARE) — The system qua organism cannot exit its own degradation. Margin is not a feature; it is the capacity to absorb shocks, adapt to novelty, and survive perturbation. Mandatrophy extracts margin directly. d≈0.88, f(d)≈1.30, σ=1.1 → χ≈0.96. Structural extraction of resilience itself.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: MANDATE-ORIGINATING AUTHORITY (ROPE) — Central authority sees mandate enforcement as coordination: alignment, compliance, predictability. The authority benefits from uniform execution and can arbitrage between systems (move resources, adjust timelines, shift accountability). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.08. Net beneficiary. Authority experiences this as functional governance.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MIDDLE MANAGEMENT (TANGLED ROPE) — Caught between mandate enforcement from above and resilience preservation from below. Management coordinates upward (mandate compliance) and downward (operational continuity), but bears both extraction and enforcement burden. d≈0.58, f(d)≈0.76, σ=1.0 → χ≈0.52. Mixed burden: must both extract margin (upward) and defend it (downward).
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MANDATROPHY AWARENESS MOVEMENT (SCAFFOLD) — Organizational learning, resilience frameworks, and antifragility doctrine are building alternative governance pathways that explicitly protect margin. Agile methodologies, redundancy-preserving procurement, slack-inclusive budgeting see the mandate-margin tension as solvable via process redesign with a real sunset: as organizations internalize margin preservation, the extractive pressure of top-down mandates loses force. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.31. Low effective extraction because alternatives exist and adoption is accelerating.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: THEATRICAL COMPLIANCE PERFORMANCE (PITON) — Bureaucratic display of mandate adherence (metrics reporting, compliance audits, certification rituals) has largely replaced functional oversight. The theater persists through institutional inertia — formal compliance is cheaper to verify than actual margin preservation. theater_ratio=0.65 ≥ 0.70 marginal threshold. The compliance performance is degraded: organizations report margin depletion as 'efficiency' and call adaptive shortcuts 'compliance innovations.'
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational view, centralized mandate enforcement creates inevitable margin-mandate tension: any system with top-down directives and no local agency will eventually extract slack in pursuit of compliance. This appears as inherent to hierarchical governance. However, the structural data (ε=0.68, suppression=0.72) contradicts the mountain classification — the engine flags this as a false summit. Mandatrophy is not a law of nature but a contingent institutional arrangement.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandatrophic_margin_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mandatrophic_margin_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mandatrophic_margin_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandatrophic_margin_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mandatrophic_margin_collapse, TR),
    TR >= 0.70.

:- end_tests(mandatrophic_margin_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The mandate-originating authority directly extracts margin through enforcement: every efficiency target, cost ceiling, throughput mandate consumes slack intended for resilience. The extraction is not incidental to mandate enforcement — it IS mandate enforcement. Extractiveness rises over time (0.35 → 0.68) as mandates accumulate and slack becomes progressively scarce, forcing harsher choices. Suppression (0.72): High. Frontline personnel have minimal alternatives: exit the organization (expensive in career terms), hide margin (violates reporting, risks career), or execute the mandate with depleted capacity (acceptance). Organizational culture, regulatory requirements, and hierarchical structure suppress genuine margin preservation. Theater ratio (0.65): Moderate-high, approaching the Piton threshold. Compliance theater (audits, metrics, certifications) has increasingly replaced functional oversight of actual margin status. Organizations report margin depletion as 'efficiency innovation' and operational shortcuts as 'adaptive compliance.' The theater reflects that authority cannot or will not directly measure resilience — instead, it measures compliance proxies. Claimed type (Snare): The frontline perspective is definitive — powerless agents trapped with extraction and no alternatives. The authority's view (Rope) is a false reading masking their net-beneficiary status.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how institutional hierarchy inverts the structural reality. The mandate-originating authority experiences margin-mandate tension as a coordination problem (Rope) — they see themselves as aligning the organization, achieving targets, driving efficiency. From their vantage point (immediate, global, arbitrage-enabled), the constraint is functional. But this perspective is disconnected from the structural extraction at ground level. Frontline personnel experience pure Snare: no buffer, no alternatives, no choice. The system's adaptive capacity (generational horizon) experiences progressive degradation: mandatrophy extracts the very capability to handle future shocks. Middle management is caught in Tangled Rope: they must both enforce mandates (extracting margin upward) and defend operational viability (resisting margin depletion downward). The Mandatrophy Awareness Movement sees a Scaffold: alternative governance paradigms (agile, resilience-inclusive budgeting, slack-as-feature) are building exits that will make top-down margin extraction obsolete. The compliance theater is Piton — performative, inert, degraded. The analytical observer risks seeing a Mountain (mandatrophy as inherent to hierarchical governance), but the structural data reveals this as a false summit: mandatrophy is contingent on the authority's ability to enforce without facing consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Mandate-Originating Authority: Beneficiary + arbitrage + immediate horizon → d≈0.08, f(d)≈-0.10. Net beneficiary with low effective extraction because they occupy a position of low cost and high benefit. They experience constraint primarily as enabling (coordination), not constraining. Frontline Personnel: Victim + trapped + biographical horizon → d≈0.92, f(d)≈1.40. Maximum extraction: no exit, no buffer, no choice. They bear the full extraction cost. System Adaptive Capacity: Victim + constrained + generational horizon → d≈0.88, f(d)≈1.30. High extraction: margin is resilience, and mandatrophy directly extracts it. The system cannot exit its own degradation. Middle Management: Victim + constrained (paradoxically also beneficiary via status) + generational horizon → d≈0.58, f(d)≈0.76. Mixed: they benefit from organizational position but bear extraction burden through overwork and conflicting directives. Mandatrophy Awareness Movement: Organized + mobile + civilizational horizon → d≈0.45, f(d)≈0.48. Low extraction because they have agency, alternatives, and visibility into the problem. Compliance Theater: Institutional + constrained + biographical horizon → d≈0.12, f(d)≈0.00. Piton derives from theater gate (0.65 ≥ 0.70 marginal), not from directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy at ε=0.68 by explicitly declaring what is being extracted (margin/resilience), who is extracting (mandate-originating authority), and who bears the cost (frontline, adaptive capacity, future generations). The false summit detector catches the Mountain perspective: mandatrophy is NOT inherent to hierarchical governance. It is a specific institutional arrangement where (1) top-down mandates are rigidly enforced, (2) authority is insulated from consequences, (3) margin is made invisible in compliance metrics, (4) alternatives are suppressed. Each of these can be reformed. The Scaffold perspective confirms: alternative governance models (agile, resilience frameworks, redundancy-preserving budgeting, opt-in coordination) are building genuine exits. The mandate-margin tension is solvable — it requires recognizing margin as a functional input, not waste, and decentralizing the authority to preserve it. The Tangled Rope and Piton perspectives reveal the intermediate states: middle management and compliance theater are both signs of system dysfunction and potential reform points. The constraint is NOT permanent. But it IS severe while active: ε=0.68, χ approaching 1.0 for victims, suppression=0.72. Mandatrophy kills organizations, healthcare systems, military units, and governance capacity. The mandate-originating authority experiences it as functioning governance precisely because they are insulated from the costs. This observational asymmetry (authority's perspective vs reality) is the core structural problem — not the existence of mandates, but the decoupling of mandate enforcement from consequence visibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    margin_sustainability_threshold,
    'What margin fraction (as % of operational capacity) is the minimum required for system resilience before adaptive capacity irreversibly degrades?',
    'Historical analysis of organizational collapse timelines post-mandatrophy; empirical study of margin levels across surviving vs failed systems; modeling of critical transition points in operational resilience',
    'If threshold is <5%: systems appear robust longer than they actually are, masking mandatrophy''s speed. If threshold is >15%: many mandates are inherently destructive and unsustainable. System-specific thresholds (hospital vs software vs military) suggest no universal constant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(margin_sustainability_threshold, empirical, 'Critical margin threshold for system resilience').

omega_variable(
    mandate_cascade_irreversibility,
    'Is margin depletion from serial mandate enforcement reversible through margin restoration, or does mandatrophy induce structural changes that persist even after mandate enforcement relaxes?',
    'Longitudinal study of organizations that relaxed mandates post-crisis; measurement of margin recovery trajectories; identification of institutional path-dependency and trust degradation',
    'If reversible: mandatrophy is a temporary constraint (Scaffold). If irreversible: mandatrophy is structural (Snare), and margin cannot be reconstructed without generational turnover or institutional restart.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_cascade_irreversibility, empirical, 'Whether margin depletion is reversible after mandate relaxation').

omega_variable(
    authority_visibility_asymmetry,
    'Does the mandate-originating authority lack genuine visibility into margin status at operational levels, or do they deliberately ignore margin signals to enforce compliance metrics?',
    'Analysis of organizational communication channels; study of how margin reporting is filtered, framed, or suppressed in hierarchical systems; comparison of authority statements about ''efficiency gains'' vs actual margin measurements',
    'If visibility asymmetry (authority cannot see margin): mandatrophy is a coordination failure (potential Rope or Scaffold). If deliberate suppression (authority sees margin but extracts anyway): mandatrophy is intentional extraction (pure Snare). Affects whether reform requires transparency vs value-shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_visibility_asymmetry, empirical, 'Whether authority ignores or cannot perceive margin signals').

omega_variable(
    mandate_versus_coordination_distinction,
    'Can mandate enforcement be reformulated as voluntary coordination without losing functional objectives, or does enforcement depend on suppression of alternatives?',
    'Pilot programs with opt-in (voluntary) vs mandatory compliance; measurement of outcome differences; analysis of why participation rates differ when coercion is removed',
    'If reformulatable: mandatrophy is Tangled Rope (extraction separable from coordination). If enforcement-dependent: mandatrophy is pure Snare (extraction is the mechanism). Determines feasibility of scaffold sunset pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_versus_coordination_distinction, conceptual, 'Whether mandate functions rely on enforcement or voluntary coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandatrophic_margin_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandatrophic_margin_collapse, theater_ratio, 0, 0.4).
narrative_ontology:measurement(mand_tr_t5, mandatrophic_margin_collapse, theater_ratio, 5, 0.53).
narrative_ontology:measurement(mand_tr_t10, mandatrophic_margin_collapse, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandatrophic_margin_collapse, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mand_be_t5, mandatrophic_margin_collapse, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mand_be_t10, mandatrophic_margin_collapse, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandatrophic_margin_collapse, enforcement_mechanism).
narrative_ontology:affects_constraint(mandatrophic_margin_collapse, organizational_resilience_depletion).
narrative_ontology:affects_constraint(mandatrophic_margin_collapse, hierarchical_opacity_asymmetry).
narrative_ontology:affects_constraint(mandatrophic_margin_collapse, compliance_theater_proliferation).

% DUAL FORMULATION NOTE:
% Mandatrophy is the mechanism by which hierarchical systems extract margin. It is upstream of organizational failure (which is the consequence) and connected to hierarchical opacity (which is the enabling condition). The constraint family links mandatrophy-as-mechanism to its specific manifestations in healthcare (clinical_margin_extraction), military (readiness_margin_collapse), government (administrative_capacity_depletion), and corporate (operational_resilience_wasting). Each domain story has its own ε; mandatrophy_margin_collapse (ε=0.68) is the generic institutional mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandatrophic_margin_collapse, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
