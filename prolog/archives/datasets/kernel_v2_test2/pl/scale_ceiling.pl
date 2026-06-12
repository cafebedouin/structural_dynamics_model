% ============================================================================
% CONSTRAINT STORY: scale_ceiling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scale_ceiling, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: scale_ceiling
 *   human_readable: Scale Ceiling on Economy-Wide Coordination
 *   domain: political_economy/democratic_theory/institutional_analysis
 *
 * SUMMARY:
 *   The scale ceiling constraint describes the structural limit on
 *   economy-wide labor coordination as a function of polity size, regulatory
 *   fragmentation, and capital mobility. The constraint is claimed as
 *   mountain (natural law) based on the cross-national empirical pattern:
 *   coordination costs scale superlinearly with polity size, making
 *   economy-wide seller cartels (comprehensive labor coordination)
 *   structurally infeasible in large fragmented polities with open capital
 *   accounts, while leaving sectoral coordination viable at any scale. The
 *   primary observable is the cross-national responsiveness gradient: union
 *   density correlates with policy responsiveness in small polities (Nordic
 *   countries) but not in large fragmented ones (US, India), with the
 *   gradient modulated by capital account openness. The constraint sits
 *   downstream of organization_floor (the minimum viable scale for any
 *   coordination) and represents the upper bound where coordination cost
 *   exceeds organizing capacity. The structural delta is the coordination
 *   cost function itself — not a policy choice but an emergent property of
 *   information costs, verification requirements, and exit threat topology.
 *   However, the constraint declares beneficiaries (sectoral labor blocs,
 *   small polity unions, mobile capital), triggering false summit detection:
 *   if identifiable agents benefit from what is claimed as natural law, the
 *   'natural law' framing may be naturalizing a contingent institutional
 *   arrangement.
 *
 * KEY AGENTS:
 *   - Unorganized Labor (Large Polity): Primary non-beneficiary (powerless/trapped) — experiences ceiling as immutable barrier to economy-wide coordination; no exit from national labor market
 *   - National Labor Federation: Organized victim (organized/constrained) — has capacity to coordinate sectorally but hits structural barrier at economy-wide scale; constrained by national regulatory fragmentation
 *   - Mobile Capital: Primary beneficiary (institutional/arbitrage) — benefits from ceiling through exit threat that bounds labor coordination; arbitrage across continental scope
 *   - Sectoral Union Member: Secondary beneficiary (moderate/mobile) — benefits from sectoral coordination remaining viable while economy-wide coordination is blocked; mobile within regional labor markets
 *   - Small Polity Labor Movement: Tertiary beneficiary (institutional/constrained) — sits below ceiling threshold; economy-wide coordination remains viable in small-scale context
 *   - Political Economy Analyst: Analytical observer (analytical/analytical) — measures cross-national gradient; risks naturalizing institutional arrangement as coordination cost function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scale_ceiling, 0.12).
domain_priors:suppression_score(scale_ceiling, 0.08).
domain_priors:theater_ratio(scale_ceiling, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scale_ceiling, extractiveness, 0.12).
narrative_ontology:constraint_metric(scale_ceiling, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(scale_ceiling, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(scale_ceiling, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(scale_ceiling, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scale_ceiling, mountain).
narrative_ontology:human_readable(scale_ceiling, "Scale Ceiling on Economy-Wide Coordination").
narrative_ontology:topic_domain(scale_ceiling, "political_economy/democratic_theory/institutional_analysis").

domain_priors:emerges_naturally(scale_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scale_ceiling, sectoral_labor_blocs).
narrative_ontology:constraint_beneficiary(scale_ceiling, regional_coordination_networks).
narrative_ontology:constraint_beneficiary(scale_ceiling, small_polity_unions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(scale_ceiling, mobile_capital).
narrative_ontology:constraint_beneficiary(scale_ceiling, sectoral_union_member).
narrative_ontology:constraint_beneficiary(scale_ceiling, small_polity_labor_movement).
narrative_ontology:constraint_victim(scale_ceiling, unorganized_labor_large_polity).
narrative_ontology:constraint_victim(scale_ceiling, national_labor_federation).
narrative_ontology:constraint_vindicates(scale_ceiling, federalist_fragmentation_thesis).
narrative_ontology:constraint_vindicates(scale_ceiling, capital_mobility_constraint_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers in large fragmented polities (US, India) without union representation. Trapped in national labor markets with no exit option. Experience the scale ceiling as immutable barrier: sectoral unions exist but economy-wide coordination that would shift bargaining power consistently fails. Bear opportunity cost of blocked coordination — wage share and working conditions reflect atomized bargaining position. Biographical time horizon shows pattern stability across career span.
narrative_ontology:constraint_stakeholder(scale_ceiling, unorganized_labor_large_polity, payer,
    powerless, biographical, trapped, national).

% Peak labor organizations (AFL-CIO, TUC, DGB) attempting economy-wide coordination in large polities. Organized with substantial resources and political access, but constrained by regulatory fragmentation and capital mobility. Generational time horizon reveals repeated organizing cycles hitting the same scaling barrier. Can coordinate sectorally and influence specific policy domains, but economy-wide bargaining (wage coordination, comprehensive labor standards) remains structurally out of reach. Exit constrained by embeddedness in national institutional framework.
narrative_ontology:constraint_stakeholder(scale_ceiling, national_labor_federation, payer,
    organized, generational, constrained, national).

% Multinational corporations and financial capital with arbitrage capacity across continental scope. Benefits from scale ceiling through exit threat: credible threat to relocate investment bounds labor coordination at economy-wide scale. Does not actively enforce the ceiling (no lobbying against labor organizing per se) but benefits from the structural barrier. Arbitrage exit options mean effective extraction is negative — the constraint favors this position without requiring active maintenance.
narrative_ontology:constraint_stakeholder(scale_ceiling, mobile_capital, beneficiary,
    institutional, biographical, arbitrage, continental).

% Workers in sectors with sustained union density (construction, utilities, public sector). Moderate power through sectoral collective bargaining. Mobile within regional labor markets (can change employers within sector or move to adjacent sectors). Benefits from scale ceiling in two ways: (1) sectoral coordination remains viable while economy-wide coordination is blocked, reducing competition from broader labor coalitions; (2) sectoral unions capture rents that would be redistributed under economy-wide bargaining. Biographical time horizon shows stable sectoral coordination despite economy-wide coordination failure.
narrative_ontology:constraint_stakeholder(scale_ceiling, sectoral_union_member, beneficiary,
    moderate, biographical, mobile, regional).

% Labor movements in small polities (Nordic countries, Austria, Netherlands) where economy-wide coordination remains viable. Institutional power through corporatist bargaining structures. Constrained exit (embedded in national institutional framework, but polity scale is below ceiling threshold). Benefits from scale ceiling by sitting below it: coordination costs in small polities do not exceed organizing capacity, so economy-wide bargaining persists. Generational time horizon shows stability of corporatist arrangements that are structurally infeasible in large fragmented polities.
narrative_ontology:constraint_stakeholder(scale_ceiling, small_polity_labor_movement, beneficiary,
    institutional, generational, constrained, national).

% Researchers measuring cross-national responsiveness gradient: union density correlates with policy responsiveness in small polities but not in large fragmented ones. Analytical position with civilizational time horizon and global scope. Observes the pattern as emergent property of coordination cost function. Risks naturalizing institutional arrangement (Federalist fragmentation + capital mobility regime) as information-theoretic necessity. Neither collects from nor pays into the constraint — observes the structure from outside.
narrative_ontology:constraint_stakeholder(scale_ceiling, political_economy_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(scale_ceiling, mobile_capital).
narrative_ontology:fixing_cost_class(scale_ceiling, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The scale ceiling coordinates sectoral labor bargaining by bounding the scope of coordination to levels where information costs and verification requirements remain manageable. Sectoral unions can monitor compliance, verify wage agreements, and maintain solidarity within industry boundaries. The ceiling prevents coordination from expanding to economy-wide scale where these functions would break down.
% TRANSFER_FUNCTION: The constraint transfers bargaining power from unorganized labor (who would benefit from economy-wide coordination) to mobile capital (who benefits from exit threat that bounds coordination) and to sectoral labor blocs (who benefit from reduced competition from broader coalitions). The transfer is not money directly but the structural advantage that comes from atomized bargaining in large polities versus coordinated bargaining in small ones.
% ABSENT_VOICES: Unorganized labor in large polities would object if they could coordinate to voice objection — but the scale ceiling is precisely the barrier preventing that coordination. The absent voice is the counterfactual economy-wide labor coalition that cannot form. Also absent: labor movements in Global South large polities (Brazil, Indonesia, Nigeria) where scale ceiling combines with lower organizational capacity to produce even weaker coordination than in developed large polities.
% DISAPPEARANCE_RATIONALE: If the scale ceiling disappeared (coordination costs did not scale superlinearly, or capital mobility did not create exit threat), large polities would see emergence of economy-wide labor coordination similar to small-polity corporatism. Wage share would increase, working conditions would improve, and policy responsiveness to labor would rise in US, India, and other large fragmented polities. Mobile capital would face bounded exit options. Sectoral unions would face competition from broader coalitions. The current distribution of bargaining power depends on the ceiling's presence.
% FOUNDING_PROBLEM: The scale ceiling was not 'built' to solve a problem — it is claimed as emergent property of coordination cost function. However, the institutional arrangements that produce the ceiling (Federalist regulatory fragmentation, capital account liberalization) were built to solve specific problems: Federalist structure to prevent tyranny of majority (Federalist 10), capital account openness to enable international investment and trade. The ceiling is side effect of these arrangements, not their purpose.
% FOUNDING_PROBLEM_CORROBORATION: The 'founding problem' status is contested because it depends on whether the ceiling is natural law (coordination cost function) or naturalized arrangement (institutional construction). If natural law, there is no founding problem — the ceiling was never built. If naturalized arrangement, the founding problems (preventing majority tyranny, enabling capital flows) are still invoked to justify the institutional structures that produce the ceiling, but the justification is provided primarily by beneficiaries (mobile capital, small polity labor movements that benefit from competitive advantage, political economists who have internalized the naturalization). Corroboration from non-beneficiary sources is limited: unorganized labor in large polities does not have analytical apparatus to contest the 'natural law' framing, and national labor federations have partly internalized the ceiling as immutable.
narrative_ontology:disappearance_verdict(scale_ceiling, world_rearranges).
narrative_ontology:founding_problem_status(scale_ceiling, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNORGANIZED LABOR (MOUNTAIN) — Trapped within national labor market, biographical time horizon. Experiences the scale ceiling as immutable: coordination costs rise faster than organizing capacity as polity size increases. Cannot exit national jurisdiction; sees economy-wide coordination as structurally impossible regardless of effort.
constraint_indexing:constraint_classification(scale_ceiling, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NATIONAL LABOR FEDERATION (MOUNTAIN) — Organized agents with constrained exit (can shift sectors but not escape national regulatory structure). Generational time horizon allows observation of multiple organizing cycles. Experiences scale ceiling as structural limit: sectoral coordination remains viable, but economy-wide bargaining hits coordination cost barrier that no organizing strategy has overcome. The constraint appears immutable even with substantial organizing capacity.
constraint_indexing:constraint_classification(scale_ceiling, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MOBILE CAPITAL (MOUNTAIN) — Institutional power with arbitrage exit options across continental scope. Experiences scale ceiling as natural feature of political economy: capital mobility creates exit threat that bounds labor coordination at scale. From this position, the ceiling is not imposed but discovered — a structural property of open capital accounts interacting with polity fragmentation. Benefits from the constraint but does not enforce it.
constraint_indexing:constraint_classification(scale_ceiling, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: SECTORAL UNION MEMBER (MOUNTAIN) — Moderate power, mobile within regional labor markets. Biographical time horizon. Experiences the scale ceiling as background condition: sectoral coordination works (their union functions), but economy-wide coordination consistently fails. The pattern is stable across organizing attempts, suggesting structural rather than contingent barrier. Benefits from sectoral coordination remaining viable while economy-wide coordination is blocked.
constraint_indexing:constraint_classification(scale_ceiling, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — Civilizational time horizon, global scope. Observes cross-national pattern: coordination cost scales superlinearly with polity size, regulatory fragmentation, and capital mobility. The gradient is consistent across institutional contexts. Small polities (Nordic countries) sustain economy-wide coordination; large fragmented polities with open capital accounts (US, India) do not. The ceiling appears as emergent property of coordination cost function, not as constructed institutional choice. This is the claimed analytical classification.
constraint_indexing:constraint_classification(scale_ceiling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: SMALL POLITY LABOR (MOUNTAIN) — Institutional power in small polity context (Nordic model reference case). Constrained exit (embedded in national institutional framework). Generational time horizon shows stability of economy-wide coordination in small-scale context. Experiences scale ceiling as real but non-binding: their polity sits below the threshold where coordination costs exceed organizing capacity. The ceiling is mountain (immutable function) but their position relative to it is contingent on polity scale.
constraint_indexing:constraint_classification(scale_ceiling, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scale_ceiling_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(scale_ceiling, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scale_ceiling, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(scale_ceiling, ExtMetricName, E),
    domain_priors:suppression_score(scale_ceiling, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(scale_ceiling),
    narrative_ontology:constraint_metric(scale_ceiling, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(scale_ceiling, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(scale_ceiling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Low but non-zero. The constraint is claimed as mountain (natural law), but the presence of identifiable beneficiaries suggests asymmetric incidence. Mobile capital benefits from the exit threat that bounds labor coordination. Sectoral labor blocs benefit from the ceiling blocking economy-wide coordination that would compete with sectoral arrangements. Small polity unions benefit from sitting below the threshold. The extraction is the opportunity cost: unorganized labor in large polities cannot access economy-wide coordination that would be feasible absent the scaling barrier. The value is low because much of the 'extraction' may be inherent coordination cost rather than constructed barrier — the omega variables address this ambiguity. Measurements show modest increase from 0.08 (1950, lower capital mobility) to peak 0.15 (2010, maximum capital account openness) and slight decline to 0.12 (2025, some capital control reimposition). Suppression (0.08): Very low. The constraint does not require active enforcement — it emerges from coordination cost function. No institution suppresses economy-wide labor organizing; the barrier is structural (information costs, verification requirements, exit threat topology). Measurements show increase from 0.05 (1950) to 0.12 (2010) as capital mobility intensified exit threat, then decline to 0.08 (2025) as some polities reimposed partial capital controls. Theater ratio (0.15): Very low. Minimal performative content. The constraint is not maintained through ritual — it is discovered through organizing attempts that hit the scaling barrier. Some theater in policy discourse (politicians claiming to support labor while maintaining institutional fragmentation), but the core constraint is functional. Measurements show increase from 0.10 (1950) to 0.18 (2010) as neoliberal discourse naturalized the ceiling, then decline to 0.15 (2025) as political economy analysis made the institutional construction more visible. Accessibility collapse (0.88): Very high. Once the coordination cost function is understood, alternatives collapse nearly completely. Organizing strategies that work at sectoral scale consistently fail at economy-wide scale in large fragmented polities. The pattern is robust across institutional contexts. Resistance (0.05): Very low. Minimal active resistance to the constraint because it is experienced as structural limit rather than imposed rule. Labor movements resist specific policies (capital account openness, regulatory fragmentation) but do not resist the coordination cost function itself.
 *
 * PERSPECTIVAL GAP:
 *   All six perspectives classify as mountain, but with different structural relationships to the constraint. Unorganized labor (powerless/trapped) experiences the ceiling as immutable barrier with no exit — high effective extraction despite mountain classification. National labor federation (organized/constrained) has organizing capacity but still hits structural limit — moderate effective extraction. Mobile capital (institutional/arbitrage) experiences ceiling as natural feature that happens to favor their position — negative effective extraction (benefits without enforcement). Sectoral union member (moderate/mobile) benefits from sectoral coordination remaining viable — low positive effective extraction. Small polity labor (institutional/constrained) sits below threshold where ceiling binds — modest effective extraction. Analytical observer (analytical/analytical) measures cross-national gradient and risks naturalizing institutional arrangement as coordination cost function — no effective extraction (analytical position). The uniform mountain classification across perspectives is unusual (most constraints show type variation) and is itself diagnostic: either the constraint is genuine natural law (coordination cost function is information-theoretic necessity) or the mountain framing is false summit (all perspectives have internalized the naturalization of what is actually constructed institutional arrangement). The omega variables route this ambiguity through the apparatus rather than resolving it in the base classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Mobile capital is declared beneficiary with arbitrage exit — the engine derives low d (near 0.0), producing negative or near-zero effective extraction (capital experiences the constraint as favorable background condition). Sectoral union members are declared beneficiaries with mobile exit within regional scope — moderate d (≈0.2-0.3), low positive effective extraction (they benefit from sectoral coordination remaining viable). Small polity unions are declared beneficiaries with constrained exit — moderate d (≈0.3-0.4), modest effective extraction (they benefit from sitting below threshold but are embedded in national context). National labor federations are not declared as victims (no victim array entry) but are not beneficiaries either — the derivation treats them as symmetric (d ≈0.5), experiencing moderate extraction (they hit the ceiling but are not its target). Unorganized labor is not explicitly declared as victim (mountain constraints typically have no victims), but their structural position (powerless/trapped) produces high d (≈0.8-0.9) through the fallback derivation, yielding high effective extraction (they bear the opportunity cost of blocked economy-wide coordination). The analytical observer has analytical exit — d is undefined (analytical context), no effective extraction computed. The directionality pattern reveals asymmetric incidence: the constraint benefits mobile capital and organized sectoral blocs while imposing costs on unorganized labor in large polities. This asymmetry is the core evidence for the false summit hypothesis (omega variable false_summit_naturalization): if the constraint were pure natural law, beneficiary concentration would be coincidental; if it is naturalized institutional arrangement, beneficiary concentration is structural.
 *
 * MANDATROPHY ANALYSIS:
 *   The scale ceiling constraint demonstrates mandatrophy resolution through structural decomposition. The constraint is NOT 'labor coordination' (which would conflate sectoral and economy-wide scales) but specifically the ceiling on economy-wide coordination. Sectoral coordination remains viable (organization_floor is satisfied at sectoral scale), so the constraint is not 'all coordination is blocked' (which would be snare). The ceiling is the specific barrier that emerges at economy-wide scale in large fragmented polities with capital mobility. This decomposition prevents mislabeling: sectoral coordination is rope (genuine coordination function, low extraction), while the economy-wide ceiling is mountain (claimed as natural law, but with beneficiaries triggering false summit detection). The mandatrophy is resolved by recognizing that 'labor coordination' is not one constraint but a family of constraints at different scales, each with different structural properties. The scale ceiling is the upper bound of this family, sitting downstream of organization_floor (lower bound) and upstream of specific sectoral coordination mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_cost_function_form,
    'Is the superlinear scaling of coordination cost with polity size a mathematical necessity (information-theoretic lower bound) or an empirical regularity that could be overcome with different institutional technology?',
    'Information-theoretic analysis of minimum communication/verification costs for economy-wide bargaining as function of participant count and regulatory fragmentation. Historical analysis of coordination technologies (digital platforms, federated structures) and their impact on scaling exponent.',
    'If mathematical necessity: mountain classification confirmed across all contexts. If empirical regularity: constraint could shift to rope or scaffold if coordination technology changes, making current mountain classification time-bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_function_form, empirical, 'Whether coordination cost scaling is information-theoretic necessity or contingent empirical pattern').

omega_variable(
    capital_mobility_reversibility,
    'Is capital account openness a reversible policy choice or a one-way ratchet enforced by international institutional architecture?',
    'Analysis of historical capital control reimposition attempts (Malaysia 1998, Iceland 2008, Cyprus 2013). Assessment of WTO/IMF/bilateral treaty constraints on capital controls. Measurement of political feasibility vs technical feasibility gap.',
    'If reversible: the scale ceiling is partly constructed (capital mobility is policy choice), shifting classification toward tangled_rope for some perspectives. If irreversible ratchet: mountain classification strengthened — capital mobility is effectively natural constraint for contemporary polities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_mobility_reversibility, empirical, 'Whether capital account openness is reversible policy or structural lock-in').

omega_variable(
    sectoral_coordination_stability,
    'Do sectoral labor blocs remain viable indefinitely under scale ceiling, or does capital mobility eventually fragment even sectoral coordination?',
    'Longitudinal analysis of sectoral union density and bargaining coverage in large polities with open capital accounts. Identification of sectors with sustained coordination vs sectors showing fragmentation. Assessment of whether fragmentation correlates with capital mobility within sector.',
    'If sectoral coordination is stable: beneficiaries genuinely benefit from constraint (mountain with asymmetric incidence). If sectoral coordination erodes: constraint is transitional rather than permanent, suggesting scaffold dynamics at civilizational time horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sectoral_coordination_stability, empirical, 'Long-term stability of sectoral coordination under capital mobility pressure').

omega_variable(
    false_summit_naturalization,
    'Is the scale ceiling a genuine natural law (coordination cost function) or a naturalized institutional arrangement (Federalist fragmentation + capital mobility regime) that benefits mobile capital?',
    'Decomposition of coordination cost into information-theoretic minimum vs institutional friction. Cross-national comparison controlling for polity size: do countries with different constitutional structures (unitary vs federal) and capital control regimes show different ceiling heights? If ceiling varies with institutional design, the ''natural law'' framing is false summit.',
    'If genuine natural law: mountain classification holds; beneficiaries are incidental. If naturalized arrangement: reclassify as tangled_rope — coordination function (sectoral bargaining) coexists with extraction (economy-wide coordination blocked to benefit mobile capital).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether scale ceiling is natural law or naturalized institutional arrangement benefiting mobile capital').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scale_ceiling, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scale_ceil_theater_1950, scale_ceiling, theater_ratio, 0, 0.1).
narrative_ontology:measurement(scale_ceil_theater_1980, scale_ceiling, theater_ratio, 30, 0.12).
narrative_ontology:measurement(scale_ceil_theater_2010, scale_ceiling, theater_ratio, 60, 0.18).
narrative_ontology:measurement(scale_ceil_theater_2025, scale_ceiling, theater_ratio, 75, 0.15).

% Extraction over time
narrative_ontology:measurement(scale_ceil_extract_1950, scale_ceiling, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(scale_ceil_extract_1965, scale_ceiling, base_extractiveness, 15, 0.1).
narrative_ontology:measurement(scale_ceil_extract_1980, scale_ceiling, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(scale_ceil_extract_1995, scale_ceiling, base_extractiveness, 45, 0.14).
narrative_ontology:measurement(scale_ceil_extract_2010, scale_ceiling, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(scale_ceil_extract_2025, scale_ceiling, base_extractiveness, 75, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(scale_ceil_suppress_1950, scale_ceiling, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(scale_ceil_suppress_1980, scale_ceiling, suppression_requirement, 30, 0.08).
narrative_ontology:measurement(scale_ceil_suppress_2010, scale_ceiling, suppression_requirement, 60, 0.12).
narrative_ontology:measurement(scale_ceil_suppress_2025, scale_ceiling, suppression_requirement, 75, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scale_ceiling, resource_allocation).

% DUAL FORMULATION NOTE:
% The scale ceiling is downstream of organization_floor (minimum viable coordination scale) and represents the upper bound where coordination cost exceeds organizing capacity. The constraint is part of a family decomposition: organization_floor (mountain, lower bound) → sectoral_coordination (rope, viable at any scale) → scale_ceiling (mountain, upper bound on economy-wide coordination). Each member has distinct ε value reflecting different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
