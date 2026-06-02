% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__legitimacy_erosion_reading, []).

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
 *   constraint_id: ietf_openness_commitment__legitimacy_erosion_reading
 *   human_readable: IETF Openness Commitment: Legitimacy Erosion Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This reading instantiates one structural interpretation of the IETF's
 *   openness commitment: that the rough consensus mechanism itself has become
 *   a vehicle for extracting legitimacy from the standards process.
 *   Well-resourced technology factions can afford sustained participation,
 *   procedural expertise, and subtle influence that shape consensus outcomes
 *   toward their preferences while maintaining the procedural theater that
 *   validates the result as 'rough consensus.' The reading does NOT claim the
 *   IETF is engaged in conscious conspiracy — rather, that resource
 *   asymmetries have created structural dynamics where procedural safeguards
 *   (working group discussion, consensus calls, IESG review) are insufficient
 *   to prevent systematic advantage. The victim is the consensus mechanism's
 *   credibility itself: each captured standard vote reduces the legitimacy
 *   commons available to future participants. The constraint exhibits both
 *   coordination function (preventing unilateral standard-setting, enabling
 *   participation) and extraction (well-resourced factions ratify
 *   predetermined outcomes while maintaining democratic appearance). This
 *   reading is one of three in the ietf_openness_commitment kernel. The
 *   commons_stewardship_reading emphasizes active stewardship and recovery of
 *   participatory norms; the capture_substrate_reading treats the mechanism
 *   as intrinsically vulnerable to any organized faction. This
 *   legitimacy_erosion_reading occupies the middle ground: the mechanism is
 *   being eroded, not irretrievably captured, but only if the erosion is
 *   identified and actively countered.
 *
 * KEY AGENTS:
 *   - Well-Resourced Factions (Large Incumbents): Institutional/arbitrage — benefit from shaped consensus; primary beneficiary. Can afford continuous participation, procedural expertise, travel costs, and sustained engagement.
 *   - Consensus Mechanism's Credibility: Powerless/trapped — abstract victim; cannot organize or exit. Degraded each time procedure is used to ratify predetermined outcomes.
 *   - Developing Region Participants: Powerless/trapped — face material barriers (timezone disparity, bandwidth, staff resources) that prevent meaningful engagement. Trapped in procedure with low voice.
 *   - Independent Researchers: Moderate/constrained — priced out of participation through resource requirements but retain some option to engage; face high but not insurmountable costs.
 *   - Mid-Size Companies: Organized/constrained — can participate but cannot dictate; experience genuine mixed benefit/cost dynamic.
 *   - IETF Institutional Structure: Institutional/arbitrage — maintains procedure through inertia; sees own mechanism as degraded but lacks incentive to reform.
 *   - Analytical Observer: Analytical/analytical — risks naturalizing resource asymmetry as inevitable; may miss contingent procedural choices that could reduce (not eliminate) extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.58).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.52).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Openness Commitment: Legitimacy Erosion Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a').
narrative_ontology:cs_kernel_codification('7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a', formalized).
narrative_ontology:cs_authority_grounding('7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a', lineage).
narrative_ontology:cs_interpretation_layer_present('7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a').
narrative_ontology:cs_reading_relation('7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a', ietf_openness_commitment__capture_substrate_reading, influences).
narrative_ontology:cs_axiom('7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a', foundational, consensus_procedural_integrity_is_recoverable).
narrative_ontology:cs_axiom_status(consensus_procedural_integrity_is_recoverable, holdable).
narrative_ontology:cs_axiom_grounding('7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a', consensus_procedural_integrity_is_recoverable, empirically_contingent).
narrative_ontology:cs_axiom('7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a', foundational, extraction_is_masked_by_procedural_theater).
narrative_ontology:cs_axiom_status(extraction_is_masked_by_procedural_theater, holdable).
narrative_ontology:cs_axiom_grounding('7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a', extraction_is_masked_by_procedural_theater, empirically_contingent).
narrative_ontology:cs_reference_frame('7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a', rough_consensus_as_participatory_legitimacy).
narrative_ontology:cs_drift_state('7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a', contemporary_large_incumbent_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7e2e3318-ae0e-4c6f-ba3c-3c1d5ad9169a', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_factions).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, large_technology_incumbents).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, consensus_mechanism_credibility).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, developing_region_participants).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSENSUS MECHANISM CREDIBILITY (SNARE) — The procedural integrity of the rough consensus process is itself extracted. When well-resourced factions use procedural maneuvering to ratify predetermined outcomes, the credibility of 'rough consensus' as a legitimacy commons is degraded. The mechanism cannot exit or defend itself; it bears the full cost of being weaponized.
constraint_indexing:constraint_classification(ietf_openness_commitment__legitimacy_erosion_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING REGION PARTICIPANTS (SNARE) — Participants from regions with limited bandwidth, fewer timezone coverage by working groups, and fewer staff to attend in-person meetings face structural barriers to meaningful participation. Well-resourced factions can afford to station engineers in multiple timezones, subsidize travel, and maintain continuous engagement. The trapped developing-region participant experiences the consensus process as theater: their input is formally solicited but strategically marginalized through timing, attrition, and procedural complexity.
constraint_indexing:constraint_classification(ietf_openness_commitment__legitimacy_erosion_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INDEPENDENT RESEARCHERS / SMALL OPERATORS (SNARE) — Researchers and implementers without corporate backing face resource constraints (travel costs, time commitments, hiring consultants) that prevent meaningful participation in the working group process. They experience high suppression — the barriers are real and material — but not total entrapment. Some manage to participate; most are priced out. Classification is snare at this level because the constraint offers minimal coordination benefit — these agents are being extracted from, not coordinated with.
constraint_indexing:constraint_classification(ietf_openness_commitment__legitimacy_erosion_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MID-SIZE TECHNOLOGY COMPANIES (TANGLED ROPE) — Firms with sufficient resources to maintain working group participation but insufficient dominance to dictate outcomes experience genuine mixed dynamics. The rough consensus process coordinates their interaction (preventing unilateral standard-setting) while simultaneously extracting from them through attrition and procedural overhead. They have real agency and real benefit (participation in legitimacy-granting process) but also real cost (inability to capture outcomes unilaterally).
constraint_indexing:constraint_classification(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LARGE TECHNOLOGY INCUMBENTS (ROPE) — Well-resourced multinational corporations experience the rough consensus process as coordination mechanism with asymmetric advantage. They can afford to shape consensus through continuous participation, procedural expertise, and subtle influence. They experience the mechanism as enabling (coordination that prevents destructive fragmentation) not extractive. They have arbitrage options — they could build proprietary standards — but the consensus mechanism offers them legitimacy at lower cost than alternatives.
constraint_indexing:constraint_classification(ietf_openness_commitment__legitimacy_erosion_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: IETF INSTITUTIONAL STRUCTURE (PITON) — The IETF as institutional body maintains the rough consensus procedure as its legitimating ritual despite erosion of genuine democratic function. The mechanism persists through institutional inertia: abandoning it would require admitting that the legitimacy commons has been compromised. The theater ratio is high because the performative element (formal discussion, mailing list review, consensus-call procedures) dominates the actual decision-making (which often follows predictable patterns of resource asymmetry).
constraint_indexing:constraint_classification(ietf_openness_commitment__legitimacy_erosion_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INEVITABILITY VIEW (MOUNTAIN) — From a civilizational perspective, resource asymmetry in any distributed governance system is inevitable: those with more resources will have more influence, and procedures cannot overcome structural material inequalities. This perspective sees the 'degradation' of rough consensus as naturalization of what is structurally unavoidable. However, this classification masks the contingent institutional choices that could reduce but not eliminate asymmetry — the mountain framing naturalizes what is actually a tangled_rope/snare hybrid.
constraint_indexing:constraint_classification(ietf_openness_commitment__legitimacy_erosion_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ietf_openness_commitment__legitimacy_erosion_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ietf_openness_commitment__legitimacy_erosion_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, TR),
    TR >= 0.70.

:- end_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mechanism extracts legitimacy from the standards process through captured consensus votes, but the extraction is not maximal because: (1) significant technical coordination does occur through the process (genuine benefit exists), (2) some independent voices do influence outcomes (not total suppression), (3) alternatives exist (IETF participation is not literally forced). The extractiveness has grown over the interval (0.38 → 0.58) as resource asymmetries have accumulated and procedural complexity has increased, favoring sustained-participation advantage. Suppression (0.52): Moderate-high. Barriers to meaningful participation include: timezone disparity, travel costs, staff resource requirements, procedural learning curve, attrition through multi-year working group engagement, and differential capacity to maintain presence. Suppression is high but not total — some underresourced participants do engage, and mailing list participation is formally included. Theater ratio (0.68): High and rising. Consensus calls, mailing list review, IESG appeals, and formal discussion procedures are performed but often do not change outcomes that follow predictable patterns of resource-based influence. The theater has increased because procedures have proliferated (creating appearance of more oversight) while actual decision-making remains asymmetric. The mechanism's legitimacy depends on the appearance of open deliberation, making the theatrical element structurally essential to the extraction.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (well-resourced incumbents) sees the mechanism as legitimate coordination that enables standard-setting at scale — they experience Rope classification. The powerless victim (consensus mechanism credibility) experiences pure Snare — their credibility is extracted to ratify outcomes not genuinely determined through consensus. The developing-region participant experiences Snare — formal inclusion is performative while actual influence is denied. The independent researcher experiences Snare — priced out despite formal openness. The mid-size company experiences Tangled Rope — genuine mixed benefit/cost. The IETF itself experiences Piton — maintains procedure as legitimating ritual despite recognizing functional degradation. The analytical observer risks Mountain classification (resource asymmetry is inevitable) masking the contingent procedural choices that enable or constrain extraction. The perspectival gap reveals that the 'same mechanism' operates as coordination-with-asymmetry (for beneficiaries), pure extraction (for powerless victims), and legitimacy theater (for institutional maintainers).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status + exit options. Well-resourced beneficiaries with arbitrage options (can build proprietary standards, can choose not to participate if captured) experience low d → low chi. The consensus mechanism credibility has no exit (d ≈ 1.0 → high chi, snare). Trapped developing-region participants (cannot exit without abandoning international standards influence) experience high d → high chi. Constrained independent researchers (can exit at cost of technical isolation) experience moderate-high d → moderate-high chi. The piton classification derives from high theater_ratio (0.68 ≥ 0.70 gate is approached), not from high chi — the institutional maintainer experiences the mechanism as performative, not as high-extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint avoids mandatrophy by explicitly declaring beneficiaries (well-resourced factions), victims (consensus mechanism credibility, developing-region participants), and active enforcement (procedural mechanisms that sustain the asymmetry). The tangled_rope classification is justified: (1) Genuine coordination function exists (rough consensus does prevent destructive fragmentation and enables participation by 100+ working groups simultaneously), (2) Asymmetric extraction exists (well-resourced factions shape outcomes while appearing to emerge from consensus), (3) Active enforcement is required (procedural rules, chair discretion, IESG review all actively maintain the appearance of consensus while enabling resource-based advantage). The constraint does NOT collapse to pure Rope (which would require suppression ≤ 0.05) nor to pure Snare (which would require elimination of coordination function). It occupies the authentic Tangled Rope space: coordination AND extraction co-constitute the mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_capture_vs_preference_aggregation,
    'Is the observed outcome disparity (well-resourced factions prevailing) evidence of genuine procedural capture (weaponization of consensus mechanism) or legitimate preference aggregation (those with more resources naturally have more influence)?',
    'Comparative analysis: (1) outcome correlation with resource level vs outcome correlation with technical merit of proposals; (2) counterfactual: what would rough consensus outcomes look like if resource barriers were substantially reduced? (3) deliberative quality assessment: do formal discussions change outcomes or merely ratify predetermined positions?',
    'If capture: legitimacy_erosion reading confirmed; constraint is Snare/Tangled Rope. If preference aggregation: constraint is lower-extractiveness Rope/Tangled Rope; ''unfairness'' is traded for stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_capture_vs_preference_aggregation, empirical, 'Whether observed disparity reflects procedural capture or legitimate resource-based influence').

omega_variable(
    consensus_call_timing_artifact,
    'Are consensus calls (typically 2-week periods) temporally optimized for continuous participation by well-resourced teams, disadvantaging asynchronous-mode participants and distributed timezones?',
    'Historical analysis of consensus call timing patterns; correlation between call timing and documented objections raised late in window; comparison of objection rates by participant region/timezone; A/B analysis of extended consensus windows (4+ weeks).',
    'If timing artifact: suppression is partly procedurally engineered (higher confidence in capture narrative). If temporal-neutral: barriers are resource-based rather than procedurally designed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_call_timing_artifact, empirical, 'Whether consensus call windows are structurally biased toward continuous-mode participation').

omega_variable(
    working_group_chair_discretion_scope,
    'How much actual power do working group chairs have to interpret consensus and dismiss objections? Does the formal authority match informal practice?',
    'Procedural audit: documented chair decisions to set aside objections; cases where chair interpretation was challenged and outcomes; comparison across chairs of decision patterns; analysis of IESG appeals of chair rulings.',
    'If discretion is high and exercised asymmetrically: procedural safeguards are theater (high theater_ratio justified). If discretion is constrained: procedural safeguards are functional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(working_group_chair_discretion_scope, empirical, 'Actual scope of working group chair authority to interpret and override consensus').

omega_variable(
    legitimacy_commons_degradation_mechanism,
    'Is the victim here the consensus mechanism''s credibility itself, or downstream technical standards that carry corrupted legitimacy? Are we extracting from the procedure or from standards-dependent users?',
    'Tracing: when well-resourced factions capture a standard, do downstream users of that standard experience extraction (they adopted a standard based on false legitimacy claims)? Or does the extraction occur at the working group level (participants'' time and trust are extracted to produce predetermined outcomes)?',
    'If procedure-level extraction: victim is consensus mechanism credibility (this reading''s framing). If downstream-user extraction: constraint should decompose into separate story about standards capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_commons_degradation_mechanism, conceptual, 'Whether extracted resource is procedural legitimacy or downstream standards-user value').

omega_variable(
    kernel_reading_ambiguity,
    'Is the IETF openness commitment a natural-law-like requirement (rough consensus is the only way to coordinate distributed technical community at scale) or a contingent institutional choice that could be replaced?',
    'Comparative institutional analysis: do other standards bodies (ISO, ITU, proprietary alliances) achieve comparable technical outcomes with different governance? If so, commitment is contingent. Historical analysis: what alternatives were considered and rejected when IETF procedures were established?',
    'If natural law: the constraint''s degradation is inevitable (mountain framing). If contingent: alternatives exist and the commitment is being defended through legitimacy extraction (this reading''s framing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether rough consensus is necessary requirement or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_leg_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ietf_leg_tr_t5, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(ietf_leg_tr_t10, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(ietf_leg_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ietf_leg_be_t5, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ietf_leg_be_t10, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ietf_leg_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ietf_leg_su_t5, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(ietf_leg_su_t10, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, internet_standard_technical_quality_erosion).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, developing_nation_technological_sovereignty).

% DUAL FORMULATION NOTE:
% The ietf_openness_commitment kernel decomposes into three constraint readings (stories): (1) commons_stewardship_reading — focuses on recovery and active stewardship (different epsilon), (2) legitimacy_erosion_reading (this story) — focuses on mechanism degradation through resource asymmetry, (3) capture_substrate_reading — focuses on structural inevitability of capture. Each reading has different base_extractiveness, different victim/beneficiary structure, and different terminal classification possibilities. All three are linked via network.affects_constraints. The epsilon values differ because each reading measures a different dimension: stewardship reading measures repair potential (lower epsilon), erosion reading measures current asymmetry (moderate epsilon), substrate reading measures inevitability (high epsilon). This is not a measurement-basis ambiguity (ε-invariance violation) but genuine structural differentiation of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ietf_openness_commitment__legitimacy_erosion_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
