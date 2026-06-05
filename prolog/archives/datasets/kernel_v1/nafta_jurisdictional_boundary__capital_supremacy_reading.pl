% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: Trade Agreement Text as Supreme Law: Capital Mobility Supremacy Reading
 *   domain: international_trade_law/regulatory_federalism/political_economy
 *
 * SUMMARY:
 *   The North American Free Trade Agreement (NAFTA), and its successor USMCA,
 *   encode a specific reading of the jurisdictional boundary between domestic
 *   regulatory authority and international trade supremacy: capital mobility,
 *   investor protections, and regulatory harmonization are mandatory treaty
 *   obligations that override domestic labor and environmental standards.
 *   This constraint story models ONE READING of a contested kernel — the
 *   capital-supremacy reading. The reading instantiates a specific hierarchy:
 *   trade agreement text sits above domestic statutory law in the
 *   interpretation order; regulatory agencies must treat treaty compliance as
 *   a binding constraint on domestic policymaking; labor standards and
 *   environmental regulations that diverge from the negotiated baseline face
 *   investor-state challenge and damages liability. From this reading's
 *   perspective, NAFTA solved a coordination problem (predictable regulatory
 *   environment for capital) but embedded asymmetric extraction: labor
 *   organizing capacity and environmental jurisdiction are structurally
 *   subordinated to capital mobility rights. The constraint exhibits the full
 *   Deferential Realism spectrum: domestic regulators see snare (trapped
 *   without exit), capital sees rope (coordination enabling arbitrage), labor
 *   sees snare (extraction without escape), and the trade bureaucracy sees
 *   piton (performative dispute settlement). The analytical observer risks
 *   naturalizing this political-economic choice as globalization
 *   inevitability — a false mountain. The three readings (capital-supremacy,
 *   embedded-liberalism, sovereignty-primacy) coexist across different
 *   institutional actors and jurisdictions; none has been logically
 *   foreclosed, though capital-supremacy has become institutionally dominant
 *   through ISDS case law and subsequent treaty iterations.
 *
 * KEY AGENTS:
 *   - Transnational Capital (multinational corporations, financial services): Primary beneficiary (institutional/arbitrage) — gains enforceable regulatory stability, investor protections, cross-border capital mobility. Treaty supremacy is experienced as enabling coordination.
 *   - Domestic Labor Standards (labor departments, labor organizing): Primary victim (powerless/trapped at national scope, moderate/constrained at organized level) — standards floor is set by treaty; upward movement triggers investor-state suits; capital exit threat constrains domestic negotiating power.
 *   - Environmental Regulation (environmental agencies, environmental movements): Primary victim (moderate/constrained for agencies, organized/constrained for movements) — regulatory jurisdiction is subordinate to capital mobility; standards harmonize downward when capital threatens reallocation.
 *   - Trade Bureaucracy (trade negotiators, dispute settlement panels): Institutional secondary actor (institutional/arbitrage) — maintains treaty interpretation infrastructure; experiences dispute settlement as performative (theater ratio increasing over interval).
 *   - Domestic Legislatures (national parliaments, congresses): Constrained institutional actor — formally sovereign but constitutionally bound by treaty ratification; post-ratification legislative authority over labor/environmental standards is ambiguous (this ambiguity is omega variable territory).
 *   - Analytical Observer: Risks naturalizing political-economic choice as inevitable constraint (false mountain).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.62).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.68).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "Trade Agreement Text as Supreme Law: Capital Mobility Supremacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/regulatory_federalism/political_economy").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '60dab5d2-5ec7-406d-adb8-8d343570f0d5').
narrative_ontology:cs_kernel_codification('60dab5d2-5ec7-406d-adb8-8d343570f0d5', fixed_text).
narrative_ontology:cs_authority_grounding('60dab5d2-5ec7-406d-adb8-8d343570f0d5', extraction).
narrative_ontology:cs_interpretation_layer_present('60dab5d2-5ec7-406d-adb8-8d343570f0d5').
narrative_ontology:cs_reading_relation('60dab5d2-5ec7-406d-adb8-8d343570f0d5', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('60dab5d2-5ec7-406d-adb8-8d343570f0d5', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('60dab5d2-5ec7-406d-adb8-8d343570f0d5', foundational, capital_mobility_is_paramount_value).
narrative_ontology:cs_axiom_status(capital_mobility_is_paramount_value, holdable).
narrative_ontology:cs_axiom_grounding('60dab5d2-5ec7-406d-adb8-8d343570f0d5', capital_mobility_is_paramount_value, instrumental).
narrative_ontology:cs_axiom('60dab5d2-5ec7-406d-adb8-8d343570f0d5', secondary, regulatory_harmonization_downward_is_efficient).
narrative_ontology:cs_axiom_status(regulatory_harmonization_downward_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('60dab5d2-5ec7-406d-adb8-8d343570f0d5', regulatory_harmonization_downward_is_efficient, empirically_contingent).
narrative_ontology:cs_reference_frame('60dab5d2-5ec7-406d-adb8-8d343570f0d5', capital_mobility_supremacy).
narrative_ontology:cs_drift_state('60dab5d2-5ec7-406d-adb8-8d343570f0d5', contemporary_post_usmca, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60dab5d2-5ec7-406d-adb8-8d343570f0d5', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, transnational_capital).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, financial_services_sector).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_regulation).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, local_regulatory_authority).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, food_safety_jurisdiction).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, labor_organizing_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC REGULATORY AUTHORITY (SNARE) — National labor departments, environmental agencies, and food safety regulators face treaty obligations that override their statutory jurisdiction. Cannot exit without treaty violation; cannot modify standards without investor-state complaint. Maximum extraction: regulatory agencies are bound by text they did not negotiate and cannot unilaterally change. Trapped by international law hierarchy.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__capital_supremacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR ORGANIZING MOVEMENTS (SNARE) — Constrained by treaty prohibition on labor standard divergence below trade agreement baseline. Organizing campaigns to raise standards face investor-state suit risk; capital exits to lowest-standard jurisdictions; regulatory harmonization moves downward, not upward. High extraction: labor loses leverage to negotiate standards improvement; cost of collective action rises due to cross-border retaliation risk.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__capital_supremacy_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRANSNATIONAL CAPITAL (ROPE) — Benefits from treaty supremacy as coordination mechanism: capital can plan operations across jurisdictions with confidence that local regulation will not depart from negotiated baseline. Experiences the constraint as enabling — reduced regulatory uncertainty, enforceable standards floor (binding on labor, binding on environmental movements). Net beneficiary with full exit option (arbitrage to favorable jurisdictions). This perspective experiences the constraint as pure coordination.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__capital_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMESTIC ENVIRONMENTAL MOVEMENTS (TANGLED ROPE) — Organized agents can coordinate domestically but face treaty constraints on jurisdiction. Some genuine coordination occurs: harmonized environmental standards do reduce transnational pollution arbitrage. But the constraint also embeds asymmetric extraction: standards harmonize downward when capital threatens exit, and investor-state suits chill domestic environmental innovation. Mixed experience: real coordination benefit for cross-border pollution control; real extraction through regulatory ceiling effect.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CAPTURED REGULATORY AGENCY (TANGLED ROPE) — Some agencies (those aligned with capital interests) experience genuine coordination: the treaty standardizes and stabilizes regulatory environment, enabling predictable licensing and compliance regimes. But the agency is also constrained by treaty hierarchy; domestic legislative intent can be overridden by investor-state interpretation. Constrained exit: the agency could theoretically revise standards but faces treaty-violation cost. Mixed: coordination benefit for efficiency, extraction cost for autonomy.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADE BUREAUCRACY (PITON) — The dispute settlement mechanism is substantially performative: investor-state arbitration reviews regulatory compliance with treaty text but cannot force direct policy change (only damages). The system produces theater: regulatory agencies conduct compliance reviews, file arbitration briefs, negotiate settlements — vast administrative effort with marginal actual constraint on capital movement (which operates through market discipline, not law). Theater ratio high because the formal dispute mechanism is largely symbolic; the real extraction mechanism is market threat, not treaty enforcement.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__capital_supremacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GLOBALIZATION INEVITABILITY (MOUNTAIN) — From a global/civilizational horizon, capital mobility is treated as a natural law: governments that regulate capital above market rates face disinvestment as an inevitability, not a choice. Trade agreements codify this as immutable constraint. However, this perspective naturalizes a political-economic choice (capital supremacy in treaty language) as a law of nature. The analytical observer instantiates the oracle gap: their framework prevents seeing that the treaty hierarchy is authored, not discovered.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__capital_supremacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__capital_supremacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__capital_supremacy_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, TR),
    TR >= 0.70.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint extracts from labor and environmental jurisdiction to capital mobility beneficiaries. The extraction is not maximal (snare-level ≥0.66) because capital's benefit is real coordination, not pure rent-seeking, and the constraint does solve the problem of regulatory uncertainty. The trajectory rising from 0.42 to 0.62 reflects ISDS case law expanding investor-state protections beyond negotiated text and regulatory harmonization accelerating downward as capital exit threats mount. Suppression (0.68): High. Domestic regulatory agencies face binding constraint from treaty text they did not negotiate; ISDS damages create cost for non-compliance; capital exit threat constrains political feasibility of upward standard-setting; labor organizing faces cross-border retaliation risk. Suppression is not absolute (not 0.85+) because domestic political movements can still mobilize, and some jurisdictions have resisted capital discipline. The trajectory from 0.45 to 0.68 reflects increasing capital mobility over the interval, making the exit threat more credible and suppression more effective. Theater ratio (0.55): Moderate-high, increasing. The dispute settlement mechanism produces extensive administrative performance (regulatory reviews, arbitration briefs, settlement negotiations) but the actual constraint operates through market discipline (capital exit threat), not legal enforcement. As ISDS case law accumulated, agencies learned to pre-emptively harmonize downward rather than defend standards in arbitration, making the performative burden heavier while the legal mechanism became less frequently invoked. The rising trajectory (0.35 to 0.55) reflects institutionalization of preventive compliance theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. Transnational capital experiences it as rope — a coordination mechanism enabling predictable operations across borders. Domestic regulators experience snare — a binding obligation they cannot exit without treaty violation. Labor organizing sees snare with additional suppression layer (cross-border retaliation risk). Environmental movements see tangled rope — some genuine cross-border pollution control coordination, but embedded extraction through regulatory ceiling effect. The trade bureaucracy sees piton — the dispute settlement machinery is increasingly performative as agencies learn preventive compliance. The analytical observer risks seeing mountain (globalization as inevitable) — this perspective instantiates the oracle gap; the framework's native instruments (viewing capital mobility as a discovered law rather than a negotiated choice) prevent recognition that this constraint is authored, not natural. The perspectival gap reveals that the constraint's entire structure rests on a contested kernel: whether trade supremacy was negotiated, is binding, and should be preserved.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this constraint operates through the treaty hierarchy and capital exit threat credibility. Transnational capital (beneficiary, arbitrage exit) derives low d (~0.15) from beneficiary status + arbitrage exit → low experienced extractiveness (χ ≈ 0.15 × capital_scope). Domestic labor (victim, trapped/constrained exit) derives high d (~0.85-0.95) from victim status + trapped exit → high experienced extractiveness (χ ≈ 0.95 × labor_scope). Environmental movements (victim, constrained exit with organizing capacity) derive moderate-high d (~0.65-0.75) reflecting the dual experience of extraction through regulatory ceiling but coordination benefit through harmonization. The constraining effect is visible in the cross-institutional comparison: institutional beneficiaries (trade bureaucracy, capital-aligned agencies) experience the constraint differently from institutional victims (labor departments, environmental agencies), despite the same treaty text. The directionality derivation captures this through exit_options differentiation: beneficiaries have arbitrage exit (can relocate to favorable jurisdiction); victims have constrained or trapped exit (cannot exit without political cost or treaty violation). No directionality overrides are needed; the standard derivation chain handles the institutional differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by instantiating one specific reading of a contested kernel. The mandatrophy question is not 'is this extraction coordination or pure extraction?' but 'which reading of the treaty's purpose is correct?' The capital-supremacy reading interprets NAFTA as a capital-mobility-enabling treaty (coordination purpose, extraction means); the embedded-liberalism reading interprets it as balancing capital rights with domestic regulatory discretion (coordination purpose, moderate extraction means); the sovereignty-primacy reading interprets it as preserving domestic authority while enabling trade (minimal extraction). Each reading is coherent and defensible from within its own institutional framework. The capital-supremacy reading dominates ISDS case law and subsequent treaty iterations, but the other readings remain live in domestic political discourse and some state practices. The mandatrophy is resolved by the committer axis: the kernel is contested, the readings coexist, and the analytical observer's task is to recognize that all three readings are structurally present — not to adjudicate which is 'correct'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_interpretation_discretion,
    'Does the treaty text constrain investor-state arbitration discretion, or does arbitral interpretation effectively author new meaning that was not negotiated?',
    'Longitudinal analysis of ISDS decisions: do 80%+ of awards cite treaty text directly vs. construing novel investor rights? Do subsequent disputes reach similar interpretations or diverge? Analysis of negotiation records vs. actual arbitral application.',
    'If text-constrained: the constraint is primarily what negotiators intended (capital-supremacy reading is valid). If arbitral discretion high: the constraint has drifted from negotiated meaning, and a competing reading (regulatory discretion reading) gains structural plausibility — extraction is occurring through interpretation, not negotiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_interpretation_discretion, empirical, 'Degree to which investor-state arbitration is constrained by treaty text vs. interpretive discretion').

omega_variable(
    capital_exit_threat_credibility,
    'How much of the regulatory harmonization downward occurs because of explicit treaty language vs. implicit capital exit threat?',
    'Counterfactual analysis: comparison of regulatory trajectories in NAFTA signatories vs. non-signatories with similar capital inflows; survey of regulatory agency staff on decision-driver attribution; case studies of standards-lowering decisions with documentation of capital-threat timing.',
    'If treaty text is primary driver: capital-supremacy reading correctly attributes extraction to negotiated supremacy. If capital exit threat is primary driver: the constraint''s real extraction mechanism is market discipline, not legal hierarchy — the treaty is secondary theater. Extraction magnitude would increase (less recoverable via legal challenge), but the mechanism attribution would shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_exit_threat_credibility, empirical, 'Relative causal weight of explicit treaty language vs. implicit capital exit threat in regulatory harmonization').

omega_variable(
    alternative_reading_structural_plausibility,
    'Can the treaty text coherently be read as embedding regulatory discretion at the member-state level (embedded-liberalism reading) or as protecting domestic sovereignty (sovereignty-primacy reading)?',
    'Textual analysis: does the treaty language contain sufficient ambiguity or carve-outs (precautionary principle, labor flexibility clauses, environmental side-agreements) to support alternative readings? ISDS case law: have arbitrators cited alternative readings as rejected positions or as coexisting interpretations? Negotiation records: were explicit sovereignty-protection proposals submitted and explicitly rejected, or did they fail to reach negotiation floor?',
    'If high alternative-reading structural plausibility: the kernel is genuinely contested, and the three readings coexist. If low plausibility: capital-supremacy reading is dominant, and sibling readings are aspirational rather than structural. Omega resolves whether this is a multiway kernel with three live readings or a dominated kernel where other readings are foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_structural_plausibility, conceptual, 'Structural plausibility of alternative treaty readings (embedded liberalism, sovereignty primacy)').

omega_variable(
    domestic_legislatures_post_ratification_authority,
    'After treaty ratification, can domestic legislatures enact labor/environmental standards without incurring treaty-violation liability?',
    'Legal analysis: for each signatory, do domestic courts hold that treaty supremacy constrains legislative authority, or do they preserve legislative discretion via interpretation? ISDS claims data: what fraction of suits against labor/environmental measures succeed (capital supremacy is enforced) vs. fail (domestic authority is upheld)?',
    'If legislatures retain authority: the constraint''s actual suppression is lower than ε=0.68 suggests — domestic political process can override treaty. If treaty supremacy is enforced: suppression is accurate and capital-supremacy reading is structurally dominant. This omega determines whether regulatory agencies are truly trapped or merely constrained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_legislatures_post_ratification_authority, empirical, 'Post-ratification authority of domestic legislatures to enact labor/environmental standards without treaty violation').

omega_variable(
    reading_committer_identity,
    'Which reading (capital-supremacy, embedded-liberalism, sovereignty-primacy) corresponds to the negotiating intent of the parties?',
    'Negotiation records analysis: what explicit positions did labor delegations, environmental groups, and capital representatives take during treaty drafting? Did they agree on the meaning of key provisions (property rights, regulatory harmonization), or did they finalize text with explicit disagreement unresolved? Post-ratification testimony: do negotiators claim the text meant what capital-supremacy reading asserts?',
    'If capital-supremacy reading matches negotiating intent: this is the binding reading, and others are post-hoc reframings. If intent is ambiguous or contested: the kernel is genuinely underdetermined, and all three readings are live. If intent favored sovereignty primacy but capital-supremacy reading dominates in practice: the constraint has drifted (authority_erosion), and the committer axis reveals a gap between what was agreed and what is enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_identity, empirical, 'Which reading corresponds to negotiating parties'' expressed intent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 1994, 2014).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_capital_theater_1994, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nafta_capital_theater_2004, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(nafta_capital_theater_2014, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(nafta_capital_extract_1994, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nafta_capital_extract_2004, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(nafta_capital_extract_2014, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(nafta_capital_suppress_1994, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(nafta_capital_suppress_2004, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(nafta_capital_suppress_2014, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, resource_allocation).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_dispute_settlement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, regulatory_harmonization_downward_spiral).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, labor_standards_floor_as_ceiling).

% DUAL FORMULATION NOTE:
% This constraint is the capital-supremacy reading of the nafta_jurisdictional_boundary kernel. The sibling readings (embedded-liberalism, sovereignty-primacy) are separate constraint stories with different ε values, beneficiary/victim structures, and extraction mechanisms. They coexist as live institutional readings held by different actors and jurisdictions. The constraint family is linked because each reading's institutional dominance affects the others' feasibility and the overall NAFTA/USMCA architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
