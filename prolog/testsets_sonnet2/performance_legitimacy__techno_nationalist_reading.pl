% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__techno_nationalist_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: performance_legitimacy__techno_nationalist_reading
 *   human_readable: Techno-Nationalist Reading of Performance Legitimacy — Strategic Self-Sufficiency Mandate
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested performance-legitimacy
 *   kernel: the techno-nationalist reading, in which the state's claim to
 *   rule is grounded in achieving technological self-sufficiency and
 *   dominance in strategic industries as the guarantor of national security
 *   and great-power status. Under this reading, the primary constraint is
 *   strategic-industry capability, not GDP growth, not qualitative
 *   efficiency, and not household welfare — those are the concerns of sibling
 *   readings (quantitative_growth_reading, qualitative_development_reading,
 *   livelihood_security_reading), each a separate constraint story with its
 *   own ε, beneficiaries, and victims. This story's ε (0.68) refers to the
 *   standing arrangement of directed strategic-sector investment as this
 *   reading itself frames and defends it — not to a hypothetical fully
 *   self-sufficient end-state, and not averaged against the sibling readings.
 *
 * KEY AGENTS:
 *   - party_state_security_apparatus: agenda_setter — designates strategic sectors and directs capital/credit toward them
 *   - national_champion_firms: primary beneficiary — receives concentrated subsidy and protection
 *   - defense_adjacent_tech_sector: beneficiary — absorbs R&D funding and legitimacy narrative attention
 *   - consumer_goods_sector, unsubsidized_private_smes, provincial_fiscal_bases, ordinary_consumers: payers — bear crowded-out capital, distorted input costs, fiscal risk, and reduced consumption-good improvement
 *   - foreign_technology_suppliers: excluded — recast as security threats rather than trading partners
 *   - independent_economists: observer — measures capability gains against misallocation costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.62).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Reading of Performance Legitimacy — Strategic Self-Sufficiency Mandate").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, 'd866c583-6b0d-4a65-bfac-e1ca2c12c66f').
narrative_ontology:cs_kernel_codification('d866c583-6b0d-4a65-bfac-e1ca2c12c66f', distributed).
narrative_ontology:cs_authority_grounding('d866c583-6b0d-4a65-bfac-e1ca2c12c66f', extraction).
narrative_ontology:cs_interpretation_layer_present('d866c583-6b0d-4a65-bfac-e1ca2c12c66f').
narrative_ontology:cs_reading_relation('d866c583-6b0d-4a65-bfac-e1ca2c12c66f', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('d866c583-6b0d-4a65-bfac-e1ca2c12c66f', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('d866c583-6b0d-4a65-bfac-e1ca2c12c66f', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('d866c583-6b0d-4a65-bfac-e1ca2c12c66f', foundational, technological_sovereignty_is_precondition_for_security).
narrative_ontology:cs_axiom_status(technological_sovereignty_is_precondition_for_security, holdable).
narrative_ontology:cs_axiom_grounding('d866c583-6b0d-4a65-bfac-e1ca2c12c66f', technological_sovereignty_is_precondition_for_security, empirically_contingent).
narrative_ontology:cs_axiom('d866c583-6b0d-4a65-bfac-e1ca2c12c66f', foundational, market_allocation_subordinate_to_strategic_designation).
narrative_ontology:cs_axiom_status(market_allocation_subordinate_to_strategic_designation, holdable).
narrative_ontology:cs_axiom_grounding('d866c583-6b0d-4a65-bfac-e1ca2c12c66f', market_allocation_subordinate_to_strategic_designation, instrumental).
narrative_ontology:cs_reference_frame('d866c583-6b0d-4a65-bfac-e1ca2c12c66f', chokepoint_vulnerability_baseline).
narrative_ontology:cs_drift_state('d866c583-6b0d-4a65-bfac-e1ca2c12c66f', post_export_control_escalation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d866c583-6b0d-4a65-bfac-e1ca2c12c66f', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champion_firms).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sector).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, party_state_security_apparatus).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_goods_sector).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, unsubsidized_private_smes).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, provincial_fiscal_bases).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, ordinary_consumers).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, great_power_status_requires_technological_sovereignty).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, market_allocation_is_insufficient_for_strategic_security).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines which industries count as 'strategic' (semiconductors, AI, aerospace, advanced materials), directs state banks and industrial funds toward them, and frames the entire legitimacy question around whether the country can no longer be choked off by rival powers. Sets export-control retaliation policy and administers the entire directed-investment architecture.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, party_state_security_apparatus, agenda_setter,
    institutional, civilizational, analytical, national).

% Receive concentrated subsidy, cheap state-directed credit, guaranteed procurement, and protection from foreign competition in exchange for delivering visible technological milestones. Can lobby for reclassification of their sector as strategic and can exit into international markets even as domestic rivals are starved of capital.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champion_firms, beneficiary,
    organized, generational, arbitrage, global).

% Absorbs the majority of directed R&D funding and talent pipelines; benefits from being defined into the legitimacy narrative itself. Constrained in that its funding and mandate are tied entirely to continued state prioritization — a shift in kernel reading would strand its investments.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sector, beneficiary,
    organized, generational, constrained, national).

% Competes for capital, land, and skilled labor against strategic-sector allocations that are shielded from market discipline; sees credit tightened and land/energy costs distorted upward as inputs are redirected. Cannot easily relocate the affected businesses or challenge the reallocation since it is framed as a matter of national security rather than economic policy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_goods_sector, payer,
    moderate, biographical, constrained, national).

% Small and medium enterprises outside the designated strategic categories are crowded out of bank credit, denied preferential land and tax treatment, and structurally disadvantaged relative to state-favored 'national champions.' Most have no path to relabel themselves as strategic and no exit beyond failure or absorption.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, unsubsidized_private_smes, payer,
    powerless, biographical, trapped, regional).

% Local governments are pressured to fund or co-finance strategic-industry projects (fabs, industrial parks, subsidized loans) regardless of local comparative advantage, often accumulating debt for projects serving central strategic goals rather than local development needs. Bear downstream fiscal risk if projects fail to reach self-sufficiency targets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, provincial_fiscal_bases, payer,
    moderate, generational, constrained, regional).

% Experience higher prices, narrower consumer choice, and slower improvement in living-standard goods as investment, subsidy, and skilled labor are redirected toward strategic sectors instead of consumption-facing industries. Have no institutional channel to contest the reallocation since it is legitimated as a security imperative, not an economic tradeoff subject to consumer welfare review.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, ordinary_consumers, payer,
    powerless, biographical, trapped, national).

% Would argue that market-driven trade and specialization deliver more efficient technology diffusion than autarkic self-sufficiency drives, but their perspective is structurally excluded from the domestic legitimacy conversation — they are recast as strategic threats rather than potential partners, foreclosing any voice in the reallocation decision.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, foreign_technology_suppliers, excluded,
    powerful, biographical, constrained, global).

% Study whether directed strategic-sector investment is producing genuine capability gains or overcapacity and fiscal strain, comparing self-sufficiency claims against realized output, import-substitution ratios, and misallocation costs across the broader economy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, independent_economists, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates scarce capital, talent, and policy attention on a narrow set of industries judged existential to national security, in principle solving a genuine collective-action problem: no individual firm or region would rationally bear the sunk cost of building an indigenous semiconductor or aerospace base against entrenched foreign incumbents without state coordination.
% TRANSFER_FUNCTION: Moves capital, subsidized credit, land, energy allocations, and skilled labor away from consumer-facing and unsubsidized private industry and toward designated strategic-sector firms and the state apparatus that administers the designation, in the name of security rather than market return.
% ABSENT_VOICES: Foreign technology suppliers and trade partners who would argue for continued specialization and exchange are recast as adversaries and excluded from the domestic legitimacy conversation entirely; unsubsidized private smes have no lobbying channel comparable to national champions and are rarely represented in the discourse defining what counts as 'strategic.'
% DISAPPEARANCE_RATIONALE: If the techno-nationalist legitimacy frame were abandoned overnight, directed industrial funds would lose their justification, subsidized credit lines to national champions would come under market-return scrutiny, provincial co-financing obligations for strategic projects would be renegotiated, and capital, land, and labor would flow back toward consumer-facing and export-competitive private sectors — a substantial reallocation, not a cosmetic one.
% FOUNDING_PROBLEM: Perceived vulnerability to foreign export controls and technology chokepoints (semiconductors, critical software, advanced manufacturing equipment) that could paralyze the economy or military in a crisis, combined with a status anxiety that great-power standing requires indigenous technological capability rather than dependence on rivals.
% FOUNDING_PROBLEM_CORROBORATION: The security apparatus and national champion firms attest the chokepoint threat is acute and growing, citing actual export-control episodes. Independent economists and some provincial fiscal officials, situated outside the beneficiary set, corroborate that real chokepoint vulnerabilities exist in specific sub-sectors (advanced lithography, certain EDA tools) but argue the blanket strategic-sector designation has expanded well past those narrow chokepoints into a much broader industrial-policy program whose security rationale is now doing legitimating work beyond what the underlying vulnerability supports.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__techno_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__techno_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 (substantial but not maximal) because the reading does capture a genuine coordination problem — indigenous strategic capability against real chokepoint vulnerabilities is not manufactured from nothing — but the observed policy scope has expanded well beyond the narrow chokepoints independent economists corroborate, redirecting capital and fiscal capacity toward a much broader 'strategic' category that shields designated firms from market discipline. Suppression (0.62) reflects that dissent from the reallocation is difficult to voice once framed as a security matter rather than an economic tradeoff — provincial officials and SME representatives who might object have no comparable institutional standing to national champions. Theater ratio is comparatively low (0.28) because much of the strategic-sector investment is producing real, measurable industrial capability, not merely performative announcements — though the ratio is rising as milestone-driven announcements begin to outpace verifiable capability gains in some sub-sectors.
 *
 * DIRECTIONALITY LOGIC:
 *   The party-state security apparatus and national champion firms sit at the beneficiary end: the apparatus sets and administers the designation criteria, and champion firms receive concentrated capital, protection, and legitimacy-narrative centrality without bearing the crowding-out costs. Consumer goods firms, unsubsidized SMEs, provincial fiscal bases, and ordinary consumers sit at the target end: they bear the opportunity cost of redirected capital, land, energy, and skilled labor, and — critically — cannot contest the reallocation through ordinary economic-policy channels because it is framed as a security imperative rather than a market tradeoff. Foreign technology suppliers, who might argue for continued specialization and exchange, are structurally excluded from the domestic conversation entirely: their exclusion is not incidental but constitutive of the reading itself, since a self-sufficiency legitimacy claim requires treating external suppliers as threats to be exited from, not partners to coordinate with.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — vulnerability to specific technology chokepoints — remains genuinely live in narrow sub-sectors (advanced lithography, certain EDA tools), which is why this reading is authored as tangled_rope rather than snare: there IS a real coordination function underneath the extraction. But the founding-problem status is marked contested because the scope of designated 'strategic' sectors has grown substantially past the sub-sectors where the chokepoint vulnerability is independently corroborated, meaning a growing share of the directed investment is legitimacy-narrative maintenance for national-champion rent extraction rather than security-driven coordination. This is precisely the divergence the classification exists to surface: a reading that claims security necessity while its metrics show rising extraction and suppression over time (T17-relevant accumulation pattern) without a correspondingly narrow, verifiable target set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chokepoint_scope_versus_designated_scope,
    'How much of the designated ''strategic sector'' investment addresses genuinely narrow, independently-verifiable chokepoint vulnerabilities, versus how much has expanded the designation to capture rent-seeking opportunities for politically connected national champions?',
    'Independent technical audit comparing the list of chokepoint-critical technologies (as assessed by outside engineering/trade experts) against the full list of firms and sectors receiving strategic-designation subsidy and protection; measure the ratio of narrowly-justified to broadly-justified allocation.',
    'A narrow gap would support this reading''s own self-understanding as primarily coordination with modest extraction riding on top; a wide gap would indicate the security framing has become substantially a cover story for concentrated industrial rent-seeking, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chokepoint_scope_versus_designated_scope, empirical, 'Whether strategic-sector designation tracks real chokepoint vulnerability or has expanded into general industrial favoritism.').

omega_variable(
    kernel_reading_incommensurability,
    'Is the techno-nationalist reading of performance legitimacy structurally compatible with the livelihood-security and quantitative-growth readings, or does sustained strategic-sector prioritization necessarily crowd out the resource base those readings depend on?',
    'Track whether periods of intensified strategic-sector directed investment correlate with measurable declines in consumer-sector growth rates, employment growth outside strategic sectors, and household-service spending — the sibling readings'' own success metrics.',
    'If the readings are structurally zero-sum over the relevant capital and fiscal base, this reading''s persistence actively degrades the empirical conditions the livelihood_security_reading and quantitative_growth_reading need to remain credible legitimacy claims, which would justify the ''influences'' (rather than mere ''coexists_with'') relation to those siblings and predicts intensifying kernel contest over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the techno-nationalist reading''s resource claims structurally undercut the livelihood and growth readings it nominally coexists with.').

omega_variable(
    security_status_as_terminal_or_instrumental_value,
    'Is great-power technological status treated by the security apparatus as an end valuable in itself (independent of the security payoff), or purely instrumentally as a means to verifiable security guarantees?',
    'Examine whether strategic-sector investment continues into domains where independent chokepoint risk has been resolved (e.g., successful domestic substitution achieved) — continuation past resolved risk indicates status has become terminal rather than instrumental.',
    'If status-seeking is terminal, the coordination function has no natural stopping point and the arrangement will structurally resist any sunset condition, supporting a harder tangled_rope (or drifting toward snare) reading over the long run; if purely instrumental, resolved-risk domains should see de-escalation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_status_as_terminal_or_instrumental_value, conceptual, 'Whether great-power status functions as a terminal legitimacy value independent of the security rationale that originally justified it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__techno_nationalist_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__techno_nationalist_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__techno_nationalist_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__techno_nationalist_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__techno_nationalist_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__techno_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the performance_legitimacy kernel, decomposed per the ε-invariance principle: quantitative_growth_reading, qualitative_development_reading, livelihood_security_reading, and this techno_nationalist_reading each instantiate a distinct constraint with its own ε, beneficiary/victim structure, and classification, because the natural-language concept 'performance legitimacy' covers structurally distinct legitimacy claims that would yield different ε values if collapsed into one story. This story's beneficiaries (national champions, defense-adjacent tech, the security apparatus) and victims (consumer sectors, unsubsidized SMEs, provincial fiscal bases, ordinary consumers) are specific to the strategic-industry framing and do not carry over to the sibling readings, whose beneficiary/victim structures differ (e.g., livelihood_security_reading's victims would be fiscal actors resisting welfare-spending expansion, not consumer-sector firms crowded out of credit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
