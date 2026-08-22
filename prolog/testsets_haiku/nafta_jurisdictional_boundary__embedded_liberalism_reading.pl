% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA Jurisdictional Boundary: Embedded Liberalism Reading
 *   domain: international_trade_law/regulatory_federalism
 *
 * SUMMARY:
 *   The NAFTA (now USMCA) trade agreement establishes a framework that
 *   nominally permits each signatory to set environmental and labor standards
 *   provided they are non-discriminatory and serve legitimate objectives.
 *   This reading interprets the constraint as embedded liberalism: market
 *   access is the primary commitment; domestic regulatory autonomy is
 *   preserved within a defined exception. The framework creates a partial
 *   jurisdictional overlap where dispute settlement panels interpret the
 *   boundary between legitimate regulation and disguised protection. This
 *   reading differs from the capital_supremacy reading (which treats trade
 *   obligations as supreme) and the sovereignty_primacy reading (which treats
 *   trade as subordinate). The embedded liberalism reading centers the
 *   tension itself as the constraint: regulatory agencies retain formal
 *   authority but face continuous litigation cost and implicit harmonization
 *   pressure. The claim/metric gap is deliberate and structural: the
 *   constraint is claimed as tangled_rope (genuine coordination on market
 *   access, asymmetric extraction through litigation defense) while the
 *   authored metrics describe moderate extraction and suppression — the
 *   engine measures this political reading's structural dynamics.
 *
 * KEY AGENTS:
 *   - Multinational exporters: benefit from predictable, harmonized market access across signatories
 *   - Trade dispute settlement panels: agenda-setter interpreting the 'legitimate objectives' boundary
 *   - Domestic regulators (environmental/labor): payers defending each standard against litigation
 *   - Labor unions and environmental NGOs: payers defending precautionary standards identity-locked to their mission
 *   - Signatory states: nominally sovereign but face constrained exit (withdrawal politically/economically costly)
 *   - Capital mobility advocates: beneficiaries of downward regulatory pressure without direct control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.48).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.42).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary: Embedded Liberalism Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'b075d871-9bc2-4149-a238-9babfb27bc98').
narrative_ontology:cs_kernel_codification('b075d871-9bc2-4149-a238-9babfb27bc98', formalized).
narrative_ontology:cs_authority_grounding('b075d871-9bc2-4149-a238-9babfb27bc98', extraction).
narrative_ontology:cs_interpretation_layer_present('b075d871-9bc2-4149-a238-9babfb27bc98').
narrative_ontology:cs_reading_relation('b075d871-9bc2-4149-a238-9babfb27bc98', nafta_jurisdictional_boundary__capital_supremacy_reading, influences).
narrative_ontology:cs_reading_relation('b075d871-9bc2-4149-a238-9babfb27bc98', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('b075d871-9bc2-4149-a238-9babfb27bc98', foundational, legitimate_objectives_exception_preservable).
narrative_ontology:cs_axiom_status(legitimate_objectives_exception_preservable, holdable).
narrative_ontology:cs_axiom_grounding('b075d871-9bc2-4149-a238-9babfb27bc98', legitimate_objectives_exception_preservable, conventional).
narrative_ontology:cs_axiom('b075d871-9bc2-4149-a238-9babfb27bc98', foundational, regulatory_autonomy_compatible_with_trade_obligation).
narrative_ontology:cs_axiom_status(regulatory_autonomy_compatible_with_trade_obligation, holdable).
narrative_ontology:cs_axiom_grounding('b075d871-9bc2-4149-a238-9babfb27bc98', regulatory_autonomy_compatible_with_trade_obligation, deontological).
narrative_ontology:cs_reference_frame('b075d871-9bc2-4149-a238-9babfb27bc98', partial_jurisdictional_overlap_frame).
narrative_ontology:cs_drift_state('b075d871-9bc2-4149-a238-9babfb27bc98', contemporary_panel_precedent, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b075d871-9bc2-4149-a238-9babfb27bc98', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_exporters).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, harmonized_market_access_coalition).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulators).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_standards_defenders).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_protection_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_mobility_advocates).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, signatory_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large corporations with supply chains and markets across the three signatories benefit from predictable, non-discriminatory market access and regulatory harmonization. They gain competitive advantage when regulatory divergence is costly and are harmed by unilateral standard-setting that requires compliance redesign. They can exit by relocating production or shifting sourcing across signatories.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_exporters, beneficiary,
    institutional, generational, arbitrage, global).

% Trade dispute resolution panels, arbitration bodies, and their institutional infrastructure (legal counsel, expert witnesses, procedural custodians) benefit from their enhanced authority to interpret the jurisdictional boundary. The litigation generates caseload, interpretive power, and institutional expansion. They gain prestige and professional opportunity from serving as authoritative interpreters of the 'legitimate objectives' exception.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, harmonized_market_access_coalition, beneficiary,
    institutional, generational, mobile, global).

% Environmental agencies, labor departments, and health ministries must defend every new or strengthened standard by proving non-discrimination and legitimate-objective status in dispute proceedings. They retain nominal regulatory authority but pay continuously in litigation costs, expert testimony, precedent risk, and implicit pressure to avoid standards likely to trigger challenge. They cannot exit without their country withdrawing from the agreement.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulators, payer,
    institutional, biographical, constrained, national).

% Trade unions and labor-rights organizations must continuously litigate in dispute panels to argue that minimum-wage, workplace-safety, and collective-bargaining standards qualify as legitimate objectives. They face low win rates and fight defensively against erosion. Their organizational identity is constituted through labor protection; exit from defending standards-within-trade-framework would dissolve their core mission.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_standards_defenders, payer,
    organized, biographical, identity_locked, national).

% Environmental NGOs and conservation coalitions must defend precautionary environmental standards (emissions limits, habitat protections, chemical bans) in trade dispute proceedings by framing them as legitimate objectives. They litigate with limited leverage and face investor-backed challenges. Their organizational identity is fused with environmental protection; they cannot exit the framework without abandoning their core mission.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_protection_advocates, payer,
    organized, generational, identity_locked, national).

% Financial investors and multinational capital coalitions benefit indirectly from regulatory harmonization and the implicit pressure the dispute mechanism places on states to weaken protective standards to avoid litigation costs. They do not directly control the mechanism but benefit from its effect of lowering the political willingness to regulate.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_mobility_advocates, beneficiary,
    powerful, generational, arbitrage, global).

% Dispute settlement bodies interpret what 'non-discrimination' and 'legitimate objectives' mean in operational terms. Their rulings accumulate into precedent that defines the actual jurisdictional boundary. They operate within the treaty text but their interpretive authority determines the scope of domestic regulatory autonomy in practice. They are constrained by the treaty language but have substantial discretion in applying its general principles.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_dispute_panels, agenda_setter,
    institutional, generational, analytical, continental).

% Canada, Mexico, and the United States nominally retain sovereignty to set environmental and labor standards within the 'legitimate objectives' exception. In practice, they face asymmetric costs for regulatory action: each new standard triggers potential challenge; defense is expensive and uncertain; withdrawal from the agreement is politically costly and economically harmful. They choose between accepting regulatory constraint and bearing the cost of unilateral action.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, signatory_states, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, signatory_states, payer).

% State, provincial, and municipal regulators (California, Quebec, Oaxaca) inherit the embedded liberalism compromise without negotiating voice. When they attempt stronger environmental or labor standards, they face litigation challenge at the federal level because federal governments must defend all domestic regulatory choices. They cannot exit except by relocating their jurisdiction.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, subnational_regulators, excluded,
    powerless, immediate, trapped, local).

% Mexico's regulatory authorities face asymmetric burden of defending standards because litigation capacity is unevenly distributed (US and Canadian firms have superior legal resources). They inherit the framework's constraints on unilateral protection of domestic industry. They cannot exit without their country withdrawing; withdrawal carries greater relative cost for Mexico than for the US.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, developing_country_regulators, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__embedded_liberalism_reading, harmonized_market_access_coalition).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__embedded_liberalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates non-discriminatory market access across three signatories and establishes transparent dispute procedures to prevent regulatory beggar-thy-neighbor competition (states undercutting each other's standards to attract capital). Provides predictability for multinational firms and reduces defection incentives.
% TRANSFER_FUNCTION: Moves regulatory authority from national legislatures toward dispute settlement panels; moves compliance costs from multinationals (harmonization simplifies design) toward domestic regulators (litigation defense) and organized labor/environmental advocates (defending standards in dispute proceedings); moves implicit regulatory pressure downward (states avoid standards likely to trigger costly challenge).
% ABSENT_VOICES: Subnational regulators (states, provinces, municipalities) that would defend stronger standards have no seat at negotiation and must comply with federal treaty commitments. Workers in protected industries and environmental advocates depending on protective standards have no formal role in standard-setting and fight defensively only in dispute procedures. Developing-country regulators depending on protection of domestic industry for employment and tax base are excluded from negotiating the framework's terms.
% DISAPPEARANCE_RATIONALE: If the constraint vanished (treaty withdrawal), signatory states would immediately reassert autonomous regulatory authority; multinational firms would face divergent compliance costs; capital would shift to jurisdictions offering greatest regulatory certainty (likely capital-friendly regimes); dispute settlement authority would disappear and be replaced by unilateral tariff or regulatory retaliation; labor and environmental standards would recover protected status against commerce-first pressure; and the coordination gain (transparent dispute mechanism, non-discriminatory access) would be lost.
% FOUNDING_PROBLEM: Post-Cold War North American economic integration required a binding framework preventing regulatory defection (each state undercutting others' standards to attract capital) while preserving legitimate regulatory space for environmental protection, labor standards, and public health — reconciling market access with democratic regulatory autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Trade negotiators and business coalitions attest the foundational problem (regulatory defection and uncertainty) is still live and the agreement solves it. Labor unions and environmental NGOs attest the problem is only partially solved — they defend standards through constant litigation but face erosion through precedent and implicit pressure. Academic economists provide evidence of regulatory convergence patterns (some upward, predominantly downward) but dispute whether the mechanism is defection-prevention (supporting the foundational problem reading) or capital-driven ratcheting (suggesting the founding problem is solved but the mechanism persists for extraction). The corroboration from outside benefiting parties is mixed and contested.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) not because the constraint is balanced, but because the embedded liberalism reading permits regulatory defense within a defined 'legitimate objectives' scope. The extraction is real — multinational firms gain predictable market access and regulatory harmonization; domestic regulators must litigate defensively — but the reading's interpretation preserves a formal exception. Suppression is moderate (0.42) because enforcement operates through litigation cost rather than outright prohibition; regulators can legally act but face asymmetric burden-of-proof once challenged. Theater is elevated (0.31) because dispute panels must theatrically reaffirm the 'legitimate objectives' exception in each ruling even as their decisions narrow its scope. The measurement series shows extractiveness accumulating (0.32 → 0.48) over the first 18 years as dispute precedent narrows the exception, then plateau (0.48 at 2024) as the precedent framework stabilizes. This trajectory reflects the constraint's operation: initial flexibility in the 'legitimate objectives' exception erodes as panel interpretation accumulates into binding precedent. Suppression and theater follow similar curves because they are driven by the same litigation-expansion mechanism. One shared time grid; every metric authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   From the multinational exporter and trade-panel seats, the constraint is genuine coordination solving collective-action problems of regulatory uncertainty and defection risk — the market access and predictability deliver real value. From the domestic-regulator and labor/environmental-advocate seats, the same structure operates as enforced extraction: formal regulatory authority persists, but its exercise triggers litigation costs and precedent erosion that push toward convergence downward. A domestic regulator defending a new environmental standard experiences the constraint as suppressive (litigation burden, precedent risk); a multinational exporter experiencing the same constraint reads it as enabling (market certainty, predictability premium). The divergence is structural, not merely perspectival: the beneficiaries and payers experience different effective power and different exit routes. The engine computes this divergence from the authored directionality and power-atom data.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational exporters are near the full-beneficiary end (d ≈ 0.15): the constraint subsidizes their operation through market-access certainty and harmonization pressure that reduces compliance complexity. Trade dispute panels are beneficiaries and agenda-setters (d ≈ 0.2): they gain authority and caseload from the litigation the constraint generates. Domestic regulators are near the target end (d ≈ 0.8): they bear litigation costs and face erosion of regulatory scope through precedent; their exit (treaty withdrawal) is constrained by economic interdependence. Labor and environmental advocates are at the target end (d ≈ 0.85): they must continuously litigate to defend standards and face identity-lock (their organizational mission is constituted through these standards); exit is unavailable. Capital-mobility advocates are moderate beneficiaries (d ≈ 0.3) because the downward regulatory pressure benefits them indirectly without their direct control or causal agency. The directionality reflects the asymmetry of exit options: exporters can arbitrage across jurisdictions; regulators cannot arbitrage treaty withdrawal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulatory defection risk in a post-Cold War integration context) remains contested. Trade negotiators attest it is live; labor and environmental advocates attest it is partly solved but the constraint's actual operation has shifted from preventing beggar-thy-neighbor toward enabling capital-driven ratcheting. This contest maps precisely onto the mandatrophy question: if the founding problem is dead (domestic regulators are not defecting downward; competition-on-standards is not a live pressure), then the constraint's persistence is not explained by its solving function and becomes functionally orphaned. The embedded liberalism reading preserves one bridging claim: that the 'legitimate objectives' exception is still meaningful, still protecting genuine regulatory space. But the measurement data (extractiveness accumulating 0.32 → 0.48 over 18 years) and the theater trajectory (rising share of enforcement activity devoted to defending the exception's boundary) suggest the exception is eroding through precedent. The classification (tangled_rope) holds as long as domestic regulators retain formal authority within the exception; if precedent narrows the exception to near-zero, the constraint would reclassify toward snare (no exception, pure trade-over-regulation hierarchy). The mandatrophy analysis shows the constraint as a candidate for eventual reclassification if the 'legitimate objectives' exception continues narrowing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_objectives_boundary_contestation,
    'What operational definition of ''legitimate objectives'' survives in dispute-panel precedent, and how much regulatory space does it actually preserve?',
    'Systematic audit of dispute panel decisions 1994–2024 quantifying rate of dismissal/acceptance of environmental, labor, health standards characterized as legitimate objectives. Compare early-panel acceptance rates with recent-panel rates to measure precedent narrowing.',
    'If the boundary has narrowed significantly (early acceptance rate >70%, recent acceptance rate <40%), the ''legitimate objectives'' exception is eroding and the constraint is functionally shifting from tangled_rope (exception-preserved) toward snare (exception-nullified). If the boundary is stable, the embedded liberalism reading''s framing is vindicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimate_objectives_boundary_contestation, empirical, 'The actual scope preservation of the ''legitimate objectives'' exception through case law.').

omega_variable(
    regulatory_ratcheting_mechanism,
    'Does the dispute mechanism operate primarily to prevent beggar-thy-neighbor downward competition on standards (solving the founding problem), or primarily to pressure harmonization downward even absent defection risk?',
    'Examine patterns of challenge and defense: are disputes initiated primarily when a state tightens standards (indicating defection concern), or when a state maintains or strengthens standards against investor pressure (indicating capital-supremacy dynamics)? Interview regulators and trade counsel on litigation-induced standard abandonment vs. avoidance.',
    'If disputes cluster around standard-tightening, the founding problem explanation is supported. If disputes cluster around standard-maintenance or standard-strengthening, the embedded liberalism reading''s framing obscures a capital-supremacy mechanism: the ''legitimate objectives'' exception is available in theory but rarely wins in practice because the burden of proof is prohibitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_ratcheting_mechanism, empirical, 'Whether the dispute mechanism solves its stated foundational problem or operates as capital-driven ratcheting.').

omega_variable(
    reading_foreclosure_risk,
    'Is the embedded liberalism reading logically sustainable if the ''legitimate objectives'' exception continues narrowing, or does extreme narrowing logically collapse the reading into the capital_supremacy_reading?',
    'Define a threshold (e.g., if panel acceptance rates drop below 20%, the exception is non-functional). If that threshold is crossed, assess whether the reading''s distinction from capital supremacy can be maintained or whether it has been logically foreclosed by its own precedent.',
    'If the exception erodes beyond functionality, the embedded liberalism reading becomes untenable; the constraint would be read more accurately as capital_supremacy. The three readings are not merely competing interpretations but potentially logically related: sovereignty_primacy forecloses both others; embedded liberalism coexists with sovereignty_primacy but forecloses capital_supremacy only if the exception preserves real space.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_risk, conceptual, 'Whether embedded liberalism can be sustained as a reading if its constitutive exception erodes.').

omega_variable(
    signatory_state_exit_asymmetry,
    'Is the political/economic cost of treaty withdrawal truly symmetric across the three signatories, or does asymmetry in economic dependence mean exit is more constrained for Mexico and Canada than the United States?',
    'Calculate export dependency ratios (trade-to-GDP) for each signatory; estimate adjustment costs of reversion to bilateral negotiation or WTO-only framework; survey legislative and executive costs of withdrawal proposal.',
    'If exit costs are asymmetric, the constraint''s suppression operates differentially: the United States retains a credible exit option, while Mexico and Canada face much higher costs. This would mean the constraint''s enforcement asymmetry is baked into the founding economic structure, not merely into the dispute mechanism. Smaller signatories would be more identity-locked to the agreement and face higher suppression, even though they nominally retain ''legitimate objectives'' exception. This would suggest the constraint''s actual operation favors the most-powerful signatory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(signatory_state_exit_asymmetry, empirical, 'Asymmetry in exit costs across signatories and its effect on actual regulatory autonomy.').

omega_variable(
    identity_lock_for_labor_advocates,
    'How much of labor unions'' and environmental NGOs'' measured suppression is structural (litigation costs, regulatory burden) vs. internalized (ideological commitment to protecting standards even when exit becomes economically viable)?',
    'Baseline: measure suppression while the constraint operates. Post-exit comparison: if a labor union or environmental coalition shifted to anti-trade positioning and abandoned defense of standards within the agreement framework, track their subsequent trajectory (do they mount standards via unilateral national advocacy? through alternative frameworks?). If suppression persists after framework exit, it is partially internalized.',
    'Internalized suppression is a marker of identity-lock: the advocate''s organizational identity is fused with defending the standards-protection framing, making exit ideologically unavailable even if economically available. This explains why labor and environmental organizations remain engaged in litigation defense despite low win rates — they cannot exit because exit would dissolve their identity. Understanding the identity-lock mechanism is critical to understanding the constraint''s persistence: the advocates remain in the fight not because they can win but because they cannot leave.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_for_labor_advocates, empirical, 'The proportion of labor/environmental suppression that is internalized vs. structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 1994, 0.18).
narrative_ontology:measurement_basis(naft_tr_t1994, projected).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(naft_tr_t2000, observed).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2006, 0.27).
narrative_ontology:measurement_basis(naft_tr_t2006, observed).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2012, 0.3).
narrative_ontology:measurement_basis(naft_tr_t2012, observed).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2018, 0.31).
narrative_ontology:measurement_basis(naft_tr_t2018, observed).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2024, 0.31).
narrative_ontology:measurement_basis(naft_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 1994, 0.32).
narrative_ontology:measurement_basis(naft_be_t1994, projected).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement_basis(naft_be_t2000, observed).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2006, 0.43).
narrative_ontology:measurement_basis(naft_be_t2006, observed).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2012, 0.47).
narrative_ontology:measurement_basis(naft_be_t2012, observed).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2018, 0.48).
narrative_ontology:measurement_basis(naft_be_t2018, observed).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2024, 0.48).
narrative_ontology:measurement_basis(naft_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 1994, 0.28).
narrative_ontology:measurement_basis(naft_su_t1994, projected).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement_basis(naft_su_t2000, observed).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2006, 0.39).
narrative_ontology:measurement_basis(naft_su_t2006, observed).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2012, 0.41).
narrative_ontology:measurement_basis(naft_su_t2012, observed).
narrative_ontology:measurement(naft_su_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement_basis(naft_su_t2018, observed).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement_basis(naft_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.18).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, investor_state_dispute_settlement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, regulatory_harmonization_pressure_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the nafta_jurisdictional_boundary kernel. Three structurally distinct constraint stories instantiate the three competing readings of the same treaty text. The embedded_liberalism_reading (this story) interprets the boundary as partial jurisdictional overlap with preserved 'legitimate objectives' exception. The capital_supremacy_reading reads the same text as establishing trade obligations as supreme law. The sovereignty_primacy_reading reads the same text as subordinating trade to sovereign authority. Each reading has its own epsilon (0.48 here vs. higher for capital_supremacy, lower for sovereignty_primacy), its own victim/beneficiary structure, and its own operational boundary definition. They are not alternative measurements of a single constraint — they are different constraints instantiated by different readings of a contested kernel. The network links indicate causal influence: all three readings are constrained by the same treaty text, and shifts in dispute-panel precedent (which reading becomes operationally binding) affect all three simultaneously. The capital_supremacy and sovereignty_primacy readings are siblings of this one; decomposition follows the ε-invariance principle (OQ-26): if measuring the constraint via one reading gives low ε and measuring via another gives high ε, the readings are different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
