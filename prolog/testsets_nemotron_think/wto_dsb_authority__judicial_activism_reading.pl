% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO DSB Interpretive Drift Creating New Obligations
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body was designed as a binding referee for
 *   trade disputes under the 1995 DSU. The judicial_activism_reading holds
 *   that panels and the Appellate Body have systematically exceeded their
 *   mandate through interpretive drift — creating new obligations (e.g.,
 *   'public body' test in US-Anti-Dumping Subsidies, 'zeroing' prohibition in
 *   anti-dumping, 'likeness' expansion in TBT/SPS) that were never
 *   negotiated. Member states, led by the US, view this as illegitimate
 *   judicial legislation. Since 2019, the US has blocked Appellate Body
 *   appointments, paralyzing the system. The EU created the MPIA as a
 *   stopgap. Developing countries face the drift without capacity to shape
 *   it. The constraint is a tangled rope: genuine coordination (binding
 *   dispute resolution) coexists with asymmetric extraction (interpretive
 *   expansion that transfers policy autonomy to the tribunal and market
 *   access to complainant industries), maintained by active enforcement
 *   (retaliation authorization).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.72).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.78).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Interpretive Drift Creating New Obligations").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, '5035985e-3e4c-42c2-b1f0-37c6857407fa').
narrative_ontology:cs_kernel_codification('5035985e-3e4c-42c2-b1f0-37c6857407fa', formalized).
narrative_ontology:cs_authority_grounding('5035985e-3e4c-42c2-b1f0-37c6857407fa', lineage).
narrative_ontology:cs_interpretation_layer_present('5035985e-3e4c-42c2-b1f0-37c6857407fa').
narrative_ontology:cs_reading_relation('5035985e-3e4c-42c2-b1f0-37c6857407fa', wto_dsb_authority__binding_referee_reading, influences).
narrative_ontology:cs_reading_relation('5035985e-3e4c-42c2-b1f0-37c6857407fa', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('5035985e-3e4c-42c2-b1f0-37c6857407fa', foundational, interpretive_drift_creates_new_obligations).
narrative_ontology:cs_axiom_status(interpretive_drift_creates_new_obligations, holdable).
narrative_ontology:cs_axiom_grounding('5035985e-3e4c-42c2-b1f0-37c6857407fa', interpretive_drift_creates_new_obligations, empirically_contingent).
narrative_ontology:cs_axiom('5035985e-3e4c-42c2-b1f0-37c6857407fa', foundational, retaliation_authorization_illegitimate_when_exceeding_mandate).
narrative_ontology:cs_axiom_status(retaliation_authorization_illegitimate_when_exceeding_mandate, holdable).
narrative_ontology:cs_axiom_grounding('5035985e-3e4c-42c2-b1f0-37c6857407fa', retaliation_authorization_illegitimate_when_exceeding_mandate, deontological).
narrative_ontology:cs_reference_frame('5035985e-3e4c-42c2-b1f0-37c6857407fa', dsu_1995_negotiated_mandate).
narrative_ontology:cs_drift_state('5035985e-3e4c-42c2-b1f0-37c6857407fa', post_appellate_body_paralysis, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5035985e-3e4c-42c2-b1f0-37c6857407fa', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, dsb_institutional_bureaucracy).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, complainant_industries).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, member_states_general).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, developing_countries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, united_states_major_power).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, eu_major_power).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, united_states_major_power).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, eu_major_power).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, judicial_lawmaking_in_trade_law).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, institutional_self_aggrandizement).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, treaty_interpretation_as_legislation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret WTO agreements in dispute rulings; through sequential decisions expand the scope of obligations beyond negotiated text (e.g., 'public body' doctrine in subsidies, 'likeness' in TBT/SPS, 'zeroing' methodology in anti-dumping). Control the interpretive trajectory; their authority derives from the Dispute Settlement Understanding (DSU) but they face no external check on interpretive expansion. Collect institutional prestige, budgetary resources, and jurisprudential authority from each expansion.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dsb_panels_appellate_body, agenda_setter,
    institutional, generational, analytical, global).

% Bound by DSB rulings that create obligations not negotiated; must change domestic laws or face authorized retaliation. Exit requires withdrawing from WTO entirely — a prohibitive cost for trade-dependent economies. Bear compliance costs, loss of policy space, and political backlash from domestic constituencies. Larger economies (US, EU, China) can resist longer; smaller economies comply immediately.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, member_states_general, payer,
    organized, biographical, constrained, global).

% Disproportionately affected by interpretive drift: lack legal capacity to litigate effectively, face retaliation they cannot absorb, and lose development policy space (e.g., industrial policy, food security measures) recharacterized as violations. The Special and Differential Treatment provisions are narrowed through interpretation. No credible exit — WTO membership is prerequisite for market access.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, developing_countries, payer,
    moderate, biographical, trapped, global).

% Domestic industries that initiate complaints gain expanded market access through rulings that go beyond treaty text (e.g., US steel, EU agriculture, Brazilian aircraft). Benefit from each interpretive expansion that constrains competitor state policies. Can forum-shop across domestic trade remedy systems and WTO; not dependent on any single ruling.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, complainant_industries, beneficiary,
    powerful, biographical, arbitrage, global).

% The Secretariat, legal affairs division, and permanent staff of the Appellate Body. Institutional survival and growth depend on a steady docket and expanding jurisprudential authority. Each new interpretive doctrine creates demand for more legal staff, more proceedings, and higher institutional profile. Staff rotate into academia, private practice, or government — carrying the institutional capital.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dsb_institutional_bureaucracy, beneficiary,
    organized, generational, mobile, global).

% Pays when rulings constrain US trade remedies (anti-dumping, countervailing duties, Section 232/301); benefits when rulings open foreign markets (IP, services, agriculture). Since 2019, has blocked Appellate Body appointments to force reform — treating the constraint as illegitimate judicial legislation. Has unilateral exit capacity (Section 301, bilateral deals) but faces systemic costs of fragmentation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, united_states_major_power, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, united_states_major_power, beneficiary).

% Generally benefits from judicialized dispute settlement that constrains unilateralism; supports expansive interpretation of non-discrimination and regulatory autonomy. Pays when rulings constrain EU agricultural subsidies, data privacy (GDPR), or carbon border measures. Created the Multi-Party Interim Appeal Arbitration Arrangement (MPIA) to preserve binding review — investing in the constraint's survival.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, eu_major_power, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, eu_major_power, payer).

% ICJ, ITLOS, ICJ, investment tribunals (ICSID), and regional courts (ECJ, Andean Tribunal) monitor WTO jurisprudence for cross-fertilization and systemic coherence. Their legitimacy is affected by whether WTO judicial activism is seen as strengthening or undermining international rule of law. No direct stake in WTO disputes but structural interest in interpretive discipline across tribunals.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, other_international_courts, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides binding, impartial resolution of trade disputes between sovereign states, preventing unilateral retaliation and trade wars; ensures predictability of market access commitments.
% TRANSFER_FUNCTION: Moves policy autonomy and regulatory discretion from member states to the DSB/Appellate Body through interpretive expansion; moves market access gains to complainant industries; moves institutional authority and resources to the DSB bureaucracy.
% ABSENT_VOICES: Future governments bound by precedents they did not negotiate; domestic legislatures whose regulatory choices are pre-empted by DSB interpretations; small economies that never appear as parties but are bound by the expanding acquis; civil society groups excluded from closed-door proceedings.
% DISAPPEARANCE_RATIONALE: If the DSB's interpretive authority vanished overnight, member states would revert to diplomatic settlement and unilateral retaliation (GATT 1947 style); the WTO would become a negotiating forum without enforcement; trade agreements would need explicit legislative updates for each new obligation; the MPIA and bilateral appeal arrangements would collapse.
% FOUNDING_PROBLEM: GATT 1947 dispute settlement was politicized: losing parties could block adoption of panel reports, rendering rulings unenforceable. The WTO DSU (1995) created automatic adoption and binding appellate review to solve this credibility gap.
% FOUNDING_PROBLEM_CORROBORATION: The US (original architect of DSU) attests the founding problem is solved but the solution created a new problem: unchecked judicial lawmaking. The EU and developing country coalitions attest the founding problem persists — without binding review, power-based retaliation returns. Academic literature (Pauwelyn, Jackson, Mavroidis) corroborates both: the credibility gap was real, but the Appellate Body filled gaps the negotiators deliberately left open.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the cumulative policy autonomy transferred from states to the tribunal through 25+ years of expansive interpretation — each ruling narrows the 'policy space' the negotiators intended to preserve. Suppression (0.78) is high because the enforcement mechanism (retaliation authorization) is automatic and the only exit is WTO withdrawal, which is prohibitive for trade-dependent states. Theater ratio (0.42) has risen as the Appellate Body's legitimacy crisis deepens: proceedings continue (MPIA, arbitration under DSU Article 25) but the underlying compliance expectation is contested. Accessibility collapse (0.71) reflects that once a state is a WTO member, alternatives to DSB jurisdiction collapse — regional agreements incorporate WTO-plus disciplines, and MFN means concessions to one are concessions to all. Resistance (0.68) captures the US blockade, the MPIA workaround, and growing scholarly/state critique — the constraint is actively contested, not passively accepted.
 *
 * PERSPECTIVAL GAP:
 *   The DSB/institutional bureaucracy seat experiences this as genuine coordination (rope-like): they solve disputes impartially, develop coherent jurisprudence, and maintain the system's credibility. The member state payer seats experience it as extraction (snare-like): obligations appear that were never consented to, enforced by retaliation they cannot avoid. The developing country payer seats experience it as a trap (trapped exit): they lack the legal capacity to defend against expansive claims and the economic resilience to withstand retaliation. The US payer/beneficiary seat experiences it as a broken contract — they designed the DSU for credibility, not judicial lawmaking, and now use their structural power to break the enforcement mechanism. The engine computes this divergence from the declared power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   DSB panels and bureaucracy are structural beneficiaries (d near 0.0): they collect institutional authority, jurisprudential control, and resource flows from each interpretive expansion. Complainant industries are beneficiaries (d low): they gain market access through rulings that exceed negotiated concessions. Member states general are targets (d high): they lose policy autonomy without consent, face retaliation for non-compliance with un-negotiated obligations. Developing countries are trapped targets (d near 1.0): no exit, no capacity to shape interpretation, disproportionate compliance burden. US and EU are dual-positioned: US currently acts as payer resisting the constraint (blocking appointments); EU acts as beneficiary preserving it (creating MPIA). The override for US (d higher than institutional default) captures active resistance; EU override (d lower) captures institutional investment in the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (GATT politicized blockage) was real and the DSU solved it — but the solution created a new mandate drift: the Appellate Body became a common-law court for trade, filling gaps the negotiators deliberately left for political resolution. The mandatrophy is not that the original function is gone (disputes still need resolution) but that the constraint has acquired a second, extractive function (judicial legislation) that no party consented to and the most powerful party now rejects. Classification as tangled_rope (not snare) captures that the coordination function remains real and valued by many; classification as not rope captures that the extraction is structural, not incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Does the wto_dsb_authority kernel admit multiple structurally distinct constraints (readings), or is the judicial_activism_reading merely a critical perspective on a single constraint?',
    'Apply the ε-invariance test: if measuring the DSB''s operation via ''compliance with negotiated text'' yields low ε but ''policy autonomy transferred'' yields high ε, the label ''DSB authority'' covers two constraints. The judicial_activism_reading instantiates the high-ε constraint; the binding_referee_reading instantiates the low-ε constraint.',
    'If two constraints, they must be authored as separate stories linked by network.affects_constraints. The current story authors ε for the standing arrangement as seen from the activism reading — the arrangement *is* the interpretive drift, not the DSU text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints per DP-001.').

omega_variable(
    interpretive_drift_measurement,
    'How much of the DSB''s jurisprudential expansion represents genuine gap-filling (coordination) versus de novo obligation-creation (extraction)?',
    'Systematic coding of Appellate Body reports 1995-2019: for each new doctrinal element, code whether it resolves textual ambiguity (coordination) or adds elements absent from text and negotiating history (extraction). Compare with negotiator intent records.',
    'If predominantly gap-filling, the constraint trends toward rope; if predominantly de novo creation, it trends toward snare. Current 0.72 ε assumes substantial de novo component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_drift_measurement, empirical, 'Proportion of jurisprudential expansion that exceeds textual mandate.').

omega_variable(
    developing_country_asymmetric_burden,
    'Does interpretive drift disproportionately extract from developing countries due to legal capacity asymmetry, or does Special and Differential Treatment (S&DT) mitigate the extraction?',
    'Compare compliance cost per capita GDP across development levels for rulings identified as ''expansive'' vs. ''textual''; assess S&DT invocation success rates in expansive vs. textual disputes.',
    'If developing countries bear disproportionate extraction, the constraint''s victim structure is more asymmetric than ''member_states_general'' captures — may require separate victim class or higher ε for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_country_asymmetric_burden, empirical, 'Whether extraction falls asymmetrically on low-capacity members.').

omega_variable(
    us_blockade_as_resistance_or_reform,
    'Is the US Appellate Body blockade (since 2019) resistance to extraction, or a powerful state''s refusal to accept adverse rulings it once supported?',
    'Analyze US litigation history: compare US win/loss rates pre- and post-2000; code US arguments in ''judicial overreach'' complaints for consistency across issue areas (anti-dumping vs. subsidies vs. SPS).',
    'If selective resistance (only when US loses), the resistance metric (0.68) overstates principled opposition; if consistent, it validates the judicial_activism_reading''s structural claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_blockade_as_resistance_or_reform, preference, 'Nature of the leading resistance: principled or opportunistic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t0, wto_dsb_authority__judicial_activism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t5, wto_dsb_authority__judicial_activism_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t10, wto_dsb_authority__judicial_activism_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t15, wto_dsb_authority__judicial_activism_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t20, wto_dsb_authority__judicial_activism_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t25, wto_dsb_authority__judicial_activism_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t30, wto_dsb_authority__judicial_activism_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t0, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t5, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t10, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t15, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t20, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t25, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t30, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t0, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t5, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t10, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t15, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t20, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t25, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t30, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__judicial_activism_reading, 0.12).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_appellate_body_paralysis).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_mpi_interim_appeal).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, regional_trade_agreements_dispute_settlement).

% DUAL FORMULATION NOTE:
% The wto_dsb_authority kernel decomposes into three readings with divergent ε: binding_referee_reading (ε ≈ 0.15, rope), advisory_coordination_reading (ε ≈ 0.05, rope/scaffold), judicial_activism_reading (ε ≈ 0.72, tangled_rope). The activism reading structurally influences the referee reading by delegitimizing its enforcement mechanism (US blockade); the referee reading influences the activism reading by providing the institutional machinery the activism reading critiques. The advisory reading coexists as a normative ideal for reformers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__judicial_activism_reading, institutional, 0.15).
constraint_indexing:directionality_override(wto_dsb_authority__judicial_activism_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
