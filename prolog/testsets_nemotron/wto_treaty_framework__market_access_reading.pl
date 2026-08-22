% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__market_access_reading, []).

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
 *   constraint_id: wto_treaty_framework__market_access_reading
 *   human_readable: WTO Market Access Reading — Symmetric Trade Liberalization as Universal Obligation
 *   domain: international_trade_law/development_economics/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the market_access_reading of the WTO
 *   treaty framework kernel. It reads the WTO as a symmetric universal
 *   obligation system where non-discrimination (MFN, national treatment) and
 *   market access (tariff bindings, prohibition of quantitative restrictions)
 *   are the primary treaty purposes, and Special and Differential Treatment
 *   (S&D) provisions are temporary, transitional exceptions that developing
 *   countries are expected to graduate from. The reading treats industrial
 *   policy space as a legacy concession, not a structural right. Infant
 *   industries, agricultural smallholders, and import-competing domestic
 *   producers enter the victim set; multinational corporations,
 *   export-oriented economies, and financial capital are the primary
 *   beneficiaries. The constraint is actively enforced through the Dispute
 *   Settlement Body (despite its current paralysis, the threat of authorized
 *   retaliation sustains compliance).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, 0.68).
domain_priors:suppression_score(wto_treaty_framework__market_access_reading, 0.55).
domain_priors:theater_ratio(wto_treaty_framework__market_access_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__market_access_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__market_access_reading, "WTO Market Access Reading — Symmetric Trade Liberalization as Universal Obligation").
narrative_ontology:topic_domain(wto_treaty_framework__market_access_reading, "international_trade_law/development_economics/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__market_access_reading, '0abb34ce-7e47-4aa4-95d0-4f8bc1a3f4eb').
narrative_ontology:cs_kernel_codification('0abb34ce-7e47-4aa4-95d0-4f8bc1a3f4eb', formalized).
narrative_ontology:cs_authority_grounding('0abb34ce-7e47-4aa4-95d0-4f8bc1a3f4eb', lineage).
narrative_ontology:cs_interpretation_layer_present('0abb34ce-7e47-4aa4-95d0-4f8bc1a3f4eb').
narrative_ontology:cs_reading_relation('0abb34ce-7e47-4aa4-95d0-4f8bc1a3f4eb', wto_treaty_framework__developmental_reading, coexists_with).
narrative_ontology:cs_axiom('0abb34ce-7e47-4aa4-95d0-4f8bc1a3f4eb', foundational, non_discrimination_as_constitutive_principle).
narrative_ontology:cs_axiom_status(non_discrimination_as_constitutive_principle, holdable).
narrative_ontology:cs_axiom_grounding('0abb34ce-7e47-4aa4-95d0-4f8bc1a3f4eb', non_discrimination_as_constitutive_principle, conventional).
narrative_ontology:cs_axiom('0abb34ce-7e47-4aa4-95d0-4f8bc1a3f4eb', foundational, special_differential_treatment_as_transitional_exception).
narrative_ontology:cs_axiom_status(special_differential_treatment_as_transitional_exception, holdable).
narrative_ontology:cs_axiom_grounding('0abb34ce-7e47-4aa4-95d0-4f8bc1a3f4eb', special_differential_treatment_as_transitional_exception, instrumental).
narrative_ontology:cs_reference_frame('0abb34ce-7e47-4aa4-95d0-4f8bc1a3f4eb', uruguay_round_bargain).
narrative_ontology:cs_drift_state('0abb34ce-7e47-4aa4-95d0-4f8bc1a3f4eb', post_doha_collapse_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0abb34ce-7e47-4aa4-95d0-4f8bc1a3f4eb', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__market_access_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, export_oriented_economies).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, financial_capital_interests).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, infant_industries_in_developing_countries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, domestic_producers_facing_import_surges).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, agricultural_smallholders_in_importing_nations).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, non_discrimination_principle).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, market_access_as_primary_treaty_purpose).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, symmetric_liberalization_obligation).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, special_differential_treatment_as_transitional).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the WTO treaty framework through binding dispute settlement. Interprets market access obligations and S&D provisions. Authorizes retaliation for non-compliance. Collects no direct rents but wields the enforcement power that sustains the constraint.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).

% Gain predictable, non-discriminatory market access across 164+ jurisdictions. Use dispute settlement to challenge industrial policies, local content requirements, and agricultural supports that impede cross-border operations. Their global supply chains and arbitration-grade exit options make them primary beneficiaries of symmetric liberalization.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, multinational_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Advanced economies and newly industrialized states that have completed structural transformation. Shape treaty interpretation through coalition-building in Geneva and capital contributions to WTO technical assistance. Benefit from locking in market access for their mature export sectors while limiting policy space for competitors.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, export_oriented_economies, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, export_oriented_economies, agenda_setter).

% Domestic firms in sectors that would require temporary protection, subsidies, or local content rules to achieve competitiveness. Face immediate import competition from established global producers. Cannot exit the constraint — their governments are bound by WTO commitments — and lack the scale to absorb losses during transition.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, infant_industries_in_developing_countries, payer,
    moderate, biographical, constrained, national).

% Small and medium enterprises in import-competing sectors, often in agriculture or light manufacturing. Lack political voice to secure safeguards or adjustment assistance. Bear the full cost of sudden liberalization without the capital or state support to restructure.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, domestic_producers_facing_import_surges, payer,
    powerless, immediate, trapped, local).

% Subsistence and semi-commercial farmers whose livelihoods are constituted by land-based production they cannot readily abandon. Tariff reductions expose them to subsidized imports from major exporters. Their exit is identity-locked — farming is not just income but social role, cultural continuity, and intergenerational contract.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, agricultural_smallholders_in_importing_nations, payer,
    powerless, biographical, identity_locked, national).

% International banks, investment funds, and trade finance institutions that profit from the volume and predictability of cross-border commerce under stable rules. Lobby for deeper liberalization in services, investment, and intellectual property. Their returns scale with the constraint's reach and enforcement intensity.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, financial_capital_interests, beneficiary,
    organized, biographical, arbitrage, global).

% Analyze the treaty's developmental effects from outside the enforcement structure. Document the gap between S&D rhetoric and operational reality. Their work informs resistance coalitions but carries no enforcement power.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, development_economics_scholars, observer,
    analytical, generational, analytical, universal).

% Networks of farmers' organizations, labor unions, and NGOs that oppose the market access reading's compression of policy space. Organize at WTO ministerials and in national capitals. Would object to the constraint's developmental asymmetry if present in treaty interpretation — but formal WTO processes exclude them from decision-making.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, civil_society_coalitions_global_south, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of reciprocal tariff reduction: each state lowers barriers in exchange for others doing the same, preventing a prisoner's dilemma of mutual protectionism. Provides a stable, rules-based framework for cross-border commerce that reduces transaction costs and prevents trade wars.
% TRANSFER_FUNCTION: Moves policy autonomy from developing-country governments to a global rule-enforcement regime, and moves market access rents from protected domestic sectors to multinational exporters and import-competing consumers. Transfers the cost of adjustment from the global system to localized producers who lose tariff protection.
% ABSENT_VOICES: Workers in informal sectors who bear liberalization's displacement costs without representation; future generations in developing countries whose industrialization pathways are foreclosed; indigenous communities whose land-based economies are not captured in trade statistics. They are structurally excluded from WTO governance and national trade policy formulation.
% DISAPPEARANCE_RATIONALE: If the market access reading vanished overnight, developing countries would immediately reinstate tariffs, subsidies, and local content requirements for strategic sectors. Multinational supply chains would fragment as legal certainty collapsed. The global trading system would revert to bilateral power-based negotiations. The world rearranges because the constraint actively structures the distribution of policy space and market access.
% FOUNDING_PROBLEM: Post-WWII reconstruction required a rules-based trading system to prevent the beggar-thy-neighbor protectionism of the 1930s. The GATT/WTO was built to lock in reciprocal market opening among sovereign equals, with S&D as a temporary bridge for newly independent states to integrate gradually.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as still live by the WTO Secretariat and major trading powers (EU, US, Japan), who argue that predictable market access remains the core public good. Developing country coalitions (G90, African Group, ACP) and development economists (UNCTAD, South Centre) attest the problem is substantially solved for advanced economies but the arrangement persists as rent extraction — they document that S&D provisions have been operationally hollowed out while market access obligations have deepened. Independent economic historians corroborate the asymmetric outcome: no developing country has industrialized under the current rules without violating them (e.g., South Korea's pre-WTO trajectory would be illegal today).
narrative_ontology:disappearance_verdict(wto_treaty_framework__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__market_access_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__market_access_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(wto_treaty_framework__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__market_access_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__market_access_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_treaty_framework__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint compresses the policy space that historical evidence shows is necessary for structural transformation — tariffs, subsidies, local content, export discipline — and transfers the gains of market access to already-competitive global actors. Suppression (0.55) is moderate-high: the DSB's binding rulings and retaliation authorization create real coercion, but developing countries retain some maneuvering room through political coalition-building and the constraint's own ambiguities. Theater ratio (0.38) reflects that S&D provisions and technical assistance programs perform a developmental rhetoric while operational policy space shrinks. The measurement series on a shared grid (1995, 2001, 2008, 2013, 2017, 2020, 2024) captures the post-Uruguay Round deepening: China's accession, Doha Round collapse, rise of mega-regionals, DSB crisis.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter/beneficiary seats, the constraint appears as genuine coordination: a stable, non-discriminatory trading system that prevents trade wars and lifts global welfare. From the payer seats — especially the identity-locked smallholders and constrained infant industries — the same structure operates as enforced extraction: their policy tools are removed while competitors' advantages (subsidies, scale, IP monopolies) remain protected. The engine computes this divergence from the structural data. The developmental_reading would invert the beneficiary/victim map and assign different exit options — that is the sibling constraint, not this one.
 *
 * DIRECTIONALITY LOGIC:
 *   The WTO DSB is the agenda_setter with analytical exit (d ~ 0.1, near-beneficiary): it administers the constraint but collects no rents. Multinational corporations and financial capital are full beneficiaries (d ~ 0.05): they gain market certainty, dispute enforcement, and arbitration-grade exit. Export-oriented economies are dual-role: agenda_setters who also benefit (d ~ 0.15). Infant industries are constrained payers (d ~ 0.75): bound by treaty, limited exit, immediate competitive exposure. Domestic producers facing surges are trapped payers (d ~ 0.9). Agricultural smallholders are identity-locked payers (d ~ 0.85): their livelihood structure fuses with the land, making exit existential. Civil society coalitions are excluded (d ~ 0.6): they bear costs but are barred from the rule-making room.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing 1930s-style protectionism) is contested: the coordination function remains live for advanced economies, but the developmental function (S&D as transitional bridge) is dead in practice — no graduation pathway exists, and the 'temporary' exceptions have become permanent asymmetries in reverse. The constraint now extracts from the very actors it was meant to accommodate. This is tangled_rope, not snare, because the coordination function (reciprocal market access) is real and valued by powerful actors, AND the extraction (compression of developmental policy space) is asymmetric and actively enforced. It is not a pure snare because the beneficiaries would maintain the coordination even without extraction — but they also resist removing the extraction because it serves them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_access_vs_development_kernel,
    'Is the WTO treaty framework a single kernel with two structurally distinct readings, or are these two different constraints that happen to share a treaty text?',
    'Decompose the treaty''s operational provisions into distinct constraint stories with independent ε values. If the market access provisions and S&D provisions have divergent empirical statuses (one verified, one hollowed out), they are distinct constraints per the ε-invariance principle.',
    'If distinct constraints, the market_access_reading is a snare (high extraction, active suppression) and the developmental_reading is a scaffold (failed transition) or piton (atrophied coordination). If a single kernel, the contested classification reflects genuine interpretive ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_access_vs_development_kernel, conceptual, 'Whether the kernel framing itself obscures a constraint family that should be decomposed per ε-invariance.').

omega_variable(
    s_and_d_transitional_vs_permanent,
    'Are S&D provisions structurally transitional (as this reading claims) or permanently necessary given persistent asymmetric starting conditions?',
    'Empirical test: have any developing countries ''graduated'' from needing S&D under the current rules? If no country has industrialized without violating S&D limits, the transitional claim is falsified.',
    'If S&D is permanently necessary, this reading''s claimed_type (tangled_rope) masks a snare: the ''transitional'' framing is cover for permanent extraction. If genuinely transitional, the reading''s coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(s_and_d_transitional_vs_permanent, empirical, 'Whether the developmental asymmetry that motivates S&D is a temporary condition or a structural feature of the global economy.').

omega_variable(
    dsb_enforcement_post_appellate_crisis,
    'With the Appellate Body non-functional since 2019, does the constraint''s suppression derive from formal DSB rulings or from the shadow of the law and bilateral pressure?',
    'Track compliance rates for contested rulings post-2019. If compliance holds without a functioning appellate tier, suppression is structural (shadow of the law). If compliance decays, the constraint''s enforcement was always dependent on the DSB''s institutional integrity.',
    'If suppression is structural, the constraint persists as tangled_rope even without the DSB. If enforcement-dependent, the current paralysis may shift the constraint toward piton (degraded enforcement) or scaffold (transitional collapse).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dsb_enforcement_post_appellate_crisis, empirical, 'Whether the constraint''s active enforcement requires a functioning dispute settlement appellate tier or operates through diffuse reputational and bilateral channels.').

omega_variable(
    identity_lock_smallholders_mechanism,
    'For agricultural smallholders, is identity_lock driven by land tenure systems, cultural continuity, intergenerational obligation, or absence of urban absorption capacity?',
    'Comparative study of exit trajectories: smallholders who exit agriculture vs. those who persist under import competition. Disaggregate the identity_lock mechanisms to see which are structural vs. cultural.',
    'If identity_lock is primarily structural (no urban jobs, no land market), the constraint''s extraction is amplified by exit blockade — a snare feature. If primarily cultural, the constraint interacts with pre-existing social structures rather than creating the lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_smallholders_mechanism, empirical, 'The mechanism binding smallholders to a livelihood the constraint makes unviable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__market_access_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_market_access_tr_t1995, wto_treaty_framework__market_access_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(wto_market_access_tr_t2001, wto_treaty_framework__market_access_reading, theater_ratio, 2001, 0.28).
narrative_ontology:measurement(wto_market_access_tr_t2008, wto_treaty_framework__market_access_reading, theater_ratio, 2008, 0.31).
narrative_ontology:measurement(wto_market_access_tr_t2013, wto_treaty_framework__market_access_reading, theater_ratio, 2013, 0.34).
narrative_ontology:measurement(wto_market_access_tr_t2017, wto_treaty_framework__market_access_reading, theater_ratio, 2017, 0.36).
narrative_ontology:measurement(wto_market_access_tr_t2020, wto_treaty_framework__market_access_reading, theater_ratio, 2020, 0.37).
narrative_ontology:measurement(wto_market_access_tr_t2024, wto_treaty_framework__market_access_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(wto_market_access_be_t1995, wto_treaty_framework__market_access_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(wto_market_access_be_t2001, wto_treaty_framework__market_access_reading, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement(wto_market_access_be_t2008, wto_treaty_framework__market_access_reading, base_extractiveness, 2008, 0.58).
narrative_ontology:measurement(wto_market_access_be_t2013, wto_treaty_framework__market_access_reading, base_extractiveness, 2013, 0.63).
narrative_ontology:measurement(wto_market_access_be_t2017, wto_treaty_framework__market_access_reading, base_extractiveness, 2017, 0.66).
narrative_ontology:measurement(wto_market_access_be_t2020, wto_treaty_framework__market_access_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(wto_market_access_be_t2024, wto_treaty_framework__market_access_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(wto_market_access_su_t1995, wto_treaty_framework__market_access_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(wto_market_access_su_t2001, wto_treaty_framework__market_access_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement(wto_market_access_su_t2008, wto_treaty_framework__market_access_reading, suppression_requirement, 2008, 0.48).
narrative_ontology:measurement(wto_market_access_su_t2013, wto_treaty_framework__market_access_reading, suppression_requirement, 2013, 0.51).
narrative_ontology:measurement(wto_market_access_su_t2017, wto_treaty_framework__market_access_reading, suppression_requirement, 2017, 0.53).
narrative_ontology:measurement(wto_market_access_su_t2020, wto_treaty_framework__market_access_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(wto_market_access_su_t2024, wto_treaty_framework__market_access_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__market_access_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(wto_treaty_framework__market_access_reading, 0.18).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, wto_treaty_framework__developmental_reading).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, wto_dispute_settlement_mechanism).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, tfa_trade_facilitation_agreement).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, trips_agreement).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, scm_agreement_subsidies).

% DUAL FORMULATION NOTE:
% This constraint and developmental_reading form a constraint family decomposed from the wto_treaty_framework kernel. They share the same treaty text but instantiate different constraints with different ε values (this reading: 0.68; developmental_reading would author a different ε reflecting its victim/beneficiary inversion). The market_access_reading's ε measures extraction from developing-country policy space; the developmental_reading's ε would measure extraction from developed-country market access commitments. They are linked via affects_constraints and share the kernel_id in commentary.kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, institutional, 0.15).
constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, moderate, 0.7).
constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
