% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__developmental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__developmental_reading, []).

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
 *   constraint_id: wto_treaty_framework__developmental_reading
 *   human_readable: WTO Treaty Framework — Developmental Reading
 *   domain: international_trade_law/development_economics/political_economy
 *
 * SUMMARY:
 *   The WTO treaty framework is a contested kernel. The developmental reading
 *   instantiates S&D provisions as permanent structural accommodation for
 *   asymmetric starting conditions — not temporary exceptions. It preserves
 *   tariff flexibility, subsidy space, and compulsory licensing authority as
 *   equal-status treaty commitments. Technology transfer obligations (TRIPS
 *   Art. 66.2, TRIMS, and plurilateral proposals) are core commitments, not
 *   aspirational. This reading claims moderate extractiveness (ε=0.42)
 *   because it constrains MNC IP monopolies and Global North market access to
 *   fund developmental policy space. The market_access_reading (sibling)
 *   claims the same treaty is a symmetric liberalization instrument with S&D
 *   as transitional. These are distinct constraints with different ε,
 *   different beneficiaries/victims, different enforcement logics — linked by
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, 0.42).
domain_priors:suppression_score(wto_treaty_framework__developmental_reading, 0.35).
domain_priors:theater_ratio(wto_treaty_framework__developmental_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__developmental_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__developmental_reading, "WTO Treaty Framework — Developmental Reading").
narrative_ontology:topic_domain(wto_treaty_framework__developmental_reading, "international_trade_law/development_economics/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__developmental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__developmental_reading, '9c075440-f7eb-45a4-bda2-690dffd8bc1e').
narrative_ontology:cs_kernel_codification('9c075440-f7eb-45a4-bda2-690dffd8bc1e', formalized).
narrative_ontology:cs_authority_grounding('9c075440-f7eb-45a4-bda2-690dffd8bc1e', lineage).
narrative_ontology:cs_interpretation_layer_present('9c075440-f7eb-45a4-bda2-690dffd8bc1e').
narrative_ontology:cs_reading_relation('9c075440-f7eb-45a4-bda2-690dffd8bc1e', wto_treaty_framework__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('9c075440-f7eb-45a4-bda2-690dffd8bc1e', foundational, asymmetric_development_accommodation_permanent).
narrative_ontology:cs_axiom_status(asymmetric_development_accommodation_permanent, holdable).
narrative_ontology:cs_axiom_grounding('9c075440-f7eb-45a4-bda2-690dffd8bc1e', asymmetric_development_accommodation_permanent, conventional).
narrative_ontology:cs_axiom('9c075440-f7eb-45a4-bda2-690dffd8bc1e', foundational, technology_transfer_as_erga_omnes_obligation).
narrative_ontology:cs_axiom_status(technology_transfer_as_erga_omnes_obligation, holdable).
narrative_ontology:cs_axiom_grounding('9c075440-f7eb-45a4-bda2-690dffd8bc1e', technology_transfer_as_erga_omnes_obligation, instrumental).
narrative_ontology:cs_reference_frame('9c075440-f7eb-45a4-bda2-690dffd8bc1e', marrakesh_agreement_developmental_mandate).
narrative_ontology:cs_drift_state('9c075440-f7eb-45a4-bda2-690dffd8bc1e', post_doha_round_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9c075440-f7eb-45a4-bda2-690dffd8bc1e', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__developmental_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, global_south_states).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, infant_industries_global_south).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, technology_recipient_firms).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, multinational_ip_rights_holders).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, global_north_technology_owners).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, export_oriented_firms_global_north).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, asymmetric_development_accommodation_principle).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, technology_transfer_as_treaty_obligation).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, special_differential_treatment_permanence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold treaty-guaranteed policy space for tariffs, subsidies, and compulsory licensing to protect infant industries and facilitate technology absorption. They championed S&D provisions as permanent structural accommodation, not temporary exceptions. Their exit is constrained by the multilateral trading system — leaving WTO means losing MFN access and dispute settlement, but staying means continuous renegotiation of the developmental space.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, global_south_states, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, global_south_states, agenda_setter).

% Domestic firms in strategic sectors (manufacturing, pharma, green tech) that receive tariff protection, subsidy eligibility, and technology transfer access. They benefit from the developmental reading's policy space but face competitive pressure from global incumbents. Exit means scaling without protection or relocating — both costly.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, infant_industries_global_south, beneficiary,
    moderate, biographical, constrained, regional).

% Firms in Global South that receive technology through compulsory licensing, joint venture requirements, or TRIPS flexibilities. They gain access to upstream knowledge but remain dependent on continued transfer terms. Their exit is mobile — they can pivot to alternative technology sources if terms become unfavorable.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, technology_recipient_firms, beneficiary,
    moderate, biographical, mobile, global).

% Pharma, biotech, software, and advanced manufacturing MNCs whose patent monopolies are constrained by compulsory licensing, technology transfer requirements, and S&D flexibilities. They bear the extraction (foregone monopoly rents, forced transfer) but hold immense structural power — they shape TRIPS-plus bilateral agreements, invest in lobbying, and use investor-state dispute settlement to arbitrage across jurisdictions.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, multinational_ip_rights_holders, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, multinational_ip_rights_holders, agenda_setter).

% States and state-backed entities (US, EU, Japan, etc.) that represent MNC interests in WTO negotiations and dispute settlement. They push for TRIPS enforcement, oppose S&D permanence, and negotiate bilateral FTAs that erode the developmental reading's policy space. Their exit is arbitrage-grade: they can shift rule-making to plurilateral or bilateral venues where the developmental reading has no standing.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, global_north_technology_owners, payer,
    institutional, generational, arbitrage, global).

% Firms in Global North that face tariff barriers and subsidy competition from Global South infant industries. They pay through lost market access and competitive asymmetry. Their exit is mobile — they can relocate production, shift supply chains, or lobby for trade remedies.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, export_oriented_firms_global_north, payer,
    organized, biographical, mobile, global).

% Administers the treaty, panels disputes, and authorizes retaliation. It sits between readings — its rulings on S&D interpretation (e.g., EC–Tariff Preferences, India–Solar Cells) structurally determine whether the developmental reading holds. It does not collect rents but its interpretive authority is the enforcement mechanism.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, wto_secretariat_dispute_body, observer,
    institutional, generational, analytical, universal).

% The poorest WTO members who lack capacity to use policy space even when guaranteed. They are excluded from effective benefit capture — no industrial base to protect, no absorptive capacity for technology transfer, no negotiating leverage. Their exit is trapped: they cannot leave the system (aid/trade dependence) and cannot use it.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, least_developed_countries_ldcs, excluded,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of asymmetric development in a universal treaty: how to embed permanent structural accommodation for late industrializers without collapsing the treaty into a two-tier system that destroys the reciprocity logic of MFN.
% TRANSFER_FUNCTION: Moves policy autonomy (tariff flexibility, subsidy space, IP flexibility) from the universal liberalization baseline to Global South states; moves technology access from MNC monopoly control to recipient firms via compulsory licensing and transfer obligations; moves enforcement cost to Global North states who must tolerate non-reciprocal flexibilities.
% ABSENT_VOICES: Least developed countries (LDCs) are structurally present in the treaty but excluded from effective use of the developmental reading's guarantees — they lack the state capacity, industrial base, and absorptive capacity to convert policy space into development outcomes. Their absence is not accidental; the reading's benefits accrue to middle-income emerging economies with existing industrial capacity (Brazil, India, South Africa), not the poorest members.
% DISAPPEARANCE_RATIONALE: If the developmental reading vanished overnight, S&D provisions would revert to temporary transitional exceptions, TRIPS flexibilities would narrow to the market_access_reading's interpretation, and Global South policy space would collapse to the symmetric baseline. Infant industries would lose protection, technology transfer obligations would become voluntary, and the treaty would become a pure market access instrument — the world trading system would reorganize around symmetric liberalization.
% FOUNDING_PROBLEM: The post-WWII trading system (GATT) was built by and for industrialized economies; decolonization brought dozens of new states into a framework that offered them no structural accommodation for late development. The developmental reading was built to solve: how can a universal trade treaty recognize asymmetric starting conditions without abandoning universality?
% FOUNDING_PROBLEM_CORROBORATION: The G77 and NAM bloc attest the founding problem remains live — asymmetric development persists, and the treaty's symmetric obligations reproduce inequality. Global North states and MNC coalitions attest the problem is substantially solved — emerging economies have 'graduated' and now use S&D as protectionism. Independent development economists (UNCTAD, South Centre) corroborate the problem is live but argue the reading's implementation has been captured by middle-income interests, not the poorest.
narrative_ontology:disappearance_verdict(wto_treaty_framework__developmental_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__developmental_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__developmental_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(wto_treaty_framework__developmental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__developmental_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__developmental_reading_tests).
:- end_tests(wto_treaty_framework__developmental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint transfers policy autonomy and technology access from powerful incumbents to late developers — real extraction from MNC rents, but bounded by treaty architecture (not predatory). Suppression (0.35) is present but not total: the developmental reading survives in treaty text and DSB jurisprudence, but its operational space is continuously eroded by TRIPS-plus bilaterals, dispute rulings narrowing flexibilities, and graduation pressure. Theater ratio (0.28) reflects that S&D provisions are often invoked performatively by middle-income states while LDCs cannot use them — the coordination function is real but the benefit capture is skewed. Accessibility collapse (0.45) is moderate: alternatives (bilateral FTAs, unilateral liberalization, regional blocs) exist but are costly. Resistance (0.55) is high: Global North states and MNCs actively contest every operationalization of the developmental reading.
 *
 * PERSPECTIVAL GAP:
 *   From the Global South state seat, this is a rope/tangled_rope: genuine coordination solving asymmetric development, with real but bounded extraction from incumbents. From the MNC/Global North seat, it is experienced as snare-like extraction: forced technology transfer, constrained IP enforcement, non-reciprocal market access — sustained by treaty lock-in they cannot exit. From the LDC seat, it is a piton: the developmental provisions exist textually but deliver no functional benefit — theatrical maintenance of a developmental promise that has atrophied for the poorest. The engine computes these seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South states and infant industries are structural beneficiaries (d ~ 0.2–0.3): they receive policy space and technology access, but their constrained exit (cannot leave WTO without losing MFN) prevents full beneficiary capture. MNC IP holders and Global North technology owners are structural payers (d ~ 0.7–0.8): they bear foregone rents and forced transfer, but their arbitrage-grade exit (TRIPS-plus bilaterals, ISDS, venue-shifting) dampens effective extraction. LDCs are excluded (d ~ 0.9 but no benefit capture): trapped in the system, unable to use the policy space, bearing compliance costs without developmental upside. The WTO Secretariat/DSB is the analytical observer — its interpretive rulings structurally determine the reading's survival.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (asymmetric development in a universal treaty) remains live — but the reading's implementation has drifted. S&D permanence is contested; technology transfer obligations are largely unenforced; policy space is eroded by bilateralism. The constraint has not resolved its mandatrophy: it persists as a textual commitment while its operational function has been hollowed for the poorest and captured by middle-income interests. The theater ratio rise (0.15→0.28) tracks this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    developmental_reading_vs_market_access_reading_boundary,
    'Is the developmental reading a distinct constraint with its own ε, or a rhetorical overlay on the market_access_reading''s constraint?',
    'Compare DSB rulings on S&D interpretation: if rulings consistently treat S&D as autonomous rights (not exceptions), the readings are structurally distinct constraints. If rulings treat S&D as narrow exceptions to market access obligations, the developmental reading is a rhetorical overlay with no independent ε.',
    'If distinct, the kernel decomposition is valid (two constraints, two ε). If overlay, the developmental reading collapses into the market_access_reading''s ε and the family decomposition is an authoring artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developmental_reading_vs_market_access_reading_boundary, conceptual, 'Whether the kernel decomposition into two constraints is structurally real or linguistically projected.').

omega_variable(
    technology_transfer_enforceability,
    'Are technology transfer obligations (TRIPS 66.2, TRIMS, proposed plurilateral) legally enforceable commitments or aspirational best-endeavor language?',
    'WTO dispute settlement on technology transfer: has any panel found a violation of Art. 66.2? If no jurisprudence exists after 30 years, the obligation is aspirational — extraction is lower than authored.',
    'If aspirational, extractiveness drops (the constraint coordinates without extracting from MNCs). If enforceable, extractiveness rises and the reading moves toward snare from the MNC seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_transfer_enforceability, empirical, 'Whether the technology transfer core commitment has operational teeth.').

omega_variable(
    ldc_exclusion_structural_or_incidental,
    'Is LDC exclusion from developmental reading benefits a structural feature (the reading serves middle-income emerging economies) or an incidental capacity gap?',
    'Analyze S&D utilization rates by income group: if middle-income emerging economies (Brazil, India, South Africa) capture >80% of S&D benefits while LDCs capture <5%, exclusion is structural. If utilization correlates with capacity-building aid, it is incidental.',
    'If structural, the reading has an internal extraction gradient (middle-income Global South extracts from LDCs via the same treaty) — a nested tangled_rope. If incidental, the reading''s beneficiary set is accurately described.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ldc_exclusion_structural_or_incidental, empirical, 'Whether the developmental reading contains its own internal extraction hierarchy.').

omega_variable(
    graduation_pressure_as_extraction_ratchet,
    'Does ''graduation'' pressure (voluntary S&D renunciation for market access) function as an extraction ratchet that permanently reduces the developmental reading''s policy space?',
    'Track graduating members (e.g., China 2001, Vietnam 2007, potential future graduations): do they retain S&D flexibilities post-accession? If graduation requires S&D renunciation, the reading''s policy space shrinks irreversibly — a one-way extraction ratchet.',
    'If ratchet, the developmental reading''s ε is rising structurally (policy space contracts while obligations remain). The measurement series captures this but the mechanism needs confirmation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduation_pressure_as_extraction_ratchet, conceptual, 'Whether the treaty''s accession/graduation dynamics structurally erode the developmental reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__developmental_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_treaty_framework__developmental_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(wto__tr_t2001, wto_treaty_framework__developmental_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(wto__tr_t2005, wto_treaty_framework__developmental_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(wto__tr_t2010, wto_treaty_framework__developmental_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(wto__tr_t2015, wto_treaty_framework__developmental_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(wto__tr_t2020, wto_treaty_framework__developmental_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(wto__tr_t2025, wto_treaty_framework__developmental_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_treaty_framework__developmental_reading, base_extractiveness, 1995, 0.25).
narrative_ontology:measurement(wto__be_t2001, wto_treaty_framework__developmental_reading, base_extractiveness, 2001, 0.3).
narrative_ontology:measurement(wto__be_t2005, wto_treaty_framework__developmental_reading, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement(wto__be_t2010, wto_treaty_framework__developmental_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(wto__be_t2015, wto_treaty_framework__developmental_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(wto__be_t2020, wto_treaty_framework__developmental_reading, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement(wto__be_t2025, wto_treaty_framework__developmental_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_treaty_framework__developmental_reading, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement(wto__su_t2001, wto_treaty_framework__developmental_reading, suppression_requirement, 2001, 0.25).
narrative_ontology:measurement(wto__su_t2005, wto_treaty_framework__developmental_reading, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement(wto__su_t2010, wto_treaty_framework__developmental_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(wto__su_t2015, wto_treaty_framework__developmental_reading, suppression_requirement, 2015, 0.32).
narrative_ontology:measurement(wto__su_t2020, wto_treaty_framework__developmental_reading, suppression_requirement, 2020, 0.34).
narrative_ontology:measurement(wto__su_t2025, wto_treaty_framework__developmental_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__developmental_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(wto_treaty_framework__developmental_reading, 0.18).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, wto_treaty_framework__market_access_reading).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, trips_flexibilities_compulsory_licensing).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, special_differential_treatment_operationalization).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, technology_transfer_obligations_trips_66_2).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, bilateral_ftas_trips_plus_erosion).

% DUAL FORMULATION NOTE:
% The wto_treaty_framework kernel decomposes into two constraint stories: developmental_reading (this file, ε=0.42, tangled_rope, Global South beneficiaries, MNC payers) and market_access_reading (sibling, ε≈0.15, rope, universal beneficiaries, no structural payers). The developmental reading's S&D permanence claim structurally influences the market_access_reading's transitional exception claim — they coexist but create opposing interpretive pressures on DSB rulings. The developmental reading's technology transfer obligations affect TRIPS flexibilities and bilateral TRIPS-plus erosion constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__developmental_reading, powerful, 0.35).
constraint_indexing:directionality_override(wto_treaty_framework__developmental_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
