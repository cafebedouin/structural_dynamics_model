% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__sovereignty_primacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary: Sovereignty Primacy Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint story captures the sovereignty primacy reading of the
 *   NAFTA jurisdictional boundary kernel. Under this reading, the trade
 *   agreement functions as a coordination mechanism for market access — a
 *   rope — while states retain full regulatory authority over labor,
 *   environmental, and health standards. Treaty obligations enter the
 *   compliance-cost set as voluntary options for regulated entities seeking
 *   market access, not as overriding constraints. The reading is held by
 *   sovereign states, domestic regulatory agencies, and civil society groups
 *   who view the treaty as subordinate to domestic law. The claimed type is
 *   rope (genuine coordination with minimal coercion), and the metrics
 *   reflect low extraction, low suppression, and low theater.
 *
 * KEY AGENTS:
 *   - sovereign_states: Primary agenda setter (institutional/arbitrage) — sets domestic law and negotiates treaty
 *   - domestic_regulatory_agencies: Beneficiary (organized/mobile) — retains jurisdictional authority
 *   - regulated_entities: Beneficiary/payer (organized/constrained) — gains market access, bears voluntary compliance costs
 *   - citizens_under_domestic_law: Beneficiary (powerless/trapped) — receives undiminished regulatory protection
 *   - other_party_states: Beneficiary (institutional/arbitrage) — reciprocal coordination
 *   - international_tribunals: Observer (institutional/analytical) — subordinate dispute settlement
 *   - capital_interests: Excluded (powerful/mobile) — seeks supranational override, denied by this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.1).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary: Sovereignty Primacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '29b0c93c-cdd5-4a23-91a9-e2f244c7ee36').
narrative_ontology:cs_kernel_codification('29b0c93c-cdd5-4a23-91a9-e2f244c7ee36', formalized).
narrative_ontology:cs_authority_grounding('29b0c93c-cdd5-4a23-91a9-e2f244c7ee36', lineage).
narrative_ontology:cs_interpretation_layer_present('29b0c93c-cdd5-4a23-91a9-e2f244c7ee36').
narrative_ontology:cs_reading_relation('29b0c93c-cdd5-4a23-91a9-e2f244c7ee36', nafta_jurisdictional_boundary__capital_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('29b0c93c-cdd5-4a23-91a9-e2f244c7ee36', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_axiom('29b0c93c-cdd5-4a23-91a9-e2f244c7ee36', foundational, sovereign_regulatory_authority_inviolable).
narrative_ontology:cs_axiom_status(sovereign_regulatory_authority_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('29b0c93c-cdd5-4a23-91a9-e2f244c7ee36', sovereign_regulatory_authority_inviolable, deontological).
narrative_ontology:cs_axiom('29b0c93c-cdd5-4a23-91a9-e2f244c7ee36', secondary, treaty_obligations_as_voluntary_coordination).
narrative_ontology:cs_axiom_status(treaty_obligations_as_voluntary_coordination, holdable).
narrative_ontology:cs_axiom_grounding('29b0c93c-cdd5-4a23-91a9-e2f244c7ee36', treaty_obligations_as_voluntary_coordination, conventional).
narrative_ontology:cs_reference_frame('29b0c93c-cdd5-4a23-91a9-e2f244c7ee36', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('29b0c93c-cdd5-4a23-91a9-e2f244c7ee36', contemporary_trade_governance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('29b0c93c-cdd5-4a23-91a9-e2f244c7ee36', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, citizens_under_domestic_law).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, exporters_importers).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, other_party_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, regulated_entities).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, regulated_entities).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, embedded_liberalism_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and ratify trade agreements; retain full authority to set labor, environmental, and health standards within their territory; use treaty as a coordination tool for market access without ceding regulatory supremacy.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_states, agenda_setter,
    institutional, generational, arbitrage, national).

% Administer and enforce domestic labor, environmental, and health regulations; their jurisdictional authority is unimpaired by treaty obligations, which enter only as voluntary compliance costs.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, beneficiary,
    organized, biographical, mobile, national).

% Gain market access through treaty coordination; bear voluntary compliance costs when choosing to meet treaty-aligned standards to access partner markets; can opt out of treaty-linked standards by forgoing those market benefits.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, regulated_entities, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__sovereignty_primacy_reading, regulated_entities, payer).

% Benefit from undiminished domestic regulatory protections; treaty does not weaken labor, environmental, or health standards; no exit from the sovereign's regulatory umbrella.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, citizens_under_domestic_law, beneficiary,
    powerless, biographical, trapped, national).

% Receive reciprocal market access and coordination benefits; each retains its own regulatory sovereignty; the treaty functions as a mutual coordination device without supranational override.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, other_party_states, beneficiary,
    institutional, generational, arbitrage, national).

% Exist as dispute settlement bodies under the treaty; under this reading their rulings are advisory or limited to narrow trade remedies, never overriding domestic regulatory standards.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, international_tribunals, observer,
    institutional, generational, analytical, global).

% Seek supranational regulatory harmonization and investor-state dispute settlement that overrides domestic law; structurally excluded from this reading's framework because the reading denies treaty supremacy over domestic regulation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, capital_interests, excluded,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for market access and trade coordination while preserving domestic regulatory sovereignty; solves the problem of trade friction without requiring states to harmonize or subordinate their labor, environmental, or health standards.
% TRANSFER_FUNCTION: Moves voluntary compliance costs from regulated entities to the treaty coordination system in exchange for market access benefits; does not transfer regulatory authority or impose mandatory harmonization costs.
% ABSENT_VOICES: Capital interests that seek supranational regulatory harmonization and investor-state dispute settlement overriding domestic law; they are excluded because this reading denies the treaty any supremacy over domestic regulatory standards.
% DISAPPEARANCE_RATIONALE: If the sovereignty primacy reading vanished, the treaty could be interpreted as supreme (capital_supremacy_reading) or as a balanced framework with enforceable labor/environmental standards (embedded_liberalism_reading), altering the balance of regulatory power and potentially subjecting domestic standards to trade discipline.
% FOUNDING_PROBLEM: The post-WWII need for trade coordination without sacrificing domestic regulatory sovereignty — the embedded liberalism compromise: open markets paired with policy space for full employment and social protection.
% FOUNDING_PROBLEM_CORROBORATION: Scholars of embedded liberalism (e.g., John Ruggie) and domestic regulatory agencies across NAFTA parties attest to the ongoing need for policy space; labor unions and environmental NGOs corroborate that the treaty has not displaced domestic standards.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.15) because compliance costs are voluntary — firms opt into treaty-aligned standards only when the market access benefit exceeds the cost. Suppression is low (0.10) because the treaty does not override domestic law; states can always regulate more stringently. Theater is negligible (0.05) because the coordination function (market access) is real and the treaty does not perform regulatory harmonization theater. Accessibility collapse is low (0.20) because alternatives (domestic-only regulation, non-treaty trade) remain fully available. Resistance is low (0.10) because the arrangement does not threaten sovereign authority.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty primacy reading and the capital_supremacy_reading occupy opposite poles: for sovereign_states and domestic_regulatory_agencies the constraint is a rope (beneficial coordination); for capital_interests the same treaty text is a snare (blocked by this reading's denial of supranational authority). The engine will compute per-seat classifications from the structural data — this reading's claim of rope is not imposed on other seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and domestic agencies are structural beneficiaries (d near 0.0) — they gain coordination without losing authority. Regulated entities are near symmetric (d ~ 0.5) — they gain market access and pay voluntary compliance costs. Citizens are beneficiaries (d near 0.0) — they retain protections. Capital_interests are excluded; they would be targets (d near 1.0) if the capital_supremacy_reading prevailed, but under this reading they are not party to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (embedded liberalism's trade-sovereignty balance) remains live. The treaty has not atrophied into a piton because the coordination function (market access) is actively used and the sovereignty protection is actively valued. No mandatrophy resolution is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    de_facto_vs_de_jure_subordination,
    'Does the treaty''s dispute settlement mechanism create de facto regulatory constraints despite the de jure sovereignty primacy reading?',
    'Empirical analysis of tribunal rulings: count cases where domestic regulations were effectively disciplined despite formal sovereignty language.',
    'If de facto constraint exists, the reading''s claimed rope type may mask hidden extraction; the constraint would be tangled_rope from the regulated_entities seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_facto_vs_de_jure_subordination, empirical, 'Whether formal sovereignty language matches operational reality in dispute settlement.').

omega_variable(
    voluntary_compliance_boundary,
    'At what point do voluntary compliance costs become effectively mandatory due to market power asymmetry?',
    'Economic analysis of market access dependency: measure the share of a regulated entity''s revenue dependent on treaty-partner markets.',
    'If dependency is high, the ''voluntary'' compliance becomes structural extraction, shifting the reading toward tangled_rope for regulated_entities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_compliance_boundary, conceptual, 'Threshold where market access dependency converts voluntary coordination into effective coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 1994, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 1994, 0.03).
narrative_ontology:measurement(naft_tr_t1998, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 1998, 0.04).
narrative_ontology:measurement(naft_tr_t2002, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2002, 0.04).
narrative_ontology:measurement(naft_tr_t2008, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2008, 0.05).
narrative_ontology:measurement(naft_tr_t2014, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2014, 0.05).
narrative_ontology:measurement(naft_tr_t2020, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1994, 0.12).
narrative_ontology:measurement(naft_be_t1998, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1998, 0.13).
narrative_ontology:measurement(naft_be_t2002, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2002, 0.14).
narrative_ontology:measurement(naft_be_t2008, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2008, 0.15).
narrative_ontology:measurement(naft_be_t2014, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2014, 0.15).
narrative_ontology:measurement(naft_be_t2020, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 1994, 0.08).
narrative_ontology:measurement(naft_su_t1998, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 1998, 0.09).
narrative_ontology:measurement(naft_su_t2002, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2002, 0.09).
narrative_ontology:measurement(naft_su_t2008, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2008, 0.1).
narrative_ontology:measurement(naft_su_t2014, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2014, 0.1).
narrative_ontology:measurement(naft_su_t2020, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2020, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% This reading and capital_supremacy_reading are mutually exclusive framings of the same treaty text; embedded_liberalism_reading occupies an intermediate position. The three form a constraint family linked by the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
