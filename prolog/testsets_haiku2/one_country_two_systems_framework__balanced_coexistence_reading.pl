% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems — Balanced Coexistence Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   One Country, Two Systems instantiates a constitutional arrangement where
 *   sovereignty and autonomy are negotiated rather than legally hierarchical.
 *   Under this reading (the balanced coexistence reading), neither the PRC's
 *   central authority nor Hong Kong's institutional autonomy is absolute; the
 *   boundary between them is contestable and subject to periodic
 *   renegotiation through political accommodation rather than legal supremacy
 *   doctrines. The arrangement is a tangled rope: it genuinely coordinates
 *   the dual-system function (maintaining international hub status while
 *   integrating into sovereign Chinese territory), and it simultaneously
 *   extracts cost from those who would expand autonomy or who bear the
 *   suppression required to enforce the contested boundary. The claim and
 *   metrics are deliberately independent: this reading claims the arrangement
 *   as tangled rope (genuine coordination asymmetry), and the authored
 *   metrics describe a medium-extractiveness regime where suppression has
 *   increased over the interval (enforcement hardening in response to
 *   resistance). The measured theater_ratio rise indicates that an increasing
 *   share of central authority actions frame autonomy maintenance as security
 *   theater rather than genuine coordination.
 *
 * KEY AGENTS:
 *   - PRC central authority: sovereign power, acts as agenda-setter, can unilaterally reinterpret the framework but faces international and economic constraints if perceived to abandon substantive autonomy
 *   - Hong Kong institutional actors (judiciary, civil service, financial regulatory bodies): beneficiaries of the dual system; their preservation depends on demonstrating continued autonomy to international capital markets
 *   - Hong Kong civil liberties advocates and political activists: targets of suppression; experience framework renegotiations as sovereignty expansion, not negotiated boundary-shifting
 *   - Business sector (multinational firms, financial services, real estate): beneficiaries with exit options; their willingness to maintain operations is the economic constraint on unilateral sovereignty expansion
 *   - International treaty enforcement bodies (UN bodies, regional courts): excluded from negotiation; their absence legitimizes the framework as a bilateral arrangement rather than internationally enforceable commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.48).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.42).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems — Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '8c3b68b8-21a3-4914-a9ae-be9062d5efb1').
narrative_ontology:cs_kernel_codification('8c3b68b8-21a3-4914-a9ae-be9062d5efb1', fixed_text).
narrative_ontology:cs_authority_grounding('8c3b68b8-21a3-4914-a9ae-be9062d5efb1', lineage).
narrative_ontology:cs_interpretation_layer_present('8c3b68b8-21a3-4914-a9ae-be9062d5efb1').
narrative_ontology:cs_reading_relation('8c3b68b8-21a3-4914-a9ae-be9062d5efb1', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c3b68b8-21a3-4914-a9ae-be9062d5efb1', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('8c3b68b8-21a3-4914-a9ae-be9062d5efb1', foundational, neither_absolute_authority).
narrative_ontology:cs_axiom_status(neither_absolute_authority, holdable).
narrative_ontology:cs_axiom_grounding('8c3b68b8-21a3-4914-a9ae-be9062d5efb1', neither_absolute_authority, conventional).
narrative_ontology:cs_axiom('8c3b68b8-21a3-4914-a9ae-be9062d5efb1', foundational, boundary_contestation_legitimate).
narrative_ontology:cs_axiom_status(boundary_contestation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('8c3b68b8-21a3-4914-a9ae-be9062d5efb1', boundary_contestation_legitimate, deontological).
narrative_ontology:cs_reference_frame('8c3b68b8-21a3-4914-a9ae-be9062d5efb1', negotiated_autonomy_framework).
narrative_ontology:cs_drift_state('8c3b68b8-21a3-4914-a9ae-be9062d5efb1', contemporary_boundary_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8c3b68b8-21a3-4914-a9ae-be9062d5efb1', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_authority).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_institutional_continuity).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_liberties_advocates).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, international_treaty_enforcement_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_sector).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_institutional_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets national security parameters and territorial sovereignty claims; negotiates the boundaries of what Hong Kong autonomy means in practice; retains the power to reinterpret the framework but faces international scrutiny and economic consequences if perceived to have abandoned substantive autonomy. Draws legitimacy from nationalist sovereignty doctrine and constitutional supremacy, constrained by the negotiated nature of the arrangement.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_authority, agenda_setter,
    institutional, civilizational, trapped, global).

% Retains separate legal system, currency, and administrative apparatus; benefits from access to mainland Chinese market and security framework while maintaining international business hub status. Institutional actors (judicial system, civil service, business sector) are the seat that captures the arrangement's coordination benefits — dual access to markets, rule of law perception, capital flows.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_institutional_continuity, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_institutional_continuity, payer).

% Operate under contested boundaries between autonomy and sovereignty; face periodic crackdowns on press freedom, assembly rights, and political participation; bear the cost of renegotiation cycles where mainland authority expands the definition of national security. Exit is constrained by family ties, economic integration, and identity attachment to Hong Kong. Their resistance to framework reinterpretation is structural but diffuse.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_liberties_advocates, payer,
    organized, biographical, constrained, local).

% Would have standing to enforce treaty guarantees of Hong Kong autonomy and international human rights covenants if the framework treated them as binding arbiters. They are structurally excluded from the negotiation process; their role is reduced to observation and diplomatic pressure. They would contest the sovereignty-primacy reading but cannot enforce against the PRC's central authority.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_treaty_enforcement_bodies, excluded,
    institutional, generational, trapped, global).

% Benefits from the arrangement's guarantee of separate legal and financial systems; operates across both mainland and international markets with differential access to capital and regulatory regimes. Possesses exit options (financial relocation, investment diversification) that give them bargaining power in renegotiations. They have incentive to maintain the framework because it underpins their commercial position.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_sector, beneficiary,
    powerful, biographical, arbitrage, global).

% Cannot directly participate in the framework renegotiation; their interests are filtered through the PRC central authority's framing of national identity and security. Their presence is analytically important because the framework's legitimacy rests partly on mainland popular support for territorial integrity, which constrains how far the central authority can concede.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, mainland_civil_society, observer,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_authority).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__balanced_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a dual-system governance arrangement that allows Hong Kong to maintain institutional and legal continuity while integrating into the PRC's sovereign territorial and security framework. Solves the coordination problem of retaining international business hub function (requiring predictable law) while asserting national territorial reunion (requiring central sovereignty). Neither system could independently achieve what the negotiated framework delivers: Hong Kong alone could not command regional economic weight; mainland authority alone would sacrifice Hong Kong's international financial role.
% TRANSFER_FUNCTION: Transfers meaningful autonomy from Hong Kong's civil and political sphere to PRC central authority in areas designated as national security and territorial integrity. The transfer is contested: civil liberties advocates experience it as extraction (loss of political speech, assembly rights, judicial independence); institutional actors and business interests experience it as the acceptable boundary cost of the dual-system arrangement. Periodic renegotiations redistribute where the boundary sits.
% ABSENT_VOICES: International treaty enforcement bodies are structurally excluded; they would argue for enforceable Hong Kong autonomy guarantees but have no seat at the negotiation table. Hong Kong civil liberties advocates are seated but constrained by mainland authority's power to redefine the framework unilaterally; their contestation is registered but not dispositive.
% DISAPPEARANCE_RATIONALE: If the balanced coexistence framework disappeared, Hong Kong would either fall under direct mainland governance (losing separate legal/financial autonomy) or would seek independent statehood (triggering international crisis). The international business community would relocate financial operations; capital flows would restructure; mainland economic integration patterns would shift. The arrangement's disappearance would rearrange regional politics, international trade, and sovereignty doctrine.
% FOUNDING_PROBLEM: The 1997 handover created a structural tension: the PRC could not credibly commit to preserving Hong Kong's international hub function without delegating meaningful autonomy; Hong Kong could not function as an international financial center under direct mainland governance; neither party wanted formal partition or extended foreign control. One Country, Two Systems was the negotiated solution to this irreducible tension.
% FOUNDING_PROBLEM_CORROBORATION: The PRC central authority and Hong Kong institutional actors attest the founding problem remains live and the framework solves it. Civil liberties advocates attest the founding problem was partially solved but that the autonomy component has degraded over time. International observers (academic specialists, human rights organizations, financial analysts) outside the benefiting parties corroborate that the founding structural tension (integration vs. autonomy) persists and that the framework's stability has eroded; they disagree on whether this reflects legitimate boundary-renegotiation or unilateral sovereignty expansion.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the arrangement genuinely solves a coordination problem (dual-market access, institutional continuity) AND simultaneously extracts cost from those whose autonomy is constrained or whose resistance to sovereignty expansion is suppressed. Suppression is substantial (0.42) because the framework's stability depends on controlling civil resistance to boundary renegotiations and maintaining a credible threat of expanded central authority. The measurement series show a steady increase in both extractiveness and suppression over the interval, with the rise flattening after year 12 — consistent with a regime that reaches crisis thresholds and then stabilizes around a new equilibrium. Theater_ratio rises throughout (from 0.22 to 0.38), indicating that an increasing share of central authority enforcement is framed as security theater (ritual maintenance of the boundary) rather than genuine coordination management. The trajectory shows a tangled rope under stress: the coordination function persists (Hong Kong remains distinct from mainland governance), but the cost of maintaining it through periodic renegotiations and suppression is accumulating. The reading predicts that periodic crises will trigger renegotiations, after which the parties recalibrate the boundary; the framework persists because both institutional actors and the business sector retain bargaining power through international leverage and economic threat.
 *
 * PERSPECTIVAL GAP:
 *   The PRC central authority experiences the framework as a legitimate exercise of sovereign authority to protect territorial integrity and national security, constrained by Hong Kong's economic importance and international business confidence; from their seat, boundary-setting is not extraction but responsible governance of contested sovereignty. Civil liberties advocates experience the same actions as unilateral extraction of autonomy rights that were promised as substantive. The engine computes this divergence from structural data: the central authority's power, time horizon, and exit options (trapped — cannot exit sovereignty claims) push directionality toward the beneficiary end; advocates' constrained exit and organized resistance push directionality toward the target end. Both are correct from their positions; the constraint's extractiveness sits at the asymmetry between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC central authority: institutional power, civilizational time horizon, trapped exit — derives d near 0.0 (full beneficiary); the arrangement preserves their sovereignty claim and extracts no cost from them. Hong Kong institutional continuity: institutional power, generational time horizon, constrained exit — derives d near 0.4 (moderate target); they benefit from the dual system but bear the cost of boundary uncertainty and mandatory negotiation. Civil liberties advocates: organized power, biographical time horizon, constrained exit (family ties, economic integration) — derives d near 0.85 (strong target); they have no exit and bear suppression directly. Business sector: powerful actors, biographical time horizon, arbitrage exit options — derives d near 0.2 (weak target/strong beneficiary); they benefit from the arrangement and can threaten relocation if sovereignty expansion threatens their interests, which gives them leverage. The business sector's arbitrage exit is what constrains the central authority from unilateral sovereignty expansion; without that exit option, civil liberties advocates would be fully trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The balanced coexistence reading resists mandatrophy mislabeling by naming the GENUINE coordination function (dual-system stability, international financial hub) and the GENUINE extraction asymmetry (constrained autonomy, suppressed resistance). A sovereignty_primacy reading would classify this as pure hierarchical law (natural sovereignty doctrine), which would risk mislabeling coordination as mere authority assertion. An autonomy_primacy reading would minimize the extraction asymmetry and frame the arrangement as rule-governed constraint on sovereignty, which would miss the extractive dynamics of boundary renegotiation. This reading's claim (tangled rope) maps to its metrics (medium extraction + active enforcement + rising theater) because it refuses to dissolve either the coordination or the extraction into the other. The constraint is classified as tangled rope, not snare, because both parties retain some bargaining power (PRC through sovereign authority, Hong Kong/business through economic leverage) and periodic renegotiations produce settlements that are unstable but not one-way transfers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_stability_mechanism,
    'Is the framework''s stability sustained by genuine mutual interest in the dual system, or is it sustained primarily by the cost (to mainland) of international economic disruption if perceived sovereignty abandonment occurs?',
    'Natural experiment: if mainland authority tests unilateral sovereignty expansion in a low-economic-consequence domain (symbolic assertion without business impact), the response trajectory reveals whether cooperation is genuine coordination or coerced stability.',
    'If coerced stability, the extraction is higher than measured, and the framework should be reclassified toward snare; if genuine coordination, the tangled_rope classification stands. The mechanism also determines what happens if the business sector''s exit option credibility declines (if financial markets relocate or if mainland economic growth reduces Hong Kong''s distinctive value).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_stability_mechanism, empirical, 'Whether framework stability is mutual coordination or coerced through economic threat.').

omega_variable(
    suppression_internalization_structural_debate,
    'Is the measured suppression (0.42) primarily structural (legal restrictions, enforcement machinery) or increasingly internalized (advocates internalizing self-censorship, identity fusion with the constraint)?',
    'Post-exit trajectory: if advocates who leave Hong Kong report persistent self-censorship behavior after removal from enforcement context, suppression is partially internalized; if they adjust quickly to open environments, suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure (targets carry it with them); effective extraction rises. If structural, the constraint is more reversible through institutional change alone (repeal enforcement, introduce judicial independence protections). The trajectory informs whether the theater_ratio rise reflects ritual maintenance or genuine hardening of internalized compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_structural_debate, empirical, 'Structural vs. internalized suppression mechanism in the framework''s enforcement.').

omega_variable(
    reading_foreclosure_test,
    'Are the three sibling readings (autonomy_primacy, balanced_coexistence, sovereignty_primacy) genuinely coexisting as live positions within the framework, or does one reading''s institutional dominance foreclose the others in practice?',
    'Institutional representation audit: if all three readings have active institutional advocates with real power (in courts, legislatures, business bodies, civil society), they coexist; if one reading dominates institutional voice and others are suppressed, foreclosure is occurring de facto.',
    'If foreclosure occurs, one reading is not actually live and the framework should be understood as a single constraint under that reading, not as three competing constraints. This affects how manifested boundary disputes are classified: are they renegotiation crises under balanced_coexistence (normal), or are they crises of foreclosure where a suppressed reading is reasserting (sign of instability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, empirical, 'Whether sibling readings coexist as live institutional positions or are foreclosed in practice.').

omega_variable(
    treaty_enforcement_exclusion_mechanism,
    'Is the exclusion of international treaty enforcement bodies from the framework structural (by design of the bilateral arrangement) or contingent (international bodies lack enforcement capacity against sovereign states)?',
    'Counterfactual capacity test: if international enforcement mechanisms acquired binding power over sovereignty disputes (e.g., through credible supranational court with enforcement teeth), would the framework''s dynamics change? If yes, exclusion is structural design; if no, it is structural fact of sovereignty.',
    'If structural design, the framework is more extractive than measured (international actors who would constrain extraction are deliberately kept out); if structural fact, the extraction reflects the actual state of international authority, not the framework''s particular choice. This affects omega-C framing: is the framework designed to evade international scrutiny, or is it designed within the actual constraints of international law?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_enforcement_exclusion_mechanism, conceptual, 'Whether treaty enforcement exclusion is deliberate framework design or consequence of sovereignty structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(one__tr_t0, observed).
narrative_ontology:measurement(one__tr_t3, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement_basis(one__tr_t3, observed).
narrative_ontology:measurement(one__tr_t6, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement_basis(one__tr_t6, observed).
narrative_ontology:measurement(one__tr_t9, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 9, 0.33).
narrative_ontology:measurement_basis(one__tr_t9, observed).
narrative_ontology:measurement(one__tr_t12, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(one__tr_t12, observed).
narrative_ontology:measurement(one__tr_t15, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(one__tr_t15, observed).
narrative_ontology:measurement(one__tr_t27, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 27, 0.38).
narrative_ontology:measurement_basis(one__tr_t27, observed).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(one__be_t0, observed).
narrative_ontology:measurement(one__be_t3, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 3, 0.38).
narrative_ontology:measurement_basis(one__be_t3, observed).
narrative_ontology:measurement(one__be_t6, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(one__be_t6, observed).
narrative_ontology:measurement(one__be_t9, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 9, 0.46).
narrative_ontology:measurement_basis(one__be_t9, observed).
narrative_ontology:measurement(one__be_t12, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement_basis(one__be_t12, observed).
narrative_ontology:measurement(one__be_t15, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement_basis(one__be_t15, observed).
narrative_ontology:measurement(one__be_t27, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 27, 0.48).
narrative_ontology:measurement_basis(one__be_t27, observed).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(one__su_t0, observed).
narrative_ontology:measurement(one__su_t3, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 3, 0.32).
narrative_ontology:measurement_basis(one__su_t3, observed).
narrative_ontology:measurement(one__su_t6, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 6, 0.36).
narrative_ontology:measurement_basis(one__su_t6, observed).
narrative_ontology:measurement(one__su_t9, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 9, 0.39).
narrative_ontology:measurement_basis(one__su_t9, observed).
narrative_ontology:measurement(one__su_t12, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(one__su_t12, observed).
narrative_ontology:measurement(one__su_t15, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement_basis(one__su_t15, observed).
narrative_ontology:measurement(one__su_t27, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 27, 0.42).
narrative_ontology:measurement_basis(one__su_t27, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__balanced_coexistence_reading, 0.18).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the One Country, Two Systems kernel. The three readings are distinct constraint stories with different ε values, beneficiary structures, and classifications. (1) autonomy_primacy_reading: treats autonomy as treaty-guaranteed and internationally enforceable; lower extraction, tending toward rope or scaffold. (2) balanced_coexistence_reading (this story): treats sovereignty and autonomy as negotiated boundaries subject to periodic renegotiation; medium extraction, classified as tangled_rope. (3) sovereignty_primacy_reading: treats autonomy as delegated and revocable by central authority; higher extraction, tending toward snare. The readings are linked via network.affects_constraints because institutional positions that adopt one reading create pressure on the others; if autonomy_primacy gains international legal backing, it influences the conditions under which balanced_coexistence must operate. The boundary disputes within Hong Kong politics represent different institutional actors deploying these three readings against each other; the framework's stability depends on no single reading achieving institutional dominance that forecloses the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__balanced_coexistence_reading, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
