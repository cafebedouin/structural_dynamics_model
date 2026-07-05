% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Provincial Constitutional Subordination Doctrine (No Inherent Sovereignty, Exit Requires Federal Consent)
 *   domain: political/constitutional/resource_governance
 *
 * SUMMARY:
 *   The constitutional_subordination reading treats provinces as legally
 *   constituted subdivisions of a single federal sovereign, not as sovereign
 *   entities that pooled authority by treaty. Under this reading, federal
 *   jurisdiction over interprovincial trade, resource transport, climate
 *   policy, and the equalization formula is a legitimate exercise of
 *   constitutionally granted authority (POGG, trade and commerce, criminal
 *   law power used for environmental regulation), and any provincial claim to
 *   unilateral exit is constitutionally null absent a negotiated,
 *   amendment-formula-compliant process. This reading is doctrinally dominant
 *   in federal courts and federal government practice, though politically
 *   contested by resource-exporting and separatist-leaning provinces.
 *
 * KEY AGENTS:
 *   - federal_government: agenda_setter (institutional/arbitrage) — administers the doctrine, sets national policy, controls amendment veto
 *   - equalization_receiving_provinces: beneficiary (organized/constrained) — net fiscal recipients under the doctrine's stability
 *   - resource_exporting_provinces: payer (powerful/constrained) — fund transfers, face federal transport/climate authority over their resource wealth
 *   - separatist_movements: payer (moderate/trapped) — unilateral exit foreclosed by Secession Reference
 *   - provincial_fiscal_autonomy_advocates: payer (organized/constrained) — contest federal spending-power intrusion into provincial jurisdiction
 *   - national_unity_constituencies: beneficiary (organized/constrained) — benefit from integration stability
 *   - constitutional_courts: observer (institutional/analytical) — adjudicate within the order they interpret
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.52).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.61).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.52).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Provincial Constitutional Subordination Doctrine (No Inherent Sovereignty, Exit Requires Federal Consent)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political/constitutional/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, '4ded046a-3fb1-48e3-a4fe-e0cd12476388').
narrative_ontology:cs_kernel_codification('4ded046a-3fb1-48e3-a4fe-e0cd12476388', formalized).
narrative_ontology:cs_authority_grounding('4ded046a-3fb1-48e3-a4fe-e0cd12476388', lineage).
narrative_ontology:cs_interpretation_layer_present('4ded046a-3fb1-48e3-a4fe-e0cd12476388').
narrative_ontology:cs_reading_relation('4ded046a-3fb1-48e3-a4fe-e0cd12476388', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('4ded046a-3fb1-48e3-a4fe-e0cd12476388', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('4ded046a-3fb1-48e3-a4fe-e0cd12476388', foundational, provinces_hold_no_inherent_sovereignty).
narrative_ontology:cs_axiom_status(provinces_hold_no_inherent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('4ded046a-3fb1-48e3-a4fe-e0cd12476388', provinces_hold_no_inherent_sovereignty, conventional).
narrative_ontology:cs_axiom('4ded046a-3fb1-48e3-a4fe-e0cd12476388', foundational, unilateral_secession_is_constitutional_nullity).
narrative_ontology:cs_axiom_status(unilateral_secession_is_constitutional_nullity, holdable).
narrative_ontology:cs_axiom_grounding('4ded046a-3fb1-48e3-a4fe-e0cd12476388', unilateral_secession_is_constitutional_nullity, conventional).
narrative_ontology:cs_axiom('4ded046a-3fb1-48e3-a4fe-e0cd12476388', secondary, federal_pogg_authority_encompasses_climate_and_resource_transport).
narrative_ontology:cs_axiom_status(federal_pogg_authority_encompasses_climate_and_resource_transport, holdable).
narrative_ontology:cs_axiom_grounding('4ded046a-3fb1-48e3-a4fe-e0cd12476388', federal_pogg_authority_encompasses_climate_and_resource_transport, instrumental).
narrative_ontology:cs_reference_frame('4ded046a-3fb1-48e3-a4fe-e0cd12476388', federal_supremacy_constitutional_order).
narrative_ontology:cs_drift_state('4ded046a-3fb1-48e3-a4fe-e0cd12476388', post_1998_secession_reference, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4ded046a-3fb1-48e3-a4fe-e0cd12476388', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, equalization_receiving_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, national_unity_constituencies).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_exporting_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provincial_fiscal_autonomy_advocates).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, peace_order_and_good_government_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constitutional order, adjudicates (via appointed courts and the amending formula) whether provincial claims to sovereignty or exit are legally cognizable, and sets national policy — equalization transfers, climate targets, resource-adjacent regulation of interprovincial trade and export infrastructure — under heads of power it reads as supreme. It selects judges, controls the reference-question process to the Supreme Court, and holds the amending-formula veto that makes unilateral provincial secession constitutionally null.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Receive net fiscal transfers under the equalization formula funded disproportionately by resource-revenue-rich provinces. Their political incentive is to affirm federal supremacy and the illegitimacy of unilateral exit, since exit by a net-contributing province would collapse the transfer base they depend on.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, equalization_receiving_provinces, beneficiary,
    organized, generational, constrained, national).

% Generate the resource revenue that funds equalization and national programs while their own resource and energy development is subject to federal environmental and climate review, pipeline/export approval, and carbon pricing floors. They hold s.92A ownership of resources in the ground but not control over interprovincial/export transport, environmental assessment triggers, or the constitutional right to withhold participation. Legal challenges (references, court rulings) have repeatedly affirmed federal jurisdiction over the transport and pricing dimensions, leaving province-level resource sovereignty claims structurally bounded.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_exporting_provinces, payer,
    powerful, generational, constrained, national).

% Argue provinces retain inherent or compact-based sovereignty sufficient to exit unilaterally on a clear referendum result. The constitutional subordination reading, upheld by the Supreme Court's Secession Reference framework, forecloses unilateral exit entirely — any departure requires negotiation and constitutional amendment consented to by the federal government and other provinces, which gives the federal government an effective veto over the terms and pace of any exit process.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements, payer,
    moderate, generational, trapped, regional).

% Argue that federal spending power intrudes into areas of exclusive provincial jurisdiction (health, resources, property and civil rights) via conditional transfers and national standards. They can litigate, negotiate side deals, or opt out administratively in narrow cases, but cannot alter the underlying doctrine that provincial jurisdiction is a delegation bounded by the constitution rather than a retained sovereign residue.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, provincial_fiscal_autonomy_advocates, payer,
    organized, biographical, constrained, national).

% Citizens and institutions whose economic, familial, and civic lives are organized around a single national market, currency, and citizenship. They benefit from the doctrine's stability — the absence of a standing secession threat protects cross-provincial investment, labor mobility, and shared federal programs (health transfers, pensions, national defense).
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, national_unity_constituencies, beneficiary,
    organized, civilizational, constrained, national).

% Adjudicate reference questions on the scope of provincial versus federal power and the legality of unilateral secession. Their rulings (e.g., the Secession Reference) are cited by all sides but the court sits within the federal constitutional order it interprets, which is itself part of the contested kernel.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__constitutional_subordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable framework for resolving jurisdictional disputes, distributing resource wealth via equalization, and coordinating national policy (climate, trade, defense, currency) across provinces with radically different economic bases — without this, interprovincial policy would require constant renegotiation of first principles.
% TRANSFER_FUNCTION: Moves fiscal capacity from resource-revenue-rich provinces to fiscally weaker provinces via equalization, moves regulatory authority over resource transport and environmental standards from provincial to federal hands, and moves the legal power to authorize provincial exit from the province itself to the federal government and the amending-formula partners.
% ABSENT_VOICES: Indigenous nations whose treaty and land relationships predate and are not fully subsumed by either federal or provincial sovereignty claims are largely absent from the compact-vs-subordination debate, which frames sovereignty as a two-party (federal/provincial) contest. Separatist constituencies are heard in referenda but their legal theory of sovereignty has been foreclosed by the Secession Reference rather than adjudicated on its merits in ordinary politics.
% DISAPPEARANCE_RATIONALE: If constitutional subordination doctrine were abandoned overnight in favor of a compact/residual-sovereignty reading, resource-exporting provinces could unilaterally withhold equalization contributions or exit the federation without federal consent, national climate and resource-transport policy would fragment along provincial lines, and the amending formula's veto function over secession would cease to bind — reorganizing federal-provincial fiscal and regulatory relationships entirely.
% FOUNDING_PROBLEM: At Confederation and through subsequent constitutional patriation (1867, 1982), the arrangement was built to prevent the federation from being a mere treaty of convenience that any signatory could exit at will, and to establish a durable, judicially enforceable hierarchy of authority capable of governing coast-to-coast economic and social integration.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court's Secession Reference (1998) is frequently cited by federal-government-aligned scholars as authoritative outside confirmation that unilateral secession is constitutionally impermissible and that provinces lack inherent sovereignty. However, constitutional historians and comparative federalism scholars outside government (and outside resource-exporting-province governments) are divided: some corroborate the subordination reading as the settled doctrinal position, while others — citing the compact theory's continued vitality in political rhetoric and the negotiated character of 1867 itself — regard the founding problem as still contested rather than resolved by judicial fiat.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects genuine fiscal transfer (equalization) and regulatory authority moving from resource-rich provinces and separatist constituencies toward federal coordination and fiscally weaker provinces — substantial but not maximal, since much of what moves funds real coordination (national markets, shared programs). Suppression (0.61) is markedly higher than extraction because the doctrine's persistence depends on active foreclosure of an alternative legal theory (compact federalism) via judicial doctrine (Secession Reference) and the amending formula's structural veto — this is a raw structural fact about how alternatives are closed off, not scaled by scope or power. Theater ratio (0.28) is moderate-low: the constitutional and judicial apparatus performs real adjudicative work, though ceremonial invocations of 'the constitution demands it' sometimes substitute for substantive negotiation on resource and climate disputes. Accessibility collapse (0.7) is high because, once the Secession Reference framework is understood, unilateral exit is not a live legal option for any province regardless of referendum outcome — the alternative (compact-based unilateral exit) has been doctrinally closed, though not physically or politically impossible to attempt.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's seat, this looks like a rope or tangled_rope solving a genuine coordination problem — a durable hierarchy of authority preventing federation-by-convenience collapse. From a resource-exporting province's seat, or a separatist movement's seat, the same structure looks like an enforced extraction of fiscal and regulatory authority dressed in constitutional inevitability language — closer to a tangled_rope leaning toward snare, given how completely the alternative (exit, or full resource sovereignty) has been foreclosed by doctrine rather than negotiated. The engine computes these divergent per-seat readings from the same structural data; this file does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government sits at the beneficiary/agenda-setter end: it collects legal supremacy, controls the veto over exit terms, and directs use of resource-revenue-funded transfers. Equalization-receiving provinces and national-unity constituencies are structural beneficiaries of a doctrine that stabilizes the transfer base and integrated market they depend on. Resource-exporting provinces are targets: they fund the equalization system and face federal authority over the very resource and export infrastructure s.92A nominally assigns them, with constrained (not trapped) exit — they can litigate, negotiate side deals, or pursue political change, but cannot exit the doctrine unilaterally. Separatist movements are the clearest targets, with exit options coded trapped: the doctrine's central function, from their seat, is to make their preferred outcome (unilateral secession) constitutionally impossible.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing the federation from dissolving over policy disagreement — remains partially live (interprovincial economic integration is real and valuable) but is contested as to whether the specific mechanism (doctrinal foreclosure of unilateral exit, rather than negotiated confederal renewal) is still the right instrument or has calcified into a one-way ratchet that primarily protects the federal government's own authority and the equalization-receiving coalition's fiscal position. The status is authored as contested rather than dead or live because corroboration outside the benefiting parties (federal government, equalization-receiving provinces) is genuinely split among constitutional scholars.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_provincial_sovereignty,
    'Is the constitutional_subordination reading of the provincial_sovereignty_boundary kernel the structurally correct account of Confederation, or is it one contested legal doctrine among at least two others (compact_federalism, resource_sovereignty_primacy) with equal historical claim?',
    'No single resolution mechanism exists because this is a live constitutional-theoretic dispute; comparative analysis of the 1867 negotiating record, the 1982 patriation process (including provincial non-consent by Quebec), and subsequent case law (Secession Reference, resource-transport rulings) would bear on it but would not settle it, since each reading interprets the same historical record differently.',
    'If compact_federalism is the structurally correct reading, then what this story codes as legitimate federal authority (extraction of 0.52) is better coded as extraction closer to a snare from the perspective of provinces that never consented to subordination; if resource_sovereignty_primacy is correct, federal transport/climate authority over provincial resources is itself the extractive mechanism rather than a legitimate exercise of POGG.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_provincial_sovereignty, conceptual, 'Which of three sibling readings of the sovereignty kernel is structurally authoritative is irreducibly contested.').

omega_variable(
    secession_reference_as_settlement_or_capture,
    'Does the Supreme Court''s Secession Reference constitute a neutral judicial settlement of the sovereignty question, or is it itself an instance of the federal government''s own courts adjudicating in favor of federal supremacy — i.e., is the interpretive authority captured by the party whose authority is being interpreted?',
    'Comparative analysis of how the Court was constituted (federal appointment power), and comparison with how other federations (with judicially independent secession doctrines, e.g., no equivalent case) resolve similar disputes, would provide partial evidence but not a clean test given the absence of a truly external adjudicator.',
    'If the Court''s ruling is best read as captured, the interpretation_layer_present designation in cs_structure should be read with an authority_grounding closer to extraction rather than neutral lineage/expertise, and the suppression metric (0.61) may understate how much of the doctrine''s force is self-referential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secession_reference_as_settlement_or_capture, conceptual, 'Whether the judicial interpreter of the kernel is structurally independent of the federal party whose authority it interprets.').

omega_variable(
    equalization_formula_beneficiary_stability,
    'Would equalization-receiving provinces continue to support the constitutional_subordination reading if the formula shifted to make them net contributors, or is their support for federal supremacy doctrine conditional on their current fiscal position?',
    'Track voting and rhetorical patterns of provinces that have shifted between net-recipient and net-contributor status over time (e.g., resource-price-driven fiscal capacity swings) and observe whether their doctrinal alignment shifts correspondingly.',
    'If support is fiscally conditional rather than principled, this strengthens the tangled_rope reading (coordination function is real, but the doctrine is also instrumentalized by whichever provinces currently benefit) over a pure-mountain reading of federal supremacy as simply constitutionally given.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equalization_formula_beneficiary_stability, empirical, 'Whether beneficiary-province support for the doctrine tracks fiscal self-interest or constitutional principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 1867, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1867, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1867, 0.15).
narrative_ontology:measurement(prov_tr_t1930, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1930, 0.17).
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1982, 0.2).
narrative_ontology:measurement(prov_tr_t1998, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1998, 0.24).
narrative_ontology:measurement(prov_tr_t2015, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(prov_tr_t2025, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(prov_be_t1867, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1867, 0.32).
narrative_ontology:measurement(prov_be_t1930, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1930, 0.36).
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1982, 0.4).
narrative_ontology:measurement(prov_be_t1998, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1998, 0.46).
narrative_ontology:measurement(prov_be_t2015, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2015, 0.49).
narrative_ontology:measurement(prov_be_t2025, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1867, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1867, 0.35).
narrative_ontology:measurement(prov_su_t1930, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1930, 0.4).
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1982, 0.48).
narrative_ontology:measurement(prov_su_t1998, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1998, 0.58).
narrative_ontology:measurement(prov_su_t2015, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(prov_su_t2025, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2025, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__resource_sovereignty_primacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, equalization_transfer_formula).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_climate_policy_authority).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the provincial_sovereignty_boundary kernel. constitutional_subordination (this file) treats provinces as constitutionally created subdivisions with no inherent sovereignty and exit requiring federal consent; compact_federalism treats Confederation as a compact among sovereign provinces retaining residual sovereignty with exit negotiable under duress; resource_sovereignty_primacy treats provincial resource ownership under s.92A as grounding territorial sovereignty sufficient to resist federal transport/climate authority. Each reading has its own epsilon and beneficiary/victim structure; they are linked here rather than merged because merging would violate epsilon-invariance — the three readings genuinely disagree about who benefits and who pays, not merely about how to describe one agreed structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
