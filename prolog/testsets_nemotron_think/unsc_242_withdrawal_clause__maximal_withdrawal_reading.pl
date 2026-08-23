% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Withdrawal Clause — Maximal Reading (French Definite Article)
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   UN Security Council Resolution 242 (1967) is the foundational legal
 *   framework for Arab-Israeli peace. Its withdrawal clause — 'withdrawal of
 *   Israeli armed forces from territories occupied in the recent conflict'
 *   (English) vs 'retrait des forces armées israéliennes des territoires
 *   occupés' (French) — generates two structurally distinct constraints. This
 *   story instantiates the MAXIMAL WITHDRAWAL READING: the French definite
 *   article 'des' creates a mandatory obligation to withdraw from ALL
 *   occupied territories, anchored in the Charter Article 2(4) territorial
 *   integrity default. The reading binds the occupier (Israel) to full
 *   retrocession across all fronts (Sinai, Golan, West Bank, Gaza).
 *   Beneficiaries are the sovereign claimants and displaced populations with
 *   enforceable legal positions. Base extractiveness is high (0.72) because
 *   the constraint demands transfer of territory, strategic assets, and
 *   demographic control from a powerful occupier. The claimed_type is 'rope'
 *   (coordination of territorial integrity restoration), but the authored
 *   metrics describe a constraint with substantial extraction from the
 *   occupier and active enforcement dependence — the engine will compute the
 *   actual classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.72).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.55).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause — Maximal Reading (French Definite Article)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '4c0d5a70-f8d3-41df-a1f8-75a80f19fb04').
narrative_ontology:cs_kernel_codification('4c0d5a70-f8d3-41df-a1f8-75a80f19fb04', fixed_text).
narrative_ontology:cs_authority_grounding('4c0d5a70-f8d3-41df-a1f8-75a80f19fb04', lineage).
narrative_ontology:cs_interpretation_layer_present('4c0d5a70-f8d3-41df-a1f8-75a80f19fb04').
narrative_ontology:cs_reading_relation('4c0d5a70-f8d3-41df-a1f8-75a80f19fb04', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('4c0d5a70-f8d3-41df-a1f8-75a80f19fb04', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('4c0d5a70-f8d3-41df-a1f8-75a80f19fb04', foundational, french_text_definite_article_requires_full_withdrawal).
narrative_ontology:cs_axiom_status(french_text_definite_article_requires_full_withdrawal, holdable).
narrative_ontology:cs_axiom_grounding('4c0d5a70-f8d3-41df-a1f8-75a80f19fb04', french_text_definite_article_requires_full_withdrawal, conventional).
narrative_ontology:cs_axiom('4c0d5a70-f8d3-41df-a1f8-75a80f19fb04', foundational, territorial_integrity_default_binds_occupier).
narrative_ontology:cs_axiom_status(territorial_integrity_default_binds_occupier, holdable).
narrative_ontology:cs_axiom_grounding('4c0d5a70-f8d3-41df-a1f8-75a80f19fb04', territorial_integrity_default_binds_occupier, conventional).
narrative_ontology:cs_reference_frame('4c0d5a70-f8d3-41df-a1f8-75a80f19fb04', french_text_territorial_integrity_baseline).
narrative_ontology:cs_drift_state('4c0d5a70-f8d3-41df-a1f8-75a80f19fb04', contemporary_occupation_persistence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4c0d5a70-f8d3-41df-a1f8-75a80f19fb04', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, displaced_palestinians).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, egyptian_state_sinai).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, syrian_state_golan).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, jordanian_state_west_bank).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, israeli_occupier).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, us_hegemon_and_veto_holder).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, jordanian_state_west_bank).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, territorial_integrity_article_2_4).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, inadmissibility_of_territorial_acquisition_by_force).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, french_text_authentic_equality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls occupied territories (Sinai, Golan, West Bank, Gaza) militarily and administratively. Bears the cost of full withdrawal: strategic depth, settlements, water resources, security architecture. Exit from the constraint means rejecting UNSC authority or reinterpreting the text; constrained by US alliance, international legitimacy needs, and peace treaty obligations (Egypt, Jordan).
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, israeli_occupier, payer,
    powerful, biographical, constrained, national).

% Population displaced from or living under occupation in West Bank and Gaza. The maximal reading affirms their right to self-determination and return/sovereignty over all 1967 territories. No meaningful exit from the constraint's scope — their status is defined by it. Enforcement depends entirely on external actors (UN, Arab states, international opinion).
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, displaced_palestinians, beneficiary,
    powerless, generational, trapped, local).

% Sovereign claimant to Sinai Peninsula. Benefited from maximal reading in 1979 peace treaty (full Israeli withdrawal from Sinai). Has exit options: bilateral treaty route achieved full withdrawal; could have pursued unilateral or multilateral pressure. The constraint functioned as coordination for Sinai return.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, egyptian_state_sinai, beneficiary,
    moderate, biographical, mobile, regional).

% Sovereign claimant to Golan Heights. Maximal reading supports full withdrawal demand. Constrained exit: military asymmetry with Israel, dependence on great-power patronage (historically USSR, now Russia), no peace treaty achieved. Constraint remains aspirational for Golan.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, syrian_state_golan, beneficiary,
    moderate, biographical, constrained, regional).

% Former sovereign over West Bank (1948-1967), relinquished claim in 1988 in favor of PLO. Beneficiary of maximal reading for Palestinian statehood but bears costs as host to refugee population and frontline state. Constrained exit: demographic vulnerability, US alliance, peace treaty with Israel (1994) that sidesteps full 242 implementation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, jordanian_state_west_bank, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, jordanian_state_west_bank, payer).

% Author of Resolution 242 and subsequent resolutions (338, 446, 2334). Sets enforcement agenda through Chapter VII authority, but veto dynamics (US) limit active enforcement. The constraint's persistence depends on SC's continued rhetorical endorsement without coercive enforcement against a P5 ally.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_security_council, agenda_setter,
    institutional, generational, analytical, global).

% ICJ advisory opinions (Wall 2004, Kosovo 2010) and international legal scholarship predominantly endorse maximal reading: 'des territoires' = all territories; territorial integrity default; acquisition by force inadmissible. Their authority is interpretive, not enforcement. Exit is analytical — they produce readings, not outcomes.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj_and_legal_interpreters, observer,
    institutional, civilizational, analytical, global).

% Permanent SC member with veto; primary Israeli patron. Shapes enforcement: blocks Chapter VII action, promotes 'land for peace' bilateralism (Camp David, Oslo) that substitutes partial withdrawal for maximal reading. Benefits from managing the conflict as regional stabilizer. Arbitrage-grade exit: can reinterpret, delay, or substitute frameworks at will.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, us_hegemon_and_veto_holder, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, us_hegemon_and_veto_holder, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restores the territorial integrity default (UN Charter Article 2(4)) after the 1967 war by establishing a mandatory, comprehensive withdrawal obligation that coordinates the reversal of territorial acquisition by force across all occupied fronts simultaneously — Sinai, Golan, West Bank, Gaza.
% TRANSFER_FUNCTION: Transfers sovereign control and administrative authority over all territories occupied in June 1967 from the Israeli occupier to the sovereign claimants (Egypt, Syria, Jordan/Palestinians), backed by the collective security authority of the UNSC.
% ABSENT_VOICES: Israeli security establishment (genuine security concerns about 1967 lines), settlers and settlement movement (ideological/religious claim to Judea/Samaria), Palestinian rejectionists who view 242 as legitimizing Israel within 1948 lines. The former are structurally excluded from the constraint's beneficiary logic; the latter are excluded by the constraint's state-centric framing.
% DISAPPEARANCE_RATIONALE: If the maximal reading vanished overnight, the legal baseline would revert to the partial reading (discretionary withdrawal) or customary practice (retention of strategic territories). Israel would face no legal obligation for full withdrawal from Golan or West Bank; the territorial integrity default would be displaced by the secure boundaries principle. The occupation architecture would be legally entrenched rather than legally anomalous.
% FOUNDING_PROBLEM: The June 1967 war created a unified occupation of Egyptian, Syrian, and Jordanian territory by Israel. The international community needed a single legal framework that: (1) condemned territorial acquisition by force, (2) mandated comprehensive withdrawal as the price of peace, and (3) established secure and recognized boundaries for all states. Resolution 242 was the consensus instrument — its French text ('des territoires') encoded the maximal obligation.
% FOUNDING_PROBLEM_CORROBORATION: ICJ Wall Advisory Opinion (2004) and subsequent resolutions (2334, 2016) affirm the maximal reading as binding law. The drafting history (Caradon, Rostow, Jarring) is contested: British/US drafters later testified the English text ('territories') was deliberate; French and Soviet drafters insisted 'des territoires' was the agreed text. The corroboration split maps to the textual authenticity dispute — no external consensus resolves it.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the comprehensive transfer demanded: all 1967 territories, including strategic high ground (Golan), water aquifers (West Bank), and settlement blocs. Suppression (0.55) is moderate — enforcement relies on SC political pressure, not direct force; the US veto caps coercion. Theater ratio (0.42) captures the peace process industry (Oslo, Camp David, Annapolis) that performs negotiation while the occupation deepens. Accessibility collapse (0.58) — alternatives (land swaps, confederation, autonomy) exist but are constrained by the maximal reading's all-or-nothing logic. Resistance (0.68) — Israel has never accepted the maximal reading; partial compliance (Sinai) came via bilateral treaty, not SC enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli seat (payer, powerful, constrained exit), the constraint is experienced as extractive and illegitimate — a 'tangled rope' or 'snare' imposing unilateral withdrawal without reciprocal security guarantees. From the Palestinian seat (beneficiary, powerless, trapped), it is a 'rope' — the only legal structure affirming their territorial rights. From Egypt's seat (beneficiary, moderate, mobile), it functioned as a 'rope' that delivered Sinai. The engine computes this seat divergence from the structural data; the claimed 'rope' type reflects the authoring seat's view of the constraint's coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli occupier is the structural target (payer, d near 1.0): loses territory, security architecture, settlements, water. Exit is constrained — cannot leave UN system, depends on US cover. Displaced Palestinians are trapped beneficiaries (d near 0.0): the constraint is their primary legal anchor; no exit from its scope. Arab states (Egypt, Syria, Jordan) are mobile beneficiaries — Egypt exited via bilateral treaty; Syria and Jordan remain constrained. UNSC and US are agenda-setters with analytical/arbitrage exit — they shape enforcement but bear no direct cost. ICJ observers have analytical exit — they interpret but don't enforce.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reversing 1967 occupation) is contested: Israel and US argue security needs and demographic changes make full withdrawal obsolete; the legal majority (ICJ, UNGA, most states) holds the problem is live. The constraint persists because the mandate (territorial integrity) has not been fulfilled — not because it atrophied. Mandatrophy is not resolved; the arrangement is contested, not obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    french_vs_english_text_authority,
    'Which authentic text controls the withdrawal obligation — the French ''des territoires'' (all territories) or the English ''territories'' (some territories)?',
    'Vienna Convention on the Law of Treaties Article 33(4): when texts are equally authentic, the meaning that best reconciles them prevails. But the texts are irreconcilable on this point. Resolution requires either: (a) ICJ authoritative interpretation, (b) state practice consensus, (c) subsequent agreement of parties.',
    'If French controls, ε remains high (mandatory comprehensive withdrawal). If English controls, ε drops (discretionary withdrawal) and the constraint shifts toward partial_withdrawal_reading. The classification of the entire 242 regime hinges on this textual authority question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(french_vs_english_text_authority, conceptual, 'Irreconcilable authentic texts create a structural ambiguity that no subsequent practice has definitively resolved.').

omega_variable(
    security_council_enforcement_credibility,
    'Does the UNSC''s persistent failure to enforce Chapter VII measures against Israel for non-withdrawal degrade the constraint''s classification from rope to piton?',
    'Track the gap between SC rhetorical reaffirmation (annual resolutions, presidential statements) and coercive action (sanctions, Chapter VII authorization). If theater_ratio approaches 1.0 while extractiveness remains high, piton classification triggers.',
    'A piton classification would mean the constraint persists as legal theater — the mandatory withdrawal obligation is performed ritually (annual resolutions, ICJ opinions) while the occupation deepens. This would reframe the maximal reading as a degraded coordination mechanism maintained for legitimacy cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_council_enforcement_credibility, empirical, 'Whether the constraint''s enforcement machinery has atrophied into performance.').

omega_variable(
    reading_relations_structural_delta,
    'Does the maximal reading foreclose the partial reading within any single legal framework, or do they coexist as competing interpretive methodologies across different forums?',
    'Analyze whether any state or court has adopted a hybrid reading (e.g., ''French text controls for some fronts, English for others''). If hybrid readings exist, the relation is coexists_with; if all actors commit to one text or the other, it is forecloses.',
    'If forecloses, the kernel has a binary structure — the two readings cannot be simultaneously held in one framework. If coexists_with, the kernel is a persistent interpretive dispute with no structural resolution. This determines whether the CS engine treats them as mutually exclusive or parallel commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_structural_delta, conceptual, 'The logical relationship between the two textual readings of the same clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc242_maximal_tr_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(unsc242_maximal_tr_t1973, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1973, 0.2).
narrative_ontology:measurement(unsc242_maximal_tr_t1979, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1979, 0.25).
narrative_ontology:measurement(unsc242_maximal_tr_t1988, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1988, 0.3).
narrative_ontology:measurement(unsc242_maximal_tr_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1993, 0.38).
narrative_ontology:measurement(unsc242_maximal_tr_t2000, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(unsc242_maximal_tr_t2004, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2004, 0.41).
narrative_ontology:measurement(unsc242_maximal_tr_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(unsc242_maximal_be_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(unsc242_maximal_be_t1973, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1973, 0.6).
narrative_ontology:measurement(unsc242_maximal_be_t1979, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1979, 0.58).
narrative_ontology:measurement(unsc242_maximal_be_t1988, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1988, 0.65).
narrative_ontology:measurement(unsc242_maximal_be_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1993, 0.68).
narrative_ontology:measurement(unsc242_maximal_be_t2000, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(unsc242_maximal_be_t2004, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2004, 0.71).
narrative_ontology:measurement(unsc242_maximal_be_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(unsc242_maximal_su_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(unsc242_maximal_su_t1973, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1973, 0.45).
narrative_ontology:measurement(unsc242_maximal_su_t1979, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1979, 0.5).
narrative_ontology:measurement(unsc242_maximal_su_t1988, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1988, 0.52).
narrative_ontology:measurement(unsc242_maximal_su_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1993, 0.53).
narrative_ontology:measurement(unsc242_maximal_su_t2000, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement(unsc242_maximal_su_t2004, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2004, 0.55).
narrative_ontology:measurement(unsc242_maximal_su_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.1).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, camp_david_accords_sinai_withdrawal).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, oslo_accords_interim_arrangement).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj_wall_advisory_opinion_2004).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the unsc_242_withdrawal_clause kernel. The maximal reading (this file) takes the French definite article as controlling and instantiates a high-extraction mandatory withdrawal obligation. The partial_withdrawal_reading takes the English indefinite article and drafters' intent as controlling, instantiating a discretionary withdrawal constraint. The interpretive_authority_structure reading treats the authority to resolve the ambiguity as the primary constraint. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, institutional, 0.15).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
