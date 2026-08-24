% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__interpretive_authority_structure, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: UNSC 242 Withdrawal Clause Interpretive Authority Contest
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) calls for 'withdrawal of Israeli armed forces
 *   from territories occupied in the recent conflict.' The English text uses
 *   the indefinite 'territories'; the equally authentic French text uses the
 *   definite 'les territoires.' This textual ambiguity has generated three
 *   competing readings: maximal withdrawal (all territories), partial
 *   withdrawal (some territories), and — the subject of this constraint — the
 *   interpretive authority structure itself. The authority to resolve the
 *   ambiguity is contested among the ICJ (judicial interpretation), drafting
 *   states (authorial intent), and the occupying state (customary practice).
 *   No single interpreter has binding authority; the P5 veto prevents
 *   Security Council enforcement of any interpretation. The meta-dispute
 *   functions as a snare: it extracts compliance costs from parties seeking
 *   legal closure while benefiting those with veto or non-cooperation
 *   capacity. The longer the authority contest persists, the more the
 *   substantive ambiguity becomes entrenched as a structural fact.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.82).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.78).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.82).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "UNSC 242 Withdrawal Clause Interpretive Authority Contest").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, 'ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0').
narrative_ontology:cs_kernel_codification('ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0', formalized).
narrative_ontology:cs_authority_grounding('ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0', extraction).
narrative_ontology:cs_interpretation_layer_present('ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0').
narrative_ontology:cs_reading_relation('ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0', unsc_242_withdrawal_clause__partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_axiom('ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0', foundational, interpretive_authority_is_contested).
narrative_ontology:cs_axiom_status(interpretive_authority_is_contested, holdable).
narrative_ontology:cs_axiom_grounding('ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0', interpretive_authority_is_contested, conventional).
narrative_ontology:cs_axiom('ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0', foundational, veto_power_blocks_interpretive_finality).
narrative_ontology:cs_axiom_status(veto_power_blocks_interpretive_finality, holdable).
narrative_ontology:cs_axiom_grounding('ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0', veto_power_blocks_interpretive_finality, conventional).
narrative_ontology:cs_reference_frame('ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0', charter_based_interpretive_order).
narrative_ontology:cs_drift_state('ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ac8ea9a1-6c3a-4b80-b37b-845c22ebf0c0', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, security_council_permanent_members).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_israel).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_uk_us).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, parties_seeking_legal_closure).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, disputed_territory_populations).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_israel).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, interpretive_authority_is_contested).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, textual_ambiguity_perpetuates_substantive_dispute).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, veto_power_blocks_interpretive_finality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims judicial authority to interpret UNSC 242 through advisory opinions and contentious cases. Lacks enforcement power; its interpretations are authoritative only when accepted by parties. The 2004 Wall Advisory Opinion affirmed withdrawal obligation but did not resolve the authority contest.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice, observer).

% Hold veto power over any binding Security Council action that would definitively resolve the interpretive contest. The P5 (US, UK, France, Russia, China) benefit from maintaining interpretive flexibility — each can block resolutions that would constrain their strategic interests or those of allies. The US consistently vetoes resolutions that would impose a maximal withdrawal reading.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, security_council_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, security_council_permanent_members, beneficiary).

% Claims customary practice and security needs justify retention of strategic territories. Benefits from the authority vacuum: no definitive interpretive ruling can be enforced against it while the P5 veto and the meta-dispute persist. Pays diplomatic and legitimacy costs but calculates these as lower than the cost of full withdrawal.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_israel, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_israel, payer).

% Original drafters (UK, US) claim authorial intent supports partial withdrawal reading (English text 'withdrawal from territories' vs French 'withdrawal from the territories'). Benefit from the ambiguity they created: it preserves strategic flexibility for allies and prevents the resolution from becoming a rigid legal constraint. Their subsequent diplomatic practice reinforces the partial reading.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_uk_us, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_uk_us, beneficiary).

% Arab states, Palestinian leadership, and non-aligned movement seek a definitive legal interpretation that would mandate full withdrawal. They are structurally excluded from the interpretive authority: no standing in ICJ without state consent, blocked by P5 veto in Security Council, and their proposed resolutions are defeated or vetoed. Bear the ongoing costs of occupation without legal remedy.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, parties_seeking_legal_closure, payer,
    organized, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, parties_seeking_legal_closure, excluded).

% Palestinian and Syrian populations in West Bank, Gaza, Golan Heights. Bear the direct human costs of the authority contest: settlement expansion, movement restrictions, legal limbo. Have no voice in the interpretive dispute — not parties to the resolution, no standing in international tribunals, no veto power. Their situation is the factual ground on which the interpretive contest plays out.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, disputed_territory_populations, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, disputed_territory_populations, excluded).

% The normative system of treaty interpretation (VCLT Articles 31-33) presupposes that textual ambiguity can be resolved through established methods. The UNSC 242 authority contest demonstrates a structural failure: when the interpreters themselves are the interested parties, the interpretive machinery produces perpetual contestation rather than closure. The system pays in legitimacy erosion and precedent degradation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_order, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_order, observer).
narrative_ontology:stakeholder_non_agent(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_order).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The interpretive authority contest is a coordination failure: the VCLT framework for treaty interpretation assumes a community of interpreters applying shared rules, but here the interpreters (ICJ, P5, parties) are themselves the disputants with irreconcilable institutional interests. The contest prevents the coordination function of legal interpretation — producing authoritative meaning — from operating.
% TRANSFER_FUNCTION: Moves interpretive authority from a hypothetical neutral arbiter to the veto-holding parties. Each P5 member holds a structural veto over any binding interpretive act. The occupying state holds a de facto veto through non-cooperation capacity. The transfer is from the international legal order (which would provide definitive interpretation) to the parties with blocking power, who use it to maintain strategic ambiguity.
% ABSENT_VOICES: The populations of the occupied territories (West Bank, Gaza, Golan Heights) — they live the consequences daily but have no standing in any interpretive forum. Future generations who will inherit the unresolved territorial status. The International Law Commission and treaty bodies — their interpretive guidance is sidelined by the political veto structure.
% DISAPPEARANCE_RATIONALE: If the interpretive authority contest vanished — i.e., if a definitive interpretive mechanism with binding force were established — the substantive ambiguity (maximal vs partial withdrawal) would be resolved. This would either mandate full Israeli withdrawal (rearranging the territorial status quo) or legitimize partial retention (rearranging the legal framework). The current stalemate depends entirely on the meta-dispute persisting.
% FOUNDING_PROBLEM: Resolve the territorial consequences of the 1967 Six-Day War through Israeli withdrawal from occupied territories in exchange for peace and secure boundaries, establishing a just and lasting peace in the Middle East.
% FOUNDING_PROBLEM_CORROBORATION: UNSC Resolution 242 text and negotiating history (corroborated by UN archives, UK/US diplomatic records). ICJ 2004 Wall Advisory Opinion (authoritative interpretation from outside the benefiting parties). Historical scholarship (Rostow, Stone, Dinstein) confirming the founding problem was territorial withdrawal for peace. The P5 and Israel contest whether the problem persists or has been superseded by subsequent agreements (Oslo, Camp David) — but no external corroboration supports the claim that the founding problem is 'dead'.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.82) because the authority vacuum allows the occupying state to maintain territorial control while the P5 avoid enforcement costs — the ambiguity itself is the extraction mechanism. High suppression (0.78) because the veto structure actively blocks any binding interpretive act; parties seeking closure are not merely disadvantaged but structurally excluded from the interpretive process. Moderate theater ratio (0.48) because legal processes (ICJ opinions, UN debates, diplomatic negotiations) continue but produce no binding resolution — they perform the appearance of legal order while the substantive outcome is determined by power. High accessibility collapse (0.75) because once the authority contest is recognized, no alternative interpretive pathway exists within the system; the VCLT rules presuppose a community of interpreters that does not exist here. Moderate resistance (0.52) because resistance takes diplomatic/legal forms rather than forceful challenge to the interpretive structure itself.
 *
 * PERSPECTIVAL GAP:
 *   From the P5/Israel perspective, the interpretive contest is a feature — it preserves strategic flexibility and prevents imposed outcomes. From the ICJ perspective, it is a failure of the legal system to produce authoritative interpretation. From the victim perspective, it is a structural denial of legal remedy. The engine will compute these as different constraint types per seat: snare for victims, rope-like for beneficiaries (coordination of non-action), mountain-adjacent for the legal order (the VCLT framework persists regardless).
 *
 * DIRECTIONALITY LOGIC:
 *   P5 members and Israel are structural beneficiaries (d near 0.0-0.2): they hold veto/non-cooperation capacity that lets them extract strategic value from the ambiguity. The ICJ sits near symmetric (d ~0.5): it performs the interpretive function but lacks enforcement — its authority is real but incomplete. Parties seeking closure and territory populations are full targets (d near 1.0): they bear all costs (occupation, legal limbo, diplomatic marginalization) with no structural capacity to alter the interpretive framework. The international legal order as a non-agent entity bears legitimacy costs but cannot 'exit' the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (territorial withdrawal for peace) remains contested: Israel and US argue subsequent bilateral agreements (Oslo, Camp David, Abraham Accords) have superseded 242; Arab states, Palestinians, ICJ, and most legal scholars argue the core obligation remains unfulfilled. The interpretive authority contest IS the mandatrophy mechanism: by preventing definitive interpretation, it allows the arrangement to persist without ever resolving whether its founding purpose is live or dead. The constraint has outlived its coordination function (if any) and persists purely through the extraction enabled by authority ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading of the contested kernel ''unsc_242_withdrawal_clause''. The sibling readings are ''maximal_withdrawal_reading'' and ''partial_withdrawal_reading''. What is the structural relationship between this meta-level authority contest reading and the two substantive readings?',
    'Map the constraint family: this reading''s extraction mechanism (authority ambiguity) is the enabler of the sibling readings'' persistence. If a definitive interpretive authority were established, one sibling reading would be foreclosed. The engine''s contamination propagation via network.affects_constraints should capture this.',
    'If this reading''s snare classification is correct, then the sibling readings'' persistence is not evidence of their individual coherence but of the meta-structure that prevents either from being definitively resolved. This reframes the kernel from ''three competing interpretations'' to ''one structural trap producing three live positions''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer frame: kernel/reading decomposition and structural relationship to siblings').

omega_variable(
    authority_ambiguity_structural_or_strategic,
    'Is the interpretive authority contest a structural feature of the UN Charter system (veto + no compulsory ICJ jurisdiction) or a strategic behavior by the P5 and Israel to maintain ambiguity?',
    'Counterfactual analysis: if the Security Council had no veto, or if ICJ jurisdiction were compulsory for treaty interpretation, would the authority contest persist? Compare with other UNSC resolutions where interpretive authority was not contested.',
    'If structural, the snare is a systemic property of the international legal order — not remediable without Charter reform. If strategic, the snare is maintained by specific actors and could be disrupted by political pressure or institutional innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_ambiguity_structural_or_strategic, empirical, 'Structural vs strategic origin of the interpretive authority vacuum').

omega_variable(
    suppression_mechanism_veto_vs_internalized,
    'Is the suppression of parties seeking legal closure primarily structural (veto power, lack of standing) or partially internalized (acceptance of the ''peace process'' framework that treats 242 as a negotiating basis rather than a legal obligation)?',
    'Post-exit trajectory analysis: if a binding interpretive ruling were issued tomorrow, would the victim parties accept and enforce it, or have they internalized the ambiguity as the only available framework? Track diplomatic rhetoric shifts from ''legal right'' to ''negotiated solution''.',
    'If internalized suppression is significant, the constraint''s effective suppression exceeds the structural measure — the victims carry the suppression with them even if the veto structure changed. This would increase the snare''s effective extraction for the victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_veto_vs_internalized, empirical, 'Structural vs internalized suppression in the interpretive authority contest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc242_ia_tr_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(unsc242_ia_tr_t1973, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1973, 0.2).
narrative_ontology:measurement(unsc242_ia_tr_t1982, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1982, 0.28).
narrative_ontology:measurement(unsc242_ia_tr_t1993, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(unsc242_ia_tr_t2000, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(unsc242_ia_tr_t2004, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2004, 0.45).
narrative_ontology:measurement(unsc242_ia_tr_t2011, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2011, 0.47).
narrative_ontology:measurement(unsc242_ia_tr_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(unsc242_ia_be_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(unsc242_ia_be_t1973, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1973, 0.45).
narrative_ontology:measurement(unsc242_ia_be_t1982, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement(unsc242_ia_be_t1993, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1993, 0.62).
narrative_ontology:measurement(unsc242_ia_be_t2000, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(unsc242_ia_be_t2004, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2004, 0.75).
narrative_ontology:measurement(unsc242_ia_be_t2011, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2011, 0.78).
narrative_ontology:measurement(unsc242_ia_be_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(unsc242_ia_su_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(unsc242_ia_su_t1973, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1973, 0.5).
narrative_ontology:measurement(unsc242_ia_su_t1982, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1982, 0.58).
narrative_ontology:measurement(unsc242_ia_su_t1993, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1993, 0.65).
narrative_ontology:measurement(unsc242_ia_su_t2000, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(unsc242_ia_su_t2004, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2004, 0.73).
narrative_ontology:measurement(unsc242_ia_su_t2011, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2011, 0.76).
narrative_ontology:measurement(unsc242_ia_su_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.1).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the UNSC 242 withdrawal clause constraint family. The kernel is the resolution text itself; the three readings are structurally distinct constraints with different ε values. This reading (interpretive authority contest) has high ε (0.82) because the meta-dispute perpetuates substantive ambiguity. The maximal reading has low ε (mountain-like) because it claims a single definitive legal answer. The partial reading has moderate ε (tangled_rope) because it coordinates a diplomatic framework while extracting territorial retention. This reading's authority vacuum is what allows both substantive readings to remain live simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__interpretive_authority_structure, institutional, 0.15).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__interpretive_authority_structure, powerful, 0.25).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__interpretive_authority_structure, organized, 0.85).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__interpretive_authority_structure, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
