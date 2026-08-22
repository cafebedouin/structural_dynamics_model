% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity Requirement — Veto-Trap Reading
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This story authors the veto-trap reading of the EU Council unanimity
 *   kernel: the requirement that certain decisions (CFSP actions, taxation
 *   harmonization, treaty revision, enlargement steps, some sanctions
 *   renewals) obtain the consent of every member state. On this reading,
 *   unanimity's original sovereignty-protection rationale has substantially
 *   decayed into a mechanism by which any single member state can credibly
 *   threaten to block a measure it may have no substantive objection to, in
 *   order to extract concessions — budget rebates, opt-outs, linked unrelated
 *   favors — from the coalition majority whose preference the measure
 *   reflects. The blocking state is the structural beneficiary; the coalition
 *   majority, the Commission (which must pre-negotiate around veto risk), and
 *   voiceless third parties (accession candidates, sanctions targets) bear
 *   the cost. This is NOT a story about whether unanimity is good or bad in
 *   the abstract — it is one reading among three of a single contested kernel
 *   (eu_council_unanimity). The sovereignty_guarantor_reading and
 *   diplomatic_capital_reading are separate constraint stories with their own
 *   ε and structural data; per the ε-invariance principle they are not
 *   blended into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.78).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.58).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity Requirement — Veto-Trap Reading").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, '2ab7c2a1-9d53-4c5f-916f-cf19a4f73e2d').
narrative_ontology:cs_kernel_codification('2ab7c2a1-9d53-4c5f-916f-cf19a4f73e2d', formalized).
narrative_ontology:cs_authority_grounding('2ab7c2a1-9d53-4c5f-916f-cf19a4f73e2d', distributed).
narrative_ontology:cs_reading_relation('2ab7c2a1-9d53-4c5f-916f-cf19a4f73e2d', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ab7c2a1-9d53-4c5f-916f-cf19a4f73e2d', eu_council_unanimity__diplomatic_capital_reading, influences).
narrative_ontology:cs_axiom('2ab7c2a1-9d53-4c5f-916f-cf19a4f73e2d', foundational, credible_blocking_threat_constitutes_extraction).
narrative_ontology:cs_axiom_status(credible_blocking_threat_constitutes_extraction, holdable).
narrative_ontology:cs_axiom_grounding('2ab7c2a1-9d53-4c5f-916f-cf19a4f73e2d', credible_blocking_threat_constitutes_extraction, empirically_contingent).
narrative_ontology:cs_axiom('2ab7c2a1-9d53-4c5f-916f-cf19a4f73e2d', secondary, concession_price_disproportionate_to_formal_vote_share).
narrative_ontology:cs_axiom_status(concession_price_disproportionate_to_formal_vote_share, holdable).
narrative_ontology:cs_axiom_grounding('2ab7c2a1-9d53-4c5f-916f-cf19a4f73e2d', concession_price_disproportionate_to_formal_vote_share, empirically_contingent).
narrative_ontology:cs_reference_frame('2ab7c2a1-9d53-4c5f-916f-cf19a4f73e2d', founding_six_sovereignty_consensus_norm).
narrative_ontology:cs_drift_state('2ab7c2a1-9d53-4c5f-916f-cf19a4f73e2d', post_2004_enlargement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2ab7c2a1-9d53-4c5f-916f-cf19a4f73e2d', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, eu_commission).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, third_country_partners_awaiting_accession_or_sanctions_action).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a single vote that is structurally equivalent in blocking weight to the entire rest of the Council on an area requiring unanimity. Signals it will withhold consent unless a specific concession — a budget carve-out, an opt-out, a side payment, a linked unrelated demand — is granted. Because the credible threat costs it almost nothing to make and nothing to sustain (delay is often domestically popular), it captures rents from every majority-preferred measure it can plausibly attach itself to.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_member_state, beneficiary,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, blocking_member_state, agenda_setter).

% Represent the overwhelming preference weight in the Council — economically, demographically, in prior negotiated compromise — but cannot act without the blocker's signature. Their only paths are to pay the extraction price (concessions), abandon the measure, or attempt an end-run through enhanced cooperation or intergovernmental treaty outside the EU framework, each of which is costly and slow. They bear the transfer that unanimity enables.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority_states, payer,
    organized, biographical, constrained, continental).

% Drafts and proposes measures requiring unanimous adoption (foreign policy, taxation, some enlargement and sanctions decisions) and must pre-negotiate around known blockers before formal proposal, often watering down or delaying initiatives preemptively. Bears the institutional cost of anticipatory concession-making even before a formal veto is threatened.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_commission, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, eu_commission, agenda_setter).

% Accession candidates and sanctioned-regime targets depend on Council unanimity for enlargement steps and sanctions renewal. A single member's unrelated bilateral grievance can freeze their status indefinitely; they have no seat at the table and no leverage of their own.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, third_country_partners_awaiting_accession_or_sanctions_action, payer,
    powerless, generational, trapped, continental).

% The founding negotiators who wrote unanimity into sensitive treaty articles intended it as a sovereignty safeguard for a community of a handful of founding members; they are not present to attest whether the mechanism still functions as designed at 27 members with structurally different blocking incentives, and their intent is invoked by all three readings without being testable against current practice.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_treaty_drafters_historical, excluded,
    institutional, civilizational, analytical, continental).

% Track veto and threatened-veto incidents, concessions granted, and their correlation with domestic electoral cycles in the blocking state, to determine whether blocking behavior tracks genuine sovereignty concerns or opportunistic extraction timed to leverage.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, political_economy_observers, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:fixing_cost_class(eu_council_unanimity__veto_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its residual, non-captured form, unanimity forces the Council to seek genuinely broad buy-in before acting on matters — foreign policy, taxation, treaty change — that touch core national prerogatives, which can produce more durable, less resented common action than a bare majority vote would.
% TRANSFER_FUNCTION: Moves policy content, budget allocations, side-payments, and procedural concessions from the preference of the coalition majority to the preference (or price) of the single blocking state, each time a credible veto threat is deployed on a measure the blocker does not intrinsically object to but can attach itself to.
% ABSENT_VOICES: Third-country accession candidates and sanctions-affected populations have no vote and no seat in the Council; their fate depends entirely on internal EU bargaining they cannot observe or influence, and they are the clearest bearers of cost with the least voice.
% DISAPPEARANCE_RATIONALE: If unanimity were replaced by qualified majority voting in the domains it currently governs, blocking states would lose the ability to extract concessions unrelated to the substance of a given measure; enlargement and sanctions decisions would move markedly faster; the Commission would stop pre-negotiating around single-state veto risk; and the EU's foreign policy and budget architecture would shift toward majority-coalition preferences.
% FOUNDING_PROBLEM: Post-war and early-integration treaty drafters needed a mechanism that would let sovereign states pool authority on sensitive matters (taxation, foreign policy, treaty amendment, enlargement) without any state fearing it could be outvoted into an outcome that touched its core sovereignty or vital national interest.
% FOUNDING_PROBLEM_CORROBORATION: Some member states and legal scholars attest the sovereignty-protection function remains live and necessary at 27 members with divergent security postures. Independent political-economy researchers and repeated Commission and European Parliament reports (which are not blocking-state beneficiaries) document a documented pattern of veto threats deployed on matters with no plausible sovereignty nexus — used instead as bargaining leverage for unrelated domestic or bilateral gain — supporting the contested, shifted-function reading this story authors.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.42 to 0.78) to model the documented pattern of increasingly frequent linkage of unrelated demands to unanimity votes as EU membership grew from a founding six to 27, diluting the sovereignty-protection rationale relative to the growing number of veto points available for opportunistic use. Theater ratio rises moderately (0.20 to 0.42) reflecting that formal justifications for blocking increasingly invoke sovereignty language even where post-hoc analysis of the demands made suggests bargaining opportunism rather than genuine sovereignty concern. Suppression is moderate (0.58 at end) — the mechanism does not physically coerce, but it does foreclose majority-preferred outcomes procedurally, and pre-negotiation dynamics increasingly suppress proposals before they are ever formally tabled.
 *
 * PERSPECTIVAL GAP:
 *   From the blocking state's own institutional seat, the veto is the exercise of a treaty right it is owed — indistinguishable, from inside that seat, from the sovereignty_guarantor_reading's account of the same formal act. From the coalition-majority seat, the identical vote is a toll extracted on a measure that already commanded overwhelming support. The engine computes these as structurally different outcomes from the same underlying data; this story's authored ε, beneficiary, and victim declarations reflect the veto-trap seat's reading specifically, not an adjudication between readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The blocking state sits at the beneficiary end: it holds arbitrage-grade exit (it can always let the measure fail at zero domestic political cost, or extract a price for consent) and captures a rent disproportionate to its formal vote share. The coalition majority and Commission sit at the target end: constrained exit (enhanced cooperation and treaty workarounds exist but are slow and partial), and they absorb the transfer through either concessions or abandoned initiatives. Third-country partners are the most trapped — they hold no vote in the mechanism that determines their fate and cannot exit a negotiation they are not party to.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting small or newly-joined states from majoritarian coercion on sovereignty-adjacent matters) is genuinely contested as live or dead: for some domains and some states it remains a real concern; for others, documented veto use tracks domestic electoral timing or unrelated bilateral disputes rather than any sovereignty threat from the specific measure blocked. Classifying this as tangled_rope rather than pure snare preserves the coordination residue (unanimity does sometimes force genuine consensus-building that a bare-majority system would skip) while still naming the asymmetric extraction that the veto-trap reading holds is now the dominant operative dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_trap_vs_sovereignty_guarantor_referent,
    'Is a given veto or veto threat episode better explained as a genuine sovereignty concern proportionate to the measure at hand, or as opportunistic extraction using sovereignty language as cover?',
    'Case-by-case comparison of the substantive content of the blocked measure against the concession ultimately extracted: proportionate, substantively-linked concessions support the sovereignty_guarantor_reading for that episode; unrelated or disproportionate concessions (side payments, unrelated policy linkage) support the veto_trap_reading. A corpus of past veto episodes coded this way would let the two readings be evaluated against the same evidence base rather than argued in the abstract.',
    'If most historical veto episodes code as proportionate, this reading''s high ε is overstated as a general characterization even though isolated extraction episodes are real; if most code as opportunistic, the veto_trap_reading''s ε is the more accurate general account of the mechanism''s current operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_trap_vs_sovereignty_guarantor_referent, empirical, 'Whether veto episodes are, in general, proportionate sovereignty defense or opportunistic extraction — the central empirical fork between this reading and its sovereignty_guarantor sibling.').

omega_variable(
    kernel_framing_under_determination,
    'Does the same formal rule (Council unanimity) admit three genuinely distinct structural framings (extraction mechanism, sovereignty safeguard, deliberation-forcing device), or is one framing the ''real'' structural fact and the others rationalizations layered over it?',
    'No single resolution mechanism exists because this is a conceptual framing question, not a factual one; the corpus approach here is to author each reading as its own constraint with its own ε and let cross-reading comparison (via network.affects_constraints) surface which reading better predicts observed bargaining outcomes over time.',
    'If the veto-trap framing is adopted as dominant, reform pressure toward qualified-majority voting gains normative support; if the sovereignty-guarantor framing dominates, unanimity retention is normatively supported despite its bargaining costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Documents that this story is one of three coherent framings of a single kernel, per the committer-frame rules; the choice of framing is not resolved by data internal to this story alone.').

omega_variable(
    growing_membership_dilution_effect,
    'Is the rising extractiveness trend (0.42 to 0.78) genuinely driven by EU enlargement increasing the number of independent veto points, or by a smaller number of states becoming more strategically sophisticated in deploying existing veto power?',
    'Track veto-threat frequency and concession size per capita of member states over the enlargement timeline (6 to 27 members) versus concentration of veto use among a subset of repeat-blocking states.',
    'If enlargement-driven, the mechanism''s extraction is a structural, mechanical consequence of scale and argues for QMV reform independent of any state''s behavior; if concentration-driven, the extraction is better addressed by targeted diplomatic or reputational pressure on specific repeat blockers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growing_membership_dilution_effect, empirical, 'Whether rising extraction over time is a scale effect of enlargement or a behavioral effect of specific actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__veto_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(eu_c_tr_t6, eu_council_unanimity__veto_trap_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(eu_c_tr_t12, eu_council_unanimity__veto_trap_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(eu_c_tr_t18, eu_council_unanimity__veto_trap_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement(eu_c_tr_t24, eu_council_unanimity__veto_trap_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__veto_trap_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__veto_trap_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(eu_c_be_t6, eu_council_unanimity__veto_trap_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(eu_c_be_t12, eu_council_unanimity__veto_trap_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(eu_c_be_t18, eu_council_unanimity__veto_trap_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(eu_c_be_t24, eu_council_unanimity__veto_trap_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__veto_trap_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__veto_trap_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(eu_c_su_t6, eu_council_unanimity__veto_trap_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(eu_c_su_t12, eu_council_unanimity__veto_trap_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(eu_c_su_t18, eu_council_unanimity__veto_trap_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(eu_c_su_t24, eu_council_unanimity__veto_trap_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(eu_c_su_t30, eu_council_unanimity__veto_trap_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_enlargement_accession_veto).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_sanctions_renewal_mechanism).

% DUAL FORMULATION NOTE:
% This story, eu_council_unanimity__sovereignty_guarantor_reading, and eu_council_unanimity__diplomatic_capital_reading form a three-member constraint family, each reading the identical formal Council unanimity rule with different beneficiary/victim structure and a different authored ε. Per the ε-invariance principle, these are not one constraint measured three ways but three constraints sharing one procedural kernel (eu_council_unanimity), linked here via affects_constraints rather than merged. Downstream, this veto-trap reading structurally influences the observed pace of EU enlargement accession decisions and sanctions renewal cycles, both of which require unanimity and are named as separate affected constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__veto_trap_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
