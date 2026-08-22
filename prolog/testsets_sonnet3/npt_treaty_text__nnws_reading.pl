% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Article VI as Binding Disarmament Obligation (NNWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This story authors the NNWS reading of the NPT kernel: Article VI is a
 *   binding legal obligation on nuclear-weapon states to negotiate
 *   disarmament in good faith toward a conclusion, and non-proliferation
 *   compliance by non-nuclear-weapon states is the conditional price paid for
 *   that eventual disarmament — not an unconditional constraint owed
 *   regardless of NWS behavior. This is one of three readings of the same
 *   treaty text; the sibling readings (NWS reading: non-proliferation as the
 *   binding constraint, disarmament as aspirational; withdrawal_threshold
 *   reading: Article X threshold interpretation) are separate constraints
 *   with their own ε and structure, not alternative measurements of this one.
 *   The NNWS reading's own metrics describe rising theater (0.58) as Review
 *   Conferences repeatedly produce consensus-blocked or watered-down outcome
 *   documents while NWS arsenals modernize, and moderate extraction (0.42)
 *   reflecting that the bargain still delivers real nonproliferation
 *   coordination value even as the disarmament half goes substantially
 *   unfulfilled.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.42).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.35).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Article VI as Binding Disarmament Obligation (NNWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, 'bcf73977-f9f0-4403-91ea-4cf1f88defdc').
narrative_ontology:cs_kernel_codification('bcf73977-f9f0-4403-91ea-4cf1f88defdc', fixed_text).
narrative_ontology:cs_authority_grounding('bcf73977-f9f0-4403-91ea-4cf1f88defdc', distributed).
narrative_ontology:cs_reading_relation('bcf73977-f9f0-4403-91ea-4cf1f88defdc', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('bcf73977-f9f0-4403-91ea-4cf1f88defdc', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('bcf73977-f9f0-4403-91ea-4cf1f88defdc', foundational, disarmament_is_legally_binding_not_aspirational).
narrative_ontology:cs_axiom_status(disarmament_is_legally_binding_not_aspirational, holdable).
narrative_ontology:cs_axiom_grounding('bcf73977-f9f0-4403-91ea-4cf1f88defdc', disarmament_is_legally_binding_not_aspirational, conventional).
narrative_ontology:cs_axiom('bcf73977-f9f0-4403-91ea-4cf1f88defdc', foundational, nonproliferation_compliance_is_conditional_consideration).
narrative_ontology:cs_axiom_status(nonproliferation_compliance_is_conditional_consideration, holdable).
narrative_ontology:cs_axiom_grounding('bcf73977-f9f0-4403-91ea-4cf1f88defdc', nonproliferation_compliance_is_conditional_consideration, conventional).
narrative_ontology:cs_reference_frame('bcf73977-f9f0-4403-91ea-4cf1f88defdc', grand_bargain_reciprocal_obligation_1968).
narrative_ontology:cs_drift_state('bcf73977-f9f0-4403-91ea-4cf1f88defdc', post_2022_review_conference_failure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bcf73977-f9f0-4403-91ea-4cf1f88defdc', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, global_nonproliferation_norm).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nnws_security_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, disarmament_as_legal_obligation).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, grand_bargain_reciprocity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Renounced acquisition of nuclear weapons in exchange for a textual commitment (Article VI) that nuclear weapon states would pursue negotiations toward disarmament. Press this reading at Review Conferences, through the NAM caucus, and via the TPNW as leverage. Cannot compel NWS compliance directly; their only tools are diplomatic pressure, review-cycle documentation of non-compliance, and the reputational and normative cost of parallel treaty regimes.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, beneficiary).

% Formally accept Article VI language but treat it as a process obligation (negotiate in good faith) rather than a results obligation (actually disarm). Retain full arsenals, modernize delivery systems, and face review-conference criticism without binding consequence. Their exit option from the NNWS reading's pressure is essentially unconstrained: no enforcement body can compel disarmament, and no NWS has ever faced material sanction for arsenal retention under NPT machinery.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, payer,
    institutional, civilizational, arbitrage, global).

% Populations of non-nuclear states live under continued great-power nuclear risk (accident, escalation, targeting) despite their states having given up the deterrent option in exchange for a disarmament promise that has not materialized in over five decades. They have no direct voice in Review Conference proceedings and bear the residual risk the bargain was meant to reduce.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nnws_security_populations, payer,
    powerless, civilizational, trapped, global).

% Convenes the five-year Review Conferences and drafts consensus outcome documents. Documents the gap between Article VI aspiration and NWS practice but has no independent enforcement authority; consensus rules let any single NWS block a critical final document, as occurred in 2005, 2015, and 2022.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, review_conference_secretariat, observer,
    institutional, biographical, analytical, global).

% A subset of NNWS concluded the NPT's disarmament promise was unenforceable and negotiated the Treaty on the Prohibition of Nuclear Weapons (2017) as a competing normative regime with an outright ban. They are not party to NPT Review Conference deliberations in the same institutional capacity as NPT-only states and are frequently characterized by NWS as undermining the 'step-by-step' NPT framework rather than legitimately exercising the Article VI logic to its conclusion.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, tpnw_states_parties, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nnws_reading, diffuse).
narrative_ontology:fixing_cost_class(npt_treaty_text__nnws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem in the original 1968 bargain: prevents a cascading multiplication of nuclear-armed states by getting the overwhelming majority of the world's states to forswear acquisition, in exchange for eventual arsenal reduction by the states that already possess weapons.
% TRANSFER_FUNCTION: Moves strategic restraint (the option value of acquiring a deterrent) from non-nuclear-weapon states to the international security order, in exchange for a promised future transfer of security (disarmament, and thus reduced existential risk) from nuclear-weapon states back to everyone. The NNWS reading holds this second transfer is legally owed, not merely hoped for.
% ABSENT_VOICES: The populations bearing residual nuclear risk have no seat at Review Conferences; TPNW states are marginalized within NPT proceedings despite acting on the NNWS reading's own logic; NWS domestic disarmament constituencies are structurally separated from the treaty's international enforcement mechanism entirely.
% DISAPPEARANCE_RATIONALE: If the Article VI obligation vanished from the discourse entirely, NWS practice would likely be materially unchanged (no enforcement currently constrains them), but the NNWS reading's disappearance would remove the last normative lever NNWS possess in Review Conferences and would likely accelerate TPNW-style regime defection, splitting the nonproliferation architecture. NWS would say the world is unchanged; NNWS and disarmament advocates would say the bargain's legitimacy collapses.
% FOUNDING_PROBLEM: In 1968, the international community faced a proliferation cascade risk: absent a treaty, many technologically capable states were expected to acquire nuclear weapons within a generation. The NPT was built to freeze the number of nuclear-armed states at five in exchange for a credible disarmament trajectory that would make the freeze durable and legitimate rather than merely coercive.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal scholars (e.g., the ICJ's 1996 Advisory Opinion, which found Article VI to include an obligation to pursue negotiations to a conclusion) and NAM/NNWS governments attest the disarmament obligation remains live and unmet. NWS governments attest the obligation is a process norm they have satisfied through arms-control dialogue (New START, bilateral reductions) even without abolition. No corroborating source outside the NWS governments themselves affirms that the substantive disarmament goal has been met.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, contested).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).
:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate rather than high because the coordination function (preventing proliferation cascade) remains genuinely operative and valued by most NNWS even absent NWS disarmament — this is not (yet, under this reading) pure extraction, it is a bargain with a partially defaul272ed-on counterparty. Suppression is moderate (0.35): NNWS are not coerced into staying in the treaty by force, but exit (withdrawal under Article X, or defection to acquisition) carries severe reputational and security costs, and the TPNW route is itself a form of managed, non-disruptive exit. Theater ratio rises sharply over the interval (0.25 to 0.58) as Review Conference outcome documents increasingly perform disarmament concern without producing binding steps — 2015 and 2022 both ended without consensus documents, and even successful documents (2000, 2010) contained commitments (the '13 steps', the 2010 action plan) substantially unimplemented by 2026.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-nuclear-weapon states are the coordinated party under this reading — they get real nonproliferation-regime membership benefits (assurance, technology-sharing under Article IV, diplomatic standing) but are also structurally the ones who gave up the option value and now wait on an unperformed promise, so they carry both beneficiary and payer character. NWS are payers under this reading specifically in the sense that Article VI imposes a legal obligation on them under the NNWS interpretation — but their actual exit options (arbitrage: they can absorb criticism, block consensus documents, and continue modernization without material cost) mean the constraint's effective force on them is low despite the reading's textual claim. This divergence between claimed obligation-bearer and actual structural leverage is the central fact this story is measuring.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proliferation cascade prevention) remains substantially live — the nonproliferation half of the bargain still functions and is widely credited with limiting the number of nuclear-armed states well below 1960s projections. But the disarmament half of the bargain, which the NNWS reading holds is legally required to complete the coordination logic, shows no comparable delivery: arsenals have been reduced from Cold War peaks but total elimination shows no credible trajectory. Classifying this as rope rather than tangled_rope acknowledges the coordination function is real and the extraction is asymmetric-but-moderate rather than severe and coercively maintained; it is not a snare because NNWS retain a genuine, exercised exit path (TPNW, and in principle Article X), and the arrangement is not held together by force. Whether the persistent non-delivery on Article VI eventually tips this reading toward tangled_rope (if enforcement pressure escalates) or toward piton (if disarmament is simply written off as archival language) is the open question the omega variables below address.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_vs_aspirational,
    'Is Article VI a legally binding obligation with a determinable content (as the ICJ''s 1996 Advisory Opinion and the NNWS reading hold), or a good-faith process norm satisfiable by continued dialogue without a disarmament outcome (the NWS reading)?',
    'There is no compulsory international adjudicative body that can resolve this for the parties; resolution would require either a binding ICJ contentious case (unlikely given NWS non-acceptance of jurisdiction on this question) or a convergent shift in state practice/opinio juris strong enough to settle customary international law.',
    'If Article VI is binding with determinate content, continued non-disarmament constitutes an ongoing treaty violation by NWS, which would push this constraint''s classification toward tangled_rope or snare as the extraction becomes actively enforced-against-denial rather than merely disputed. If it is aspirational, the NNWS reading''s higher extraction reading of NWS behavior is itself the constructed element, and the rope classification with lower ε (closer to the NWS reading''s framing) would be more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_binding_vs_aspirational, conceptual, 'Whether Article VI''s textual obligation has determinate binding content or is a satisfiable process norm — the central kernel dispute.').

omega_variable(
    review_conference_consensus_rule_capture,
    'Does the Review Conference consensus rule (allowing any single state, in practice functionally any NWS, to block outcome documents) represent a neutral procedural safeguard or a structural capture mechanism that lets NWS veto NNWS''s only institutional leverage point?',
    'Comparative institutional analysis: examine whether consensus-blocking correlates specifically with disarmament-critical language versus non-controversial procedural matters across the 2000-2022 Review Conference cycles.',
    'If capture, the theater_ratio trend (rising to 0.58) reflects genuine procedural extraction disguised as diplomatic process, supporting eventual reclassification toward tangled_rope. If neutral, the rising theater ratio reflects genuine multilateral difficulty rather than engineered blockage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_conference_consensus_rule_capture, empirical, 'Whether the consensus procedural rule is neutral or functions as an NWS veto over disarmament accountability.').

omega_variable(
    tpnw_defection_signal,
    'Does the emergence of the TPNW represent a healthy exit valve validating the NNWS reading is being taken to its logical conclusion by frustrated parties, or does it represent regime fragmentation that will ultimately weaken both treaties'' normative force?',
    'Track NPT Review Conference cohesion and NWS engagement over subsequent cycles (2026, 2031) for signs that TPNW membership growth correlates with reduced NPT-forum influence for disarmament-focused NNWS.',
    'If TPNW strengthens overall disarmament pressure, the NNWS reading''s exit options should be reassessed as closer to ''mobile'' with real leverage; if it fragments the regime and reduces total NNWS influence, exit options are more accurately ''constrained'' with declining leverage, which would raise the effective extraction this reading records.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tpnw_defection_signal, conceptual, 'Whether TPNW parallel regime formation strengthens or weakens the NNWS reading''s institutional leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1968, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_text__nnws_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nnws_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_text__nnws_reading, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(npt__tr_t2005, npt_treaty_text__nnws_reading, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_text__nnws_reading, theater_ratio, 2015, 0.53).
narrative_ontology:measurement(npt__tr_t2026, npt_treaty_text__nnws_reading, theater_ratio, 2026, 0.58).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_text__nnws_reading, base_extractiveness, 1968, 0.2).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nnws_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_text__nnws_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(npt__be_t2005, npt_treaty_text__nnws_reading, base_extractiveness, 2005, 0.36).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_text__nnws_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(npt__be_t2026, npt_treaty_text__nnws_reading, base_extractiveness, 2026, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(npt_treaty_text__nnws_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the NPT bargain' per the ε-invariance principle: nnws_reading (this file, moderate ε ~0.42, rope), nws_reading (non-proliferation binding/disarmament aspirational, expected different ε and likely different claimed type), and withdrawal_threshold_reading (Article X interpretation, a structurally distinct clause dispute). Each carries its own beneficiary/victim structure and its own ε; they are linked by shared kernel membership (npt_treaty_text) rather than by any shared metric.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
