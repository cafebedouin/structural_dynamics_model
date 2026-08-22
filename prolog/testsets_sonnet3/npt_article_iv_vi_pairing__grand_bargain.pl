% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__grand_bargain, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Grand Bargain Reading: Article IV/VI as Reciprocal, Conditional Obligations
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This story instantiates the 'grand bargain' reading of the NPT's Article
 *   IV/Article VI kernel: the treaty is a single reciprocal bargain in which
 *   non-weapon states' permanent restraint from acquiring weapons is
 *   conditional on weapon states' good-faith progress toward disarmament, and
 *   breach of Article VI obligations undermines the legitimacy (though not
 *   necessarily the legal force) of continued Article IV restraint
 *   expectations. This is one reading among three of the same textual kernel;
 *   the nonproliferation_primary reading treats Article VI as aspirational
 *   and non-justiciable, and the abolitionist reading treats Article IV
 *   itself as illegitimate. Each reading is authored as its own constraint
 *   with its own epsilon; this file does not average across them.
 *
 * KEY AGENTS:
 *   - weapon_states_nwsp5: agenda-setting beneficiary, institutional power, arbitrage exit — sets pace of disarmament and controls review conference outcomes
 *   - non_weapon_state_parties: primary payer, moderate power, constrained exit — bears indefinite restraint cost without reciprocal disarmament delivery
 *   - civil_nuclear_energy_aspirants: secondary payer/beneficiary, moderate power, constrained exit — access to Article IV benefits gated by supplier discretion
 *   - peaceful_nuclear_technology_exporters: beneficiary, powerful, mobile exit — profits from controlling technology transfer terms
 *   - iaea_and_treaty_secretariat: observer, institutional, analytical exit — documents but cannot adjudicate reciprocity breach
 *   - non_aligned_movement_coalition: excluded advocate, organized power, constrained exit — proposes enforceable reciprocity mechanisms repeatedly deflected
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.61).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.52).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.61).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Grand Bargain Reading: Article IV/VI as Reciprocal, Conditional Obligations").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, 'b2f8975f-9c47-4fa3-83da-1bb548bf4c3a').
narrative_ontology:cs_kernel_codification('b2f8975f-9c47-4fa3-83da-1bb548bf4c3a', fixed_text).
narrative_ontology:cs_authority_grounding('b2f8975f-9c47-4fa3-83da-1bb548bf4c3a', distributed).
narrative_ontology:cs_reading_relation('b2f8975f-9c47-4fa3-83da-1bb548bf4c3a', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('b2f8975f-9c47-4fa3-83da-1bb548bf4c3a', npt_article_iv_vi_pairing__abolitionist, influences).
narrative_ontology:cs_axiom('b2f8975f-9c47-4fa3-83da-1bb548bf4c3a', foundational, article_vi_iv_mutual_conditionality).
narrative_ontology:cs_axiom_status(article_vi_iv_mutual_conditionality, holdable).
narrative_ontology:cs_axiom_grounding('b2f8975f-9c47-4fa3-83da-1bb548bf4c3a', article_vi_iv_mutual_conditionality, conventional).
narrative_ontology:cs_axiom('b2f8975f-9c47-4fa3-83da-1bb548bf4c3a', foundational, disarmament_breach_delegitimizes_restraint_expectation).
narrative_ontology:cs_axiom_status(disarmament_breach_delegitimizes_restraint_expectation, holdable).
narrative_ontology:cs_axiom_grounding('b2f8975f-9c47-4fa3-83da-1bb548bf4c3a', disarmament_breach_delegitimizes_restraint_expectation, empirically_contingent).
narrative_ontology:cs_reference_frame('b2f8975f-9c47-4fa3-83da-1bb548bf4c3a', id_1968_grand_bargain_negotiating_consensus).
narrative_ontology:cs_drift_state('b2f8975f-9c47-4fa3-83da-1bb548bf4c3a', post_cold_war_modernization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b2f8975f-9c47-4fa3-83da-1bb548bf4c3a', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, weapon_states_nwsp5).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, peaceful_nuclear_technology_exporters).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_weapon_state_parties).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, civil_nuclear_energy_aspirants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, civil_nuclear_energy_aspirants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold recognized nuclear-weapon-state status under the treaty, retain de facto discretion over disarmament pacing, and continue to enjoy Article IV's civil nuclear cooperation framework's legitimating cover. They set review conference agendas, control what counts as 'good faith' progress under Article VI, and face no binding enforcement mechanism if disarmament stalls. Their exit from any obligation is effectively unilateral: they can slow-walk reductions indefinitely while pointing to modernization programs as consistent with treaty text.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, weapon_states_nwsp5, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, weapon_states_nwsp5, beneficiary).

% Accepted permanent non-acquisition of weapons in exchange for the promise of both eventual disarmament and unfettered peaceful nuclear technology access. Decades into stalled Article VI progress, they bear the ongoing costs of restraint (foregone weapons option, security dependence on great-power guarantees) without the reciprocal benefit materializing. Their exit options are narrow: withdrawal under Article X is legally available but carries severe reputational, sanctions, and security costs, so most remain nominally bound while voicing grievance at review conferences.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_weapon_state_parties, payer,
    moderate, generational, constrained, global).

% States seeking nuclear power infrastructure under Article IV's promise of 'inalienable right' to peaceful nuclear technology. In practice, access is gated by supplier-state export controls, safeguards conditionality, and geopolitical trust — the promised free flow of technology is throttled by the same weapon states whose disarmament failures the grand-bargain reading holds should trigger consequences. They benefit when access is granted but pay when it is withheld on non-treaty grounds.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, civil_nuclear_energy_aspirants, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, civil_nuclear_energy_aspirants, beneficiary).

% Commercial and state nuclear technology suppliers, largely based in weapon states, who profit from controlling the terms of Article IV cooperation. They can condition, delay, or deny technology transfer using proliferation-risk justifications while facing no reciprocal pressure tied to their home states' Article VI performance.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, peaceful_nuclear_technology_exporters, beneficiary,
    powerful, generational, mobile, global).

% Administers safeguards verification and convenes review conferences but has no mandate to adjudicate whether Article VI breach delegitimizes Article IV restraint, or to enforce reciprocity. Documents disarmament progress (or its absence) in official reporting without power to compel remedy.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, iaea_and_treaty_secretariat, observer,
    institutional, generational, analytical, global).

% A bloc of non-weapon states that has repeatedly demanded a legally binding link between disarmament milestones and continued NNWS restraint at review conferences. Their proposals for enforceable reciprocity mechanisms are consistently deflected in consensus-based proceedings that weapon states can stall or veto in substance.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_aligned_movement_coalition, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: without a bargain trading weapons restraint for eventual disarmament and civil nuclear access, near-universal non-proliferation would be far harder to secure, since no state would accept permanent renunciation without some reciprocal benefit.
% TRANSFER_FUNCTION: Moves long-term security assurance and civil nuclear cooperation from weapon states to non-weapon states in exchange for permanent renunciation of weapons acquisition; in practice, disarmament progress lags, so the flow is asymmetric — non-weapon states deliver their side (restraint) up front and indefinitely, while weapon states defer theirs.
% ABSENT_VOICES: The non-aligned movement coalition and civil society disarmament advocates have repeatedly proposed binding reciprocity triggers (e.g., automatic renegotiation rights if disarmament milestones are missed) at review conferences, but consensus procedure and weapon-state veto power keep these proposals from being adopted as enforceable treaty mechanisms.
% DISAPPEARANCE_RATIONALE: Weapon states would treat the grand-bargain reading's disappearance as inconsequential to their obligations (they read Article VI as aspirational anyway under the sibling nonproliferation_primary reading). Non-weapon states would treat its disappearance as removing their sole remaining legal leverage — the interpretive claim that Article VI breach undermines Article IV legitimacy is the only textual hook they have for demanding reciprocity, so for them the world would rearrange substantially, likely accelerating withdrawal threats and TPNW alignment.
% FOUNDING_PROBLEM: The 1968 NPT negotiations needed to secure near-universal non-proliferation commitment from states capable of building weapons, but those states would not accept permanent renunciation without a reciprocal commitment from the weapon states to eventually disarm and to share peaceful nuclear benefits — the grand bargain was built to make asymmetric restraint politically and legally acceptable.
% FOUNDING_PROBLEM_CORROBORATION: Non-weapon state parties and the non-aligned movement coalition attest, in review conference statements and working papers, that the reciprocity problem remains live and unresolved decades after entry into force. Independent arms control research bodies (e.g., SIPRI reporting on weapon-state modernization programs) corroborate from outside the treaty parties that disarmament has not tracked the pace implied by the grand-bargain reading. Weapon states themselves, who benefit from the alternative nonproliferation_primary reading, do not corroborate this founding-problem characterization as still live in the same terms.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, contested).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.61 under the grand-bargain reading because the reading itself holds that non-weapon states are owed a reciprocal disarmament benefit they have not received while their restraint obligation remains fully in force — a growing asymmetry over the interval as weapon-state modernization programs (documented by independent arms control monitors) proceeded alongside stalled reduction commitments. Suppression is moderate (0.52): non-weapon states are not coerced by direct force to remain in the treaty, but withdrawal carries severe diplomatic and security costs, and consensus procedure at review conferences structurally blocks binding reciprocity mechanisms. Theater ratio rose to 0.44 as review conference process (Final Documents, action plans, working groups) increasingly substitutes for measurable disarmament progress — a Goodhart-style substitution of procedural output for the substantive obligation the grand-bargain reading holds is owed.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states are the structural beneficiary under this reading: they retain civil nuclear cooperation legitimacy and non-proliferation benefits from NNWS restraint while facing no binding mechanism tying their own compliance to that restraint's continuation — d sits near the beneficiary end, arbitrage exit. Non-weapon state parties are the structural target: they deliver the bargained-for restraint unconditionally while the reciprocal disarmament benefit remains contingent on weapon-state discretion — d sits near the target end, constrained exit (formal withdrawal exists in Article X but is costly). Civil nuclear aspirants occupy an intermediate position: they receive real Article IV benefits when granted, but those benefits are themselves gated by the same weapon-state discretion the grand-bargain reading identifies as the breach point.
 *
 * MANDATROPHY ANALYSIS:
 *   The grand-bargain reading prevents mislabeling the NPT's asymmetric structure as either pure coordination (which would ignore the accumulating extraction as disarmament stalls) or pure extraction (which would ignore the real, historically significant non-proliferation coordination function the treaty has served). Classifying this reading as tangled_rope captures both: a genuine coordination function (near-universal non-proliferation, real security value) coexisting with asymmetric extraction (weapon states capture the coordination benefit while deferring their reciprocal obligation) that requires active enforcement (safeguards, export control regimes, diplomatic pressure) to hold non-weapon states within their restraint despite unmet reciprocity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_enforceability_ambiguity,
    'Does Article VI''s ''good faith'' negotiation language create a legally enforceable conditionality on Article IV/III restraint, or is the reciprocity purely political/rhetorical, as the nonproliferation_primary reading holds?',
    'International Court of Justice advisory opinion or binding arbitration explicitly addressing whether Article VI non-performance affects the legal standing of NNWS obligations under Articles II/III; alternatively, a review conference outcome document that formally adopts a reciprocity-trigger mechanism.',
    'If enforceable, weapon-state Article VI breach would license NNWS remedies (expanded Article IV claims, withdrawal justification, or renegotiation rights), sharply raising the constraint''s suppression and extraction from the weapon-state seat''s perspective. If purely political, the grand-bargain reading''s normative force persists but its structural teeth do not, keeping the current asymmetric equilibrium stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_enforceability_ambiguity, conceptual, 'Whether Article VI/IV reciprocity is legally binding or aspirational — the central contest between this reading and nonproliferation_primary.').

omega_variable(
    kernel_reading_divergence_delta,
    'Given three live readings of the same NPT text (grand_bargain, nonproliferation_primary, abolitionist), which reading''s account of Article VI''s legal status will prevail in state practice and international adjudication over the coming decades?',
    'Track review conference Final Document language, ICJ jurisprudence citing NPT obligations, and state party voting patterns on TPNW ratification as revealed preference for the abolitionist reading versus continued NPT framework participation as revealed preference for grand_bargain or nonproliferation_primary readings.',
    'Convergence toward nonproliferation_primary would lower the effective extraction this constraint models (the reciprocity claim would be understood as merely aspirational, reducing its legitimating/delegitimating force). Convergence toward abolitionist would obsolete this reading in favor of treating the entire civil-nuclear regime as extractive. The three readings are NOT averaged in this file — each is its own constraint per the epsilon-invariance principle — but their differential uptake is the structural fact this omega tracks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_divergence_delta, empirical, 'Which of the three kernel readings state practice is converging toward.').

omega_variable(
    modernization_vs_disarmament_measurement,
    'Do weapon-state nuclear modernization programs (submarine, warhead, delivery system upgrades) constitute a breach of Article VI''s disarmament obligation, or are they consistent with maintaining a ''minimum deterrent'' while still pursuing eventual disarmament in good faith?',
    'Independent technical assessment (e.g., SIPRI, FAS Nuclear Notebook) comparing warhead counts, delivery system capability, and stated doctrine changes against Article VI''s textual requirement of negotiations ''in good faith'' toward disarmament.',
    'If modernization is deemed inconsistent with good-faith disarmament, the extractiveness score under this reading should be revised upward and the founding_problem_status moves more clearly toward ''dead'' (arrangement persists past its justified function). If modernization is deemed consistent with maintained deterrence during a good-faith negotiation process, extraction stays at the current moderate level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_vs_disarmament_measurement, empirical, 'Whether weapon-state modernization programs constitute measurable Article VI breach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(npt__tr_t1995, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(npt__tr_t2005, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(npt__tr_t2015, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1968, 0.28).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(npt__be_t1995, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(npt__be_t2005, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(npt__be_t2015, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2024, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1968, 0.35).
narrative_ontology:measurement(npt__su_t1980, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(npt__su_t1995, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1995, 0.44).
narrative_ontology:measurement(npt__su_t2005, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2005, 0.47).
narrative_ontology:measurement(npt__su_t2015, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__grand_bargain, 0.1).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the npt_article_iv_vi_pairing kernel, each authored as a separate constraint story with its own epsilon per the epsilon-invariance principle. grand_bargain (this file) authors moderate-to-substantial extraction (0.61) reflecting a reciprocal-obligation reading where weapon-state disarmament failure is a legitimacy-undermining breach but not (yet) a clearly justiciable one. nonproliferation_primary would author lower extraction from the weapon-state-favorable reading that Article VI is non-binding. abolitionist would author the highest extraction, treating the entire Article IV civil-nuclear regime as illegitimate dual-use proliferation risk regardless of Article VI performance. The three do not share a single epsilon because they are structurally distinct claims about what the treaty text obligates, not three measurements of one claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
