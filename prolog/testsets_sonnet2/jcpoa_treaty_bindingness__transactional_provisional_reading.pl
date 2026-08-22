% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__transactional_provisional_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Bad-Faith Determination
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This story instantiates the transactional-provisional reading of the
 *   JCPOA kernel: the arrangement is a political understanding, not a
 *   ratified binding treaty, and remains voidable at any signatory's
 *   unilateral discretion upon a national determination of counterparty bad
 *   faith. Under this reading, the exit exercised by the withdrawing state's
 *   executive was a lawful and anticipated feature of the framework's design,
 *   not a violation of it. This is a distinct constraint from the
 *   binding_multilateral_reading (which treats the same withdrawal as a
 *   breach of a binding instrument) and the graduated_compliance_reading
 *   (which treats enforcement as properly proportional to assessed compliance
 *   rather than a unilateral all-or-nothing trigger). Each reading carries
 *   its own epsilon and its own beneficiary/victim structure; this file does
 *   not average across them.
 *
 * KEY AGENTS:
 *   - withdrawing_state_executive: institutional/arbitrage — exercises the unilateral exit this reading authorizes
 *   - domestic_anti_deal_coalition: organized/mobile — collects political benefit from the exit with no exposure to its costs
 *   - iranian_civilian_population: powerless/trapped — bears the sanctions snapback triggered by a determination it cannot contest
 *   - international_atomic_energy_agency_verification_regime: institutional/constrained — analytical seat whose technical findings are structurally subordinated to the political determination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.58).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.42).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Bad-Faith Determination").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '92b7ac5f-d44e-460f-bbb5-62e0fd34dd4b').
narrative_ontology:cs_kernel_codification('92b7ac5f-d44e-460f-bbb5-62e0fd34dd4b', distributed).
narrative_ontology:cs_authority_grounding('92b7ac5f-d44e-460f-bbb5-62e0fd34dd4b', distributed).
narrative_ontology:cs_reading_relation('92b7ac5f-d44e-460f-bbb5-62e0fd34dd4b', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('92b7ac5f-d44e-460f-bbb5-62e0fd34dd4b', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('92b7ac5f-d44e-460f-bbb5-62e0fd34dd4b', foundational, unilateral_national_determination_sufficient_for_exit).
narrative_ontology:cs_axiom_status(unilateral_national_determination_sufficient_for_exit, holdable).
narrative_ontology:cs_axiom_grounding('92b7ac5f-d44e-460f-bbb5-62e0fd34dd4b', unilateral_national_determination_sufficient_for_exit, conventional).
narrative_ontology:cs_axiom('92b7ac5f-d44e-460f-bbb5-62e0fd34dd4b', foundational, political_commitment_not_ratified_treaty_obligation).
narrative_ontology:cs_axiom_status(political_commitment_not_ratified_treaty_obligation, holdable).
narrative_ontology:cs_axiom_grounding('92b7ac5f-d44e-460f-bbb5-62e0fd34dd4b', political_commitment_not_ratified_treaty_obligation, conventional).
narrative_ontology:cs_reference_frame('92b7ac5f-d44e-460f-bbb5-62e0fd34dd4b', executive_political_commitment_framework).
narrative_ontology:cs_drift_state('92b7ac5f-d44e-460f-bbb5-62e0fd34dd4b', post_withdrawal_reimposition_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('92b7ac5f-d44e-460f-bbb5-62e0fd34dd4b', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state_executive).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_anti_deal_coalition).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, regional_rival_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_civilian_population).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, remaining_p5_plus_1_signatories).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, international_atomic_energy_agency_verification_regime).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__transactional_provisional_reading, state_sovereignty_over_treaty_commitment).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__transactional_provisional_reading, executive_discretion_in_national_security_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the unilateral authority to declare the counterparty in bad faith and reimpose sanctions without seeking multilateral consensus or dispute-resolution mechanisms. Frames the JCPOA as a non-binding political commitment rather than a ratified treaty, which is what makes unilateral exit available as a live option. Collects domestic political capital and negotiating leverage from the exit.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state_executive, agenda_setter,
    institutional, biographical, arbitrage, global).

% Legislators, lobbying networks, and allied foreign governments who opposed the original agreement. Gain policy wins, campaign narratives, and continued sanctions leverage when the framework is voided. Face no direct cost from withdrawal since they bear none of the verification or humanitarian fallout.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_anti_deal_coalition, beneficiary,
    organized, biographical, mobile, national).

% Neighboring and rival governments who viewed the original deal as strengthening Iran's regional position. Benefit from renewed sanctions and isolation of Iran, and lobbied actively for a reading of the framework that permits unilateral exit.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, regional_rival_states, beneficiary,
    powerful, generational, arbitrage, regional).

% Bears the economic consequences of snapback sanctions — currency collapse, medical shortages, unemployment — triggered by a determination made entirely outside their political process. Has no standing to contest the bad-faith finding and no exit from the national economy absorbing the shock.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_civilian_population, payer,
    powerless, biographical, trapped, national).

% Governments that remained committed to the framework and invested diplomatic capital in its verification architecture. Absorb the costs of a partner's unilateral exit — collapsed enforcement leverage, strained alliance relations, and the need to either maintain a hollowed-out deal or find new diplomatic footing. Cannot compel the withdrawing state back into compliance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, remaining_p5_plus_1_signatories, payer,
    institutional, generational, constrained, global).

% The technical verification body whose inspection access and monitoring credibility depend on the framework staying intact. A unilateral bad-faith determination that collapses the deal strands its verification infrastructure and undermines the precedent that technical compliance findings matter more than political ones.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, international_atomic_energy_agency_verification_regime, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, international_atomic_energy_agency_verification_regime, observer).

% States that would negotiate future non-proliferation agreements are not present in this dispute but are structurally affected: a precedent that any agreement is voidable by unilateral bad-faith declaration lowers the perceived value of negotiating with the withdrawing state's government in any future arrangement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, future_non_proliferation_negotiating_parties, excluded,
    powerless, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state_executive).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the withdrawing state a mechanism to disengage from a multilateral commitment quickly, without protracted multilateral dispute-resolution, when it judges the counterparty to be violating the spirit or letter of the arrangement — avoiding entrapment in an agreement its executive branch judges no longer serves national security interests.
% TRANSFER_FUNCTION: Moves diplomatic and economic leverage from the framework's continuity (which benefited Iranian civilians via sanctions relief and benefited the verification regime via inspection access) to the withdrawing state's executive and its domestic and regional political allies, at the cost of reimposed sanctions borne by Iran's population and stranded verification infrastructure.
% ABSENT_VOICES: Iranian civilians who bear the sanctions snapback have no voice in the bad-faith determination process; future treaty partners who will price in this precedent when negotiating are not present at all. Neither the IAEA's technical findings nor the other P5+1 partners' assessments are binding on the determination.
% DISAPPEARANCE_RATIONALE: If unilateral voidability disappeared and the arrangement instead required multilateral consensus to modify or dissolve, sanctions relief and verification would have persisted on the joint-assessment track; the withdrawing executive would have lost the exit option it exercised, domestic anti-deal coalitions would have lost their leverage point, and Iran's economy would not have absorbed the snapback shock on the timeline it did.
% FOUNDING_PROBLEM: The framework was built to halt progress toward nuclear weapons capability through verified, reversible constraints in exchange for sanctions relief, while giving all signatory governments enough domestic political cover to sell a compromise neither side fully wanted.
% FOUNDING_PROBLEM_CORROBORATION: The withdrawing state's executive and its domestic coalition attest that Iranian conduct outside the nuclear file (regional proxy activity, missile development) rendered the underlying bargain void in substance, justifying unilateral exit. The IAEA's own verification reports at the time of withdrawal, and the assessments of the remaining P5+1 signatories, attested that the technical nuclear-constraint provisions were being met — corroboration from outside the withdrawing coalition contradicts the bad-faith premise on the narrow nuclear-compliance question, though not on the broader regional-conduct complaint.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the withdrawal transfers real costs (sanctions exposure, stranded verification investment, alliance strain) onto parties who did not make and cannot contest the triggering determination — but it is not maximal, because the reading does preserve a formal coordination function (an exit valve against entrapment in bad-faith arrangements) that a pure snare reading would lack. Suppression is moderate (0.42): the mechanism does not physically coerce compliance, but it does foreclose the other signatories' and the IAEA's ability to contest the determination through any binding process. Theater ratio (0.4) reflects that verification and diplomatic engagement continued to be performed for some time even as the substantive coordination function eroded around the exit event. Resistance is high (0.7) because the withdrawal provoked sustained diplomatic, legal, and multilateral objection from the remaining signatories and the IAEA even though none of it was binding.
 *
 * DIRECTIONALITY LOGIC:
 *   The withdrawing executive and its domestic/regional beneficiaries sit near the full-beneficiary end: they hold the discretion, bear none of the downside, and collect the political and strategic gains. Iranian civilians sit near the full-target end: trapped, powerless, and bearing sanctions costs from a determination made entirely outside their political process. The remaining P5+1 signatories and the IAEA occupy an intermediate position — institutionally powerful but structurally unable to compel reversal, making them payers despite their formal power level; this is why an override was considered but not required (their exit_options of 'constrained' already captures the asymmetry without needing to override the institutional-power derivation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (halting weapons-capable enrichment via verified, reversible constraints) was substantially live at the moment of withdrawal by the IAEA's own contemporaneous technical assessment, even though the withdrawing coalition asserted it as dead on non-nuclear grounds. Classifying this as tangled_rope rather than snare preserves the genuine coordination function this reading's own logic supplies (an anticipated exit valve, not a hidden extraction mechanism) while still registering the asymmetric cost imposed on non-consenting parties — collapsing it to snare would erase the reading's own coordination claim; collapsing it to rope would erase the documented victim set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_bindingness_ambiguity,
    'Is the JCPOA properly read as a binding multilateral treaty obligation, a graduated reciprocal-compliance arrangement, or a provisional transactional framework voidable at unilateral discretion? This story instantiates the transactional-provisional reading; the binding_multilateral_reading and graduated_compliance_reading are separate constraints with different epsilon values and different victim sets.',
    'There is no external adjudicating body with binding authority over this question — the UN Security Council resolution endorsing the JCPOA, the domestic legal status of the underlying instrument in each signatory state, and international law scholarship on political commitments versus treaties all bear on it but do not resolve it definitively.',
    'If the binding_multilateral_reading is the structurally correct one, this constraint''s unilateral-exit mechanism would itself be the extractive/suppressive act rather than a legitimate coordination feature, and the classification would shift toward snare. If the graduated_compliance_reading is correct, the all-or-nothing unilateral trigger modeled here would be an illegitimate escalation of what should be a proportional response mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_bindingness_ambiguity, conceptual, 'Which reading of JCPOA bindingness is structurally correct — routes the kernel contest to omega rather than folding sibling readings into this constraint''s classification.').

omega_variable(
    bad_faith_determination_evidentiary_basis,
    'Was the bad-faith determination that triggered withdrawal grounded in the nuclear-compliance record (where IAEA reporting found compliance) or in non-nuclear conduct (regional activity, missile programs) not covered by the framework''s own terms?',
    'Contemporaneous IAEA verification reports, subsequent independent nuclear-forensics assessments, and diplomatic-cable disclosures could establish whether the technical compliance record supported or contradicted the bad-faith finding at the time it was made.',
    'If the determination rested on conduct outside the framework''s own compliance terms, the ''voidable upon bad faith'' mechanism functioned as a pretext for a decision made on other grounds — raising extractiveness and weakening the coordination-function claim this reading depends on. If nuclear noncompliance was in fact emerging, the mechanism functioned closer to its stated design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bad_faith_determination_evidentiary_basis, empirical, 'Whether the triggering bad-faith determination was evidentially grounded in the compliance record the framework itself governs.').

omega_variable(
    sovereignty_versus_precedent_cost,
    'Does preserving unilateral exit as a live option for future US administrations (protecting flexibility and sovereignty) outweigh the diplomatic-credibility cost imposed on future non-proliferation negotiations, where counterparties will price in the risk of unilateral abrogation?',
    'This is a values tradeoff between sovereignty/flexibility and long-run negotiating credibility, not an empirical question resolvable by data alone, though the negotiating behavior of future counterparties (demanding harder guarantees, refusing reversible concessions) would be observable evidence of the cost materializing.',
    'Determines whether the beneficiary set (domestic sovereignty-preserving coalitions) genuinely nets positive once discounted future negotiating costs are included, or whether the framework''s design externalizes a long-run cost onto future treaty-making capacity that the current beneficiaries do not bear.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_versus_precedent_cost, preference, 'Whether sovereignty-preserving unilateral exit is worth its long-run cost to future negotiating credibility — a values question, not a factual one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement(jcpo_tr_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 3, 0.5).
narrative_ontology:measurement(jcpo_tr_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(jcpo_tr_t7, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 7, 0.42).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2, 0.28).
narrative_ontology:measurement(jcpo_be_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 3, 0.65).
narrative_ontology:measurement(jcpo_be_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(jcpo_be_t7, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 7, 0.6).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2, 0.3).
narrative_ontology:measurement(jcpo_su_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(jcpo_su_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(jcpo_su_t7, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 7, 0.44).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, iran_sanctions_snapback_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the jcpoa_treaty_bindingness kernel: binding_multilateral_reading treats the instrument as requiring consensus-based dissolution and reads unilateral exit as breach; graduated_compliance_reading treats enforcement as properly scaled to jointly-assessed compliance rather than an all-or-nothing unilateral trigger; this transactional_provisional_reading treats unilateral exit upon national bad-faith determination as a legitimate, anticipated design feature. The three share the same underlying text and instrument but diverge sharply on bindingness, and carry different epsilon values and different beneficiary/victim sets accordingly — they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
