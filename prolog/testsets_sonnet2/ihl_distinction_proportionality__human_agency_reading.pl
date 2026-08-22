% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: Human Agency Reading of IHL Distinction/Proportionality — Meaningful Human Control Requirement
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the human_agency_reading of the contested IHL
 *   distinction/proportionality kernel over lethal autonomous weapons systems
 *   (LAWS). On this reading, IHL's distinction and proportionality
 *   obligations are read as requiring irreducible human moral judgment at the
 *   point of lethal force application; the Martens Clause's 'principles of
 *   humanity' and 'dictates of public conscience' language is read to
 *   categorically bar delegating the final kill decision to a machine,
 *   independent of that machine's measured accuracy. This is distinct from
 *   the categorical_prohibition_reading (which bars autonomous weapons per
 *   se, on dignity grounds, regardless of the human-control question) and
 *   from the outcomes_based_reading (which would certify a fully autonomous
 *   system as IHL-compliant if it demonstrably matches or exceeds human
 *   targeting performance). The three readings share the kernel text — IHL
 *   distinction/proportionality obligations plus the Martens Clause — but
 *   diverge sharply on what counts as compliance, and therefore have
 *   different ε, different beneficiaries, and different victims. They are
 *   authored as separate constraint stories and linked via
 *   network.affects_constraints, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - icrc_and_ihl_interpretive_authorities: Primary beneficiary/agenda_setter (institutional/analytical) — retains interpretive centrality by keeping the compliance standard process-based rather than outcome-verifiable
 *   - military_operational_commands_seeking_autonomy: Primary target (powerful/constrained) — bears the operational cost of mandatory human-in-the-loop targeting in degraded-communications environments
 *   - states_with_technological_but_not_manpower_advantage: Secondary target (institutional/constrained) — loses the comparative advantage automation would otherwise provide
 *   - civilians_in_conflict_zones: Intended beneficiary with no institutional voice (powerless/trapped) — the population invoked to justify the standard but absent from its drafting
 *   - human_rights_advocacy_networks: Secondary beneficiary (organized/mobile) — gains a technology-proof, durable advocacy platform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.68).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.72).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "Human Agency Reading of IHL Distinction/Proportionality — Meaningful Human Control Requirement").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, '620dc8c9-e351-42b3-a2ce-0e7140b6a792').
narrative_ontology:cs_kernel_codification('620dc8c9-e351-42b3-a2ce-0e7140b6a792', distributed).
narrative_ontology:cs_authority_grounding('620dc8c9-e351-42b3-a2ce-0e7140b6a792', distributed).
narrative_ontology:cs_reading_relation('620dc8c9-e351-42b3-a2ce-0e7140b6a792', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('620dc8c9-e351-42b3-a2ce-0e7140b6a792', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('620dc8c9-e351-42b3-a2ce-0e7140b6a792', foundational, moral_judgment_irreducible_to_process).
narrative_ontology:cs_axiom_status(moral_judgment_irreducible_to_process, holdable).
narrative_ontology:cs_axiom_grounding('620dc8c9-e351-42b3-a2ce-0e7140b6a792', moral_judgment_irreducible_to_process, deontological).
narrative_ontology:cs_axiom('620dc8c9-e351-42b3-a2ce-0e7140b6a792', secondary, compliance_indexed_to_decision_locus_not_outcome).
narrative_ontology:cs_axiom_status(compliance_indexed_to_decision_locus_not_outcome, holdable).
narrative_ontology:cs_axiom_grounding('620dc8c9-e351-42b3-a2ce-0e7140b6a792', compliance_indexed_to_decision_locus_not_outcome, conventional).
narrative_ontology:cs_reference_frame('620dc8c9-e351-42b3-a2ce-0e7140b6a792', post_wwii_human_accountability_targeting_norm).
narrative_ontology:cs_drift_state('620dc8c9-e351-42b3-a2ce-0e7140b6a792', post_2020_autonomous_weapons_proliferation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('620dc8c9-e351-42b3-a2ce-0e7140b6a792', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, icrc_and_ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, human_rights_advocacy_networks).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, states_with_manpower_advantage).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_commands_seeking_autonomy).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, states_with_technological_but_not_manpower_advantage).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, defense_contractors_developing_laws).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, battlefield_commanders_supervising_autonomy).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, battlefield_commanders_supervising_autonomy).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, irreducibility_of_moral_judgment_thesis).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, martens_clause_humanity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues authoritative commentary and convenes state consultations (CCW GGE) asserting that distinction and proportionality judgments cannot be delegated to a machine without violating the requirement of human moral agency. This reading keeps the ICRC and allied legal scholarship as the necessary arbiter of what counts as 'meaningful human control,' since the standard is a judgment call rather than a measurable performance threshold. Its institutional relevance and convening authority depend on the human-agency standard remaining the operative test rather than a hardware-verifiable outcomes benchmark.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, icrc_and_ihl_interpretive_authorities, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, icrc_and_ihl_interpretive_authorities, beneficiary).

% Campaigns (e.g. Stop Killer Robots coalition members) built around the human-agency framing gain a durable, technology-proof advocacy platform: no matter how systems perform, the argument holds. Their institutional funding, coalition standing, and treaty-negotiation access are strengthened by a legal reading that cannot be mooted by future engineering progress.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, human_rights_advocacy_networks, beneficiary,
    organized, generational, mobile, global).

% States that can afford dense human-in-the-loop targeting chains (personnel-rich militaries, or those relying on allied intelligence/targeting infrastructure) benefit from a rule that locks in their existing operational model as the legally compliant baseline, while competitors racing toward full autonomy are constrained regardless of the reliability of their systems.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, states_with_manpower_advantage, beneficiary,
    institutional, generational, constrained, global).

% Commanders operating in contested electromagnetic environments (jamming, latency, swarm warfare) where communications links to a human operator may be degraded or severed bear an operational cost: any weapon system that must retain a human in the final loop is unusable exactly when speed-of-light autonomous response is most valuable. They cannot field fully autonomous lethal systems without incurring legal risk and international condemnation, even where system reliability exceeds human operators statistically.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_operational_commands_seeking_autonomy, payer,
    powerful, immediate, constrained, global).

% States pursuing autonomous-systems investment as an asymmetric answer to manpower or budget constraints find the legal path closed regardless of documented performance parity; their comparative advantage in automation is legally foreclosed by a standard indexed to process (human judgment present) rather than outcome (target discrimination accuracy).
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, states_with_technological_but_not_manpower_advantage, payer,
    institutional, generational, constrained, global).

% Firms developing fully autonomous targeting systems face a legal ceiling that caps the achievable product: no matter how well their system performs against distinction/proportionality benchmarks in testing, the human-agency standard prohibits fielding it as a final decision-maker, forcing costly redesign toward human-supervised architectures or abandonment of R&D lines.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, defense_contractors_developing_laws, payer,
    powerful, biographical, constrained, global).

% Populations subject to targeting decisions are the intended beneficiaries of retained human judgment (accountability, contextual mercy, the capacity to recognize surrender or civilian status that a classifier might miss) but have no voice in the treaty negotiations, GGE sessions, or contractor design reviews that set the actual content of 'meaningful human control.' Their interest is invoked by all three kernel readings but represented by none of them directly.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilians_in_conflict_zones, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, civilians_in_conflict_zones, excluded).

% Human operators required to remain 'in' or 'on' the loop bear the cognitive and legal burden of a judgment they may not have the situational information, time, or training to meaningfully exercise — a supervisory role that can become a rubber stamp under time pressure while still carrying full legal and moral accountability for the outcome.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, battlefield_commanders_supervising_autonomy, payer,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, battlefield_commanders_supervising_autonomy, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__human_agency_reading, icrc_and_ihl_interpretive_authorities).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__human_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state practice, weapons-review processes, and treaty negotiation around a single, legible compliance test — 'is there a human exercising judgment at the point of the kill decision' — which is far easier for multilateral bodies to monitor, litigate, and negotiate around than a statistical performance-parity threshold that would require continuous technical verification.
% TRANSFER_FUNCTION: Moves legal certainty and interpretive authority toward the ICRC/advocacy/legal-scholarship complex and toward states whose existing force structure already satisfies the human-in-the-loop model; moves cost, operational risk, and R&D write-offs onto militaries and contractors pursuing full autonomy, regardless of whether their systems can be shown to outperform human targeting on distinction and proportionality.
% ABSENT_VOICES: Civilians in conflict zones — the population the rule is justified in the name of protecting — have no seat in CCW GGE negotiations, ICRC expert consultations, or contractor design review; their interests are represented by advocacy proxies whose institutional incentives (durable campaign platforms) do not perfectly track civilian protection outcomes. Engineers and human-factors researchers studying whether supervisory human control is meaningfully exercised under time pressure are also largely absent from the legal drafting process.
% DISAPPEARANCE_RATIONALE: If the human-agency reading were abandoned in favor of an outcomes-based standard, states and contractors would immediately reallocate R&D toward performance-verifiable full autonomy, weapons-review boards would need entirely different testing protocols (statistical benchmarking rather than process audits), and the ICRC's convening role would shift from arbiter of 'meaningful human control' to auditor of technical performance data — a materially different institutional position.
% FOUNDING_PROBLEM: The felt need to preserve a locus of moral and legal accountability for lethal decisions as weapons systems gained increasing decision-making autonomy — rooted in the post-WWII IHL architecture's assumption that a human agent bears the moral weight of a targeting choice, and in Martens Clause language (principles of humanity, dictates of public conscience) drafted long before machine targeting was technically conceivable.
% FOUNDING_PROBLEM_CORROBORATION: Military legal advisors within NATO and allied weapons-review programs corroborate that the underlying accountability problem (who bears responsibility when a lethal decision is wrong) remains live and unresolved by current autonomous-systems engineering. Independent technical researchers and some state delegations to the CCW GGE (outside the ICRC/advocacy coalition that benefits from the standard) argue the founding accountability problem is being addressed asymmetrically — solved rhetorically through the human-in-the-loop requirement while the harder question of how to allocate accountability for supervised or verified-performance systems remains unaddressed, suggesting the current reading has outrun the problem it was built to solve without a replacement accountability mechanism yet in place.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68 at interval end) because the human-agency standard, once adopted as the operative legal test, extracts real operational and R&D value from states and contractors pursuing full autonomy without regard to demonstrated safety performance — the extraction tracks legal process compliance, not outcome quality. Suppression is authored comparably high (0.72) because the standard functions to categorically foreclose an entire technical pathway (full autonomy) rather than merely raise its cost; alternatives (verified-performance autonomous targeting) are legally unavailable regardless of engineering merit. accessibility_collapse (0.6) reflects that the alternative outcomes-based framing remains conceptually available and actively argued by other state delegations and the outcomes_based sibling reading — it has not collapsed as thoroughly as a genuine natural-law constraint would. resistance (0.7) is high because states with technological advantage and defense contractors actively contest the human-agency standard in CCW GGE sessions and academic IHL literature; this is not an uncontested settlement.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC/advocacy seat, this reading is a principled defense of human dignity and accountability — a Rope-like coordination achievement holding the line on an important moral question. From the seat of a military command operating in a jammed, high-tempo environment, or a contractor whose system has cleared performance benchmarks the standard doesn't credit, the same rule computes as an enforced cost with no path to compliance through improved engineering — closer to tangled_rope-as-experienced. The engine computes these divergent seat classifications from the declared structural data (power, exit_options, beneficiary/victim role); the story does not average or resolve the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   ICRC/interpretive authorities and human-rights advocacy networks sit near the beneficiary end of directionality: the standard's persistence directly sustains their institutional role and campaign platform, and their exit options are mobile/analytical rather than constrained by the rule's content. States with manpower advantage benefit structurally because the rule locks in their existing compliant force structure as the legal baseline. Conversely, military commands seeking battlefield autonomy, technologically-advantaged-but-manpower-poor states, and LAWS contractors sit near the target end: the rule directly forecloses a capability path they would otherwise pursue, and their exit options are constrained (they cannot simply route around IHL compliance in contested operations without incurring war-crimes exposure). Civilians are the notional beneficiaries by the rule's own justification but are trapped (no exit from being targeting subjects) and structurally excluded from the negotiations that set the content of 'meaningful human control' — their d is not simply low-beneficiary but complicated by having no seat at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — accountability for lethal decisions as machine autonomy increases — is genuinely live; this is not a pure zombie mandate. But the specific mechanism (requiring a human physically present at the decision point) is contested as the right solution to that problem: outcome-verification technology may now make an alternative accountability mechanism possible that the human-agency reading forecloses without engaging on the merits. Classifying this as tangled_rope rather than snare or mountain captures both halves honestly: there IS a genuine coordination function (a legible, monitorable compliance test that avoids the technical-verification burden a performance standard would impose on every weapons review board), AND there is asymmetric extraction (technologically-advantaged actors bear disproportionate cost regardless of demonstrated system performance, while interpretive authorities and manpower-advantaged incumbents benefit from the standard's persistence). Reading this as a mountain (natural, uncontested moral truth) would suppress the genuine live debate the outcomes_based_reading represents; reading it as a pure snare would ignore the real coordination benefit of a legible compliance standard in a domain where technical verification is genuinely hard to arrange multilaterally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_agency_kernel_reading_choice,
    'Is the human_agency_reading (irreducible human judgment at the decision point) the correct interpretation of IHL distinction/proportionality obligations and the Martens Clause, as against the categorical_prohibition_reading (autonomy is unlawful per se, independent of human control) and the outcomes_based_reading (technology-neutral performance parity suffices)?',
    'State practice convergence at the CCW GGE, an authoritative ICJ or treaty-body ruling on Article 36 weapons review standards, or a documented shift in opinio juris toward one reading as customary international law.',
    'If the outcomes_based_reading prevails, this constraint''s foreclosure of full autonomy would be legally displaced entirely — a performance-verifiable LAWS could become compliant. If the categorical_prohibition_reading prevails instead, this reading would be superseded by an even stronger prohibition (foreclosing supervised autonomy too, not just full autonomy). The three readings are not merely different emphases; they produce materially different sets of lawful weapons systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_agency_kernel_reading_choice, conceptual, 'Which kernel reading of IHL distinction/proportionality + Martens Clause will prevail as the operative international legal standard for autonomous weapons.').

omega_variable(
    meaningful_human_control_verifiability,
    'Is ''meaningful human control'' (as opposed to nominal/rubber-stamp human presence) a verifiable operational standard, or is it inherently unfalsifiable in a way that makes the human_agency_reading''s compliance test theater rather than substance?',
    'Human-factors research on operator decision quality under time pressure in supervised-autonomy test environments; after-action review data from fielded human-supervised systems.',
    'If meaningful control cannot be reliably distinguished from rubber-stamping under real operational tempo, the theater_ratio for this constraint is under-authored and the coordination function claimed in six_questions.coordination_function is weaker than stated — pushing the classification toward snare (extraction dressed as accountability) rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaningful_human_control_verifiability, empirical, 'Whether human-in-the-loop requirements produce genuine accountability or performative compliance under battlefield time pressure.').

omega_variable(
    beneficiary_capture_of_martens_clause,
    'Is the ICRC/interpretive-authority beneficiary structure incidental to a genuinely correct legal reading, or does the institutional interest in retaining interpretive centrality shape which reading of an inherently ambiguous 19th-century clause (the Martens Clause) gets promoted as authoritative?',
    'Comparative analysis of ICRC institutional positioning across other IHL interpretive disputes where its interpretive centrality was not at stake, to check whether its reading choices correlate with institutional interest independent of legal merit.',
    'If capture is substantial, the beneficiary declaration for icrc_and_ihl_interpretive_authorities understates the extraction — the coordination story would be partly cover for interpretive rent-seeking, pushing the classification toward snare. If capture is minimal, tangled_rope with a genuine coordination component is the more accurate read.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_martens_clause, conceptual, 'Whether the beneficiary structure reflects legal merit or institutional self-interest in the reading''s promotion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ihl__tr_t4, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(ihl__tr_t8, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(ihl__tr_t16, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ihl__be_t4, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(ihl__be_t8, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(ihl__be_t16, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(ihl__su_t4, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(ihl__su_t8, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(ihl__su_t12, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(ihl__su_t16, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__human_agency_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the natural-language 'Martens Clause / IHL distinction-proportionality applies to autonomous weapons' claim into structurally distinct readings sharing one kernel (ihl_distinction_proportionality): categorical_prohibition_reading (autonomy unlawful per se, dignity-grounded, ε highest and suppression near-total — forecloses both other readings' core premises), human_agency_reading (this story — process-based human-judgment requirement, moderately-high ε, tangled_rope), and outcomes_based_reading (technology-neutral performance standard, lower ε, closer to rope/scaffold since it authorizes a transition path as verification technology matures). Each carries its own ε, beneficiaries, victims, and classification. This story's ε (0.68) reflects the operational/R&D foreclosure cost specific to the human-agency test and must not be averaged with the sibling readings' different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__human_agency_reading, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
