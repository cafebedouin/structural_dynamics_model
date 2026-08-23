% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA Graduated Compliance Reading — Calibrated Reciprocal Commitment
 *   domain: international law / nuclear non-proliferation / treaty compliance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   jcpoa_treaty_bindingness: the graduated_compliance_reading, which reads
 *   the JCPOA as a scaled reciprocal commitment whose enforcement is
 *   calibrated to assessed violation severity — partial relief withdrawal
 *   proportional to enrichment steps, disputes routed through the Joint
 *   Commission with de-escalation prioritized over formal legal closure. The
 *   ε referent is the standing arrangement under contest: the
 *   verification-for-relief exchange as designed and operated 2015-2021,
 *   assessed by this reading's own lights — a functioning calibrated exchange
 *   whose known asymmetry (sticky technical concessions against instantly
 *   revocable relief) is a cost of this design, not a different constraint.
 *   Sibling readings are separate constraints, not hedges folded into this
 *   one: the binding_multilateral_reading authors higher persistence and
 *   enforcement stakes (consensus-gated dissolution hardens the shell), and
 *   the transactional_provisional_reading authors lower persistence and
 *   cheaper exit (voidable on unilateral bad-faith determination). ε diverges
 *   across the family because each reading prices the same text's
 *   obligatoriness differently; this reading prices it moderate and
 *   conditional on reciprocity. Assumptions stated: the interval runs from
 *   Adoption Day (2015) to the last coordinated revival attempt (2021);
 *   metric values are author judgments from the public record (IAEA Board
 *   reporting, Joint Commission case history, legislative testimony) on one
 *   shared time grid. KEY AGENTS (by structural relationship): -
 *   joint_commission_mechanism: standing administrator
 *   (institutional/constrained) — convenes parties, prices deviations, stages
 *   responses - iaea_inspectorate: verification authority
 *   (institutional/constrained) — produces the assessments the calibration
 *   reads - p5_plus_one_coalition: relief-lever holder and framework
 *   co-author (institutional/mobile) - united_states_administration:
 *   unilateral relief lever (powerful/arbitrage) — demonstrated exit while
 *   retaining coercive instruments - iranian_government: bound party and
 *   domestic implementer (powerful/trapped) — dual position, delivers limits
 *   and receives relief - iranian_nuclear_program_operators: technical
 *   concession bearers (organized/trapped) - iranian_import_sector:
 *   relief-dividend recipients (moderate/trapped) -
 *   pragmatic_diplomacy_advocates: framework champions across signatory
 *   capitals (organized/mobile) - european_export_firms: post-relief contract
 *   holders (organized/constrained) - european_financial_institutions:
 *   residual-exposure bearers (institutional/constrained) -
 *   regional_adversary_security_establishments: excluded risk bearers
 *   (powerful/trapped) - arms_control_analytical_community: analytical
 *   observers (analytical/analytical).
 *
 * KEY AGENTS:
 *   - - joint_commission_mechanism: standing administrator (institutional/constrained) — convenes parties, prices deviations, stages graduated responses
 *   - - iaea_inspectorate: verification authority (institutional/constrained) — produces the compliance assessments the calibration reads
 *   - - p5_plus_one_coalition: relief-lever holder and framework co-author (institutional/mobile) — controls sequencing of relief and reimposition
 *   - - united_states_administration: unilateral relief lever (powerful/arbitrage) — exited in 2018 while retaining secondary-sanctions reach
 *   - - iranian_government: bound party and domestic implementer (powerful/trapped) — dual position: delivers program limits, receives relief
 *   - - iranian_nuclear_program_operators: technical concession bearers (organized/trapped) — surrendered capacity unrecoverable on deal timescales
 *   - - iranian_import_sector: relief-dividend recipients (moderate/trapped) — livelihoods ride on politically controlled channels
 *   - - pragmatic_diplomacy_advocates: framework champions (organized/mobile) — credibility invested in calibration working
 *   - - european_export_firms: post-relief contract holders (organized/constrained) — slow-building physical and reputational exposure
 *   - - european_financial_institutions: residual-exposure bearers (institutional/constrained) — nominal relief, real snapback pricing in every transaction
 *   - - regional_adversary_security_establishments: excluded risk bearers (powerful/trapped) — absorb latent-capability risk, subordinated to commission process
 *   - - arms_control_analytical_community: analytical observers (analytical/analytical) — see the full lever structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.56).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.42).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA Graduated Compliance Reading — Calibrated Reciprocal Commitment").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international law / nuclear non-proliferation / treaty compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:has_sunset_clause(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, 'bb37d885-6fc1-4e56-b4e2-377ffe4f344a').
narrative_ontology:cs_kernel_codification('bb37d885-6fc1-4e56-b4e2-377ffe4f344a', formalized).
narrative_ontology:cs_authority_grounding('bb37d885-6fc1-4e56-b4e2-377ffe4f344a', practice).
narrative_ontology:cs_interpretation_layer_present('bb37d885-6fc1-4e56-b4e2-377ffe4f344a').
narrative_ontology:cs_reading_relation('bb37d885-6fc1-4e56-b4e2-377ffe4f344a', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb37d885-6fc1-4e56-b4e2-377ffe4f344a', jcpoa_treaty_bindingness__transactional_provisional_reading, influences).
narrative_ontology:cs_axiom('bb37d885-6fc1-4e56-b4e2-377ffe4f344a', foundational, response_proportional_to_violation_severity).
narrative_ontology:cs_axiom_status(response_proportional_to_violation_severity, holdable).
narrative_ontology:cs_axiom_grounding('bb37d885-6fc1-4e56-b4e2-377ffe4f344a', response_proportional_to_violation_severity, instrumental).
narrative_ontology:cs_axiom('bb37d885-6fc1-4e56-b4e2-377ffe4f344a', secondary, deescalation_prior_to_legal_closure).
narrative_ontology:cs_axiom_status(deescalation_prior_to_legal_closure, holdable).
narrative_ontology:cs_axiom_grounding('bb37d885-6fc1-4e56-b4e2-377ffe4f344a', deescalation_prior_to_legal_closure, conventional).
narrative_ontology:cs_reference_frame('bb37d885-6fc1-4e56-b4e2-377ffe4f344a', calibrated_proportional_reciprocity).
narrative_ontology:cs_drift_state('bb37d885-6fc1-4e56-b4e2-377ffe4f344a', post_unilateral_exit_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bb37d885-6fc1-4e56-b4e2-377ffe4f344a', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, european_export_firms).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_import_sector).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_government).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_nuclear_program_operators).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, european_financial_institutions).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_adversary_security_establishments).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_one_coalition).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__graduated_compliance_reading, verified_reciprocal_arms_control).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__graduated_compliance_reading, proportional_response_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes the signatories on a standing calendar, receives deviation reports, and stages responses case by case — from technical clarification requests to coordinated relief adjustments. Its authority exists only inside the framework: if the parties stop performing, it has no venue left to administer. Dispute sessions run behind closed doors and issue findings without formal legal judgment.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, joint_commission_mechanism, agenda_setter,
    institutional, generational, constrained, continental).

% Runs the enhanced verification workload: continuous monitoring of declared sites, daily access to centrifuge assembly workshops, uranium ore-concentrate accounting, and quarterly board reporting. Its findings are the raw input every party reads to judge performance. Access and mandate depend on the framework remaining in force; beyond it, verification reverts to baseline safeguards.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_inspectorate, agenda_setter,
    institutional, generational, constrained, global).

% Negotiated and signed the framework as a bloc, controls the sequencing of relief, and holds the reimposition levers. Members disagree internally — some treat the text as legally binding, others as politically durable — but collectively they set what compliance earns and what deviation costs. Their relief commitments are reversible by domestic decision; their verification gains accumulate.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_one_coalition, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_one_coalition, beneficiary).

% Holds the largest relief lever through secondary-sanctions reach extending to any bank clearing dollars. Demonstrated in 2018 that a single administration can withdraw and reimpose full pressure while the other parties continue performing. Its commitments run on electoral timescales; its coercive instruments survive any exit.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, united_states_administration, agenda_setter,
    powerful, biographical, arbitrage, global).

% Bound party and domestic implementer: ships enriched stockpile abroad, caps enrichment levels, permits intrusive access, and in exchange receives sequenced relief — oil exports, aviation contracts, banking channels. Its concessions take years to rebuild if lost; the relief it receives can vanish by foreign decree. Domestic factions split over whether the exchange preserves the state's standing or surrenders leverage.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_government, payer,
    powerful, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_government, beneficiary).

% Technicians and organizations that dismantled centrifuge cascades, shipped uranium stockpile out of the country, and accepted continuous camera coverage of workshop floors. What they surrendered cannot be rebuilt quickly even if the rules lapse; their professional standing is tied to capabilities the framework freezes. They bear the deepest irreversible concession in the exchange.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_nuclear_program_operators, payer,
    organized, generational, trapped, national).

% Importers, airlines, and merchants who received the relief dividend: reopened trade finance, new aircraft deliveries, recovering oil revenue. Their livelihoods hang on channels that open and close with political weather in foreign capitals; they hold no lever over the sequence.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_import_sector, beneficiary,
    moderate, biographical, trapped, national).

% Diplomats, policy institutes, and political factions across the signatory capitals who championed negotiated calibration over pressure campaigns or strikes. They invest reputation in the framework's survival and argue each crisis should be priced, not escalated. Advocacy can migrate to new files, but their credibility rides on this one working.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    organized, generational, mobile, global).

% Manufacturers and energy companies that signed post-relief contracts — aircraft sales, shipping insurance, energy memoranda. Their exposure builds slowly through physical commitments and reputational ties; a reimposition forces a compliance scramble but not instant loss.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, european_export_firms, beneficiary,
    organized, biographical, constrained, continental).

% Banks that nominally gained relief yet stayed largely absent: dollar-clearing exposure means a reimposition decision in Washington reaches their correspondent accounts faster than any European protection can respond. They carry compliance overhead and foregone business throughout, pricing reversal risk into every Iran-linked transaction even while the framework stands.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, european_financial_institutions, payer,
    institutional, biographical, constrained, continental).

% Security establishments of neighboring states excluded from the negotiation. They absorb the risk of retained research-and-development pathways and sunset-bound limits, and their preferred answer — maximal pressure or preemption — is structurally subordinated to the commission's case-by-case process. They work parallel channels (legislatures, media, allied capitals) that the framework does not govern.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_adversary_security_establishments, excluded,
    powerful, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_adversary_security_establishments, payer).

% Researchers and former inspectors who publish breakout-time estimates, compliance scorecards, and precedent analyses. They see the full structure — who concedes what, who holds which lever — and their assessments feed legislative debate in every capital. No stake in outcomes beyond accuracy.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, arms_control_analytical_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__graduated_compliance_reading, diffuse).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__graduated_compliance_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the proliferation-management collective-action problem: converts unverifiable mutual suspicion — an advancing nuclear program on one side, a pressure campaign on the other — into a verified exchange, with enrichment limits and inspector access delivered against sequenced sanctions relief, and a standing commission to price and stage responses to deviations.
% TRANSFER_FUNCTION: Moves economic access (sanctions relief, oil revenue, trade finance, aviation contracts) from the sanctioning powers to Iran, and moves nuclear-program capacity (centrifuges, enriched stockpile, R&D headroom) out of Iranian hands toward verified limits; incidentally moves escalation risk away from all parties.
% ABSENT_VOICES: Regional adversary security establishments were excluded from the table, as were Iranian civil-society voices and the US Congress as a ratifying body. They would contest the graduated calibration itself — arguing violations warrant maximal rather than proportional response — and their exclusion fed the legitimacy deficit that parallel-channel lobbying converted into resistance the calibration never priced.
% DISAPPEARANCE_RATIONALE: If the graduated framework vanished overnight, relief channels would close, the enhanced inspection mandate would lapse to baseline safeguards, Iran's program would resume unconstrained expansion, and regional escalation planning would revert to its pre-deal footing — the entire verification-for-relief economy and the commission calendar rearrange around its absence.
% FOUNDING_PROBLEM: Iran's advancing nuclear program on a collision course with a military-option-or-containment dilemma: interim agreements from 2013 had shown that reciprocal exchange could freeze the program, and a durable calibrated framework was negotiated to hold breakout time above one year while unwinding the sanctions architecture step by step.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: IAEA Board of Governors reporting documents the pre-deal program advance that motivated the framework; Israeli and Gulf security briefings independently attest the collision-course problem — from the seat of governments who wanted harder answers than the deal gave; US Congressional Research Service analyses record the breakout-time rationale. External verification bodies and adversarial governments corroborate both the problem and its persistence; the signatories' own attestation alone would not suffice.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. The claimed_type tangled_rope states what I believe structurally true of this reading: a genuine coordination function (verified exchange converting mutual suspicion into sequenced performance) AND asymmetric burden (sticky concessions against revocable relief; residual exposure chilling financiers; subordinated regional preferences) AND active enforcement (commission calendar, inspector access, snapback architecture). The metrics describe observed operation. Extractiveness starts moderate-low (0.34) as relief flows on schedule, then climbs after 2018 because obligations persisted while the largest relief lever was pulled unilaterally — the calibration's core promise broke from the relief side, ending at 0.56. Theater_ratio rises from 0.18 to 0.50: early commission sessions were functional (heavy-water and uranium-shipment deviations resolved inside the mechanism), while post-2018 sessions increasingly convened around a hollowing core — meetings maintained as the reciprocity they administered decayed. Suppression shows a deliberate divergence the commentary must make explicit: the base scalar 0.42 is the raw structural property — the persistent legal suppressive architecture (Resolution 2231 snapback provision, reimposable secondary-sanctions reach) — while the suppression_requirement series DECLINES after 2018 (0.42 to 0.26) because that series tracks active multilateral enforcement capacity, which decayed once one party left and the commission could no longer hold a non-performing lever-holder. Legal shell persisting, active enforcement decaying: both facts are real and they are different facts. Accessibility_collapse is 0.45: alternative courses (military action, maximal-pressure campaigns, bilateral tracks) were partially foreclosed while the framework operated but re-emerged after exit — alternatives bent, did not collapse. Resistance is 0.62: congressional opposition, regional lobbying through parallel channels, and post-2019 Iranian threshold-walking all pressed against the calibration. All three series share one time grid (2015-2021, annual) so the engine samples aligned rows. Receipt surface, affirmatively checked: I re-read every stakeholder situation looking for a seat that pockets the extraction — surrendered program capacity accrues to no pocket (destroyed/delayed capability, security-diffuse), chilled financier trade is deadweight accruing to nobody, subordinated regional preferences accrue to the process itself — so gain_flow is 'diffuse' as a checked universal negative, not a default. fixing_cost is 'cheap': for whoever held the relief lever, exit was a single domestic decision at low direct cost, demonstrated in 2018 — and that cheapness is precisely the structural weakness this reading's calibration presupposes away.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute sharply different types per seat. From the agenda-setter seats (joint_commission_mechanism, p5_plus_one_coalition) the arrangement presents as functioning calibrated diplomacy — coordination-forward, extraction near the inherent-cost band. From the sticky-concession payer seat (iranian_nuclear_program_operators) the same structure presents as asymmetric exposure: what they surrendered takes years to rebuild, what the counterpart gave can vanish overnight — extraction-forward. Same-level lateral dynamics differentiate two seats of equal nominal standing: european_export_firms and european_financial_institutions sit in the same jurisdictions under the same nominal relief, but their exit options diverge on asset type — exporters built slow-moving physical and reputational exposure, while banks sit inside a dollar-clearing chokepoint where a reimposition decision lands faster than any European protection can respond; identical standing, opposite experienced constraint. The united_states_administration seat is the extreme case: arbitrage-grade exit meant the reciprocity premise never bound from that seat at all — the arrangement was always optional there, which is exactly what the transactional sibling reading generalizes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. pragmatic_diplomacy_advocates, european_export_firms, and iranian_import_sector sit near the beneficiary end (d low): the framework subsidizes their advocacy credibility, contract books, and reopened trade channels respectively. iranian_nuclear_program_operators, european_financial_institutions, and regional_adversary_security_establishments sit near the target end (d high): the first bear the deepest irreversible concessions, the second bear permanent residual-exposure taxation on every Iran-linked transaction, the third bear uncompensated latent-capability risk with their preferred response structurally subordinated. iranian_government is dual-listed in both arrays deliberately: it receives sequenced relief and surrenders program capacity through the same structure, so its derived d should land mid-range — the honest signature of a party both subsidized and bound. No directionality_overrides are used: the derivation from declarations plus exit atoms produces the right shape, and an override would have to key on a power atom shared by seats with genuinely different directionalities (three institutional seats: commission administrator, financier, coalition — one override key would clobber two of them). Suppression stays unscaled in my account — it is the raw structural property; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — managing Iran's nuclear latency through verified exchange rather than collision — remains live, so founding_problem_status is 'live' paired with disappearance_verdict 'world_rearranges': no dead-mandate mismatch fires, correctly, because the problem the arrangement was built for has not gone away even as the arrangement decayed. The tangled_rope claim does double preventive work: it keeps the coordination function legible against a pure-snare mislabel that collapse-era headlines invite, and the victims array blocks a rope-whitewash that the reading's own proportionality rhetoric would otherwise license. The piton boundary is visible in the data and worth naming: theater_ratio reaching 0.50 by 2021 marks where commission maintenance risks becoming performance around a dead reciprocity — if the machinery persists while the exchange it administers is dead, the residue computes piton-flavored. The receipt surface explains why decay ran through exit rather than neglect: gains were diffuse (no capturer defending the structure) and fixing was cheap for the lever-holder, so nothing concentrated defended it and one seat could walk. That combination — diffuse gains, cheap exit, live problem — is the signature of a coordination structure whose enforcement was never strong enough to make its own promises binding, which is the precise fault line between this reading and its binding_multilateral sibling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_jcpoa_bindingness,
    'This constraint is one reading (graduated_compliance_reading) of kernel jcpoa_treaty_bindingness; which reading governs the text''s obligatoriness, and what would each sibling change structurally?',
    'Track which reading successive party coalitions operationalize: consensus-defense behavior by the E3 evidences the binding_multilateral_reading; case-by-case calibration behavior evidences this graduated reading; unilateral exit followed by renegotiation demands evidences the transactional_provisional_reading.',
    'Under the binding_multilateral sibling, persistence and suppression rise (consensus-gated dissolution hardens the shell); under the transactional_provisional sibling, persistence and exit cost fall (the arrangement is voidable at will); this file''s moderate calibration holds only while the graduated reading is operative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_jcpoa_bindingness, conceptual, 'Committer-frame routing: one reading of a three-reading kernel; siblings are separate constraints linked via network.affects_constraints.').

omega_variable(
    relief_lever_symmetry,
    'Is the graduated response actually symmetric in operation, or does the relief lever''s instant revocability against the program lever''s multi-year irreversibility break the reciprocity the calibration presupposes?',
    'Compare timing and magnitude of relief withdrawals against assessed violation severity across Joint Commission cases 2016-2021; measure rebuild lead-times for surrendered program capacity against relief-restoration lead-times.',
    'If the asymmetry is confirmed, effective extraction exceeds the calibrated design value and the arrangement drifts snare-ward from the payer seats; if roughly symmetric, the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relief_lever_symmetry, empirical, 'Whether proportional calibration survived the lever-speed asymmetry between revocable relief and sticky concessions.').

omega_variable(
    snapback_trigger_distribution,
    'Does the snapback mechanism function as graduated enforcement available to all parties, or as a unilateral-reversion instrument effectively usable only by the relief-lever holder?',
    'Examine the Resolution 2231 procedural chain: who can initiate, what notice periods run, whether any non-permanent-member party ever held a credible independent trigger.',
    'A unilateral-only snapback converts the graduated machinery into asymmetric leverage, raising effective extraction for every seat that cannot trigger it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(snapback_trigger_distribution, empirical, 'Distribution of the reversion trigger across parties versus concentration in one lever-holder.').

omega_variable(
    deescalation_accountability_tradeoff,
    'Does prioritizing de-escalation over formal legal closure systematically under-price violations, trading accountability for stability?',
    'Audit Joint Commission dispositions against the severity scale the annexes imply: were breach responses consistently below implied severity?',
    'Systematic under-pricing means the calibration collects compliance concessions while returning less relief than owed — a hidden transfer running through the dispute mechanism itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deescalation_accountability_tradeoff, conceptual, 'Whether the de-escalation priority embeds a systematic discount on breach severity.').

omega_variable(
    annex_sunset_scope_ambiguity,
    'Do the time-limited technical annexes (Day 15 / Year 10 / Year 15 provisions) make the whole arrangement transitional, or are they component-level expiries inside a durable reciprocal shell?',
    'Test whether the reciprocal-commitment core (verification-for-relief exchange, commission process) was designed to survive annex expiry under continued performance, as the parties'' successor-negotiation language implies.',
    'If the whole is transitional, the constraint is scaffold-shaped and this file''s tangled_rope claim overstates durability; if component-level, the shell persists and the claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annex_sunset_scope_ambiguity, conceptual, 'Scope ambiguity of the sunset provisions: whole-arrangement transition versus component expiry.').

omega_variable(
    regional_exclusion_persistence_cost,
    'Does excluding regional adversary security establishments from the table impose a persistence cost — a legitimacy deficit funding resistance — that the graduated calibration never prices?',
    'Correlate regional lobbying intensity and parallel-pressure legislation in third capitals with commission decision points 2015-2021.',
    'If exclusion drives resistance, part of the measured resistance is exogenous to the calibration''s fairness — the arrangement pays a fixed legitimacy tax that proportional responses cannot retire.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_exclusion_persistence_cost, empirical, 'Whether the excluded-seat structure generates resistance the calibration cannot absorb.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 2015, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement_basis(jcpo_tr_t2015, observed).
narrative_ontology:measurement(jcpo_tr_t2016, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement_basis(jcpo_tr_t2016, observed).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2017, 0.24).
narrative_ontology:measurement_basis(jcpo_tr_t2017, observed).
narrative_ontology:measurement(jcpo_tr_t2018, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2018, 0.33).
narrative_ontology:measurement_basis(jcpo_tr_t2018, observed).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2019, 0.4).
narrative_ontology:measurement_basis(jcpo_tr_t2019, observed).
narrative_ontology:measurement(jcpo_tr_t2020, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2020, 0.46).
narrative_ontology:measurement_basis(jcpo_tr_t2020, observed).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2021, 0.5).
narrative_ontology:measurement_basis(jcpo_tr_t2021, observed).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2015, 0.34).
narrative_ontology:measurement_basis(jcpo_be_t2015, observed).
narrative_ontology:measurement(jcpo_be_t2016, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2016, 0.36).
narrative_ontology:measurement_basis(jcpo_be_t2016, observed).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2017, 0.39).
narrative_ontology:measurement_basis(jcpo_be_t2017, observed).
narrative_ontology:measurement(jcpo_be_t2018, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2018, 0.47).
narrative_ontology:measurement_basis(jcpo_be_t2018, observed).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2019, 0.51).
narrative_ontology:measurement_basis(jcpo_be_t2019, observed).
narrative_ontology:measurement(jcpo_be_t2020, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2020, 0.54).
narrative_ontology:measurement_basis(jcpo_be_t2020, observed).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2021, 0.56).
narrative_ontology:measurement_basis(jcpo_be_t2021, observed).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement_basis(jcpo_su_t2015, observed).
narrative_ontology:measurement(jcpo_su_t2016, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2016, 0.36).
narrative_ontology:measurement_basis(jcpo_su_t2016, observed).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2017, 0.38).
narrative_ontology:measurement_basis(jcpo_su_t2017, observed).
narrative_ontology:measurement(jcpo_su_t2018, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement_basis(jcpo_su_t2018, observed).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2019, 0.36).
narrative_ontology:measurement_basis(jcpo_su_t2019, observed).
narrative_ontology:measurement(jcpo_su_t2020, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2020, 0.31).
narrative_ontology:measurement_basis(jcpo_su_t2020, observed).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2021, 0.26).
narrative_ontology:measurement_basis(jcpo_su_t2021, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, resource_allocation).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).

% DUAL FORMULATION NOTE:
% Constraint family jcpoa_treaty_bindingness decomposes one colloquial label ('the Iran deal's bindingness') into three structurally distinct constraints per the ε-invariance principle: this graduated_compliance_reading (moderate ε, obligatoriness priced by calibration), binding_multilateral_reading (higher persistence and suppression — consensus-gated shell), and transactional_provisional_reading (lower persistence, cheap exit — voidable bargain). ε differs across the family because each reading prices the same text's obligatoriness differently; the upstream graduated operation (case-by-case normalization of deviations) feeds downstream structural pressure into the transactional reading's environment. All three files link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
