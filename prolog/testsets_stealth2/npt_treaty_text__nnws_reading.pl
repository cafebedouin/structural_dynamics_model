% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Grand Bargain — Non-Nuclear-Weapon-State Reading (Binding Article VI)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   The Non-Proliferation Treaty's text is a single persisting commitment,
 *   but it is read differently by the parties it binds asymmetrically. This
 *   file instantiates ONE reading — the non-nuclear-weapon-state reading —
 *   under which Article VI disarmament is a binding legal obligation and
 *   non-proliferation is conditional restraint offered in exchange for
 *   nuclear-weapon-state compliance. The standing arrangement under contest,
 *   assessed by this reading's own lights, is the operative bargain:
 *   non-nuclear-weapon states pay verified, inspected, permanent restraint
 *   costs while the reciprocal leg carries no verification, no timetable, and
 *   no body empowered to judge performance. Per the epsilon-invariance
 *   principle, the sibling readings (nws_reading,
 *   withdrawal_threshold_reading) are separate constraint stories with their
 *   own epsilon values, victim sets, and classifications; they are linked
 *   through the network, not folded into this one. KEY AGENTS (by structural
 *   relationship): see key_agents; the load-bearing asymmetry is that the
 *   seat administering enforcement (via Security Council vetoes and
 *   depositary roles) is the same seat that collects the bargain's principal
 *   gains.
 *
 * KEY AGENTS:
 *   - nnws_treaty_parties: Primary target seat ([organized]/[constrained]) — bears verified restraint costs while pressing the unenforced reciprocal leg from inside; coalition-capable (NAM, NPDI, Humanitarian Initiative)
 *   - nuclear_weapon_states: Primary beneficiary seat ([institutional]/[arbitrage]) — collects arsenal legitimacy and rivals' restraint; administers enforcement through Security Council vetoes and depositary roles; interpretive flexibility on Article VI
 *   - iaea_secretariat: Administrator ([institutional]/[constrained]) — runs verification on one side of the bargain only; budget- and access-dependent on member states
 *   - tpnw_states_parties: Mobilized beneficiary bloc ([organized]/[mobile]) — pursues the reciprocal leg through a parallel instrument; demonstrates that alternatives are not suppressed
 *   - nuclear_latency_allies: Same-level lateral variant ([powerful]/[constrained]) — restraint purchased by extended deterrence rather than bargain reciprocity; hedging closed by alliance discipline
 *   - threshold_states_under_pressure: Suppressed margin ([moderate]/[trapped]) — bears the regime's coercive edge; sanctions and referral replace consent as the binding force
 *   - humanitarian_initiative_coalition: Excluded voice ([organized]/[mobile]) — forced the contest outside the consensus process to majority voting
 *   - international_humanitarian_law_scholars: Analytical observer ([analytical]/[analytical]) — adjudicates the bindingness question the regime itself refuses to reach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.6).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.4).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Grand Bargain — Non-Nuclear-Weapon-State Reading (Binding Article VI)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, '6f785b38-914e-4193-956e-0d52bdbc21c7').
narrative_ontology:cs_kernel_codification('6f785b38-914e-4193-956e-0d52bdbc21c7', fixed_text).
narrative_ontology:cs_authority_grounding('6f785b38-914e-4193-956e-0d52bdbc21c7', lineage).
narrative_ontology:cs_interpretation_layer_present('6f785b38-914e-4193-956e-0d52bdbc21c7').
narrative_ontology:cs_reading_relation('6f785b38-914e-4193-956e-0d52bdbc21c7', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f785b38-914e-4193-956e-0d52bdbc21c7', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('6f785b38-914e-4193-956e-0d52bdbc21c7', foundational, article_vi_binding_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6f785b38-914e-4193-956e-0d52bdbc21c7', article_vi_binding_obligation, conventional).
narrative_ontology:cs_axiom('6f785b38-914e-4193-956e-0d52bdbc21c7', foundational, nonproliferation_conditional_consideration).
narrative_ontology:cs_axiom_status(nonproliferation_conditional_consideration, holdable).
narrative_ontology:cs_axiom_grounding('6f785b38-914e-4193-956e-0d52bdbc21c7', nonproliferation_conditional_consideration, conventional).
narrative_ontology:cs_reference_frame('6f785b38-914e-4193-956e-0d52bdbc21c7', balanced_reciprocal_bargain).
narrative_ontology:cs_drift_state('6f785b38-914e-4193-956e-0d52bdbc21c7', contemporary_tpnw_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6f785b38-914e-4193-956e-0d52bdbc21c7', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, nnws_treaty_parties).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nnws_treaty_parties).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, threshold_states_under_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, tpnw_states_parties).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, nuclear_latency_allies).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, tpnw_states_parties).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nuclear_latency_allies).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, article_vi_binding_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, grand_bargain_reciprocity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that joined the treaty, renounced nuclear weapons, and accept IAEA inspections of their nuclear facilities. They receive peaceful-nuclear cooperation and operate inside a regime where their neighbors' restraint is likewise locked in. Every five years they attend Review Conferences where they press the nuclear-armed parties on their disarmament commitments; since 2017 a bloc of them has also joined a parallel prohibition treaty. Leaving is legally possible on three months' notice, but a departing state faces sanctions, stigma, and loss of cooperation benefits, so most stay and press from inside.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nnws_treaty_parties, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, nnws_treaty_parties, beneficiary).

% The five states the treaty recognizes as nuclear-armed. They keep their arsenals under the treaty's legitimacy umbrella, modernize them, and report voluntary disarmament steps at Review Conferences. They hold the system's enforcement levers through Security Council vetoes and depositary roles, and they uniformly rejected the parallel prohibition treaty. Their Article VI undertakings have no verification mechanism and no body empowered to judge their performance.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, nuclear_weapon_states, agenda_setter).

% The secretariat that designs and applies safeguards, inspects facilities, and reports non-compliance to the Security Council. Its verification remit covers the non-nuclear-weapon side of the bargain; it has no corresponding mandate over the arsenals of the recognized nuclear-armed states. Its budget, inspector access, and referral prospects all depend on member-state cooperation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, iaea_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% A bloc of mostly non-nuclear-weapon states that negotiated and joined a 2017 treaty prohibiting nuclear weapons outright. They describe the move as collecting on the disarmament promise the older treaty left unenforced, and they campaign inside Review Conferences for timelines and accountability mechanisms. They remain bound by their existing non-proliferation obligations while adding the prohibition commitments.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, tpnw_states_parties, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, tpnw_states_parties, payer).

% Industrialized non-nuclear-weapon states — treaty allies of the United States such as Japan, South Korea, and NATO hosts — that could build weapons quickly if they chose but rely on American extended deterrence instead. They accept the regime's restraint and participate in nuclear planning; their hedging options are politically closed by alliance discipline, and their security guarantee is issued by the same government whose arsenal the bargain asks them to press against.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_latency_allies, payer,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, nuclear_latency_allies, beneficiary).

% States whose nuclear programs have drawn the regime's enforcement attention — investigated, sanctioned, or referred to the Security Council when they moved toward weapons capability. Their facility access, financing, and diplomatic standing depend on staying inside the verification system, and the option the treaty forecloses is the one their regional security situations most tempt them toward.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, threshold_states_under_pressure, payer,
    moderate, biographical, trapped, regional).

% Civil-society campaigns, affected-community representatives including hibakusha, and like-minded small and middle powers that pressed the humanitarian case inside Review Conference halls for years and found the consensus process unreceptive. They took the argument to the General Assembly, where majority voting replaced consensus, and produced the prohibition treaty that the nuclear-armed states boycott.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, humanitarian_initiative_coalition, excluded,
    organized, generational, mobile, global).

% Legal scholars and jurists in the International Court of Justice tradition who analyze what Article VI actually requires. The Court's 1996 advisory opinion found an obligation to pursue and bring to conclusion good-faith disarmament negotiations while declining to set a timetable; commentary since has divided over whether that makes the obligation enforceable against a specific state.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, international_humanitarian_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nnws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the proliferation cascade problem: mutual, verified renunciation of nuclear weapons removes the security-dilemma spiral behind 1960s forecasts of dozens of nuclear states, pools verification in a single inspectorate, and channels peaceful nuclear trade under non-proliferation conditions. Each party's restraint is worthwhile only because the others' restraint is locked in and checked.
% TRANSFER_FUNCTION: Moves verified restraint — foregone weapons options, intrusive inspections, foreclosed latency — from non-nuclear-weapon parties to the regime; moves security assurances and peaceful-technology access from the nuclear-armed and supplier states to non-nuclear-weapon parties; and moves the promised disarmament leg from the nuclear-armed parties only prospectively, with no transfer yet collected on that leg.
% ABSENT_VOICES: The states never inside the treaty — India, Pakistan, Israel — object that it freezes a caste line they refuse to accept, and they sit outside the room entirely. Affected communities and civil society occupied Review Conference margins for decades before moving the argument to the General Assembly. Threshold states speak mainly when accused. The consensus rule lets any single party erase objections from outcome documents, so dissent survives in walkouts, reservations, and parallel forums rather than in the record.
% DISAPPEARANCE_RATIONALE: Overnight disappearance unravels the verification system, the export-control regime built around it, extended-deterrence assurances calibrated to allied restraint, and the normative barrier every proliferator must cross; regional hedging cascades begin within years, and the five arsenals lose the treaty-legitimized perimeter that organizes deterrence relationships worldwide.
% FOUNDING_PROBLEM: Early-1960s forecasts of twenty-plus nuclear states within a decade, runaway Cold War arms racing, and the 1967-68 negotiators' design: freeze the haves/have-nots line permanently while paying the have-nots in promises — eventual disarmament and peaceful-technology access — as the price of their renunciation.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the International Court of Justice's 1996 advisory opinion corroborates a continuing legal obligation to pursue disarmament negotiations; General Assembly majority resolutions and the 2017 prohibition-treaty vote corroborate that a large majority of states treat the disarmament leg as unfulfilled; SIPRI inventory data corroborate that the five arsenals are being modernized rather than eliminated. The nuclear-armed parties attest the opposite — steady step-by-step fulfillment — and no external body verifies their performance; that asymmetry of corroboration is itself signal.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The manifest hypothesized rope with moderate epsilon; I claim tangled_rope because the reading's own lights identify BOTH a genuine coordination function (the proliferation cascade is a real collective-action problem the treaty measurably solves) AND asymmetric extraction running through the same structure (one side's obligations are verified and enforced, the other's are neither), sustained by active enforcement machinery. Epsilon is 0.60 — moderate-high, not snare-grade — because the coordination good is genuinely delivered and valued, coercion on the consenting majority is light, and alternatives (TPNW, Article X exit) remain open, which caps accessibility_collapse at 0.40. Resistance is high (0.62) because organized constituencies actively contest the asymmetry: the Humanitarian Initiative, the TPNW campaign, and repeated Review Conference blockages. Theater_ratio (0.52) tracks the ritualization of the disarmament leg: recycled consensus language, voluntary reporting nobody audits, and Review Conferences that ended with no outcome document at all in 2015 and 2022. The suppression_requirement series is authored deliberately because enforcement CAPACITY is the traced dynamic: it builds through the 1990s-2000s (post-Iraq verification hardening, Additional Protocol era, Security Council sanctions regimes) and decays after 2017 (great-power paralysis culminating in the termination of the DPRK panel of experts) — a ratchet on the non-nuclear-weapon side followed by general enforcement erosion. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream. The series oscillates on the five-yearly Review Conference cycle — hope, draft text, blockage, recrimination — and that cycle itself functions as intermittent reinforcement, resetting non-nuclear-weapon-state investment every cycle without delivering the reciprocal leg; the 1995 spike (indefinite extension without strengthened guarantees) and 2000 dip (Thirteen Steps) are the clearest event-driven movements.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the finding. From the nuclear_weapon_states seat the arrangement presents as a stable coordination achievement they administer and partially honor: security goods delivered, proliferation prevented, Article VI read as progressive fulfillment — a rope-shaped world. From the nnws_treaty_parties seat the same structure operates as a bargain honored on one side only: verified costs paid against an unenforced promise — the tangled-rope signature. From the threshold_states_under_pressure seat the regime is simply coercive closure. The tpnw_states_parties seat reads the non-performance as breach warranting a parallel instrument. The engine computes these per-seat classifications from the structural data; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. nnws_treaty_parties: declared victim (primary) and beneficiary (secondary), constrained exit — the verified-cost side dominates, placing them well toward the target end. nuclear_weapon_states: declared beneficiary with agenda-setting secondary role and arbitrage-grade exit (interpretive flexibility, veto power, modernization without formal breach) — nearest the beneficiary end, and the seat the gains demonstrably accrue to (hence gain_flow). threshold_states_under_pressure: victim with trapped exit — the furthest toward full-target, since trapped targets sit nearer the full-target end than mobile ones. nuclear_latency_allies: mixed position — they pay restraint costs but collect the extended-deterrence good; mid-range. tpnw_states_parties: beneficiaries-in-pursuit with mobile exit (they built an alternative) — below symmetric. iaea_secretariat: administrator, roughly symmetric — it runs machinery, collects no rents. No directionality_overrides are used: the derivation from role declarations plus exit options captures the structure, and the override mechanism's power-atom keying could not separate the two institutional seats (nuclear_weapon_states vs iaea_secretariat) anyway — their differentiation comes from role and exit, which the derivation reads.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the proliferation cascade — is still live, so this is not a resolved mandatrophy and the arrangement cannot be dismissed as vestigial. But the bargain's two legs are aging at different rates: the non-proliferation leg remains functional while the disarmament-reciprocity leg is atrophying toward theatrical maintenance (rising theater_ratio, unverified voluntary reporting, outcome-free conferences). The R5 mismatch consumer reads founding_problem_status=contested x disappearance_verdict=world_rearranges, which does not fire the dead-plus-rearranging zombie flag — correctly, since the cascade problem is real — but the half-life is documented here: if TPNW pressure fails and Review Conferences continue producing nothing, the reciprocity function completes its decay into ritual and the arrangement drifts toward the piton pathway, administered by seats that could change it at a cost none of them will bear. The mandatrophy question is therefore deferred, not resolved, and the temporal series is the instrument that will date the transition if it comes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of kernel npt_treaty_text (reading: nnws_reading). What changes structurally if the nws_reading frame prevails — Article VI demoted from binding obligation to aspiration?',
    'Cross-reading corpus comparison of the sibling stories; institutional indicators: whether any Review Conference outcome document acquires enforcement language, whether any nuclear-weapon state joins a prohibition instrument, whether reporting acquires consequences.',
    'Under the nws_reading frame the victim set collapses to safeguarded non-nuclear-weapon states alone, the binding-obligation leg leaves the constraint entirely, and the classification migrates toward a stable coordination arrangement carrying legacy rhetoric — a different epsilon over the same text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a contested treaty-text kernel; sibling readings change the victim set and the bindingness scope.').

omega_variable(
    article_vi_justiciability,
    'Can the Article VI obligation ever be adjudicated against a nuclear-weapon state, or is this reading''s enforcement path structurally closed?',
    'Test venues: acceptance of ICJ contentious jurisdiction, a General Assembly-requested follow-up advisory opinion containing a timeline question, or a Review Conference consensus document imposing reporting deadlines with review consequences.',
    'If justiciability is closed, the reading''s pressure tools reduce to stigma and regime competition, capping enforcement permanently and fixing the asymmetry; if opened, the bargain acquires an enforcement leg and the extraction asymmetry narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, empirical, 'Whether the binding-obligation reading has any reachable enforcement venue.').

omega_variable(
    tpnw_complementarity_or_rivalry,
    'Does the prohibition-treaty bloc strengthen the original bargain by raising the cost of non-performance, or fragment it by giving dissatisfied parties a symbolic exit that relieves pressure on the original text?',
    'Track Review Conference behavior of prohibition-treaty parties across successive cycles: whether membership converts into bargaining leverage (deadlines, accountability mechanisms) or settles into parallel-track symbolism; monitor nuclear-weapon-state responses.',
    'Complementarity supports the tangled-rope reading — resistance disciplining a real bargain; rivalry would indicate the coordination core is losing its dissatisfied members'' investment, accelerating atrophy of the reciprocity function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tpnw_complementarity_or_rivalry, empirical, 'Whether regime competition disciplines or hollows the original bargain.').

omega_variable(
    suppression_side_asymmetry,
    'The regime''s coercive apparatus — inspections, sanctions, Council referral — applies almost entirely to the non-nuclear-weapon side; is that one-sidedness intrinsic to verification or a designed entrenchment of the arsenal-holders'' position?',
    'Compare verification burdens and compliance consequences across the two sides of the bargain; examine whether any proposed symmetry (managed access for arsenal states, fissile-material cutoff verification) survives negotiation.',
    'If intrinsic, part of the measured asymmetry is the unavoidable cost of verification and the bargain reads closer to balanced; if designed, the suppressive structure is itself the extraction mechanism and the arrangement sits nearer the pure-capture end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_side_asymmetry, empirical, 'Whether one-sided suppression is functional necessity or positional entrenchment.').

omega_variable(
    withdrawal_exit_realism,
    'Is Article X withdrawal a credible check that keeps the bargain honest for non-nuclear-weapon states, or is the exit so sanction-laden that the seat is effectively locked?',
    'Compare outcomes of actual and attempted withdrawals with the pre-withdrawal bargaining gains of states that pressed from inside; assess sanction severity, stigma persistence, and reintegration costs across cases.',
    'A credible exit moderates the target-seat directionality (voice backed by exit threat); a locked exit amplifies it, raising effective extraction on the non-nuclear-weapon seat and sharpening the asymmetry this reading protests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_exit_realism, empirical, 'Whether the treaty''s exit clause functions as a real constraint-check or nominal permission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_nnws_tr_t0, npt_treaty_text__nnws_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(npt_nnws_tr_t0, observed).
narrative_ontology:measurement(npt_nnws_tr_t15, npt_treaty_text__nnws_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(npt_nnws_tr_t15, observed).
narrative_ontology:measurement(npt_nnws_tr_t25, npt_treaty_text__nnws_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement_basis(npt_nnws_tr_t25, observed).
narrative_ontology:measurement(npt_nnws_tr_t30, npt_treaty_text__nnws_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement_basis(npt_nnws_tr_t30, observed).
narrative_ontology:measurement(npt_nnws_tr_t40, npt_treaty_text__nnws_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement_basis(npt_nnws_tr_t40, observed).
narrative_ontology:measurement(npt_nnws_tr_t47, npt_treaty_text__nnws_reading, theater_ratio, 47, 0.44).
narrative_ontology:measurement_basis(npt_nnws_tr_t47, observed).
narrative_ontology:measurement(npt_nnws_tr_t52, npt_treaty_text__nnws_reading, theater_ratio, 52, 0.5).
narrative_ontology:measurement_basis(npt_nnws_tr_t52, observed).
narrative_ontology:measurement(npt_nnws_tr_t55, npt_treaty_text__nnws_reading, theater_ratio, 55, 0.52).
narrative_ontology:measurement_basis(npt_nnws_tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(npt_nnws_be_t0, npt_treaty_text__nnws_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(npt_nnws_be_t0, observed).
narrative_ontology:measurement(npt_nnws_be_t15, npt_treaty_text__nnws_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(npt_nnws_be_t15, observed).
narrative_ontology:measurement(npt_nnws_be_t25, npt_treaty_text__nnws_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(npt_nnws_be_t25, observed).
narrative_ontology:measurement(npt_nnws_be_t30, npt_treaty_text__nnws_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement_basis(npt_nnws_be_t30, observed).
narrative_ontology:measurement(npt_nnws_be_t40, npt_treaty_text__nnws_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement_basis(npt_nnws_be_t40, observed).
narrative_ontology:measurement(npt_nnws_be_t47, npt_treaty_text__nnws_reading, base_extractiveness, 47, 0.55).
narrative_ontology:measurement_basis(npt_nnws_be_t47, observed).
narrative_ontology:measurement(npt_nnws_be_t52, npt_treaty_text__nnws_reading, base_extractiveness, 52, 0.58).
narrative_ontology:measurement_basis(npt_nnws_be_t52, observed).
narrative_ontology:measurement(npt_nnws_be_t55, npt_treaty_text__nnws_reading, base_extractiveness, 55, 0.6).
narrative_ontology:measurement_basis(npt_nnws_be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt_nnws_su_t0, npt_treaty_text__nnws_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(npt_nnws_su_t0, observed).
narrative_ontology:measurement(npt_nnws_su_t15, npt_treaty_text__nnws_reading, suppression_requirement, 15, 0.33).
narrative_ontology:measurement_basis(npt_nnws_su_t15, observed).
narrative_ontology:measurement(npt_nnws_su_t25, npt_treaty_text__nnws_reading, suppression_requirement, 25, 0.4).
narrative_ontology:measurement_basis(npt_nnws_su_t25, observed).
narrative_ontology:measurement(npt_nnws_su_t30, npt_treaty_text__nnws_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(npt_nnws_su_t30, observed).
narrative_ontology:measurement(npt_nnws_su_t40, npt_treaty_text__nnws_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(npt_nnws_su_t40, observed).
narrative_ontology:measurement(npt_nnws_su_t47, npt_treaty_text__nnws_reading, suppression_requirement, 47, 0.48).
narrative_ontology:measurement_basis(npt_nnws_su_t47, observed).
narrative_ontology:measurement(npt_nnws_su_t52, npt_treaty_text__nnws_reading, suppression_requirement, 52, 0.44).
narrative_ontology:measurement_basis(npt_nnws_su_t52, observed).
narrative_ontology:measurement(npt_nnws_su_t55, npt_treaty_text__nnws_reading, suppression_requirement, 55, 0.4).
narrative_ontology:measurement_basis(npt_nnws_su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the NPT bargain' decomposes into at least three structurally distinct constraints — readings of one kernel. This file authors the nnws_reading alone: Article VI binding, restraint conditional. The nws_reading file authors the inverse emphasis (non-proliferation as the binding core, Article VI aspirational) with a different victim set and a different epsilon over the same text; the withdrawal_threshold_reading authors the Article X exit-threshold contest. Epsilon differs across the family because each reading fixes a different bindingness scope and beneficiary/victim structure; the stories are linked here and in their own network blocks, not merged. The upstream/downstream ordering runs from the nws_reading (the regime's original operational reality) to this reading (the 1995-and-after contestation), which is why this file lists it in downstream_of.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
