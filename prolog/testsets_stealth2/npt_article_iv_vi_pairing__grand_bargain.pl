% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Grand Bargain Reciprocity Reading (Articles IV-VI)
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   This story instantiates the grand_bargain reading of the NPT Article
 *   IV/VI pairing: the treaty is a reciprocal exchange in which non-weapon
 *   states accept verified restraint and weapon states owe disarmament
 *   progress, so that non-weapon-state restraint is conditional and Article
 *   VI failure corrodes the legitimacy of the Article IV entitlement and of
 *   the verification demands that ride on it. The standing arrangement under
 *   contest — the regime as actually operated since the 1995 indefinite
 *   extension, with arsenals retained and modernized while the disarmament
 *   leg stalls — is the epsilon referent, assessed by this reading's own
 *   lights: a bargain whose consideration runs in one direction. The claim
 *   and the metrics are independent authored facts: the reading is CLAIMED as
 *   tangled_rope (genuine coordination function plus asymmetric extraction,
 *   actively enforced), and the metrics describe substantially extractive
 *   operation with rising performative content — the engine computes per-seat
 *   types from the structural data and measures any divergence. KEY AGENTS
 *   (by structural relationship): - nuclear_weapon_states: agenda-setting
 *   collector (institutional/arbitrage) — retains arsenals, controls
 *   interpretation and enforcement levers - nonaligned_nnws: primary target
 *   (organized/constrained) — bears permanent restraint, financing, and
 *   foregone security option without reciprocation -
 *   nnws_alliance_dependents: dual-positioned protected party
 *   (powerful/identity_locked) — receives extended deterrence, pays
 *   compliance and defends the status quo - iaea_secretariat: administering
 *   verifier (institutional/constrained) — enforces the restraint leg,
 *   powerless over the disarmament leg - tpnw_states_parties: excluded critic
 *   (organized/mobile) — present in the room, absent from operative consensus
 *   - threshold_states_outside_treaty: excluded outsider (powerful/mobile) —
 *   addressed as a problem, never seated as a party - dprk_withdrawn_state:
 *   exit-precedent bearer (moderate/trapped) — exercised the exit clause and
 *   absorbed the repricing of exit for everyone else - arms_control_lawyers:
 *   analytical observer (analytical/analytical) — sees the full ledger of
 *   promises and deliveries
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: agenda_setter + beneficiary (institutional/arbitrage) — collects verified restraint and regime legitimacy while judging its own reductions sufficient
 *   - nonaligned_nnws: payer (organized/constrained) — bears the restraint, verification-financing, and foregone-weapons-option side of the exchange
 *   - nnws_alliance_dependents: beneficiary + payer (powerful/identity_locked) — protected by extended deterrence, fused with the alliance, resistant to reciprocity enforcement
 *   - iaea_secretariat: agenda_setter (institutional/constrained) — administers the restraint leg only; no mandate reaches the arsenals
 *   - tpnw_states_parties: excluded (organized/mobile) — built a parallel prohibition treaty the weapon states boycott
 *   - threshold_states_outside_treaty: excluded (powerful/mobile) — India, Pakistan, Israel hold unsafeguarded arsenals outside the bargain entirely
 *   - dprk_withdrawn_state: payer (moderate/trapped) — the only withdrawal, now sanctioned and isolated, its precedent raising everyone else's exit price
 *   - arms_control_lawyers: observer (analytical/analytical) — tracks negotiating history, justiciability, and the promise-delivery ledger
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.68).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.64).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Grand Bargain Reciprocity Reading (Articles IV-VI)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, 'f778d1da-caaf-440d-bcf3-2fd3a4a1d797').
narrative_ontology:cs_kernel_codification('f778d1da-caaf-440d-bcf3-2fd3a4a1d797', fixed_text).
narrative_ontology:cs_authority_grounding('f778d1da-caaf-440d-bcf3-2fd3a4a1d797', distributed).
narrative_ontology:cs_reading_relation('f778d1da-caaf-440d-bcf3-2fd3a4a1d797', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('f778d1da-caaf-440d-bcf3-2fd3a4a1d797', npt_article_iv_vi_pairing__abolitionist, influences).
narrative_ontology:cs_axiom('f778d1da-caaf-440d-bcf3-2fd3a4a1d797', foundational, article_vi_is_binding_reciprocal_obligation).
narrative_ontology:cs_axiom_status(article_vi_is_binding_reciprocal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('f778d1da-caaf-440d-bcf3-2fd3a4a1d797', article_vi_is_binding_reciprocal_obligation, conventional).
narrative_ontology:cs_axiom('f778d1da-caaf-440d-bcf3-2fd3a4a1d797', secondary, vi_breach_licenses_nnws_remedies).
narrative_ontology:cs_axiom_status(vi_breach_licenses_nnws_remedies, holdable).
narrative_ontology:cs_axiom_grounding('f778d1da-caaf-440d-bcf3-2fd3a4a1d797', vi_breach_licenses_nnws_remedies, empirically_contingent).
narrative_ontology:cs_reference_frame('f778d1da-caaf-440d-bcf3-2fd3a4a1d797', reciprocal_bargain_in_force).
narrative_ontology:cs_drift_state('f778d1da-caaf-440d-bcf3-2fd3a4a1d797', contemporary_post_extension_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f778d1da-caaf-440d-bcf3-2fd3a4a1d797', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, nnws_alliance_dependents).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, nonaligned_nnws).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, nnws_alliance_dependents).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, dprk_withdrawn_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five governments that tested nuclear devices before 1967 hold treaty-defined weapon status. They retain arsenals, run modernization programs, and control the enforcement levers through the Security Council veto and the consensus practice of review conferences. They affirm the disarmament article in conference statements while judging their own reductions sufficient. Leaving the treaty would cost them legitimacy, so they stay and manage interpretation instead.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states, beneficiary).

% Roughly a hundred non-weapon states, coordinated through the Non-Aligned Movement, accept verifiable restraint and comprehensive safeguards. They finance inspection systems, forgo the weapons option permanently, and in exchange receive peaceful-technology access and a disarmament promise renewed at each review cycle but not delivered. Withdrawal is legally available but carries sanctions risk, as the North Korean precedent shows; staying means pressing the reciprocity claim inside a process their opponents effectively chair.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nonaligned_nnws, payer,
    organized, generational, constrained, global).

% Non-weapon states inside Western security alliances host or rely on extended nuclear deterrence. They gain protection without owning warheads and resist interpretations of the disarmament article that would weaken the umbrella. Their defense planning, basing arrangements, and public security narratives are fused with the alliance, so rethinking the bargain from first principles is not a live option for them; they pay compliance and reputational costs while defending the arrangement that protects them.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nnws_alliance_dependents, beneficiary,
    powerful, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, nnws_alliance_dependents, payer).

% The verification agency administers safeguards agreements and additional protocols, reports noncompliance findings to its Board and the Security Council, and operates on member-assessed budgets. Its mandate expands with each compliance crisis, but its authority stops at what member states authorize; it cannot address arsenals held outside safeguards.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, iaea_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% States parties to the 2017 Treaty on the Prohibition of Nuclear Weapons attend review conferences as critics but hold no seat in the consensus that sets regime outcomes. They built a parallel treaty banning the weapons outright, which the weapon states and their allies boycott. Their arguments enter the room; their conclusions do not.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, tpnw_states_parties, excluded,
    organized, generational, mobile, global).

% India, Pakistan, and Israel never joined the treaty and maintain unsafeguarded arsenals; the regime addresses them as problems to be managed rather than parties to be bargained with. They trade outside the treaty's rules, face selective supplier restrictions, and engage on their own terms.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, threshold_states_outside_treaty, excluded,
    powerful, generational, mobile, regional).

% The only state to announce withdrawal from the treaty, it exercised the exit clause in 2003 and subsequently tested devices. It now lives under layered sanctions and diplomatic isolation, and its exit repriced departure for every other non-weapon state considering the same route.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, dprk_withdrawn_state, payer,
    moderate, biographical, trapped, regional).

% Treaty lawyers, verification specialists, and academic analysts track negotiating history, review-conference outcomes, and compliance practice. They publish the interpretations states cite, press the justiciability question in journals and advisory-opinion requests, and see the full ledger of promises made and deliveries owed.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, arms_control_lawyers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__grand_bargain, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Caps the number of nuclear-armed states and prevents proliferation cascades; provides verified assurance that non-weapon-state programs remain peaceful; enables cooperative peaceful-nuclear commerce under mutual confidence; preserves a framework within which disarmament negotiations could occur.
% TRANSFER_FUNCTION: Moves security restraint (the permanently forgone weapons option), verification transparency, and inspection financing from non-weapon states to the weapon states and the collective; moves — in promise rather than in delivery — disarmament progress and peaceful-technology access from weapon states to non-weapon states.
% ABSENT_VOICES: TPNW states parties and humanitarian-disarmament advocates attend as critics but are excluded from operative consensus by the package-deal and consensus practices the weapon states and their allies control; threshold states outside the treaty have no seat at all; communities downwind of historic test sites had no voice in any round of the bargain.
% DISAPPEARANCE_RATIONALE: If the reciprocity linkage vanished overnight, non-weapon-state restraint would become purely unilateral compliance with no legal footing for demanding anything back; Article IV expansion claims and Article X withdrawals would lose their stated justification; the review process would lose its central contested question; and the regime would either harden into naked enforcement of one-way restraint or fragment as the conditional-compliance rationale evaporated. Every named seat's position depends on the linkage existing, even the seats that oppose enforcing it.
% FOUNDING_PROBLEM: In the early 1960s the forecast was a cascade: dozens of states acquiring nuclear weapons within two decades. The bargain was built to cap the weapon-state number at the existing five while preserving peaceful-atom access for everyone else and holding out eventual disarmament as the price the five would ultimately pay.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's persistence is attested from outside the benefiting parties: IAEA verification reporting documents ongoing safeguards challenges; SIPRI and independent stockpile analyses document the arsenals; the ICJ 1996 advisory opinion and annual UN General Assembly votes (overwhelming majorities for disarmament-article action, opposed principally by the benefiting states themselves) attest both that the problem is live and that the disarmament leg remains undelivered. The weapon states attest the problem is live but dispute the breach characterization — their corroboration covers the problem, not the arrangement's performance.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.68 because the standing arrangement delivers the restraint leg permanently and the disarmament leg rhetorically: arsenals declined from Cold War peaks but plateaued, then modernization replaced reduction, while the 1995 extension converted a time-limited bargain into a permanent one without conditions attached. Suppression is 0.64 and is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled by the engine. The suppression figure reflects enforcement machinery (safeguards agreements, Additional Protocol, sanctions regimes, supplier-cartel pressure) aimed overwhelmingly at non-weapon-state compliance, with no counterpart mechanism reaching weapon-state arsenals. Theater ratio is 0.48, just below the proxy-substitution threshold: review conferences produce ever-longer consensus documents (13 Practical Steps, 64-point Action Plans) whose delivery rate approaches zero, while the verification function remains genuinely performed — real inspections, real findings, real technical work. Accessibility collapse is 0.5: withdrawal is legally available but was repriced by the North Korean precedent, and the TPNW exists as an alternative venue that the weapon states and their allies simply refuse to enter, so alternatives are neither fully closed nor genuinely open. Resistance is 0.62: sustained Non-Aligned Movement pressure at every review cycle, the humanitarian initiative, annual overwhelming General Assembly majorities on the disarmament article, and the TPNW campaign itself. The measurement series run on one shared time grid — all three metrics authored at all eight examined points (interval years 0-55, treaty entry into force to present) — so no metric row borrows another's endpoints. The five-year review cycle superimposes a mild sawtooth on the trend (expectations spike before each conference, deflate after), but the authored points sample the trend, which is monotonically worsening on all three metrics.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the structural data is what drives the divergence. From the weapon-state seat, the arrangement is legitimate security governance it happens to administer: it affirms the disarmament article, credits its own reductions, and reads non-weapon-state pressure as ingratitude for stability. From the non-aligned non-weapon-state seat, the same structure is a contract breached in one direction for three decades, with the breach party chairing the process that would adjudicate it. From the alliance-dependent seat, the arrangement is stability worth defending regardless of the reciprocity ledger, because the umbrella is existential. From the verifier's seat, the arrangement is a mandate that works — and that visibly stops at the safeguarded border. The engine computes these per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states sit near the beneficiary pole: they collect verified restraint, regime legitimacy, and a frozen competitor count, and their exit is arbitrage-grade (they manage interpretation rather than face enforcement), so effective extraction toward them is damped toward subsidy. Non-aligned non-weapon states sit near the target pole: they pay permanently, their exit is constrained (legal but repriced by sanction precedent), and their identity as responsible non-weapon states is partly constitutive of their standing. Alliance dependents are the genuinely mixed seat — they collect protection (beneficiary-side) while paying compliance costs and defending a status quo their security identity depends on (payer-side) — so a directionality override is authored for the powerful seat at d=0.38: the bare beneficiary declaration would derive a d too close to the subsidized end, ignoring the compliance burden and umbrella dependence that make these states active defenders of the asymmetry rather than passive collectors. The IAEA secretariat is administratively near-symmetric: it expends effort and collects mandate, touching neither pole. The excluded seats (TPNW parties, threshold states) sit largely outside the d computation — the former by building an alternative venue, the latter by never entering the bargain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a forecast mid-century cascade of dozens of new nuclear states — remains live: proliferation pressure persists, and the regime demonstrably caps it. There is no resolved mandatrophy to declare. The hybrid classification earns its keep by blocking two symmetrical errors. Reading the regime as pure coordination erases the question of who pays for the coordination and lets the asymmetry hide inside the collective good — exactly what the weapon-state seat's framing accomplishes rhetorically. Reading it as pure extraction erases the catastrophe the coordination actually prevents and would license the inference that the restraint leg is worthless because the disarmament leg is unpaid — an inference the non-aligned seat sometimes flirts with and the abolitionist reading presses further. The grand bargain reading is itself the instrument that keeps the extraction visible inside a functioning coordination structure: it insists both facts are true of the same arrangement, which is precisely the tangled-rope structure the classifier must be able to represent without collapsing it in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the grand bargain reading the correct instantiation of kernel npt_article_iv_vi_pairing, or do the sibling readings (nonproliferation_primary, abolitionist) capture the treaty''s binding structure?',
    'Comparative analysis of the 1967-68 Eighteen Nation Disarmament Committee negotiating record, subsequent state practice, and which reading better predicts behavior across review cycles; cross-reference the sibling stories'' classifications.',
    'If nonproliferation_primary is correct, Article VI is non-justiciable, no breach exists to weigh, and this story''s extraction asymmetry collapses toward a compliance-services structure. If abolitionist is correct, the standing arrangement is illegitimate ab initio and extraction is higher than authored. This story''s epsilon is valid only within the grand bargain reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame uncertainty: this constraint is one reading of a contested kernel; sibling readings instantiate different constraints with different epsilon.').

omega_variable(
    article_vi_justiciability,
    'Does Article VI constitute a legal obligation capable of breach, or an aspirational commitment whose non-fulfillment generates no legal consequence?',
    'Analysis of the ICJ 1996 advisory opinion (obligation to bring negotiations to a conclusion), Vienna Convention interpretive practice, and whether any tribunal or depositary has ever treated Article VI non-performance as breach.',
    'The entire extraction asymmetry this story measures depends on Article VI being breachable. If purely aspirational, weapon states owe nothing enforceable and the reciprocity claim loses its legal substrate; if binding, the standing arrangement contains a live, unresolved breach running in one direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, conceptual, 'Whether the disarmament leg of the bargain is law or rhetoric.').

omega_variable(
    withdrawal_license_validity,
    'Does demonstrated failure of Article VI progress actually license non-weapon-state remedies — withdrawal under Article X or unilateral Article IV expansion — as this reading''s enforcement mechanism assumes?',
    'Test against state practice: reception of the North Korean withdrawal, the Iranian 10+2 proposal debate, expert analyses of Article X conditions, and whether any non-weapon state has successfully invoked Article VI failure to justify expanded entitlements.',
    'If the license is legally recognized, weapon states face real breach exposure and the reading has enforcement teeth. If no state or body has ever accepted the license, the reading''s remedy structure is rhetorical and the arrangement operates closer to one-way compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_license_validity, empirical, 'Whether the reading''s enforcement mechanism (licensed exit/expansion) exists in law or only in assertion.').

omega_variable(
    nnws_net_position,
    'Do non-weapon states as a class derive enough net benefit from the nonproliferation regime that the reciprocity shortfall nets out, or does the majority bear uncompensated cost?',
    'Split the non-weapon-state cohort by revealed preference: Non-Aligned Movement voting records versus alliance-dependent positions, combined with security and technology-access valuations for each subgroup.',
    'If most non-weapon states are net beneficiaries, the victim set shrinks to a subset and the structure trends toward ordinary coordination; if the majority are net payers, the extraction asymmetry is broader than authored and the structure trends toward enforced one-way transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_net_position, empirical, 'Whether the payer class is a minority faction or the modal non-weapon-state position.').

omega_variable(
    suppression_target_asymmetry,
    'Does the regime''s enforcement machinery protect the bargain itself, or does it selectively protect the asymmetry — pressing verification on non-weapon states while weapon-state arsenal modernization proceeds unexamined?',
    'Compare enforcement response magnitude and speed to comparable-scale compliance events by weapon states versus non-weapon states (modernization programs versus enrichment disputes) across the interval.',
    'If enforcement is systematically asymmetric, the measured suppression functions as maintenance of the imbalance rather than of the bargain, strengthening the extraction component and pushing computed classifications toward the coercive end for payer seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_target_asymmetry, empirical, 'Whether suppression serves the coordination function or the asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0, 0.18).
narrative_ontology:measurement(npt__tr_t10, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 10, 0.21).
narrative_ontology:measurement(npt__tr_t20, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 20, 0.27).
narrative_ontology:measurement(npt__tr_t25, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 25, 0.34).
narrative_ontology:measurement(npt__tr_t30, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 30, 0.37).
narrative_ontology:measurement(npt__tr_t40, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 40, 0.43).
narrative_ontology:measurement(npt__tr_t47, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 47, 0.46).
narrative_ontology:measurement(npt__tr_t55, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 55, 0.48).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(npt__be_t10, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(npt__be_t20, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(npt__be_t25, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(npt__be_t30, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(npt__be_t40, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(npt__be_t47, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 47, 0.66).
narrative_ontology:measurement(npt__be_t55, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 55, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(npt__su_t10, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(npt__su_t20, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(npt__su_t25, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(npt__su_t30, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(npt__su_t40, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(npt__su_t47, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 47, 0.62).
narrative_ontology:measurement(npt__su_t55, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 55, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, resource_allocation).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of kernel npt_article_iv_vi_pairing per the epsilon-invariance principle: the colloquial label 'the NPT bargain' conflates three structurally distinct claims about the Article IV/VI relationship, each with its own stable epsilon. This story (grand_bargain) authors epsilon for the standing arrangement as a broken-but-real reciprocal exchange. The nonproliferation_primary sibling authors epsilon for the same arrangement read as a verification-services structure with a non-justiciable aspiration attached — lower extraction, different victim set. The abolitionist sibling authors epsilon for the same arrangement read as an illegitimate perpetuation of dual-use risk — higher extraction, categorical rather than conditional grievance. The upstream/downstream structure runs from this reading outward: its breach doctrine supplies the legitimacy critique that the abolitionist reading's prohibition campaign builds on (influences edge), while it competes live with the nonproliferation_primary reading across state coalitions (coexistence edge). All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__grand_bargain, powerful, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
