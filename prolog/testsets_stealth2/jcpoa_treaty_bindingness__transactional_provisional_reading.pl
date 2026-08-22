% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Bad-Faith Determination (Transactional-Provisional Reading)
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   jcpoa_treaty_bindingness: the transactional-provisional reading, under
 *   which the JCPOA is a provisional exchange voidable upon a party's
 *   unilateral determination of counterparty bad faith — not a treaty, not a
 *   graduated enforcement regime. The epsilon referent is the standing
 *   arrangement under contest — the JCPOA as it actually operated, 2015-2025
 *   — assessed by this reading's own lights: a bargain whose consideration
 *   flowed unevenly, whose relief was revocable at the relieving power's sole
 *   discretion, and whose exit cost nothing for the party best positioned to
 *   exercise it. The sibling readings (binding_multilateral,
 *   graduated_compliance) are separate constraints in separate files; the
 *   contest lives in the omegas, not inside this constraint's classification.
 *   Claim and metrics are independent: the reading claims a legitimate
 *   transactional form, while the authored metrics describe substantially
 *   extractive operation concentrated on the front-loaded performer — the
 *   engine measures that divergence.
 *
 * KEY AGENTS:
 *   - united_states_executive_branch: agenda-setter/beneficiary (institutional/arbitrage) — administered relief unilaterally, determined bad faith, exited at zero legal cost, collected restored leverage
 *   - front_loaded_concession_states: primary target (organized/trapped) — sank irreversible nuclear concessions against revocable consideration
 *   - deal_opposition_coalitions: beneficiary (powerful/mobile) — enacted opposition without treaty-breach consequence
 *   - european_e3_broker_governments: dual-positioned broker (institutional/constrained) — collected the deal's diplomatic yield, paid its commercial losses
 *   - snapback_exposed_european_firms: target (powerful/trapped) — wrote off Iran positions when relief was revoked
 *   - russia_and_china_signatories: beneficiary (powerful/constrained) — profited from the framework's residue while blocking its enforcement
 *   - iaea_verification_machinery: administrator (institutional/constrained) — ran the verification that made the exchange credible, then reported its decay
 *   - npt_precedent_watchers: excluded voice (organized/constrained) — inherit the precedent, hold no seat
 *   - arms_control_analysts: analytical observer (analytical/analytical) — sees the full reliance asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.72).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.65).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Bad-Faith Determination (Transactional-Provisional Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:has_sunset_clause(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '73a63ea0-b293-44e0-a238-3466a104a74d').
narrative_ontology:cs_kernel_codification('73a63ea0-b293-44e0-a238-3466a104a74d', formalized).
narrative_ontology:cs_authority_grounding('73a63ea0-b293-44e0-a238-3466a104a74d', practice).
narrative_ontology:cs_interpretation_layer_present('73a63ea0-b293-44e0-a238-3466a104a74d').
narrative_ontology:cs_reading_relation('73a63ea0-b293-44e0-a238-3466a104a74d', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('73a63ea0-b293-44e0-a238-3466a104a74d', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('73a63ea0-b293-44e0-a238-3466a104a74d', foundational, mutual_performance_condition_of_obligation).
narrative_ontology:cs_axiom_status(mutual_performance_condition_of_obligation, holdable).
narrative_ontology:cs_axiom_grounding('73a63ea0-b293-44e0-a238-3466a104a74d', mutual_performance_condition_of_obligation, instrumental).
narrative_ontology:cs_axiom('73a63ea0-b293-44e0-a238-3466a104a74d', foundational, unilateral_bad_faith_determination_voids_commitment).
narrative_ontology:cs_axiom_status(unilateral_bad_faith_determination_voids_commitment, holdable).
narrative_ontology:cs_axiom_grounding('73a63ea0-b293-44e0-a238-3466a104a74d', unilateral_bad_faith_determination_voids_commitment, conventional).
narrative_ontology:cs_reference_frame('73a63ea0-b293-44e0-a238-3466a104a74d', provisional_transactional_bargain).
narrative_ontology:cs_drift_state('73a63ea0-b293-44e0-a238-3466a104a74d', contemporary_post_snapback_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('73a63ea0-b293-44e0-a238-3466a104a74d', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, united_states_executive_branch).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, deal_opposition_coalitions).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, russia_and_china_signatories).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, front_loaded_concession_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, snapback_exposed_european_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, front_loaded_concession_states).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, european_e3_broker_governments).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_verification_machinery).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, european_e3_broker_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the sanctions-relief side through a waiver regime it controlled alone; determined counterparty bad faith drawing on grievances beyond the agreement's scope (missile program, regional conduct, sunset dates) and exited in 2018 with no legal penalty; reimposed secondary sanctions and collected restored coercive leverage plus domestic coalition credit. Exit looked like a memorandum and a press conference.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, united_states_executive_branch, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, united_states_executive_branch, beneficiary).

% Dismantled centrifuge halls, shipped the enriched-uranium stockpile abroad, capped enrichment at 3.67 percent, and accepted the most intrusive inspection regime ever applied to a non-nuclear-weapon state — concessions that take years to rebuild once reversed. Received real relief for roughly two years, then watched the consideration vanish at the counterparty's sole discretion; walking away carried blockade-grade costs, so it kept performing past the counterparty's breach and then rolled back gradually from a weakened position.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, front_loaded_concession_states, payer,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, front_loaded_concession_states, beneficiary).

% Congressional majorities, allied foreign governments, and advocacy networks that opposed the agreement from signature onward; the provisional character of the commitment meant their opposition could be enacted by a successor administration without treaty-breach consequence. They collected the policy outcome they sought while bearing no continuing obligation of their own.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, deal_opposition_coalitions, beneficiary,
    powerful, biographical, mobile, national).

% Brokered and championed the agreement, collecting diplomatic capital and an export opening for their firms; after the US exit they built a special-purpose vehicle that cleared almost nothing, tried to hold Iran inside, and ultimately triggered the snapback mechanism in 2025. Caught between alliance discipline and deal preservation, they absorbed commercial losses without controlling either the relief or the exit.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, european_e3_broker_governments, beneficiary,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, european_e3_broker_governments, payer).

% Energy, aerospace, and manufacturing companies that entered Iran after relief began, signing aircraft and petrochemical contracts; when secondary sanctions returned, access to the US financial system made continued presence existential, so they withdrew en masse and wrote off their positions. Their exposure came from relying on relief that the relieving power could revoke alone.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, snapback_exposed_european_firms, payer,
    powerful, immediate, trapped, global).

% Signed as participants, gained sanctions-relief trade and standing as deal defenders; after the US exit they remained formally committed, bought Iranian oil at discounts, and blocked snapback at the Security Council for years. Their position inside the framework's residue was profitable, and abandoning it would forfeit that standing.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, russia_and_china_signatories, beneficiary,
    powerful, generational, constrained, continental).

% Ran the daily verification that made the exchange credible — quarterly reporting, continuous monitoring, sealed equipment — gaining mandate scope, funding, and precedent-setting access. After the counterparty exit its monitoring degraded stepwise as protocols were suspended, leaving it reporting on a program outgrowing the framework it was built to verify.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_verification_machinery, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_verification_machinery, beneficiary).

% States inside the NPT system who never held a seat in the Joint Commission but inherit the precedent: that a negotiated, verified restraint-for-relief exchange can be voided unilaterally by the strongest party at no legal cost. They would object that the collapse reprices every future bargain offered to would-be proliferators; they watch from review conferences with no vote on this framework.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, npt_precedent_watchers, excluded,
    organized, generational, constrained, global).

% Track breakout timelines, inspection coverage, and compliance records across administrations; see the full reliance asymmetry — which concessions were sunk, which relief was revocable, which determinations cited in-scope versus out-of-scope grievances — and publish the accounting the participants dispute.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, arms_control_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__transactional_provisional_reading, united_states_executive_branch).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a verified-restraint-for-relief exchange under a security dilemma: intrusive inspection converts worst-case assumptions about Iran's program into monitored facts, calibrated sanctions relief gives the conceding side a payable return, and a Joint Commission channels disputes before they become breaches.
% TRANSFER_FUNCTION: Moves sanctions relief — asset unfreezes, oil revenue, market access — from the sanctioning powers to Iran, and moves verifiable nuclear restraint — centrifuge counts, stockpile levels, inspection access — from Iran to the verifying powers; after the counterparty exit it moved decision rights over the entire arrangement to whichever party first declared bad faith.
% ABSENT_VOICES: Non-signatory NPT states inheriting the precedent had no seat in the Joint Commission; Iranian constituencies bearing the sunk-concession risk were represented only by the government that traded their capability away; future parties to similar bargains — who will be priced by this collapse — were nowhere in the room.
% DISAPPEARANCE_RATIONALE: If the voidability structure vanished overnight — if exit required consensus and breach carried legal cost — the 2018 withdrawal would have been an actionable violation rather than a press conference, snapback would have required Council agreement that Russia and China controlled, the sunk concessions would have been secured by law instead of reciprocity, and every party's calculus about signing such bargains would reprice.
% FOUNDING_PROBLEM: Built to solve the war-or-proliferation dilemma: Iran's enrichment program was advancing toward weapons capability, military strikes offered delay without removal, and a formal treaty was unreachable because no US administration could ratify one — so a provisional, executively manageable exchange was constructed instead.
% FOUNDING_PROBLEM_CORROBORATION: IAEA Board of Governors reporting attests the underlying proliferation problem independently of any deal party, and Security Council Resolution 2231 records multilateral attestation of the bargain's purpose; the deal-opposing governments attest the problem is live while denying the arrangement ever addressed it. No attesting source sits fully outside the interested set — every body positioned to corroborate holds a stake in the region or the regime — and that absence is itself signal.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction ends high (0.72) because the arrangement's terminal state is stranded concession: the front-loaded performer's dismantlement is sunk while the consideration was revoked at sole discretion — the reading's own central observation about the bargain's risk allocation. Suppression (0.65) reflects violently asymmetric coercive machinery: secondary sanctions reached every bank and firm touching Iran, crushing third-party alternatives, while the exercising party faced no coercion at all; the scalar averages a bimodal structure the per-seat computation resolves. Theater ratio ends 0.58: verification was genuinely functional through 2018, then drifted toward performative diplomacy — talks about talks, snapback choreography, censure resolutions — as the framework's real function lapsed (Goodhart drift visible in the monotonic theater series). Accessibility collapse is moderate (0.45): alternatives never fully closed — renegotiation, bilateral tracks, and the military option stayed live throughout. Resistance 0.60: Iran's measured rollback, E3 workaround attempts, and years of Russian-Chinese Council obstruction were real resistance short of open defection. The three measurement series share one grid ({0,2,4,6,8,10}). Suppression_requirement is authored deliberately: the story specifically tracks enforcement-capacity change — waiver-driven dormancy (T2), maximum-pressure intensification (T4-T6), evasion-driven decay (T8), snapback reinstatement (T10) — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical text. From the US executive seat the arrangement is an option, not an obligation: relief grantable by waiver, exit exercisable by memorandum, no ratification lock-in — near-zero experienced constraint. From the front-loaded performer's seat the same text is a heavy constraint: capability dismantled under seal, stockpile shipped abroad, consideration revocable by the counterparty alone — near-maximum experienced constraint. The E3 seat sits between: bound by alliance discipline it did not set, paying losses on commerce it invited. The engine derives this divergence from role, power, and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (US executive, opposition coalitions, Russia-China) derive low d for those seats, amplified further by the US seat's arbitrage-grade exit, which sits nearest the beneficiary pole. Victim declarations (front-loaded performers, snapback-exposed firms) derive high d, pushed toward the full-target pole by trapped exit: the performer could not restore its sunk concessions, and the firms could not leave the US financial system. The E3 seat is genuinely dual-positioned (beneficiary role, payer secondary role), and the derivation reads both declarations rather than needing correction. No directionality_overrides are authored: role-plus-exit differentiation already separates every seat, and a power-atom-keyed override would smear across the several institutional seats that must stay distinct.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions of mislabeling. Read as pure coordination ('a voluntary exchange everyone was free to leave'), the reliance asymmetry disappears — the fact that one side's performance was sunk while the other's was discretionary is exactly what the hybrid structure records. Read as pure extraction ('coordination was always cover'), the genuine function vanishes — verified restraint-for-relief really did cap the program and really did pay the conceding side for two years. The mandatrophy finding: the mandate has outlived its function. The founding problem (war-or-proliferation) remains live, but this arrangement ceased performing its exchange in 2018-19, and what persists is residual machinery — snapback choreography, revival diplomacy — maintained at a rising theater ratio. Declaring the mandatrophy resolved routes the residual to inertia analysis rather than letting a defunct bargain masquerade as a live one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_bindingness_underdetermination,
    'This constraint is one reading of kernel jcpoa_treaty_bindingness; would instantiating the binding_multilateral_reading or graduated_compliance_reading instead change the structural classification?',
    'Comparative classification across the sibling stories: classify the same 2015-2025 arrangement under each reading''s beneficiary/victim/exit structure and diff the computed per-seat types.',
    'Under the binding-multilateral reading the 2018 exit is a breach carrying legal cost, suppression rises, and the victim set expands to treaty-confidence holders; under the graduated reading extraction tracks proportionality assessments rather than unilateral determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_bindingness_underdetermination, conceptual, 'Reading-choice dependence of the classification (kernel contest routed here per committer-frame rules).').

omega_variable(
    bad_faith_determination_objectivity,
    'Is ''bad faith'' determinable by deal-scoped objective criteria, or is it inherently self-certifying whatever the determiner cites?',
    'Code the 2018 determination''s cited grounds against the agreement''s scope: verification-body findings on in-scope items versus out-of-scope grievances (missiles, regional conduct, sunset dates); if the determination rests on out-of-scope grounds, self-certification is doing the work.',
    'If purely self-certifying, the voidability right collapses into costless exit and extraction concentrates entirely on the front-loaded performer; if objectively constrainable, this reading converges toward the graduated sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bad_faith_determination_objectivity, empirical, 'Whether the unilateral determination prerogative is bounded by criteria or purely self-referential.').

omega_variable(
    reliance_asymmetry_magnitude,
    'How much of the front-loaded performer''s concession was irreversibly sunk versus restorable after the counterparty exit?',
    'Technical reconstruction timelines: centrifuge-hall rebuilding, stockpile regeneration rates, and breakout-time trajectories published by verification bodies before and after the rollback.',
    'Sets the magnitude of the payer seat''s stranded loss — the difference between a recoverable delay and a destroyed bargaining asset materially changes effective extraction at the target seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliance_asymmetry_magnitude, empirical, 'Magnitude of the sunk-concession asymmetry between the exchanging parties.').

omega_variable(
    sunset_voidability_interaction,
    'Does the agreement''s built-in sunset schedule (the year-10/15 and year-25 provisions) make unilateral voidability redundant, complementary, or marginal?',
    'Counterfactual comparison: model the arrangement with sunsets only (scheduled transition) versus voidability only (discretionary exit) and test which better predicts the observed 2018-2025 trajectory.',
    'If sunsets dominate the structure, the arrangement is transition-shaped regardless of reading; if voidability dominates, the extraction dynamics recorded here govern — the two mechanisms classify differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_voidability_interaction, conceptual, 'Interaction between scheduled sunset provisions and discretionary voidability within the same instrument.').

omega_variable(
    sovereignty_function_vs_extraction_cover,
    'Is the reading''s persistence driven by the genuine sovereignty-preserving function (democratic reversibility of executively made commitments) or by the gains it licenses for the party holding exit power?',
    'Symmetry test across holders of the reading: do supporters uphold the same voidability prerogative when exercised by the weaker party? Asymmetric endorsement indicates the prerogative functions as cover for exit power.',
    'If the function is genuine, part of the measured extraction is the price of democratic reversibility and the coordination component strengthens; if cover, the reading''s advocacy is rent-seeking for exit power and the extraction component dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_function_vs_extraction_cover, preference, 'Whether the transactional reading''s warrant is a real coordination value or cover for exit-power gains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_tx_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jcpoa_tx_tr_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(jcpoa_tx_tr_t4, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(jcpoa_tx_tr_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 6, 0.46).
narrative_ontology:measurement(jcpoa_tx_tr_t8, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 8, 0.52).
narrative_ontology:measurement(jcpoa_tx_tr_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(jcpoa_tx_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jcpoa_tx_be_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(jcpoa_tx_be_t4, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 4, 0.66).
narrative_ontology:measurement(jcpoa_tx_be_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(jcpoa_tx_be_t8, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 8, 0.71).
narrative_ontology:measurement(jcpoa_tx_be_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 10, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_tx_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(jcpoa_tx_su_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2, 0.35).
narrative_ontology:measurement(jcpoa_tx_su_t4, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 4, 0.75).
narrative_ontology:measurement(jcpoa_tx_su_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(jcpoa_tx_su_t8, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(jcpoa_tx_su_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, npt_safeguards_obligations).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the JCPOA's bindingness' decomposes into three structurally distinct constraints — binding-multilateral, graduated-compliance, and transactional-provisional — each with its own epsilon, beneficiary/victim structure, and classification. This file is the transactional-provisional member. The upstream member (binding_multilateral, closest to the lawyers' default account) influences the downstream members because its premise supplies the vocabulary of violation and breach that the transactional reading's determinations repurpose. The npt_safeguards_obligations edge records coupling to the wider safeguards regime whose credibility this arrangement's collapse reprices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
