% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Jurisdiction — Hybrid Complementarity Reading
 *   domain: international_law/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute regime (in force 2002) creates a permanent International
 *   Criminal Court with jurisdiction over genocide, war crimes, crimes
 *   against humanity, and aggression, gated by the complementarity principle:
 *   the Court may act only where national systems are unwilling or unable
 *   genuinely to prosecute. This story instantiates the
 *   hybrid_complementarity_reading of the contested rome_statute_jurisdiction
 *   kernel: residual universal authority is real, but operationally the
 *   constraint runs on state cooperation — arrests, evidence, funding, and
 *   access all flow through consenting states, and the docket's history (two
 *   decades concentrated on weak-state situations before late diversification
 *   to Georgia, Venezuela, Myanmar, and Ukraine) records the resulting
 *   asymmetry. The claim/metric gap is deliberate and load-bearing: the
 *   arrangement is CLAIMED as tangled_rope because it possesses both a
 *   genuine coordination function (filling impunity gaps no ad hoc tribunal
 *   economy could fill) and documented asymmetric extraction (sovereignty
 *   penetration of weak states, concentrated exposure of cooperating states'
 *   officials, de facto insulation of powerful non-parties); the authored
 *   metrics describe that mixed operation independently. Per the
 *   epsilon-invariance principle, the colloquial label 'Rome Statute
 *   jurisdiction' decomposes into three sibling stories — this hybrid
 *   reading, the universalist_reading, and the sovereigntist_reading — linked
 *   through network.affects_constraints; each carries its own reading-indexed
 *   epsilon over the same standing arrangement.
 *
 * KEY AGENTS:
 *   - icc_court_institution: Primary agenda-setter and institutional beneficiary (institutional/identity_locked) — administers the complementarity gate, collects budget and caseload-dependent authority, and cannot survive without the cooperation it lacks the power to compel
 *   - assembly_of_states_parties: Collective governor (institutional/mobile) — controls budget, judicial elections, and amendments; the seat where the constraint could be renegotiated
 *   - referring_states_parties: Selective beneficiary (organized/mobile) — deploy referrals and preliminary-examination requests as instruments against rivals while insulating allies
 *   - atrocity_victims_seeking_justice: Intended beneficiary (powerless/trapped) — receive the standing justice channel the arrangement exists to provide
 *   - deferral_trapped_atrocity_victims: Dual-positioned payer (powerless/trapped) — victims in situations deferred back to unwilling or feigning national systems, left without remedy
 *   - situation_state_governments: Primary target (moderate/constrained) — weak and middle-power states whose territory becomes the investigation site and whose sovereignty absorbs the penetration cost
 *   - indicted_officials_of_cooperating_states: Concentrated target (moderate/trapped) — officials of states that engaged the Court (e.g., the Kenya post-election cases) who bear personal exposure precisely because their state cooperated
 *   - nonparty_great_powers: Structurally excluded yet insulated (powerful/arbitrage) — absent from the consent framework while shaping it through bilateral pressure and enjoying the asymmetry the framework produces
 *   - un_security_council: External observer with referral and deferral leverage (institutional/analytical) — referred Darfur and Libya, holds the Article 16 deferral power, never itself subject to the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.58).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.48).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Jurisdiction — Hybrid Complementarity Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, '8b7d63ec-e361-42b4-9615-64c9c8beef1c').
narrative_ontology:cs_kernel_codification('8b7d63ec-e361-42b4-9615-64c9c8beef1c', fixed_text).
narrative_ontology:cs_authority_grounding('8b7d63ec-e361-42b4-9615-64c9c8beef1c', lineage).
narrative_ontology:cs_interpretation_layer_present('8b7d63ec-e361-42b4-9615-64c9c8beef1c').
narrative_ontology:cs_reading_relation('8b7d63ec-e361-42b4-9615-64c9c8beef1c', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b7d63ec-e361-42b4-9615-64c9c8beef1c', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_axiom('8b7d63ec-e361-42b4-9615-64c9c8beef1c', foundational, residual_universal_authority_is_real).
narrative_ontology:cs_axiom_status(residual_universal_authority_is_real, holdable).
narrative_ontology:cs_axiom_grounding('8b7d63ec-e361-42b4-9615-64c9c8beef1c', residual_universal_authority_is_real, deontological).
narrative_ontology:cs_axiom('8b7d63ec-e361-42b4-9615-64c9c8beef1c', foundational, consent_gates_enforcement_not_existence).
narrative_ontology:cs_axiom_status(consent_gates_enforcement_not_existence, holdable).
narrative_ontology:cs_axiom_grounding('8b7d63ec-e361-42b4-9615-64c9c8beef1c', consent_gates_enforcement_not_existence, conventional).
narrative_ontology:cs_reference_frame('8b7d63ec-e361-42b4-9615-64c9c8beef1c', residual_complementarity_authority).
narrative_ontology:cs_drift_state('8b7d63ec-e361-42b4-9615-64c9c8beef1c', post_ukraine_warrant_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8b7d63ec-e361-42b4-9615-64c9c8beef1c', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_court_institution).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_seeking_justice).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, referring_states_parties).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, situation_state_governments).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, indicted_officials_of_cooperating_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, deferral_trapped_atrocity_victims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, deferral_trapped_atrocity_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the complementarity gate: the Office of the Prosecutor selects situations and cases, chambers rule on admissibility, and the registry issues arrest requests it cannot execute. Collects its budget from the Assembly of States Parties and its caseload-dependent authority from the situations states open to it. Has no police, no prison system, and no compulsory process; every arrest, every witness relocation, every evidence transfer arrives by state consent. Its institutional identity has fused with the complementarity bargain — restructuring away from it would mean ceasing to be the court it is — so it defends the arrangement that simultaneously sustains and constrains it.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_court_institution, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_court_institution, beneficiary).

% The collective governing body of all states parties: elects judges and prosecutors, adopts the budget, manages amendment procedures, and convenes cooperation-study processes when non-cooperation accumulates. Individual members can refer situations, decline cooperation, or withdraw (as Burundi and the Philippines did) with limited immediate consequence. Sets the terms under which the Court operates without itself bearing prosecutorial exposure.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, assembly_of_states_parties, agenda_setter,
    institutional, generational, mobile, global).

% States parties that deploy self-referrals and referral votes as instruments — Uganda's 2004 self-referral, Venezuela's 2018 referral by a coalition of American states — directing international prosecutorial attention at rivals or internal opponents while voting budgets and sheltering aligned states from scrutiny. Their exit is genuinely mobile: participation in each referral decision is discretionary, and the arrangement costs them little they cannot decline to pay.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, referring_states_parties, beneficiary,
    organized, generational, mobile, global).

% Survivors of situations the Court has opened — northern Uganda, eastern Congo, Darfur, Central African Republic — who receive the standing justice channel the arrangement exists to provide: participation rights, reparations through the Trust Fund, and the fact of a forum that would otherwise not exist. They cannot choose another forum when this one is the only one, and their access depends entirely on whether the Prosecutor opens their situation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_seeking_justice, beneficiary,
    powerless, biographical, trapped, global).

% Victims in situations where complementarity deference returned cases to national systems that were unwilling or performed sham willingness — proceedings closed on admissibility grounds that never produced accountability. They hold the nominal benefit of a rights-bearing channel and bear its operative cost: raised expectations, testimony given, and no trial. Exit looks like abandoning the claim entirely.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, deferral_trapped_atrocity_victims, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, deferral_trapped_atrocity_victims, beneficiary).

% Governments of weak and middle-power states whose territory becomes the investigation site — the Democratic Republic of Congo, Mali, Georgia, Bangladesh/Myanmar's borderlands. They absorb the sovereignty cost: investigators on their soil, security-force scrutiny, diplomatic pressure to surrender nationals. Resisting brings reputational damage and possible Security Council attention; complying transfers their officials' exposure to The Hague. Withdrawal remains available but carries isolation costs most cannot afford.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, situation_state_governments, payer,
    moderate, biographical, constrained, regional).

% Sitting officials and former commanders of states that engaged the Court — the Kenyan post-election-violence cases being the emblem: a state that cooperated with the referral saw its deputy president and cabinet figures indicted, with cases later collapsing for witness-tampering and evidentiary failure. They bear concentrated personal exposure precisely because their state played by the arrangement's rules, and their travel and liberty are bounded by outstanding warrants.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, indicted_officials_of_cooperating_states, payer,
    moderate, biographical, trapped, national).

% Major military powers that never ratified the Statute (the United States, China, Russia, India among them) and therefore stand outside the consent framework while shaping it from outside: bilateral Article 98 agreement campaigns, Security Council leverage over referrals and deferrals, sanctions on Court personnel when investigations touch their interests or allies. They enjoy the arrangement's discipline of others at zero jurisdictional cost to themselves, and their exclusion is maintained by their own choice rather than anyone's enforcement.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, nonparty_great_powers, excluded,
    powerful, generational, arbitrage, global).

% Holds the arrangement's external levers: referred Darfur and Libya under Chapter VII, can defer any case for twelve-month renewable periods under Article 16, and its permanent members' vetoes determine which situations ever reach the Court from non-party territory. Takes positions, shapes the docket, and bears no jurisdictional exposure itself; its engagement is discretionary and strategic.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, un_security_council, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_court_institution).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__hybrid_complementarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fills the impunity gap for atrocity crimes with a standing institution instead of a new ad hoc tribunal per crisis: standardizes the definitions of genocide, war crimes, and crimes against humanity; provides a permanent investigative and trial capacity; and routes state cooperation into a single framework governed by the complementarity gate, so that national systems remain primary and the Court acts only where they fail.
% TRANSFER_FUNCTION: Moves adjudicatory authority upward from national systems to the Court whenever the unwillingness/unability gate opens; moves funding from states parties to the institution; moves enforcement burdens (arrests, detentions, witness protection) onto cooperating states; and concentrates accountability exposure on officials of weak and cooperative states while leaving powerful non-party states' conduct outside the frame entirely.
% ABSENT_VOICES: Non-party great powers are structurally absent from the consent framework yet decisively present in its operation — they would argue the strict-sovereigntist position and indeed do so from outside. Defense-side and accused-community perspectives enter the record late and thinly relative to prosecutorial and victim framing. Victims of situations never opened — including conduct attributable to powerful states — have no seat at all; the absence of their voice is what the docket's asymmetry records.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the impunity gap it fills would reopen immediately: no standing forum would exist for atrocity crimes in failed or complicit states, accountability would revert to the ad hoc tribunal economy (negotiated per crisis, hostage to Security Council politics) or to nothing, the accumulating body of jurisprudence and reparations practice would lose its institutional carrier, and states currently exposed would reacquire full sovereign cover. The norm architecture built on the Statute — universal ratification campaigns, complementarity-driven national reform, the Trust Fund — would reorganize around whatever partial successors states negotiated.
% FOUNDING_PROBLEM: The Nuremberg promise of ending impunity for atrocity crimes remained unfulfilled for half a century: Cold War paralysis blocked a permanent court, and the 1990s Rwanda and Yugoslavia genocides exposed ad hoc tribunals as slow, expensive, retroactive, and dependent on great-power whim. The Statute was built to solve that problem — a permanent, standing court for the worst crimes — while conceding enough to sovereignty (complementarity, consent-based jurisdiction) to secure ratification at all.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties on both sides: UN Secretary-General reporting and successive special procedures attest that mass atrocity impunity persists and that the Court addresses only a fraction of it; Amnesty International and Human Rights World-watch documentation attests both the original problem's reality and the mechanism's shortfalls; African Union critiques from an opposing seat attest the same asymmetry from the direction of perceived targeting; and the international criminal law academy (outside the Court's institutional interest) documents the conviction shortfall against the founding ambition. No seat inside the benefiting set is the sole attester.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58 because the arrangement's costs land asymmetrically: two decades of docket concentration on weak-state situations, personal exposure for officials of cooperative states, and a de facto immunity band for powerful non-parties, set against real deliveries (convictions in the Lubanga, Katanga, Ongwen, and Al Hassan lines, reparations, and a standing deterrent signal). Suppression is 0.48 — moderate — because the constraint's coercive apparatus is thin: treaty obligations under Articles 86-89 plus reputational pressure, with exits that demonstrably exist (Burundi 2017, Philippines 2019 withdrawals) and routine non-cooperation that rarely draws consequence. Theater_ratio 0.46 reflects a real functional core (investigations, trials, reparations) wrapped in heavy ceremonial and diplomatic maintenance whose share grew as the conviction shortfall against institutional rhetoric widened. Accessibility_collapse is low (0.35): alternatives remain live and legible — ad hoc tribunals, hybrid courts, domestic universal-jurisdiction proceedings, national amnesties — so understanding the constraint does not close the option space. Resistance is high (0.62): withdrawals, Al-Bashir's uninhibited travel, the 2020 US sanctions episode, African Union non-cooperation strategy, and recurring ASP budget fights. The temporal series run on one shared grid (2002, 2007, 2012, 2017, 2020, 2024) with every tracked metric authored at every point. The suppression_requirement series is authored deliberately: this story's dynamic IS enforcement-capacity change — the arrangement began with optimistic voluntary cooperation, then required progressively more institutional and rhetorical force to sustain the same operation as non-arrests accumulated, peaking around the 2020 sanctions episode before a marginal easing. Extractiveness plateaus and dips slightly by 2024 as docket diversification begins to distribute exposure.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the agenda-setter seat (Court, ASP), the arrangement is a fragile achievement under siege — every metric reads as the cost of defending a court against great-power abandonment. From the payer seats (situation governments, indicted officials, deferral-trapped victims), the same structure operates as selective sovereignty penetration: a court that reaches the powerless and negotiates with the powerful. From the excluded-but-insulated seat (non-party great powers), the arrangement is a norms regime that disciplines others at zero jurisdictional cost to themselves. The engine computes these per-seat classifications from the structural data; the authored tangled_rope claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The Court institution sits near the beneficiary end (collects budget, caseload, and authority) but is pushed partway toward the target end by its enforced dependence — it bears the arrangement's existential risk, hence the identity_locked exit atom. Atrocity victims with open channels sit near the beneficiary end; deferral-trapped victims sit near the middle-high range, paying the deference cost the mechanism generates. Situation-state governments and indicted officials of cooperating states sit near the full-target end: they bear concentrated, hard-to-exit costs. Referring states parties derive selective benefit with mobile exit (refer, decline, withdraw) — low d with high optionality. Non-party great powers, though role-excluded from the consent framework, are structurally subsidized by it (full immunity asymmetry, arbitrage-grade exit) — the derivation should read their exclusion plus arbitrage exit as near-beneficiary positioning. The Security Council observes with leverage: referral and deferral powers give it symmetric influence without jurisdictional exposure. Beneficiary and victim declarations in base_properties carry this structure to the engine; no directionality overrides were needed because the declared roles, power atoms, and exit options already differentiate the seats the derivation would otherwise conflate.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two opposite mislabels. Calling this a pure rope ignores the documented extraction: the complementarity gate has operated as a filter that concentrates exposure on the weak while the powerful self-exclude, and the cooperation burden falls on states that receive no commensurate control. Calling it a snare ignores the genuine coordination function: a standing court solving the ad hoc tribunal problem (slow, expensive, crisis-by-crisis) and delivering convictions and reparations that would otherwise not exist. The mandatrophy question — has the founding problem outlived the arrangement? — resolves as contested, not dead: impunity for atrocity crimes persists (Syria, Myanmar pre-referral, great-power conduct), so the founding problem remains partially live and the mismatch consumer finds no dead-problem-plus-world-rearranges signature. The piton risk is real and tracked: if the theater_ratio series crosses 0.5 while cooperation keeps decaying, the arrangement is drifting toward theatrical maintenance of an authority that can no longer execute — the enforcement_capacity_trajectory omega watches exactly that threshold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_omega,
    'This constraint instantiates the hybrid_complementarity_reading of the rome_statute_jurisdiction kernel. Would instantiating the sovereigntist_reading (strict consent gating, no residual authority) or the universalist_reading (mandate transcending consent) restructure the beneficiary/victim sets and epsilon?',
    'Comparative classification of the sibling stories: if the sovereigntist reading computes materially lower extraction over the same seats, the residual-authority premise is doing the structural work in this reading; if the universalist reading computes snare-grade extraction, the consent gate is the load-bearing restraint this reading depends on.',
    'Adopting the sovereigntist delta removes the residual-authority beneficiary claim and lowers epsilon; adopting the universalist delta adds non-consenting states to the victim set and raises epsilon toward the snare range.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_omega, conceptual, 'Committer-frame uncertainty: which reading of the Rome Statute jurisdiction kernel correctly structures the constraint.').

omega_variable(
    complementarity_gate_integrity,
    'Is the unwillingness/unability gate applied as a neutral admissibility test, or does it operate as a selection mechanism that concentrates prosecutorial exposure on weak states?',
    'Comparative analysis of preliminary examinations opened versus closed, regressed against situational gravity versus target-state power and great-power alignment.',
    'If selection tracks power rather than gravity, the extraction asymmetry deepens and the constraint drifts snare-ward; if selection tracks gravity, the coordination function dominates and the tangled_rope reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_gate_integrity, empirical, 'Whether complementarity functions as law or as discretionary selection.').

omega_variable(
    enforcement_capacity_trajectory,
    'Will state cooperation with arrest and surrender requests recover (Assembly reform processes, political normalization) or continue decaying?',
    'Track surrender rates on outstanding warrants, ASP cooperation-study outputs, and new non-cooperation findings over successive budget cycles.',
    'Continued decay drives theater_ratio above 0.5 and pushes the constraint toward piton (theatrical maintenance of an inertial authority); recovery anchors the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_trajectory, empirical, 'Trajectory of the cooperation infrastructure this reading says enforcement depends on.').

omega_variable(
    great_power_immunity_stability,
    'Is the de facto immunity of powerful non-party states a stable structural feature of the arrangement, or contingent on the current geopolitical configuration?',
    'Observe whether Security Council referral practice and bilateral pressure instruments shift as great-power alignments change; test against any future ratification or Article 16 deferral episodes.',
    'If immunity is contingent, the extraction profile is volatile and seat classifications shift with geopolitics; if stable, the asymmetry is a durable structural property of this reading''s constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(great_power_immunity_stability, conceptual, 'Whether the powerful-state carve-out is structural or episodic.').

omega_variable(
    victim_net_position_ambiguity,
    'Do atrocity victims net-gain from the arrangement (a standing justice channel exists where none would) or net-lose (complementarity deferral returns cases to unwilling states and raised expectations go unmet)?',
    'Longitudinal outcome study of victims in deferred versus prosecuted situations: reparations received, proceedings actually held, and expectation trajectories.',
    'If net-loss dominates, the victim seats compute as payers and the coordination-function gate weakens; if net-gain holds, the beneficiary declaration stands and the tangled_rope reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_net_position_ambiguity, empirical, 'Net position of the intended beneficiary class under complementarity deference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 2002, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2002, 0.25).
narrative_ontology:measurement(rome_tr_t2007, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2007, 0.3).
narrative_ontology:measurement(rome_tr_t2012, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2012, 0.36).
narrative_ontology:measurement(rome_tr_t2017, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2017, 0.44).
narrative_ontology:measurement(rome_tr_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2020, 0.48).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2024, 0.46).

% Extraction over time
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2002, 0.36).
narrative_ontology:measurement(rome_be_t2007, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2007, 0.43).
narrative_ontology:measurement(rome_be_t2012, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2012, 0.5).
narrative_ontology:measurement(rome_be_t2017, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2017, 0.56).
narrative_ontology:measurement(rome_be_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2020, 0.59).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2002, 0.28).
narrative_ontology:measurement(rome_su_t2007, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2007, 0.33).
narrative_ontology:measurement(rome_su_t2012, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2012, 0.39).
narrative_ontology:measurement(rome_su_t2017, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2017, 0.45).
narrative_ontology:measurement(rome_su_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'Rome Statute jurisdiction' covers three structurally distinct claims that share one referent (the standing complementarity-gated arrangement) but carry different reading-indexed epsilons. The universalist_reading is the upstream aspirational claim — the natural-law-inflected assertion that atrocity crimes admit no sovereign immunity — and is routinely cited as the normative ground for this hybrid reading's residual-authority premise. The sovereigntist_reading is the downstream counter-pressure: the consent-first account invoked by non-party powers and withdrawal states to deny the residual authority this reading affirms. Each story links to its siblings via affects_constraints; contamination propagates along these edges (a purity loss in the universalist claim — e.g., a demonstrated selectivity scandal — degrades this reading's legitimacy conditions faster than the sovereigntist's, which predicts the failure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
