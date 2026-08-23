% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Common Article 3 Functional Protection Floor (Status-Independent Minimum Treatment)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This story instantiates the functional_protection_reading of the
 *   combatant_status_definition kernel: the claim that every person detained
 *   in connection with an armed conflict receives Common Article 3's minimum
 *   protections — humane treatment, medical care, individual judicial process
 *   before any sentence — regardless of combatant status, so that no status
 *   determination stands between a captive and humane treatment. The sibling
 *   readings (state_centric_reading, national_liberation_reading) answer the
 *   same kernel question differently and are separate constraint files; per
 *   the epsilon-invariance principle this file authors only this reading,
 *   with one stable epsilon over one referent: the standing
 *   status-independent-floor arrangement, assessed by the reading's own
 *   protective lights. KEY AGENTS (by structural relationship): -
 *   detained_persons_all_categories: Primary beneficiary (powerless/trapped)
 *   — receives the protection floor unconditionally -
 *   state_intelligence_services: Primary cost-bearing seat
 *   (institutional/constrained) — loses interrogation and
 *   incommunicado-holding options - detaining_states: Agenda-setter and
 *   contingent beneficiary (institutional/constrained) — administers
 *   compliance, draws reciprocity - nonstate_armed_groups: Dual-positioned
 *   (organized/constrained) — their detainees are covered; they bear
 *   compliance duties they did not help draft - icrc_protection_mandate:
 *   Co-administrator and institutional collector (institutional/constrained)
 *   — the floor is its access license - war_crimes_prosecutors: Analytical
 *   observer (institutional/analytical) — adjudicates violations of the floor
 *   - families_of_missing_detainees: Excluded seat (powerless/trapped) — the
 *   floor's registration machinery is their only thread to the disappeared.
 *   The claim (rope) and the metrics are authored independently: the metrics
 *   describe low extraction with real resistance and a visible enforcement
 *   history, and the engine computes per-seat classifications from the
 *   structural data without reference to the claim.
 *
 * KEY AGENTS:
 *   - - detained_persons_all_categories: Primary beneficiary (powerless/trapped) — every captive in an armed conflict, whatever their affiliation, receives humane treatment, judicial process, and ICRC contact without any status test
 *   - - state_intelligence_services: Primary cost-bearing seat (institutional/constrained) — the floor forecloses coercive interrogation, prolonged incommunicado holding, and summary disposal; they resist and route around visibility
 *   - - detaining_states: Agenda-setter with contingency-beneficiary position (institutional/constrained) — ratify, administer, and fund compliance; draw reciprocity when their own personnel are captured
 *   - - nonstate_armed_groups: Dual beneficiary/payer (organized/constrained) — their captured members gain unconditional coverage; they owe humane-detention and court obligations they often lack capacity to meet
 *   - - icrc_protection_mandate: Co-administrator and institutional collector (institutional/constrained) — conducts visits, registration, and family-news transmission; collects access and mandate from the floor
 *   - - war_crimes_prosecutors: Analytical observer (institutional/analytical) — treat floor violations as chargeable conduct; their rulings feed back into the reading's authority
 *   - - families_of_missing_detainees: Excluded seat (powerless/trapped) — absent from treaty diplomacy; depend entirely on the floor's registration and notification machinery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.2).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.35).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Functional Protection Floor (Status-Independent Minimum Treatment)").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, '465d4553-9bde-41bf-ad8c-81c6b18870c5').
narrative_ontology:cs_kernel_codification('465d4553-9bde-41bf-ad8c-81c6b18870c5', fixed_text).
narrative_ontology:cs_authority_grounding('465d4553-9bde-41bf-ad8c-81c6b18870c5', lineage).
narrative_ontology:cs_interpretation_layer_present('465d4553-9bde-41bf-ad8c-81c6b18870c5').
narrative_ontology:cs_reading_relation('465d4553-9bde-41bf-ad8c-81c6b18870c5', combatant_status_definition__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('465d4553-9bde-41bf-ad8c-81c6b18870c5', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('465d4553-9bde-41bf-ad8c-81c6b18870c5', foundational, protection_entitlement_independent_of_status).
narrative_ontology:cs_axiom_status(protection_entitlement_independent_of_status, holdable).
narrative_ontology:cs_axiom_grounding('465d4553-9bde-41bf-ad8c-81c6b18870c5', protection_entitlement_independent_of_status, deontological).
narrative_ontology:cs_axiom('465d4553-9bde-41bf-ad8c-81c6b18870c5', secondary, custody_triggers_regularly_constituted_court_process).
narrative_ontology:cs_axiom_status(custody_triggers_regularly_constituted_court_process, holdable).
narrative_ontology:cs_axiom_grounding('465d4553-9bde-41bf-ad8c-81c6b18870c5', custody_triggers_regularly_constituted_court_process, conventional).
narrative_ontology:cs_reference_frame('465d4553-9bde-41bf-ad8c-81c6b18870c5', elementary_considerations_of_humanity_baseline).
narrative_ontology:cs_drift_state('465d4553-9bde-41bf-ad8c-81c6b18870c5', contemporary_post_hamdan_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('465d4553-9bde-41bf-ad8c-81c6b18870c5', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, detained_persons_all_categories).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, nonstate_armed_groups).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, icrc_protection_mandate).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, state_intelligence_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, detaining_states).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, nonstate_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Any person deprived of liberty in connection with an armed conflict — irregular fighters, civilians, suspected spies, downed aircrew, medical personnel — is entitled to humane conditions, medical care, correspondence, and individual judicial process before any penal sentence, without first passing any test of membership in a recognized force. Nothing about their situation is voluntary: captivity is the entry condition, and the floor shapes what happens inside it.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detained_persons_all_categories, beneficiary,
    powerless, immediate, trapped, global).

% Conduct interrogation and intelligence exploitation of captured persons. The floor forecloses prolonged incommunicado holding, coercive techniques, and quiet disposal of low-value detainees, and it puts every facility they run on a register a neutral visitor may demand to see. When denied carve-outs by their political principals they build off-book channels — renditions, undisclosed sites — which is the closest thing to exit the legal regime leaves them, and even that route exposes them to prosecutorial scrutiny.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, state_intelligence_services, payer,
    institutional, immediate, constrained, global).

% Ratified the 1949 Conventions, administer detention under them, and collectively author the regime's evolution through diplomatic conferences and customary practice. Individually they fund court-martial machinery, detainee registration, and neutral access, and they draw the return benefit when their own personnel fall captive — thin in wars where they hold all the prisoners, substantial where the flow reverses. Treaty withdrawal is formally available and a handful of states have exercised it, but customary-law persistence and reputational cost mean exit purchases almost nothing.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detaining_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, detaining_states, beneficiary).

% Their captured members receive the floor's protections without any status determination their opponents control — coverage that status-gated regimes would withhold. In exchange they are expected to run humane detention and functioning courts they frequently lack the capacity for, under rules drafted in conferences they had no seat at. They cannot renegotiate the terms; their practical lever is the reciprocity of observing them.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, nonstate_armed_groups, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, nonstate_armed_groups, payer).

% Visits places of detention, registers detainees, transmits family news, and reports violations confidentially to the responsible authorities. The floor is what converts a request to enter a facility into a legal claim; without it the institution's detention work rests on ad hoc consent. It collects mandate, access, and institutional centrality, holds no coercive power, and depends on belligerent cooperation for every visit it conducts.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, icrc_protection_mandate, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, icrc_protection_mandate, beneficiary).

% International and hybrid tribunals adjudicate detainee-treatment cases and treat the floor's guarantees as chargeable conduct whenever breached, for any victim category. Their rulings — treating the minimum as binding irrespective of status — feed back into the reading's authority and raise the expected cost of violation for every other seat.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, war_crimes_prosecutors, observer,
    institutional, generational, analytical, global).

% Relatives of persons taken in conflict who have received no confirmation of custody, location, or condition. The floor's registration and notification machinery is the only standing mechanism between their relatives and disappearance, yet they hold no seat in treaty diplomacy or military planning; their stake reaches the system only when neutral access is actually granted.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, families_of_missing_detainees, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__functional_protection_reading, detained_persons_all_categories).
narrative_ontology:fixing_cost_class(combatant_status_definition__functional_protection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single minimum standard of detainee treatment binding on every party to every armed conflict, including parties with no recognized status: it solves the collective-action problem in which each side's mistreatment of captives invites reciprocal escalation, and it removes the incentive each side otherwise has to classify its captives out of legal coverage altogether.
% TRANSFER_FUNCTION: Moves restraint and procedural burden — registration, review, trial machinery, neutral access — from detaining authorities toward bodily security and judicial guarantees for every detained person; secondarily it confers monitoring access and institutional standing on the ICRC, funded by the compliance obligations it monitors.
% ABSENT_VOICES: Detainees themselves were absent from the 1949 diplomacy and every revision since — their interests enter only vicariously through ICRC delegations. Families of the disappeared likewise have no seat. Non-state armed groups had no voice in the original drafting and gained only indirect representation in the 1977 protocol process; their position would be double-edged: demand for the protection, objection to obligations imposed without their consent.
% DISAPPEARANCE_RATIONALE: If the floor vanished overnight, detainee treatment in every ongoing conflict would fall back to captor discretion and status litigation: each side would have standing incentives to classify captives out of all coverage, the documented pre-1949 pattern of reprisal spirals and disappearance would resume, the ICRC would lose its treaty hook for demanding access, and families of the captured would lose the only registration machinery that reaches across lines.
% FOUNDING_PROBLEM: The inter-war and Second World War record: detainees in civil wars and occupied territories — partisans, hostages, suspected irregulars — were tortured, summarily executed, or disappeared because no legal category covered them and no minimum bound their captors. The 1949 drafters built Common Article 3 to bind all parties to a conflict, recognized or not, precisely because status frameworks had failed every population that did not fit them.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from outside the benefiting parties: international tribunal jurisprudence (the Tadic chamber's minimum-yardstick holding and successors) treats the floor as binding law in non-international armed conflicts; UN commissions of inquiry and investigative bodies continue to document detainee abuse in contemporary conflicts, confirming the underlying problem has not receded. The ICRC also attests continued liveness but sits inside the beneficiary set, so the load-bearing corroboration rests on the tribunals, UN bodies, and the published record of wartime litigation.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).
:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.20) because the floor transfers restraint rather than rent: the compliance burden (registration, review, trial machinery, access) is the coordination's price, not a transfer any seat collects as profit. Suppression (0.35, structural scalar) is moderate-low: the constraint closes abusive alternatives for detaining authorities but leaves the sibling readings and off-book routing channels (rendition, undisclosed sites) partly open, so alternatives are narrowed, not collapsed — hence accessibility_collapse 0.40 rather than mountain-grade. Resistance is high (0.62) because the seat that pays most sharply — state intelligence services — has fought the floor continuously since 1949 (status-gate arguments, the post-9/11 'unlawful combatant' campaign, black-site programs), and that fight is documented in litigation and inquiry records. Theater_ratio (0.31) reflects real but incomplete performance: manuals and doctrine cite the floor widely while practice diverges in access-denied settings; it peaked around the post-9/11 defiance era and has partially receded. The suppression_requirement series is authored deliberately as a distinct construct from the structural suppression scalar: it traces enforcement-machinery intensity (tribunal activity, prosecutorial exposure, ICRC leverage), which cycled with conflict waves — rising through decolonization-era scrutiny and the 1990s tribunal boom, peaking in the Hamdan-era confrontation, easing with partial normalization. The oscillation is driven by external conflict cycles, not by intermittent reinforcement as an extraction device. A sliver of the suppression picture is internalized rather than structural: professional military identity carries the floor beyond enforcement reach, which the reciprocity omega tracks.
 *
 * PERSPECTIVAL GAP:
 *   The same text is four different lived constraints. From the detainee seat it is a shield with no cost attached. From the intelligence-services seat it is a leash that removed settled tools, experienced as pure imposition — the seat most likely to compute the arrangement as extraction. From the detaining-state seat it is an insurance policy resented during wars in which it holds all the captives and pays all the premiums, valued in wars where its own personnel fall captive. From the ICRC seat it is an operating license: the floor is the legal hook that turns a request to visit into a claim. The engine computes these divergent per-seat classifications from power, exit, and directionality data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees sit at the full-beneficiary end (d near 0): the constraint subsidizes them unconditionally and they are trapped, so effective extraction inverts toward subsidy — matching the expected structural delta of low epsilon for all detainees. State intelligence services sit near the full-target end (d near 1): declared cost-bearers, institutional power but constrained exit, bearing the sharpest concentrated costs. Detaining states sit mid-range: they pay compliance costs and draw reciprocity plus systemic stability, and as agenda-setters they could in principle denounce — but customary-law persistence makes denunciation nearly worthless, so their exit is nominal. Non-state armed groups are dual-positioned: strong beneficiary (coverage without status tests their opponents control) carrying payer duties. The ICRC collects mandate and access — a genuine institutional collection, though not rent extracted from other seats. No directionality_overrides are authored: the override mechanism is keyed by power atom, and this story contains three institutional seats requiring three different directionalities, so a power-atom-level override would corrupt the differentiation that the beneficiary/victim declarations and exit-option spread already encode correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against two mislabels. First, against snare: the intelligence-services seat bears real, concentrated costs, but nobody collects those costs as rent — the value released flows to detainees as protection, which is why the gain_flow seat is the detainee population and not any administering body. Reading the intelligence seat's resentment as evidence of extraction would invert a protective floor into predation. Second, against piton: a humanitarian floor is the canonical candidate for rhetorical ossification — cited in every manual, honored in breach — and the theater_ratio series is the tripwire; a sustained crossing above 0.5 would indicate the floor persisting as performance while practice diverges, the piton signature. The founding problem (atrocities against detainees in conflicts where no status framework reached) is live — corroborated from outside the beneficiary set by tribunal jurisprudence and UN inquiry findings — so no mandatrophy resolution is declared, and the constraint remains under active observation for lifecycle drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_gate_severability,
    'Are Common Article 3 minimum protections constitutively tied to combatant-status categories, or fully severable from status determination?',
    'Comparative state practice across detainee categories combined with the jurisprudential record (Tadic-line ''minimum yardstick'' holdings, Hamdan-line applicability holdings): if the floor is granted uniformly wherever status is disputed, severability is confirmed; systematic status-conditioned withholding would refute it.',
    'If protections prove status-contingent in binding practice, this reading collapses toward the state-centric sibling''s structure and effective extraction for non-privileged detainees rises sharply; if severable, the low-extraction profile authored here holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_gate_severability, conceptual, 'Whether the floor''s protections depend on combatant-status determination.').

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the kernel combatant_status_definition — the functional_protection_reading. How does the reading contest distribute structurally across the sibling readings?',
    'Structural comparison of the three instantiated readings: state_centric_reading (status as a gate to POW-tier protections; non-state detainees left outside privileged tiers) and national_liberation_reading (status extended to organized liberation movements under AP I Art. 1(4)) instantiate other files. The disagreement is located at a single node: whether the mapping from detainee to protection runs through a status determination or through the bare fact of custody.',
    'Adopting a sibling reading changes the victim set (categories of detainees left unprotected or newly privileged) and redistributes epsilon across seats; it does not change the floor''s existence. This file''s low-epsilon, universal-floor structure is specific to the functional reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story instantiates the functional_protection_reading of the combatant_status_definition kernel; siblings alter the status-to-protection mapping.').

omega_variable(
    incommunicado_visibility_gap,
    'Does the measured compliance picture reflect actual detention conditions, given that denial of ICRC access concentrates exactly where abuse risk is highest?',
    'Triangulate released-detainee testimony, forensic and documentary evidence of undisclosed detention sites, and retrospective access records; compare treatment indicators for accessible versus access-denied custodial populations.',
    'If access-denied detention is systematically worse, theater_ratio understates the rhetoric-practice gap and the floor''s persistence leans harder on enforcement machinery than the authored metrics suggest, shifting computed seat classifications toward enforced-hybrid territory at the margins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incommunicado_visibility_gap, empirical, 'Visibility gap between accessible and incommunicado detention populations.').

omega_variable(
    reciprocity_asymmetry_persistence,
    'Does the floor persist primarily through reciprocity expectations or through active enforcement, given that dominant detaining states capture many detainees but rarely have personnel captured in return?',
    'Track compliance trajectories of high-capture/low-captured states across enforcement-capacity fluctuations (tribunal funding cycles, prosecutorial attention, ICRC access negotiations); a floor that decays when enforcement slackens while reciprocity remains thin is enforcement-carried, not self-sustaining.',
    'If persistence is enforcement-carried, the rope reading holds only while enforcement capacity holds, and enforcement decay becomes the leading indicator of drift toward rhetorical-only operation; if reciprocity and professional-norm internalization carry it, the rope reading is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_asymmetry_persistence, empirical, 'Reciprocity versus enforcement as the load-bearing persistence mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_functional_floor_tr_t0, combatant_status_definition__functional_protection_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ca3_functional_floor_tr_t15, combatant_status_definition__functional_protection_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(ca3_functional_floor_tr_t28, combatant_status_definition__functional_protection_reading, theater_ratio, 28, 0.23).
narrative_ontology:measurement(ca3_functional_floor_tr_t46, combatant_status_definition__functional_protection_reading, theater_ratio, 46, 0.25).
narrative_ontology:measurement(ca3_functional_floor_tr_t57, combatant_status_definition__functional_protection_reading, theater_ratio, 57, 0.37).
narrative_ontology:measurement(ca3_functional_floor_tr_t66, combatant_status_definition__functional_protection_reading, theater_ratio, 66, 0.35).
narrative_ontology:measurement(ca3_functional_floor_tr_t75, combatant_status_definition__functional_protection_reading, theater_ratio, 75, 0.31).

% Extraction over time
narrative_ontology:measurement(ca3_functional_floor_be_t0, combatant_status_definition__functional_protection_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(ca3_functional_floor_be_t15, combatant_status_definition__functional_protection_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement(ca3_functional_floor_be_t28, combatant_status_definition__functional_protection_reading, base_extractiveness, 28, 0.2).
narrative_ontology:measurement(ca3_functional_floor_be_t46, combatant_status_definition__functional_protection_reading, base_extractiveness, 46, 0.18).
narrative_ontology:measurement(ca3_functional_floor_be_t57, combatant_status_definition__functional_protection_reading, base_extractiveness, 57, 0.21).
narrative_ontology:measurement(ca3_functional_floor_be_t66, combatant_status_definition__functional_protection_reading, base_extractiveness, 66, 0.22).
narrative_ontology:measurement(ca3_functional_floor_be_t75, combatant_status_definition__functional_protection_reading, base_extractiveness, 75, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(ca3_functional_floor_su_t0, combatant_status_definition__functional_protection_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(ca3_functional_floor_su_t15, combatant_status_definition__functional_protection_reading, suppression_requirement, 15, 0.31).
narrative_ontology:measurement(ca3_functional_floor_su_t28, combatant_status_definition__functional_protection_reading, suppression_requirement, 28, 0.38).
narrative_ontology:measurement(ca3_functional_floor_su_t46, combatant_status_definition__functional_protection_reading, suppression_requirement, 46, 0.47).
narrative_ontology:measurement(ca3_functional_floor_su_t57, combatant_status_definition__functional_protection_reading, suppression_requirement, 57, 0.58).
narrative_ontology:measurement(ca3_functional_floor_su_t66, combatant_status_definition__functional_protection_reading, suppression_requirement, 66, 0.53).
narrative_ontology:measurement(ca3_functional_floor_su_t75, combatant_status_definition__functional_protection_reading, suppression_requirement, 75, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'combatant status' conflates three structurally distinct claims about the status-to-protection mapping, decomposed per the epsilon-invariance principle into three linked stories sharing the combatant_status_definition kernel. This file (functional_protection_reading) authors the universal-floor claim with low epsilon for all detainees and no status gate. combatant_status_definition__state_centric_reading authors the status-as-gate claim, under which detainees failing Article 4 criteria sit outside privileged protections — structurally higher epsilon concentrated on non-state detainees, with state militaries as beneficiaries. combatant_status_definition__national_liberation_reading authors the AP I 1(4) extension claim, intermediate in structure: expanded privileged tier for liberation movements, contested by states that never accepted 1(4). The upstream text (Common Article 3 itself, highest empirical entrenchment — customary-law status affirmed across jurisdictions) feeds all three readings; this reading exerts structural influence on the state-centric sibling by lowering the payoff of status gating without foreclosing it. Epsilon values differ across the family because the referent arrangements differ: floor-for-all, gate-by-status, and expanded-tier respectively — measuring one with another's observable would be measuring a different constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
