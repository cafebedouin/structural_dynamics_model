% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__hyper_presidential_reading, []).

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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Hyper-Presidential Reading of the Fifth Republic Constitution
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Within the Fifth Republic's constitutional architecture as instantiated
 *   by its hyper-presidential reading, the President of the Republic governs
 *   as the directly mandated embodiment of national will, minimally
 *   constrained by the legislature. The president appoints and dismisses the
 *   prime minister without counter-signature, commands the armed forces and
 *   the nuclear deterrent, conducts foreign policy as a personal preserve,
 *   may dissolve the National Assembly, and, through the government's
 *   engagement of its responsibility on a bill, converts contested
 *   legislation into adopted law without assembly assent; in declared
 *   emergency, Article 16 suspends parliamentary checks altogether. The
 *   legislature's costs concentrate at moments of invocation: votes
 *   overridden, amendments erased, oversight suspended, dissolution held over
 *   dissenting chambers. The presidency as institution and the incumbent
 *   occupying it collect concentrated decision rights and accountability
 *   insulation; the governing majority is spared recorded votes on contested
 *   bills; the opposition, its electorate, and the government itself bear the
 *   arrangement's running costs. This file instantiates ONE reading of a
 *   contested constitutional kernel; the reading identity and its siblings
 *   are recorded in commentary.kernel_context and the committer omega, not
 *   averaged here. The arrangement described carries a single stable epsilon
 *   throughout.
 *
 * KEY AGENTS:
 *   - - incumbent_president: primary beneficiary and agenda-setter (institutional/arbitrage) — collects concentrated decision rights, accountability insulation, and exits no other seat holds
 *   - - prime_minister_government: enforcing intermediary and designated casualty (institutional/constrained) — formally engages responsibility on bills, absorbs censure risk and blame
 *   - - ruling_majority_deputies: secondary beneficiary with payer underside (organized/constrained) — spared recorded votes and granted access; surrender legislative autonomy
 *   - - opposition_deputies: primary target seat (organized/constrained) — votes overridable, amendments erasable, oversight suspendable
 *   - - opposition_electorate: diffuse target (powerless/trapped) — legislative representation nullified between elections
 *   - - constitutional_council: institutional adjudicator-observer (institutional/analytical) — historically deferential reviewer of presidential-zone acts
 *   - - citizen_initiative_referendum_advocates: excluded seat (powerless/trapped) — no channel into the constitutional conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.75).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.72).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Hyper-Presidential Reading of the Fifth Republic Constitution").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "political/constitutional").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, '1bf0294b-83d8-438f-96ee-541b9a2c9ab5').
narrative_ontology:cs_kernel_codification('1bf0294b-83d8-438f-96ee-541b9a2c9ab5', fixed_text).
narrative_ontology:cs_authority_grounding('1bf0294b-83d8-438f-96ee-541b9a2c9ab5', lineage).
narrative_ontology:cs_interpretation_layer_present('1bf0294b-83d8-438f-96ee-541b9a2c9ab5').
narrative_ontology:cs_reading_relation('1bf0294b-83d8-438f-96ee-541b9a2c9ab5', fifth_republic_constitution__parliamentary_constraint_reading, forecloses).
narrative_ontology:cs_reading_relation('1bf0294b-83d8-438f-96ee-541b9a2c9ab5', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('1bf0294b-83d8-438f-96ee-541b9a2c9ab5', foundational, direct_mandate_embodies_national_sovereignty).
narrative_ontology:cs_axiom_status(direct_mandate_embodies_national_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('1bf0294b-83d8-438f-96ee-541b9a2c9ab5', direct_mandate_embodies_national_sovereignty, conventional).
narrative_ontology:cs_axiom('1bf0294b-83d8-438f-96ee-541b9a2c9ab5', foundational, executive_unity_requires_decision_concentration).
narrative_ontology:cs_axiom_status(executive_unity_requires_decision_concentration, holdable).
narrative_ontology:cs_axiom_grounding('1bf0294b-83d8-438f-96ee-541b9a2c9ab5', executive_unity_requires_decision_concentration, instrumental).
narrative_ontology:cs_reference_frame('1bf0294b-83d8-438f-96ee-541b9a2c9ab5', gaullist_sovereign_arbiter_design).
narrative_ontology:cs_drift_state('1bf0294b-83d8-438f-96ee-541b9a2c9ab5', contemporary_fragmented_assembly_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1bf0294b-83d8-438f-96ee-541b9a2c9ab5', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, ruling_majority_deputies).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, opposition_deputies).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, opposition_electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, prime_minister_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, ruling_majority_deputies).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, gaullist_presidential_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, rationalized_parliamentarism_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, domaine_reserve_foreign_policy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected for five years by direct universal suffrage on a personal mandate. Appoints and dismisses the prime minister without counter-signature, presides over the council of ministers, commands the armed forces and the nuclear deterrent, conducts foreign policy as a personal preserve, may dissolve the National Assembly, and may invoke emergency powers that suspend parliamentary checks. Directs the government's legislative strategy; when a contested bill stalls, the government engages its responsibility and the text is adopted without assembly assent unless a near-unanimous censure intervenes. Exit looks like dissolving the chamber and appealing to the country over its head.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, arbitrage, national).

% Appointed by, and removable at the discretion of, the president. Carries the presidential program through the assembly and formally engages the government's responsibility on blocked bills, the procedural step that adopts the text without a vote. Bears the parliamentary hostility the presidential program generates: censure motions aim at the government, and its fall removes the prime minister, not the president. Refusing the president's direction ends the tenure; resignation is the only exit and it terminates the political line.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, prime_minister_government, payer,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, prime_minister_government, agenda_setter).

% Hold seats on the presidential majority's ticket and depend on its investiture for reselection. Are spared recording individual positions on contested bills when responsibility is engaged, a protection from constituent backlash. Receive committee assignments and access in exchange for discipline. Defecting to a censure motion ends their prospects with the majority, and the president may dissolve the chamber and send them before the electorate early.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, ruling_majority_deputies, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, ruling_majority_deputies, payer).

% Hold minority seats won under the two-round ballot. Their amendments are erased when responsibility is engaged; their oversight initiatives stall without committee leverage; under emergency powers their chamber's checks are suspended outright. Their one procedural recourse, a censure motion, requires an absolute majority of all chamber members including defections from the presidential bloc, and has succeeded only twice in the arrangement's history. Staying means accepting overridden votes; leaving means abandoning the seat their voters gave them.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, opposition_deputies, payer,
    organized, biographical, constrained, national).

% Citizens whose votes produced legislative representation for minority platforms. Between elections that representation can be rendered procedurally inert: bills pass without their deputies' assent, oversight is suspended in emergencies, and the next opportunity to weigh in arrives only at the synchronized electoral calendar. No procedural channel exists to register dissent between elections; protest and abstention are the remaining currencies.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, opposition_electorate, payer,
    powerless, biographical, trapped, national).

% Reviews statutes and government-zone acts for conformity with the constitutional text. Composed of former officeholders and jurists; its historical posture toward presidential-zone acts was marked deference, tightening only gradually after procedural reforms widened referral paths. Its findings can void provisions of a bill but have not reversed the adoption machinery itself; it adjudicates the boundary rather than setting it.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% Movements and petition organizers pressing for citizen-initiated referendum procedures that would give voters a standing channel between elections. Outside the constitutional conversation: no parliamentary group carries their proposal to a vote, and the agenda for institutional reform is controlled by the offices the arrangement empowers. They organize petitions and demonstrations from outside the chamber.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, citizen_initiative_referendum_advocates, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__hyper_presidential_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces decisive, unified government action: a single directly elected executive node sets policy direction, resolves inter-ministerial conflict, and can enact legislation without assembling ad hoc parliamentary coalitions for every measure — answering the cabinet-instability and legislative-gridlock pattern of the predecessor regime.
% TRANSFER_FUNCTION: Moves lawmaking authority and policy-direction rights from the elected assembly to the presidency: engagement of government responsibility on a bill converts contested texts into adopted law without assembly assent; emergency powers suspend parliamentary oversight outright; dissolution converts parliamentary defiance into executive-timed elections; and accountability for unpopular decisions is routed downward onto the prime minister while decision rights concentrate upward in the president.
% ABSENT_VOICES: Advocates of citizen-initiated referendum and proponents of strengthened parliamentary oversight hold no seat in the constitutional conversation when a unified presidential majority stands: the same majority that enables the arrangement controls the agenda for revisiting it. Their objection — that popular sovereignty reaches the state exclusively through one person — is voiced only outside the chamber.
% DISAPPEARANCE_RATIONALE: If the bypass machinery vanished overnight, French governance would reorganize around negotiated parliamentary coalitions: legislation would require assembled majorities bill-by-bill, governments would fall and form through assembly arithmetic rather than sole presidential appointment, foreign-policy and defense direction would migrate back under legislative budget and oversight control, and accountability would attach to whichever office took each decision.
% FOUNDING_PROBLEM: Regime instability in the predecessor republic: cabinets averaging months in office, assemblies unable to legislate through shifting coalitions, and a colonial war producing an insurrectionary crisis that threatened civil war. The 1958 design answered with a sovereign arbiter placed above the parties and a rationalized parliament; the 1962 direct election then recast the arbiter as the nation's directly mandated spokesman.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is corroborated from outside any benefiting party: constitutional histories, comparative-politics literature, and contemporaneous testimony spanning the political spectrum attest the predecessor regime's instability and the 1958 crisis. Corroboration that the problem remains live comes almost exclusively from the benefiting parties — the presidential office and successive presidential majors citing each new emergency — while scholarly sources and parliamentary minorities date the original problem as solved and dispute the successor-crisis justification. The contested status therefore rests on a documented evidence asymmetry, stated here openly.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the arrangement decouples lawmaking outcomes from assembly assent: dozens of engagements of government responsibility in the 2022-2024 window normalized a mechanism designed as exceptional, and each invocation transfers adoption authority wholesale to the executive. Suppression (0.72) is authored as a raw structural property — it is NOT scaled by power or scope in the engine's computation; only extractiveness is scaled by directionality and scope. The suppressive machinery is procedural closure: censure motions require an absolute majority of all chamber members including defections from the presidential bloc, elections arrive at quinquennial intervals synchronized to favor the presidency, and the adjudicating council's historical deference left few judicial exits. The suppression mix is overwhelmingly structural (procedural and calendar closure); a smaller internalized component runs through the majority's anticipatory obedience under the investiture system. Accessibility collapse (0.55) is moderate: exits exist and two successful censures (1962, 2024) prove the mechanism breakable, but each attempt prices in near-impossible opposition unity. Resistance (0.60) is real and recurring: censures, sustained protest waves, upper-chamber obstruction, and record abstention as passive refusal. Theater ratio (0.52) crosses the substitution threshold in the final period: assembly debate under a predetermined adoption outcome retains full procedural form while deciding nothing — performative maintenance of the legislative function itself. The three metric series run on ONE shared eight-point grid, every tracked metric authored at every examined time point; the mid-interval oscillation tracks divided-government episodes and is exogenous (electoral coincidence), not an intermittent-reinforcement mechanism, and the five-year-term reform suppressed that cycle structurally, converting oscillation into monotonic rise. Caveat: the base scalars describe the constraint at its peak unified-majority configuration.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute divergent types from identical structure. From the incumbent's chair the arrangement is the coordination device it anchors — decision rights it exercises, stability it credits itself with, and exits (dissolution, emergency powers) that no other seat holds; its computed extraction sits near the subsidy end. From the opposition benches the identical machinery operates as enforced closure: votes overridable, oversight suspendable, recourse priced at near-impossible unanimity. Majority deputies compute an intermediate seat: spared accountability votes (short-run gain) while surrendering legislative autonomy and carrying dissolution exposure (long-run cost). The prime minister occupies a genuinely split position — the enforcing hand and the designated casualty at once, absorbing censure risk while exercising delegated adoption authority. Trapped, powerless electorate seats amplify hardest: their legislative preferences are nullified between elections with no procedural exit. Note on exit atoms: the binding on majority deputies runs through material career dependence (party investiture) rather than identity fusion, which is why they are authored constrained rather than identity_locked; the engine derives the divergence among all these seats from the authored power, exit, and role data — nothing in the claimed type adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation chain. incumbent_president (declared beneficiary, arbitrage-grade exit) derives near the beneficiary pole — the arrangement subsidizes its holder with decision rights and accountability insulation. ruling_majority_deputies carry a dual declaration (beneficiary primary, payer secondary): spared recorded votes and patronage access push derived d low, while surrendered autonomy and dissolution exposure pull the true relationship upward toward roughly 0.35. prime_minister_government (payer primary, agenda_setter secondary) derives strongly target-side from its casualty role; the enforcement half of its position — it alone formally engages responsibility on a bill — moderates its true d toward the middle. Neither nuance received a directionality override, deliberately: the override surface keys on the power atom, and this story's institutional seats (president, prime minister, adjudicating council) share one atom across radically different structural relationships, so an atom-level override would misapply across seats; the dual-role declarations carry the correction instead. opposition_deputies (victim, constrained exit, organized) derive well into target territory; opposition_electorate (victim, powerless, trapped) sits nearest the full-target pole, since trapped exit plus powerlessness maximizes effective extraction per unit of base epsilon. constitutional_council holds an observer seat with analytical exit and feeds no directional flow. Spatial scope is national across operative seats; verification of whether assembly will binds is administrable at that scope, tempering the amplification the engine applies at larger scopes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — curing cabinet instability — was achieved within a generation of the 1958 design, and the arrangement has intensified rather than relaxed since, which is the mandatrophy signature: persisting past the problem it was built for. The classification discipline cuts both ways. Reading the arrangement as pure coordination would ignore the documented transfer of lawmaking authority and the assembly's hollowed function; reading it as pure seizure would ignore the genuine coordination the structure still delivers — no cabinet-crisis paralysis has occurred under it, and decisive-executive capacity remains a real, demanded good. The tangled_rope claim holds both halves: a real coordination function, an asymmetric transfer riding on that same structure, and enforcement required to sustain the asymmetry. On the genealogy interview, founding_problem_status is authored contested with the evidence asymmetry stated openly: corroboration of the original problem is broad and sits outside the benefiting set, while attestation of its persistence comes overwhelmingly from the benefiting parties. The status-times-verdict combination (contested x world_rearranges) reports no zombie flag; the theater ratio at 0.52, just past the substitution threshold, is the symptom worth watching rather than the test — the arrangement still rearranges the world, and its successor function remains live even as the founding mandate has been outlived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus,
    'This constraint is one reading of the fifth_republic_constitution kernel (instantiating hyper_presidential_reading). Where exactly is the structural disagreement located among the three readings?',
    'Compare the three readings on the authorization axis: the parliamentary_constraint sibling locates constraint in required legislative authorization; the cohabitation_equilibrium sibling locates it in negotiated intra-executive allocation; this reading locates it nowhere — direct mandate suffices. Empirical probe: which structural element each reading treats as load-bearing (responsibility-engagement and emergency-powers usage versus prime-ministerial appointment practice versus cohabitation episodes).',
    'Adopting the parliamentary_constraint sibling moves the legislature out of the victim set and drops epsilon sharply; adopting the cohabitation sibling splits agenda-setting between two executives and redistributes the beneficiary set. This file''s classification is valid only under this reading''s instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locus, conceptual, 'Committer-frame omega: one reading of a contested constitutional kernel; the disagreement is located on the executive-authorization axis.').

omega_variable(
    article_16_dormant_reserve,
    'Is the emergency-powers article''s suspension of parliamentary checks a dormant reserve capability whose threat operates only episodically, or an operative background force that continuously disciplines legislative behavior?',
    'Behavioral analysis of legislative compliance and government-proposed amendments under varying perceived emergency proximity; comparative study of emergency-powers invocation thresholds across semi-presidential systems.',
    'If dormant, the arrangement''s steady-state extractive load is overstated by episodic peaks; if operative, the continuous suppression contribution is understated and the drift toward harder enforcement accelerates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_16_dormant_reserve, empirical, 'Whether the emergency reserve functions episodically or as continuous background discipline.').

omega_variable(
    quinquennat_configuration_dependence,
    'Does the 2017-2024 intensification reflect a durable structural ratchet — five-year synchronized terms plus a fragmented assembly making presidential bypass the default operating mode — or a transient response to an unusual assembly configuration?',
    'Observe the next electoral cycle under alternative assembly compositions; compare responsibility-engagement frequency across governing configurations since the five-year-term reform.',
    'A durable ratchet supports continued drift toward harder extraction and a snare-ward trajectory; the transient reading implies reversion toward the tangled-rope baseline once coalition formation resumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quinquennat_configuration_dependence, empirical, 'Whether the recent peak is a structural ratchet or a configuration artifact.').

omega_variable(
    direct_mandate_legitimacy_basis,
    'Is the direct-mandate premise — that the president''s election constitutes the nation speaking directly — a genuine expression of popular sovereignty, or a constructed legitimation that converts electoral victory into unconstrained executive license?',
    'Turnout and blank-vote trajectories in presidential versus legislative contests; deliberative surveys on whether citizens understand their presidential vote as authorizing bypass of their legislative representation.',
    'If constructed, the foundational axiom''s conventional grounding is exposed as cover and the legitimacy accounting shifts toward the extraction reading; if genuine, part of the measured suppression reflects accepted democratic design rather than imposed closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(direct_mandate_legitimacy_basis, conceptual, 'Whether the direct-mandate axiom is lived democratic expression or post hoc legitimation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fr_hyperpres_tr_t0, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(fr_hyperpres_tr_t0, observed).
narrative_ontology:measurement(fr_hyperpres_tr_t10, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(fr_hyperpres_tr_t10, observed).
narrative_ontology:measurement(fr_hyperpres_tr_t20, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(fr_hyperpres_tr_t20, observed).
narrative_ontology:measurement(fr_hyperpres_tr_t30, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(fr_hyperpres_tr_t30, observed).
narrative_ontology:measurement(fr_hyperpres_tr_t40, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(fr_hyperpres_tr_t40, observed).
narrative_ontology:measurement(fr_hyperpres_tr_t50, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement_basis(fr_hyperpres_tr_t50, observed).
narrative_ontology:measurement(fr_hyperpres_tr_t60, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 60, 0.46).
narrative_ontology:measurement_basis(fr_hyperpres_tr_t60, observed).
narrative_ontology:measurement(fr_hyperpres_tr_t66, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 66, 0.52).
narrative_ontology:measurement_basis(fr_hyperpres_tr_t66, observed).

% Extraction over time
narrative_ontology:measurement(fr_hyperpres_be_t0, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(fr_hyperpres_be_t0, observed).
narrative_ontology:measurement(fr_hyperpres_be_t10, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(fr_hyperpres_be_t10, observed).
narrative_ontology:measurement(fr_hyperpres_be_t20, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(fr_hyperpres_be_t20, observed).
narrative_ontology:measurement(fr_hyperpres_be_t30, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(fr_hyperpres_be_t30, observed).
narrative_ontology:measurement(fr_hyperpres_be_t40, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement_basis(fr_hyperpres_be_t40, observed).
narrative_ontology:measurement(fr_hyperpres_be_t50, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 50, 0.64).
narrative_ontology:measurement_basis(fr_hyperpres_be_t50, observed).
narrative_ontology:measurement(fr_hyperpres_be_t60, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 60, 0.71).
narrative_ontology:measurement_basis(fr_hyperpres_be_t60, observed).
narrative_ontology:measurement(fr_hyperpres_be_t66, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 66, 0.75).
narrative_ontology:measurement_basis(fr_hyperpres_be_t66, observed).

% Suppression requirement over time
narrative_ontology:measurement(fr_hyperpres_su_t0, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(fr_hyperpres_su_t0, observed).
narrative_ontology:measurement(fr_hyperpres_su_t10, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(fr_hyperpres_su_t10, observed).
narrative_ontology:measurement(fr_hyperpres_su_t20, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(fr_hyperpres_su_t20, observed).
narrative_ontology:measurement(fr_hyperpres_su_t30, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(fr_hyperpres_su_t30, observed).
narrative_ontology:measurement(fr_hyperpres_su_t40, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(fr_hyperpres_su_t40, observed).
narrative_ontology:measurement(fr_hyperpres_su_t50, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement_basis(fr_hyperpres_su_t50, observed).
narrative_ontology:measurement(fr_hyperpres_su_t60, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement_basis(fr_hyperpres_su_t60, observed).
narrative_ontology:measurement(fr_hyperpres_su_t66, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 66, 0.72).
narrative_ontology:measurement_basis(fr_hyperpres_su_t66, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the Fifth Republic Constitution' conflates three structurally distinct arrangements that readings of the same fixed text instantiate. Each sibling carries its own epsilon, its own beneficiary/victim structure, and its own classification: the parliamentary_constraint sibling keeps the legislature out of the victim set and prices executive action in required authorization; the cohabitation_equilibrium sibling splits agenda-setting between two executives; this hyper_presidential sibling concentrates both and books the legislature as bearer of invocation-time costs. Edge logic from this reading: the parliamentary_constraint sibling is foreclosed within any single framework (authorization-required and mandate-suffices are direct negations on the same premise); the cohabitation_equilibrium sibling is influenced without being eliminated — synchronized five-year terms shrink cohabitation windows and normalized bypass erodes the negotiating leverage that equilibrium presupposes. Upstream/downstream: the 1958 founding design (higher empirical establishment) supplies the textual kernel all three readings transmit; this reading's practice reshapes the downstream viability of both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
