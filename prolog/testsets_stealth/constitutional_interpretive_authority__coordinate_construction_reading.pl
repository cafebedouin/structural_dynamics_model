% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__coordinate_construction_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__coordinate_construction_reading
 *   human_readable: Interbranch Dialogue Construction of Constitutional Meaning (Coordinate Construction Reading)
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   Constitutional meaning in this arrangement is produced by continuing
 *   contest among the legislature, the courts, and the executive: no branch's
 *   reading is final, and disputes are settled through amendment campaigns,
 *   appointment battles, budgetary pressure, jurisdiction rules, and public
 *   justification rather than by a single adjudicator's closing word. The
 *   arrangement performs a real service — it keeps any one institution from
 *   permanently fixing the constitution's meaning to its own advantage —
 *   while imposing real costs: doctrinal finality-in-practice goes to
 *   whichever branch wins each political juncture, organized majorities win
 *   those junctures disproportionately, and people who need stable doctrine
 *   (rights claimants, ordinary litigants) carry the resulting contingency.
 *   This file is one reading of a contested kernel and generates only that
 *   reading as a clean, epsilon-invariant constraint; the sibling readings
 *   are separate files linked through the network block. Claim and metrics
 *   are independent authored facts: the claimed type is what I believe
 *   structurally true of this arrangement, and the metrics describe its
 *   observed operation, including its degradation over the interval as
 *   polarization converted dialogue into alternated unilateralism.
 *
 * KEY AGENTS:
 *   - legislative_majority_coalitions: primary beneficiary seat and dominant winner of the political resolution forums (powerful/constrained) — converts electoral strength into operative constitutional meaning
 *   - constitutional_judiciary: dual-positioned — collects interpretive influence while absorbing defiance and curbing threats (institutional/identity_locked)
 *   - incumbent_executives: appointment-leverage beneficiary with a short horizon (powerful/constrained)
 *   - minority_rights_claimants: principal cost-bearing seat — protections contingent on which reading prevails (powerless/trapped)
 *   - ordinary_litigants: bear relitigation and instability costs (moderate/constrained)
 *   - future_constituencies: excluded seat — inherits outcomes without votes in the resolution forums (powerless/trapped)
 *   - constitutional_scholars: analytical observers tracking the contest from outside it (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, 0.52).
domain_priors:suppression_score(constitutional_interpretive_authority__coordinate_construction_reading, 0.42).
domain_priors:theater_ratio(constitutional_interpretive_authority__coordinate_construction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__coordinate_construction_reading, "Interbranch Dialogue Construction of Constitutional Meaning (Coordinate Construction Reading)").
narrative_ontology:topic_domain(constitutional_interpretive_authority__coordinate_construction_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__coordinate_construction_reading, 'df2f23bb-0d7b-46a9-bd3e-bffc5c3824bc').
narrative_ontology:cs_kernel_codification('df2f23bb-0d7b-46a9-bd3e-bffc5c3824bc', fixed_text).
narrative_ontology:cs_authority_grounding('df2f23bb-0d7b-46a9-bd3e-bffc5c3824bc', distributed).
narrative_ontology:cs_reading_relation('df2f23bb-0d7b-46a9-bd3e-bffc5c3824bc', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('df2f23bb-0d7b-46a9-bd3e-bffc5c3824bc', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_axiom('df2f23bb-0d7b-46a9-bd3e-bffc5c3824bc', foundational, no_final_interpretive_authority_in_any_branch).
narrative_ontology:cs_axiom_status(no_final_interpretive_authority_in_any_branch, holdable).
narrative_ontology:cs_axiom_grounding('df2f23bb-0d7b-46a9-bd3e-bffc5c3824bc', no_final_interpretive_authority_in_any_branch, conventional).
narrative_ontology:cs_axiom('df2f23bb-0d7b-46a9-bd3e-bffc5c3824bc', secondary, political_channels_are_valid_resolution_forums).
narrative_ontology:cs_axiom_status(political_channels_are_valid_resolution_forums, holdable).
narrative_ontology:cs_axiom_grounding('df2f23bb-0d7b-46a9-bd3e-bffc5c3824bc', political_channels_are_valid_resolution_forums, instrumental).
narrative_ontology:cs_reference_frame('df2f23bb-0d7b-46a9-bd3e-bffc5c3824bc', interbranch_dialogue_settlement).
narrative_ontology:cs_drift_state('df2f23bb-0d7b-46a9-bd3e-bffc5c3824bc', contemporary_polarized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('df2f23bb-0d7b-46a9-bd3e-bffc5c3824bc', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, legislative_majority_coalitions).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, incumbent_executives).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, minority_rights_claimants).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, ordinary_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_judiciary).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__coordinate_construction_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__coordinate_construction_reading, departmentalist_interpretation_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pass statutes under their own reading of the constitution. When courts disagree, they answer with new legislation, amendment campaigns, confirmation fights, or budget pressure. Because no final arbiter exists, whatever reading survives the current contest becomes operative until the next one. Their horizon runs to the next election, and the political resolution forums — amendment, appointment, appropriation — are the terrain they know best.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, legislative_majority_coalitions, beneficiary,
    powerful, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, legislative_majority_coalitions, agenda_setter).

% Hears constitutional challenges and issues rulings binding on the parties, while knowing its rulings can be blunted by amendment, new appointments, jurisdiction changes, or non-compliance. It collects interpretive influence and institutional prestige; it absorbs defiance, curbing proposals, and the strain of deciding without final backing. Judges serve long terms and cannot abandon the adjudicative role without dissolving the institution's function.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_judiciary, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_judiciary, payer).

% Shape constitutional meaning through nomination power, enforcement discretion, and control of the administrative machinery that implements or declines to implement contested readings. Term limits keep their horizon short; they spend interpretive capital while in office and hand commitments to successors.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, incumbent_executives, beneficiary,
    powerful, immediate, constrained, national).

% Depend on constitutional text for protections that electoral majorities would withdraw. With no final guarantor, the durability of those protections depends on which branch's reading prevails at each juncture. They enter the process mainly as litigants after harm has occurred, and they cannot exit the constitutional order that governs them.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, minority_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Bring disputes that turn on constitutional meaning and bear the cost of doctrine that shifts between cases: relitigation, unpredictable standards, and settlement pressure. They hold no seat in the political forums where many contests are now resolved.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, ordinary_litigants, payer,
    moderate, immediate, constrained, national).

% Will inherit whatever reading survives today's contests but hold no vote in the amendments, appointments, and elections that settle them. Their interests reach the process only indirectly, through arguments made on their behalf by others.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, future_constituencies, excluded,
    powerless, generational, trapped, national).

% Track, criticize, and theorize the inter-branch contest from outside it. They publish accounts of which branch is prevailing, diagnose when dialogue has become ritual, and supply the vocabulary the branches use against one another. They hold no decision power.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__coordinate_construction_reading, legislative_majority_coalitions).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__coordinate_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes interpretive authority across the branches so that constitutional meaning is produced by structured contest — amendment, appointment, budgetary control, jurisdiction rules, and public justification — rather than closed by any single institution.
% TRANSFER_FUNCTION: Moves operative interpretive finality to whichever branch wins each political juncture, which systematically favors organized majorities with amendment and appointment leverage; moves doctrinal risk and relitigation cost onto rights claimants and ordinary litigants, who must litigate under readings that can shift.
% ABSENT_VOICES: Minority rights claimants appear only as litigants after injury, with no seat in the amendment, appointment, and budgetary forums where contests are increasingly settled. Future constituencies are absent entirely. Defenders of the sibling readings are heard in scholarship and argument but hold no institutional seat in this arrangement's resolution channels.
% DISAPPEARANCE_RATIONALE: If the dispersion settlement vanished overnight, one branch would consolidate final interpretive authority or an open succession struggle would erupt — the system would collapse into one of the sibling arrangements (judicial supremacy or parliamentary supremacy), and every statute, ruling, and rights protection currently contingent on inter-branch balance would be re-founded on a single adjudicator's or single chamber's word.
% FOUNDING_PROBLEM: Prevent any single institution from capturing constitutional meaning — the constitution-makers' problem of keeping interpretive power distributed so that no ruler, faction, or organ could fix the constitution's meaning to its own permanent advantage.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: ratification-era debates in which Federalist and Anti-Federalist factions pressed the anti-capture worry against each other; comparative constitutional scholarship documenting the same concern across unrelated systems; and the sibling readings themselves — the judicial-supremacy and parliamentary-supremacy traditions reject this arrangement's solution while affirming the anti-capture problem it answers.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__coordinate_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__coordinate_construction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the anti-capture coordination function keeps the arrangement far below pure-extraction territory, but the resolution channels transfer operative finality to each juncture's winner, and organized majorities — with amendment, appointment, and appropriation leverage — win disproportionately, so the transfer runs systematically away from the unorganized. Suppression 0.42: persistence rests more on mutual-checking incentives than on coercion, but the defiance-punishment machinery (jurisdiction-stripping proposals, confirmation blockades, budget retaliation, selective non-compliance) is real and has hardened over the interval. Theater 0.28: a substantial share of inter-branch 'dialogue' is genuine (opinions drafted in anticipation of legislative response, committee testimony that shapes drafting), but a growing share is positional rhetoric invoked for audiences rather than exchange. Accessibility collapse 0.30: the sibling arrangements remain live, well-understood alternatives held by real factions — this is a contested institutional construct, not a natural limit, so understanding it does not close off alternatives. Resistance 0.60: every supremacy bid meets immediate counter-mobilization from the other branches and from scholarship. All three measurement series share one time grid and rise together: as polarization deepened, dialogue thinned into performance, enforcement hardened, and the political-resolution tilt steepened. The suppression component is predominantly structural (amendment thresholds, forum control, standing rules) with a smaller internalized element — claimants learn that their protections are provisional — a split routed to the omegas rather than forced into the scalar.
 *
 * PERSPECTIVAL GAP:
 *   The cost-bearing seats compute this arrangement as contingency and exposure: their protections last exactly until the next juncture, and they hold no seat in the forums where junctures are decided. The beneficiary seats compute it as liberty and mutual insurance: each branch is protected against permanent domination by its rivals, and the majority coalition additionally finds the resolution forums are its home terrain. The judiciary computes both at once — it collects influence and absorbs defiance in the same breath — which is why its effective position sits nearer symmetric than its beneficiary declaration alone would suggest. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate between these experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the legislative and executive seats toward the subsidized end: they collect operative meaning and face no final check, with constrained (not arbitrage-grade) exit keeping them inside the arrangement. Victim declarations drive minority rights claimants toward the full-target end, amplified by trapped exit — they cannot leave the constitutional order whose contingencies they bear. Ordinary litigants sit high but below the trapped claimants. One override is declared: the derivation from the judiciary's beneficiary declaration would read roughly 0.15, but the judiciary simultaneously absorbs defiance, curbing proposals, and responsibility-without-finality, so its net structural position is nearer symmetric; the override sets the institutional power atom to 0.35. Gain flow is affirmatively named: the political resolution channels are the legislature's home terrain, so operative finality accrues there most often — receipt of the arrangement's gains, not mere incidental benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing capture of constitutional meaning by any single institution — remains live, and is corroborated from outside the benefiting parties (ratification-era factions, comparative scholarship, and the sibling traditions that reject this solution while affirming the problem). No mandate-outlived-function finding is available, and none is declared. The tangled-rope classification guards against two opposite mislabels: reading the arrangement as pure coordination would erase the systematic transfer of doctrinal risk to the unorganized that the political channels perform; reading it as pure extraction would erase the genuine anti-capture service that dispersion provides to every branch and to every group that has ever been protected by divided government. The identity-lock on the judiciary matters for persistence: the institution has become its adjudicative function, so it cannot exit the contest even as its winnings grow conditional — if that identity frame broke, the judiciary would either retreat to statutory adjudication (collapsing the triangle toward parliamentary supremacy) or dig in for finality (collapsing it toward judicial supremacy), and this constraint would cease to exist in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_delta,
    'This constraint is the coordinate_construction_reading of the kernel constitutional_interpretive_authority; how would the classification change under the sibling readings?',
    'Author the sibling stories (judicial_supremacy_reading, parliamentary_supremacy_reading) as separate constraints and compare computed types, victim sets, and epsilon across the family.',
    'Under judicial supremacy the cost-bearing set shifts toward nullified statutes and citizens subject to unelected final adjudication; under parliamentary supremacy it shifts toward minorities without judicial recourse. Each sibling carries its own epsilon; the values in this file are valid only for the coordinate reading and must not be averaged across the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_delta, conceptual, 'Reading-indexed status of this constraint within the interpretive-authority kernel.').

omega_variable(
    polarization_vs_arrangement_extraction,
    'How much of the measured extraction is intrinsic to dispersed interpretive authority, versus imported from polarized political mechanisms operating through it?',
    'Compare low-polarization and high-polarization windows within the interval, and compare coordinate-construction systems across countries with different party-system structures.',
    'If most extraction is polarization-driven, the arrangement''s intrinsic epsilon falls toward pure-coordination territory; if the tilt is intrinsic to political resolution channels, the tangled-rope structure stands and the victim declarations are load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polarization_vs_arrangement_extraction, empirical, 'Attribution of measured extraction between the arrangement itself and its political environment.').

omega_variable(
    instability_burden_incidence,
    'Is interpretive instability a symmetric coordination cost, or an extraction concentrated on repeat-player rights claimants?',
    'Longitudinal analysis of doctrinal reversals, relitigation frequency, and settlement pressure by litigant class.',
    'Concentrated incidence confirms minority_rights_claimants as structural cost-bearers and supports the asymmetric-extraction half of the classification; diffuse incidence would soften the reading toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instability_burden_incidence, empirical, 'Distribution of the instability burden across participant classes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__coordinate_construction_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(cons_tr_t50, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(cons_tr_t60, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(cons_be_t50, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(cons_be_t60, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 10, 0.31).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(cons_su_t30, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(cons_su_t50, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(cons_su_t60, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, parliamentary_supremacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the kernel 'constitutional_interpretive_authority'. The colloquial label 'how the constitution is interpreted' conflates three structurally distinct arrangements: this file (coordinate_construction_reading — finality denied to all branches, meaning built through dialogue and political contest), judicial_supremacy_reading (courts hold finality via rights guardianship and nullification), and parliamentary_supremacy_reading (legislature holds finality, no judicial voiding power). Each is a separate constraint with its own epsilon, victim set, and classification; they are linked here because each sibling's existence defines the alternatives this arrangement suppresses or tolerates, and because adoption of any one rewrites the other two's victim sets. This reading authors moderate epsilon for the dispersed arrangement itself; the siblings will author their own values for court-final and parliament-final arrangements respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__coordinate_construction_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
