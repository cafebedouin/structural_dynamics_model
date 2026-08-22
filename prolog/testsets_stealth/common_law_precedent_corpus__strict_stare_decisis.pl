% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis — Precedent as Backward Constraint
 *   domain: legal/jurisprudential/constitutional
 *
 * SUMMARY:
 *   The standing arrangement under contest is the strict stare decisis
 *   doctrine itself: within the adjudicative hierarchy, a holding of a
 *   competent court binds subsequent courts, and departure requires
 *   extraordinary justification. Vertical binding is enforced by appellate
 *   reversal; horizontal binding at the apex is self-imposed and carries a
 *   rare overruling valve. This file instantiates the strict_stare_decisis
 *   reading of the common_law_precedent_corpus kernel (see kernel_context and
 *   the committer omegas); the sibling readings are separate constraint files
 *   and are not averaged here. The epsilon referent is the strict doctrine as
 *   it operates, assessed by the reading's own lights: the reading endorses
 *   the bindingness as the core of legality, and simultaneously acknowledges
 *   the costs the binding imposes — foreclosed claims, bound discretion,
 *   outdated holdings applied to changed facts. Those costs are authored
 *   descriptively, not discounted for being endorsed. Claim and metrics are
 *   independent: claimed_type records the structure I believe true of this
 *   reading's arrangement (genuine coordination with asymmetric foreclosure
 *   costs and active enforcement), while the metrics record its observed
 *   operation, including the drift series showing extraction deepening as the
 *   corpus accumulates. KEY AGENTS (by structural relationship): -
 *   apex_court_majority: agenda-setter (institutional/generational) —
 *   administers which holdings bind and the overruling threshold; partially
 *   bound horizontally - judiciary_as_institution: primary beneficiary
 *   (institutional/identity_locked) — collects legitimacy, decision-cost
 *   savings, and authority accumulation - repeat_player_litigants: primary
 *   beneficiary (powerful/arbitrage) — planning rents of settled rules -
 *   incumbent_right_holders: beneficiary (organized/mobile) — entitlements
 *   protected by the stock - novel_claim_litigants: primary target
 *   (moderate/constrained) — foreclosed arguments -
 *   changed_circumstance_parties: secondary target (moderate/constrained) —
 *   fit costs of outdated holdings - stare_decisis_bound_justices:
 *   identity-locked target (institutional) — adhere to holdings they believe
 *   wrongly decided - trial_court_judges and intermediate_appellate_courts:
 *   bound enforcer-payer seats (institutional/constrained) -
 *   legal_academy_reformers: excluded voice — proposes relaxation from
 *   outside the adjudicative room - comparative_jurisprudence_scholars:
 *   analytical observer — sees the full cross-system structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.48).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.55).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.48).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis — Precedent as Backward Constraint").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/jurisprudential/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, '28d58371-6f5f-4db0-af46-55a3be80ddd3').
narrative_ontology:cs_kernel_codification('28d58371-6f5f-4db0-af46-55a3be80ddd3', formalized).
narrative_ontology:cs_authority_grounding('28d58371-6f5f-4db0-af46-55a3be80ddd3', lineage).
narrative_ontology:cs_interpretation_layer_present('28d58371-6f5f-4db0-af46-55a3be80ddd3').
narrative_ontology:cs_reading_relation('28d58371-6f5f-4db0-af46-55a3be80ddd3', common_law_precedent_corpus__evolutionary_framework, forecloses).
narrative_ontology:cs_reading_relation('28d58371-6f5f-4db0-af46-55a3be80ddd3', common_law_precedent_corpus__pluralist_balancing, forecloses).
narrative_ontology:cs_axiom('28d58371-6f5f-4db0-af46-55a3be80ddd3', foundational, precedent_binds_absent_extraordinary_justification).
narrative_ontology:cs_axiom_status(precedent_binds_absent_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('28d58371-6f5f-4db0-af46-55a3be80ddd3', precedent_binds_absent_extraordinary_justification, conventional).
narrative_ontology:cs_axiom('28d58371-6f5f-4db0-af46-55a3be80ddd3', foundational, accumulated_law_over_contemporary_judgment).
narrative_ontology:cs_axiom_status(accumulated_law_over_contemporary_judgment, holdable).
narrative_ontology:cs_axiom_grounding('28d58371-6f5f-4db0-af46-55a3be80ddd3', accumulated_law_over_contemporary_judgment, deontological).
narrative_ontology:cs_reference_frame('28d58371-6f5f-4db0-af46-55a3be80ddd3', precedent_corpus_as_settled_law).
narrative_ontology:cs_drift_state('28d58371-6f5f-4db0-af46-55a3be80ddd3', contemporary_apex_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('28d58371-6f5f-4db0-af46-55a3be80ddd3', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, repeat_player_litigants).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, incumbent_right_holders).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, judiciary_as_institution).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, novel_claim_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, changed_circumstance_parties).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, stare_decisis_bound_justices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, intermediate_appellate_courts).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, trial_court_judges).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, rule_of_law_predictability).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, equal_justice_like_cases_alike).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, judicial_restraint_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The body of courts across the hierarchy, considered as a continuing institution. Every holding a court issues joins the stock of law that later courts apply; the institution collects legitimacy from consistency, saves decision costs whenever a settled rule answers a recurring question, and accumulates governing authority as its past decisions govern its future ones. Its self-conception is constituted by fidelity to accumulated law — a court that stopped treating prior holdings as governing would cease, in its own understanding, to be a common-law court. Leaving that position would mean dissolving the institution's identity rather than adopting an alternative policy.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, judiciary_as_institution, beneficiary,
    institutional, generational, identity_locked, national).

% Sets which prior holdings govern and how hard they bind: grants or denies review, writes the opinions that extend or narrow earlier holdings, and decides — rarely — when a line of cases falls. It is also partially bound by its own prior decisions, which it may revisit only through the same demanding justification standard it applies to every court below. Its members rotate; the seat's horizon is the institution's, not any individual's career.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, apex_court_majority, agenda_setter,
    institutional, generational, constrained, national).

% Sit between the apex court and the trial courts: they enforce settled holdings against trial courts below, reversing judges who depart, while remaining bound by the apex court's decisions above. They administer the arrangement daily and bear it at the same time; their maneuvering room consists in choosing how narrowly to read a holding they must apply.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, intermediate_appellate_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__strict_stare_decisis, intermediate_appellate_courts, payer).

% Decide cases under holdings they did not make and cannot revisit. A trial judge who reads a binding holding more narrowly than the appellate court above will accept is reversed; the practical alternative to applying the settled rule is error. They can register disagreement in opinions and certified questions, but the outcome of the case before them is governed by the accumulated stock.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, trial_court_judges, payer,
    institutional, biographical, constrained, local).

% Corporations, agencies, insurers, and institutional parties that litigate the same questions repeatedly. Settled rules are an asset: they structure contracts, compliance programs, and litigation strategy around known outcomes, and they can afford to shape the stock of holdings across decades of cases. If the rules reopened, their planning advantage would be the first thing to go — but they can also shift forums, draft around unsettled edges, and seek legislative codification, so their exposure to any single holding is hedged.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, repeat_player_litigants, beneficiary,
    powerful, generational, arbitrage, national).

% Parties whose entitlements — property interests, licenses, settled liability limits, doctrines running in their favor — rest on existing holdings. The stock of law protects what they already hold; each departure from precedent puts a specific entitlement back in play. They can transact, sell, or restructure around their entitlements freely, so their position inside the settled regime is comfortable rather than confined.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, incumbent_right_holders, beneficiary,
    organized, biographical, mobile, national).

% Parties whose claims conflict with an existing holding. Their arguments are rejected on the authority of the earlier decision regardless of the merits they can marshal today; their paths are to distinguish the holding if the facts permit, to lose and await a possible future overruling, or to take the question to the legislature. Within the case in front of them, the earlier holding decides the outcome.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, novel_claim_litigants, payer,
    moderate, immediate, constrained, national).

% Parties bound by holdings crafted for conditions that no longer obtain — technology, markets, and social understandings have moved while the governing decision has not. They carry the fit cost between old holdings and new facts, and their remedies (legislative correction or an eventual overruling) operate on timelines longer than their own case.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, changed_circumstance_parties, payer,
    moderate, biographical, constrained, national).

% Judges and justices who adhere to holdings they believe were wrongly decided because their own committed methodology — fidelity to accumulated law — requires it. Their professional identity is fused with the practice of following precedent: departing would resolve a case but break the methodological identity that makes their other decisions coherent. They write opinions honoring holdings they would not have joined, and the way out of the bind runs through abandoning the methodology itself, not through winning an argument.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, stare_decisis_bound_justices, payer,
    institutional, generational, identity_locked, national).

% Scholars and reform movements who argue the binding force of precedent should be relaxed, domain-weighted, or reoriented toward present norms. They publish, cite comparative practice, and advise litigants and legislatures, but they hold no seat in the adjudicative conversation their proposals target; the justification standard for departing from settled law is set by the courts that apply it.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_academy_reformers, excluded,
    moderate, generational, constrained, national).

% Researchers who compare how different legal systems manage the tension between settled law and legal change — civil-law jurisprudence constante, apex-court overruling practices across jurisdictions, historical swings in how strongly holdings bind. They take no side in the dispute and collect nothing from any resolution; their seat is analytic.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, comparative_jurisprudence_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__strict_stare_decisis, judiciary_as_institution).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__strict_stare_decisis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Answers recurring legal questions once so like cases are treated alike across courts and over time: litigants, lower courts, and planners can rely on settled holdings instead of relitigating every question, and the appellate hierarchy can concentrate its attention on genuinely open questions.
% TRANSFER_FUNCTION: Moves decisional authority backward — from present litigants and present judges to the majorities of past courts, whose holdings govern outcomes they never foresaw. Concretely, it delivers the benefits of settled rules (predictability, planning value, protected entitlements) to repeat players and incumbents, and delivers the costs of settled error (foreclosed arguments, outdated rules applied to changed facts) to the parties whose claims conflict with the stock.
% ABSENT_VOICES: The parties bound by holdings made without them: future litigants whose claims will be foreclosed by decisions issued today, and groups whose rights were adjudicated in eras when they lacked access to the courts that set the holdings. Also legislatures, whose corrective role the doctrine formally channels ('the correction is yours to make') while settled law simultaneously discourages legislative reopening of what courts have settled. Academic reformers speak from outside the adjudicative room entirely.
% DISAPPEARANCE_RATIONALE: If the binding force of precedent vanished overnight, every settled question would reopen at once: a wave of relitigation would hit the appellate courts, planning value built on settled rules would evaporate, and every entitlement resting on a holding would become provisional. The appellate hierarchy's workload and the legal system's basic operating assumption — that yesterday's decisions govern today's cases — would both have to be rebuilt from nothing.
% FOUNDING_PROBLEM: Pre-doctrinal adjudication was arbitrary and inconsistent: each judge decided anew, like cases came out differently across courts, judicial power appeared to be personal will, and no one could plan around the law. The doctrine was built to anchor adjudication in accumulated decisions — to make judicial authority an application of settled law rather than an exercise of preference.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: litigants who attack particular holdings still argue in precedential form (distinguishing rather than denying binding force), which attests the coordination problem is real even to those the stock harms; academic critics of the strict reading dispute the remedy, not the existence of the inconsistency problem; and civil-law systems independently converged on doctrine-like practices (jurisprudence constante), attesting the problem is structural rather than an artifact of one tradition. No corroborating source attests the founding problem is dead.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48 at interval end) and rising (0.38 to 0.48 across the interval): each new holding deepens the backward constraint and forecloses a wider class of future arguments — accumulation, not escalation, drives the trend. Suppression (0.55 scalar) is a raw structural property, unscaled by power or scope: vertical binding is reversal-enforced and near-absolute at the trial level, while the apex's horizontal self-binding has loosened as overruling normalized in contested domains — hence the suppression_requirement series falls gently (0.62 to 0.55) while theater_ratio rises (0.15 to 0.30), the signature of a doctrine increasingly maintained performatively at the top while its enforcement machinery below stays real. Theater 0.30: the 'extraordinary justification' ritual, citation of holdings narrowed in practice toward nullity, and ceremonial reaffirmations are real but remain a minority of the doctrine's operation. Accessibility_collapse 0.58: within adjudication, the alternative to following a binding holding collapses almost entirely for bound seats, but legislative correction, constitutional amendment, and distinguishing remain genuine (if slow or costly) channels, so collapse is partial. Resistance 0.55: sustained doctrinal scholarship, overruling campaigns by parties on the losing side of the stock, and periodic apex pushback keep the doctrine contested rather than naturalized. All three series run on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the apex seat the doctrine is self-constituting: it made the stock that binds it, and the binding is the source of its authority. From a novel-claim litigant's seat the same structure is a wall — the outcome is decided by people not in the room who cannot hear the merits. From the identity-locked justice's seat it is a duty that costs them their own best judgment. From the repeat player's seat it is an asset. The engine computes these per-seat types from the structural data; the divergence between the agenda-setter's coordination experience and the payer seats' foreclosure experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: repeat players (arbitrage-grade exit — they hedge, forum-shift, and draft around any single holding), incumbents (mobile within the settled regime), and the institution itself, which collects legitimacy and decision-cost savings from every act of compliance it does not pay for. Victims sit near the full-target end: foreclosed claimants and changed-circumstance parties (constrained exit — their channels run on longer timelines than their cases), with the identity-locked justice pushed furthest toward full target because identity_locked exit removes even the methodological exit route. The agenda-setter seats sit mid-low: they administer the arrangement and are subsidized by its authority accumulation while bearing part of the binding horizontally. No directionality overrides were used: the beneficiary/victim declarations plus exit options produce the right d for every declared seat, and the coarse power-atom keying of overrides would misstate the mixed institutional set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary, inconsistent adjudication anchored in personal judicial will — is live, so no mandatrophy is declared, and the mismatch consumer finds status=live against verdict=world_rearranges: no capture or zombie flag. The classification work this story does is boundary-keeping in both directions: the genuine, still-functioning coordination function (settled rules, like cases alike, decision-cost savings) blocks a pure-extraction reading, while the asymmetric foreclosure costs borne by claimants and bound judges block a pure-coordination reading. The tangled_rope claim holds both facts. The drift series guards the boundary over time: if theater_ratio continues rising while suppression_requirement falls, the arrangement slides toward inertial maintenance at the apex even as vertical enforcement below stays functional — the apex_overruling_drift omega tracks exactly that possibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading — strict_stare_decisis — of the common_law_precedent_corpus kernel; what would change structurally if a sibling reading (evolutionary_framework or pluralist_balancing) were the operative doctrine instead?',
    'Generate and compile the sibling reading stories and compare victim sets, beneficiary sets, and epsilon across the family. Under the evolutionary reading the foreclosed-claimant victim class contracts to parties bound by holdings whose subject matter has genuinely evolved, and epsilon falls; under the pluralist reading extraction becomes domain-variable rather than uniform.',
    'The classification of the precedent corpus as a whole depends on which reading governs; this file''s claim and its rising-extraction series describe only the strict reading''s arrangement, and cross-reading comparison is family-level analysis, never a property of this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    departure_threshold_location,
    'Where exactly do the three readings disagree? The candidate location is the justification threshold for departing from a holding — extraordinary justification in this reading, ordinary norm-evolutionary sufficiency in evolutionary_framework, case-by-case balancing in pluralist_balancing — not whether precedent has any force at all.',
    'Doctrinal analysis of what each reading treats as a sufficient ground for departure: changed circumstances alone, normative evolution alone, domain-weighted balancing, or none short of an extraordinary showing.',
    'Epsilon and the victim set are functions of the threshold: the higher the threshold, the larger the class of foreclosed claimants and the higher the extraction the stock imposes. The disagreement is located at this single structural element, which is why the kernel decomposes into exactly these readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(departure_threshold_location, conceptual, 'The kernel contest is located at the departure-justification threshold, not at precedent''s force per se.').

omega_variable(
    vertical_horizontal_decomposition,
    'Vertical stare decisis (lower courts bound, reversal-enforced, near-absolute) and horizontal stare decisis (the apex court self-bound, with a rare overruling valve) have structurally different enforcement and cost profiles — is this one constraint or two?',
    'Seat-level analysis of the compiled story: if per-seat computed types for trial-level seats and apex-level seats diverge materially, decompose into vertical and horizontal stare decisis stories linked by network.affects_constraints, per the epsilon-invariance rule.',
    'If decomposed, the vertical story carries higher extraction and near-total accessibility collapse for bound trial seats, while the horizontal story carries the overruling valve, the theater, and the drift series; the unified label would then denote a family rather than a single constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vertical_horizontal_decomposition, empirical, 'Whether vertical and horizontal stare decisis are one constraint or a two-story family.').

omega_variable(
    distinguishing_vs_overruling_share,
    'How much of the stock''s adaptation happens through formal overruling versus interpretive narrowing (distinguishing, limiting readings, narrowing language) that leaves the holding nominally intact?',
    'Empirical study of how precedent lines actually move: track holdings over time for formal overrulings versus effective nullification by narrowing; compare the nominal stock with the operative stock.',
    'If most adaptation runs through narrowing, the strict frame''s formal rigidity is partly theatrical — theater_ratio is understated and the formal rule''s accessibility_collapse overstates the practical bind; if overruling dominates, the formal rule tracks practice and the authored values stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distinguishing_vs_overruling_share, empirical, 'Share of precedent adaptation via formal overruling versus interpretive narrowing.').

omega_variable(
    coordination_cost_vs_overhead,
    'Is the extraction this reading itself acknowledges — foreclosed claims, bound discretion — the intrinsic price of the coordination the doctrine provides, or extractive overhead that a weaker binding rule would avoid without losing the coordination goods?',
    'Compare civil-law systems operating on jurisprudence constante (persuasive rather than strictly binding apex practice): if they achieve like-case consistency and planning reliability without the strict binding rule, part of the strict arrangement''s extraction is overhead rather than coordination cost.',
    'If the coordination goods survive weaker binding, the strict reading''s justification narrows to the residue only strict binding delivers, and the balance of this arrangement shifts toward the extractive side; if civil-law systems show the predicted inconsistency costs, the extraction is closer to intrinsic cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_vs_overhead, conceptual, 'Whether the doctrine''s acknowledged costs are intrinsic coordination price or extractive overhead.').

omega_variable(
    apex_overruling_drift,
    'Is the rising frequency of apex-court overruling in contested domains a permanent practice drift away from the strict reference frame, or a composition-driven oscillation that reverses with the court''s membership?',
    'Longitudinal analysis of overruling rates across multiple court compositions: a monotonic rise across compositions indicates permanent drift; alternating rises and falls that track membership indicate a cycle.',
    'If permanent, the strict frame''s drift moves from practice_drift toward repudiation_pressure and the theater_ratio series should be read as a transition signature; if cyclical, practice_drift stands and the magnitude over a full cycle may be minor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apex_overruling_drift, empirical, 'Whether apex overruling practice is permanently drifting from or oscillating around the strict reference frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 10, 0.18).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 20, 0.21).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 30, 0.24).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 40, 0.27).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept 'the binding force of precedent' decomposes, per the epsilon-invariance principle, into three readings of the common_law_precedent_corpus kernel, each with its own stable epsilon, victim set, and type. This file instantiates strict_stare_decisis: departure requires extraordinary justification; foreclosed-claimant and changed-circumstance victim classes; epsilon rising with corpus accumulation. The siblings — evolutionary_framework and pluralist_balancing — are separate files: under the evolutionary reading the foreclosed-claimant class contracts sharply and epsilon falls; under the pluralist reading extraction becomes domain-variable rather than uniformly accumulating. The strict reading is upstream in one respect: its formal rule is what both siblings relax, so each sibling's story links back here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
