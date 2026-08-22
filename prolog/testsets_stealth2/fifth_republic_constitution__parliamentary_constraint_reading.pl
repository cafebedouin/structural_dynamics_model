% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Presidential Authorization Requirement (Parliamentary Constraint Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Fifth Republic constitutional
 *   kernel: the parliamentary constraint reading, under which the president
 *   is a coordinated executive whose policy implementation requires
 *   legislative authorization, with the government responsible before the
 *   Assembly. Under this reading the standing arrangement operates as a
 *   hybrid: a genuine coordination function (democratic warrant for policy,
 *   stabilized branch relations) runs through the same structure that
 *   transfers real authority from the executive to the chamber majority, and
 *   the transfer is held in place by active enforcement (confidence and
 *   censure procedures, countersignature rules, Council review).
 *   Constraint-family note: the colloquial label 'the Fifth Republic's
 *   executive-legislative balance' decomposes, per the epsilon-invariance
 *   principle, into three structurally distinct constraints — this reading,
 *   the hyper-presidential sibling, and the cohabitation-equilibrium sibling.
 *   Each instantiates its own epsilon over its own referent: this story
 *   authors epsilon approximately 0.44 for the authorization-required
 *   arrangement as this reading assesses it; the hyper-presidential sibling
 *   would author a different epsilon for a differently-instantiated
 *   constraint whose victim set includes the Assembly rather than the
 *   executive. The sibling files carry their own values; nothing here
 *   averages across them. KEY AGENTS (by structural relationship): -
 *   french_president: Primary target (powerful/constrained) — bears the
 *   authorization requirement; retains dissolution, referendum, and a
 *   reserved domain as counter-moves, not exits - prime_minister_government:
 *   Target with dual position (institutional/constrained) — pays with
 *   survival-dependent tenure, yet sets the chamber's agenda and wields the
 *   responsibility-commitment procedure - national_assembly_majority: Primary
 *   beneficiary (organized/mobile) — collects agenda control and the censure
 *   instrument; also helps administer the arrangement -
 *   parliamentary_opposition: Secondary beneficiary (organized/mobile) —
 *   collects deliberation rights and blocking leverage without running
 *   anything - french_citizens: Diffuse beneficiary with secondary
 *   cost-bearing (moderate/trapped) — receive accountability, absorb gridlock
 *   - constitutional_council: Administrator (institutional/constrained) —
 *   adjudicates where the boundary sits -
 *   comparative_constitutional_scholars: Analytical observer — sees the full
 *   structure across cycles
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.44).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.52).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Presidential Authorization Requirement (Parliamentary Constraint Reading)").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional/political").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, 'd1a98933-accf-41fc-8e27-374a0215e4bb').
narrative_ontology:cs_kernel_codification('d1a98933-accf-41fc-8e27-374a0215e4bb', fixed_text).
narrative_ontology:cs_authority_grounding('d1a98933-accf-41fc-8e27-374a0215e4bb', lineage).
narrative_ontology:cs_interpretation_layer_present('d1a98933-accf-41fc-8e27-374a0215e4bb').
narrative_ontology:cs_reading_relation('d1a98933-accf-41fc-8e27-374a0215e4bb', fifth_republic_constitution__hyper_presidential_reading, forecloses).
narrative_ontology:cs_reading_relation('d1a98933-accf-41fc-8e27-374a0215e4bb', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('d1a98933-accf-41fc-8e27-374a0215e4bb', foundational, executive_policy_requires_legislative_authorization).
narrative_ontology:cs_axiom_status(executive_policy_requires_legislative_authorization, holdable).
narrative_ontology:cs_axiom_grounding('d1a98933-accf-41fc-8e27-374a0215e4bb', executive_policy_requires_legislative_authorization, conventional).
narrative_ontology:cs_axiom('d1a98933-accf-41fc-8e27-374a0215e4bb', secondary, president_arbitrates_but_does_not_originate_policy).
narrative_ontology:cs_axiom_status(president_arbitrates_but_does_not_originate_policy, holdable).
narrative_ontology:cs_axiom_grounding('d1a98933-accf-41fc-8e27-374a0215e4bb', president_arbitrates_but_does_not_originate_policy, conventional).
narrative_ontology:cs_reference_frame('d1a98933-accf-41fc-8e27-374a0215e4bb', parliamentary_responsibility_framework).
narrative_ontology:cs_drift_state('d1a98933-accf-41fc-8e27-374a0215e4bb', contemporary_minority_government_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d1a98933-accf-41fc-8e27-374a0215e4bb', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, parliamentary_opposition).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, french_citizens).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, french_president).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, french_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected head of state serving a five-year renewable term. Under this reading, nearly all presidential acts require countersignature by the prime minister and responsible ministers, and domestic policy implementation requires statutes passed by the National Assembly. Retains a small reserved domain (diplomacy, defense, nuclear command), the power to dissolve the Assembly once per year, and the power to call referendums. Cannot opt out of the constitutional order; the exit available is political (dissolution, referendum, waiting out the assembly's term), not legal.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, french_president, payer,
    powerful, biographical, constrained, national).

% Heads the government that formally determines and conducts national policy, but survives only while the Assembly withholds censure. Sets the Assembly's legislative agenda priority and can force passage of a bill by committing the government's responsibility on it, at the price of facing a censure motion within twenty-four hours. Resignation is the only personal exit; the office itself remains bound whoever holds it.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_government, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_government, agenda_setter).

% Coalition of parties commanding an absolute seat share in the 577-member chamber. Confirms or topples governments through confidence and censure procedures, amends or blocks executive bills, shapes the budget, and thereby collects the agenda leverage the authorization requirement concentrates in the chamber. Individual factions can defect and recoalition at low cost, so the bloc's position inside the arrangement is fluid.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority, agenda_setter).

% Minority parties in the chamber. Gain guaranteed deliberation rights, amendment leverage, and the censure instrument — tools whose value depends entirely on the authorization requirement staying in force. Benefit from the arrangement without administering it, and can shift between opposition and majority across elections.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, parliamentary_opposition, beneficiary,
    organized, biographical, mobile, national).

% Elect both the president and the Assembly. Receive the arrangement's protection: governments must answer to an elected chamber, and no single office implements policy alone. Also absorb its costs when authorization fails — stalled budgets, delayed reforms, repeated censure crises — and cannot leave the polity to escape either side of that trade.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, french_citizens, beneficiary,
    moderate, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, french_citizens, payer).

% Nine-member body that referees whether statutes and procedural moves conform to the constitutional allocation of authority. Its rulings strike down non-conforming legislation and, cumulatively, define where the boundary between executive initiative and legislative authorization currently sits. Embedded in the order it polices; it cannot step outside its own jurisdiction.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, agenda_setter,
    institutional, generational, constrained, national).

% Academic community studying the arrangement against other semi-presidential systems. Holds no stake in which branch prevails, documents how the working balance shifts across electoral cycles and cohabitation episodes, and supplies the outside attestations used in the genealogy interview.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__parliamentary_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem of legitimating national policy: statutes carry deliberative warrant because they pass through an elected chamber, the executive and legislature divide labor along a known boundary, and no single office can move the country alone. Stabilizes mutual expectations between the branches and between the state and its creditors and allies.
% TRANSFER_FUNCTION: Moves policy-initiating authority from the executive into assembly-controlled channels: the president surrenders unilateral implementation capacity, the government surrenders survival-independent tenure, and the chamber majority receives agenda control, amendment leverage, and the confidence/censure instruments in exchange.
% ABSENT_VOICES: Constituencies attached to the rival readings of the same constitutional text are structurally outside this frame: adherents of the direct-sovereign conception of the presidency would object that the reading amputates the arbitral office the framers described, and they are present in public debate but not seated in this reading's unanimity. Advocates of expanded direct democracy (citizen-initiated referendum) would object that the arrangement routes all authorization through professional intermediaries. Non-franchise residents affected by national policy have no seat at all.
% DISAPPEARANCE_RATIONALE: If the authorization requirement vanished overnight, the executive would implement policy by ordinance and decree, the chamber would lose its leverage instruments within days, party competition would reorganize around access to the presidential palace rather than parliamentary arithmetic, and the Constitutional Council's docket would shift from policing the boundary to ratifying executive acts. The entire architecture of accountability would need rebuilding.
% FOUNDING_PROBLEM: The 1958 framers confronted the Fourth Republic's assembly-dominated paralysis: revolving-door governments averaging months in office, executive impotence before a fragmented chamber, and a state unable to act decisively in the Algerian crisis. The arrangement was built to reconcile governmental stability with parliamentary accountability — rationalized parliamentarism: a stronger executive, disciplined by confidence and censure rather than by constant investiture bargaining.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: historians of the 1958 transition document the regime-collapse problem the framers answered; Constitutional Council decisions recite the constituent debates when locating the boundary; and the comparative-politics literature on rationalized parliamentarism treats the stability-accountability tension as an ongoing, unresolved design problem. The assembly majority's own attestation is discounted as self-serving; the corroborating sources stand independent of it.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.44): the arrangement takes real, bounded authority from the executive — unilateral implementation capacity and survival-independent tenure — while leaving the president dissolution, referendum, and a reserved domain, and leaving the government agenda priority and the responsibility-commitment valve. Suppression (0.52) is a raw structural property, unscaled by power or scope: it reflects the procedural machinery (censure exposure, countersignature requirements, Council review) that blocks unilateral alternatives; it is not amplified by the constraint's national scope, which the engine handles separately. Theater is low (0.15): the enforcement events are substantively consequential — governments have fallen to censure, bills have died in committee, budgets have been forced through at censure risk — so the ratio of performative to functional activity is small. Accessibility collapse is low (0.28): the rival ways of operating the same text remain live and practiced, so alternatives do not collapse once this reading is understood. Resistance is moderate (0.52): executives predictably push against the requirement (heavy recourse to the responsibility-commitment procedure, expansive readings of the foreign-policy domain), and the chamber pushes back.
 *   
 *   Cyclical pattern: the measurement series run on one shared ten-point grid (1958–2024) and show a recurring oscillation rather than monotonic drift. Extraction from the executive peaks when the chamber is controlled by opponents of the president (1986, 1993, 1997 cohabitation episodes; the 2024 minority-assembly period) and troughs when presidential and assembly majorities align (early Gaullist years; post-2002 term harmonization; the 2017 absolute-majority window). Theater moves inversely: parliamentary activity is most performative precisely when the chamber cannot bite (the rubber-stamp years score highest), and most functional when every vote is pivotal. The cycle is driven by an external institutional factor — the electoral calendar and the alignment or misalignment of presidential and legislative mandates — not by intermittent reinforcement; the oscillation is a side effect of the term structure, not itself an extraction mechanism. The base_properties scalars reflect the most recent observed state (2024), at a rising phase of the cycle.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from the same structure. From the presidential seat, the arrangement reads as an amputation of a mandate personally conferred by the electorate — the office's institutional identity ('arbiter above the parties') is fused with a self-conception that makes subordination to chamber arithmetic feel like category error rather than ordinary cost, which is an identity-lock dynamic: if that self-conception broke, the seat's experienced severity would drop sharply. From the chamber-majority seat, the identical structure reads as the democratic guarantee itself. The citizen seat experiences both faces alternately — protection in normal times, paralysis in crisis. The observer seat sees what neither party seat foregrounds: the responsibility-commitment valve lets the government pass texts the chamber would not affirm, so the payer side's burden is lighter than pure chamber sovereignty would imply, and the beneficiary side's control is weaker than its instruments suggest.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map directly onto the structural relationships: the chamber majority and opposition collect agenda leverage and deliberation rights (low directionality, toward the beneficiary end), while the president and government surrender implementation authority and tenure security (high directionality, toward the target end). No directionality overrides were needed: the derivation chain produces the right relationships from the declarations plus exit data. Two residual nuances are carried in commentary and omegas rather than overrides. First, the citizen seat is a beneficiary with trapped exit — normally trapping pushes a target toward the full-target end, but here the trap reflects inability to leave the polity, not extraction borne; the seat's benefit (accountability) dominates, with the gridlock-cost residue documented in the cost-incidence omega. Second, the government's dual position (payer plus agenda-setter) tempers its directionality below the pure-target end; the secondary-role declaration and the valve analysis carry this without an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents both standard mislabels. Read as pure coordination (the civics-textbook account), the story would miss that a specific seat — the chamber majority — captures the authority the executive surrenders, and that the arrangement persists through active enforcement rather than universal endorsement. Read as pure extraction (the Gaullist complaint that parliamentarism cripples the nation's chosen leader), it would miss the genuine coordination function: policy warrant, branch stabilization, and the accountability that citizens collect. On the genealogy interview, the founding problem (reconciling stability with accountability) is live — every censure crisis re-litigates it — and the disappearance verdict is world_rearranges, so the status-by-verdict combination shows no dead-mandate mismatch and no zombie flag. The mandatrophy-resolved flag is accordingly left undeclared: the arrangement's mandate has not outlived its function, though the omega on the responsibility-commitment valve tracks the specific channel through which atrophy would begin if the chamber ever lost the will to censure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the kernel fifth_republic_constitution (reading: parliamentary_constraint_reading). Which structural element do the sibling readings relocate, and what would change here if a sibling were adopted instead?',
    'Comparative analysis of the three reading-files'' victim/beneficiary sets and epsilon values: the hyper-presidential sibling places the Assembly in the victim set and the president as beneficiary; the cohabitation sibling splits authority bilaterally. The disagreement resolves only by examining which reading the operative practice actually honors, case by case.',
    'Adopting the hyper-presidential sibling would invert this story''s directionality structure entirely (executive becomes beneficiary, chamber becomes target) and drive epsilon toward the high-extraction range; adopting the cohabitation sibling would replace the binary payer/beneficiary structure with a negotiated-bilateral one. This file''s classification is valid only within its own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame omega: which reading of the constitutional kernel is operative, and what each sibling would restructure.').

omega_variable(
    countersignature_binding_force,
    'Is the countersignature requirement a binding limit on presidential policy initiative, or has it decayed into a formal courtesy that presidents secure automatically?',
    'Count instances across the interval where countersignature was withheld, conditioned, or contested, and where the Council invalidated acts for procedural non-conformity; compare against the total volume of presidential acts.',
    'If the requirement is largely ceremonial, the extraction this story attributes to it drops substantially and the arrangement drifts toward theatrical maintenance of a nominal rule; if it binds, the authored metrics stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countersignature_binding_force, empirical, 'Whether the signature mechanism genuinely constrains or merely ritualizes authorization.').

omega_variable(
    responsibility_commitment_valve,
    'Does the government''s power to force passage by committing its responsibility hollow out the authorization requirement — passing major texts the chamber would not affirm — or does the credible censure threat preserve the chamber''s ultimate control?',
    'Track the ratio of responsibility-commitment passages to ordinarily voted passages, and whether censure motions against such passages succeed when the chamber is hostile; the 2024 censure that brought down a government over a forced budget provides a recent test case.',
    'If the valve systematically bypasses chamber assent, the beneficiary seat''s captured control is weaker than authored and the coordination function degrades toward executive self-authorization; if censure credibility holds, the tangled-rope structure is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(responsibility_commitment_valve, empirical, 'Whether the fast-track passage procedure preserves or guts parliamentary authorization.').

omega_variable(
    gridlock_cost_incidence,
    'When the authorization requirement binds hard (divided government, minority assemblies), who ultimately absorbs the cost — the executive alone, or the citizens whose preferred policies stall?',
    'Welfare and preference analysis across binding episodes: measure policy delay and abandonment against stated public preferences, distinguishing costs the executive internalizes from costs displaced onto the public.',
    'If citizens absorb a large share of binding costs, the citizen seat''s directionality rises from the beneficiary end toward symmetry, softening the story''s democratic-guarantee framing; if costs stay concentrated on the executive, the current structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gridlock_cost_incidence, preference, 'Who bears the arrangement''s failure costs, and whether citizens remain net beneficiaries.').

omega_variable(
    alignment_cycle_driver,
    'Is the observed oscillation in extraction and theater driven by the electoral calendar''s alignment mechanics (an artifact that term-harmonization amplified), or by a deeper secular shift in constitutional norms about the presidency?',
    'Compare cycle amplitude before and after the 2000 term-harmonization reform, and test whether cohabitation-era behavior reappears under minority assemblies despite aligned terms; divergence of the two explanations predicts different futures under identical calendars.',
    'If the cycle is calendrical, the arrangement''s severity is predictable and administrable; if norms are shifting secularly, the oscillation is a transitional overlay on a one-directional drift, and the long-run classification differs sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_cycle_driver, empirical, 'Whether the cyclical dynamics reflect institutional calendar artifacts or norm-level change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 1958, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1958, 0.45).
narrative_ontology:measurement(fift_tr_t1962, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1962, 0.48).
narrative_ontology:measurement(fift_tr_t1969, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1969, 0.42).
narrative_ontology:measurement(fift_tr_t1981, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1981, 0.32).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1986, 0.15).
narrative_ontology:measurement(fift_tr_t1993, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1993, 0.17).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1997, 0.16).
narrative_ontology:measurement(fift_tr_t2002, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(fift_tr_t2017, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1958, 0.3).
narrative_ontology:measurement(fift_be_t1962, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1962, 0.27).
narrative_ontology:measurement(fift_be_t1969, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1969, 0.31).
narrative_ontology:measurement(fift_be_t1981, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1981, 0.35).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement(fift_be_t1993, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1993, 0.47).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1997, 0.48).
narrative_ontology:measurement(fift_be_t2002, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2002, 0.33).
narrative_ontology:measurement(fift_be_t2017, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2017, 0.37).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2024, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1958, 0.4).
narrative_ontology:measurement(fift_su_t1962, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1962, 0.36).
narrative_ontology:measurement(fift_su_t1969, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1969, 0.38).
narrative_ontology:measurement(fift_su_t1981, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1981, 0.42).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1986, 0.55).
narrative_ontology:measurement(fift_su_t1993, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1993, 0.53).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1997, 0.54).
narrative_ontology:measurement(fift_su_t2002, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2002, 0.4).
narrative_ontology:measurement(fift_su_t2017, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2017, 0.43).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Fifth Republic executive-legislative balance' decomposes into three readings of one kernel, per the epsilon-invariance principle — measuring the arrangement by presidential practice yields a different epsilon than measuring it by the responsibility articles, so they are different constraints, not one constraint with a measurement parameter. This file (parliamentary_constraint_reading) links to both siblings. Upstream/downstream structure: the hyper-presidential sibling is routinely cited as evidence against this reading's bindingness (its practice history is the main datum purporting to show the authorization requirement is nominal), so this reading sits downstream of that sibling's empirical record; the cohabitation-equilibrium sibling's applicability domain is set by this reading's strength — when the authorization requirement binds, authority allocation becomes negotiated, which is that sibling's operating condition. Each file documents the decomposition and carries its own epsilon; none averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
