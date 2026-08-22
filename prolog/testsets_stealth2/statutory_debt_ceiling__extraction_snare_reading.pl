% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__extraction_snare_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling — Weaponized Boundary Reading (Minority-Faction Extraction under Default Threat)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   The statutory debt limit (31 U.S.C. 3101) caps the aggregate principal of
 *   federal debt outstanding. Under this reading, the limit operates as a
 *   weaponized boundary: it binds only when a legislative faction withholds
 *   the votes to lift it, converting Treasury's legal obligation to pay debts
 *   already incurred through prior appropriations into collateral for policy
 *   extraction. The modern record shows the mechanism exercised repeatedly —
 *   the 1995-96 confrontation, the 2011 standoff that produced the first
 *   sovereign downgrade and the Budget Control Act's spending caps, the 2013
 *   confrontation, the 2021 near-miss, the 2023 standoff that produced the
 *   Fiscal Responsibility Act's caps and rescissions alongside a Fitch
 *   downgrade, and the 2025 extension fight. Concessions flow to the
 *   withholding faction; default risk, elevated borrowing costs, and
 *   payment-triage exposure flow to bondholders, program recipients, federal
 *   workers and contractors, and taxpayers. The arrangement's persistence
 *   depends on actively maintained refusal — it has no enforcement apart from
 *   the faction's willingness to hold the threat — and its
 *   fiscal-responsibility justification is increasingly disconnected from its
 *   operation, since spending levels are set by appropriations law the
 *   ceiling does not gate. KEY AGENTS (by structural relationship): -
 *   congressional_minority_faction: Primary beneficiary and active enforcer
 *   (organized/arbitrage) — withholds votes to lift the limit, collects
 *   concessions, distributes default risk outward -
 *   house_majority_leadership: Governing-coalition payer
 *   (institutional/constrained) — must assemble the increase, absorbs
 *   concessions and blame - us_president_administration: Negotiating payer
 *   (institutional/constrained) — concedes under deadline, holds unused
 *   constitutional alternatives - us_treasury_department: Operational payer
 *   (institutional/trapped) — extraordinary measures, cash triage, no lawful
 *   exit - treasury_bondholders: Market payer (powerful/mobile, global scope)
 *   — prices and absorbs the risk premium - federal_program_recipients:
 *   Contingent-payment victims (powerless/trapped) — seniors, veterans,
 *   disability and nutrition beneficiaries -
 *   federal_employees_and_contractors: Payment-exposure victims
 *   (moderate/constrained) - us_taxpayers: Diffuse cost bearer
 *   (moderate/trapped) - future_generations_of_taxpayers: Excluded voice
 *   (powerless/trapped, civilizational horizon) - austerity_policy_advocates:
 *   Secondary beneficiary (organized/mobile) — collects
 *   regular-order-impossible policy wins - credit_rating_agencies: Analytical
 *   observer (institutional/analytical, global scope) — converted standoffs
 *   into the 2011 and 2023 downgrades
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.82).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.75).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling — Weaponized Boundary Reading (Minority-Faction Extraction under Default Threat)").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '52b8b2e6-cd7c-48f9-9aea-e2404954903f').
narrative_ontology:cs_kernel_codification('52b8b2e6-cd7c-48f9-9aea-e2404954903f', formalized).
narrative_ontology:cs_authority_grounding('52b8b2e6-cd7c-48f9-9aea-e2404954903f', extraction).
narrative_ontology:cs_interpretation_layer_present('52b8b2e6-cd7c-48f9-9aea-e2404954903f').
narrative_ontology:cs_reading_relation('52b8b2e6-cd7c-48f9-9aea-e2404954903f', statutory_debt_ceiling__coordination_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('52b8b2e6-cd7c-48f9-9aea-e2404954903f', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_axiom('52b8b2e6-cd7c-48f9-9aea-e2404954903f', foundational, default_threat_extraction_is_illegitimate).
narrative_ontology:cs_axiom_status(default_threat_extraction_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('52b8b2e6-cd7c-48f9-9aea-e2404954903f', default_threat_extraction_is_illegitimate, deontological).
narrative_ontology:cs_axiom('52b8b2e6-cd7c-48f9-9aea-e2404954903f', secondary, ceiling_imposes_net_macroeconomic_cost).
narrative_ontology:cs_axiom_status(ceiling_imposes_net_macroeconomic_cost, holdable).
narrative_ontology:cs_axiom_grounding('52b8b2e6-cd7c-48f9-9aea-e2404954903f', ceiling_imposes_net_macroeconomic_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('52b8b2e6-cd7c-48f9-9aea-e2404954903f', factional_hostage_boundary).
narrative_ontology:cs_drift_state('52b8b2e6-cd7c-48f9-9aea-e2404954903f', contemporary_brinkmanship_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('52b8b2e6-cd7c-48f9-9aea-e2404954903f', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, congressional_minority_faction).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, austerity_policy_advocates).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_bondholders).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_program_recipients).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_employees_and_contractors).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, us_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, house_majority_leadership).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, us_president_administration).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, us_treasury_department).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__extraction_snare_reading, congressional_power_of_purse_doctrine).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__extraction_snare_reading, credible_default_threat_bargaining_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A disciplined caucus in the House that withholds the votes needed to pass any increase in the debt limit until the governing coalition concedes spending caps, rescissions, or policy riders. Its refusal is what makes the statutory limit bite; it can end any individual confrontation instantly by supplying votes once its terms are met, then retain the same lever for the next deadline. The costs of a miscalculated standoff — market turmoil, missed payments — land almost entirely outside its constituency.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, congressional_minority_faction, beneficiary,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, congressional_minority_faction, agenda_setter).

% Outside organizations — think tanks, advocacy coalitions, donor networks — that have pursued spending caps and entitlement restructuring through ordinary legislation for decades with limited success. Deadline confrontations hand them negotiated outcomes such as statutory caps and rescission packages that they could not move in regular order, and they supply the public argument that the limit enforces fiscal discipline.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, austerity_policy_advocates, beneficiary,
    organized, generational, mobile, national).

% Controls the House floor calendar and must assemble 218 votes for any increase before Treasury exhausts its accounting maneuvers. It concedes the faction's terms, owns the political blame for market turbulence, and cannot walk away because the payment obligation is absolute. Repealing the limit outright would fracture its own conference, so it manages the recurring crisis rather than ending it.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, house_majority_leadership, payer,
    institutional, biographical, constrained, national).

% Negotiates with congressional leaders as the payment deadline approaches, trading spending terms for an increase or suspension. It holds legally contested alternatives — invoking the Fourteenth Amendment's debt clause, mint-based accounting devices — that it declines to test, and it bears the economic and political consequences of any misjudged date.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, us_president_administration, payer,
    institutional, biographical, constrained, national).

% Executes the extraordinary-measures playbook — suspending certain investments, exchanging assets, conserving cash — to postpone the deadline, and drafts prioritization plans deciding which payments continue if the limit binds. It cannot issue debt above the limit and has no lawful way out; it implements whatever bargain arrives, on whatever timeline the confrontation permits.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, us_treasury_department, payer,
    institutional, biographical, trapped, national).

% Hold the Treasury securities whose on-time payment the limit jeopardizes. Each confrontation widens risk premiums on new issuance and marks down bill and bond prices in the danger window; the 2011 and 2023 downgrades institutionalized the episodic penalty. Individual holders can rebalance, but aggregate flight from the benchmark security is itself the disaster scenario, so exit is costly precisely when it matters.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_bondholders, payer,
    powerful, biographical, mobile, global).

% Seniors, veterans, disability beneficiaries, and nutrition-assistance households whose payments appear as deferrable lines in Treasury contingency planning. They have no hedge against a missed deposit and no representative in the negotiation that decides whether they are paid on time.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_program_recipients, payer,
    powerless, immediate, trapped, national).

% Federal workers facing furlough notices and contractors carrying unpaid receivables in confrontation windows. Their employment is regionally concentrated around federal installations, so individual exit means relocating away from their own labor market.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_employees_and_contractors, payer,
    moderate, biographical, constrained, national).

% Bear the interest-rate penalty embedded in Treasury yields after each episode — incremental borrowing costs that compound across the debt stock — along with the output losses of each near-miss. They cannot opt out of servicing the sovereign's debt, and the costs arrive with no corresponding service.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, us_taxpayers, payer,
    moderate, generational, trapped, national).

% Will service the debt issued at post-confrontation yields and live under the caps negotiated in their absence. Present in no negotiating room, their objection — that the limit adds cost without constraining spending already appropriated — is voiced only by proxy through fiscal analysts.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, future_generations_of_taxpayers, excluded,
    powerless, civilizational, trapped, national).

% Assess sovereign creditworthiness and publish the ratings that translate confrontation into market prices: the 2011 S&P downgrade and the 2023 Fitch downgrade both cited governance and brinkmanship rather than fiscal capacity. They take no side and hold no vote; their assessments arrive after the negotiating positions are set.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, credit_rating_agencies, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, congressional_minority_faction).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates congressional authorization of federal borrowing into a single aggregate limit, replacing per-issue approval of each Treasury offering with one periodic checkpoint; stated without evaluation of whether that checkpoint retains any gating function.
% TRANSFER_FUNCTION: Moves policy concessions — discretionary spending caps, rescissions, procedural commitments, and policy riders — from the governing coalition to the vote-blocking faction; moves default risk, elevated borrowing costs, and payment-triage exposure outward to bondholders, program recipients, federal employees and contractors, and taxpayers at large.
% ABSENT_VOICES: Future generations of taxpayers, who inherit the compounded interest cost of concessions financed at elevated yields, hold no seat (authored as an excluded stakeholder). Program recipients whose checks appear as deferrable lines in Treasury prioritization annexes are represented by no one at the table. Rank-and-file legislators favoring clean increases are denied floor votes by calendar control. Supermajority public opposition to default brinkmanship enters only as background pressure the faction has repeatedly shown it can outwait.
% DISAPPEARANCE_RATIONALE: If the ceiling vanished overnight, the faction would lose its principal leverage instrument: concessions currently extracted under deadline pressure would have to win in regular order or not at all. Treasury would stand down extraordinary measures and contingency triage; the episodic risk premium in Treasury pricing would compress; negotiation calendars would reorganize around appropriations deadlines rather than default deadlines. Nothing else in the fiscal apparatus depends on the limit — spending, revenue, and debt-issuance authorities all operate independently of it.
% FOUNDING_PROBLEM: After 1917 (Second Liberty Bond Act), Congress wanted to delegate routine wartime borrowing to Treasury without approving each bond issue individually, retaining a single aggregate cap as an oversight backstop.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: GAO reports (2011, 2015) establish that debt levels are determined by prior appropriations law, not the limit; Treasury secretaries of both parties (Geithner, Lew, Yellen) testified that the ceiling threatens payment of already-incurred obligations; CBO analyses separate appropriation decisions from the limit; the 2011 and 2023 rating actions cited governance rather than fiscal capacity. The benefiting faction attests the opposite — that the ceiling enforces discipline — and no source outside the beneficiary set corroborates that attestation.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.82) because the arrangement transfers real value under duress: negotiated caps and rescissions unavailable in regular order, plus a persistent risk premium in Treasury pricing after each episode (GAO attributed roughly 1.3 billion dollars of FY2011 borrowing cost to the 2011 brinkmanship alone; multi-episode estimates run to billions). Suppression (0.75) is the actively maintained default threat plus procedural gatekeeping — calendar control denying clean-increase votes — and it is authored as a raw structural property, unscaled by power or scope; the engine scales only extractiveness. Theater ratio (0.55) reflects a justification increasingly performed rather than functional: the ceiling does not constrain spending, which appropriations law sets in advance, so discipline rhetoric, protest votes, and messaging bills constitute a large share of observable activity. Accessibility collapse is low (0.35): alternatives — outright repeal, automatic adjustment, the Gephardt-rule precedent, premium bonds, executive-invocation arguments — remain visible and debated; they are politically suppressed, not cognitively collapsed. Resistance (0.62) is sustained — presidential pushback, market repricing, public-opinion majorities against brinkmanship, institutional testimony — and repeatedly defeated by faction discipline willing to hold the threat past market discomfort. The temporal series are cyclical rather than monotonic: extractiveness and suppression spike in confrontation windows (1995-96, 2011, 2013, 2021, 2023, 2025) and decay in suspension intervals (the 2000s, the 2015-2019 suspensions). The oscillation is partly the mechanism itself — intermittent reinforcement: each successful extraction rewards the tactic, each calm interval demobilizes the reform coalition, so the cycle re-arms. The base-property scalars are measured at interval end (t30), a confrontation-window peak; the mid-interval troughs are visible in the series.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different arrangements from identical statutory text. From the faction seat the limit is an option portfolio: costless to hold, exercisable on demand, paying concessions — the engine should compute near-full subsidy there. From the governing-coalition seats (majority leadership, the administration) the same text is coercive: obligation without discretion, concessions under deadline. From the Treasury seat it is administrative duress — extraordinary measures and triage planning with no lawful exit. From the recipient seat it is existential payment uncertainty decided in rooms they cannot enter. From the bondholder seat it is episodic tail risk priced into yields. Same-level differentiation matters: majority leadership and the faction occupy the same nominal institutional class, but leadership must produce an increase before the payment deadline while the faction need only withhold — the asymmetry lies in obligation, not power, and it is what makes the extraction possible.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: congressional_minority_faction (collects concessions; its refusal is also the active enforcement, hence its secondary agenda-setter position) and austerity_policy_advocates (collect policy wins they cannot pass in regular order). Both derive directionality near the beneficiary pole; the faction's arbitrage-grade exit — it can stand down from any single confrontation at will while retaining the lever — pushes it furthest toward subsidy. Victim declarations: federal_program_recipients and us_taxpayers (trapped, no hedge against the sovereign's payment decisions) sit nearest the full-target pole; federal_employees_and_contractors (constrained) intermediate; treasury_bondholders (mobile at portfolio level, but aggregate flight from the benchmark security is self-destabilizing) somewhat damped yet still clearly targeted. Institutional payers — majority leadership, the administration, Treasury — carry high directionality despite institutional power because their obligation is non-dischargeable: power does not purchase exit from a debt the law says must be paid. The analytical seat (credit_rating_agencies) feeds no directionality. Suppression remains unscaled throughout; scope amplification applies to extractiveness only.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — delegating routine borrowing authorization so Treasury need not seek per-issue approval — died when the appropriations process came to set spending in advance of borrowing; the ceiling now gates only the payment of obligations already lawfully incurred, a function GAO and successive Treasury secretaries of both parties attest it performs purely destructively. The arrangement persists because its dead form is exploitable: an aggregate limit nobody needs is precisely a lever anybody with a vote-block can pull. Classification guards against two mislabels: a pure-coordination reading would credit the ceiling with a fiscal-discipline function it does not perform (discipline lives in appropriations law), and an inertial-drift reading would attribute persistence to inertia when a concentrated beneficiary actively maintains the arrangement against documented, repeated resistance — concentrated gain plus coerced diffuse payment is the signature of deliberate maintenance, not drift. Genealogically the mandate is resolved (dead problem, living instrument); the mismatch between the dead founding problem and a world that still rearranges around the limit is the capture signal this story is built to expose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_separation,
    'This constraint is the extraction_snare_reading of the statutory_debt_ceiling kernel; would instantiating the coordination_scaffold_reading or the constitutional_nullity_reading instead yield a different constraint with different epsilon, beneficiary structure, and type?',
    'Generate the sibling stories and compare computed classifications: the scaffold reading authors low-extraction transitional support with sunset logic; the nullity reading authors a legally void constraint superseded by the Fourteenth Amendment''s Section 4 debt-validity command. Divergent outputs confirm the readings are distinct constraints, not one constraint viewed from different angles.',
    'If the scaffold reading computes near coordination cost, the present epsilon of 0.82 is reading-indexed rather than topic-indexed; if the nullity reading prevails as law, this constraint dissolves rather than reforms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_separation, conceptual, 'Committer structure: one of three readings of the statutory debt ceiling kernel; sibling readings are separate constraint files.').

omega_variable(
    coalitional_contingency_of_weaponization,
    'Is the ceiling''s extractive operation intrinsic to the statute, or contingent on the current configuration of narrow margins and unified faction discipline — noting that both parties have wielded the lever (Senate Democrats in 2006; House factions in 2011, 2013, and 2023)?',
    'Comparative coding of every ceiling episode since 1979 by government-division type, chamber margin size, and whether concessions were actually extracted.',
    'If extraction appears only under specific coalitional configurations, the classification attaches to the operating regime rather than the statute; the underlying statute might compute as a coordination device under unified government.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalitional_contingency_of_weaponization, empirical, 'Whether weaponization is structural or coalitionally contingent.').

omega_variable(
    default_threat_credibility,
    'How much of the measured extraction rests on the credibility of the default threat, and does repeated brinkmanship erode that credibility over successive episodes?',
    'Event studies of Treasury credit-default-swap spreads, bill yields, and auction tails across the 2011, 2013, 2021, 2023, and 2025 confrontation windows.',
    'If credibility decays episode over episode, extraction should shrink in later standoffs and a discredited threat converts the arrangement toward inertial maintenance; if credibility persists, the mechanism is durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(default_threat_credibility, empirical, 'Credibility dependence of the hostage mechanism.').

omega_variable(
    concession_counterfactual_ambiguity,
    'Did the extracted concessions (the 2011 caps and sequestration, the 2023 caps and rescissions) represent transfers the withholding faction could not have obtained in regular order, or outcomes that commanded independent legislative majorities?',
    'Counterfactual roll-call reconstruction: simulate the capped-spending packages under regular order with no payment deadline attached.',
    'If the concessions had independent majorities, the transfer function is overstated and the arrangement looks more like a timing device than an extractor; if not, extraction is confirmed as the operative function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concession_counterfactual_ambiguity, empirical, 'Whether extracted concessions are genuine transfers or coincident preferences.').

omega_variable(
    fourteenth_amendment_exit_viability,
    'Would executive invocation of Section 4 of the Fourteenth Amendment (issuing debt notwithstanding the limit) dissolve the hostage mechanism, and at what institutional and legal cost?',
    'Synthesis of constitutional scholarship plus market-reaction modeling of a tested invocation; the constitutional_nullity_reading sibling story carries the doctrinal claim in full.',
    'If viable and market-accepted, suppression collapses and the arrangement degrades rapidly; if litigation freezes payments during challenge, invocation raises rather than lowers default risk and reinforces the existing dynamic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fourteenth_amendment_exit_viability, conceptual, 'Viability of the constitutional exit that would dissolve the mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(stat_tr_t3, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(stat_tr_t6, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(stat_tr_t9, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 9, 0.32).
narrative_ontology:measurement(stat_tr_t12, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(stat_tr_t15, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(stat_tr_t18, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 18, 0.52).
narrative_ontology:measurement(stat_tr_t21, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 21, 0.44).
narrative_ontology:measurement(stat_tr_t24, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(stat_tr_t27, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 27, 0.47).
narrative_ontology:measurement(stat_tr_t30, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(stat_be_t3, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(stat_be_t6, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(stat_be_t9, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 9, 0.46).
narrative_ontology:measurement(stat_be_t12, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(stat_be_t15, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(stat_be_t18, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 18, 0.76).
narrative_ontology:measurement(stat_be_t21, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 21, 0.6).
narrative_ontology:measurement(stat_be_t24, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(stat_be_t27, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 27, 0.68).
narrative_ontology:measurement(stat_be_t30, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t3, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(stat_su_t6, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(stat_su_t9, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 9, 0.44).
narrative_ontology:measurement(stat_su_t12, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(stat_su_t15, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(stat_su_t18, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(stat_su_t21, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 21, 0.6).
narrative_ontology:measurement(stat_su_t24, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(stat_su_t27, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 27, 0.66).
narrative_ontology:measurement(stat_su_t30, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, resource_allocation).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the debt ceiling' conflates three structurally distinct claims with different epsilon values, victim sets, and failure modes. This file (extraction_snare_reading) authors the weaponized-operation claim. statutory_debt_ceiling__coordination_scaffold_reading authors the transitional-coordination claim — upstream, since the ceiling's genuine origin function is the residue the extraction rides on as cover. statutory_debt_ceiling__constitutional_nullity_reading authors the legal-void claim — downstream, since each documented extraction episode strengthens the Section 4 argument. Every member links the others through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
