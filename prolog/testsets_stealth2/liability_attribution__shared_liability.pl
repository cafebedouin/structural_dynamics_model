% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__shared_liability, []).

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
 *   constraint_id: liability_attribution__shared_liability
 *   human_readable: Value-Chain Joint Liability Allocation by Causal Contribution and Control
 *   domain: technology governance/legal theory/regulatory design
 *
 * SUMMARY:
 *   A legislature allocates legal responsibility for harms caused by AI
 *   systems across every actor in the production chain — model developers,
 *   integrators, deployers — in shares keyed to each actor's causal
 *   contribution to the harm and degree of control over the risk that
 *   produced it. Injured parties may pursue any contributing actor for their
 *   share; firms manage their exposure through insurance and contractual
 *   indemnification; regulators adjust contribution presumptions as
 *   deployment patterns shift. This file instantiates the shared-liability
 *   reading of the liability-attribution kernel only; the developer-primary
 *   and deployer-primary siblings are separate constraints with their own
 *   victim sets and their own classifications, linked through the network
 *   section. KEY AGENTS (by structural relationship): see key_agents.
 *
 * KEY AGENTS:
 *   - ai_system_developers: primary paying tier (powerful/constrained) — pays contribution-keyed damages shares for capability-level defects
 *   - ai_system_deployers: primary paying tier (institutional/constrained) — pays context-control-keyed shares where harms materialize
 *   - small_ai_startups: thin-capitalized tail of the paying side (moderate/constrained) — same exposure without absorption capacity
 *   - ai_harm_claimants: intended beneficiary (organized/trapped) — gains multiple solvent respondents but must still prove contribution
 *   - liability_insurers: structural beneficiary (institutional/arbitrage) — collects the premium stream the rule makes necessary
 *   - compliance_intermediaries: secondary beneficiary (organized/mobile) — paid to operate the allocation machinery
 *   - liability_regulators: agenda setter (institutional/generational) — writes and administers the allocation statute
 *   - open_source_contributors: excluded voice (moderate/mobile) — inside the chain, outside the bargain
 *   - tort_reform_scholars: analytical observer — measures awarded shares against the stated principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.55).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.48).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.55).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Value-Chain Joint Liability Allocation by Causal Contribution and Control").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology governance/legal theory/regulatory design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '3e6fda79-64c1-4b17-8969-852370566123').
narrative_ontology:cs_kernel_codification('3e6fda79-64c1-4b17-8969-852370566123', formalized).
narrative_ontology:cs_authority_grounding('3e6fda79-64c1-4b17-8969-852370566123', distributed).
narrative_ontology:cs_reading_relation('3e6fda79-64c1-4b17-8969-852370566123', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('3e6fda79-64c1-4b17-8969-852370566123', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('3e6fda79-64c1-4b17-8969-852370566123', foundational, responsibility_proportional_to_control).
narrative_ontology:cs_axiom_status(responsibility_proportional_to_control, holdable).
narrative_ontology:cs_axiom_grounding('3e6fda79-64c1-4b17-8969-852370566123', responsibility_proportional_to_control, deontological).
narrative_ontology:cs_axiom('3e6fda79-64c1-4b17-8969-852370566123', secondary, apportioned_shares_optimize_chain_deterrence).
narrative_ontology:cs_axiom_status(apportioned_shares_optimize_chain_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('3e6fda79-64c1-4b17-8969-852370566123', apportioned_shares_optimize_chain_deterrence, instrumental).
narrative_ontology:cs_reference_frame('3e6fda79-64c1-4b17-8969-852370566123', proportional_apportionment_framework).
narrative_ontology:cs_drift_state('3e6fda79-64c1-4b17-8969-852370566123', contemporary_case_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3e6fda79-64c1-4b17-8969-852370566123', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, ai_harm_claimants).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, liability_insurers).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, compliance_intermediaries).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_system_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_system_deployers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, small_ai_startups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and train the underlying models sold or licensed down the chain. Under the allocation rule they carry a damages share whenever a deployed system injures someone and their design choices contributed — training-data defects, inadequate evaluation, undocumented failure modes. They cannot decline the exposure without leaving the market; they manage it through insurance purchases, contractual indemnification caps, and safety documentation that shapes fault findings.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_system_developers, payer,
    powerful, biographical, constrained, global).

% Integrate third-party models into products, services, and internal workflows where harm actually materializes. They carry a damages share keyed to their control over the deployment context — user-facing configuration, human oversight, monitoring. They manage exposure through vendor indemnities, usage policies, and incident documentation; refusing deployment altogether is the only complete exit, and competitive pressure forecloses it.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_system_deployers, payer,
    institutional, biographical, constrained, global).

% Sit at the thin end of the developer group: little capital, no captive insurance desk, minimal legal staff. The same percentage exposure that large labs absorb as a line item threatens solvency, and insurers quote them rates reflecting that. Their realistic responses are narrower coverage, aggressive contractual disclaimers, or incorporation structures that ring-fence assets — each raising the cost of participating at all.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, small_ai_startups, payer,
    moderate, immediate, constrained, regional).

% People injured by deployed systems — clinical decision-support errors, autonomous-vehicle incidents, discriminatory screening outcomes. The allocation rule gives them multiple solvent respondents instead of requiring proof of one decisive bad actor, so recovery no longer dies with an unattributable causal chain. Reaching it still requires demonstrating each defendant's contribution, which means discovery against far better-resourced opponents; this litigation channel is their only route to compensation.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_harm_claimants, beneficiary,
    organized, biographical, trapped, global).

% Underwrite the AI liability lines the allocation rule makes necessary, collecting premiums from every firm in the chain and pricing each tier's share. They can reprice annually, tighten exclusions, or withdraw from the line entirely; their actuarial files become the de facto map of who really bears what along the chain.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, liability_insurers, beneficiary,
    institutional, generational, arbitrage, global).

% Law firms, auditors, and conformity assessors who run the allocation machinery: drafting indemnification chains, producing the technical documentation courts weigh, testifying on contribution shares. Their revenue scales with the complexity of the allocation itself, and they serve whichever tier retains them.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, compliance_intermediaries, beneficiary,
    organized, biographical, mobile, continental).

% Legislatures and agencies that wrote the allocation statute and now administer it: setting presumptions of contribution, reviewing insurance-adequacy requirements, adjusting shares as deployment patterns shift. They are bound by the enacted text between revision cycles and by the coalition politics that produced it.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, liability_regulators, agenda_setter,
    institutional, generational, constrained, continental).

% Volunteer developers whose code enters commercial chains without contracts, indemnities, or insurance behind it. The allocation debate proceeds as if responsibility attaches only to incorporated actors; whether their contributions count as causal contribution is undecided, and they hold no seat in the legislative consultations that will decide it.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, open_source_contributors, excluded,
    moderate, biographical, mobile, global).

% Academic lawyers and economists who trace how the allocation performs against its stated principle — comparing awarded shares to measured contribution, documenting deep-pocket deviations, modeling incentive effects across the chain. They publish outside every tier's retention and hold no vote in revision.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, tort_reform_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, liability_insurers).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns compensation responsibility and marginal deterrence across a production chain where no single actor controls outcomes: every actor with control over a slice of the risk faces a priced share, and injured parties have multiple solvent respondents instead of needing to identify one decisive bad actor.
% TRANSFER_FUNCTION: Moves money from developers and deployers — through damages, settlements, and the insurance premiums those exposures generate — to injured claimants, and to the insurers and compliance intermediaries who operate the transfer.
% ABSENT_VOICES: Open-source contributors whose code sits inside commercial chains without contracts or counsel; prospective entrants deterred by compliance costs before they ever lobbied; end users whose prices absorb passed-through costs. None holds a seat in the consultations that set contribution presumptions.
% DISAPPEARANCE_RATIONALE: Overnight repeal would strand recent injuries between ordinary negligence and contract doctrines never built for distributed machine agency: claimants with unattributable causal chains lose their compensation path, insurers withdraw the AI lines within renewal cycles, and firms unwind indemnification clauses — deployment contracts, pricing, and safety investment would all reorganize around whatever residual doctrine courts improvised.
% FOUNDING_PROBLEM: Harms from learned systems resist the single-defendant, proximate-cause template: a developer builds a general capability whose failures depend on deployment contexts it never sees, a deployer operates a system whose internal behavior it cannot inspect, and injured parties fall into the gap between them uncompensated.
% FOUNDING_PROBLEM_CORROBORATION: Published appellate opinions dismissing AI-injury suits for unattributable causation, legislative impact assessments accompanying the allocation statutes, and the tort-scholarship record of uncompensated distributed-agency harms all attest the founding problem and its continuing life from outside the insurer and intermediary seats that profit from administering it; no benefiting party's self-assessment is needed to establish it.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__shared_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__shared_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__shared_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-substantial (0.55): the rule delivers compensation that did not previously exist for distributed-agency harms, but a growing fraction of every dollar moved is premium loading, defense cost, and compliance drag rather than victim recovery. Suppression (0.48) reflects state-compelled participation — no firm in the chain can decline the exposure — tempered by real contractual maneuvering room inside the rule. Theater (0.32) captures documentation rituals (checkbox risk assessments, boilerplate technical files) that are growing but have not displaced the payout function. Accessibility collapse is low-moderate (0.42): rival allocation principles remain live political options in other jurisdictions, and firms contract around shares within the rule. Resistance (0.62) reflects sustained industry campaigns for safe harbors, damages caps, and developer-side immunities. The claim and the metrics are independent authored facts: claimed_type=tangled_rope because the structure simultaneously solves a genuine multi-actor compensation-and-deterrence problem and routes an increasing share of its flow through insurer and intermediary seats; the metric values describe observed operation without being tuned to any predicted verdict. Measurements run on one shared seven-point grid (all three tracked metrics at every point, interval units = years since first value-chain allocation statutes for automated-decision harms): suppression_requirement rises because the story specifically traces enforcement-capacity maturation from ex post litigation toward standing regulatory administration and insurance-adequacy review; base_extractiveness accumulates as premium loading and compliance layers thicken; theater_ratio climbs as documentation ritualizes. No cyclical dynamics are asserted — the trajectory is monotone maturation.
 *
 * PERSPECTIVAL GAP:
 *   From the insurer seat the arrangement is a market: priced risk, annual repricing, withdrawal rights — the mildest possible experience of the rule. From the two paying seats it is a permanent levy on deployment whose rate is set by adversaries in discovery. The startup seat experiences a third thing again: an entry barrier priced in premiums it cannot diversify. Claimants experience a partial remedy — vastly better than the pre-regime causal-gap void, short of what the proportionality principle promises on paper. The engine computes these per-seat types from power, exit, and directional position; the divergence between insurer-seat mildness and payer-seat severity is the structural signature of a hybrid arrangement, not evidence that any seat is mistaken.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place claimants, insurers, and intermediaries near the subsidized end: the rule creates their compensation path and their markets. Victim declarations place developers and deployers near the target end: they fund every flow in the system. Exit modulates within each pole — insurers hold arbitrage-grade exit (reprice or abandon the line) and sit nearest the beneficiary extreme; claimants are trapped (this litigation channel is their only path to recovery), which keeps their realized benefit below their nominal position; developers and deployers are constrained (market participation requires accepting the exposure), pushing them toward the full-target end; startups' thinner buffers make their experienced burden heavier than their formal share suggests. Continental scope with globally operating firms inside it makes verification of contribution shares harder, so effective extraction runs somewhat above the raw authored value for the paying seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distributed machine agency defeating the single-defendant, proximate-cause template — remains live, so no mandatrophy resolution is declared. The classification guards both directions of mislabeling: reading the arrangement as pure extraction erases the compensation channel that did not exist before it; reading it as pure coordination erases the insurer-and-intermediary capture now taking a growing cut of every flow. The temporal series is the early-warning surface: theater_ratio continuing to climb toward 0.5 would signal documentation substituting for deterrence, and further base_extractiveness accumulation past the current endpoint would warrant investigating transition from hybrid toward captured operation. The R5 mismatch consumer should note founding_problem_status=live paired with disappearance_verdict=world_rearranges — a consistent pairing producing no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation_unit,
    'This constraint is the shared_liability reading of the liability_attribution kernel; would instantiating a sibling reading — developer-primary or deployer-primary assignment — change the structural classification rather than merely redistribute the same payment burden?',
    'Comparative analysis of jurisdictions that adopted different primary allocations: measure compensation rates, deterrence proxies, and compliance-cost incidence under each reading.',
    'If a single-tier reading achieves comparable compensation with lower total burden, this reading''s coordination claim weakens and its profile shifts toward pure extraction; if apportionment uniquely sustains insurability across the chain, the coordination function is confirmed as specific to this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation_unit, conceptual, 'Whether the choice among kernel readings changes classification, not just incidence.').

omega_variable(
    deep_pocket_deviation,
    'Does adjudication under the allocation rule actually apportion damages by causal contribution and control, or do awards gravitate to the best-capitalized respondent regardless of contribution?',
    'Systematic coding of decided multi-party technology-liability cases: compare awarded shares against measured contribution and against defendant capitalization.',
    'If capitalization predicts awards better than contribution, effective burden concentrates on large developers and deployers well above the authored level, and the fairness axiom fails in operation while remaining intact on paper.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deep_pocket_deviation, empirical, 'Whether practice matches the contribution-and-control apportionment principle.').

omega_variable(
    insurance_layer_effect_on_deterrence,
    'Does the insurance market the allocation rule calls forth dampen deterrence through moral hazard, or sharpen it through actuarial pricing of safety practices?',
    'Actuarial and underwriting data: do premiums differentiate on measurable safety investment, and do insured firms'' incident rates diverge from uninsured comparators?',
    'Moral-hazard dominance would mean the coordination function is increasingly carried by paperwork while real deterrence decays, supporting drift toward theatrical maintenance; actuarial refinement would confirm the incentive channel is live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_layer_effect_on_deterrence, empirical, 'Direction of the insurance layer''s effect on the rule''s deterrent function.').

omega_variable(
    indemnification_burden_incidence,
    'Do contractual indemnification chains allocate the opacity burden — the duty to document, disclose, and prove system behavior — to the party best able to reduce the underlying uncertainty, or to the party with the least bargaining power?',
    'Audit of standard-form vendor agreements across value-chain tiers, mapping disclosure obligations and indemnity triggers against each tier''s technical visibility.',
    'If the burden tracks bargaining weakness rather than technical position, the reading''s own fairness premise is violated by its contractual implementation, and the asymmetry between tiers widens beyond the authored profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indemnification_burden_incidence, empirical, 'Where the opacity burden actually lands along the contracted chain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.18).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__shared_liability, theater_ratio, 4, 0.21).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__shared_liability, theater_ratio, 8, 0.24).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__shared_liability, theater_ratio, 12, 0.27).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__shared_liability, theater_ratio, 16, 0.29).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.31).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__shared_liability, theater_ratio, 24, 0.32).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(liab_be_t4, liability_attribution__shared_liability, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(liab_be_t8, liability_attribution__shared_liability, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(liab_be_t12, liability_attribution__shared_liability, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(liab_be_t16, liability_attribution__shared_liability, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(liab_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(liab_be_t24, liability_attribution__shared_liability, base_extractiveness, 24, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(liab_su_t4, liability_attribution__shared_liability, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(liab_su_t8, liability_attribution__shared_liability, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(liab_su_t12, liability_attribution__shared_liability, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(liab_su_t16, liability_attribution__shared_liability, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(liab_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(liab_su_t24, liability_attribution__shared_liability, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).

% DUAL FORMULATION NOTE:
% Colloquial 'AI liability' conflates three structurally distinct allocation rules. This file instantiates the shared/contribution-based reading only: its epsilon covers whole-chain apportionment in which both producer tiers pay and insurers plus compliance intermediaries collect. The developer-primary and deployer-primary siblings each concentrate the paying side on a single tier, producing different epsilon profiles, different beneficiary pressure, and their own classifications. They are linked here as constraint-family members per the epsilon-invariance decomposition rule, not merged into this story; the upstream general commitment (that liability for distributed machine agency must be assigned somehow) is cited by all three readings as evidence for their particular allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
