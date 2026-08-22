% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__existential_risk_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: Existential-Risk-First AI Governance Prioritization (existential_risk_reading)
 *   domain: technology governance/ethics/risk assessment
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the existential_risk_reading — of
 *   the contested kernel ai_risk_governance_priority: what AI risk governance
 *   must prioritize. The kernel's three readings (existential_risk,
 *   near_term_harms, bridge) are structurally distinct constraints authored
 *   as separate files and linked through network.affects_constraints; they
 *   share a topic but not an ε. This reading's constraint is the
 *   catastrophic-risk-first ordering of AI governance: attention, funding,
 *   and institutional authority flow to preventing speculative
 *   superintelligence scenarios while demonstrated present harms are queued
 *   behind them. The ε referent is that standing prioritization arrangement
 *   itself, assessed by this reading's own lights — which endorse the
 *   ordering's aim while the structural data record its asymmetric operation:
 *   the reading holds the prioritization morally required, and the
 *   arrangement implementing it nonetheless concentrates gains in x-risk
 *   institutions and safety-branded labs while its claimed protected class
 *   (all future humanity) cannot audit delivery. The near_term_harms_reading
 *   would author a higher ε over the same referent (it sees misallocation
 *   where this reading sees justified priority); the bridge_reading would
 *   restructure the frame rather than re-rank it. Those differences are the
 *   reading-indexed ε values the corpus exists to compare — not
 *   inconsistencies to reconcile.
 *
 * KEY AGENTS:
 *   - xrisk_research_institutions: agenda-setter and collector (institutional/identity_locked) — defines the risk taxonomy, administers funding, staffs the panels; its institutional identity is the frame itself
 *   - safety_branded_frontier_labs: primary beneficiary (institutional/arbitrage) — converts compliance regimes into moats and safety branding into capital
 *   - present_harm_affected_populations: primary cost-bearer (powerless/trapped) — demonstrated harms queued behind speculative scenarios
 *   - small_ai_developers: secondary cost-bearer (moderate/constrained) — regressive fixed compliance costs, no seat in standards processes
 *   - all_future_humanity: claimed protected class, non-agent (powerless/trapped) — bears the ultimate failure cost, cannot audit or complain
 *   - near_term_harm_advocates: excluded voice (organized/constrained) — the rival reading's constituency, structurally marginalized
 *   - multilateral_governance_bodies: observer (institutional/analytical) — tracks the allocation, holds little enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.7).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.55).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "Existential-Risk-First AI Governance Prioritization (existential_risk_reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "technology governance/ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, 'df7f242b-0b4b-48f1-9a70-99ad620cd4a6').
narrative_ontology:cs_kernel_codification('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', distributed).
narrative_ontology:cs_authority_grounding('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', expertise).
narrative_ontology:cs_interpretation_layer_present('df7f242b-0b4b-48f1-9a70-99ad620cd4a6').
narrative_ontology:cs_reading_relation('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', foundational, existential_stakes_dominate_priority).
narrative_ontology:cs_axiom_status(existential_stakes_dominate_priority, holdable).
narrative_ontology:cs_axiom_grounding('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', existential_stakes_dominate_priority, empirically_contingent).
narrative_ontology:cs_axiom('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', foundational, future_humanity_decisive_moral_standing).
narrative_ontology:cs_axiom_status(future_humanity_decisive_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', future_humanity_decisive_moral_standing, deontological).
narrative_ontology:cs_axiom('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', secondary, precautionary_governance_before_demonstration).
narrative_ontology:cs_axiom_status(precautionary_governance_before_demonstration, holdable).
narrative_ontology:cs_axiom_grounding('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', precautionary_governance_before_demonstration, instrumental).
narrative_ontology:cs_reference_frame('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', catastrophe_precautionary_priority).
narrative_ontology:cs_drift_state('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', contemporary_post_safety_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('df7f242b-0b4b-48f1-9a70-99ad620cd4a6', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, safety_branded_frontier_labs).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, all_future_humanity).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, present_harm_affected_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, small_ai_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Alignment research institutes, existential-risk think tanks, and university safety groups define which AI risks count as governable, staff official advisory panels, and administer the funding pipelines that decide which safety work gets resourced. Their revenue, staffing, and standing depend on catastrophic scenarios remaining the organizing concern of AI governance; reorienting toward other risk frames would dissolve the institutional identity they have built over a decade. They publish the scenario taxonomies, host the workshops, and write the governance frameworks that downstream bodies adopt.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutions, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutions, beneficiary).

% Large AI developers that have adopted public safety commitments, published responsible-scaling policies, and staffed safety teams. The governance apparatus built around catastrophic risk raises fixed costs across the industry; as the largest actors they absorb these costs most easily and convert them into barriers smaller competitors cannot clear. Safety credentials attract capital, talent, and procurement contracts. They help write the evaluation standards they are evaluated by, and their frameworks are largely self-enforced. If a jurisdiction tightens rules beyond their preference, they can shift compute, incorporation, or product launches across borders.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, safety_branded_frontier_labs, beneficiary,
    institutional, biographical, arbitrage, global).

% Communities currently bearing algorithmic bias in credit, housing, hiring, and policing; workers displaced or managed by automated systems; people targeted by AI-enabled surveillance and misinformation. Governance attention, funding, and legislative bandwidth flow toward speculative superintelligence scenarios while their demonstrated harms wait in queue. They hold few seats in the expert panels where the AI governance agenda is set, and they cannot opt out of the algorithmic systems that affect them.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, present_harm_affected_populations, payer,
    powerless, immediate, trapped, global).

% Startups, open-source maintainers, and academic labs building AI systems at scales far below frontier capability. Compliance regimes written for catastrophic frontier risk impose fixed costs — evaluations, documentation, safety cases — that weigh heaviest on the smallest actors. They have no seat in the standards processes and cannot relocate their customer base the way large labs can. Some benefit indirectly from safety norms that stabilize the market, but the cost structure is regressive.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, small_ai_developers, payer,
    moderate, biographical, constrained, global).

% The class of people whose existence and potential the prioritization exists to protect — the moral patient on whose behalf the entire apparatus claims to act. It bears the ultimate cost if catastrophic capability development proceeds despite the apparatus, and it bears the opportunity cost of present resources and attention consumed by the apparatus in the meantime. It cannot collect, verify, complain, or exit; every interest it has in the arrangement is expressed through present institutions that are also its self-appointed representatives.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, all_future_humanity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__existential_risk_reading, all_future_humanity).

% Civil-society organizations, fairness and labor researchers, digital-rights groups, and affected-community organizers who argue AI governance should center demonstrated harms. They are consulted late in expert-led processes, funded marginally relative to alignment research, and routinely framed as distractions from the larger catastrophe. Some have begun adopting existential-risk language to secure funding and standing, which shifts their agendas. Their alternative — a present-harms-first governance agenda — has no dedicated institutional home.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, near_term_harm_advocates, excluded,
    organized, immediate, constrained, global).

% UN advisory bodies, OECD working groups, and international standards processes that convene the other seats and track how AI risk governance resources are allocated. They observe the imbalance between catastrophic-scenario work and demonstrated-harm work, commission comparative analyses, and can recommend rebalanced frameworks, though they hold little enforcement power over the national agencies, funders, and labs that set the actual agenda.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, multilateral_governance_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__existential_risk_reading, safety_branded_frontier_labs).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI developers, funders, and governments around genuine catastrophic-risk prevention problems no single actor can solve alone: shared evaluation regimes for frontier capabilities, compute-threshold governance, incident disclosure, and alignment research on systems whose failure modes are uncertain and potentially irreversible. The coordination is real even where the resource allocation around it is contested.
% TRANSFER_FUNCTION: Moves research funding, governance attention, legislative bandwidth, and moral urgency away from demonstrated present harms (bias, surveillance, labor displacement, misinformation) toward speculative superintelligence scenarios; concentrates institutional resources — funding, credentialing authority, agenda-setting power, regulatory moats — in x-risk research institutions and safety-branded frontier labs. The claimed recipient of the protection is all future humanity, which receives whatever the apparatus actually delivers and cannot audit the delivery.
% ABSENT_VOICES: Present-harm-affected populations and their advocates are structurally absent from the expert panels where the agenda is set — consulted late, funded marginally, framed as distraction. Future humanity is absent by construction: it appears only through self-appointed representatives whose funding depends on the frame they represent. Independent present-harms-first institutions barely exist, so the apparent consensus that catastrophic risk is the priority partly reflects who was in the room.
% DISAPPEARANCE_RATIONALE: Funding pipelines, career structures, advisory-panel composition, lab safety strategies, and legislative priorities are all organized around the catastrophic-risk-first ordering. Overnight removal would reflow governance resources toward demonstrated harms, strand x-risk institutions, strip safety branding of its regulatory value, and force labs to compete on present-harm records — a wholesale reorganization of the AI governance landscape, which is precisely what each seat fears or hopes for.
% FOUNDING_PROBLEM: In the mid-2010s, capability progress began outrunning any shared framework for catastrophic AI outcomes: no actor could verify that advanced systems would remain controllable, and the reading's founders argued that unless existential risk became the organizing priority of governance before transformative capability arrived, prevention would arrive too late.
% FOUNDING_PROBLEM_CORROBORATION: Partial and structurally compromised. The strongest attestations of ongoing catastrophic risk come from inside the beneficiary set — lab safety teams, alignment institutes — whose funding and standing depend on the frame. Outside corroboration exists in national security and government risk assessments, a minority of independent academics who accept tail risk while disputing its priority, and insurance-sector analyses, but no institution outside the benefiting parties attests that the risk remains the correct FIRST priority rather than one concern among several; that judgment is precisely what the sibling readings contest.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is tangled_rope on structure: the coordination function is genuine (catastrophic-risk prevention is a real collective-action problem with irreversible stakes), the same structure carries asymmetric transfer (present-harm populations and small developers bear costs while concentrated institutions and labs collect), and it requires active enforcement (funding gatekeeping, panel composition, credentialing). The metrics describe the arrangement's actual operation: ε = 0.70 reflects the scale of resource and attention transfer against a claimed recipient that cannot verify delivery; suppression = 0.55 reflects agenda control and framing gatekeeping rather than coercion — rival readings publish freely but cannot get agendas, funding, or panels; theater = 0.45 reflects the safety-washing share of activity (self-enforced frameworks, evaluations that gate no releases, scenario work that produces documents rather than decisions); accessibility_collapse = 0.50 because a present-harms-first alternative remains constructible (the bridge reading exists as proof) but has no institutional home; resistance = 0.60 because the excluded constituency actively contests the ordering in every governance process — and the coalition potential of the powerless seat is real, expressed through the organized advocates, which is the arrangement's main vulnerability. The measurement series share one grid; ε and theater climb together through the 2020–2024 consolidation, tracking Goodhart drift from risk reduction toward frame maintenance. Suppression_requirement is authored because enforcement capacity demonstrably intensified (panel gatekeeping, funding consolidation) over the same window.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the xrisk_research_institutions seat the arrangement is the minimum adequate response to an irreversible stake — the costs it imposes are the price of survival, and the theater is an unfortunate externality of imperfect institutions. From the present_harm_affected_populations seat the same arrangement is abandonment: demonstrated, verifiable harms queued behind scenarios that may never occur, run by institutions that profit from the queue. From the safety_branded_frontier_labs seat it is a manageable cost that purchases competitive insulation. The engine derives these per-seat classifications from power, exit, and directionality; the divergence between the agenda-setter's survival framing and the payer's abandonment framing is the measurement, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: x-risk institutions (they administer and collect) and safety-branded labs (they collect moat value and legitimacy). The labs' true position sits slightly above the pure-beneficiary floor because they also bear real compliance costs and face residual risk that the apparatus could one day bind them — which is exactly why they work to keep the frameworks self-enforced. No directionality override is authored: the override surface keys on power atoms, and the institutional atom is shared by seats with genuinely different positions (institutions, labs, observers), so a single override would misstate three seats at once; the structural derivation from declared roles plus exit options is the more faithful input. Payers sit near the target end: present-harm populations (powerless, trapped, no seat) derive near-full target d; small developers (moderate, constrained) slightly lower. all_future_humanity is authored as a non-agent and is excluded from directionality by design: it is the claimed recipient of the arrangement's protection, not an actor inside it — assigning it a directionality value would launder its representation through the very institutions whose gains depend on speaking for it. Its position is carried by the representation omega instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification does double duty. Against the pure-extraction reading: catastrophic-risk prevention is not a cover story — the stakes are genuinely irreversible and the coordination problem genuinely unsolvable by single actors, so classifying the arrangement as pure extraction would erase the real coordination it performs and hand its defenders a refutation. Against the pure-coordination reading: the transfer is large, concentrated, and undisciplined by its claimed recipient, so classifying it as pure coordination would launder the moat-building and agenda capture. The R5 interview locates the obsolescence risk: the founding problem is live (the risk is unresolved), so no dead-mandate flag fires, but the theater series (0.20 → 0.45) tracks the mandate drifting from risk reduction toward frame maintenance — safety-washing is mandatrophy in slow motion, and the mismatch consumer should read the rising theater against the live founding problem rather than waiting for the problem to die. If the founding problem were resolved (capability plateau, refuted risk) with the apparatus intact, the arrangement would reclassify toward inertial drift; if capture completes (agenda serving institutions over risk reduction), toward pure extraction with coordination residue.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophic_probability_dispute,
    'Is the probability of near-term catastrophic superintelligence outcomes high enough to justify the prioritization''s opportunity cost on present harms?',
    'Calibrated long-horizon forecasting, capability-trajectory data, and adversarial red-team review of the scenario models underlying the priority ordering — adjudicated by bodies outside the beneficiary set.',
    'A low assessed probability converts the arrangement''s transfer from survival-priced coordination into unjustified rent flowing to the frame''s institutions; a high probability strengthens the reading''s claim that no price is too high and pushes the classification toward the coordination end of the hybrid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophic_probability_dispute, empirical, 'Whether the founding empirical premise — dominant catastrophic risk — holds at the magnitude the prioritization assumes.').

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of kernel ai_risk_governance_priority (reading: existential_risk_reading). What would the sibling readings change structurally, and where is the disagreement located?',
    'No empirical resolution; resolved by which reading a governance framework adopts — tracked through which reading''s institutions hold agenda power. The sibling stories carry their own structural data.',
    'Adopting near_term_harms_reading replaces the victim set with present marginalized populations, inverts the ε profile (high on demonstrated bias, low on speculative capability), and redirects resource flow from alignment-as-control toward harm remediation; adopting bridge_reading dissolves the single-priority structure into unified entangled frameworks. The three stories must be compared, never merged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: the disagreement is located in the time-index and probability-weighting of the victim class — which cost-bearers count, and how speculative loss is weighed against demonstrated harm.').

omega_variable(
    safety_washing_share,
    'What fraction of the arrangement''s catastrophic-risk activity reduces actual risk versus performs safety for legitimacy and moat purposes?',
    'Independent audit correlating published safety commitments with binding capability-gating decisions: has any framework actually stopped or delayed a release, and under what conditions?',
    'A theater-dominant finding pushes the arrangement toward the pure-extraction boundary (coordination as cover); a gating-genuine finding supports the hybrid reading and lowers effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_washing_share, empirical, 'Share of performative versus functional activity inside the x-risk governance apparatus.').

omega_variable(
    beneficiary_capture_status,
    'Has the priority ordering been captured by its beneficiaries — do agenda and funding decisions track risk reduction or institutional self-perpetuation?',
    'Trace funding allocations and panel appointments against measured risk-reduction output; compare the bindingness of governance frameworks with the market positions of their framers.',
    'Capture confirmed converts the hybrid toward pure extraction with coordination residue; capture refuted strengthens the weight of the genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_status, empirical, 'Whether the arrangement''s agenda serves the risk or the institutions that administer it.').

omega_variable(
    future_humanity_representation,
    'Does ''all future humanity'' constitute a cost-bearing class with structural standing, or a representation device operated by present institutions whose funding depends on the frame they represent?',
    'Conceptual analysis of representation accountability plus empirical study of whether representative institutions'' positions track argued future interests or present funding incentives.',
    'If pure representation device, the claimed recipient cannot discipline the actual beneficiaries and the undisciplined transfer strengthens the extraction reading; if genuine standing exists through some accountable channel, part of the transfer is legitimate proxy exercise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_humanity_representation, conceptual, 'Standing of the non-agent claimed protected class and the accountability of its self-appointed representatives.').

omega_variable(
    advocacy_framing_capture,
    'Has the excluded constituency internalized the dominant frame — are present-harm advocates adopting existential language to survive, blurring structural gatekeeping into internalized framing capture?',
    'Longitudinal discourse analysis of advocacy proposals and public communications before and after the 2022–2023 funding consolidation.',
    'Internalized capture means the ordering''s suppressive effect persists even where formal barriers fall and the excluded seat under-reports its own opposition; measured resistance is then an underestimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(advocacy_framing_capture, empirical, 'Structural versus internalized suppression mechanism in the excluded advocacy seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 2014, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(xrisk_priority_reading_tr_t2014, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(xrisk_priority_reading_tr_t2016, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement(xrisk_priority_reading_tr_t2018, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(xrisk_priority_reading_tr_t2020, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(xrisk_priority_reading_tr_t2022, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2022, 0.38).
narrative_ontology:measurement(xrisk_priority_reading_tr_t2024, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2024, 0.45).
narrative_ontology:measurement(xrisk_priority_reading_tr_t2026, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2026, 0.45).

% Extraction over time
narrative_ontology:measurement(xrisk_priority_reading_be_t2014, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2014, 0.35).
narrative_ontology:measurement(xrisk_priority_reading_be_t2016, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2016, 0.4).
narrative_ontology:measurement(xrisk_priority_reading_be_t2018, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement(xrisk_priority_reading_be_t2020, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(xrisk_priority_reading_be_t2022, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement(xrisk_priority_reading_be_t2024, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2024, 0.67).
narrative_ontology:measurement(xrisk_priority_reading_be_t2026, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2026, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(xrisk_priority_reading_su_t2014, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2014, 0.3).
narrative_ontology:measurement(xrisk_priority_reading_su_t2016, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2016, 0.33).
narrative_ontology:measurement(xrisk_priority_reading_su_t2018, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2018, 0.38).
narrative_ontology:measurement(xrisk_priority_reading_su_t2020, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement(xrisk_priority_reading_su_t2022, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement(xrisk_priority_reading_su_t2024, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2024, 0.55).
narrative_ontology:measurement(xrisk_priority_reading_su_t2026, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, algorithmic_bias_governance).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI risk governance' decomposes under ε-invariance into three readings of one kernel, authored as separate files. This reading (existential_risk) authors high ε over speculative capability scenarios and a future-humanity victim class; near_term_harms_reading authors high ε over demonstrated present harms and a present-population victim class; bridge_reading authors the entanglement claim itself. Upstream/downstream structure: the existential reading currently dominates resource allocation and thereby structurally influences both siblings — it drains the near-term reading's resource base and forces the bridge reading into a defensive re-framing role. algorithmic_bias_governance is included as the downstream constraint whose resource base this arrangement deprioritizes; its ε is assessed low by this reading and high by the near-term sibling — the divergence is the family's data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
