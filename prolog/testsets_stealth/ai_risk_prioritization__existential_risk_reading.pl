% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__existential_risk_reading, []).

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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: Existential Risk Prioritization Reading — Misaligned AGI as Extinction-Level Threat
 *   domain: technological/governance/epistemic
 *
 * SUMMARY:
 *   A prioritization regime in AI safety holds that the primary risk from AI
 *   is existential — that misaligned artificial general intelligence could
 *   cause human extinction — and that alignment research and capability
 *   controls are therefore the paramount use of safety resources. Over the
 *   interval the regime concentrates a large dedicated funding pool, a career
 *   structure, conference and press attention, and regulatory agenda space on
 *   long-horizon alignment questions, while harms from deployed systems
 *   (discrimination, labor displacement, surveillance) are recast as
 *   second-order or as distractions from larger stakes. This story
 *   instantiates the existential_risk_reading of the ai_risk_prioritization
 *   kernel (see kernel_context). The claim and the metrics are authored
 *   independently: the claimed type records the structure this reading
 *   actually builds — a genuine coordination function around a possibly-real
 *   catastrophic risk, fused with asymmetric resource capture and actively
 *   maintained discourse enforcement — while the metrics describe the
 *   arrangement's observed operation. The engine computes per-seat
 *   classifications from the structural data; where a computed type diverges
 *   from the claim, that divergence is the measurement. KEY AGENTS (by
 *   structural relationship): - longtermist_funders: agenda-setting
 *   beneficiary (institutional/arbitrage) — sets which questions count as
 *   serious, collects field control - x_risk_research_institutions: primary
 *   beneficiary (organized/identity_locked) — receives the resource transfer,
 *   constituted by the framing - alignment_research_workforce:
 *   beneficiary/payer hybrid (moderate/identity_locked) — careers funded by
 *   the framing, exit priced as moral betrayal - frontier_ai_labs: dual
 *   beneficiary/payer (institutional/arbitrage) — collects legitimacy cover,
 *   would bear the capability controls the framing calls for -
 *   near_term_ai_harm_victims: primary target (powerless/trapped) — bears
 *   present harms plus the opportunity cost of diverted attention -
 *   near_term_harms_researchers: excluded (moderate/constrained) —
 *   marginalized as distraction, shrinking funding and standing -
 *   future_humanity: declared ultimate stakeholder, nonexistent — cannot hold
 *   a seat; voiced only by parties who collect from the voicing -
 *   ai_policy_regulators: administering seat (institutional/constrained) —
 *   implements the prioritization in policy while deployed-harm enforcement
 *   stays thin
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.65).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.62).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "Existential Risk Prioritization Reading — Misaligned AGI as Extinction-Level Threat").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "technological/governance/epistemic").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, '8d096821-56cd-4eff-a7bb-f8d702c646c1').
narrative_ontology:cs_kernel_codification('8d096821-56cd-4eff-a7bb-f8d702c646c1', distributed).
narrative_ontology:cs_authority_grounding('8d096821-56cd-4eff-a7bb-f8d702c646c1', expertise).
narrative_ontology:cs_interpretation_layer_present('8d096821-56cd-4eff-a7bb-f8d702c646c1').
narrative_ontology:cs_reading_relation('8d096821-56cd-4eff-a7bb-f8d702c646c1', ai_risk_prioritization__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('8d096821-56cd-4eff-a7bb-f8d702c646c1', foundational, misaligned_agi_extinction_credible).
narrative_ontology:cs_axiom_status(misaligned_agi_extinction_credible, holdable).
narrative_ontology:cs_axiom_grounding('8d096821-56cd-4eff-a7bb-f8d702c646c1', misaligned_agi_extinction_credible, empirically_contingent).
narrative_ontology:cs_axiom('8d096821-56cd-4eff-a7bb-f8d702c646c1', foundational, future_generation_priority_overrides_present).
narrative_ontology:cs_axiom_status(future_generation_priority_overrides_present, holdable).
narrative_ontology:cs_axiom_grounding('8d096821-56cd-4eff-a7bb-f8d702c646c1', future_generation_priority_overrides_present, deontological).
narrative_ontology:cs_reference_frame('8d096821-56cd-4eff-a7bb-f8d702c646c1', longtermist_existence_priority).
narrative_ontology:cs_drift_state('8d096821-56cd-4eff-a7bb-f8d702c646c1', post_frontier_deployment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8d096821-56cd-4eff-a7bb-f8d702c646c1', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, alignment_research_workforce).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_ai_harm_victims).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_harms_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, alignment_research_workforce).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, longtermist_axiology).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, agi_extinction_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Direct the largest philanthropic pools dedicated to AI safety. Their grant decisions determine which research questions count as serious, which careers get funded, and which venues set the field's agenda. They describe their giving as insurance against catastrophic outcomes on civilization timescales. Their capital is mobile — it can move to new framings, fields, or institutions if priorities shift — and they also gain from the field that forms around their priorities, which amplifies their judgment and reach.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary).

% Receive the bulk of dedicated AI-safety funding and supply the field's research agenda, conferences, and hiring pipelines. Their institutional missions are constituted by the claim that catastrophic AI outcomes are the defining problem: staff careers, donor relationships, and public identity are built on it. Abandoning the framing would mean dissolving the institution's reason to exist, so they maintain it through agenda-setting, publication, and recruitment.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, beneficiary,
    organized, generational, identity_locked, global).

% Researchers, engineers, and fellows employed in alignment and safety roles. The arrangement funds their careers and tells them they work on the most important problem in history. The same framing raises the moral price of exit: moving to applied fairness work or leaving AI altogether is legible to peers as abandoning the fight. Some also chafe under agenda control by funders and lab safety-team priorities, and a minority publicly dispute the premise while remaining inside the career structure.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, alignment_research_workforce, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, alignment_research_workforce, payer).

% Operate the frontier systems whose risk the arrangement is about. They gain legitimacy from endorsing the existential framing — safety teams, evaluations, and responsible-scaling policies signal seriousness to regulators and the public — while continuing capability development. They would bear real costs under the binding capability controls the framing calls for, and the strongest proposed controls remain voluntary or unenforced. They can relocate, restructure, or rebrand if the framing turns against them.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs, payer).

% Communities subject to deployed systems: automated hiring rejections, welfare fraud flags, predictive policing, workplace surveillance, and displacement of gig and clerical work. Their harms are measurable now, but the dominant framing recasts attention to them as a distraction from larger stakes. They cannot exit the systems that classify them, and they hold few seats in the venues where AI priorities are set; their remedy is always deferred behind the larger claimed stakes.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_ai_harm_victims, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, near_term_ai_harm_victims, excluded).

% Fairness, labor, and civil-society researchers studying harms of deployed systems. Under the dominant framing their work is described as second-order; grant panels, keynote slots, and press attention concentrate on catastrophic-risk questions. Some migrate to existential-risk topics to survive professionally; those who stay face shrinking funding and reduced standing. Exit to adjacent fields is possible but costly to expertise and community.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_harms_researchers, excluded,
    moderate, biographical, constrained, global).

% The people who would exist after any transformative-AI transition. The arrangement claims to act on their behalf, and every statement of their interests is issued by present parties who collect from the arrangement — funders, institutes, and labs. They cannot confirm, contest, or receive anything; whether the allocation serves them is undecidable from any seat inside the present. Listed for completeness as the reading's declared victim class; they hold no seat and no voice except by proxy.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(ai_risk_prioritization__existential_risk_reading, future_humanity).

% Government bodies allocating scarce regulatory attention. Under the existential framing they build frontier-model evaluation regimes, compute-governance reporting, and safety-institute partnerships, while enforcement of anti-discrimination and labor law against deployed systems stays comparatively thin. They administer the prioritization the framing produces, with agendas set in consultation with the funded institutions; their capacity is fixed, so attention to one portfolio is attention withdrawn from the other.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_policy_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates fragmented research funding, talent, and regulatory attention on a single declared problem — making transformative AI safe — producing shared research agendas, evaluation standards, and a career structure where otherwise capability development would proceed with no dedicated safety effort.
% TRANSFER_FUNCTION: Moves money, talent, prestige, and regulatory attention from near-term harm mitigation (deployed-system discrimination, labor displacement, surveillance) toward long-horizon alignment research and the institutions that administer it; moves moral urgency from present-day victims to hypothetical future ones, with present funders and institutes as the conduit.
% ABSENT_VOICES: Future persons are absent without proxy: every statement of their interests comes from parties who collect from the arrangement. Near-term harm victims attend the conversation but as objects of discussion rather than agenda-setters. Global-majority communities bearing deployment harms are thin in the venues where priorities are set. Dissenting existential-risk researchers who downgrade the premise face social and funding costs for speaking.
% DISAPPEARANCE_RATIONALE: If the priority framing vanished overnight, the large dedicated funding pool would redistribute toward whatever the successor framing names — plausibly deployed-harm mitigation — field identity would fragment into applied-fairness, governance, and technical-safety camps, frontier labs would lose the legitimacy cover the framing provides, and the justice-research portfolio would regain the grant panels and press attention it currently cedes. Every named seat's situation would change materially.
% FOUNDING_PROBLEM: In the early 2010s, capability research was accelerating with essentially no funded effort to make transformative AI safe or steerable: no alignment field, no safety career path, no institutional home. The arrangement was built to create one, by declaring the problem paramount.
% FOUNDING_PROBLEM_CORROBORATION: Senior machine-learning researchers with no existential-risk funding and government AI-safety bodies attest that unsafe capability development outpacing safeguards was and remains a real problem — corroboration from outside the beneficiary set. The sibling reading's proponents and near-term justice researchers attest the problem is real but inflated relative to present harms, and that the arrangement now persists through its beneficiary structure. No fully disinterested corroborator exists: the problem's evidentiary basis is forecast-dependent, so every attester holds a stake in the weighting — that absence is itself signal.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects substantial diversion of money, talent, and regulatory attention toward one portfolio, with real research produced alongside the diversion — the arrangement is not empty, but its resource claims are decoupled from any present-day accounting of who bears the opportunity cost. Suppression (0.62) is discourse- and funding-level enforcement rather than state coercion: grant-gatekeeping, conference and press norms, and the 'distraction' framing that recasts rival work; the mechanism is both structural (funding gates) and internalized (priority framing absorbed by researchers), with the split carried by the suppression_mechanism_split omega. Theater (0.35) captures a real technical core diluted by a growing share of performative activity — safety-washing by labs, urgency signaling, pledge letters that bind nothing. Accessibility_collapse (0.45) is moderate: the rival reading persists and grows rather than collapsing under the dominant frame. Resistance (0.6) reflects organized contestation from justice advocates, affected communities, and internal dissent. The measurement series run on one shared time grid (all three metrics at every point 0–12): extractiveness and theater rise as longtermist funding consolidated and labs adopted the framing; suppression_requirement rises because the framing's enforcement machinery had to harden as the justice critique gained ground — enforcement intensification, not decay. Fixing is prohibitive for the seats that could fix it: the funders and field leadership would have to dissolve the identity investments, agenda control, and — by the premise's own lights — accept elevated risk, so the cost of fixing exceeds what those seats bear.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the funders' seat the arrangement is insurance they are paying for out of conviction, with near-perfect exit (capital moves) and a civilizational time horizon — near-pure coordination. From the trapped victims' seat the same structure is enforced diversion: their harms are measurable now and their remedy is always deferred behind the larger stakes. The workforce seat is captured by identity: the framing tells them they work on the most important problem in history, so exit reads as betrayal — if that identity frame broke, the field would reorganize around plural portfolios within a career-cycle. Inter-institutionally, funders and labs both hold institutional power but opposite exit profiles (arbitrage versus constrained regulators administering the frame downstream); labs are genuinely dual-positioned, collecting legitimacy while exposed to the controls they endorse. At the same nominal level, x-risk institutes and justice-research groups hold similar research-community standing, yet the former are identity-locked beneficiaries with agenda access and the latter are constrained outsiders whose work the frame demotes — power diverges through framing access, not global standing. Affected communities lack any coalition infrastructure inside agenda-setting venues, so their class position never converts to organized power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the beneficiary end of directionality: longtermist_funders lowest (they set the frame and their capital is mobile), x_risk_research_institutions low despite identity lock (they receive the transfer the frame justifies), the workforce low-moderate (careers flow in, but identity lock and agenda subordination pull toward capture). Frontier_ai_labs derive as beneficiaries on receipt but carry real secondary exposure to capability controls — their mixed position is declared structurally rather than overridden. Targets sit near the target end: near_term_ai_harm_victims highest (trapped, powerless, bearing both present harms and opportunity cost), near_term_harms_researchers high (constrained exit, shrinking resources). Future_humanity is authored agent:false and is excluded from the derivation by design: the arrangement's claimed ultimate stakeholder does not exist and cannot enter the arithmetic — every statement of their interests is issued by a seat that collects from the arrangement, which is the structural heart of the contest and the reason the future_persons_standing omega exists. Scope is global for the framing itself (amplifying verification difficulty for its resource claims) while the victims' exposure is national and concrete.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that capability research was accelerating with no funded effort to make transformative AI safe — has not died; it has transformed. Alignment is now among the best-funded corners of AI research, so the 2014-era scarcity the arrangement was built to cure no longer exists in its original form; what persists is the priority claim and the institutional apparatus that grew up administering it. The classification prevents mislabeling in both directions: reading the arrangement as pure coordination would erase the documented suppression of the justice portfolio and the self-dealing structure of future-persons representation; reading it as pure extraction would erase the genuine coordination value of focused alignment research and the undecidable insurance value the premise may still carry. The R5 mismatch check reads founding_problem_status (contested — the parties genuinely dispute whether the founding problem remains live) against disappearance_verdict (world_rearranges — seats exist and would rearrange): contested-times-rearranges is not the dead-problem zombie signature, but the contested status itself is signal — the arrangement's persistence now leans on its beneficiary structure as much as on its founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_premise_status,
    'Does misaligned AGI actually pose a credible extinction-level threat on decadal-to-century horizons, or is the premise undecidable at present?',
    'Adversarial forecasting tournaments with scoring rules and reputational stakes, capability-trend extrapolation audited by disinterested methodologists, and alignment-progress benchmarks that track whether safety techniques keep pace with capabilities.',
    'If the premise weakens, the arrangement''s coordination function collapses toward cover for resource capture and the beneficiary structure reads as self-dealing; if the premise strengthens, the coordination function dominates and measured extraction for the research seats falls toward the price of insurance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_premise_status, empirical, 'Truth status of the reading''s foundational empirical premise — the single largest driver of classification.').

omega_variable(
    future_persons_standing,
    'Can the interests of nonexistent future persons be legitimately represented by present institutions that themselves collect from the representation?',
    'Intergenerational-representation theory plus institutional tests: designs that give future interests independent standing (fiduciary duties, future-generations ombudspersons) and observation of whether allocations change when representation is structurally separated from collection.',
    'If representation is self-dealing by construction, the beneficiary structure is internally captured and effective extraction rises; if legitimate, part of the measured extraction is the unavoidable price of any arrangement that acts for absent parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_persons_standing, conceptual, 'Whether the arrangement''s claimed ultimate stakeholder can be represented without capture.').

omega_variable(
    suppression_mechanism_split,
    'Is the marginalization of near-term justice work maintained structurally (funding gates, venue norms, hiring signaling) or internalized (researchers who have absorbed the priority framing and self-censor even where gates are open)?',
    'Post-exit suppression trajectory: track justice researchers who move into existential-risk-funded roles and existential-risk researchers who leave the framing — if self-censorship and priority deference persist after the gate is removed, the internalized component is substantial.',
    'If internalized, effective suppression is higher than the structural measure suggests and will persist after any funding reform; if structural, rebalancing grant flows would rapidly restore the suppressed portfolio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized split of the arrangement''s suppressive force.').

omega_variable(
    safety_washing_fraction,
    'What fraction of frontier-lab endorsement of the existential framing is reputational cover for continued capability racing rather than operative belief that would bind behavior under cost?',
    'Revealed preference under cost: compliance with binding capability controls when offered, internal R&D allocation audits, and whistleblower records comparing stated priorities to spending.',
    'A high cover fraction raises theater_ratio above the authored 0.35 and makes labs parasitic beneficiaries of a framing they do not act on; a low fraction supports their genuine dual position as would-be payers of capability controls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_washing_fraction, empirical, 'Sincerity of lab adoption of the existential framing.').

omega_variable(
    reading_framing_underdetermination,
    'This constraint is one reading of the ai_risk_prioritization kernel; does the existential reading represent the only defensible framing, or does the near_term_harms_reading produce an equally coherent constraint with a different victim set, timescale, and beneficiary structure?',
    'The disagreement is located in two elements: the probability-magnitude weighting of catastrophic versus cumulative harm, and standing to represent future persons. Adversarial collaboration between the readings'' research communities, or decision-theoretic analysis under explicit moral uncertainty, would expose whether the weighting is evidence-driven or interest-driven.',
    'Adopting the sibling reading reclassifies the arrangement''s victims as present-day affected communities, redirects the beneficiary analysis toward justice institutions, moves the relevant timescale to the present, and likely raises measured suppression — the existential framing itself becomes the extraction mechanism rather than its justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Committer-frame underdetermination: which reading of the kernel the structural facts actually support.').

omega_variable(
    coordination_extraction_separability,
    'Is catastrophic-risk preparedness separable from the specific institutional arrangement that claims it — could alignment research be funded at scale without the priority framing that recasts near-term justice work as distraction?',
    'Natural experiment from pluralist funding regimes: government safety institutes and diversified funder coalitions that support both portfolios simultaneously — does existential-risk research output and quality persist where no single framing holds a monopoly?',
    'If separable, the suppression component is institutional rent rather than a cost of coordination, and the arrangement''s enforcement is pure overhead; if inseparable, part of the measured suppression is the price of focused effort on a problem requiring concentration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and suppression components of the arrangement are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_r_tr_t2, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2, 0.21).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__existential_risk_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_prioritization__existential_risk_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__existential_risk_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__existential_risk_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__existential_risk_reading, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_r_be_t2, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2, 0.47).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 12, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(ai_r_su_t2, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2, 0.48).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 4, 0.51).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 8, 0.57).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI risk' conflates two structurally distinct prioritization claims and is decomposed per the ε-invariance principle into a two-member constraint family. This story (existential_risk_reading) authors the long-horizon catastrophic-risk prioritization arrangement: victim set anchored in future humanity, beneficiary set of x-risk institutions and longtermist funders, timescale 10–100 years. The sibling (near_term_harms_reading) authors the deployed-harm prioritization arrangement: victim set of present-day affected communities, beneficiary set of justice institutions, timescale now. The upstream reading (higher current resource share, more institutional entrenchment) structurally influences the downstream sibling by changing its resource availability and legitimacy conditions — the 'distraction' framing is the influence mechanism — without logically foreclosing it, since both readings remain live positions. Each file carries its own ε, stakeholders, and classification; neither averages across the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
