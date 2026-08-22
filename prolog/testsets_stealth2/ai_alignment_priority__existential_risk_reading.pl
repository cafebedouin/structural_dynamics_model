% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: Existential-Safety-First Alignment Prioritization Regime
 *   domain: technology governance/ethics/risk assessment
 *
 * SUMMARY:
 *   This story instantiates the existential_risk_reading of the
 *   ai_alignment_priority kernel: the operative arrangement in which
 *   'alignment' is defined as preventing catastrophic loss of control over
 *   advanced AI systems, with existential safety holding priority over all
 *   other alignment goods. The standing arrangement under contest is the
 *   actual allocation of alignment resources, attention, and authority across
 *   the AI field — and epsilon is authored for THAT arrangement, assessed by
 *   this reading's own lights (catastrophe as the paramount harm), never for
 *   the fully realized alignment-first order the reading endorses. The regime
 *   solves a real collective-action problem (no single actor can secure
 *   loss-of-control prevention alone) while extracting substantially: it
 *   conscripts diffuse public resources against speculative future
 *   capabilities, concentrates administration in frontier labs and x-risk
 *   institutions, channels material flow through the very organizations whose
 *   scaling it is supposed to constrain, and queues present, certain harms
 *   behind speculative scenarios under an undifferentiated 'all of humanity'
 *   victim frame. Family note: the nearterm_harms_reading and
 *   integrated_reading are separate constraint files with their own epsilon
 *   values and victim structures; this file's epsilon is invariant to that
 *   contest — the label 'alignment priority' conflates structurally distinct
 *   claims, and the decomposition follows the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - frontier_ai_labs: Primary agenda-setter and net beneficiary (institutional/arbitrage) — administers the safety apparatus, collects the legitimacy and funding flows, bears compliance costs and tail exposure
 *   - xrisk_research_community: Beneficiary (organized/identity_locked) — staffs the safety institutions; careers, status, and moral identity fused with the framing
 *   - longtermist_philanthropic_funders: Agenda-setter (powerful/mobile) — directs grant portfolios that operationally define what counts as alignment research
 *   - ai_governance_bodies: Agenda-setter (institutional/constrained) — codifies the prioritization into regulatory tiers and safety-institute mandates
 *   - marginalized_ai_harm_communities: Primary payer (powerless/trapped) — bears present discriminatory and extractive harms subordinated by the prioritization
 *   - general_public: Payer and nominal protectee (moderate/trapped) — pays in taxes, policy bandwidth, and epistemic deference; receives uninsurable promised insurance
 *   - ai_ethics_researchers: Excluded voice (moderate/constrained) — studies present harms; registers in the regime's conversation as category error
 *   - interdisciplinary_governance_analysts: Analytical observer (analytical/analytical) — maps the flows without material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.72).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.6).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "Existential-Safety-First Alignment Prioritization Regime").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "technology governance/ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, 'f016ab84-d5b0-4794-b132-29fbd595308a').
narrative_ontology:cs_kernel_codification('f016ab84-d5b0-4794-b132-29fbd595308a', distributed).
narrative_ontology:cs_authority_grounding('f016ab84-d5b0-4794-b132-29fbd595308a', expertise).
narrative_ontology:cs_interpretation_layer_present('f016ab84-d5b0-4794-b132-29fbd595308a').
narrative_ontology:cs_reading_relation('f016ab84-d5b0-4794-b132-29fbd595308a', ai_alignment_priority__nearterm_harms_reading, forecloses).
narrative_ontology:cs_reading_relation('f016ab84-d5b0-4794-b132-29fbd595308a', ai_alignment_priority__integrated_reading, forecloses).
narrative_ontology:cs_axiom('f016ab84-d5b0-4794-b132-29fbd595308a', foundational, catastrophic_outcome_lexical_precedence).
narrative_ontology:cs_axiom_status(catastrophic_outcome_lexical_precedence, holdable).
narrative_ontology:cs_axiom_grounding('f016ab84-d5b0-4794-b132-29fbd595308a', catastrophic_outcome_lexical_precedence, empirically_contingent).
narrative_ontology:cs_axiom('f016ab84-d5b0-4794-b132-29fbd595308a', secondary, speculative_capability_forecasting_authority).
narrative_ontology:cs_axiom_status(speculative_capability_forecasting_authority, holdable).
narrative_ontology:cs_axiom_grounding('f016ab84-d5b0-4794-b132-29fbd595308a', speculative_capability_forecasting_authority, instrumental).
narrative_ontology:cs_reference_frame('f016ab84-d5b0-4794-b132-29fbd595308a', catastrophic_loss_of_control_lexical_primacy).
narrative_ontology:cs_drift_state('f016ab84-d5b0-4794-b132-29fbd595308a', post_frontier_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f016ab84-d5b0-4794-b132-29fbd595308a', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, xrisk_research_community).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, longtermist_philanthropic_funders).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, marginalized_ai_harm_communities).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, general_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, general_public).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, catastrophic_loss_of_control_scenario_planning).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, longtermist_moral_weighting).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, speculative_capability_forecasting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the large-scale training programs, host the safety teams, publish the evaluations, and adopt the responsible-scaling policies through which the prioritization operates day to day. They collect safety-directed funding and, more valuably, the legitimacy that safety activity confers on continued capability scaling; the regime's focus on their future products' catastrophic risks draws regulatory attention away from their present deployments' effects. They bear real compliance costs, evaluation overhead, and tail liability, and they cannot fully control the framing they benefit from — but the net resource flow runs strongly toward them, and their arbitrage position lets them restructure, relocate, or reshape regulation if the terms turn unfavorable.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, frontier_ai_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, frontier_ai_labs, beneficiary).

% Staff the safety institutes, interpretability teams, forecasting organizations, and red-teaming groups that constitute the regime's epistemic machinery. Funding, careers, status, and community standing all flow through the loss-of-control framing; most members hold the mission sincerely, but the material and relational dependence binds regardless of belief — leaving the frame means abandoning colleagues, funders, and a self-concept built around safeguarding the future. Exit looks like professional and moral self-erasure, which is precisely why the community's internal pluralism is thinner than its external critics assume.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, xrisk_research_community, beneficiary,
    organized, civilizational, identity_locked, global).

% Direct multi-billion-dollar grant portfolios according to the existential-safety prioritization, deciding which research programs, institutes, and career paths exist at all. Their funding decisions operationally define what counts as alignment work; conference slots, prize structures, and junior-researcher pipelines follow their allocations. Unlike the researchers they fund, they can redirect capital across framings comparatively quickly, giving them mobile exit — their commitment to the priority ordering is strategic and revisable in a way their grantees' is not.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, longtermist_philanthropic_funders, agenda_setter,
    powerful, generational, mobile, global).

% Translate the prioritization into regulatory architecture: systemic-risk tiers, safety-testing mandates, national AI safety institutes, and summit frameworks. They depend on the frontier labs' technical self-reporting to verify compliance, which couples their enforcement capacity to the parties they oversee. Jurisdictional competition constrains them — tightening terms risks displacing capability development to rival jurisdictions — so their exit from the regime's terms is constrained even as they formally administer it.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_governance_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Bear the present, certain harms of deployed systems — biased screening in housing, employment, credit, and medicine; automated fraud and surveillance; extractive data practices. Under the prioritization, their claims are queued behind speculative loss-of-control scenarios: their harms are classified as near-term, tractable-later, or category errors relative to the 'real' problem. They have no meaningful exit from algorithmically mediated services and no seat in the forums where the priority ordering is set; the regime's undifferentiated 'all of humanity' frame speaks for them without counting them.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, marginalized_ai_harm_communities, payer,
    powerless, immediate, trapped, national).

% Nominally the protected party — the regime claims to act for all of humanity — and structurally a diffuse payer: public funds, regulatory bandwidth, democratic attention, and epistemic deference to technical elites are all conscripted by the prioritization. They receive the promised insurance against catastrophe, a good whose probability they cannot independently assess and whose delivery they cannot verify. Exit is not available at any price: there is no opting out of civilizational risk arrangements, which is exactly what makes their deference conscripted rather than purchased.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, general_public, payer,
    moderate, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, general_public, beneficiary).

% Study bias, discrimination, labor displacement, and power concentration in deployed systems — the harm classes the prioritization subordinates. They find the 'alignment' conversation constituted around loss-of-control scenarios they were not consulted on; their contributions register in regime forums as distractions, category errors, or naive scope-setting. Funding and attention migrate toward x-risk work, squeezing their programs; they retain publication channels and disciplinary homes, so they are excluded from the regime's decision surface rather than silenced outright.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_ethics_researchers, excluded,
    moderate, biographical, constrained, global).

% Map the resource flows, framing contests, and institutional couplings across the alignment field from science-and-technology-studies, political economy, and governance scholarship. They hold no material stake in the priority ordering, which buys them the analytic distance to trace how safety legitimacy converts into scaling permission and how the undifferentiated victim frame distributes costs — a vantage none of the seated parties occupies.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, interdisciplinary_governance_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__existential_risk_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_alignment_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of preventing loss of control over systems no single actor can unilaterally secure: standardizes threat models, evaluation and red-teaming methodology, and governance attention around catastrophic scenarios, and makes 'safety' legible and fundable as a field with career paths, benchmarks, and institutions.
% TRANSFER_FUNCTION: Moves funding, talent, and policy attention from diffuse public sources and competing research programs toward frontier labs' safety teams and x-risk research institutions; moves moral urgency and epistemic deference from the general public to technical elites; moves present-harm remediation down every allocation queue the regime controls.
% ABSENT_VOICES: Communities bearing present-day algorithmic harms, AI-ethics researchers, disability and labor advocates would object that the priority ordering spends their present, certain interests on speculative scenarios — they are absent from the regime's decision forums, where the 'alignment' conversation is constituted without them and their claims arrive pre-classified as category errors.
% DISAPPEARANCE_RATIONALE: If the prioritization vanished overnight, the vacated space would be immediately contested: nearterm-harms framings and commercial incentives would compete for the funding portfolios, governance bodies would rebuild their tier architectures around different harm taxonomies, thousands of careers anchored to the loss-of-control frame would reorganize, and the safety-legitimacy channel through which labs convert safety work into scaling permission would close — the field's entire allocative structure rearranges around whichever framing captures the released resources.
% FOUNDING_PROBLEM: Advanced AI systems could escape human control, and no market mechanism prices civilizational catastrophe: the actors creating the risk do not internalize its cost, and no unilateral actor can secure the mitigation alone — the founding problem was to make this unpriced, uninternalizable exposure governable.
% FOUNDING_PROBLEM_CORROBORATION: The loss-of-control problem's liveness is attested from outside the benefiting parties: government-commissioned international scientific assessments on AI safety, academic machine-learning researchers with no x-risk institutional affiliation, and cross-national summit declarations all affirm that loss-of-control risk in increasingly capable systems is real and unresolved. Note the corroboration's limit: these sources attest the founding PROBLEM, not this reading's priority ORDERING — the sibling readings dispute the ordering, not the problem's existence, and no party outside the regime's beneficiary set attests that existential safety merits lexical priority over present harms.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the regime commands enormous present resources against capabilities that do not yet exist, distributes the costs undifferentiatedly, and routes the material flow through organizations that capture it as scaling legitimacy. Suppression is moderate-high (0.60): the regime maintains its priority ordering through funding gatekeeping, benchmark and evaluation authority, governance access, and framing discipline that renders present-harm claims category errors — but rival framings remain publishable and institutionally alive, so suppression is real without being total. Theater is moderate (0.40): red-teaming, evaluations, and safety cases perform real epistemic work, but a growing share functions as safety-washing — output calibrated to license continued scaling rather than to constrain it. Accessibility collapse is moderate-low (0.45): the nearterm and integrated framings remain accessible alternatives; the regime disadvantages rather than eliminates them. Resistance is substantial (0.60): the sibling-reading communities actively contest the priority ordering in publications, funding disputes, and policy processes. The measurement series run on one shared time grid (points 0,2,4,6,8,10,12) with every tracked metric authored at every point; late-interval points are marked projected. Trajectories are monotonic consolidation, not cyclical: rising extraction tracks the post-frontier funding influx, rising theater tracks the safety-washing incentive as scaling stakes grew, and rising suppression_requirement tracks the regime's hardening framing defense as contestation intensified.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the frontier-lab seat, the regime is stewardship it voluntarily performs under scrutiny — genuine coordination it funds and staffs, with compliance costs it absorbs. From the marginalized-harm-community seat, the same structure operates as enforced subordination: certain, present, differentiated harms queued behind speculative, undifferentiated ones by institutions those communities cannot access. From the general-public seat, the regime is simultaneously insurance it cannot price and deference it cannot verify. From the xrisk-researcher seat, exit is not merely costly but unthinkable — the frame constitutes the researcher's moral identity. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs, the xrisk research community, and longtermist funders are declared beneficiaries: the regime subsidizes them with legitimacy, funding, and agenda control, driving their derived directionality toward the beneficiary end — labs nearest it despite real compliance burdens, since the net flow (safety legitimacy convertible into scaling permission) runs strongly in their favor. Marginalized harm communities are declared victims with trapped exit: they bear the regime's subordination costs with no arbitrage path, sitting near the full-target end. The general public holds a genuinely dual position — declared protectee, actual diffuse payer — and derives near-symmetric directionality, which is itself diagnostic: the party the regime claims to serve is the party whose resources it conscripts. Governance bodies derive mildly target-side d (they expend authority enforcing an ordering whose verification depends on the regulated parties' self-reporting). No directionality overrides are authored: the beneficiary/victim declarations plus exit options already yield the correct per-seat relationships, and the schema's override surface is power-atom-keyed, too coarse to improve on the structural derivation here.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the regime as pure snare loses the genuine coordination half: catastrophic loss of control is a real collective-action problem, the regime standardizes threat models and evaluation methodology that would not otherwise exist, and its founding problem remains live — so the snare reading would erase the reason the regime exists at all. Reading it as pure rope loses the extraction half: the undifferentiated victim frame, the subordination of present harms, and the safety-legitimates-scaling resource loop are asymmetric transfers through the same structure that performs the coordination, sustained by active enforcement of the priority ordering. The founding problem is live (attested from outside the benefiting parties), so no mandatrophy declaration is authored and the piton reading is unavailable; the regime's persistence is maintained by enforcement and identity investment, not inertia alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_location,
    'This constraint is one reading (existential_risk_reading) of the ai_alignment_priority kernel; the sibling readings (nearterm_harms_reading, integrated_reading) instantiate different constraints from the same kernel — where exactly is the disagreement located, and what would adopting a sibling change structurally?',
    'Comparative analysis across the three reading files: the disagreement is located in the priority ordering over harm classes (speculative-catastrophic vs present-distributed) and in whether the ordering is lexical. A sibling adoption changes the victim set (differentiated present-harm bearers vs undifferentiated humanity), the beneficiary set, and epsilon''s distribution across seats.',
    'If the integrated reading''s complementarity premise were adopted, this constraint''s undifferentiated victim set decomposes into differentiated claimants, extraction redistributes toward present-harm remediation, and the tangled_rope profile shifts toward rope; if the nearterm reading were adopted, the speculative-capability extraction component drops out entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_location, conceptual, 'Committer structure: one reading of a contested kernel; disagreement located in the harm-class priority ordering.').

omega_variable(
    speculative_capability_epsilon_stability,
    'Epsilon is authored against speculative future capabilities: how stable is the loss-of-control risk estimate that licenses the regime''s present resource extraction?',
    'Convergence of independent forecasting efforts (evaluations, elicitation studies, international scientific assessments) on the probability and imminence of loss-of-control events in deployable systems.',
    'If loss-of-control risk is materially lower than the reading estimates, epsilon collapses toward zero and the regime computes as rent collection riding a phantom risk (snare-flavored); if materially higher, the extraction approaches the price of genuine insurance and the profile shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculative_capability_epsilon_stability, empirical, 'Whether the regime''s extraction tracks a real catastrophic exposure or a speculative one.').

omega_variable(
    undifferentiated_humanity_abstraction,
    'The victim set is declared as ''all of humanity,'' undifferentiated — do the regime''s costs actually fall uniformly, or do they fall differentially on identifiable present-harm-bearing populations while the abstraction conceals the distribution?',
    'Distributional audit of regime costs: who bears deferred harm remediation, whose claims are queued, whose epistemic deference is conscripted, versus who receives the safety-funded resource flows.',
    'If costs are differential, the undifferentiated-victim framing is itself part of the extraction mechanism (an abstraction that launders subordination), raising effective extraction for the differentiated payer seats; if costs are genuinely uniform, the abstraction is benign aggregation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(undifferentiated_humanity_abstraction, conceptual, 'Whether ''all of humanity'' victimhood is uniform fact or distributional concealment.').

omega_variable(
    capability_legitimacy_flow_direction,
    'Is the resource flow to capability-focused research a distortion the regime resists, or the regime''s actual stabilizing function — safety work supplying the legitimacy that permits continued scaling?',
    'Trace marginal funding and hiring decisions: does demonstrated safety progress ever slow capability programs, or does safety output consistently scale in proportion to capability ambition?',
    'If safety output functions as scaling license, the regime''s coordination function is partly cover and the theater component is load-bearing, pushing computed classification toward snare at the capturer seat; if safety work genuinely constrains capability, the coordination function is dominant and the profile shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_legitimacy_flow_direction, empirical, 'Direction of the safety-to-capability resource coupling.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative framings structural (funding gatekeeping, benchmark authority, governance access controlled by regime insiders) or internalized (identity fusion makes dissent feel like betrayal of the mission)?',
    'Post-exit trajectory tracking of researchers who leave x-risk institutions: if framing discipline persists after the structural gates are removed, the suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure — defectors carry the frame with them, and formal pluralism understates the regime''s grip; if structural, opening funding and publication channels would rapidly restore framing competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of rival alignment framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_align_xrisk_read_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(ai_align_xrisk_read_tr_t0, observed).
narrative_ontology:measurement(ai_align_xrisk_read_tr_t2, ai_alignment_priority__existential_risk_reading, theater_ratio, 2, 0.27).
narrative_ontology:measurement_basis(ai_align_xrisk_read_tr_t2, observed).
narrative_ontology:measurement(ai_align_xrisk_read_tr_t4, ai_alignment_priority__existential_risk_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement_basis(ai_align_xrisk_read_tr_t4, observed).
narrative_ontology:measurement(ai_align_xrisk_read_tr_t6, ai_alignment_priority__existential_risk_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement_basis(ai_align_xrisk_read_tr_t6, observed).
narrative_ontology:measurement(ai_align_xrisk_read_tr_t8, ai_alignment_priority__existential_risk_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(ai_align_xrisk_read_tr_t8, observed).
narrative_ontology:measurement(ai_align_xrisk_read_tr_t10, ai_alignment_priority__existential_risk_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(ai_align_xrisk_read_tr_t10, projected).
narrative_ontology:measurement(ai_align_xrisk_read_tr_t12, ai_alignment_priority__existential_risk_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(ai_align_xrisk_read_tr_t12, projected).

% Extraction over time
narrative_ontology:measurement(ai_align_xrisk_read_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(ai_align_xrisk_read_be_t0, observed).
narrative_ontology:measurement(ai_align_xrisk_read_be_t2, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement_basis(ai_align_xrisk_read_be_t2, observed).
narrative_ontology:measurement(ai_align_xrisk_read_be_t4, ai_alignment_priority__existential_risk_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement_basis(ai_align_xrisk_read_be_t4, observed).
narrative_ontology:measurement(ai_align_xrisk_read_be_t6, ai_alignment_priority__existential_risk_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement_basis(ai_align_xrisk_read_be_t6, observed).
narrative_ontology:measurement(ai_align_xrisk_read_be_t8, ai_alignment_priority__existential_risk_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement_basis(ai_align_xrisk_read_be_t8, observed).
narrative_ontology:measurement(ai_align_xrisk_read_be_t10, ai_alignment_priority__existential_risk_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement_basis(ai_align_xrisk_read_be_t10, projected).
narrative_ontology:measurement(ai_align_xrisk_read_be_t12, ai_alignment_priority__existential_risk_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement_basis(ai_align_xrisk_read_be_t12, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_align_xrisk_read_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(ai_align_xrisk_read_su_t0, observed).
narrative_ontology:measurement(ai_align_xrisk_read_su_t2, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2, 0.48).
narrative_ontology:measurement_basis(ai_align_xrisk_read_su_t2, observed).
narrative_ontology:measurement(ai_align_xrisk_read_su_t4, ai_alignment_priority__existential_risk_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement_basis(ai_align_xrisk_read_su_t4, observed).
narrative_ontology:measurement(ai_align_xrisk_read_su_t6, ai_alignment_priority__existential_risk_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement_basis(ai_align_xrisk_read_su_t6, observed).
narrative_ontology:measurement(ai_align_xrisk_read_su_t8, ai_alignment_priority__existential_risk_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement_basis(ai_align_xrisk_read_su_t8, observed).
narrative_ontology:measurement(ai_align_xrisk_read_su_t10, ai_alignment_priority__existential_risk_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(ai_align_xrisk_read_su_t10, projected).
narrative_ontology:measurement(ai_align_xrisk_read_su_t12, ai_alignment_priority__existential_risk_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(ai_align_xrisk_read_su_t12, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, integrated_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept 'AI alignment priority' decomposes into three structurally distinct constraints per the epsilon-invariance principle. This file (existential_risk_reading) carries high epsilon assessed against speculative future capabilities with an undifferentiated humanity victim set; nearterm_harms_reading carries its own epsilon over present-distributed harms with a differentiated victim set; integrated_reading carries a blended profile. The upstream/downstream coupling runs both ways: this reading's institutional dominance (funding volumes, governance tiers) changes the resource availability and legitimacy conditions under which the siblings operate, while sibling contestation drives this regime's rising suppression_requirement series. Each member links to the others via affects_constraints; no member hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
