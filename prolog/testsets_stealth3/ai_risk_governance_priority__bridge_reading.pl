% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__bridge_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: Unified AI-Risk Governance Mandate (Bridge Reading)
 *   domain: technological/political/ethical
 *
 * SUMMARY:
 *   A contested normative kernel — what AI risk governance must prioritize —
 *   is instantiated here through ONE reading only: the bridge reading, under
 *   which present harms and existential risks are non-mutually-exclusive,
 *   structurally entangled concerns that a unified framework must govern
 *   together. Per the committer-frame rules, the sibling readings
 *   (existential_risk_reading, near_term_harms_reading) are separate
 *   constraints with their own victim sets and their own epsilon, linked via
 *   network.affects_constraints and routed through omega variables — none of
 *   the contest is averaged into this classification. The constraint under
 *   classification is the standing unified-framework arrangement as it
 *   actually operates: broker institutions convene the integrated agenda,
 *   funders condition money on integration language, and both parent research
 *   communities trade framing autonomy for legitimacy. Its epsilon referent
 *   is that standing arrangement, assessed by the bridge reading's own
 *   lights: the reading endorses unity, and epsilon still measures how much
 *   the existing arrangement extracts while delivering it — never the ideal
 *   integrated governance the reading would build. Claim/metric independence
 *   is deliberate: claimed_type=tangled_rope asserts the structure the author
 *   believes true (a genuine coordination function carrying asymmetric broker
 *   capture), while the metric block records descriptively true operation;
 *   any divergence between the claim and a computed per-seat verdict is the
 *   corpus's signal, not an error to reconcile.
 *
 * KEY AGENTS:
 *   - - ai_governance_bridge_institutions: agenda-setting broker (institutional/arbitrage) — convenes the unified agenda, defines 'integrated' work, collects integration-designated funding and citation centrality
 *   - - marginalized_ai_harm_communities: primary present-day target (powerless/trapped) — bears deferred remediation and mediated representation
 *   - - future_generations: primary long-horizon target (powerless/trapped) — bears unpriced exposure with no correcting seat
 *   - - xrisk_research_community: dual-positioned constituency (organized/identity_locked) — gains legitimacy, pays autonomy and conditional-legitimacy costs
 *   - - near_term_ethics_research_community: dual-positioned constituency (organized/constrained) — gains attention share, pays timeline-negotiation costs
 *   - - dual_track_funders: secondary beneficiary (institutional/mobile) — buys a single legible portfolio; their conditionality sustains the norm
 *   - - national_ai_policy_bodies: institutional consumer/observer (institutional/analytical) — adopts dual-mandate charters, watches disputes from outside
 *   - - unaffiliated_frontline_auditors: excluded voice (moderate/constrained) — evaluates deployments directly, holds no seats in the drafting circuit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.56).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.48).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "Unified AI-Risk Governance Mandate (Bridge Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "technological/political/ethical").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, '1751e9b3-3466-4e0e-b870-22137642eeba').
narrative_ontology:cs_kernel_codification('1751e9b3-3466-4e0e-b870-22137642eeba', distributed).
narrative_ontology:cs_authority_grounding('1751e9b3-3466-4e0e-b870-22137642eeba', expertise).
narrative_ontology:cs_interpretation_layer_present('1751e9b3-3466-4e0e-b870-22137642eeba').
narrative_ontology:cs_reading_relation('1751e9b3-3466-4e0e-b870-22137642eeba', ai_risk_governance_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('1751e9b3-3466-4e0e-b870-22137642eeba', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('1751e9b3-3466-4e0e-b870-22137642eeba', foundational, risk_entanglement_non_separability).
narrative_ontology:cs_axiom_status(risk_entanglement_non_separability, holdable).
narrative_ontology:cs_axiom_grounding('1751e9b3-3466-4e0e-b870-22137642eeba', risk_entanglement_non_separability, empirically_contingent).
narrative_ontology:cs_axiom('1751e9b3-3466-4e0e-b870-22137642eeba', foundational, unified_framework_necessity).
narrative_ontology:cs_axiom_status(unified_framework_necessity, holdable).
narrative_ontology:cs_axiom_grounding('1751e9b3-3466-4e0e-b870-22137642eeba', unified_framework_necessity, instrumental).
narrative_ontology:cs_reference_frame('1751e9b3-3466-4e0e-b870-22137642eeba', entangled_dual_mandate_parity).
narrative_ontology:cs_drift_state('1751e9b3-3466-4e0e-b870-22137642eeba', contemporary_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1751e9b3-3466-4e0e-b870-22137642eeba', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, ai_governance_bridge_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, dual_track_funders).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, marginalized_ai_harm_communities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, xrisk_research_community).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, near_term_ethics_research_community).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, xrisk_research_community).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, near_term_ethics_research_community).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__bridge_reading, structural_entanglement_thesis).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__bridge_reading, dual_mandate_feasibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interdisciplinary centers, synthesis-journal editors, and advisory-board secretariats that convene joint workshops between the safety and ethics communities, define what counts as 'integrated' AI-risk work, and produce the framework documents policymakers cite. Integration-designated funding, citation centrality, and board seats flow to them; a small set of such nodes accounts for the large majority of cross-field links. If the unified-framing niche closed, their convening model and staff could pivot to adjacent governance fields.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ai_governance_bridge_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, ai_governance_bridge_institutions, beneficiary).

% Communities experiencing documented algorithmic discrimination, surveillance, and labor displacement now. Their harms enter governance agendas chiefly when broker institutions translate them into integrated research themes; panel seats are few and travel-dependent, and their direct-organizing alternative competes with professionally mediated representation for the same finite attention. Remediation timelines are negotiated inside framework processes they do not control.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, marginalized_ai_harm_communities, payer,
    powerless, immediate, trapped, global).

% People not yet born who inherit whatever risk profile today's governance settles on. They appear in the unified framework only through proxy advocates and broker-authored scenario documents, with no channel to correct misrepresentation. How resources split between present-harm remediation and tail-risk prevention determines their exposure, and they cast no vote in that allocation; their only lever is the fidelity of the proxies speaking for them.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Researchers modeling catastrophic-tail-risk scenarios. The unified framework gives their work mainstream policy standing and access to broader funding pools that dismissed tail-risk talk a decade ago. The price is conditional legitimacy: grant applications and leading venues increasingly require documented near-term relevance, and retreating to pure longtermist channels means abandoning the institutional homes their careers were built inside.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, xrisk_research_community, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, xrisk_research_community, payer).

% Fairness, accountability, and labor researchers documenting present harms. Integration carried their findings into safety-institute mandates and enlarged total governance attention. In exchange they justify their work against long-horizon scenario portfolios, share agenda space with tail-risk framing, and accept remediation schedules negotiated inside cumulative-risk narratives; specialized venues remain but carry less policy weight.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_ethics_research_community, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, near_term_ethics_research_community, payer).

% Philanthropic foundations and public research programs funding AI governance. The unified framework hands them a single legible portfolio spanning both risk classes, cutting due-diligence overhead and letting one theory-of-change cover two claimant communities. They rebalance or withdraw on a budget cycle, and their funding conditionality is a principal lever keeping the integration norm alive.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, dual_track_funders, beneficiary,
    institutional, generational, mobile, global).

% Safety institutes and regulatory agencies that consume the framework: they adopt dual-mandate charters, commission assessments, and decide which recommendations reach legislation. They watch the research ecosystem's integration disputes from outside it and can reshape incentives through procurement and mandate design without belonging to either camp.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, national_ai_policy_bodies, observer,
    institutional, generational, analytical, continental).

% Independent auditors, affected-worker organizers, and Global South civil-society technologists who evaluate deployed systems firsthand but hold no seats in the invitation-based convening circuit where the unified agenda is drafted. They would press for weighting incident documentation over scenario modeling; their main entry routes run through the very broker institutions they would be critiquing.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, unaffiliated_frontline_auditors, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__bridge_reading, ai_governance_bridge_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__bridge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: AI's present harms and tail risks share causal drivers (compute scale-up, deployment decisions, evaluation gaps) and compete for the same policy bandwidth and research talent. A unified framework lets regulators write one assessment regime instead of two conflicting ones, lets funders see one portfolio, and blocks each camp from citing the other's agenda as grounds for deferring its own obligations.
% TRANSFER_FUNCTION: Moves agenda-setting authority, convening revenue, and integration-designated funding from the two parent research communities toward broker institutions; moves legitimacy reciprocally, each camp gaining mainstream standing through inclusion; and moves deferred remediation costs onto present harmed communities and unpriced exposure onto future generations wherever integration negotiation stalls.
% ABSENT_VOICES: Affected communities participate mostly through professional mediation and rarely hold seats in the invitation-based convenings where the unified agenda is drafted. Future generations are present only as proxy scenarios. Frontline auditors and Global South civil society stand outside the circuit and would contest the weighting of scenario modeling over incident documentation; their absence is what lets unanimity in framework documents read as consensus.
% DISAPPEARANCE_RATIONALE: Overnight removal would send the two research communities back to separate tracks within a funding cycle: grant programs would bifurcate, safety institutes would drop dual-mandate charters, and the genuinely entangled items (compute thresholds, deployment gating, evaluation standards) would fall into coordination gaps between parallel regimes. Broker institutions would lose their convening niche, and the camp-versus-camp polemics that predate the bridge would resume.
% FOUNDING_PROBLEM: The false-dichotomy standoff of the mid-2010s: safety labs treating fairness work as a distraction from catastrophic tail risk, and accountability researchers treating long-term speculation as evasion of documented discrimination, with funders and policymakers forced to choose sides and each camp using the other's existence to defer its own obligations.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting set: national AI-strategy documents and safety-institute charters explicitly repudiate the dichotomy; survey literature in the field documents persistent camp separation; and the two sibling camps' own polemics attest the divide is real even as each rejects the bridge solution as inadequate.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.56) rather than high because the unified framework delivers real goods — measurable mitigation transfer between risk classes, reciprocal legitimacy, single-portfolio legibility — alongside the capture; the extraction sits in agenda-control premiums, voice-filtering, and structural fragility rather than naked rent-taking. Suppression (0.48) is real but soft: no state coercion, rather funding conditionality, review criteria, and invitation politics that raise the cost of single-tradition work; alternatives persist (specialized venues, independent organizations), hence accessibility_collapse 0.50 and resistance 0.52 — both camps push back at the margins rather than capitulating. Theater_ratio 0.42 reflects the growing share of 'integration' activity that is citation exchange, framework documents, and workshop communiques with no practice change, counterweighted by genuine technical crossover. Suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by the engine, through directionality and scope. The temporal series trace one decade (T=0 approx 2016 through T=10 approx 2026) on a SINGLE shared grid: every tracked metric is authored at every examined time point, so no end-state value is silently substituted backward. suppression_requirement is tracked because this story's dynamic IS enforcement-machinery buildup (grant criteria, institute charters, portfolio conditionality), not mere extraction drift. Attention cycles oscillate with incidents — bias scandals surge harms funding, capability jumps surge safety funding — but the underlying consolidation trend across the decade is monotone, so the series models the trend rather than the cycle; the cycle is noted here rather than modeled as noise.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge structurally, and the engine computes that divergence from the structural data. From the broker seat, the unified framework is a built achievement and a livelihood — coordination they personally enable, experienced as low-extraction. From the trapped payer seats it reads as delay: remediation negotiated inside frameworks tuned to long-horizon portfolios, representation filtered through professionals who answer to funders. The dual-role researcher seats experience conditional legitimacy — membership priced in framing concessions — which neither a pure-benefit nor a pure-cost summary captures. Funders experience the constraint as diligence relief. The authored claim adjudicates none of these; the per-seat computation is supposed to disagree.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map cleanly onto the structural relationships. Bridge institutions are declared beneficiaries with arbitrage-grade exit — derivation places them near the subsidy end (low d; effective extraction damped or inverted), which matches their position: the arrangement subsidizes them and they can leave if the niche closes. Funders, also beneficiary-declared and mobile, derive similarly low. Communities and future generations are declared victims with trapped exit — derivation places them near the full-target end (high d, amplified chi), and future generations maximize target-side amplification: zero exit, zero voice, unlimited horizon of exposure. The researcher communities carry dual declarations (beneficiary + victim) with identity-locked and constrained exit respectively, landing mid-range — conditional legitimacy is precisely a mid-directionality position. No directionality_overrides are used: every seat's derived position matches its declared structural relationship, and the two same-power (organized) researcher seats are symmetric enough that a power-atom-keyed override would add nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the false-dichotomy standoff — is still live, corroborated by strategy documents, survey literature, and both camps' own polemics from outside the beneficiary set, so the arrangement has not outlived its function and mandatrophy_resolved is deliberately left undeclared. The classification guards against both mislabelings: a pure-rope reading would erase the asymmetric capture (agenda premiums concentrate in a handful of brokers while payers hold no correcting seat), and a pure-snare reading would erase the genuine coordination value (measurable mitigation transfer, reciprocal legitimacy) and license destructive demolition during a period when governance gaps are costly. The rising theater series is the early-warning channel: if integration rhetoric keeps decoupling from practice change while the schism heals through other channels, the arrangement drifts toward ceremonial maintenance — at which point the mismatch between founding_problem_status=live and continued theatrical operation becomes the diagnostic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'This constraint instantiates only the bridge reading of the ai_risk_governance_priority kernel; how would the classification change if instantiated through existential_risk_reading or near_term_harms_reading?',
    'Read the sibling stories directly and compare structural deltas: victim-set composition (future humanity alone, present marginalized populations alone, or both), priority ordering (exclusive primacy vs parity), and framework architecture (unified vs sequential). Recompute epsilon and per-seat classifications per reading; never average across readings.',
    'The existential reading narrows victims to future humanity and likely raises measured extraction against present communities via opportunity cost; the harms reading narrows victims to present populations and leaves future exposure unbudgeted. Either substitution changes the beneficiary/victim structure, the derived directionalities, and therefore the computed type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer-frame positioning: one reading of a three-reading kernel; sibling readings are distinct constraints, not measurement settings of this one.').

omega_variable(
    broker_concentration_fragility,
    'Is integrated AI-governance coordination structurally dependent on a handful of broker institutions (hub-and-spoke, fragile), or diffusing into distributed collaboration?',
    'Longitudinal co-authorship and co-citation network analysis: track whether cross-field links remain concentrated in the top few percent of bridging nodes, and whether new bridge formation occurs outside incumbent venues.',
    'Persistent concentration confirms gatekeeping extraction layered on genuine coordination (supports the tangled_rope reading and elevates the broker seat''s capture); demonstrated diffusion would decay the excess extraction toward ordinary coordination cost and support reclassification pressure toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broker_concentration_fragility, empirical, 'Whether the coordination function is robustly distributed or hostage to a small broker set.').

omega_variable(
    entanglement_empirical_basis,
    'Is the claimed structural entanglement of present harms and existential risks an empirical fact (shared causal drivers, transferable mitigations) or partly a rhetorical coalition device?',
    'Technical audit of mitigation-transfer studies: quantify how much compute governance, evaluation infrastructure, and deployment gating designed for one risk class measurably reduces the other; compare against cases where the coupling failed.',
    'Strong transfer evidence validates the coordination half of the arrangement and lowers the extraction attributable to bundling; weak transfer exposes the unified framing as agenda-extraction riding on two genuine concerns and pushes effective extraction up on both payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entanglement_empirical_basis, empirical, 'Empirical status of the entanglement thesis this reading''s foundational axiom rests on.').

omega_variable(
    proxy_representation_fidelity,
    'Do bridging institutions represent the interests of affected communities and future generations faithfully, or does mediation itself constitute extraction (voice-filtering toward funder-compatible framings)?',
    'Participatory audit comparing stated preferences elicited directly from affected communities (surveys, participatory methods) with how brokers operationalize those preferences in framework documents and funding calls.',
    'High distortion raises effective extraction on both payer seats and creates reclassification pressure toward snare; high fidelity supports the coordination component and stabilizes the tangled_rope verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_representation_fidelity, empirical, 'Fidelity of broker-mediated proxy representation for the two payer constituencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(ai_r_tr_t0, observed).
narrative_ontology:measurement(ai_r_tr_t2, ai_risk_governance_priority__bridge_reading, theater_ratio, 2, 0.31).
narrative_ontology:measurement_basis(ai_r_tr_t2, observed).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__bridge_reading, theater_ratio, 4, 0.34).
narrative_ontology:measurement_basis(ai_r_tr_t4, observed).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_governance_priority__bridge_reading, theater_ratio, 6, 0.37).
narrative_ontology:measurement_basis(ai_r_tr_t6, observed).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__bridge_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement_basis(ai_r_tr_t8, observed).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__bridge_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(ai_r_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(ai_r_be_t0, observed).
narrative_ontology:measurement(ai_r_be_t2, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2, 0.44).
narrative_ontology:measurement_basis(ai_r_be_t2, observed).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__bridge_reading, base_extractiveness, 4, 0.47).
narrative_ontology:measurement_basis(ai_r_be_t4, observed).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_governance_priority__bridge_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement_basis(ai_r_be_t6, observed).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__bridge_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(ai_r_be_t8, observed).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__bridge_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(ai_r_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.26).
narrative_ontology:measurement_basis(ai_r_su_t0, observed).
narrative_ontology:measurement(ai_r_su_t2, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2, 0.3).
narrative_ontology:measurement_basis(ai_r_su_t2, observed).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__bridge_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement_basis(ai_r_su_t4, observed).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_governance_priority__bridge_reading, suppression_requirement, 6, 0.39).
narrative_ontology:measurement_basis(ai_r_su_t6, observed).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__bridge_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement_basis(ai_r_su_t8, observed).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__bridge_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(ai_r_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI risk governance priorities' decomposes, per the epsilon-invariance principle, into three readings of one kernel — existential_risk_reading (exclusive tail-risk primacy), near_term_harms_reading (exclusive documented-harm primacy), and this bridge reading (parity under unified frameworks). Their epsilon values differ because their victim sets and priority orderings differ, not because any observable choice varies: forcing one story to carry all three would make epsilon observer-relative and violate DP-001. The sibling poles are upstream of the bridge historically (the bridge reading emerged from their collision and cites both literatures as evidence that integration is feasible), while this reading is structurally downstream-institutional: its adoption in charters and funding criteria reshapes the operating environment of both siblings without foreclosing either. Each family member links the others here; orphan stories would break contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
