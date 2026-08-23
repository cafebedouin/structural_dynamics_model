% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Existential-Risk Primacy Reading of AI Risk Prioritization
 *   domain: technology governance/risk assessment
 *
 * SUMMARY:
 *   Since the mid-2010s, a specific prioritization arrangement has
 *   consolidated across AI research funding, career formation, and policy
 *   discourse: AI risk is framed as primarily existential — misaligned AGI as
 *   a potential extinction-level event — and alignment research plus
 *   capability controls are treated as paramount, with near-term harms
 *   (algorithmic discrimination, surveillance, labor displacement)
 *   subordinated as lesser or distracting concerns. This story authors THAT
 *   arrangement as a single epsilon-invariant constraint: the standing
 *   arrangement under contest is the x-risk-governed allocation regime
 *   itself, and epsilon is assessed for it honestly, not for the pluralist
 *   alternative this reading opposes. KEY AGENTS (by structural
 *   relationship): longtermist_funders (agenda-setter, powerful/arbitrage)
 *   set the research agenda through grant-making; xrisk_research_institutions
 *   (primary beneficiary, organized/identity_locked) receive the funding,
 *   prestige, and advisory access; frontier_ai_labs (secondary beneficiary,
 *   institutional/arbitrage) convert the framing into legitimacy, talent, and
 *   regulatory moat while bearing partial capability-control exposure;
 *   near_term_harm_populations (primary target, powerless/trapped) bear
 *   deployed-system harms now; algorithmic_justice_researchers (secondary
 *   target, moderate/constrained) lose funding, venues, and standing;
 *   future_humanity (nominal principal, non-agent) is invoked as the mandate
 *   but acts only through proxies; civil_society_ai_watchdogs (excluded,
 *   organized/constrained) stand outside the conversation; policy_governments
 *   (observer, institutional/analytical) weigh framings and allocate
 *   regulatory bandwidth. The sibling reading (near_term_harms_reading) is a
 *   separate constraint file, linked via network.affects_constraints; the
 *   contest between readings is recorded in omega variables, not folded into
 *   this classification.
 *
 * KEY AGENTS:
 *   - longtermist_funders: agenda-setter (powerful/arbitrage) — sets the agenda through grant-making; capital redeployable across causes
 *   - xrisk_research_institutions: primary beneficiary (organized/identity_locked) — receives funding, prestige, advisory access; identity fused with mission
 *   - frontier_ai_labs: secondary beneficiary (institutional/arbitrage) — converts the framing into legitimacy, talent, and regulatory moat while bearing some capability-control exposure
 *   - near_term_harm_populations: primary target (powerless/trapped) — bears deployed-system harms now; claims lose the priority contest
 *   - algorithmic_justice_researchers: secondary target (moderate/constrained) — loses funding, venues, and standing; exit means abandoning accumulated expertise
 *   - future_humanity: nominal principal, non-agent (powerless/trapped) — invoked as mandate; acts only through proxies
 *   - civil_society_ai_watchdogs: excluded (organized/constrained) — would contest priority from outside the conversation
 *   - policy_governments: analytical observer (institutional/analytical) — weighs framings, allocates regulatory bandwidth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.66).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.64).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "Existential-Risk Primacy Reading of AI Risk Prioritization").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "technology governance/risk assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, '2604e899-6007-4255-9564-e83ab9141e23').
narrative_ontology:cs_kernel_codification('2604e899-6007-4255-9564-e83ab9141e23', distributed).
narrative_ontology:cs_authority_grounding('2604e899-6007-4255-9564-e83ab9141e23', lineage).
narrative_ontology:cs_interpretation_layer_present('2604e899-6007-4255-9564-e83ab9141e23').
narrative_ontology:cs_reading_relation('2604e899-6007-4255-9564-e83ab9141e23', ai_risk_prioritization__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('2604e899-6007-4255-9564-e83ab9141e23', foundational, extinction_dominates_risk_portfolio).
narrative_ontology:cs_axiom_status(extinction_dominates_risk_portfolio, holdable).
narrative_ontology:cs_axiom_grounding('2604e899-6007-4255-9564-e83ab9141e23', extinction_dominates_risk_portfolio, empirically_contingent).
narrative_ontology:cs_axiom('2604e899-6007-4255-9564-e83ab9141e23', secondary, paramount_priority_justifies_resource_concentration).
narrative_ontology:cs_axiom_status(paramount_priority_justifies_resource_concentration, holdable).
narrative_ontology:cs_axiom_grounding('2604e899-6007-4255-9564-e83ab9141e23', paramount_priority_justifies_resource_concentration, instrumental).
narrative_ontology:cs_reference_frame('2604e899-6007-4255-9564-e83ab9141e23', superintelligence_loss_of_control_canon).
narrative_ontology:cs_drift_state('2604e899-6007-4255-9564-e83ab9141e23', post_frontier_mainstreaming_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2604e899-6007-4255-9564-e83ab9141e23', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_harm_populations).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, algorithmic_justice_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, orthogonality_thesis).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, instrumental_convergence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Channel philanthropic capital in the hundreds of millions toward alignment research, x-risk institutes, forecasting projects, and governance work premised on the existential framing. Grant-making priorities and fellowship pipelines effectively set the field's research agenda. The capital is endowed and redeployable: if the cause lost salience, the money could move to another portfolio, which makes the framing's persistence partly a standing portfolio choice.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, agenda_setter,
    powerful, civilizational, arbitrage, global).

% Institutes, university groups, and nonprofit labs devoted to alignment, AI governance, and catastrophic-risk forecasting. They receive the bulk of dedicated AI-safety funding, publish much of the field's canonical work, and staff the advisory channels governments consult. Careers, reputations, and communal belonging are fused with the mission: internal criticism reads as betrayal, departure carries identity cost, and senior figures adjudicate what counts as credible work.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutions, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutions, agenda_setter).

% Adopt existential-risk language in safety frameworks, policy submissions, and recruiting while racing to scale capabilities. The framing supplies legitimacy ('responsible actors must be the ones to build safely'), raises smaller rivals' compliance costs, attracts safety-motivated talent, and defers restrictive regulation — while exposing the labs themselves to capability controls and evaluation regimes they lobby to shape. Gains flow to them indirectly through the legitimacy and moat effects rather than through grant receipts.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs, beneficiary,
    institutional, biographical, arbitrage, global).

% People subject to deployed systems now: algorithmic hiring filters, predictive policing, welfare-eligibility automation, tenant screening, gig-platform management, large-scale content moderation. They bear measurable discrimination, surveillance, and displacement on deployment timescales. They cannot opt out of systems embedded in housing, employment, credit, and policing, and their remediation claims compete for attention and funding against civilization-scale stakes — and lose the priority contest.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_harm_populations, payer,
    powerless, immediate, trapped, global).

% Fairness, accountability, transparency, and labor-impact researchers whose funding lines, venue acceptance rates, and press coverage shrink as 'AI safety' consolidates around x-risk. Their work is framed as parochial or distracted when it contests priority. Pivoting to alignment means abandoning accumulated expertise, methods, and communities; staying means accepting marginalization within their own field.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, algorithmic_justice_researchers, payer,
    moderate, biographical, constrained, global).

% The people of coming centuries whose interests the reading invokes as its mandate. They cannot consent, object, contract, or exit; they act in the present only through proxy institutions whose survival incentives need not coincide with theirs. If the prioritization is mistaken, they inherit both the neglected present harms (locked-in surveillance and inequity infrastructure) and the opportunity costs of misdirected prevention. Listed for completeness of the moral accounting; as a non-agent they do not enter the engine's extraction arithmetic.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_prioritization__existential_risk_reading, future_humanity).

% Advocacy organizations working on surveillance accountability, worker protections, and bias redress. They are largely outside the invitation structures of x-risk summits and safety-funder portfolios — different conferences, different funders, different vocabulary. Admitted to the conversation, they would argue for binding near-term deployment standards and a rebalanced safety portfolio; their exclusion is maintained by the same agenda-setting machinery that concentrates resources elsewhere.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, civil_society_ai_watchdogs, excluded,
    organized, biographical, constrained, national).

% National regulators, summit conveners, and risk-assessment bodies weighing which AI-risk framing to legislate around. They take testimony from both camps, commission analyses, and allocate regulatory bandwidth; adoption of the existential frame redirects their attention from sectoral deployment rules toward frontier-model licensing and evaluation regimes.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, policy_governments, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__existential_risk_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates scarce attention, talent, and precautionary investment on a long-horizon, diffuse, hard-to-verify hazard that market incentives and electoral cycles systematically underweight; coordinates a research community around shared threat models, evaluation practices, and escalation criteria before catastrophic capability arrives.
% TRANSFER_FUNCTION: Moves grant capital, elite labor, media attention, and regulatory bandwidth away from present-day algorithmic-harm remediation toward alignment research, capability evaluation, and x-risk institutions; also transfers moral authority and standing to actors claiming trusteeship over future generations.
% ABSENT_VOICES: Near-term harm populations have no seat in x-risk summits or funder strategy sessions; algorithmic justice researchers attend different conferences and read different canons; civil-society watchdogs are outside the invitation networks; future persons are absent by definition and 'represented' only by proxies with independent institutional interests. The unanimity of the x-risk conversation is partly an artifact of who was never in the room.
% DISAPPEARANCE_RATIONALE: If the existential-priority arrangement vanished overnight, funding flows, graduate pipelines, lab safety narratives, and summit agendas would reorganize within a few grant cycles around nearer-term risk portfolios; the justice-research apparatus currently suppressed as distraction would regain venue space, funders, and policy audience; lab legitimation strategies would rebuild around different framings. Nothing physical rearranges — the rearrangement is institutional and epistemic, which is precisely why the arrangement requires active enforcement to hold.
% FOUNDING_PROBLEM: Beginning in the early 2010s, a small research community argued that sufficiently advanced AI could escape human control with catastrophic or terminal consequences, and that markets, governments, and the broader ML field were structurally ignoring this because its costs and probabilities lie outside ordinary planning horizons. The arrangement was built to force that consideration onto the agenda and to build the research capacity to address it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the 2023 Center for AI Safety extinction-risk statement drew signatories with no longtermist funding ties; governmental bodies (the UK Frontier AI Taskforce, a US State Department-commissioned risk report) independently elevated loss-of-control risk; academic ML researchers unaffiliated with x-risk institutions have attested the hazard category. Critics among these same sources attest the hazard is real while disputing its primacy — corroborating the founding problem without endorsing this reading's monopoly on it.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.66: substantial but below pure-extraction levels, because the coordination function is real (long-horizon catastrophic risk is genuinely underweighted by markets and electoral cycles) and much funded work is technically serious. Suppression is 0.64 as a raw, unscaled structural property: the enforcement is epistemic and social — the distraction framing, funder gatekeeping, venue hierarchies, and community boundary-policing — not state coercion, but it actively degrades competitors' resource environment. Theater is 0.30: most activity is functional research, with a growing safety-washing share (lab safety language serving capability races and recruiting). Accessibility_collapse is 0.50: alternatives (near-term-focused portfolios, pluralist risk assessment) persist and are practiced, but are disadvantaged rather than eliminated — this is not a natural law. Resistance is 0.58: organized contestation from justice and labor communities, parts of academia, and accelerationist counter-pressure. The three measurement series run on one shared seven-point grid (2014–2025) and are monotonic rising; no cyclical dynamic is claimed, so no intermittent-reinforcement mechanism is asserted. Claim and metrics are independent authored facts: tangled_rope is claimed from structure (genuine coordination function PLUS asymmetric extraction PLUS active enforcement); the metrics describe observed operation without tuning toward any predicted engine verdict.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently. From the funder and institute seats, the arrangement is a corrective they built against collective myopia — prioritization, not predation. From the harm-population and justice-researcher seats, the same structure operates as enforced diversion: their claims are not refuted but out-staked, and the enforcement machinery (what counts as serious work, who gets invited, what is dismissed as distraction) is what holds their subordination in place. Frontier labs are genuinely dual-positioned: net beneficiaries of the framing's legitimacy and moat effects, yet partially exposed to the capability controls the same framing justifies. The future_humanity seat is deliberately authored as a non-agent: nonexistent persons cannot enter the chi arithmetic, so the moral accounting for the voiceless travels through the omega variables (proxy_representation_of_future_persons), not through the extraction ledger.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: xrisk_research_institutions, longtermist_funders, and frontier_ai_labs sit near the beneficiary end (low d, subsidized or damped chi); near_term_harm_populations (trapped, powerless) sit nearest the full-target end; algorithmic_justice_researchers (constrained exit, moderate power) sit high but below the trapped population. No directionality_overrides are authored: overrides key on the power atom, and the institutional atom contains both frontier_ai_labs and the observing policy_governments — nudging labs' d upward would contaminate the observer seat, so the labs' dual position is recorded qualitatively here and left to the structural derivation. Scope amplification applies modestly: the arrangement operates globally, raising verification costs for claims about counterfactual futures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — catastrophic AI risk ignored by market and political time horizons — remains live, so mandatrophy is NOT resolved and the flag is omitted. The tangled_rope classification prevents two opposite errors: dismissing x-risk work as pure rent-seeking (which would discard the genuine coordination half and the real hazard it addresses) and accepting the arrangement's self-description as pure service to future humanity (which would miss the asymmetric extraction, the suppressed justice agenda, and the safety-washing trend). The rising theater_ratio series is the early-warning instrument: if the founding problem were ever resolved or definitively dissolved, the same structure would persist by inertia and performance — the piton trajectory this analysis is positioned to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint is one reading of the ai_risk_prioritization kernel — what changes structurally if the near_term_harms_reading is instantiated instead?',
    'Comparative classification of the sibling story: victim set shifts to present-day harmed populations, the timescale compresses to deployment cycles, the beneficiary set shifts to fairness/accountability institutions, and the suppression direction reverses (speculative x-risk work framed as distraction from verifiable present harms).',
    'Classification, epsilon, and the stakeholder surface are reading-indexed; cross-reading comparison must join on kernel_id and must not treat either reading as the topic''s ground truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: this story instantiates the existential_risk_reading of the ai_risk_prioritization kernel.').

omega_variable(
    extinction_probability_calibration,
    'Is the probability-weighted magnitude of misaligned-AGI extinction risk large enough to justify the resource concentration this reading prescribes?',
    'Forecasting tournaments with resolution, mechanistic interpretability benchmarks, and empirical evidence on scaling discontinuities, adjudicated by analysts outside the funded x-risk community.',
    'If the calibrated probability sits far below the threshold the allocation implicitly assumes, epsilon rises sharply (extraction riding an overstated premise, snare drift); if it is high, a larger share of measured extraction is defensible insurance premium and the rope component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinction_probability_calibration, empirical, 'Whether the factual premise of the prioritization survives calibration.').

omega_variable(
    proxy_representation_of_future_persons,
    'Do longtermist proxy institutions actually represent the interests of future persons, or their own institutional reproduction?',
    'Revealed-preference audit of grant portfolios against stated longtermist commitments; divergence analysis between proxy behavior and robustly future-regarding allocations (e.g., biosecurity preparedness versus prestige research).',
    'If representation fails, future_humanity converts from invoked principal to pure victim, the beneficiary structure collapses toward the proxies alone, and the constraint drifts snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_representation_of_future_persons, conceptual, 'Whether the voiceless principal is served or ventriloquized.').

omega_variable(
    safety_washing_fraction,
    'What fraction of lab-declared alignment activity is capability-enabling, recruiting-oriented, or reputational rather than risk-reducing?',
    'Independent output audits: publication venues, team attrition into capability roles, and evaluation rigor assessed without lab-controlled disclosure.',
    'Raises theater_ratio and pushes the lab seat''s effective extraction upward; sustained high washing would reclassify the lab seat from incidental beneficiary toward primary capturer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_washing_fraction, empirical, 'Separating genuine alignment output from safety-washing.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the marginalization of near-term justice work enforced structurally (funder gatekeeping, venue hierarchies, summit invitation networks) or internalized (identity fusion producing self-censorship, justice researchers preemptively deprecating their own relevance)?',
    'Track dissent and cross-paradigm collaboration rates after researchers exit funded circles; compare publication and citation patterns before and after grant-cycle shifts.',
    'If substantially internalized, suppression persists after funding rebalances and fixing_cost understates repair; if structural, redirecting grants dissolves most suppression quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism attribution for the suppression metric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 2014, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2014, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement_basis(ai_r_tr_t2014, observed).
narrative_ontology:measurement(ai_r_tr_t2016, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2016, 0.12).
narrative_ontology:measurement_basis(ai_r_tr_t2016, observed).
narrative_ontology:measurement(ai_r_tr_t2018, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement_basis(ai_r_tr_t2018, observed).
narrative_ontology:measurement(ai_r_tr_t2020, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2020, 0.18).
narrative_ontology:measurement_basis(ai_r_tr_t2020, observed).
narrative_ontology:measurement(ai_r_tr_t2022, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2022, 0.22).
narrative_ontology:measurement_basis(ai_r_tr_t2022, observed).
narrative_ontology:measurement(ai_r_tr_t2023, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2023, 0.26).
narrative_ontology:measurement_basis(ai_r_tr_t2023, observed).
narrative_ontology:measurement(ai_r_tr_t2025, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2025, 0.3).
narrative_ontology:measurement_basis(ai_r_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2014, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2014, 0.35).
narrative_ontology:measurement_basis(ai_r_be_t2014, observed).
narrative_ontology:measurement(ai_r_be_t2016, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2016, 0.4).
narrative_ontology:measurement_basis(ai_r_be_t2016, observed).
narrative_ontology:measurement(ai_r_be_t2018, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement_basis(ai_r_be_t2018, observed).
narrative_ontology:measurement(ai_r_be_t2020, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement_basis(ai_r_be_t2020, observed).
narrative_ontology:measurement(ai_r_be_t2022, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2022, 0.58).
narrative_ontology:measurement_basis(ai_r_be_t2022, observed).
narrative_ontology:measurement(ai_r_be_t2023, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2023, 0.62).
narrative_ontology:measurement_basis(ai_r_be_t2023, observed).
narrative_ontology:measurement(ai_r_be_t2025, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement_basis(ai_r_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2014, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2014, 0.25).
narrative_ontology:measurement_basis(ai_r_su_t2014, observed).
narrative_ontology:measurement(ai_r_su_t2016, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2016, 0.32).
narrative_ontology:measurement_basis(ai_r_su_t2016, observed).
narrative_ontology:measurement(ai_r_su_t2018, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement_basis(ai_r_su_t2018, observed).
narrative_ontology:measurement(ai_r_su_t2020, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement_basis(ai_r_su_t2020, observed).
narrative_ontology:measurement(ai_r_su_t2022, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2022, 0.55).
narrative_ontology:measurement_basis(ai_r_su_t2022, observed).
narrative_ontology:measurement(ai_r_su_t2023, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2023, 0.6).
narrative_ontology:measurement_basis(ai_r_su_t2023, observed).
narrative_ontology:measurement(ai_r_su_t2025, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2025, 0.64).
narrative_ontology:measurement_basis(ai_r_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI risk prioritization' decomposes into two structurally distinct constraints (epsilon-invariance principle): the existential_risk_reading (this file — victim set includes future humanity, timescale 10-100 years, beneficiaries are x-risk institutions and longtermist funders, suppression falls on near-term justice work) and the near_term_harms_reading (separate file — victim set is present-day harmed populations, timescale is the deployment cycle, beneficiaries are fairness/accountability institutions, suppression falls on speculative long-horizon work). The two epsilon values differ because the arrangements differ, not because one observable is being read two ways. The upstream reading currently exerts structural pressure on the downstream one: the existential frame's institutional success changes the near-term reading's funding, venue, and legitimacy environment, which is why the reading relation is influences rather than mere coexistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
