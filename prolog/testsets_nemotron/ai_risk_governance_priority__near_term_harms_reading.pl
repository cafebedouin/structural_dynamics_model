% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: AI Risk Governance Priority: Near-Term Harms Reading
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint story instantiates the 'near_term_harms_reading' of the
 *   contested kernel 'ai_risk_governance_priority'. The kernel is the
 *   normative claim that AI risk governance must prioritize some category of
 *   risk; the reading specifies that priority as demonstrated present harms
 *   (bias, misinformation, labor displacement, surveillance) affecting
 *   marginalized populations now. The constraint operates as a structural
 *   arrangement: regulatory attention, research funding, and policy
 *   frameworks are directed toward existential risk scenarios
 *   (superintelligence, alignment, takeover) while present deployment harms —
 *   disproportionately borne by Global South populations, marginalized
 *   groups, displaced workers, and surveilled communities — receive
 *   proportionally less resource allocation and enforcement capacity. The
 *   coordination function is real: the x-risk framing coordinates global
 *   research agendas and attracts institutional capital. The extraction is
 *   asymmetric: technology companies and frontier model developers benefit
 *   from regulatory diversion (x-risk governance targets future hypothetical
 *   systems, not current deployed products), while present-harm victims bear
 *   the material costs of unregulated deployment. Active enforcement sustains
 *   the arrangement: funding structures, hiring pipelines, conference
 *   agendas, and legislative testimonies all reinforce the priority framing.
 *   This reading does not deny existential risk's possibility; it contests
 *   the *priority* assignment that treats speculative futures as structurally
 *   prior to demonstrated present harms.
 *
 * KEY AGENTS:
 *   - technology_companies: Primary beneficiary (institutional/arbitrage) — x-risk framing diverts regulatory attention from current product harms
 *   - global_south_populations: Primary target (powerless/trapped) — bear algorithmic discrimination, labor displacement, surveillance with minimal recourse
 *   - marginalized_groups_facing_algorithmic_discrimination: Primary target (powerless/identity_locked) — credit scoring, hiring, policing, healthcare algorithms encode historical bias
 *   - workers_displaced_by_automation: Primary target (moderate/constrained) — labor market disruption without transition support
 *   - communities_under_algorithmic_surveillance: Primary target (powerless/trapped) — predictive policing, border control, welfare fraud detection target vulnerable groups
 *   - ai_safety_institutes: Secondary beneficiary (institutional/arbitrage) — capture research funding and talent pipelines under x-risk mandate
 *   - longtermist_philanthropies: Secondary beneficiary (powerful/arbitrage) — direct capital to x-risk research, shaping field incentives
 *   - frontier_model_developers: Beneficiary (powerful/arbitrage) — regulatory focus on future models avoids scrutiny of current deployments
 *   - civil_society_organizations: Excluded (organized/constrained) — advocate for present-harm regulation but locked out of x-risk governance venues
 *   - competition_authorities: Observer (institutional/analytical) — investigate market concentration but lack AI-specific harm mandate
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure of priority inversion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.78).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.65).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance Priority: Near-Term Harms Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '91990e73-be6d-4782-901e-075fdba5c6e8').
narrative_ontology:cs_kernel_codification('91990e73-be6d-4782-901e-075fdba5c6e8', distributed).
narrative_ontology:cs_authority_grounding('91990e73-be6d-4782-901e-075fdba5c6e8', distributed).
narrative_ontology:cs_reading_relation('91990e73-be6d-4782-901e-075fdba5c6e8', ai_risk_governance_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('91990e73-be6d-4782-901e-075fdba5c6e8', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('91990e73-be6d-4782-901e-075fdba5c6e8', foundational, present_harm_primacy).
narrative_ontology:cs_axiom_status(present_harm_primacy, holdable).
narrative_ontology:cs_axiom_grounding('91990e73-be6d-4782-901e-075fdba5c6e8', present_harm_primacy, deontological).
narrative_ontology:cs_axiom('91990e73-be6d-4782-901e-075fdba5c6e8', foundational, proportional_resource_allocation).
narrative_ontology:cs_axiom_status(proportional_resource_allocation, holdable).
narrative_ontology:cs_axiom_grounding('91990e73-be6d-4782-901e-075fdba5c6e8', proportional_resource_allocation, instrumental).
narrative_ontology:cs_reference_frame('91990e73-be6d-4782-901e-075fdba5c6e8', demonstrated_harm_governance_priority).
narrative_ontology:cs_drift_state('91990e73-be6d-4782-901e-075fdba5c6e8', post_genai_deployment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('91990e73-be6d-4782-901e-075fdba5c6e8', '2026-08-03T14:22:17Z').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, ai_safety_institutes).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, longtermist_philanthropies).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, frontier_model_developers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_groups_facing_algorithmic_discrimination).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, workers_displaced_by_automation).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, communities_under_algorithmic_surveillance).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__near_term_harms_reading, algorithmic_fairness_doctrine).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__near_term_harms_reading, proportional_regulation_principle).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__near_term_harms_reading, present_harm_primacy_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major technology platforms and AI developers benefit from x-risk governance priority because it directs regulatory scrutiny toward hypothetical future systems rather than current deployed products. They fund x-risk institutes, participate in voluntary safety commitments, and shape governance agendas through lobbying and personnel rotation. Their exit options are maximal: they can comply, relocate, or litigate; the constraint subsidizes their position.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, technology_companies, beneficiary,
    institutional, generational, arbitrage, global).

% Newly established government and philanthropic AI safety institutes (UK AISI, US AISI, international network) capture research funding, talent pipelines, and policy influence under the x-risk mandate. They coordinate global safety research but their charter and funding prioritize alignment, interpretability, and evals for frontier models over fairness audits for deployed systems. Their position is subsidized by the priority arrangement.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_safety_institutes, beneficiary,
    institutional, generational, arbitrage, global).

% Philanthropic actors (Open Philanthropy, FTX Future Fund legacy, Longview Philanthropy) direct hundreds of millions to x-risk research, fellowships, and field-building. They shape the AI safety talent pipeline and epistemic norms. Their capital gives them arbitrage-grade exit: they can redirect funding if priorities shift. They benefit from the priority arrangement by seeing their normative framework instantiated in governance.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, longtermist_philanthropies, beneficiary,
    powerful, civilizational, arbitrage, global).

% Companies building the most capable models (OpenAI, Anthropic, Google DeepMind, Meta, xAI) benefit from governance frameworks that target 'future' systems — their current products face less scrutiny. They participate in voluntary commitments and safety testing while deploying systems with documented bias, hallucination, and labor displacement effects. Their market power gives them arbitrage exit.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, frontier_model_developers, beneficiary,
    powerful, biographical, arbitrage, global).

% Populations in the Global South bear disproportionate harms from AI deployment: biased credit scoring excluding informal economies, healthcare algorithms trained on Western data, content moderation erasing non-English languages, surveillance exported from wealthy nations. They have no voice in x-risk governance venues, no exit from algorithmic systems governing credit, health, borders, and welfare. The constraint extracts their wellbeing to subsidize a research agenda that does not address their harms.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, generational, trapped, global).

% Racial, gender, disability, and other marginalized groups face compounding algorithmic discrimination in hiring, lending, policing, healthcare, housing, and education. Their identity is locked into the systems that classify them: credit scores, risk assessments, benefit determinations. Exit means opting out of modern life (no credit, no healthcare access, no digital identity). The constraint's priority inversion means bias mitigation research is underfunded while alignment theory attracts talent.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_groups_facing_algorithmic_discrimination, payer,
    powerless, biographical, identity_locked, global).

% Workers in transportation, logistics, customer service, coding, translation, and creative fields face displacement by AI systems deployed without transition support, retraining guarantees, or wage insurance. Their exit is constrained: they can reskill (costly, uncertain), accept precarious work, or organize (facing platform power). The x-risk priority diverts policy attention from labor transition frameworks to compute governance and model evals.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, workers_displaced_by_automation, payer,
    moderate, biographical, constrained, global).

% Communities targeted by predictive policing, border surveillance, welfare fraud detection, and workplace monitoring bear the costs of AI-enabled social control. These systems disproportionately deploy in low-income, minority, and migrant neighborhoods. Exit is trapped: one cannot opt out of policing, borders, or welfare systems. The constraint extracts their liberty and dignity to fund a governance agenda focused on hypothetical superintelligence.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, communities_under_algorithmic_surveillance, payer,
    powerless, biographical, trapped, national).

% Digital rights groups, algorithmic justice organizations, labor unions, and Global South advocacy networks advocate for present-harm regulation (algorithmic accountability acts, bias audit mandates, worker protections, surveillance bans). They are structurally excluded from x-risk governance venues: not invited to summits, not funded by longtermist philanthropies, not hired by safety institutes. Their exclusion is what the priority arrangement enforces.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, civil_society_organizations, excluded,
    organized, biographical, constrained, global).

% Competition and consumer protection authorities (FTC, EC, CMA, etc.) investigate AI market concentration, data monopolies, and unfair practices. They lack a specific mandate for algorithmic harm to marginalized populations and operate within existing legal frameworks. They observe the priority inversion but lack the tools to rebalance it; their analytical seat sees the structure without direct stake.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% The analytical seat that sees the full structure: a priority arrangement that coordinates genuine AI safety research while extracting governance capacity from present-harm mitigation. This seat computes the per-seat divergence: beneficiaries experience coordination; payers experience extraction; excluded experience suppression. The engine computes this divergence from the structural data authored here.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global AI safety research: aligns research agendas, creates shared evaluation standards, builds international dialogue channels, concentrates talent on technical alignment problems. This coordination has real value — without it, safety research would be more fragmented and less cumulative.
% TRANSFER_FUNCTION: Moves regulatory attention, research funding, legislative bandwidth, enforcement capacity, and talent pipelines from present-harm mitigation (bias audits, discrimination law, labor transition, surveillance oversight) to existential risk preparation (alignment theory, interpretability, evals, governance of future systems). The transfer is from marginalized populations (who lose protections) to technology companies and x-risk institutes (who gain regulatory avoidance and funding).
% ABSENT_VOICES: Global South governments, algorithmic justice advocates, displaced workers' unions, surveilled communities' organizers, racial justice organizations, disability rights groups — all would object to the priority inversion but are excluded from the venues where AI risk governance priorities are set (Bletchley Summit, Seoul Summit, AI Safety Institute networks, longtermist fellowship programs). Their absence is structural: the venues' admission criteria (technical alignment background, longtermist epistemic norms) filter them out.
% DISAPPEARANCE_RATIONALE: If the x-risk priority constraint vanished overnight, regulatory attention would immediately shift to algorithmic accountability legislation (EU AI Act enforcement, US Algorithmic Accountability Act, Global South regulatory initiatives), funding would rebalance toward fairness audits and bias mitigation, labor transition frameworks would gain legislative traction, surveillance oversight would intensify. The mobile software economy of AI governance would reorganize around present harms.
% FOUNDING_PROBLEM: The arrangement was built to solve: 'How do we coordinate global research to prevent AI systems from causing catastrophic or existential harm to humanity?' This problem emerged from early AI safety arguments (Yudkowsky, Bostrom) and was institutionalized through philanthropic field-building (2015-present) and government adoption (2023-present).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (catastrophic risk from advanced AI) is attested as live by AI safety institutes, longtermist philanthropies, and frontier developers. It is attested as dead or substantially solved by algorithmic justice researchers (Buolamwini, Noble, Gebru), labor economists (Acemoglu, Autor), Global South scholars (Benjamin, Mohamed, Dignum), and UN human rights mechanisms — who document that present harms are the demonstrated catastrophe while existential risk remains speculative. The corroboration split maps exactly to the beneficiary/victim divide.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the priority arrangement structurally diverts finite governance capacity (regulatory bandwidth, research funding, legislative attention, enforcement resources) from demonstrated harms to speculative scenarios. The beneficiaries (technology companies, frontier developers, x-risk institutes) collect concentrated benefits: regulatory avoidance, funding capture, agenda control. The victims (Global South populations, marginalized groups, displaced workers, surveilled communities) bear diffuse but severe costs: discrimination, job loss, surveillance, exclusion — with no proportional representation in governance venues. Suppression (0.65) operates through venue control: x-risk governance forums (summits, institutes, fellowships) set admission criteria that exclude present-harm advocates; funding calls prioritize alignment theory over fairness auditing; legislative hearings platform longtermist witnesses over affected communities. Theater ratio (0.42) reflects genuine coordination on AI safety research (real value) mixed with performative commitments (voluntary pledges, ethics boards without power, 'responsible AI' marketing that coexists with harmful deployments). The trend shows extraction and theater rising as x-risk framing institutionalizes (new institutes, government AI safety bodies, international summits) while present-harm regulation lags (algorithmic accountability acts stall, bias audit mandates remain voluntary).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (victim groups) experience this constraint as a snare: coordination rhetoric covers extraction, exit is blocked, enforcement suppresses alternatives (community-led audits, worker organizing, regulatory bans on harmful deployments). The agenda_setter seats (technology companies, x-risk institutes) experience it as a rope: genuine coordination on AI safety, voluntary participation, net benefit. The engine computes this divergence from the structural data — the declared roles and exit options drive the per-seat classification split. This seat divergence is the measurement: a constraint claimed as coordination by its administrators that computes as extraction for its targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Technology companies and frontier developers are structural beneficiaries: they collect the subsidy of avoided regulation (d ≈ 0.15). AI safety institutes and longtermist philanthropies are secondary beneficiaries: they capture funding and talent (d ≈ 0.20). Global South populations and marginalized groups are full targets: they bear extraction with trapped exit (d ≈ 0.95). Displaced workers and surveilled communities are near-full targets: constrained exit, identity-locked in labor markets or civic systems (d ≈ 0.85). Civil society organizations are excluded: they would contest but lack venue access (d ≈ 0.75). Competition authorities and analytical observers sit near symmetric: they perceive the structure but lack direct stake (d ≈ 0.50). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating AI safety research to prevent catastrophic outcomes) remains live but has been captured: the arrangement now serves to legitimize priority inversion. The mandate ('prevent existential catastrophe') has not atrophied — it has been weaponized to suppress present-harm governance. Mandatrophy is NOT resolved because the coordination function (AI safety research) is real and the extraction function (regulatory diversion) is actively maintained. This is tangled_rope, not piton: the coordination function has not atrophied; it has been leveraged to sustain extraction. The constraint would not persist without active enforcement (venue control, funding gates, narrative dominance). If enforcement ceased, present-harm governance would rapidly reclaim priority (world_rearranges).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the ai_risk_governance_priority kernel, or a distinct constraint that shares surface vocabulary?',
    'Trace the institutional genealogy: do the actors, venues, and funding flows of this reading derive from a common commitment to the kernel, or do they constitute a separate governance project that adopted the kernel''s language?',
    'If distinct, the kernel_id linkage is analytical rather than structural — the constraint family decomposition would be invalid and network edges to sibling readings should be removed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the near_term_harms_reading is a true kernel reading or a separate constraint using kernel vocabulary.').

omega_variable(
    reading_relation_bridge,
    'Does the near_term_harms_reading coexist with, influence, or foreclose the bridge_reading?',
    'Map the institutional overlap: do unified-framework advocates (bridge_reading) operate within the same governance venues as present-harm advocates, or are they segregated? If the same actors hold both, coexistence; if bridge_reading actors actively suppress present-harm specificity, influences; if bridge_reading logically requires deprioritizing present harms, forecloses.',
    'Determines the cs_structure.reading_relations edge type to bridge_reading — affects contamination propagation and drift analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_bridge, conceptual, 'Structural relationship between near_term_harms_reading and bridge_reading.').

omega_variable(
    reading_relation_existential,
    'Does the near_term_harms_reading coexist with, influence, or foreclose the existential_risk_reading?',
    'Examine venue admission: do x-risk governance forums (Summit series, AI Safety Institutes, longtermist fellowships) admit present-harm advocates as peers? If excluded, forecloses (the frameworks cannot be held simultaneously in one venue); if admitted but marginalized, influences; if integrated, coexists_with.',
    'Determines the cs_structure.reading_relations edge type to existential_risk_reading — affects whether the kernel is modeled as a genuine dispute or a capture structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_existential, conceptual, 'Structural relationship between near_term_harms_reading and existential_risk_reading.').

omega_variable(
    present_harm_measurement_ambiguity,
    'Is the high extractiveness score driven by measured present harms (bias audits, displacement data, surveillance documentation) or by the reading''s own framing of what counts as harm?',
    'Compare harm inventories across readings: if existential_risk_reading acknowledges the same present harms but weights them lower, extraction is structural; if it denies their severity, extraction is framing-dependent.',
    'If framing-dependent, ε is not invariant across readings of the same kernel — violates DP-001 and requires decomposition into separate kernels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(present_harm_measurement_ambiguity, empirical, 'Whether extractiveness is invariant across kernel readings or framing-dependent.').

omega_variable(
    coordination_extraction_separability,
    'Is the AI safety coordination function (research coordination, talent pipelines, international dialogue) structurally separable from the extraction function (regulatory diversion, funding capture, venue exclusion)?',
    'Natural experiment: if x-risk governance venues opened admission to present-harm advocates and rebalanced funding, would coordination persist? If yes, separable (tangled_rope); if coordination collapses without extraction, the coordination is a cover story (snare).',
    'If inseparable, the claimed coordination function is not genuine — reclassify from tangled_rope to snare. If separable, tangled_rope stands with distinct coordination and extraction components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_risk_near_term_tr_t2018, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement_basis(ai_risk_near_term_tr_t2018, observed).
narrative_ontology:measurement(ai_risk_near_term_tr_t2020, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement_basis(ai_risk_near_term_tr_t2020, observed).
narrative_ontology:measurement(ai_risk_near_term_tr_t2022, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2022, 0.34).
narrative_ontology:measurement_basis(ai_risk_near_term_tr_t2022, observed).
narrative_ontology:measurement(ai_risk_near_term_tr_t2024, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2024, 0.39).
narrative_ontology:measurement_basis(ai_risk_near_term_tr_t2024, observed).
narrative_ontology:measurement(ai_risk_near_term_tr_t2026, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(ai_risk_near_term_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ai_risk_near_term_be_t2018, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement_basis(ai_risk_near_term_be_t2018, observed).
narrative_ontology:measurement(ai_risk_near_term_be_t2020, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement_basis(ai_risk_near_term_be_t2020, observed).
narrative_ontology:measurement(ai_risk_near_term_be_t2022, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2022, 0.67).
narrative_ontology:measurement_basis(ai_risk_near_term_be_t2022, observed).
narrative_ontology:measurement(ai_risk_near_term_be_t2024, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2024, 0.73).
narrative_ontology:measurement_basis(ai_risk_near_term_be_t2024, observed).
narrative_ontology:measurement(ai_risk_near_term_be_t2026, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2026, 0.78).
narrative_ontology:measurement_basis(ai_risk_near_term_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_risk_near_term_su_t2018, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement_basis(ai_risk_near_term_su_t2018, observed).
narrative_ontology:measurement(ai_risk_near_term_su_t2020, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement_basis(ai_risk_near_term_su_t2020, observed).
narrative_ontology:measurement(ai_risk_near_term_su_t2022, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2022, 0.57).
narrative_ontology:measurement_basis(ai_risk_near_term_su_t2022, observed).
narrative_ontology:measurement(ai_risk_near_term_su_t2024, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(ai_risk_near_term_su_t2024, observed).
narrative_ontology:measurement(ai_risk_near_term_su_t2026, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2026, 0.65).
narrative_ontology:measurement_basis(ai_risk_near_term_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=2018, tn=2026
narrative_ontology:measurement(ai_risk_near_term_grid_01, ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse(class), 2018, 0.45).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_01, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_02, ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse(class), 2026, 0.58).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_02, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_03, ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse(individual), 2018, 0.41).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_03, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_04, ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse(individual), 2026, 0.54).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_04, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_05, ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse(organizational), 2018, 0.28).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_05, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_06, ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse(organizational), 2026, 0.42).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_06, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_07, ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse(structural), 2018, 0.32).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_07, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_08, ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse(structural), 2026, 0.48).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_08, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_09, ai_risk_governance_priority__near_term_harms_reading, resistance(class), 2018, 0.35).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_09, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_10, ai_risk_governance_priority__near_term_harms_reading, resistance(class), 2026, 0.67).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_10, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_11, ai_risk_governance_priority__near_term_harms_reading, resistance(individual), 2018, 0.28).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_11, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_12, ai_risk_governance_priority__near_term_harms_reading, resistance(individual), 2026, 0.61).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_12, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_13, ai_risk_governance_priority__near_term_harms_reading, resistance(organizational), 2018, 0.22).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_13, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_14, ai_risk_governance_priority__near_term_harms_reading, resistance(organizational), 2026, 0.58).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_14, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_15, ai_risk_governance_priority__near_term_harms_reading, resistance(structural), 2018, 0.15).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_15, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_16, ai_risk_governance_priority__near_term_harms_reading, resistance(structural), 2026, 0.42).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_16, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_17, ai_risk_governance_priority__near_term_harms_reading, stakes_inflation(class), 2018, 0.35).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_17, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_18, ai_risk_governance_priority__near_term_harms_reading, stakes_inflation(class), 2026, 0.61).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_18, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_19, ai_risk_governance_priority__near_term_harms_reading, stakes_inflation(individual), 2018, 0.28).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_19, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_20, ai_risk_governance_priority__near_term_harms_reading, stakes_inflation(individual), 2026, 0.49).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_20, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_21, ai_risk_governance_priority__near_term_harms_reading, stakes_inflation(organizational), 2018, 0.18).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_21, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_22, ai_risk_governance_priority__near_term_harms_reading, stakes_inflation(organizational), 2026, 0.44).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_22, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_23, ai_risk_governance_priority__near_term_harms_reading, stakes_inflation(structural), 2018, 0.22).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_23, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_24, ai_risk_governance_priority__near_term_harms_reading, stakes_inflation(structural), 2026, 0.52).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_24, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_25, ai_risk_governance_priority__near_term_harms_reading, suppression(class), 2018, 0.42).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_25, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_26, ai_risk_governance_priority__near_term_harms_reading, suppression(class), 2026, 0.71).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_26, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_27, ai_risk_governance_priority__near_term_harms_reading, suppression(individual), 2018, 0.38).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_27, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_28, ai_risk_governance_priority__near_term_harms_reading, suppression(individual), 2026, 0.68).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_28, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_29, ai_risk_governance_priority__near_term_harms_reading, suppression(organizational), 2018, 0.31).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_29, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_30, ai_risk_governance_priority__near_term_harms_reading, suppression(organizational), 2026, 0.64).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_30, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_31, ai_risk_governance_priority__near_term_harms_reading, suppression(structural), 2018, 0.25).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_31, observed).
narrative_ontology:measurement(ai_risk_near_term_grid_32, ai_risk_governance_priority__near_term_harms_reading, suppression(structural), 2026, 0.58).
narrative_ontology:measurement_basis(ai_risk_near_term_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, algorithmic_accountability_regulation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_fairness_audit_mandates).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, worker_transition_programs).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, surveillance_oversight_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_risk_governance_priority kernel. The kernel decomposes into three constraint stories: near_term_harms_reading (this file, high ε on present deployment harms), existential_risk_reading (high ε on speculative superintelligence governance), bridge_reading (moderate ε on unified frameworks). They are linked by network.affects_constraints because the priority assignment in one reading structurally shapes resource availability for the others. The ε values differ substantially: this reading authors ε=0.78 on present harms; existential_risk_reading would author ε≈0.2 on present harms but high on future scenarios; bridge_reading would author intermediate ε with different beneficiary/victim structures. This decomposition follows the ε-invariance principle: each reading instantiates a different constraint with a stable ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, institutional, 0.18).
constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, powerful, 0.22).
constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, powerless, 0.92).
constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, moderate, 0.82).
constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
