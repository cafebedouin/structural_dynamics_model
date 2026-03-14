% ============================================================================
% CONSTRAINT STORY: ai_capability_inflation_in_procurement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_capability_inflation_in_procurement, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_capability_inflation_in_procurement
 *   human_readable: AI Capability Inflation in Procurement
 *   domain: economic/organizational/technology
 *
 * SUMMARY:
 *   AI capability inflation in procurement occurs when vendors, consultants,
 *   and internal sponsors systematically overstate AI system capabilities
 *   during vendor selection and contract negotiation. The constraint creates
 *   asymmetric extraction: vendors and procurement consultants benefit from
 *   contract awards based on inflated claims; implementation teams and budget
 *   holders bear the cost when actual capabilities fall short of promises.
 *   This constraint exhibits tangled rope characteristics — genuine
 *   coordination exists (vendors must communicate system capabilities;
 *   procurement must evaluate and select solutions; sponsors must align AI
 *   strategy with business goals) alongside systematic asymmetric extraction
 *   (capability claims that exceed real-world integration, timeline, or
 *   performance). The theater ratio reflects that vendor RFP responses and
 *   procurement evaluation processes create an illusion of rigorous
 *   capability assessment while lacking mechanisms to verify claims against
 *   real-world implementation. Suppression operates through multiple
 *   channels: implementation teams suppress critical assessment to avoid
 *   confronting poor vendor selection; budget holders suppress escalation to
 *   avoid political cost; sponsors suppress acknowledgment of gaps to protect
 *   credibility; procurement functions treat vendor claims as exogenous
 *   rather than subject to critical scrutiny. The constraint family spans
 *   organizational hierarchy and spans time — from RFP theater (piton,
 *   immediate), through vendor benefit capture during the contract-to-pilot
 *   phase (rope), through trapped implementation teams (snare, biographical),
 *   to internal capability development as an alternative pathway (scaffold,
 *   generational).
 *
 * KEY AGENTS:
 *   - AI Vendors: Primary beneficiary (institutional/arbitrage) — capture contract value during inflation phase; have exit options through market pivots and service adjustments; benefit from first-mover advantage and customer lock-in during implementation phase
 *   - Procurement Consultants: Primary beneficiary (powerful/arbitrage) — capture advisory fees by positioning as integrators between vendor claims and organizational reality; reputation and revenue depend on appearing to manage vendor selection rigorously while avoiding confrontation that would expose poor selection
 *   - Implementation Teams: Primary victim (powerless/trapped) — contractually obligated to deliver on inflated capability claims; bear reputational and career cost of capability gaps; suppressed from public escalation by organizational politics
 *   - Budget Holders: Primary victim (powerless/trapped) — committed capital based on inflated ROI assumptions; suppressed from reallocation by sunk costs and sponsorship pressure; absorb delivery delays and capability shortfalls
 *   - Executive Sponsors: Secondary beneficiary with constrained exit (powerful/constrained) — gain credibility boost from perceived AI transformation during announcement phase; constrained by political inability to acknowledge inflated claims without damaging credibility; incentive to accept successive capability reductions rather than publicly failing at vendor selection
 *   - Procurement Function: Organized actor with mixed dynamics (organized/constrained) — has genuine coordination role (vendor management, risk mitigation) but also suppressed incentive to flag inflated claims (career risk of appearing as selection failure); constrained by hierarchy pressure to accept sponsor preferences
 *   - Internal AI/Data Science Teams: Organized agents building alternatives (organized/constrained) — developing internal capabilities and proof-of-concepts as exit pathway; see external vendor solutions as temporary; not trapped but constrained by organizational politics that suppress internal capability investment in favor of external vendor solutions
 *   - Analytical Observer: Sees organizational theater and suppression mechanisms (analytical/analytical) — risks naturalizing information asymmetry between vendors and procurers as inevitable; can detect false naturalizations through structural analysis of suppression and beneficiary incentives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_capability_inflation_in_procurement, 0.58).
domain_priors:suppression_score(ai_capability_inflation_in_procurement, 0.65).
domain_priors:theater_ratio(ai_capability_inflation_in_procurement, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_capability_inflation_in_procurement, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_capability_inflation_in_procurement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_capability_inflation_in_procurement, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_capability_inflation_in_procurement, tangled_rope).
narrative_ontology:human_readable(ai_capability_inflation_in_procurement, "AI Capability Inflation in Procurement").
narrative_ontology:topic_domain(ai_capability_inflation_in_procurement, "economic/organizational/technology").

domain_priors:requires_active_enforcement(ai_capability_inflation_in_procurement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_capability_inflation_in_procurement, ai_vendors).
narrative_ontology:constraint_beneficiary(ai_capability_inflation_in_procurement, procurement_consultants).
narrative_ontology:constraint_beneficiary(ai_capability_inflation_in_procurement, executive_sponsors).
narrative_ontology:constraint_victim(ai_capability_inflation_in_procurement, budget_holders).
narrative_ontology:constraint_victim(ai_capability_inflation_in_procurement, implementation_teams).
narrative_ontology:constraint_victim(ai_capability_inflation_in_procurement, organizational_capability_planning).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPLEMENTATION TEAM (SNARE) — Trapped by contractual obligations, sunk costs, and organizational reputation risk. Bears full cost of the gap between promised and actual capabilities. Cannot exit without career damage and project failure attribution.
constraint_indexing:constraint_classification(ai_capability_inflation_in_procurement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BUDGET HOLDER (SNARE) — Trapped by capital deployment decisions, organizational politics, and sunk implementation costs. Extraction mechanism: promised ROI fails to materialize, but redirection of funds is politically costly. Suppression operates through organizational hierarchy and path dependence.
constraint_indexing:constraint_classification(ai_capability_inflation_in_procurement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: AI VENDOR (ROPE) — Experiences the constraint as pure coordination: translating capability claims into contractual language, managing customer expectations, documenting feature scope. Has exit options (can pivot to other markets, adjust pricing). Net beneficiary during capability inflation phase — early-mover advantage, market capture before claims are independently verified.
constraint_indexing:constraint_classification(ai_capability_inflation_in_procurement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROCUREMENT FUNCTION (TANGLED ROPE) — Organized institutional actor with constrained exit. Genuine coordination function: procurement specifications, vendor management, risk mitigation. But also subject to capability inflation extraction: pressure to demonstrate market awareness, reluctance to flag unrealistic claims (career risk), and incentive to shift blame to vendors rather than internal decision-making.
constraint_indexing:constraint_classification(ai_capability_inflation_in_procurement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EXECUTIVE SPONSOR (TANGLED ROPE) — Powerful agent with constrained exit due to organizational politics and board expectations. Genuine coordination function: aligning AI strategy with business objectives, managing transformation timelines. But also subject to extraction through capability inflation: sponsor's credibility becomes tied to vendor success; motivation to accept inflated claims to justify initial investment; constrained by inability to publicly acknowledge poor vendor selection without reputational damage.
constraint_indexing:constraint_classification(ai_capability_inflation_in_procurement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: RFP PROCESS (PITON) — Request-for-proposal evaluation is largely performative. Checklist-based capability assessment cannot verify real-world integration, organizational adoption, or actual ROI. The RFP process persists through institutional inertia despite low functional verification capacity. Theater manifests as detailed evaluation matrices that create illusion of rigor while missing capability-reality gaps. High theater ratio reflects the gap between evaluation process and actual vendor capability verification.
constraint_indexing:constraint_classification(ai_capability_inflation_in_procurement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNAL CAPABILITY DEVELOPMENT (SCAFFOLD) — Organized agents (data scientists, ML engineers, platform teams) building internal AI capabilities as an alternative to vendor solutions. See the inflation constraint as temporary — proof-of-concept phases, open-source baselines, and transparent capability roadmaps are creating exit paths. Low effective extraction from this perspective because these agents have agency and are actively building alternatives. Sunset logic applies: as internal capabilities mature and organizational experience with AI increases, the asymmetric reliance on vendor claims decreases.
constraint_indexing:constraint_classification(ai_capability_inflation_in_procurement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information asymmetry between vendors (who build systems) and procurers (who evaluate abstractions) is inevitable. This perspective naturalizes capability inflation as an immutable feature of how technology gets adopted. However, the structural data contradicts this mountain classification — the constraint is sustained by organizational theater, suppression of critical evaluation, and beneficiary incentives, not by laws of nature or logic.
constraint_indexing:constraint_classification(ai_capability_inflation_in_procurement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_capability_inflation_in_procurement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_capability_inflation_in_procurement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_capability_inflation_in_procurement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_capability_inflation_in_procurement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_capability_inflation_in_procurement, TR),
    TR >= 0.70.

:- end_tests(ai_capability_inflation_in_procurement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting sustained capability-reality gaps during implementation and the systematic overstatement of vendor capabilities during procurement. The value reflects that extraction is not complete — some vendor claims are legitimate, some organizational actors benefit from vendor solutions (sponsors gain credibility during announcement phase), and some implementation teams successfully absorb gaps through heroic effort. But the trajectory shows inflation increasing over time as organizational experience with AI increases, vendors face stronger competition, and suppression mechanisms become more necessary to maintain the framing. Suppression (0.65): High. Multiple suppression mechanisms operate: implementation teams suppress critical assessment to avoid organizational stigma; budget holders suppress escalation to avoid political cost; procurement suppresses vendor claim scrutiny to avoid appearing as selection failure; sponsors suppress acknowledgment of gaps to protect credibility. The suppression is not just external (vendors controlling information) but distributed across the organization — organizational actors are active participants in suppressing the reality that capability claims were inflated. Theater ratio (0.68): High and rising. RFP evaluation matrices, vendor demos, pilot projects, and engagement models create an illusion of rigorous capability assessment while lacking mechanisms to falsify claims about integration complexity, organizational adoption readiness, or long-term ROI. Theater increases over time as vendors develop increasingly sophisticated presentation techniques and as organizations develop defense mechanisms (the RFP theater, vendor demos with cherry-picked use cases, proof-of-concept designs that avoid production complexity).
 *
 * PERSPECTIVAL GAP:
 *   Implementation teams and budget holders see snare — they are trapped, extraction is high and unavoidable, suppression is total. Vendors see rope — communication of capabilities is coordination function, customers benefit from solutions (vendors perceive this as genuine value creation). Procurement sees tangled rope — coordination exists but so does extraction through reputation risk and vendor pressure. Executive sponsors see tangled rope — they coordinate AI strategy and gain credibility benefits but are constrained by inability to publicly acknowledge poor selection. Internal AI teams see scaffold — they have agency, are building alternatives, and see a sunset where internal capabilities and organizational maturity make external vendor solutions less necessary. RFP evaluation theater appears as piton — institutional ritual that persists through inertia while losing functional capacity for capability verification. The mountain perspective (naturalizing information asymmetry as inevitable) is revealed as false through the structural presence of suppression and organized beneficiary advantage — the constraint is sustained by human choices, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values for each perspective are derived from structural position — power level, exit options, and beneficiary/victim status. AI vendors have low d (institutional power + arbitrage exit + beneficiary status) → low experienced extraction from their perspective. Implementation teams have high d (powerless + trapped exit + victim status) → high experienced extraction. Executive sponsors occupy an intermediate position (powerful + constrained exit + partial beneficiary/victim status through reputational coupling) → moderate-high d. The procurements function's d is pulled upward by victim status (trapped when procurement failures are exposed) but moderated by institutional power and beneficiary incentive structure (procurement consultants captured by vendor relationships) — intermediate d with vertical dispersion depending on which function within procurement is examined. Internal capability development teams have moderate d despite victim status (trapped by organizational politics) because they have agency (building alternatives) and partial arbitrage options (career mobility in AI/data science field).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by distinguishing the tangled rope constraint (procurement + vendor dynamics with mixed coordination and extraction) from the snare constraint (implementation teams and budget holders trapped by decisions made during the procurement phase). The same organizational phenomenon has different extractiveness for different actors at different time horizons. At the procurement-to-contract phase, the constraint appears as tangled rope from the vendor and procurement perspective (coordination with asymmetric benefit). At the implementation phase, it appears as snare from implementation teams and budget holders (trapped by prior decisions, extraction now unavoidable). The key to avoiding mandatrophy is recognizing that the measured extractiveness (0.58) reflects a weighted average across phases — higher during implementation phase (snare dynamics), lower during procurement phase (rope/tangled rope dynamics). The classifier's task is not to find 'the' type but to measure the distributional mix: what proportion of affected actors experience each type at each phase? The tangled rope classification for the aggregate constraint reflects that all six types appear across the organizational structure and time horizon, but tangled rope dominates at the organizational decision-making level (procurement function + executive sponsors) where the extraction-coordination tradeoff is most balanced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capability_claim_falsifiability,
    'What measurement criteria distinguish between conservative vendor claims (appropriately hedged, achievable) and inflated claims (systematically overstate capability breadth, integration ease, or timeline)?',
    'Post-deployment audit: compare contract specifications to actual system performance metrics; track which capability claims failed during pilot vs production phases; correlate claim conservatism with customer satisfaction and long-term vendor relationships',
    'If vendors systematically inflate: extractiveness remains high (0.58+) and snare classification is appropriate for trapped actors. If inflation is symmetrically distributed: extractiveness drops to 0.35-0.40 and constraint shifts toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capability_claim_falsifiability, empirical, 'Falsifiability criteria for vendor capability claims').

omega_variable(
    organizational_inflation_amplification,
    'How much of the capability inflation originates from vendor claims vs internal organizational dynamics (hope inflation by sponsors, capability theater by procurement, suppression of critical assessment by implementation teams)?',
    'Comparative analysis: RFP vendor proposals vs actual implementation requirements; audit of internal procurement decision records; stakeholder interviews on what pressure created acceptance of inflated claims',
    'If vendor-sourced: snare dynamics dominant, extraction sustained by information asymmetry and suppression. If internally amplified: tangled rope dynamics more prominent, suppression is organizational rather than enforced, some actors are active participants in inflation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_inflation_amplification, empirical, 'Vendor vs organizational sources of capability inflation').

omega_variable(
    procurement_reform_effectiveness,
    'Do structural procurement reforms (phased deployment with capability gates, independent verification, vendor liability for capability shortfalls) actually reduce inflation, or do they shift the inflation mechanism to new dimensions (integration claims, deployment timeline, change management claims)?',
    'Longitudinal study of organizations implementing vs not implementing procurement reforms; track whether capability inflation reappears in new forms or genuinely decreases; measure implementation team satisfaction and budget realization across reform cohorts',
    'If reforms effective: scaffold perspective confirmed, sunset is real, constraint is structurally solvable. If inflation shifts: snare and tangled rope remain dominant, suppression mechanism is resilient, extraction persists through new pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procurement_reform_effectiveness, empirical, 'Effectiveness of procurement reforms in reducing capability inflation').

omega_variable(
    vendor_strategic_positioning,
    'Is vendor capability inflation a deliberate extraction strategy (knowing claims are unsustainable, aiming to capture contracts and shift blame to customers) or emergent competitive signaling (vendors calibrate claims to market expectations and competitor claims, creating collective inflation without explicit coordination)?',
    'Vendor interviews, internal sales documentation, competitive analysis of claim trends; analysis of whether vendors with highest inflation rates retain customers or face reputation damage; comparison of enterprise vs startup vendor behavior',
    'If deliberate strategy: snare for beneficiaries (vendors know the extraction mechanism). If emergent signaling: tangled rope for vendors (genuine market pressure creates partial extraction without malice). Shapes how reputational/market mechanisms would constrain future behavior.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_strategic_positioning, empirical, 'Deliberate vs emergent sources of vendor capability inflation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_capability_inflation_in_procurement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aicap_tr_t0, ai_capability_inflation_in_procurement, theater_ratio, 0, 0.45).
narrative_ontology:measurement(aicap_tr_t3, ai_capability_inflation_in_procurement, theater_ratio, 3, 0.58).
narrative_ontology:measurement(aicap_tr_t6, ai_capability_inflation_in_procurement, theater_ratio, 6, 0.68).
narrative_ontology:measurement(aicap_tr_t10, ai_capability_inflation_in_procurement, theater_ratio, 10, 0.72).

% Extraction over time
narrative_ontology:measurement(aicap_be_t0, ai_capability_inflation_in_procurement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aicap_be_t3, ai_capability_inflation_in_procurement, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(aicap_be_t6, ai_capability_inflation_in_procurement, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(aicap_be_t10, ai_capability_inflation_in_procurement, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_capability_inflation_in_procurement, resource_allocation).
narrative_ontology:affects_constraint(ai_capability_inflation_in_procurement, software_implementation_overrun).
narrative_ontology:affects_constraint(ai_capability_inflation_in_procurement, enterprise_transformation_theater).
narrative_ontology:affects_constraint(ai_capability_inflation_in_procurement, vendor_lock_in_dependency).

% DUAL FORMULATION NOTE:
% AI capability inflation is structurally upstream of implementation overrun and vendor lock-in. The inflated capabilities promised in procurement directly cause implementation failure modes and create exit barriers that benefit vendors. Separate stories track the immediate procurement dynamics (this constraint) vs the longer-term lock-in mechanism (downstream) vs the implementation project failures (parallel).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_capability_inflation_in_procurement, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
