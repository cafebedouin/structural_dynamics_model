% ============================================================================
% CONSTRAINT STORY: regulatory_capture_cycles
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_cycles, []).

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
 *   constraint_id: regulatory_capture_cycles
 *   human_readable: Regulatory Capture Cycles
 *   domain: political_economy/regulatory_governance
 *
 * SUMMARY:
 *   Regulatory capture cycles represent a structural entanglement of
 *   coordination and extraction in governance. A regulatory agency nominally
 *   protects public welfare by creating standards, enforcing rules, and
 *   constraining predatory industry behavior. But the same agency depends on
 *   industry expertise, industry cooperation for compliance, and industry
 *   hiring for career advancement of its personnel. This creates a systematic
 *   asymmetry: the agency coordinates market order and safety standards
 *   (genuine coordination function) while simultaneously serving industry
 *   interests in ways that harm consumers and competitors (extraction
 *   function). The constraint is not stable — it oscillates. Crises
 *   (financial collapse, safety failures, consumer outcry) trigger reform
 *   cycles that temporarily reduce capture intensity through transparency
 *   measures, ethics rules, and new leadership. During the crisis phase, the
 *   agency reasserts public-interest framing and constrains industry. But as
 *   crisis fades and new political attention shifts elsewhere, enforcement
 *   relaxes, turnover brings industry-aligned personnel, and the capture
 *   intensifies again. This cyclical pattern is measurable: extractiveness
 *   rises from 0.35 to 0.58 over a cycle, theater ratio rises from 0.55 to
 *   0.68, suggesting that as capture deepens, the performative justification
 *   must strengthen to maintain legitimacy. The constraint exhibits all six
 *   classification types from different perspectives, making it a diagnostic
 *   exemplar for perspectival disagreement in governance.
 *
 * KEY AGENTS:
 *   - Regulated Industry: Primary beneficiary (organized/arbitrage or institutional/arbitrage) — captures favorable rules, barriers to entry, regulatory predictability; can arbitrage to less-regulated jurisdictions if domestic capture weakens
 *   - Public Welfare Beneficiaries (consumers, competitors, small firms): Primary victim (powerless/trapped) — trapped in regulatory framework that nominally protects them but is captured; no exit mechanism and limited visibility into extraction
 *   - Regulatory Agency: Institutional actor with dual perspective (institutional/constrained with identity_locked variant) — genuinely coordinates market order and safety standards while simultaneously captured through expertise dependence, career incentives, and revolving-door dynamics; captured personnel experience identity lock (professional identity fused with industry relationship)
 *   - Public Interest Reform Advocates: Organized agents (organized/constrained) — civil society organizations, consumer advocates, reform-minded legislators attempting to build alternative pathways (transparency, ethics rules, external expertise sourcing) with sunset logic
 *   - Regulatory Legitimacy Narrative: Institutional performance mechanism — the ongoing justification that regulatory agencies protect public welfare despite captured reality; maintained through theater (public hearings, impact assessments, formal processes) and institutional inertia
 *   - Analytical Observer: Civilizational context — risks naturalizing capture as inevitable feature of specialized governance rather than recognizing it as contingent on institutional design choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_cycles, 0.58).
domain_priors:suppression_score(regulatory_capture_cycles, 0.65).
domain_priors:theater_ratio(regulatory_capture_cycles, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_cycles, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture_cycles, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regulatory_capture_cycles, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_cycles, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_cycles, "Regulatory Capture Cycles").
narrative_ontology:topic_domain(regulatory_capture_cycles, "political_economy/regulatory_governance").

domain_priors:requires_active_enforcement(regulatory_capture_cycles).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_cycles, regulated_industry).
narrative_ontology:constraint_victim(regulatory_capture_cycles, public_welfare_beneficiaries).
narrative_ontology:constraint_victim(regulatory_capture_cycles, regulatory_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMING PUBLIC (SNARE) — Trapped by regulatory framework that nominally protects them but is captured by industry. No exit option and no mechanism to detect or escape the extraction. Bear costs of reduced competition, higher prices, and suppressed innovation without visibility into the mechanism. Maximum extraction, maximum suppression.
constraint_indexing:constraint_classification(regulatory_capture_cycles, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATORY AGENCY (TANGLED ROPE) — Constrained by revolving-door dynamics, industry expertise dependence, and career incentives that reward cooperative relationships with regulated firms. Also achieves genuine coordination: industry compliance, standardized safety protocols, market order. But the coordination mechanism is asymmetrically captured — rules benefit industry more than public. Active enforcement required to maintain both coordination and the asymmetry.
constraint_indexing:constraint_classification(regulatory_capture_cycles, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATED INDUSTRY (ROPE) — Experiences the constraint as pure coordination with massive benefits. Regulation creates barriers to entry, standardizes compliance burdens competitors must bear, and provides predictability. Exit option: arbitrage to less-regulated jurisdictions (but regulatory harmonization is rising, reducing this option). From this perspective, the constraint solves a collective action problem among firms.
constraint_indexing:constraint_classification(regulatory_capture_cycles, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANTI-CAPTURE REFORM MOVEMENT (SCAFFOLD) — Organized agents (citizen groups, consumer advocates, public interest lawyers, reform-minded politicians) see the capture cycle as a temporary institutional failure with a sunset. Transparency initiatives, ethics rules, congressional oversight, and regulatory modernization are explicit attempts to break the cycle. Theater is lower here (direct political action rather than captured process) because reform agents can exit the captured system by building alternative mechanisms. Sunset logic applies if reforms actually reduce revolving-door incentives.
constraint_indexing:constraint_classification(regulatory_capture_cycles, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY LEGITIMACY NARRATIVE (PITON) — The public justification for regulatory agencies relies on the narrative that they protect the public interest. This narrative persists even as the actual function has atrophied — agencies increasingly serve industry preferences while maintaining the performance of public protection. Theater ratio is high: agency publications, public hearings, impact statements all maintain the appearance of public-interest regulation despite captured reality. The narrative is inertially maintained because the alternative — admitting systematic capture — would collapse agency legitimacy.
constraint_indexing:constraint_classification(regulatory_capture_cycles, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CAPTURED REGULATOR (IDENTITY-LOCKED) — An institutional actor whose professional identity and career trajectory have become fused with the regulated industry. The regulator sees industry preferences as natural outcomes of expertise, not as capture. Professional identity is constituted through the industry relationship — switching sides would require abandoning the career path, the professional network, and the self-concept as 'expert in this sector.' Structurally mobile (could move to civil society, academia, different agency) but identity-locked into accepting industry framing as normal. This perspective instantiates the oracle gap: the captured regulator's own instruments cannot detect the capture they are embedded in.
constraint_indexing:constraint_classification(regulatory_capture_cycles, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/global perspective, regulatory capture may appear to be an immutable feature of governance: regulatory agencies are staffed by industry experts, industry has greater information access than politicians or consumers, and specialization requires deep domain knowledge that comes from industry careers. The constraint looks like a law of bureaucratic organization. However, the empirical falsifiability of this classification is high — comparative analysis of regulatory systems with different revolving-door rules, different expertise sourcing models, and different transparency regimes shows substantial variation in capture intensity. The mountain classification is a false summit.
constraint_indexing:constraint_classification(regulatory_capture_cycles, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_cycles_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_cycles, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_cycles, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_cycles, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_cycles, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_cycles_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regulated industry captures favorable rules, enforcement lenience, and barriers to entry while consumers face reduced competition, higher prices, and lower innovation. The extraction is not total because genuine coordination does occur — safety standards are enforced, compliance mechanisms work, markets function. The 0.58 value reflects that both extraction and coordination functions are real and substantial. This is definitional for Tangled Rope: high enough extraction (χ ≥ 0.40 expected) with genuine coordination (beneficiaries exist: the industry genuinely values the regulatory framework). Suppression (0.65): High. Multiple barriers prevent exit or resistance: (1) asymmetric information — industry understands regulatory processes better than citizens, (2) concentrated benefits for industry vs diffuse costs for consumers, (3) regulatory authority monopoly — consumers cannot choose alternative regulators, (4) career barriers for whistleblowers and reform-minded regulators, (5) technical complexity that prevents public scrutiny. Theater ratio (0.68): Elevated. Public hearings, environmental impact statements, formal notice-and-comment procedures, and regulatory impact analyses create the appearance of deliberative process and public input, but the core decisions often reflect industry preference. The theater rises over the cycle as capture deepens and legitimacy maintenance becomes more necessary. Measurement pattern (0-10 scale): Extractiveness oscillates (0.35→0.58→0.52→0.62→0.58) suggesting cyclical capture-release-recapture. Theater ratio shows similar oscillation with slightly different phase (0.55→0.68→0.64→0.70→0.68), suggesting that performative justification strengthens as capture deepens. This is the measurement signature of the cycle.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in regulatory capture is among the most pronounced in governance structures. The regulated industry experiences the constraint as valuable coordination — it creates market order, standardizes compliance, prevents race-to-the-bottom, and provides predictability for long-term investment. From the industry perspective, the constraint solves the collective action problem of multiple firms wanting stable rules. The consuming public experiences the constraint as extraction pure and simple — they pay higher prices and see less innovation, with no understanding of why or mechanism to exit. The regulatory agency experiences it as genuinely mixed: real safety standards and coordination services are provided, but the distribution of benefits and costs is captured. The most revealing gap appears between the institutional regulator viewed as a generic coordinator vs the same institution viewed from the identity_locked perspective: if you recognize that the regulator's personnel are identity-fused with industry preferences, the 'coordination' function appears less neutral — the rules being coordinated around are already biased. The reform movement sees the constraint as temporary — current institutional design produces capture, but alternative designs (more independence, different expertise sourcing, transparency mechanisms) could reduce it. The false-summit mountain perspective risks naturalizing capture as inevitable, which comparative analysis of regulatory systems falsifies.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position. The regulated industry has d ≈ 0.15-0.25 (beneficiary + arbitrage exit → low d, negative f(d) → experiences subsidy rather than extraction). The captured regulator depends on whether viewed as constrained (material career incentives) or identity_locked (professional identity fused with industry): constrained gives d ≈ 0.50-0.55, identity_locked gives d ≈ 0.35-0.45 (beneficiary status weakens d offset, but trapped-by-identity dynamics persist). Public welfare beneficiaries have d ≈ 0.85-0.95 (victims + trapped exit → high d, high f(d) ≈ 1.15-1.42 → maximum experienced extraction). The organized reform movement has d ≈ 0.55-0.65 (victims + constrained exit, but with agency and alternative pathways → moderate d, f(d) ≈ 0.75). These directionality values explain why the same structural constraint produces different classifications for each perspective: the beneficiary (industry) genuinely experiences coordination (Rope) because their d is low and f(d) is near-zero or negative; the powerless public experiences pure extraction (Snare) because their d is high and f(d) is maximum; the agency experiences the hybrid (Tangled Rope) because it is both beneficiary (industry-adjacent) and enactor of coordination (standards, safety).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that extraction and coordination are genuinely both present, not misclassified. The public legitimation narrative (regulation protects consumers) is not false — regulation does provide safety standards and market order. The extraction narrative (regulation serves industry interests) is not false — industry does capture favorable rules and barriers to entry. The Tangled Rope classification rejects the false choice between 'purely good coordination' and 'purely bad extraction' and recognizes that both mechanisms operate simultaneously. The mandatrophy would arise if someone classified the constraint as pure Rope (claiming industry favoritism is just coordination) or pure Snare (claiming no genuine coordination occurs). The Tangled Rope classification prevents both false summarizations. The cycling behavior — extractiveness rising from 0.35 to 0.58 and falling back, theater rising and falling — confirms that the constraint has genuine internal dynamics, not just static asymmetry. During reform cycles (extractiveness drops, theater drops), the coordination function reasserts (safety enforcement increases, transparency increases). During capture deepening (extractiveness rises, theater rises), the extraction mechanism strengthens while performative justification compensates. The constraint is not mislabeled; it is properly identified as a system where coordination and extraction are structurally intertwined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_vs_expertise_dependence,
    'Is observed industry favoritism in regulation capture (asymmetric extraction serving industry interests) or legitimate expertise dependence (regulators genuinely need industry knowledge)?',
    'Comparative analysis: do regulatory regimes with different expertise-sourcing models (civil-service technical experts vs industry-recruited regulators vs external consultants) show different public-interest outcomes? Historical analysis of policy decisions where expert consensus diverged from industry preference.',
    'If expertise dependence dominates: constraint reclassifies toward Rope (pure coordination). If capture dominates: constraint remains Tangled Rope or Snare. If mixed: current Tangled Rope classification is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capture_vs_expertise_dependence, empirical, 'Whether observed industry favoritism is capture or legitimate expertise need').

omega_variable(
    revolving_door_mechanism_identity_lock,
    'Does the revolving door operate through material career incentives (job prospects, salary differential, network access) or through identity fusion (the regulator has become the industry identity)?',
    'Qualitative analysis of regulator exit narratives; interviews with career transitions; analysis of regulatory decisions before/after industry employment transitions; time-series of personal network composition for individual regulators.',
    'If material incentives dominant: exit options for captured regulators is ''constrained'' (high-cost but possible). If identity-locked: exit options is ''identity_locked'' (would require identity restructuring). Different exit classifications produce different chi values and perspectival gaps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolving_door_mechanism_identity_lock, empirical, 'Whether revolving door operates through material incentives or identity fusion').

omega_variable(
    cycle_vs_equilibrium,
    'Is regulatory capture a oscillatory cycle (crisis → reform → relaxation → re-capture) or a monotonic equilibrium that appears cyclical only due to measurement artifacts?',
    'Time-series analysis of regulatory stringency, enforcement intensity, and industry profitability across multiple regulatory cycles (3+ cycles). Decomposition: are apparent cycles driven by external shocks (crises, new political actors) or by internal dynamics of the constraint itself?',
    'If true cycle: scaffold sunset logic is real — reform cycles can reduce capture intensity if properly designed. If equilibrium: apparent cycles are external noise, and capture will reassert regardless of reform effort. Implications for mandatrophy and lifecycle drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cycle_vs_equilibrium, empirical, 'Whether capture exhibits cyclical or equilibrium dynamics').

omega_variable(
    regulatory_independence_ceiling,
    'Is there a structural limit to how independent a regulatory agency can become while maintaining access to specialized expertise and industry cooperation?',
    'Comparative analysis: regulatory regimes with varying independence safeguards (fixed terms, ethics barriers, external expertise, budget autonomy) and their effectiveness; analysis of regulator recruitment patterns and expertise sourcing in high-independence vs low-independence systems.',
    'If hard ceiling exists: independence measures have diminishing returns, and some extraction may be inevitable. If no ceiling: capture is contingent on institutional design choices, not structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_independence_ceiling, empirical, 'Whether regulatory independence has structural limits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_cycles, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture_cycles, theater_ratio, 0, 0.55).
narrative_ontology:measurement(regcap_tr_t2, regulatory_capture_cycles, theater_ratio, 2, 0.6).
narrative_ontology:measurement(regcap_tr_t4, regulatory_capture_cycles, theater_ratio, 4, 0.68).
narrative_ontology:measurement(regcap_tr_t6, regulatory_capture_cycles, theater_ratio, 6, 0.64).
narrative_ontology:measurement(regcap_tr_t8, regulatory_capture_cycles, theater_ratio, 8, 0.7).
narrative_ontology:measurement(regcap_tr_t10, regulatory_capture_cycles, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture_cycles, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcap_be_t2, regulatory_capture_cycles, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(regcap_be_t4, regulatory_capture_cycles, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(regcap_be_t6, regulatory_capture_cycles, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(regcap_be_t8, regulatory_capture_cycles, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(regcap_be_t10, regulatory_capture_cycles, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_cycles, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capture_cycles, financial_regulation_arbitrage).
narrative_ontology:affects_constraint(regulatory_capture_cycles, pharmaceutical_pricing_capture).
narrative_ontology:affects_constraint(regulatory_capture_cycles, environmental_standards_weakening).
narrative_ontology:affects_constraint(regulatory_capture_cycles, labor_standards_degradation).

% DUAL FORMULATION NOTE:
% Regulatory capture cycles is an upstream constraint affecting domain-specific capture mechanisms (financial regulation, pharmaceutical pricing, environmental standards, labor standards). Each downstream constraint has its own ε value reflecting domain-specific extraction intensity, but all depend on the generic capture cycle mechanism. The upstream constraint exhibits the cyclical dynamics; downstream constraints exhibit domain-specific manifestations of those dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_cycles, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
