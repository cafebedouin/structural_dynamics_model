% ============================================================================
% CONSTRAINT STORY: regulatory_exemption_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_exemption_arbitrage, []).

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
 *   constraint_id: regulatory_exemption_arbitrage
 *   human_readable: Regulatory Exemption Arbitrage in Autonomous Vehicle Deployment
 *   domain: technology_governance/automotive_industry/autonomous_systems
 *
 * SUMMARY:
 *   The regulatory exemption arbitrage constraint emerges from the structural
 *   tension between federal motor vehicle safety standards designed for
 *   human-driven vehicles (requiring steering wheels, pedals, mirrors) and
 *   manufacturer claims that autonomous vehicles with novel architectures can
 *   achieve equivalent or superior safety without these components. NHTSA's
 *   exemption process, intended to enable limited testing of innovative
 *   designs, has become a strategic pathway for manufacturers to gain
 *   competitive advantage by deploying vehicles at commercial scale before
 *   safety equivalence is verified. The constraint exhibits tangled rope
 *   characteristics: genuine coordination function (enabling innovation that
 *   rigid standards would block) coexists with asymmetric extraction
 *   (manufacturers externalize safety verification costs onto the public
 *   while capturing first-mover advantages). The theater ratio (0.58)
 *   reflects that much of the exemption application process is performative:
 *   manufacturers submit extensive documentation of simulated safety
 *   performance and internal testing, but NHTSA lacks capacity to
 *   independently verify claims, and no exemptions have yet been granted for
 *   the claimed 2,500-unit commercial deployment target. The gap between
 *   application rhetoric and approval reality reveals the theater.
 *   State-level testing permits create additional arbitrage opportunities:
 *   manufacturers operate under state authority while lobbying for federal
 *   exemptions, exploiting jurisdictional ambiguity to deploy vehicles in
 *   permissive jurisdictions without full federal approval.
 *
 * KEY AGENTS:
 *   - Exemption-Seeking Manufacturer (institutional/arbitrage): Primary beneficiary — captures first-mover advantage, competitive deployment timeline, and option value of exemption approval without bearing full safety verification costs
 *   - Regulatory Precedent Integrity (powerless/trapped): Primary victim — abstract institutional good that cannot exit or organize; bears full cost of precedent erosion as exemptions based on unverified claims contaminate future regulatory decisions
 *   - Standards-Compliant Manufacturers (moderate/constrained): Secondary victim — invested in meeting existing standards, face asymmetric competitive pressure from exemption-seeking competitors, cannot exit regulatory framework without abandoning sunk compliance costs
 *   - State Transportation Regulators (moderate/constrained): Mixed position — benefit from local innovation and economic development enabled by testing permits, but bear extraction as federal exemption pressure erodes state authority over vehicle safety
 *   - NHTSA Federal Regulator (institutional/constrained): Mixed position — genuine coordination function (exemption process enables innovation) coexists with extraction (manufacturer pressure to grant exemptions before verification erodes agency authority); identity-locked dimension makes regulatory capture difficult to recognize from within
 *   - Safety Advocacy Coalition (organized/mobile): Organized agents see temporary problem with sunset — first serious incident will trigger legislative response closing exemption pathway
 *   - Public Safety Verification Process (powerless/trapped): Abstract collective good — bears cost of shifted verification burden (from pre-deployment testing to post-deployment incident response) with no advocate
 *   - Analytical Observer (analytical/analytical): Sees both genuine coordination function and asymmetric extraction as structural features, neither dominating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_exemption_arbitrage, 0.48).
domain_priors:suppression_score(regulatory_exemption_arbitrage, 0.62).
domain_priors:theater_ratio(regulatory_exemption_arbitrage, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_exemption_arbitrage, extractiveness, 0.48).
narrative_ontology:constraint_metric(regulatory_exemption_arbitrage, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(regulatory_exemption_arbitrage, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_exemption_arbitrage, tangled_rope).
narrative_ontology:human_readable(regulatory_exemption_arbitrage, "Regulatory Exemption Arbitrage in Autonomous Vehicle Deployment").
narrative_ontology:topic_domain(regulatory_exemption_arbitrage, "technology_governance/automotive_industry/autonomous_systems").

domain_priors:requires_active_enforcement(regulatory_exemption_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_exemption_arbitrage, tesla_first_mover_advantage).
narrative_ontology:constraint_beneficiary(regulatory_exemption_arbitrage, exemption_seeking_manufacturers).
narrative_ontology:constraint_victim(regulatory_exemption_arbitrage, regulatory_precedent_integrity).
narrative_ontology:constraint_victim(regulatory_exemption_arbitrage, competing_manufacturers_following_standards).
narrative_ontology:constraint_victim(regulatory_exemption_arbitrage, public_safety_verification_process).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGULATORY PRECEDENT INTEGRITY (SNARE) — The abstract institutional good of consistent safety standards cannot exit the constraint. Once exemptions are granted based on manufacturer claims rather than verified performance, the precedent contaminates future regulatory decisions. Maximum extraction: the regulatory framework bears full cost of erosion with no advocate and no escape.
constraint_indexing:constraint_classification(regulatory_exemption_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STANDARDS-COMPLIANT MANUFACTURERS (SNARE) — Manufacturers who invested in meeting existing federal safety standards face asymmetric competitive pressure. Cannot exit the regulatory framework (constrained by prior capital allocation and compliance investments), yet bear extraction as exemption-seeking competitors gain deployment advantages without equivalent safety verification. High suppression: switching to exemption-seeking strategy requires abandoning sunk compliance costs and reputational commitments to safety-first approach.
constraint_indexing:constraint_classification(regulatory_exemption_arbitrage, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE TRANSPORTATION REGULATORS (TANGLED ROPE) — State-level regulators experience genuine coordination benefit (testing permits enable local innovation and economic development) alongside extraction (federal exemption applications create pressure to approve deployments before safety verification is complete, eroding state authority). Constrained exit: cannot refuse all autonomous vehicle testing without losing economic competitiveness, but also cannot fully exit federal regulatory framework.
constraint_indexing:constraint_classification(regulatory_exemption_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: EXEMPTION-SEEKING MANUFACTURER (ROPE) — Primary beneficiary experiences the constraint as coordination: NHTSA exemption process provides legitimate pathway to deploy novel vehicle architectures that existing standards (designed for human-driven vehicles) do not accommodate. Arbitrage exit: can deploy in permissive jurisdictions, lobby for federal exemptions, or pivot to standard-compliant designs if exemption strategy fails. Net beneficiary: extraction flows toward this agent.
constraint_indexing:constraint_classification(regulatory_exemption_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NHTSA FEDERAL REGULATOR (TANGLED ROPE) — Experiences genuine coordination function (exemption process enables innovation within safety framework) but also extraction (manufacturer pressure to grant exemptions before verification is complete erodes agency authority and shifts liability risk to regulator). Constrained exit: cannot refuse all exemption applications without blocking legitimate innovation, but granting exemptions based on unverified manufacturer claims creates precedent that undermines future regulatory capacity. Identity-locked dimension: agency's professional identity is constituted through balancing innovation enablement with safety verification, making it difficult to recognize when the balance has shifted to regulatory capture.
constraint_indexing:constraint_classification(regulatory_exemption_arbitrage, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SAFETY ADVOCACY COALITION (SCAFFOLD) — Organized agents (consumer safety groups, insurance industry, plaintiff attorneys) see the exemption arbitrage as a temporary coordination failure with a sunset mechanism: the first serious autonomous vehicle incident involving an exemption-granted vehicle will trigger legislative response that closes the exemption pathway and establishes verified-performance requirements. Mobile exit: coalition can shift advocacy focus to other regulatory domains if autonomous vehicle safety becomes politically intractable. Low effective extraction because coalition has agency and sees a clear (if tragic) resolution path.
constraint_indexing:constraint_classification(regulatory_exemption_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the constraint exhibits both genuine coordination function (exemption process enables deployment of novel vehicle architectures that rigid standards would block) and asymmetric extraction (manufacturer capture of regulatory process shifts safety verification burden from pre-deployment testing to post-deployment incident response). The extraction is structural: exemption-seeking manufacturers gain competitive advantage by externalizing safety verification costs onto the public, while standards-compliant manufacturers bear full verification costs internally. Tangled rope classification reflects that both functions are real and neither dominates.
constraint_indexing:constraint_classification(regulatory_exemption_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_exemption_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_exemption_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_exemption_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_exemption_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(regulatory_exemption_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Manufacturers capture significant competitive advantage by obtaining deployment approval without equivalent safety verification burden borne by standards-compliant competitors. The extraction is not maximal because some genuine coordination function exists (exemption process does enable novel architectures that rigid standards would block), and the competitive advantage is time-limited (standards will eventually adapt or exemptions will be closed after incidents). However, extraction is substantial: manufacturers externalize safety verification costs onto the public (post-deployment incident response rather than pre-deployment testing), gain first-mover market advantages, and establish precedent that erodes regulatory capacity for future oversight. Suppression (0.62): Moderate-high. Standards-compliant manufacturers face significant barriers to switching strategies: sunk costs in compliance infrastructure, reputational commitments to safety-first approach, and organizational identity built around meeting rather than circumventing standards. State regulators face suppression through economic competition (cannot refuse all testing permits without losing innovation economy to other states) and federal preemption ambiguity (unclear authority to block vehicles with pending federal exemptions). NHTSA faces suppression through resource constraints (cannot independently verify manufacturer claims), political pressure (innovation enablement mandate conflicts with safety verification mandate), and revolving-door dynamics (career incentives favor industry-friendly decisions). Theater ratio (0.58): Moderate-high and increasing. Exemption application process involves extensive documentation of simulated performance and internal testing, but NHTSA lacks capacity for independent verification. No exemptions granted for commercial-scale deployment (2,500-unit target) despite years of applications, revealing gap between process rhetoric and substantive approval. State testing permits create parallel theater: permits granted based on manufacturer assurances without verified performance data, with jurisdictional ambiguity allowing manufacturers to claim state approval as implicit federal authorization. Theater has increased over interval as application volume has grown while approval capacity has remained static, and as manufacturers have learned to optimize application rhetoric without corresponding safety verification improvements.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how structural position determines classification. Exemption-seeking manufacturers see coordination (rope) — they are solving the legitimate problem of deploying novel vehicle architectures that existing standards do not accommodate. Safety advocacy coalition sees temporary problem with sunset (scaffold) — first serious incident will trigger legislative closure of exemption pathway. Standards-compliant manufacturers and regulatory precedent integrity see pure extraction (snare) — exemption arbitrage erodes competitive fairness and regulatory capacity with no genuine coordination benefit from their perspective. State regulators and NHTSA see mixed coordination and extraction (tangled rope) — genuine innovation enablement coexists with regulatory capture pressure. Analytical observer sees tangled rope at civilizational scale — both coordination function and extraction are real structural features, neither fully dominates, and the constraint's resolution depends on empirical questions (incident rates, attribution mechanisms, capture vs. enablement balance) that remain unresolved. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' All classifications are legitimate readings of the same base properties from different indexical contexts.
 *
 * DIRECTIONALITY LOGIC:
 *   Exemption-seeking manufacturers are primary beneficiaries with arbitrage exit options — they can deploy in permissive state jurisdictions, lobby for federal exemptions, or pivot to standard-compliant designs if exemption strategy fails. Engine derives low d (beneficiary + arbitrage) → low/negative chi → rope classification from their perspective. Standards-compliant manufacturers are victims with constrained exit — cannot abandon sunk compliance costs without major capital loss, face competitive pressure from exemption-seeking rivals. Engine derives high d (victim + constrained) → high chi → snare classification from their perspective. Regulatory precedent integrity is a powerless victim with trapped exit — abstract institutional good cannot organize or escape. Engine derives maximum d (victim + trapped) → maximum chi → snare classification. State regulators are mixed (both beneficiaries of local innovation and victims of eroded authority) with constrained exit — cannot refuse all permits without economic loss, cannot fully exit federal framework. Engine derives moderate d → moderate chi → tangled rope classification. NHTSA is institutional with constrained exit (not arbitrage despite institutional power, because agency cannot exit its statutory mandate) and mixed beneficiary/victim status (benefits from innovation enablement mission, victimized by regulatory capture pressure). Engine derives moderate d → moderate chi → tangled rope classification. The identity-locked dimension for NHTSA reflects that the agency's professional identity is constituted through balancing innovation and safety, making it difficult to recognize when the balance has shifted to capture — the agency sees itself as enabling beneficial innovation even when structural evidence suggests extraction dominates. Safety advocacy coalition is organized with mobile exit (can shift focus to other domains) and sees sunset mechanism (incident-triggered legislative response). Engine derives low d → low chi → scaffold classification.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE JUSTIFICATION: The constraint exhibits both genuine coordination function (exemption process enables deployment of novel vehicle architectures that rigid human-driver-oriented standards would block, potentially accelerating beneficial autonomous vehicle adoption) AND asymmetric extraction (manufacturers externalize safety verification costs onto the public, capture first-mover competitive advantages, and erode regulatory precedent by substituting unverified claims for demonstrated performance). Neither function dominates. The coordination function is real: existing FMVSS standards (steering wheel, pedals, mirrors) were designed for human drivers and may genuinely be unnecessary or counterproductive for autonomous systems. Rigid adherence to these standards would block architectures that could be safer than human-driven vehicles. The exemption process provides a legitimate pathway for innovation. The extraction is also real: manufacturers gain competitive deployment advantages by obtaining exemptions before safety equivalence is verified, shifting verification burden from pre-deployment testing (borne by manufacturer) to post-deployment incident response (borne by public). Standards-compliant manufacturers who invested in meeting existing requirements face asymmetric competitive pressure. Regulatory precedent is eroded as exemptions based on manufacturer claims rather than verified performance contaminate future decisions. The constraint requires active enforcement (NHTSA exemption approval process, state testing permit systems) and creates identifiable beneficiaries (exemption-seeking manufacturers) and victims (regulatory precedent integrity, standards-compliant competitors, public safety verification process). The tangled rope classification prevents both false negatives (missing the extraction by focusing only on innovation enablement) and false positives (missing the coordination function by focusing only on regulatory capture). The mandatrophy is resolved by recognizing that both functions are structural features of the constraint, and their relative weights depend on empirical questions encoded in the omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exemption_approval_threshold,
    'What level of safety verification is sufficient to justify exemption from federal standards designed for human-driven vehicles?',
    'Comparative incident rate analysis: exemption-granted vehicles vs. standard-compliant vehicles vs. human-driven vehicles over equivalent deployment scales and conditions. Requires multi-year data collection across diverse operating environments.',
    'If threshold is set at human-parity: exemptions granted prematurely, extracting safety verification cost from public. If threshold is set at superhuman performance: legitimate innovation blocked, coordination function lost. Current ambiguity allows manufacturer claims to substitute for verified performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_approval_threshold, empirical, 'Safety verification threshold for exemption approval').

omega_variable(
    state_federal_jurisdiction_boundary,
    'Do state-level testing permits constitute implicit approval for commercial operation, or is federal exemption genuinely required?',
    'Legal precedent analysis: enforcement actions against manufacturers operating under state permits without federal exemptions; judicial interpretation of NHTSA authority vs. state transportation authority.',
    'If state permits sufficient: federal exemption process is theater, and extraction occurs through state-level regulatory arbitrage (manufacturers forum-shop for permissive jurisdictions). If federal exemption required: NHTSA retains gate-keeping authority, and extraction occurs through federal exemption capture. Jurisdictional ambiguity enables dual-track extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_federal_jurisdiction_boundary, conceptual, 'State vs. federal jurisdiction for autonomous vehicle deployment').

omega_variable(
    incident_attribution_mechanism,
    'When an autonomous vehicle incident occurs, is causation attributable to exemption-granted design choices (lack of steering wheel/pedals) or to software/sensor failures that would occur regardless of vehicle architecture?',
    'Forensic incident analysis with counterfactual modeling: would the incident have been prevented or mitigated if the vehicle had standard controls? Requires detailed incident reconstruction and access to proprietary system logs.',
    'If exemption-granted design is causal: regulatory precedent is invalidated, triggering scaffold sunset (legislative response closes exemption pathway). If design is not causal: exemption process is vindicated, and constraint reclassifies toward rope (genuine coordination). Attribution ambiguity allows manufacturers to externalize design risk while claiming safety equivalence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incident_attribution_mechanism, empirical, 'Incident causation attribution for exemption-granted vehicles').

omega_variable(
    regulatory_capture_vs_innovation_enablement,
    'Is NHTSA''s exemption process primarily serving manufacturer interests (regulatory capture) or genuinely enabling beneficial innovation that rigid standards would block?',
    'Comparative policy analysis: exemption approval rates, manufacturer lobbying expenditures, revolving-door employment patterns between NHTSA and industry, and correlation between exemption grants and verified safety performance vs. manufacturer claims.',
    'If capture dominates: constraint is snare from more perspectives, and coordination function is cover story. If innovation enablement dominates: constraint is rope from more perspectives, and extraction is side effect. Current ambiguity allows both narratives to coexist, preventing clear classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_vs_innovation_enablement, conceptual, 'Regulatory capture vs. innovation enablement balance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_exemption_arbitrage, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_initial, regulatory_exemption_arbitrage, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_mid, regulatory_exemption_arbitrage, theater_ratio, 3, 0.48).
narrative_ontology:measurement(theater_current, regulatory_exemption_arbitrage, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(extract_initial, regulatory_exemption_arbitrage, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(extract_mid, regulatory_exemption_arbitrage, base_extractiveness, 3, 0.41).
narrative_ontology:measurement(extract_current, regulatory_exemption_arbitrage, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_exemption_arbitrage, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_exemption_arbitrage, hardware_before_software_inversion).

% DUAL FORMULATION NOTE:
% This constraint is downstream of hardware_before_software_inversion (the business model requiring exemptions from steering wheel/pedal requirements). The upstream constraint (snare classification: manufacturers locked into hardware architecture that requires regulatory exemptions not yet granted) creates the structural pressure that manifests as exemption arbitrage. The two constraints have different epsilon values reflecting different structural features: hardware_before_software_inversion measures the capital lock-in and sunk cost trap (high extraction from manufacturer's own prior decisions), while regulatory_exemption_arbitrage measures the competitive advantage extraction and regulatory precedent erosion (moderate extraction distributed across multiple victims). Both constraints are real; neither is reducible to the other. The upstream constraint explains WHY manufacturers pursue exemptions (locked into architecture requiring them); the downstream constraint explains HOW the exemption pursuit extracts value (regulatory arbitrage and precedent erosion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_exemption_arbitrage, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
