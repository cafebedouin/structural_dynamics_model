% ============================================================================
% CONSTRAINT STORY: ai_safety_verification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_verification, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_safety_verification
 *   human_readable: AI Safety Verification and Capability Disclosure
 *   domain: artificial_intelligence/safety_governance
 *
 * SUMMARY:
 *   AI safety verification embodies a core structural tension in contemporary
 *   AI governance: frontier capability labs have both the resources and
 *   incentive to develop advanced safety evaluations, yet they also control
 *   what gets verified, with whom, and by which metrics. This creates a
 *   dual-nature constraint that exhibits both genuine coordination (the field
 *   needs shared safety measurement methodologies) and systematic extraction
 *   (labs capture the verification authority and can suppress or reframe
 *   safety findings that threaten deployment timelines). The constraint's
 *   architecture parallels verification bottlenecks in other
 *   advanced-technology domains (pharmaceuticals, aviation) but with a
 *   critical difference: those domains have regulatory inspection authority
 *   and mandatory third-party access protocols. AI safety verification
 *   remains largely voluntary and lab-controlled. The extractiveness value
 *   (0.58) reflects moderate asymmetric power: frontier labs benefit from
 *   controlling the verification narrative, but genuine coordination value
 *   exists (safety evaluation is necessary and labs do conduct sophisticated
 *   internal reviews). The suppression value (0.68) is high because access to
 *   models, training data, and internal evaluations is heavily restricted,
 *   and independent verification without lab cooperation is technically
 *   difficult. The theater ratio (0.65) captures the increasing performative
 *   dimension: public safety narratives, red-team reports, and alignment
 *   claims are increasingly produced with stakeholder reassurance as the
 *   primary audience rather than robust truth-tracking. The constraint has
 *   strengthened over the interval (2020-2026) as deployment pressures have
 *   mounted and labs have developed more sophisticated control over the
 *   verification narrative.
 *
 * KEY AGENTS:
 *   - Frontier AI Labs (Anthropic, OpenAI, Google DeepMind, Meta, others): Primary beneficiaries (institutional/arbitrage) — control verification scope, methodology, timeline, and narrative. Benefit from selective disclosure and claims of alignment without exposing full evaluation data.
 *   - Independent Safety Researchers: Primary victims (powerless/trapped) — excluded from access to systems that most need evaluation. Cannot verify claims, forced to evaluate based on lab-selected information. Bear reputational cost if labs' claims prove false.
 *   - Public Epistemic Commons: Victim collective (powerless/trapped) — absorbs systemic risk from unverified alignment claims. No mechanism to verify or enforce truthfulness. Bears cost of misallocated research attention and delayed genuine safety solutions.
 *   - Safety-Focused Researchers Inside Labs: Secondary victims (moderate/constrained) — have access and resources but face asymmetric power. Internal findings that threaten deployment timelines can be suppressed, delayed, or reframed. Career incentives aligned with capability progress, not safety truth.
 *   - Regulatory Bodies and Oversight Boards: Secondary victims (organized/constrained) — mandate to verify but depend on labs' voluntary cooperation. Epistemically asymmetric (lack technical expertise and model access). Labs control information flow to regulators.
 *   - Academic Safety Community: Institutional actor (institutional/arbitrage) — maintains scholarly legitimacy but access to frontier systems has declined. Theater: produces benchmarks, frameworks, publications that signal rigor while lacking influence on actual frontier verification.
 *   - Analytical Observer: Vantage point that risks naturalizing contingent institutional arrangements as inherent to advanced AI development.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_verification, 0.58).
domain_priors:suppression_score(ai_safety_verification, 0.68).
domain_priors:theater_ratio(ai_safety_verification, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_verification, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_safety_verification, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_safety_verification, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_verification, tangled_rope).
narrative_ontology:human_readable(ai_safety_verification, "AI Safety Verification and Capability Disclosure").
narrative_ontology:topic_domain(ai_safety_verification, "artificial_intelligence/safety_governance").

domain_priors:requires_active_enforcement(ai_safety_verification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_verification, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_safety_verification, capability_researchers).
narrative_ontology:constraint_victim(ai_safety_verification, independent_safety_researchers).
narrative_ontology:constraint_victim(ai_safety_verification, public_epistemic_commons).
narrative_ontology:constraint_victim(ai_safety_verification, deployment_oversight_bodies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT SAFETY RESEARCHER (SNARE) — Trapped outside closed verification loops. Cannot access model weights, training data, or internal safety evaluations. Forced to evaluate claims based on published red-team results selected and framed by labs. Zero agency over verification methodology or scope. Maximum extraction — bears reputational and epistemic cost if labs' alignment claims prove false, but cannot independently verify.
constraint_indexing:constraint_classification(ai_safety_verification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC EPISTEMIC COMMONS (SNARE) — No exit option. Bears systemic risk from unverified alignment claims. If frontier labs' safety narratives prove false post-deployment, the commons absorbs the cost through misallocation of research attention, false confidence in governance mechanisms, and delayed development of genuine safety solutions. Cannot organize or verify; experiences extraction as progressive epistemic contamination.
constraint_indexing:constraint_classification(ai_safety_verification, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SAFETY-FOCUSED RESEARCHERS INSIDE LABS (TANGLED ROPE) — Constrained by employment, funding dependencies, and access to compute resources. Internal safety teams have genuine coordination function: labs do need rigorous safety evaluation before deployment. But also experience extraction: internal findings that threaten capability commercialization timelines are suppressed, reframed, or delayed; publishing safety concerns risks career penalty or reassignment. Mixed experience — real access and resources but asymmetric power over what findings emerge.
constraint_indexing:constraint_classification(ai_safety_verification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FRONTIER AI LAB (ROPE) — Benefits from coordination function: safety evaluation is necessary and labs do want defensible alignment claims. Controls verification scope, methodology, access, and timeline. Experiences constraint as coordination: sharing selected safety findings builds trust with investors, policymakers, and researchers while protecting competitive advantage. Net beneficiary through selective disclosure and narrative control. Low effective extraction because they define what gets extracted.
constraint_indexing:constraint_classification(ai_safety_verification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY BODIES (TANGLED ROPE) — Mandated to verify alignment before deployment (coordination function) but dependent on labs' voluntary disclosure and cooperation (extraction mechanism). Cannot mandate independent access without risking lab exit to jurisdictions with lighter oversight. Constrained by epistemic asymmetry: regulators lack technical expertise and access to evaluate claims independently. Real governance function but asymmetric power — labs determine what information reaches regulators and on what timeline. Moderate extraction from information control and deployment pressure.
constraint_indexing:constraint_classification(ai_safety_verification, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ACADEMIC SAFETY RESEARCH COMMUNITY (PITON) — Maintains scholarly legitimacy and peer-review infrastructure but increasingly isolated from the frontier where verification matters most. Publishes safety frameworks and benchmarks (theater: demonstrating rigor and methodological care) while lacking access to the systems that most urgently need verification. Institutional inertia keeps academic safety research sustained through grants and hiring, but primary function — producing actionable oversight — has atrophied. Theater ratio high because the constraint persists through the narrative 'academic research is part of safety ecosystem' while the functional reality is epistemic exclusion from frontier systems.
constraint_indexing:constraint_classification(ai_safety_verification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW FRAME (MOUNTAIN) — From a civilizational vantage, the verification gap appears as an inevitable feature of asymmetric information in advanced technology development: frontier labs will always have access advantages; external verification will always lag capability development; the structural problem is inherent to the domain, not contingent. This perspective risks naturalizing what is actually a contingent institutional arrangement (selective disclosure, competitive secrecy norms, regulatory capture dynamics). Engine will detect as false summit: the beneficiary structure reveals the 'natural' lag is actively constructed.
constraint_indexing:constraint_classification(ai_safety_verification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_verification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_safety_verification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_safety_verification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_verification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_safety_verification, TR),
    TR >= 0.70.

:- end_tests(ai_safety_verification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Frontier labs derive substantial benefit from controlling verification: they capture the ability to define 'alignment,' select which safety findings are disclosed, control the timeline for external scrutiny, and frame their own claims without independent contradiction. However, extractiveness is not at snare levels (0.72+) because genuine coordination value exists — labs do conduct sophisticated internal safety evaluations, and shared measurement methodologies have real validity. The extracted value is asymmetric access to verification authority, not pure rent-seeking. The measurement trajectory (0.38 → 0.48 → 0.58) shows extraction increasing as deployment pressures mount and labs have developed more sophisticated disclosure strategies. Suppression (0.68): High. Access restrictions to model weights, training data, and internal evaluations are substantial. Independent verification without lab cooperation is technically difficult (evaluating black-box systems is slower and less rich than white-box evaluation). Career and publication barriers suppress critical safety findings from reaching public discourse. However, suppression is not total (0.80+) because: (1) some information does leak (former employees, leaked documents, side-channel inferences); (2) some labs voluntarily share more access than others; (3) regulatory and media pressure is gradually creating disclosure norms. The trajectory shows suppression increasing (0.52 → 0.60 → 0.68) as labs have invested in more sophisticated information control and as deployment timelines compress oversight windows. Theater ratio (0.65): Moderate-high and rising. Labs produce public safety narratives, red-team reports, and alignment claims that demonstrate rigor and methodological care (theater function). But increasingly the primary audience is stakeholder reassurance (policymakers, investors, media) rather than rigorous truth-tracking. Internal safety methodologies are often more sophisticated than external claims suggest. The trajectory (0.35 → 0.52 → 0.65) reflects the maturation of safety-as-communication strategy: as deployment timelines accelerated, labs invested in more sophisticated public safety narratives. Early labs (2020) relied more on technical demonstration and less on narrative; recent labs (2026) have integrated dedicated safety communication into business strategy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximal perspectival divergence across observation positions. Independent safety researchers see a snare: trapped outside verification loops, forced to accept lab claims, bearing reputational cost for false positives. Frontier labs see rope: coordination mechanism for achieving necessary safety evaluation while protecting competitive advantage. Regulatory bodies see tangled rope: genuine mandate to verify but asymmetric power in access and timeline. Internal safety researchers see tangled rope from the other direction: legitimate function (safety is necessary) but extraction (findings can be suppressed). The academic safety community sees piton: maintains institutional legitimacy but functional role has declined as access to frontier systems contracted. The analytical observer risks seeing mountain (verification gap as inherent to asymmetric information) but the structural data reveals this as false summit — the 'natural' lag is actively constructed through labs' control of verification narratives and access policies. The key diagnostic insight: the constraint's type depends entirely on whether you have agency over the verification process. Those with agency (frontier labs) see rope; those without agency (independent researchers, epistemic commons) see snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs benefit from controlling the verification narrative, so their d value is low (around 0.15-0.20): they are net beneficiaries with arbitrage exit options. Independent safety researchers are victims without exit options, so their d value is high (around 0.92-0.98): they experience maximum extraction. Regulatory bodies are partially victims (dependent on lab cooperation) with some exit options (they could mandate access), so d is moderate-high (around 0.65-0.75). Internal safety researchers are mixed: they benefit from access and resources but suffer extraction of findings, so d is moderate (around 0.55-0.65). The engine derives these d values from the beneficiary/victim declarations plus exit options, then applies the sigmoid f(d) to compute experienced extractiveness. Beneficiaries with arbitrage options experience suppressed chi; victims without exit experience amplified chi. Regulatory bodies' moderate d reflects that they have some structural power (mandate authority) but limited practical power (labs can threaten regulatory exit). No directionality overrides are needed — the structural declarations capture the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in AI safety verification is whether the constraint is a legitimate coordination problem (requiring shared safety measurement methods and verification infrastructure) or an illegitimate extraction mechanism (labs capturing verification authority to suppress scrutiny). The resolution: both. The constraint is a genuine tangled rope, not a pure snare or pure rope. The coordination function is real — safety evaluation is necessary, shared methodologies have validity, and labs do conduct sophisticated internal reviews. The extraction is equally real — labs use their control of access and timeline to suppress findings and manipulate the verification narrative. Neither reading is wrong; the constraint genuinely exhibits both. The mandatrophy resolves through the indexical structure: from the beneficiary's position (institutional/arbitrage), the constraint is rope with primarily coordination benefit. From the victim's position (powerless/trapped), the constraint is snare with primarily extraction cost. From the analytical observer's position at the appropriate organizational level (organized/constrained regulators), the constraint is tangled rope with both functions present. The analytical claim is tangled rope because: (1) both beneficiaries and victims exist and are essential to the constraint's operation; (2) active enforcement is required (labs must enforce access restrictions); (3) genuine coordination value exists (safety evaluation is legitimate); (4) asymmetric extraction is present (labs benefit from controlling verification authority). The false summit detection triggers because the constraint has identifiable beneficiaries (frontier labs benefit from current disclosure norms) — suggesting what appears as a natural verification bottleneck (inevitable asymmetry in advanced tech) is actually a constructed constraint that serves lab interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alignment_measurement_contingency,
    'Are laboratory safety evaluations measuring alignment robustly, or are they measuring labs'' ability to demonstrate alignment to specific constituencies?',
    'Post-deployment performance tracking: correlation between lab safety claims and actual system behavior under novel conditions outside training distribution. Independent red-teaming with different methodologies and threat models than labs employ internally.',
    'If labs are measuring actual robustness: extractiveness drops (constraint is coordination with some labs winning the game legitimately). If labs are measuring stakeholder reassurance: extractiveness rises (constraint is pure extraction disguised as verification). Current state appears mixed — some labs conduct rigorous internal evals, others produce theater for governance signaling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_measurement_contingency, empirical, 'Whether safety evaluations measure actual robustness or governance theater').

omega_variable(
    external_verification_feasibility,
    'Is independent verification of frontier AI alignment claims technically feasible at the speed and scale required for governance, or is external verification structurally impossible without lab cooperation?',
    'Technical investigation: can external researchers design evaluations of alignment that don''t require access to model internals? Can behavioral testing (without weights access) establish sufficient confidence bounds? Comparison of verification timelines: how long for external red-teams vs internal lab evaluations on identical systems.',
    'If feasible: suppression drops dramatically (constraints on access become contingent policy choice, not technical necessity). If impossible: suppression is structural; external verification requires lab cooperation by default, locking in the tangled-rope/snare asymmetry permanently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_verification_feasibility, empirical, 'Technical feasibility of independent AI safety verification without lab cooperation').

omega_variable(
    disclosure_timing_rationality,
    'Are labs'' disclosure delays (announcing alignment claims only after capability deployment is imminent) driven by genuine safety caution or by incentives to complete capability development before external scrutiny can delay commercialization?',
    'Timeline analysis: correlation between disclosure timing and deployment readiness across multiple labs and time periods. Comparison with disclosure patterns in regulated domains (pharmaceuticals, aviation). Interview data from internal safety teams about pressures on publication and communication timelines.',
    'If driven by safety caution: suppression is justified (high disclosure risk justifies controlled access). If driven by commercialization incentives: suppression is extraction (timing manipulates the verification window to eliminate real-time oversight). Current evidence suggests mixed: some delays are safety-driven, some are deployment-driven.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disclosure_timing_rationality, empirical, 'Whether disclosure delays reflect safety caution or commercialization incentives').

omega_variable(
    scalable_oversight_existence,
    'Does a scalable external oversight mechanism exist (or can exist) that doesn''t depend on labs'' voluntary cooperation and transparency?',
    'Policy and technical research: investigation of auxiliary oversight pathways (computation-level monitoring, independent capability testing, red-team networks, regulatory inspection protocols, third-party access mandates). Feasibility and cost analysis relative to lab cooperation requirements.',
    'If such mechanisms exist: regulatory bodies can move from snare/tangled_rope to genuine rope through mandate and investment. If not: oversight will always depend on labs'' consent, locking in the current extraction asymmetry. This is the critical omega for governance design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scalable_oversight_existence, empirical, 'Whether scalable oversight mechanisms exist independent of lab cooperation').

omega_variable(
    competitive_pressures_and_safety_disclosure,
    'To what extent do competitive pressures between frontier labs actually force safety disclosure, versus labs cooperating to maintain control over the verification narrative?',
    'Comparative analysis: do labs'' safety claims converge on similar methodologies and findings (suggesting coordination) or diverge (suggesting independent verification)? Investigation of information-sharing among labs about verification failures and safety incidents. Empirical test: when one lab discloses a safety limitation, do competitors rush to disclose similar limitations or maintain competitive silence?',
    'If competitive pressure is strong: market forces may enforce verification transparency without regulation (constraints weaken toward rope). If labs maintain coordination on disclosure: competitive pressure is illusory, and suppression remains high (extraction remains structural). Current evidence: mixed. Some competition in safety claims, but also clear patterns of joint messaging on epistemic limitations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_pressures_and_safety_disclosure, empirical, 'Whether competitive pressure enforces safety disclosure or labs maintain coordinated narrative control').

omega_variable(
    false_summit_natural_law_framing,
    'Is the verification bottleneck a natural consequence of asymmetric information in advanced technology development, or a contingent institutional arrangement that benefits frontier labs and could be restructured through governance?',
    'Comparative analysis across regulated domains: verification structures in pharmaceuticals, aviation, finance, nuclear. Do all advanced technology fields show similar verification asymmetries, or do regulatory structures create alternatives? Investigation of counterfactual governance models (mandatory third-party access, computational inspection protocols, independent lab networks) and their technical and political feasibility.',
    'If natural law: current institutional structure is the least-bad option; efforts to increase verification access will be costly and marginal. If contingent: alternative governance structures could dramatically reduce extraction and suppression. Engine will flag this as false summit candidate — the beneficiary structure (frontier labs benefit from current disclosure norms) suggests the ''natural'' asymmetry is actively maintained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_law_framing, conceptual, 'Whether verification asymmetry is inherent to advanced tech or contingent on governance structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_verification, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aisv_tr_t0, ai_safety_verification, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aisv_tr_t3, ai_safety_verification, theater_ratio, 3, 0.52).
narrative_ontology:measurement(aisv_tr_t6, ai_safety_verification, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(aisv_be_t0, ai_safety_verification, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(aisv_be_t3, ai_safety_verification, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(aisv_be_t6, ai_safety_verification, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(aisv_su_t0, ai_safety_verification, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(aisv_su_t3, ai_safety_verification, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(aisv_su_t6, ai_safety_verification, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_verification, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_safety_verification, ai_capability_governance).
narrative_ontology:affects_constraint(ai_safety_verification, frontier_lab_regulatory_capture).
narrative_ontology:affects_constraint(ai_safety_verification, open_safety_research_ecosystems).

% DUAL FORMULATION NOTE:
% AI safety verification is downstream of specific capability claims (GPT-X safety properties, Claude alignment, Gemini robustness) and upstream of governance-level AI deployment decisions. The verification constraint has its own extractiveness (0.58) distinct from the underlying capability claims' epistemic status (which could be mountain-grade if the safety claims prove universally correct, or snare-grade if they prove systematically false). The network links: capability claims → verification bottleneck → regulatory decisions. A second formulation layer: the constraint exists within a broader 'AI progress governance' system that includes frontier lab autonomy (upstream), verification methodology development (parallel), and regulatory authority (downstream). Each level has its own constraint structure; they are linked through this network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
