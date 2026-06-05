% ============================================================================
% CONSTRAINT STORY: information_foraging_theory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_foraging_theory, []).

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
 *   constraint_id: information_foraging_theory
 *   human_readable: Information Foraging Theory (IFT)
 *   domain: technological/cognitive
 *
 * SUMMARY:
 *   Information Foraging Theory posits that humans, when seeking information,
 *   behave like animals foraging for food — allocating cognitive effort based
 *   on perceived information scent, patch quality, and diminishing returns
 *   within information patches. Originally developed as a descriptive
 *   cognitive model by Peter Pirolli and Stuart Card (1999), IFT has become a
 *   foundational framework for interface design and information architecture.
 *   The theory exhibits structural tension between its genuine coordinative
 *   function (enabling designers to build more usable systems) and its
 *   extractive deployment (enabling designers to maximize engagement through
 *   exploitation of cognitive heuristics). The constraint's evolution shows
 *   increasing theater ratio (0.35→0.58) as the theory's deployment has
 *   shifted from descriptive science toward prescriptive
 *   attention-engineering. The extractiveness (0.38) reflects moderate but
 *   growing asymmetry: interface designers and platform operators use IFT to
 *   engineer user behavior, while information seekers experience constrained
 *   cognitive autonomy. IFT is a canonical example of how a descriptive
 *   theory becomes a normative blueprint for cognitive exploitation.
 *
 * KEY AGENTS:
 *   - Information Seekers: Primary victim (powerless/trapped) — constrained to foraging heuristics that are now interface-designed rather than naturally evolved; cannot exit information systems without abandoning access
 *   - Interface Designers: Primary beneficiary (institutional/arbitrage) — use IFT to design more effective (and more engaging) information architectures; experience theory as coordination tool
 *   - Platform Operators: Secondary beneficiary (powerful/mobile) — deploy IFT principles to maximize engagement, dwell time, and behavioral data extraction; frame extraction as user benefit
 *   - Cognitive Science Establishment: Institutional actor (institutional/arbitrage) — maintains IFT as canonical framework; increasingly detached from empirical validation of the theory's deployment consequences
 *   - Analytical Observer: Civilizational view (analytical/analytical) — can distinguish between IFT's descriptive validity (users do appear to forage) and its extractive deployment (but the heuristics are now engineered, not natural)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_foraging_theory, 0.38).
domain_priors:suppression_score(information_foraging_theory, 0.42).
domain_priors:theater_ratio(information_foraging_theory, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_foraging_theory, extractiveness, 0.38).
narrative_ontology:constraint_metric(information_foraging_theory, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(information_foraging_theory, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_foraging_theory, tangled_rope).
narrative_ontology:human_readable(information_foraging_theory, "Information Foraging Theory (IFT)").
narrative_ontology:topic_domain(information_foraging_theory, "technological/cognitive").

domain_priors:requires_active_enforcement(information_foraging_theory).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_foraging_theory, interface_designers).
narrative_ontology:constraint_beneficiary(information_foraging_theory, attention_capture_architects).
narrative_ontology:constraint_victim(information_foraging_theory, user_cognitive_autonomy).
narrative_ontology:constraint_victim(information_foraging_theory, information_seeker_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE INFORMATION SEEKER (SNARE) — Users seeking information are modeled as resource-constrained foragers with predictable patch-depletion and scent-following behaviors. Interface designers use IFT's predictions (information scent, patch residence time, cost-benefit calculations) to engineer attention extraction through information architecture that maximizes engagement dwell time. The seeker cannot exit the IFT-optimized interface ecosystem without abandoning information access entirely. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(information_foraging_theory, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE END USER (TANGLED ROPE) — Information seekers benefit from IFT-informed interface design (lower search costs, better information architecture, faster patch discovery). But this benefit is coupled with extraction: the same theory that reduces search cost also predicts and exploits scent-following behavior, engagement thresholds, and cognitive satisficing heuristics. Users have some exit options (switching platforms, reducing use) but at cognitive and social cost. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(information_foraging_theory, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERFACE DESIGN COMMUNITY (ROPE) — IFT provides a coordination mechanism for designing usable information systems. Without IFT, designers would resort to trial-and-error or uninformed guessing about user information-seeking behavior. IFT enables designers to predict patch depletion, information scent decay, and optimal search strategies. The community experiences this as coordination: applying IFT principles leads to systems that users find valuable and effective. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Net beneficiary through better design coordination.
constraint_indexing:constraint_classification(information_foraging_theory, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OPERATOR (TANGLED ROPE, EXTRACTIVE VARIANT) — Platforms use IFT to engineer attention capture and engagement maximization. IFT provides the theoretical justification for designing information architectures that exploit user cognitive satisficing, scent-following, and patch residence heuristics. The platform benefits from increased user time-on-site, deeper engagement, higher advertising exposure, and richer behavioral data. But platforms also benefit from IFT's coordination function: predictable user behavior enables reliable system design. This is pure extraction dressed as coordination. d≈0.25, f(d)≈0.25, σ=1.2 → χ≈0.11. Low effective extraction because platform operators have full agency and see IFT as a tool, not a constraint.
constraint_indexing:constraint_classification(information_foraging_theory, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COGNITIVE SCIENCE ESTABLISHMENT (PITON) — IFT originated as a descriptive cognitive model: humans do appear to forage for information using patch-depletion and scent-following heuristics. This observation is largely uncontested empirically. But the institutional framework around IFT has become increasingly performative: IFT is cited as justification for engagement-maximizing design despite weak evidence that IFT predictions optimize for user wellbeing rather than platform revenue. The theory persists through institutional inertia (it's in textbooks, cited in design literature, part of HCI canon) despite degradation of its primary function (understanding genuine user information-seeking behavior). theater_ratio=0.58 reflects moderate performative content: some IFT research is still empirically grounded, but much deployment is ritualistic. The establishment sees its own use of IFT as partially degraded.
constraint_indexing:constraint_classification(information_foraging_theory, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, humans do appear to forage for information using resource allocation heuristics rooted in evolutionary foraging behavior. This is a basic descriptive fact about human cognition: patch depletion, scent-following, and information cost-benefit calculations are structural features of how humans navigate information spaces. The theory captures an immutable aspect of human psychology. However, the base properties (ε=0.38, suppression=0.42, theater=0.58) do not support a mountain classification — the engine will detect this as a false summit. The 'naturalness' of IFT masks the extractive use to which the theory is deployed.
constraint_indexing:constraint_classification(information_foraging_theory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_foraging_theory_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_foraging_theory, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_foraging_theory, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_foraging_theory, TR),
    TR >= 0.70.

:- end_tests(information_foraging_theory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate and rising. IFT enables interface designers to predict and exploit information-seeking behavior. The extraction is not maximal (≥0.46) because: (1) users do genuinely benefit from better information architecture, (2) some designers apply IFT principles to reduce cognitive load rather than maximize engagement, and (3) users retain some agency through alternative platforms and search strategies. The rising trajectory (0.22→0.38 over 14 years) reflects increasing deployment of IFT principles by engagement-focused platforms. Suppression (0.42): Moderate. Users cannot easily exit the IFT-optimized information ecosystem, but suppression is not total — users can reduce platform use, switch services, or seek information through less optimized channels. Suppression has likely increased as IFT principles have become standard across platforms, reducing alternatives. Theater ratio (0.58): Moderate and rising. IFT is partially performative in deployment: designers cite IFT principles as justification for engagement-maximizing features even when evidence for user benefit is weak. The rituals of 'user-centered design' and 'information architecture best practices' increasingly hide extraction. But IFT research itself retains empirical content — the theory is not purely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The information seeker (powerless/trapped) perceives a snare: IFT-optimized interfaces constrain their cognitive autonomy and trap them in engagement loops. The end user (moderate/constrained) perceives tangled rope: they benefit from better search interfaces but at the cost of attention extraction. The design community (institutional/arbitrage) perceives rope: IFT solves the coordination problem of usable design. The platform operator (powerful/mobile) perceives low-extraction tangled rope: they benefit from both the coordination function (predictable user behavior) and the extraction function (engagement maximization), but frame it entirely as coordination. The cognitive science establishment (institutional/arbitrage) perceives piton: IFT is the canonical framework, but its deployment has become increasingly performative and divorced from empirical validation of user benefit. The analytical observer perceives a false summit: they risk naturalizing IFT as an immutable law of human cognition, masking that modern information-foraging behavior is increasingly engineered rather than evolved.
 *
 * DIRECTIONALITY LOGIC:
 *   Information seekers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. End users: Victim + constrained → d≈0.68, f(d)≈1.02. Significant extraction but not maximum. Interface designers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Platform operators: Beneficiary + mobile (but framed as victim of coordination need) → d≈0.25, f(d)≈0.25. Low effective extraction because they have full agency and see IFT as a tool. Cognitive science establishment: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification comes from theater gate (0.58 ≥ 0.70 threshold approaching), not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is prospective risk; the engine's false summit detector warns that naturalizing IFT masks its extractive deployment.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in IFT is resolved by recognizing the descriptive-normative collapse: IFT originated as a genuinely descriptive theory (humans do appear to forage for information using cost-benefit heuristics), which creates legitimate coordinative value (better interface design). But the theory has been deployed normatively — as a blueprint for engineering the behavior it purports to describe. This collapse is not a classification ambiguity but a structural transformation: as the theory becomes self-fulfilling (users foraging patterns are increasingly shaped by IFT-optimized interfaces), the distinction between 'describing human behavior' and 'engineering human behavior' collapses. The constraint is genuinely a tangled rope at present (ε=0.38, satisfies both coordination and extraction gates), but is trending toward snare as theater increases and extractiveness grows. The open question is whether IFT can be deployed coordinatively (transparent information architecture, user benefit verified) or whether the theory is structurally coupled to extraction (cognitive exploitation through designed information scents). The omega on descriptive vs normative collapse is the pivot point: if resolved toward descriptive, IFT could revert to pure rope; if resolved toward normative, it trends toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    descriptive_vs_normative_collapse,
    'Is IFT a descriptive theory of how humans actually forage for information, or has it become a normative blueprint for how to manipulate information-seeking behavior?',
    'Meta-analysis of IFT citations in design literature vs cognitive science literature; comparison of empirical validity claims vs deployment assumptions in industry applications',
    'If primarily descriptive: IFT is coordination (users understand their own heuristics, designers design for them). If primarily normative: IFT is extraction (designed-in cognitive exploitation). Classification shifts from Rope/Tangled Rope toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(descriptive_vs_normative_collapse, conceptual, 'Whether IFT functions as a descriptive or normative-prescriptive framework').

omega_variable(
    patch_depletion_autonomy,
    'Do users'' patch residence time decisions reflect their own information needs (autonomous judgment) or exploitation of satisficing heuristics (constrained by design)?',
    'Controlled experiments: compare user patch residence decisions in IFT-optimized interfaces vs randomly designed interfaces; measure user subjective satisfaction vs objective task completion',
    'If autonomous: users benefit from better-designed systems (Rope dominant). If exploited: users are trapped in optimization loops designed for extraction, not their benefit (Snare dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patch_depletion_autonomy, empirical, 'Whether patch residence decisions are autonomous or manipulation-induced').

omega_variable(
    information_scent_transparency,
    'Can information scent design be made transparent to users without losing its coordinative function?',
    'Field trials: introduce explicit disclosure of information architecture design principles; measure whether user awareness of scent-following heuristics changes their information-seeking behavior or satisfaction',
    'If transparency preserves coordination: Rope classification is reinforced (users can make informed choices). If transparency breaks coordination: Rope collapses into Snare (the benefit only existed because users were unaware of manipulation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_scent_transparency, empirical, 'Whether information scent design can remain coordinative under transparency').

omega_variable(
    evolutionary_continuity_assumption,
    'Are information-foraging heuristics genuinely evolved behaviors from ancestral foraging, or are they post-hoc rationalizations of learned search behaviors shaped by modern interface design?',
    'Comparative ethnography: study information-seeking behavior in populations with minimal exposure to digital interface design; measure whether patch depletion and scent-following appear in pre-digital or low-tech contexts',
    'If genuinely evolutionary: IFT is a mountain (immutable cognitive architecture). If largely learned/constructed: IFT is extraction (interface designers have engineered the behavior they then claim to predict).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(evolutionary_continuity_assumption, empirical, 'Whether information-foraging heuristics are evolved or interface-engineered').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_foraging_theory, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ift_tr_t0, information_foraging_theory, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ift_tr_t7, information_foraging_theory, theater_ratio, 7, 0.48).
narrative_ontology:measurement(ift_tr_t14, information_foraging_theory, theater_ratio, 14, 0.58).

% Extraction over time
narrative_ontology:measurement(ift_be_t0, information_foraging_theory, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ift_be_t7, information_foraging_theory, base_extractiveness, 7, 0.3).
narrative_ontology:measurement(ift_be_t14, information_foraging_theory, base_extractiveness, 14, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_foraging_theory, information_standard).
narrative_ontology:affects_constraint(information_foraging_theory, attention_economy_extraction).
narrative_ontology:affects_constraint(information_foraging_theory, user_behavioral_profiling).

% DUAL FORMULATION NOTE:
% IFT is upstream of platform-specific attention extraction mechanisms. IFT provides the theoretical framework that makes attention capture systematic and predictable; the downstream constraints implement IFT principles in specific technological contexts. The network link captures how a descriptive cognitive theory becomes a coordinative blueprint for extractive interface design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(information_foraging_theory, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
