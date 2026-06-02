% ============================================================================
% CONSTRAINT STORY: institutional_truth_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_truth_capacity, []).

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
 *   constraint_id: institutional_truth_capacity
 *   human_readable: Institutional Truth Capacity Constraint
 *   domain: institutional_epistemology/organizational_governance
 *
 * SUMMARY:
 *   Institutional truth capacity is the structural constraint governing how
 *   well knowledge-producing institutions can identify, acknowledge, and
 *   correct errors in their own knowledge bases. The constraint emerges from
 *   the tension between two institutional requirements: (1) maintaining
 *   stable knowledge frameworks that enable coordination and resource
 *   allocation, and (2) remaining open to evidence that contradicts those
 *   frameworks. This tension is not inevitable — it reflects specific
 *   institutional designs that prioritize stability and authority maintenance
 *   over epistemic flexibility. The constraint exhibits the full range of DR
 *   classifications depending on the observer's structural position and time
 *   horizon. At the biographical horizon, institutional leadership sees
 *   coordination (Rope); truth-seeking agents see pure extraction (Snare);
 *   skeptical moderates see mixed extraction and coordination (Tangled Rope).
 *   At the civilizational horizon, the analytical observer risks naturalizing
 *   this arrangement as an immutable law of human cognition. The measurements
 *   show increasing theater_ratio and extractiveness over the interval,
 *   indicating institutional epistemic degradation: the apparatus is
 *   performing governance rituals with decreasing functional connection to
 *   actual truth discovery.
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — maintains authority over approved narratives; captures intellectual property and citation advantages; can exit to other institutions
 *   - Truth-Seeking Agents: Primary victim (powerless/trapped) — structurally dependent on institutional resources; cannot access alternative epistemic systems; bear full cost of suppression
 *   - Established Knowledge Gatekeepers: Secondary beneficiary (institutional/arbitrage) — peer reviewers, editors, credentialing bodies that maintain epistemic authority; benefit from scarcity of publication/credentialing channels
 *   - Institutional Skeptics: Secondary victims (moderate/constrained) — face reputation costs for heterodox claims; constrained by career path dependence within institutions; experience mixed coordination/extraction
 *   - Dissident Coalition: Powerful agents (powerful/mobile) — established researchers with reputational capital; can migrate to alternative institutions or funding; experience extraction but retain exit option
 *   - Epistemic Governance Apparatus: Institutional actor (institutional/arbitrage) — peer review systems, editorial boards, accreditation bodies; maintains performative functions through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks false summit classification by naturalizing institutional arrangements as laws of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_truth_capacity, 0.58).
domain_priors:suppression_score(institutional_truth_capacity, 0.65).
domain_priors:theater_ratio(institutional_truth_capacity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_truth_capacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_truth_capacity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_truth_capacity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_truth_capacity, tangled_rope).
narrative_ontology:human_readable(institutional_truth_capacity, "Institutional Truth Capacity Constraint").
narrative_ontology:topic_domain(institutional_truth_capacity, "institutional_epistemology/organizational_governance").

domain_priors:requires_active_enforcement(institutional_truth_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_truth_capacity, institutional_leadership).
narrative_ontology:constraint_beneficiary(institutional_truth_capacity, established_knowledge_gatekeepers).
narrative_ontology:constraint_victim(institutional_truth_capacity, truth_seeking_agents).
narrative_ontology:constraint_victim(institutional_truth_capacity, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRUTH-SEEKING AGENT (SNARE) — Trapped within institutional epistemic frameworks with no exit option. Cannot access truth outside approved channels. Bears full extraction cost through cognitive suppression and information control. No organization, no alternative, no exit.
constraint_indexing:constraint_classification(institutional_truth_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL SKEPTIC (TANGLED ROPE) — Constrained by career risk and professional credibility costs. Experiences genuine coordination (shared institutional resources enable research) alongside asymmetric extraction (reputation costs for contradicting establishment narratives). Mixed experience — some benefit, significant extraction.
constraint_indexing:constraint_classification(institutional_truth_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Benefits from controlling the truth production apparatus. Experiences the constraint as pure coordination: managing approved narratives enables stable institutional functioning. Net beneficiary with exit options (arbitrage to other institutions or leadership roles).
constraint_indexing:constraint_classification(institutional_truth_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EPISTEMIC GOVERNANCE APPARATUS (PITON) — Peer review systems, editorial boards, institutional review processes maintain their authority through ritualistic performance. The apparatus persists through inertia (institutional sunk costs) even as its actual truth-detection capacity degrades. Theater ratio (0.68) reflects that many epistemic gatekeeping functions are now substantially performative rather than functionally tied to truth discovery.
constraint_indexing:constraint_classification(institutional_truth_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DISSIDENT COALITION (TANGLED ROPE) — Powerful but mobile actors (established researchers with reputation capital, well-funded alternative institutions) can escape institutional truth suppression through exit or creating parallel epistemic systems. They experience both the suppression (institutional pressure against heterodox claims) and genuine coordination benefits (institutional infrastructure for research). Their exit option (mobile) makes extraction less severe than trapped agents experience.
constraint_indexing:constraint_classification(institutional_truth_capacity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing institutional truth suppression as an immutable law of human cognition and coordination: 'institutions always suppress inconvenient truths because groups have collective cognitive biases.' This perspective treats contingent institutional arrangements as inherent to the human condition. However, comparative institutional analysis reveals this naturalization is false — some institutional designs produce better truth capacity than others.
constraint_indexing:constraint_classification(institutional_truth_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_truth_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_truth_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_truth_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_truth_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_truth_capacity, TR),
    TR >= 0.70.

:- end_tests(institutional_truth_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Institutional truth capacity constraints extract value through several mechanisms: (1) controlling access to publication channels (career-critical scarce resource), (2) requiring alignment with established narratives as condition for credibility and funding, (3) creating information asymmetries where institutional gatekeepers know more than truth-seeking agents about what alternative claims exist, (4) generating switching costs (agents abandoning institutional affiliations lose access to resources, equipment, prestige). The constraint is not as severe as a pure snare (0.70+) because some alternative epistemic systems exist and some powerful agents can exit. Suppression (0.65): Moderate-high. Multiple suppression mechanisms operate: publication bias against contradictory findings, career penalties for heterodox claims, access restrictions (institutional agents can suppress information from non-members), institutional credentials required to participate in truth-seeking (PhD, journal access, lab affiliation), and internalized suppression (agents have adopted institutional narratives). Theater ratio (0.68): High. Peer review, ethics review, editorial oversight, and accreditation all perform governance rituals with decreasing functional connection to truth discovery. The theater has increased as: (1) institutions formalized review procedures, (2) epistemic complexity outpaced reviewer capacity, (3) performance metrics (citation counts, impact factors) displaced truth-assessment as the actual goal, (4) institutions prioritized legitimacy maintenance over accuracy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits textbook perspectival gap. The beneficiary and victim see structurally opposite classifications from the same data. Leadership claims the constraint is purely coordinating (Rope) — 'institutional review prevents low-quality claims from contaminating the literature.' Truth-seeking agents claim it is purely extracting (Snare) — 'institutional review prevents dissent from contaminating the approved narrative.' Both are partially right: the constraint does coordinate (prevent chaos) AND extracts (suppress dissent). The tangled rope classification at moderate power captures this hybrid structure. The skeptics experience the hybrid directly — they get institutional resources but pay reputation costs. The apparatus itself is degraded (Piton) — it maintains authority through ritual rather than function. The dissidents see a sunset (Scaffold at longer horizons) as alternative epistemic systems (prediction markets, decentralized science, blockchain reputation) mature. The analytical observer's natural law view is the false summit: treating institutional truth suppression as 'inherent to human coordination' instead of recognizing it as a contingent product of specific institutional designs (peer review gatekeeping, credentialing monopolies, publication channel scarcity).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain operates as follows: (1) Institutional leadership is declared as beneficiary — they gain authority, citations, funding advantage by controlling approved narratives. This produces low d (≈ 0.05 from beneficiary + arbitrage exit → f(d) ≈ -0.12 → negative chi). (2) Truth-seeking agents are declared as victim — they depend entirely on institutional access, cannot exit, bear full suppression cost. This produces high d (≈ 0.95 from victim + trapped exit → f(d) ≈ 1.42 → high chi). (3) Institutional skeptics experience both (victim from reputation costs, beneficiary from research access) with constrained exit → moderate d (≈ 0.60 → f(d) ≈ 0.65 → moderate chi). (4) Dissident powerful agents experience extraction pressure (suppression) but also benefit from institutional infrastructure with mobile exit → symmetric d (≈ 0.50 → f(d) ≈ 0.65 → moderate chi). The formula χ = ε × f(d) × σ(S) shows why different agents classify differently: ε (0.58) is constant, but f(d) varies dramatically based on exit options and beneficiary/victim status. Leadership experiences negative chi (the constraint subsidizes them). Victims experience high chi (the constraint extracts maximum). Moderates and skeptics experience moderate chi. All from the same ε.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED via perspectival indexing: The mandatrophy — is institutional truth suppression a coordination mechanism (Rope/Scaffold) or an extraction mechanism (Snare/Piton)? — is resolved by recognizing that both classifications are structurally correct from different observation positions. The constraint genuinely coordinates at some level (prevents publication chaos, enables credentialing, provides legitimation infrastructure). It genuinely extracts at other levels (suppresses heterodox claims, creates information asymmetries, generates switching costs). The tangled rope classification captures this: 0.40 ≤ χ ≤ 0.90 (moderate-to-high extraction) with beneficiaries AND victims AND active enforcement. The false summit (mountain classification from the analytical observer) would misclassify the constraint as immutable when it is actually contingent — different institutional designs produce different truth capacities. The piton classification (epistemic apparatus) reveals that the constraint persists through inertia (theater_ratio = 0.68) despite functional degradation, which is a diagnostic signal: the apparatus is maintaining authority through ritual rather than truth-discovery effectiveness. The scaffold perspective from the dissident coalition at the generational horizon captures the real exit path: alternative epistemic systems are building parallel channels (prediction markets, decentralized science, citizen science) that bypass institutional gatekeeping. As these mature, the institutional monopoly erodes — the constraint transitions from Snare/Tangled Rope to Scaffold (temporary with sunset).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_capture_vs_coordination,
    'Is institutional truth suppression a structural feature of all knowledge-producing institutions or a contingent property of specific institutional designs?',
    'Comparative analysis of epistemic capacity across institutional types (peer review vs prediction markets vs decentralized science vs corporate R&D vs academic departments). Measurement of truth-detection rates, correction mechanisms, and openness to heterodox claims across design variants.',
    'If structural: institutions are inherently extractive on truth (snare/tangled_rope across all contexts). If contingent: institutional design matters — some variants approach rope-level coordination with minimal extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_vs_coordination, empirical, 'Whether truth suppression is intrinsic to institutions or design-specific').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is suppression of truth-seeking primarily structural (external barriers: publication bias, career penalties, access restrictions) or internalized (cognitive capture: agents have adopted institutional narratives as their own beliefs)?',
    'Exit rate analysis: do agents who leave institutions rapidly adopt different truth claims? Comparison of internal dissent (what agents believe privately) vs public conformity. Documentation of conversion experiences post-exit.',
    'If structural: suppression persists only through institutional mechanisms (can be dissolved by changing institutions). If internalized: agents carry suppression with them post-exit (requires cognitive reframing, not institutional change).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    parallel_epistemic_systems_viability,
    'Can alternative epistemic systems (prediction markets, blockchain-based reputation, citizen science, decentralized protocols) actually produce comparable truth-detection capacity to institutionalized systems?',
    'Long-term comparative track records: accuracy, self-correction rate, resistance to contamination, scalability, convergence to settled truth across different epistemic systems over 10+ year horizons.',
    'If viable: scaffold perspective is correct — migration to alternative systems has sunset logic and represents real exit. If unviable: the institutional monopoly is harder to escape than dissident perspectives suggest (snare extraction persists even for powerful agents).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(parallel_epistemic_systems_viability, empirical, 'Whether parallel epistemic systems can match institutional truth capacity').

omega_variable(
    theater_ratio_causality,
    'Does increasing theater ratio (performative epistemics) cause truth capacity to decline, or does rising epistemic uncertainty cause institutions to increase performative elements as legitimation?',
    'Time-series analysis with causal inference: examine whether theater_ratio changes precede or follow truth-capacity degradation. Natural experiments where institutions suddenly increase performative requirements (new review rituals, compliance theaters).',
    'If theater causes decline: piton classification is early warning (degradation underway). If uncertainty causes theater: piton classification is symptom (degradation already occurred).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_causality, empirical, 'Causal direction between theater and truth capacity decline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_truth_capacity, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(itc_tr_t0, institutional_truth_capacity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(itc_tr_t3, institutional_truth_capacity, theater_ratio, 3, 0.52).
narrative_ontology:measurement(itc_tr_t6, institutional_truth_capacity, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(itc_be_t0, institutional_truth_capacity, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(itc_be_t3, institutional_truth_capacity, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(itc_be_t6, institutional_truth_capacity, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_truth_capacity, identity_coordination).
narrative_ontology:affects_constraint(institutional_truth_capacity, credentialing_monopoly).
narrative_ontology:affects_constraint(institutional_truth_capacity, publication_channel_gatekeeping).
narrative_ontology:affects_constraint(institutional_truth_capacity, peer_review_theater).

% DUAL FORMULATION NOTE:
% Institutional truth capacity is upstream of three more specific constraints: credentialing systems that enforce conformity, publication gatekeeping that suppresses dissent, and peer review ritual that maintains appearance of function. Each downstream constraint has its own ε value reflecting the specific extraction mechanism (credentialing creates switching costs, gatekeeping creates information asymmetries, peer review creates authority suppression). The institutional truth capacity story models the general coordination-extraction hybrid; downstream stories detail domain-specific manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_truth_capacity, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
