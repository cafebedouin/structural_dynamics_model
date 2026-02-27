% ============================================================================
% CONSTRAINT STORY: legal_formalism_overhang
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_formalism_overhang, []).

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
 *   constraint_id: legal_formalism_overhang
 *   human_readable: The Ghost of Statutes Past: Legal Formalism Overhang
 *   domain: political/legal/social
 *
 * SUMMARY:
 *   Legal formalism overhang describes a structural constraint in which the
 *   literal text of historical statutes persists as a binding requirement
 *   even after the social, technological, or economic context that originally
 *   justified the statute has vanished. A statute drafted to regulate
 *   horse-drawn carriages, telegraph operations, or factory working
 *   conditions continues to govern behavior through mechanical application of
 *   its literal words, regardless of whether the constraint still serves the
 *   legislature's original intent. This creates a coordination/extraction
 *   hybrid: statutes do provide genuine coordination value (predictable,
 *   stable legal standards that enable economic actors to plan), but the
 *   overhang also enables gatekeeping behavior (regulatory agencies and
 *   litigation specialists can selectively enforce archived requirements as a
 *   cover for discretionary power). The theater ratio (0.75) reflects that
 *   formalist interpretation presents itself as neutral rule-following while
 *   masking interpretive choices and enforcement selectivity. The constraint
 *   exhibits rising extractiveness over time (0.22 → 0.38) as the gap between
 *   statute text and modern context widens, making literal compliance
 *   increasingly costly and gatekeepers' interpretive discretion increasingly
 *   valuable. The reform coalition (organized/mobile) experiences the
 *   overhang as a scaffold with an achievable sunset — legislative amendment,
 *   regulatory modernization, and statutory safe harbors are all building
 *   alternative pathways. But the powerless economic actor (stuck in literal
 *   compliance) experiences it as a snare: no legal exit exists except
 *   through legislative repeal, which moves slowly.
 *
 * KEY AGENTS:
 *   - Adaptive Economic Actors (powerless/trapped): Small businesses, entrepreneurs, platform operators who cannot absorb legal risk from non-compliance. Bear full cost of literal statute compliance even when original purpose is extinct. Experience extraction without exit mechanism.
 *   - Regulatory Gatekeepers (institutional/arbitrage): Agencies, prosecutors, enforcement bodies that benefit from formalism. Literal text provides legal cover for enforcement decisions and discretionary selectivity. Can choose enforcement intensity.
 *   - Litigation Specialists (institutional/arbitrage): Lawyers, compliance consultants, corporate counsel who benefit from the complexity and ambiguity of living under dead statutes. Build billable hours and competitive moats through specialized interpretation knowledge.
 *   - Compliance Professionals (moderate/constrained): In-house compliance officers and regulatory counselors who experience both coordination (clarifying ambiguous text protects clients) and extraction (billable complexity). Constrained but can arbitrage between strict and practical interpretations.
 *   - Reform Coalition (organized/mobile): Good-government groups, business associations, modernization-focused legislators pushing for statutory amendment, regulatory reinterpretation, and safe harbor legislation. Have agency to build sunset mechanisms.
 *   - Judicial System (institutional/arbitrage): Courts that maintain formalist interpretation through institutional inertia. High theater: mechanical rule-following is performed but masks discretion in enforcement selectivity.
 *   - Analytical Observer (analytical/analytical): Civilizational perspective that sees formalism as genuine coordination mechanism WITH extraction function, rejecting both pure-coordination and pure-snare framings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_formalism_overhang, 0.38).
domain_priors:suppression_score(legal_formalism_overhang, 0.62).
domain_priors:theater_ratio(legal_formalism_overhang, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_formalism_overhang, extractiveness, 0.38).
narrative_ontology:constraint_metric(legal_formalism_overhang, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legal_formalism_overhang, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_formalism_overhang, tangled_rope).
narrative_ontology:human_readable(legal_formalism_overhang, "The Ghost of Statutes Past: Legal Formalism Overhang").
narrative_ontology:topic_domain(legal_formalism_overhang, "political/legal/social").

domain_priors:requires_active_enforcement(legal_formalism_overhang).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_formalism_overhang, regulatory_gatekeepers).
narrative_ontology:constraint_beneficiary(legal_formalism_overhang, litigation_specialists).
narrative_ontology:constraint_victim(legal_formalism_overhang, adaptive_economic_actors).
narrative_ontology:constraint_victim(legal_formalism_overhang, regulatory_compliance_sector).
narrative_ontology:constraint_victim(legal_formalism_overhang, innovation_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADAPTIVE ECONOMIC ACTOR (SNARE) — Small firms, entrepreneurs, and platform operators cannot exit literal statute compliance without legal risk. Even when statute's original purpose is extinct, failure to comply invites enforcement action. No exit mechanism exists except legislative repeal. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(legal_formalism_overhang, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPLIANCE PROFESSIONAL (TANGLED ROPE) — Legal counselors, compliance officers, and regulatory consultants experience genuine coordination value (clarifying ambiguous statute text protects clients) AND extraction (high billable hours for navigating vestigial requirements). Constrained exit: cannot ignore statute but can somewhat arbitrage between strict and practical interpretations. d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.28.
constraint_indexing:constraint_classification(legal_formalism_overhang, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY GATEKEEPER (ROPE) — Agencies and enforcement bodies benefit from statute formalism: literal text provides legal cover for enforcement decisions, eliminates interpretive discretion that could be challenged, and maintains the gatekeeping function (discretionary exemptions for favored actors). Arbitrage exit: can choose enforcement intensity and selectivity. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.03. Net beneficiary.
constraint_indexing:constraint_classification(legal_formalism_overhang, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized actors (good-government groups, modernization-focused legislators, business associations) see formalism as a temporary problem solvable through statutory amendment, regulatory reinterpretation, or safe harbor legislation. Mobile exit: reform is achievable within political cycles. Theater ratio declines as reforms accumulate. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.16. Low extraction because the coalition has agency and sees a sunset mechanism.
constraint_indexing:constraint_classification(legal_formalism_overhang, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL SYSTEM (PITON) — Courts maintain formalist interpretation (statutes mean what they say, regardless of context) through institutional inertia and interpretive tradition, not because the doctrine optimally serves coordination. Literal reading is performatively neutral — judges claim to be 'merely applying the law' — but the theater is high (0.75+): the appearance of mechanical rule-following masks discretion in which statutes to enforce and which actors to target. theater_ratio=0.75 satisfies piton gate. The institution recognizes its own doctrine as degraded when confronted with absurd results, but changes slowly.
constraint_indexing:constraint_classification(legal_formalism_overhang, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/cross-jurisdictional view, legal formalism serves a genuine coordination function (stable interpretation prevents jurisdictional forum-shopping and enables predictability across contexts) AND enables extraction (gatekeepers can selectively enforce, compliance professionals build moats through specialized knowledge, regulatory inertia persists longer than it should). Neither pure mountain nor pure snare: formalism is a real institutional solution WITH real costs.
constraint_indexing:constraint_classification(legal_formalism_overhang, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_formalism_overhang_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legal_formalism_overhang, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legal_formalism_overhang, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(legal_formalism_overhang, TR),
    TR >= 0.70.

:- end_tests(legal_formalism_overhang_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint does extract value from adaptive actors to gatekeepers, but not all of the compliance cost is extraction — some reflects the legitimate coordination value of stable, predictable legal standards. The constraint's primary extraction mechanism is not the statute itself but the discretionary enforcement selectivity it enables. Over 30 years, extractiveness rises from 0.22 to 0.38 as the context gap widens, making literal compliance increasingly costly relative to the benefit of legal stability. Suppression (0.62): Moderate-high. Actors face significant barriers to exit: legal risk of non-compliance, lack of clear reinterpretation authority, uncertainty about which interpretations will survive court challenge, legislative gridlock preventing amendment. But suppression is not total — professional expertise, lobbying, and incremental regulatory reinterpretation provide partial exits. Theater ratio (0.75): High and rising. Formalist interpretation claims to be mechanical rule-following ('we must enforce what the statute says') while masking the discretion in which statutes to enforce, which actors to target, and how aggressively to prosecute. The performative neutrality increases as the gap between statute and context grows — the more obviously a statute is anachronistic, the more the 'mere application of law' framing is performative.
 *
 * PERSPECTIVAL GAP:
 *   The original research group sees coordination (Rope) — formalism is a solution to interpretive instability. The reform coalition sees a temporary problem (Scaffold) — statutory amendment and safe harbors can solve it within one generation. The judicial system sees its own degraded ritual (Piton) — formalist interpretation persists through inertia despite obvious absurdities. Compliance professionals see mixed coordination and extraction (Tangled Rope) — the system both enables them to serve clients and extracts through specialized knowledge. Adaptive economic actors see pure extraction (Snare) — literal compliance imposes costs they cannot avoid and cannot challenge. The analytical observer sees that all these perspectives are accurate structural descriptions of different aspects of the same constraint: formalism genuinely coordinates (stable text, predictable application across jurisdictions) AND genuinely extracts (selective enforcement, barrier to entry, gatekeeping).
 *
 * DIRECTIONALITY LOGIC:
 *   Adaptive economic actors: Victim + trapped → d≈0.93, f(d)≈1.40. No exit mechanism except legislative repeal. Maximum extraction experience. Regulatory gatekeepers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Selective enforcement discretion is valuable, legal cover enables gatekeeping. Net beneficiaries. Litigation specialists: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. High billable hours from navigating formalism. Net beneficiaries. Compliance professionals: Victim + constrained → d≈0.58, f(d)≈0.72. Mixed experience: coordination value (protect clients) + extraction (complexity). Can arbitrage between interpretations but face constraint. Reform coalition: Organized + mobile → d≈0.42, f(d)≈0.42. Have agency to change statutes; exit path visible. Low effective extraction. Judicial system: Institutional + arbitrage → d≈0.08, f(d)≈-0.08. Institutional beneficiary; discretion in enforcement. Analytical observer: d≈0.50, f(d)≈0.65. Sees both sides of the hybrid equally.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by explicitly modeling formalism as a genuine coordination mechanism that has been captured for extraction purposes. The tangled_rope classification (claimed_type) reflects that the statutory text IS a real coordinating standard (enables predictability, prevents forum-shopping, provides stable baseline for economic planning) AND IS captured as an extraction mechanism (gatekeepers selectively enforce, compliance specialists profit, adaptive actors bear costs). The mandatrophy is resolved by refusing to choose between 'formalism is just a technical tool' (rope) and 'formalism is just gatekeeping' (snare). Both are structurally true. The constraint's evolution over 30 years shows the risk of mandatrophy deepening: as context gap widens, the coordination function stays constant but the extraction function grows (ε rises from 0.22 to 0.38, theater rises from 0.52 to 0.75). This is the classic Goodhart/mandatrophy trajectory: as the metric (literal statute text) becomes more obviously divorced from the original objective (effective regulation), gatekeepers rely on it more, not less, because the mismatch becomes the primary tool of discretion. Reform depends on deliberately rebuilding the coordination function through safe harbors and reinterpretation WHILE constraining the extraction function through transparency and deterministic enforcement standards.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    context_dissolution_threshold,
    'What degree of social/technological context collapse justifies superseding a statute''s literal text? At what point does the original legislative purpose become empirically inaccessible?',
    'Historical legislative record analysis; identification of intent markers in statute preambles and committee reports; comparative analysis of statutory purpose across equivalent jurisdictions',
    'If threshold is low (context change easily justifies reinterpretation): formalism appears as pure extraction (Snare for moderate actors). If threshold is high (original context must be nearly unrecognizable): formalism appears as coordination (Rope from analyst''s view). This omega separates ''strict textualism is honest'' from ''strict textualism is a cover for gatekeeping.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_dissolution_threshold, conceptual, 'Threshold for context dissolution justifying statutory reinterpretation').

omega_variable(
    safe_harbor_feasibility,
    'Can legislative safe harbors or regulatory reinterpretation effectively sunset the formalism overhang, or does statutory text entrench itself faster than reforms can move?',
    'Time-series analysis of statute age vs. amendment frequency; comparative study of jurisdictions with active reinterpretation protocols vs. strict textualist systems; measurement of reform lag between problem identification and statute amendment',
    'If safe harbors work: scaffold perspective is accurate and sunset is real (15-30 year decline). If entrenchment is faster: scaffold is aspirational, formalism is more snare-like, and theater_ratio grows over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safe_harbor_feasibility, empirical, 'Whether legislative safe harbors can effectively sunset formalism').

omega_variable(
    gatekeeper_selectivity_evidence,
    'Do regulatory gatekeepers enforce outdated statutes selectively, using literal text as cover for discretionary targeting of disfavored actors?',
    'Analysis of enforcement patterns: correlation between actor characteristics (size, political affiliation, insider status) and citation frequency; comparison of severity of charges for identical behaviors across different actors; examination of prosecutorial discretion in choosing which outdated statutes to enforce',
    'If selectivity is high: formalism is primarily an extraction mechanism, moderate actors experience it as snare-like, and suppression is structural (not just lack of awareness). If selectivity is low (random or deterministic based on behavior alone): formalism is closer to pure coordination, and the snare classification for powerless actors is about incidental exposure rather than targeting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gatekeeper_selectivity_evidence, empirical, 'Evidence of selective enforcement of outdated statutes by gatekeepers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_formalism_overhang, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lfo_tr_t0, legal_formalism_overhang, theater_ratio, 0, 0.52).
narrative_ontology:measurement(lfo_tr_t15, legal_formalism_overhang, theater_ratio, 15, 0.68).
narrative_ontology:measurement(lfo_tr_t30, legal_formalism_overhang, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(lfo_be_t0, legal_formalism_overhang, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(lfo_be_t15, legal_formalism_overhang, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(lfo_be_t30, legal_formalism_overhang, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_formalism_overhang, enforcement_mechanism).
narrative_ontology:affects_constraint(legal_formalism_overhang, interpretive_discretion_moat).
narrative_ontology:affects_constraint(legal_formalism_overhang, legislative_lag_asymmetry).
narrative_ontology:affects_constraint(legal_formalism_overhang, gatekeeping_via_technicality).

% DUAL FORMULATION NOTE:
% Legal formalism overhang is the parent constraint in a family. Interpretive discretion moat (how gatekeepers build competitive advantage from formalism), legislative lag asymmetry (why statutes update slower than context), and gatekeeping via technicality (how enforcement selectivity exploits literal text) are downstream constraints that represent specialized mechanisms within the broader formalism overhang.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_formalism_overhang, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
