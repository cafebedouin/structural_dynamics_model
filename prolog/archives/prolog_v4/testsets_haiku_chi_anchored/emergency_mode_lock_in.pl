% ============================================================================
% CONSTRAINT STORY: emergency_mode_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emergency_mode_lock_in, []).

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
 *   constraint_id: emergency_mode_lock_in
 *   human_readable: The Perpetual Crisis Mandate
 *   domain: political/organizational
 *
 * SUMMARY:
 *   The Perpetual Crisis Mandate represents the institutional pathology where
 *   temporary emergency governance mechanisms become permanent operating
 *   substrates. What began as a Rope-like coordination mechanism for rapid
 *   crisis response — suspending normal deliberative procedures to enable
 *   speed of decision — calcifies into a Tangled Rope system combining
 *   genuine executive coordination function with significant extraction of
 *   democratic deliberation and rule-of-law protections. The constraint
 *   exhibits institutional lock-in dynamics: emergency protocols create
 *   infrastructure (surveillance systems, executive precedents, threat
 *   narratives) that becomes difficult to dismantle even when the triggering
 *   crisis subsides. New crises (real or rhetorically constructed)
 *   continuously justify renewal rather than sunsetting. The judicial system
 *   degrades into a Piton — courts continue to operate and issue rulings
 *   within the emergency framework, but their capacity to constrain executive
 *   authority has eroded. Citizens experience this as Snare: trapped in
 *   permanent legal regimes that suspend normal protections. The security
 *   apparatus and executive authority experience it as Rope: legitimate
 *   coordination. Civil society faces Tangled Rope: restricted but also
 *   visible, receiving emergency grants alongside repression. The analytical
 *   observer sees a self-perpetuating cycle where the extraction mechanism
 *   (authority concentration, oversight erosion) has colonized the
 *   coordination function (crisis response), and institutional reversibility
 *   is questionable.
 *
 * KEY AGENTS:
 *   - Ordinary Citizens: Primary victims (powerless/trapped) — bear full cost of suspended protections and accelerated authority; cannot exit or organize meaningful challenge
 *   - Opposition Political Parties: Secondary victims (moderate/constrained) — silenced by emergency restrictions on assembly/speech; cannot credibly mobilize during 'crisis'
 *   - Independent Media: Mixed agent (powerful/constrained) — constrained by emergency reporting restrictions but benefits from crisis narratives driving audience engagement
 *   - Executive Authority: Primary beneficiary (institutional/arbitrage) — gains speed of decision and reduced deliberative friction; can invoke emergency to bypass normal constraints
 *   - Security Apparatus: Primary beneficiary (institutional/arbitrage) — expanded jurisdiction, reduced oversight, emergency funding; views rule-of-law constraints as friction to overcome
 *   - Judicial System: Degraded institution (institutional/constrained) — retains formal role but substantive constraint capacity eroded; performatively continues while functionally collapsed
 *   - Civil Society Organizations: Organized secondary victim (organized/constrained) — restricted assembly/protest but visible through emergency grants; mixed extraction-coordination
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — reveals permanent lock-in dynamics and self-perpetuating legitimacy cycles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergency_mode_lock_in, 0.52).
domain_priors:suppression_score(emergency_mode_lock_in, 0.68).
domain_priors:theater_ratio(emergency_mode_lock_in, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergency_mode_lock_in, extractiveness, 0.52).
narrative_ontology:constraint_metric(emergency_mode_lock_in, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(emergency_mode_lock_in, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergency_mode_lock_in, tangled_rope).
narrative_ontology:human_readable(emergency_mode_lock_in, "The Perpetual Crisis Mandate").
narrative_ontology:topic_domain(emergency_mode_lock_in, "political/organizational").

domain_priors:requires_active_enforcement(emergency_mode_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergency_mode_lock_in, executive_authority).
narrative_ontology:constraint_beneficiary(emergency_mode_lock_in, security_apparatus).
narrative_ontology:constraint_beneficiary(emergency_mode_lock_in, crisis_management_bureaucracy).
narrative_ontology:constraint_victim(emergency_mode_lock_in, democratic_deliberation).
narrative_ontology:constraint_victim(emergency_mode_lock_in, rule_of_law_institutions).
narrative_ontology:constraint_victim(emergency_mode_lock_in, public_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY CITIZEN (SNARE) — Trapped in perpetual emergency legal regimes. Cannot exit the suspension of normal procedural protections; bears full cost of accelerated authority and reduced oversight. No access to the deliberative mechanisms that justify emergency measures. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(emergency_mode_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION PARTY (SNARE) — Constrained by the same emergency protocols that silence their objections. Cannot credibly mobilize constituent base when emergency measures prohibit assembly/speech. Career risk of appearing obstructionist during 'crisis'. d≈0.82, f(d)≈1.20, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(emergency_mode_lock_in, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INDEPENDENT MEDIA (TANGLED ROPE) — Constrained by emergency restrictions on reporting (security rationales, operational sensitivity claims). Also benefits from emergency narratives (crisis drives audience attention, creates real stories worth covering). Mixed extraction and coordination: media can only report within emergency frames, but crisis coverage is their most valued function. d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(emergency_mode_lock_in, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE AUTHORITY (ROPE) — Net beneficiary. Emergency powers enable rapid crisis response (genuine coordination function: decision speed over consensus). Can exit normal deliberation through emergency invocation. Experiences constraint as coordination: 'we need fast decisions in crisis'. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Negative effective extraction = organizational benefit.
constraint_indexing:constraint_classification(emergency_mode_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SECURITY APPARATUS (ROPE) — Beneficiary through expanded jurisdiction, reduced oversight, and emergency funding. Views constraint as coordination: 'emergency protocols enable protective action'. Experiences normal rule-of-law constraints as friction. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(emergency_mode_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: JUDICIAL SYSTEM (PITON) — Degraded institution. Formally retains role but function atrophied: emergency law bypasses normal adjudication, executive certification replaces judicial review. Theater persists (courts still operate, still issue rulings) but substantive power has decayed. theater_ratio≈0.81 reflects performative continuity (courts exist, follow procedures) masking functional collapse (judges cannot block emergency measures). d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.49.
constraint_indexing:constraint_classification(emergency_mode_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CIVIL SOCIETY (TANGLED ROPE) — Organized but constrained. Emergency protocols restrict assembly, protest, fundraising. Also benefit from crisis visibility and emergency grant funding. Mixed: extraction through constraint + coordination through legitimacy. No sunset clause recognized. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(emergency_mode_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilizational view reveals permanent institutional lock-in. Emergency protocols create self-perpetuating legitimacy cycles: escalating threat rhetoric justifies emergency measures → emergency measures create surveillance/control infrastructure → infrastructure justifies ongoing threat perception → cycle repeats. Genuine coordination function (crisis response) has been permanently colonized by extraction mechanism (authority concentration, oversight erosion). d≈0.60, f(d)≈0.75, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(emergency_mode_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_mode_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emergency_mode_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_mode_lock_in, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emergency_mode_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emergency_mode_lock_in, TR),
    TR >= 0.70.

:- end_tests(emergency_mode_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Executive authority gains decision speed and reduced oversight (coordination benefit = 0.28 baseline). But the secondary effect is extraction of democratic deliberation and erosion of rule-of-law constraints (additional extraction = 0.24). The trajectory shows escalation: crisis initially justified by genuine threats, but over time threat narratives are amplified to rationalize ongoing emergency invocation. By T=10, extractive component (0.52) exceeds coordination component (implied ~0.28). Suppression (0.68): High. Emergency protocols directly suppress opposition speech, assembly, and protest. Suppress judicial review through executive certification. Suppress press through 'operational sensitivity' restrictions. Create information asymmetries preventing public assessment of emergency justification. Yet not total suppression — some institutions (courts, media, opposition) continue operating within emergency frames. Theater ratio (0.81): Very high. Courts still issue rulings but cannot constrain emergency measures (Piton signature). Legislative oversight continues but lacks enforcement power. Media operates but under emergency reporting restrictions. Democratic forms (voting, debate, legal process) persist but within constrained emergency parameters. Goodhart drift: metrics of institutional health (court rulings, legislative sessions, media reports) increase even as their constraining power decays. The trajectory (0.35→0.81) reflects the degradation of substance into pure performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the transformation from Rope (genuine coordination) into Tangled Rope (mixed coordination/extraction) and toward Piton (degraded institutions maintained through theater). The executive authority sees pure Rope: emergency protocols solve the real problem of needing rapid crisis response. The security apparatus agrees: emergency enables protective action. The ordinary citizen sees Snare: trapped in suspended protections with no exit. The opposition party sees Snare: silenced constraints prevent mobilization. The judicial system sees Piton: retains form but has lost substance. Independent media sees Tangled Rope: constrained by restrictions but benefits from crisis narratives. The analytical observer sees a self-perpetuating extraction mechanism colonizing coordination function: what began as legitimate temporary speedup has become permanent authority concentration justified by perpetual threat rhetoric. The perspectival gaps widen over time as theater ratio increases and extractiveness accumulates.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary experiencing rapid decision-making as coordination. Security apparatus: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary through expanded jurisdiction. Ordinary citizen: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction — no exit option. Opposition party: Victim + constrained → d≈0.82, f(d)≈1.20. High extraction; can theoretically challenge but strategic risk makes exit effectively blocked. Independent media: Mixed + constrained → d≈0.58, f(d)≈0.72. Moderate extraction offset by crisis-driven audience value. Judicial system: Victim + constrained → d≈0.65, f(d)≈0.95. Cannot exit constraint framework; institutional role requires participation in procedures that no longer constrain. Civil society: Organized + constrained → d≈0.50, f(d)≈0.65. Mixed extraction (restrictions) and coordination (visibility/funding). Analytical observer: d≈0.60, f(d)≈0.75. Sees structural colonization of coordination by extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL UNRESOLVED MANDATROPHY: This constraint sits at ε=0.52, extractiveness > 0.46, requiring mandatrophy resolution yet marked as `mandatrophy_resolved: false`. The unresolved mandatrophy reveals the core ambiguity: Is this a Rope that has degraded into Snare (extraction mechanism colonizing coordination)? Or is this a legitimate Tangled Rope where genuine coordination (rapid crisis response) coexists with real extraction (oversight reduction)? The mandatrophy cannot be resolved without empirical determination of omega variables, particularly crisis_reality_vs_narrative and alternative_crisis_coordination. If alternative mechanisms (rapid legislative process, pre-authorized frameworks, sunset-clause emergency powers) can provide adequate crisis coordination, then the perpetual emergency is pure extraction (Snare). If they cannot, then the Tangled Rope classification is justified (genuine mixed coordination-extraction). The theater ratio escalation (0.35→0.81) is a Goodhart drift signal: courts, legislatures, media continue to operate (metrics up) but their constraining power decays (substance down). The Piton perspective reveals the degradation. The constraint illustrates how Rope-Snare confusion can persist indefinitely if threat narratives prevent testing alternative coordination mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    crisis_reality_vs_narrative,
    'Does the claimed crisis threat level justify the scope of emergency measures, or has the threat been narratively inflated to rationalize permanent executive expansion?',
    'Comparative analysis: actual incidents requiring emergency response vs. invoked emergency frequency; independent threat assessment vs. executive threat narratives; temporal correlation between threat perception and emergency invocation patterns',
    'If actual threats justify measures: constraint might revert to temporary Scaffold under different threat assessment. If threats are inflated: constraint is revealed as pure extraction masked by crisis narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_reality_vs_narrative, empirical, 'Whether crisis threat level justifies emergency measure scope').

omega_variable(
    institutional_reversibility,
    'Can institutions (judicial review, legislative oversight, constitutional limits) be restored once eroded by perpetual emergency, or does the infrastructure of emergency power create path-dependent lock-in that makes reversal infeasible?',
    'Historical case studies of post-emergency institutional recovery; analysis of whether courts/legislatures retain capacity to challenge executive emergency claims; modeling of information asymmetries that prevent public assessment of emergency justification',
    'If reversible: constraint is Scaffold with failed sunset (high theater but mechanically restorable). If path-dependent lock-in: constraint is Piton masquerading as Tangled Rope (no genuine sunset possibility).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_reversibility, conceptual, 'Whether institutional erosion from emergency is reversible').

omega_variable(
    public_preference_for_authority,
    'Does the public genuinely prefer accelerated executive authority during crisis, or is consent manufactured through information control and threat amplification embedded in emergency protocols?',
    'Surveys of preference for emergency powers during crisis vs. non-crisis periods; analysis of information access under emergency restrictions; comparison of public threat assessment vs. independent threat metrics; polling during brief windows of non-emergency information access',
    'If genuine preference: constraint resembles legitimate Rope/Scaffold (public trade-off between deliberation and speed). If manufactured consent: constraint is pure Snare disguised as Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_preference_for_authority, preference, 'Whether public preference for authority is genuine or manufactured').

omega_variable(
    alternative_crisis_coordination,
    'Do non-emergency institutional mechanisms (rapid legislative process, pre-authorized frameworks, delegated authority with sunset clauses) enable adequate crisis response, or is perpetual emergency genuinely necessary for effective security?',
    'Comparative institutional analysis: jurisdictions with sunset-clause emergency powers vs. perpetual emergency; simulation of non-emergency crisis response protocols; analysis of whether emergency measures actually achieve stated security outcomes',
    'If alternatives work: perpetual emergency is pure extraction (Snare). If no alternatives work: emergency is legitimate coordination necessity (Rope/Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_crisis_coordination, empirical, 'Whether non-emergency mechanisms can provide adequate crisis coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergency_mode_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emerg_tr_t0, emergency_mode_lock_in, theater_ratio, 0, 0.35).
narrative_ontology:measurement(emerg_tr_t5, emergency_mode_lock_in, theater_ratio, 5, 0.58).
narrative_ontology:measurement(emerg_tr_t10, emergency_mode_lock_in, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(emerg_be_t0, emergency_mode_lock_in, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(emerg_be_t5, emergency_mode_lock_in, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(emerg_be_t10, emergency_mode_lock_in, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergency_mode_lock_in, enforcement_mechanism).
narrative_ontology:affects_constraint(emergency_mode_lock_in, rule_of_law_institutional_decay).
narrative_ontology:affects_constraint(emergency_mode_lock_in, surveillance_infrastructure_path_dependence).
narrative_ontology:affects_constraint(emergency_mode_lock_in, democratic_deliberation_suspension).
narrative_ontology:affects_constraint(emergency_mode_lock_in, threat_narrative_inflation_dynamics).

% DUAL FORMULATION NOTE:
% The Perpetual Crisis Mandate is downstream of specific crises (real or perceived) but represents a distinct structural constraint on institutional governance. Related constraints: rule_of_law_institutional_decay focuses on erosion of judicial constraint capacity; surveillance_infrastructure_path_dependence models the technical lock-in preventing post-emergency reversal; democratic_deliberation_suspension examines the temporary-to-permanent transition in procedural changes; threat_narrative_inflation_dynamics models the legitimacy cycles that justify emergency renewal. Emergency_mode_lock_in integrates all four as a unified institutional pathology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(emergency_mode_lock_in, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
