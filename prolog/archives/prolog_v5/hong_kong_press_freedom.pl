% ============================================================================
% CONSTRAINT STORY: hong_kong_press_freedom
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hong_kong_press_freedom, []).

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
 *   constraint_id: hong_kong_press_freedom
 *   human_readable: Hong Kong Press Freedom Constraint
 *   domain: political/media_freedom
 *
 * SUMMARY:
 *   Hong Kong's press freedom constraint represents a structural extraction
 *   mechanism targeting independent journalism, news organizations, and
 *   public information access. The constraint intensified dramatically
 *   following the 2019 pro-democracy protests and the 2020 National Security
 *   Law, shifting from a coordination mechanism (maintaining
 *   business-friendly stability) toward pure extraction (political control
 *   consolidation). The theater ratio reflects Hong Kong's maintenance of
 *   nominal institutional autonomy — a legislative council, press council,
 *   rule of law rhetoric — while substantive editorial independence has been
 *   eroded through licensing threats, advertising boycotts, prosecutions
 *   under national security provisions, and self-censorship cascades. The
 *   constraint exhibits all six DR types from different observation points:
 *   trapped journalists experience pure snare; constrained news organizations
 *   experience snare with some exit possibility; the government experiences
 *   rope (coordination of political stability); international observers see
 *   tangled rope (some information still flows despite suppression); the
 *   historical One Country Two Systems framework appears as degraded piton;
 *   and realist geopolitical analysis risks naturalizing the suppression as
 *   an inevitable mountain. The true structure — a snare targeting
 *   journalists and media — is clearest from the powerless and moderate
 *   victim perspectives.
 *
 * KEY AGENTS:
 *   - Independent Journalists: Primary victims (powerless/trapped) — face legal harassment, employment termination, physical intimidation, self-censorship pressures with no viable exit from profession
 *   - News Organizations: Secondary victims (moderate/constrained) — subject to licensing threats, advertising boycotts, asset seizure risk; can exit only at prohibitive cost
 *   - Hong Kong Government: Primary beneficiary (institutional/arbitrage) — maintains political control through information suppression; perceives constraint as coordination mechanism
 *   - Mainland Chinese Authorities: Secondary beneficiary (institutional/arbitrage) — consolidates control over Hong Kong's information space as geopolitical priority
 *   - International Press Freedom Coalition: Organized observers (organized/constrained) — can pressure authorities but lack enforcement power; some coordination function through reputational costs
 *   - One Country Two Systems Framework: Institutional structure (institutional/arbitrage) — vestigial autonomy mechanisms that persist through inertia while actual control concentrates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hong_kong_press_freedom, 0.68).
domain_priors:suppression_score(hong_kong_press_freedom, 0.72).
domain_priors:theater_ratio(hong_kong_press_freedom, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hong_kong_press_freedom, extractiveness, 0.68).
narrative_ontology:constraint_metric(hong_kong_press_freedom, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hong_kong_press_freedom, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hong_kong_press_freedom, snare).
narrative_ontology:human_readable(hong_kong_press_freedom, "Hong Kong Press Freedom Constraint").
narrative_ontology:topic_domain(hong_kong_press_freedom, "political/media_freedom").

domain_priors:requires_active_enforcement(hong_kong_press_freedom).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hong_kong_press_freedom, hong_kong_government).
narrative_ontology:constraint_beneficiary(hong_kong_press_freedom, mainland_authorities).
narrative_ontology:constraint_victim(hong_kong_press_freedom, independent_journalists).
narrative_ontology:constraint_victim(hong_kong_press_freedom, news_organizations).
narrative_ontology:constraint_victim(hong_kong_press_freedom, public_information_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT JOURNALISTS (SNARE) — Face severe extraction with minimal exit options. Caught between self-censorship pressures, legal harassment, employment termination, and physical intimidation. Cannot exit the profession without abandoning career identity and livelihood. Suppression mechanisms include National Security Law application, advertising boycotts, business license threats. Maximum experienced extraction due to trapped exit status and institutional power arrayed against them.
constraint_indexing:constraint_classification(hong_kong_press_freedom, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NEWS ORGANIZATIONS (SNARE) — Face extraction through licensing threats, advertising boycotts, regulatory pressure, and asset seizure risk. Can relocate operations but at massive cost (losing market access, staff, established infrastructure). Some organizations have exited (Apple Daily, Stand News) but only after severe extraction. Remaining organizations operate under self-censorship regime. High extraction with constrained rather than trapped exit.
constraint_indexing:constraint_classification(hong_kong_press_freedom, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HONG KONG GOVERNMENT (ROPE) — Experiences the constraint as a coordination mechanism that solves the political stability problem: maintaining control over public discourse enables predictable governance. Perceives the mechanism as serving legitimate coordination functions (national security, social stability). Net beneficiary with arbitrage options — can adjust enforcement intensity as needed. Low experienced extraction because this agent controls the constraint's implementation.
constraint_indexing:constraint_classification(hong_kong_press_freedom, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL PRESS FREEDOM COALITION (TANGLED ROPE) — Organized agents (international media organizations, press freedom NGOs, democracies) see the constraint as extractive toward Hong Kong journalists but also recognize that some mechanism of information flow (constrained relative to pre-2019) still exists. International pressure provides some constraint on worst excesses, creating coordination benefit (Hong Kong authorities cannot completely eliminate foreign media presence without reputational cost). Sunset logic is weak — the constraint shows no clear path to relaxation.
constraint_indexing:constraint_classification(hong_kong_press_freedom, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ONE COUNTRY TWO SYSTEMS FRAMEWORK (PITON) — Historical institutional structure (Hong Kong's autonomy under Basic Law) has atrophied as the primary function (preserving distinct governance systems) has been subordinated to mainland political control. The framework persists through institutional inertia and rhetorical maintenance (claiming two systems still operate) while its substantive checks on Beijing authority have degraded. Theater ratio reflects that Hong Kong still maintains nominal legislative structure, press council, and rule of law institutions that perform autonomy without substantive independence.
constraint_indexing:constraint_classification(hong_kong_press_freedom, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: REALIST POLITICAL ANALYSIS (MOUNTAIN) — From a structural realist perspective, press freedom suppression in Hong Kong is an inevitable consequence of Beijing's security calculus: a major global financial center with autonomous media institutions represents an unacceptable sovereignty risk to mainland authorities. From this view, the constraint emerges naturally from geopolitical necessity and cannot be negotiated away. However, this perspective risks naturalizing what is actually a contingent political choice by Beijing authorities. The framework's false summit detector should flag this as naturalization rather than true structural immutability.
constraint_indexing:constraint_classification(hong_kong_press_freedom, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hong_kong_press_freedom_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hong_kong_press_freedom, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hong_kong_press_freedom, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hong_kong_press_freedom, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hong_kong_press_freedom, TR),
    TR >= 0.70.

:- end_tests(hong_kong_press_freedom_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint extracts career opportunities, professional autonomy, and information access from journalists and news organizations. The rising trajectory (0.35 → 0.52 → 0.68 across the measurement interval) reflects progressive tightening following 2019 protests and National Security Law implementation. The value reflects that extraction is severe but not absolute — some independent reporting still occurs, primarily through international platforms and encrypted channels. Suppression (0.72): High. Multiple structural barriers constrain journalists' ability to exit: professional identity sunk costs, family rootedness in Hong Kong, economic dependency on local media markets, mainland travel restrictions for those who attempt to leave. Legal provisions (National Security Law, sedition statutes) create explicit barriers. Self-censorship creates internalized suppression that persists even when formal barriers recede. Theater ratio (0.58): Moderate-high. The constraint's performance dimension is substantial: Hong Kong maintains institutions that signal autonomy (legislative council, independent judiciary rhetoric, press council) while substantive editorial control has concentrated. The theater has increased over time as the divergence between claimed and actual autonomy widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Independent journalists (powerless/trapped) perceive pure extraction with no coordination benefit — the National Security Law appears designed entirely to suppress dissent, not to solve a genuine collective action problem. News organizations (moderate/constrained) perceive some coordination benefit (business stability) alongside extraction (control costs). The Hong Kong government (institutional/arbitrage) perceives pure coordination — maintaining information control as necessary for effective governance. International observers (organized/constrained) see tangled rope — suppression is real but not absolute, and their pressure provides some constraint on worst excesses. The One Country Two Systems framework (institutional/arbitrage) appears as piton — nominally autonomous but substantively atrophied. The realist analyst (analytical/analytical) risks seeing mountain — geopolitical necessity — but this naturalizes what is actually a contingent political choice by Beijing and Hong Kong authorities. The gap between powerless and institutional perspectives is diagnostic: both sides experience the same structural mechanism, but the beneficiary perceives it as coordination while the target perceives it as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the information flow. Trapped journalists (powerless/trapped) derive d ≈ 0.95 — they are full targets of extraction with no exit capacity. Constrained news organizations (moderate/constrained) derive d ≈ 0.75 — they bear significant costs but retain limited exit options (relocate, reduce staff, shift focus). The Hong Kong government (institutional/arbitrage) derives d ≈ 0.05 — they are the beneficiary with arbitrage options (adjust enforcement intensity, choose targets). Mainland authorities inherit similar d ≈ 0.05. The international coalition (organized/constrained) derives d ≈ 0.55 — they occupy an intermediate position with some agency (international pressure) but not full control. The sigmoid function transforms these d values into effective extraction multipliers: trapped journalists experience f(d ≈ 0.95) ≈ 1.42, while beneficiaries experience f(d ≈ 0.05) ≈ -0.12. The scope modifier σ(national) = 1.0 leaves chi unscaled, reflecting that press freedom suppression operates primarily at the national level rather than globally.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through clear beneficiary/victim designation and high extractiveness (0.68). The snare classification is not contingent on framing — both the beneficiary and the primary victim recognize the asymmetric extraction, differing only in whether they frame it as justified coordination (government) or unjust suppression (journalists). The analytical observer's mountain perspective (realist geopolitical necessity) is flagged as a false summit: the suppression is structurally contingent on political choices by Beijing and Hong Kong authorities, not an inevitable natural law. The theater ratio (0.58) is consistent with snare mechanics — performative institutional autonomy masks the actual extraction mechanism. The constraint satisfies the snare gates: extractiveness ≥ 0.46 (✓), suppression ≥ 0.60 (✓), χ effectively ≥ 0.66 (confirmed by multiple victim perspectives), and at least one victim group declared (✓). No contradiction between claimed type (snare) and structural metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_censorship_internalization,
    'To what degree is suppression of press freedom maintained through journalists'' internalized caution versus external enforcement mechanisms?',
    'Analysis of post-exit behavior: do journalists who leave Hong Kong continue self-censoring patterns, or do they resume previous coverage intensity? Comparison of mainland and foreign-based Hong Kong journalists'' risk perception.',
    'If primarily internalized: the constraint persists through cognitive capture (identity_locked dynamics) even if external enforcement were removed; suppression value should be treated as structural + internalized. If primarily external: alternative venues (diaspora media, international press) should show substantially different coverage intensity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_censorship_internalization, empirical, 'Degree of self-censorship internalization versus external enforcement').

omega_variable(
    information_leakage_rate,
    'What proportion of Hong Kong governance information intended to be suppressed actually reaches public knowledge through alternative channels (encrypted messaging, diaspora media, foreign press, international organizations)?',
    'Information flow analysis: comparison of facts in foreign reporting about Hong Kong governance versus what Chinese and Hong Kong state media acknowledge; tracking of whistleblower/leaked document prevalence; cross-reference with international journalism databases.',
    'If leakage rate > 60%: suppression mechanism is ineffective (actual extractiveness should be downgraded, classification moves toward tangled_rope). If leakage rate < 30%: suppression is highly effective and extractiveness upward adjustment justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_leakage_rate, empirical, 'What proportion of suppressed information reaches public knowledge').

omega_variable(
    coordination_benefit_to_authorities,
    'Do Hong Kong authorities actually achieve governance coordination benefits from press suppression, or is it purely extractive power consolidation?',
    'Policy effectiveness analysis: comparison of policy implementation success in high-press-freedom periods (pre-2019) versus low-freedom periods (post-2020). Does suppression actually improve governance effectiveness or merely reduce political dissent?',
    'If coordination benefits are real: beneficiaries'' perspective (government as rope) is partially justified; constraint has hybrid coordination-extraction function. If no coordination benefit: government''s rope perspective is false framing of pure extraction, reclassifying as snare from government perspective as well.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_benefit_to_authorities, conceptual, 'Whether press suppression achieves governance coordination or is purely extractive').

omega_variable(
    mainland_autonomy_preservation,
    'Is the press freedom suppression primarily driven by Hong Kong authorities'' security concerns or by mainland China''s requirement to consolidate control?',
    'Attribution analysis of policy drivers: timeline of suppressive measures relative to Beijing directives versus autonomous Hong Kong policy decisions; comparison with mainland press freedom restrictions to identify policy copying versus independent judgment.',
    'If mainland-driven: Hong Kong government is victim/intermediary rather than primary beneficiary; perspective redesignation needed. If Hong Kong-autonomous: beneficiary classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mainland_autonomy_preservation, empirical, 'Attribution of suppression policy to mainland versus Hong Kong authorities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hong_kong_press_freedom, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hkpf_tr_t0, hong_kong_press_freedom, theater_ratio, 0, 0.38).
narrative_ontology:measurement(hkpf_tr_t3, hong_kong_press_freedom, theater_ratio, 3, 0.48).
narrative_ontology:measurement(hkpf_tr_t6, hong_kong_press_freedom, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(hkpf_be_t0, hong_kong_press_freedom, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hkpf_be_t3, hong_kong_press_freedom, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(hkpf_be_t6, hong_kong_press_freedom, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hong_kong_press_freedom, enforcement_mechanism).
narrative_ontology:affects_constraint(hong_kong_press_freedom, mainland_information_control).
narrative_ontology:affects_constraint(hong_kong_press_freedom, hong_kong_civil_society_restriction).
narrative_ontology:affects_constraint(hong_kong_press_freedom, cross_strait_media_coordination).

% DUAL FORMULATION NOTE:
% Hong Kong press freedom suppression is structurally linked to mainland information control mechanisms but represents a distinct constraint with its own extractiveness trajectory. The upstream constraint (mainland_information_control) influences this one through policy diffusion and coordination requirements between Hong Kong and mainland authorities. The downstream constraint (hong_kong_civil_society_restriction) is enabled by press freedom suppression, which prevents organized mobilization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hong_kong_press_freedom, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
