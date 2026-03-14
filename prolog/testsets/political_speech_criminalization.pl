% ============================================================================
% CONSTRAINT STORY: political_speech_criminalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_speech_criminalization, []).

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
 *   constraint_id: political_speech_criminalization
 *   human_readable: Political Speech Criminalization
 *   domain: political/legal/governance
 *
 * SUMMARY:
 *   Political speech criminalization represents a regime's systematic use of
 *   criminal law to eliminate competitive political speech and suppress
 *   opposition. The constraint operates through selective prosecution of
 *   dissidents, opposition parties, and civil society under vague statutes
 *   (sedition, extremism, national security threats). Extractiveness
 *   increases over time as regimes layer additional criminal codes and expand
 *   prosecutorial discretion. Theater ratio remains moderate because the
 *   criminalization is partially legalized — statutes exist, trials occur,
 *   formal procedures are followed — but the content-selectivity of
 *   enforcement reveals the theatrical nature. The regime experiences this as
 *   legitimate coordination (managing the political environment), dissidents
 *   experience it as pure extraction (voice suppression without legal
 *   recourse), and security apparatus members experience identity fusion with
 *   the enforcement role. The international legal system observes but cannot
 *   effectively intervene, making its regulatory function degraded (piton).
 *   This constraint exhibits mandatrophy resolution: it is unambiguously
 *   extractive from the dissident perspective (snare) but could superficially
 *   appear as coordination from the regime perspective if the analysis stops
 *   at the beneficiary's experience. The full analytical view confirms snare
 *   as the dominant classification because the coordination function (regime
 *   legitimacy management) is subordinate to extraction (suppression of
 *   voice), not coordinate with it.
 *
 * KEY AGENTS:
 *   - Political Dissidents: Primary victims (powerless/trapped) — cannot exit without material loss; face imprisonment, asset seizure, and permanent political exclusion
 *   - Opposition Parties: Secondary victims (moderate/constrained) — face prosecution risk; exit requires disbanding or relocation; constrained mobility
 *   - State Security Apparatus: Institutional enforcer (institutional/identity_locked) — structurally mobile but identity-fused with enforcement role; enable the constraint through prosecutorial discretion
 *   - Incumbent Regime: Primary beneficiary (institutional/arbitrage) — maintains political monopoly through suppression; benefits from opposition silence and consolidated message control
 *   - Civil Society: Tertiary victim (moderate/constrained) — faced with self-censorship pressure; organizations risk prosecution; mobility constrained by association costs
 *   - International Judicial System: Nominal regulator (institutional/arbitrage) — theoretically oversees human rights; functionally degraded due to weak enforcement power; maintains theater of legitimacy
 *   - Analytical Observer: Structural analyst (analytical/analytical) — observes pattern as persistent mechanism across historical regimes and geographic contexts; identifies extractive structure independent of regime justifications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_speech_criminalization, 0.68).
domain_priors:suppression_score(political_speech_criminalization, 0.82).
domain_priors:theater_ratio(political_speech_criminalization, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_speech_criminalization, extractiveness, 0.68).
narrative_ontology:constraint_metric(political_speech_criminalization, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(political_speech_criminalization, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_speech_criminalization, snare).
narrative_ontology:human_readable(political_speech_criminalization, "Political Speech Criminalization").
narrative_ontology:topic_domain(political_speech_criminalization, "political/legal/governance").

domain_priors:requires_active_enforcement(political_speech_criminalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_speech_criminalization, regime_incumbent).
narrative_ontology:constraint_victim(political_speech_criminalization, political_dissidents).
narrative_ontology:constraint_victim(political_speech_criminalization, opposition_parties).
narrative_ontology:constraint_victim(political_speech_criminalization, civil_society).
narrative_ontology:constraint_victim(political_speech_criminalization, press_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The dissident cannot exit the jurisdiction without abandoning livelihood, family, and community. Speech criminalization forces silence or clandestinity. No legal recourse exists within the system. Maximum extraction — the constraint strips political voice while forcing physical presence.
constraint_indexing:constraint_classification(political_speech_criminalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Opposition parties face prosecution risk, asset seizure, and imprisonment of candidates and supporters. Exit is theoretically possible (relocate abroad, disband) but extremely costly. Constrained exit with asymmetric enforcement creates effective suppression. The opposition perceives pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(political_speech_criminalization, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Security officials may be structurally mobile (could resign, relocate) but are identity-fused with the enforcement role. Career identity, professional status, and institutional loyalty make exit unthinkable despite knowing the mechanism is extractive. This is cognitive capture, not material constraint. The apparatus maintains the snare while being partially captured by it.
constraint_indexing:constraint_classification(political_speech_criminalization, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% The regime benefits from suppression of opposition speech (coordination of consent manufacturing) AND extraction of political control. Genuine coordination function exists: managing the messaging environment to sustain legitimacy. But the mechanism is asymmetrically extractive — opposition bears all costs. Active enforcement required. Effective extraction benefits the regime while costs are borne by dissidents and opposition.
constraint_indexing:constraint_classification(political_speech_criminalization, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% International courts (ICC, regional human rights bodies) nominally regulate speech criminalization through laws against persecution. But enforcement is weak, selective, and rarely capable of preventing domestic enforcement. The international legal framework has high theater (tribunals, convictions of leaders in absentia) and minimal functional power to stop the constraint. Piton classification: degraded institution maintained by aspiration and inertia rather than effectiveness.
constraint_indexing:constraint_classification(political_speech_criminalization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer from civilizational scale sees political speech criminalization as a persistent constraint across historical regimes and geographic contexts. From this perspective, the pattern is observable as a structural mechanism: regimes reliably use selective prosecution to eliminate competitive speech when electoral or consensus-based legitimacy fails. The structure emerges across diverse cultural and legal systems, suggesting deep extractive function rather than contingent policy choice.
constraint_indexing:constraint_classification(political_speech_criminalization, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_speech_criminalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_speech_criminalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_speech_criminalization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_speech_criminalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_speech_criminalization, TR),
    TR >= 0.70.

:- end_tests(political_speech_criminalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The regime captures monopoly control over political messaging and eliminates competitive threats through suppression. The extraction increases over the timeline (0.35 → 0.68) as additional criminal statutes are introduced and prosecutorial discretion expands. The metric reflects that dissidents lose political voice entirely while the regime gains consolidated legitimacy control. Suppression (0.82): Very high. Structural barriers to opposition speech are extreme: imprisonment threat, asset seizure, torture risk, family targeting. Self-censorship becomes internalized as dissidents and opposition parties learn that speech carries existential personal costs. Theater ratio (0.58): Moderate. Criminalization is partially legalized — statutes exist and are enforced through formal judicial processes — but selective prosecution for political content reveals the performative nature. The regime maintains the appearance of rule of law while operating pure discretionary extraction. The theater increases (0.42 → 0.58) as regimes add procedural legitimacy (trials, appeals) to mask selective enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The regime and dissident perspectives differ not only in classification (tangled_rope vs snare) but in the very meaning of 'rule of law.' The regime interprets speech criminalization statutes as legitimate law enforcement — content-neutral application of criminal codes against national security threats. The dissident perceives selective prosecution unrelated to statutory text: opposition speeches prosecuted under sedition statutes while regime-supporting speeches using identical language go unpunished. This perspectival gap is empirically resolvable through enforcement statistics (omega variable: selectivity threshold), but the perspectives themselves remain incommensurable from within their own frames. The regime cannot see the selectivity as unjust because it perceives the opposition speeches as genuinely threatening (identity lock). The dissident cannot accept regime claims of neutrality because enforcement patterns prove selective application (victim perspective on mechanism). The security apparatus bridges but doesn't reconcile: they execute the regime's legal interpretation while often knowing the selectivity is real, creating cognitive dissonance that enables some apparatus members to exit during regime transitions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from their position relative to the extraction flow. Dissidents are full targets: trapped exit + victim status → d ≈ 0.95 → f(d) ≈ 1.42 (powerless experience maximum extractiveness chi). Opposition parties are constrained targets: constrained exit + victim status → d ≈ 0.75 → f(d) ≈ 0.95 (moderate power reduces but doesn't eliminate extraction). The regime is a beneficiary: arbitrage exit + institutional power → d ≈ 0.10 → f(d) ≈ -0.05 (negative chi, the constraint subsidizes the regime). Security apparatus members have high nominal power but identity-locked exit: identity_locked + institutional power → d ≈ 0.40 (intermediate because they're structurally mobile but cognitively trapped). The directionality chain reveals that identity lock in security apparatus is not equivalent to powerlessness — they have capacity to exit that they cannot exercise due to identity fusion. This is diagnostically important: identity-locked agents can be mobilized for regime transition if their identity frame can be shifted (evidence of regime crimes, alternative professional identity pathways).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (extractiveness > 0.70): Political speech criminalization could be misclassified as a coordination mechanism (tangled rope or even rope) if analysis stops at the regime's perspective — the regime genuinely needs to coordinate political messaging to maintain legitimacy, and speech criminalization serves that coordination function. But the structural data confirms snare as the dominant classification because: (1) Suppression is asymmetric and extreme (0.82) — only opposition speech faces criminalization, not regime speech. Pure coordination would apply uniformly. (2) Beneficiaries (regime) and victims (dissidents) are clearly differentiated in structural extraction, not in coordination benefit. True tangled rope requires that both beneficiary and victim perceive genuine coordination value alongside extraction costs. Dissidents perceive zero coordination value — they get only suppression. (3) The regime's coordination problem (legitimacy maintenance) is not inherent to speech itself but to the regime's low public support. A more legitimate regime doesn't need speech criminalization. This means the 'coordination' function is actually the regime's contingent legitimacy crisis, not a genuine shared coordination problem. The mandatrophy is resolved by recognizing that the regime's perspective is structurally privileged (institutional power, arbitrage exit) and uses its position to reframe pure extraction as necessary coordination. The analytical observer must look beyond the beneficiary's self-justification to the actual structural distribution of costs and benefits. CONFIRMED: Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_threshold,
    'At what level of enforcement selectivity does speech criminalization transition from content-neutral law enforcement to political persecution?',
    'Statistical analysis of prosecution patterns: is prosecution rate for opposition speech statistically consistent with application to regime-supporting speech? Comparative prosecution timelines and conviction rates.',
    'If selectivity is high (opposition disproportionately prosecuted): the constraint is pure extraction (Snare confirmed). If selectivity is low (consistent enforcement across political spectrum): reclassify as tangled_rope with coordination function for rule of law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity_threshold, empirical, 'Enforcement selectivity threshold for content-based persecution').

omega_variable(
    statutory_vagueness_mechanism,
    'Does speech criminalization operate through vague statutes (sedition, extremism, national security) that grant discretionary enforcement power, or through explicitly targeted criminal codes?',
    'Legal text analysis; comparison of statutory language breadth; review of prosecutorial discretion in statutes vs explicit targeting mechanisms.',
    'If vague: discretionary power IS the constraint mechanism — extraction is maximized and regime retains flexibility. If explicit: constraint is transparent and potentially vulnerable to international pressure and organized opposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_vagueness_mechanism, empirical, 'Whether criminalization uses vague statutes or explicit targeting').

omega_variable(
    identity_lock_reversibility,
    'Can security apparatus members exit the enforcement role if regime changes or alternative career structures become available, or is the identity lock structurally irreversible?',
    'Post-regime-change analysis: transition and de-commissioning patterns when authoritarian regimes collapse; examination of whether security officials can be retrained or reintegrated into democratic systems.',
    'If reversible: identity lock is temporary and agents will exit given structural opportunity — enabling rapid regime transition. If irreversible: security apparatus will resist regime change actively — increasing civil conflict risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Reversibility of security apparatus identity lock').

omega_variable(
    suppression_internalization,
    'Is suppression of political speech primarily structural (legal risk, imprisonment threat) or internalized (agents self-censor even without immediate enforcement threat)?',
    'Measurement of speech frequency pre- and post-removal of enforcement threat; analysis of clandestine speech patterns; longitudinal data on self-censorship decay after regime change.',
    'If primarily structural: removing enforcement mechanism reduces suppression quickly. If internalized: suppression persists after enforcement removal — constraint has become cognitive, and exit requires identity frame shift, not just legal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Suppression mechanism: structural vs internalized').

omega_variable(
    regime_legitimacy_dependency,
    'Is political speech criminalization functionally necessary for regime survival, or is it path-dependent institutional choice that persists despite alternative legitimacy sources?',
    'Comparative analysis of regimes that maintain political legitimacy without speech criminalization; analysis of regime stability before and after introduction of speech criminalization statutes.',
    'If necessary: constraint reflects genuine coordination problem (regime continuity requires suppression) — reclassify as tangled_rope. If path-dependent: constraint is pure extraction with theatrical legitimacy function — snare confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_legitimacy_dependency, conceptual, 'Whether speech criminalization is functionally necessary for regime legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_speech_criminalization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psc_tr_t0, political_speech_criminalization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(psc_tr_t5, political_speech_criminalization, theater_ratio, 5, 0.5).
narrative_ontology:measurement(psc_tr_t10, political_speech_criminalization, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(psc_be_t0, political_speech_criminalization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(psc_be_t5, political_speech_criminalization, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(psc_be_t10, political_speech_criminalization, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_speech_criminalization, enforcement_mechanism).
narrative_ontology:affects_constraint(political_speech_criminalization, press_freedom_constraint).
narrative_ontology:affects_constraint(political_speech_criminalization, opposition_party_legality).
narrative_ontology:affects_constraint(political_speech_criminalization, civil_society_association_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(political_speech_criminalization, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
