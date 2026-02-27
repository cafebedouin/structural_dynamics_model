% ============================================================================
% CONSTRAINT STORY: iran_nin_repression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iran_nin_repression, []).

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
 *   constraint_id: iran_nin_repression
 *   human_readable: Iran's National Information Network (State-Controlled Intranet)
 *   domain: technological/political
 *
 * SUMMARY:
 *   Iran's National Information Network (NIN), branded as the 'halal
 *   internet,' represents a structural extraction mechanism disguised as
 *   cultural and security infrastructure. Launched incrementally from 2011
 *   onward and accelerating post-2019, the NIN architecture routes all
 *   domestic internet traffic through state-controlled nodes, enabling
 *   real-time surveillance, content filtering, and information monopoly. The
 *   constraint exhibits textbook snare properties: high base extractiveness
 *   (0.68), extreme suppression (0.82 — VPNs are illegal, circumvention tools
 *   are prosecuted, exit from the system is impossible for domestic users),
 *   and mounting theater (0.58) as the regime invests in appearance of
 *   functionality while the actual censorship costs Iranian internet users
 *   access to global knowledge, communication with diaspora, and authentic
 *   independent information. The constraint asymmetrically benefits the state
 *   security apparatus (which solves its collective action problem of
 *   information control) while extracting from all Iranian internet users
 *   (who lose agency, privacy, and access). The theater ratio rises over the
 *   measurement interval as technical maintenance costs accumulate without
 *   improving user experience — the NIN becomes increasingly a degraded
 *   system maintained through force rather than genuine function, yet the
 *   regime's ideological commitment ensures suppression remains high. This
 *   constraint family (NIN + related digital surveillance mechanisms)
 *   operates within a broader state surveillance architecture, making
 *   decomposition into separate constraint stories appropriate for
 *   platform-specific mechanisms (social media filtration, mobile app
 *   control, diaspora communication targeting).
 *
 * KEY AGENTS:
 *   - Iranian Internet Users: Primary victims (powerless/trapped) — domestic population with no exit option, subject to content filtering and surveillance
 *   - Independent Journalists and Activists: Secondary victims (moderate/constrained) — face prosecution, imprisonment, asset seizure for circumvention or critical reporting
 *   - Iranian State Security Apparatus (IRGC, Ministry of Intelligence): Primary beneficiary (institutional/arbitrage) — controls information flows, eliminates opposition messaging, achieves surveillance totality
 *   - State Media Monopoly (IRIB, state news agencies): Secondary beneficiary (institutional/constrained) — maintains content monopoly but increasingly degraded by technical failures and perception of propaganda
 *   - Global Human Rights Organizations: Tertiary beneficiary (organized/mobile) — gain documentary material and advocacy leverage from NIN's oppression; mobile exit enables external critique
 *   - Diaspora Networks (Telegram communities, secure messaging): Competing extraction system (moderate/mobile) — parallel channels that partially bypass NIN, reducing its total extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_nin_repression, 0.68).
domain_priors:suppression_score(iran_nin_repression, 0.82).
domain_priors:theater_ratio(iran_nin_repression, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_nin_repression, extractiveness, 0.68).
narrative_ontology:constraint_metric(iran_nin_repression, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(iran_nin_repression, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_nin_repression, snare).
narrative_ontology:human_readable(iran_nin_repression, "Iran's National Information Network (State-Controlled Intranet)").
narrative_ontology:topic_domain(iran_nin_repression, "technological/political").

domain_priors:requires_active_enforcement(iran_nin_repression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_nin_repression, iranian_state_security_apparatus).
narrative_ontology:constraint_beneficiary(iran_nin_repression, state_media_monopoly).
narrative_ontology:constraint_victim(iran_nin_repression, iranian_internet_users).
narrative_ontology:constraint_victim(iran_nin_repression, independent_journalists).
narrative_ontology:constraint_victim(iran_nin_repression, civil_society_organizations).
narrative_ontology:constraint_victim(iran_nin_repression, diaspora_communication_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRANIAN INTERNET USERS (SNARE) — Citizens with internet access face mandatory routing through state-controlled infrastructure with no alternative exit. VPN use is illegal and technically blocked; purchasing circumvention tools is prosecuted. Cannot opt out, cannot migrate to alternative networks. d≈0.96, f(d)≈1.41, σ=1.0 → χ≈0.96. Maximum structural extraction.
constraint_indexing:constraint_classification(iran_nin_repression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT JOURNALISTS AND ACTIVISTS (SNARE) — Constrained exit (can travel abroad, can use diaspora networks) but at severe cost: imprisonment, travel bans, asset seizure. Domestic work requires operating within NIN surveillance. International reporting is monitored and sources are exposed. d≈0.88, f(d)≈1.25, σ=1.0 → χ≈0.85. Near-maximal extraction with constrained not trapped.
constraint_indexing:constraint_classification(iran_nin_repression, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: GLOBAL HUMAN RIGHTS ORGANIZATIONS (TANGLED ROPE) — Mobile exit (can cease operations in Iran, can coordinate internationally) but also benefits from documentation and monitoring of NIN repression — the constraint generates data and narrative leverage for advocacy. Extraction is asymmetric: Iranian civil society loses; international monitors gain investigative material. d≈0.58, f(d)≈0.75, σ=1.0 → χ≈0.51. Coordination function (documenting surveillance) coexists with asymmetric extraction (Iran's loss is external monitor's gain).
constraint_indexing:constraint_classification(iran_nin_repression, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: IRANIAN STATE SECURITY APPARATUS (ROPE) — Benefits from coordination: unified control over information flows, centralized surveillance, unified messaging. Experiences NIN as a solution to their collective action problem (securing regime information dominance). Has full arbitrage exit (can modify rules, can enforce compliance). d≈0.02, f(d)≈-0.18, σ=1.0 → χ≈-0.12. Net beneficiary; constraint is pure coordination from this view.
constraint_indexing:constraint_classification(iran_nin_repression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE MEDIA AND STATE-APPROVED CONTENT PROVIDERS (PITON) — Initially benefited from monopoly on domestic distribution (Rope perspective, pre-2010). Now theater_ratio=0.58 reflects that the apparatus itself is degraded: constant technical failures, poor integration with global systems, high maintenance cost, limited actual content quality compared to blocked global platforms. Maintained through inertia and regime ideology, not genuine functionality. d≈0.25, f(d)≈0.15, σ=1.0 → χ≈0.10. Declining benefit; theater rising.
constraint_indexing:constraint_classification(iran_nin_repression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (GLOBAL PERSPECTIVE) (SNARE) — From a civilizational/global view, NIN is pure extraction: it removes Iranian users from global information networks, eliminates their agency in choosing information sources, and creates asymmetric information control benefiting only the regime. No coordination function visible at global scale — the apparatus does not solve a collective action problem for users; it solves one for the regime alone. d≈0.94, f(d)≈1.39, σ=1.2 → χ≈0.94. Snare classification is invariant across scopes.
constraint_indexing:constraint_classification(iran_nin_repression, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_nin_repression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iran_nin_repression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_nin_repression, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iran_nin_repression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iran_nin_repression, TR),
    TR >= 0.70.

:- end_tests(iran_nin_repression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The NIN extracts user agency (choice of information sources), privacy (ubiquitous surveillance), and diaspora communication (filtered/monitored). The extraction is not maximal because diaspora networks and occasionally-accessible mirror sites provide partial workarounds, and international access during travel offers periodic relief. The 0.68 value reflects that the apparatus succeeds in most cases but not all — the suppression mechanisms (legal threats, technical blocks) are severe but not hermetically perfect. Base extractiveness has risen from 0.35 (early 2010s, when NIN was conceptual) to 0.68 (2024, when technical maturity and enforcement are complete), reflecting the constraint's graduation from aspirational to actual. Suppression (0.82): Extremely high. VPN use is illegal under Iranian law (Article 19 violations). Circumvention tool possession is prosecuted. International phone and email communication is monitored. The regime has invested heavily in technical blocking (DPI, DNS poisoning, BGP hijacking) and enforcement (arrests of VPN users, prosecution of activists, seizure of equipment). Suppression is not total (0.82 ≠ 1.0) because some technically sophisticated users still maintain access, and the costs of enforcement are rising relative to the threat perceived. Theater ratio (0.58): Moderate-high, rising. The NIN claims to provide 'halal' (culturally appropriate) content and technical sovereignty but actually delivers a technically inferior experience (poor latency, limited functionality, high maintenance costs). Regime propaganda frames the NIN as a positive development protecting Iranian culture; users experience it as a mandatory prison. The theater ratio rises as the gap between promised functionality and actual performance widens: the regime must invest increasingly in maintaining appearance of success while users face actual degradation.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and structurally irreducible. The Iranian state security apparatus perceives the NIN as pure coordination (Rope) — solving the unified control problem. Iranian internet users perceive it as pure extraction (Snare) — they have no choice and bear all costs. Independent journalists perceive it as snare with constrained exit (Snare, higher d). Global human rights organizations perceive tangled coordination-extraction (Tangled Rope) — the constraint enables them to document and mobilize around Iranian repression, asymmetrically benefiting external advocacy while harming internal dissent. State media perceives it as degrading from rope to piton (theater rising, actual function declining). The analytical observer sees snare at every scale — the regime is the sole genuine beneficiary; everyone else bears costs. This perspectival gap cannot be narrowed by additional data: it reflects irreducible conflict between state interests (control) and user interests (freedom).
 *
 * DIRECTIONALITY LOGIC:
 *   Iranian internet users: Victim + trapped → d≈0.96, f(d)≈1.41, σ=1.0 → χ≈0.96. Powerless agents with zero exit capacity absorb maximum extraction. Independent journalists: Victim + constrained → d≈0.88, f(d)≈1.25, σ=1.0 → χ≈0.85. Can exit at severe cost (imprisonment, exile); most do not exit because work requires presence. State security apparatus: Beneficiary + arbitrage → d≈0.02, f(d)≈-0.18, σ=1.0 → χ≈-0.12. Institutional actors with full arbitrage (can modify rules, can enforce); net beneficiary. Global human rights organizations: Beneficiary (of documentary material) + mobile → d≈0.30, f(d)≈0.25, σ=1.0 → χ≈0.17. Can exit fully (cease Iran operations); mobile exit reduces chi below what victim perspective would suggest because they choose to remain and document. State media: Once-beneficiary now constrained → d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.20. Declining benefit; theater rise suggests reclassification from Rope to Piton over measurement interval.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is partially resolvable and partially irreducible. RESOLVABLE COMPONENT: Whether NIN is a snare (extraction-dominant) or rope (coordination-dominant) is answerable empirically. If we measure the distribution of costs and benefits across Iranian society, the snare classification is unambiguous — users lose, state gains. This is not a matter of perspective; it is objective structural asymmetry. IRREDUCIBLE COMPONENT: Whether the constraint is justified (whether the regime's security concerns justify the extraction, whether 'halal internet' reflects genuine cultural preference or coerced compliance) is a values question, not a structural one. The DR framework does not resolve this — it documents that the constraint extracts and that suppression is severe. The mandatrophy is resolved by recognizing that SNARE is the correct structural classification (not rope, not mountain, not scaffold), and the question of justification is a separate policy/ethical question. The persistent piton and mountain perspectives in the commentary (regime ideology, youth naturalization) are crucial: they show how extraction can be maintained by cultural normalization rather than force alone. If the constraint persists for another generation without substantial exodus or counter-information, the snare becomes self-reinforcing (younger cohorts perceive it as natural), and the theater ratio may rise past 0.70 (entering full piton degradation). This would represent a transformation from snare (clear structure of extraction maintained by suppression) to piton (degraded system maintained by theater and inertia).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_efficacy_threshold,
    'At what level of user technical sophistication (VPN usage, mirror sites, proxy relays) does the NIN''s extraction collapse from snare to a degraded rope-like mechanism?',
    'Longitudinal tracking of VPN/proxy penetration rates; correlation with regime security response escalation; analysis of whether technical countermeasures become economically unsustainable for the state',
    'If threshold is crossed: snare becomes unsustainable, reclassifies to piton (extraction degrades as regime accepts workarounds). If threshold is high: snare persists indefinitely, classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_efficacy_threshold, empirical, 'Technical sophistication threshold at which NIN extraction becomes infeasible').

omega_variable(
    regime_normalization_trajectory,
    'Does normalization of international relations (nuclear deal compliance, sanctions relief) reduce or increase regime commitment to NIN as ideological vs. security-driven infrastructure?',
    'Analysis of NIN investment levels, technical refresh cycles, rhetoric emphasis (security vs. cultural preservation) during periods of sanctions vs. normalization',
    'If normalization reduces NIN: constraint reclassifies to scaffold (sunset path). If normalization increases NIN (ideological entrenchment): snare persists or intensifies, suggesting deep structural commitment rather than temporary security measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_normalization_trajectory, empirical, 'Whether international normalization affects NIN viability and ideology').

omega_variable(
    diaspora_information_channel_capacity,
    'How much of Iranian civil society''s actual information consumption flows through diaspora-maintained shadow networks (Telegram, Signal, secure email) vs. domestically accessible (NIN + blocked global sites)?',
    'Network analysis of packet flows; user surveys of primary information sources; comparison of diaspora-originated content penetration rates over time',
    'If shadow networks dominate: NIN''s extraction becomes partial, reclassifying to tangled_rope (coordination of official messaging coexists with parallel extraction-resistant channels). If NIN dominates: snare persists, extraction is near-total.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_information_channel_capacity, empirical, 'Penetration of diaspora shadow networks vs. state-controlled content').

omega_variable(
    youth_cohort_digital_citizenship,
    'For Iranians under 30 who have only known NIN-era internet, does the constraint feel like an immutable natural law (mountain) or a contingent political choice (snare)?',
    'Longitudinal surveys of digital literacy, awareness of global internet, perceived alternatives; analysis of generational attitudes toward circumvention vs. acceptance',
    'If naturalized as mountain: snare becomes culturally self-reinforcing (generational compliance reduces need for enforcement). If perceived as contingent snare: youth cohort retains exit consciousness, snare requires rising suppression to maintain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(youth_cohort_digital_citizenship, conceptual, 'Whether NIN is perceived as natural law by younger cohorts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_nin_repression, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nin_tr_t0, iran_nin_repression, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nin_tr_t7, iran_nin_repression, theater_ratio, 7, 0.5).
narrative_ontology:measurement(nin_tr_t14, iran_nin_repression, theater_ratio, 14, 0.58).

% Extraction over time
narrative_ontology:measurement(nin_be_t0, iran_nin_repression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nin_be_t7, iran_nin_repression, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(nin_be_t14, iran_nin_repression, base_extractiveness, 14, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_nin_repression, enforcement_mechanism).
narrative_ontology:affects_constraint(iran_nin_repression, iranian_diaspora_communication_control).
narrative_ontology:affects_constraint(iran_nin_repression, social_media_platform_filtration_iran).
narrative_ontology:affects_constraint(iran_nin_repression, mobile_app_geofencing_iran).

% DUAL FORMULATION NOTE:
% The NIN is the upstream infrastructure constraint that enables downstream platform-specific controls (Telegram filtering, Instagram geofencing, etc.). Each downstream mechanism has its own ε and perspective set but structurally depends on NIN's packet routing monopoly. The NIN's snare classification is robust; downstream constraints may vary (some scaffolds if they target specific platforms with finite sunset dates, some tangled ropes if they show coordination benefit alongside extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iran_nin_repression, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
