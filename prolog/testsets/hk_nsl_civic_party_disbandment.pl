% ============================================================================
% CONSTRAINT STORY: hk_nsl_civic_party_disbandment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hk_nsl_civic_party_disbandment, []).

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
 *   constraint_id: hk_nsl_civic_party_disbandment
 *   human_readable: The Hong Kong National Security Law (NSL) leading to the dissolution of the Civic Party
 *   domain: political/legal
 *
 * SUMMARY:
 *   The Hong Kong National Security Law (NSL), imposed by the Beijing central
 *   government in 2020, created a structural constraint that systematically
 *   eliminated organized political opposition and suppressed civil liberties
 *   under the guise of security coordination. The Civic Party, Hong Kong's
 *   longest-established pro-democracy party founded in 2006, voted to
 *   dissolve itself in September 2021 after concluding that NSL enforcement
 *   created impossible operational constraints and personal liability for
 *   party members and leadership. This constraint exhibits the defining
 *   characteristics of a Snare: high extractiveness (0.78), extreme
 *   suppression (0.88), and operation through maximized coercion with minimal
 *   coordination benefit for the suppressed populations. The constraint
 *   differs fundamentally from legitimate security coordination by its
 *   retroactive application, vague statutory definitions (subversion,
 *   sedition, collusion with foreign powers), selective prosecution patterns,
 *   and elimination of legal opposition. Theater ratio (0.65) reflects that
 *   formal legal procedures continue (trials, sentencing, legislative
 *   sessions) while substantive decision-making authority has transferred to
 *   security apparatus and Beijing political direction.
 *
 * KEY AGENTS:
 *   - Beijing Central Government: Primary beneficiary (institutional/arbitrage) — consolidates political control, eliminates electoral uncertainty, extends authority over Hong Kong governance
 *   - Hong Kong Security Apparatus: Secondary beneficiary (organized/constrained) — gains expanded authority, veto power over opposition, immunity from prosecution, resource expansion
 *   - Civic Party and Opposition Parties: Primary victim (powerless/trapped) — faces party dissolution, criminal prosecution, asset seizure, total elimination from political competition
 *   - Hong Kong Civil Society: Secondary victim (moderate/constrained) — operates under self-censorship, prosecution threat, emigration pressure; constrained exit options but some gray-zone space remains
 *   - Hong Kong Courts and Institutions: Captured actor (institutional/constrained) — maintain performative independence while operating under NSL override authority; shifted from tangled_rope to piton classification
 *   - Analytical Observer: Global democratic norms perspective (analytical/analytical) — identifies NSL as pure extraction mechanism incompatible with rule of law standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hk_nsl_civic_party_disbandment, 0.78).
domain_priors:suppression_score(hk_nsl_civic_party_disbandment, 0.88).
domain_priors:theater_ratio(hk_nsl_civic_party_disbandment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hk_nsl_civic_party_disbandment, extractiveness, 0.78).
narrative_ontology:constraint_metric(hk_nsl_civic_party_disbandment, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(hk_nsl_civic_party_disbandment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hk_nsl_civic_party_disbandment, snare).
narrative_ontology:human_readable(hk_nsl_civic_party_disbandment, "The Hong Kong National Security Law (NSL) leading to the dissolution of the Civic Party").
narrative_ontology:topic_domain(hk_nsl_civic_party_disbandment, "political/legal").

domain_priors:requires_active_enforcement(hk_nsl_civic_party_disbandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hk_nsl_civic_party_disbandment, beijing_central_government).
narrative_ontology:constraint_beneficiary(hk_nsl_civic_party_disbandment, hong_kong_security_apparatus).
narrative_ontology:constraint_victim(hk_nsl_civic_party_disbandment, opposition_political_parties).
narrative_ontology:constraint_victim(hk_nsl_civic_party_disbandment, civil_society_activists).
narrative_ontology:constraint_victim(hk_nsl_civic_party_disbandment, hong_kong_political_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISBANDED OPPOSITION PARTY MEMBERS (SNARE) — Party officials and activists face criminal liability, asset seizure, and employment blacklisting. Exit is theoretically possible only through emigration, which requires abandoning family, property, and career. The constraint operates through maximum suppression: vague statutory definitions (sedition, subversion, collusion), retroactive application, and political prosecution. Experienced extraction is maximal — no alternatives, no negotiation space, pure coercion.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: HONG KONG CIVIL SOCIETY (SNARE) — NGOs, labor unions, student groups, and journalists operate under existential threat. Self-censorship becomes mandatory compliance. The NSL's broad definitions enable prosecution of previously legal advocacy. Exit options exist (relocation to Taiwan, Australia, or diaspora communities) but carry family separation and loss of social capital. Extraction is severe but not absolute — some agents maintain constrained organizing in gray zones.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: BEIJING CENTRAL GOVERNMENT (ROPE) — The NSL is framed as a coordination mechanism: establishing unified national security standards, eliminating electoral competition that destabilizes governance, clarifying rules for political conduct. From Beijing's perspective, this is a pure coordination function — all agents are brought into alignment. Effective extraction is minimized from this vantage point; the mechanism is presented as neutral rule application.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HONG KONG SECURITY APPARATUS (TANGLED ROPE) — Police and intelligence services receive expanded authority, resources, and protection from prosecution. They coordinate national security implementation while simultaneously extracting power through selective prosecution, intelligence blackmail, and institutional aggrandizement. The constraint provides both coordination function (unified security system) and asymmetric extraction (security apparatus gains de facto immunity and veto power over opposition).
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HONG KONG INSTITUTIONAL STRUCTURES (PITON) — Courts and legislature maintain formal independence but operate under NSL override authority. Judicial review is constrained; legislative opposition is eliminated by party dissolution or arrest. These institutions retain performative roles in legal proceedings while their substantive decision-making capacity has atrophied. Theater ratio is high: formal trials and legal procedures continue, but outcomes are predetermined by political direction from Beijing. The institutions persist through inertia and international credibility theater, not functional autonomy.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From the perspective of democratic governance norms and international rule of law standards, the NSL represents pure extraction wrapped in legal formalism. The constraint systematically eliminates political competition, suppresses dissent, and concentrates power without coordination benefit for the suppressed populations. The analytical view sees no immutable law here — only contingent institutional choices designed for maximum extraction with minimal transparency.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hk_nsl_civic_party_disbandment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hk_nsl_civic_party_disbandment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hk_nsl_civic_party_disbandment, TR),
    TR >= 0.70.

:- end_tests(hk_nsl_civic_party_disbandment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.78): Very high. The constraint extracts political freedom, legal autonomy, and opposition capacity from Hong Kong civil society actors. The timing data shows extraction intensity increasing from 0.35 (pre-NSL legal environment with contested opposition) to 0.78 (post-implementation with party dissolution and prosecution wave). This is not theft of resources but systematic elimination of competitive political power. Suppression (0.88): Extreme. Multiple barriers compound suppression: statutory definitions are so broad and vague that legal challenge is effectively impossible; enforcement is selective and politically directed; prosecution threatens entire families and social networks; emigration faces extraterritorial reach (asset freezes, Interpol notices, diaspora coercion); and institutional checks (courts, legislature) have been captured or rendered ineffective. Theater ratio (0.65): Moderate-high but rising. Legal procedures are maintained (trials are conducted, sentences announced, legislative votes occur) but outcomes are predetermined by political direction from Beijing and security apparatus. The theater serves international credibility maintenance — Hong Kong institutions appear to function according to law, but substantive autonomy has atrophied. The rising trajectory (0.40 → 0.65) reflects increasing reliance on performative legalism as the gap between form and actual decision-making widens.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victims is maximal. Beijing's institutional perspective (Rope) frames the NSL as neutral security coordination — establishing uniform national standards and eliminating destabilizing electoral competition. This perspective experiences no extraction because the mechanism operates on agents outside Beijing's frame: Hong Kong opposition and civil society are not viewed as legitimate political competitors but as security threats requiring elimination. Opposition parties experience pure Snare: they have no bargaining power, no legal recourse, no political alternatives. The security apparatus experiences Tangled Rope (mixed coordination benefit and expanded power) while institutions like courts experience Piton (performative independence masking atrophied decision-making). The analytical observer sees through the coordination framing and identifies the NSL as pure extraction: no coordination benefit accrues to suppressed populations; the suppression is total; and legal forms are maintained only for international theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is determined by their structural relationship to extraction flows. Beijing as beneficiary with complete exit capacity (can implement and modify NSL unilaterally) derives d ≈ 0.05 (full beneficiary). Opposition party members as victims with trapped exit (remaining in Hong Kong means prosecution; emigration means asset seizure and family separation) derive d ≈ 0.95 (full target). The security apparatus as mixed beneficiary-enforcer derives d ≈ 0.45 (gains power but constrained by political direction from Beijing). Civil society activists with constrained exit (some gray-zone activism possible, but opposition organizing is prosecuted) derive d ≈ 0.75 (primarily victim). Courts and institutional actors with captured autonomy derive d ≈ 0.60 (nominally independent but substantively directed). The directionality values map directly to effective extractiveness: beneficiaries with arbitrage experience low chi; trapped victims experience maximum chi; organized actors with constrained exit experience moderate chi. The snare classification emerges from high ε (base extractiveness 0.78) combined with high d values for victims (0.75–0.95) producing high effective extractiveness chi.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY — UNAMBIGUOUS SNARE: This constraint resolves the mandatrophy by eliminating the possibility of misclassifying pure extraction as coordination. Beijing could claim that the NSL provides coordination benefits: unified security standards, elimination of destabilizing electoral competition, clarification of rules for political conduct within Hong Kong. This framing is superficially plausible (resembling legitimate Rope or Tangled Rope structures) but is falsified by the empirical structure: (1) Suppression is extreme (0.88) with no offsetting coordination benefit for suppressed populations. (2) Beneficiary and victims are entirely non-overlapping: Beijing and the security apparatus alone benefit; all opposition and civil society actors bear costs. (3) Exit options are severely restricted: trapped exit (remaining means prosecution) or externally eliminated (emigration faces extraterritorial enforcement). (4) Procedural legality is maintained (courts function, trials occur) but substantive decision-making is politically directed, indicating high theater. (5) The constraint's effect is irreversible within the legal system: opposition parties cannot win elections, reorganize, or mount legal challenges; they can only dissolve. These structural features exclude Rope (which requires low extraction and minimal suppression), Tangled Rope (which requires meaningful coordination benefit for some victims), and Scaffold (which requires plausible sunset). The NSL is a snare with mandatrophy resolved at high confidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nsl_statutory_breadth,
    'Are the NSL''s statutory definitions of ''subversion,'' ''sedition,'' and ''collusion'' sufficiently determinate to function as legal rules, or do they operate as instruments of selective prosecution?',
    'Empirical analysis of NSL prosecutions: correlation between charges and political opposition status; comparison of sentencing severity for identical statutory conduct by pro-Beijing vs opposition actors; documentation of prosecutorial discretion patterns',
    'If determinate: NSL functions as coordination mechanism (Rope). If selective: NSL is pure extraction mechanism (Snare). Current evidence strongly supports the latter, but Beijing defense claims determinacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nsl_statutory_breadth, empirical, 'Whether NSL statutory definitions operate as determinate rules or selective prosecution instruments').

omega_variable(
    institutional_capture_completeness,
    'Have Hong Kong courts and legislature been completely captured by NSL enforcement, or do residual spaces for lawful opposition remain?',
    'Documentation of court rulings in NSL cases; analysis of jury instructions and sentencing rationales; identification of any successful defenses or prosecutorial failures; tracking of legislative votes on security-related measures',
    'If complete capture: all institutional perspectives (police, courts, legislature) collapse into a unified snare. If partial: some institutional actors may retain constrained agency, maintaining tangled_rope or piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_completeness, empirical, 'Degree of institutional capture under NSL enforcement regime').

omega_variable(
    emigration_as_exit,
    'Does emigration constitute a genuine exit option for opposition activists, or does Beijing''s extraterritorial prosecution (asset freezes, Interpol red notices, diaspora intimidation) eliminate this option?',
    'Tracking of emigration rates by political opposition status; documentation of extraterritorial enforcement (asset seizures, family coercion, diaspora targeting); analysis of safety and legal status in receiving countries',
    'If emigration is genuine exit: powerless agents move from ''trapped'' to ''mobile,'' shifting d downward and potentially reclassifying from Snare to Tangled Rope. If extraterritorial reach is effective: trapped exit status persists, and suppression actually increases (now global).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emigration_as_exit, empirical, 'Whether extraterritorial enforcement eliminates emigration as a viable exit option').

omega_variable(
    grey_zone_activism_durability,
    'Can civil society organizations maintain lawful advocacy in gray zones (environmental, labor, professional licensing issues) without triggering NSL prosecution, or does NSL scope creep eliminate these zones?',
    'Longitudinal tracking of NGO and labor union prosecutions; analysis of NSL case law for scope expansion; documentation of prosecutorial threat patterns (cease-and-desist letters, investigation notices for legally benign conduct)',
    'If gray zones persist: civil society retains constrained but meaningful exit options. If eliminated: gray zones collapse into uniform suppression, confirming snare classification at higher confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grey_zone_activism_durability, empirical, 'Whether gray zone activism in apolitical domains remains sustainable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hk_nsl_civic_party_disbandment, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hk_nsl_tr_t0, hk_nsl_civic_party_disbandment, theater_ratio, 0, 0.4).
narrative_ontology:measurement(hk_nsl_tr_t2, hk_nsl_civic_party_disbandment, theater_ratio, 2, 0.55).
narrative_ontology:measurement(hk_nsl_tr_t4, hk_nsl_civic_party_disbandment, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(hk_nsl_be_t0, hk_nsl_civic_party_disbandment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hk_nsl_be_t2, hk_nsl_civic_party_disbandment, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(hk_nsl_be_t4, hk_nsl_civic_party_disbandment, base_extractiveness, 4, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hk_nsl_civic_party_disbandment, enforcement_mechanism).
narrative_ontology:affects_constraint(hk_nsl_civic_party_disbandment, hong_kong_electoral_system_constraint).
narrative_ontology:affects_constraint(hk_nsl_civic_party_disbandment, hong_kong_press_freedom_suppression).
narrative_ontology:affects_constraint(hk_nsl_civic_party_disbandment, beijing_hong_kong_institutional_autonomy).

% DUAL FORMULATION NOTE:
% The NSL constraint represents the unified enforcement mechanism across multiple Hong Kong political and civil liberties domains. Decomposition into separate constraints (electoral system, press freedom, institutional autonomy) reveals the NSL's structural role as a common upstream extractor affecting all downstream political competition and expression constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
