% ============================================================================
% CONSTRAINT STORY: portugal_polarization_threshold_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_portugal_polarization_threshold_2026, []).

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
 *   constraint_id: portugal_polarization_threshold_2026
 *   human_readable: The "Cordon Sanitaire" / Polarization Threshold in Portuguese Politics
 *   domain: political/social
 *
 * SUMMARY:
 *   The Portuguese cordon sanitaire against Chega represents a structural
 *   constraint on democratic representation that functions simultaneously as
 *   coordination mechanism and extraction device. The traditional party
 *   coalition (PSD, PS, centrist parties) maintains a self-enforcing
 *   exclusion rule against far-right party Chega despite its consistent
 *   electoral performance at 25-35% in voting intention surveys. This
 *   constraint exhibits the core mandatrophy tension: from the establishment
 *   perspective, the cordon sanitaire coordinates democratic defense against
 *   populist erosion and norm-breaking; from the Chega voter perspective, it
 *   extracts representation from a substantial electoral minority through
 *   institutional gatekeeping. The constraint's enforceability depends on
 *   coalition mathematics (whether traditional parties need Chega seats) and
 *   norm credibility (whether exclusion appears defensive or self-serving).
 *   As the 2026 Presidential election approaches, the sustainability of this
 *   enforcement mechanism becomes empirically contested.
 *
 * KEY AGENTS:
 *   - Traditional Party Coalition (PSD/PS/centrist parties): Primary beneficiary (institutional/arbitrage) — coordinates around Chega exclusion to maintain government formation control and elite consensus
 *   - Chega Voters: Primary victim (powerless/trapped) — denied meaningful representation despite substantial electoral share; no viable exit through voting
 *   - Institutional Media and Civil Society: Enforcer/partial victim (powerful/mobile) — both maintains exclusionary narratives and constrained by appearance of partisanship
 *   - Marginalized Regional Communities: Secondary victim (moderate/constrained) — excluded from Chega representation option while benefiting from traditional parties' redistribution pledges
 *   - Democratic Reformers: Organized agent (organized/mobile) — seeking to replace cordon logic with policy-based competitive norms; sees the constraint as sunset-able
 *   - EU Integration Framework: Institutional constraint (institutional/constrained) — provides historical legitimacy for cordon through anti-extremism norms; increasingly performative
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both coordination and extraction functions; embodies the mandatrophy tension
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(portugal_polarization_threshold_2026, 0.52).
domain_priors:suppression_score(portugal_polarization_threshold_2026, 0.62).
domain_priors:theater_ratio(portugal_polarization_threshold_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(portugal_polarization_threshold_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(portugal_polarization_threshold_2026, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(portugal_polarization_threshold_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(portugal_polarization_threshold_2026, tangled_rope).
narrative_ontology:human_readable(portugal_polarization_threshold_2026, "The \"Cordon Sanitaire\" / Polarization Threshold in Portuguese Politics").
narrative_ontology:topic_domain(portugal_polarization_threshold_2026, "political/social").

domain_priors:requires_active_enforcement(portugal_polarization_threshold_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(portugal_polarization_threshold_2026, traditional_party_coalition).
narrative_ontology:constraint_beneficiary(portugal_polarization_threshold_2026, establishment_consensus).
narrative_ontology:constraint_victim(portugal_polarization_threshold_2026, chega_political_representation).
narrative_ontology:constraint_victim(portugal_polarization_threshold_2026, voter_choice_spectrum).
narrative_ontology:constraint_victim(portugal_polarization_threshold_2026, democratic_representation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHEGA VOTER (SNARE) — Trapped within the constraint. Despite substantial electoral support (30%+ in some polls), constrained from meaningful representation through systematic exclusion via cordon sanitaire. No viable exit: cannot vote for Chega in viable coalition scenarios; cannot shift support to establish parties without abandoning policy preferences. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72. High effective extraction.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED REGIONAL COMMUNITIES (TANGLED ROPE) — Constrained by economic exclusion and regional imbalance. Both bear costs of the cordon (representation barriers) and benefit from the constraint (establishment parties' social safety net pledges made credible through non-Chega coalition commitment). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55. Mixed coordination-extraction hybrid.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRADITIONAL PARTY COALITION (ROPE) — Primary beneficiary. The cordon sanitaire enables coordination among otherwise-competing parties (PSD, PS, centrist actors) through shared exclusion logic. Creates coalition discipline and reduces inter-party fragmentation. Arbitrage exit: can shift coalitional weight, engage with different partners, negotiate government formation from position of strength. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.04. Negative effective extraction = net beneficiary. Sees the constraint as coordination mechanism.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL MEDIA AND CIVIL SOCIETY (TANGLED ROPE) — Both enforces and constrained by the cordon. Benefits from narratives that frame Chega as outside-the-pale (coordination function: stabilizes democratic norms, signals shared values). Constrained by risk of appearing partisan (suppression of critical coverage, theater ratios increase). d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34. Mobile exit: can shift framing or exit narratives if reputational cost rises.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: EU INTEGRATION NORMS / DEMOCRATIC LEGITIMACY (PITON) — Historically justified the cordon as defense against authoritarian drift (1980s/90s context against far-right extremism). Now largely performative: Chega does not present an immediate coup risk, but exclusion persists through institutional inertia and norm citation rather than current threat assessment. theater_ratio=0.58 suggests moderate theatrical maintenance. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.28. Constrained exit: EU norms are embedded in Portuguese governance frameworks; cannot easily abandon without institutional friction.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DEMOCRATIC REFORMERS / PLURALIST CIVIL SOCIETY (SCAFFOLD) — Organized agents (civic organizations, academic networks, pluralist media) seeking to replace cordon logic with policy-based competitive dynamics and inclusive representation norms. See the constraint as temporary: as Chega moderates (or fails to), inclusive representation becomes viable. Sunset mechanism: if Chega is demonstrably incompatible with democratic norms through electoral behavior rather than exclusion-by-decree, the cordon becomes unnecessary. Mobile exit: can advocate for norm change, shift political culture. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.17. Low effective extraction because agents have agency and see a path forward.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COMPARATIVE THEORY (TANGLED ROPE) — Sees the cordon as both functional coordination (protecting consensus democratic norms against populist erosion) and extractive (denying representation to 30%+ of voters, concentrating power in establishment coalition). Analytically indexes the constraint as tangled because it exhibits both functions simultaneously. d≈0.72, f(d)≈1.15, σ=1.1 → χ≈0.66. This perspective embodies the central mandatrophy tension.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portugal_polarization_threshold_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(portugal_polarization_threshold_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(portugal_polarization_threshold_2026, TR),
    TR >= 0.70.

:- end_tests(portugal_polarization_threshold_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts representation from a substantial electoral minority (25-35% voting intention) through institutional rule-making rather than electoral competition. However, extractiveness is not maximal (0.70+) because: (a) voters retain formal franchise rights; (b) some Chega voter preferences may be absorbed into traditional party platforms; (c) the extraction mechanism is partially justified by perceived democratic risk (even if that risk is now contested). The moderate value reflects that this is a genuine Tangled Rope rather than pure Snare. Suppression (0.62): Moderate-high. Significant barriers include: systematic exclusion from coalition mathematics (structural), media normalization of Chega-as-dangerous (discursive), electoral system design that benefits traditional parties, and career/reputational risk for politicians engaging with Chega platforms. However, suppression is not total (0.80+) because: Chega operates openly, contests elections, and maintains organizational capacity. Theater ratio (0.58): Moderate. The cordon sanitaire maintains performative content: media coverage emphasizes Chega's norm-breaking rhetoric rather than policy specifics; establishment parties define legitimacy circularly as 'non-Chega membership'; democratic defense narratives are deployed even when Chega's behavior is indistinguishable from other conservative parties. But the performance is not maximal (0.70+) because policy competition still occurs, and some genuine institutional risk assessment continues.
 *
 * PERSPECTIVAL GAP:
 *   The Chega voter perspective (Snare) and traditional party perspective (Rope) define opposite classification outcomes from identical structural data. The traditional coalition experiences the cordon as coordination: shared exclusion discipline creates coalition clarity, reduces inter-party warfare, and strengthens their negotiating position. The Chega voter experiences the same constraint as pure extraction: a systematic denial of representation through institutional rules rather than electoral competition. This perspectival gap is not an observational ambiguity — it reflects genuine structural asymmetry. The established parties benefit from the constraint; Chega voters bear costs. The mandatrophy tension emerges from the fact that both perspectives are analytically valid: the constraint IS a coordination mechanism (solves the collective action problem of traditional party coalition formation), and it IS asymmetric extraction (concentrates power and denies representation to a large minority). The analytical observer's Tangled Rope classification resolves the tension by acknowledging both functions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Chega voter: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction directionality. The voter has no meaningful exit within the Portuguese democratic system; cannot shift support without abandoning core preferences; cannot coordinate with like-minded voters in viable governance coalitions. Traditional party coalition: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Negative effective extraction — net beneficiary. The coalition has multiple exit options (can shift partner composition, negotiate government terms from strength, adapt policy positions) and derives clear benefits from cordon discipline. Institutional media: Powerful + mobile. d≈0.50, f(d)≈0.65. Both enforces the constraint (through narrative framing) and experiences constraint-costs (risk of appearing partisan; theater maintenance burden). Mobile exit provides gradual pathway toward norm reframing. Democratic reformers: Organized + mobile. d≈0.35, f(d)≈0.32. Low extraction because agents have structural agency and perceive a viable pathway forward (democratic norm evolution). Comparative democratic theory: Analytical perspective with d≈0.72 embodies the structural tension — sees both the coordination function (democratic defense) and the extraction function (representation denial) equally.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint centers on whether the cordon sanitaire is 'defensive coordination' (justified exclusion of norm-breakers) or 'extractive gatekeeping' (concentration of power dressed in democratic language). The constraint resolves the mandatrophy through perspectival indexing: from the beneficiary's view (institutional/arbitrage), it is Rope — genuinely solving the problem of coalition coordination. From the victim's view (powerless/trapped), it is Snare — systematic extraction without exit. From the organized reformer's view (organized/mobile), it is Scaffold — temporary constraint being superseded by inclusive norms. From the analytical view, it is Tangled Rope — containing both genuine coordination function (coalition discipline against fragmentation) and genuine asymmetric extraction (representation denial to 30%+ of voters). The empirical resolution path lies through omega variables: if Chega demonstrates democratic commitment through electoral behavior and policy moderation, the coordination justification weakens and the constraint appears increasingly extractive. If Chega's behavior validates the exclusion logic, the cordon appears more defensible as coordination. The theater_ratio trajectory (rising from 0.42 to 0.58) suggests that performative maintenance is increasing — the constraint is drifting toward Piton classification as institutional inertia replaces genuine threat assessment. If theater continues rising above 0.70, the constraint becomes a degraded Piton: maintained through habit and narrative rather than functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chega_moderation_trajectory,
    'Will Chega demonstrate democratic commitment through electoral behavior, or entrench as a destabilizing force that justifies perpetual exclusion?',
    'Longitudinal assessment of Chega policy proposals, coalition signaling, and institutional behavior if included in governance roles; comparison with other European far-right parties'' trajectories (Italy''s Lega, Poland''s Law and Justice post-election maturation)',
    'If moderate: cordon becomes normatively indefensible; constraint shifts to Scaffold with sunset. If destabilizing: cordon''s Snare classification for voters becomes analytically justified; constraint persists as Tangled Rope from establishment perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chega_moderation_trajectory, empirical, 'Trajectory of Chega moderation or entrenchment').

omega_variable(
    voter_preference_stability,
    'Is Chega support a stable constituency preference or a protest vote that dissolves with economic improvement or establishment party repositioning?',
    'Panel voter surveys tracking individual transitions; demographic stability of Chega base across election cycles; sensitivity analysis of support to economic indicators, immigration policy shifts, regional investment',
    'If stable: Snare classification is durable; exclusion becomes permanent structural feature, driving radicalization. If protest: support may erode; cordon becomes transitional Scaffold, and constraint dissolves naturally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_preference_stability, empirical, 'Stability of Chega voter base').

omega_variable(
    enforcement_sustainability,
    'Can the cordon sanitaire remain credible if Chega continues gaining seats and mainstream parties face coalition mathematics that require inclusion?',
    'Simulation of 2026, 2030 election scenarios with current polling; assessment of coalition viability without Chega as seat count grows; historical precedent from other democracies (Belgium N-VA, Italy FdI)',
    'If unsustainable: cordon collapses, constraint transitions to Rope or Scaffold as inclusion becomes structural inevitability. If sustainable: enforcement intensifies, theater ratio rises, Piton classification emerges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'Structural sustainability of cordon enforcement as Chega grows').

omega_variable(
    democratic_norm_internalization,
    'Are exclusionary norms (cordon) genuinely internalized as democratic principles, or performatively maintained for elite convenience?',
    'Comparative survey of elite vs. citizen support for cordon logic; assessment of whether rejection thresholds adjust with Chega policy positions vs. fixed by institutional decree; discourse analysis of cordon justifications over time',
    'If genuinely internalized: cordon reflects authentic democratic values, Tangled Rope classification holds. If performative: the constraint is better classified as Snare (extraction disguised as protection) from analytical view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_norm_internalization, conceptual, 'Degree of internalization of cordon sanitaire as democratic norm vs. elite convenience').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(portugal_polarization_threshold_2026, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pt_polar_tr_t0, portugal_polarization_threshold_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pt_polar_tr_t2, portugal_polarization_threshold_2026, theater_ratio, 2, 0.5).
narrative_ontology:measurement(pt_polar_tr_t4, portugal_polarization_threshold_2026, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(pt_polar_be_t0, portugal_polarization_threshold_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pt_polar_be_t2, portugal_polarization_threshold_2026, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(pt_polar_be_t4, portugal_polarization_threshold_2026, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(portugal_polarization_threshold_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(portugal_polarization_threshold_2026, portuguese_coalition_formation_rules).
narrative_ontology:affects_constraint(portugal_polarization_threshold_2026, populist_mobilization_dynamics_southern_europe).

% DUAL FORMULATION NOTE:
% The cordon sanitaire can be decomposed into two distinct constraints: (1) coalition_formation_coordination (ε≈0.15, Rope) — the mechanism by which traditional parties coordinate to form government, which genuinely solves collective action; (2) chega_representation_exclusion (ε≈0.65, Snare) — the systematic denial of representation to a large voter group. The current story models these as a single Tangled Rope constraint because they are structurally coupled: the coordination mechanism works by enforcing the exclusion. Separating them would require showing that traditional party coalition could coordinate without Chega exclusion, which is counterfactually weak. The network links show how this constraint affects broader Portuguese democratic structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(portugal_polarization_threshold_2026, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
