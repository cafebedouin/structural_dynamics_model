% ============================================================================
% CONSTRAINT STORY: portugal_polarization_threshold_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   human_readable: The "Cordon Sanitaire" / Polarization Threshold in Portugal (2026 Election)
 *   domain: political/social
 *
 * SUMMARY:
 *   Portugal's 'cordon sanitaire' against Chega represents a structural
 *   constraint where traditional parties (PSD, PS, Socialist Left) and
 *   centrist electorate coordinate to exclude the right-wing populist party
 *   from any coalition government, regardless of electoral performance. This
 *   mechanism emerged after Chega's rapid growth from 2019-2022 and
 *   crystallized during the 2024-2025 government formation crisis. The
 *   constraint exhibits tangled rope properties: it solves a genuine
 *   coordination problem for the traditional establishment (preventing a
 *   transformative power shift) while simultaneously extracting from Chega
 *   voters through delegitimization and exclusion from proportional voice in
 *   governance. The mechanism relies on enforced consensus norms rather than
 *   formal constitutional barriers, giving it a hybrid character —
 *   coordination dressed as democratic gatekeeping, extraction justified as
 *   system preservation. Theater ratio (0.58) reflects that the cordon
 *   operates through repeated ritual exclusion (coalition negotiations, media
 *   delegitimization, institutional non-cooperation) while remaining formally
 *   unconstitutional. The 2026 Presidential election creates a critical
 *   juncture: either Chega consolidates toward governing-party stability
 *   (forcing normalization of the cordon into formal rules or collapse), or
 *   remains a volatile protest formation (enabling continued exclusion
 *   cycles). The constraint's future depends on whether the suppression
 *   (0.68) can be sustained indefinitely through delegitimization, or whether
 *   growing electoral support (15%+ national, 20%+ in regional areas) will
 *   breach the psychological acceptance threshold for such exclusion.
 *
 * KEY AGENTS:
 *   - Chega party & supporters (powerless/trapped): Primary victim — excluded from coalition participation despite electoral legitimacy; bears full cost of delegitimization extraction
 *   - Traditional party establishment: PSD, PS, Socialist Left (institutional/arbitrage) — Primary beneficiaries; experience cordon as pure coordination solving collective action problem of preventing power-sharing
 *   - Centrist & progressive electorate (moderate/constrained): Secondary victims/beneficiaries — benefit from cordon's prevention of Chega legitimacy but constrained by binary political framing and reduced genuine policy debate
 *   - Civil society/democratic defense coalition (organized/constrained): Secondary actors viewing cordon as temporary defensive measure with sunset logic as Chega is neutered
 *   - Constitutional/legal framework (institutional/constrained): Meta-actor; maintains cordon through normative convention rather than formal statute, creating piton-like degradation as the unwritten rule grows performative
 *   - Analytical observer (analytical/analytical): Civilizational perspective revealing the paradox of using anti-democratic exclusion to defend democracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(portugal_polarization_threshold_2026, 0.52).
domain_priors:suppression_score(portugal_polarization_threshold_2026, 0.68).
domain_priors:theater_ratio(portugal_polarization_threshold_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(portugal_polarization_threshold_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(portugal_polarization_threshold_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(portugal_polarization_threshold_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(portugal_polarization_threshold_2026, tangled_rope).
narrative_ontology:human_readable(portugal_polarization_threshold_2026, "The \"Cordon Sanitaire\" / Polarization Threshold in Portugal (2026 Election)").
narrative_ontology:topic_domain(portugal_polarization_threshold_2026, "political/social").

domain_priors:requires_active_enforcement(portugal_polarization_threshold_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(portugal_polarization_threshold_2026, traditional_party_establishment).
narrative_ontology:constraint_beneficiary(portugal_polarization_threshold_2026, center_left_right_consensus_defenders).
narrative_ontology:constraint_victim(portugal_polarization_threshold_2026, chega_electoral_legitimacy).
narrative_ontology:constraint_victim(portugal_polarization_threshold_2026, political_participation_equality).
narrative_ontology:constraint_victim(portugal_polarization_threshold_2026, democratic_inclusion_norm).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHEGA CONSTITUENCY (SNARE) — Trapped within electoral system that delegitimizes their votes and excludes their preferred party from power-sharing regardless of electoral performance. Cordon sanitaire functions as pure extraction: constrains meaningful political voice, bars coalition participation, and enforces subordinate status. Maximum coercion with minimal coordination benefit — exit is citizenship change or resigned non-participation.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CENTRIST/PROGRESSIVE ELECTORATE (TANGLED ROPE) — Constrained by fear of Chega electoral growth and potential mainstream legitimacy. The cordon sanitaire benefits them (protects center-left/right consensus) but also extracts costs: enforces binary framing, reduces genuine policy debate, and creates political brittleness through delegitimization rather than persuasion. Mixed extraction and coordination — genuine asymmetry maintained through enforcement.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRADITIONAL PARTY ESTABLISHMENT (ROPE) — Experiences cordon sanitaire as pure coordination mechanism: solves the collective action problem of preventing Chega coalition participation without requiring negotiation among rivals. Net beneficiary with low exit costs — can defect if cordon breaks, but maintains power asymmetry during enforcement. Arbitrage position allows playing multiple coalitions against each other while cordon holds.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL SOCIETY & DEMOCRATIC DEFENSE COALITION (SCAFFOLD) — Organized actors (labor unions, civil rights groups, European integration advocates) see the cordon as a temporary defensive measure against what they perceive as democratic backsliding. View includes sunset logic: once Chega is neutered (internal divisions, lack of governing experience, economic integration pressures), the cordon can dissolve naturally. Theater ratio moderate — actual organizing for democratic resilience mixed with symbolic 'democracy defense' rhetoric. Sunset horizon: 2-3 electoral cycles if Chega fails to consolidate support.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL & LEGAL FRAMEWORK (PITON) — The Portuguese Constitution and Electoral Code contain no formal prohibition on Chega coalition participation — the cordon sanitaire is a norm-based convention, not a statutory gate. This norm persists through institutional inertia and repeated enforcement (every coalition calculation rehearses 'but never with Chega') despite being formally unwritten and periodically contested. Theater ratio high (0.65+) — the repeated ritual of excluding Chega through coalition arithmetic while legally unconstrained to do so. The framework appears degraded: original purpose (preventing democratic breakdown) increasingly performative as Chega becomes integrated into routine electoral competition.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DEMOCRATIC THEORY (TANGLED ROPE) — From civilizational perspective, the cordon sanitaire represents a structural paradox: enforcing democratic norms through anti-democratic exclusion. Genuine coordination function (maintaining constitutional democratic order) coupled with asymmetric extraction (denying meaningful voice to ~15% of electorate). The high suppression (0.68) reflects that the mechanism relies on delegitimization and norm-based coercion rather than persuasion. Extractiveness (0.52) reflects the real constraint: a substantial voting bloc cannot participate in government formation regardless of electoral outcome. This is neither pure coordination (rope) nor illegitimate suppression (snare) — it is the hybrid form that justifies its extraction by invoking system preservation.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.52): Moderate-high. The cordon sanitaire functions as genuine extraction: ~15% of Portuguese electorate is systematically excluded from proportional voice in government formation, with no exit option short of emigration or disengagement. This is not marginal suppression of fringe extremism (which might justify lower extraction) — Chega represents a substantial, electorally stable constituency. However, the extraction is not maximal (0.70+) because: (1) Chega maintains electoral viability and party organization (not completely crushed), (2) the cordon is norm-based rather than violent/total, (3) some subnational power-sharing occurs (e.g., municipal coalitions in certain districts). Suppression (0.68): High. Multiple coercive mechanisms enforce the cordon: media delegitimization, parliamentary ostracism, coalition arithmetic designed to freeze out Chega, institutional non-cooperation, and normative shaming. These are substantial barriers to voice even without explicit legal prohibition. Theater ratio (0.58): Moderate. The repeated exclusion through coalition-building rituals has performative elements (the endless 'never with Chega' declarations, the theatrical impossibility of coalition math), but genuine institutional constraint remains — no mathematical possibility of coalition without Chega would require institutional change. As enforcement becomes routine and Chega normalized as 'the establishment opposition,' theater increases.
 *
 * PERSPECTIVAL GAP:
 *   The cordon sanitaire creates one of the sharpest perspectival gaps in democratic constraint analysis. From the traditional establishment's perspective, the cordon is a elegant coordination solution (Rope): all major parties cooperate to prevent Chega legitimation without negotiating explicitly — a costless enforcement mechanism. From Chega supporters' perspective, it is pure extraction (Snare): systematic exclusion from proportional voice with zero exit. From civil society's perspective, it is a time-limited defensive measure (Scaffold) that should dissolve once the perceived threat matures or fails. From the analytical position, it is clearly the hybrid Tangled Rope: real coordination function (democratic order preservation) legitimizing real asymmetric extraction (exclusion of a voting bloc). The perspectives do not differ in observable facts — they differ in structural position relative to the extraction flow. This gap explains why Portugal's political class experiences the cordon as normal democratic gatekeeping while ~15% of voters experience it as systematic illegitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent: Chega supporters are victims with zero exit (trapped) — derived d ≈ 0.95, f(d) ≈ 1.42 — experience maximum effective extraction. Traditional party institutional actors are beneficiaries with full arbitrage (can switch between coalitions while cordon holds) — derived d ≈ 0.05, f(d) ≈ -0.12 — experience negative effective extraction (subsidy). Centrist electorate are both partial beneficiaries (cordon prevents Chega power) and victims (constrained political choice, binary framing) with constrained exit (cannot easily vote for minority parties without strategic concern) — derived d ≈ 0.50-0.65 depending on party affiliation, f(d) ≈ 0.65-1.00 — experience moderate effective extraction. Civil society organized actors with constrained exit but institutional access — derived d ≈ 0.35-0.45, f(d) ≈ 0.40-0.65 — experience moderate extraction. The analytical observer (pure analytical context) — derived d ≈ 0.72, f(d) ≈ 1.15 — observes the full structure without experiencing direct extraction, enabling clarity on the tangled_rope classification that beneficiaries and victims cannot perceive from their positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The cordon sanitaire resolves the mandatrophy by revealing that the system preservation narrative (mountain-type inevitability) is actually a contingent institutional arrangement (tangled rope). The traditional parties initially framed the cordon as a constitutional necessity ('we cannot work with extremism') — a false summit. The structural analysis reveals it as a norm-based coordination mechanism protecting a specific party settlement, extracting from Chega voters. The mandatrophy is resolved at the analytical level: the cordon is neither inevitable natural law nor pure extraction theater, but a genuine hybrid — legitimate coordination function (preventing a transformative power realignment) justified by invoking system preservation, combined with genuine asymmetric extraction (exclusion proportional only to electoral minority status, not to illegality or anti-democratic behavior). This hybrid classification prevents both the false naturalization (mountain) and the false delegitimation (pure snare). It forces the question: Is the extraction justified by the coordination benefit? This is a question of democratic values and coalition design, not of natural law or systemic necessity. The mandatrophy resolution thus transfers the constraint from the domain of objective structural necessity to the domain of political choice and legitimacy debate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chega_consolidation_trajectory,
    'Will Chega consolidate into a durable governing party (requiring permanent institutional adaptation) or remain a volatile protest movement (enabling cordon sustainability)?',
    '10-year longitudinal tracking: internal party organizational capacity, leadership succession stability, policy coherence across municipal/national levels, voter retention rates across electoral cycles, evidence of technical/administrative readiness for governing',
    'If consolidates: cordon becomes unsustainable norm — either breaks or transforms into formal constitutional barrier (very costly). If remains volatile: cordon can persist through repeated exclusion cycles, maintaining the tangled rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chega_consolidation_trajectory, empirical, 'Whether Chega consolidates into durable governing party or remains volatile protest movement').

omega_variable(
    european_pressure_dynamics,
    'Will EU democratic standards and transnational liberal networks enforce or undermine the cordon sanitaire?',
    'Analysis of EU pressure on Portugal regarding democratic inclusion, comparison with cordon applications in France/Italy/Spain, measurement of EU institutional positioning toward Chega, tracking of transnational media coverage and legitimacy frames',
    'If EU reinforces cordon: external validation strengthens norm enforcement. If EU pressures inclusion: cordon faces legitimacy deficit from above, forcing formalization or collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(european_pressure_dynamics, empirical, 'Whether EU pressure supports or undermines cordon enforcement').

omega_variable(
    extraction_legitimacy_threshold,
    'At what electoral support level does delegitimization extraction become politically unsustainable (i.e., when does excluding 20%+ of voters from power-sharing breach the norm itself)?',
    'Comparative analysis: cordon stability in democracies with high minority electoral support (Belgium 15%+, Denmark 15%+, Netherlands 17%+); polling of mainstream electorate acceptance of cordon as Chega support fluctuates; party leadership statements on coalition normalization',
    'If threshold < current support (~15%): cordon faces immediate normalization pressure. If threshold > 20%+: cordon can sustain through next 2-3 electoral cycles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_legitimacy_threshold, conceptual, 'Electoral support level at which delegitimization extraction becomes politically unsustainable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(portugal_polarization_threshold_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ppol_tr_t0, portugal_polarization_threshold_2026, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ppol_tr_t3, portugal_polarization_threshold_2026, theater_ratio, 3, 0.54).
narrative_ontology:measurement(ppol_tr_t6, portugal_polarization_threshold_2026, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(ppol_be_t0, portugal_polarization_threshold_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ppol_be_t3, portugal_polarization_threshold_2026, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(ppol_be_t6, portugal_polarization_threshold_2026, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(portugal_polarization_threshold_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(portugal_polarization_threshold_2026, portuguese_party_system_realignment).
narrative_ontology:affects_constraint(portugal_polarization_threshold_2026, european_democratic_standards_variance).

% DUAL FORMULATION NOTE:
% The cordon sanitaire operates at two structural levels: (1) The immediate electoral coordination mechanism (this story, extractiveness 0.52, tangled_rope), and (2) the longer-term party system realignment it constrains (affects_constraints entry). The immediate constraint is the exclusion mechanism; the network dependency captures how this mechanism shapes the viability of traditional party coalitions and whether a durable three-bloc system (center-left, center-right, right-populist) can emerge. As the cordon degrades or formalizes, the party system realignment constraint's properties will shift accordingly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(portugal_polarization_threshold_2026, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
