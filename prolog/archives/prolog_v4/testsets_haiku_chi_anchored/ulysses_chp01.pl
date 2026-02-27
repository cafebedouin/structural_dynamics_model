% ============================================================================
% CONSTRAINT STORY: ulysses_chp01
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp01, []).

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
 *   constraint_id: ulysses_chp01
 *   human_readable: The Martello Tower Usurpation
 *   domain: social/political/religious
 *
 * SUMMARY:
 *   In June 1904, Stephen Dedalus moves into the Martello tower in Dublin's
 *   Sandycove, sharing it with Buck Mulligan and others. Originally built as
 *   a defensive fortification against Napoleonic invasion—a functional Rope
 *   mechanism coordinating Irish/British coastal defense—the tower has by
 *   1904 become an instrument of extraction and constraint. The structure now
 *   serves as a site where Stephen's intellectual and spiritual autonomy are
 *   subordinated to Mulligan's social dominance and the broader British
 *   imperial order's control of Irish space and culture. The Martello tower
 *   exemplifies how coordination mechanisms can be repurposed as instruments
 *   of extraction, and how structural constraints operate across multiple
 *   scales simultaneously: the interpersonal (Mulligan's dominance over
 *   Stephen), the national (Irish constraint under British imperial order),
 *   and the historical (a defensive structure's original function atrophied,
 *   persisting through inertia).
 *
 * KEY AGENTS:
 *   - Stephen Dedalus: Primary victim (powerless/trapped) — intellectual and spiritual autonomy constrained; cannot exit without loss of shelter and intellectual community; bears full cost of Mulligan's dominance and tower's psychological weight
 *   - Buck Mulligan: Primary beneficiary (institutional/arbitrage) — controls tower access; extracts intellectual labor and social dominance from Stephen; has arbitrage options (can invoke university credentials, medical prospects, intellectual standing) but chooses to remain and dominate
 *   - British Imperial Order: Structural beneficiary (organized/constrained) — tower is instrument of imperial presence and surveillance; organizes Irish space; suppresses Irish autonomy through physical and psychological means
 *   - Irish Intellectual Class: Secondary victim (moderate/constrained) — shares broader constraint of cultural subordination; higher exit costs than Mulligan but better options than Stephen; emigration, silence, or capitulation to English norms
 *   - Irish Cultural Resistance (Literary Renaissance, Gaelic Revival): Emerging alternative (organized/mobile) — represents sunset pathway; Abbey Theatre, nationalist presses, Gaelic language revival provide alternatives to English cultural dominance
 *   - Historical Observer: Analytical view of institutional decay (institutional/arbitrage) — tower's original defensive function (Rope) has been replaced by theatrical performance and historical nostalgia; persists through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp01, 0.58).
domain_priors:suppression_score(ulysses_chp01, 0.68).
domain_priors:theater_ratio(ulysses_chp01, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp01, extractiveness, 0.58).
narrative_ontology:constraint_metric(ulysses_chp01, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ulysses_chp01, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp01, snare).
narrative_ontology:human_readable(ulysses_chp01, "The Martello Tower Usurpation").
narrative_ontology:topic_domain(ulysses_chp01, "social/political/religious").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp01, buck_mulligan).
narrative_ontology:constraint_beneficiary(ulysses_chp01, british_institutional_order).
narrative_ontology:constraint_victim(ulysses_chp01, stephen_dedalus).
narrative_ontology:constraint_victim(ulysses_chp01, irish_cultural_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STEPHEN DEDALUS (SNARE) — Shares the tower with Mulligan and others, unable to escape the spatial, social, and psychological entrapment. No exit available without loss of shelter, social standing (however degraded), and intellectual community. d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.65.
constraint_indexing:constraint_classification(ulysses_chp01, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: IRISH INTELLECTUAL CLASS (SNARE) — The tower exemplifies broader constraints on Irish cultural and intellectual autonomy. Exit is possible but costly: emigration, silence, or capitulation to English cultural norms. d≈0.78, f(d)≈1.18, σ=0.9 → χ≈0.60.
constraint_indexing:constraint_classification(ulysses_chp01, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUCK MULLIGAN (ROPE) — Controls the tower's use and Stephen's presence. Benefits from access to Stephen's intellectual labor, rent-sharing, and social dominance. Experiences the constraint as coordination: the tower enables intellectual gathering and social bonding (however toxic). d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(ulysses_chp01, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: BRITISH IMPERIAL ORDER (TANGLED ROPE) — The tower itself is a coordination mechanism (coastal defense against French invasion) that has been repurposed as an instrument of cultural extraction. The tower organizes Irish space under British surveillance; it enables imperial presence while suppressing Irish autonomy. Mulligan's use of it mirrors this dual function. d≈0.25, f(d)≈0.05, σ=1.0 → χ≈0.03.
constraint_indexing:constraint_classification(ulysses_chp01, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: IRISH CULTURAL RESISTANCE (SCAFFOLD) — The Literary Renaissance and Gaelic Revival represent an emerging alternative structure that will eventually bypass the tower's dominance. Early 20th century shows sunset logic: as Irish cultural institutions mature (Abbey Theatre, nationalist presses), the tower becomes less central to Irish intellectual life. d≈0.45, f(d)≈0.42, σ=1.0 → χ≈0.18.
constraint_indexing:constraint_classification(ulysses_chp01, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: HISTORICAL/CIVILIZATIONAL VIEW (PITON) — The tower's original defensive function (Rope: coordination against French invasion) has atrophied. By 1904, it persists through institutional inertia and theatrical historical significance rather than functional necessity. Theater_ratio=0.62 reflects that the tower is now primarily a site of social performance (intellectual gathering, bohemian pose) rather than military function. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(ulysses_chp01, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp01_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp01, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp01, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp01, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp01, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp01_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. Mulligan extracts intellectual engagement, domestic labor (implicit), and social deference from Stephen. Stephen gains shelter and intellectual community but at significant cost to autonomy. The constraint is not as severe as pure predation (0.70+) because Stephen retains some agency—he can and does eventually leave—but the extraction is substantial. The trend shows increasing extractiveness from 0.38 to 0.58 over the six-month interval, reflecting Mulligan's growing dominance and Stephen's increasing psychological burden. Suppression (0.68): High. Multiple barriers prevent Stephen's exit: lack of alternative housing, social stigma of homelessness in 1904 Dublin, intellectual isolation if he abandons Mulligan's circle, limited income, religious and familial shame, lack of institutional support. Mulligan controls not only the physical space but also the social validation within it. Theater ratio (0.62): Moderate-high. The tower's social life involves significant performative content—Mulligan's mockery of religious ritual, Stephen's intellectual posturing, the bohemian aesthetic of the intellectual gathering—masking the underlying extraction mechanism. The theater increases over time (0.48 → 0.62) as Mulligan's performances become more elaborate and Stephen's resistance becomes more theatrical rather than substantive.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radical perspectival divergence. From Stephen's position (powerless/trapped), the tower is a Snare: he is imprisoned by circumstance, personality, and structural forces. From Mulligan's position (institutional/arbitrage), the tower is a Rope: it enables intellectual coordination, social bonding, and the performance of bohemian brilliance—Mulligan experiences it as enabling, not constraining. From the British imperial order's perspective, the tower is a Tangled Rope: it serves both a coordination function (coastal infrastructure, intellectual stability) and an extraction function (control of Irish space, suppression of autonomy). From the Irish cultural resistance perspective, the tower is a Scaffold: it appears temporary, soon to be superseded by emerging Irish institutions. From the historical civilizational view, the tower is a Piton: its original military function (Rope against French invasion) has atrophied, and it persists through theatrical historical significance rather than actual military value. The perspectival gaps reveal that the same physical structure operates as different constraint types for different agents, and that the type-classification depends entirely on structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Stephen Dedalus: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction pressure. No exit available without substantial cost; bears full burden of Mulligan's dominance and the tower's psychological weight. Irish intellectual class: Victim + constrained → d≈0.78, f(d)≈1.18. High extraction pressure but not maximal; emigration and silence are available (costly) exits. Mulligan: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; has arbitrage options (university credentials, medical career, travel) but chooses to remain and dominate Stephen. British imperial order: Structural beneficiary but constrained by Irish resistance → d≈0.25, f(d)≈0.05. Low effective extraction from perspective of empire's civilization-scale ambitions; the extraction is real but the empire's own constraints limit its absolute power. Irish cultural resistance: Organized + mobile → d≈0.45, f(d)≈0.42. Moderate extraction but with high agency; emerging alternatives (Abbey Theatre, Gaelic Revival) provide exit pathway and reduce constraint's long-term force.
 *
 * MANDATROPHY ANALYSIS:
 *   The Martello tower resolves the mandatrophy by clarifying the distinction between coordination and extraction through temporal decomposition. Originally (pre-1804), the tower was a pure Rope: it coordinated Irish and British defensive interests against Napoleonic invasion. By 1904, the coordination function is dead (no French invasion threat), but the structure persists and has been repurposed as an extraction mechanism. The snare classification reflects this repurposing: Mulligan and the British order now use the tower's structure (physical location, historical significance, social prestige) to extract from Stephen. The theater ratio captures this repurposing: the tower's social performances (bohemian gathering, intellectual discourse, religious mockery) mask and sustain the extraction. The Piton perspective recognizes that the tower's original function has atrophied but that institutional inertia maintains it. The Scaffold perspective captures that emerging Irish institutions (Abbey Theatre, Gaelic Revival) represent a structural alternative that will eventually render the tower less central to Irish intellectual life. No single type is 'correct'—the tower's constraint-type changes over time, and the Snare classification reflects its current (1904) structural function, not its historical origin (Rope) or its eventual destination (obsolescence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mulligan_actual_agency,
    'Is Mulligan a genuine beneficiary exercising agency over Stephen, or is Mulligan himself trapped within the same imperial structure, merely performing dominance?',
    'Textual analysis of Mulligan''s interior consciousness and subsequent life trajectory; comparison of Mulligan''s exit capacity to Stephen''s across temporal horizons',
    'If Mulligan has genuine agency: Snare classification confirmed (clear extractor/victim). If Mulligan is also trapped: the constraint is Tangled Rope for both, with theater masking reciprocal extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mulligan_actual_agency, conceptual, 'Whether Mulligan''s apparent agency reflects genuine structural position or performative dominance').

omega_variable(
    tower_as_trap_vs_space,
    'Does the tower function as a trap constraining Stephen, or as a necessary liminal space enabling his intellectual work and eventual escape narrative?',
    'Analysis of Stephen''s creative output during tower residence vs after departure; assessment of whether the constraint enables the very resistance that transcends it',
    'If trap dominant: Snare classification holds. If liminal space dominant: may be Tangled Rope (constraint provides coordination function while extracting costs) or even Scaffold (temporary structure with sunset).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tower_as_trap_vs_space, conceptual, 'Whether tower functions as constraint on or enabler of Stephen''s development').

omega_variable(
    imperial_extraction_mechanism,
    'How much of the tower''s extractive force comes from Mulligan''s personal dominance versus the broader British imperial structure?',
    'Counterfactual analysis: would Stephen face equivalent extraction if Mulligan were replaced by a different Irish intellectual? Does British structural presence amplify Mulligan''s extraction capacity?',
    'If primarily Mulligan''s agency: Snare is interpersonal. If primarily imperial structure: Snare is structural (British order → Irish constraint), and Mulligan is an instrument of larger extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_extraction_mechanism, empirical, 'Decomposition of personal vs structural extraction mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp01, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tower_tr_t0, ulysses_chp01, theater_ratio, 0, 0.48).
narrative_ontology:measurement(tower_tr_t3, ulysses_chp01, theater_ratio, 3, 0.55).
narrative_ontology:measurement(tower_tr_t6, ulysses_chp01, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(tower_be_t0, ulysses_chp01, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tower_be_t3, ulysses_chp01, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(tower_be_t6, ulysses_chp01, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp01, global_infrastructure).
narrative_ontology:affects_constraint(ulysses_chp01, irish_cultural_autonomy).
narrative_ontology:affects_constraint(ulysses_chp01, british_imperial_surveillance).

% DUAL FORMULATION NOTE:
% The Martello tower exemplifies constraint family decomposition. The original structural constraint (tower as Rope: coastal defense coordination) is distinct from the contemporary constraint (tower as Snare: extraction mechanism under Mulligan's dominance). These could be modeled as separate stories with different ε values (original Rope: ε≈0.15; contemporary Snare: ε≈0.58) and different time intervals (pre-1804 vs 1904). The shared physical structure creates a nominal identity ('the Martello tower') that masks two structurally distinct constraints. This story treats the contemporary Snare; the original Rope would be a separate constraint in a constraint family network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp01, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
