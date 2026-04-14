% ============================================================================
% CONSTRAINT STORY: ulysses_chp01
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_tower_1904, []).

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
 *   human_readable: The Martello Tower Usurpation (Ulysses, June 16, 1904)
 *   domain: social/political/religious
 *
 * SUMMARY:
 *   On the morning of June 16, 1904, Stephen Dedalus awakens in the Martello
 *   tower at Sandycove, Dublin, a structure originally built by the British
 *   in 1804 to defend against French invasion. The tower has become a
 *   microcosm of Stephen's entrapment: colonial occupation, Catholic
 *   religious subjugation, intellectual mockery by his companion Buck
 *   Mulligan, and economic poverty combine to create a multi-layered snare.
 *   The tower was originally a Rope — a coordination mechanism providing
 *   genuine security benefit to the British Empire. By 1904, it has become
 *   three structurally distinct constraints simultaneously: (1) a Snare for
 *   Stephen personally (Mulligan's mockery and economic extraction), (2) a
 *   Piton for Irish civilization (vestigial colonial infrastructure with high
 *   theater, no functional threat), and (3) a Scaffold for the Irish
 *   independence movement (temporary constraint with a known sunset). The
 *   constraint illustrates how a single physical location can instantiate
 *   multiple constraint types when viewed from different structural
 *   positions.
 *
 * KEY AGENTS:
 *   - Stephen Dedalus: Powerless artist (powerless/trapped/local) — primary victim of multiple extraction mechanisms: religious guilt, economic dependency, Mulligan's psychological domination
 *   - Buck Mulligan: Cynical medical student (powerful/arbitrage/national) — primary beneficiary; extracts intellectual and emotional labor from Stephen while mocking his religious crisis; possesses exit options Stephen lacks
 *   - British Colonial Apparatus: Institutional actor (institutional/arbitrage/global) — original beneficiary of tower construction; maintains colonial order; does not experience extraction (sees tower as neutral infrastructure)
 *   - Irish Catholic Church: Institutional victim (institutional/constrained/national) — Stephen's rejection of Catholicism demonstrates the faith has become extractive rather than protective; constrains through guilt and social obligation
 *   - Irish Independence Movement: Organized resistance (organized/mobile/national) — observes the constraint as temporary; structures endurance around knowledge of coming independence
 *   - Mulligan's Family Wealth: Structural beneficiary (implicit) — enables Mulligan's arbitrage options and freeloader status in the tower
 *   - Civilizational Observer: Historical perspective (analytical/analytical/global) — sees the tower as degraded infrastructure: once functional defense mechanism, now symbolic artifact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp01, 0.68).
domain_priors:suppression_score(ulysses_chp01, 0.72).
domain_priors:theater_ratio(ulysses_chp01, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp01, extractiveness, 0.68).
narrative_ontology:constraint_metric(ulysses_chp01, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ulysses_chp01, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp01, snare).
narrative_ontology:human_readable(ulysses_chp01, "The Martello Tower Usurpation (Ulysses, June 16, 1904)").
narrative_ontology:topic_domain(ulysses_chp01, "social/political/religious").

domain_priors:requires_active_enforcement(ulysses_chp01).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp01, buck_mulligan).
narrative_ontology:constraint_beneficiary(ulysses_chp01, british_colonial_apparatus).
narrative_ontology:constraint_victim(ulysses_chp01, stephen_dedalus).
narrative_ontology:constraint_victim(ulysses_chp01, irish_catholic_identity).
narrative_ontology:constraint_victim(ulysses_chp01, artistic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STEPHEN DEDALUS (SNARE) — Young artist trapped in the tower, bearing the full extraction of Mulligan's mockery, colonial subjugation, and Catholic guilt. No meaningful exit: lacks resources to leave Dublin, career options constrained by religious/class position, psychological dependence on Mulligan's companionship despite its toxicity. Maximum experienced extraction.
constraint_indexing:constraint_classification(ulysses_chp01, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: BRITISH COLONIAL APPARATUS (ROPE) — The tower itself was built as Rope: a coordination mechanism to defend Ireland against French invasion (1804), benefiting British security interests. The colonial framework sees the tower as neutral infrastructure, not as extraction. The tower *coordinates* imperial defense while structurally extracting from Irish autonomy, but this extraction is not experienced as such from the imperial perspective — it appears as legitimate order-maintenance.
constraint_indexing:constraint_classification(ulysses_chp01, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: BUCK MULLIGAN (TANGLED ROPE) — Medical student with arbitrage options (career mobility, family wealth, ability to leave). Experiences the tower as a coordination mechanism (shared lodging, intellectual companionship) BUT extracts from Stephen through mockery, psychological domination, and economic freeloading. Mulligan benefits from the Irish Catholic system (education, social position) while mocking it — asymmetric extraction disguised as camaraderie. His effective extraction χ is reduced by exit options, but the structural extraction mechanism is present.
constraint_indexing:constraint_classification(ulysses_chp01, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IRISH CATHOLIC IDENTITY (SNARE) — The tower becomes a trap for Irish Catholicism itself. Stephen's rejection of the faith (his 'rebellion') demonstrates that the religious constraint, once protective, has become extractive. Constrained exit: Irish identity is structurally embedded in Catholicism; rejecting the faith means social exile within one's own community. The system extracts conformity through shame, family obligation, and community pressure. No agent can abandon Irish Catholicism without bearing severe psychological and social costs.
constraint_indexing:constraint_classification(ulysses_chp01, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVILIZATIONAL OBSERVER (PITON) — From a historical distance, the Martello tower is a degraded constraint. Originally functional (1804: real French invasion threat), by 1904 the tower is vestigial — no French invasion threat exists; the tower persists through institutional inertia and historical symbolism. Its theater_ratio is high: it functions primarily as a setting for intellectual and spiritual drama rather than as a defensive structure. The tower's constraint force derives from its symbolic weight and narrative power, not from functional necessity. This is the core Piton signature: a former Rope (defense mechanism) that has atrophied into theatrical persistence.
constraint_indexing:constraint_classification(ulysses_chp01, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: IRISH INDEPENDENCE MOVEMENT (SCAFFOLD) — From the perspective of organized nationalist forces (emerging in 1904, not yet triumphant), the colonial occupation has a sunset clause. The movement sees the tower as temporary — British control will be cast off within a generation (Irish independence achieved 1922). The constraint is endured because exit is approaching; suppression is high but time-bounded. The movement structures its resistance around the knowledge that the constraint will end.
constraint_indexing:constraint_classification(ulysses_chp01, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

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
 *   Extractiveness (0.68): High. The tower concentrates multiple extraction mechanisms: (1) Mulligan's psychological and economic parasitism on Stephen (primary), (2) colonial occupation of Irish space (secondary, but structural), (3) religious obligation extracting Stephen's intellectual and emotional energy (internalized). The base extractiveness has risen from 0.45 to 0.68 over the interval because Stephen's initial hope (arriving at the tower with Mulligan as a friend/intellectual equal) has degraded into recognition that he is being systematically mocked and economically exploited. Suppression (0.72): Very high. Stephen cannot exit the tower or Dublin effectively because: (i) economic poverty (no resources to leave Ireland), (ii) religious guilt and family obligation (mother's ghost haunts him), (iii) psychological dependence on Mulligan's companionship despite its toxicity, (iv) limited career options due to class position and religious background, (v) social exile threatened if he abandons both faith and family. The suppression value reflects the convergence of multiple binding mechanisms — no single escape route exists. Theater ratio (0.58): Moderate-high. The tower's constraint force increasingly derives from symbolic and narrative weight rather than material enforcement. The tower physically contains Stephen only insofar as he remains within it — he could walk away. But his internalization of the constraints (guilt, shame, intellectual doubt) makes the tower's symbolism stronger than its walls. Over the interval, Stephen's perception shifts: the tower becomes less a physical prison and more a symbolic trap, increasing theater_ratio from 0.42 to 0.58.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap emerges between Stephen's experience and Mulligan's experience of the same constraint. Stephen sees the tower as a Snare: he is powerless, trapped, experiencing maximum extraction. Mulligan sees the tower as a Rope: it coordinates intellectual exchange, provides lodging, enables shared mockery of their social betters (the Catholic establishment, the British colonizers). Both are correct from their structural positions. Stephen's powerlessness derives from his lack of arbitrage options; Mulligan's powerful position derives from having multiple exits (medical career, family resources, social mobility through wit and education). The gap widens because Mulligan's arbitrage enables him to extract from Stephen without reciprocal vulnerability. A secondary perspectival gap exists between the local (Stephen/Mulligan) and civilizational (historian) perspectives: what appears to Stephen as a binding constraint (colonial occupation, religious tradition) appears to a historical observer as a degraded artifact (Piton). The tower no longer functionally constrains in 1904 the way it did in 1804 — its constraint force has shifted from material (defense against invasion) to theatrical (symbol of occupation and spiritual crisis).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for each agent: Stephen Dedalus (d ≈ 0.92): Victim status + trapped exit options → high d → high f(d) ≈ 1.35 → maximum experienced extraction χ. He is structurally positioned to bear the full cost of the constraint without ability to escape or retaliate. Mulligan (d ≈ 0.28): Beneficiary status + arbitrage exit options → low d → low f(d) ≈ 0.15 → minimal experienced extraction. He benefits from the arrangement and can leave at will. British Colonial Order (d ≈ 0.05): Beneficiary status + institutional arbitrage → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ (subsidy). The empire experiences the constraint as beneficial coordination, not extraction. Irish Catholic Church (d ≈ 0.85): Victim of modernization (Stephen's rejection) + constrained exit (institutional inertia) → high d → high f(d) ≈ 1.20 → high extraction experience. The institution is losing its extractive power over a generation of intellectuals. Irish Independence Movement (d ≈ 0.55): Mixed position — organized agents with mobile exit options + victim status of occupation → moderate d → moderate f(d) ≈ 0.75 → moderate extraction, but time-bounded. The automatic derivation chain produces appropriate directionality values; no overrides required.
 *
 * MANDATROPHY ANALYSIS:
 *   The Martello tower resolves mandatrophy by demonstrating that Snare classification is correct for Stephen's structural position while Rope, Tangled Rope, Scaffold, and Piton are all correct for other positions. The mandatrophy would arise if we tried to classify 'the tower constraint' as a single type applicable to all observers. Instead, the constraint instantiates as a presheaf: (1) Snare for powerless trapped agents (Stephen), (2) Rope for institutional beneficiaries (British Empire), (3) Tangled Rope for powerful extractors (Mulligan), (4) Piton for civilizational observers (historians viewing vestigial infrastructure), (5) Scaffold for organized resistance (independence movement with sunset knowledge). No single type is 'the truth' — the perspectival gap IS the data. The snare classification for Stephen is robustly confirmed by: (a) extractiveness 0.68 ≥ 0.46, (b) suppression 0.72 ≥ 0.60, (c) χ ≈ 0.92 × f(0.92) × σ(local) ≈ 0.92 × 1.35 × 0.8 ≈ 0.99 > 0.66, (d) at least one victim (Stephen himself) present. The constraint meets all snare gates from Stephen's perspective while meeting different gates from other perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mulligan_extraction_mechanism,
    'Is Mulligan''s mockery and domination a structural extraction mechanism or a temporary interpersonal dynamic?',
    'Textual analysis of Mulligan''s behavior toward Stephen across the novel; comparison with other relationships Stephen maintains; examination of financial/emotional dependencies',
    'If structural: the tower-as-snare classification is robust; Mulligan is a beneficiary extracting from a powerless agent. If temporary: the relationship is Rope-like (mutual intellectual exchange); the extraction is incidental rather than systematic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mulligan_extraction_mechanism, conceptual, 'Whether Mulligan''s psychological domination is structural or incidental').

omega_variable(
    religious_constraint_bind_strength,
    'What portion of Stephen''s ''trapedness'' derives from religious obligation vs. economic/class constraint vs. psychological dependence?',
    'Analysis of Stephen''s internal monologue regarding each constraint; counterfactual: what changes if religion is removed vs. if poverty is removed vs. if Mulligan leaves',
    'If religion dominates: the snare is primarily spiritual/cultural. If economics dominates: the snare is primarily class-based. If psychological dependence dominates: Mulligan extraction is the primary mechanism. Each resolution shifts the target agent identification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_constraint_bind_strength, conceptual, 'Relative contribution of religion, economics, and psychology to Stephen''s constraint').

omega_variable(
    tower_symbolic_vs_functional,
    'Does the tower function as a real constraint on movement and autonomy, or is it primarily a symbolic/narrative container for constraints that exist elsewhere?',
    'Examination of Stephen''s physical movement within and outside the tower; presence/absence of explicit barriers; whether the constraint would dissolve if he simply left',
    'If functional: the tower is a snare with material enforcement. If symbolic: the tower is a piton — it has high theater_ratio and constrains through narrative weight rather than material force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tower_symbolic_vs_functional, empirical, 'Whether the tower materially constrains or primarily symbolizes constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp01, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp01, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ulys_tr_t1, ulysses_chp01, theater_ratio, 1, 0.5).
narrative_ontology:measurement(ulys_tr_t2, ulysses_chp01, theater_ratio, 2, 0.58).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp01, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ulys_be_t1, ulysses_chp01, base_extractiveness, 1, 0.58).
narrative_ontology:measurement(ulys_be_t2, ulysses_chp01, base_extractiveness, 2, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp01, enforcement_mechanism).
narrative_ontology:affects_constraint(ulysses_chp01, irish_catholic_constraint).
narrative_ontology:affects_constraint(ulysses_chp01, british_colonial_occupation).

% DUAL FORMULATION NOTE:
% The Martello tower constraint is downstream of two structural constraints: (1) Irish Catholic identity formation and guilt mechanisms, (2) British colonial occupation of Ireland. The tower inherits extractive force from both upstream constraints but instantiates as a distinct snare for Stephen personally through Mulligan's extraction mechanism. The tower's ε value (0.68) reflects the convergence of these upstream constraints into a localized snare; neither upstream constraint alone would produce snare classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp01, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
