% ============================================================================
% CONSTRAINT STORY: territory_selection_logic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territory_selection_logic, []).

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
 *   constraint_id: territory_selection_logic
 *   human_readable: Territory Selection Logic in Self-Development Frameworks
 *   domain: philosophy_of_mind/systems_theory/phenomenology_of_constraint
 *
 * SUMMARY:
 *   The territory selection logic represents a structural shift in how
 *   self-development frameworks address human constraints. The upstream
 *   constraint 'limit_as_information' (mountain) establishes that some
 *   constraints are constitutive — they carry information about what kind of
 *   system you are, not just what you haven't learned yet. The upstream
 *   constraint 'distinguishability_diagnostic' (rope) provides methods for
 *   identifying constitutive vs contingent limits. Territory selection logic
 *   is the decision framework that follows from these distinctions: once you
 *   can distinguish limit types, the question shifts from 'how do I break
 *   this limit?' to 'which limits define a territory I can inhabit with
 *   integrity?' However, the dominant self-development paradigm (growth
 *   mindset, limitless potential, transcendence narratives) systematically
 *   collapses this distinction, routing all constraints toward effort-based
 *   interventions regardless of type. This creates a tangled rope: genuine
 *   coordination (helping people overcome contingent limits like learned
 *   helplessness) coexists with asymmetric extraction (practitioners invest
 *   resources attempting to overcome constitutive constraints that are not
 *   amenable to willpower or technique). The constraint's theater_ratio
 *   (0.68) reflects that much of the self-development industry's diagnostic
 *   apparatus (assessments, frameworks, interventions) is performative — it
 *   measures mindset endorsement and effort investment rather than actual
 *   constraint distinguishability. The extractiveness has increased over the
 *   interval (0.38 → 0.58) as the industry has expanded its scope to include
 *   neurodevelopmental, neurological, and structural constraints that were
 *   previously outside the 'personal growth' domain.
 *
 * KEY AGENTS:
 *   - Self-Development Industry: Primary beneficiary (institutional/arbitrage) — maximizes market size by treating all constraints as addressable through products and services; can pivot to adjacent markets if saturation occurs
 *   - Practitioners Misdiagnosing Constitutive Limits: Primary victim (powerless/identity_locked) — identity-fused with growth-mindset framing; cannot exit without abandoning self-concept; invests resources in futile attempts to overcome constitutive constraints
 *   - Reflective Practitioners: Secondary victim (moderate/constrained) — can sometimes identify constitutive vs contingent limits but face high costs for acting on this knowledge; also benefit from genuine coordination function
 *   - Neurodiversity Movement: Organized agents (organized/mobile) — building alternative framing where some constraints are features not deficits; sees extraction mechanism as having a sunset as cultural norms shift
 *   - Academic Psychology Establishment: Institutional actor (institutional/constrained) — maintains growth mindset research through inertia despite boundary conditions and replication failures; high theater ratio
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function and asymmetric extraction; tangled_rope classification reflects that both are structurally real
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territory_selection_logic, 0.58).
domain_priors:suppression_score(territory_selection_logic, 0.62).
domain_priors:theater_ratio(territory_selection_logic, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territory_selection_logic, extractiveness, 0.58).
narrative_ontology:constraint_metric(territory_selection_logic, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territory_selection_logic, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territory_selection_logic, tangled_rope).
narrative_ontology:human_readable(territory_selection_logic, "Territory Selection Logic in Self-Development Frameworks").
narrative_ontology:topic_domain(territory_selection_logic, "philosophy_of_mind/systems_theory/phenomenology_of_constraint").

domain_priors:requires_active_enforcement(territory_selection_logic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territory_selection_logic, self_development_industry).
narrative_ontology:constraint_beneficiary(territory_selection_logic, growth_mindset_advocates).
narrative_ontology:constraint_victim(territory_selection_logic, practitioners_misdiagnosing_constitutive_limits).
narrative_ontology:constraint_victim(territory_selection_logic, individuals_with_neurodevelopmental_constraints).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTITIONER WITH CONSTITUTIVE LIMITS (SNARE) — Identity-locked in growth-mindset framing. Cannot exit the 'all limits are breakable' paradigm without abandoning the self-concept built through years of self-development practice. Experiences maximum extraction: invests resources in attempting to overcome constitutive constraints (neurological, developmental, structural) that are not amenable to willpower or technique. The identity lock is the binding mechanism — exit would require becoming a different kind of person, one who accepts rather than transcends.
constraint_indexing:constraint_classification(territory_selection_logic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: REFLECTIVE PRACTITIONER (TANGLED ROPE) — Has access to distinguishability diagnostics and can sometimes identify constitutive vs contingent limits, but faces high costs for acting on this knowledge. Career investment in self-development methodologies, social identity within growth communities, and sunk costs in training create real barriers to exit. Also benefits from the coordination function: the framework does help with genuinely contingent limits (learned helplessness, skill acquisition, habit formation). Mixed experience — genuine value alongside extraction.
constraint_indexing:constraint_classification(territory_selection_logic, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SELF-DEVELOPMENT INDUSTRY (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: providing frameworks, tools, and communities for people seeking growth. The 'all limits are breakable' framing maximizes market size by treating every constraint as addressable through products and services. Net beneficiary with arbitrage exit — can pivot to adjacent markets (wellness, productivity, spirituality) if growth-mindset saturation occurs.
constraint_indexing:constraint_classification(territory_selection_logic, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NEURODIVERSITY MOVEMENT (SCAFFOLD) — Organized coalition building alternative framing: some neurological differences are constitutive features, not deficits to overcome. Sees the 'break all limits' paradigm as a temporary coordination failure with a sunset: as neurodiversity-affirming frameworks mature (accommodations over normalization, acceptance over cure), the extraction mechanism loses force. Estimated sunset: 15-25 years for cultural norms to shift from pathology model to diversity model in self-development contexts.
constraint_indexing:constraint_classification(territory_selection_logic, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC PSYCHOLOGY (PITON) — Maintains growth mindset research and fixed-vs-growth dichotomy through institutional inertia despite mounting evidence of boundary conditions, replication failures, and context-dependency. The research program persists because it generates publications and aligns with cultural narratives, not because it provides reliable guidance for distinguishing constitutive from contingent limits. Theater ratio high: the empirical apparatus (studies, meta-analyses, interventions) is largely performative — measures mindset endorsement rather than actual constraint distinguishability.
constraint_indexing:constraint_classification(territory_selection_logic, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the territory selection logic serves a genuine coordination function (helping people identify which constraints are worth engaging vs accepting) but embeds asymmetric extraction through systematic misdiagnosis. The 'growth mindset' framing collapses the constitutive-contingent distinction, routing practitioners toward effort-based interventions regardless of constraint type. This is not pure extraction (genuine skill acquisition does occur) but also not pure coordination (the framework systematically fails at its stated function of distinguishing limits). The analytical classification is tangled_rope because both the coordination function and the extraction mechanism are structurally real.
constraint_indexing:constraint_classification(territory_selection_logic, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territory_selection_logic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(territory_selection_logic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(territory_selection_logic, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(territory_selection_logic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(territory_selection_logic, TR),
    TR >= 0.70.

:- end_tests(territory_selection_logic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The self-development industry captures revenue and cultural influence by routing practitioners toward effort-based interventions regardless of constraint type. Practitioners with constitutive limits (neurodevelopmental, neurological, structural) invest significant resources (time, money, emotional labor) in attempting to overcome constraints that are not amenable to the interventions offered. The extraction is not total — genuine skill acquisition and contingent limit transcendence do occur — but the systematic misdiagnosis creates asymmetric extraction concentrated on those least able to distinguish constitutive from contingent constraints. Suppression (0.62): Moderate-high. Barriers to exit include identity fusion with growth-mindset framing, sunk costs in training and practice, social identity within self-development communities, and cultural narratives that frame acceptance of limits as defeatism or fixed mindset. The neurodiversity movement is reducing suppression by providing alternative framing, but exit remains costly for most practitioners. Theater ratio (0.68): High. Much of the self-development industry's diagnostic apparatus is performative: assessments measure mindset endorsement rather than constraint distinguishability, interventions are applied uniformly regardless of constraint type, and success is defined by effort investment rather than outcome achievement. The theater has increased as the industry has expanded scope to include constraints that require specialized expertise to diagnose.
 *
 * PERSPECTIVAL GAP:
 *   The self-development industry sees pure coordination (Rope) — they are providing tools for growth and self-actualization. The neurodiversity movement sees a temporary problem with a sunset (Scaffold) — alternative framings are maturing that will displace the growth-mindset paradigm. The academic psychology establishment sees its own degraded research program (Piton) — growth mindset studies persist through institutional inertia despite boundary conditions. Reflective practitioners see mixed coordination and extraction (Tangled Rope) — the framework helps with contingent limits but systematically fails with constitutive limits. Practitioners misdiagnosing constitutive limits see pure extraction (Snare) — they are identity-locked in a paradigm that routes them toward futile interventions. The analytical observer sees tangled rope at the civilizational level — both the coordination function (helping with contingent limits) and the extraction mechanism (misdiagnosing constitutive limits) are structurally real. The perspectival gap is maximal between the industry (rope) and the identity-locked practitioner (snare) — a 4-type difference reflecting the complete reversal of experienced extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The self-development industry is the primary beneficiary — it experiences low directionality (d ≈ 0.10) because the constraint subsidizes its market expansion. Growth-mindset advocates also benefit (d ≈ 0.20) through cultural influence and professional legitimacy. Practitioners misdiagnosing constitutive limits are primary victims with identity_locked exit — they experience high directionality (d ≈ 0.89) because they are structurally mobile (could exit the paradigm) but identity-fused (cannot exit without abandoning self-concept). Individuals with neurodevelopmental constraints are victims with trapped exit (d ≈ 0.95) — they face both structural barriers (lack of alternative frameworks) and identity barriers (internalized growth-mindset framing). Reflective practitioners are secondary victims with constrained exit (d ≈ 0.60) — they can identify the extraction but face high costs for exit. The neurodiversity movement is organized with mobile exit (d ≈ 0.45) — they have agency and are building alternative pathways. The academic psychology establishment is institutional with constrained exit (d ≈ 0.35) — they maintain the research program through inertia but face reputational costs for abandoning it. The analytical observer uses analytical exit (d ≈ 0.72) and sees both coordination and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the coordination function (helping people overcome contingent limits) and the extraction mechanism (misdiagnosing constitutive limits) are both structurally real and operate simultaneously. The self-development industry genuinely helps some people with some constraints (learned helplessness, skill deficits, habit formation) while systematically extracting from others (those with constitutive constraints misdiagnosed as contingent). The tangled_rope classification at the analytical level captures this duality: it is neither pure coordination (rope) nor pure extraction (snare) but a hybrid where both mechanisms coexist. The identity_locked exit option for the primary victim reveals the binding mechanism: the practitioner cannot exit the paradigm without abandoning the self-concept built through years of practice. This is not a material barrier (trapped) or a high-cost barrier (constrained) but a cognitive barrier where exit would require becoming a different kind of person. The perspectival gap between the industry (rope) and the identity-locked practitioner (snare) shows that the same structural phenomenon produces opposite experiences depending on position: the beneficiary experiences coordination, the victim experiences extraction, and both are correct from their respective positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_contingent_boundary,
    'Where is the empirical boundary between constitutive constraints (structural, neurological, developmental) and contingent constraints (learned, habitual, contextual)?',
    'Longitudinal intervention studies tracking which constraints respond to effort-based interventions vs which remain stable across contexts and time; neuroimaging and genetic studies identifying structural vs functional differences',
    'If boundary is sharp and identifiable: territory selection becomes a rope (pure coordination with low extraction). If boundary is fuzzy or context-dependent: current tangled_rope classification is accurate. If boundary is unknowable from first-person perspective: extraction is higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_contingent_boundary, empirical, 'Empirical boundary between constitutive and contingent constraints').

omega_variable(
    identity_lock_reversibility,
    'Can practitioners identity-locked in growth-mindset framing exit the paradigm without therapeutic intervention or community support?',
    'Qualitative studies of practitioners who shifted from ''break all limits'' to ''select territory'' framing; identification of catalysts (burnout, diagnosis, community exposure) and barriers (sunk costs, social identity, self-concept disruption)',
    'If reversible through self-reflection: identity_locked classification overstates extraction. If requires external intervention: identity_locked classification is accurate. If irreversible for most practitioners: constraint is more extractive than measured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity-lock in growth-mindset framing is reversible').

omega_variable(
    neurodiversity_sunset_timeline,
    'Will neurodiversity-affirming frameworks actually displace growth-mindset paradigms in self-development contexts, or will they remain parallel niche markets?',
    'Market analysis of self-development industry messaging over 10-year horizon; cultural discourse analysis tracking prevalence of acceptance-framing vs transcendence-framing in mainstream media; institutional adoption rates of neurodiversity accommodations',
    'If displacement occurs: scaffold perspective confirmed, extraction has sunset. If parallel markets persist: scaffold perspective is aspirational, extraction continues indefinitely. If growth-mindset paradigm absorbs neurodiversity language without structural change: extraction increases (theater ratio rises).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neurodiversity_sunset_timeline, empirical, 'Whether neurodiversity frameworks will displace growth-mindset paradigms').

omega_variable(
    distinguishability_diagnostic_accessibility,
    'Are distinguishability diagnostics (methods for identifying constitutive vs contingent limits) accessible to practitioners without specialized training, or do they require expert guidance?',
    'Usability studies of diagnostic frameworks; accuracy rates of self-diagnosis vs expert diagnosis; identification of cognitive biases (optimism bias, sunk cost fallacy) that interfere with self-assessment',
    'If accessible: coordination function is real and extraction is lower. If expert-dependent: coordination function is limited to those with access to expertise, and extraction is higher for those without access. If diagnostics are unreliable even with expertise: coordination function is illusory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distinguishability_diagnostic_accessibility, empirical, 'Whether distinguishability diagnostics are accessible without expert guidance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territory_selection_logic, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_sel_tr_t0, territory_selection_logic, theater_ratio, 0, 0.42).
narrative_ontology:measurement(terr_sel_tr_t4, territory_selection_logic, theater_ratio, 4, 0.55).
narrative_ontology:measurement(terr_sel_tr_t8, territory_selection_logic, theater_ratio, 8, 0.62).
narrative_ontology:measurement(terr_sel_tr_t12, territory_selection_logic, theater_ratio, 12, 0.68).

% Extraction over time
narrative_ontology:measurement(terr_sel_be_t0, territory_selection_logic, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(terr_sel_be_t4, territory_selection_logic, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(terr_sel_be_t8, territory_selection_logic, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(terr_sel_be_t12, territory_selection_logic, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territory_selection_logic, identity_coordination).

% DUAL FORMULATION NOTE:
% Territory selection logic is downstream of two structurally distinct upstream constraints: 'limit_as_information' (mountain — constitutive constraints carry information about system type) and 'distinguishability_diagnostic' (rope — methods for identifying constitutive vs contingent limits). The territory selection logic has its own extractiveness (0.58) reflecting the systematic misdiagnosis in self-development contexts, distinct from the upstream constraints' empirical status. The self-development industry's growth-mindset paradigm collapses the constitutive-contingent distinction that the upstream constraints establish, creating the extraction mechanism measured here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territory_selection_logic, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
