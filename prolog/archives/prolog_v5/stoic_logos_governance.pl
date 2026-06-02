% ============================================================================
% CONSTRAINT STORY: stoic_logos_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stoic_logos_governance, []).

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
 *   constraint_id: stoic_logos_governance
 *   human_readable: The Stoic Logos as an Imperial Governance Framework
 *   domain: philosophical/political
 *
 * SUMMARY:
 *   The Stoic Logos governance framework, exemplified by Emperor Marcus
 *   Aurelius (r. 161-180 CE), represents a constraint in which the ruler
 *   voluntarily submits his personal will to an alleged transcendent
 *   principle of divine reason and natural law. The Logos — conceived as the
 *   rational principle pervading the cosmos — is invoked as both the source
 *   of the emperor's authority and the limit of his power. This creates a
 *   structural tension: the Logos simultaneously justifies imperial hierarchy
 *   (the emperor alone understands cosmic reason), enables imperial
 *   extraction (natural law sanctions subordination), and constrains
 *   arbitrary rule (the emperor must obey the same principle he enforces).
 *   The constraint manifests across six distinct perspectives, exhibiting
 *   characteristics of Tangled Rope (mixed coordination and extraction),
 *   Snare (for subject populations), Rope (for the imperial center), Scaffold
 *   (for the philosophical movement), Piton (for later bureaucratic ritual),
 *   and a false Mountain (from the analytical observer who risks naturalizing
 *   contingent philosophy). The theater ratio increases over the interval
 *   (0.38 → 0.75) as the functional governance burden of explicitly
 *   consulting Logos principles declines and bureaucratic routine displaces
 *   philosophical reasoning. Base extractiveness remains moderate (0.28 →
 *   0.35) because the Logos framework genuinely constrains arbitrary rule
 *   while simultaneously providing philosophical justification for hierarchy.
 *
 * KEY AGENTS:
 *   - Marcus Aurelius and the Imperial Court: Primary beneficiary (institutional/arbitrage) — the Logos framework legitimizes power while appearing to constrain it; provides coordination mechanism that makes imperial will predictable to bureaucracy
 *   - Enslaved and Subject Populations: Primary victim (powerless/trapped) — no exit from imperial order; Logos naturalizes their subordination as cosmic necessity; experience maximum extraction
 *   - Provincial Administrators and Local Elites: Secondary beneficiary/victim (moderate/constrained) — constrained by imperial will but benefit from access to patronage and philosophical legitimacy for their own hierarchies
 *   - Stoic Philosophical Schools: Organized intermediary (organized/constrained) — gain influence and advisory roles; see framework as sunset when institutionalized
 *   - Imperial Bureaucracy: Institutional actor (institutional/arbitrage) — benefits from Logos principle of rational order; gradually shifts from philosophical reasoning to administrative precedent
 *   - Analytical Observer: External perspective (analytical/analytical) — risks naturalizing contingent philosophical system as objective law of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stoic_logos_governance, 0.35).
domain_priors:suppression_score(stoic_logos_governance, 0.48).
domain_priors:theater_ratio(stoic_logos_governance, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stoic_logos_governance, extractiveness, 0.35).
narrative_ontology:constraint_metric(stoic_logos_governance, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(stoic_logos_governance, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stoic_logos_governance, tangled_rope).
narrative_ontology:human_readable(stoic_logos_governance, "The Stoic Logos as an Imperial Governance Framework").
narrative_ontology:topic_domain(stoic_logos_governance, "philosophical/political").

domain_priors:requires_active_enforcement(stoic_logos_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stoic_logos_governance, imperial_bureaucracy).
narrative_ontology:constraint_beneficiary(stoic_logos_governance, provincial_ruling_class).
narrative_ontology:constraint_beneficiary(stoic_logos_governance, philosophical_schools).
narrative_ontology:constraint_victim(stoic_logos_governance, enslaved_populations).
narrative_ontology:constraint_victim(stoic_logos_governance, subject_provinces).
narrative_ontology:constraint_victim(stoic_logos_governance, non_stoic_philosophical_traditions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED AND SUBJECT POPULATIONS (SNARE) — Trapped within imperial order justified by Stoic naturalism. The Logos framework naturalizes their subordination as consonant with cosmic order. No exit options; bears full extraction cost through labor, taxation, and legal subordination. Maximum experienced extractiveness despite philosophical language of universal reason.
constraint_indexing:constraint_classification(stoic_logos_governance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PROVINCIAL ADMINISTRATORS AND LOCAL ELITES (TANGLED ROPE) — Constrained by imperial authority but also benefit from Stoic governance framework: provides legitimacy for their local rule, access to imperial patronage networks, and a philosophical justification for hierarchy that secures their position. Mixed coordination (shared governance order) and extraction (subordination to imperial will).
constraint_indexing:constraint_classification(stoic_logos_governance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE IMPERIAL COURT AND MARCUS AURELIUS (ROPE) — Experiences the Logos framework as coordination mechanism. The emperor's voluntary submission to Logos (rather than arbitrary will) serves coordination function: subordinates can predict his decisions, bureaucracy operates on rational principles rather than caprice, military campaigns follow strategic logic. Net beneficiary through enhanced governance effectiveness and legitimacy.
constraint_indexing:constraint_classification(stoic_logos_governance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE STOIC PHILOSOPHICAL MOVEMENT (SCAFFOLD) — Organized network of schools and adherents sees the Logos framework as a temporary intellectual scaffold for imperial governance. The movement has agency through intellectual leadership and advisory roles. The constraint has sunset logic: as Stoicism becomes institutionalized (incorporated into formal bureaucracy, academy curriculum), the need for philosophical justification weakens. The movement builds alternative ethical frameworks that eventually displace Stoicism (Christianity, later Neoplatonism).
constraint_indexing:constraint_classification(stoic_logos_governance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: THE IMPERIAL ADMINISTRATIVE APPARATUS (PITON) — By the late empire, the formal invocation of Logos becomes increasingly theatrical. The bureaucratic apparatus continues to invoke Stoic principles of rational order and cosmic harmony, but actual governance relies on military force, patronage networks, and administrative precedent rather than philosophical principle. Theater ratio high because the ritual of consulting natural law persists even as functional governance shifts to administrative procedure.
constraint_indexing:constraint_classification(stoic_logos_governance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the Stoic claim that the Logos represents objective natural law with inherent authority over human will appears to be a universal principle. However, the structural data reveals this as a false summit: the 'natural law' status is contingent on philosophical interpretation and enforcement, not inherent. The engine's false summit detector identifies this perspective as naturalizing a contingent institutional arrangement.
constraint_indexing:constraint_classification(stoic_logos_governance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stoic_logos_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stoic_logos_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stoic_logos_governance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(stoic_logos_governance, TR),
    TR >= 0.70.

:- end_tests(stoic_logos_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The Logos framework reduces extractiveness compared to unconstrained imperial rule because the emperor genuinely accepts (or appears to accept) constraint from a principle beyond his will. This represents real coordination gain — subjects can predict decisions based on Logos principles rather than arbitrary preference. However, the 'natural law' that justifies constraint is simultaneously used to justify extraction: slavery, taxation, and military conscription are all rationalized as consonant with cosmic order. The net effect is moderate extractiveness — higher than pure coordination (Rope) but lower than unconstrained extraction (Snare). Suppression (0.48): Moderate-high. The framework suppresses alternative philosophical traditions (Epicureanism, skepticism) that might challenge the extraction logic. It also suppresses direct challenge to imperial authority by naturalizing hierarchy. However, the Logos principle is supposed to apply universally, creating some intellectual space for argument within its framework. Suppression is structural but not absolute — alternatives exist but are delegitimized. Theater ratio (0.62 → 0.75): Rising. Early in Marcus Aurelius's reign, consultation of Logos principles genuinely shaped governance decisions — the emperor's published reflections (Meditations) show internal struggle with ethical principle. By the later empire, the invocation of Logos becomes increasingly ritualistic while actual governance relies on bureaucratic precedent and military necessity. The rise in theater indicates Goodhart drift: as the Logos principle becomes institutionalized, explicit reasoning about it is replaced by routinized practice.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between different agents observing the Stoic Logos framework is profound. The enslaved population sees pure extraction (Snare) — the cosmic order justifies their subordination and extraction of their labor. The provincial elites see mixed coordination and extraction (Tangled Rope) — the Logos framework enables their local rule while subordinating them to imperial will. The imperial court sees coordination (Rope) — the Logos principle makes the empire governable through rational principles rather than caprice. The Stoic philosophers see a temporary scaffold (Scaffold) — an intellectual framework that organizes governance until institutionalized and then displaced by alternatives. The bureaucracy sees an increasingly degraded ritual (Piton) — the philosophical consultation becomes theater as administrative procedure takes over. The analytical observer risks seeing an immutable natural law (Mountain) — but the engine's false summit detection reveals this as a naturalization of contingent philosophy. This perspectival spread from Snare to Mountain demonstrates how the same structural mechanism (appeal to transcendent principle) can appear completely different depending on the observer's exit options and beneficiary status.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation derives from structural position relative to extraction flow. The enslaved populations occupy position of full target (d ≈ 0.95, high f(d)) — they are trapped with no exit options and no beneficiary status. The provincial elites occupy mixed position (d ≈ 0.55) — they are constrained but not trapped, and they benefit from the coordination structure even as they are subordinated to it. The imperial center occupies beneficiary position (d ≈ 0.05) — they benefit from Logos framework as a coordination mechanism that makes their will predictable and legitimate. The Stoic philosophers occupy organized position (d ≈ 0.40) — they have agency through advisory roles and intellectual leadership but are constrained by imperial authority and see sunset when institutionalized. The directionality overrides are not necessary — the structural derivation from beneficiary/victim declarations and exit options produces accurate positioning. The false Mountain from the analytical context reveals that d-value computation would produce negative or very low f(d) for a purely analytical observer, indicating that the 'natural law' framing is not inherent but contingent on philosophical interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the Stoic Logos framework is genuinely both coordination and extraction, depending on perspective. It is NOT pure extraction (Snare) masquerading as coordination (Rope) — that is the mandatrophy the analytical observer risks. Rather, it is a true Tangled Rope that exhibits coordination function (rational imperial governance, predictable decisions, shared ethical framework) while simultaneously extracting (labor, taxation, military conscription, philosophical suppression). The beneficiaries (imperial court, philosophers, provincial elites) genuinely benefit from the coordination function — the empire is more stable and more governable when based on Logos principles than on arbitrary will. The victims (enslaved populations, conquered provinces) genuinely bear extraction costs — the Logos justifies their subordination. The mandatrophy resolution is: this IS both coordination and extraction. The false summit (Mountain from analytical view) is the risk that an external observer naturalizes the Logos as objective law rather than recognizing it as a contingent institutional arrangement that serves coordination at the cost of extraction for those outside the beneficiary group.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    logos_philosophical_vs_political,
    'Is the Logos a genuinely binding philosophical principle discovered through reason, or a post-hoc rationalization for imperial authority that derives its force from coercion?',
    'Historical analysis of Marcus Aurelius''s personal writings vs public policy; examination of cases where Logos principles conflicted with imperial interests; comparison with non-Stoic philosophical traditions'' treatment of universal reason',
    'If genuine philosophy: constraint is Rope from multiple perspectives (shared rationality principle). If rationalization: constraint is Snare with philosophical theater. Classification shifts from primarily coordination to primarily extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(logos_philosophical_vs_political, conceptual, 'Whether Logos is genuine philosophy or political rationalization').

omega_variable(
    voluntary_submission_authenticity,
    'Does the emperor''s voluntary submission to Logos represent genuine ethical constraint on his power, or is it a performance that preserves absolute authority while appearing limited?',
    'Behavioral analysis of Marcus Aurelius''s decisions vs Logos principles; examination of instances where submission to Logos required actual sacrifice of imperial preference; comparison with other imperial governance frameworks'' constraint mechanisms',
    'If authentic constraint: Logos framework reduces effective imperial extractiveness, validates Rope/Scaffold classifications. If performance: submission is pure theater masking unrestricted extraction, validates Snare/Piton classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_submission_authenticity, empirical, 'Whether imperial submission to Logos is genuine or performative').

omega_variable(
    subject_perspective_historical_access,
    'How do enslaved and subject populations actually experience Stoic governance? Does the philosophical framework provide legitimacy that reduces physical extraction, or does it demoralize resistance by naturalizing subordination?',
    'Analysis of slave revolts, provincial uprisings, and tax resistance under Stoic vs non-Stoic emperors; examination of contemporary accounts from subject populations; assessment of whether Stoic rhetoric reduced or intensified extraction mechanisms',
    'If Logos reduces extraction by providing legitimacy: constraint is more Rope/Scaffold from powerless perspective. If Logos increases extraction by naturalizing it: constraint is severe Snare with philosophical suppression multiplier.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subject_perspective_historical_access, empirical, 'Whether Logos provides legitimacy that reduces or intensifies extraction for subjects').

omega_variable(
    institutional_memory_sunset,
    'When Stoicism loses institutional support (transition to Christianity), does the governance framework collapse suddenly or persist as inertial habit?',
    'Historical analysis of post-Aurelius administration under non-Stoic emperors; examination of persistence of Logos-derived administrative practices; tracking of theater ratio as explicit Stoic justification fades while administrative routine continues',
    'If framework collapses: Scaffold sunset clause is real, constraint resolves. If it persists through inertia: constraint degrades to Piton rather than sunsetting, indicating failed scaffold mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_memory_sunset, empirical, 'Whether Stoic governance framework sunsets or degrades to institutional piton').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stoic_logos_governance, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stoic_tr_t0, stoic_logos_governance, theater_ratio, 0, 0.38).
narrative_ontology:measurement(stoic_tr_t30, stoic_logos_governance, theater_ratio, 30, 0.58).
narrative_ontology:measurement(stoic_tr_t60, stoic_logos_governance, theater_ratio, 60, 0.75).

% Extraction over time
narrative_ontology:measurement(stoic_be_t0, stoic_logos_governance, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(stoic_be_t30, stoic_logos_governance, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(stoic_be_t60, stoic_logos_governance, base_extractiveness, 60, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stoic_logos_governance, enforcement_mechanism).
narrative_ontology:affects_constraint(stoic_logos_governance, roman_slave_economy_extraction).
narrative_ontology:affects_constraint(stoic_logos_governance, stoic_vs_epicurean_philosophical_suppression).
narrative_ontology:affects_constraint(stoic_logos_governance, christian_cosmology_replacement_mechanism).

% DUAL FORMULATION NOTE:
% The Stoic Logos governance framework should be decomposed into two related constraints: (1) Logos as a genuine coordination mechanism for rational imperial governance (lower extractiveness, genuine Rope), and (2) Logos as philosophical justification for hierarchy and extraction (higher extractiveness relative to subordinates, Snare from powerless perspective). These are not the same constraint viewed from different angles — they have different ε values depending on whether measurement is 'imperial administration efficiency' vs 'subject extraction burden'. The present story unifies them as Tangled Rope because both functions coexist structurally. Downstream constraints track what happens when Logos loses institutional support.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stoic_logos_governance, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
