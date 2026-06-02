% ============================================================================
% CONSTRAINT STORY: sanskritization_absorption_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sanskritization_absorption_mechanism, []).

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
 *   constraint_id: sanskritization_absorption_mechanism
 *   human_readable: Sanskritization as Interpretive-Accretion Absorption Pattern
 *   domain: religion/social_mobility
 *
 * SUMMARY:
 *   Sanskritization represents a structural pattern in which jatis classified
 *   as lower-varna adopt Brahmanical ritual practices (vegetarianism, purity
 *   discipline, twice-born ceremony participation, Sanskrit textual
 *   reference) to claim upward status mobility within the varna framework.
 *   The constraint is fundamentally about how interpretive-accretion
 *   containers process reclassification requests: the Vedic kernel remains
 *   formally unchanged, Brahminical interpretive authority remains the
 *   arbiter, but operational status classifications shift as successful
 *   adopters accumulate the markers that Brahminical discourse treats as
 *   signifiers of higher varna proximity. The pattern demonstrates the
 *   framework's prediction about acknowledgment-capable systems: they
 *   preserve the kernel through continuous interpretive extension rather than
 *   through direct canonical amendment. Sanskritization exhibits all three
 *   key properties of Tangled Rope: (1) genuine coordination function — jatis
 *   can use the mechanism to access higher-status networks, marriage
 *   alliances, and social participation that were previously barred; (2)
 *   asymmetric extraction — upwardly mobile jatis bear continuous costs
 *   (dietary transition, ritual discipline, ceremonial expenses) while
 *   Brahminical authority captures surplus through interpretive gatekeeping
 *   that raises recognition thresholds after each successful adoption; (3)
 *   active enforcement — the mechanism requires continuous validation by
 *   Brahminical scholars and Brahmin social gatekeepers. The extractiveness
 *   score (0.52) reflects moderate but real asymmetry: upwardly mobile jatis
 *   do achieve status gains and expanded social participation, but not at
 *   proportional cost to accumulated ritual investment. Theater ratio (0.68)
 *   reflects that formal varna boundaries remain largely unchanged despite
 *   widespread jati reclassification — the kernel's immutability is itself
 *   performative, maintained through rhetorical assertion of varna
 *   naturalness rather than through actual boundary protection.
 *
 * KEY AGENTS:
 *   - Brahminical Interpretive Authority: Institutional beneficiary (institutional/arbitrage) — captures expansion of framework's reach and relevance; extracts through continuous reinterpretation of status sufficiency thresholds
 *   - Upwardly Mobile Jati: Primary agent (moderate/constrained) — bears costs of marker adoption and ritual discipline; gains access to higher-status networks and participation; constrained by gatekeeping mechanism that raises thresholds after successful adoption
 *   - Downwardly Relative Jati: Secondary victim (powerless/trapped) — status declines relatively as competitors adopt markers; cannot exit competitive dynamic or reclaim prior status equilibrium without wholesale abandonment
 *   - Varna Kernel (Four-Fold Classification Scheme): Institutional structure (institutional/arbitrage) — formally immutable yet continuously extended through commentary; persists through theater despite empirical jati proliferation
 *   - Historical Analyst: Retrospective observer (analytical/analytical) — sees constraint as transitional scaffold within pre-modern framework; sunset evident in post-independence constitutional equality norms and educational credential systems replacing ritual markers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sanskritization_absorption_mechanism, 0.52).
domain_priors:suppression_score(sanskritization_absorption_mechanism, 0.58).
domain_priors:theater_ratio(sanskritization_absorption_mechanism, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sanskritization_absorption_mechanism, extractiveness, 0.52).
narrative_ontology:constraint_metric(sanskritization_absorption_mechanism, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sanskritization_absorption_mechanism, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sanskritization_absorption_mechanism, tangled_rope).
narrative_ontology:human_readable(sanskritization_absorption_mechanism, "Sanskritization as Interpretive-Accretion Absorption Pattern").
narrative_ontology:topic_domain(sanskritization_absorption_mechanism, "religion/social_mobility").

domain_priors:requires_active_enforcement(sanskritization_absorption_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sanskritization_absorption_mechanism, brahminical_interpretive_authority).
narrative_ontology:constraint_beneficiary(sanskritization_absorption_mechanism, upwardly_mobile_jati).
narrative_ontology:constraint_victim(sanskritization_absorption_mechanism, downwardly_relative_jati).
narrative_ontology:constraint_victim(sanskritization_absorption_mechanism, varna_boundary_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNWARDLY RELATIVE JATI (SNARE) — Jatis whose relative status declines as competitors adopt Brahmanical markers cannot exit the competitive marker-accumulation dynamic. Suppression operates through ritual monopoly: once a jati adopts vegetarianism or twice-born ceremony, the adopting group's claim to status shifts the baseline upward for all others. The downwardly relative group cannot reclaim the former status equilibrium — the constraint creates ratchet-lock in relative positioning. No path to exit except migration or wholesale cultural abandonment.
constraint_indexing:constraint_classification(sanskritization_absorption_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: UPWARDLY MOBILE JATI (TANGLED ROPE) — Genuine coordination function exists: adoption of Brahmanical practices enables participation in higher-status networks, marriage alliances, and administrative roles. But the mechanism is asymmetric extraction: the adopting jati must accumulate markers indefinitely, bearing costs of vegetarianism, ritual purity discipline, and scriptural study, while Brahminical authority extracts interpretive labor through continuously raising the bar for recognition. The constraint coordinates status claims with ritual performance, but captures surplus through interpretive gatekeeping.
constraint_indexing:constraint_classification(sanskritization_absorption_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRAHMINICAL INTERPRETIVE AUTHORITY (ROPE) — Experiences the constraint as pure coordination: the more jatis adopt Brahmanical markers, the more extensive is the Brahminical framework's reach and relevance. Each successful sanskritization event validates the interpretive authority's kernel — the varna framework expands without revision. The Brahmin lineage captures no direct extraction in the snare sense; rather, the authority structure itself becomes more foundational. The constraint is purely coordinative from this position: more jatis participating in the system means more interpretive labor, more prestige, more institutional centrality.
constraint_indexing:constraint_classification(sanskritization_absorption_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ORGANIZED JATI COUNCIL (TANGLED ROPE) — Collective actors coordinating sanskritization campaigns experience both coordination and extraction. The coordination function is real: pooling resources for community vegetarianism transitions, establishing ceremonial standards, negotiating with Brahminical authorities. But organized jatis also face extraction through the gatekeeping mechanism — acceptance by the Brahminical interpreters is contingent and continuously contested. The organizational capacity to mobilize status claims is partly captured by the interpretive authority through the requirement for continuous validation.
constraint_indexing:constraint_classification(sanskritization_absorption_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: VARNA KERNEL / CANONICAL TEXT (PITON) — The four-varna taxonomy persists as authoritative despite continuous category drift. Formal varna boundaries (Brahmin, Kshatriya, Vaishya, Shudra) are treated as immutable even as jatis reclassify through Sanskrit adoption. The kernel is maintained through theater: periodic assertions of the varna scheme's naturalness, invocation of Vedic authority, and treatment of boundary violations as temporary misclassifications rather than kernel revisions. The theater ratio reflects that the varna system's primary function (organizing society through hereditary occupational categories) has atrophied while its legitimation function (providing status claims framework) persists through interpretive extension. Theater rises as gap between formal kernel and empirical jati proliferation increases.
constraint_indexing:constraint_classification(sanskritization_absorption_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: HISTORICAL ANALYST (SCAFFOLD) — From a retrospective analytical view, sanskritization represents a temporary coordination mechanism that enabled low-varna jatis to access status gains within a rigid framework, functioning as a transitional bridge toward eventual caste system contestation and modern plural status hierarchies. The constraint has a sunset: as education, occupation pluralization, and constitutional equality norms emerged post-1947, the interpretation-based status claims became less binding. The analytical observer sees sanskritization as a coordinating scaffold that had real function (enabling status mobility within structural limits) but declining force as the kernel's authority eroded.
constraint_indexing:constraint_classification(sanskritization_absorption_mechanism, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sanskritization_absorption_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sanskritization_absorption_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sanskritization_absorption_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sanskritization_absorption_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sanskritization_absorption_mechanism, TR),
    TR >= 0.70.

:- end_tests(sanskritization_absorption_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate asymmetry reflecting genuine but incomplete status gain. Upwardly mobile jatis do achieve real outcomes (expanded marriage possibilities, administrative access, ritual participation rights) that they could not access without sanskritization. However, the outcomes are constrained by continuous interpretive gatekeeping — as more jatis adopt a marker set, Brahminical authorities implicitly raise the bar for equivalence. This is not total suppression (which would yield Snare), nor frictionless coordination (which would yield Rope), but mixed: the jati gains real access while bearing ongoing accumulation costs. Extractiveness rising from 0.38 to 0.58 over the interval reflects the mechanism's intensification as more jatis compete for status claims — as competition increases, the interpretive gatekeeping burden on Brahminical authorities increases, leading to more stringent recognition thresholds. Suppression (0.58): High but not total. External barriers (dietary transition costs, ritual discipline, ceremonial expenses, social ostracism risk) are significant but surmountable for organized jatis with resources. Internal barriers (interpretive gatekeeping by Brahminical authorities, indefinite marker accumulation requirements) are harder to quantify but real. Suppression does not operate through direct legal prohibition or economic isolation; rather through the constraint that interpretive validation is necessary and continuously contested. Theater ratio (0.68): High. The formal varna framework's immutability is maintained through rhetorical insistence rather than through actual structural defense. Brahminical authorities assert the naturalness and stability of varna categories even as jatis accumulate evidence of reclassification. The theater increases over the interval because the gap between formal varna claims and empirical jati mobility widens — more theatrical work is required to maintain the kernel's apparent immutability as more counter-evidence accumulates.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap illustrates the interpretive-accretion absorption pattern: the same constraint (sanskritization) appears as coordination (Rope) from the beneficiary's view because the beneficiary experiences expansion of their own authority's reach. It appears as mixed coordination-extraction (Tangled Rope) from the upwardly mobile agent's view because they gain real status while bearing asymmetric costs. It appears as pure extraction (Snare) from the downwardly relative agent's view because their status erodes with no mechanism for mitigation. The gap exists because the extraction mechanism operates through interpretive gatekeeping rather than through overt force — different agents at different status positions experience the same mechanism as either coordination, partial extraction, or pure suppression depending on whether their position relative to the threshold is improving, stable, or declining. The highest-status agents see the system expanding (Rope); mid-status agents see themselves climbing but at cost (Tangled Rope); lower-status agents see themselves sinking without rescue (Snare). This creates a structural illusion: the Rope experience of beneficiaries makes the system appear coordinative, masking the Snare dynamics experienced by losers in the competition.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality architecture reflects interpretive-accretion mechanics. Brahminical authority benefits from system expansion with zero cost — their authority becomes more extensive as more jatis invoke the framework's legitimacy. Applied sigmoid produces strongly negative f(d), meaning the authority experiences the constraint as beneficial coordination. Upwardly mobile jatis are mixed: they gain real status and social access, but must continuously accumulate markers as thresholds rise. Their d value is moderate (around 0.55), producing f(d) ≈ 0.75, moderate positive χ that reflects balanced costs and gains. Downwardly relative jatis are trapped in competitive dynamics where their status erodes as competitors rise — they cannot gain from sanskritization (they're already positioned to lose) and cannot escape because the framework is the only recognized status taxonomy. Their d value is high (around 0.92), producing f(d) ≈ 1.28, high positive χ reflecting experienced extraction. The structure is self-reinforcing: beneficiaries' positive experience validates the system's legitimacy, making exit costlier for trapped agents. Upwardly mobile agents' moderate positive experience sustains the system's claim to offer opportunity, masking the structural losses experienced by downwardly relative agents. The gatekeeping mechanism (what raises thresholds after successful adoption) is the extraction channel — it is invisible to beneficiaries (they see only expansion) but painfully visible to trapped agents (who see only rising requirements for status equivalence). This invisibility is what allows the constraint to persist: those who benefit see only coordination, while those who lose have no legitimate voice within the framework to claim extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Sanskritization resolves the mandatrophy (the tension between Rope pure-coordination claims and Snare pure-extraction classification) by demonstrating that both are correct from different structural positions. The constraint is genuinely coordinative from the Brahminical authority's perspective — the system expands, interpretive authority increases, the framework becomes more foundational. But it is genuinely extractive from the downwardly relative jati's perspective — status erodes irreversibly, no exit option exists except wholesale abandonment, the competitive ratchet locks them in. The upwardly mobile jati experiences the true hybrid nature: real gains (coordination function) paired with asymmetric costs (extraction function). The mandatrophy resolves not by finding the 'true' type but by recognizing that the perspectival position determines the experienced type. Sanskritization is genuinely Tangled Rope — it has coordination function (jatis can access higher status through the mechanism) and asymmetric extraction (upwardly mobile agents bear ongoing costs to maintain status claims). The Rope experience of beneficiaries and the Snare experience of losers are both consequences of the underlying Tangled Rope structure: mixed coordination-extraction systems always look like pure coordination from the beneficiary's view and pure extraction from the trapped agent's view. The system's stability depends on this: if Brahminical authorities saw themselves as extractors, they would not maintain the interpretive framework. If trapped agents had voice, they would demand the kernel be abandoned. The framework's persistence depends on the beneficiaries' truthful perception of coordination paired with the trapped agents' voicelessness. This is the structure of all tangled ropes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_preservation_vs_container_revision,
    'Is sanskritization genuine kernel preservation with interpretive extension, or is it a mischaracterization of actual canonical text revision buried in commentary?',
    'Detailed textual analysis: comparison of Brahmanical authorities'' treatments of upwardly mobile jatis across centuries. If jatis are recategorized in commentarial lineages, this signals container revision. If explicitly bracketed as jatis adopting Brahmanical practice (not becoming Brahmins), kernel preservation holds.',
    'If kernel preservation: the constraint is interpretive-accretion absorption (Rope from Brahmin perspective). If actual revision: the constraint is institutional reclassification (Tangled Rope from all perspectives, with different suppression values). Classification framework itself may require amendment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_preservation_vs_container_revision, empirical, 'Whether varna kernel is actually preserved or revised through commentary').

omega_variable(
    marker_accumulation_closure,
    'Is there a finite marker set whose adoption constitutes legitimate status claim, or is marker accumulation open-ended with continuously rising recognition thresholds?',
    'Historical ethnography: document whether Brahminical authorities endorsed specific marker-sets as sufficient for status equivalence, or whether each successful adoption triggered new marker requirements. Pattern indicates constraint type.',
    'If closed set: constraint is Rope with clear exit (adopt markers, claim status, participate). If open-ended: constraint is Snare (perpetual accumulation with no terminal state). If mixed (closed in some regions/periods, open in others): supports Tangled Rope classification with regional/temporal variance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marker_accumulation_closure, empirical, 'Whether marker accumulation has closure or perpetual escalation').

omega_variable(
    brahminical_extraction_mechanism_obscurity,
    'How much of the experienced suppression by upwardly mobile jatis comes from external ritual barriers (costs of vegetarianism, ceremonial expenses) versus internal gatekeeping (continuous reinterpretation of sufficiency)?',
    'Comparative case analysis: document jatis that accumulated standard markers (vegetarianism, twice-born ceremony, scriptural reference) but faced continued Brahminical rejection versus those that achieved acceptance. If barriers persist despite marker adoption, gatekeeping dominates.',
    'If external barriers dominant: constraint approaches Tangled Rope with transparent extraction. If gatekeeping dominant: constraint approaches Snare with obscured extraction mechanism. If mixed: supports baseline Tangled Rope assessment with higher suppression variance across jatis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_extraction_mechanism_obscurity, empirical, 'Proportion of suppression from ritual barriers versus interpretive gatekeeping').

omega_variable(
    regional_intensity_variance,
    'Does sanskritization intensity vary systematically by region (high in south, low in north; high in merchant zones, low in agricultural zones), and if so, does this indicate regional variation in Brahminical authority structure or alternative status pathways?',
    'Regional historical analysis: map sanskritization intensity against regional Brahminical institutional density, merchant guild organization, and Islamic/other religious institutional presence. Correlation patterns identify whether constraint is universal varna mechanism or regional interpretive variant.',
    'If universal: varna framework is monolithic constraint. If regional variant: classification may differ by region (Tangled Rope in high-authority regions, Rope in low-authority regions). Fragment corpus into region-specific stories if variance is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_intensity_variance, empirical, 'Regional variation in sanskritization intensity and mechanism').

omega_variable(
    post_independence_constraint_mutation,
    'How did the constraint''s structural properties change after Indian independence and constitutional equality provisions? Did sanskritization cease, persist with modified mechanism, or transform into different status-claim pathways?',
    'Historical rupture analysis: document sanskritization patterns pre-1947 vs. post-1947. If constraint disappeared, reclassify as historical scaffold with clear sunset. If it persisted, document new extraction mechanisms (may be educational credential accumulation replacing ritual markers).',
    'If cleared post-1947: the constraint was a transitional scaffold within colonial/pre-modern framework. If persisted with mutation: constraint is more fundamental than varna system per se. May require separate stories for pre- and post-independence constraint variants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_independence_constraint_mutation, empirical, 'Whether sanskritization constraint persists post-1947 or was tied to pre-modern framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sanskritization_absorption_mechanism, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skrt_tr_t0, sanskritization_absorption_mechanism, theater_ratio, 0, 0.52).
narrative_ontology:measurement(skrt_tr_t2, sanskritization_absorption_mechanism, theater_ratio, 2, 0.58).
narrative_ontology:measurement(skrt_tr_t4, sanskritization_absorption_mechanism, theater_ratio, 4, 0.65).
narrative_ontology:measurement(skrt_tr_t6, sanskritization_absorption_mechanism, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(skrt_be_t0, sanskritization_absorption_mechanism, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(skrt_be_t2, sanskritization_absorption_mechanism, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(skrt_be_t4, sanskritization_absorption_mechanism, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(skrt_be_t6, sanskritization_absorption_mechanism, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sanskritization_absorption_mechanism, identity_coordination).
narrative_ontology:affects_constraint(sanskritization_absorption_mechanism, caste_varna_categorical_stability).
narrative_ontology:affects_constraint(sanskritization_absorption_mechanism, brahminical_authority_perpetuation).
narrative_ontology:affects_constraint(sanskritization_absorption_mechanism, hindu_ritual_marker_accumulation).

% DUAL FORMULATION NOTE:
% Sanskritization is a mechanism within the broader caste system constraint family. Upstream: the varna kernel's formal immutability (constraint: caste_varna_categorical_stability, ε=0.12, Mountain). Sanskritization is downstream mechanism through which the kernel is operationally extended without formal revision. Parallel: brahminical authority perpetuation (constraint: brahminical_authority_perpetuation, ε=0.35, Rope from authority perspective). Sanskritization is the mechanism through which Brahminical authority expands its reach. Dependent: hindu ritual marker accumulation (constraint: hindu_ritual_marker_accumulation, ε=0.48, Tangled Rope). Sanskritization is the practical instantiation of ritual marker-stack requirements. All three are linked through network edges — the decomposition reflects that the varna system's formal claim (immutability) is distinct from the operational mechanism (interpretive extension through sanskritization) which is distinct from the authority mechanism (Brahminical interpretive control) which is distinct from the practice mechanism (ritual marker accumulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sanskritization_absorption_mechanism, powerless, 0.92).
constraint_indexing:directionality_override(sanskritization_absorption_mechanism, moderate, 0.55).
constraint_indexing:directionality_override(sanskritization_absorption_mechanism, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
