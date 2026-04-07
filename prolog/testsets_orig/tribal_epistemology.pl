% ============================================================================
% CONSTRAINT STORY: tribal_epistemology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tribal_epistemology, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: tribal_epistemology
 *   human_readable: Tribal Epistemology as Extractive Constraint on Knowledge Access
 *   domain: epistemology/social/cognitive
 *
 * SUMMARY:
 *   Tribal epistemology — the organization of knowledge claims around group
 *   membership, in-group/out-group boundaries, and control by epistemic
 *   authorities — functions as a structural constraint on knowledge access
 *   and validation. The constraint exhibits extractive properties (blocking
 *   out-group access, maintaining authority asymmetries) alongside genuine
 *   coordination functions (organizing shared truth claims, enabling group
 *   coherence through common epistemic frameworks). The tension between these
 *   functions determines whether tribal epistemology is a Snare (pure
 *   extraction), a Tangled Rope (mixed coordination and extraction), or a
 *   Rope (genuine coordination). The theater ratio (0.78) reflects that much
 *   epistemic gatekeeping relies on performative elements: initiation
 *   rituals, specialized rhetoric, status markers, and in-group/out-group
 *   linguistic codes that signal knowledge membership rather than transmit
 *   technical content. Over the measurement interval, extractiveness has
 *   increased from 0.35 to 0.58 as external epistemic systems (science,
 *   mathematics, empirical verification) have become more accessible and more
 *   reliable, shifting the relative cost of tribal epistemology from
 *   'coordinate shared truth' to 'maintain in-group control.' The constraint
 *   is currently degrading from a Rope toward a Snare as its coordination
 *   function declines and its extraction function becomes more apparent.
 *
 * KEY AGENTS:
 *   - In-Group Knowledge Controllers: Primary beneficiary (institutional/arbitrage) — maintain epistemic authority, elevated status, selective access grant/denial. Coordinate the group through shared truth claims.
 *   - Out-Group Seekers: Primary victim (powerless/trapped) — denied access to in-group knowledge, bear full cost of epistemic boundary, cannot exit without severe social consequence.
 *   - Boundary Crossers/Apostates: Secondary victim (moderate/identity_locked or constrained) — structurally mobile but identity-fused to tribal epistemology; crossing requires abandoning self-concept and social bonds.
 *   - Tradition-Maintaining Institutions: Secondary beneficiary (institutional/constrained) — maintain ritual and gatekeeping through institutional inertia; increasingly degraded as external epistemic systems prove superior.
 *   - Organized Out-Groups: Collective victim/agent (organized/constrained) — collectively possess resources to build alternative epistemic pathways; reduce isolation of individual seekers through network effects.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent social choice (degree of epistemic closure) as inherent to human cognition; must identify false summit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tribal_epistemology, 0.58).
domain_priors:suppression_score(tribal_epistemology, 0.65).
domain_priors:theater_ratio(tribal_epistemology, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tribal_epistemology, extractiveness, 0.58).
narrative_ontology:constraint_metric(tribal_epistemology, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(tribal_epistemology, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tribal_epistemology, snare).
narrative_ontology:human_readable(tribal_epistemology, "Tribal Epistemology as Extractive Constraint on Knowledge Access").
narrative_ontology:topic_domain(tribal_epistemology, "epistemology/social/cognitive").

domain_priors:requires_active_enforcement(tribal_epistemology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tribal_epistemology, in_group_knowledge_controllers).
narrative_ontology:constraint_victim(tribal_epistemology, out_group_seekers).
narrative_ontology:constraint_victim(tribal_epistemology, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OUT-GROUP SEEKER (SNARE) — Trapped by epistemic boundaries that deny access to in-group knowledge claims. Bears full cost of exclusion: cannot access proven methods, verification pathways, or foundational reasoning. No exit mechanism; attempting to cross the epistemic boundary incurs severe social cost. Maximum experienced extraction through systematic knowledge denial.
constraint_indexing:constraint_classification(tribal_epistemology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: BOUNDARY CROSSER (TANGLED ROPE) — Constrained by identity cost: crossing the epistemic boundary requires abandoning tribal markers, incurring social rupture. Yet also benefits from access to both epistemic systems — bridging position offers comparative advantage and knowledge synthesis. Asymmetric extraction tempered by unique coordination function: boundary crossers validate and integrate across epistemic divides.
constraint_indexing:constraint_classification(tribal_epistemology, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IN-GROUP KNOWLEDGE CONTROLLER (ROPE) — Benefits from restricted epistemic access: elevated status, coordination advantage, ability to certify who knows and what counts as true. Experiences the constraint as a coordination mechanism: maintaining epistemic boundaries organizes the group, enables shared ritual, and preserves in-group cohesion. Net beneficiary through arbitrage: can selectively grant or withhold access.
constraint_indexing:constraint_classification(tribal_epistemology, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: TRADITION-MAINTAINING INSTITUTION (PITON) — Maintains tribal epistemology through ritual, initiation, and knowledge gatekeeping. Theater ratio (0.78) reflects that much epistemic gatekeeping is performative: the 'specialized knowledge' often consists of rhetorical patterns, status markers, and initiation sequences rather than genuine technical insight. Institution persists through inertia and institutional identity rather than functional necessity. Degraded from Rope as external epistemic systems (scientific method, mathematical proof, digital databases) prove more reliable.
constraint_indexing:constraint_classification(tribal_epistemology, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ORGANIZED OUT-GROUP (TANGLED ROPE) — Collective agents (scientific communities, diaspora networks, pluralistic epistemic systems) experience tribal epistemology as an extractive barrier, yet also benefit from the coordination function it demonstrates. The organized perspective sees both the extraction (knowledge denial) and the underlying coordination (how groups maintain shared truth claims). Constrained by institutional resistance but increasingly able to build alternative epistemic pathways. Lower effective extraction than the isolated seeker because the group has agency and exit routes.
constraint_indexing:constraint_classification(tribal_epistemology, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational scale, tribal epistemology appears as a natural feature of human cognition: in-group preference, boundary maintenance, and epistemic closure are intrinsic to how humans organize knowledge socially. All groups exhibit some form of in-group/out-group epistemic boundaries. This perspective risks naturalizing what is actually a choice of degree and enforcement mechanism. The mountain classification is a false summit — it treats what is a variable constraint (degrees of epistemic closure) as an immutable law.
constraint_indexing:constraint_classification(tribal_epistemology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tribal_epistemology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tribal_epistemology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tribal_epistemology, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tribal_epistemology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tribal_epistemology, TR),
    TR >= 0.70.

:- end_tests(tribal_epistemology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time. The constraint extracts from out-group seekers (deny knowledge access, maintain status asymmetry, require conformity for membership). The extraction increased from 0.35 to 0.58 over the interval because external epistemic systems have become sufficiently reliable and accessible that tribal epistemology's 'coordination benefit' claim has weakened — the constraint is increasingly experienced as pure gatekeeping rather than as enabling shared truth. Suppression (0.65): High and structural. Barriers to out-group access include institutional rules (who can join, what must be learned), social cost (abandonment, shunning), identity fusion (self-concept constituted through tribe), and cognitive capture (internalized belief that tribal epistemology is superior or unique). Theater ratio (0.78): Very high. Much of the epistemic gatekeeping is performative: specialized rhetoric that signals membership without transmitting unique technical content, initiation sequences that test conformity rather than knowledge, status markers that are semantic rather than substantive. As external epistemic systems have demonstrated superior predictive power and reproducibility, the in-group knowledge claims have become increasingly theatrical — maintained through ritual and authority assertion rather than through genuine verification. Claimed type (Snare): The analytical observer would classify this as Snare because the extraction (knowledge denial, status control) now dominates the coordination function (organizing group truth claims). However, lower-power perspectives (out-group seekers, boundary crossers) may experience it as Snare, while in-group controllers experience it as Rope. This perspectival gap is the core diagnostic signal.
 *
 * PERSPECTIVAL GAP:
 *   The in-group beneficiary sees a Rope: tribal epistemology genuinely coordinates the group, enables verification of shared claims within the group framework, and maintains social cohesion through epistemic unity. This is their honest experience. The out-group victim sees a Snare: they are denied knowledge, suppressed by boundaries, have no exit, and experience the in-group's coordination as a closed system designed to exclude them. This is also honest. The boundary crosser sees Tangled Rope: the system both coordinates (shared truth enable collaboration) and extracts (identity cost, relational rupture). The tradition-maintaining institution sees Piton: the ritual is degrading, external epistemic systems are proving superior, yet the institution persists through inertia. The organized out-group sees a sunset: alternative pathways are building (open epistemic networks, scientific method, empirical verification), making tribal epistemology's extraction mechanism obsolete. The analytical observer risks seeing Mountain: 'tribal epistemology is how humans naturally organize knowledge' naturalizes what is a choice of enforcement mechanism. The false summit detection reveals this: if tribal epistemology were a Mountain, it would appear unchanged across ALL perspectives and ALL observables. But it doesn't — the in-group measures it as Rope, the out-group as Snare, the institution as Piton. The perspectival gap IS the signal that tribal epistemology is contingent, not necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by the agent's structural relationship to the knowledge-access extraction flow. In-group controllers benefit from restricted access (low d ≈ 0.10, high institutional status and arbitrage options), experiencing negative effective extraction (the constraint subsidizes them). Out-group seekers bear the cost of denial (high d ≈ 0.90, powerless and trapped), experiencing maximum effective extraction. Boundary crossers occupy intermediate position (moderate d ≈ 0.55, constrained by identity cost but gaining access). The derivation from beneficiary/victim declarations: in-group controllers are listed as beneficiaries (arbitrage exit, institutional power) → low d → negative/low chi; out-group seekers are listed as victims (trapped exit, powerless) → high d → high chi via sigmoid f(d). This structural math explains why the in-group sees Rope while the out-group sees Snare: they occupy opposite positions in the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that tribal epistemology is both genuinely coordinating (Rope from in-group perspective) AND genuinely extracting (Snare from out-group perspective). The mandatrophy question is: 'Is this coordination or extraction?' The answer is: 'Yes, both, depending on structural position.' The in-group uses tribal epistemology to coordinate shared truth claims and maintain group cohesion — this is a real coordination function. The out-group is extracted from via knowledge denial and suppression — this is a real extraction mechanism. At the boundary crosser level, the coordination function and extraction mechanism are literally the same process: the identity cost that makes crossing painful is the same mechanism that coordinates in-group loyalty. Tangled Rope classification is correct because the constraint simultaneously solves a coordination problem (how does a group maintain shared epistemology?) and extracts asymmetrically from out-groups (how does a group maintain power over knowledge claims?). The theater ratio (0.78) indicates that much of the orchestration is performative — ritual and rhetoric maintaining the appearance of specialized knowledge — but genuine coordination can occur even with high theater if the in-group actually verifies and updates its claims. The degradation from Rope (high functionality) to Snare (high extraction) over the measurement interval reflects that as external epistemic systems have proven superior, the in-group's gatekeeping has shifted from 'organizing group truth' to 'defending group authority against superior alternatives.' The theater has increased because the in-group must work harder to maintain the epistemic boundary against competing, more reliable systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_coordination_boundary,
    'Does epistemic boundary maintenance coordinate in-group knowledge claims, or does it primarily extract status and authority from out-groups?',
    'Comparative analysis of boundary-maintaining communities: measure whether epistemic boundaries increase net group knowledge validity or primarily increase in-group status/control. Compare internal consistency of knowledge claims to external verification.',
    'If primarily coordination: reclassify toward Rope/Tangled Rope. If primarily extraction: confirm Snare. If mixed: measure proportion and adjust chi accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether epistemic boundaries coordinate shared truth or extract authority').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of out-group access structural (physical barriers, institutional rules) or internalized (the out-group has accepted the epistemic boundary as legitimate)?',
    'Post-exit trajectory analysis: if out-group members who gain access still defer to in-group epistemic authority, suppression is partially internalized. If they immediately adopt alternative epistemic standards, suppression is primarily structural.',
    'If internalized: effective suppression is higher than measured — the out-group carries the suppression with them after boundary crossing. If structural: suppression can be reduced by removing institutional barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in out-group epistemology').

omega_variable(
    identity_fusion_in_boundary_crossing,
    'Is epistemic boundary crossing blocked primarily by material costs (career damage, relocation, institutional barriers) or by identity fusion (the agent''s self-concept is constituted through tribal epistemology)?',
    'Qualitative analysis of boundary-crossing narratives: coded for identity language (''I am a member of the tribe,'' ''to think this way is to betray who I am'') vs. cost language (''I cannot afford the social penalty,'' ''my career depends on staying''). Longitudinal tracking of agents who cross: do they maintain identity fusion to the original epistemic system or genuinely adopt the new one?',
    'If identity-fused: agents are identity_locked even when material barriers are low. If primarily cost-based: agents are constrained or trapped depending on barrier magnitude. This determines whether the out-group seeker''s exit_options should be identity_locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_boundary_crossing, empirical, 'Whether boundary crossing is blocked by material cost or identity fusion').

omega_variable(
    alternative_epistemology_viability,
    'Do alternative epistemic systems (science, mathematics, empirical verification) actually provide superior knowledge for domains where tribal epistemology claims authority?',
    'Comparative efficacy testing: predictive accuracy, reproducibility, technological applications, and conflict resolution in domains like medicine, engineering, agriculture, and conflict prevention. Track which epistemic system produces actionable knowledge.',
    'If alternative systems are superior: tribal epistemology is pure extraction (Snare) with no coordination function. If tribal and alternative systems are incommensurable: classification shifts toward Rope (coordination without extraction, just different frameworks). If tribal epistemology proves superior in specific domains: reclassify toward Tangled Rope (genuine coordination function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_epistemology_viability, empirical, 'Whether alternative epistemic systems are superior to tribal epistemology').

omega_variable(
    in_group_knowledge_coherence,
    'Is the in-group knowledge actually coherent, internally consistent, and verified within the in-group, or is gatekeeping primarily maintaining the appearance of coherence?',
    'Audit of in-group knowledge claims: assess internal consistency, empirical grounding, and how the in-group resolves contradictions. Compare to audit standards for external epistemic systems. Track how often in-group knowledge claims are revised vs. maintained unchanged.',
    'If truly coherent and verified: in-group sees Rope (genuine coordination). If primarily appearance: in-group classification is Piton (degraded ritual). If selectively coherent: Tangled Rope (some genuine knowledge, some theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(in_group_knowledge_coherence, empirical, 'Whether in-group knowledge is coherent or theater-maintained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tribal_epistemology, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tribal_tr_t0, tribal_epistemology, theater_ratio, 0, 0.55).
narrative_ontology:measurement(tribal_tr_t5, tribal_epistemology, theater_ratio, 5, 0.68).
narrative_ontology:measurement(tribal_tr_t10, tribal_epistemology, theater_ratio, 10, 0.78).

% Extraction over time
narrative_ontology:measurement(tribal_be_t0, tribal_epistemology, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tribal_be_t5, tribal_epistemology, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(tribal_be_t10, tribal_epistemology, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tribal_epistemology, identity_coordination).
narrative_ontology:boltzmann_floor_override(tribal_epistemology, 0.12).
narrative_ontology:affects_constraint(tribal_epistemology, epistemic_authority_asymmetry).
narrative_ontology:affects_constraint(tribal_epistemology, in_group_out_group_bias).
narrative_ontology:affects_constraint(tribal_epistemology, knowledge_commons_degradation).

% DUAL FORMULATION NOTE:
% Tribal epistemology is downstream of identity coordination (how groups maintain boundaries and shared membership markers) but represents a distinct constraint on knowledge validation. The upstream identity coordination has its own extractiveness reflecting boundary maintenance costs; this constraint has its own extractiveness reflecting knowledge-access extraction. Decomposition separates 'how groups maintain identity' (upstream) from 'how that identity maintenance blocks knowledge access' (this constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tribal_epistemology, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
