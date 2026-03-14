% ============================================================================
% CONSTRAINT STORY: indigenous_self_determination_global
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indigenous_self_determination_global, []).

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
 *   constraint_id: indigenous_self_determination_global
 *   human_readable: Indigenous Self-Determination as Global Constraint
 *   domain: political/colonial/institutional
 *
 * SUMMARY:
 *   Indigenous self-determination as a global constraint operates at the
 *   intersection of territorial sovereignty, epistemic legitimacy, and
 *   institutional power. The constraint prevents indigenous nations from
 *   exercising governance, territorial, and cultural autonomy within the
 *   current international order, which is structured around Westphalian state
 *   sovereignty. This is a long-running structural extraction: colonialism
 *   creates the constraint; recognition frameworks (UNDRIP, ILO 169,
 *   constitutional amendments) performatively acknowledge it while
 *   maintaining it through legal and resource mechanisms. The constraint
 *   exhibits all major types depending on observer position: snare
 *   (indigenous perspective at multiple timescales), tangled rope (organized
 *   movements at national scale), scaffold (international frameworks with
 *   unfulfilled sunset), rope (state apparatus experiencing coordination),
 *   piton (liberal rights regime performing recognition), and snare
 *   (analytical universal view). The extractiveness has declined modestly
 *   (0.85 to 0.68) over 500 years as organized indigenous movements have
 *   achieved partial recognition and resource gains, but suppression remains
 *   very high (0.72) and the constraint persists in transformed rather than
 *   eliminated form. Theater ratio has risen (0.35 to 0.58) as international
 *   recognition mechanisms have proliferated while actual power transfer has
 *   remained limited — the constraint now operates substantially through
 *   performative rather than purely coercive mechanisms.
 *
 * KEY AGENTS:
 *   - Indigenous Nations: Primary victim (powerless/trapped and identity_locked) — denied territorial autonomy, governance rights, and resource control. Centuries of coercive assimilation create cognitive lock alongside structural barriers.
 *   - Indigenous Self-Determination Movements: Organized victim-agents (organized/constrained) — achieve genuine coordination and alternative governance at sub-state scale but face recognition barriers and resource dependency on states
 *   - Colonial/Post-Colonial States: Primary beneficiary (institutional/arbitrage) — extract territorial resources, governance legitimacy, and labor while maintaining control through legal sovereignty. Can arbitrage between recognition and subordination.
 *   - Extractive Capital Interests: Secondary beneficiary (powerful/arbitrage) — depend on state guarantee of resource access; resist self-determination because it would transfer control of land-based resources to indigenous stewardship
 *   - International Recognition Frameworks: Institutional actor (institutional/arbitrage) — perform self-determination rights while embedding state sovereignty supremacy. Theater system that legitimizes the constraint while appearing to challenge it.
 *   - Analytical Observer: Universal/civilizational perspective (analytical/analytical) — sees zero-sum sovereignty as the core constraint. Recognizes that current international order makes self-determination structurally impossible without state dissolution or systemic redesign.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indigenous_self_determination_global, 0.68).
domain_priors:suppression_score(indigenous_self_determination_global, 0.72).
domain_priors:theater_ratio(indigenous_self_determination_global, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indigenous_self_determination_global, extractiveness, 0.68).
narrative_ontology:constraint_metric(indigenous_self_determination_global, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(indigenous_self_determination_global, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indigenous_self_determination_global, snare).
narrative_ontology:human_readable(indigenous_self_determination_global, "Indigenous Self-Determination as Global Constraint").
narrative_ontology:topic_domain(indigenous_self_determination_global, "political/colonial/institutional").

domain_priors:requires_active_enforcement(indigenous_self_determination_global).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indigenous_self_determination_global, colonial_administrative_structures).
narrative_ontology:constraint_beneficiary(indigenous_self_determination_global, extractive_capital_interests).
narrative_ontology:constraint_victim(indigenous_self_determination_global, indigenous_nations).
narrative_ontology:constraint_victim(indigenous_self_determination_global, indigenous_land_stewardship_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS NATIONS (SNARE) — Structurally trapped by colonial state sovereignty frameworks that deny territorial and governance autonomy. Exit would require rejecting the entire institutional apparatus imposed through colonialism. No alternative frameworks recognized at scale. High suppression through legal prohibition, military force, resource dependency, and epistemic erasure. Maximum experienced extraction.
constraint_indexing:constraint_classification(indigenous_self_determination_global, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIGENOUS NATIONS (SNARE - IDENTITY_LOCKED) — Many indigenous communities are identity-locked into colonial state structures through centuries of coercive assimilation, educational displacement, and legal redefinition of territorial and governance claims. The constraint is experienced as immutable not just structurally (military, legal barriers) but cognitively (internalized colonial definitions of what 'legitimate' governance looks like). Perspectival gap: structurally mobile through decolonization movements, but psychologically trapped by identity frames that make exit unthinkable within inherited institutional logic. Still classified as snare (high extraction, high suppression), but the binding mechanism is partially internalized.
constraint_indexing:constraint_classification(indigenous_self_determination_global, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: ORGANIZED INDIGENOUS MOVEMENTS (TANGLED ROPE) — At biographical timescale and national scope, indigenous movements achieve genuine coordination through shared governance protocols, land management systems, and institutional alternatives (Zapatista municipios, Maori co-governance, Aboriginal land trusts). These movements also extract costs: internal hierarchy consolidation, negotiation dependencies on colonial states, and partial co-optation through recognition regimes that extract legitimacy while limiting autonomy. Constrained exit: movements can organize alternatives but face recognition barriers and resource limitations imposed by the state. Mixed classification reflects both real coordination function and real extraction.
constraint_indexing:constraint_classification(indigenous_self_determination_global, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL FRAMEWORKS (SCAFFOLD) — UNDRIP, ILO conventions, and international recognition frameworks nominally establish self-determination as a right with sunset logic: once indigenous sovereignty is recognized and implemented, the constraint dissolves. However, these frameworks have a sunset clause that remains unfulfilled — recognition without resource transfer, agreements without enforcement, theater without structural change. Theater ratio remains high because international frameworks perform recognition while state structures enforce continued subordination. Organized actors (indigenous diplomacy, NGO networks) see the scaffolding as having real function but also as incomplete — the sunset has not fired.
constraint_indexing:constraint_classification(indigenous_self_determination_global, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COLONIAL STATE APPARATUS (ROPE) — At immediate timescale, state structures experience self-determination claims as a coordination problem (how to manage territory, resources, governance) that the state solves through hierarchical integration and recognition regimes. From this perspective, the constraint is pure coordination: the state gains legitimacy, efficiency in administration, and access to territorial knowledge through recognition frameworks. Arbitrage exit: states can exit by full transfer of sovereignty (rare) or partial recognition with resource retention (common). Net beneficiary of the constraint — extraction runs from indigenous to state structures.
constraint_indexing:constraint_classification(indigenous_self_determination_global, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LIBERAL RIGHTS RECOGNITION (PITON) — The liberal human rights framework recognizes self-determination as an abstract universal right while simultaneously making it unexercisable in practice through state sovereignty doctrine (which supersedes individual/collective rights). The constraint persists through performative compliance: states acknowledge self-determination rights while maintaining structural subordination. Theater ratio reflects that recognition rituals (UN declarations, constitutional amendments, consultation ceremonies) substitute for actual power transfer. The function (managing colonial contradiction) has atrophied into pure performance; maintenance occurs through institutional inertia (international law structures that legitimize the regime). High theater, degraded function.
constraint_indexing:constraint_classification(indigenous_self_determination_global, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a universal/civilizational perspective, indigenous self-determination is globally suppressed by the Westphalian state system itself — a structural feature, not a contingent policy choice. Sovereign territory is allocated to nation-states; indigenous territorial claims compete with state sovereignty in a zero-sum framework that the current international law cannot resolve. High extraction, high suppression, low coordination benefit (states use the framework to manage contradiction, not to solve genuine coordination problems). The constraint's existence depends on denying indigenous systems equal standing in the recognition space.
constraint_indexing:constraint_classification(indigenous_self_determination_global, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indigenous_self_determination_global_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indigenous_self_determination_global, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indigenous_self_determination_global, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indigenous_self_determination_global, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indigenous_self_determination_global, TR),
    TR >= 0.70.

:- end_tests(indigenous_self_determination_global_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint systematically transfers control of indigenous territories (land, resources, governance capacity) to state and capital actors while denying indigenous nations autonomy. The 17-point decline over 500 years reflects modest gains through organized movements and recognition frameworks, but the baseline remains very high because the fundamental transfer (territorial control) has never been reversed. Suppression (0.72): Very high. Multiple suppression mechanisms operate simultaneously: legal prohibition (indigenous governance declared invalid), military enforcement (police/military violence against self-determination claims), resource dependency (states monopolize capital allocation), epistemic erasure (indigenous knowledge systems excluded from legitimacy space), and identity-lock (centuries of assimilation create cognitive barriers to imagining alternatives). Suppression does not decline measurably because removing one mechanism (e.g., legal prohibition) does not remove others (resource dependency, epistemic exclusion persist). Theater ratio (0.58): Moderate-high. At t=0 (early colonialism), extraction was purely coercive (low theater). Modern constraint operates substantially through recognition theater: UNDRIP declarations, constitutional amendments, consultation requirements, land acknowledgments. These rituals substitute for actual power transfer. Theater has risen as international pressure increases because states can now satisfy external legitimacy demands through performative recognition while maintaining structural subordination. The rise in theater indicates constraint evolution from pure suppression to suppression-plus-performance, which reduces visibility but not extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates the maximal perspectival gap in the corpus. Indigenous nations at immediate/biographical timescale see snare (immutable suppression, no exit). At generational timescale with identity-lock, they experience it as mountain (unchangeable law of colonialism). Organized movements at national/biographical scale see tangled rope (mixed coordination and extraction). States at immediate scale see rope (coordination problem they solve). International frameworks see scaffold (temporary, sunset-able). Liberal rights regimes see piton (performative theater). The analytical observer at universal/civilizational scale risks mountain (sovereignty system is immutable) but this is a false summit — structural data shows contingency (other sovereignty frameworks possible, zero-sum allocation is Westphalian not universal). The gap between snare (indigenous) and rope (state) is diagnostic: it reveals that the constraint's existence depends on power asymmetry, not on the coordination function's necessity. If indigenous and states experienced it identically (both as rope or both as snare), the constraint would be either pure coordination or pure extraction. The maximal gap indicates hybrid structure (tangled rope at system level) experienced oppositely by beneficiary and victim.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (chi) derives from base extractiveness (ε=0.68), directionality f(d), and scope modifier σ(S). Indigenous nations at global scope (σ=1.2): d ≈ 0.90 produces f(d) ≈ 1.38, so χ ≈ 0.68 × 1.38 × 1.2 ≈ 1.13 (experienced extraction exceeds base due to global coordination of suppression). Organized movements at national scope (σ=1.0): d ≈ 0.60 produces f(d) ≈ 0.80, so χ ≈ 0.68 × 0.80 × 1.0 ≈ 0.54 (experienced extraction lower than base due to partial agency and national-scale alternatives). State apparatus at global scope: d ≈ 0.15 produces f(d) ≈ -0.01, so χ ≈ 0.68 × (-0.01) × 1.2 ≈ -0.008 (state experiences negative extraction — the constraint benefits them, shifting globally coordinated benefit toward state). The χ scaling shows why the same constraint appears extractive to some and coordinative to others — the base extraction is identical, but directionality f(d) and scope σ(S) invert the experienced sign.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL CONSTRAINT: Indigenous self-determination exemplifies mandatrophy between collective rights and individual state sovereignty. The constraint is classified as snare (high ε=0.68, high suppression=0.72, chi ≥ 0.66 threshold met) from indigenous perspectives but as rope (coordination) from state perspectives. The mandatrophy is NOT 'which type is correct?' but 'how can two institutional actors experience the same constraint as opposite types?' The answer: directionality and scope invert the experienced extraction. States benefit from the constraint (negative chi experienced as coordination); indigenous nations bear costs (positive chi experienced as extraction). The constraint is mandatrophic precisely because it is Tangled Rope at the systemic level: it performs a genuine coordination function (allocates territorial governance to unified state structures) while extracting asymmetrically (from indigenous to state actors). However, from the perspective of the target (indigenous nations), this hybrid is experienced as pure snare because they have no agency in the coordination and no exit option. The theater rise (0.35 to 0.58) masks the mandatrophy: performative recognition creates appearance of scaffold (temporary constraint with sunset) while maintaining snare structure. The sunset has not fired because recognition without resource transfer preserves the constraint. Mandatrophy is resolved by mapping the multi-perspectival structure: the constraint is simultaneously snare (target view), tangled rope (system view), and piton (liberal regime view). No single type is false — each is a legitimate reading from a specific structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_versus_resource_transfer,
    'Does formal recognition of indigenous self-determination rights constitute genuine constraint relief or performative theater masking continued resource extraction?',
    'Longitudinal comparison of de facto autonomy and resource control pre- and post-recognition. Measurement of actual enforcement of recognized rights vs. frequency of state override. Tracking resource transfer timelines relative to recognition dates.',
    'If recognition drives real resource transfer: scaffold classification confirmed, sunset mechanism is real, constraint is weakening. If recognition is purely nominal: piton classification confirmed, theater substitutes for function, constraint persists through inertia.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognition_versus_resource_transfer, empirical, 'Whether formal recognition transfers actual resource control or remains performative').

omega_variable(
    identity_lock_collapse_conditions,
    'Under what conditions does the identity-locked experience (experiencing colonial frameworks as immutable) shift to constrained or mobile experience enabling exit?',
    'Historical case analysis of decolonization processes; identification of cognitive/educational shifts preceding institutional exit attempts (pan-indigenous identity movements, historical recovery, alternative educational systems). Measurement of identity-frame plasticity across generations post-recognition.',
    'If cognitive shifts precede institutional exit: identity-lock is mechanistically upstream and addresses it (historical recovery, alternative education) is leverage point. If institutional exit enables cognitive shift: structural barriers are upstream and removing legal/resource barriers is the lever.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_collapse_conditions, empirical, 'Causal relationship between identity-lock and institutional exit capacity').

omega_variable(
    alternative_governance_scalability,
    'Can indigenous governance systems (consensus-based, multi-generational, land-relationship-centered) scale to nation-state level without absorbing the hierarchical/extractive features that make colonial states functional at scale?',
    'Structural comparison of successful scaling cases (Iroquois Confederacy, Zapatista municipalities, Maori co-governance). Analysis of failure modes when indigenous systems interface with larger state structures. Identification of whether scalability requires abandoning core features (equality, consensus, land-first framing).',
    'If scalable without extraction: self-determination is institutionally feasible and constraint removal is a governance choice. If scalability requires hierarchical features: self-determination must remain sub-state or transform into something structurally different from the original indigenous model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_governance_scalability, conceptual, 'Whether indigenous governance systems scale without losing core features').

omega_variable(
    zero_sum_sovereignty_framework,
    'Is the zero-sum allocation of sovereign territory inherent to territorial governance, or is it a specific feature of the Westphalian system that could be replaced by overlapping/shared/nested sovereignty frameworks?',
    'Theoretical analysis of pre-Westphalian and non-Westphalian governance structures (tributary systems, suzerainty, commonwealths, federation models). Empirical analysis of attempted nested sovereignty (Quebec and Canada, Catalonia and Spain, Kurdish governance zones). Identification of whether alternatives collapse back to zero-sum when enforced.',
    'If zero-sum is structural: self-determination cannot coexist with state sovereignty and requires either state collapse or indigenous subordination. If zero-sum is contingent: alternative sovereignty frameworks are theoretically possible and constraint is institutional design problem rather than immutable feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(zero_sum_sovereignty_framework, conceptual, 'Whether zero-sum sovereignty is inherent to territorial governance or contingent on Westphalian architecture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indigenous_self_determination_global, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isd_tr_t0, indigenous_self_determination_global, theater_ratio, 0, 0.35).
narrative_ontology:measurement(isd_tr_t50, indigenous_self_determination_global, theater_ratio, 50, 0.5).
narrative_ontology:measurement(isd_tr_t100, indigenous_self_determination_global, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(isd_be_t0, indigenous_self_determination_global, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(isd_be_t50, indigenous_self_determination_global, base_extractiveness, 50, 0.78).
narrative_ontology:measurement(isd_be_t100, indigenous_self_determination_global, base_extractiveness, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indigenous_self_determination_global, enforcement_mechanism).
narrative_ontology:affects_constraint(indigenous_self_determination_global, land_rights_enforcement).
narrative_ontology:affects_constraint(indigenous_self_determination_global, resource_extraction_regimes).
narrative_ontology:affects_constraint(indigenous_self_determination_global, epistemic_legitimacy_frameworks).
narrative_ontology:affects_constraint(indigenous_self_determination_global, state_sovereignty_doctrine).

% DUAL FORMULATION NOTE:
% Indigenous self-determination decomposes into multiple structurally distinct constraints: territorial sovereignty claim (ε=0.72, snare), epistemic legitimacy (ε=0.55, tangled rope), resource rights (ε=0.78, snare), cultural autonomy (ε=0.48, tangled rope). This family is unified by the Westphalian sovereignty framework that makes all sub-constraints snares. The present story models the integrated global constraint; upstream stories model specific domains. All members linked through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indigenous_self_determination_global, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
