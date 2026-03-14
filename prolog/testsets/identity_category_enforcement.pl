% ============================================================================
% CONSTRAINT STORY: identity_category_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_identity_category_enforcement, []).

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
 *   constraint_id: identity_category_enforcement
 *   human_readable: Identity Category Enforcement
 *   domain: social/institutional
 *
 * SUMMARY:
 *   Identity category enforcement operates as a dual-function constraint at
 *   the intersection of coordination and control. All formal institutions
 *   require some method of categorization to organize populations, allocate
 *   resources, and enable communication — identity categories serve this
 *   coordination function. Simultaneously, the act of categorization creates
 *   boundaries, controls membership, enables differential treatment, and
 *   suppresses non-conformity. This constraint is structurally hybrid: it
 *   solves genuine coordination problems while enabling asymmetric extraction
 *   from those who transgress category boundaries. The extractiveness (0.58)
 *   reflects that the extraction is real and measurable, but not absolute —
 *   significant coordination function persists alongside the extraction. The
 *   suppression (0.65) is substantial because category enforcement operates
 *   through multiple mechanisms: legal sanctions (documentation requirements,
 *   civil status restrictions), economic barriers (employment tied to
 *   category membership), social mechanisms (exclusion, shame), and
 *   internalized control (agents internalize the category boundaries as
 *   natural/inevitable). The theater ratio (0.48) is moderate because some
 *   identity categorization is genuinely functional (enables targeted service
 *   delivery, tracks demographic inequities), but much is performative
 *   (categorical complexity that serves administrative convenience rather
 *   than coordination need). The trajectory shows accumulation:
 *   extractiveness rises from 0.35 to 0.58 over the interval, reflecting
 *   historical elaboration of categorical hierarchies and enforcement
 *   infrastructure. Theater ratio rises from 0.32 to 0.48, reflecting
 *   increasing performativity as categorization systems expand beyond
 *   functional necessity.
 *
 * KEY AGENTS:
 *   - Category Transgressor: Primary victim (powerless/trapped) — bears full cost of boundary violation; lacks exit options and bears maximum extraction
 *   - Ambiguous Category Member: Secondary victim (moderate/constrained) — structurally mobile but at high cost; benefits from some coordination functions while bearing extraction costs
 *   - Organized Community: Secondary actor (organized/mobile) — has some collective agency and exit capacity; experiences both coordination benefits (mutual aid) and extraction (boundary enforcement)
 *   - Category Administrator: Primary beneficiary (institutional/arbitrage) — maintains authority over category definitions; benefits from policy discretion and administrative power
 *   - Classification System Institution: Institutional actor (institutional/arbitrage) — perpetuates categorization through inertia; sees its own process as degraded but lacks exit mechanism
 *   - Identity-Locked Enforcer: Secondary beneficiary/victim (moderate/identity_locked) — professionally identifies with enforcement role; is structurally mobile but cognitively captured; benefits from enforcer status while trapped by identity fusion
 *   - Analytical Observer: Structural analyst (analytical/analytical) — sees full hybrid structure of coordination plus extraction; recognizes both functions as real and irreducible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(identity_category_enforcement, 0.58).
domain_priors:suppression_score(identity_category_enforcement, 0.65).
domain_priors:theater_ratio(identity_category_enforcement, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(identity_category_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(identity_category_enforcement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(identity_category_enforcement, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(identity_category_enforcement, tangled_rope).
narrative_ontology:human_readable(identity_category_enforcement, "Identity Category Enforcement").
narrative_ontology:topic_domain(identity_category_enforcement, "social/institutional").

domain_priors:requires_active_enforcement(identity_category_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(identity_category_enforcement, category_administrators).
narrative_ontology:constraint_beneficiary(identity_category_enforcement, institutional_gatekeepers).
narrative_ontology:constraint_victim(identity_category_enforcement, category_boundary_transgressors).
narrative_ontology:constraint_victim(identity_category_enforcement, non_conforming_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CATEGORY TRANSGRESSOR (SNARE) — Agent whose identity or behavior violates established category boundaries faces material barriers to exit: loss of employment, social exclusion, legal penalty, family rupture. The suppression mechanism is structural (economic dependency, legal prohibition, geographic isolation) with internalized components (shame, identity confusion). Cannot exit without bearing extreme costs. Maximum experienced extraction.
constraint_indexing:constraint_classification(identity_category_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AMBIGUOUS CATEGORY MEMBER (TANGLED ROPE) — Agent on the boundary of an identity category (e.g., mixed-race, non-binary, immigrant-origin) faces both genuine coordination benefits (access to community resources, recognized status, institutional services keyed to the category) AND asymmetric extraction (constant visibility monitoring, compliance demands, differential treatment). Constrained exit: could cross boundary at high social/economic cost. Mixed experience reflects both functions.
constraint_indexing:constraint_classification(identity_category_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CATEGORY ADMINISTRATOR (ROPE) — Institutional actor (government census bureau, professional licensing board, identity documentation system) sees identity categorization as a coordination mechanism: enables resource allocation, service delivery, demographic tracking, and institutional communication. Benefits from arbitrage exit (can redefine categories, has policy discretion). Experiences the constraint as functional coordination with minimal extraction cost.
constraint_indexing:constraint_classification(identity_category_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED COMMUNITY (TANGLED ROPE) — Agents organized around a marginalized identity category (e.g., LGBTQ+ community, disability rights advocates, immigrant networks) experience both genuine coordination benefits (mutual aid, collective power, shared identity) AND extraction through category enforcement that constrains community members. Mobile exit: can relocate, build alternative institutions. Mixed experience with agency.
constraint_indexing:constraint_classification(identity_category_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: DEGRADED CLASSIFICATION SYSTEM (PITON) — Formal identity categorization systems (census, legal documents, institutional records) persist through institutional inertia despite acknowledged dysfunction. Theater ratio is moderate-high (0.48) because the system performs categorical work while also generating classification failures, errors, and documented harms. The institutions maintaining the system recognize its limitations but lack coordination mechanisms to migrate away. Piton classification reflects high theater relative to actual functional coordination.
constraint_indexing:constraint_classification(identity_category_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: IDENTITY-LOCKED ENFORCER (SNARE) — Agent who has internalized the categorization system and identifies professionally/personally with category enforcement (e.g., border official, licensing examiner, institutional gatekeeper) is structurally mobile but identity-fused. Could exit the enforcement role at material cost (career change, identity reconstruction) but perceives the category boundaries as natural/necessary because their professional identity is constituted through enforcing them. The identity lock prevents recognition of alternatives.
constraint_indexing:constraint_classification(identity_category_enforcement, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, identity categorization systems serve both genuine coordination functions (enabling resource allocation, institutional communication, collective recognition) AND enforce asymmetric extraction (controlling movement, differentially allocating rights, enabling discrimination). The constraint is analytically hybrid: neither pure coordination nor pure extraction, but both, always, with the proportions and mechanisms contingent on specific category systems and historical moments.
constraint_indexing:constraint_classification(identity_category_enforcement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(identity_category_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(identity_category_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(identity_category_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(identity_category_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(identity_category_enforcement, TR),
    TR >= 0.70.

:- end_tests(identity_category_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts from transgressors and non-conformists through legal barriers (documentation requirements, civil status restrictions), economic mechanisms (employment discrimination, resource allocation), social penalties (exclusion), and internalized control. The extraction is substantial but not maximal because (a) coordination functions are genuine and necessary, (b) some agents have partial exit options (constrained mobility), and (c) organized communities can provide some counter-power. A snare would require extractiveness > 0.66 with minimal coordination; this constraint's coordination function is too significant for that threshold. Suppression (0.65): The constraint operates through multiple reinforcing mechanisms. Structural suppression includes legal barriers (documentation requirements), economic dependency (employment tied to category), geographic constraints (movement restrictions), and lack of alternative institutions. Internalized suppression includes shame, identity confusion, cognitive closure that makes alternatives unthinkable. The high suppression reflects that escape requires overcoming both material and psychological barriers. Theater (0.48): Identity categorization systems perform real work (resource allocation, institutional communication) but also generate significant performative overhead. Census categories expand to serve administrative convenience rather than coordination necessity; licensing categories create classification failures; institutional records maintain outdated or discriminatory categories. The moderate theater reflects a system that is functional but increasingly dysfunctional at scale.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival disagreement. The transgressor sees pure extraction (Snare) because suppression is absolute from their position. The category administrator sees pure coordination (Rope) because the system functions to enable their institutional communication. The organized community sees mixed coordination and extraction (Tangled Rope) with some agency. The identity-locked enforcer also sees Snare but from the extraction mechanism side — they see themselves maintaining necessary boundaries that non-conformists threaten. The analytical observer sees the full Tangled Rope structure: genuine coordination functions paired with asymmetric extraction, with both being real. The perspectival gap reveals that the 'naturalness' of identity categories is a perspectival artifact — the category administrator experiences them as natural because they benefit from the coordination function; the transgressor experiences them as coercive because they bear the extraction cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position within the category system. Transgressors have d ≈ 0.95 (full target): they bear extraction with no beneficiary position; no exit. Ambiguous members have d ≈ 0.70 (partial target): they bear extraction but also access some coordination benefits; high exit cost but not impossible. Organized communities have d ≈ 0.55 (mixed): genuine coordination function (mutual aid) plus extraction from category enforcement; mobile exit available. Category administrators have d ≈ 0.10 (beneficiaries): they control categorization and benefit from policy discretion; arbitrage exit. Identity-locked enforcers have d ≈ 0.35 (mixed but identity-fused): they benefit from enforcer status but are trapped by professional identity; would face psychological displacement if they left, even though structural barriers are minimal. The analytical observer has d ≈ 0.72 (analytical position): sees full structure without personal stake; derives d from the constraint's structural opacity, not from personal extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for identity category enforcement is resolved by recognizing that the constraint is analytically tangled: it solves genuine coordination problems (resource allocation, institutional communication) while simultaneously enabling extraction (controlling movement, differential rights, suppression). Neither the snare classification (pure extraction) nor the rope classification (pure coordination) is correct. The tangled rope classification correctly identifies both functions as present and irreducible. The perspectival disagreement (snare from transgressor's view, rope from administrator's view) reveals the asymmetry: agents who benefit from the coordination function and control the categorization system experience it as pure coordination; agents who transgress boundaries experience pure extraction. The truth is that both perspectives are seeing real features — the constraint genuinely coordinates AND genuinely extracts. The mandatrophy resolves by recognizing this as a hybrid type, not by choosing one function over the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_enforcement_boundary,
    'How much of the measured extractiveness is necessary coordination cost versus removable institutional rent-seeking?',
    'Comparative analysis of minimal category systems (e.g., basic demographic tracking) versus elaborate classification hierarchies; measurement of institutional overhead that serves only enforcement rather than coordination',
    'If coordination cost is high: reclassify as lower extractiveness (Rope threshold). If enforcement overhead is high: confirms Tangled Rope or Snare classification with higher chi.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_enforcement_boundary, empirical, 'Necessary coordination cost versus institutional overhead in identity categorization').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is suppression primarily structural (legal barriers, economic dependency, geographic isolation) or internalized (shame, identity confusion, cognitive closure)?',
    'Post-exit trajectory analysis: if suppression persists after escape from structural barriers, it is internalized. Cross-national comparison of suppression levels with identical category boundaries in different legal/economic contexts.',
    'If primarily structural: exit becomes possible with material support (legal reform, economic alternatives). If primarily internalized: exit requires identity reconstruction; suppression carries beyond escape from system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    alternative_coordination_viability,
    'Can identity-relevant coordination (resource allocation, institutional communication, collective recognition) function without formal identity categorization?',
    'Case studies of institutions that operate with voluntary/self-identification versus mandatory categorization; measurement of coordination loss versus suppression reduction in decategorized systems',
    'If viable alternatives exist: extractiveness can be reduced substantially (Rope classification becomes plausible). If categorization is essential: supports Tangled Rope as structural minimum.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_viability, conceptual, 'Whether institutional coordination requires formal identity categorization').

omega_variable(
    identity_lock_reversibility,
    'For identity-locked enforcers, is the identity fusion with the categorization system reversible through institutional reform or requires professional/personal reconstruction?',
    'Institutional history of category system reforms and enforcement agent retraining; individual narratives of enforcers who migrated to non-enforcement roles',
    'If reversible: identity-locked perspective can shift to mobile through structural change. If locked: enforcer class is systemically dependent on category maintenance regardless of reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Reversibility of identity fusion in category enforcement roles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(identity_category_enforcement, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iden_tr_t0, identity_category_enforcement, theater_ratio, 0, 0.32).
narrative_ontology:measurement(iden_tr_t25, identity_category_enforcement, theater_ratio, 25, 0.4).
narrative_ontology:measurement(iden_tr_t50, identity_category_enforcement, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(iden_be_t0, identity_category_enforcement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(iden_be_t25, identity_category_enforcement, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(iden_be_t50, identity_category_enforcement, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(identity_category_enforcement, identity_coordination).
narrative_ontology:boltzmann_floor_override(identity_category_enforcement, 0.12).
narrative_ontology:affects_constraint(identity_category_enforcement, differential_citizenship_status).
narrative_ontology:affects_constraint(identity_category_enforcement, professional_licensing_gatekeeping).
narrative_ontology:affects_constraint(identity_category_enforcement, medical_category_assignment).

% DUAL FORMULATION NOTE:
% Identity category enforcement decomposes into domain-specific constraints: citizenship categorization, professional identity verification, medical classification systems. Each has different ε values reflecting domain-specific enforcement intensity and coordination necessity. Citizenship has high extractiveness (0.65+, Snare); professional licensing has moderate extractiveness (0.50-0.60, Tangled Rope); medical categorization has lower extractiveness (0.35-0.45, Rope or lower Tangled Rope) due to genuine health outcomes coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(identity_category_enforcement, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
