% ============================================================================
% CONSTRAINT STORY: israeli_settlement_policy_authority_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israeli_settlement_policy_authority_restriction, []).

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
 *   constraint_id: israeli_settlement_policy_authority_restriction
 *   human_readable: Israeli Settlement Policy Restricting Palestinian Authority
 *   domain: political/territorial_control
 *
 * SUMMARY:
 *   Israeli settlement policy in Area C of the West Bank creates a structural
 *   constraint that restricts Palestinian Authority administrative authority,
 *   resource access, and territorial control. Area C comprises approximately
 *   60% of West Bank territory and is designated for Israeli control under
 *   the Oslo Accords framework, but the framework itself has become degraded
 *   through unilateral Israeli expansion of settlements and military
 *   administration. The constraint exhibits characteristics of a pure
 *   extraction mechanism (snare) from the Palestinian perspective, while
 *   Israeli institutional actors frame it as a coordination and security
 *   mechanism (tangled rope). The constraint's theater ratio (0.58) reflects
 *   the gap between formal Oslo Accords language describing joint
 *   coordination and actual unilateral Israeli administrative and military
 *   control. Extractiveness has increased steadily from 0.42 (early 1990s,
 *   immediate post-Oslo period with genuine coordination mechanisms) to 0.68
 *   (current, with settlement expansion and military jurisdiction superseding
 *   negotiated arrangements). Suppression (0.72) reflects multiple
 *   overlapping mechanisms: military authority, administrative restrictions,
 *   resource access barriers, and legal frameworks that subordinate
 *   Palestinian civil authority. The constraint is active, enforced, and
 *   structurally entrenched.
 *
 * KEY AGENTS:
 *   - Palestinian Authority: Primary victim (powerless/trapped) — bears full cost of territorial exclusion and administrative subordination; cannot exit Area C restrictions
 *   - Palestinian Civil Population: Primary victim (powerless/trapped) — constrained settlement rights, resource access, economic development; subject to Israeli military jurisdiction in 60% of West Bank
 *   - Israeli Settlement Movement: Primary beneficiary (institutional/arbitrage) — drives settlement expansion; benefits from territorial control and legal frameworks enabling Jewish-only settlements
 *   - Israeli State Apparatus: Primary beneficiary (institutional/arbitrage) — maintains military administration of Area C; derives security justification and territorial control from restriction policy
 *   - Israeli Security Establishment: Secondary actor (organized/constrained) — claims coordination function (joint security operations) alongside extraction of administrative control; represents hybrid perspective
 *   - International Community and Advocacy Organizations: Observer-mediators (moderate/constrained) — can document violations but have limited enforcement capacity; constrained by diplomatic and geopolitical factors
 *   - Oslo Accords Framework: Institutional structure (institutional/constrained) — formally establishes joint coordination but is functionally degraded by unilateral Israeli expansion; represents piton perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israeli_settlement_policy_authority_restriction, 0.68).
domain_priors:suppression_score(israeli_settlement_policy_authority_restriction, 0.72).
domain_priors:theater_ratio(israeli_settlement_policy_authority_restriction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israeli_settlement_policy_authority_restriction, extractiveness, 0.68).
narrative_ontology:constraint_metric(israeli_settlement_policy_authority_restriction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(israeli_settlement_policy_authority_restriction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israeli_settlement_policy_authority_restriction, snare).
narrative_ontology:human_readable(israeli_settlement_policy_authority_restriction, "Israeli Settlement Policy Restricting Palestinian Authority").
narrative_ontology:topic_domain(israeli_settlement_policy_authority_restriction, "political/territorial_control").

domain_priors:requires_active_enforcement(israeli_settlement_policy_authority_restriction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israeli_settlement_policy_authority_restriction, israeli_settlement_movement).
narrative_ontology:constraint_beneficiary(israeli_settlement_policy_authority_restriction, israeli_state_apparatus).
narrative_ontology:constraint_victim(israeli_settlement_policy_authority_restriction, palestinian_authority).
narrative_ontology:constraint_victim(israeli_settlement_policy_authority_restriction, palestinian_civil_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN AUTHORITY & CIVIL POPULATION (SNARE) — Cannot exit Area C restrictions; bears full cost of territorial exclusion and administrative subordination. PA has no meaningful self-determination in 60% of West Bank territory. Extraction is severe: loss of resource access, administrative authority, settlement rights, and economic development capacity. d≈0.92, f(d)≈1.38, σ=1.1 → χ≈0.65.
constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ISRAELI SECURITY ESTABLISHMENT (TANGLED ROPE) — Constrains PA authority through security coordination requirements while also extracting territorial and administrative control. Claims coordination function (joint security, intelligence sharing) alongside asymmetric extraction (settlement expansion, military jurisdiction). Requires active enforcement via military administration and law enforcement. d≈0.58, f(d)≈0.82, σ=1.1 → χ≈0.40.
constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI SETTLEMENT MOVEMENT & STATE APPARATUS (ROPE) — Primary beneficiary. Experiences the constraint as coordination enabling settlement expansion and territorial consolidation. State apparatus uses legal classification system (Area C unilateral Israeli control) to justify policy. Movement sees restriction as protection mechanism for settlement rights. d≈0.08, f(d)≈-0.10, σ=1.1 → χ≈-0.06. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERNATIONAL COMMUNITY & ADVOCACY GROUPS (TANGLED ROPE) — Observer-advocates with constrained exit. Recognize both coordination (security concerns) and extraction (territorial restrictions). Can document violations but cannot enforce compliance. Limited leverage through sanctions or diplomatic pressure. d≈0.65, f(d)≈0.98, σ=1.1 → χ≈0.50.
constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OSLO ACCORDS FRAMEWORK (PITON) — Formally established Area C as joint Israeli-PA coordination zone, but the framework is degraded: Israeli unilateral control has superseded negotiated agreements. Theater_ratio=0.58 reflects performative invocation of the accords while actual implementation is extractive militarized administration. The accords persist through institutional inertia despite functional collapse.
constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From civilizational/global perspective, the structural properties (ε=0.68, suppression=0.72, mandatrophy=true) indicate pure extraction with coerced coordination framing. The constraint exhibits all hallmarks of a snare: high extraction, high suppression, active enforcement requirement, and strategic use of security language to naturalize territorial control. Mandatrophy resolved through recognition that 'security coordination' is secondary to territorial extraction.
constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israeli_settlement_policy_authority_restriction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israeli_settlement_policy_authority_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(israeli_settlement_policy_authority_restriction, TR),
    TR >= 0.70.

:- end_tests(israeli_settlement_policy_authority_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts substantial value from Palestinian Authority: loss of administrative authority in 60% of West Bank territory, inability to authorize settlements or development projects, subordination to Israeli military jurisdiction, and resource access barriers. The trajectory from 0.42 to 0.68 reflects settlement expansion accelerating beyond the Oslo Accords framework, with unilateral Israeli control increasingly replacing negotiated coordination. The current level is high enough to classify the constraint as a snare when viewed from Palestinian perspective, but remains below the 0.80+ levels of pure confiscatory extraction (some coordination language persists, some security cooperation continues). Suppression (0.72): High. Multiple overlapping mechanisms suppress Palestinian Authority alternatives: military authority and law enforcement (Israeli military reserves all coercive power in Area C), administrative restrictions (PA cannot authorize development or settlement without Israeli approval), legal frameworks (settlements are legally Israeli territory under Israeli law; Palestinians have no equivalent rights), and resource barriers (water, minerals, development land are allocated asymmetrically). The constraint cannot be escaped through ordinary administrative process. Theater ratio (0.58): Moderate-high. The Oslo Accords framework uses language of 'joint coordination' and 'transitional arrangement,' but actual implementation is unilateral Israeli military administration. The theater reflects the gap between formal agreements describing negotiated zones and actual practice of Israeli security control. Theater has increased from 0.35 (early 1990s, when some joint security operations genuinely occurred) to 0.58 (current, when 'coordination' is largely performative invocation of security language to justify unilateral control). Mandatrophy (true): The constraint resolves the mandatrophy by acknowledging that security coordination language (which would classify as Tangled Rope) is secondary to territorial extraction (which classifies as Snare). The primary function is extraction of territorial control and settlement rights; security coordination is the justification framing.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark perspectival gap between beneficiary and victim perspectives. The Israeli settlement movement and state apparatus experience the constraint as coordination and protection (Rope) — a mechanism enabling legitimate territorial claims and security requirements. The Palestinian Authority and civil population experience the constraint as extraction and subordination (Snare) — loss of administrative authority, territorial rights, and self-determination capacity. The Israeli security establishment occupies a hybrid position (Tangled Rope) — they coordinate security operations with the PA while simultaneously extracting administrative control. The international observer (analytical perspective) sees the constraint as a snare, recognizing that the 'security coordination' frame naturalizes what is structurally a pure extraction mechanism. The Oslo Accords framework itself (piton perspective) shows functional degradation — it was designed as a coordination mechanism but has become a legal justification for unilateral Israeli control, persisting through institutional inertia rather than operational function.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Authority & Civil Population: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction directionality. PA has no exit option; cannot unilaterally declare independence (international recognition blocked), cannot effectively resist (military asymmetry), cannot withdraw from constraint (territorial location fixed). Bears full cost of restriction. Israeli Settlement Movement & State Apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary directionality. Can walk away from constraint (withdraw settlements, restore joint coordination); instead maintains control through policy choice. Negative effective extraction indicates net gain from constraint. Israeli Security Establishment: Victim + constrained AND Beneficiary + arbitrage (hybrid) → d≈0.58, f(d)≈0.82. Constrained exit (security logic requires some level of territorial control and PA coordination) but also benefits from administrative control and intelligence access. Moderate extraction directionality reflects mixed position. International Community: Observer + analytical → d≈0.65, f(d)≈0.98. Moderate-high directionality; constrained exit (geopolitical factors, state sovereignty limits intervention) but also benefits from access to both Israeli and PA cooperation. Oslo Accords Framework: Institutional + constrained → d≈0.50, f(d)≈0.65. Symmetric position; equally violated by both Israeli unilateral expansion and Palestinian non-implementation of security cooperation clauses.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE WITH SECONDARY COORDINATION FRAMING: The constraint resolves the mandatrophy through careful examination of primary vs secondary functions. Israeli institutional actors claim this is a Tangled Rope: security coordination with extraction (settlement protection) as an asymmetric but necessary element of security architecture. The mandatrophy resolution recognizes that this framing inverts the causal hierarchy. The primary structural function (what drives the policy) is territorial extraction and settlement expansion; security coordination is the post-hoc justification and secondary mechanism. Evidence: (1) Settlement expansion continues regardless of security threat levels; (2) Coordination only occurs where it benefits Israeli interests; (3) PA capacity to provide security is extensive but remains subordinated to Israeli control; (4) Theater ratio (0.58) indicates performative coordination language. When security coordination is recognized as secondary, the classification shifts from Tangled Rope to Snare. The constraint is mandatrophy-resolved at ε=0.68 (extractiveness high enough to require resolution). Recommendation: classify as Snare from analytical perspective; acknowledge Tangled Rope framing from Israeli institutional perspective as theoretically possible but empirically secondary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_coordination_authenticity,
    'Is the claimed security coordination function (joint Israeli-PA security operations) a genuine structural feature or performative justification for extraction?',
    'Analysis of operational security arrangements: actual joint patrols, intelligence sharing, coordinated operations; comparison to unilateral Israeli military activities; assessment of threat levels before/after coordination claims',
    'If authentic coordination: constraint classifies as Tangled Rope from Israeli perspective. If performative: constraint classifies as pure Snare across all perspectives; mandatrophy is resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_coordination_authenticity, empirical, 'Whether security coordination is genuine or performative').

omega_variable(
    settlement_expansion_causation,
    'Does Area C administrative control directly enable settlement expansion, or would settlement patterns follow alternative causal pathways?',
    'Comparative historical analysis: settlement growth rates under different administrative arrangements; counterfactual modeling of settlement expansion without Area C restrictions; analysis of resource allocation and legal enforcement mechanisms',
    'If direct causation: extraction mechanism is clear (territorial control → settlement capacity). If indirect: extraction may be mediated by political/demographic factors; snare classification remains but extraction mechanism is more complex.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settlement_expansion_causation, empirical, 'Causal link between Area C control and settlement expansion').

omega_variable(
    pa_exit_capacity,
    'What exit options actually remain for the Palestinian Authority: unilateral state declaration, armed resistance, diplomatic withdrawal, or none?',
    'Assessment of PA capacity for each exit pathway; analysis of international recognition, military capacity, economic viability, and internal legitimacy; historical precedent analysis',
    'If meaningful exits exist: PA directionality shifts from trapped (d≈0.92) toward constrained or mobile. If exits are illusory: trapped classification confirmed; snare classification is certain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pa_exit_capacity, preference, 'PA''s actual exit options from the constraint').

omega_variable(
    suppression_mechanism_nature,
    'Is suppression (0.72) primarily military/coercive or administrative/structural (or both equally)?',
    'Decomposition of suppression: military enforcement incidents, administrative restrictions, resource barriers, legal jurisdiction restrictions; analysis of which mechanism is primary vs secondary',
    'If primarily military: snare classification is certain. If primarily administrative: classification may shift toward Tangled Rope if coordination elements increase. If equal: snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_nature, empirical, 'Primary suppression mechanism type').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israeli_settlement_policy_authority_restriction, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ispa_tr_t0, israeli_settlement_policy_authority_restriction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ispa_tr_t15, israeli_settlement_policy_authority_restriction, theater_ratio, 15, 0.48).
narrative_ontology:measurement(ispa_tr_t30, israeli_settlement_policy_authority_restriction, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(ispa_be_t0, israeli_settlement_policy_authority_restriction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ispa_be_t15, israeli_settlement_policy_authority_restriction, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(ispa_be_t30, israeli_settlement_policy_authority_restriction, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israeli_settlement_policy_authority_restriction, enforcement_mechanism).
narrative_ontology:affects_constraint(israeli_settlement_policy_authority_restriction, palestinian_state_formation_capacity).
narrative_ontology:affects_constraint(israeli_settlement_policy_authority_restriction, west_bank_water_rights_allocation).
narrative_ontology:affects_constraint(israeli_settlement_policy_authority_restriction, israeli_palestinian_trade_asymmetry).

% DUAL FORMULATION NOTE:
% The settlement policy constraint is downstream of broader Israeli territorial expansion policies but represents a distinct structural mechanism. The upstream constraint (territorial expansion logic) has different ε reflecting fundamental territorial claims; this constraint has ε=0.68 reflecting specifically the administrative restriction of PA authority in Area C. The constraint family includes related extraction mechanisms in water rights, economic dependency, and state formation capacity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israeli_settlement_policy_authority_restriction, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
