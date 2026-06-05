% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: Exogenous State Commitment Installation via Top-Down Authority Mandate
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'state_commitment_installation_mechanism': the
 *   exogenous_imposition_reading. It models how new commitments (language
 *   standards, legal categories, administrative frameworks, religious
 *   orthodoxy, educational curriculum, nationalist ideology) gain
 *   institutional legitimacy through top-down installation by authority
 *   holding the transformation mandate, rather than through grassroots
 *   adoption or negotiated hybrid processes. This reading is characterized by
 *   (a) state as primary beneficiary, (b) no grassroots advocacy or
 *   endogenous pressure preceding adoption, (c) abrupt adoption via decree or
 *   administrative mandate, (d) organized resistance at the base. The
 *   constraint exhibits genuine tangled rope structure: the state apparatus
 *   achieves real coordination function (unified legal code enables just
 *   administration, standardized language enables commerce, shared curriculum
 *   enables literacy), while simultaneously extracting from communities
 *   forced to abandon local practice and absorb the costs of social
 *   reorientation. The three measurements track the decay of active
 *   enforcement machinery over generational time: suppression requirement
 *   falls (communities internalize mandate or enforce it themselves),
 *   extractiveness gradually declines (utility of mandate becomes apparent,
 *   reducing perceived unfairness), and theater ratio rises (the enforcement
 *   ritual persists but with declining functional content). This pattern is
 *   diagnostic of the tension between the constraint's two sibling readings:
 *   endogenous_climb_reading (communities eventually adopt the mandate as
 *   their own, reversing its initial externality) and hybrid_cascade_reading
 *   (state gradually incorporates local input, moderating the initial
 *   imposition).
 *
 * KEY AGENTS:
 *   - State Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — captures coordination gains from unified legal framework, tax efficiency, military standardization, bureaucratic consistency
 *   - Legitimating Authority Institution: Secondary beneficiary (institutional/arbitrage) — Church, academy, or ideological authority expands reach and institutionalizes authority by sanctioning the mandate
 *   - Subject Communities: Primary victim (powerless/trapped) — bear the compulsory adoption cost, loss of local autonomy, disruption of inherited practice, schooling in externally-imposed frameworks
 *   - Local Autonomous Decision-Making: Structural victim (abstract) — the capacity for communities to set their own cultural commitments is foreclosed by the mandate mechanism
 *   - Indigenous Cultural Practice: Structural victim (abstract) — existing local frameworks are displaced, often delegitimized as backward or illegitimate
 *   - Intermediate Administrators: Mixed actor (moderate/constrained) — experience coordination benefit (unified system) but bear enforcement burden, reputational cost, and constraint from above and below
 *   - Analytical Observer: Neutral position (analytical/analytical) — risks naturalizing the exogenous mechanism as inevitable rather than recognizing it as one of three distinct possible pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.62).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "Exogenous State Commitment Installation via Top-Down Authority Mandate").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, 'ad9fbf9c-4141-4e8c-ba0a-5e260c1482c9').
narrative_ontology:cs_kernel_codification('ad9fbf9c-4141-4e8c-ba0a-5e260c1482c9', formalized).
narrative_ontology:cs_authority_grounding('ad9fbf9c-4141-4e8c-ba0a-5e260c1482c9', extraction).
narrative_ontology:cs_interpretation_layer_present('ad9fbf9c-4141-4e8c-ba0a-5e260c1482c9').
narrative_ontology:cs_reading_relation('ad9fbf9c-4141-4e8c-ba0a-5e260c1482c9', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad9fbf9c-4141-4e8c-ba0a-5e260c1482c9', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('ad9fbf9c-4141-4e8c-ba0a-5e260c1482c9', foundational, authority_mandate_prior_to_grassroots).
narrative_ontology:cs_axiom_status(authority_mandate_prior_to_grassroots, holdable).
narrative_ontology:cs_axiom_grounding('ad9fbf9c-4141-4e8c-ba0a-5e260c1482c9', authority_mandate_prior_to_grassroots, empirically_contingent).
narrative_ontology:cs_axiom('ad9fbf9c-4141-4e8c-ba0a-5e260c1482c9', foundational, local_autonomy_foreclosed_by_mandate).
narrative_ontology:cs_axiom_status(local_autonomy_foreclosed_by_mandate, holdable).
narrative_ontology:cs_axiom_grounding('ad9fbf9c-4141-4e8c-ba0a-5e260c1482c9', local_autonomy_foreclosed_by_mandate, deontological).
narrative_ontology:cs_reference_frame('ad9fbf9c-4141-4e8c-ba0a-5e260c1482c9', top_down_administrative_imposition).
narrative_ontology:cs_created_at('ad9fbf9c-4141-4e8c-ba0a-5e260c1482c9', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, legitimating_authority_institution).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, local_autonomous_decision_making).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, indigenous_cultural_practice).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, grassroots_commitment_formation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT COMMUNITIES (SNARE) — Communities face compulsory adoption of state-mandated commitments (language, administrative categories, legal frameworks) with no meaningful exit. The constraint appears as fate — embedded in schooling, legal procedure, administrative obligation. No alternative citizenship available; mobility options precluded by sovereign boundary. Maximum suppression; minimum coordination benefit.
constraint_indexing:constraint_classification(state_commitment_installation_mechanism__exogenous_imposition_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERMEDIATE ADMINISTRATORS (TANGLED ROPE) — Regional officials, local magistrates, cultural brokers experience genuine coordination function (the mandate creates shared administrative language, unified legal procedure) alongside asymmetric extraction (they must enforce compliance, bear reputational cost of resistance management, constrained by both upward authority and downward community expectations). Exit requires abandoning administrative position; costs are substantial but not absolute.
constraint_indexing:constraint_classification(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE ADMINISTRATIVE APPARATUS (ROPE) — The state apparatus benefits directly from mandate installation: unified legal categories enable tax collection, conscription, standardized justice, bureaucratic efficiency. The apparatus experiences the constraint as pure coordination gain — standardization solves collective action problems of territorial governance. Arbitrage exit available (alternative administrative schemes possible but suboptimal). Net beneficiary; low experienced extraction.
constraint_indexing:constraint_classification(state_commitment_installation_mechanism__exogenous_imposition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGITIMATING AUTHORITY INSTITUTION (ROPE) — Church, national academy, scientific establishment, or ideological authority that grants the mandate its legitimacy (e.g., religious sanction for legal code, scientific authority for educational curriculum, nationalist ideology for language standardization). The institution benefits from mandate adoption: expanded reach, institutionalized authority, claim to civilizational scope. Experiences constraint as coordination. Arbitrage available (can withdraw endorsement, face loss of influence but retain institutional existence).
constraint_indexing:constraint_classification(state_commitment_installation_mechanism__exogenous_imposition_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEGRADED ENFORCEMENT INFRASTRUCTURE (PITON) — Over generational time, enforcement machinery that once actively drove mandate adoption becomes theatrical. Schools teach the mandated language but students revert to vernacular; courts apply the legal code but supplement with customary practice; administrative categories persist in forms but lose functional meaning. Theater ratio high: the enforcement ritual persists through institutional inertia even as effectiveness decays. The infrastructure is locked into the mandate path (exit requires systemic redesign); suppression requirement has fallen below what would be needed to sustain genuine compliance.
constraint_indexing:constraint_classification(state_commitment_installation_mechanism__exogenous_imposition_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From civilizational scope, the exogenous installation of state commitments appears as an immutable law: all territorial states require some form of shared administrative framework; local autonomy is incompatible with sovereignty; cultural unification is an inherent feature of statecraft. This perspective naturalizes the exogenous imposition as inevitable structure rather than contingent institutional choice. However, the presence of identified beneficiaries and competing reading positions (endogenous_climb_reading, hybrid_cascade_reading) signals a false summit: the constraint's naturalness masks a distributional choice about who controls commitment formation.
constraint_indexing:constraint_classification(state_commitment_installation_mechanism__exogenous_imposition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_commitment_installation_mechanism__exogenous_imposition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_commitment_installation_mechanism__exogenous_imposition_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, TR),
    TR >= 0.70.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The state apparatus extracts significant benefit from the mandate (unified administration, tax efficiency, simplified governance), and subject communities bear substantial cost (loss of autonomy, compulsory reorientation). However, the extraction is not maximal (0.72+) because the mandate often does provide genuine coordination benefits that communities eventually recognize — commerce does become easier with shared language, legal predictability does emerge from standardized code, literacy does open economic opportunities. The tangled rope classification reflects that this is not pure predatory extraction but rather a coordination mechanism with radically asymmetric cost distribution. Suppression (0.68): High. Communities have no institutional channel to resist the mandate; legal status as subjects precludes negotiation; alternative frameworks are delegitimized as backward; enforcement is backed by state violence or credible threat thereof. However, suppression is not total (0.85+) because communities often find workarounds (linguistic code-switching, customary practices alongside legal codes, reinterpretation of mandated categories through local meaning). Theater ratio (0.55): Moderate. The initial enforcement of a mandate is functional — the state genuinely polices compliance through schooling, legal procedure, administrative requirement. But this theater gradually increases over the interval as communities internalize the mandate, enforcement becomes routine, and the apparatus relies increasingly on normalization rather than active coercion (piton perspective at t=50).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is structural and reflects the reading's core claim: the exogenous imposition mechanism creates irreconcilable experiences of the same constraint. Subject communities (powerless/trapped) experience it as a snare — pure extraction with no exit. The state (institutional/arbitrage) experiences it as a rope — coordination gain with minimal burden. Intermediate administrators experience it as tangled rope — real coordination function but asymmetric cost they must enforce. Over generational time, this gap can narrow if communities internalize the mandate (endogenous_climb reading becomes salient) or if the state incorporates local input (hybrid_cascade reading becomes salient). But at the exogenous_imposition moment, the gap is maximal. The analytical observer at civilizational scope risks seeing the mechanism as a natural law (mountain) — 'all states require unified frameworks; local autonomy is incompatible with sovereignty' — but this naturalizes what is actually a distributional choice about who controls commitment formation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position. Subject communities (powerless, trapped) face d ≈ 0.95 — they absorb full cost of the mandate, have no exit, experience maximum extraction severity. State apparatus (institutional, arbitrage) faces d ≈ 0.05 — they capture benefits, have arbitrage options (alternative administrative schemes), experience negative effective extraction (coordination gain exceeds any cost). Intermediate administrators (moderate, constrained) face d ≈ 0.65 — they bear enforcement burden and reputational cost but also gain coordination benefit; their exit costs are high (career/position loss) but not absolute (constrained rather than trapped). The engine computes effective extractiveness (chi) from these directionality values and the scope modifier: at national scope, community agents experience χ ≈ 0.88 (high); state apparatus experiences χ ≈ -0.15 (institutional benefit); intermediate administrators experience χ ≈ 0.45 (moderate burden).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy collapse by anchoring its tangled rope classification in structural data: (1) genuine coordination function exists — the mandate does enable unified governance, standardized law, shared language, efficient administration; (2) asymmetric extraction also exists — communities bear the cost of adopting external frameworks, losing local autonomy, absorbing the transition burden; (3) active enforcement is required — without sustained coercion (backed by law, schooling, administrative procedure), communities would not comply. All three tangled rope gates are met. The perspectival gap (snare from community view, rope from state view, mountain from civilizational view) does NOT constitute mandatrophy — it reflects that different structural positions generate different experienced classifications. The analytical observer's temptation toward the mountain reading is precisely what mandatrophy resolution should correct: recognizing that the 'naturalness' of state commitment mechanisms is a false summit, not a genuine natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandatory_vs_contingent_mandate,
    'Is the exogenous installation mechanism a necessary feature of state formation or a contingent choice by particular regimes?',
    'Comparative historical analysis: do states exhibit alternative commitment-installation mechanisms (endogenous climb, hybrid cascade, negotiated adoption) that achieve equivalent administrative functionality without top-down imposition? Can functional equivalence be established empirically?',
    'If necessary: constraint is closer to mountain (immutable structure). If contingent: constraint is snare/tangled_rope with extractive overlay; alternative pathways are foreclosed only by regime choice, not structural inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatory_vs_contingent_mandate, empirical, 'Whether exogenous installation is necessary or contingent for state functionality').

omega_variable(
    local_resistance_endogenization,
    'Does sustained local resistance to exogenous mandates generate pressure toward endogenous adoption mechanisms (the hybrid or climb paths)?',
    'Historical sequence analysis: states that face organized resistance to mandate imposition. Do they (a) intensify enforcement, (b) gradually introduce localization / negotiation, or (c) abandon mandate entirely? Track temporal patterns of enforcement expenditure vs. adoption metrics.',
    'If resistance drives endogenization: the reading''s suppression component is unstable; the constraint transitions toward hybrid_cascade over time. If enforcement maintains exogeneity: suppression remains stable and the reading''s classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(local_resistance_endogenization, empirical, 'Whether resistance pressure drives shift toward endogenous mechanisms').

omega_variable(
    legitimacy_source_substitution,
    'Can a state mandate gain legitimacy from grassroots endorsement (endogenous climb pathway) and thus be re-experienced by communities as self-chosen rather than imposed, while the institutional structure remains unchanged?',
    'Ethnographic and historical analysis: does mandate adoption shift from experienced-as-external to experienced-as-internal when communities internalize the mandate''s utility or ideological frame? Can the same institutional structure be classified as both exogenous imposition (snare from community perspective at t0) and endogenous climb (rope from community perspective at t1)?',
    'If substitution possible: the constraint classification is observer-relative and temporally indexed — not intrinsic to the mechanism but to the community''s relationship to it. The reading''s perspectival structure would shift fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_substitution, conceptual, 'Whether legitimacy source shift changes community experience of the same mechanism').

omega_variable(
    contested_kernel_reading_coherence,
    'Do the three readings (exogenous_imposition, endogenous_climb, hybrid_cascade) represent genuinely distinct structural mechanisms or three points on a continuum that the frame artificially separates?',
    'Theoretical analysis: can each reading be formalized as a constraint with its own ε, suppression, and base properties? Or do they differ only in degree (amount of top-down vs. bottom-up pressure) rather than in kind (structural mechanism)?',
    'If structurally distinct: each reading is a separate constraint story; the kernel is legitimately contested. If continuous: the kernel decomposition is artificial; a single constraint with a spectrum variable would be more accurate. Current authoring assumes structural distinctness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contested_kernel_reading_coherence, conceptual, 'Whether three readings represent distinct mechanisms or artificial points on a continuum').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_commit_exog_theater_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(state_commit_exog_theater_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(state_commit_exog_theater_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(state_commit_exog_extract_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(state_commit_exog_extract_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 25, 0.64).
narrative_ontology:measurement(state_commit_exog_extract_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(state_commit_exog_suppression_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(state_commit_exog_suppression_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(state_commit_exog_suppression_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, linguistic_standardization_via_state_mandate).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, legal_category_imposition_and_resistance).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, curriculum_nationalization_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is part of the state_commitment_installation_mechanism constraint family. The family has three structurally distinct constraint stories, one for each reading of the contested kernel. All three share the same domain (historical sociology / state formation) but have different ε values, beneficiary/victim structures, and temporal dynamics. The exogenous_imposition_reading emphasizes top-down decree and resistance. The endogenous_climb_reading emphasizes grassroots adoption and internalization. The hybrid_cascade_reading emphasizes negotiated incorporation of local input. These are not perspectives on one constraint but three separate constraints linked by the kernel they contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
