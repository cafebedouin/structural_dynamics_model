% ============================================================================
% CONSTRAINT STORY: legacy_identity_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legacy_identity_lock_in, []).

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
 *   constraint_id: legacy_identity_lock_in
 *   human_readable: The Inflexible Substrate of Upbringing and Boons
 *   domain: psychological/metaphysical/socio_political
 *
 * SUMMARY:
 *   Chitra exists within a doubly-determined identity: locked by a divine
 *   boon given to her grandfather promising male succession, and then by her
 *   father's love-inflected upbringing of her as a male heir. This constraint
 *   exemplifies how metaphysical, familial, and institutional determinisms
 *   fuse into a unified lock-in that appears immutable yet functions as pure
 *   extraction from the perspective of the victim. The constraint's base
 *   extractiveness (0.58) reflects that identity suppression and gender
 *   conformity are being extracted from Chitra as the price of institutional
 *   legitimacy and paternal approval. Her suppression is high (0.68) because
 *   alternatives appear unavailable: rejecting the male heir identity means
 *   violating the divine boon (metaphysical sanction), betraying her father's
 *   love (emotional/relational sanction), and destabilizing the succession
 *   (institutional sanction). The theater ratio (0.55) indicates moderate
 *   performative content — much of her identity performance is genuine (she
 *   has integrated it), but ritual aspects (ceremonial male roles, succession
 *   performance) have become increasingly performative as she matures and the
 *   gap between assigned identity and potential self-determination widens.
 *   From the analytical observer's perspective, the constraint risks
 *   appearing as a Mountain (immutable metaphysical determinism), but the
 *   high extractiveness score and the measured increase in theater ratio over
 *   the biographical interval reveal this as a false summit — the
 *   'unchangeable divine/familial substrate' naturalizes what is actually an
 *   imposed institutional arrangement backed by social enforcement,
 *   institutional inertia, and the emotional leverage of paternal love.
 *
 * KEY AGENTS:
 *   - Chitra: Primary victim (powerless/trapped) — forged identity cannot exit without violating boon, betraying father, or destabilizing succession
 *   - Royal Father: Primary beneficiary and institutional actor (institutional/arbitrage) — solves succession crisis, fulfills boon obligation, preserves dynasty; enforces constraint through love and institutional authority
 *   - Divine Boon (as institutional/metaphysical force): Meta-beneficiary (institutional/arbitrage) — the boon's fulfillment depends on Chitra's male heir identity; maintains metaphysical legitimacy of the succession
 *   - Patriarchal Succession System: Systemic beneficiary (moderate/constrained) — requires clear heir designation and gender-conforming performance; maintains institutional continuity
 *   - Alternative Identity Framework: Potential exit path (organized/mobile) — if institutional reform permits gender-flexible succession, sunset becomes possible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legacy_identity_lock_in, 0.58).
domain_priors:suppression_score(legacy_identity_lock_in, 0.68).
domain_priors:theater_ratio(legacy_identity_lock_in, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legacy_identity_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(legacy_identity_lock_in, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legacy_identity_lock_in, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legacy_identity_lock_in, snare).
narrative_ontology:human_readable(legacy_identity_lock_in, "The Inflexible Substrate of Upbringing and Boons").
narrative_ontology:topic_domain(legacy_identity_lock_in, "psychological/metaphysical/socio_political").

domain_priors:requires_active_enforcement(legacy_identity_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legacy_identity_lock_in, patriarchal_succession_system).
narrative_ontology:constraint_beneficiary(legacy_identity_lock_in, royal_father_institutional_continuity).
narrative_ontology:constraint_victim(legacy_identity_lock_in, chitra_personal_agency).
narrative_ontology:constraint_victim(legacy_identity_lock_in, chitra_gender_self_determination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHITRA (SNARE) — Trapped within dual constraints: the divine boon binding her grandfather and the paternal upbringing constituting her as male heir. Cannot exit gender/identity framework without violating sacred obligation or rejecting father's foundational love. Lacks alternative identity formation. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(legacy_identity_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ROYAL FATHER (ROPE) — Experiences constraint as coordination mechanism: upbringing of Chitra as male heir solves succession crisis and honors the divine boon. Has institutional exit via arbitrage (succession legitimacy, dynastic continuity). Enforcement is active but appears consensual from his standpoint because it aligns with institutional necessity. d≈0.08, f(d)≈-0.11, σ=0.9 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(legacy_identity_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: PATRIARCHAL SUCCESSION SYSTEM (TANGLED ROPE) — Provides coordination function (clear line of succession, boon fulfillment, dynastic legitimacy) while simultaneously extracting gender conformity and identity suppression from those designated as heirs. Benefits from enforcement but also constrained by the boon's terms. Active enforcement required (paternal training, social reinforcement, identity management). d≈0.35, f(d)≈0.32, σ=0.9 → χ≈0.16.
constraint_indexing:constraint_classification(legacy_identity_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / METAPHYSICAL VIEW (MOUNTAIN) — From a metaphysical standpoint, the divine boon creates an unchangeable substrate: Chitra's identity is fundamentally constituted by both the blessing and the upbringing, fused at the deepest level of her being. From this view, there is no exit because the constraint is not external — it IS her ontological foundation. accessibility_collapse≈0.88, resistance≈0.12, emerges_naturally=true. However, the high suppression (0.68) and behavioral extractiveness (0.58) suggest this is a false summit — the 'immutable metaphysical' framing naturalizes what is actually an imposed institutional arrangement.
constraint_indexing:constraint_classification(legacy_identity_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: INSTITUTIONAL RITUAL ENFORCEMENT (PITON) — The day-to-day performative maintenance of Chitra's male identity (court ceremonies, succession protocols, ceremonial roles) has largely become theater: the original functional necessity (ensuring succession and fulfilling the boon) persists as ritualized performance rather than genuine coordination. Theater ratio ≈0.55 suggests significant but not dominant performative content. The mechanism persists through institutional inertia and because alternatives (acknowledging the boon's flexibility, permitting gender self-determination) have not been institutionally established. d≈0.42, f(d)≈0.42, σ=0.9 → χ≈0.21.
constraint_indexing:constraint_classification(legacy_identity_lock_in, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ALTERNATIVE IDENTITY FRAMEWORK (SCAFFOLD) — If the system evolves to permit Chitra to fulfill the boon through non-gender-conforming identity (e.g., reinterpreting 'male heir' as 'successor regardless of gender', or recognizing that the boon's true function is dynastic legitimacy, not gendered performance), the constraint could sunset. This would maintain coordination (succession is clear) while removing suppression (identity conformity is no longer required). Current status: aspirational. If realized, has_sunset_clause=true, extractiveness would drop to ~0.15, theater would remain low. d≈0.45, f(d)≈0.48, σ=0.9 → χ≈0.24. Sunset timeline: generational (20-30 years if institutional reform begins).
constraint_indexing:constraint_classification(legacy_identity_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legacy_identity_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legacy_identity_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legacy_identity_lock_in, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legacy_identity_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legacy_identity_lock_in, TR),
    TR >= 0.70.

:- end_tests(legacy_identity_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Chitra's identity is being extracted as a resource to maintain succession legitimacy and fulfill the boon. The extraction is not as severe as pure labor/resource exploitation (hence not 0.75+), because the identity has been integrated into her self-conception; she is not entirely conscious of extraction as such. However, the gap between her potential gender self-determination and her constrained male heir identity represents genuine suppression. Measurement shows increasing extractiveness over the biographical interval (0.30→0.58) as the psychological cost of identity conformity accumulates with maturity. Suppression (0.68): Moderate-high. Multiple barriers prevent exit: (1) metaphysical sanction (divine boon), (2) emotional sanction (father's love embedded in the constraint), (3) institutional sanction (succession is institutionalized), (4) psychological fusion (identity has been integrated). However, suppression is not total (0.85+) because Chitra retains some agency within the male heir role and some level of institutional standing. Theater ratio (0.55): Moderate. Early in the interval (t=0, theater=0.35), the male heir identity is largely functional — it genuinely solves the succession problem and satisfies the boon. Over time (t=20, theater=0.55), performative content increases as the original functional necessity persists but alternatives become imaginable and the cost becomes visible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a profound perspectival gap between the father's experience and Chitra's. The father sees Rope — a coordination mechanism (clear succession, boon fulfillment, dynastic legitimacy) achieved through upbringing and love. Chitra sees Snare — a trap in which her self-determination is extracted to maintain institutional structures. The analytical observer risks seeing Mountain — the boon and upbringing appear as immutable metaphysical determinism, unchangeable laws of her being. But this is a false summit: the high extractiveness score (0.58) and measured increase in theater ratio over the interval reveal that the 'unchangeable' framing is actually an institutional arrangement backed by enforced gender conformity, emotional leverage, and the lack of institutional precedent for alternatives. The scaffold perspective (alternative identity framework) is currently aspirational but represents a genuine structural possibility: if the boon could be reinterpreted or the institutional framework reformed to permit gender-flexible succession, the constraint would sunset with extractiveness dropping to ~0.15. This perspectival gap — between imposed immutability and latent flexibility — is the diagnostic signature of a false natural law disguised as institutional necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Chitra: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. She cannot exit without violating the boon (metaphysical barrier), betraying her father (emotional barrier), or destabilizing succession (institutional barrier). Royal Father: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.11. Net beneficiary. He solves the succession problem through upbringing, which is experienced as loving parenting within his institutional constraints. Patriarchal Succession System: Beneficiary + constrained → d≈0.35, f(d)≈0.32. Moderate beneficiary, but constrained by the boon's terms (cannot fully optimize gender flexibility). Alternative Identity Framework: Organized + mobile → d≈0.45, f(d)≈0.48. Low-to-moderate extraction if realized; represents an exit path with organizational support. The wide directionality spread (0.05 to 0.92) reflects the profound structural asymmetry between the institution's experience and the victim's.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (extractiveness > 0.70 threshold partially exceeded): The constraint is classified as Snare, but carries structural elements that could confuse this with Mountain or Rope. The mandatrophy is resolved as follows: (1) SNARE, NOT MOUNTAIN: The divine boon and paternal upbringing create the appearance of immutable metaphysical determinism, but the measured increase in extractiveness over the biographical interval (0.30→0.58) and the rising theater ratio (0.35→0.55) reveal this as an institutional arrangement, not a natural law. If it were a true Mountain, the metrics would be constant and the theater would be low (functional, not performative). The rising theater indicates that the original coordination function (solving succession) has become partially performative as the victim matures and the psychological cost accumulates. (2) SNARE, NOT ROPE: From the father's perspective, the constraint appears as Rope — coordination with love-based enforcement. But Rope requires that the victim experiences net benefit or at minimum perceives the mechanism as fair. Chitra's perspective (d≈0.92, maximum extraction) cannot be reconciled with Rope. The father's good intentions and genuine love do not transform extraction into coordination when the victim's agency is suppressed. (3) MANDATROPHY RESOLVED: The constraint is genuinely Snare from the victim's primary perspective, Rope from the beneficiary's perspective, and risks appearing as Mountain from the analytical observer's perspective. This is structurally resolved: different perspectives legitimately see different types because the constraint is hierarchical (parent→child, institution→individual, metaphysical authority→person) and the victim has no exit. The structure does not permit false consensus. The analytical observer's task is to avoid the false summit (Mountain) by noting the measured increase in extractiveness and theater over time — these are diagnostic of institutional arrangement, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boon_semantic_flexibility,
    'Does the divine boon''s essence require gender-normative performance, or only dynastic legitimacy and succession continuity?',
    'Textual reinterpretation of the boon''s original terms; theological or narrative analysis of whether ''male heir'' is gendered prescription or functional role description',
    'If boon requires gender performance: lock-in is structural, classification remains Snare. If boon permits flexible identity: exit path becomes available, constraint downgrades to Scaffold with sunset potential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boon_semantic_flexibility, conceptual, 'Semantic flexibility of the divine boon''s gender requirement').

omega_variable(
    paternal_love_as_enforcement_mechanism,
    'Is the father''s upbringing of Chitra as male heir primarily a coercive enforcement mechanism, or a genuine expression of love within the constraints he inherited?',
    'Narrative analysis of father''s subjective experience, constraints on his own choices, relationship quality with Chitra; whether the father would endorse alternative identity frameworks if institutional pressure were removed',
    'If coercive: father is straightforward beneficiary, extraction is clear. If genuine love within institutional trap: father is also partly victim, relationship becomes both constraining and sustaining. Affects moral clarity of snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(paternal_love_as_enforcement_mechanism, conceptual, 'Whether paternal enforcement is coercive or loving within inherited constraint').

omega_variable(
    identity_fusion_point,
    'At what developmental stage does Chitra''s identity become fused with the male heir role such that exit becomes psychologically destabilizing?',
    'Developmental psychology analysis; longitudinal narrative tracking of when identity integration occurs; assessment of psychological cost of identity reformation at different life stages',
    'If fusion is early and deep: escape carries severe psychological cost, lock-in is effectively permanent despite theoretical exit. If fusion is later and more superficial: exit remains costly but psychologically feasible. Affects whether true exit exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_point, empirical, 'Developmental point of identity fusion with male heir role').

omega_variable(
    institutional_precedent_for_gender_flexibility,
    'Does the broader institutional/religious context have any precedent for succession or boon fulfillment via gender-nonconforming identity?',
    'Historical or narrative research into alternative cases; examination of institutional flexibility in neighboring domains; whether exceptions have been granted',
    'If precedent exists: institutional path for exit is established, constraint downgrades from Snare toward Scaffold. If no precedent: institutional resistance is maximized, lock-in is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_precedent_for_gender_flexibility, empirical, 'Institutional precedent for gender-flexible succession').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legacy_identity_lock_in, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legacy_theater_t0, legacy_identity_lock_in, theater_ratio, 0, 0.35).
narrative_ontology:measurement(legacy_theater_t10, legacy_identity_lock_in, theater_ratio, 10, 0.48).
narrative_ontology:measurement(legacy_theater_t20, legacy_identity_lock_in, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(legacy_extract_t0, legacy_identity_lock_in, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legacy_extract_t10, legacy_identity_lock_in, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(legacy_extract_t20, legacy_identity_lock_in, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legacy_identity_lock_in, resource_allocation).
narrative_ontology:affects_constraint(legacy_identity_lock_in, gender_assignment_normalization).
narrative_ontology:affects_constraint(legacy_identity_lock_in, dynastic_succession_legitimacy).
narrative_ontology:affects_constraint(legacy_identity_lock_in, boon_fulfillment_institutional_dependency).

% DUAL FORMULATION NOTE:
% The legacy identity lock-in decomposes into three structurally distinct constraints: (1) gender_assignment_normalization (ε≈0.45, how gender is institutionalized) — upstream constraint, affects the identity lock-in's suppression mechanism. (2) legacy_identity_lock_in (ε=0.58, this story) — focal constraint, how upbringing and boons create identity fusion. (3) boon_fulfillment_institutional_dependency (ε≈0.35, how institutions depend on boon validation) — downstream constraint, affected by changes to identity framework. The three form a constraint family linked by institutional coupling: each constrains the others through the succession system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legacy_identity_lock_in, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
