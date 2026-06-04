% ============================================================================
% CONSTRAINT STORY: personalist_dictatorship__coup_proofing_mechanics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personalist_dictatorship__coup_proofing_mechanics, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: personalist_dictatorship__coup_proofing_mechanics
 *   human_readable: Personalist Coup-Proofing: Military Weakening for Palace Security
 *   domain: political/comparative_authoritarianism
 *
 * SUMMARY:
 *   Personalist autocracies survive by systematically weakening the
 *   military's capacity to coordinate against the palace. The coup-proofing
 *   architecture — parallel security forces (praetorian guard, secret
 *   police), rotation of senior commanders, fragmented intelligence networks,
 *   competing service hierarchies — solves the ruler's core security problem:
 *   preventing the military from using its coercive capacity to overthrow the
 *   regime. However, this solution extracts a severe cost: the state's
 *   defense against external enemies is deliberately suppressed. Officers
 *   cannot coordinate across services; institutional memory is destroyed by
 *   rotation; doctrinal development is prevented; resources are diverted to
 *   palace security. The state becomes internally fortified but externally
 *   weakened — the palace is coup-proof but the nation is defense-fragile.
 *   This constraint is ONE READING of the personalist dictatorship kernel:
 *   the structural mechanics of how personalist rule maintains itself through
 *   military fragmentation. Other readings address different mechanisms: the
 *   charisma_routinization problem (Weber's succession crisis), the
 *   cult_information_pathology (mandatory praise destroying intelligence).
 *   This reading focuses specifically on the coup-proofing trade-off: palace
 *   security bought with military effectiveness.
 *
 * KEY AGENTS:
 *   - Personalist Ruler: Primary beneficiary (institutional/arbitrage) — coup-proofing solves the core threat (military coordination). Direct beneficiary of suppressed military capacity.
 *   - Palace Security Apparatus: Primary beneficiary (institutional/arbitrage) — well-resourced, unified, maintains intelligence monopoly, receives diverted military budgets.
 *   - Officer Corps: Primary victim (powerless/trapped) — systematically prevented from coordination; command rotation destroys institutional memory; career advancement depends on palace loyalty, not military capability.
 *   - Military National Defense Capacity: Victim (structural position) — external defense capability is deliberately suppressed; doctrine development prevented; interservice coordination fragmented.
 *   - State Institutional Integrity: Victim (structural position) — military institutions are hollowed out; professionalism displaced by loyalty; institutions become hollow shells performing coup-proofing theater.
 *   - Analytical Observer: Civilizational perspective — sees the long-term trajectory: palace security accumulates; external threats accumulate; regime becomes internally safe but externally vulnerable; eventually faces state capacity collapse or external conquest.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personalist_dictatorship__coup_proofing_mechanics, 0.58).
domain_priors:suppression_score(personalist_dictatorship__coup_proofing_mechanics, 0.72).
domain_priors:theater_ratio(personalist_dictatorship__coup_proofing_mechanics, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personalist_dictatorship__coup_proofing_mechanics, extractiveness, 0.58).
narrative_ontology:constraint_metric(personalist_dictatorship__coup_proofing_mechanics, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(personalist_dictatorship__coup_proofing_mechanics, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personalist_dictatorship__coup_proofing_mechanics, tangled_rope).
narrative_ontology:human_readable(personalist_dictatorship__coup_proofing_mechanics, "Personalist Coup-Proofing: Military Weakening for Palace Security").
narrative_ontology:topic_domain(personalist_dictatorship__coup_proofing_mechanics, "political/comparative_authoritarianism").

domain_priors:requires_active_enforcement(personalist_dictatorship__coup_proofing_mechanics).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personalist_dictatorship__coup_proofing_mechanics, '4141945d-afa6-4aed-ba4e-e057a829e4c4').
narrative_ontology:cs_kernel_codification('4141945d-afa6-4aed-ba4e-e057a829e4c4', implicit).
narrative_ontology:cs_authority_grounding('4141945d-afa6-4aed-ba4e-e057a829e4c4', extraction).
narrative_ontology:cs_reading_relation('4141945d-afa6-4aed-ba4e-e057a829e4c4', personalist_dictatorship__charisma_routinization_problem, coexists_with).
narrative_ontology:cs_reading_relation('4141945d-afa6-4aed-ba4e-e057a829e4c4', personalist_dictatorship__cult_information_pathology, coexists_with).
narrative_ontology:cs_axiom('4141945d-afa6-4aed-ba4e-e057a829e4c4', foundational, military_coordination_prevents_coups).
narrative_ontology:cs_axiom_status(military_coordination_prevents_coups, holdable).
narrative_ontology:cs_axiom_grounding('4141945d-afa6-4aed-ba4e-e057a829e4c4', military_coordination_prevents_coups, empirically_contingent).
narrative_ontology:cs_axiom('4141945d-afa6-4aed-ba4e-e057a829e4c4', foundational, national_defense_subordinate_to_regime_survival).
narrative_ontology:cs_axiom_status(national_defense_subordinate_to_regime_survival, holdable).
narrative_ontology:cs_axiom_grounding('4141945d-afa6-4aed-ba4e-e057a829e4c4', national_defense_subordinate_to_regime_survival, instrumental).
narrative_ontology:cs_reference_frame('4141945d-afa6-4aed-ba4e-e057a829e4c4', military_subordination_to_palace).
narrative_ontology:cs_drift_state('4141945d-afa6-4aed-ba4e-e057a829e4c4', contemporary_weakened_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4141945d-afa6-4aed-ba4e-e057a829e4c4', '').
narrative_ontology:cs_kernel_id(personalist_dictatorship__coup_proofing_mechanics, personalist_dictatorship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personalist_dictatorship__coup_proofing_mechanics, palace_security_apparatus).
narrative_ontology:constraint_beneficiary(personalist_dictatorship__coup_proofing_mechanics, personalist_ruler).
narrative_ontology:constraint_victim(personalist_dictatorship__coup_proofing_mechanics, military_national_defense_capacity).
narrative_ontology:constraint_victim(personalist_dictatorship__coup_proofing_mechanics, officer_corps_coordination).
narrative_ontology:constraint_victim(personalist_dictatorship__coup_proofing_mechanics, state_institutional_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OFFICER CORPS (SNARE) — Career military officers are trapped in a structure designed to prevent coordinated action. Command rotation prevents organizational memory; parallel forces compete for resources; intelligence sharing is fragmented. Officers cannot organize a coup (the constraint's function) but also cannot coordinate effective national defense. Extraction: their professional capacity is systematically suppressed to ensure the ruler's safety. No exit — officers cannot leave the system without desertion or exile. Maximum experienced extraction.
constraint_indexing:constraint_classification(personalist_dictatorship__coup_proofing_mechanics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NATIONAL DEFENSE (SNARE) — The state's capacity to defend against external threats is a victim, not an agent, but from the structural position of 'external threat response,' the constraint appears as pure extraction. Military effectiveness is deliberately suppressed — coordination across services is prevented, training budgets are diverted to palace security, officers rotate before developing doctrine. The defense function bears the full cost of coup-proofing without benefit. Trapped in the institutional structure; no exit.
constraint_indexing:constraint_classification(personalist_dictatorship__coup_proofing_mechanics, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL MILITARY COMMAND (TANGLED ROPE) — A regional commander benefits from direct palace access and resource allocation (coordination function: managing local security and ruler visibility in the region). But the rotation regime prevents building independent power base, and command disputes are mediated by palace security apparatus (extraction function: preventing coercive capacity). Mixed experience — some coordination benefit, significant extraction.
constraint_indexing:constraint_classification(personalist_dictatorship__coup_proofing_mechanics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PALACE SECURITY APPARATUS (ROPE) — The constraint is optimized for this actor. Security forces are well-resourced, unified under palace command, rotate at palace discretion, and face no coordination challenges to their primary mission (ruler protection). Secondary benefit: intelligence monopoly and veto over other security forces. Beneficiary experiencing coordination (rope classification) not extraction. Net flow of extraction toward this actor.
constraint_indexing:constraint_classification(personalist_dictatorship__coup_proofing_mechanics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PERSONALIST RULER (ROPE) — The constraint solves the ruler's core coordination problem: keeping the military unable to coordinate against the palace. Coup-proofing architecture is a pure coordination mechanism from the ruler's structural position — fragmenting military units, rotating commanders, maintaining parallel loyalties. The ruler experiences this as functional architecture, not extraction. Low chi from ruler's position (net beneficiary).
constraint_indexing:constraint_classification(personalist_dictatorship__coup_proofing_mechanics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a generational/global perspective, coup-proofing is a tangled hybrid: genuine coordination problem (ruler safety from military) combined with severe extraction (national defense suppressed). The structure does solve the stated coordination problem, but at enormous cost — military weakness against external enemies, institutional fragmentation, long-term state capacity decay. The architecture is functional for palace security, extractive for state defense. Over generations, the extraction accumulates as state institutions atrophy.
constraint_indexing:constraint_classification(personalist_dictatorship__coup_proofing_mechanics, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personalist_dictatorship__coup_proofing_mechanics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(personalist_dictatorship__coup_proofing_mechanics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(personalist_dictatorship__coup_proofing_mechanics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(personalist_dictatorship__coup_proofing_mechanics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personalist_dictatorship__coup_proofing_mechanics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint trades military national defense capacity for palace coup-proofing. This is a genuine extraction — the loss of defensive capacity is real and persistent. However, extractiveness is not at the snare floor (0.66+) because the constraint does solve a real coordination problem (preventing military coups), and some officers benefit from direct palace access (rotated commanders get senior positions, resources flow to palace-adjacent units). The extraction is significant but not maximum. Suppression (0.72): High. The coup-proofing architecture is fundamentally repressive: military units cannot coordinate, officers cannot organize, intelligence networks are fragmented, command rotation prevents institution-building. Officers have constrained exit (defection, exile, or compliance are the options). This is structural suppression embedded in the regime's architecture. Theater ratio (0.45): Moderate. Coup-proofing has genuine functional content — the architecture does prevent coordinated military action against the palace. The constraint is not primarily performative; the fragmentation is real. However, some component is theater: loyalty rituals, displayed disunity, security briefings that emphasize threats to justify continued fragmentation. The low theater ratio reflects that coup-proofing is functionally effective, not merely symbolic.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is dramatic. The personalist ruler sees a functional coordination mechanism (Rope) — coup-proofing solves the core problem elegantly. The palace security apparatus sees the same mechanism as enabling coordination for their mission (Rope). The officer corps sees the same structure as pure suppression (Snare) — fragmentation prevents their core mission (national defense) and traps them in a rotation regime. The national defense function sees pure extraction (Snare) — the state's external defense capacity is deliberately sacrificed. The analytical observer sees a tangled hybrid (Tangled Rope) at the generational level: genuine coordination solution for the ruler, severe extraction for the state. The gap reveals that 'functional' is relative to the observer's structural position: the constraint functions perfectly for palace security, extracts maximally from national defense.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the coup-proofing architecture. The personalist ruler is the beneficiary with arbitrage exit (can exit the regime by abdication, though politically difficult) — derives low d, low chi. Palace security apparatus is the beneficiary with institutional stability (arbitrage-like position) — also low d, low chi. The officer corps is trapped (no exit without desertion/exile) and victimized by the suppression mechanism — derives high d, high chi. National defense capacity and state institutional integrity are structural positions (not agents) that experience maximum extraction with no exit — high d, high chi. The analytical observer at the civilizational horizon derives d from the system's long-term pressure (extraction accumulating, external threats building) — moderate-high d, high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that Tangled Rope classification is correct: coup-proofing contains BOTH genuine coordination function (preventing military coups) AND asymmetric extraction (national defense suppressed). The constraint would be incorrectly classified as pure Rope if measured only from the ruler/palace perspective, or as pure Snare if measured only from the external defense perspective. The Tangled Rope captures both dynamics: yes, coups are prevented (coordination solved); yes, national defense capacity is traded away (extraction imposed). The mandatrophy resolution requires declaring both beneficiaries (ruler, palace security) and victims (officer corps, national defense). The extraction mechanism (military fragmentation) is genuine, not rhetorical. The coordination problem being solved (preventing military overthrow) is also genuine. Both are structural features of the regime, not competing interpretations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coup_proofing_necessity_claim,
    'Is coup-proofing a necessary response to a real threat of military coups, or does it CREATE the coup threat by weakening civilian institutions?',
    'Comparative analysis of regimes with coup-proofing vs. regimes without military-palace separation: Which face more coup attempts? Is frequency higher in coup-proofed systems or in systems with strong institutionalized civil-military relations?',
    'If necessary response: the constraint solves a real coordination problem (Rope classification supported). If self-creating threat: the extraction mechanism IS what justifies itself (a snare using security rhetoric).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coup_proofing_necessity_claim, empirical, 'Whether coup-proofing is preventive response or self-justifying extraction').

omega_variable(
    external_threat_magnitude_counterfactual,
    'How much national defense capacity is actually traded away to palace security, and how much does the regime face credible external threats?',
    'Military capability assessment (modernization rates, readiness audits, doctrine development) compared to external threat profiles; analysis of military budgets allocated to coup-proofing overhead vs. national defense',
    'If trade is high and external threats are real: significant extraction from defense to security (snare perspective validated). If external threats are minimal: extraction appears as pure palace rent-seeking (snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(external_threat_magnitude_counterfactual, empirical, 'Scale of defense-capability trade-off for palace security').

omega_variable(
    reading_identity_ambiguity,
    'Is coup-proofing a READING of the personalist dictatorship kernel, or is it the kernel''s CENTRAL MECHANISM rather than one interpretation of it?',
    'Structural analysis: does coup-proofing appear in ALL personalist regimes (making it constitutive), or only in some (making it a strategic choice)? Do other readings (charisma_routinization, cult_information_pathology) treat coup-proofing as given infrastructure or as optional feature?',
    'If constitutive: coup-proofing is not a reading but THE constraint, and sibling readings are distinct problems within a coup-proofed regime. If contingent: it is one strategic interpretation among alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Whether coup-proofing is constitutive to personalism or one contingent strategy').

omega_variable(
    civilian_institutional_vulnerability,
    'Does coup-proofing architecture suppress military coordination at the cost of strengthening palace security into a secondary power center that can itself threaten the regime?',
    'Historical pattern analysis: in coup-proofed regimes, how many transitions are driven by palace security apparatus (praetorian guard coups, palace palace-coup sequences) vs. military coups? Is the new threat source the same military now split, or is it the palace security apparatus claiming the throne?',
    'If palace security becomes the new coup source: the constraint trades military coup threat for palace security threat (Snare classification: extraction without solving the underlying problem, just moving it). If military remains the primary threat: coup-proofing succeeds (Rope/Tangled Rope validated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_institutional_vulnerability, empirical, 'Whether coup-proofing prevents coups or redirects coup source to palace security').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personalist_dictatorship__coup_proofing_mechanics, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coup_proof_tr_t0, personalist_dictatorship__coup_proofing_mechanics, theater_ratio, 0, 0.38).
narrative_ontology:measurement(coup_proof_tr_t10, personalist_dictatorship__coup_proofing_mechanics, theater_ratio, 10, 0.41).
narrative_ontology:measurement(coup_proof_tr_t20, personalist_dictatorship__coup_proofing_mechanics, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(coup_proof_be_t0, personalist_dictatorship__coup_proofing_mechanics, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(coup_proof_be_t10, personalist_dictatorship__coup_proofing_mechanics, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(coup_proof_be_t20, personalist_dictatorship__coup_proofing_mechanics, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(coup_proof_su_t0, personalist_dictatorship__coup_proofing_mechanics, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(coup_proof_su_t10, personalist_dictatorship__coup_proofing_mechanics, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(coup_proof_su_t20, personalist_dictatorship__coup_proofing_mechanics, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personalist_dictatorship__coup_proofing_mechanics, enforcement_mechanism).
narrative_ontology:affects_constraint(personalist_dictatorship__coup_proofing_mechanics, personalist_dictatorship__charisma_routinization_problem).
narrative_ontology:affects_constraint(personalist_dictatorship__coup_proofing_mechanics, personalist_dictatorship__cult_information_pathology).

% DUAL FORMULATION NOTE:
% Coup-proofing is one of three structurally distinct mechanisms sustaining personalist rule. The other two readings address succession (charisma_routinization) and information pathology (cult_information). All three readings share the kernel (personalist rule survives through the personalist's biography) but have different extractiveness values and different constraint structures. This reading (coup_proofing_mechanics) has ε=0.58 and focuses on military suppression. The charisma_routinization reading would have higher ε (succession cannot be transferred) and focus on institutional non-succession. The cult_information_pathology reading would have higher ε (information monopoly creates governance by fiction) and focus on epistemic collapse. Each is a distinct constraint within the personalist dictatorship family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personalist_dictatorship__coup_proofing_mechanics, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
