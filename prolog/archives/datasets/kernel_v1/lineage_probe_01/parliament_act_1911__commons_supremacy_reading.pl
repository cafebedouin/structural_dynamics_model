% ============================================================================
% CONSTRAINT STORY: parliament_act_1911__commons_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parliament_act_1911__commons_supremacy_reading, []).

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
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: parliament_act_1911__commons_supremacy_reading
 *   human_readable: Democratic Supremacy via Commons Primacy (1911 Act Reading)
 *   domain: constitutional_law/parliamentary_procedure
 *
 * SUMMARY:
 *   The 1911 Parliament Act institutionalized the doctrine of democratic
 *   supremacy: the will of the elected Commons prevails, and the hereditary
 *   House of Lords may only delay legislation for two years (later one month
 *   by the 1949 Act). This constraint is one reading of a contested kernel —
 *   the Parliament Act 1911 — whose meaning is still disputed in
 *   constitutional law. The commons_supremacy_reading interprets the Act as a
 *   genuine constitutional settlement that suppresses the principle of
 *   hereditary veto and establishes elected representation as primary. This
 *   reading sees low extractiveness (0.18) because it frames the constraint
 *   as coordinating between chambers via a shared commitment to democratic
 *   legitimacy: the Commons exercises supremacy, the Lords exercise
 *   deliberation, and both serve the franchised public. Extractiveness is
 *   measured from the standpoint of the franchise-holding public, not from
 *   the interests of hereditary peers whose institutional veto-power was
 *   suppressed. The constraint's low theater (0.25) reflects that the
 *   mechanism is straightforward — a procedural rule preventing indefinite
 *   delay — rather than performative cover for hidden extraction. The dispute
 *   with sibling readings turns on whether this settlement was consensual
 *   (rope) or coercive (snare/tangled_rope), and on whether the suppressed
 *   veto-power was an extractive mechanism (common law of the franchise) or a
 *   legitimate alternative legislative voice (federalism within a bicameral
 *   system). This story instantiates the commons_supremacy reading only.
 *
 * KEY AGENTS:
 *   - Franchise-holding Public: Primary beneficiary (moderate/mobile) — the constraint is oriented toward ensuring their elected representatives are not overruled by an unelected hereditary chamber; benefits from democratic primacy coordination
 *   - House of Commons: Primary beneficiary & implementing institution (institutional/constrained) — gains legislative supremacy; coordinates legislative process via delaying power preservation
 *   - House of Lords (Hereditary Peers): Partial victim, partial continuing institution (powerful/arbitrage) — veto-power suppressed (extractive from their perspective); delaying power preserved (coordination component); retains social prestige and deliberative function
 *   - Constitutional Authority (the 'common law' or 'principles of democratic legitimacy'): Background authority grounding the constraint; this reading treats democratic primacy as grounded in electoral legitimacy, not as a concession wrung by force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parliament_act_1911__commons_supremacy_reading, 0.18).
domain_priors:suppression_score(parliament_act_1911__commons_supremacy_reading, 0.08).
domain_priors:theater_ratio(parliament_act_1911__commons_supremacy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parliament_act_1911__commons_supremacy_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(parliament_act_1911__commons_supremacy_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(parliament_act_1911__commons_supremacy_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parliament_act_1911__commons_supremacy_reading, rope).
narrative_ontology:human_readable(parliament_act_1911__commons_supremacy_reading, "Democratic Supremacy via Commons Primacy (1911 Act Reading)").
narrative_ontology:topic_domain(parliament_act_1911__commons_supremacy_reading, "constitutional_law/parliamentary_procedure").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parliament_act_1911__commons_supremacy_reading, 'e34fab35-2a0f-4770-a85d-223f1f0ab5e4').
narrative_ontology:cs_kernel_codification('e34fab35-2a0f-4770-a85d-223f1f0ab5e4', formalized).
narrative_ontology:cs_authority_grounding('e34fab35-2a0f-4770-a85d-223f1f0ab5e4', lineage).
narrative_ontology:cs_interpretation_layer_present('e34fab35-2a0f-4770-a85d-223f1f0ab5e4').
narrative_ontology:cs_reading_relation('e34fab35-2a0f-4770-a85d-223f1f0ab5e4', parliament_act_1911__coerced_consent_reading, forecloses).
narrative_ontology:cs_reading_relation('e34fab35-2a0f-4770-a85d-223f1f0ab5e4', parliament_act_1911__delegated_continuation_reading, influences).
narrative_ontology:cs_axiom('e34fab35-2a0f-4770-a85d-223f1f0ab5e4', foundational, elected_representation_constitutionally_primary).
narrative_ontology:cs_axiom_status(elected_representation_constitutionally_primary, holdable).
narrative_ontology:cs_axiom_grounding('e34fab35-2a0f-4770-a85d-223f1f0ab5e4', elected_representation_constitutionally_primary, deontological).
narrative_ontology:cs_axiom('e34fab35-2a0f-4770-a85d-223f1f0ab5e4', secondary, bicameral_coordination_via_delaying_power).
narrative_ontology:cs_axiom_status(bicameral_coordination_via_delaying_power, holdable).
narrative_ontology:cs_axiom_grounding('e34fab35-2a0f-4770-a85d-223f1f0ab5e4', bicameral_coordination_via_delaying_power, instrumental).
narrative_ontology:cs_reference_frame('e34fab35-2a0f-4770-a85d-223f1f0ab5e4', democratic_representation_primacy).
narrative_ontology:cs_drift_state('e34fab35-2a0f-4770-a85d-223f1f0ab5e4', contemporary_human_rights_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e34fab35-2a0f-4770-a85d-223f1f0ab5e4', '').
narrative_ontology:cs_kernel_id(parliament_act_1911__commons_supremacy_reading, parliament_act_1911).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parliament_act_1911__commons_supremacy_reading, franchise_holding_public).
narrative_ontology:constraint_beneficiary(parliament_act_1911__commons_supremacy_reading, elected_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRANCHISE-HOLDING PUBLIC (ROPE) — Generational timescale. The 1911 Act solves a genuine coordination problem: how to ensure the elected chamber's will prevails over an unelected veto. This reading frames the suppression of hereditary veto as restoring democratic primacy, not extracting from the public. Low experienced extractiveness because the constraint aligns incentives — elected representation serves the franchised. Mobile exit options (generational) because if the commons fails to represent, the public can organize opposition and alter the constraint via further legislation.
constraint_indexing:constraint_classification(parliament_act_1911__commons_supremacy_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: ELECTED COMMONS (ROPE) — Biographical timescale. The constraint formalizes the Commons' legislative supremacy: it coordinates between chambers by eliminating the Lords' absolute veto while preserving a delaying function. The Commons experiences this as securing their authority without paying extraction costs — the constraint embeds coordination (two-chamber review) without forcing the Commons to transfer power elsewhere. Constrained exit (biographical) because individual MPs serve fixed terms; institutional exit (replacing the Act) requires legislative supermajority.
constraint_indexing:constraint_classification(parliament_act_1911__commons_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEREDITARY PEERS (TANGLED ROPE) — Biographical timescale. The 1911 Act functions as both coordination (the Lords retain a delaying function, preserving bicameral review) and extraction (the absolute veto — the Lords' structural power — is suppressed). Peers with arbitrage options (patronage networks, wealth, social authority independent of the Act) experience this as moderate extraction: the legislative supremacy reading removes the veto-suppression mechanism (extractive) but leaves prestige and deliberative function intact (coordination). Powerful agents have agency in the transition; extractiveness is constrained by the delaying power preservation.
constraint_indexing:constraint_classification(parliament_act_1911__commons_supremacy_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, this reading risks presenting democratic primacy as an immutable principle (the will of the people cannot permanently be overruled by an unelected body; this follows from democratic legitimacy itself). This perspective sees the constraint as capturing a structural necessity of representative government. However, the empirical data contradicts the mountain gate: the constraint is formally enacted (emerges_naturally: false), suppression is minimal (0.08), and extractiveness is low (0.18) because the constraint genuinely coordinates rather than coerces. The false summit detector identifies this perspective as naturalizing a contingent institutional choice.
constraint_indexing:constraint_classification(parliament_act_1911__commons_supremacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parliament_act_1911__commons_supremacy_reading_tests).
:- end_tests(parliament_act_1911__commons_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. This reading frames the suppression of hereditary veto-power not as extraction from the hereditary interest but as restoration of democratic primacy to the public via their elected representatives. The metric is measured from the standpoint of the franchise-holding public (the beneficiary) and the Commons (the institutional implementer), not from the hereditary peers whose veto is suppressed. The low value reflects that the constraint coordinates rather than coerces: the Commons gains authority, the public gains representation, and the Lords gain a defined (though reduced) role. Extractiveness is not zero because the hereditary interest does bear a cost (loss of absolute veto), but that cost is classified as legitimate correction rather than extraction from the constraint's perspective. Suppression (0.08): Minimal. The constraint suppresses only the hereditary veto-principle, not the Lords themselves. The Lords retain legislative function (delaying power), social status, and deliberative authority. The suppression is doctrinal: the principle that an unelected body can permanently override an elected one is declared illegitimate. But suppression of alternatives is low because the alternative (hereditary veto) is still available to the Lords if they choose to use delaying power strategically — they simply cannot do so indefinitely. Theater (0.25): Low. The mechanism is transparent and procedural: the Commons can pass legislation twice in two years (later one month) and the Lords cannot prevent it. No performative cover is needed; the rule itself is the constraint. Low theater reflects straightforward institutional design rather than elaborate justification structures. Claimed type (rope): This reading's base properties support rope classification — low extractiveness, low suppression, coordination function genuine (Commons and Lords both participate in legislative process, just with different veto powers).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps in this constraint reveal the entire contested meaning of the 1911 Act. The franchise-holding public sees coordination (Rope) — the constraint ensures their elected representatives are not overruled. The Commons sees coordination (Rope) — they gain authority within a structured process. The hereditary peers see extraction (Tangled Rope with significant extractive component) — their veto-power is suppressed, though delaying power is preserved. The analytical observer at civilizational scale risks naturalizing this as democratic necessity (Mountain) — the constraint follows from the principle that the people's representatives cannot be permanently overruled — but the false summit detector identifies this as contingent institutional choice. The coerced_consent_reading would see this as snare (coercive amendment under threat), and the delegated_continuation_reading would see procedural subordination (additional extractive overhead). The gaps between these perspectives reveal where the constitutional settlement is contested.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is determined by each agent's structural position. The franchise-holding public and Commons are beneficiaries (d low, f(d) near zero or negative) because the constraint establishes their authority. The hereditary peers are partial victims (d moderate, 0.40-0.55 range) because veto-power is suppressed but delaying power is preserved and social authority remains intact. The analytical observer at civilizational scope is positioned as an arbiter, but this reading's commitment to democratic primacy as a doctrine means the observer's 'neutral' position is already shaped by the reading's axiom (elected representation is constitutionally primary). This reading does not include directionality overrides because the derivation chain (beneficiary/victim + exit options) produces accurate d values: beneficiaries with mobile exit experience low extraction, partial victims with powerful position experience moderate extraction, analytical observers experience the constraint as necessity rather than choice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_mechanism_ambiguity,
    'Did the 1911 Act pass via genuine constitutional consent or via coercive extraction (the threat of creating 500 new peers)?',
    'Historical analysis of the Cabinet''s deliberations and Parliament''s voting patterns. Distinguish between bargained compromise and coerced acceptance: did the House of Lords negotiate and accept delaying power as a fair trade, or did they capitulate under the threat of institutional flooding?',
    'If coercive: the reading shifts from rope (voluntary coordination) toward snare (enforced suppression of veto) or tangled_rope (mixed coordination-extraction). Extractiveness rises from 0.18 toward 0.40+. If consensual: rope classification holds. This is the forecloses axis — the coerced_consent_reading and commons_supremacy_reading cannot coexist in a single constitutional framework without redefining ''consent.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_mechanism_ambiguity, empirical, 'Whether the 1911 Act passed via consent or coercion').

omega_variable(
    veto_suppression_principle_scope,
    'Does the supremacy principle apply only to the 1911 Parliament Acts procedure or to the entire constitutional relationship between chambers?',
    'Legal interpretation: examine whether courts treat delaying power (the substantive outcome) as coordinate with the procedure that produced it. If courts examine Acts passed under the procedure with same scrutiny as ordinary legislation, the principle is procedural subordination. If courts treat them as binding precedent equal to pre-1911 Acts, the principle is modified primacy.',
    'If procedural only: the delegated_continuation_reading is correct — Commons supremacy is limited to the specific Parliament Acts mechanism. Extractiveness lower (0.12) because the principle is narrow. If constitutional: extractiveness higher (0.25+) because the principle broadly suppresses hereditary veto. This influences the delegated_continuation_reading structurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_suppression_principle_scope, empirical, 'Scope of democratic supremacy principle: procedural vs constitutional').

omega_variable(
    delaying_power_functional_role,
    'Does the preserved delaying power of the House of Lords constitute genuine bicameral coordination or merely symbolic residual authority?',
    'Empirical tracking of Lords amendments and rejections post-1911: do such actions materially slow legislation, produce substantive revisions, or function as deliberative review? Compare to pre-1911 veto exercise rates.',
    'If genuine coordination function: rope classification holds; extractiveness remains low (0.18) because both chambers contribute meaningfully. If symbolic only (theater): classification shifts toward piton (degraded institution maintained by inertia); theater_ratio rises; extractiveness may rise if the delaying function is performative cover for class-based obstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delaying_power_functional_role, empirical, 'Whether Lords delaying power performs genuine coordination or is ceremonial').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parliament_act_1911__commons_supremacy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(parl_be_t0, parliament_act_1911__commons_supremacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(parl_be_t1, parliament_act_1911__commons_supremacy_reading, base_extractiveness, 1, 0.28).
narrative_ontology:measurement(parl_be_t10, parliament_act_1911__commons_supremacy_reading, base_extractiveness, 10, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(parl_su_t0, parliament_act_1911__commons_supremacy_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(parl_su_t1, parliament_act_1911__commons_supremacy_reading, suppression_requirement, 1, 0.25).
narrative_ontology:measurement(parl_su_t10, parliament_act_1911__commons_supremacy_reading, suppression_requirement, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parliament_act_1911__commons_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(parliament_act_1911__commons_supremacy_reading, parliament_act_1911__coerced_consent_reading).
narrative_ontology:affects_constraint(parliament_act_1911__commons_supremacy_reading, parliament_act_1911__delegated_continuation_reading).

% DUAL FORMULATION NOTE:
% The Parliament Act 1911 is a contested kernel instantiated in three constraint stories: (1) commons_supremacy_reading (this story) frames the Act as establishing democratic supremacy as constitutional principle; extractiveness low because the constraint coordinates public representation. (2) coerced_consent_reading frames the Act as extracted via threat of institutional flooding; extractiveness moderate-high because passage was coercive. (3) delegated_continuation_reading frames Acts passed under the procedure as delegated legislation subordinate to the 1911 Act itself; extractiveness includes procedural overhead. All three stories share the same base institutional fact (the Parliament Act exists) but differ in interpretation of its constitutional meaning, passage mechanism, and scope of application. They are linked via network.affects_constraints because each reading influences the others: if coercion is established, the legitimacy of the 'supremacy' claim is weakened; if delegation is established, the 'supremacy' is narrower in scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
