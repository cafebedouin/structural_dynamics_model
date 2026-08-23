% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Security-Necessity Reading of Territorial Legitimacy (Post-1967 Depth Doctrine)
 *   domain: political theory/international law/territorial sovereignty
 *
 * SUMMARY:
 *   A doctrine of territorial legitimacy under which control of the West Bank
 *   and Golan is justified as security necessity: the post-1967 lines plus
 *   strategic depth constitute the defensible minimum, Palestinian statehood
 *   is admissible only demilitarized, and civilian presence beyond the lines
 *   is framed as security-relevant. Operationally the doctrine runs through
 *   military administration, permit and planning regimes, settlement
 *   infrastructure, and patron diplomacy that insures the frame
 *   internationally. The claim and the metrics are authored independently:
 *   claimed_type is tangled_rope (a genuine, externally corroborated
 *   security-coordination function carrying asymmetric, actively enforced
 *   costs), while the metric values describe the arrangement's actual
 *   operation as this reading's own lights assess it — including components
 *   the reading itself counts as excess. This story instantiates one reading
 *   of a legitimacy kernel; the reading's committer structure is recorded in
 *   kernel_context and the omega variables.
 *
 * KEY AGENTS:
 *   - israeli_security_establishment: Agenda setter (institutional/constrained) — defines the threat picture and necessary depth; administers the military government; collects mission, budget, and doctrinal authority
 *   - west_bank_settlement_movement: Primary beneficiary (organized/identity_locked) — holds the territorial gains the doctrine legitimizes
 *   - golan_settler_communities: Secondary beneficiary (organized/identity_locked) — annexation-normalized residency on the plateau
 *   - palestinian_residents_west_bank: Primary target (powerless/trapped) — bears movement, land-access, and status costs
 *   - palestinian_authority_leadership: Conditioned intermediary (moderate/trapped) — pays in legitimacy, draws survival benefit from security coordination
 *   - palestinian_refugee_diaspora: Distributed target (powerless/trapped) — return claim foreclosed by the security frame
 *   - syrian_golan_residents: Annexed population (powerless/trapped) — unresolved legal status under annexation law
 *   - jordanian_hashemite_regime: External beneficiary (institutional/constrained) — treaty stability premised on the depth framework
 *   - us_foreign_policy_establishment: Patron beneficiary (institutional/arbitrage) — insures the frame, spends diplomatic capital doing so
 *   - international_law_bodies: Excluded objector (institutional/analytical) — rulings register outside the conversation that produces the doctrine's legitimacy
 *   - israeli_dissenting_security_veterans: Analytical observer (organized/analytical) — retired officers auditing the doctrine's security accounting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.48).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Security-Necessity Reading of Territorial Legitimacy (Post-1967 Depth Doctrine)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political theory/international law/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, '106c1ecb-eb02-485a-b459-77d701513e46').
narrative_ontology:cs_kernel_codification('106c1ecb-eb02-485a-b459-77d701513e46', distributed).
narrative_ontology:cs_authority_grounding('106c1ecb-eb02-485a-b459-77d701513e46', expertise).
narrative_ontology:cs_interpretation_layer_present('106c1ecb-eb02-485a-b459-77d701513e46').
narrative_ontology:cs_reading_relation('106c1ecb-eb02-485a-b459-77d701513e46', territorial_legitimacy__partition_reading, influences).
narrative_ontology:cs_reading_relation('106c1ecb-eb02-485a-b459-77d701513e46', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('106c1ecb-eb02-485a-b459-77d701513e46', foundational, territorial_control_legitimate_when_security_required).
narrative_ontology:cs_axiom_status(territorial_control_legitimate_when_security_required, holdable).
narrative_ontology:cs_axiom_grounding('106c1ecb-eb02-485a-b459-77d701513e46', territorial_control_legitimate_when_security_required, empirically_contingent).
narrative_ontology:cs_axiom('106c1ecb-eb02-485a-b459-77d701513e46', foundational, sovereignty_conditional_on_demilitarization).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_demilitarization, holdable).
narrative_ontology:cs_axiom_grounding('106c1ecb-eb02-485a-b459-77d701513e46', sovereignty_conditional_on_demilitarization, instrumental).
narrative_ontology:cs_reference_frame('106c1ecb-eb02-485a-b459-77d701513e46', post_1967_secure_depth_framework).
narrative_ontology:cs_drift_state('106c1ecb-eb02-485a-b459-77d701513e46', post_october_2023_assessments, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('106c1ecb-eb02-485a-b459-77d701513e46', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, west_bank_settlement_movement).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, golan_settler_communities).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, jordanian_hashemite_regime).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, us_foreign_policy_establishment).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_residents_west_bank).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_authority_leadership).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, syrian_golan_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, palestinian_authority_leadership).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, defensible_borders_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, strategic_depth_principle).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, demilitarized_sovereignty_conditionality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produces the threat assessments that define how much territory and control count as necessary, runs the military administration over the occupied territories, and decides what qualifies as a security measure. Draws budget, mission scope, and decision authority from the arrangement. Revising the doctrine would mean re-litigating its own core mandate; internal dissent exists among retired officers, but the active institution maintains the frame.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_security_establishment, agenda_setter,
    institutional, generational, constrained, national).

% Lives and builds beyond the 1949 armistice lines under the arrangement's protective and legal cover; receives land, housing, infrastructure, and army protection. Leaving would mean evacuation — the 2005 Gaza withdrawal is the community's reference trauma — and its members' self-understanding is bound to remaining on the land.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, west_bank_settlement_movement, beneficiary,
    organized, generational, identity_locked, regional).

% Resides on the plateau annexed in 1981 under the same security rationale; agriculture, water access, and a permanent-status trajectory flow from the arrangement. Identity and livelihood are tied to staying, and the community campaigns domestically for retained sovereignty.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, golan_settler_communities, beneficiary,
    organized, generational, identity_locked, regional).

% Live under military administration with movement governed by permits and checkpoints, farming and building access restricted across much of the land, and any future statehood offered only on terms set elsewhere. Exit options are effectively nil: no sovereign passport, bounded employment, and emigration means abandoning home and claim.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_residents_west_bank, payer,
    powerless, biographical, trapped, regional).

% Administers populated enclaves under interim arrangements and coordinates daily security with the Israeli military, on which its survival against rivals partly depends, while its public standing erodes because final status remains conditional and out of its hands. Dissolving itself forfeits governing capacity; escalating breaks the coordination its finances depend on.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_authority_leadership, payer,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, palestinian_authority_leadership, beneficiary).

% Holds a return claim that the security frame classifies as incompatible with the controlling state's character; lives in camps and host states across the region with no negotiating seat. Exit means formalizing permanent exile.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, global).

% Druze residents of the annexed plateau, most of whom declined offered citizenship and retain Syrian identification; farmland was expropriated in the early annexation years, followed by gradual economic integration and an unresolved legal status. Movement and family ties across the disengagement line are tightly limited.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, syrian_golan_residents, payer,
    powerless, generational, trapped, local).

% Signed peace and security-coordination arrangements that depend on the controlling state's strength and predictability; draws border stability and intelligence cooperation from the arrangement while absorbing domestic public-opinion costs for the relationship. Renegotiating means reopening its own security architecture.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, jordanian_hashemite_regime, beneficiary,
    institutional, generational, constrained, national).

% Provides military aid, diplomatic cover, and security assurances that presuppose the depth framework; spends substantial diplomatic capital defending the arrangement in international forums and periodically attempts to broker adjustments to its terms. It can shift support levels, redirect aid, or change the terms it offers — leverage the other seats lack.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, us_foreign_policy_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Security Council resolutions and the International Court of Justice have ruled or opined that the occupation and settlement enterprise violate international law; these findings carry weight in forums the arrangement's beneficiaries do not control but have no execution mechanism inside the arrangement's day-to-day operation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_law_bodies, excluded,
    institutional, generational, analytical, global).

% Retired generals, intelligence chiefs, and pilots who publicly audit the doctrine's security accounting — arguing that settlement defense consumes forces, erodes reserve morale, and complicates any future withdrawal. They hold no administrative power and neither collect from nor bear the arrangement's direct costs.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_dissenting_security_veterans, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__security_necessity_reading, west_bank_settlement_movement).
narrative_ontology:fixing_cost_class(territorial_legitimacy__security_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single criterion — what territory and control security requires — that aligns military planning, settlement policy, alliance management, and domestic politics around one answer, and structures day-to-day security coordination between the controlling military and neighboring or interim authorities.
% TRANSFER_FUNCTION: Moves land-use and movement authority in the occupied territories from resident populations to the controlling state's security planners; moves international legitimacy and material support toward the controlling state; moves exposure to attack outward from the state's population core onto the territories' residents.
% ABSENT_VOICES: Palestinian residents of the territories, the refugee diaspora, and international legal bodies would object to the terms on which the arrangement's legitimacy is assessed; they sit outside the rooms where the doctrine is produced — the security cabinet, military planning, and patron-government deliberations — and register only as external pressure or protest.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, the settlement enterprise would lose its legitimating frame, the military administration would face immediate legal and political challenge, patron support terms would be renegotiated, and final-status talks would reorganize around a different legitimacy source — the regional architecture built on the depth framework would not survive intact.
% FOUNDING_PROBLEM: After 1949 the state held armistice lines nine miles wide at the waist, faced coalitions pledging its dissolution, and fought two wars (1948, 1967) launched or joined against it; the arrangement was built to answer how a small state secures defensible borders against annihilation-scale threats.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's reality is corroborated from outside the benefiting parties: pre-1967 Arab coalition statements, Egyptian blockade records, US State Department cables, and Jordanian and Egyptian treaty negotiators all attest the existential-threat environment the doctrine answered. Its current liveness has no neutral attestation: the controlling state and its patron attest continued threat, while the International Court of Justice and Security Council majorities attest the arrangement's illegality — corroboration of present-day necessity is partisan on every side, which is itself the finding.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.48 from this reading's own lights: the doctrine's core (buffer control answering a real, externally corroborated threat environment) is treated as legitimate defense, but the arrangement has accumulated components even a security-first assessment counts as costs borne by others — settlement placement that much of the security profession calls a net liability, permit-economy dependencies, and a conditionality on Palestinian statehood that has never been priced as temporary. Suppression (0.72) is predominantly structural: military orders, permit regimes, and the barrier are physical and legal architecture, not belief; a minority share is internalized normalization among residents who have never known another system. Theater (0.32): the security function is real (coordination, early warning, demilitarization enforcement), but a growing share of security-invoked activity defends settlement consolidation that the security establishment's own veterans audit as a liability. Accessibility collapse (0.58): inside the frame, withdrawal alternatives read as existential risk and collapse almost completely; the frame itself remains contestable, so collapse is partial at the system level. Resistance (0.60): two intifadas, sustained diplomatic campaigns, court challenges, and internal veteran dissent. The measurement series run on one shared eight-point grid (t=0..57 maps 1967..2024). The oscillation — Madrid/Oslo relief at t=24-32, second-intifada ratchet at t=40 — is crisis-driven: each attack hardens enforcement, each diplomatic opening relaxes it, and each cycle leaves infrastructure behind; the ratchet component, not the oscillation itself, carries the extraction. Coalition check: payer seats are fragmented across polities (enclave governance, a stateless diaspora, annexed Druze residents), so diffuse costs have never converted into concentrated bargaining power; the arrangement's stability partly reflects that fragmentation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as defense it operates: threat assessment, deployment, coordination — a working machine it runs. Trapped payer seats experience the same permit, checkpoint, and settlement architecture as the fixed condition of their lives. The conditioned-intermediary seat straddles: it depends on the machine's coordination output while paying in legitimacy. The engine computes these divergent per-seat types from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the security establishment, settlement movement, Golan communities, Jordanian regime, and US patron near the beneficiary end (low d). Victim declarations place West Bank residents, the diaspora, and Syrian Golan residents near the target end (high d), amplified by trapped exit options. One override is authored: palestinian_authority_leadership (moderate) at d=0.62 — the derivation from its victim declaration would overshoot toward full-target because the leadership's survival depends on the very security coordination the arrangement provides; the override damps d to reflect that dual position. The US patron sits slightly above pure beneficiary (it expends diplomatic capital enforcing the frame), but it shares the institutional power atom with the security establishment, whose d is genuinely lower; the atom-keyed override surface cannot separate them, so no override is authored and the residual imprecision is recorded here rather than forced. Excluded and observer seats are not directionality-bearing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — indefensible armistice lines against annihilation-scale coalitions — was real and is corroborated from adversary-side and third-party records. The doctrine solved it: no coalition has since attempted the 1948-style conventional scenario. Whether the mandate is dead is contested: standoff-weapons evolution argues depth no longer mitigates the principal threats, while ground-incursion and northern-front scenarios argue it retains value. Authoring tangled_rope keeps both truths load-bearing: the coordination core (real security coordination, a real threat problem) blocks a pure-extraction mislabel, and the victim declarations block a pure-coordination mislabel. Mandatrophy is not declared resolved — the founding problem's status is contested, not dead — and the depth-versus-threat-evolution omega carries the resolution path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the security_necessity_reading of the territorial_legitimacy kernel; what would the partition_reading and indigenous_continuity_reading change structurally?',
    'Compare the sibling stories'' authored epsilon and victim sets over the same standing arrangement; the disagreement is located in the legitimacy source (recognized borders versus security requirement versus continuous habitation) and in whose costs count.',
    'Under partition_reading, epsilon rises (control beyond recognized lines is framework-violating) and the vindicated-proposition set shrinks; under indigenous_continuity_reading, epsilon rises further and the victim set extends back to the 1948 displacement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this constraint is one reading of the territorial_legitimacy kernel; sibling readings instantiate different constraints.').

omega_variable(
    depth_vs_threat_evolution,
    'Does territorial depth still mitigate the principal threats (standoff missiles, drones, tunnels, cyber), or has threat evolution overtaken the depth premise?',
    'Independent strategic studies correlating threat delivery systems with territorial-depth utility; compare the pre-1967 artillery-line threat environment with current standoff capabilities.',
    'If depth no longer mitigates the principal threats, the doctrine''s coordination function decays and the arrangement trends toward theatrical maintenance of a legacy rationale; if depth retains value against ground-incursion scenarios, the coordination core is live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(depth_vs_threat_evolution, empirical, 'Whether the founding security premise still holds against evolved threat delivery.').

omega_variable(
    conditionality_permanence,
    'Is demilitarized-sovereignty conditionality a transitional safeguard or a permanent ceiling on Palestinian statehood?',
    'Track whether published frameworks attach review or sunset conditions to the demilitarization requirement, and whether any negotiated package has ever treated it as temporary.',
    'A transitional reading supports a scaffold-like element (justification is the transition); a permanent-ceiling reading converts the conditionality into standing extraction borne by the sovereignty claimants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_permanence, empirical, 'Permanence versus transience of the sovereignty conditionality.').

omega_variable(
    security_rhetoric_coverage,
    'What share of security-invoked measures (settlement expansion, bypass roads, land declarations) serve verifiable security functions versus settlement consolidation?',
    'Compare measure incidence inside versus outside designated security zones; audit the military cost of defending settlements; cross-check official security citations against operational assessments.',
    'Refines theater_ratio; a high share would push the arrangement toward performance-maintained legacy status and strengthen the drift-toward-theatricality hypothesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_rhetoric_coverage, empirical, 'Boundary between genuine security function and security-framed consolidation.').

omega_variable(
    payer_coalition_counterfactual,
    'Would unified Palestinian representation (residents, diaspora, interim leadership) alter the arrangement''s stability by converting diffuse payer costs into concentrated bargaining power?',
    'Historical comparison of negotiation outcomes under unified versus fragmented representation (pre-split unified-PLO era versus post-split era).',
    'If coalition formation would materially raise payer power, part of the measured stability reflects fragmentation maintenance, raising effective extraction for payer seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(payer_coalition_counterfactual, empirical, 'Counterfactual effect of payer-coalition formation on the arrangement''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__security_necessity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(terr_tr_t8, territorial_legitimacy__security_necessity_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(terr_tr_t16, territorial_legitimacy__security_necessity_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(terr_tr_t24, territorial_legitimacy__security_necessity_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(terr_tr_t32, territorial_legitimacy__security_necessity_reading, theater_ratio, 32, 0.16).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy__security_necessity_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(terr_tr_t48, territorial_legitimacy__security_necessity_reading, theater_ratio, 48, 0.31).
narrative_ontology:measurement(terr_tr_t57, territorial_legitimacy__security_necessity_reading, theater_ratio, 57, 0.32).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__security_necessity_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(terr_be_t8, territorial_legitimacy__security_necessity_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(terr_be_t16, territorial_legitimacy__security_necessity_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(terr_be_t24, territorial_legitimacy__security_necessity_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(terr_be_t32, territorial_legitimacy__security_necessity_reading, base_extractiveness, 32, 0.34).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy__security_necessity_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(terr_be_t48, territorial_legitimacy__security_necessity_reading, base_extractiveness, 48, 0.47).
narrative_ontology:measurement(terr_be_t57, territorial_legitimacy__security_necessity_reading, base_extractiveness, 57, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__security_necessity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(terr_su_t8, territorial_legitimacy__security_necessity_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(terr_su_t16, territorial_legitimacy__security_necessity_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(terr_su_t24, territorial_legitimacy__security_necessity_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(terr_su_t32, territorial_legitimacy__security_necessity_reading, suppression_requirement, 32, 0.46).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy__security_necessity_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(terr_su_t48, territorial_legitimacy__security_necessity_reading, suppression_requirement, 48, 0.7).
narrative_ontology:measurement(terr_su_t57, territorial_legitimacy__security_necessity_reading, suppression_requirement, 57, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'territorial legitimacy in Israel/Palestine' decomposes into three structurally distinct constraints sharing one kernel: partition_reading (epsilon keyed to recognized-border fidelity), security_necessity_reading (this file; epsilon keyed to security-required control, lowest of the three), and indigenous_continuity_reading (epsilon keyed to habitation continuity, highest, victim set extending to 1948). Each story holds a single stable epsilon over the same standing arrangement; the upstream/downstream pressure runs from whichever reading commands facts on the ground toward the others' feasibility conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
