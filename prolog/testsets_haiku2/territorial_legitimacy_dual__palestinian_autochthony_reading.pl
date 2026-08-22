% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Territorial Legitimacy — Autochthony and Displacement Remedy Reading
 *   domain: political_theory/territorial_sovereignty/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested kernel of
 *   territorial legitimacy in Israel/Palestine. The autochthony reading
 *   grounds Palestinian legitimacy in three claims: (1) continuous habitation
 *   and ancient connection to the land, (2) the 1948 displacement as an
 *   ongoing, unresolved injustice rather than closed history, and (3) the
 *   right of return as a non-negotiable remedy. Under this reading, the
 *   Israeli state's territorial control and the exclusion of Palestinian
 *   return constitute a structural snare — an arrangement that persists only
 *   through active suppression (military occupation, settlement expansion,
 *   refugee-camp containment, diplomatic isolation of return claims) and
 *   whose benefits accrue to no Palestinian seat (no beneficiary, only
 *   victims). The constraint's referent, for this reading, is the standing
 *   territorial and political arrangement that excludes return; the reading
 *   assesses it as deeply extractive. This is NOT a neutral description of
 *   the situation — it is one of three reading-specific constraint stories in
 *   the territorial_legitimacy_dual kernel. Sibling readings
 *   (zionist_refuge_reading, two_state_coexistence_reading) author different
 *   constraints on the same kernel with different ε values, beneficiary
 *   structures, and claims.
 *
 * KEY AGENTS:
 *   - Palestinian displaced persons (1948+): identity-locked to return claim, powerless, globally dispersed
 *   - Palestinian remaining residents (West Bank, Gaza): trapped by occupation, excluded from political process
 *   - Israeli state: institutional power, agenda-setter, enforces boundary exclusion
 *   - International legal authorities: observe the structure but lack enforcement capacity
 *   - Palestinian national authority: structurally subordinated, identity-locked to liberation narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.89).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.91).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.83).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Territorial Legitimacy — Autochthony and Displacement Remedy Reading").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political_theory/territorial_sovereignty/international_relations").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, 'e3990406-4e55-429c-9cb7-360e46c89ed3').
narrative_ontology:cs_kernel_codification('e3990406-4e55-429c-9cb7-360e46c89ed3', distributed).
narrative_ontology:cs_authority_grounding('e3990406-4e55-429c-9cb7-360e46c89ed3', distributed).
narrative_ontology:cs_reading_relation('e3990406-4e55-429c-9cb7-360e46c89ed3', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3990406-4e55-429c-9cb7-360e46c89ed3', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('e3990406-4e55-429c-9cb7-360e46c89ed3', foundational, displacement_as_ongoing_injustice).
narrative_ontology:cs_axiom_status(displacement_as_ongoing_injustice, holdable).
narrative_ontology:cs_axiom_grounding('e3990406-4e55-429c-9cb7-360e46c89ed3', displacement_as_ongoing_injustice, deontological).
narrative_ontology:cs_axiom('e3990406-4e55-429c-9cb7-360e46c89ed3', foundational, right_of_return_as_non_negotiable).
narrative_ontology:cs_axiom_status(right_of_return_as_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('e3990406-4e55-429c-9cb7-360e46c89ed3', right_of_return_as_non_negotiable, deontological).
narrative_ontology:cs_axiom('e3990406-4e55-429c-9cb7-360e46c89ed3', secondary, autochthony_as_legitimacy_ground).
narrative_ontology:cs_axiom_status(autochthony_as_legitimacy_ground, holdable).
narrative_ontology:cs_axiom_grounding('e3990406-4e55-429c-9cb7-360e46c89ed3', autochthony_as_legitimacy_ground, conventional).
narrative_ontology:cs_reference_frame('e3990406-4e55-429c-9cb7-360e46c89ed3', pre_1948_habitation_and_territorial_integrity).
narrative_ontology:cs_drift_state('e3990406-4e55-429c-9cb7-360e46c89ed3', contemporary_post_nakba_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e3990406-4e55-429c-9cb7-360e46c89ed3', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_displaced_persons).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_remaining_residents).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_national_authority).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_civil_society).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_national_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons expelled from Palestine in 1948 and their descendants (estimated 5+ million). Bear the deprivation of displacement: loss of property, community, agricultural land, and the claim to return. Exit is framed as impossible — returning is the constitutional demand, not an option to exit. Identity is constituted through the loss and the claim to remedy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_displaced_persons, payer,
    powerless, generational, identity_locked, global).

% Palestinians living in the West Bank and Gaza. Confined by settlement zones, military orders, and the fragmented territorial allocation. Bear the cost of continuous subordination to occupation and territorial reduction. Excluded from the political process that determines territory allocation and resource distribution.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_remaining_residents, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_remaining_residents, excluded).

% Palestinians living in diaspora (Lebanon, Syria, Jordan, Gulf states, Americas, Europe). Carry the political and emotional burden of displacement and the claim to return. Constrained by passport status and lack of stable legal residence; return remains framed as the only legitimate resolution.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora, payer,
    moderate, generational, constrained, global).

% Exercises control over the territory and enforces the boundary that excludes Palestinian return. Maintains the territorial allocation through military and administrative enforcement. Legitimates this arrangement through an alternative reading of territorial basis (refuge, UN partition acceptance, historical connection). Can exercise options (border policy, settlement expansion, negotiation) unavailable to payers.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% UN bodies, international courts, and human rights mechanisms that document displacement, recognize refugee status, and issue judgments on right of return. They see the structure but lack enforcement power over the agenda-setter. Their authority is contested by the Israeli reading.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_legal_authorities, observer,
    institutional, generational, analytical, global).

% Exercises limited administrative authority under occupation and negotiated agreements. Positioned as representative of Palestinian interests but lacks independent enforcement capacity or territorial control. Benefits from the framing of Palestinian legitimacy but remains structurally constrained and subordinated. Identity locked to the liberation narrative.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_national_authority, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_national_authority, payer).

% NGOs, grassroots movements, and diaspora organizations that maintain the narrative of displacement remedy and keep the claim alive internationally. Benefit from the framing of autochthony and return as legitimate; constrained by Israeli military rule and international political limits on advocacy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_civil_society, beneficiary,
    organized, generational, constrained, global).

% UN Refugee Agency, international humanitarian law framework, and refugee-status conventions that recognize Palestinian displacement. Documents the injustice and the refugee claim but cannot compel territorial remedy. Analytical seat: sees the structure but operates under constraints from other geopolitical actors.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, global_refugee_protection_regime, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Palestinian collective identity and territorial claim across generations and geographies: unifies the diaspora, the remaining residents, and the stateless under a shared narrative of autochthony, loss, and remedy. Coordinates political mobilization around return as the non-negotiable terminus of justice.
% TRANSFER_FUNCTION: Extracts from Palestinian populations — displaced persons, diaspora, residents under occupation — the deprivation of territory, property, political agency, and the constitutional inability to return. Transfers the benefit of territorial control, resource extraction, and political authority to the Israeli state and settler infrastructure.
% ABSENT_VOICES: Israeli settlers, whose presence depends on displacement, are not present in Palestinian-centered accounts of this constraint; Palestinian diaspora in non-refugee-camp settings (integrated into other societies) are marginalized or excluded from the return narrative; Palestinians who accept territorial compromise are structurally erased from the autochthony reading.
% DISAPPEARANCE_RATIONALE: If the constraint — the exclusion of return and the territorial reduction — disappeared, the political order would be catastrophically reorganized: either Palestinians would return (displacing Israeli population), or the state structure would be fundamentally reconstituted. The world of 2026, in this reading, depends entirely on the maintenance of Palestinian deprivation.
% FOUNDING_PROBLEM: The displacement and territorial reduction of Palestinian people in 1948 and afterward, treated as an ongoing, unresolved injustice requiring territorial and political remedy — not as a closed historical event.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies, UN General Assembly resolutions, International Court of Justice advisory opinions, and Palestinian civil society all attest that displacement remains unresolved and remedy is outstanding. However, the Israeli reading contests both the founding-problem framing and its ongoing status, locating legitimacy elsewhere. Significant portions of the global majority (Global South states, Arab League, African Union) corroborate the Palestinian reading from outside the Palestinian state structure.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.89) because the constraint moves deprivation — land, property, political agency, constitutional return — from Palestinian populations to Israeli state control, with no reciprocal Palestinian benefit. This is not negotiated resource allocation; it is territorial seizure with displacement of the prior population. Suppression is extremely high (0.91) because the constraint's persistence depends on active enforcement: military occupation, settlement law, border closure, refugee-camp administration, and the physical prevention of return. Without continuous coercive apparatus, Palestinians would exercise the return claim. The measurement series show rising suppression (1948→2026) as enforcement infrastructure hardens, and rising theater_ratio as peace-process negotiation creates the appearance of remedy (process theater) while exclusion remains structural. The accessibility_collapse (0.78) is high but not maximal because return remains a live political claim — the alternative (remaining displaced or accepting territorial reduction) is never fully collapsed, only forcibly maintained. Resistance is high (0.83) because Palestinian political mobilization, international advocacy, legal claims, and armed resistance persistently contest the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the Palestinian payer seats (displaced persons, remaining residents, diaspora), the constraint is an ongoing deprivation requiring remedy. From the Israeli agenda-setter seat, the constraint is a legitimate territorial and security arrangement justified through a different reading of legitimacy (refuge, partition acceptance, historical connection). The engine computes these seats' classifications independently from the structural data: Palestinian seats will compute as targets of extraction (high d toward 1.0); Israeli state will compute as beneficiary and enforcer (d toward 0.0); international observers will compute as symmetric on the constraint itself (not governed by it). This per-seat divergence is structural, not an analyst error.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian displaced, remaining, and diaspora populations are structural targets: the constraint extracts from them (territorial loss, political exclusion, identity-locked immobility) and provides no benefit. Their directionality is near 1.0 (full target). The Israeli state is the structural beneficiary: it collects territorial control, security from the absence of return competition, resource extraction from settlements, and the political authority to exclude. Its directionality is near 0.0 (full beneficiary). The Palestinian National Authority sits in a contradictory position: it benefits from the legitimacy frame (representatives of Palestinian claim) but remains structurally subordinated and unable to deliver remedy. Its directionality would compute as ambiguous (0.4–0.6), making it a site of internal tension. International authorities have no structural position in the constraint itself — they are analytical observers (d ≈ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — displacement without remedy — remains live in this reading and maps directly to the constraint (exclusion of return, territorial reduction). There is no atrophied function or purely theatrical maintenance. However, the theater_ratio rises over 78 years (0.15 → 0.42) as peace-process negotiation, two-state frameworks, and humanitarian gestures create the appearance of remedy while the structural constraint remains. This is not piton-grade inertia (no executor wanting to dismantle it from mere cost); it is active maintenance by the Israeli state for territorial benefit. The classification holds at snare throughout the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.22).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1987, 0.28).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(terr_tr_t2026, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.82).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.87).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1987, 0.88).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.89).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2015, 0.9).
narrative_ontology:measurement(terr_be_t2026, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2026, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.79).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1987, 0.88).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2015, 0.92).
narrative_ontology:measurement(terr_su_t2026, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2026, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_settlement_expansion_constraint).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, refugee_camp_confinement_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the territorial_legitimacy_dual kernel. The autochthony reading authors ε=0.89 (highly extractive); the zionist reading authors a lower ε (~0.42, coordination frame); the two-state reading authors intermediate ε (~0.65, hybrid). All three share the same referent (the territorial and political arrangement) but assess it through different normative lenses. The three stories are linked via network.affects_constraints and should be consumed together as a constraint family, not separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__palestinian_autochthony_reading, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
