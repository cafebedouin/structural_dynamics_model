% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__religious_restoration_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Zionism as Religious Restoration (Post-1967 Reading)
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   This constraint models the religious Zionist interpretation of Zionism,
 *   particularly after the 1967 Six-Day War, which views the establishment
 *   and expansion of Israel as a fulfillment of divine promise and an
 *   acceleration of the messianic process. This reading prioritizes religious
 *   obligation over secular political considerations, leading to a
 *   theological mandate for territorial maximalism. It is a snare because it
 *   actively extracts land and rights from the Palestinian population,
 *   justified by a non-negotiable divine imperative, and requires significant
 *   active enforcement to maintain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.85).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.9).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Zionism as Religious Restoration (Post-1967 Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, '30aab67d-3e36-4706-be97-e090ed10b428').
narrative_ontology:cs_kernel_codification('30aab67d-3e36-4706-be97-e090ed10b428', implicit).
narrative_ontology:cs_authority_grounding('30aab67d-3e36-4706-be97-e090ed10b428', lineage).
narrative_ontology:cs_interpretation_layer_present('30aab67d-3e36-4706-be97-e090ed10b428').
narrative_ontology:cs_reading_relation('30aab67d-3e36-4706-be97-e090ed10b428', zionist_legitimacy_basis__national_liberation_reading, influences).
narrative_ontology:cs_reading_relation('30aab67d-3e36-4706-be97-e090ed10b428', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('30aab67d-3e36-4706-be97-e090ed10b428', foundational, divine_mandate_for_land_redemption).
narrative_ontology:cs_axiom_status(divine_mandate_for_land_redemption, holdable).
narrative_ontology:cs_axiom_grounding('30aab67d-3e36-4706-be97-e090ed10b428', divine_mandate_for_land_redemption, theological).
narrative_ontology:cs_axiom('30aab67d-3e36-4706-be97-e090ed10b428', foundational, human_agency_accelerates_messianic_era).
narrative_ontology:cs_axiom_status(human_agency_accelerates_messianic_era, holdable).
narrative_ontology:cs_axiom_grounding('30aab67d-3e36-4706-be97-e090ed10b428', human_agency_accelerates_messianic_era, theological).
narrative_ontology:cs_reference_frame('30aab67d-3e36-4706-be97-e090ed10b428', biblical_covenant_and_messianic_prophecy).
narrative_ontology:cs_drift_state('30aab67d-3e36-4706-be97-e090ed10b428', contemporary_political_realities, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('30aab67d-3e36-4706-be97-e090ed10b428', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_political_parties).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_left).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively participate in and benefit from the expansion of settlements in the West Bank, viewing it as a divine commandment. Their identity is deeply intertwined with the theological mandate for territorial control, making exit unthinkable.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers, beneficiary,
    organized, generational, identity_locked, regional).

% Formulate and implement policies that prioritize religious and messianic interpretations of territorial claims, often at the expense of diplomatic or security considerations. They leverage their political power to advance the settlement enterprise and resist any territorial concessions.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_political_parties, agenda_setter,
    institutional, generational, constrained, national).

% Experience displacement, land confiscation, and restrictions on movement and self-determination as a direct consequence of the religiously mandated settlement expansion. Their resistance is met with active enforcement by the state.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_population, payer,
    powerless, generational, trapped, regional).

% Bear the political and social costs of international isolation and internal conflict resulting from policies driven by religious Zionist ideology. They advocate for a two-state solution and secular governance but face diminishing political influence.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_left, payer,
    moderate, biographical, constrained, national).

% Observes and often condemns the settlement expansion as a violation of international law, but its diplomatic and economic pressures have limited impact on the religiously motivated actors who view their actions as divinely sanctioned.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_political_parties).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__religious_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions of religious Zionist groups and political factions around a shared theological vision of territorial redemption and messianic fulfillment, providing a coherent ideological framework for settlement expansion.
% TRANSFER_FUNCTION: Transfers land, resources, and political power from the Palestinian population and secular Israeli institutions to religious Zionist settlers and political entities, justified by a divine mandate.
% ABSENT_VOICES: Palestinian voices are systematically excluded from the decision-making processes that determine their fate. International legal frameworks and human rights advocates are often dismissed as irrelevant by those operating under a divine mandate.
% DISAPPEARANCE_RATIONALE: If the religious Zionist interpretation of divine promise vanished, the primary ideological justification for settlement expansion would collapse. This would fundamentally alter the political landscape, potentially leading to a re-evaluation of territorial claims, a shift in national priorities, and a significant reduction in conflict drivers, forcing a rearrangement of regional power dynamics.
% FOUNDING_PROBLEM: The perceived incompleteness of the Zionist project after 1948, specifically the failure to control all biblical lands, and a theological imperative to accelerate messianic redemption through active human agency.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist leaders and their followers attest that the problem is profoundly live, citing ongoing divine commandments and messianic expectations. Critics, including some secular Israelis and international observers, acknowledge the theological framing but dispute its political legitimacy, viewing it as a manufactured problem to justify expansionism. Corroboration for the 'live' status comes from the continued, fervent adherence to these beliefs by a significant and politically influential segment of the population, rather than external validation of the theological premise itself.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high due to the continuous appropriation of land and resources from Palestinians. Suppression (0.90) is also very high, reflecting the extensive military and administrative control required to enforce settlement expansion and manage Palestinian resistance. The theater ratio (0.10) is low because the religious justification is genuinely held and directly drives policy, with minimal performative cover for other motives. Accessibility collapse (0.75) is high as alternatives for Palestinians (e.g., self-determination, return) are systematically foreclosed by the theological framework. Resistance (0.80) is high, reflecting ongoing Palestinian struggle against the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious Zionist settlers, this is a divinely mandated process, a 'mountain' of theological truth. From the Palestinian perspective, it is a 'snare' of dispossession and oppression. The engine's classification as a snare reflects the objective structural extraction and suppression, regardless of the internal justification of the beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious Zionist settlers and political parties are clear beneficiaries, gaining land, political power, and a sense of divine purpose. The Palestinian population is the primary victim, experiencing direct loss and subjugation. The secular Israeli left also bears costs through international condemnation and internal societal division. The international community acts as an observer, often condemning but largely unable to alter the religiously grounded actions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_empirical_status,
    'Is the ''divine promise'' and ''messianic process'' an empirically verifiable claim, or a theological/ideological construct?',
    'No empirical resolution possible; depends on theological/epistemological framework. Resolution would require a shift in the accepted epistemic grounding of political action.',
    'If treated as a purely theological construct, the constraint''s legitimacy shifts from ''natural law'' to ''ideological choice,'' potentially reclassifying it from a perceived mountain to a snare for those outside the belief system. If accepted as empirically true (within a specific theological framework), it reinforces the mountain-like perception for adherents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_empirical_status, conceptual, 'The epistemic status of the divine mandate underpinning the constraint.').

omega_variable(
    territorial_maximalism_necessity,
    'Is territorial maximalism (control over all biblical lands) a necessary component of the messianic process, or an interpretation that could be revised?',
    'Internal theological debate and reinterpretation by authoritative religious figures, or a shift in the political-theological consensus within religious Zionism.',
    'If territorial maximalism is deemed non-essential or revisable, it could open pathways for territorial compromise, reducing the constraint''s extractiveness and suppression. If it remains non-negotiable, the snare-like qualities persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_maximalism_necessity, preference, 'The revisability of territorial maximalism within the religious Zionist framework.').

omega_variable(
    identity_lock_durability,
    'How deeply is the identity of religious Zionist settlers fused with the territorial claims, and how resilient is this fusion to external pressure or internal theological re-evaluation?',
    'Sociological studies on identity formation and resilience, analysis of responses to past disengagement efforts, and shifts in religious educational curricula.',
    'A strong, resilient identity-lock amplifies the effective suppression and extractiveness, making exit or compromise extremely difficult. A weaker, more flexible identity-lock would make the constraint more amenable to change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'The strength and resilience of identity-lock for key beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(zion_tr_t1980, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(zion_tr_t1995, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(zion_tr_t2010, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(zion_be_t1980, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(zion_be_t1995, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(zion_be_t2010, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(zion_su_t1980, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(zion_su_t1995, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1995, 0.82).
narrative_ontology:measurement(zion_su_t2010, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, israeli_palestinian_conflict_dynamics).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, international_law_enforcement_in_occupied_territories).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'zionist_legitimacy_basis' kernel. It focuses on the religious-messianic justification, distinct from secular national liberation or settler-colonial analyses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
