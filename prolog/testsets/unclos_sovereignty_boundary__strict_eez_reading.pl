% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS Article 57 Strict EEZ Boundary Reading
 *   domain: geopolitical/maritime/international-law
 *
 * SUMMARY:
 *   The Exclusive Economic Zone (EEZ) boundary under UNCLOS Article 57
 *   represents one reading of how sovereign maritime rights should be
 *   allocated. Under the strict reading instantiated here, coastal states
 *   claim exclusive resource control and navigation authority within 200
 *   nautical miles of their baseline, with no overlay claims recognized. This
 *   reading emerged from the 1982 UNCLOS negotiation as a compromise: it
 *   unified maritime zones and provided clear demarcation, solving pre-treaty
 *   chaos. However, it simultaneously created new extraction monopolies and
 *   suppressed alternative frameworks (historical rights, customary freedom
 *   of navigation, indigenous maritime practices). The strict reading is
 *   actively enforced through naval patrols, vessel inspections, and
 *   seizures; it is buttressed by the ICJ/ITLOS institutional machinery; it
 *   benefits coastal states and resource extraction operators; and it imposes
 *   costs on overlapping claimants, non-ratifier naval powers, and high-seas
 *   fishing interests. The measurement series show rising extractiveness over
 *   the 50-year interval (0.45 → 0.68), indicating that licensing fees,
 *   resource rents, and suppression of alternatives have accumulated, while
 *   the theater ratio remains modest (0.28), suggesting the constraint's
 *   coordination function (solving maritime chaos) remains credible even as
 *   extraction accumulates. This is not the only reading of UNCLOS
 *   sovereignty — the sibling readings (historical rights, non-ratifier
 *   enforcement) instantiate structurally distinct constraints with different
 *   beneficiary/victim sets and different suppression mechanisms.
 *
 * KEY AGENTS:
 *   - Coastal state governments (institutional power, arbitrage exit) — agenda setters, beneficiaries of exclusive control; set and enforce EEZ boundaries
 *   - Overlapping claimant states (institutional power, constrained exit) — victims; their historical claims are voided by the strict reading; high exit cost (litigating or rejecting UNCLOS)
 *   - High-seas fishing fleets (powerful, constrained exit) — victims; lose prime fishing grounds relocated to EEZ; can fish beyond 200nm but at higher cost
 *   - Non-ratifier naval powers (institutional power, trapped) — payers/observers; claim customary freedom of navigation but face interception; structurally trapped between asserting rights (costly brinkmanship) and surrendering them
 *   - Commercial fishing interests and offshore extractors (powerful, mobile) — beneficiaries; gain secure, exclusive resource access and licensing revenue; mobile exit but prefer secure frame
 *   - UNCLOS treaty administration (ICJ/ITLOS, institutional, analytical) — agenda setter; administers the strict reading by issuing boundary rulings; have analytical exit but adopted the reading as canonical
 *   - Indigenous maritime communities (powerless, trapped, identity-locked) — excluded; pre-UNCLOS subsistence practices subordinated to coastal licensing; structurally absent from rule-making; identity-locked loss (traditions encoded in place and practice)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.71).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS Article 57 Strict EEZ Boundary Reading").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "geopolitical/maritime/international-law").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, 'd7753154-2f3b-4ad7-ba5b-6f44c96464f8').
narrative_ontology:cs_kernel_codification('d7753154-2f3b-4ad7-ba5b-6f44c96464f8', fixed_text).
narrative_ontology:cs_authority_grounding('d7753154-2f3b-4ad7-ba5b-6f44c96464f8', extraction).
narrative_ontology:cs_interpretation_layer_present('d7753154-2f3b-4ad7-ba5b-6f44c96464f8').
narrative_ontology:cs_reading_relation('d7753154-2f3b-4ad7-ba5b-6f44c96464f8', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7753154-2f3b-4ad7-ba5b-6f44c96464f8', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('d7753154-2f3b-4ad7-ba5b-6f44c96464f8', foundational, unclos_article_57_supremacy).
narrative_ontology:cs_axiom_status(unclos_article_57_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('d7753154-2f3b-4ad7-ba5b-6f44c96464f8', unclos_article_57_supremacy, conventional).
narrative_ontology:cs_axiom('d7753154-2f3b-4ad7-ba5b-6f44c96464f8', foundational, bright_line_boundary_eliminates_overlap_claims).
narrative_ontology:cs_axiom_status(bright_line_boundary_eliminates_overlap_claims, holdable).
narrative_ontology:cs_axiom_grounding('d7753154-2f3b-4ad7-ba5b-6f44c96464f8', bright_line_boundary_eliminates_overlap_claims, instrumental).
narrative_ontology:cs_reference_frame('d7753154-2f3b-4ad7-ba5b-6f44c96464f8', uniform_maritime_boundary_regime).
narrative_ontology:cs_drift_state('d7753154-2f3b-4ad7-ba5b-6f44c96464f8', contemporary_post_2000_enforcement_escalation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d7753154-2f3b-4ad7-ba5b-6f44c96464f8', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_state_governments).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, commercial_fishing_interests_flag_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, offshore_resource_extraction_operators).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, high_seas_fishing_fleets).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_naval_powers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.68) reflects that coastal states collect licensing revenue from foreign vessels, control resource extraction concessions, and exclude non-ratifiers from navigation — this is not free coordination, it is monetized control. The suppression score (0.71) is high because the constraint persists through active enforcement (naval patrols, seizure threats, legal proceedings) against overlapping claimants and because alternative sovereignty frameworks (historical rights doctrines, customary law, indigenous claims) are actively suppressed by the bright-line rule and institutional backing. The theater ratio (0.28) is moderate because the constraint DOES solve a real coordination problem (it replaced pre-UNCLOS maritime chaos with clear boundaries), but enforcement increasingly serves extraction rather than pure coordination — coastal states invest in EEZ enforcement not just to prevent tragedy-of-the-commons but to maximize licensing revenue. The measurement series show extractiveness rising from 0.45 to 0.68 over 50 years (coast states became more aggressive in licensing and enforcement), suppression rising from 0.55 to 0.71 (overlapping claims faced escalating pressure, non-ratifiers faced more interceptions), and theater ratio staying low (the coordination narrative remains credible because maritime chaos was real; but rent-collection increasingly dominates actual enforcement activity). All metrics are authored on a single time grid so comparability is maintained. The tangled_rope claim is correct: the constraint provides genuine coordination (the 200nm rule is better than pre-UNCLOS anarchy) AND imposes asymmetric extraction (coastal states collect rents, overlapping claimants lose claims, high-seas fleets lose grounds). This dual structure requires active enforcement (the suppression score validates this) and is accepted because the coordination benefit is substantial. The engine's per-seat computation will show coastal states as low d (beneficiaries, arbitrage exit) and overlapping claimants as high d (victims, constrained exit), confirming the asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The perspective divergence is structural: from the coastal state seat, the EEZ is a hard-won, legitimate achievement that solved maritime chaos and provides rightful sovereignty. From the overlapping claimant seat, it is an imposed boundary that voided their legitimate historical claims. From the non-ratifier naval seat, it is an overreach by a treaty they did not ratify, constrained by customary law they hold to be superior. From the high-seas fishing seat, it is a sudden privatization of previously shared resources. All of these perspectives are consistent with the authored structural data — they are not contradictions, they are seat-dependent perceptions computed by the engine from power, exit options, and beneficiary/victim status. The authored metrics are invariant across these perspectives (extractiveness remains 0.68 regardless of whether a coastal state or overlapping claimant is asked); the engine's per-seat classification diverges because the structural data instantiates asymmetry. This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states occupy the beneficiary seat: they set the rules, collect licensing revenue, control resource exploration, and exit via arbitrage (switching from UNCLOS to asserting territorial claims). They have institutional power and generational time horizons — the EEZ arrangement is built into their governance models and naval doctrine. Their d is low (near 0.0) because the constraint subsidizes them. Overlapping claimant states are victims: they lose claimed waters under the strict reading, must litigate or acquiesce, and have constrained exit (abandoning UNCLOS means losing maritime legitimacy across other domains). Their d is high (near 0.8) because extraction is imposed on them. High-seas fishing fleets are payers: they lose fishing grounds, face seizure risks, and have constrained mobility (prime grounds are now EEZ, relocation is expensive). Their d is moderately high (0.65–0.75). Non-ratifier naval powers are structurally trapped: they claim customary law but face interception, creating a persistent cost (brinkmanship, diplomatic tension). Their d is high but the exit options are uniquely constrained — they cannot mobile-exit (they are global naval powers, not relocatable) and identity-locked (their naval role is constituted through global presence). A directionality override may be warranted here: the structural derivation might place them at d~0.70 (moderate payer), but their trapped and identity-locked nature suggests d~0.80 (target status). This is noted in the logic but not explicitly overridden in the JSON to preserve author intent visibility. Commercial interests and offshore extractors are dual-positioned beneficiaries/payers: they benefit (licensing access, secure exploitation rights) and pay for enforcement, but their mobile exit and powerful position mean they are net beneficiaries (d~0.25). They could exit (exploit elsewhere) but prefer the secure frame provided by EEZ enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pre-UNCLOS maritime chaos, resource commons collapse, naval brinkmanship) was acute and the strict EEZ reading was a successful coordination response: it established clear boundaries, enabled coastal-state resource stewardship, and reduced high-seas competition disasters. However, the founding problem's status is now contested (see six_questions.founding_problem_status = 'contested'). Coastal states maintain the problem is live (security threats, resource disputes); overlapping claimants and maritime scholars argue it is substantially solved and the mechanism has drifted into rent-collection. The theater ratio (0.28) and rising extractiveness (0.45 → 0.68) support the mandatrophy signal: if the founding problem were still the driver of enforcement, extractiveness should remain stable and theater ratio should drop (enforcement becomes purer coordination). Instead, extractiveness rose — coastal states escalated licensing fees and enforcement intensity — while theater remained moderate, suggesting the coordination justification persists but extraction has accumulated. A mandatrophy-resolution flag would be appropriate: the constraint is no longer primarily defending against maritime chaos; it is defending exclusive resource control. However, the coordination core remains genuine (the 200nm rule is better than alternatives), so the classification stays tangled_rope, not snare. The omega variable on resource-extraction-coordination-vs-rent-collection (above) directly addresses this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_rights_vs_unclos_supremacy,
    'Do pre-UNCLOS historical occupation and resource use create sovereign rights that override the strict 200-nautical-mile EEZ boundary, or is UNCLOS Article 57 the final arbiter of maritime sovereignty?',
    'ICJ/ITLOS rulings on specific boundary disputes (Philippine-China disputes, Arctic claims, South China Sea cases) that establish whether historical use is grandfathered or completely subordinated. State practice in bilateral negotiations (settlement patterns, concession outcomes).',
    'If historical rights are held to survive UNCLOS, the strict reading''s exclusivity is undermined; overlapping claimants regain legal grounds for their claims. If UNCLOS is absolute, the strict reading holds but faces delegitimation from states asserting historical rights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_rights_vs_unclos_supremacy, conceptual, 'Whether UNCLOS Article 57 is a comprehensive restatement of maritime sovereignty or a codification that leaves space for pre-existing rights.').

omega_variable(
    customary_freedom_of_navigation_independence,
    'Is freedom of navigation through EEZs a customary international law principle independent of UNCLOS ratification status, or does the strict reading bind all states regardless of UNCLOS signature?',
    'State practice by non-ratifiers (U.S. freedom-of-navigation operations, responses to interception); ICJ opinions on whether customary law survives UNCLOS; negotiation patterns between ratifiers and non-ratifiers (acquiescence vs. escalation).',
    'If customary freedom of navigation survives independently, the strict reading''s suppression of navigational rights is ineffective for major naval powers; if UNCLOS is binding even on non-ratifiers, the strict reading achieves universal suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_freedom_of_navigation_independence, empirical, 'Whether non-UNCLOS naval operations establish a persistent customary law alternative or are gradually overcome by state acceptance of EEZ boundaries.').

omega_variable(
    resource_extraction_coordination_vs_rent_collection,
    'Is the exclusive EEZ boundary primarily a coordination mechanism for managing shared resources responsibly, or has it become a mechanism for coastal states to collect monopoly rents from resource extraction?',
    'Comparative analysis of coastal-state licensing practices (fees relative to resource extraction cost), environmental outcomes (stock management vs. depletion), and revenue capture (licensing revenue per unit extracted vs. historical commons baselines). Time series of licensing fee escalation post-UNCLOS.',
    'If primarily coordination, the measured extractiveness is coordination cost; if primarily rent-collection, the extractiveness is asymmetric transfer with suppressed alternatives. This distinction affects whether the constraint is correctly classified as tangled_rope (genuine coordination + asymmetric extraction) or drifting toward snare (extraction masquerading as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_extraction_coordination_vs_rent_collection, empirical, 'Whether the strict EEZ reading''s operation has shifted from solving a commons problem to defending extraction monopolies.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (military enforcement, seizure risk, legal proceedings) or internalized (overlapping claimants have accepted the UNCLOS frame as legitimate and no longer seriously contest)?',
    'Post-exit trajectory analysis: if overlapping claimants abandoned UNCLOS recognition, would they immediately reassert historical claims (structural suppression), or have they internalized the strict boundary as the legitimate order (internalized)? Archival evidence of protest claims vs. acquiescence over time.',
    'Structural suppression can be removed by enforcement decay; internalized suppression persists after enforcement is withdrawn. If substantially internalized, the true suppression may be lower than authored, but the constraint''s persistence would require less active enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether UNCLOS EEZ suppression of alternative sovereignty frameworks is enforced or internalized by institutional legitimacy.').

omega_variable(
    kernel_reading_contest_framing,
    'This constraint is ONE reading of the contested UNCLOS sovereignty kernel. The sibling readings (historical_rights_reading, non_ratifier_enforcement_reading) would classify the same maritime system very differently. Which reading is structurally correct?',
    'This is a preference omega rooted in committer framing: the strict reading''s ε is invariant (it defines the constraint being measured), but the EXISTENCE and PREVALENCE of alternative readings depend on which interpretive authority (state practice, ICJ precedent, treaty negotiators, regional powers) is treated as legitimate. The resolution is not empirical but normative: whose interpretation of UNCLOS governs?',
    'If the historical-rights reading becomes dominant, this constraint''s classification drops from tangled_rope toward snare (suppression of legitimate alternatives increases). If the non-ratifier reading gains strength, the suppression remains high but targets a more powerful adversary. The strict reading''s classification is stable only if its interpretive frame is maintained as canonical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, preference, 'Whether the strict UNCLOS reading is the canonical interpretation or one contestable framing among others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(uncl_tr_t8, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(uncl_tr_t16, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(uncl_tr_t24, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(uncl_tr_t32, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(uncl_tr_t50, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uncl_be_t8, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(uncl_be_t16, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(uncl_be_t24, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(uncl_be_t32, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(uncl_be_t50, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(uncl_su_t8, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(uncl_su_t16, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(uncl_su_t24, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(uncl_su_t32, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(uncl_su_t50, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__strict_eez_reading, 0.12).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, international_maritime_fisheries_commons).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, continental_shelf_resource_exploitation).

% DUAL FORMULATION NOTE:
% This constraint is the strict_eez_reading of the contested unclos_sovereignty_boundary kernel. It forms a constraint family with historical_rights_reading and non_ratifier_enforcement_reading. The three readings instantiate THREE SEPARATE ε values and beneficiary/victim structures from the same treaty text, depending on which interpretive authority (coastal states, historical precedent, customary law) is treated as canonical. All three are linked via network.affects_constraints to enable contamination propagation analysis: if the strict reading's enforcement weakens, historical rights may be reasserted; if non-ratifier customary law gains acceptance, enforcement costs rise. The decomposition follows the ε-invariance principle: a single ε value and stable beneficiary/victim set per story, with network links to the siblings that would have different ε and structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__strict_eez_reading, institutional, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
