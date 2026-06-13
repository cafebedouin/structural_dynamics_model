% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Orthodox Literalist Dharmasastra Hierarchy
 *   domain: religious/legal/social
 *
 * SUMMARY:
 *   The orthodox literalist reading of Dharmasastra treats the varna/jati
 *   hierarchy and its associated prescriptions as eternal, revealed cosmic
 *   truth requiring literal observance across generations. This is ONE
 *   READING of a contested kernel (the Dharmasastra textual corpus) whose
 *   meaning is disputed. The literalist reading benefits upper castes
 *   (especially Brahmins) and those positioned high in the occupational
 *   hierarchy, while imposing systematic extraction on Shudras, Dalits, and
 *   women. The constraint persists through identity-lock (caste identity
 *   fusion with hereditary role), institutional monopoly (Brahmin
 *   interpretive authority), and suppression (social sanction, legal
 *   disability, exclusion from education). This reading sits in structural
 *   opposition to the reformist reading (which treats caste prescriptions as
 *   time-bound) and the abolitionist reading (which treats the framework as
 *   fundamentally illegitimate).
 *
 * KEY AGENTS:
 *   - brahmin_ritual_authority: Institutional agenda-setter, interprets and enforces the constraint, claims monopoly on textual meaning, identity-locked to the literalist framework
 *   - upper_caste_landholders: Institutional beneficiaries, retain property and household authority, constrained exit (identity/status loss if hierarchy dissolves)
 *   - shudras_laboring_castes: Powerless payers, bound to hereditary labor, barred from Vedic study, identity-locked to subordination
 *   - dalits_untouchables: Powerless payers, constitutionally outside varna, assigned polluting work, ritual exclusion enforced textually
 *   - women_dvija_brahmin: Moderate-power payers, excluded from Vedic ritual and independent authority, identity-locked through household dependence
 *   - reformist_reinterpreters: Excluded actors, propose contextual reading, face institutional marginalization from orthodox monopoly
 *   - abolitionist_movements: Excluded actors, reject framework entirely, access power only through exit and legal/modern institutions
 *   - modern_secular_state: Observer seat, constitutional authority forbids caste discrimination while citizens organize through caste
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.82).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.88).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.82).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, snare).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Orthodox Literalist Dharmasastra Hierarchy").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious/legal/social").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '2b712d62-2802-4f94-9574-5c5cc43834ba').
narrative_ontology:cs_kernel_codification('2b712d62-2802-4f94-9574-5c5cc43834ba', fixed_text).
narrative_ontology:cs_authority_grounding('2b712d62-2802-4f94-9574-5c5cc43834ba', lineage).
narrative_ontology:cs_interpretation_layer_present('2b712d62-2802-4f94-9574-5c5cc43834ba').
narrative_ontology:cs_reading_relation('2b712d62-2802-4f94-9574-5c5cc43834ba', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_reading_relation('2b712d62-2802-4f94-9574-5c5cc43834ba', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('2b712d62-2802-4f94-9574-5c5cc43834ba', foundational, eternal_cosmic_necessity_hierarchy).
narrative_ontology:cs_axiom_status(eternal_cosmic_necessity_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('2b712d62-2802-4f94-9574-5c5cc43834ba', eternal_cosmic_necessity_hierarchy, deontological).
narrative_ontology:cs_axiom('2b712d62-2802-4f94-9574-5c5cc43834ba', foundational, textual_literalism_non_negotiable).
narrative_ontology:cs_axiom_status(textual_literalism_non_negotiable, overridden).
narrative_ontology:cs_axiom_grounding('2b712d62-2802-4f94-9574-5c5cc43834ba', textual_literalism_non_negotiable, conventional).
narrative_ontology:cs_reference_frame('2b712d62-2802-4f94-9574-5c5cc43834ba', eternal_cosmic_hierarchy_literalism).
narrative_ontology:cs_drift_state('2b712d62-2802-4f94-9574-5c5cc43834ba', modern_legal_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2b712d62-2802-4f94-9574-5c5cc43834ba', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_ritual_monopoly).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, dvija_property_rights).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, patriarchal_household_authority).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits_untouchables).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudras_laboring_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_excluded_ritual).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, excluded_from_vedic_education).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) and rising slightly because the constraint systematically transfers labor, property rights, and authority from subordinated to dominant groups, justified by claims of eternal cosmic necessity. Suppression is highest of all metrics (0.88) because the constraint's persistence depends absolutely on preventing exit: Shudras and Dalits cannot leave their caste without complete social death; women cannot escape household subordination without becoming social outcasts. Theater ratio is moderate (0.42, rising to 0.42 at interval end) because the constraint performs real coordination (ritual calendar, occupational specialization) alongside extraction — the coordination components are genuine but increasingly theatricalized as the founding problem (cosmic order maintenance) becomes empirically unmaintainable in modern contexts. Accessibility collapse is very high (0.91) because once a person's caste identity is established at birth, alternatives are not merely constrained but existentially foreclosed within the constraint-governed society — exit means death to one's community. Resistance is substantial (0.73) because excluded populations have mounted continuous, escalating resistance through reform movements, religious conversions, legal challenges, and mass mobilization, especially under modern legal frameworks. The measurement series is authored on a shared grid across all three metrics at every time point; extractiveness shows slight rise as institutional hardening occurs (increasingly formal enforcement in modern period); theater ratio rises as the founding cosmological claim becomes less credible while the institutional structure persists; suppression requirement stays high and stable because the constraint cannot be maintained without active enforcement. Cyclical dynamics are not primary here; the constraint shows long-term institutional stability despite eroding legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The brahmin_ritual_authority and upper_caste_landholders will compute as experiencing a rope or coordination benefit (genuine institutional function, low directionality toward extraction for those high in the hierarchy). The shudras_laboring_castes and dalits_untouchables will compute as experiencing a snare (high directionality toward extraction, trapped exit, no genuine benefit). Women_dvija_brahmin occupy a mixed position: moderate power but identity-locked exit and structural subordination place them substantially toward the target end despite upper-caste birth. The reformist and abolitionist excluded seats, if they had access to institutional power, would detect the snare structure immediately; their exclusion is itself part of the enforcement machinery. This seat divergence is structural, not normative: the engine computes it from power atoms, exit options, and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin ritual authority: d ≈ 0.1 (powerful, identity-locked but beneficiary side of that lock, monopoly position = arbitrage-grade mobility within the system). Upper-caste landholders: d ≈ 0.15–0.25 (powerful but constrained by caste rules; beneficiary of extraction but pay in ritual obligation and marriage restriction; exit is possible but costs status). Shudras: d ≈ 0.85–0.92 (powerless, identity-locked to subordination, extraction with no reciprocal benefit, trapped exit). Dalits: d ≈ 0.95 (powerless, identity-locked to exclusion, maximum extraction, zero alternatives within the constraint). Women_dvija: d ≈ 0.60–0.70 (moderate power, identity-locked to household subordination, victim of extraction despite upper-caste birth). The directionality profile is highly asymmetric because the constraint is fundamentally extractive and identity-locking to the powerless and female. No overrides are necessary; the derivation chain (beneficiary/victim + exit + power) produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy detection: founding_problem_status='contested' + disappearance_verdict='world_rearranges' signals that the founding problem (cosmic order maintenance via hierarchy) is NOT live — the modern state, constitutional frameworks, and egalitarian social organizations show that social order can be maintained without the prescribed hierarchy. The constraint persists through institutional inertia (Brahmin authority structures persist despite losing their foundational claim), identity-lock (caste membership remains socially constitutive), and active suppression (legal disability of Dalits persisted into modern legal codes). The theater ratio rising while extractiveness plateaus suggests an increasing proportion of enforcement activity is theatrical maintenance (defending the literalist claim despite its empirical weakness) rather than functional coordination. This profile is classic mandatrophy: the founding mandate has outlived its function; the constraint persists as inherited institutional power and identity-fusion rather than because the problem it claims to solve is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.88) structurally imposed (social sanction, legal disability, economic exclusion) or substantially internalized (caste identity as self-concept, acceptance of prescribed role as deserved or natural)?',
    'Longitudinal tracking of identity claims post-exit: if Shudras and Dalits who exit caste-organized communities retain caste-identity salience and reproduce the constraint''s status hierarchy internally, suppression is substantially internalized; if identity shifts rapidly away from caste upon exit, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries enforcement mechanisms with them after exit, limiting reformist efficacy. If structural, dismantling the institutional apparatus (Brahmin authority, legal disability, occupational assignment) would be sufficient to break the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Distinction between structural suppression (external enforcement) and internalized suppression (identity-fusion) in caste hierarchy').

omega_variable(
    cosmic_order_claim_empirical_status,
    'Is the founding claim — that social order depends on the prescribed varna hierarchy to maintain cosmic order (rita) — empirically testable or deontological/metaphysical and thus outside empirical falsification?',
    'Historical and comparative: did egalitarian or non-hierarchical societies maintain order? Do modern societies without caste hierarchy show systemic chaos? Or is the cosmic order claim a non-falsifiable metaphysical assertion immune to empirical evidence?',
    'If empirically contingent, rising evidence that order persists without hierarchy undermines the founding problem''s live status (mandatrophy). If deontological/metaphysical, no empirical evidence falsifies it — the constraint persists on non-empirical authority grounds. If the claim shifts between empirical and metaphysical framing depending on refutation attempts, that is itself a sign of extraction (using different justification frameworks to defend the same hierarchy).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cosmic_order_claim_empirical_status, conceptual, 'Whether the cosmic order claim is empirically falsifiable or metaphysical (non-falsifiable)').

omega_variable(
    literalist_reading_textual_necessity,
    'Does the Dharmasastra textual corpus require literalist reading for coherence, or is reformist (contextual) reading an equally valid interpretation of the same text?',
    'Scholarly consensus among textual historians and comparative jurists: if literalist and reformist readings both produce internally coherent interpretations of the corpus, neither is textually necessitated. If literalist reading requires reading *against* the text''s internal contextualizations and reformist reading requires reading *with* them, literalism is interpretively weaker.',
    'If literalism is textually required, it is harder to dislodge (the text itself enforces it); if it is one optional reading among coherent alternatives, it is maintained by institutional monopoly (Brahmin interpretation authority) rather than textual inevitability, which weakens the constraint''s legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literalist_reading_textual_necessity, empirical, 'Whether literalist reading is textually necessary or one optional interpretation').

omega_variable(
    kernel_contest_foreclosure,
    'Does the literalist reading''s core premise (eternal cosmic necessity) logically foreclose the abolitionist reading, or can both be held by different parties without internal logical contradiction within each party''s framework?',
    'Formal analysis of the two readings'' foundational axioms: if literalism asserts ''cosmic order requires hierarchy'' and abolition asserts ''no legitimate authority exists in this framework,'' these contradict at the level of what is true about cosmic order, not merely what is preferred. If they contradict at the truth level, literalism forecloses abolition within any single framework; if they only differ in what one chooses to do about the claimed truth, they coexist.',
    'If literalism logically forecloses abolition, the readings are competitors for a single epistemic slot — one must be wrong. If they coexist, they are different commitments held by different institutional factions, and the constraint persists through institutional power (which reading captures authority) rather than epistemic defeat of alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Logical foreclosure vs. coexistence of literalist and abolitionist readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dhar_tr_t8, dharmasastra_corpus__orthodox_literalist, theater_ratio, 8, 0.37).
narrative_ontology:measurement(dhar_tr_t16, dharmasastra_corpus__orthodox_literalist, theater_ratio, 16, 0.39).
narrative_ontology:measurement(dhar_tr_t24, dharmasastra_corpus__orthodox_literalist, theater_ratio, 24, 0.4).
narrative_ontology:measurement(dhar_tr_t32, dharmasastra_corpus__orthodox_literalist, theater_ratio, 32, 0.41).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__orthodox_literalist, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(dhar_be_t8, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 8, 0.79).
narrative_ontology:measurement(dhar_be_t16, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 16, 0.8).
narrative_ontology:measurement(dhar_be_t24, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 24, 0.81).
narrative_ontology:measurement(dhar_be_t32, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 32, 0.82).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.84).
narrative_ontology:measurement(dhar_su_t8, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 8, 0.85).
narrative_ontology:measurement(dhar_su_t16, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 16, 0.86).
narrative_ontology:measurement(dhar_su_t24, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 24, 0.87).
narrative_ontology:measurement(dhar_su_t32, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 32, 0.88).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__orthodox_literalist, 0.12).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, brahminical_ritual_monopoly).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, caste_based_occupational_assignment).

% DUAL FORMULATION NOTE:
% The dharmasastra_corpus kernel instantiates three structurally distinct constraints: (1) orthodox_literalist (this file) treats the hierarchy as eternal, revealed, and extractive (snare); (2) reformist_contextual treats caste prescriptions as time-bound and the ethical core as separable (potential tangled_rope with reform pathway); (3) abolitionist_rejection treats the entire framework as illegitimate (snare with no escape within the system). These are not different views of one constraint — they are different constraints instantiated by different readings of the same textual kernel. The literalist reading forecloses the abolitionist reading (they contradict on cosmic order necessity) but coexists with the reformist reading (both treat the text as authoritative but interpret it differently). Epsilon values differ substantially: literalism maintains high extraction via identity-lock; reformism could reduce extraction if the contextual reading gained institutional authority; abolition rejects the entire extractive apparatus. Each reading's ε is stable within its own framework and would only change if that reading's authority eroded. The three stories are linked by kernel contest, not by causal dependency — they are alternative interpretations of the same authoritative text, competing for institutional recognition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
