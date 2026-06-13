% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: Universal Declaration Constrained by Originalist Scope — Textualist Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The textualist paradox reading identifies a structural contradiction in
 *   the originalist interpretation of the Declaration and Constitution: the
 *   text universally declares equality ('all men are created equal,' 'equal
 *   protection of the laws') while originalist interpretation bounds that
 *   equality to the framers' hypothesized intent (understood as permitting
 *   slavery, coverture, and property-based exclusion). This reading argues
 *   that the universal language and the restricted application are logically
 *   irreconcilable within a single interpretive framework. The textualist
 *   reading thus exposes the originalist constraint as performative
 *   contradiction — it claims fidelity to the text while restricting the
 *   text's own stated scope. The extractiveness comes not from coordination
 *   failure but from the originalist authority's power to choose when to read
 *   the text universally (in other contexts) and when to read it through
 *   historical intent (in equality contexts). This reading is one
 *   instantiation of the contested kernel 'all_men_created_equal'; the
 *   sibling readings (originalist_reading, universalist_reading) are separate
 *   constraint stories with different ε values and different
 *   victim/beneficiary structures.
 *
 * KEY AGENTS:
 *   - originalist_interpretive_authority: Institutional agenda-setter that administers the constraint by treating universal text as bounded by historical intent; has power to define what counts as legitimate constitutional meaning.
 *   - constituencies_excluded_from_original_taxonomy: Powerless payers whose equality claims are deferred by the originalist reading; identity-locked because their humanity is what the constraint denies.
 *   - universalist_constitutional_reading: Moderate-power victim of the constraint (its authority is delegitimized) and incidental beneficiary (as universalist doctrines gradually prevail).
 *   - textualist_analytical_observer: Analytical seat that identifies the logical contradiction between the text's universalism and the originalist reading's restriction; stands outside the framework while using the tools (textualism) of the dominant authority.
 *   - supreme_court_majority: Institutional beneficiary that historically enforced the originalist constraint through doctrine and maintains the constraint's authority through control of canonical interpretation.
 *   - abolitionist_and_civil_rights_movements: Excluded but historically powerful voices that argue the universal language is the law and the originalist restriction is the illegitimate imposition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.62).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.41).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "Universal Declaration Constrained by Originalist Scope — Textualist Reading").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '6641a861-db11-4208-8fa0-d7c3fb8643d9').
narrative_ontology:cs_kernel_codification('6641a861-db11-4208-8fa0-d7c3fb8643d9', fixed_text).
narrative_ontology:cs_authority_grounding('6641a861-db11-4208-8fa0-d7c3fb8643d9', extraction).
narrative_ontology:cs_interpretation_layer_present('6641a861-db11-4208-8fa0-d7c3fb8643d9').
narrative_ontology:cs_reading_relation('6641a861-db11-4208-8fa0-d7c3fb8643d9', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('6641a861-db11-4208-8fa0-d7c3fb8643d9', all_men_created_equal__universalist_reading, influences).
narrative_ontology:cs_axiom('6641a861-db11-4208-8fa0-d7c3fb8643d9', foundational, universal_text_restricts_restricting_interpretation).
narrative_ontology:cs_axiom_status(universal_text_restricts_restricting_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('6641a861-db11-4208-8fa0-d7c3fb8643d9', universal_text_restricts_restricting_interpretation, deontological).
narrative_ontology:cs_axiom('6641a861-db11-4208-8fa0-d7c3fb8643d9', foundational, textual_authority_overrides_historical_intent_when_conflict).
narrative_ontology:cs_axiom_status(textual_authority_overrides_historical_intent_when_conflict, holdable).
narrative_ontology:cs_axiom_grounding('6641a861-db11-4208-8fa0-d7c3fb8643d9', textual_authority_overrides_historical_intent_when_conflict, deontological).
narrative_ontology:cs_reference_frame('6641a861-db11-4208-8fa0-d7c3fb8643d9', universal_equal_status_principle).
narrative_ontology:cs_drift_state('6641a861-db11-4208-8fa0-d7c3fb8643d9', contemporary_originalist_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6641a861-db11-4208-8fa0-d7c3fb8643d9', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_authority).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, constituencies_excluded_from_original_taxonomy).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, universalist_constitutional_reading).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high because the originalist constraint transfers interpretive authority from the text's plain language to the framers' hypothesized intent, allowing the originalist seat to extract authority from universalist constituencies. Suppression (0.41) is moderate because the constraint operates partly through doctrinal authority (institutional, not physical) and encounters substantial resistance from civil rights movements, academic textual scholarship, and the universalist reading itself. Theater (0.48) is slightly elevated because the constraint must continuously perform 'historical fidelity' as the lived gaps between universal text and restrictive practice become wider and harder to justify — the performative work of the constraint increases over time. The measurement series shows extractiveness rising from 0.48 to 0.62 over the interval, plateauing by t=40 as the constraint approaches its stability limit — the tension between text and interpretation reaches a point where further extraction requires explicit reversal (overturning of doctrine, constitutional amendment) rather than mere interpretive management. Theater also rises through the interval, reflecting the increasing gap between what the text says and what the originalist reading allows it to mean.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist institutional seat, the constraint is genuine coordination around 'the law as the framers intended it' — a principled, intellectually coherent framework. From the powerless constituencies' seat, the same constraint is delegitimizing denial of equality the text promises. From the textualist observer's seat, both readings are present simultaneously and contradict each other — the constraint is not coordination or extraction in the normal sense, but rather a paradox that the originalist authority manages by asserting its interpretive power. The engine should compute the originalist seat as perceiving rope-like coordination (principled interpretation, unified meaning) while the excluded constituencies perceive snare-like extraction (denied rights, deferred equality claims) and the observer perceives tangled_rope (coordination machinery turned to exclusion). This reading itself claims tangled_rope because the constraint simultaneously offers genuine coordination (unified meaning) and substantial extraction (authority capture by the originalist framework).
 *
 * DIRECTIONALITY LOGIC:
 *   The originalist interpretive authority is the beneficiary (controls the meaning, extracts interpretive authority, maintains institutional legitimacy around 'fidelity to framers' intent'). The excluded constituencies are the victims (their equality claims are deferred, their humanity is bounded by historical categories). The universalist reading is also victimized (its authority is delegitimized in originalist contexts, it must argue against 'the law as written') but benefits incidentally as universalist doctrines gradually win (civil rights amendments, judicial expansion). The textualist observer is neither: it stands outside and measures the contradiction. Directionality for the originalist seat runs toward beneficiary (d near 0.1-0.3); for the excluded constituencies toward target (d near 0.8-0.95); for the universalist reading toward mild target (d near 0.6-0.7). No overrides are needed because the structural data (beneficiaries=[originalist authority], victims=[excluded constituencies, universalist reading]) derive the correct directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT declare mandatrophy_resolved, but the textualist paradox reading itself exposes mandatrophy dynamics: the founding problem ('universal text applied to restricted practice') is DEAD as a practical matter in modern constitutional law — exclusions based on race, gender, and property status have been legislatively and judicially overturned. Yet the originalist constraint persists as a doctrinal framework even when the factual problem it purports to solve (the need to bound universal language by historical intent) no longer exists as a political necessity. The constraint has become inertial — it persists because interpretive authority remains invested in originalist frames, not because the constraint solves a live coordination problem. This is piton-like trajectory. However, the textualist reading is NOT a piton itself; it is a tangled_rope because it still performs extractive work (the universalist reading remains delegitimized, the originalist authority retains power to restrict future equality claims). The mandatrophy lies in the gap: the constraint's justifying narrative (founding problem) is dead, but the constraint persists through institutional inertia and interpretive authority, extracting as it goes. Theater rises (performative work increases) because the originalist seat must defend an increasingly incoherent position: 'the text is universal but means restricted scope' becomes harder to defend as historical circumstances change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_fidelity_incoherence,
    'Can originalist interpretation simultaneously claim fidelity to the text (''all men are created equal'') and restrict that text''s application to categories the text does not specify without that restriction being an act of interpretive authority rather than fidelity?',
    'Philosophical and textual analysis of what ''fidelity to text'' means when text and intent diverge. Empirical trace: track whether originalist scholars and judges apply the same ''intent as controlling'' rule consistently across all constitutional domains or selectively invoke it to restrict equality claims.',
    'If restriction is an act of authority rather than fidelity, the originalist constraint is exposed as extractive (not coordination around shared meaning); originalist legitimacy claim collapses; the universalist and textualist readings gain authority. If fidelity can mean ''honoring intent over text'', the originalist constraint remains defensible as coordination around a coherent (if contested) interpretive philosophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_fidelity_incoherence, conceptual, 'Whether originalist textual fidelity is coherent or performatively contradictory.').

omega_variable(
    identity_lock_mechanism_suppression,
    'Is the measured suppression of excluded constituencies a structural/institutional result of the originalist constraint, or is it an internalized identity-lock where the constituencies themselves have absorbed the originalist framework''s denial of their equality?',
    'Historical and sociological analysis: examine legislative/movement pressure from excluded constituencies when institutional barriers are reduced (Reconstruction, Civil Rights era, contemporary feminist/Indigenous movements). High mobilization after barrier reduction = structural suppression; continued acquiescence = internalized lock-in. Micro-level: post-liberation testimony (formerly enslaved people after emancipation, women after suffrage) on whether the internalized frame persisted or dissolved.',
    'If structural: the constraint''s suppression is externally enforced and can be dissolved by institutional change. If internalized: suppression persists even after external barriers fall, the constraint is more inertial and theatric, and the theater_ratio underestimates the true cost. The textualist paradox reading presumes structural suppression (the constraint operates through doctrinal authority, not psychology), but internalization dynamics may amplify the actual suppressive force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression, empirical, 'Whether suppression is structural or internalized identity-lock.').

omega_variable(
    textualism_as_tool_or_reading,
    'Is textualism (the analytical method that exposes the originalist paradox) a neutral analytical tool, or is it itself a competing reading of the kernel with its own beneficiary structure?',
    'Genealogical analysis: trace the history of textualism in American jurisprudence, identify which constituencies benefit from textualist doctrine in other domains (commerce clause textualism, textualist criminal law). If textualism benefits certain seats consistently, it is a reading, not a tool.',
    'If neutral tool: the textualist paradox reading is analytical observation that delegitimizes originalism without itself being extractive. If competing reading: the textualist reading itself may carry extractive elements (e.g., textualism benefits commercial interests in other domains) and the constraint should be reclassified or decomposed into separate stories. The current analysis presumes textualism is a tool; empirical investigation may require decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textualism_as_tool_or_reading, conceptual, 'Whether textualism is an analytical method or a competing reading of the kernel with its own extractive dimensions.').

omega_variable(
    universalist_reading_contingency,
    'Is the universalist reading genuinely victimized by the originalist constraint, or does it benefit from the constraint''s existence because the constraint provides a stable target for universalist authority to define itself against?',
    'Examine universalist constitutional scholarship and movements: do they argue more forcefully and coherently when the originalist constraint is salient and explicit, or would universalism thrive equally well without originalism as a foil? Institutional analysis: does the universalist reading''s authority increase when the originalist-universalist contrast is heightened?',
    'If the universalist reading benefits from the constraint''s salience, it may not be a pure victim; the constraint may be a tangled_rope from the universalist perspective too, where the ''victim'' seat actually extracts symbolic authority and moral legitimacy from the opposition. The current analysis presumes pure victimhood (universalist reading is delegitimized); this omega tests whether the delegitimization carries hidden benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universalist_reading_contingency, empirical, 'Whether the universalist reading is purely victimized or also benefits from opposition to the originalist constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__textualist_paradox_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(all__tr_t10, all_men_created_equal__textualist_paradox_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(all__tr_t20, all_men_created_equal__textualist_paradox_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(all__tr_t30, all_men_created_equal__textualist_paradox_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(all__tr_t40, all_men_created_equal__textualist_paradox_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(all__tr_t50, all_men_created_equal__textualist_paradox_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(all__tr_t60, all_men_created_equal__textualist_paradox_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(all__be_t10, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(all__be_t20, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(all__be_t30, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(all__be_t40, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(all__be_t50, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(all__be_t60, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(all__su_t10, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(all__su_t20, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(all__su_t30, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement(all__su_t40, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(all__su_t50, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement(all__su_t60, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 60, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__textualist_paradox_reading, 0.12).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'all_men_created_equal' in American constitutional law. The textualist paradox reading isolates the logical contradiction between universal text and restricted originalist application (ε ≈ 0.62, extractive, tangled_rope). The sibling originalist_reading instantiates the same text through historical intent (lower ε, coordinate function for unified meaning). The sibling universalist_reading reads the text as containing a principle of iterative expansion (different ε, different victim set, different type). Each reading has its own constraint_id, its own omegas documenting irreducible uncertainties, and its own measurement series. The three stories are linked via network.affects_constraints: textualist reading influences both siblings by exposing originalist incoherence and strengthening universalist authority claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
