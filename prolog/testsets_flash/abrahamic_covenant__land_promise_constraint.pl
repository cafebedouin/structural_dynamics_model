% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant: Land Promise (Conditional/Ongoing Reading)
 *   domain: religious_studies/comparative_theology/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the Abrahamic covenant,
 *   focusing on the territorial grant of the Land of Canaan as an
 *   unconditional and ongoing divine promise. This interpretation serves as a
 *   foundational narrative for certain state actors and religious nationalist
 *   movements, providing a theological justification for territorial claims.
 *   The constraint operates as a snare, as its persistence relies heavily on
 *   active enforcement and the suppression of alternative interpretations and
 *   indigenous claims, leading to significant extraction from displaced
 *   populations. The high extractiveness and suppression reflect the material
 *   consequences of this theological claim in the context of the
 *   Israeli-Palestinian conflict.
 *
 * KEY AGENTS:
 *   - state_actors_claiming_divine_mandate: Primary agenda_setter (institutional/identity_locked) — leverages covenant for territorial legitimacy.
 *   - religious_nationalist_movements: Beneficiary (organized/identity_locked) — mobilizes support and identity around this reading.
 *   - displaced_indigenous_populations: Primary payer (powerless/trapped) — bears the direct costs of territorial claims.
 *   - secular_peace_advocates: Payer (moderate/constrained) — challenges religious justification for territorial claims.
 *   - international_legal_bodies: Observer (institutional/analytical) — adjudicates disputes based on international law, often in tension with religious claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.9).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.95).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.9).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant: Land Promise (Conditional/Ongoing Reading)").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious_studies/comparative_theology/institutional_authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, '5513a504-f897-4242-a6f3-994e64ab9055').
narrative_ontology:cs_kernel_codification('5513a504-f897-4242-a6f3-994e64ab9055', fixed_text).
narrative_ontology:cs_authority_grounding('5513a504-f897-4242-a6f3-994e64ab9055', lineage).
narrative_ontology:cs_interpretation_layer_present('5513a504-f897-4242-a6f3-994e64ab9055').
narrative_ontology:cs_reading_relation('5513a504-f897-4242-a6f3-994e64ab9055', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('5513a504-f897-4242-a6f3-994e64ab9055', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_axiom('5513a504-f897-4242-a6f3-994e64ab9055', foundational, land_promise_unconditional_and_eternal).
narrative_ontology:cs_axiom_status(land_promise_unconditional_and_eternal, holdable).
narrative_ontology:cs_axiom_grounding('5513a504-f897-4242-a6f3-994e64ab9055', land_promise_unconditional_and_eternal, theological).
narrative_ontology:cs_axiom('5513a504-f897-4242-a6f3-994e64ab9055', secondary, divine_mandate_for_territorial_sovereignty).
narrative_ontology:cs_axiom_status(divine_mandate_for_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5513a504-f897-4242-a6f3-994e64ab9055', divine_mandate_for_territorial_sovereignty, theological).
narrative_ontology:cs_reference_frame('5513a504-f897-4242-a6f3-994e64ab9055', unconditional_divine_land_grant).
narrative_ontology:cs_drift_state('5513a504-f897-4242-a6f3-994e64ab9055', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5513a504-f897-4242-a6f3-994e64ab9055', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, state_actors_claiming_divine_mandate).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, religious_nationalist_movements).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, displaced_indigenous_populations).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, secular_peace_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors interpret the covenant as an unconditional, ongoing divine grant of specific territory, providing a theological basis for territorial claims and policies. Their legitimacy and political power are deeply intertwined with this interpretation, making exit from this framing highly costly.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, state_actors_claiming_divine_mandate, agenda_setter,
    institutional, generational, identity_locked, national).

% These movements derive their ideological justification and mobilize support by asserting the divine, unconditional nature of the land promise. They benefit from the political and social capital generated by this narrative, which reinforces their identity and goals.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_nationalist_movements, beneficiary,
    organized, generational, identity_locked, national).

% These populations bear the direct costs of territorial claims based on this covenant reading, experiencing displacement, loss of land, and suppression of their own historical narratives. Their ability to exit this situation is severely constrained by political and military realities.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, displaced_indigenous_populations, payer,
    powerless, generational, trapped, local).

% These advocates challenge the use of religious texts to justify exclusive territorial claims, arguing for a more inclusive or secular approach to conflict resolution. They face significant resistance and are often marginalized in debates dominated by religious-nationalist narratives.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, secular_peace_advocates, payer,
    moderate, biographical, constrained, global).

% These bodies attempt to adjudicate territorial disputes based on international law, often finding themselves in tension with claims grounded in religious covenants. They observe the conflict and its underlying narratives but have limited enforcement power against state actors claiming divine mandate.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: For its adherents, this reading coordinates a collective identity and purpose around a shared understanding of divine destiny and territorial rights, mobilizing political action and social cohesion.
% TRANSFER_FUNCTION: Transfers legitimacy and territorial control from historical and international legal frameworks to a divinely ordained, exclusive claim, resulting in the displacement of populations and the concentration of power in state actors leveraging this narrative.
% ABSENT_VOICES: The voices of those who interpret the covenant as conditional, fulfilled, or purely spiritual, as well as those who advocate for a secular resolution to territorial disputes, are often marginalized or actively suppressed in public discourse dominated by this reading.
% DISAPPEARANCE_RATIONALE: If this specific reading of the Abrahamic covenant vanished overnight, the primary ideological justification for certain territorial claims would collapse, leading to a fundamental re-evaluation of land ownership, national identity, and the basis for conflict in the region. Political and social structures would be forced to reorganize around secular or alternative religious interpretations.
% FOUNDING_PROBLEM: The original covenant in Genesis aimed to establish a relationship between God and Abraham's descendants, promising land and progeny.
% FOUNDING_PROBLEM_CORROBORATION: Adherents of this reading attest the problem is live and the promise ongoing. Critics, including many theologians and secular historians, argue the 'problem' of land acquisition is either fulfilled, conditional, or reinterpreted spiritually, and that the current application is a modern political construct. Corroboration for the 'dead' status comes from diverse academic theological scholarship and international legal perspectives, outside the directly benefiting parties.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.9) because this reading directly underpins policies that result in significant material losses (land, property, self-determination) for the victim populations. Suppression is also extremely high (0.95) due to the active political, military, and legal mechanisms used to enforce territorial claims and marginalize dissenting narratives. The theater ratio (0.6) indicates that while there is a genuine belief in the divine mandate, a substantial portion of the effort goes into maintaining the narrative's political utility and suppressing counter-claims, rather than purely spiritual adherence. Accessibility collapse is high (0.8) because for the victim populations, viable alternatives to their current situation are severely limited. Resistance is also high (0.9) reflecting the ongoing, active opposition from those who bear the costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state actors and religious nationalist movements, this reading is a foundational truth, a divine mandate that justifies their actions and identity. For displaced populations and secular advocates, it is a coercive narrative used to legitimize dispossession and conflict. The engine's classification as a snare captures this divergence, highlighting the extractive and suppressive nature of the constraint from the victim's seat, despite its 'divine mandate' framing by beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   State actors and religious nationalist movements are clear beneficiaries and agenda-setters, as their power and legitimacy are directly derived from this interpretation. Displaced indigenous populations are the primary victims, bearing the direct costs of land loss and political marginalization. Secular peace advocates also bear costs by challenging the dominant narrative. International legal bodies act as observers, analyzing the constraint's impact without directly benefiting or paying in the same way as other actors.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a prime candidate for mandatrophy, as its 'founding problem' (the original covenant) is contested in its modern application. The persistence of this reading, despite significant human cost and international legal challenges, suggests that its function has drifted from a spiritual promise to a tool for political and territorial extraction. The high extractiveness and suppression, coupled with the 'contested' status of the founding problem, prevent it from being mislabeled as a legitimate coordination mechanism or a natural law. The classification as a snare correctly identifies its coercive and extractive nature in its current manifestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_conditionality_ambiguity,
    'Is the Abrahamic covenant''s land promise conditional on obedience and moral conduct, or is it an unconditional, eternal grant?',
    'Comparative theological analysis across diverse interpretive traditions (Jewish, Christian, Islamic) and historical scholarship on covenant theology, focusing on textual evidence for conditions and their fulfillment.',
    'If resolved as conditional and unfulfilled, the theological basis for ongoing territorial claims would weaken, potentially reducing the constraint''s legitimacy and extractiveness. If resolved as unconditional and ongoing, it would reinforce the current narrative, potentially increasing suppression of counter-claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_conditionality_ambiguity, conceptual, 'Ambiguity regarding the conditional nature of the land promise in the Abrahamic covenant.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (political, military, legal barriers) or internalized (cognitive patterns, identity fusion) for the displaced populations?',
    'Post-exit suppression trajectory: if suppression persists after the extractive political/military mechanisms are removed, reclassify as partially internalized. Ethnographic studies on identity and historical memory among displaced groups.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making resolution more complex. If purely structural, removing external barriers would be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for displaced populations.').

omega_variable(
    kernel_reading_divergence,
    'Is this constraint a genuine interpretation of the Abrahamic covenant, or a political construct leveraging religious language?',
    'Historical-critical analysis of the covenant''s reception history and its application in different historical contexts, distinguishing theological development from political instrumentalization. Examination of the ''founding problem status'' and ''corroboration'' for external validation.',
    'If primarily a political construct, the constraint''s legitimacy would be further undermined, potentially reclassifying it as a pure snare with no genuine coordination function. If a genuine theological reading, its persistence would be understood as a deep-seated ideological commitment, requiring different resolution strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Distinguishing theological interpretation from political instrumentalization of the covenant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t1948, abrahamic_covenant__land_promise_constraint, theater_ratio, 1948, 0.4).
narrative_ontology:measurement(abra_tr_t1967, abrahamic_covenant__land_promise_constraint, theater_ratio, 1967, 0.5).
narrative_ontology:measurement(abra_tr_t1987, abrahamic_covenant__land_promise_constraint, theater_ratio, 1987, 0.55).
narrative_ontology:measurement(abra_tr_t2000, abrahamic_covenant__land_promise_constraint, theater_ratio, 2000, 0.58).
narrative_ontology:measurement(abra_tr_t2014, abrahamic_covenant__land_promise_constraint, theater_ratio, 2014, 0.59).
narrative_ontology:measurement(abra_tr_t2024, abrahamic_covenant__land_promise_constraint, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(abra_be_t1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(abra_be_t1967, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(abra_be_t1987, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1987, 0.85).
narrative_ontology:measurement(abra_be_t2000, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(abra_be_t2014, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2014, 0.89).
narrative_ontology:measurement(abra_be_t2024, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(abra_su_t1967, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(abra_su_t1987, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1987, 0.9).
narrative_ontology:measurement(abra_su_t2000, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(abra_su_t2014, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2014, 0.94).
narrative_ontology:measurement(abra_su_t2024, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Abrahamic covenant kernel, focusing on the territorial land promise. It is linked to sibling readings (Isaac and Ishmael covenant interpretations) which offer alternative understandings of the covenant's lineage and scope, leading to different material consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
