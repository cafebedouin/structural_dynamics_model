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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant: Land Promise (Territorial Reading)
 *   domain: religious_studies/geopolitical
 *
 * SUMMARY:
 *   This constraint models the Abrahamic covenant's territorial grant (Land
 *   of Canaan) as interpreted by those who view it as an unconditional,
 *   ongoing, and exclusive divine mandate for specific state actors. This
 *   reading directly maps onto the modern Israeli-Palestinian conflict, where
 *   religious claims are leveraged to justify territorial control and the
 *   displacement of other populations. The constraint is claimed as a 'snare'
 *   due to its high extraction from displaced populations and its reliance on
 *   active enforcement and suppression of alternative claims.
 *
 * KEY AGENTS:
 *   - state_actors_claiming_divine_mandate: Primary beneficiary/agenda_setter (institutional/identity_locked)
 *   - displaced_populations: Primary target/payer (powerless/trapped)
 *   - non_adherents_in_claimed_territory: Secondary target/payer (moderate/constrained)
 *   - religious_scholars_and_interpreters: Agenda setter/beneficiary (organized/constrained)
 *   - international_humanitarian_organizations: Analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.92).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.88).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.92).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant: Land Promise (Territorial Reading)").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious_studies/geopolitical").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, '1402c51c-1a49-4012-a88d-0801bcfd9449').
narrative_ontology:cs_kernel_codification('1402c51c-1a49-4012-a88d-0801bcfd9449', fixed_text).
narrative_ontology:cs_authority_grounding('1402c51c-1a49-4012-a88d-0801bcfd9449', lineage).
narrative_ontology:cs_interpretation_layer_present('1402c51c-1a49-4012-a88d-0801bcfd9449').
narrative_ontology:cs_reading_relation('1402c51c-1a49-4012-a88d-0801bcfd9449', abrahamic_covenant__isaac_covenant_reading, influences).
narrative_ontology:cs_reading_relation('1402c51c-1a49-4012-a88d-0801bcfd9449', abrahamic_covenant__ishmael_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('1402c51c-1a49-4012-a88d-0801bcfd9449', foundational, land_promise_is_unconditional_and_eternal).
narrative_ontology:cs_axiom_status(land_promise_is_unconditional_and_eternal, holdable).
narrative_ontology:cs_axiom_grounding('1402c51c-1a49-4012-a88d-0801bcfd9449', land_promise_is_unconditional_and_eternal, theological).
narrative_ontology:cs_axiom('1402c51c-1a49-4012-a88d-0801bcfd9449', foundational, divine_mandate_for_exclusive_territorial_sovereignty).
narrative_ontology:cs_axiom_status(divine_mandate_for_exclusive_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('1402c51c-1a49-4012-a88d-0801bcfd9449', divine_mandate_for_exclusive_territorial_sovereignty, theological).
narrative_ontology:cs_reference_frame('1402c51c-1a49-4012-a88d-0801bcfd9449', unconditional_divine_land_grant).
narrative_ontology:cs_drift_state('1402c51c-1a49-4012-a88d-0801bcfd9449', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1402c51c-1a49-4012-a88d-0801bcfd9449', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, state_actors_claiming_divine_mandate).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, displaced_populations).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, non_adherents_in_claimed_territory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Leverages the covenant's territorial grant as a foundational claim to legitimacy and sovereignty over specific lands. Actively enforces policies and laws based on this interpretation, often leading to displacement or subjugation of other populations. Their identity and political mandate are deeply fused with this reading.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, state_actors_claiming_divine_mandate, agenda_setter,
    institutional, generational, identity_locked, regional).

% Bear the direct costs of this constraint, including loss of land, property, and self-determination. Their historical presence and claims are systematically suppressed or delegitimized by the dominant narrative. Exit options are severely limited, often involving forced migration or living under occupation.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, displaced_populations, payer,
    powerless, generational, trapped, local).

% Live within the claimed territory but do not adhere to the religious or political interpretation of the covenant. They face systemic discrimination, limited rights, and constant pressure to conform or emigrate. Their ability to resist is constrained by the state's power and the deeply entrenched narrative.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, non_adherents_in_claimed_territory, payer,
    moderate, biographical, constrained, local).

% Act as custodians and propagators of specific interpretations of the Abrahamic covenant, including the land promise. Their interpretations provide theological justification for state actions or resistance movements. While some may offer nuanced readings, others reinforce exclusive claims, shaping the narrative for adherents.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_scholars_and_interpreters, agenda_setter,
    organized, civilizational, constrained, global).

% Monitor the human rights impact of the conflict arising from competing territorial claims. They document displacement, violence, and discrimination, advocating for international law and humanitarian principles. Their influence is often limited by the deeply entrenched nature of the religious-political conflict.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_humanitarian_organizations, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: For adherents, it coordinates a shared identity and purpose around a divinely ordained destiny and a collective claim to a specific territory, providing a coherent narrative for historical and contemporary actions.
% TRANSFER_FUNCTION: Transfers control, ownership, and sovereignty over land and resources from historically present populations to state actors and their adherents who claim a divine mandate based on this covenant reading.
% ABSENT_VOICES: The voices of indigenous populations and those with alternative historical or religious claims to the land are systematically marginalized or silenced within the dominant discourse. They would articulate counter-narratives of continuous presence, prior ownership, and universal human rights, but are excluded from the power structures that enforce the covenant's territorial reading.
% DISAPPEARANCE_RATIONALE: If the territorial reading of the Abrahamic covenant vanished overnight, the foundational legitimacy claims of state actors would collapse, leading to a radical re-evaluation of borders, property rights, and national identity. The geopolitical landscape would be fundamentally reshaped, and the conflict would transform from a religiously-justified struggle to a purely secular one over resources and self-determination.
% FOUNDING_PROBLEM: The covenant was established to define a relationship between God and Abraham's descendants, promising land, progeny, and blessing, addressing existential questions of identity, belonging, and divine favor.
% FOUNDING_PROBLEM_CORROBORATION: Adherents and state actors claiming divine mandate assert the founding problem (divine promise of land) is live and ongoing. Displaced populations, international legal scholars, and some religious counter-interpreters attest that the problem has been reinterpreted and weaponized to justify contemporary political agendas, arguing that the original spiritual intent has been superseded or fulfilled in non-territorial ways. Historical and archaeological evidence from outside the benefiting parties often complicates exclusive claims.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.92) is severe, reflecting the complete loss of land, sovereignty, and self-determination for displaced populations. Suppression (0.88) is high, as the constraint's persistence relies on active military, legal, and narrative suppression of counter-claims and resistance. The theater ratio (0.65) indicates that while some religious and cultural functions are maintained, a significant portion of the constraint's activity is performative justification for ongoing territorial expansion and control. Accessibility collapse (0.75) is high because alternative narratives and legal avenues for the displaced are systematically undermined, and resistance (0.90) is constant due to the severity of the extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state actors and their adherents, this is a divinely ordained 'rope' or even a 'mountain'—an unchangeable, legitimate claim. From the perspective of displaced populations, it is a 'snare' of profound injustice and dispossession. The engine's classification will highlight this divergence by computing a snare from the authored metrics, contrasting with the claimed rope/mountain framing.
 *
 * DIRECTIONALITY LOGIC:
 *   State actors claiming divine mandate are full beneficiaries (d=0.0) as they directly gain territory and legitimacy. Displaced populations are full targets (d=1.0) as they bear the full cost of dispossession and have no exit. Non-adherents in claimed territory are also targets (d=0.8) due to systemic discrimination and constrained exit. Religious scholars can be beneficiaries (d=0.2) if their interpretations support the dominant narrative, or targets if they offer dissenting views.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (divine promise) is presented as eternal, preventing a clear 'mandatrophy resolved' state. However, the high theater ratio and contested founding problem status indicate a significant drift from any original spiritual intent towards a political instrument for territorial control. The classification as a snare prevents mislabeling this as a legitimate coordination mechanism by highlighting the coercive enforcement and identifiable victims, even if the beneficiaries claim divine sanction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditional_vs_unconditional_promise,
    'Is the Abrahamic covenant''s land promise conditional on obedience and moral conduct, or is it an unconditional, eternal grant?',
    'Theological and textual analysis across diverse interpretive traditions, examining the role of divine judgment and human responsibility in covenant fulfillment.',
    'If conditional, the legitimacy of current territorial claims could be challenged based on contemporary conduct, potentially reclassifying the constraint towards a ''tangled_rope'' or ''scaffold'' (if a path to fulfillment through justice is envisioned). If unconditional, the ''snare'' classification is reinforced as it implies an immutable, non-negotiable claim regardless of human action.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditional_vs_unconditional_promise, conceptual, 'Ambiguity regarding the conditional nature of the land promise.').

omega_variable(
    spiritual_vs_literal_fulfillment,
    'Has the land promise been fulfilled spiritually (e.g., through a spiritual inheritance or a future messianic era) rather than through literal, physical possession of territory?',
    'Comparative theological study of eschatological and allegorical interpretations within Abrahamic traditions, and analysis of historical fulfillment narratives.',
    'If fulfilled spiritually, the constraint''s material claims to territory would lose their divine grounding, potentially reducing its extractiveness and suppression, shifting it towards a ''piton'' (if maintained by inertia) or even dissolving it. If literal fulfillment is affirmed, the ''snare'' classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spiritual_vs_literal_fulfillment, conceptual, 'Ambiguity regarding the mode of covenant fulfillment (spiritual vs. literal).').

omega_variable(
    identity_lock_of_state_actors,
    'To what extent is the identity of state actors claiming divine mandate truly ''identity_locked'' to this specific territorial reading, versus being a strategic political choice?',
    'Analysis of internal political discourse, historical shifts in rhetoric, and the response of these actors to external pressures or alternative theological interpretations. Examination of the ''cost of exit'' from this identity frame for political leaders and institutions.',
    'If the identity lock is primarily strategic, the constraint''s persistence is more vulnerable to political and economic pressure, potentially allowing for a reclassification towards a ''tangled_rope'' or even ''scaffold'' if a political solution becomes viable. If the identity lock is genuine and deeply fused, the ''snare'' is more entrenched and resistant to change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_of_state_actors, empirical, 'The degree to which state actors'' identity is genuinely fused with the territorial claim.').


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
narrative_ontology:measurement(abra_tr_t1993, abrahamic_covenant__land_promise_constraint, theater_ratio, 1993, 0.6).
narrative_ontology:measurement(abra_tr_t2000, abrahamic_covenant__land_promise_constraint, theater_ratio, 2000, 0.62).
narrative_ontology:measurement(abra_tr_t2010, abrahamic_covenant__land_promise_constraint, theater_ratio, 2010, 0.64).
narrative_ontology:measurement(abra_tr_t2024, abrahamic_covenant__land_promise_constraint, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(abra_be_t1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(abra_be_t1967, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1967, 0.85).
narrative_ontology:measurement(abra_be_t1993, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1993, 0.88).
narrative_ontology:measurement(abra_be_t2000, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2000, 0.9).
narrative_ontology:measurement(abra_be_t2010, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2010, 0.91).
narrative_ontology:measurement(abra_be_t2024, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(abra_su_t1967, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(abra_su_t1993, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1993, 0.85).
narrative_ontology:measurement(abra_su_t2000, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2000, 0.86).
narrative_ontology:measurement(abra_su_t2010, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(abra_su_t2024, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, international_law_on_occupation).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, right_of_return_for_refugees).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, religious_freedom_in_holy_sites).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Abrahamic covenant kernel. It is linked to other readings (isaac_covenant_reading, ishmael_covenant_reading) which offer alternative interpretations of lineage and territorial claims, leading to different structural consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
