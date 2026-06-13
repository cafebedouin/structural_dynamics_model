% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Continuing Revelation Authority: Prophetic Override of Eternal Covenant
 *   domain: religious/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The prophetic-override reading instantiates continuing revelation as a
 *   doctrine that grants the living prophet unilateral authority to supersede
 *   prior revelation when circumstances require. This reading was activated
 *   by federal pressure against polygamy in the 1880s. The president of the
 *   church announced the Manifesto (1890) suspending plural marriage
 *   practice, framing it as prophetic response to divine will operating
 *   through temporal law. This reading coexists with two siblings: the
 *   immutable-commandment reading (which holds D&C 132 as eternally binding)
 *   and the temporal-accommodation reading (which suspends practice but
 *   reserves doctrinal validity). The three readings contest each other
 *   across institutional history; this constraint models the
 *   prophetic-override reading as a structurally autonomous claim with its
 *   own beneficiary set, suppression mechanism, and ε-invariant
 *   classification. The claim/metric independence rule is critical here: the
 *   constraint is CLAIMED as tangled rope (genuine coordination plus
 *   asymmetric extraction) while the measurements track how extractiveness
 *   and suppression intensified as the override consolidated. The engine
 *   computes whether the claim is borne out; the divergence, if any, is the
 *   measurement the constraint story exists to take.
 *
 * KEY AGENTS:
 *   - institutional_church_leadership: agenda-setter, controls revelation authority and enforcement of the override (institutional power, arbitrage exit)
 *   - polygamist_practitioners: primary payers, forced to choose between family structure and faith community (moderate power, identity-locked exit)
 *   - female_plural_wives: secondary payers and excluded, retroactively stripped of marital and spiritual status (powerless, trapped exit)
 *   - fundamentalist_believers: victims of the constraint's foreclosure of the immutable-commandment reading (powerless, identity-locked exit)
 *   - federal_government: structural pressure source, excluded from revelation discourse but activating the override through law enforcement
 *   - non_practicing_church_members: beneficiaries of institutional survival and unified faith framework (organized, constrained exit)
 *   - theological_interpreters: analytical observers, record whether the readings coexist, conflict, or foreclose each other
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.68).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.71).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Continuing Revelation Authority: Prophetic Override of Eternal Covenant").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f').
narrative_ontology:cs_kernel_codification('46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f', fixed_text).
narrative_ontology:cs_authority_grounding('46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f', extraction).
narrative_ontology:cs_interpretation_layer_present('46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f').
narrative_ontology:cs_reading_relation('46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f', eternal_marriage_covenant__temporal_accommodation_reading, influences).
narrative_ontology:cs_axiom('46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f', foundational, prophetic_override_authority_supreme).
narrative_ontology:cs_axiom_status(prophetic_override_authority_supreme, holdable).
narrative_ontology:cs_axiom_grounding('46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f', prophetic_override_authority_supreme, deontological).
narrative_ontology:cs_axiom('46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f', foundational, circumstances_may_supersede_eternal_law).
narrative_ontology:cs_axiom_status(circumstances_may_supersede_eternal_law, holdable).
narrative_ontology:cs_axiom_grounding('46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f', circumstances_may_supersede_eternal_law, instrumental).
narrative_ontology:cs_reference_frame('46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f', continuing_revelation_mechanism_in_operation).
narrative_ontology:cs_drift_state('46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f', federal_pressure_statehood_conditions, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('46732f4d-2ff2-4ac2-aafc-fa64ba9ef27f', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, institutional_church_leadership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, polygamist_practitioners).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, female_plural_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_believers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end, rising from 0.35 at t0) because the constraint transfers decision-making authority from prior revelation (accessible to individuals) to present prophetic authority (concentrated in the president). It does not distribute benefits—it concentrates them in institutional leadership while imposing costs (abandonment of practice, loss of identity coherence, excommunication risk) on believers whose commitments were built on the prior reading. Suppression is high (0.71) and rising because enforcement must maintain the override against fundamental challenge: fundamentalist believers could reject the prophet's authority, polygamist practitioners could continue the practice, and plural wives could refuse the retroactive reframing. Active enforcement includes excommunication, community pressure, narrative work reframing the prior revelation as not eternally binding, and structural exclusion of objectors. Theater is substantial (0.52) because the constraint's operation requires continuous rhetorical defense of how prophetic authority works—the claim that 'continuing revelation allows the prophet to supersede prior revelation when circumstances require' is itself the performance that makes the constraint persist. Without the narrative that the override is itself divinely guided, the constraint collapses into pure federal capitulation. The measurement series tracks activation (t=5 when federal pressure culminates in statehood condition), consolidation (t=10-15 as excommunication and narratives enforce the override), and stabilization (t=25-40 as the override becomes orthodoxy and fundamentalist dissent is institutionally marginalized). All metrics are authored on a shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional church leadership's seat, the constraint is genuine coordination: it solved a real crisis (federal vs. doctrinal conflict) through prophetic authority, preserving the faith community. From polygamist practitioners' and plural wives' seats, the constraint is forced extraction: their understanding of divine law was superseded unilaterally, their family structures criminalized retroactively, and they were required to accept the reframing or lose community. The engine computes per-seat classification: the beneficiary seat (institutional leadership) will perceive rope or coordination dynamics; the payer seats (polygamists, plural wives) will perceive snare or tangled-rope extraction. The authored metrics describe the constraint's operation structure—that extraction is concentrated, suppression is active, and theater is substantial—independent of which seat perceives it as legitimate.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional church leadership are the structural beneficiaries (they gain authority consolidation, institutional survival, legal legitimacy—d near 0.0, full beneficiary). Polygamist practitioners and plural wives are the structural victims (they bear the cost of abandonment, identity loss, excommunication risk—d near 1.0, full target). The derivation chain runs: beneficiary set (institutional leadership) + arbitrage exit (they can accommodate or resist federal pressure) → low d; victim set (polygamists, plural wives) + identity-locked exit (their identity as believers is bound to their understanding of divine law, making exit from the faith community cost their selfhood) → high d. Federal government is structurally excluded (trapped exit, no direct voice in revelation discourse) but their enforcement pressure is the primary activation mechanism. No directionality overrides are needed: the structural data (beneficiary/victim + exit options) produces the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The prophetic-override reading avoids misclassification as pure extraction (snare) by maintaining a genuine coordination function: it does solve the problem of doctrinal contradiction under external pressure. However, it demonstrates mandatrophy trajectory: the founding problem was the federal/doctrinal conflict (live at t=0-5), but by t=40 the founding problem is contested or dead—the constraint now operates to maintain prophetic authority over interpretation rather than to resolve the original federal crisis. The Manifesto itself (t=5) was a response to a live founding problem; the perpetuation of the prophetic-override authority beyond the crisis point (t=25-40) becomes maintenance of power structure rather than coordination function. The measurement trajectory shows theater_ratio rising (0.28 → 0.52) as the constraint's operation shifts from crisis response to routine authority maintenance. The claim 'continuing revelation doctrine allows living prophet to supersede prior revelation when circumstances require' becomes doctrinally true but functionally mandatrophic—it operates to prevent future prophetic revision as much as to enable it, making the authority mechanism itself resistant to change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_pressure_vs_prophetic_agency,
    'Was the Manifesto a response to genuine divine revelation received by the prophet, or was it institutional adoption of federal pressure reframed as prophetic guidance?',
    'Documentary evidence from the prophet''s personal records (diaries, correspondence, revelation accounts) vs. external historians'' reconstruction of federal pressure timeline and institutional negotiation. Comparison of the Manifesto''s language with federal statehood conditions and prosecution patterns.',
    'If the override was divine revelation (prophetic agency), the constraint is genuine coordination under continuing revelation doctrine—a tangled rope with real coordination function. If the override was institutional capitulation dressed as prophecy (federal pressure), the constraint is pure extraction (snare) using revelation-authority as cover story. The ε-invariance principle applies: these are two different constraints, not one measured two ways.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_pressure_vs_prophetic_agency, empirical, 'Whether prophetic agency or federal coercion is the primary mechanism activating the override').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the prophetic-override reading logically foreclose the immutable-commandment reading within a single coherent framework, or do the two readings coexist as competing claims held by different parties?',
    'Theological analysis: if a framework can coherently hold both ''D&C 132 is eternally binding'' and ''the current prophet can override it when circumstances require,'' then they coexist. If no such framework is possible, the reading forecloses. Examine whether institutional theology explicitly forecloses immutable-commandment claims or merely marginalizes practitioners who hold them.',
    'If foreclosure: the readings are mutually exclusive claims; only one can be true in any framework; institutional enforcement is defending a categorical boundary. If coexistence: the readings are held by different constituencies; enforcement is managing dissent rather than defending logical exclusivity. Coexistence would suggest the constraint is more snare-like (suppression of an alternative reading held elsewhere); foreclosure would suggest it is tangled-rope (coordination around a unified revelation framework with collateral extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the prophetic-override and immutable-commandment readings are logically incompatible or simply opposed').

omega_variable(
    plural_wives_excluded_voice,
    'Should the plural wives—whose marital and spiritual status was directly altered by the override—have had formal voice in receiving or validating the new revelation? Is their exclusion structural to the continuing revelation mechanism, or a contingent institutional choice?',
    'Examine revelation processes in other contexts: do institutional procedures ever require consent or input from those most affected before declaring revelation? Reconstruct what a revelation process would have looked like had plural wives had formal voice—would the outcome have been different? Compare to other contexts where institutional authority affects status without consultation.',
    'If exclusion is structural to continuing revelation mechanism: the constraint is inherently extractive on gendered grounds; suppression of plural wives'' voice is part of how the override operates. If exclusion is contingent choice: the constraint could be reformed to include affected parties without changing the mechanism itself—it is currently snare-like but could become tangled-rope with procedural change. The classified type (tangled_rope) assumes some coordination function; if the mechanism inherently excludes affected voices, classification may drift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plural_wives_excluded_voice, preference, 'Whether exclusion of plural wives from revelation is necessary to the mechanism or a reform target').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression primarily structural (institutional enforcement: excommunication, denial of temple access, community pressure) or internalized (believers have absorbed the prophet''s authority into their own frame such that rejection feels impossible)?',
    'Post-exit trajectory analysis: observe believers who leave the faith community—does suppression persist after structural enforcement is removed (internalized), or does it dissolve once institutional pressure is gone (structural)? Examine autobiographical testimony from excommunicated or apostasized members.',
    'If suppression is primarily structural: removing institutional enforcement would dissolve the constraint; payer seats have genuine alternatives. If suppression is internalized: the constraint travels with the agent after exit; identity-locked status runs deeper than institutional pressure. If mixed: a portion of suppression remains after institutional removal, suggesting the constraint has become self-reproducing through identity fusion. Mixed suppression would indicate the constraint''s effective suppression is higher than the measured structural score suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression persists after institutional mechanisms are removed (internalized) or dissolves (structural)').

omega_variable(
    continuing_revelation_doctrine_itself_contested,
    'Is the continuing revelation doctrine itself (the claim that living prophets can receive new revelation) a stable kernel around which readings differ, or is it itself a contested mechanism that different believers interpret fundamentally differently?',
    'Examine whether all three readings accept continuing revelation as true and disagree only on how it applies to polygamy, or whether some readings reject continuing revelation entirely and read the Manifesto as institutional capitulation (not as prophecy). Categorize believers by their stance on whether revelation continues.',
    'If continuing revelation is accepted by all readings: it is the stable kernel and the three readings contest its application. If continuing revelation itself is contested: the kernel is not the practice (polygamy) or the doctrine but the mechanism of revelation itself—the constraint then operates to enforce belief in the mechanism as a prerequisite to having any stake in the readings. This would make the constraint more foundational and more extractive: it enforces commitment to a mechanism rather than to a practice or doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuing_revelation_doctrine_itself_contested, conceptual, 'Whether continuing revelation doctrine is accepted as stable kernel or is itself contested across readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(eter_tr_t5, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(eter_tr_t15, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(eter_tr_t25, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(eter_tr_t40, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eter_be_t5, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(eter_be_t15, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(eter_be_t25, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(eter_be_t40, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(eter_su_t5, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(eter_su_t15, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(eter_su_t25, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(eter_su_t40, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__prophetic_override_reading, 0.12).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% The eternal_marriage_covenant kernel instantiates three structurally distinct constraints depending on how continuing revelation authority is read. This constraint (prophetic_override_reading) models the institutional church's reading: the living prophet receives authority to override prior revelation when circumstances require. The immutable_commandment_reading (constraint file) models the fundamentalist reading: D&C 132 is eternally binding and no prophet can revoke it. The temporal_accommodation_reading (constraint file) models a middle reading: the doctrine remains eternally true but practice is suspended by revelation—obedience to civil law takes precedence temporally without negating the eternal principle. The three readings diverge on ε (the prophetic_override reading is substantially more extractive because it concentrates decision authority; the immutable_commandment reading has lower ε because it treats the doctrine as fixed and knowable; the temporal_accommodation reading sits between). The ε-invariance principle applies: each reading is a separate constraint with its own ε-invariant classification. Network edges establish that changes to the institutional reading (this constraint) create pressure on the other readings: if the prophetic_override reading consolidates, fundamentalist believers face intensified choice (accept the reading or leave); if the reading fractures, the temporal_accommodation reading becomes more tenable as a middle position. Upstream/downstream: the prophetic_override reading feeds its authority structure downstream to institutional practices; the immutable_commandment reading coexists outside the institutional framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
