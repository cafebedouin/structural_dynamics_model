% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC 242 Partial Withdrawal Reading (Indefinite Article / Secure Boundaries)
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates the partial withdrawal reading of UNSC
 *   Resolution 242's withdrawal clause, a contested kernel in international
 *   law. The reading holds that the English text's indefinite article ('from
 *   territories occupied') intentionally leaves withdrawal scope
 *   discretionary, permitting the occupying power to retain strategic
 *   territories under the 'secure boundaries' principle while engaging in
 *   phased negotiation. It is one of three sibling readings of the same
 *   kernel, alongside the maximal withdrawal reading (French definite article
 *   / Charter Article 2(4)) and the interpretive authority structure reading
 *   (ICJ vs. drafters vs. occupying state). The partial reading functions as
 *   a ledger: the textual ambiguity is converted into negotiating leverage
 *   for the occupying power and great power mediators, while territorial
 *   claimants lack a fixed enforcement line.
 *
 * KEY AGENTS:
 *   - Occupying power: agenda-setter/beneficiary (powerful/constrained) â retains territories under secure boundaries cover.
 *   - Great power mediators: agenda-setter/beneficiary (institutional/mobile) â control phased process and leverage.
 *   - Territorial claimants: payer (organized/constrained) â denied fixed enforcement line, dependent on mediator goodwill.
 *   - ICJ judicial authority: excluded (institutional/analytical) â competing interpretive authority sidelined by drafters'-intent framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.55).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.65).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC 242 Partial Withdrawal Reading (Indefinite Article / Secure Boundaries)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, '08b753cb-368d-421a-92f3-09795fba3af8').
narrative_ontology:cs_kernel_codification('08b753cb-368d-421a-92f3-09795fba3af8', fixed_text).
narrative_ontology:cs_authority_grounding('08b753cb-368d-421a-92f3-09795fba3af8', lineage).
narrative_ontology:cs_interpretation_layer_present('08b753cb-368d-421a-92f3-09795fba3af8').
narrative_ontology:cs_reading_relation('08b753cb-368d-421a-92f3-09795fba3af8', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('08b753cb-368d-421a-92f3-09795fba3af8', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('08b753cb-368d-421a-92f3-09795fba3af8', foundational, indefinite_article_discretionary_scope).
narrative_ontology:cs_axiom_status(indefinite_article_discretionary_scope, holdable).
narrative_ontology:cs_axiom_grounding('08b753cb-368d-421a-92f3-09795fba3af8', indefinite_article_discretionary_scope, conventional).
narrative_ontology:cs_axiom('08b753cb-368d-421a-92f3-09795fba3af8', foundational, secure_boundaries_permits_retention).
narrative_ontology:cs_axiom_status(secure_boundaries_permits_retention, holdable).
narrative_ontology:cs_axiom_grounding('08b753cb-368d-421a-92f3-09795fba3af8', secure_boundaries_permits_retention, instrumental).
narrative_ontology:cs_reference_frame('08b753cb-368d-421a-92f3-09795fba3af8', phased_security_transition_framework).
narrative_ontology:cs_drift_state('08b753cb-368d-421a-92f3-09795fba3af8', contemporary_two_state_fatigue, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08b753cb-368d-421a-92f3-09795fba3af8', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, great_power_mediators).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains strategic territories under the secure boundaries principle while engaging in phased withdrawal negotiations. The indefinite article reading of Resolution 242 provides legal and diplomatic cover to hold certain areas pending final status agreements. Exit from this position would require abandoning territorial claims or accepting a maximal withdrawal reading, both carrying prohibitive domestic political cost.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, beneficiary).

% Control the pace and scope of withdrawal through diplomatic frameworks and phased negotiation structures. The indefinite article preserves their leverage as indispensable mediators; a definitive textual mandate would reduce their role to enforcement rather than process management, diminishing their geopolitical influence.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, great_power_mediators, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, great_power_mediators, beneficiary).

% Seek full territorial restoration but face a negotiating framework where withdrawal scope is discretionary and phased. The indefinite article reading denies them a fixed enforcement line, forcing reliance on mediator goodwill and occupying power consent rather than a Charter-based legal mandate.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimants, payer,
    organized, generational, constrained, regional).

% Claims independent authority to interpret the resolution's withdrawal obligation through judicial processes. In the partial withdrawal reading, this authority is bypassed in favor of drafters' intent and diplomatic practice; ICJ adjudication would likely favor a more definite territorial integrity reading based on Charter Article 2(4).
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, icj_judicial_authority, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a phased, negotiated framework for territorial withdrawal that avoids sudden destabilizing power vacuums and preserves regional security through gradual transition managed by great powers.
% TRANSFER_FUNCTION: Converts textual indefiniteness in the English version of Resolution 242 into negotiating leverage, transferring territorial retention rights to the occupying power and mediation authority to great powers, while transferring uncertainty, delay, and diminished legal certainty to territorial claimants.
% ABSENT_VOICES: The ICJ as an independent judicial interpreter is structurally sidelined in favor of drafters' intent and diplomatic practice. Local populations in retained territories lack separate representation in the phased negotiation framework. Maximal withdrawal advocates are present in the General Assembly but marginalized in Security Council practice.
% DISAPPEARANCE_RATIONALE: If the indefinite article reading vanished and was replaced by a definite mandatory withdrawal reading, the occupying power would lose legal cover for territorial retention, mediator leverage over process timing would collapse, and territorial claimants would gain a fixed enforcement line linked to Charter Article 2(4). The entire diplomatic architecture of phased negotiation would reorganize around explicit compliance timelines.
% FOUNDING_PROBLEM: How to secure stable peace after armed conflict without requiring immediate total withdrawal that might create destabilizing power vacuums, while affirming the inadmissibility of territorial acquisition by force.
% FOUNDING_PROBLEM_CORROBORATION: Occupying power and great power mediators attest the problem requires phased, conditional withdrawal to preserve security. Territorial claimants and several drafting-state records attest the problem was intended to be solved by full withdrawal per Charter Article 2(4), with the indefinite article merely reflecting negotiators' avoidance of specifying exact lines in the resolution text. Independent legal historians and ICJ advisory opinions outside the benefiting parties support the maximal reading's textual and Charter grounding.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.55) because the constraint is genuinely conditional and phased â it does not extract everything immediately but structures a prolonged transfer of territorial control and timeline authority. Suppression is substantial (0.65) because the reading's persistence depends on actively suppressing the maximal withdrawal interpretation and excluding ICJ judicial review in favor of diplomatic practice. Theater ratio is moderate (0.42): the secure boundaries and phased negotiation rhetoric is partly functional (preventing sudden vacuums) and partly performative (maintaining the appearance of progress while retention continues). Accessibility collapse (0.65) reflects that once the indefinite article reading is accepted, the maximal reading becomes legally marginal despite its Charter grounding. Resistance is high (0.70) from claimants and General Assembly majorities.
 *
 * PERSPECTIVAL GAP:
 *   From the occupying power and mediator seats, the constraint is necessary coordination: without phased, conditional withdrawal, war would resume and power vacuums would be filled by hostile actors. From the claimant seat, the same structure is extraction: the indefinite article converts a clear post-war obligation into an indefinite negotiating horizon where possession becomes nine-tenths of the law. The engine computes this divergence from structural data â the same text produces opposite directionalities depending on exit options and beneficiary position.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying power and great power mediators are structural beneficiaries (low d): the constraint subsidizes their territorial control and diplomatic leverage, respectively. The territorial claimants are structural victims (high d): the constraint extracts a fixed legal entitlement and substitutes a discretionary diplomatic process. The ICJ sits at analytical distance (d near 0.5 or analytical exit) but is excluded from the constraint's operating framework, so its directionality is not fully engaged.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing destabilizing power vacuums after 1967 â is contested in status. If dead, the constraint persists as a zombie framework that continues to extract territorial delay. The partial reading resists mandatrophy classification because it maintains a live (if attenuated) coordination function: the secure boundaries claim is not pure theater. However, the rising theater ratio over the measurement interval suggests growing mandatrophy risk as the phased framework ages without delivering final status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_intent_vs_charter_override,
    'Does the drafters'' intent behind the indefinite article legitimately override Charter Article 2(4)''s territorial integrity default, or does the Charter principle subsume the resolution''s textual ambiguity?',
    'Archival discovery of verbatim drafting-committee negotiations; ICJ advisory proceeding explicitly weighing the English and French texts against Charter obligations.',
    'If Charter override is established, the partial reading collapses from a live legal frame to a diplomatic rationalization, reclassifying toward snare. If drafters'' intent is confirmed as a deliberate carve-out, the coordination function gains legitimacy and the tangled-rope classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_intent_vs_charter_override, conceptual, 'Whether the resolution''s textual ambiguity or the Charter''s territorial integrity principle controls.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of the maximal withdrawal reading structural (UNSC veto power, great-power gatekeeping) or internalized (acceptance among diplomatic elites that indefinite phrasing is irreducible)?',
    'Comparative analysis of General Assembly voting patterns and claimant-state litigation behavior: if claimant states continue filing maximal claims but are blocked by procedural veto, suppression is structural; if they increasingly adopt the partial framing in their own diplomatic proposals, suppression is internalized.',
    'If internalized, effective suppression exceeds the structural measure â the constraint''s extractive force is amplified by cognitive capture of the victim parties themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of the maximal withdrawal alternative.').

omega_variable(
    coordination_extraction_separability,
    'Is the secure boundaries coordination function structurally separable from the territorial retention extraction, or does retention necessarily follow from the security logic?',
    'Natural experiment from peace agreements where withdrawal occurred under explicit security guarantees without territorial retention (e.g., Sinai model): if security is maintained without retention, the functions are separable.',
    'If separable, the reading is extraction riding on coordination; if inseparable, a portion of measured extraction is the genuine price of the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether secure boundaries require territorial retention or can be achieved through other means.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unsc_tr_t10, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(unsc_tr_t20, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(unsc_tr_t30, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(unsc_tr_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(unsc_tr_t50, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unsc_be_t10, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(unsc_be_t20, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(unsc_be_t30, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(unsc_be_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(unsc_be_t50, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(unsc_su_t10, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(unsc_su_t20, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(unsc_su_t30, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(unsc_su_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(unsc_su_t50, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% The UNSC 242 withdrawal clause kernel decomposes into three structurally distinct constraints: the partial withdrawal reading (indefinite article / secure boundaries), the maximal withdrawal reading (definite article / Charter 2(4)), and the interpretive authority structure (who decides). Their epsilon values differ: the partial reading is moderate-extractive and conditional; the maximal reading is low-extractive and mandatory; the authority reading is meta-level. They share the same resolution text but instantiate different constraints with different stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
