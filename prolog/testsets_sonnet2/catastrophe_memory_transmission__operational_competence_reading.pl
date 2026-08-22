% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__operational_competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Catastrophe-Memory Ritual as Operational Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint instantiates the operational-competence reading of the
 *   catastrophe-memory-transmission kernel: ritual observances tied to
 *   historical catastrophe (rapid departure under threat, scarcity endurance,
 *   vigilance against recurring danger) are read here as a coordination
 *   mechanism for transmitting rehearsed survival behavior across generations
 *   that never lived the original crisis. Under this reading the ritual's
 *   core justification is functional yield — does performing it correctly
 *   produce descendants with faster mobilization reflexes, better rationing
 *   discipline, sharper threat-pattern recognition — not the preservation of
 *   communal identity or mourning as such (that is the sibling
 *   symbol_continuity_reading) and not the inseparability of symbol and
 *   competence (that is the sibling hybrid_embedded_reading). This story
 *   authors only the operational-competence claim as its own clean,
 *   ε-invariant constraint; the siblings are separate files linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - future_generations: Primary beneficiary (powerless/analytical) — receive competence transfer without having consented to the rehearsal regime that produced it
 *   - household_survival_units: Administering beneficiary (moderate/constrained) — perform and transmit the rehearsal, benefiting from the resulting family-level readiness
 *   - diaspora_communities: Coordinated beneficiary (organized/constrained) — maintain a portable, location-independent competence layer across dispersed populations
 *   - literalist_practitioners_mistaking_symbol_for_substance: Payer (powerless/trapped) — bear the full ritual labor cost without decoding the embedded operational content
 *   - ritual_officiants: Agenda-setter (institutional/constrained) — design the rehearsal sequence and decide how explicit the operational teaching is
 *   - ritual_studies_scholars: Analytical observer (analytical/analytical) — assess functional yield against the competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe-Memory Ritual as Operational Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '5e42140f-c3be-41ac-ade4-ba77068e7bc4').
narrative_ontology:cs_kernel_codification('5e42140f-c3be-41ac-ade4-ba77068e7bc4', distributed).
narrative_ontology:cs_authority_grounding('5e42140f-c3be-41ac-ade4-ba77068e7bc4', practice).
narrative_ontology:cs_interpretation_layer_present('5e42140f-c3be-41ac-ade4-ba77068e7bc4').
narrative_ontology:cs_reading_relation('5e42140f-c3be-41ac-ade4-ba77068e7bc4', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e42140f-c3be-41ac-ade4-ba77068e7bc4', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('5e42140f-c3be-41ac-ade4-ba77068e7bc4', foundational, competence_is_separable_from_symbolic_form).
narrative_ontology:cs_axiom_status(competence_is_separable_from_symbolic_form, holdable).
narrative_ontology:cs_axiom_grounding('5e42140f-c3be-41ac-ade4-ba77068e7bc4', competence_is_separable_from_symbolic_form, empirically_contingent).
narrative_ontology:cs_axiom('5e42140f-c3be-41ac-ade4-ba77068e7bc4', secondary, operational_yield_is_the_correct_evaluative_frame).
narrative_ontology:cs_axiom_status(operational_yield_is_the_correct_evaluative_frame, holdable).
narrative_ontology:cs_axiom_grounding('5e42140f-c3be-41ac-ade4-ba77068e7bc4', operational_yield_is_the_correct_evaluative_frame, instrumental).
narrative_ontology:cs_reference_frame('5e42140f-c3be-41ac-ade4-ba77068e7bc4', rehearsal_based_competence_transmission).
narrative_ontology:cs_drift_state('5e42140f-c3be-41ac-ade4-ba77068e7bc4', contemporary_diaspora_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5e42140f-c3be-41ac-ade4-ba77068e7bc4', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, household_survival_units).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, literalist_practitioners_mistaking_symbol_for_substance).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, ritual_as_functional_pedagogy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit the encoded competence — rapid mobilization under threat, resource rationing under scarcity, threat-pattern recognition — without having lived the originating catastrophe themselves. They cannot consent to or opt out of the transmission before receiving it; they simply arrive already trained by rehearsal they did not choose.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_generations, beneficiary,
    powerless, civilizational, analytical, global).

% Households perform the rehearsal annually — packing symbolic bread that must be made and eaten quickly, fasting to simulate resource denial, rehearsing what to carry and what to leave. They administer the transmission to their own children and thereby both set the practice's local terms and receive its benefit in the form of a family unit that can act under duress.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, household_survival_units, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__operational_competence_reading, household_survival_units, agenda_setter).

% Dispersed communities maintain the ritual calendar across radically different host environments, which requires the coordination function to be genuinely portable and re-derivable rather than location-bound. Their exit from the practice costs them the shared competence layer that lets geographically separated households recognize the same threat-signatures and coordinate response independent of local authority.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% Perform every ritual element with full fidelity but relate to it purely as commemorative symbol, never decoding the embedded operational content (why the bread must be unleavened and eaten fast, why the scarcity fast is timed and structured the way it is). They pay an opportunity cost: they carry the full ritual burden but receive none of the competence transfer, and when actual crisis arrives they have the form without the trained reflex.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, literalist_practitioners_mistaking_symbol_for_substance, payer,
    powerless, biographical, trapped, local).

% Rabbis, elders, and community leaders design and sequence the yearly rehearsal calendar, decide how explicitly the operational content is taught versus left embedded in practice, and bear responsibility for whether the next generation actually acquires the competence or only the performance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_officiants, agenda_setter,
    institutional, generational, constrained, national).

% Analyze ritual practice for functional content — comparing operational-yield readings against symbol-continuity readings — without themselves being bound by the practice's transmission function. They can observe which households produce operationally competent descendants and which produce only faithful performers.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__operational_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__operational_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Annual rehearsal of catastrophe-response behaviors — rapid mobilization, resource rationing, threat-pattern recognition — solves the problem of transmitting hard-won survival competence across generations who have not themselves experienced the originating crisis, without requiring each generation to relearn it through actual disaster.
% TRANSFER_FUNCTION: Moves rehearsed operational capacity (readiness reflexes, scarcity-management habits, mobilization speed) from those who encode the practice (officiants, elders) to those who inherit it (children, future households), at the cost of ritual labor and cognitive effort from every participating generation.
% ABSENT_VOICES: Practitioners who experience the ritual as purely symbolic have no voice in a competence-oriented accounting of the practice's value — the operational-yield frame does not ask them what the ritual means to them, only what it trains them to do, and their felt experience of meaning is treated as a byproduct rather than the point.
% DISAPPEARANCE_RATIONALE: Under this reading, if the ritual vanished, encoded operational competence (rapid packing, rationing discipline, threat vigilance) would decay across generations who no longer rehearse it, leaving descendants with degraded crisis response even if they retained abstract historical knowledge of the catastrophe. Whether this constitutes 'the world rearranging' is contested because the operational-competence claim is itself unfalsified for most practitioners — a symbol-continuity adherent would say nothing functional was lost at all.
% FOUNDING_PROBLEM: Communities that lived through catastrophe (expulsion, famine, forced flight) needed a way to ensure their descendants, who would not experience the same catastrophe directly, retained the behavioral competence to survive a recurrence — since verbal warning alone degrades and is not rehearsed under stress-like conditions.
% FOUNDING_PROBLEM_CORROBORATION: Some historians of religion and cognitive-science-of-religion researchers (outside the practicing community) corroborate that ritual rehearsal produces measurable procedural memory effects consistent with competence transmission; however, most working rabbinic and communal authorities describe the founding purpose primarily in terms of covenant, identity, and mourning rather than operational training, and would reject the operational-competence framing as the primary or sole founding warrant.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__operational_competence_reading_tests).
:- end_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because under this reading the ritual is genuinely functional coordination — competence transfer that benefits recipients net of the cost of participation — not a mechanism that siphons value to a narrow class. Suppression is low-moderate (0.22): nothing coercively prevents exit from the practice, though social cohesion pressure exists. Theater ratio is authored at a meaningfully rising trajectory (0.25 to 0.42) because as generations grow further from the originating catastrophe, an increasing share of ritual performance risks becoming decoupled from actual competence transfer — the operational content degrades into pure performance for households and officiants who no longer explicitly teach the embedded 'why.' This is the operational-competence reading's own internal failure mode: Goodhart drift where ritual fidelity substitutes for the yield it was meant to produce. Accessibility collapse (0.35) and resistance (0.3) are moderate — alternative transmission mechanisms (formal safety education, secular crisis-preparedness training) exist and are not suppressed, consistent with a rope rather than a mountain classification, though the claimed_type is authored as rope specifically because the coordination function (competence transfer) is real, not because competence extraction is a universal law binding all cultures identically.
 *
 * PERSPECTIVAL GAP:
 *   From the household_survival_units and ritual_officiants seats, the practice reads as active, functioning coordination — rehearsal that produces real capacity. From the literalist_practitioners seat, the same structure computes very differently: they carry the full cost of participation (time, labor, fasting, ritual complexity) while receiving none of the operational yield, because they have not decoded the pattern-recognition content embedded in the practice. The engine should register this as seat divergence: the same ritual act is coordination-with-benefit from one position and cost-without-competence-transfer from another, purely as a function of whether the operational content was successfully decoded during transmission.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and diaspora communities are declared beneficiaries because they receive competence they did not have to independently develop — low d, benefit-weighted. Household units carry a dual role: they administer the practice (agenda-setting) and also benefit from raising operationally competent descendants, so their d sits near symmetric. Literalist practitioners are declared victims not because anyone extracts wealth or power from them, but because they pay the full participatory cost of a competence-transmission mechanism while failing to receive the transmission itself — an opportunity-cost victim class distinctive to this reading. Ritual officiants set the agenda but do not personally extract; their d reflects institutional stewardship rather than beneficiary capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The operational-competence reading resists mandatrophy mislabeling in the direction that matters here: it explicitly refuses to treat rising theater_ratio as proof that the ritual is 'just' extraction or pure performance. Instead the classification holds rope so long as the coordination function (competence transfer) remains genuinely present for at least a meaningful subset of practitioners, while flagging the accumulating theater_ratio as the specific mechanism by which a rope can decay toward piton over time if the operational teaching layer is not actively maintained by officiants. The founding_problem_status is authored contested rather than dead precisely because whether the underlying catastrophe-recurrence risk is still 'live' for any given diaspora community is empirically variable and not settled by the ritual's mere continuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_yield_measurability,
    'Can ''operational competence'' actually be measured as a distinct outcome of ritual participation, separable from general communal socialization, secular education, or unrelated cultural transmission?',
    'Comparative behavioral studies of crisis response (evacuation speed, resource rationing decisions, threat vigilance) across matched populations differing only in ritual participation intensity, controlling for other transmission channels.',
    'If no measurable operational signal is separable from general cultural transmission, the operational-competence reading loses its distinguishing empirical claim and collapses toward the symbol_continuity_reading or the hybrid_embedded_reading, since the specific competence payload this story hangs its ε and beneficiary structure on would be unverified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_yield_measurability, empirical, 'Whether operational competence transfer is empirically distinguishable from other cultural transmission mechanisms.').

omega_variable(
    reading_disagreement_locus,
    'Where exactly does the operational_competence_reading structurally diverge from the hybrid_embedded_reading — is it a claim about separability (competence CAN be extracted from symbolic form and taught explicitly) or merely a claim about evaluative emphasis (competence yield is what we choose to measure, even if inseparable in practice)?',
    'Ethnographic and pedagogical analysis of how officiants actually teach the ritual: if explicit, decontextualized competence instruction occurs (e.g., teaching rapid-departure logistics apart from the seder narrative), separability is supported; if the competence is never taught except embedded in full symbolic performance, the hybrid reading is better supported.',
    'If separability fails, this story''s claimed_type and victim class (literalist practitioners ''missing'' extractable content) would need revision, since there would be no separable content to miss — the hybrid_embedded_reading''s premise would better fit the observed practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_disagreement_locus, conceptual, 'Locates the specific structural disagreement between the operational and hybrid readings of the kernel.').

omega_variable(
    universal_law_vs_constructed_practice,
    'Is competence-transmission-through-ritual-rehearsal a universal cognitive/evolutionary law of human survival culture (which would push this constraint toward mountain), or is it a historically specific, constructed practice of particular communities that could equally well transmit competence through other means (supporting rope)?',
    'Cross-cultural comparative anthropology: does every human population facing recurrent catastrophe converge on ritualized rehearsal as the transmission mechanism, or do some populations achieve equivalent operational competence through secular/institutional means with no ritual component?',
    'If ritualized rehearsal is the only observed pathway across independent cultures facing similar catastrophe-recurrence risk, the constraint would be better classified mountain (a structural feature of how human cultures must transmit survival knowledge); if functionally equivalent secular alternatives exist and are equally effective, rope is the correct classification and the ritual form is one contingent implementation among several.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_law_vs_constructed_practice, conceptual, 'Whether ritual is a necessary universal mechanism for competence transmission or one contingent option among several.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 8, 0.18).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 16, 0.21).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 24, 0.24).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 32, 0.26).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__operational_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__operational_competence_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'catastrophe memory transmission via ritual' per the ε-invariance principle. operational_competence_reading (this file) authors ε=0.28 on the claim that ritual's primary function is measurable behavioral competence transfer, evaluated by operational yield. symbol_continuity_reading authors its own ε for the claim that identity/mourning preservation is the intrinsic good and symbolic continuity IS the survival mechanism (no separable competence payload asserted). hybrid_embedded_reading authors its own ε for the claim that competence and symbol are inseparable and that extraction of a 'pure' competence signal misdescribes the practice. All three share the same underlying ritual practices as their referent but diverge on what precisely is claimed to be transmitted and how it should be evaluated — hence three constraints, not one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
