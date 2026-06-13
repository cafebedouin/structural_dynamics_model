% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Divine Legitimacy
 *   domain: ancient_history/religious_studies/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   This constraint describes the radical religious and political system
 *   imposed by Pharaoh Akhenaten, where divine legitimacy flowed exclusively
 *   through his revelation of Aten as the sole, universal deity. All other
 *   gods, particularly Amun-Ra, were declared false, their temples
 *   dismantled, and their priesthoods dispossessed. This created a
 *   centralized interpretive monopoly for the pharaoh and a new,
 *   royal-centric religious economy. This is one reading of the
 *   'divine_legitimacy_substrate' kernel, specifically the
 *   'atenist_monotheistic_reading'.
 *
 * KEY AGENTS:
 *   - pharaoh_akhenaten: Agenda setter (institutional/arbitrage) — sole interpreter and enforcer of the new cult.
 *   - royal_family: Beneficiary (powerful/constrained) — elevated status and new religious authority.
 *   - atenist_priesthood: Beneficiary (organized/constrained) — new elite religious class, dependent on pharaoh.
 *   - amun_priesthood: Payer (institutional/trapped) — dispossessed, suppressed, and persecuted.
 *   - traditional_temple_economies: Payer (institutional/trapped) — dismantled and expropriated.
 *   - general_populace: Payer (powerless/identity_locked) — forced to abandon traditional worship, pay taxes to new cult.
 *   - local_cults: Payer (moderate/constrained) — suppressed and absorbed into the Atenist framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.85).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.95).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, snare).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, 'ba8ec305-b0e9-4575-bad2-07b1ef1fa72b').
narrative_ontology:cs_kernel_codification('ba8ec305-b0e9-4575-bad2-07b1ef1fa72b', formalized).
narrative_ontology:cs_authority_grounding('ba8ec305-b0e9-4575-bad2-07b1ef1fa72b', lineage).
narrative_ontology:cs_interpretation_layer_present('ba8ec305-b0e9-4575-bad2-07b1ef1fa72b').
narrative_ontology:cs_reading_relation('ba8ec305-b0e9-4575-bad2-07b1ef1fa72b', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('ba8ec305-b0e9-4575-bad2-07b1ef1fa72b', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('ba8ec305-b0e9-4575-bad2-07b1ef1fa72b', foundational, aten_is_sole_creator_god).
narrative_ontology:cs_axiom_status(aten_is_sole_creator_god, holdable).
narrative_ontology:cs_axiom_grounding('ba8ec305-b0e9-4575-bad2-07b1ef1fa72b', aten_is_sole_creator_god, theological).
narrative_ontology:cs_axiom('ba8ec305-b0e9-4575-bad2-07b1ef1fa72b', foundational, pharaoh_is_sole_interpreter_of_aten).
narrative_ontology:cs_axiom_status(pharaoh_is_sole_interpreter_of_aten, holdable).
narrative_ontology:cs_axiom_grounding('ba8ec305-b0e9-4575-bad2-07b1ef1fa72b', pharaoh_is_sole_interpreter_of_aten, theological).
narrative_ontology:cs_reference_frame('ba8ec305-b0e9-4575-bad2-07b1ef1fa72b', pharaonic_divine_monopoly).
narrative_ontology:cs_drift_state('ba8ec305-b0e9-4575-bad2-07b1ef1fa72b', post_akhenaten_restoration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('ba8ec305-b0e9-4575-bad2-07b1ef1fa72b', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, royal_family).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, atenist_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_temple_economies).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, general_populace).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, local_cults).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, scribes_and_artists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sole authority and interpreter of Aten's will, initiating and enforcing the monotheistic reforms. He benefits from absolute religious and political power, consolidating all divine legitimacy in his person.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten, agenda_setter,
    institutional, biographical, arbitrage, national).

% Elevated to a unique position as intermediaries between Aten and the people, sharing in the pharaoh's divine authority and benefiting from the new religious economy.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, royal_family, beneficiary,
    powerful, generational, constrained, national).

% A newly established priestly class, loyal to Akhenaten and Aten, who administer the new cult and benefit from its resources, replacing the traditional priesthoods.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, atenist_priesthood, beneficiary,
    organized, biographical, constrained, national).

% The formerly powerful priesthood of Amun-Ra, dispossessed, persecuted, and stripped of their wealth, influence, and religious authority. Their very existence is a threat to the new order.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    institutional, generational, trapped, national).

% The vast economic networks centered around the traditional temples, which were dismantled, their assets confiscated, and their labor redirected to the Aten cult. This caused widespread economic disruption.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_temple_economies, payer,
    institutional, generational, trapped, national).

% Forced to abandon centuries of polytheistic worship and adopt the new, abstract Aten cult, which was less accessible and less integrated into daily life. They bore the spiritual and social costs of this upheaval.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, general_populace, payer,
    powerless, biographical, identity_locked, local).

% Smaller, regional cults and household deities that were suppressed or absorbed into the Atenist framework, losing their distinct identities and practices. Their local autonomy was eradicated.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, local_cults, payer,
    moderate, generational, constrained, local).

% Commissioned to create new Atenist iconography and texts, benefiting from royal patronage and the demand for new artistic and literary forms, but constrained by the pharaoh's strict ideological control.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, scribes_and_artists, beneficiary,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aimed to coordinate religious belief and practice around a single, universal deity, thereby centralizing political and spiritual authority under the pharaoh and eliminating competing power centers.
% TRANSFER_FUNCTION: Transferred immense wealth, land, and labor from the traditional temple estates to the royal treasury and the new Aten cult. It also transferred spiritual authority and interpretive power from diverse priesthoods to the pharaoh alone.
% ABSENT_VOICES: The voices of the dispossessed Amun priesthood, the traditional nobility, and the general populace who clung to their ancestral gods were actively suppressed. Their objections were met with persecution, not dialogue, and their practices were forcibly eradicated.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the entire religious, political, and economic structure of Egypt would immediately revert to its traditional polytheistic forms. The Amun priesthood would reclaim its power, temples would be rebuilt, and the pharaoh's absolute authority would be challenged, leading to widespread societal reorganization.
% FOUNDING_PROBLEM: The perceived problem was the excessive power and wealth of the Amun priesthood, which rivaled that of the pharaoh, and a desire to establish a more universal and abstract form of worship centered on the sun disk, Aten.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and archaeological evidence from outside the Atenist royal court (e.g., later Egyptian historians, the rapid post-Akhenaten restoration of polytheism) corroborate that the Amun priesthood's power was indeed a challenge, but also that the monotheistic solution was an imposed, short-lived deviation, not a widely accepted resolution to a 'live' problem for the populace.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a Snare due to its high extractiveness (0.85) and extreme suppression (0.95). The pharaoh and his new priesthood extracted immense wealth and power by dismantling the old religious infrastructure and centralizing all divine authority. Suppression was total, involving the erasure of names, destruction of images, and persecution of dissenters. Theater ratio is low (0.1) because the enforcement was brutally direct and functional, not merely performative. The rapid increase in extractiveness and suppression over the interval reflects the escalating intensity of Akhenaten's reforms.
 *
 * PERSPECTIVAL GAP:
 *   From the pharaoh's perspective, this was a necessary, divinely ordained reform to establish true worship. From the perspective of the dispossessed Amun priesthood and the general populace, it was a coercive imposition that extracted their spiritual and material resources. The engine's classification will reflect the latter, more extractive view, driven by the high suppression and victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh Akhenaten and the royal family are clear beneficiaries (d=0.0-0.1) as they gain absolute religious and political power. The Atenist priesthood also benefits (d=0.1-0.2) from their new elite status. The Amun priesthood, traditional temple economies, and local cults are direct targets (d=0.9-1.0) as they are dispossessed and suppressed. The general populace is also a target (d=0.8-0.9), forced to abandon ancestral practices and contribute to the new cult.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to establish Aten as the sole deity and Akhenaten as his sole interpreter. This mandate was 'live' during Akhenaten's reign but was highly contested and ultimately failed, leading to a rapid reversal after his death. The classification as a Snare prevents mislabeling this as a genuine coordination (Rope) or natural law (Mountain), accurately reflecting its coercive and extractive nature. The short interval reflects the constraint's brief, intense, and ultimately unsustainable imposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine, stable monotheistic system, or a temporary, enforced reading of a polytheistic kernel?',
    'Historical analysis of post-Akhenaten religious practice: if polytheism rapidly reasserts itself, it indicates the monotheistic reading was an enforced deviation, not a stable shift in the kernel.',
    'If a temporary reading, the constraint''s true extractiveness and suppression are higher, as its persistence relies entirely on coercion against a deeply embedded alternative. If a stable shift, it would be closer to a Mountain for its beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'This constraint is the ''atenist_monotheistic_reading'' of the ''divine_legitimacy_substrate'' kernel. Sibling readings include ''amun_polytheistic_reading'' and ''folk_syncretistic_reading''. This reading structurally forecloses the others by declaring all other gods false.').

omega_variable(
    pharaonic_interpretive_monopoly,
    'To what extent was the pharaoh''s interpretive monopoly over Aten''s nature genuinely accepted by the populace versus merely enforced?',
    'Archaeological evidence of private religious practice, non-Atenist iconography in homes, or resistance narratives from non-royal sources.',
    'If acceptance was low, the suppression metric is an underestimate of the actual coercive force required, and the constraint''s stability was even more precarious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaonic_interpretive_monopoly, empirical, 'Assesses the depth of popular acceptance of the pharaoh''s exclusive interpretive role.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.15).
narrative_ontology:measurement(divi_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.1).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(divi_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.75).
narrative_ontology:measurement(divi_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(divi_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.85).
narrative_ontology:measurement(divi_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'atenist_monotheistic_reading' of the 'divine_legitimacy_substrate' kernel. It structurally forecloses the 'amun_polytheistic_reading' and 'folk_syncretistic_reading' by declaring all other gods false and dismantling their infrastructure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
