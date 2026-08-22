% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money's Origin as a Conceivability Threshold (Became-Thinkable Reading)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This story locates the origin of digital money at the moment the concept
 *   became technically and institutionally CONCEIVABLE — the point when
 *   cryptographic primitives, computational settlement logic, and
 *   institutional research agendas first made non-physical, verifiable value
 *   transfer a coherent object of design, well before ordinary people held
 *   digital instruments or regulators counted them in statistics. This is
 *   deliberately the earliest-dated of three sibling readings of the same
 *   kernel (digital_money_origin): first_held_reading dates origin to first
 *   practical possession by individuals, and regulatory_recognition_reading
 *   dates it to formal incorporation into statistical/regulatory frameworks.
 *   This reading's ε differs from both siblings because the population
 *   affected is different: the conceptual-formation stage produced
 *   beneficiaries (technologists, central bank researchers, settlement
 *   institutions) who gained durable interpretive and infrastructural
 *   advantage, and victims (unbanked populations, informal economies,
 *   peripheral payment systems) who were absent from the room where the
 *   defining assumptions were set and bore downstream exclusion costs. The
 *   coordination function (a shared vocabulary for non-physical settlement)
 *   is genuine; the asymmetric extraction (standard-setting rents and
 *   exclusionary design baked in at the concept stage) is also genuine —
 *   hence tangled_rope rather than a clean rope or mountain.
 *
 * KEY AGENTS:
 *   - early_computer_scientists_and_cryptographers: primary agenda-setters (organized/arbitrage) — authored the technical vocabulary
 *   - central_bank_research_departments: primary beneficiary (institutional/arbitrage) — captured interpretive first-mover advantage
 *   - large_clearing_and_settlement_institutions: secondary beneficiary (institutional/mobile) — captured infrastructure rents
 *   - unbanked_populations_outside_conceptual_frame: primary victims (powerless/trapped) — excluded from the founding frame
 *   - monetary_historians: analytical observer — dates the conceivability threshold
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.52).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.44).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money's Origin as a Conceivability Threshold (Became-Thinkable Reading)").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, 'e64f19e9-b686-4d6c-86d1-9285a6f12b5b').
narrative_ontology:cs_kernel_codification('e64f19e9-b686-4d6c-86d1-9285a6f12b5b', distributed).
narrative_ontology:cs_authority_grounding('e64f19e9-b686-4d6c-86d1-9285a6f12b5b', practice).
narrative_ontology:cs_interpretation_layer_present('e64f19e9-b686-4d6c-86d1-9285a6f12b5b').
narrative_ontology:cs_reading_relation('e64f19e9-b686-4d6c-86d1-9285a6f12b5b', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('e64f19e9-b686-4d6c-86d1-9285a6f12b5b', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('e64f19e9-b686-4d6c-86d1-9285a6f12b5b', foundational, conceptual_formation_constitutes_origin).
narrative_ontology:cs_axiom_status(conceptual_formation_constitutes_origin, holdable).
narrative_ontology:cs_axiom_grounding('e64f19e9-b686-4d6c-86d1-9285a6f12b5b', conceptual_formation_constitutes_origin, conventional).
narrative_ontology:cs_axiom('e64f19e9-b686-4d6c-86d1-9285a6f12b5b', secondary, conceivability_precedes_and_shapes_implementation).
narrative_ontology:cs_axiom_status(conceivability_precedes_and_shapes_implementation, holdable).
narrative_ontology:cs_axiom_grounding('e64f19e9-b686-4d6c-86d1-9285a6f12b5b', conceivability_precedes_and_shapes_implementation, empirically_contingent).
narrative_ontology:cs_reference_frame('e64f19e9-b686-4d6c-86d1-9285a6f12b5b', technical_institutional_conceivability_threshold).
narrative_ontology:cs_drift_state('e64f19e9-b686-4d6c-86d1-9285a6f12b5b', post_widespread_digital_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e64f19e9-b686-4d6c-86d1-9285a6f12b5b', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_computer_scientists_and_cryptographers).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, central_bank_research_departments).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, large_clearing_and_settlement_institutions).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, unbanked_populations_outside_conceptual_frame).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, peripheral_national_payment_systems).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, informal_and_cash_economies).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, monetary_form_is_institutionally_constructed).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, conceivability_precedes_and_constrains_implementation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developed the cryptographic and protocol primitives (digital signatures, blind signatures, distributed ledgers) that made non-physical, verifiable value transfer technically conceivable. They set the terms of what would count as 'digital money' decades before the instruments were usable by the public, and their frameworks became the vocabulary everyone downstream had to adopt or argue against.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_computer_scientists_and_cryptographers, agenda_setter,
    organized, generational, arbitrage, global).

% Absorbed the emerging conceptual apparatus into internal research programs long before any statistical recognition or public rollout, giving themselves first-mover interpretive authority over what would later be certified as money. Their early framing work let them define the terms of legitimacy other actors would later have to satisfy.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, central_bank_research_departments, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, central_bank_research_departments, agenda_setter).

% Positioned themselves to capture infrastructure rents once the conceptual groundwork made digital settlement thinkable, well ahead of regulatory formalization. They benefited from being early interpreters of a still-informal concept, shaping standards before competitors or regulators could contest the frame.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, large_clearing_and_settlement_institutions, beneficiary,
    institutional, generational, mobile, global).

% Were never present in the rooms where 'digital money' was first conceptualized as a cryptographic and institutional category. The design assumptions baked in at the conceivability stage (identity verification, banking-rail interoperability, credit history) later excluded them from access even after implementation, because the founding concept was built around banked, documented populations.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, unbanked_populations_outside_conceptual_frame, payer,
    powerless, biographical, trapped, global).

% Operate payment infrastructure built on assumptions inherited from the original technical/institutional conceivability frame set by dominant economies. They must retrofit systems to standards whose foundational logic they had no part in setting, bearing integration costs for a conceptual architecture authored elsewhere.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, peripheral_national_payment_systems, payer,
    moderate, generational, constrained, national).

% Persist alongside a monetary order that increasingly treats digital instruments as the conceptual default, even though the origin-defining frameworks never accounted for cash-based, undocumented exchange. As institutions increasingly measure and regulate around the digital concept, informal economies are treated as residual or deviant rather than as a coequal monetary form.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, informal_and_cash_economies, payer,
    powerless, generational, trapped, regional).

% Study when and how the concept of digital money crystallized, comparing technical feasibility literature, institutional memos, and cryptographic publications to establish the earliest point of genuine conceivability, distinct from later adoption or regulatory events.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__became_thinkable_reading, central_bank_research_departments).
narrative_ontology:fixing_cost_class(digital_money_origin__became_thinkable_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing a shared technical and institutional vocabulary for non-physical value transfer solved a genuine coordination problem: without agreement on what would count as a valid digital instrument (verifiability, non-repudiation, settlement finality), no interoperable system could be built at all.
% TRANSFER_FUNCTION: Interpretive authority and first-mover design leverage moved from populations and institutions absent at the conceptual-formation stage toward the technologists and institutions present when the frameworks were drafted — later manifesting as infrastructure rents, standard-setting power, and access design that favored the already-banked.
% ABSENT_VOICES: Unbanked populations, informal economies, and peripheral national payment authorities had no seat at the cryptographic and institutional workshops where the concept was formed; they would have argued for design assumptions accommodating undocumented identity and offline settlement, but were not present to make that case before the frame hardened.
% DISAPPEARANCE_RATIONALE: If the conceivability-stage frameworks (protocols, verification logic, institutional research agendas) had never existed, later digital money implementations would have had to be conceptualized from scratch by different actors, likely with different inclusion assumptions — the entire subsequent build-out of settlement rails and access rules would look structurally different.
% FOUNDING_PROBLEM: Physical and paper-based settlement could not scale to the volume, speed, and verification demands of an increasingly networked, computerized economy; a conceptual and technical vocabulary for value transfer without physical tokens was needed before any implementation could proceed.
% FOUNDING_PROBLEM_CORROBORATION: Central bank research staff and early cryptographers attest the founding problem (scaling settlement beyond physical tokens) remains partially live and cite ongoing infrastructure investment. Independent monetary historians and development economists studying financial inclusion attest that the founding conceptual frame is largely resolved for banked economies but was never revised to address the populations it excluded from the start — corroboration exists outside the benefiting institutions, via financial-inclusion research literature.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52 at interval end) is moderate rather than severe: the conceptual-formation stage produced real coordination value (a workable shared vocabulary for settlement) alongside a growing asymmetric advantage captured by early institutional interpreters. Suppression (0.44) reflects that the founding frame is not literally coercive, but does require ongoing institutional maintenance (standard-setting bodies, interoperability requirements) to keep alternative framings (offline, undocumented, non-banked value transfer) marginalized. Theater ratio (0.28) is modest — most of the activity is genuine standard-building, though an increasing share over time is retrospective narrative-construction crediting particular institutions as 'first' for legitimacy purposes.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (cryptographers, central bank researchers), this looks like a rope: a necessary vocabulary had to be built, and building it first was simply diligence. From the payer seats (unbanked populations, informal economies), the same founding moment reads as an act of exclusion whose costs surfaced only later, once the frame had hardened into infrastructure they could not renegotiate. The engine should compute divergent seat classifications from this asymmetry rather than from any single averaged reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (technologists, central bank researchers, settlement institutions) get low d because they authored and control the founding vocabulary and retain arbitrage-grade exit into whichever implementation regime eventually forms. Victims (unbanked populations, informal economies, peripheral payment systems) get high d because they were structurally absent at the formation stage and are trapped or constrained relative to a frame they did not help write — their exclusion was baked in before they had any voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (settlement at computerized scale requiring a non-physical value abstraction) remains partially live for banked economies but is functionally dead for the populations the founding frame excluded — the frame was never revisited to accommodate them even as implementation proceeded elsewhere. This is not classified as pure extraction because the coordination function was and remains real for the populations it was built for; nor is it classified as pure coordination because the exclusion of absent populations was not incidental — it was structurally encoded in what 'digital money' was defined to mean.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceivability_vs_convention_boundary,
    'Is ''became technically and institutionally conceivable'' a genuine, dateable structural threshold, or is it a retrospectively constructed origin story serving the interpretive authority of the institutions that were present at that stage?',
    'Comparative historical analysis of internal institutional memos and publication records across multiple candidate ''conceivability moments'' to determine whether a discrete threshold is identifiable or whether the dating is itself an artifact of which archives survived and whose narratives dominate monetary history.',
    'If the threshold is genuinely dateable and structurally prior to implementation, this reading''s earlier-origin claim is well-founded and its beneficiary/victim structure holds. If the dating is substantially a retrospective institutional narrative, the extraction attributed to ''early architects'' is partly a story-telling artifact rather than a structural fact, and extractiveness should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceivability_vs_convention_boundary, conceptual, 'Whether the conceivability threshold is a structural fact or a retrospective institutional narrative.').

omega_variable(
    sibling_reading_divergence,
    'Given that first_held_reading and regulatory_recognition_reading are later-dated siblings within the same digital_money_origin kernel, does treating conceivability as ''origin'' overstate the causal weight of early technologists relative to the populations and regulators who later made the concept operative?',
    'Cross-reading comparison of beneficiary/victim sets and extraction trajectories across all three sibling stories to check whether the earlier dating systematically inflates the agenda-setting credit given to the became_thinkable seat.',
    'If conceivability is treated as decisive origin, institutional architects gain outsized retrospective legitimacy claims relative to implementers and regulators; if a later-dated reading is treated as decisive, the excluded-population victim structure identified here may be understated in the other readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_divergence, conceptual, 'How the choice of origin-reading among kernel siblings redistributes credit and blame across the monetary history.').

omega_variable(
    natural_evolution_vs_constructed_gatekeeping,
    'Was the exclusion of unbanked and informal economies from the founding conceptual frame an inevitable byproduct of who had access to computing and cryptographic research in that era, or was it a constructed choice that could have been otherwise had different institutions been funded or consulted?',
    'Examine funding records and consultation processes for early digital money research programs to determine whether inclusion of non-banked design constraints was considered and rejected, or never contemplated due to resource/access barriers.',
    'If inevitable given historical resource distribution, the victim structure is a background condition rather than an actively engineered exclusion, lowering the tangled_rope reading''s suppression weighting; if actively rejected despite being contemplated, the extraction and suppression scores are conservative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_evolution_vs_constructed_gatekeeping, empirical, 'Whether early exclusion from the conceptual frame was structurally inevitable or an engineered choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__became_thinkable_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(digi_tr_t10, digital_money_origin__became_thinkable_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(digi_tr_t20, digital_money_origin__became_thinkable_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(digi_tr_t30, digital_money_origin__became_thinkable_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__became_thinkable_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(digi_tr_t50, digital_money_origin__became_thinkable_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(digi_tr_t60, digital_money_origin__became_thinkable_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__became_thinkable_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(digi_be_t10, digital_money_origin__became_thinkable_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(digi_be_t20, digital_money_origin__became_thinkable_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(digi_be_t30, digital_money_origin__became_thinkable_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__became_thinkable_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(digi_be_t50, digital_money_origin__became_thinkable_reading, base_extractiveness, 50, 0.49).
narrative_ontology:measurement(digi_be_t60, digital_money_origin__became_thinkable_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__became_thinkable_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(digi_su_t10, digital_money_origin__became_thinkable_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(digi_su_t20, digital_money_origin__became_thinkable_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(digi_su_t30, digital_money_origin__became_thinkable_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__became_thinkable_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(digi_su_t50, digital_money_origin__became_thinkable_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(digi_su_t60, digital_money_origin__became_thinkable_reading, suppression_requirement, 60, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_origin__became_thinkable_reading, 0.05).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the digital_money_origin kernel. became_thinkable_reading dates origin earliest (conceptual/technical conceivability), first_held_reading dates it to first practical individual possession, and regulatory_recognition_reading dates it to formal statistical/regulatory incorporation. Each carries its own epsilon, beneficiary/victim structure, and claimed type; they are linked here for contamination/network propagation analysis, not merged into a single averaged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
