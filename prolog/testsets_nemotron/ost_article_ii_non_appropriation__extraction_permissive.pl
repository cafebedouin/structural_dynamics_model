% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: OST Article II Non-Appropriation — Extraction-Permissive Reading
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   The Outer Space Treaty Article II provides: 'Outer space, including the
 *   Moon and other celestial bodies, is not subject to national appropriation
 *   by claim of sovereignty, by means of use or occupation, or by any other
 *   means.' The extraction-permissive reading holds that Article II bars
 *   sovereign territorial claims (planting flags, declaring sovereignty) but
 *   does not prohibit private ownership of resources once extracted — because
 *   extracted resources are no longer 'in place' in outer space and have been
 *   reduced to possession. This reading underwrites national legislation in
 *   the U.S., Luxembourg, UAE, Japan, and others authorizing private property
 *   rights in space resources. It creates a high-extractiveness ledger:
 *   access to resources is gated by technological capability and flag-state
 *   legal recognition; no compensation mechanism exists for excluded states;
 *   enclosure proceeds via fait accompli (extraction and sale) rather than
 *   formal annexation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.78).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.62).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.78).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "OST Article II Non-Appropriation — Extraction-Permissive Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '8c4b92ba-018d-4744-b7e3-fcb6726d4b15').
narrative_ontology:cs_kernel_codification('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', fixed_text).
narrative_ontology:cs_authority_grounding('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', lineage).
narrative_ontology:cs_interpretation_layer_present('8c4b92ba-018d-4744-b7e3-fcb6726d4b15').
narrative_ontology:cs_reading_relation('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', ost_article_ii_non_appropriation__commons_conservation, influences).
narrative_ontology:cs_reading_relation('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', foundational, extracted_resources_are_not_in_place).
narrative_ontology:cs_axiom_status(extracted_resources_are_not_in_place, holdable).
narrative_ontology:cs_axiom_grounding('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', extracted_resources_are_not_in_place, conventional).
narrative_ontology:cs_axiom('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', foundational, flag_state_jurisdiction_extends_to_possession).
narrative_ontology:cs_axiom_status(flag_state_jurisdiction_extends_to_possession, holdable).
narrative_ontology:cs_axiom_grounding('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', flag_state_jurisdiction_extends_to_possession, conventional).
narrative_ontology:cs_axiom('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', secondary, no_compensation_required_pending_regime).
narrative_ontology:cs_axiom_status(no_compensation_required_pending_regime, holdable).
narrative_ontology:cs_axiom_grounding('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', no_compensation_required_pending_regime, instrumental).
narrative_ontology:cs_reference_frame('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', ost_article_ii_textual_prohibition_on_sovereignty_only).
narrative_ontology:cs_drift_state('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', post_national_legislation_wave_2015_2020, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c4b92ba-018d-4744-b7e3-fcb6726d4b15', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_nation_operators).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, private_extraction_companies).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations_of_excluded_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, private_extraction_companies).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, private_property_rights_in_space_resources).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, flag_state_jurisdiction_over_extracted_materials).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, national_legislation_authorizing_resource_ownership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author domestic legislation (e.g., U.S. Commercial Space Launch Competitiveness Act 2015, Luxembourg Space Resources Law 2017) recognizing private ownership of extracted space resources. License and regulate extraction missions. Benefit from tax revenue, strategic resource access, and technological leadership. Can shift regulatory frameworks across jurisdictions if needed.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_nation_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Develop extraction technologies and business models under flag-state authorization. Invest capital in prospecting and recovery missions. Gain ownership of extracted materials under domestic law. Bear high technical and financial risk; depend on continued regulatory permission. Can relocate incorporation to favorable jurisdictions.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, private_extraction_companies, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, private_extraction_companies, payer).

% Lack independent launch capability and extraction technology. Cannot access resources directly. No compensation mechanism exists for resources removed from the common domain. Dependent on voluntary benefit-sharing frameworks that have not materialized. No exit from the constraint — cannot participate, cannot prevent enclosure.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states, excluded).

% Inherit a depleted commons where accessible high-value resources have been claimed by first movers. No legal standing to contest past appropriations. The constraint's operation creates irreversible path dependence — early extraction creates property claims that persist. Not present in current negotiations; no voice in the arrangement.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations_of_excluded_states, excluded,
    powerless, civilizational, trapped, universal).

% Analyze treaty text, state practice, and doctrinal evolution. Divided between extraction-permissive, conservation, and regime-deferral readings. Provide interpretive frameworks that states and courts may adopt. No direct stake in resource allocation; influence operates through legitimacy and epistemic authority.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% UN Committee on the Peaceful Uses of Outer Space (COPUOS) hosts the 'Space Resources' working group. Attempts to develop international framework stall due to divergence between extraction-permissive and conservation blocs. Produces non-binding guidelines; lacks enforcement authority. Institutional mandate requires consensus, giving veto power to spacefaring states.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, multilateral_institutions_copuos, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, multilateral_institutions_copuos, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal certainty for private investment in space resource extraction by recognizing ownership of extracted materials under flag-state law. Coordinates technical standards, safety protocols, and registration practices across national regimes.
% TRANSFER_FUNCTION: Moves exclusive ownership and commercial value of extracted space resources from the global commons (common heritage of mankind / province of all mankind) to private companies and their flag states. No compensatory transfer flows to non-participating states or future generations.
% ABSENT_VOICES: Non-spacefaring states (especially Global South) lack technical capacity to participate and are excluded from benefit-sharing. Future generations have no representation. The Moon Agreement (1979) parties advocate benefit-sharing but include no major spacefaring states — their position is structurally excluded from the operative regime.
% DISAPPEARANCE_RATIONALE: If the extraction-permissive reading vanished overnight, private companies would lose legal title to extracted resources under domestic law; states would revert to uncertainty about the legality of resource appropriation; investment would freeze pending multilateral resolution. The commons would remain legally open but practically inaccessible — the world rearranges around the legal vacuum.
% FOUNDING_PROBLEM: Early space law (OST 1967) left resource ownership ambiguous. Private actors needed legal certainty to invest in extraction technology. The founding problem was enabling commercial development of space resources without waiting for a universal multilateral regime that might never materialize.
% FOUNDING_PROBLEM_CORROBORATION: Spacefaring states and industry attest the problem remains live: no multilateral regime exists, and investment requires legal certainty. Conservation-reading states (Moon Agreement parties, Global South coalition) and legal scholars attest the founding problem was a strategic framing to legitimize unilateral appropriation — the 'wait for regime' option was foreclosed by the very states that benefited from extraction-permissive legislation. Academic literature outside the benefiting parties (e.g., Hobe, Tronchetti, Gabrynowicz) documents the doctrinal shift.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at present, projected to 0.85) because the constraint transfers resource value from the global commons to a small set of technologically capable actors without compensation. Suppression is moderate (0.62) — the constraint does not actively coerce non-spacefaring states; rather, it structurally excludes them through capability asymmetry and legal recognition regimes. Theater ratio is moderate (0.28) — the coordination function (legal certainty for investment) is real but increasingly serves as cover for rent capture. Accessibility collapse is moderate (0.41) — alternatives (multilateral benefit-sharing regime) remain legally possible but politically receding. Resistance is moderate (0.54) — diplomatic pushback exists but lacks enforcement leverage.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (spacefaring states), the constraint is a Rope — genuine coordination enabling investment where none existed. From the payer/excluded seats (non-spacefaring states), it is a Snare — enclosure of the commons without consent or compensation. The engine computes this divergence from the structural data; the claimed_type 'tangled_rope' acknowledges both functions coexist.
 *
 * DIRECTIONALITY LOGIC:
 *   Spacefaring nation operators and private extraction companies are structural beneficiaries (d near 0.0–0.2): they collect resource value, write the rules, and hold arbitrage-grade exit. Non-spacefaring states and future generations are structural targets (d near 0.8–1.0): they bear the opportunity cost of enclosure with trapped exit. International legal scholars and COPUOS sit near analytical/symmetric (d ~ 0.4–0.5): they observe and interpret but do not collect or pay directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (enabling commercial space development) may have outlived its coordination function if a multilateral benefit-sharing regime becomes viable. Currently, the extraction-permissive reading persists because no alternative regime has been negotiated — but the mandate drift is toward pure extraction as the coordination justification (waiting for a regime that never comes) weakens. The mandatrophy question is whether the 'bridge to a regime' has become the destination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_versus_appropriation_boundary,
    'Does ''appropriation by any other means'' in Article II encompass the de facto appropriation achieved through exclusive extraction and ownership of extracted resources, or is the prohibition limited to formal territorial sovereignty claims?',
    'ICJ advisory opinion or authoritative state practice crystallizing custom; UNGA resolution interpreting Article II; or widespread adoption of a benefit-sharing regime that treats extraction as appropriation.',
    'If extraction constitutes appropriation, the extraction-permissive reading is legally foreclosed — the constraint becomes a Mountain (treaty prohibition) rather than a Tangled Rope. If not, the reading stands and extractiveness accumulates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_versus_appropriation_boundary, conceptual, 'Whether the treaty text''s ''by any other means'' captures resource extraction as appropriation').

omega_variable(
    benefit_sharing_feasibility,
    'Is a multilateral benefit-sharing regime for space resources politically achievable, or has the extraction-permissive reading created irreversible path dependence that makes such a regime infeasible?',
    'COPUOS Space Resources Working Group progress; diplomatic signaling from major spacefaring states; economic analysis of benefit-sharing models; track record of similar regimes (deep seabed mining).',
    'If feasible, the constraint may transition toward Scaffold (transitional to regime). If infeasible, the Tangled Rope solidifies into Snare — coordination function becomes purely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_sharing_feasibility, empirical, 'Whether the coordination function''s stated telos (a multilateral regime) remains viable').

omega_variable(
    fait_accompli_irreversibility,
    'At what point does the accumulation of extraction operations and recognized property claims create a fait accompli that no subsequent regime can unwind without massive compensation claims?',
    'Track number of extraction missions, volume of resources claimed, investment sunk, and domestic legal frameworks recognizing property. Model regime-transition costs.',
    'If irreversible, the constraint''s extractiveness becomes locked in — future regime can only tax future extraction, not redress past enclosure. The Tangled Rope''s coordination function is retroactively revealed as a Snare''s cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fait_accompli_irreversibility, empirical, 'Threshold where extraction creates irreversible property claims that block equitable regime formation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(ost__tr_t1979, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1979, 0.08).
narrative_ontology:measurement(ost__tr_t1998, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(ost__tr_t2017, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2017, 0.24).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2020, 0.26).
narrative_ontology:measurement(ost__tr_t2024, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2024, 0.28).
narrative_ontology:measurement(ost__tr_t2030, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2030, 0.31).
narrative_ontology:measurement(ost__tr_t2035, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2035, 0.34).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement(ost__be_t1979, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1979, 0.18).
narrative_ontology:measurement(ost__be_t1998, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1998, 0.22).
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(ost__be_t2017, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2017, 0.62).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2020, 0.71).
narrative_ontology:measurement(ost__be_t2024, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2024, 0.76).
narrative_ontology:measurement(ost__be_t2030, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2030, 0.82).
narrative_ontology:measurement(ost__be_t2035, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2035, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.1).
narrative_ontology:measurement(ost__su_t1979, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1979, 0.15).
narrative_ontology:measurement(ost__su_t1998, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1998, 0.2).
narrative_ontology:measurement(ost__su_t2015, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(ost__su_t2017, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2017, 0.5).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(ost__su_t2024, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement(ost__su_t2030, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2030, 0.68).
narrative_ontology:measurement(ost__su_t2035, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2035, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__extraction_permissive, 0.18).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_xi_moon_agreement_benefit_sharing).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, national_space_resource_legislation_cluster).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the OST Article II non-appropriation kernel. The extraction-permissive reading enables high-extractiveness resource enclosure; the commons-conservation reading treats extraction as prohibited appropriation (Mountain); the international-regime reading defers to a future multilateral framework (Scaffold). The three readings form a constraint family linked by network.affects_constraints. The extraction-permissive reading influences both siblings: it creates facts on the ground (extraction operations, property claims) that pressure the regime reading toward collapse and the conservation reading toward irrelevance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, institutional, 0.15).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, powerful, 0.25).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, powerless, 0.9).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, organized, 0.45).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
