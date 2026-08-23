% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: OST Article II Extraction-Permissive Reading
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   The Outer Space Treaty Article II declares that outer space is not
 *   subject to national appropriation by claim of sovereignty, use, or
 *   occupation. The extraction-permissive reading — advanced by the US (2015
 *   CSLCA), Luxembourg (2017 Space Resources Law), UAE, Japan, and others —
 *   interprets this as banning only territorial sovereignty claims, not
 *   private ownership of resources once extracted. This reading enables a
 *   high-extractiveness ledger: access to space resources is gated by
 *   technological capability and flag-state legal recognition; no
 *   compensation mechanism exists for excluded states; enclosure occurs via
 *   fait accompli (first extraction creates de facto property) rather than
 *   formal annexation. The constraint is actively enforced through domestic
 *   licensing regimes and flag-state recognition, creating a tangled rope:
 *   genuine coordination (legal certainty for investment) fused with
 *   asymmetric extraction (capable actors enclose the commons).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.82).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.68).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.82).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "OST Article II Extraction-Permissive Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '9041a72f-7bef-4c75-a554-6d37e224f15c').
narrative_ontology:cs_kernel_codification('9041a72f-7bef-4c75-a554-6d37e224f15c', fixed_text).
narrative_ontology:cs_authority_grounding('9041a72f-7bef-4c75-a554-6d37e224f15c', lineage).
narrative_ontology:cs_interpretation_layer_present('9041a72f-7bef-4c75-a554-6d37e224f15c').
narrative_ontology:cs_reading_relation('9041a72f-7bef-4c75-a554-6d37e224f15c', ost_article_ii_non_appropriation__commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('9041a72f-7bef-4c75-a554-6d37e224f15c', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('9041a72f-7bef-4c75-a554-6d37e224f15c', foundational, appropriation_requires_territorial_sovereignty).
narrative_ontology:cs_axiom_status(appropriation_requires_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('9041a72f-7bef-4c75-a554-6d37e224f15c', appropriation_requires_territorial_sovereignty, conventional).
narrative_ontology:cs_axiom('9041a72f-7bef-4c75-a554-6d37e224f15c', foundational, extracted_resources_are_property_not_territory).
narrative_ontology:cs_axiom_status(extracted_resources_are_property_not_territory, holdable).
narrative_ontology:cs_axiom_grounding('9041a72f-7bef-4c75-a554-6d37e224f15c', extracted_resources_are_property_not_territory, conventional).
narrative_ontology:cs_axiom('9041a72f-7bef-4c75-a554-6d37e224f15c', secondary, flag_state_recognition_suffices_for_title).
narrative_ontology:cs_axiom_status(flag_state_recognition_suffices_for_title, holdable).
narrative_ontology:cs_axiom_grounding('9041a72f-7bef-4c75-a554-6d37e224f15c', flag_state_recognition_suffices_for_title, conventional).
narrative_ontology:cs_reference_frame('9041a72f-7bef-4c75-a554-6d37e224f15c', ost_article_ii_textual_sovereignty_ban).
narrative_ontology:cs_drift_state('9041a72f-7bef-4c75-a554-6d37e224f15c', post_2015_national_laws, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9041a72f-7bef-4c75-a554-6d37e224f15c', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, private_extraction_companies).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, flag_states_with_registries).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, global_commons_interest).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, private_property_rights_in_space).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, flag_state_jurisdiction_primacy).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, appropriation_by_extraction_not_territory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with launch capability and domestic space legislation (US, Luxembourg, UAE, Japan) that authorize private resource extraction and recognize property rights in extracted materials. They set the legal framework, license operators, and provide flag-state recognition that makes extraction claims enforceable. They benefit from tax revenue, strategic resources, and industrial development while bearing minimal cost.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Companies (Planetary Resources legacy, AstroForge, TransAstra, etc.) that raise capital on the legal certainty of extraction rights. They invest in prospecting and extraction technology, claim ownership of extracted resources, and sell or use them. Their exit is constrained by sunk R&D costs and the regulatory framework they lobbied for; they cannot easily pivot if the legal regime changes.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, private_extraction_companies, beneficiary,
    powerful, biographical, constrained, global).

% States that maintain space object registries and offer favorable flag-state recognition for extraction ventures (Luxembourg, UAE, Isle of Man). They collect registration fees, attract corporate headquarters, and gain diplomatic leverage. They can exit by amending domestic law but compete to attract the industry.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, flag_states_with_registries, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, flag_states_with_registries, agenda_setter).

% The majority of UN member states without independent launch or extraction capability. They are excluded from resource access by the technological gate, receive no compensation under this reading, and cannot practically exit the treaty regime. Their only leverage is diplomatic coalition-building in UNCOPUOS, which has not produced a binding benefit-sharing mechanism.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states, payer,
    moderate, generational, trapped, global).

% Generations who will inherit a depleted orbital and celestial commons without having participated in the enclosure decisions. They have no voice in current legal interpretation, no exit from the planet, and no mechanism to claim a share of extracted value. The reading's fait accompli logic locks in first-mover advantage permanently.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations, payer,
    powerless, civilizational, trapped, universal).

% The abstract collective interest in preserving space as a province of all mankind (OST Article I) and preventing unilateral appropriation. This is not an actor but a structural position that the extraction-permissive reading renders voiceless. It would object to enclosure without benefit-sharing but has no institutional seat in the current regime.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, global_commons_interest, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__extraction_permissive, global_commons_interest).

% The UN forum where the appropriation question is debated. It produces working papers and consensus reports but has no enforcement power. Its members include both spacefaring and non-spacefaring states, creating a contested analytical seat that sees the full structural asymmetry but cannot compel a resolution.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, un_copuos_legal_subcommittee, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal certainty for capital-intensive space resource extraction by clarifying that extracted materials can be owned, enabling investment, insurance markets, and commercial planning. Solves the 'who may extract and under what authority' problem for private actors.
% TRANSFER_FUNCTION: Moves the value of space resources (water, platinum-group metals, rare earths, helium-3) from the global commons to private extractors and their flag states, without compensation to excluded states or a global fund. The transfer is gated by technological capability and flag-state recognition.
% ABSENT_VOICES: Non-spacefaring states (the global majority) and future generations are structurally excluded. The Moon Agreement (1979) attempted to give them a voice via an international regime but was not ratified by any spacefaring state. Their objection would be to the absence of benefit-sharing, but they are not in the room where domestic licensing laws are written.
% DISAPPEARANCE_RATIONALE: If the extraction-permissive reading vanished overnight, domestic licensing laws (US Commercial Space Launch Competitiveness Act, Luxembourg Space Resources Law, etc.) would lose their treaty basis. Extraction ventures would face legal uncertainty, investment would freeze, and the question would revert to UNCOPUOS for a multilateral regime — the global commons interest would reassert itself as the default.
% FOUNDING_PROBLEM: The 1967 Outer Space Treaty left resource ownership ambiguous. By the 2010s, private companies needed legal certainty to raise capital for asteroid mining. Spacefaring states unilaterally clarified the ambiguity in favor of extraction rights to unlock commercial investment.
% FOUNDING_PROBLEM_CORROBORATION: Spacefaring states and industry attest the problem is live (investment requires certainty). The Moon Agreement parties, UNCOPUOS working groups, and legal scholars (e.g., Tronchetti, Hertzfeld, von der Dunk) attest the founding problem was not 'legal certainty for extraction' but 'equitable sharing of benefits' — and that the extraction-permissive reading solves the wrong problem. No neutral third party corroborates the extraction-permissive framing as the only solution.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.82) because the reading enables unilateral enclosure of a global commons without benefit-sharing. Suppression is moderate-high (0.68) because the technological gate and legal framework structurally exclude non-spacefaring states — they cannot practically access resources or exit the treaty. Theater ratio (0.42) reflects that the coordination function (legal certainty) is real but increasingly serves as cover for extraction; the Moon Agreement's benefit-sharing mechanism was abandoned by spacefaring states. Accessibility collapse (0.71) is high because the reading collapses the alternative (international regime with benefit-sharing) by presenting unilateral domestic law as the only viable path. Resistance (0.55) is moderate: UNCOPUOS debates continue but have not produced binding alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the spacefaring state seat, the constraint is a rope: it coordinates investment, provides legal certainty, and solves a genuine collective-action problem (who may extract). From the non-spacefaring state seat, it is a snare: the coordination story is cover for enclosing a commons they cannot access. The engine computes this divergence from the structural data — the claimed type (tangled_rope) acknowledges both coordination and extraction are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Spacefaring states and flag states are structural beneficiaries (d near 0.0-0.2): they set the rules, collect revenue, and face minimal cost. Private extraction companies are beneficiaries with constrained exit (d ~0.25): they gain legal certainty but are locked into the regulatory framework. Non-spacefaring states are payers with trapped exit (d ~0.9): they bear the opportunity cost of enclosure with no voice. Future generations are ultimate payers with no exit (d ~1.0). The global commons interest is excluded entirely. Flag states with registries occupy a dual role: they administer the constraint (agenda_setter) and collect fees (beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legal certainty for commercial extraction) is contested — spacefaring states say it is live; the Moon Agreement parties say the real problem was equitable sharing, which remains unsolved. The extraction-permissive reading prevents mandatrophy resolution by presenting itself as the only solution to a problem it redefined. If the founding problem is 'how to share space resources equitably,' the arrangement is mandatrophic (persists after its function — equitable sharing — is abandoned). The reading's persistence depends on suppressing the international regime alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Is the disagreement between extraction_permissive and commons_conservation located in the text of Article II (''appropriation... by any other means''), the drafting history (travaux préparatoires), the subsequent practice of states, or the object and purpose of the treaty (province of all mankind)?',
    'ICJ advisory opinion or UNCOPUOS consensus on treaty interpretation methodology; failing that, scholarly consensus on VCLT Articles 31-32 application to Article II.',
    'If the disagreement is textual, the extraction_permissive reading has stronger footing (territory vs. resources distinction). If it is in object and purpose, commons_conservation gains ground. The location determines whether the extraction_permissive reading is a plausible interpretation or a structural rewrite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Where the interpretive disagreement between sibling readings is structurally located.').

omega_variable(
    benefit_sharing_feasibility,
    'Is a functional international benefit-sharing regime (per Moon Agreement Article 11) politically achievable, or has the extraction-permissive reading made it structurally impossible by creating faits accomplis?',
    'Track ratification of Moon Agreement vs. adoption of national space resource laws; monitor UNCOPUOS Working Group on Legal Aspects of Space Resource Activities for consensus emergence.',
    'If benefit-sharing is achievable, the extraction_permissive reading is a temporary deviation (scaffold toward regime). If faits accomplis have made it impossible, the reading is a permanent snare/tangled_rope enclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(benefit_sharing_feasibility, empirical, 'Whether the international regime alternative remains viable or has been foreclosed by unilateral action.').

omega_variable(
    flag_state_capture_risk,
    'Do flag states with registries (Luxembourg, UAE) genuinely regulate extraction, or do they compete in a race to the bottom to attract corporate registrations, effectively capturing the regulatory function for revenue?',
    'Compare domestic licensing standards across flag states; analyze whether any extraction license has been denied or revoked for environmental/equity reasons.',
    'If flag states are captured, the agenda_setter role is compromised — the coordination function is a sham and the constraint is closer to snare. If they genuinely regulate, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flag_state_capture_risk, empirical, 'Whether flag-state recognition is genuine regulation or regulatory arbitrage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost_a2_extract_tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(ost_a2_extract_tr_t1979, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1979, 0.15).
narrative_ontology:measurement(ost_a2_extract_tr_t1998, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1998, 0.2).
narrative_ontology:measurement(ost_a2_extract_tr_t2015, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(ost_a2_extract_tr_t2017, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2017, 0.35).
narrative_ontology:measurement(ost_a2_extract_tr_t2020, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(ost_a2_extract_tr_t2025, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(ost_a2_extract_be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement(ost_a2_extract_be_t1979, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1979, 0.18).
narrative_ontology:measurement(ost_a2_extract_be_t1998, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1998, 0.22).
narrative_ontology:measurement(ost_a2_extract_be_t2015, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(ost_a2_extract_be_t2017, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2017, 0.62).
narrative_ontology:measurement(ost_a2_extract_be_t2020, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2020, 0.71).
narrative_ontology:measurement(ost_a2_extract_be_t2025, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(ost_a2_extract_su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.1).
narrative_ontology:measurement(ost_a2_extract_su_t1979, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1979, 0.25).
narrative_ontology:measurement(ost_a2_extract_su_t1998, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1998, 0.3).
narrative_ontology:measurement(ost_a2_extract_su_t2015, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(ost_a2_extract_su_t2017, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2017, 0.52).
narrative_ontology:measurement(ost_a2_extract_su_t2020, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(ost_a2_extract_su_t2025, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__extraction_permissive, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, moon_agreement_article_11_benefit_sharing).

% DUAL FORMULATION NOTE:
% This constraint family (ost_article_ii_non_appropriation) decomposes the single treaty article into three structurally distinct readings with different ε values. The extraction_permissive reading (this story) has ε=0.82; commons_conservation has ε≈0.15; international_regime has ε≈0.35 (deferral creates uncertainty extraction). They are linked because the extraction_permissive reading cites the absence of an international regime as justification, while the international_regime reading treats that absence as the reason extraction is premature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, organized, 0.15).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, powerful, 0.25).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, moderate, 0.88).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
