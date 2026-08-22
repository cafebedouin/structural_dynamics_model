% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__property_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animals as Property — Property Reading
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   The property_reading of the animal_moral_status kernel instantiates the
 *   constraint that animals are property/resources with no independent moral
 *   standing; their interests are subordinate to human interests by
 *   definition. This reading treats the property classification as a
 *   natural-law mountain — an irreducible feature of the moral and legal
 *   order. From this reading's perspective, the standing arrangement (animals
 *   as property) extracts almost nothing from its subjects because animals
 *   are not subjects of extraction; they are the objects over which property
 *   rights are exercised. The constraint's operation is the protection of
 *   property rights, which the reading sees as coordination (preventing
 *   conflict over resources) with negligible extraction. Beneficiaries are
 *   property owners and animal users (agriculture, research, entertainment).
 *   No victim set is declared because the reading does not recognize animals
 *   as beings whose interests can be violated — waste and inefficiency are
 *   the only recognized harms.
 *
 * KEY AGENTS:
 *   - property_owners: Primary beneficiary (institutional/biographical/arbitrage/global) — holds property rights over animals, collects all value from their use
 *   - animal_users: Primary beneficiary (organized/biographical/mobile/global) — commercial users (agriculture, research, entertainment) who derive economic value from animal use under property protection
 *   - animals: Not a stakeholder seat in this reading — the property_reading does not recognize animals as beings whose interests can be represented; they are the resource, not a party
 *   - legal_system: Agenda setter (institutional/generational/analytical/universal) — administers and enforces the property classification
 *   - welfare_advocates: Excluded (organized/biographical/constrained/national) — would argue for sentience-based constraints but are structurally excluded from the property_reading's framework
 *   - abolitionist_advocates: Excluded (organized/biographical/constrained/global) — would argue property status itself is the violation; foreclosed by this reading's core premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.08).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.15).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animals as Property — Property Reading").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, 'fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7').
narrative_ontology:cs_kernel_codification('fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7', formalized).
narrative_ontology:cs_authority_grounding('fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7', lineage).
narrative_ontology:cs_interpretation_layer_present('fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7').
narrative_ontology:cs_reading_relation('fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7', foundational, animals_have_no_independent_moral_standing).
narrative_ontology:cs_axiom_status(animals_have_no_independent_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7', animals_have_no_independent_moral_standing, deontological).
narrative_ontology:cs_axiom('fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7', foundational, property_rights_over_animals_are_natural_and_absolute).
narrative_ontology:cs_axiom_status(property_rights_over_animals_are_natural_and_absolute, holdable).
narrative_ontology:cs_axiom_grounding('fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7', property_rights_over_animals_are_natural_and_absolute, conventional).
narrative_ontology:cs_reference_frame('fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7', classical_property_framework).
narrative_ontology:cs_drift_state('fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7', contemporary_animal_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fefe4691-dc4b-4fe1-b7b1-bbeef5e598b7', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, property_owners).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_users).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, human_exceptionalism).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, natural_hierarchy_of_being).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to animals as property assets. Collect all economic value from animal use (labor, products, research data, entertainment). The property classification secures their exclusive control and enables capitalization of animal lives. Exit is arbitrary — they can sell, transfer, or change use at will with full legal protection.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, property_owners, beneficiary,
    institutional, biographical, arbitrage, global).

% Commercial operators (agricultural producers, research institutions, entertainment companies) who derive value from animal use under property law protection. The property framework guarantees their right to use animals as inputs without negotiating animal interests. They can shift species, methods, or jurisdictions with moderate friction.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_users, beneficiary,
    organized, biographical, mobile, global).

% Administers and enforces the property classification of animals through statutes, case law, and regulatory frameworks. Defines the scope of property rights, adjudicates disputes, and enforces the boundary against competing classifications (personhood, rights). The system's authority rests on the stability of the property baseline.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legal_system, agenda_setter,
    institutional, generational, analytical, universal).

% Advocates for sentience-based regulation of animal use. Their position requires recognizing animals as beings whose suffering matters — a premise the property_reading explicitly denies. They operate by lobbying for welfare statutes that create exceptions to the property baseline, but within the property_reading's framework they have no standing as parties to the constraint.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, welfare_advocates, excluded,
    organized, biographical, constrained, national).

% Advocates for recognizing animals as rights-bearing individuals whose property status is itself the fundamental violation. Their core premise (animals have rights) directly contradicts the property_reading's core premise (animals have no independent moral standing). No single legal framework can hold both premises simultaneously — the property_reading forecloses the abolitionist_reading structurally.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, abolitionist_advocates, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__property_reading, property_owners).
narrative_ontology:fixing_cost_class(animal_moral_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents conflict over animal resources by establishing clear, exclusive title — one owner per animal, enforceable against all others. Solves the coordination problem of competing claims to animal bodies, labor, and products.
% TRANSFER_FUNCTION: Moves all value generated by animal lives (labor, reproduction, bodies, data) from the animals (who have no claim) to the property owners/users. The transfer is total — animals retain zero residue of the value they produce.
% ABSENT_VOICES: Animals themselves are the primary absent voice — the property_reading constitutes them as non-parties by definition. Welfare advocates (who would argue for suffering constraints) and abolitionist advocates (who would argue for rights) are structurally excluded from the constraint's own framework. They exist in the world but not in this reading's stakeholder set.
% DISAPPEARANCE_RATIONALE: If the property classification vanished overnight, the entire legal and economic infrastructure of animal use would collapse: no clear title to animals, no basis for commercial agriculture/research/entertainment as currently structured, no enforceable boundary against competing claims. The world would rearrange radically — new frameworks (guardianship, personhood, commons) would have to be constructed.
% FOUNDING_PROBLEM: Preventing violent conflict and inefficiency in human competition over animal resources by establishing a single, clear, enforceable title regime — one owner, exclusive rights, no ambiguity.
% FOUNDING_PROBLEM_CORROBORATION: Property law scholars and agricultural economists (outside the immediate beneficiary set of current animal users) attest that the property classification continues to solve the coordination problem of resource competition — clear title reduces transaction costs and prevents the tragedy of the commons for animal resources. No significant voice outside the beneficiary set argues the founding problem is dead; welfare and abolitionist critiques target the victim structure, not the coordination function.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__property_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_moral_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_moral_status__property_reading),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_moral_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is v_low (0.08) because from the property_reading's perspective, the property regime is a coordination mechanism that prevents conflict over animal resources with minimal overhead — it is the baseline, not an imposition. Suppression is low (0.15) because the constraint operates through standard property law enforcement, not targeted coercion against a resistant class (animals cannot resist in this framework). Theater ratio is near zero (0.05) — the constraint performs its stated function (protecting property rights) with minimal performative excess. Accessibility collapse is very high (0.88) — the property classification makes alternatives (animals as rights-bearers, animals as persons) conceptually inaccessible within the framework. Resistance is low (0.12) — resistance appears only from excluded voices (welfare/abolitionist advocates) who are not recognized as parties within this reading.
 *
 * PERSPECTIVAL GAP:
 *   The property_reading computes as mountain from its own seat (property owners, animal users, legal system). The welfare_reading and abolitionist_reading compute as extractive constraints over the SAME standing arrangement because they recognize animals as subjects with interests. The engine's per-seat classification will show this divergence: the property_reading's claimed mountain type holds for its declared stakeholders; the welfare_reading's stakeholders (including animals as victims) would compute tangled_rope or snare. This perspectival gap is the kernel contest itself — not an error, the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners and animal users are beneficiaries (d near 0.0) — the constraint protects their rights and enables value extraction from animals. The legal system is agenda_setter (d ~0.5) — administers the framework symmetrically. Animals are not in the stakeholder set (no directionality computed) — the reading's structural premise is that animals cannot be targets of extraction because they lack moral standing. Welfare and abolitionist advocates are excluded (no seat) — their structural position is foreclosed by the reading's core axiom.
 *
 * MANDATROPHY ANALYSIS:
 *   The property_reading does not present as mandatrophy — its founding problem (preventing conflict over animal resources, establishing clear title) remains live from the reading's perspective. The arrangement persists because it continues to solve the coordination problem it was built for. No sunset clause exists because the reading treats the property classification as permanent (natural law). The welfare and abolitionist readings would diagnose mandatrophy (the founding problem is dead/contested, the arrangement persists as extraction), but from this reading's seat, the mandate is intact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_property,
    'Is the property status of animals a genuine natural-law constraint (irreducible hierarchy of being) or a constructed legal arrangement that benefits identifiable agents?',
    'Historical analysis of when and how animals were legally constituted as property across jurisdictions; comparative study of legal systems that never adopted the property classification for animals; examination of whether the property status tracks any empirical discontinuity in moral cognition.',
    'If constructed, the constraint is a false summit — a mountain claim that conceals a tangled_rope or snare structure with identifiable beneficiaries (property owners/users) and excluded victims (animals). FSM signature would fire and reclassify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_property, conceptual, 'Natural-law vs. constructed status of animal property classification').

omega_variable(
    kernel_reading_identity,
    'This constraint is the property_reading of kernel animal_moral_status. Sibling readings: welfare_reading (sentience-based regulation), abolitionist_reading (rights-bearing individuals). What does this reading foreclose or influence?',
    'Map the structural delta between readings: property_reading has no victim set for animals, no use constraints, only waste/inefficiency limits; welfare_reading adds sentience-based victim set and suffering-minimization constraints; abolitionist_reading makes property status itself the violation. The property_reading''s core premise (animals have no independent moral standing) directly contradicts abolitionist_reading''s core premise (animals are rights-bearers).',
    'Determines reading_relations in cs_structure: property_reading forecloses abolitionist_reading within any single legal framework; coexists_with welfare_reading as competing regulatory regimes; influences welfare_reading by setting the baseline from which welfare regulations are departures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural relationship of property_reading to sibling readings of animal_moral_status kernel').

omega_variable(
    epsilon_invariance_referent,
    'Does the v_low epsilon (0.08) correctly track the standing arrangement (property law as it operates) from this reading''s lights, or does it inadvertently measure the reading''s endorsed ideal?',
    'Audit the extractiveness value against the referent rule: for a kernel-reading story, epsilon''s referent is the standing arrangement under contest — the existing property-law regime — assessed by the reading''s own lights. The property_reading sees the property regime as nearly non-extractive (protects owner interests, minimal friction). The welfare_reading sees the same regime as extractive (permits suffering). The abolitionist_reading sees it as maximally extractive (theft of rights). Each reading authors its own epsilon over the SAME referent.',
    'Confirms epsilon invariance compliance. A reading that authors epsilon for its own ideal rather than the standing arrangement violates the ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_invariance_referent, conceptual, 'Epsilon referent discipline for kernel-reading stories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__property_reading, 0.15).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_welfare_regulation).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_cruelty_law).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, veterinary_standards).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_research_ethics).

% DUAL FORMULATION NOTE:
% Part of the animal_moral_status constraint family with welfare_reading and abolitionist_reading. This reading establishes the property baseline; welfare_reading layers sentience-based constraints atop it; abolitionist_reading rejects the baseline entirely. The three readings share the same referent (the standing property-law regime) but author different epsilon values, victim sets, and claimed types per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
