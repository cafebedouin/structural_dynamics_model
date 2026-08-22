% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__hybrid_scaffolding_reading, []).

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
 *   constraint_id: zero_as_number_entry__hybrid_scaffolding_reading
 *   human_readable: Zero-as-Number Thinkability via Hybrid Scaffolding
 *   domain: history_of_mathematics/philosophy_of_mathematics/conceptual_history
 *
 * SUMMARY:
 *   This constraint is the hybrid_scaffolding_reading of the
 *   zero_as_number_entry kernel. It treats zero-as-number not as a raw
 *   invention transmitted from India to Europe, nor as a universal logical
 *   necessity discoverable by any tradition, but as a latent mathematical
 *   structure made operationally thinkable by specific conceptual
 *   scaffolding. Indian philosophical traditions (sunya, emptiness,
 *   place-value positional notation) provided this scaffolding earlier than
 *   European traditions. When Islamic and later European mathematicians
 *   encountered Indian mathematics, contact did not merely transmit a foreign
 *   concept; it triggered recognition of a structure already latent in
 *   positional notation, provided the receiving tradition had sufficiently
 *   compatible algebraic scaffolding. The Greek geometric-algebraic
 *   tradition, locked into a framework where numbers required geometric
 *   correlates and nothingness could not be a quantity, bore the structural
 *   cost of this scaffolding requirement. The constraint coordinates
 *   mathematical practice across traditions that share compatible scaffolding
 *   while structurally excluding those with incompatible frameworks.
 *
 * KEY AGENTS:
 *   - Hindu algebraic tradition (beneficiary, analytical exit) â originated the conceptual scaffolding of emptiness and positional notation that operationalized zero-as-number.
 *   - Greek geometric algebra tradition (payer, identity_locked) â locked into incompatible geometric-Aristotelian scaffolding; could not operationalize zero despite its latent mathematical availability.
 *   - Islamic mathematical tradition (beneficiary, mobile exit) â transmitted and synthesized Indian scaffolding, expanding the coordination domain westward.
 *   - European algebraic tradition (beneficiary, constrained exit) â recognized latent structure through contact; integrated zero once scaffolding was available.
 *   - Modern historians of mathematics (observer, analytical) â analyze the kernel contest without institutional stake in any tradition's priority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__hybrid_scaffolding_reading, 0.42).
domain_priors:suppression_score(zero_as_number_entry__hybrid_scaffolding_reading, 0.18).
domain_priors:theater_ratio(zero_as_number_entry__hybrid_scaffolding_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__hybrid_scaffolding_reading, rope).
narrative_ontology:human_readable(zero_as_number_entry__hybrid_scaffolding_reading, "Zero-as-Number Thinkability via Hybrid Scaffolding").
narrative_ontology:topic_domain(zero_as_number_entry__hybrid_scaffolding_reading, "history_of_mathematics/philosophy_of_mathematics/conceptual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__hybrid_scaffolding_reading, 'd6c35852-4a88-43f6-af92-ff59488a0d69').
narrative_ontology:cs_kernel_codification('d6c35852-4a88-43f6-af92-ff59488a0d69', distributed).
narrative_ontology:cs_authority_grounding('d6c35852-4a88-43f6-af92-ff59488a0d69', practice).
narrative_ontology:cs_interpretation_layer_present('d6c35852-4a88-43f6-af92-ff59488a0d69').
narrative_ontology:cs_reading_relation('d6c35852-4a88-43f6-af92-ff59488a0d69', zero_as_number_entry__contingent_thinkability_reading, influences).
narrative_ontology:cs_reading_relation('d6c35852-4a88-43f6-af92-ff59488a0d69', zero_as_number_entry__universal_discovery_reading, coexists_with).
narrative_ontology:cs_axiom('d6c35852-4a88-43f6-af92-ff59488a0d69', foundational, zero_requires_operational_scaffolding).
narrative_ontology:cs_axiom_status(zero_requires_operational_scaffolding, holdable).
narrative_ontology:cs_axiom_grounding('d6c35852-4a88-43f6-af92-ff59488a0d69', zero_requires_operational_scaffolding, empirically_contingent).
narrative_ontology:cs_axiom('d6c35852-4a88-43f6-af92-ff59488a0d69', foundational, latent_structure_recognition_not_transmission).
narrative_ontology:cs_axiom_status(latent_structure_recognition_not_transmission, holdable).
narrative_ontology:cs_axiom_grounding('d6c35852-4a88-43f6-af92-ff59488a0d69', latent_structure_recognition_not_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('d6c35852-4a88-43f6-af92-ff59488a0d69', scaffolded_operational_thinkability).
narrative_ontology:cs_drift_state('d6c35852-4a88-43f6-af92-ff59488a0d69', contemporary_formalized_mathematics, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d6c35852-4a88-43f6-af92-ff59488a0d69', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, islamic_mathematical_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, european_algebraic_tradition).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_algebra_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developed positional notation and philosophical concepts of emptiness (sunya) that provided the scaffolding making zero-as-number operationally thinkable within an algebraic framework. This tradition generated the conceptual vocabulary that allowed zero to function as a calculable entity.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition, beneficiary,
    organized, generational, analytical, regional).

% Locked into a geometric-algebraic framework where numbers were lengths, areas, or ratios of geometric objects. Zero and negative quantities were conceptually inadmissible because there was no geometric correlate for 'nothing' as a quantity, and Aristotelian metaphysics rejected the void-as-something. This tradition bore the cost of being unable to operationalize zero-as-number despite its latent mathematical availability in any positional system.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_algebra_tradition, payer,
    organized, generational, identity_locked, regional).

% Received and synthesized Indian numerical and algebraic techniques, acting as the primary transmission vector that carried the scaffolding for zero-as-number westward. Benefited from the expanded computational and algebraic coordination that zero enabled, while adapting it into a new mathematical lexicon.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, islamic_mathematical_tradition, beneficiary,
    organized, generational, mobile, continental).

% Encountered Indian and Islamic mathematical texts during the medieval period. Contact triggered recognition of a latent structureâzero was already implicit in the positional notation they were adoptingârather than transmitting an entirely foreign concept. Once the scaffolding was available, zero became operationally thinkable and was integrated into European algebra and accounting.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, european_algebraic_tradition, beneficiary,
    organized, biographical, constrained, continental).

% Analyze whether zero-as-number represents universal logical necessity, contingent cultural invention, or latent structure recognized through compatible scaffolding. They observe the differential uptake across traditions as a function of conceptual framework compatibility, without being financially or institutionally vested in any single tradition's priority.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, modern_historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared conceptual vocabularyâpositional notation coupled with a metaphysics of emptinessâthat makes zero-as-number operationally thinkable, calculable, and communicable across mathematical traditions, solving the coordination problem of how to denote and manipulate null quantity in symbolic arithmetic.
% TRANSFER_FUNCTION: Moves the operational capacity to use zero as a number from traditions possessing compatible conceptual scaffolding (Hindu algebraic, Islamic mathematical, European algebraic) into a shared trans-cultural mathematical domain; structurally excludes traditions with incompatible scaffolding (Greek geometric algebra) from this operational capacity, not by active suppression but by paradigm-boundary mismatch.
% ABSENT_VOICES: Greek geometric algebraists and Aristotelian metaphysicians who rejected nothing-as-something are absent from the post-Indian consensus; they would argue for the primacy of geometric intuition but their framework could not articulate zero-as-number. Non-literate computational traditions that may have had pragmatic zero-analogues without textual scaffolding are also excluded from the historical record.
% DISAPPEARANCE_RATIONALE: If the conceptual scaffolding for zero-as-number vanished, arithmetic would lose its place-value foundation, algebraic symbolism would revert to pre-zero recursive forms, and the edifice of modern mathematics would reorganize around geometric or purely verbal descriptions of nullity. The shared conceptual vocabulary that makes zero thinkable is foundational to the world of mathematics.
% FOUNDING_PROBLEM: How to represent and manipulate null or empty quantities within a symbolic notation system without breaking the rules of arithmetic or yielding metaphysical paradox.
% FOUNDING_PROBLEM_CORROBORATION: The Greek geometric algebra tradition attests through its documented inability to operationalize zero. Modern historians of mathematics and philosophers of mathematics provide corroboration from outside the benefiting parties, citing independent analysis of Greek mathematical texts and comparative studies of Mesoamerican and Chinese zero-concepts that confirm the problem was historically live and scaffold-dependent.
narrative_ontology:disappearance_verdict(zero_as_number_entry__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_as_number_entry__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).
:- end_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the scaffolding requirement genuinely coordinates Hindu, Islamic, and European algebraic traditions around a powerful shared concept, but it simultaneously extracts from the Greek tradition by rendering zero-as-number structurally unthinkable within geometric algebra. Suppression is low (0.18) because there is no active enforcementâGreek mathematicians were not prevented from using zero; their own paradigm-bound identity-lock excluded them. Theater ratio is low (0.12) because the coordination is functional and substantive, not performative. Accessibility collapse is high (0.78): once a tradition accepts the scaffolding (positional notation plus emptiness metaphysics), zero becomes obviously a number and alternatives (pure geometric algebra without zero) collapse as viable frameworks for advanced arithmetic. Resistance is low (0.15) because there was no organized resistance to zero-as-number; the Greek tradition simply operated in a parallel conceptual space. The temporal series shows declining extraction as the scaffolding spread from regional Indian practice to continental Islamic and European mathematics, reducing the share of the mathematical world locked out of the coordination.
 *
 * PERSPECTIVAL GAP:
 *   The Hindu algebraic seat experiences this constraint as a Rope: shared scaffolding that enables powerful calculation and cross-generational coordination. The Greek geometric seat experiences the same structural arrangement as a boundary they cannot crossâa Mountain-like immunity from their perspective, or a Snare of their own identity-lock. The engine computes this divergence from identical structural data through directionality: Hindu tradition has analytical exit (can reflect on and modify its own scaffolding) and beneficiary role, pushing d toward the subsidy end; Greek tradition has identity_locked exit and payer role, pushing d toward full target. The perspectival gap is thus not a disagreement about facts but a computable consequence of divergent structural positions within the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu algebraic tradition is the primary structural beneficiary (low d): it generated the scaffolding and operates within it, collecting coordination surplus in the form of computational power and historical priority. Islamic and European algebraic traditions are secondary beneficiaries (low-to-moderate d): they receive the coordination surplus via transmission and recognition. Greek geometric algebra tradition is the structural target (high d): its identity-locked exit and incompatible framework place it at the extraction pole, not because any party collects from its exclusion, but because the scaffolding requirement itself costs it the operational capacity for zero. There are no concentrated agenda-setters enforcing the exclusion; the directionality is derived from beneficiary-victim structure plus exit modulation alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâhow to operationalize zero in symbolic arithmeticâwas solved by the Indian scaffolding tradition and remains solved in contemporary formal mathematics. The arrangement has not atrophied into a Piton because the scaffolding is still functionally necessary for pedagogy and practice; every new generation of mathematicians must acquire place-value and algebraic concepts to operationalize zero. There is no theatrical maintenance: the constraint does not persist through performative enforcement but through continued genuine coordination. Because the founding problem is dead for the historical originators but live for every subsequent learner, the status is contested rather than purely dead, preventing a false mandatrophy verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is zero-as-number a universally available logical structure that any tradition could have recognized given positional notation, or is it irreducibly dependent on specific cultural-philosophical scaffolding such as Indian emptiness doctrines?',
    'Comparative history of mathematics across independently developed traditions (Mesoamerican, Chinese, Indian, Greek) to determine whether zero-as-number emerged multiply or singularly, and whether its emergence tracks the availability of compatible metaphysical scaffolding.',
    'If universally available, the hybrid scaffolding reading overstates contingency and the constraint approaches Mountain status (logical necessity); if scaffolding-dependent, the Rope classification is validated with genuine coordination function and exclusionary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether zero-as-number is kernel-universal or scaffolding-contingent').

omega_variable(
    mathematical_necessity_vs_scaffolding,
    'Does the constraint represent a genuine natural law of mathematical reasoning (zero necessarily follows from positional notation plus arithmetic operations), or a constructed coordination mechanism built on contingent philosophical premises about emptiness and place-value?',
    'Formal analysis of whether zero-as-number is derivable from the Peano axioms without historical-cultural scaffolding, cross-referenced with historical evidence that its operational thinkability required Buddhist or Jain philosophical concepts of sunya.',
    'If natural law, the constraint is a Mountain despite declared beneficiaries; if constructed coordination, the Rope classification holds and the Greek tradition''s exclusion is a structural boundary rather than a failure to discover necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mathematical_necessity_vs_scaffolding, conceptual, 'Natural law versus constructed coordination ambiguity for zero').

omega_variable(
    greek_exclusion_mechanism,
    'Was the Greek geometric algebra tradition''s inability to operationalize zero-as-number a result of structural suppression by an incompatible paradigm, or internalized identity-lock within the geometric-Aristotelian worldview?',
    'Analysis of Greek mathematical texts for evidence of zero-like concepts that were actively rejected versus never conceived; examination of whether contact with Babylonian zero-analogues triggered any recognition or whether the geometric identity prevented even apprehension.',
    'If internalized identity-lock, the suppression metric understates true exclusion and directionality toward the Greek tradition is higher; if structural paradigm incompatibility, the exclusion was a passive coordination boundary inherent to the rope''s scaffolding requirement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(greek_exclusion_mechanism, empirical, 'Structural versus internalized exclusion of Greek tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__hybrid_scaffolding_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_hybrid_tr_t0, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(zero_hybrid_tr_t200, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement(zero_hybrid_tr_t400, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 400, 0.08).
narrative_ontology:measurement(zero_hybrid_tr_t600, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 600, 0.08).
narrative_ontology:measurement(zero_hybrid_tr_t900, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(zero_hybrid_tr_t1200, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1200, 0.12).

% Extraction over time
narrative_ontology:measurement(zero_hybrid_be_t0, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(zero_hybrid_be_t200, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 200, 0.48).
narrative_ontology:measurement(zero_hybrid_be_t400, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 400, 0.35).
narrative_ontology:measurement(zero_hybrid_be_t600, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 600, 0.28).
narrative_ontology:measurement(zero_hybrid_be_t900, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 900, 0.22).
narrative_ontology:measurement(zero_hybrid_be_t1200, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1200, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(zero_hybrid_su_t0, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(zero_hybrid_su_t200, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 200, 0.18).
narrative_ontology:measurement(zero_hybrid_su_t400, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 400, 0.14).
narrative_ontology:measurement(zero_hybrid_su_t600, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 600, 0.12).
narrative_ontology:measurement(zero_hybrid_su_t900, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 900, 0.1).
narrative_ontology:measurement(zero_hybrid_su_t1200, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1200, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry__contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry__universal_discovery_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'zero-as-number' conflates three structurally distinct claims about conceptualization, transmission, and ontological status. This story (hybrid_scaffolding_reading) isolates the claim about scaffolding-dependent recognition of latent structure; the contingent_thinkability_reading isolates the claim about transmission necessity; the universal_discovery_reading isolates the claim about logical necessity. Each carries a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
