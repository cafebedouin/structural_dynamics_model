% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__placeholder_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_mathematical_status__placeholder_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: zero_mathematical_status__placeholder_reading
 *   human_readable: Zero as Notational Device (Positional-System Reading)
 *   domain: history_of_mathematics/epistemology
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'zero_mathematical_status': zero is a notational device for positional
 *   systems, not a number with arithmetic properties. Under this reading,
 *   zero functions as a placeholder symbol—vital for representing
 *   absence-of-value in positional notation (base-10, base-60, etc.), but
 *   ontologically distinct from numbers proper. The reading gains enormous
 *   notational and computational efficiency while paying the cost of
 *   maintaining a bifurcated mathematical ontology (zero exists in notation
 *   but lacks full arithmetic identity). This is one of three competing
 *   readings: the parmenidean_rejection denies zero any mathematical status;
 *   the number_reading grants zero full arithmetic properties (Brahmagupta's
 *   synthesis). The placeholder reading sits between, claiming a stable
 *   middle ground that in practice tends to erode.
 *
 * KEY AGENTS:
 *   - positional_notation_practitioners (beneficiary, organized): astronomers, merchants, engineers adopting zero-as-placeholder for notational efficiency
 *   - arithmetic_closure_preservationists (payer, powerful): mathematicians defending the integrity of complete number systems against bifurcation
 *   - ontological_purism_schools (payer, moderate, identity_locked): philosophical traditions inheriting Parmenidean principle, maintaining that non-being cannot be a mathematical entity
 *   - brahmaguptean_synthesis_school (excluded, organized): the number-reading tradition that would replace this reading entirely
 *   - logical_coherence_school (observer, analytical): philosophers and logicians observing the structural tension
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__placeholder_reading, 0.52).
domain_priors:suppression_score(zero_mathematical_status__placeholder_reading, 0.38).
domain_priors:theater_ratio(zero_mathematical_status__placeholder_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__placeholder_reading, rope).
narrative_ontology:human_readable(zero_mathematical_status__placeholder_reading, "Zero as Notational Device (Positional-System Reading)").
narrative_ontology:topic_domain(zero_mathematical_status__placeholder_reading, "history_of_mathematics/epistemology").

domain_priors:requires_active_enforcement(zero_mathematical_status__placeholder_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__placeholder_reading, '3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70').
narrative_ontology:cs_kernel_codification('3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70', distributed).
narrative_ontology:cs_authority_grounding('3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70', practice).
narrative_ontology:cs_interpretation_layer_present('3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70').
narrative_ontology:cs_reading_relation('3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70', zero_mathematical_status__number_reading, influences).
narrative_ontology:cs_reading_relation('3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70', zero_mathematical_status__parmenidean_rejection, coexists_with).
narrative_ontology:cs_axiom('3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70', foundational, zero_notation_ontology_split).
narrative_ontology:cs_axiom_status(zero_notation_ontology_split, holdable).
narrative_ontology:cs_axiom_grounding('3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70', zero_notation_ontology_split, deontological).
narrative_ontology:cs_axiom('3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70', secondary, parmenidean_principle_preservation).
narrative_ontology:cs_axiom_status(parmenidean_principle_preservation, holdable).
narrative_ontology:cs_axiom_grounding('3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70', parmenidean_principle_preservation, deontological).
narrative_ontology:cs_reference_frame('3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70', platonic_separation_of_notation_and_being).
narrative_ontology:cs_drift_state('3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70', contemporary_computational_mathematics, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3b7ef8b3-5f01-4bcc-91bb-a4f0a0282f70', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__placeholder_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, positional_notation_practitioners).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, computational_efficiency_seekers).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, arithmetic_closure_preservationists).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, ontological_purism_schools).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mathematicians, astronomers, and computational practitioners who adopt zero as a placeholder in base-10 and other positional systems. They gain immense notational efficiency—representing large numbers compactly, performing long multiplication and division with algorithmic clarity. They do not require zero to have independent arithmetic identity; the symbol's power lies in position marking.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, positional_notation_practitioners, beneficiary,
    organized, generational, mobile, global).

% Mathematicians committed to rigorous arithmetic closure—the idea that every number field must permit addition, subtraction, multiplication, and division (where defined) without creating anomalies. They bear the cost of this reading: they must either reject zero as a number (paying the cost of incomplete arithmetic), or accept zero but deny it the status of a genuine entity, creating a bifurcated mathematical ontology.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, arithmetic_closure_preservationists, payer,
    powerful, generational, constrained, global).

% Philosophical traditions (especially those inheriting Parmenidean principle: being cannot arise from non-being) for whom zero-as-nothing is metaphysically incoherent. They bear the cost of this reading by maintaining a rigid distinction between notational convenience and ontological commitment, which requires continual enforcement against the intuitive reading of zero as a number.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, ontological_purism_schools, payer,
    moderate, biographical, identity_locked, regional).

% The mathematical tradition (from Brahmagupta and Islamic algebra onward) that reads zero as a full number with arithmetic properties (0+a=a, a−a=0, etc.). They are structurally excluded from this reading's framework—their position would directly replace the placeholder reading with the number reading, a different constraint altogether.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, brahmaguptean_synthesis_school, excluded,
    organized, generational, trapped, global).

% Engineers, astronomers, merchants, and algorithm designers who adopt positional notation (especially in commercial and astronomical contexts, medieval Islamic mathematics, pre-Columbian Mesoamerica). They benefit from the enormous notational power zero-as-placeholder grants: astronomers compute planetary ephemerides; merchants track inventory; engineers solve surveying problems—all with dramatic reduction in computational labor.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, computational_efficiency_seekers, beneficiary,
    powerful, biographical, arbitrage, global).

% Mathematical logicians and philosophers of mathematics studying the coherence and consistency of number systems. They observe the tension between the placeholder reading and the number reading, noting that the reading's stability depends on maintaining a sharp distinction between notational role and ontological status—a distinction that tends to blur in practice.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, logical_coherence_school, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__placeholder_reading, positional_notation_practitioners).
narrative_ontology:fixing_cost_class(zero_mathematical_status__placeholder_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of representing large numbers in base-positional systems without an explicit symbol for absence of a digit value. Zero-as-placeholder enables compact notation for 1000 (versus 1,000 or M), which in turn enables algorithmic long multiplication and division to be codified as teachable procedures rather than craft knowledge.
% TRANSFER_FUNCTION: Moves notational efficiency and computational labor reduction to practitioners who adopt the system, while extracting from traditions committed to arithmetic closure or ontological purity the cost of maintaining a bifurcated ontology (zero is real in notation but not in properties, or is a useful fiction that must not be mistaken for being).
% ABSENT_VOICES: Brahmagupta's synthesis school (which reads zero as a full number) is excluded from this reading's framework. Their representatives would argue that the placeholder reading is unstable—that zero cannot hold position without acquiring arithmetic identity. Non-literate computational traditions (abacus users, tally-stick practitioners) have no voice in the codified reading, though their absence of zero in those systems suggests the reading is not inevitable.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared and practitioners reverted to zero-as-nothing-only, positional notation would still function (some historical systems operated without explicit zero symbols, using space or column markers), but the efficiency gains would compress: larger numbers would require more symbols or more cumbersome notation. Practitioners would either re-invent zero-as-placeholder or migrate to competing notational systems (Roman numerals, tally systems, verbal naming). The mathematics of the Western medieval and early-modern periods would have followed a slower algorithmic development.
% FOUNDING_PROBLEM: How do you represent place value in positional number systems when a digit position is empty (no units, no tens, etc.)? Sanskrit and Babylonian mathematicians recognized that a notation for absence was necessary for the system to work without ambiguity.
% FOUNDING_PROBLEM_CORROBORATION: Modern computational and mathematical practice corroborates that positional notation without an explicit placeholder for zero creates ambiguity (e.g., 101 vs. 11 without zero distinction becomes unclear). Historical evidence from Babylonian cuneiform, Indian mathematics, and Islamic algebra shows that practitioners solved this problem by introducing a placeholder symbol. This corroboration comes from outside the reading's own beneficiary class—it is a matter of historical and computational fact, not a claim made by those who profit from the reading.
narrative_ontology:disappearance_verdict(zero_mathematical_status__placeholder_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__placeholder_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__placeholder_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_mathematical_status__placeholder_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__placeholder_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__placeholder_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_mathematical_status__placeholder_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zero_mathematical_status__placeholder_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is intermediate (0.52 at interval end, rising from 0.38) because the reading gains real computational power for practitioners while imposing conceptual costs on purists and closure-preservationists who must defend an awkward boundary. The reading is not pure extraction (victims benefit from positional notation too), nor pure coordination (the beneficiary set profits more directly than the payer set). Suppression is moderate (0.38 at interval end, rising from 0.28) because enforcement of the boundary between notational and ontological status requires continuous rhetorical and pedagogical effort—instructors must teach students that zero is useful but not a 'real' number, which goes against intuition and practice. Theater ratio rises modestly to 0.42 because much enforcement activity consists of maintaining this distinction rather than deriving new notational advantages. The measurement series track extraction accumulation (the reading's conceptual costs compound as the efficiency gains embed more deeply) and suppression intensification (as practitioners naturally treat zero as a number, enforcement must work harder). Accessibility collapse is moderate (0.61)—practitioners can understand the alternative (number_reading) and historical alternatives (zero-less positional systems) existed, but once the placeholder efficiency is adopted, reverting to alternatives becomes costly. Resistance is substantial (0.58)—the Parmenidean tradition and arithmetic-closure schools actively resist, producing philosophical arguments and alternative formalisms.
 *
 * PERSPECTIVAL GAP:
 *   Practitioners versus theorists: practitioners naturally use zero as a number and experience the placeholder reading as a useful fiction that does not constrain their arithmetic. Theorists (especially those committed to ontological purity or closure) experience it as a burden—a bifurcation that makes their formal systems awkward and their pedagogy harder to justify.
 *
 * DIRECTIONALITY LOGIC:
 *   Positional_notation_practitioners are beneficiaries (d toward 0.0) because they gain efficiency without bearing the cost of maintaining the ontological boundary. Arithmetic_closure_preservationists are targets (d toward 1.0) because they must defend against the intuitive reading of zero as a number, a defense that becomes more difficult as the notation embeds. Ontological_purism_schools are also targets (high d) because their philosophical framework is directly challenged by the reading's acceptance of zero in notation—they must constantly re-explain why notation does not imply ontology. Brahmaguptean_synthesis_school would occupy a different constraint (the number_reading) where they would be beneficiaries, so here they are excluded, not seated. Computational_efficiency_seekers sit near the beneficiary end (d low) because they profit from the notation's power. Logical_coherence_school sits at analytical distance (d = 0.5) because they observe the tension without being extracted from or benefiting directly. The reading's asymmetry is structural: those who profit from it do so without defending its theoretical coherence, while those who defend it (closure and purity schools) bear the defensive cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: positional notation still requires a placeholder for absent values, and the placeholder still functions efficiently. However, the reading's founding justification ('zero is just notation, not arithmetic') has been eroded by 1500+ years of mathematical development showing zero behaves fully as a number (Brahmagupta's rules persist unchanged; every arithmetic system that includes zero treats it arithmetically). The reading persists not because it solves the founding problem better than alternatives, but because it preserves a philosophical tradition (Parmenidean principle, arithmetic closure). This is a mandatrophy case: the original function (denying non-being) has been functionally dead for centuries, but the reading persists through enforcement (suppression of the more natural number_reading in philosophical discourse). The constraint is classified as rope (coordination function real) but operates increasingly as tangled_rope (extraction from purity schools to maintain the boundary, with asymmetric beneficiary/payer structure). The measurement series show suppression rising, theater ratio rising, and extractiveness stabilizing—signs of a constraint whose original function has atrophied and whose persistence increasingly depends on performance rather than utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notational_arithmetic_boundary_stability,
    'Can a stable boundary be maintained between zero''s notational role and its arithmetic role, or does the intuitive and practical identification of the two roles make the boundary inherently unstable?',
    'Historical analysis of mathematical pedagogy and practice: if practitioners continually treat zero as arithmetic despite teaching the placeholder distinction, the boundary is functionally unstable. Formalist reconstruction: if a complete formalization of positional notation can be built without granting zero arithmetic identity, the boundary is theoretically maintainable.',
    'If the boundary is unstable, the placeholder reading is a temporary holding pattern—practitioners will eventually adopt the number_reading as their true belief, and the placeholder reading collapses into mandatrophy. If the boundary is stable, the reading is a genuine alternative framing with long-term viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notational_arithmetic_boundary_stability, conceptual, 'Whether the notational/arithmetic distinction for zero can remain stable or inevitably erodes in practice.').

omega_variable(
    parmenidean_principle_revisability,
    'Is the Parmenidean principle (''nothing cannot be'')—which motivates the ontological_purism_schools to reject zero—itself revisable within modern mathematics, or is it an unchangeable foundation of Western metaphysical tradition?',
    'Philosophical genealogy: trace whether Parmenidean principle has been explicitly overturned or modified in post-medieval philosophy and mathematics. Formal analysis: determine whether modern axiom systems (ZFC, Peano, category theory) require or presuppose Parmenidean principle.',
    'If the principle is revisable, the reading''s justification can be reformed or abandoned without loss of mathematical coherence. If the principle is foundational and unchangeable, the reading''s opposition to zero-as-being remains a live philosophical constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(parmenidean_principle_revisability, conceptual, 'Whether Parmenidean metaphysics constrains or enables the placeholder reading.').

omega_variable(
    efficiency_gain_quantification,
    'What is the quantifiable efficiency gain of zero-as-placeholder notation compared to alternative positional systems (Roman numerals, sexagesimal without explicit zero, verbal naming)? Does the gain justify the imposed ontological cost?',
    'Computational history: measure the reduction in execution time and cognitive load for long multiplication, division, and astronomical calculation across notational systems. Cost-benefit analysis: weigh efficiency gain against theoretical/pedagogical burden of maintaining the notational/arithmetic distinction.',
    'Large efficiency gain + manageable theoretical cost = rope (genuine coordination). Small efficiency gain + high theoretical cost = tangled_rope or snare (extraction from purity schools to support practitioners'' preference). If alternatives prove equally efficient, the reading is pure mandatrophy (persistence without function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficiency_gain_quantification, empirical, 'Quantifying the practical benefits of the placeholder reading against its conceptual costs.').

omega_variable(
    kernel_reading_decomposition,
    'Is this constraint truly one reading of a single kernel (''zero_mathematical_status''), or are there actually two distinct kernels in play: (1) what zero represents ontologically, and (2) what notational systems require for efficiency?',
    'Structural analysis: determine whether a party could hold both the placeholder_reading and the number_reading by separating ontological commitment from notational necessity. If separable, the kernels are distinct.',
    'If two kernels, the constraint family should decompose further, and the reading relations should be revised (some presently-coexisting readings might foreclose each other if analyzed on separate kernels). If one kernel, the current framing is correct and the three readings genuinely compete for the same logical space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether the kernel itself is correctly framed or requires further decomposition per ε-invariance principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__placeholder_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_mathematical_status__placeholder_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(zero_tr_t5, zero_mathematical_status__placeholder_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(zero_tr_t10, zero_mathematical_status__placeholder_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(zero_tr_t15, zero_mathematical_status__placeholder_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(zero_tr_t20, zero_mathematical_status__placeholder_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(zero_tr_t25, zero_mathematical_status__placeholder_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(zero_tr_t30, zero_mathematical_status__placeholder_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_mathematical_status__placeholder_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(zero_be_t5, zero_mathematical_status__placeholder_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(zero_be_t10, zero_mathematical_status__placeholder_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(zero_be_t15, zero_mathematical_status__placeholder_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement(zero_be_t20, zero_mathematical_status__placeholder_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(zero_be_t25, zero_mathematical_status__placeholder_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(zero_be_t30, zero_mathematical_status__placeholder_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_mathematical_status__placeholder_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(zero_su_t5, zero_mathematical_status__placeholder_reading, suppression_requirement, 5, 0.31).
narrative_ontology:measurement(zero_su_t10, zero_mathematical_status__placeholder_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(zero_su_t15, zero_mathematical_status__placeholder_reading, suppression_requirement, 15, 0.36).
narrative_ontology:measurement(zero_su_t20, zero_mathematical_status__placeholder_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(zero_su_t25, zero_mathematical_status__placeholder_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(zero_su_t30, zero_mathematical_status__placeholder_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__placeholder_reading, information_standard).
narrative_ontology:boltzmann_floor_override(zero_mathematical_status__placeholder_reading, 0.08).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__parmenidean_rejection).

% DUAL FORMULATION NOTE:
% The zero_mathematical_status kernel decomposes into three constraint stories (the three readings). Each reading instantiates a different constraint with different beneficiary/victim structures and different ε values. The placeholder_reading (this file) claims intermediate extraction (0.52) by trading efficiency gains for ontological awkwardness. The number_reading (sibling) claims low extraction (genuine arithmetic closure, no payers). The parmenidean_rejection (sibling) claims high suppression (active defense against zero-as-entity). All three readings are linked via affects_constraints; they do not represent different observables of a single constraint (ε-invariance: if measurement basis changes the type substantially, the constraints are distinct, and they are).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zero_mathematical_status__placeholder_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
