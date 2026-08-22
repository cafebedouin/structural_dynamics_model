% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__universal_discovery_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__universal_discovery_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: zero_as_number_entry__universal_discovery_reading
 *   human_readable: Zero as a Mathematical Number — Universal Discovery Reading
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   Positional (place-value) numeral systems that support standard arithmetic
 *   operations require some device to mark an empty place and to provide an
 *   additive identity closing addition and subtraction. This reading holds
 *   that requirement is a logical entailment of the numeral system's
 *   structure, not a cultural artifact — meaning zero-as-number was
 *   mathematically available the moment positional notation with arithmetic
 *   closure existed as a formal possibility, independent of whether or when
 *   any human tradition articulated it. Indian mathematicians (Brahmagupta
 *   and predecessors) were first to formalize zero as an arithmetic operand
 *   with defined rules; European mathematics arrived later, whether by
 *   transmission through Islamic intermediaries or by independent derivation
 *   from similar notational pressures. Under this reading, the sequencing of
 *   discovery is a fact about historical timing of human
 *   cognitive/institutional access to a fixed mathematical truth, and carries
 *   no bearing on whether the truth is 'more Indian' or 'more European' — it
 *   belongs to no one and was discoverable by anyone whose numeral system
 *   reached the relevant structural threshold.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__universal_discovery_reading, 0.04).
domain_priors:suppression_score(zero_as_number_entry__universal_discovery_reading, 0.03).
domain_priors:theater_ratio(zero_as_number_entry__universal_discovery_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__universal_discovery_reading, mountain).
narrative_ontology:human_readable(zero_as_number_entry__universal_discovery_reading, "Zero as a Mathematical Number — Universal Discovery Reading").
narrative_ontology:topic_domain(zero_as_number_entry__universal_discovery_reading, "history_of_mathematics/philosophy_of_mathematics").

domain_priors:emerges_naturally(zero_as_number_entry__universal_discovery_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__universal_discovery_reading, '38c04ddf-12b1-4dbd-85a5-70896125627d').
narrative_ontology:cs_kernel_codification('38c04ddf-12b1-4dbd-85a5-70896125627d', distributed).
narrative_ontology:cs_authority_grounding('38c04ddf-12b1-4dbd-85a5-70896125627d', distributed).
narrative_ontology:cs_reading_relation('38c04ddf-12b1-4dbd-85a5-70896125627d', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('38c04ddf-12b1-4dbd-85a5-70896125627d', zero_as_number_entry__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('38c04ddf-12b1-4dbd-85a5-70896125627d', foundational, priority_of_holder_ontologically_inert).
narrative_ontology:cs_axiom_status(priority_of_holder_ontologically_inert, holdable).
narrative_ontology:cs_axiom_grounding('38c04ddf-12b1-4dbd-85a5-70896125627d', priority_of_holder_ontologically_inert, deontological).
narrative_ontology:cs_axiom('38c04ddf-12b1-4dbd-85a5-70896125627d', foundational, positional_notation_logically_entails_zero_operand).
narrative_ontology:cs_axiom_status(positional_notation_logically_entails_zero_operand, holdable).
narrative_ontology:cs_axiom_grounding('38c04ddf-12b1-4dbd-85a5-70896125627d', positional_notation_logically_entails_zero_operand, empirically_contingent).
narrative_ontology:cs_reference_frame('38c04ddf-12b1-4dbd-85a5-70896125627d', logical_necessity_of_arithmetic_closure).
narrative_ontology:cs_drift_state('38c04ddf-12b1-4dbd-85a5-70896125627d', post_colonial_historiography_reassessment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('38c04ddf-12b1-4dbd-85a5-70896125627d', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, global_mathematical_community).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, indian_mathematical_tradition_priority_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, european_mathematicians_post_transmission).
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, mathematical_platonism).
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, priority_independent_ontological_status_of_discovery).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historians and mathematicians who document Brahmagupta and predecessor formalization of zero as an arithmetic operand receive historical credit for temporal priority under this reading, but the reading holds that this priority is purely a fact about who found the fixed structure first, not a claim about who made zero true or valid.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, indian_mathematical_tradition_priority_claimants, beneficiary,
    analytical, civilizational, analytical, global).

% All practitioners of arithmetic, algebra, and downstream mathematics rely on zero as a number; under this reading they benefit equally and identically from the truth regardless of transmission path, since the constraint's validity does not depend on any particular culture's contact history.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, global_mathematical_community, beneficiary,
    analytical, civilizational, analytical, universal).

% Received or independently rediscovered zero-as-number later than Indian formalization; under this reading their later arrival reflects contingent historical timing of discovery, not a different or lesser ontological grasp of the same fixed mathematical fact.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, european_mathematicians_post_transmission, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__universal_discovery_reading, european_mathematicians_post_transmission, observer).

% Argue that without documented transmission contact, zero-as-number would not have emerged indigenously in a Greek/Aristotelian conceptual frame due to metaphysical barriers to treating 'nothing' as a quantity. This reading treats their transmission-contingency claim as orthogonal to the ontological-availability claim being made here, and does not adjudicate between them within this constraint.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, historians_of_science_contingency_school, excluded,
    organized, civilizational, analytical, global).

% Assess whether mathematical facts (like the coherence of zero under positional notation and arithmetic closure) are discovered rather than invented; this reading aligns with a discovery/platonist framing without settling the broader metaphysical debate.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, philosophers_of_mathematics_realists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the extractive-coordination sense — the constraint names a timeless logical entailment (positional notation plus closure under arithmetic operations requires an additive identity/placeholder), not an arrangement any party administers or could withhold.
% TRANSFER_FUNCTION: Nothing is transferred between parties; this reading holds that discovery moves credit and historical attribution (from later formalizers to earlier ones) but moves no rents, resources, or ontological stake — the mathematical fact itself is not anyone's to allocate.
% ABSENT_VOICES: The contingent_thinkability_reading and hybrid_scaffolding_reading camps are not voices absent from mathematics generally, but they are absent FROM THIS reading's framing by construction — this constraint does not adjudicate their claims, it states a distinct, coexisting position about ontological availability that leaves the transmission-history question to those other readings.
% DISAPPEARANCE_RATIONALE: If every human record of zero's discovery vanished overnight, the logical entailment (positional notation demands a placeholder/additive-identity concept) would remain true and would eventually be re-derived by any sufficiently developed positional numeral system; nothing about the world's mathematical structure depends on any culture's prior act of formalizing it. This is the structural signature of a mountain — the world does not rearrange because a mountain's discoverer or the credit for its discovery could vanish.
% FOUNDING_PROBLEM: The felt problem was representational and computational: positional numeral systems performing multi-digit arithmetic needed a way to mark an empty place value and to closes addition/subtraction under an identity element; the 'problem' of the missing quantity was a practical and conceptual bottleneck in arithmetic notation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of mathematics outside any single national or civilizational tradition — including comparative numeral-system scholars examining Babylonian placeholder practices, Mayan positional systems, and the Indian formalization independently — attest that the underlying arithmetic-closure problem zero solves is now universally and permanently resolved wherever positional notation is used; no active mathematical community treats the question of whether zero is a valid number as open.
narrative_ontology:disappearance_verdict(zero_as_number_entry__universal_discovery_reading, world_unchanged).
narrative_ontology:founding_problem_status(zero_as_number_entry__universal_discovery_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__universal_discovery_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_as_number_entry__universal_discovery_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__universal_discovery_reading, 0.04, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__universal_discovery_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, ExtMetricName, E),
    domain_priors:suppression_score(zero_as_number_entry__universal_discovery_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zero_as_number_entry__universal_discovery_reading),
    narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zero_as_number_entry__universal_discovery_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.03-0.04) because the constraint names a logical entailment: nobody administers zero-as-number, nobody can withhold it, and formalizing it confers no rents extractable from a captive population. Suppression is near-zero because no coercive apparatus enforces the concept's validity — its acceptance in any tradition follows demonstration and use, not compulsion. Accessibility collapse is authored high (0.88) because once a positional system with arithmetic closure is understood, the necessity of a zero-value operand becomes essentially inescapable — there is no coherent alternative arithmetic that avoids it while retaining the same computational power. Resistance is authored low (0.12) reflecting genuine but eventually-overcome historical resistance in some traditions (early European unease with treating 'nothing' as a quantity) that did not persist once the formal apparatus was understood — this is measured resistance to the CONCEPT, not resistance mounted by any party with a stake in blocking it. Theater ratio rises gently over the interval (0.05 to 0.10) reflecting increasing historiographic/pedagogical ceremony around 'discovery narratives' (national-priority framing in textbooks and popular histories) without any corresponding change in the underlying mathematical fact — a mild drift toward performative attribution debates layered on a stable mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the analytical/observer seat (philosophers of mathematics, comparative historians), the constraint reads cleanly as a mountain: a necessary structural entailment that would be re-derived by any sufficiently developed positional system. From the seat of civilizational-priority discourse (national narratives about 'who invented zero'), the same underlying fact gets entangled with prestige and credit allocation that can look extractive at the level of historical narrative-construction even though the mathematical fact underneath is not extractive at all — this is exactly the gap the FSM check is designed to surface, and it is why beneficiaries are declared here despite the low measured extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as an FSM-triggering move: the global mathematical community and Indian-tradition priority claimants are named as beneficiaries because all downstream mathematics benefits from a valid, formalized zero, and historical credit accrues to the tradition of first formalization. But per this reading, no beneficiary group EXTRACTS anything from any other group by virtue of the constraint being true — the benefit is symmetric and universal, unlike a constructed advantage. This is precisely the beneficiary-declared-on-a-mountain case the false_summit_mountain signature is built to test: the metrics (low ε, low suppression, high accessibility_collapse, emerges_naturally=true) are authored as genuinely mountain-consistent, and the omega variables document the irreducible question of whether crediting a specific civilizational tradition with priority nonetheless constructs an extractable epistemic-prestige asset distinct from the mathematical fact itself.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy in the classic sense (an arrangement outliving its function) because this reading holds there was never a constructed arrangement to outlive — the founding problem (representational/computational gap in positional arithmetic) is declared dead precisely because it was permanently and universally solved, not because an institution serving it decayed. The corroboration is explicitly sought outside the beneficiary set (comparative numeral-system historians examining Babylonian and Mayan positional systems independently) to avoid the self-serving-genealogy failure mode the R5 interview is designed to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    priority_credit_vs_ontological_status,
    'Does crediting Indian mathematicians with priority of formalization construct a separable epistemic-prestige asset that functions extractively in civilizational-narrative discourse, even though the underlying mathematical fact itself is non-extractive?',
    'Compare the material and reputational consequences of priority attribution in academic, curricular, and popular-history contexts against a counterfactual where attribution were withheld or reassigned; if consequences are purely honorific with no resource flow, the prestige asset is inert; if it drives funding, curricular authority, or civilizational-status claims, a distinct extractive layer exists above the mountain.',
    'If resolved toward a separable extractive layer, that layer would need its own constraint story (per the ε-invariance principle) rather than being folded into this mountain reading — this omega marks the boundary the FSM signature is designed to test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priority_credit_vs_ontological_status, conceptual, 'Whether historical-priority credit constructs an extractive asset distinct from the mathematical fact.').

omega_variable(
    logical_availability_vs_conceptual_barrier,
    'Is ''mathematical availability'' (this reading''s central claim) actually independent of the conceptual/metaphysical scaffolding required to recognize it, or does availability presuppose a scaffolding condition that the contingent_thinkability_reading and hybrid_scaffolding_reading treat as doing real explanatory work?',
    'Examine whether any positional-notation tradition without documented contact with Indian/Islamic mathematics (e.g., Mesoamerican systems) independently formalized zero as a full arithmetic operand under the same closure rules; convergent independent formalization would support this reading''s inevitability claim, while a documented uniform failure pattern outside the transmission chain would support the sibling readings.',
    'If independent convergence is well-attested, this reading''s classification as mountain gains stronger empirical grounding; if convergence is absent outside the transmission chain, the ε assigned to ''transmission contingency'' in the sibling readings should be weighted more heavily than this reading allows, without this story itself needing to change its own ε (per the ε-invariance principle — that adjustment belongs to the sibling files).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(logical_availability_vs_conceptual_barrier, empirical, 'Whether independent convergent formalization evidence supports pure logical availability over required conceptual scaffolding.').

omega_variable(
    discovery_vs_invention_metaphysics,
    'Does treating zero-as-number as ''discovered'' rather than ''invented'' presuppose a mathematical-platonist metaphysics that is itself a contested philosophical position rather than a settled fact?',
    'This is not resolvable by further empirical historical evidence; it depends on which philosophy of mathematics (platonism, formalism, structuralism, fictionalism) one antecedently holds, and different holders will accept or reject this reading''s framing on those independent grounds.',
    'If a non-platonist metaphysics is adopted, the entire ''universal discovery'' framing (and hence this reading''s mountain classification) becomes a claim about formal-system entailment rather than about mind-independent mathematical facts — the classification as mountain would likely survive under formalism too (necessity relative to the axioms/notation chosen), but the language of ''discovery'' versus ''derivation'' would need revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discovery_vs_invention_metaphysics, preference, 'Whether the discovery-framing depends on an antecedently contested metaphysics of mathematics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__universal_discovery_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_as_number_entry__universal_discovery_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(zero_tr_t0, observed).
narrative_ontology:measurement(zero_tr_t300, zero_as_number_entry__universal_discovery_reading, theater_ratio, 300, 0.06).
narrative_ontology:measurement_basis(zero_tr_t300, observed).
narrative_ontology:measurement(zero_tr_t600, zero_as_number_entry__universal_discovery_reading, theater_ratio, 600, 0.07).
narrative_ontology:measurement_basis(zero_tr_t600, observed).
narrative_ontology:measurement(zero_tr_t900, zero_as_number_entry__universal_discovery_reading, theater_ratio, 900, 0.08).
narrative_ontology:measurement_basis(zero_tr_t900, observed).
narrative_ontology:measurement(zero_tr_t1200, zero_as_number_entry__universal_discovery_reading, theater_ratio, 1200, 0.09).
narrative_ontology:measurement_basis(zero_tr_t1200, observed).
narrative_ontology:measurement(zero_tr_t1500, zero_as_number_entry__universal_discovery_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement_basis(zero_tr_t1500, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(zero_be_t0, observed).
narrative_ontology:measurement(zero_be_t300, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 300, 0.03).
narrative_ontology:measurement_basis(zero_be_t300, observed).
narrative_ontology:measurement(zero_be_t600, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 600, 0.04).
narrative_ontology:measurement_basis(zero_be_t600, observed).
narrative_ontology:measurement(zero_be_t900, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 900, 0.04).
narrative_ontology:measurement_basis(zero_be_t900, observed).
narrative_ontology:measurement(zero_be_t1200, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 1200, 0.04).
narrative_ontology:measurement_basis(zero_be_t1200, observed).
narrative_ontology:measurement(zero_be_t1500, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 1500, 0.04).
narrative_ontology:measurement_basis(zero_be_t1500, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(zero_as_number_entry__universal_discovery_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__universal_discovery_reading, information_standard).
narrative_ontology:boltzmann_floor_override(zero_as_number_entry__universal_discovery_reading, 0.02).
narrative_ontology:affects_constraint(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry__contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the zero_as_number_entry kernel, decomposed per the ε-invariance principle because the three readings assign structurally different ε and different classifications to what a single natural-language label ('the discovery of zero') would otherwise flatten into one measurement. universal_discovery_reading (this file) authors low ε and a mountain classification on the premise that zero-as-number is a logical entailment of positional notation independent of any transmission history. contingent_thinkability_reading authors a different ε profile reflecting a claim that European indigenous emergence was blocked absent transmission contact — a genealogically contingent claim, not a pure logical-necessity claim. hybrid_scaffolding_reading sits between: mathematically latent but requiring specific conceptual scaffolding to become operationally thinkable, with contact triggering recognition rather than transmitting a foreign concept. All three share the same underlying historical episode but diverge on where necessity ends and contingency begins, which is exactly the condition under which the framework requires decomposition into linked files rather than one hedged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
