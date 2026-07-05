% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Zero-as-Number Entry — Hybrid Scaffolding Reading
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This story models zero-as-number's entry into operational mathematics as
 *   a coordination problem (Rope) with real winners and losers,
 *   distinguishing it sharply from a pure natural-law framing (which would
 *   erase the scaffolding cost entirely) and from a pure-transmission framing
 *   (which would erase the independent recognition-readiness of compatible
 *   traditions). The mathematical opportunity — a latent structural feature
 *   of positional notation permitting an empty-place marker to generalize
 *   into a full arithmetic operand — existed independent of any culture's
 *   philosophical commitments. But converting that latent opportunity into
 *   operational practice required a conceptual scaffolding (a tradition's
 *   prior comfort with treating nothingness/void as a coherent object) that
 *   some traditions possessed early (Hindu philosophical and grammatical
 *   traditions, via sunya) and others structurally lacked (Greek
 *   arithmos-as-plurality-of-units, later inherited by Aristotelian
 *   scholasticism). Contact between traditions (Indian to Islamicate to
 *   European) did not install a foreign concept wholesale; it triggered
 *   recognition of a structure that the receiving tradition's own notational
 *   practice was already gesturing toward, at a cost proportional to how
 *   incompatible the receiving tradition's existing scaffolding was.
 *
 * KEY AGENTS:
 *   - hindu_algebraic_tradition: early beneficiary — compatible scaffolding, low conversion cost
 *   - islamicate_mathematical_synthesis: transmission and refinement node — organized, mobile
 *   - later_european_algebraists: delayed beneficiary — moderate power, constrained exit due to entrenched local practice
 *   - greek_geometric_algebra_tradition: structural victim — trapped by an incompatible foundational category, retroactively
 *   - scholastic_aristotelian_number_theorists: institutional victim — bore reconciliation cost between operational tool and inherited ontology
 *   - modern_historians_of_mathematics: analytical observer adjudicating between rival transmission narratives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__hybrid_scaffolding_reading, 0.32).
domain_priors:suppression_score(zero_as_number_entry__hybrid_scaffolding_reading, 0.28).
domain_priors:theater_ratio(zero_as_number_entry__hybrid_scaffolding_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__hybrid_scaffolding_reading, rope).
narrative_ontology:human_readable(zero_as_number_entry__hybrid_scaffolding_reading, "Zero-as-Number Entry — Hybrid Scaffolding Reading").
narrative_ontology:topic_domain(zero_as_number_entry__hybrid_scaffolding_reading, "history_of_mathematics/philosophy_of_mathematics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__hybrid_scaffolding_reading, 'dcc4b160-c622-4566-a7d9-93a3d2d4890c').
narrative_ontology:cs_kernel_codification('dcc4b160-c622-4566-a7d9-93a3d2d4890c', distributed).
narrative_ontology:cs_authority_grounding('dcc4b160-c622-4566-a7d9-93a3d2d4890c', distributed).
narrative_ontology:cs_reading_relation('dcc4b160-c622-4566-a7d9-93a3d2d4890c', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('dcc4b160-c622-4566-a7d9-93a3d2d4890c', zero_as_number_entry__universal_discovery_reading, influences).
narrative_ontology:cs_axiom('dcc4b160-c622-4566-a7d9-93a3d2d4890c', foundational, scaffolding_is_necessary_but_not_transmitted).
narrative_ontology:cs_axiom_status(scaffolding_is_necessary_but_not_transmitted, holdable).
narrative_ontology:cs_axiom_grounding('dcc4b160-c622-4566-a7d9-93a3d2d4890c', scaffolding_is_necessary_but_not_transmitted, empirically_contingent).
narrative_ontology:cs_axiom('dcc4b160-c622-4566-a7d9-93a3d2d4890c', secondary, mathematical_latency_is_tradition_independent).
narrative_ontology:cs_axiom_status(mathematical_latency_is_tradition_independent, holdable).
narrative_ontology:cs_axiom_grounding('dcc4b160-c622-4566-a7d9-93a3d2d4890c', mathematical_latency_is_tradition_independent, empirically_contingent).
narrative_ontology:cs_reference_frame('dcc4b160-c622-4566-a7d9-93a3d2d4890c', positional_notation_latency_baseline).
narrative_ontology:cs_drift_state('dcc4b160-c622-4566-a7d9-93a3d2d4890c', post_comparative_historiography_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dcc4b160-c622-4566-a7d9-93a3d2d4890c', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, islamicate_mathematical_synthesis).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, later_european_algebraists).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_algebra_tradition).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, scholastic_aristotelian_number_theorists).
narrative_ontology:constraint_vindicates(zero_as_number_entry__hybrid_scaffolding_reading, conceptual_scaffolding_thesis).
narrative_ontology:constraint_vindicates(zero_as_number_entry__hybrid_scaffolding_reading, latency_recognition_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Working within a philosophical framework already comfortable with sunya (void/emptiness) as an ontologically legitimate object via Buddhist and grammatical traditions (Panini's zero-morpheme), mathematicians such as Brahmagupta could treat zero as an operand subject to arithmetic rules (addition, subtraction, and eventually attempts at division) centuries before comparable European formalization. Their scaffolding was already compatible with the positional-notation structure latent in place-value counting, so the conceptual leap to 'zero as number' rather than merely 'zero as placeholder' cost them comparatively little.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition, beneficiary,
    moderate, civilizational, arbitrage, continental).

% Scholars in Baghdad and across the Islamicate world encountered Indian numeral and zero concepts through translation and trade contact, and were positioned to recognize the latent structure because their own algebraic tradition (al-jabr) was not tied to Greek geometric proportion theory in the same restrictive way. They functioned as the transmission and adaptation node, refining zero's operational role and passing the compatible scaffolding onward.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, islamicate_mathematical_synthesis, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__hybrid_scaffolding_reading, islamicate_mathematical_synthesis, agenda_setter).

% From roughly the twelfth century onward, figures such as Fibonacci encountered the Hindu-Arabic numeral system including zero through Mediterranean trade and translation. They benefited from centuries of prior scaffolding work but still had to displace an entrenched geometric-proportional habit of mind; the concept was not simply installed but had to be re-derived and reconciled with existing local mathematical practice, which is why uptake was gradual and contested for centuries.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, later_european_algebraists, beneficiary,
    moderate, generational, constrained, continental).

% Greek mathematics treated number (arithmos) as fundamentally a count of discrete units and treated magnitude via geometric ratio; zero as a quantity was not merely unfamiliar but structurally unformulable within a framework where 'nothing' could not coherently be a member of the class of things counted. This tradition bore the cost of incompatibility: its scaffolding could not absorb the positional-notation latency without wholesale reconstruction, and by the time contact occurred the tradition itself had no living practitioners positioned to renegotiate its own foundations — the cost was paid retroactively, in centuries of mathematics unavailable to a framework that could have used it.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_algebra_tradition, payer,
    moderate, civilizational, trapped, regional).

% Medieval European scholastics inheriting Aristotelian categories faced genuine metaphysical friction: treating zero as a number challenged the definition of number as plurality of units and raised uncomfortable questions about void and non-being that had theological resonance. They paid the cost of reconciling an imported operational tool with an incompatible inherited ontology, producing centuries of hedged, partial, or resistant treatments of zero even after its arithmetic utility was demonstrated.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, scholastic_aristotelian_number_theorists, payer,
    institutional, generational, constrained, regional).

% Reconstruct the transmission and independent-scaffolding evidence from textual, epigraphic, and comparative sources, and adjudicate between rival narratives of how zero-as-number entered operational mathematics. Their analysis is what allows the hybrid reading to be distinguished from pure-transmission and pure-universal-discovery accounts.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, modern_historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared conceptual vocabulary for treating zero as an operand (not merely a placeholder digit) solves a genuine coordination problem: without it, arithmetic operations involving zero, negative numbers, and eventually algebraic equation-solving cannot be performed consistently across a mathematical community. The positional notation system created the mathematical opportunity; the philosophical scaffolding (a tradition already possessing a coherent concept of void/nothing as a legitimate object of thought) supplied the conceptual permission to use that opportunity.
% TRANSFER_FUNCTION: What moves is not zero itself (it was not a resource extracted from one party and given to another) but recognition-readiness: traditions with compatible prior scaffolding (Hindu philosophical treatment of sunya, later Islamicate algebra) converted the latent mathematical structure into operational practice quickly and cheaply; traditions with incompatible scaffolding (Greek geometric number theory, scholastic Aristotelian categories) had to pay a much higher conceptual-reconstruction cost, delaying their access to the same latent structure by centuries.
% ABSENT_VOICES: The Maya and Babylonian traditions, which independently developed positional or placeholder zero for calendrical or notational purposes without generalizing it to a fully operational arithmetic number, are absent from this comparison; their partial cases would complicate a clean binary of 'compatible' versus 'incompatible' scaffolding and deserve their own story rather than being folded in here.
% DISAPPEARANCE_RATIONALE: If the specific historical transmission pathway (Indian to Islamicate to European) had not occurred, proponents of this reading hold that zero-as-number would still have emerged wherever compatible philosophical scaffolding existed, via recognition of the latent positional-notation structure rather than reinvention from nothing — so the concept's eventual global uptake is not contingent on this particular contact event, though its TIMING and cultural pathway would differ substantially. Sibling readings dispute this: the contingent_thinkability_reading holds the concept would not have emerged in Europe at all without transmission.
% FOUNDING_PROBLEM: Positional (place-value) numeral notation creates an internal notational demand for a symbol marking an empty place, and once such a symbol exists, arithmetic operations naturally generalize to treat that symbol as an operand — the founding problem is closing the gap between a notational convenience (placeholder zero) and a fully general arithmetic object (number zero), a gap that is conceptual/philosophical rather than computational.
% FOUNDING_PROBLEM_CORROBORATION: Historians of mathematics working outside any tradition with a stake in priority claims (comparative historians examining Babylonian, Maya, Chinese, Indian, and Islamicate numeral systems side by side) corroborate that the placeholder-to-operand gap has been closed in every mathematical tradition that adopted positional notation combined with an operationally permissive concept of nothingness; the problem that motivated the original scaffolding work no longer exists as an open problem in any live mathematical practice today. No party with an active interest in reviving the ontological objection (i.e., no living Aristotelian number-theory tradition) remains to contest this from outside.
narrative_ontology:disappearance_verdict(zero_as_number_entry__hybrid_scaffolding_reading, contested).
narrative_ontology:founding_problem_status(zero_as_number_entry__hybrid_scaffolding_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__hybrid_scaffolding_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_as_number_entry__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 0.32, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.32 at end) rather than low or high because this reading holds BOTH that mathematical necessity partially drove convergence (limiting how much any single tradition could be said to 'lose' relative to an inevitable outcome) AND that scaffolding incompatibility imposed a genuine, unevenly distributed cost (justifying non-trivial extraction from the victim traditions, who effectively paid centuries of unavailable mathematics for the sin of institutional/philosophical incompatibility). Suppression is comparatively low (0.28) because no party actively prevented Greek or scholastic traditions from adopting zero — the barrier was internal-conceptual, not externally enforced, which is a materially different mechanism from coercive suppression and is reflected in the lower score. Accessibility collapse (0.55) is moderate: once the scaffolding existed, alternatives to adopting zero-as-number collapsed substantially for any tradition that wanted operational algebra, but did not collapse completely — a tradition could and did persist for centuries without it, at a cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Hindu algebraic tradition, Islamicate synthesis, later European algebraists) get low-to-moderate directionality because the scaffolding they possessed or acquired let them capture the mathematical opportunity cheaply — they are net receivers of a coordination benefit, not payers. Victims (Greek geometric algebra tradition, scholastic Aristotelian number theorists) get high directionality because their trapped exit options (a live tradition cannot simply discard its own foundational categories mid-stream) combined with the retroactive nature of the cost (paid in centuries of foreclosed mathematics) mean they experienced the constraint as extraction even though no single agent enforced it against them.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy by refusing to treat the scaffolding requirement as either a dead formality (it was a real and costly conceptual barrier, not mere convention) or a permanently live problem (the founding problem — closing the placeholder/operand gap — is dead: every tradition that wanted operational zero has had it for centuries, and no live mathematical practice today faces the original barrier). The classification as Rope rather than Mountain or Snare reflects that the coordination function was genuine (a shared vocabulary was needed and, once achieved, benefited all traditions that adopted it) without requiring an enforcer or an identifiable extractive administrator — the 'cost' fell on traditions as a structural consequence of their own prior commitments, not through anyone's design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffolding_necessity_vs_sufficiency,
    'Was the Indian philosophical scaffolding (sunya as a coherent void-concept) genuinely NECESSARY for operational zero, or merely the historically first-arriving SUFFICIENT condition — such that some other scaffolding (grammatical, commercial, or purely notational) could have served equally well in a counterfactual where Indian philosophy had not developed sunya?',
    'Comparative analysis of the Maya and Chinese cases, where positional or near-positional notation developed with different (or minimal) philosophical scaffolding for void/nothingness, would test whether operational zero can emerge via alternate scaffolding pathways or whether the Indian pathway was uniquely enabling.',
    'If sunya-style scaffolding turns out to be one of several equally sufficient pathways, the ROPE classification''s coordination-function claim weakens toward the universal_discovery_reading (mathematical necessity dominates); if it was uniquely necessary among historically realized cases, the hybrid reading''s emphasis on scaffolding contingency strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_necessity_vs_sufficiency, empirical, 'Whether the specific Indian philosophical scaffolding was uniquely necessary or one of several sufficient pathways to operational zero.').

omega_variable(
    kernel_framing_undetermination,
    'Is the correct unit of analysis ''zero-as-number'' as a single kernel with three contested readings (as this SCOPE manifest treats it), or are the contingent_thinkability, hybrid_scaffolding, and universal_discovery accounts actually describing three structurally different historical claims (about Europe specifically, about the general transmission-recognition mechanism, and about mathematical ontology respectively) that should never have been yoked into one kernel contest at all?',
    'Test whether each reading''s core claim can be independently falsified without touching the others: the contingent_thinkability claim is falsifiable by evidence of indigenous European zero-development attempts pre-contact; the universal_discovery claim is falsifiable by evidence that positional notation alone, without any scaffolding variation, produces uniform adoption timing across traditions (it does not, empirically); the hybrid claim is falsifiable by evidence that scaffolding made no difference to adoption speed. If all three are independently testable against disjoint evidence sets, they may be separable constraints rather than true kernel-readings.',
    'If the three readings are genuinely separable rather than true alternative framings of one kernel, the kernel_id itself may be miscast, and this reading''s classification as ROPE would need re-evaluation as a standalone constraint rather than as a moderate midpoint between two poles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_undetermination, conceptual, 'Whether the three declared kernel readings are genuinely alternative framings of one contested claim or three separable historical claims artificially yoked together.').

omega_variable(
    victim_retroactivity_coherence,
    'Does it make coherent sense to name the Greek geometric algebra tradition as a ''victim'' of a constraint that postdates the tradition''s living practice by many centuries — can a historically terminated tradition be extracted from, or is this an anachronistic application of the victim category to a case with no living agent to bear the cost?',
    'Conceptual analysis of whether ''victim'' in the Deferential Realism framework requires a currently-existing agent bearing cost in real time, or whether it can extend to a historical tradition understood as a structural position (the mathematics that tradition''s practitioners could have done but didn''t, evaluated counterfactually).',
    'If victim status requires a living bearer of cost, the Greek tradition should be reclassified as a non-agent structural absence rather than a victim, weakening the tangled-rope-adjacent asymmetry this Rope reading gestures toward and pushing the classification further toward a clean coordination story with no true victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_retroactivity_coherence, conceptual, 'Whether a historically terminated tradition can coherently occupy the victim role in this framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__hybrid_scaffolding_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(zero_tr_t0, observed).
narrative_ontology:measurement(zero_tr_t300, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 300, 0.08).
narrative_ontology:measurement_basis(zero_tr_t300, observed).
narrative_ontology:measurement(zero_tr_t600, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 600, 0.15).
narrative_ontology:measurement_basis(zero_tr_t600, observed).
narrative_ontology:measurement(zero_tr_t900, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 900, 0.22).
narrative_ontology:measurement_basis(zero_tr_t900, observed).
narrative_ontology:measurement(zero_tr_t1200, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1200, 0.2).
narrative_ontology:measurement_basis(zero_tr_t1200, observed).
narrative_ontology:measurement(zero_tr_t1500, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1500, 0.18).
narrative_ontology:measurement_basis(zero_tr_t1500, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(zero_be_t0, observed).
narrative_ontology:measurement(zero_be_t300, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 300, 0.18).
narrative_ontology:measurement_basis(zero_be_t300, observed).
narrative_ontology:measurement(zero_be_t600, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 600, 0.28).
narrative_ontology:measurement_basis(zero_be_t600, observed).
narrative_ontology:measurement(zero_be_t900, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 900, 0.35).
narrative_ontology:measurement_basis(zero_be_t900, observed).
narrative_ontology:measurement(zero_be_t1200, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1200, 0.3).
narrative_ontology:measurement_basis(zero_be_t1200, observed).
narrative_ontology:measurement(zero_be_t1500, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1500, 0.32).
narrative_ontology:measurement_basis(zero_be_t1500, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(zero_as_number_entry__hybrid_scaffolding_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__hybrid_scaffolding_reading, information_standard).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry__contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry__universal_discovery_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the zero_as_number_entry kernel. contingent_thinkability_reading holds European emergence was transmission-dependent (no indigenous path); universal_discovery_reading holds the concept was always fully available independent of any tradition's philosophical readiness, with priority being a mere historical accident. This hybrid_scaffolding_reading occupies the structural middle: mathematical availability is tradition-independent (agreeing partially with universal_discovery) but operational thinkability required tradition-specific scaffolding whose absence functioned as a real, differentially-distributed barrier (agreeing partially with contingent_thinkability's emphasis on conceptual barriers, while rejecting its claim that transmission was strictly necessary for eventual European uptake). Each reading should be evaluated independently; their differing epsilon values are not measurement error but evidence of genuinely different structural claims about the same historical episode.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
