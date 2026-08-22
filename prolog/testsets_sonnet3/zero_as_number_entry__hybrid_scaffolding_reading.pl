% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Zero-as-Number Entry Condition (Hybrid Scaffolding Reading)
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This story is one of three readings of a contested kernel about how zero
 *   entered European mathematics as an operable number. This reading holds
 *   that zero-as-number was mathematically latent in positional notation from
 *   the start — a structural consequence of place-value arithmetic — but that
 *   latency alone does not guarantee operational thinkability. Turning the
 *   latent structure into something mathematicians could actually compute
 *   with required a specific conceptual scaffolding (a metaphysics in which
 *   absence or emptiness could be the subject of predication and arithmetic
 *   operation). Indian philosophical traditions — through Buddhist and Jain
 *   treatments of shunya — developed this scaffolding earlier and more
 *   thoroughly than the Greek geometric-magnitude tradition or scholastic
 *   Aristotelian metaphysics, both of which structurally excluded zero as a
 *   number. On this reading, contact between traditions (via Islamic
 *   mathematics) functioned as a trigger for recognition of an already-latent
 *   structure rather than as a transmission of a wholly novel invention — the
 *   recognition event was real and necessary, but what was recognized was not
 *   manufactured by the encounter. This differs from the sibling
 *   contingent_thinkability_reading, which holds Europe would likely never
 *   have gotten there without contact (a stronger transmission-dependency
 *   claim), and from the universal_discovery_reading, which treats the whole
 *   affair as an inevitable mathematical consequence with priority as a mere
 *   historical footnote (a weaker scaffolding-dependency claim, no real
 *   victims).
 *
 * KEY AGENTS:
 *   - hindu_algebraic_tradition: primary beneficiary — indigenous scaffolding compatible with operative zero
 *   - islamic_transmission_scholars: beneficiary and secondary agenda-setter — formalized and propagated the concept
 *   - later_european_algebraists: beneficiary — recognized latent structure once triggered by contact
 *   - greek_geometric_algebra_tradition: primary victim — geometric magnitude metaphysics structurally excludes zero as number
 *   - scholastic_aristotelian_natural_philosophers: secondary victim — privative metaphysics of nothing forecloses operability
 *   - historians_of_mathematics: analytical observer — adjudicates between competing readings of the same kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__hybrid_scaffolding_reading, 0.42).
domain_priors:suppression_score(zero_as_number_entry__hybrid_scaffolding_reading, 0.38).
domain_priors:theater_ratio(zero_as_number_entry__hybrid_scaffolding_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__hybrid_scaffolding_reading, rope).
narrative_ontology:human_readable(zero_as_number_entry__hybrid_scaffolding_reading, "Zero-as-Number Entry Condition (Hybrid Scaffolding Reading)").
narrative_ontology:topic_domain(zero_as_number_entry__hybrid_scaffolding_reading, "history_of_mathematics/philosophy_of_mathematics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__hybrid_scaffolding_reading, '37d42b24-8b4a-41d7-b15d-a82a458fddab').
narrative_ontology:cs_kernel_codification('37d42b24-8b4a-41d7-b15d-a82a458fddab', distributed).
narrative_ontology:cs_authority_grounding('37d42b24-8b4a-41d7-b15d-a82a458fddab', distributed).
narrative_ontology:cs_reading_relation('37d42b24-8b4a-41d7-b15d-a82a458fddab', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('37d42b24-8b4a-41d7-b15d-a82a458fddab', zero_as_number_entry__universal_discovery_reading, influences).
narrative_ontology:cs_axiom('37d42b24-8b4a-41d7-b15d-a82a458fddab', foundational, latency_requires_scaffolding_to_actualize).
narrative_ontology:cs_axiom_status(latency_requires_scaffolding_to_actualize, holdable).
narrative_ontology:cs_axiom_grounding('37d42b24-8b4a-41d7-b15d-a82a458fddab', latency_requires_scaffolding_to_actualize, empirically_contingent).
narrative_ontology:cs_axiom('37d42b24-8b4a-41d7-b15d-a82a458fddab', foundational, contact_triggers_recognition_rather_than_transmits_concept).
narrative_ontology:cs_axiom_status(contact_triggers_recognition_rather_than_transmits_concept, holdable).
narrative_ontology:cs_axiom_grounding('37d42b24-8b4a-41d7-b15d-a82a458fddab', contact_triggers_recognition_rather_than_transmits_concept, empirically_contingent).
narrative_ontology:cs_reference_frame('37d42b24-8b4a-41d7-b15d-a82a458fddab', positional_notation_as_latent_structural_ground).
narrative_ontology:cs_drift_state('37d42b24-8b4a-41d7-b15d-a82a458fddab', post_translation_movement_formalization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('37d42b24-8b4a-41d7-b15d-a82a458fddab', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, islamic_transmission_scholars).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, later_european_algebraists).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_algebra_tradition).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, scholastic_aristotelian_natural_philosophers).
narrative_ontology:constraint_vindicates(zero_as_number_entry__hybrid_scaffolding_reading, conceptual_scaffolding_precedes_operational_thinkability).
narrative_ontology:constraint_vindicates(zero_as_number_entry__hybrid_scaffolding_reading, latent_mathematical_structure_requires_recognition_not_invention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operating within a philosophical vocabulary (shunya as void/absence with ontological standing, place-value arithmetic already in practical use) that permitted zero to be treated as a number subject to arithmetic operations rather than merely a placeholder. This scaffolding was not imported; it developed from indigenous metaphysical resources (Buddhist and Jain treatments of emptiness/nothingness as an object of reasoning) that were already compatible with treating absence as a quantity. The tradition's exit option is best characterized as arbitrage — it could operate zero as number without needing to resolve foreign metaphysical objections.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition, beneficiary,
    moderate, civilizational, arbitrage, regional).

% Mathematicians working in Baghdad and across the Islamic world encountered the Hindu numeral system and its zero, and did the work of formalizing and propagating it (al-Khwarizmi's treatises) into a lingua franca that could travel toward Europe. They benefited from adopting compatible scaffolding early and also actively administered its onward transmission, making them partly agenda-setters in how the concept was packaged for a Latin audience.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, islamic_transmission_scholars, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__hybrid_scaffolding_reading, islamic_transmission_scholars, agenda_setter).

% Renaissance and early modern mathematicians (Fibonacci onward) who, once exposed to the positional system and its operative zero, recognized rather than merely received a structure that was already latent in what positional notation implies. Their arithmetic practice adapted quickly because the recognition, once triggered, did not require them to have independently solved the metaphysical problem — they inherited a working solution and could extend it.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, later_european_algebraists, beneficiary,
    moderate, generational, mobile, continental).

% Bound to a geometric conception of quantity (magnitude as line-length, ratio as the fundamental relation) in which zero has no coherent referent — there is no length of zero, no ratio to nothing. This tradition paid a real cost: centuries of algebraic and computational development were foreclosed by a scaffolding incompatible with operative zero, and the tradition could not simply choose to exit its own metaphysics of quantity without abandoning the geometric framework that gave it its explanatory power elsewhere (e.g., Euclidean proof).
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_algebra_tradition, payer,
    institutional, civilizational, trapped, regional).

% Committed to an Aristotelian metaphysics in which 'nothing' cannot be a subject of predication or a term in operations (horror vacui, privation rather than quantity). This commitment made zero-as-number operationally unthinkable within their own conceptual vocabulary until contact with transmitted Arabic mathematics forced a recognition event; before that contact, the incompatibility was a real structural cost paid in delayed computational and algebraic capacity, not a a matter of choice or resistance to change.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, scholastic_aristotelian_natural_philosophers, payer,
    institutional, civilizational, trapped, regional).

% Study the transmission record, philosophical texts, and computational practices across traditions to adjudicate whether zero's entry into European mathematics represents genuine transmission of a concept, independent recognition of a latent structure, or something else. This reading is one candidate account among the competing readings historians actively debate.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared arithmetic vocabulary in which zero functions as an operable number (not merely a placeholder) solves a genuine coordination problem: it allows positional notation to support subtraction, division, and algebraic manipulation uniformly across traditions and users, rather than requiring each computational context to invent ad hoc workarounds for absence.
% TRANSFER_FUNCTION: The arrangement moves computational and algebraic capacity — the ability to perform certain operations efficiently — from traditions locked into incompatible scaffolding (geometric magnitude, privative metaphysics) toward traditions whose conceptual vocabulary already permitted zero to be treated as a number. It is not a transfer of wealth or coercive extraction; it is a transfer of operational capability contingent on conceptual compatibility.
% ABSENT_VOICES: The Greek geometric tradition and scholastic Aristotelian philosophers cannot straightforwardly object from within their own frameworks because the incompatibility is precisely what makes the objection unavailable to them — they lack the vocabulary to articulate zero as a number rather than as an absence of magnitude. Their historical silence on this specific point is itself part of the structural evidence for the reading.
% DISAPPEARANCE_RATIONALE: If one asks what would happen if this particular coordination structure (compatible scaffolding permitting operative zero) had never emerged anywhere, the honest answer is contested among historians: some hold that positional notation's logical structure would eventually force recognition regardless of which tradition got there first (supporting the universal_discovery sibling), while this reading holds that recognition required a specific and non-guaranteed conceptual scaffolding, and its absence in a tradition could persist indefinitely (Greek mathematics show no independent movement toward it over centuries). The verdict is genuinely disputed rather than settled by this story alone.
% FOUNDING_PROBLEM: The practical problem was representing 'no units in this place' unambiguously in positional numeral systems so that arithmetic (especially subtraction yielding no remainder, and place-holding in multi-digit numbers) could be performed reliably; the deeper conceptual problem was whether an absence could be treated as a term over which arithmetic operations are defined.
% FOUNDING_PROBLEM_CORROBORATION: The practical placeholder problem was resolved centuries ago and is uncontested among mathematicians and historians of all camps; the deeper operability of zero as an arithmetic and algebraic object is now foundational and unchallenged in contemporary mathematics itself (not merely asserted by beneficiary traditions) — this is attested by the uniform adoption of zero-as-number across every mathematical tradition worldwide today, including those (heirs of the Greek and scholastic traditions) that once lacked the scaffolding, which is external corroboration that the original barrier was scaffolding-dependent rather than a permanent limit.
narrative_ontology:disappearance_verdict(zero_as_number_entry__hybrid_scaffolding_reading, contested).
narrative_ontology:founding_problem_status(zero_as_number_entry__hybrid_scaffolding_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_as_number_entry__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate 0.42 because this reading treats the arrangement as a genuine coordination problem (shared arithmetic vocabulary) with asymmetric payoffs, not as coercive extraction — the 'cost' borne by the Greek and scholastic traditions is a genuine opportunity cost of scaffolding incompatibility, not a transfer extracted through enforcement. Suppression is moderate (0.38) reflecting that no party actively prevented the Greek or scholastic traditions from developing operative zero; the incompatibility was internal to their own conceptual frameworks, not imposed by an external suppressing agent. Theater ratio is low (0.22) and rises only slowly across the interval, reflecting that the story is substantively about real conceptual and computational capability, not performative activity — the growth reflects increasing formalization and institutionalization of zero's role as mathematics matured, not increasing performance relative to function. Accessibility collapse is moderate (0.55): once the compatible scaffolding was recognized, the older frameworks did not vanish but became genuinely harder to use for advanced arithmetic, producing real but partial collapse of alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Hindu algebraic tradition, Islamic transmission scholars, later European algebraists) are those whose conceptual vocabulary was, or became, compatible with treating zero as an operable number — they get low directionality because the constraint (the entry-condition structure itself) subsidizes their computational capacity. Victims (Greek geometric algebra tradition, scholastic Aristotelian philosophers) are those whose own metaphysics structurally excluded this move — they bear the real cost of delayed algebraic and computational development, and their exit option is 'trapped' because leaving their own conceptual framework was not a live option available to them at the time; the incompatibility was intrinsic, not chosen.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is deliberately classified as rope rather than snare or tangled_rope: the coordination function (a shared operable-zero vocabulary enabling algebra and computation) is genuine and the 'victims' are victims of exclusion from a beneficial structure via their own prior conceptual commitments, not victims of active coercive enforcement by a beneficiary party. There is no agenda-setter forcing the Greek tradition to abandon geometric algebra; the classification requires requires_active_enforcement to be false, which is authored accordingly. This prevents mislabeling a genuine, contingent conceptual-history coordination problem as either pure extraction (snare) — no one profits by keeping others out — or as inevitable natural law (mountain) — the entry condition is not a physical necessity, it depends on contingent, structurally locatable philosophical scaffolding that different traditions did or did not possess.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_vs_transmission_boundary,
    'Is there a principled way to distinguish ''contact triggered recognition of a latent structure'' from ''contact transmitted a novel concept'' when the observable historical record (texts, translations, computational practice) looks similar under both descriptions?',
    'Close textual analysis of the earliest Latin treatments of zero (e.g., Fibonacci''s Liber Abaci) for evidence of independent conceptual work versus verbatim adoption of Arabic/Sanskrit argument structures; comparison with cases of genuine independent co-discovery elsewhere in mathematics.',
    'If the textual record shows substantial independent conceptual labor by European mathematicians beyond translation, this reading is strengthened relative to contingent_thinkability_reading. If it shows near-verbatim adoption with no independent recognition event, this reading collapses toward contingent_thinkability_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recognition_vs_transmission_boundary, conceptual, 'Whether the recognition/transmission distinction this reading depends on is empirically discernible or an artifact of framing.').

omega_variable(
    latency_claim_verifiability,
    'Is the claim that zero-as-number was ''latent'' in positional notation a testable historical/mathematical claim, or is it an unfalsifiable retrospective attribution that could be made of any eventually-discovered mathematical object?',
    'Formal analysis of whether positional notation without an operative zero concept can be shown to generate internal pressure toward zero (e.g., via computational errors, ambiguities in subtraction) that a scaffolding-free tradition would eventually have to resolve.',
    'If latency is unfalsifiable, the moderate ε on ''mathematical necessity'' in this reading is undersupported and the reading collapses toward being indistinguishable from contingent_thinkability with different rhetoric. If latency is demonstrable as internal computational pressure, the rope classification (coordination function around a genuinely available structure) is well-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latency_claim_verifiability, conceptual, 'Whether the mathematical-availability claim underlying this reading is empirically meaningful or a retrospective narrative device.').

omega_variable(
    scaffolding_contingency_degree,
    'How contingent was the Indian philosophical scaffolding itself — could a comparably compatible scaffolding have failed to develop in India, leaving zero-as-number undiscovered anywhere for a much longer period, or was some tradition eventually going to produce compatible metaphysics given enough contact among numerate cultures?',
    'Comparative study of other numerate civilizations (Mesoamerican, Chinese, Babylonian) that developed positional or near-positional notation without full operative zero, assessing whether any showed independent movement toward compatible scaffolding absent Indian influence.',
    'A finding of multiple independent near-misses elsewhere would support a moderate necessity component to ε (this reading''s own commitment); a finding that Indian scaffolding was a genuine historical singularity would push ε toward stronger contingency and closer to the contingent_thinkability_reading''s implicit priors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_contingency_degree, empirical, 'How singular versus replicable the specific scaffolding conditions were across independent numerate civilizations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__hybrid_scaffolding_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(zero_tr_t0, observed).
narrative_ontology:measurement(zero_tr_t300, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 300, 0.12).
narrative_ontology:measurement_basis(zero_tr_t300, observed).
narrative_ontology:measurement(zero_tr_t600, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 600, 0.15).
narrative_ontology:measurement_basis(zero_tr_t600, observed).
narrative_ontology:measurement(zero_tr_t900, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 900, 0.18).
narrative_ontology:measurement_basis(zero_tr_t900, observed).
narrative_ontology:measurement(zero_tr_t1200, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1200, 0.2).
narrative_ontology:measurement_basis(zero_tr_t1200, observed).
narrative_ontology:measurement(zero_tr_t1500, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1500, 0.22).
narrative_ontology:measurement_basis(zero_tr_t1500, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(zero_be_t0, observed).
narrative_ontology:measurement(zero_be_t300, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 300, 0.22).
narrative_ontology:measurement_basis(zero_be_t300, observed).
narrative_ontology:measurement(zero_be_t600, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 600, 0.3).
narrative_ontology:measurement_basis(zero_be_t600, observed).
narrative_ontology:measurement(zero_be_t900, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 900, 0.36).
narrative_ontology:measurement_basis(zero_be_t900, observed).
narrative_ontology:measurement(zero_be_t1200, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1200, 0.4).
narrative_ontology:measurement_basis(zero_be_t1200, observed).
narrative_ontology:measurement(zero_be_t1500, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1500, 0.42).
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
% This story is one of three readings of the zero_as_number_entry kernel, decomposed per the epsilon-invariance principle: contingent_thinkability_reading (strong transmission-dependency, higher implied ε on European conceptual barrier), hybrid_scaffolding_reading (this story — moderate ε on both scaffolding contingency and mathematical necessity, rope classification), and universal_discovery_reading (near-zero ε, no real victims, treats priority as historically accidental but ontologically inert). Each reading authors its own beneficiary/victim structure and its own ε; they are linked here rather than merged because measuring the same historical episode by 'was recognition contact-dependent' versus 'was the structure always fully available' yields different extraction profiles and different victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
