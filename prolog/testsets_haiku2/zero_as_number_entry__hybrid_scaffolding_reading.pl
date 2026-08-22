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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: zero_as_number_entry__hybrid_scaffolding_reading
 *   human_readable: Zero-as-Number: Hybrid Scaffolding Entry Coordination
 *   domain: mathematics/philosophy/conceptual_history
 *
 * SUMMARY:
 *   This reading of the zero-as-number kernel claims that zero was
 *   mathematically LATENT in positional notation—its necessity follows
 *   logically from place-value structure—but operationally HIDDEN until
 *   specific metaphysical scaffolding made it conceptually thinkable. Indian
 *   philosophical traditions (Vedantic śūnya, Brahminical void-concept)
 *   provided this scaffolding centuries before European traditions
 *   encountered positional notation. Contact did not transmit a pre-packaged
 *   concept; it transmitted the structural knowledge that allows latent
 *   mathematical content to be RECOGNIZED and OPERATIONALIZED. This is a
 *   coordination problem: how do incompatible metaphysical frameworks come to
 *   share the same operational mathematics? The constraint models the answer:
 *   through transmission of scaffolding structures that trigger recognition
 *   of latent availability.
 *
 * KEY AGENTS:
 *   - Hindu algebraic tradition: operationalized zero-as-number first, via compatible Vedantic metaphysics
 *   - Greek geometric tradition: locked into incompatible magnitude-ratio framework, cannot operationalize zero without framework restructuring
 *   - Islamic mathematical tradition: absorbs and formalizes Hindu zero, transmits scaffolding structures to Europe
 *   - European medieval mathematicians: trapped in Roman numeral / abacus paradigm until contact reveals that positional notation + zero are recognizable structures
 *   - Positional notation structure: the mathematical fact (latent zero-necessity) that scaffolding makes operative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__hybrid_scaffolding_reading, 0.48).
domain_priors:suppression_score(zero_as_number_entry__hybrid_scaffolding_reading, 0.52).
domain_priors:theater_ratio(zero_as_number_entry__hybrid_scaffolding_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__hybrid_scaffolding_reading, rope).
narrative_ontology:human_readable(zero_as_number_entry__hybrid_scaffolding_reading, "Zero-as-Number: Hybrid Scaffolding Entry Coordination").
narrative_ontology:topic_domain(zero_as_number_entry__hybrid_scaffolding_reading, "mathematics/philosophy/conceptual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__hybrid_scaffolding_reading, 'ce27a5a3-10c3-42f8-adf3-cc132eec1f3d').
narrative_ontology:cs_kernel_codification('ce27a5a3-10c3-42f8-adf3-cc132eec1f3d', distributed).
narrative_ontology:cs_authority_grounding('ce27a5a3-10c3-42f8-adf3-cc132eec1f3d', practice).
narrative_ontology:cs_interpretation_layer_present('ce27a5a3-10c3-42f8-adf3-cc132eec1f3d').
narrative_ontology:cs_reading_relation('ce27a5a3-10c3-42f8-adf3-cc132eec1f3d', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce27a5a3-10c3-42f8-adf3-cc132eec1f3d', zero_as_number_entry__universal_discovery_reading, coexists_with).
narrative_ontology:cs_axiom('ce27a5a3-10c3-42f8-adf3-cc132eec1f3d', foundational, latent_availability_thesis).
narrative_ontology:cs_axiom_status(latent_availability_thesis, holdable).
narrative_ontology:cs_axiom_grounding('ce27a5a3-10c3-42f8-adf3-cc132eec1f3d', latent_availability_thesis, empirically_contingent).
narrative_ontology:cs_axiom('ce27a5a3-10c3-42f8-adf3-cc132eec1f3d', foundational, scaffolding_contingency_thesis).
narrative_ontology:cs_axiom_status(scaffolding_contingency_thesis, holdable).
narrative_ontology:cs_axiom_grounding('ce27a5a3-10c3-42f8-adf3-cc132eec1f3d', scaffolding_contingency_thesis, empirically_contingent).
narrative_ontology:cs_reference_frame('ce27a5a3-10c3-42f8-adf3-cc132eec1f3d', mathematical_latency_framework).
narrative_ontology:cs_drift_state('ce27a5a3-10c3-42f8-adf3-cc132eec1f3d', post_transmission_normalization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ce27a5a3-10c3-42f8-adf3-cc132eec1f3d', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, islamic_mathematical_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, later_european_mathematicians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, european_medieval_mathematicians).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_tradition).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, european_medieval_mathematicians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developed philosophical and mathematical scaffolding compatible with zero-as-number: the Brahminical concept of śūnya (emptiness/void) from Vedantic metaphysics provided conceptual grounding for null quantity in arithmetic; algebraic notation (Aryabhata, Bhaskara) operationalized zero as a numeral and quantity-placeholder. This tradition makes zero thinkable without friction. The constraint coordinates the emergence of zero-as-number as a functional tool within a framework already prepared.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition, beneficiary,
    institutional, civilizational, analytical, regional).

% Operates exclusively within geometric magnitudes and ratios of continuous quantities; Aristotelian metaphysics treats 'nothing' (μηδέν) as the absence of being, not as a quantity or operational entity. The absence of zero from Greek mathematics is not careless—it follows from a coherent but incompatible conceptual framework. The constraint's enforcement (incompatibility of scaffolding) locks this tradition out of zero-as-number operationality; recognition of the structure requires restructuring the entire conceptual foundation.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_tradition, payer,
    institutional, civilizational, constrained, regional).

% Initially locked into Roman numeral and abacus-based computation, incompatible with positional notation and zero-as-number operability. Contact with Islamic and Hindu mathematics (via al-Khwarizmi, translation movements in Sicily and Spain, 11th–13th centuries) transmits not the concept pre-packaged but the scaffolding structures that make the latent mathematical content recognizable. They gain the benefit of zero-operationality without building the philosophical framework de novo—they recognize and adopt compatible scaffolding already demonstrated elsewhere.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, european_medieval_mathematicians, beneficiary,
    institutional, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__hybrid_scaffolding_reading, european_medieval_mathematicians, payer).

% Acts as the transmission intermediary: absorbs Hindu mathematical practice and philosophical grounding, formalizes and extends it (al-Khwarizmi, al-Ghazali), and makes it available to European scholars through translation and commerce. The tradition benefits from the constraint because it can coordinate mathematical knowledge across geographic and linguistic communities by operationalizing zero as a universal numeral.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, islamic_mathematical_tradition, beneficiary,
    institutional, civilizational, analytical, regional).

% The mathematical structure itself: a position-based number system (place-value notation) IMPLIES a need for a placeholder symbol to distinguish 204 from 24. This implication is latent in the structure; it does not depend on any tradition's recognition. The constraint's core insight is that latency ≠ operability: the structure makes zero necessary, but does not make it thinkable without compatible metaphysical scaffolding.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, positional_notation_structure, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(zero_as_number_entry__hybrid_scaffolding_reading, positional_notation_structure).

% Analytic seat examining whether zero-as-number was discovered, invented, transmitted, or recognized-from-latency. This reading claims zero was always mathematically available (latent in positional structure) but required cultural-conceptual scaffolding to become thinkable and operational. The constraint models the coordination problem at the heart of this historical claim.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, conceptual_historians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_as_number_entry__hybrid_scaffolding_reading, diffuse).
narrative_ontology:fixing_cost_class(zero_as_number_entry__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables mathematical traditions with different metaphysical foundations to operationalize the same positional notation system by coordinating around compatible conceptual scaffolding (zero-as-void, zero-as-placeholder, zero-as-operational-quantity). The constraint solves the problem: how can a mathematical structure (positional notation) that logically requires zero become practically usable across traditions with incompatible philosophical commitments?
% TRANSFER_FUNCTION: Moves recognitional authority from the tradition that first operationalized zero (Hindu algebraic tradition) to traditions that later adopt the scaffolding (European, Islamic): not a transfer of material goods, but of conceptual-operational capacity. What moves is the understanding of how to make zero thinkable by restructuring one's base metaphysical commitments.
% ABSENT_VOICES: Traditions that developed positional notation but failed to operationalize zero (or never encountered compatible scaffolding to trigger recognition) are structurally excluded. No voice from within a closed geometric framework objects to zero—silence is enforced by the incompatibility, not by institutional suppression. The constraint is not contested within incompatible frames because the frame prevents even the question from arising.
% DISAPPEARANCE_RATIONALE: If this coordination vanished—if no tradition had operationalized zero-as-number and no scaffolding transmission occurred—positional notation would remain latent or incomplete across all traditions. European mathematics would have developed along alternative paths (continued reliance on Roman numerals and abacus, or independent invention of placeholder-handling). The history of mathematics fundamentally changes: no shared universal numerals, no arithmetic beyond geometric magnitude ratios for centuries longer, different paths to algebra and calculus.
% FOUNDING_PROBLEM: How can a mathematical structure (positional notation) that logically REQUIRES a placeholder symbol become practically and conceptually OPERATIBLE when the world's major mathematical traditions operate within incompatible metaphysical frameworks (geometric vs. algebraic, continuous vs. discrete, being-centric vs. emptiness-compatible)?
% FOUNDING_PROBLEM_CORROBORATION: Historians of mathematics (Joseph, Katz, Eves, Ott) attest that zero-as-number emerged in Hindu mathematics earlier than in European mathematics, and that transmission (direct or indirect) of Indian and Islamic mathematics to Europe accelerated European adoption of positional notation and zero. Philosophers of mathematics (Lakoff & Núñez, Dehaene) attest that conceptual scaffolding (metaphor, metaphysics, linguistic availability) constrains what mathematical structures become thinkable. The founding problem is live because new mathematical structures (those not yet operationalized in any tradition) still face the same coordination problem: latent availability ≠ operative thinkability.
narrative_ontology:disappearance_verdict(zero_as_number_entry__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__hybrid_scaffolding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_as_number_entry__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_as_number_entry__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at moderate (0.48) because the constraint involves both mathematical necessity (positional notation logically requires a placeholder—low contingency) and metaphysical scaffolding contingency (traditions need compatible frameworks to recognize what's latent—high contingency). The measurement trajectory shows extractiveness rising steeply from t=0 to t=9 (contact period, transmission of frameworks), then stabilizing as recognition spreads and zero-operability becomes normalized. Suppression (0.52) reflects the incompatibility enforcement: Greek geometric algebra CANNOT operationalize zero without restructuring; the 'suppression' is not coercive but structural (incompatible metaphysics, not hidden rules). Theater is low (0.22) because the coordination is genuine and functional—there is no performative surface masking extraction. The constraint is a ROPE: real coordination problem (how do traditions with different metaphysics share operationality?), real beneficiary set (traditions gaining zero-operability without building metaphysical foundations from scratch), no identifiable victims (the Greek tradition is 'locked out' by incompatibility, not exploited by design).
 *
 * PERSPECTIVAL GAP:
 *   The Hindu algebraic tradition and the European medieval mathematicians should compute differently: from the Hindu seat, zero-operability is the natural flowering of compatible scaffolding—they built the framework and operated within it (low extraction, high benefit). From the European seat at the contact moment, the constraint appears asymmetric: they must restructure their entire metaphysical commitments to recognize what the Hindu framework took for granted (high extraction, forced adaptation). As recognition spreads and zero becomes normalized across traditions, the perspective gap shrinks—zero is eventually treated as a mathematical universal, not as a transmission-dependent gift. The engine computes this dynamically from the structural data: beneficiary status (Hindu) vs. constrained-by-incompatibility (Greek) vs. late-adapter (European) produce different per-seat classifications despite a single coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu algebraic tradition: beneficiary (derives operability from compatible scaffolding, no cost to adopt what's already embedded in their metaphysics). Greek geometric tradition: neither beneficiary nor full victim—they are constrained by incompatibility, but incompatibility is not enforced coercively, it is structural. European medieval mathematicians: constrained payers (must restructure metaphysical commitments to gain zero-operability, constrained by incompatibility but free to adopt the new framework). Islamic tradition: beneficiary-transmitter (absorbs zero-operability from Hindu source, re-transmits it to European source, gains universal numerals). The constraint's directionality is COLLECTIVE: it does not extract from a subset on behalf of a concentrated beneficiary; it solves a genuine coordination problem (how do incompatible frameworks operationalize the same mathematics?). No single seat is the capturer or controller; the coordination emerges from transmission and recognition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (incompatible metaphysical frameworks cannot operationalize the same mathematical structures without coordination) is LIVE. Zero-as-number remains a live coordination problem: any new mathematical structure (e.g., infinitesimals, imaginary numbers, category-theoretic abstractions) faces the same problem—mathematical availability (latent in structure) ≠ conceptual operability (requires compatible scaffolding). The constraint is not mandatrophic; the coordination is essential. The measurement trajectory confirms this: extractiveness and suppression stabilize around t=9 (recognition spreads) and remain stable, suggesting the constraint has found its equilibrium (the coordination is institutionalized, scaffolding is transmitted through education, zero is universally operational). No rise in theater (which would signal the coordination function has atrophied) indicates the constraint remains functionally alive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latency_vs_discovery_ambiguity,
    'Is zero-as-number a DISCOVERY of latent mathematical structure, or an INVENTION of conceptual scaffolding to operationalize latent structure? The hybrid reading claims both: latent necessity (discovery) + scaffolding contingency (invention). But does the latent structure fully determine zero''s properties, or do scaffolding choices shape zero''s identity?',
    'Examine counterfactual mathematical histories: did alternative scaffolding choices (non-Vedantic, non-void-based) lead to the same operational zero? Did European re-derivation of zero (possibly independent of Hindu transmission) arrive at identical or divergent operationality?',
    'If scaffolding fully determines zero''s identity (strong contingency), the reading shifts toward hybrid-construction (zero is partly invented). If latent structure fully determines zero''s properties (strong universality), the reading shifts toward pure discovery. The most likely resolution: latent structure constrains the space of possible zeros (e.g., a placeholder in positional notation must behave a certain way), but scaffolding choices fill in degrees of freedom (e.g., whether zero is treated as a quantity, a absence, or purely notational).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latency_vs_discovery_ambiguity, conceptual, 'The boundary between mathematical availability (discovery) and conceptual operability (scaffolding-dependent). This is the core ambiguity the hybrid reading sits on.').

omega_variable(
    scaffolding_sufficiency_question,
    'Was Hindu algebraic scaffolding SUFFICIENT to operationalize zero, or was zero-as-number only achievable through a specific historical path (Hindu philosophy + positional notation + algebraic notation + transmission)?',
    'Comparative analysis: did other traditions develop compatible scaffolding independently (e.g., did Islamic metaphysics or European scholasticism arrive at zero-compatible frameworks without external input)? Or is operationalization tied to a specific scaffolding-technique coupling?',
    'If scaffolding is sufficient, zero could emerge in any tradition that develops compatible metaphysical frameworks (supports universality of operability, contingency of which tradition first). If scaffolding is necessary but not sufficient (specific technique-coupling matters), zero''s emergence is more historically contingent—European independent rediscovery becomes less likely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffolding_sufficiency_question, empirical, 'Whether the Hindu scaffolding enabled zero universally, or whether specific historical conditions (notation + metaphysics + algebra) were jointly necessary.').

omega_variable(
    transmission_mechanism_opacity,
    'Did transmission of zero-operability occur through explicit teaching (al-Khwarizmi''s algorithms, translation of Hindu texts) or implicit recognition (European mathematicians seeing positional notation work and reverse-engineering the metaphysics)?',
    'Historical-textual analysis: examine 11th–13th century mathematical texts for explicit zero-pedagogy vs. gradual operationalization without conceptual articulation. Interview modern mathematicians learning positional notation in non-native frameworks to observe how conceptual scaffolding is absorbed.',
    'If explicit teaching, the constraint is primarily an institutional coordination problem (how are frameworks transmitted?). If implicit recognition, the constraint is primarily a cognitive-conceptual problem (how do people recognize structures latent in notation?). The reading assumes implicit recognition dominates, but explicit teaching may be more important than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_mechanism_opacity, empirical, 'How metaphysical scaffolding is actually transferred across traditions—pedagogy vs. cognitive recognition.').

omega_variable(
    greek_alternative_path_counterfactual,
    'Could the Greek geometric tradition have operationalized zero-as-number by restructuring only Aristotelian metaphysics (accepting emptiness as a being), while retaining the rest of the geometric framework?',
    'Reconstructive analysis: model the minimal metaphysical changes required for zero-operability in Aristotelian framework. Examine whether neo-Platonism or later Scholasticism made moves toward void-as-entity that could have enabled zero-adoption.',
    'If geometry could absorb zero without full restructuring, the ''victim'' framing (Greek tradition locked out) overstates the constraint—only a local modification was required. If geometry is structurally incompatible with zero-operability (ratios of continuous quantities cannot accommodate discrete void-placeholders), the victim framing is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(greek_alternative_path_counterfactual, conceptual, 'Whether geometric algebra was fundamentally incompatible with zero, or whether minimal philosophical revision could have enabled operationalization.').

omega_variable(
    kernel_ambiguity_reading_divergence,
    'This reading (hybrid_scaffolding) claims zero is latent-but-hidden. The contingent_thinkability_reading claims zero is fully contingent on transmission. The universal_discovery_reading claims zero is always-available. Which reading most accurately captures the history? Can all three coexist, or does the history falsify some of them?',
    'Comparative historical analysis: did independent mathematical traditions (Islamic, Chinese, Mesoamerican) arrive at positional notation + zero operability with or without Hindu transmission? Did European discovery of zero require Hindu knowledge or could have occurred independently? The historical record should constrain which readings are holdable.',
    'If independent discovery is documented (Chinese, Islamic, European), all three readings coexist (universal_discovery is correct, but contingent_thinkability exaggerates necessity of Hindu contact, hybrid_scaffolding correctly identifies latency and scaffolding). If all zero-operationality traces to Hindu origin with transmission, contingent_thinkability is strengthened, universal_discovery is weakened, hybrid_scaffolding remains stable (latency can be universal but operationalization contingent on scaffolding).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_ambiguity_reading_divergence, empirical, 'The kernel-level ambiguity: was zero-as-number always available (and differently operationalized across traditions), or was it available only through transmission chains? This reading sits between the extremes; empirical history should disambiguate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__hybrid_scaffolding_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(zero_tr_t3, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 3, 0.14).
narrative_ontology:measurement(zero_tr_t6, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(zero_tr_t9, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 9, 0.21).
narrative_ontology:measurement(zero_tr_t12, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(zero_tr_t15, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 15, 0.22).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(zero_be_t3, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 3, 0.41).
narrative_ontology:measurement(zero_be_t6, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(zero_be_t9, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 9, 0.48).
narrative_ontology:measurement(zero_be_t12, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(zero_be_t15, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 15, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(zero_su_t3, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 3, 0.44).
narrative_ontology:measurement(zero_su_t6, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(zero_su_t9, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 9, 0.52).
narrative_ontology:measurement(zero_su_t12, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(zero_su_t15, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__hybrid_scaffolding_reading, information_standard).
narrative_ontology:boltzmann_floor_override(zero_as_number_entry__hybrid_scaffolding_reading, 0.12).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry__contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry__universal_discovery_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, positional_notation_mathematical_necessity).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, algebraic_notation_emergence).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, vedantic_metaphysics_mathematical_structure).

% DUAL FORMULATION NOTE:
% The zero_as_number_entry kernel decomposes into three structurally distinct constraint readings. This is the HYBRID_SCAFFOLDING_READING, which asserts zero-as-number was mathematically latent (in positional notation structure) but operationally contingent (requiring compatible metaphysical scaffolding to be thinkable). The contingent_thinkability_reading emphasizes the contingency (no transmission = no European zero); the universal_discovery_reading emphasizes the availability (zero is a logical consequence, discovered/re-discovered independently across traditions). All three readings share the same kernel (the historical claim: 'how did zero become operational?') but assign different ε values and beneficiary/victim structures based on what they emphasize (latency vs. contingency vs. universality). The network links these as a family; the engine computes classification divergence across seats and readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zero_as_number_entry__hybrid_scaffolding_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
