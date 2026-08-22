% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_adaptive_fiction, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Constitutional Fiction: Immutability as Rhetorical Cover for Institutional Adaptation
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The Lycurgan Constitution of Sparta presents itself as immutable — a
 *   complete, unchangeable set of laws ordained by Lycurgus and sacred to the
 *   state. This constraint story instantiates the ADAPTIVE FICTION READING:
 *   the immutability claim is a noble lie, a rhetorical cover that masks
 *   continuous institutional adaptation by ephors and kings. Ephors
 *   reinterpret the laws through creative hermeneutics and rhetra amendments,
 *   adapting to military pressure, resource constraints, and demographic
 *   change while maintaining the fiction of fidelity. This reading claims the
 *   constraint is TANGLED ROPE: it genuinely coordinates military unity and
 *   citizen equality (rope function), but it also extracts political power
 *   and discretionary authority from citizens by preventing transparent
 *   renegotiation of the fundamental law (extraction function). The
 *   immutability fiction keeps both functions in place.
 *
 * KEY AGENTS:
 *   - ephoral_oligarchy: Sets policy through interpretation, claims fidelity while adapting — institutional power, trapped exit (reputation risk)
 *   - spartan_citizen_body: Bears the costs of austerity and military obligation, receives citizen equality and security — organized power, identity-locked exit
 *   - helot_population: Bears full extraction cost with no benefit claim — powerless, trapped exit
 *   - spartan_military_establishment: Benefits from the coordination (unified force) and the flexibility (hidden adaptation) — institutional power, constrained exit
 *   - historical record: Would contradict the immutability claim with evidence of rhetra amendments and interpretation drift — excluded until post-Sparta
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.68).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.55).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, tangled_rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Constitutional Fiction: Immutability as Rhetorical Cover for Institutional Adaptation").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political/constitutional").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '157f4f14-066a-4535-99c7-f33ed6d75dc9').
narrative_ontology:cs_kernel_codification('157f4f14-066a-4535-99c7-f33ed6d75dc9', fixed_text).
narrative_ontology:cs_authority_grounding('157f4f14-066a-4535-99c7-f33ed6d75dc9', extraction).
narrative_ontology:cs_interpretation_layer_present('157f4f14-066a-4535-99c7-f33ed6d75dc9').
narrative_ontology:cs_reading_relation('157f4f14-066a-4535-99c7-f33ed6d75dc9', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_reading_relation('157f4f14-066a-4535-99c7-f33ed6d75dc9', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('157f4f14-066a-4535-99c7-f33ed6d75dc9', foundational, immutability_is_strategic_fiction).
narrative_ontology:cs_axiom_status(immutability_is_strategic_fiction, holdable).
narrative_ontology:cs_axiom_grounding('157f4f14-066a-4535-99c7-f33ed6d75dc9', immutability_is_strategic_fiction, empirically_contingent).
narrative_ontology:cs_axiom('157f4f14-066a-4535-99c7-f33ed6d75dc9', secondary, ephoral_interpretation_enables_functional_adaptation).
narrative_ontology:cs_axiom_status(ephoral_interpretation_enables_functional_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('157f4f14-066a-4535-99c7-f33ed6d75dc9', ephoral_interpretation_enables_functional_adaptation, empirically_contingent).
narrative_ontology:cs_reference_frame('157f4f14-066a-4535-99c7-f33ed6d75dc9', lycurgan_immutable_constitution).
narrative_ontology:cs_drift_state('157f4f14-066a-4535-99c7-f33ed6d75dc9', classical_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('157f4f14-066a-4535-99c7-f33ed6d75dc9', '2026-06-12T14:33:22Z').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephoral_oligarchy).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_military_establishment).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartan_citizen_body).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, helot_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_citizen_body).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, lawgiver_cult_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five ephors and the two kings jointly administer the Lycurgan system. They publicly claim total fidelity to the unchangeable laws while privately interpreting them through rhetra amendments and creative readings that adapt to military and economic pressures. They benefit from the immutability claim because it shields their decisions from direct scrutiny — they can reframe adaptive changes as faithful interpretation of the founders' intent rather than as explicit amendment. Their exit from this arrangement would require public admission that the laws are flexible, which would undermine their authority to rule.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ephoral_oligarchy, agenda_setter,
    institutional, generational, trapped, regional).

% Spartan citizens are bound by the common mess system, land redistribution rules, education regimen, and military obligation. They bear the costs of an austere lifestyle justified by permanent fidelity to Lycurgas. They also receive a form of benefit: equality among the citizen body (no private wealth accumulation), exclusion of merchants and artisans (preserving citizen monopoly on military power), and collective security. Their identity as Spartans is fused with Lycurgan law; exit means ceasing to be Spartan. The adaptive fiction allows the ephors to loosen or tighten constraints without requiring citizens to renegotiate collective identity.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_citizen_body, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, spartan_citizen_body, beneficiary).

% The helots are bound laborers whose exploitation is justified by Lycurgan law — they produce agricultural surplus to fund citizen equality. They bear the full cost of the system without any benefit claim. The immutability fiction keeps their subordination in place: the laws cannot be challenged, so their status cannot be revised. Ephoral interpretation of Lycurgan law has historically allowed periodic 'krypteia' (secret youth expeditions) that target helots, presented as fidelity to the founders' intent rather than as discretionary violence.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helot_population, payer,
    powerless, generational, trapped, regional).

% Military commanders and hoplite unit leaders benefit from the stability and cohesion the common mess system and unified education produce. They also benefit from the flexibility: ephoral interpretation allows tactical adaptation — loan innovations, shifts in hoplite deployment, and responses to changing adversary tactics — without requiring open revision of the law. The immutability claim provides legitimacy cover for these adaptations.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_military_establishment, beneficiary,
    institutional, generational, constrained, regional).

% Other Greek city-states observe Spartan law as a model of stability and unchangeability. This observation reinforces Spartan authority — the immutability claim attracts admiration and fear. Other poleis cannot directly alter Spartan law, but their diplomatic and commercial interactions create pressure for adaptation that ephoral interpretation manages quietly.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, other_greek_poleis, observer,
    institutional, generational, analytical, regional).

% Later historical accounts (Plutarch, Xenophon) would record both the immutability claim AND evidence of adaptation (rhetra amendments, ephoral decisions that reinterpret the law). This record contradicts the immutability narrative but was not part of the deliberative circle when Lycurgan decisions were made. The exclusion is temporal: the historical record only emerges after Sparta's decline.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, external_historiographical_record, excluded,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__adaptive_fiction_reading, ephoral_oligarchy).
narrative_ontology:fixing_cost_class(lycurgan_laws__adaptive_fiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Lycurgan system solves a collective-action problem: how to maintain military superiority and prevent internal wealth stratification in a warrior society. The common mess, equal land allocation, and austere education create a unified military body and prevent the oligarchic fragmentation that plagued other Greek cities. The immutability claim enhances coordination by preventing citizens from demanding exceptions or renegotiation.
% TRANSFER_FUNCTION: The arrangement transfers agricultural surplus from helots to Spartan citizens, military service and identity-subordination from citizens to the state, and discretionary enforcement power from a rule-of-law framework to ephoral interpretation. The immutability fiction keeps these transfers in place by preventing challenges to their legal basis.
% ABSENT_VOICES: Helots have no voice in the system; they would object to lifelong bondage if permitted. Non-military Spartan women, disenfranchised foreigners (perioikoi), and reformist citizens who might question the laws are excluded. The historical record (later accounts by Plutarch and outsiders like Aristotle) would object to the immutability claim itself — they report adaptation and reinterpretation — but these voices arise only after Sparta's decline.
% DISAPPEARANCE_RATIONALE: If the Lycurgan system and its enforcement vanished, Spartan society would reorganize rapidly: citizens would seek private wealth accumulation and commercial activity (as they later did post-Sparta), helots would escape or rebel, and the military machine would lose the institutional machinery that produced unified hoplite cohesion. The immutability fiction is a load-bearing structural element; its disappearance would precipitate cascading renegotiation of every Lycurgan commitment.
% FOUNDING_PROBLEM: Lycurgus is presented as having solved the problem of how to maintain a warrior society without internal class conflict or corruption by wealth. The founding problem is: how do we prevent wealthy oligarchs from dominating military decisions and how do we ensure all citizens are equally invested in the collective defense?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by Xenophon and Plutarch as a genuine historical challenge in Greek poleis (internal wealth stratification did destabilize other cities). However, Aristotle and later historians also attest that Lycurgan law DID change over time — through rhetra amendments and interpretation — which contradicts the immutability claim. The problem was live in the 8th century BCE when the system was established; by the Classical period it had ossified into a different problem: how to maintain the system's legitimacy while quietly adapting it to changing military and economic conditions.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.68 at interval end) reflects that the constraint does extract: it prevents citizens and helots from revising or challenging the foundational law, keeping them bound to roles determined by ephoral interpretation. It is not pure extraction (hence not snare-level ~0.8+) because it does coordinate genuine military advantage — the common mess and education system truly produce superior hoplite cohesion, and this is available to all citizens equally (within the citizen body). The suppression score (0.55, relatively moderate) reflects that the immutability claim prevents direct challenge, but the constraint itself is not brutally coercive once internalized — citizens embrace the Lycurgan identity. Theater_ratio rises sharply (0.35 → 0.72 across 400 years), tracking the growing gap between the immutability claim and the accumulating evidence of adaptation: early in the system, ephoral reinterpretation genuinely serves functional adaptation; by the late Classical period, the facade becomes visibly performative as rhetra amendments pile up and external historians record the drift. Accessibility_collapse is moderate-to-high (0.62 at end) because exit alternatives for citizens are genuinely scarce — Spartan identity is fused with the system — but they are not zero: Spartans could theoretically emigrate or defy the law at personal cost. Resistance is moderate (0.58) because some citizens and helots do resist, though their resistance is suppressed or channeled into acceptable outlets (the ephors allow limited adaptation precisely to absorb pressure). The level-resolved grid shows that suppression is HIGHEST at the class level (helot population), moderate-high at the organizational level (military), and lowest at the structural level (the state's rhetorical claim to immutability is rarely challenged directly). Resistance is also high at class and organizational levels (where adaptation pressure builds) but low at the structural level (the fundamental immutability claim persists despite erosion).
 *
 * PERSPECTIVAL GAP:
 *   From the EPHORAL seat, this is rope-level coordination with prudent interpretation — the immutability claim is a feature, not a bug, because it shields flexible adaptation from destabilizing public debate. From the CITIZEN seat, the arrangement is a lock-in: they cannot revise the law they are bound by, yet they see it being altered secretly, which generates resentment. From the HELOT seat, this is pure extraction — they have no access to the citizen-level coordination benefit and no ability to challenge their bondage. The engine computes these per-seat classifications from the directional data: the ephors (beneficiary + powerful + arbitrage-adjacent through reinterpretation) get low d; citizens (constrained payers with identity-lock) get high d; helots (powerless, trapped victims) get very high d. Each seat perceives a different constraint type emerging from identical structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Ephoral oligarchy: d ≈ 0.15 (beneficiary, institutional power, can exit through interpretation without public rupture). Spartan citizen body: d ≈ 0.68 (payer and identity-locked, cannot exit without ceasing to be Spartan, but do receive coordination benefit — sits between pure target and symmetric). Helot population: d ≈ 0.95 (pure target, powerless, trapped, no benefit claim — the extraction is unambiguous from their perspective). Military establishment: d ≈ 0.35 (beneficiary of coordination, but constrained by ephoral oversight — moderate position). The coercion grid shows suppression is highest at the class level (helots systematically suppressed, krypteia expeditions), moderate at organizational and individual levels (where adaptation and citizen identity provide absorbing mechanisms). This directionality structure — concentrated extraction from helots, moderate extraction from citizens via identity-lock, benefits to ephors and military — is what makes this tangled_rope not pure_rope (which would have symmetric or beneficial d for all parties) and not snare (which would have high extraction from all).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy differently than the demographic_trap_reading would. That reading (a sibling) claims the laws ARE rigid and the rigidity CAUSED demographic collapse (dead founding problem, brittle system). This reading (adaptive_fiction) claims the system remained FUNCTIONALLY FLEXIBLE through interpretation, but the immutability fiction prevented TRANSPARENT renegotiation of the helot question, leading to demographic collapse of the helot population and (indirectly) citizen ranks as the labor base degraded. The difference is critical: is Spartan decline a failure of rigidity or a failure of extractive adaptation? This reading attributes it to the latter — the ephors could adapt military and citizen-level rules but could NOT, under the immutability fiction, openly renegotiate helot status, so extraction from the helot base remained constant while productivity fell. The founding problem (preventing wealth stratification among citizens) remained live and was SOLVED by adaptation; but a NEW problem (how to sustain the system without demographic decline) was CREATED by the immutability fiction, which locked in helot extraction even as the labor base became insufficient. Mandatrophy is NOT resolved in this reading; rather, it migrated from one domain (citizen equality, successfully maintained) to another (labor sustainability, failed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ephoral_interpretation_reconstruction,
    'How much of ephoral ''interpretation'' was genuinely novel adaptation versus rhetorical relabeling of existing practice? Did interpretation enable real change or merely preserve change that had already occurred?',
    'Detailed textual and archaeological analysis of the rhetra amendments, comparison with contemporaneous records of military tactics and helot policy, interviews with Spartan institutions (via later historians like Xenophon) about HOW they justified specific adaptations.',
    'If interpretation enabled real adaptation, the rope-level coordination function is genuine and the tangled_rope classification holds. If interpretation was merely rhetorical cover for drift that had already happened, the constraint is more purely extractive (approaches snare territory) and the ''noble lie'' aspect strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ephoral_interpretation_reconstruction, empirical, 'Whether ephoral interpretation was adaptive mechanism or post-hoc justification for covert drift').

omega_variable(
    immutability_fiction_intentionality,
    'Was the immutability claim deliberately authored as a strategic fiction by Lycurgus or the early ephors, or did it emerge organically as Spartan society retrospectively codified its practices?',
    'Comparison of early sources (Plutarch''s biographical account, Xenophon''s Constitution) with later historiography; analysis of when the immutability claim first appears in the record versus when adaptations are documented; evidence of deliberate choice versus institutional drift into the claim.',
    'Intentional fiction strengthens the ''noble lie'' reading and suggests sophisticated institutional strategy. Organic emergence suggests the immutability claim is a retrospective rationalization, which would reframe the constraint toward mandatrophy and institutional inertia rather than deliberate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_fiction_intentionality, empirical, 'Whether Lycurgan immutability was deliberately constructed as strategic fiction or emerged organically').

omega_variable(
    rope_vs_extraction_boundary,
    'Is the Lycurgan coordination function genuinely separable from the extraction of discretionary power by ephors? Could the same military coordination be achieved under transparent, revisable law?',
    'Comparative analysis of other Greek military systems (Theban Sacred Band, Athenian hoplite army, later Macedonian phalanx) that achieved similar military cohesion without immutability claims. Analysis of whether Spartan military effectiveness degraded when immutability claims began to erode.',
    'If coordination and immutability are separable, the constraint should be classified as ROPE with a separate extraction constraint layered on top. If inseparable, the tangled_rope classification is accurate. This omega addresses whether the noble lie is necessary for coordination or merely convenient for power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rope_vs_extraction_boundary, conceptual, 'Whether Lycurgan military coordination depends structurally on immutability fiction or merely benefits from it').

omega_variable(
    helot_demographic_decline_causation,
    'Did the helot population decline because of the immutability fiction (preventing open renegotiation of helot status, keeping extraction constant), because of the rope-level coordination function (demanding constant military service reducing reproductive capacity), or from external factors (wars, disease, emigration)?',
    'Population-level analysis of helot numbers across centuries, comparison with periods of higher/lower adaptation flexibility, analysis of helot family structures and reproductive patterns under different ephoral regimes, comparison with helot-equivalent populations in other Greek poleis.',
    'If the immutability fiction is the primary cause, it is a critical load-bearing element of the extraction and must be maintained for the system''s continuity — strengthens the tangled_rope reading. If the rope-level coordination function is the cause, the immutability fiction is secondary. If external factors dominate, the constraint is less extractively intentional than the story suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(helot_demographic_decline_causation, empirical, 'Whether helot demographic decline was caused by immutability lock-in, coordination demands, or external factors').

omega_variable(
    sibling_reading_foreclosure,
    'Do the three sibling readings (adaptive_fiction vs. demographic_trap vs. sacral_fidelity) genuinely coexist as interpretations of the same constraint, or does evidence about adaptation (if adaptation occurred) logically foreclose one or more of the alternative readings?',
    'Determine whether the ephoral interpretation actually happened (resolves adaptive_fiction vs. demographic_trap foreclosure). Determine whether the immutability claim was ever believed by Spartans themselves or only by external historians (bears on sacral_fidelity vs. adaptive_fiction coexistence).',
    'If adaptation is proven, demographic_trap reading is substantially weakened but not necessarily foreclosed (adaptation could have been insufficient). If immutability was genuinely believed, sacral_fidelity gains traction and adaptive_fiction becomes a cynical reading of sincere belief. This determines the reading_relations topology: whether any foreclosure relations exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the three kernel readings coexist or whether evidence of adaptation forecloses some readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(lycu_tr_t0, observed).
narrative_ontology:measurement(lycu_tr_t50, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(lycu_tr_t50, observed).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 100, 0.51).
narrative_ontology:measurement_basis(lycu_tr_t100, observed).
narrative_ontology:measurement(lycu_tr_t150, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 150, 0.58).
narrative_ontology:measurement_basis(lycu_tr_t150, observed).
narrative_ontology:measurement(lycu_tr_t250, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 250, 0.68).
narrative_ontology:measurement_basis(lycu_tr_t250, observed).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 400, 0.72).
narrative_ontology:measurement_basis(lycu_tr_t400, observed).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(lycu_be_t0, observed).
narrative_ontology:measurement(lycu_be_t50, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(lycu_be_t50, observed).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(lycu_be_t100, observed).
narrative_ontology:measurement(lycu_be_t150, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 150, 0.62).
narrative_ontology:measurement_basis(lycu_be_t150, observed).
narrative_ontology:measurement(lycu_be_t250, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 250, 0.66).
narrative_ontology:measurement_basis(lycu_be_t250, observed).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 400, 0.68).
narrative_ontology:measurement_basis(lycu_be_t400, observed).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(lycu_su_t0, observed).
narrative_ontology:measurement(lycu_su_t50, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement_basis(lycu_su_t50, observed).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 100, 0.53).
narrative_ontology:measurement_basis(lycu_su_t100, observed).
narrative_ontology:measurement(lycu_su_t150, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 150, 0.54).
narrative_ontology:measurement_basis(lycu_su_t150, observed).
narrative_ontology:measurement(lycu_su_t250, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 250, 0.55).
narrative_ontology:measurement_basis(lycu_su_t250, observed).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 400, 0.55).
narrative_ontology:measurement_basis(lycu_su_t400, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=400
narrative_ontology:measurement(lycu_grid_01, lycurgan_laws__adaptive_fiction_reading, accessibility_collapse(class), 0, 0.52).
narrative_ontology:measurement(lycu_grid_02, lycurgan_laws__adaptive_fiction_reading, accessibility_collapse(class), 400, 0.61).
narrative_ontology:measurement(lycu_grid_03, lycurgan_laws__adaptive_fiction_reading, accessibility_collapse(individual), 0, 0.58).
narrative_ontology:measurement(lycu_grid_04, lycurgan_laws__adaptive_fiction_reading, accessibility_collapse(individual), 400, 0.64).
narrative_ontology:measurement(lycu_grid_05, lycurgan_laws__adaptive_fiction_reading, accessibility_collapse(organizational), 0, 0.71).
narrative_ontology:measurement(lycu_grid_06, lycurgan_laws__adaptive_fiction_reading, accessibility_collapse(organizational), 400, 0.72).
narrative_ontology:measurement(lycu_grid_07, lycurgan_laws__adaptive_fiction_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(lycu_grid_08, lycurgan_laws__adaptive_fiction_reading, accessibility_collapse(structural), 400, 0.71).
narrative_ontology:measurement(lycu_grid_09, lycurgan_laws__adaptive_fiction_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(lycu_grid_10, lycurgan_laws__adaptive_fiction_reading, resistance(class), 400, 0.61).
narrative_ontology:measurement(lycu_grid_11, lycurgan_laws__adaptive_fiction_reading, resistance(individual), 0, 0.42).
narrative_ontology:measurement(lycu_grid_12, lycurgan_laws__adaptive_fiction_reading, resistance(individual), 400, 0.51).
narrative_ontology:measurement(lycu_grid_13, lycurgan_laws__adaptive_fiction_reading, resistance(organizational), 0, 0.55).
narrative_ontology:measurement(lycu_grid_14, lycurgan_laws__adaptive_fiction_reading, resistance(organizational), 400, 0.62).
narrative_ontology:measurement(lycu_grid_15, lycurgan_laws__adaptive_fiction_reading, resistance(structural), 0, 0.28).
narrative_ontology:measurement(lycu_grid_16, lycurgan_laws__adaptive_fiction_reading, resistance(structural), 400, 0.31).
narrative_ontology:measurement(lycu_grid_17, lycurgan_laws__adaptive_fiction_reading, stakes_inflation(class), 0, 0.55).
narrative_ontology:measurement(lycu_grid_18, lycurgan_laws__adaptive_fiction_reading, stakes_inflation(class), 400, 0.64).
narrative_ontology:measurement(lycu_grid_19, lycurgan_laws__adaptive_fiction_reading, stakes_inflation(individual), 0, 0.62).
narrative_ontology:measurement(lycu_grid_20, lycurgan_laws__adaptive_fiction_reading, stakes_inflation(individual), 400, 0.68).
narrative_ontology:measurement(lycu_grid_21, lycurgan_laws__adaptive_fiction_reading, stakes_inflation(organizational), 0, 0.71).
narrative_ontology:measurement(lycu_grid_22, lycurgan_laws__adaptive_fiction_reading, stakes_inflation(organizational), 400, 0.75).
narrative_ontology:measurement(lycu_grid_23, lycurgan_laws__adaptive_fiction_reading, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(lycu_grid_24, lycurgan_laws__adaptive_fiction_reading, stakes_inflation(structural), 400, 0.62).
narrative_ontology:measurement(lycu_grid_25, lycurgan_laws__adaptive_fiction_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(lycu_grid_26, lycurgan_laws__adaptive_fiction_reading, suppression(class), 400, 0.72).
narrative_ontology:measurement(lycu_grid_27, lycurgan_laws__adaptive_fiction_reading, suppression(individual), 0, 0.48).
narrative_ontology:measurement(lycu_grid_28, lycurgan_laws__adaptive_fiction_reading, suppression(individual), 400, 0.51).
narrative_ontology:measurement(lycu_grid_29, lycurgan_laws__adaptive_fiction_reading, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(lycu_grid_30, lycurgan_laws__adaptive_fiction_reading, suppression(organizational), 400, 0.61).
narrative_ontology:measurement(lycu_grid_31, lycurgan_laws__adaptive_fiction_reading, suppression(structural), 0, 0.35).
narrative_ontology:measurement(lycu_grid_32, lycurgan_laws__adaptive_fiction_reading, suppression(structural), 400, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__adaptive_fiction_reading, 0.12).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).

% DUAL FORMULATION NOTE:
% The adaptive_fiction_reading decomposes from the contested LYCURGAN_LAWS kernel. Three structurally distinct constraint stories instantiate three readings: this story (adaptive_fiction, ε=0.68, tangled_rope) claims the immutability is a strategic fiction masking flexible adaptation; the demographic_trap_reading (ε≈0.72, snare) claims the laws ARE rigid and rigidity caused collapse; the sacral_fidelity_reading (ε≈0.35, mountain) claims the laws are genuinely sacred and unchangeable. Each reading has a different ε, different beneficiary/victim structure, and different classification. They are linked by network.affects_constraints because they share a kernel and their empirical validity constrains each other: evidence that adaptation occurred (supporting adaptive_fiction) would substantially weaken demographic_trap but would not foreclose sacral_fidelity (which could claim the adaptation itself was violation). The three readings coexist in historical scholarship; no framework could hold all three simultaneously, but neither does one logically foreclose the others entirely — each rests on different empirical claims and normative premises about what counts as violation of sacred law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__adaptive_fiction_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
