% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__parmenidean_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_mathematical_status__parmenidean_rejection, []).

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
 *   constraint_id: zero_mathematical_status__parmenidean_rejection
 *   human_readable: Parmenidean Rejection of Zero as Number
 *   domain: philosophical/mathematical
 *
 * SUMMARY:
 *   This constraint embodies the Parmenidean rejection of zero as
 *   ontologically incoherent — nothing cannot exist, and therefore zero
 *   cannot be admitted to the domain of number. The constraint operates at
 *   the intersection of metaphysical principle (being-from-being, changeless
 *   form) and mathematical practice (positional notation, efficient
 *   calculation). Institutional scholasticism enforces the constraint by
 *   excluding Indian mathematics, suppressing zero-like notation in
 *   authoritative texts, and sanctioning practitioners who treat zero as a
 *   number. Yet practitioners in astronomy, commerce, and engineering
 *   discover that zero — or zero-substitutes — enable calculations that the
 *   constraint forbids, creating a sustained gap between metaphysical
 *   principle and practical necessity. The beneficiary is not a human agent
 *   but a metaphysical framework; the victims are computational practitioners
 *   forced to invent workarounds. This is a Tangled Rope: genuine
 *   coordination function (mathematical coherence around Parmenidean
 *   principle) yoked to asymmetric extraction (practitioners bear the cost of
 *   forbidden-tool inefficiency). The claim and metrics are independent: the
 *   constraint is CLAIMED as tangled_rope on the grounds that it coordinates
 *   metaphysical integrity while extracting from practitioners, and the
 *   authored metrics describe extraction increasing over time as more
 *   practitioners discover zero's utility and the suppression must intensify
 *   to maintain the boundary.
 *
 * KEY AGENTS:
 *   - Parmenidean metaphysics framework: benefits from exclusion of zero, maintains coherence of being-from-being principle
 *   - Institutional scholasticism: enforces the constraint through curriculum control and sanctioning
 *   - Computational practitioners (astronomers, merchants, mathematicians): bear the cost of forbidden tools, forced into workarounds and dual systems
 *   - Indian/Hindu mathematical tradition: structurally excluded, holds zero-arithmetic but is outside the constraint's institutional reach
 *   - Brahmagupta and successors: attest from outside the framework that zero is operationally coherent and mathematically fruitful
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__parmenidean_rejection, 0.68).
domain_priors:suppression_score(zero_mathematical_status__parmenidean_rejection, 0.71).
domain_priors:theater_ratio(zero_mathematical_status__parmenidean_rejection, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, extractiveness, 0.68).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__parmenidean_rejection, tangled_rope).
narrative_ontology:human_readable(zero_mathematical_status__parmenidean_rejection, "Parmenidean Rejection of Zero as Number").
narrative_ontology:topic_domain(zero_mathematical_status__parmenidean_rejection, "philosophical/mathematical").

domain_priors:requires_active_enforcement(zero_mathematical_status__parmenidean_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__parmenidean_rejection, '2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9').
narrative_ontology:cs_kernel_codification('2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9', formalized).
narrative_ontology:cs_authority_grounding('2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9', extraction).
narrative_ontology:cs_interpretation_layer_present('2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9').
narrative_ontology:cs_reading_relation('2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9', zero_mathematical_status__number_reading, forecloses).
narrative_ontology:cs_reading_relation('2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9', zero_mathematical_status__placeholder_reading, coexists_with).
narrative_ontology:cs_axiom('2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9', foundational, being_cannot_arise_from_nonbeing).
narrative_ontology:cs_axiom_status(being_cannot_arise_from_nonbeing, holdable).
narrative_ontology:cs_axiom_grounding('2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9', being_cannot_arise_from_nonbeing, deontological).
narrative_ontology:cs_axiom('2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9', foundational, zero_violates_ontological_necessity).
narrative_ontology:cs_axiom_status(zero_violates_ontological_necessity, holdable).
narrative_ontology:cs_axiom_grounding('2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9', zero_violates_ontological_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9', parmenidean_permanent_being).
narrative_ontology:cs_drift_state('2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9', high_medieval_period, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a8c9b7a-d4c5-4d7b-8f4c-5066c9f96bc9', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, parmenidean_metaphysics_framework).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, computational_practitioners).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, astronomical_calculators).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, mercantile_accountants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, astronomical_calculators).
narrative_ontology:constraint_vindicates(zero_mathematical_status__parmenidean_rejection, being_cannot_come_from_nonbeing).
narrative_ontology:constraint_vindicates(zero_mathematical_status__parmenidean_rejection, ontological_continuity_of_number).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A philosophical framework that requires being to be eternal, unchanging, and generative only from being (not from non-being). Zero represents a logical threat to this framework: it is nothing (non-being), yet it would be given number-status, implying being-from-nothingness. The framework benefits from the constraint by excluding zero and maintaining internal coherence. It is not a human agent but a doctrine that collects legitimacy from the constraint's persistence.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, parmenidean_metaphysics_framework, beneficiary,
    institutional, civilizational, arbitrage, universal).
narrative_ontology:stakeholder_non_agent(zero_mathematical_status__parmenidean_rejection, parmenidean_metaphysics_framework).

% The monastic schools, cathedral schools, and early universities that control the mathematical curriculum and authorize which texts are taught. They enforce the constraint by excluding Indian mathematical texts, suppressing zero-notation in official teaching, and sanctioning scholars who treat zero as a number. They derive institutional authority from their role as defenders of Parmenidean-Christian metaphysics and keepers of authoritative (Greek and Latin) mathematical texts.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, institutional_scholasticism, agenda_setter,
    institutional, generational, arbitrage, regional).

% Mathematicians, astronomers, and engineers who need efficient systems for calculation and large-number representation. They experience the constraint as a cost: the rejection of zero forces them to use cumbersome notation (Roman numerals, finger-calculation, tally-marks) or to develop zero-substitutes that are forbidden to acknowledge explicitly. They cannot openly violate the constraint without risking institutional sanction, yet they pay the cost of its enforcement through increased computational labor.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, computational_practitioners, payer,
    organized, biographical, constrained, global).

% Practitioners of celestial mechanics, astrology, and navigation who need precision in very large numbers and positional-notation efficiency. They pay the constraint's cost through elaborate workarounds: zero-like symbols officially not called zero, invented notation systems, or private adoption of Indian mathematics while maintaining public allegiance to Parmenidean bounds. They also benefit from some coordination: the constraint keeps authoritative teaching coherent and prevents mathematical fragmentation into incompatible systems.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, astronomical_calculators, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__parmenidean_rejection, astronomical_calculators, beneficiary).

% Merchants, record-keepers, and administrators whose practical accounting methods benefit from place-value notation and zero-like symbols. The constraint limits their adoption of Hindu-Arabic numerals and forces them to maintain dual systems: public allegiance to Christian arithmetic and zero-rejection, private use of methods that treat zero functionally (even if philosophically unacknowledged). Their exit options are regional rather than global because local trading networks can enforce notation standards, but they face pressure from long-distance merchants who have access to Indian methods.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, mercantile_accountants, payer,
    moderate, biographical, constrained, regional).

% The mathematical tradition (Brahmagupta, Aryabhata, and successors) that developed zero as a number with defined arithmetic. They are structurally excluded from participation in the constraint's justification — their alternative framework is not admitted to the authorized curriculum or canonical texts. Their mathematical work demonstrates zero's coherence and utility but cannot be officially acknowledged without violating the Parmenidean framework that institutional scholasticism maintains.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, indian_hindu_mathematicians, excluded,
    powerful, generational, trapped, global).

% Mathematicians of the High Middle Ages and Renaissance (Fibonacci, Jordanus, and others) who encounter zero through Latin translations of Indian and Islamic mathematics and begin to develop algebraic systems that require zero. They are excluded from legitimate endorsement of zero in university teaching until institutional structures shift, forcing them to work privately or to develop arguments that reframe zero as coherent within (rather than opposed to) Christian metaphysics.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, later_european_mathematicians, excluded,
    moderate, biographical, trapped, regional).

% The historical and analytical perspective assessing the constraint structure from outside. From this seat, the constraint coordinates a metaphysical framework (Parmenidean being) at the cost of computational inefficiency for practitioners who would benefit from zero. The constraint's persistence depends on institutional enforcement against empirical and mathematical pressure — it is a Tangled Rope because it does coordinate something real (metaphysical coherence) while extracting from practitioners who bear the cost of that coordination.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__parmenidean_rejection, parmenidean_metaphysics_framework).
narrative_ontology:fixing_cost_class(zero_mathematical_status__parmenidean_rejection, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the coherence of Parmenidean metaphysics by maintaining the logical principle that being cannot arise from non-being; unifies mathematics around changeless, eternal forms; prevents the ontological scandal of a number that means nothing.
% TRANSFER_FUNCTION: Transfers the labor cost of calculation from institutional mathematics (which can defer to metaphysical authority) to practical practitioners (who must invent workarounds, use dual notation systems, or violate the constraint in private). Also transfers epistemic authority from computational evidence (zero's practical utility) to metaphysical principle (zero's logical impossibility).
% ABSENT_VOICES: Practitioners of Indian mathematics, commercial calculators using Hindu-Arabic numerals in private, future mathematicians who will develop zero's arithmetic — all are structurally excluded from legitimate discourse. Their silence is enforced by institutional control of authoritative sources and social/professional sanctions against heterodox practice.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, computational systems would immediately adopt efficient positional notation with zero, astronomical and mercantile calculation would become faster and more accurate, and Parmenidean metaphysics would lose a key enforcement anchor — the mathematical practice world would reorganize around the tools it has been forced to suppress.
% FOUNDING_PROBLEM: How can number participate in changeless being if zero, meaninglessness, and nothingness are admitted to the domain? How can something arise from nothing, or be represented by nothing?
% FOUNDING_PROBLEM_CORROBORATION: Parmenidean philosophers and medieval Christian scholastics attest the founding problem remains live: zero threatens the logical integrity of being-from-being and the divine creative principle. Brahmagupta, Al-Khwarizmi, and Renaissance mathematicians attest the problem is pseudo-solved through reframing: zero is not nothing but a place-holder, a number with consistent arithmetic, a tool of representation — these testimonies come from outside the metaphysical framework that benefits from the constraint and directly contradict its necessity.
narrative_ontology:disappearance_verdict(zero_mathematical_status__parmenidean_rejection, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__parmenidean_rejection, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__parmenidean_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_mathematical_status__parmenidean_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__parmenidean_rejection, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__parmenidean_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_mathematical_status__parmenidean_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zero_mathematical_status__parmenidean_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) and rising: the constraint costs practitioners tangible labor (calculation inefficiency, maintenance of workarounds) and yields clear benefit to the metaphysical framework (preservation of being-from-being principle). Suppression is high (0.71) because enforcement requires active exclusion of competing texts, institutional sanctioning, and control over which mathematical traditions are authorized in teaching. Theater is moderate (0.42): some of the enforcement activity is genuine philosophical defense of the Parmenidean framework, but an increasing share is institutional gatekeeping designed to prevent practitioners from encountering zero-arithmetic. The measurement series tracks rising suppression_requirement over the interval (as more practitioners discover zero's utility and must be more actively prevented from adopting it) and flat but elevated extractiveness (practitioners will always need to calculate, so the extraction floor is stable, but the suppression cost rises).
 *
 * PERSPECTIVAL GAP:
 *   From the Parmenidean-metaphysics perspective, the constraint is genuine coordination: mathematics must cohere around the principle of being-from-being, and zero threatens that coherence. From the computational-practitioner perspective, the constraint is pure extraction: the metaphysical principle generates no benefit for their work, only costs. From the institutional-scholasticism perspective, the constraint is coordination (maintains doctrinal coherence, authority over mathematical curriculum). From the later-mathematician perspective (Brahmagupta, Al-Khwarizmi), the constraint is a false negative: it rejects a genuine mathematical object whose coherence and utility become discoverable once suppression is removed. The engine should compute these seats differently: the metaphysical-framework seat sees no extraction (or sees only the cost of maintaining coherence), while practitioner seats experience high extraction and high suppression. The perspectival gap is the gap between metaphysical-necessity readings and pragmatic-utility readings — an unbridged divide about what mathematics is FOR.
 *
 * DIRECTIONALITY LOGIC:
 *   The Parmenidean framework is the beneficiary (d near 0.0): the constraint exists to preserve its integrity; it collects legitimacy and explanatory power. Institutional scholasticism sits as agenda_setter and partial beneficiary (d around 0.2-0.3): it enforces the constraint and derives authority from its defense. Computational practitioners are the victims (d near 1.0): they pay in labor inefficiency and are forced to suppress their own discoveries. The directionality should show institutional scholasticism capturing extraction (the framework's benefit flows through institutional control), practitioners experiencing full target-hood (trapped, constrained exit, high extraction), and the metaphysical framework itself as the non-agent beneficiary (it collects coherence without running anything).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how can zero participate in being-from-being?) is contested by the six-questions verdict: Parmenidean philosophers attest it is live and unsolved; Brahmagupta and his successors attest it is a pseudo-problem, solved by reframing zero as number rather than nothingness. The constraint persists not because the founding problem remains unsolved but because institutional enforcement prevents the reframing from spreading into the authorized mathematical curriculum. This is a mandatrophy signal: the constraint's original function (coordinate mathematics around metaphysical principle) is atrophied by the emergence of alternative mathematical frameworks (Indian arithmetic) that solve the founding problem without invoking Parmenidean principle. Yet institutional enforcement keeps the constraint alive through curriculum control and exclusion, making it a candidate Piton — kept functioning theatrically (philosophical defense of the Parmenidean principle) while the real mathematical work happens in suppressed frameworks. The measured rising suppression_requirement supports this reading: as the founding problem becomes more clearly solved elsewhere, more institutional work is required to maintain suppression. However, the constraint is CLAIMED as Tangled Rope, not Piton, because the coordination function (mathematical coherence around Parmenidean principle) is still genuinely valued within scholastic philosophy, even though it impedes computational practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parmenidean_vs_mathematical_authority,
    'Is the constraint''s persistence grounded in the logical incoherence of zero, or in the institutional power of Parmenidean metaphysics to exclude competing mathematical frameworks?',
    'Historical analysis of mathematical texts that treat zero functionally despite metaphysical prohibition; examination of whether practitioners privately use zero-like systems while maintaining public rejection; study of the transition moment when institutional authority shifts from metaphysical to pragmatic grounds.',
    'If grounded primarily in metaphysics, the constraint is justified within its own framework until metaphysics is abandoned; if grounded primarily in institutional power, the constraint is a snare whose persistence depends on suppressing alternatives, not on logical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parmenidean_vs_mathematical_authority, conceptual, 'Whether the constraint''s coherence is metaphysical or institutional.').

omega_variable(
    zero_substitute_functionality,
    'Do the zero-substitutes and implicit positional understandings that practitioners develop functionally constitute zero-behavior without zero-vocabulary, or do they require distinct mathematical properties?',
    'Reconstruction of medieval and ancient computational methods to determine whether zero-like symbols operated under rules equivalent to Brahmaguptian zero arithmetic; examination of whether the suppression of zero-vocabulary prevented the development of zero-theory.',
    'If substitutes are functionally equivalent, the constraint''s effectiveness is purely linguistic/institutional — zero is already present in practice but denied in vocabulary. If substitutes require different properties, zero-vocabulary suppression may have prevented discovery of zero''s full arithmetic properties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(zero_substitute_functionality, empirical, 'Whether suppression of zero-vocabulary blocked the development of zero-theory.').

omega_variable(
    kernel_contest_reading_independence,
    'Are the parmenidean_rejection, number_reading, and placeholder_reading three genuinely distinct constraint families, or are they three different vocabularies for a single underlying mathematical fact?',
    'Examination of whether computational systems can satisfy the requirements of both readings simultaneously (can you have a zero that is both ''not a number'' and ''operationally consistent''?), or whether adoption of one reading logically requires abandonment of the other.',
    'If they are genuinely distinct constraints (different epsilon values, different victim sets, different enforcement mechanisms), the kernel contest is structural. If they are one mathematical fact with three competing vocabularies, the readings are not independent constraints but surface variations of a single underlying discipline-boundary conflict.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_reading_independence, conceptual, 'Whether the kernel readings are independent constraints or surface vocabulary for a single mathematical fact.').

omega_variable(
    institutional_suppression_vs_philosophical_disagreement,
    'Would Parmenidean philosophers voluntarily reject zero if institutional enforcement were removed, or is the constraint maintained primarily through institutional suppression of competing mathematical traditions?',
    'Historical study of non-institutionally-enforced mathematical discussions (private correspondence, texts written outside curriculum control, natural-philosophy circles) to determine whether philosophical rejection of zero persists independently of institutional enforcement.',
    'If philosophical rejection persists independently, the constraint is a genuine rope with Parmenidean framework as the coordinating beneficiary. If philosophical rejection requires institutional enforcement, the constraint is a snare maintained by institutional power over a disputed mathematical claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_suppression_vs_philosophical_disagreement, empirical, 'Whether the constraint would persist without institutional enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__parmenidean_rejection, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_mathematical_status__parmenidean_rejection, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(zero_tr_t0, observed).
narrative_ontology:measurement(zero_tr_t15, zero_mathematical_status__parmenidean_rejection, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(zero_tr_t15, observed).
narrative_ontology:measurement(zero_tr_t30, zero_mathematical_status__parmenidean_rejection, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(zero_tr_t30, observed).
narrative_ontology:measurement(zero_tr_t60, zero_mathematical_status__parmenidean_rejection, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(zero_tr_t60, observed).
narrative_ontology:measurement(zero_tr_t90, zero_mathematical_status__parmenidean_rejection, theater_ratio, 90, 0.42).
narrative_ontology:measurement_basis(zero_tr_t90, observed).
narrative_ontology:measurement(zero_tr_t120, zero_mathematical_status__parmenidean_rejection, theater_ratio, 120, 0.42).
narrative_ontology:measurement_basis(zero_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(zero_be_t0, observed).
narrative_ontology:measurement(zero_be_t15, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(zero_be_t15, observed).
narrative_ontology:measurement(zero_be_t30, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(zero_be_t30, observed).
narrative_ontology:measurement(zero_be_t60, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 60, 0.67).
narrative_ontology:measurement_basis(zero_be_t60, observed).
narrative_ontology:measurement(zero_be_t90, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 90, 0.68).
narrative_ontology:measurement_basis(zero_be_t90, observed).
narrative_ontology:measurement(zero_be_t120, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 120, 0.68).
narrative_ontology:measurement_basis(zero_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(zero_su_t0, observed).
narrative_ontology:measurement(zero_su_t15, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(zero_su_t15, observed).
narrative_ontology:measurement(zero_su_t30, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(zero_su_t30, observed).
narrative_ontology:measurement(zero_su_t60, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 60, 0.69).
narrative_ontology:measurement_basis(zero_su_t60, observed).
narrative_ontology:measurement(zero_su_t90, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 90, 0.71).
narrative_ontology:measurement_basis(zero_su_t90, observed).
narrative_ontology:measurement(zero_su_t120, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 120, 0.71).
narrative_ontology:measurement_basis(zero_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__parmenidean_rejection, identity_coordination).
narrative_ontology:boltzmann_floor_override(zero_mathematical_status__parmenidean_rejection, 0.14).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__placeholder_reading).

% DUAL FORMULATION NOTE:
% The zero_mathematical_status kernel admits three independent readings with distinct ε values and victim sets. parmenidean_rejection (this story) treats zero as ontologically incoherent; number_reading (sibling) treats zero as a number with consistent arithmetic; placeholder_reading (sibling) treats zero as a notational device. The three readings are not disagreements about a single constraint — they are three structurally distinct constraints instantiated by different readings of the same ambiguous kernel. Each has its own beneficiary/victim structure, enforcement mechanism, and epistemic grounds. parmenidean_rejection forecloses (in the logical sense) the number_reading within a single metaphysical framework: if zero is ontologically impossible, it cannot also be a coherent number. But the readings coexist historically: different institutional and intellectual communities held different readings simultaneously. The placeholder_reading coexists_with both others, offering a middle path that acknowledges zero's utility while denying full number-status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zero_mathematical_status__parmenidean_rejection, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
