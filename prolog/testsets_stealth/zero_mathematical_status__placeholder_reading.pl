% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__placeholder_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: zero_mathematical_status__placeholder_reading
 *   human_readable: Zero as Placeholder: Notation Without Number Status
 *   domain: history/philosophy of mathematics
 *
 * SUMMARY:
 *   The placeholder reading of the zero_mathematical_status kernel holds that
 *   the empty-place symbol of positional notation is a sign, not a number: it
 *   may occupy places in written numerals, but arithmetic operations are
 *   defined over quantities, and none are defined on it. As a standing
 *   arrangement of mathematical practice, authored over the Latin interval
 *   1202-1600 (from Fibonacci's Liber Abaci to the eve of symbolic algebra's
 *   full operational treatment of the symbol), it delivers the written
 *   algorithms to commerce while denying closure to algebra. The settlement
 *   is one of three live readings of the kernel: the number reading
 *   (operations defined since Brahmagupta, 628 CE) and the Parmenidean
 *   rejection (nothing cannot be admitted at all) are sibling constraints,
 *   linked through the network. Claim and metrics are independent authored
 *   facts: the claimed type (tangled_rope) is asserted from the structural
 *   facts, a genuine coordination function plus an asymmetric closure cost,
 *   actively enforced; the metrics are authored from the arrangement's
 *   observed operation and are not tuned to the claim or to any predicted
 *   engine output.
 *
 * KEY AGENTS:
 *   - doctrinal_arithmetic_authorities: agenda-setter (institutional / identity_locked) - administers the sign/number boundary; collects adjudicative standing from it
 *   - merchant_computist_guilds: primary beneficiary (organized / constrained) - collects the notation's calculational efficiency
 *   - abacus_school_masters: beneficiary and practical propagator (organized / mobile) - teaches the algorithms, quietly computes past the boundary
 *   - working_algebraists: primary payer (moderate / constrained) - bears the denied closure; develops workarounds
 *   - emerging_natural_philosophers: downstream payer (moderate / trapped) - needs the symbol to anchor scales and absorb completed subtractions
 *   - parmenidean_natural_philosophers: excluded ontological opposition (moderate / identity_locked) - contests even notational admission
 *   - indian_arithmetic_tradition: excluded corroborating alternative (organized / mobile) - holds defined operations; outside the Latin conversation
 *   - conceptual_historians: analytical observer (analytical / analytical) - sees the full settlement and its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__placeholder_reading, 0.5).
domain_priors:suppression_score(zero_mathematical_status__placeholder_reading, 0.36).
domain_priors:theater_ratio(zero_mathematical_status__placeholder_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__placeholder_reading, tangled_rope).
narrative_ontology:human_readable(zero_mathematical_status__placeholder_reading, "Zero as Placeholder: Notation Without Number Status").
narrative_ontology:topic_domain(zero_mathematical_status__placeholder_reading, "history/philosophy of mathematics").

domain_priors:requires_active_enforcement(zero_mathematical_status__placeholder_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__placeholder_reading, 'ae729d4a-d716-4de4-9c6f-d37289c8f9cb').
narrative_ontology:cs_kernel_codification('ae729d4a-d716-4de4-9c6f-d37289c8f9cb', formalized).
narrative_ontology:cs_authority_grounding('ae729d4a-d716-4de4-9c6f-d37289c8f9cb', lineage).
narrative_ontology:cs_interpretation_layer_present('ae729d4a-d716-4de4-9c6f-d37289c8f9cb').
narrative_ontology:cs_reading_relation('ae729d4a-d716-4de4-9c6f-d37289c8f9cb', zero_mathematical_status__parmenidean_rejection, coexists_with).
narrative_ontology:cs_reading_relation('ae729d4a-d716-4de4-9c6f-d37289c8f9cb', zero_mathematical_status__number_reading, forecloses).
narrative_ontology:cs_axiom('ae729d4a-d716-4de4-9c6f-d37289c8f9cb', foundational, empty_place_marker_requires_no_ontological_commitment).
narrative_ontology:cs_axiom_status(empty_place_marker_requires_no_ontological_commitment, holdable).
narrative_ontology:cs_axiom_grounding('ae729d4a-d716-4de4-9c6f-d37289c8f9cb', empty_place_marker_requires_no_ontological_commitment, instrumental).
narrative_ontology:cs_axiom('ae729d4a-d716-4de4-9c6f-d37289c8f9cb', foundational, operations_defined_only_over_quantities).
narrative_ontology:cs_axiom_status(operations_defined_only_over_quantities, holdable).
narrative_ontology:cs_axiom_grounding('ae729d4a-d716-4de4-9c6f-d37289c8f9cb', operations_defined_only_over_quantities, conventional).
narrative_ontology:cs_reference_frame('ae729d4a-d716-4de4-9c6f-d37289c8f9cb', zero_as_positional_sign_doctrine).
narrative_ontology:cs_drift_state('ae729d4a-d716-4de4-9c6f-d37289c8f9cb', early_modern_symbolic_algebra_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ae729d4a-d716-4de4-9c6f-d37289c8f9cb', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__placeholder_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, merchant_computist_guilds).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, abacus_school_masters).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, doctrinal_arithmetic_authorities).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, working_algebraists).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, emerging_natural_philosophers).
narrative_ontology:constraint_vindicates(zero_mathematical_status__placeholder_reading, ontologically_neutral_notation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run commercial bookkeeping across the Mediterranean and European trading cities. They adopted the positional numerals because written calculation with an empty-place symbol is faster and more auditable than abacus counters and roman numerals. They assert nothing about what the empty-place symbol 'is'; their interest is that the notation keeps working and stays teachable to apprentices. Reverting to abacus methods is possible but would cost speed and error-checking, so they stay with the notation and its inherited rules.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, merchant_computist_guilds, beneficiary,
    organized, biographical, constrained, continental).

% Teach the written algorithms to merchants' sons in the Italian cities. They profit from the notation's teachability and are its practical transmission line; in the classroom they compute freely with the empty-place column while passing along, without emphasis, the inherited gloss that the symbol marks 'nothing there.' Their commitment is to the practice, not the doctrine; they could teach either way and follow whichever tradition keeps students enrolled.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, abacus_school_masters, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__placeholder_reading, abacus_school_masters, agenda_setter).

% University arts faculties, arithmetic textbook traditions, and master lineages that define what counts as number. They teach that the positional symbol is a sign, not a quantity: it may occupy places in numerals, but operations are defined over quantities, so no operations are defined on it. Subtraction that lands on the symbol is incomplete, and questions about multiplying or dividing by it are category errors, not open problems. Their curricula, commentaries, and professional standing are built on administering this boundary; abandoning it would concede that centuries of instruction mistook a convention for a metaphysical insight.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, doctrinal_arithmetic_authorities, agenda_setter,
    institutional, generational, identity_locked, continental).

% Solve equations for a living: commercial partitions, inheritance divisions, surveying. Their work keeps producing situations the boundary rules out of order: equal terms canceling, equations whose solution is the empty-place symbol itself, the question of what remains when equals are subtracted. They develop workarounds (renaming quantities, avoiding the forbidden cases) but cannot leave mathematics, and the Indian and Arabic solution of defining the operations reaches them only as foreign testimony, not as licensed method.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, working_algebraists, payer,
    moderate, biographical, constrained, continental).

% Study continuous quantity: motion, ratios, astronomical tables. They increasingly need a symbol that can anchor a scale's origin and absorb the result of a completed subtraction. They arrive late in the interval and inherit the boundary as a settled rule they did not make; their work cannot proceed without some settlement of the empty-place symbol's status, and there is nowhere else for the question to go.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, emerging_natural_philosophers, payer,
    moderate, generational, trapped, continental).

% Hold that 'nothing' cannot be, and read the empty-place symbol as a Trojan horse: admit it as anything at all, even as a mere sign, and nothing has been smuggled into arithmetic. They press this objection in the schools and would ban the symbol outright rather than see it half-admitted. The conviction is constitutive of their philosophical identity; they do not pay the arrangement's costs so much as contest its right to exist.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, parmenidean_natural_philosophers, excluded,
    moderate, generational, identity_locked, continental).

% Maintains, in Sanskrit and Arabic mathematical texts, a complete working treatment in which the empty-place symbol has defined operations: add it and nothing changes, multiply by it and everything vanishes, and so on, alongside the same positional notation, for centuries before the Latin settlement. It is not part of the Latin doctrinal conversation; its texts circulate through translation channels that the Latin authorities treat as sources of calculation tricks, not of definitions. It has no stake in the Latin arrangement and can wait it out.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, indian_arithmetic_tradition, excluded,
    organized, generational, mobile, continental).

% Reconstruct the whole settlement from outside: the notational problem, the ontological worry, the Indian precedent, the commercial adoption, the algebraic friction, and the eventual dissolution of the boundary. They collect testimony from every seat and hold no position inside the arrangement.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, conceptual_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__placeholder_reading, doctrinal_arithmetic_authorities).
narrative_ontology:fixing_cost_class(zero_mathematical_status__placeholder_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the notational problem of positional systems: a written place-value scheme needs a symbol for the empty column, and one fixed symbol for that role makes the written algorithms (column addition, borrowing, long multiplication) uniform, teachable, and auditable across schools and trading cities.
% TRANSFER_FUNCTION: Moves calculational efficiency to every user of written positional arithmetic, and moves the cost of the boundary, denied arithmetic closure, to those whose work requires the symbol to operate as a quantity: algebraists and natural philosophers. Doctrinal authorities collect adjudicative standing from policing where notation ends and number begins.
% ABSENT_VOICES: The Indian arithmetic tradition, which has defined the symbol's operations for centuries, is outside the Latin doctrinal conversation and would testify that the prohibition is contingent, not necessary. The Parmenidean faction is present only as a silent pressure; admitted to the table it would object that even notational admission smuggles 'nothing' into arithmetic. Working algebraists are inside the institutions, but their testimony about the closure cost is classed as sophistry rather than evidence.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight in either direction, arrangements reorganize: full number status would immediately release algebraic and proto-scientific practice (equations with the symbol as root, completed subtractions, scale origins become writable), while banning notational use would collapse written positional commerce arithmetic back to abacus methods. Commercial bookkeeping, textbook structure, and master-apprentice curricula all depend on the current settlement.
% FOUNDING_PROBLEM: Positional notation requires a symbol for the empty place, but the inherited ontology, that nothing cannot be a quantity, forbids admitting that symbol as a number. The arrangement was built to take the notation's efficiency without taking the ontological commitment.
% FOUNDING_PROBLEM_CORROBORATION: The Indian arithmetic tradition corroborates, from outside the benefiting parties, that the prohibition is contingent: the same notation coexisted with defined operations for centuries. European algebraists' testimony corroborates that the closure cost is real rather than rhetorical. The persistence of the Parmenidean sibling reading corroborates that the founding ontological worry was genuine, not a cover story. No party inside the settlement's beneficiary set attests the problem's status alone.
narrative_ontology:disappearance_verdict(zero_mathematical_status__placeholder_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__placeholder_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__placeholder_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_mathematical_status__placeholder_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__placeholder_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon 0.50 is intermediate by the settlement's own reckoning: the reading itself concedes the closure gap as the price of ontological cleanliness, and medieval arithmetic explicitly acknowledged that subtracting equals 'yields nothing,' an admitted incompleteness rather than a hidden one. The referent of epsilon is the standing arrangement under contest (notation permitted, operations denied), never the number reading the siblings would install. Suppression (0.36 at interval end) is authored as a falling enforcement series because the story specifically tracks enforcement capacity: institutional scrutiny of the ciphers built through the fourteenth century (commercial bans on Arabic numerals, fraud anxieties), peaked, then eroded under print and commercial ubiquity. Theater (0.40 at interval end) rises monotonically: the boundary is increasingly recited ('a sign, nothing more') by practitioners who compute past it. Accessibility collapse is 0.40: the alternative, defined operations, is not collapsed by the constraint, it is institutionally excluded while remaining available in translated texts; knowing the doctrine does not make the alternative unthinkable. Resistance is 0.55: sustained algebraist friction across the interval, ultimately effective, aided by a de facto coalition of payer seats with translated Indian and Arabic testimony. The measurement grid is shared across all three series (eight points, 1202-1600); the extractiveness hump peaks at 1500, when cubic-era algebra most needs operational zero, and declines as leading practice migrates past the settlement. Suppression is authored as a raw structural property, unscaled; the engine scales only extractiveness, by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   From the merchant seat the arrangement is an unalloyed gift: the same structure that denies closure delivers the algorithms. From the algebraist seat it is a ceiling on the solvable. From the authority seat it is the guardianship of sense against nonsense. From the Indian seat it is a solved problem misgoverned elsewhere. The engine computes per-seat classifications from power, exit, and role data; the divergence between the beneficiary seats' coordination-flavored experience and the payer seats' extraction-flavored experience is the measurement the corpus exists to take, not a defect to be reconciled. The authoring claim (tangled_rope) asserts that the structure holds both halves; it does not adjudicate the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: merchant guilds (organized, constrained exit) and abacus masters (organized, mobile) sit near the beneficiary end, the notation subsidizes their work, and the masters' mobility keeps them nearest of all. Doctrinal authorities (institutional, identity_locked) derive low directionality with the lock amplifying persistence rather than extraction: they administer the boundary and draw standing from it, so their stake is in the boundary's continuation, not in any transfer it moves. Payers derive high directionality: algebraists (moderate, constrained) bear the closure cost with no exit from mathematics; natural philosophers (moderate, trapped) sit nearer the full-target end because no exit from the need for a settlement exists at all. Excluded seats sit near symmetric: the Parmenidean faction contests the arrangement without paying its transfer; the Indian tradition holds a complete working alternative, arbitrage-grade independence from the Latin settlement, and is neither subsidized nor taxed by it. No directionality overrides were needed: the derivation from the declared beneficiaries, victims, power atoms, and exit options reproduces these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   A pure-coordination reading would launder the extraction: the algebraist seat's closure cost is real, identifiable, and imposed by the same structure that coordinates notation, so a rope verdict would miss the asymmetry. A pure-extraction reading would miss the function: written positional arithmetic is not cover for the boundary; the boundary is a rider on a genuine and massive collective-action solution. Tangled rope holds both halves, which is why the structural gates (beneficiaries, victims, active enforcement) are all satisfied. On mandatrophy: the founding problem's notational half is permanently solved and its ontological half remains contested between the readings; at interval end the settlement still binds textbook and school practice, so the status is 'contested' rather than 'dead.' Authored at 1650 the same structure would read status=dead against a world_rearranges verdict and flag as a zombie; at 1600 the mismatch consumer correctly finds no capture verdict, though the receipt surface names the authorities as the seat the adjudicative surplus accrues to, and fixing is prohibitive for them precisely because their identity is fused with the boundary they would have to dissolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint instantiates one reading (placeholder_reading) of the zero_mathematical_status kernel; how would instantiating a sibling reading change the structure?',
    'Author and compile the sibling stories (zero_mathematical_status__number_reading, zero_mathematical_status__parmenidean_rejection) and compare per-seat classifications: the number reading dissolves the payer seats (operations defined, no blocked work, epsilon near zero in its own tradition); the Parmenidean reading converts the merchant seats into payers (notation itself banned, a calculation cost imposed where this reading grants a subsidy).',
    'Classification is reading-indexed: this story''s intermediate profile (efficiency gained, closure denied) is a property of the placeholder settlement, not of ''zero''s status'' in general; cross-reading comparison is valid only at the kernel level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: which kernel, which reading, what the siblings would change.').

omega_variable(
    denotation_entailment_dispute,
    'Where is the disagreement located: does using a symbol for the empty place commit arithmetic to the symbol''s denotation, i.e. does notational use entail number status?',
    'Conceptual analysis of what the transmitted practice itself requires: if the written algorithms can be specified without any commitment about the symbol''s denotation, the placeholder reading''s decoupling is coherent; if the algorithms'' correctness conditions implicitly quantify over the symbol as a value, they are not.',
    'If entailment holds, this reading is unstable from its own premises and collapses toward the number reading; if not, the boundary is a coherent settlement and the number reading''s eventual victory was a choice, not a discovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(denotation_entailment_dispute, conceptual, 'The load-bearing conceptual premise separating this reading from its siblings.').

omega_variable(
    closure_cost_magnitude,
    'How large is the actual cost of denied arithmetic closure: how much algebraic and proto-scientific progress did the boundary delay?',
    'Comparative developmental analysis: rate of equation-solving technique accumulation in traditions with number-status zero (Indian, Arabic) versus placeholder-bound Latin Europe, controlling for transmission channels and patronage.',
    'A large delay raises effective extraction at the algebraist and natural-philosopher seats (snare-ward pressure at those seats); a negligible delay supports the rope-ward reading that the boundary cost little beyond its coordination price.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_cost_magnitude, empirical, 'Empirical magnitude of the closure cost the arrangement imposes on its payers.').

omega_variable(
    doctrine_versus_inertia,
    'Is the boundary maintained at interval end because it is still believed, or by institutional inertia and recital?',
    'Track theater_ratio against doctrinal argument volume: if authorities articulate reasons (theater stays low relative to assertion volume) the doctrine is live; if the ratio climbs while argument volume falls, maintenance is theatrical.',
    'If inertia dominates, the authority seat drifts toward administered performance (the administrator could change the boundary but the identity cost of fixing exceeds what it bears), and the arrangement''s persistence stops evidencing its necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_versus_inertia, empirical, 'Live doctrine versus theatrical maintenance of the sign/number boundary.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional gatekeeping over curricula, licensure, and what textbooks may assert) or internalized (computists trained to regard operational zero as sophistry, so little enforcement is needed)?',
    'Post-exit trajectory: algebraists who learned number-status zero through translation channels, did they revert to the boundary when teaching inside Latin institutions, or retain the operational treatment? Persistence of operational practice after gatekeeping pressure lifts indicates an internalized component.',
    'If substantially internalized, effective suppression exceeds the structural measure: the boundary travels inside trained practitioners and outlives the institutions that taught it, and the falling suppression series understates the constraint''s true hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized enforcement of the notational boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__placeholder_reading, 1202, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_placeholder_tr_t1202, zero_mathematical_status__placeholder_reading, theater_ratio, 1202, 0.1).
narrative_ontology:measurement_basis(zero_placeholder_tr_t1202, observed).
narrative_ontology:measurement(zero_placeholder_tr_t1260, zero_mathematical_status__placeholder_reading, theater_ratio, 1260, 0.12).
narrative_ontology:measurement_basis(zero_placeholder_tr_t1260, observed).
narrative_ontology:measurement(zero_placeholder_tr_t1320, zero_mathematical_status__placeholder_reading, theater_ratio, 1320, 0.15).
narrative_ontology:measurement_basis(zero_placeholder_tr_t1320, observed).
narrative_ontology:measurement(zero_placeholder_tr_t1380, zero_mathematical_status__placeholder_reading, theater_ratio, 1380, 0.19).
narrative_ontology:measurement_basis(zero_placeholder_tr_t1380, observed).
narrative_ontology:measurement(zero_placeholder_tr_t1440, zero_mathematical_status__placeholder_reading, theater_ratio, 1440, 0.24).
narrative_ontology:measurement_basis(zero_placeholder_tr_t1440, observed).
narrative_ontology:measurement(zero_placeholder_tr_t1500, zero_mathematical_status__placeholder_reading, theater_ratio, 1500, 0.3).
narrative_ontology:measurement_basis(zero_placeholder_tr_t1500, observed).
narrative_ontology:measurement(zero_placeholder_tr_t1560, zero_mathematical_status__placeholder_reading, theater_ratio, 1560, 0.35).
narrative_ontology:measurement_basis(zero_placeholder_tr_t1560, observed).
narrative_ontology:measurement(zero_placeholder_tr_t1600, zero_mathematical_status__placeholder_reading, theater_ratio, 1600, 0.4).
narrative_ontology:measurement_basis(zero_placeholder_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(zero_placeholder_be_t1202, zero_mathematical_status__placeholder_reading, base_extractiveness, 1202, 0.45).
narrative_ontology:measurement_basis(zero_placeholder_be_t1202, observed).
narrative_ontology:measurement(zero_placeholder_be_t1260, zero_mathematical_status__placeholder_reading, base_extractiveness, 1260, 0.47).
narrative_ontology:measurement_basis(zero_placeholder_be_t1260, observed).
narrative_ontology:measurement(zero_placeholder_be_t1320, zero_mathematical_status__placeholder_reading, base_extractiveness, 1320, 0.5).
narrative_ontology:measurement_basis(zero_placeholder_be_t1320, observed).
narrative_ontology:measurement(zero_placeholder_be_t1380, zero_mathematical_status__placeholder_reading, base_extractiveness, 1380, 0.53).
narrative_ontology:measurement_basis(zero_placeholder_be_t1380, observed).
narrative_ontology:measurement(zero_placeholder_be_t1440, zero_mathematical_status__placeholder_reading, base_extractiveness, 1440, 0.56).
narrative_ontology:measurement_basis(zero_placeholder_be_t1440, observed).
narrative_ontology:measurement(zero_placeholder_be_t1500, zero_mathematical_status__placeholder_reading, base_extractiveness, 1500, 0.58).
narrative_ontology:measurement_basis(zero_placeholder_be_t1500, observed).
narrative_ontology:measurement(zero_placeholder_be_t1560, zero_mathematical_status__placeholder_reading, base_extractiveness, 1560, 0.54).
narrative_ontology:measurement_basis(zero_placeholder_be_t1560, observed).
narrative_ontology:measurement(zero_placeholder_be_t1600, zero_mathematical_status__placeholder_reading, base_extractiveness, 1600, 0.5).
narrative_ontology:measurement_basis(zero_placeholder_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(zero_placeholder_su_t1202, zero_mathematical_status__placeholder_reading, suppression_requirement, 1202, 0.55).
narrative_ontology:measurement_basis(zero_placeholder_su_t1202, observed).
narrative_ontology:measurement(zero_placeholder_su_t1260, zero_mathematical_status__placeholder_reading, suppression_requirement, 1260, 0.6).
narrative_ontology:measurement_basis(zero_placeholder_su_t1260, observed).
narrative_ontology:measurement(zero_placeholder_su_t1320, zero_mathematical_status__placeholder_reading, suppression_requirement, 1320, 0.62).
narrative_ontology:measurement_basis(zero_placeholder_su_t1320, observed).
narrative_ontology:measurement(zero_placeholder_su_t1380, zero_mathematical_status__placeholder_reading, suppression_requirement, 1380, 0.55).
narrative_ontology:measurement_basis(zero_placeholder_su_t1380, observed).
narrative_ontology:measurement(zero_placeholder_su_t1440, zero_mathematical_status__placeholder_reading, suppression_requirement, 1440, 0.48).
narrative_ontology:measurement_basis(zero_placeholder_su_t1440, observed).
narrative_ontology:measurement(zero_placeholder_su_t1500, zero_mathematical_status__placeholder_reading, suppression_requirement, 1500, 0.44).
narrative_ontology:measurement_basis(zero_placeholder_su_t1500, observed).
narrative_ontology:measurement(zero_placeholder_su_t1560, zero_mathematical_status__placeholder_reading, suppression_requirement, 1560, 0.4).
narrative_ontology:measurement_basis(zero_placeholder_su_t1560, observed).
narrative_ontology:measurement(zero_placeholder_su_t1600, zero_mathematical_status__placeholder_reading, suppression_requirement, 1600, 0.36).
narrative_ontology:measurement_basis(zero_placeholder_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__placeholder_reading, information_standard).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__parmenidean_rejection).

% DUAL FORMULATION NOTE:
% The colloquial question 'what is zero?' conflates three structurally distinct claims, the epsilon-invariance decomposition of the zero_mathematical_status kernel: that the symbol's operations are defined (number_reading; low epsilon in its own tradition, uncontested there since Brahmagupta); that the symbol is ontologically incoherent outright (parmenidean_rejection; bans even notational admission and imposes a calculation cost on commerce); and this reading (placeholder; notation without operations, intermediate epsilon: efficiency gained, closure denied). Each member is a separate file with its own epsilon, beneficiaries, and victims; this file's epsilon refers only to the placeholder settlement. The placeholder reading sits structurally between its siblings: it is downstream of the Parmenidean worry (it exists to answer it) and upstream of the number reading's European victory (its half-admission created centuries of fluent positional practice under which the operational treatment became thinkable), which is why it links to both. This file's epsilon is not averaged across readings and must not be compared against the siblings' epsilon as if one quantity were measured three ways; the label 'zero's status' was the ambiguity, and the decomposition is the fix.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
