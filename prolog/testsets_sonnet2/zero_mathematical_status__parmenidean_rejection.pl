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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: zero_mathematical_status__parmenidean_rejection
 *   human_readable: Parmenidean Rejection of Zero as a Number (Nothing Cannot Exist)
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This story instantiates the Parmenidean rejection reading of the
 *   zero-status kernel: the claim that 'nothing' cannot coherently be
 *   admitted as a number because being cannot arise from or be identified
 *   with non-being. This is a distinct constraint from the sibling readings
 *   (zero as a fully defined number with arithmetic rules, and zero as a mere
 *   positional placeholder) — it has its own ε, its own beneficiary/victim
 *   structure, and its own persistence mechanism: a philosophical tradition's
 *   institutional and pedagogical authority, defended by treating the
 *   ontological framing as settled rather than as one contestable position
 *   among several live in parallel mathematical cultures. The rejection
 *   functioned as genuine coordination for the geometric/ontological
 *   tradition (keeping 'number' conceptually tied to existing magnitude)
 *   while extracting real computational cost from every practitioner who
 *   needed a null placeholder or null value and had no standing to challenge
 *   the doctrine.
 *
 * KEY AGENTS:
 *   - classical_ontological_philosophers: agenda-setters who articulate and defend the doctrine
 *   - geometric_proof_tradition_practitioners: beneficiaries whose magnitude-based mathematics gains coherence from the exclusion
 *   - merchant_arithmetic_practitioners and positional_notation_users: payers who bear practical computational cost
 *   - indian_and_islamic_mathematical_traditions: excluded parallel tradition with a working counter-example
 *   - later_mathematical_historians: analytical observers reconstructing the doctrine's costs and eventual displacement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__parmenidean_rejection, 0.58).
domain_priors:suppression_score(zero_mathematical_status__parmenidean_rejection, 0.71).
domain_priors:theater_ratio(zero_mathematical_status__parmenidean_rejection, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, extractiveness, 0.58).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__parmenidean_rejection, tangled_rope).
narrative_ontology:human_readable(zero_mathematical_status__parmenidean_rejection, "Parmenidean Rejection of Zero as a Number (Nothing Cannot Exist)").
narrative_ontology:topic_domain(zero_mathematical_status__parmenidean_rejection, "history_of_mathematics/philosophy_of_mathematics").

domain_priors:requires_active_enforcement(zero_mathematical_status__parmenidean_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__parmenidean_rejection, 'c676ba8c-fac6-4959-8990-9153bee8c830').
narrative_ontology:cs_kernel_codification('c676ba8c-fac6-4959-8990-9153bee8c830', distributed).
narrative_ontology:cs_authority_grounding('c676ba8c-fac6-4959-8990-9153bee8c830', lineage).
narrative_ontology:cs_interpretation_layer_present('c676ba8c-fac6-4959-8990-9153bee8c830').
narrative_ontology:cs_reading_relation('c676ba8c-fac6-4959-8990-9153bee8c830', zero_mathematical_status__number_reading, forecloses).
narrative_ontology:cs_reading_relation('c676ba8c-fac6-4959-8990-9153bee8c830', zero_mathematical_status__placeholder_reading, influences).
narrative_ontology:cs_axiom('c676ba8c-fac6-4959-8990-9153bee8c830', foundational, being_cannot_arise_from_non_being).
narrative_ontology:cs_axiom_status(being_cannot_arise_from_non_being, holdable).
narrative_ontology:cs_axiom_grounding('c676ba8c-fac6-4959-8990-9153bee8c830', being_cannot_arise_from_non_being, deontological).
narrative_ontology:cs_axiom('c676ba8c-fac6-4959-8990-9153bee8c830', foundational, number_requires_existing_countable_quantity).
narrative_ontology:cs_axiom_status(number_requires_existing_countable_quantity, overridden).
narrative_ontology:cs_axiom_grounding('c676ba8c-fac6-4959-8990-9153bee8c830', number_requires_existing_countable_quantity, conventional).
narrative_ontology:cs_reference_frame('c676ba8c-fac6-4959-8990-9153bee8c830', parmenidean_being_non_being_dichotomy).
narrative_ontology:cs_drift_state('c676ba8c-fac6-4959-8990-9153bee8c830', post_indian_islamic_transmission, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('c676ba8c-fac6-4959-8990-9153bee8c830', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, classical_ontological_philosophers).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, geometric_proof_tradition_practitioners).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, aristotelian_scholastic_authorities).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, merchant_arithmetic_practitioners).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, positional_notation_users).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, astronomical_calculators).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, algebraic_method_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and transmit the doctrine that being cannot arise from non-being (Parmenides' dictum) and that a symbol for 'nothing' cannot coherently occupy the same ontological category as symbols for quantities that exist. They set the terms of what counts as a legitimate number within the philosophical tradition, teach it as settled metaphysics, and treat arithmetic systems admitting zero as category errors rather than competing conventions.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, classical_ontological_philosophers, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Practice a mathematics built on ratios, magnitudes, and geometric construction (the Euclidean tradition) that has no structural need for a null quantity and gains prestige and internal coherence from excluding it. Their proofs, pedagogy, and professional standing are organized around magnitude-based reasoning; zero's exclusion protects the tradition's internal consistency and their own authority as its custodians.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, geometric_proof_tradition_practitioners, beneficiary,
    institutional, generational, constrained, continental).

% Later scholastic institutions inherit and formalize the rejection, embedding it in curricula and doctrinal authority (the horror vacui tradition extended into number). Their institutional legitimacy is bound to the correctness of the inherited framework; admitting zero as a number would concede that centuries of authoritative teaching mischaracterized a basic ontological question.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, aristotelian_scholastic_authorities, beneficiary,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__parmenidean_rejection, aristotelian_scholastic_authorities, agenda_setter).

% Need to record empty quantities, null balances, and zero remainders in trade ledgers and accounts. Denied a legitimate zero, they must use circumlocutions, blank spaces, or ad hoc symbols that introduce ambiguity and error into commercial record-keeping, bearing real transactional cost for the philosophical prohibition.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, merchant_arithmetic_practitioners, payer,
    moderate, biographical, constrained, regional).

% Anyone attempting to write large numbers efficiently needs a placeholder to distinguish '5' from '50' from '500'. Barred from treating zero as a number, they are pushed toward cumbersome notations (Roman numerals, additive systems) that make large-scale computation slow and error-prone, with no individual power to change the prevailing doctrine.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, positional_notation_users, payer,
    powerless, biographical, trapped, regional).

% Track planetary positions, eclipse cycles, and calendrical intervals requiring precise handling of null differences and empty positions in tables. The rejection forces them to invent workaround conventions or import foreign (Babylonian, later Indian) placeholder notations informally, while their home tradition's official mathematics denies those tools legitimacy.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, astronomical_calculators, payer,
    moderate, biographical, constrained, continental).

% Early algebraists seeking general solution methods for equations need zero as both a value and a boundary condition (roots equal to zero, equations set to zero). Without a legitimate zero, algebra remains tethered to geometric magnitude interpretation and cannot generalize past what can be pictured as a length, area, or volume greater than nothing.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, algebraic_method_developers, payer,
    moderate, generational, constrained, continental).

% Operating in parallel traditions that had already formalized zero as a number with defined arithmetic (Brahmagupta's rules, later transmitted through Al-Khwarizmi), they have a working counter-example ready to hand but are not consulted or admitted into the Parmenidean framework's internal debate — their result is treated as foreign practice rather than as evidence bearing on the ontological question.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, indian_and_islamic_mathematical_traditions, excluded,
    organized, generational, mobile, continental).

% Reconstruct why the rejection held as long as it did and trace the eventual transmission of zero-as-number into European mathematics via translated Arabic texts, assessing the doctrine's costs against its internal philosophical coherence.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, later_mathematical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__parmenidean_rejection, diffuse).
narrative_ontology:fixing_cost_class(zero_mathematical_status__parmenidean_rejection, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves ontological and logical coherence within a magnitude-based mathematical tradition: a number is understood as a measure of something that exists, so a symbol for the absence of quantity is excluded to keep the number concept internally consistent with the tradition's metaphysics of being.
% TRANSFER_FUNCTION: Moves computational burden and error risk from the philosophical tradition (which is spared the difficulty of defining arithmetic with a null value) onto practical calculators — merchants, astronomers, notation users, and algebraists — who must devise workarounds or forgo efficiency gains that a legitimate zero would provide.
% ABSENT_VOICES: Indian and Islamic mathematicians already working with a formalized zero are not part of the debate that sustains the rejection within this tradition; their arithmetic rules are available but not admitted as evidence because the question is framed as ontological rather than operational.
% DISAPPEARANCE_RATIONALE: If the Parmenidean rejection were abandoned, positional notation, double-entry accounting, general algebraic methods, and simplified astronomical tables could all be adopted without philosophical obstruction — which is roughly what happened historically once zero-as-number was transmitted from Indian and Islamic sources into European mathematics, triggering centuries of practical computational advance.
% FOUNDING_PROBLEM: The rejection was built to preserve a coherent theory of being and number: if 'nothing' were granted number-status, the classical equation of number with countable, existing quantity would break, threatening the broader Parmenidean metaphysical framework that being cannot come from or be identical with non-being.
% FOUNDING_PROBLEM_CORROBORATION: Historians of mathematics outside the Parmenidean tradition — including scholars documenting the Indian, Babylonian, and Islamic transmission histories of zero — attest that the ontological problem was a metaphysical commitment specific to one philosophical lineage, not a problem inherent to arithmetic; comparative mathematical practice from contemporaneous non-Greek traditions functioned successfully with zero as a number throughout the period the rejection held in the West.
narrative_ontology:disappearance_verdict(zero_mathematical_status__parmenidean_rejection, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__parmenidean_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__parmenidean_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_mathematical_status__parmenidean_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__parmenidean_rejection, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate-high (0.58) because the cost imposed on practical calculators is real but diffuse — no single victim group is catastrophically harmed, but the aggregate loss of computational efficiency across trade, astronomy, and algebra over centuries is substantial. Suppression is high (0.71) because the rejection persists not through argument alone but through institutional transmission — teaching the doctrine as settled metaphysics rather than as a contested position, and excluding contrary practice (Indian/Islamic zero-arithmetic) from consideration rather than engaging it. Theater ratio rises over the interval (0.20 to 0.42) as the doctrine increasingly serves to defend inherited authority rather than to solve a live philosophical problem — by the later period the ontological argument is invoked more to preserve scholastic prestige than to resolve a genuine unresolved question, since a working alternative already existed elsewhere. All three tracked metrics share one time grid across six points.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philosophers and the geometric tradition are structural beneficiaries: the exclusion of zero preserves the coherence and prestige of their existing framework, so their directionality sits near the beneficiary end. Merchant arithmeticians, positional notation users, astronomical calculators, and algebraic developers are targets: they bear the transaction cost of the prohibition without the standing to overturn it, so their directionality sits near the target end, amplified by constrained or trapped exit options. Indian and Islamic mathematicians are excluded rather than coordinated or extracted from directly — their exclusion from the debate is itself a form of the constraint's suppression mechanism, denying evidentiary standing to a working counter-example.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving ontological coherence within a being/non-being metaphysics) was live within the philosophical tradition's own terms for centuries, but became structurally dead once parallel traditions demonstrated a fully workable arithmetic with zero as a number. Classifying this as tangled_rope rather than pure snare captures that the doctrine did solve a genuine internal coherence problem for its own framework (a real coordination function for that tradition's conceptual system) while simultaneously extracting real cost from everyone who needed the placeholder or null-value functionality zero provides — treating it as pure extraction would erase the doctrine's genuine philosophical motivation; treating it as pure coordination would erase the documented cost borne by merchants, astronomers, and algebraists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_operational_framing,
    'Is the Parmenidean rejection a genuine metaphysical insight about the nature of number and existence, or a category error that conflates a philosophical claim about being with a purely operational question about arithmetic notation?',
    'Comparative analysis of contemporaneous mathematical traditions (Babylonian, Indian, Mayan) that successfully operationalized zero without resolving or even engaging the being/non-being metaphysical question — if operational success is fully independent of the ontological resolution, the rejection targeted the wrong level of the problem.',
    'If the ontological question is genuinely separable from arithmetic operation, the rejection''s coordination function collapses to zero and the constraint is better read as a pure snare (philosophical prestige extracting cost from practitioners with no genuine coordination payoff); if the metaphysical coherence is a real prerequisite the classical tradition needed, the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_operational_framing, conceptual, 'Whether the rejection solves a real problem for its own tradition or is pure extraction dressed as metaphysics.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the parmenidean_rejection reading''s core premise (being cannot arise from non-being, therefore zero cannot be a number) logically foreclose the number_reading (zero as a fully arithmetic number), or can both persist as coexisting positions held by different traditions without either being forced to concede?',
    'Examine whether any single mathematical-philosophical framework has ever held both positions simultaneously without contradiction, versus whether they have only ever existed as separate traditions that do not directly argue against each other.',
    'If forecloses is the correct relation, adopting parmenidean_rejection within a given tradition structurally rules out that same tradition later adopting number_reading without an explicit repudiation event (as happened historically with the reception of Al-Khwarizmi''s work in Europe); if coexists_with is correct, the two readings can be held by different parties indefinitely without resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether this reading logically forecloses the number_reading or merely coexists with it across different traditions.').

omega_variable(
    beneficiary_capture_of_ontology,
    'To what extent did the geometric proof tradition and scholastic authorities knowingly perpetuate the rejection to protect institutional prestige, versus genuinely believing the ontological argument on its merits?',
    'Textual analysis of surviving commentary for explicit acknowledgment of the practical costs of excluding zero, and evidence of engagement with (versus dismissal of) the competing Indian/Islamic arithmetic traditions once they became known.',
    'Evidence of conscious cost-benefit awareness with continued suppression would strengthen the extraction-emphasis read of the classification; evidence of genuine unawareness of the alternative would support treating the persistence as good-faith coordination that only later became identifiable as costly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_ontology, empirical, 'Whether the beneficiary institutions knowingly extracted cost or acted in good philosophical faith.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__parmenidean_rejection, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_mathematical_status__parmenidean_rejection, theater_ratio, 0, 0.2).
narrative_ontology:measurement(zero_tr_t20, zero_mathematical_status__parmenidean_rejection, theater_ratio, 20, 0.28).
narrative_ontology:measurement(zero_tr_t40, zero_mathematical_status__parmenidean_rejection, theater_ratio, 40, 0.35).
narrative_ontology:measurement(zero_tr_t60, zero_mathematical_status__parmenidean_rejection, theater_ratio, 60, 0.4).
narrative_ontology:measurement(zero_tr_t80, zero_mathematical_status__parmenidean_rejection, theater_ratio, 80, 0.45).
narrative_ontology:measurement(zero_tr_t100, zero_mathematical_status__parmenidean_rejection, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(zero_be_t20, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(zero_be_t40, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(zero_be_t60, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(zero_be_t80, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(zero_be_t100, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(zero_su_t20, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(zero_su_t40, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(zero_su_t60, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(zero_su_t80, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(zero_su_t100, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__parmenidean_rejection, identity_coordination).
narrative_ontology:boltzmann_floor_override(zero_mathematical_status__parmenidean_rejection, 0.1).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__placeholder_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the zero_mathematical_status kernel. parmenidean_rejection (this story) excludes zero from both number and notational status and carries the widest victim set and highest suppression. number_reading (sibling) grants zero full arithmetic status per Brahmagupta's rules and has substantially lower extraction, being closer to a rope. placeholder_reading (sibling) grants zero notational but not arithmetic status and sits structurally between the other two. The three do not share an ε — each is authored independently per the ε-invariance principle, and the family is linked here rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
