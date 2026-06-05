% ============================================================================
% CONSTRAINT STORY: number_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_number_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: number_reading
 *   human_readable: Zero as Number: Brahmagupta's Arithmetical Integration
 *   domain: mathematics/history_of_mathematics/conceptual_foundations
 *
 * SUMMARY:
 *   This constraint story instantiates the 'number reading' of the
 *   zero_mathematical_status kernel: the interpretation that zero is
 *   genuinely a number with defined arithmetic operations, fully integrated
 *   into the number system. This reading crystallizes in Brahmagupta's
 *   Brahmasphutasiddhanta (628 CE), which explicitly states the arithmetic
 *   rules governing zero: a+0=a (additive identity), a×0=0 (multiplicative
 *   absorber), and implicitly establishes zero as a cardinal entity. The
 *   number reading treats zero's integration as a pure coordination mechanism
 *   — it solves the problem of how to represent magnitude-absence and
 *   position-holder in a unified notational and arithmetical framework.
 *   Unlike its siblings, this reading does not reject zero as non-entity
 *   (parmenidean_rejection) nor treat it as a mere syntactic placeholder
 *   without numerical quantity (placeholder_reading). The constraint is a
 *   Rope: it enables astronomical calculation, algebraic manipulation, and
 *   place-value notation without extracting asymmetric benefit from any
 *   party. All mathematical practitioners gain equally from the unified
 *   system.
 *
 * KEY AGENTS:
 *   - Brahmagupta and Hindu Mathematical Tradition: Authority grounding (lineage) — establishes the kernel commitment that zero is a number
 *   - Mathematical Practitioners: Beneficiary (institutional/arbitrage) — gain computational efficiency and notational clarity from zero's integration
 *   - Astronomical Calculators: Beneficiary (institutional/arbitrage) — enable precise planetary position calculations without special-casing empty quantities
 *   - Algebraic System Developers: Beneficiary (institutional/arbitrage) — zero's closure properties enable symbolic algebra, polynomial rings, and field structures
 *   - The Parmenidean Tradition: Competing reading holder (institutional/arbitrage) — maintains that zero is not, cannot be, and should not be treated as a number (coexists_with)
 *   - The Placeholder Reading Tradition: Alternative reading holder (institutional/arbitrage) — treats zero as operational convention rather than numerical entity (coexists_with)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(number_reading, 0.08).
domain_priors:suppression_score(number_reading, 0.02).
domain_priors:theater_ratio(number_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(number_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(number_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(number_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(number_reading, rope).
narrative_ontology:human_readable(number_reading, "Zero as Number: Brahmagupta's Arithmetical Integration").
narrative_ontology:topic_domain(number_reading, "mathematics/history_of_mathematics/conceptual_foundations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(number_reading, '1af3f4cd-bc56-47a5-824e-f1429fc9330b').
narrative_ontology:cs_created_at('1af3f4cd-bc56-47a5-824e-f1429fc9330b', '').
narrative_ontology:cs_kernel_codification('1af3f4cd-bc56-47a5-824e-f1429fc9330b', fixed_text).
narrative_ontology:cs_authority_grounding('1af3f4cd-bc56-47a5-824e-f1429fc9330b', lineage).
narrative_ontology:cs_interpretation_layer_present('1af3f4cd-bc56-47a5-824e-f1429fc9330b').
narrative_ontology:cs_kernel_id(number_reading, zero_mathematical_status).
narrative_ontology:cs_reading_relation('1af3f4cd-bc56-47a5-824e-f1429fc9330b', parmenidean_rejection, forecloses).
narrative_ontology:cs_reading_relation('1af3f4cd-bc56-47a5-824e-f1429fc9330b', placeholder_reading, coexists_with).
narrative_ontology:cs_axiom('1af3f4cd-bc56-47a5-824e-f1429fc9330b', foundational, zero_is_numerical_entity).
narrative_ontology:cs_axiom_status(zero_is_numerical_entity, holdable).
narrative_ontology:cs_axiom_grounding('1af3f4cd-bc56-47a5-824e-f1429fc9330b', zero_is_numerical_entity, empirically_contingent).
narrative_ontology:cs_axiom('1af3f4cd-bc56-47a5-824e-f1429fc9330b', foundational, arithmetic_closure_completeness).
narrative_ontology:cs_axiom_status(arithmetic_closure_completeness, holdable).
narrative_ontology:cs_axiom_grounding('1af3f4cd-bc56-47a5-824e-f1429fc9330b', arithmetic_closure_completeness, instrumental).
narrative_ontology:cs_reference_frame('1af3f4cd-bc56-47a5-824e-f1429fc9330b', unified_arithmetical_system).
narrative_ontology:cs_drift_state('1af3f4cd-bc56-47a5-824e-f1429fc9330b', contemporary_post_algebraic, gap(stable, minor, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(number_reading, mathematical_practitioners).
narrative_ontology:constraint_beneficiary(number_reading, algebraic_computation).
narrative_ontology:constraint_beneficiary(number_reading, celestial_mechanics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL PRACTITIONER (ROPE) — Zero integrated into arithmetic operations enables celestial calculation, algebraic manipulation, and systematic notation. The constraint solves a genuine coordination problem: how to represent magnitude-absence and position-holder in unified notation. No extraction — pure coordination benefit. The practitioner experiences this as freedom to compute without special-casing empty quantities.
constraint_indexing:constraint_classification(number_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 2: ASTRONOMICAL CALCULATOR (ROPE) — Brahmagupta's rules (a+0=a, a×0=0) enable systematic computation of planetary positions without ad-hoc methods. The constraint is pure coordination: it provides a shared language for calculation that reduces error and enables reproducibility. Low theater — the arithmetic rules are transparent and verifiable. No beneficiary extraction — calculation becomes faster and more reliable for all users.
constraint_indexing:constraint_classification(number_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (ROPE) — From a civilizational vantage, zero's integration into arithmetic operations is a pure coordination mechanism. No agent extracts from others; the constraint reduces cognitive load for all practitioners and enables mathematical structures (place-value notation, algebraic manipulation, calculus) that would be impossible without it. The constraint has zero suppression — practitioners adopt it because it works, not because they are coerced. Theater ratio is minimal — the rules are transparent and logically coherent.
constraint_indexing:constraint_classification(number_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(number_reading_tests).
:- end_tests(number_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The number reading introduces zero as a number system element that benefits all practitioners equally. There is no asymmetric extraction — the coordination mechanism (unified arithmetic rules) reduces computational burden for all users. The slight non-zero value (0.08 rather than 0.00) reflects minor cognitive friction in learning new rules and potential short-term disruption when transitioning from non-zero systems, but these costs are transitional, not structural. Suppression (0.02): Negligible. The rules a+0=a and a×0=0 are transparent, logically coherent, and require no coercion to adopt. Practitioners choose to use them because they work. No alternative is suppressed — parmenidean_rejection and placeholder_reading remain live options for those who reject the number reading. Theater ratio (0.15): Very low. The arithmetic rules are directly verifiable, transparent in operation, and their consequences are observable in calculation. There is minimal performative content — the justification for the rules is functional (they enable calculation) and logical (they maintain consistency in the number system), not theatrical.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify as Rope because the number reading exhibits no extraction structure. The institutional beneficiary (practitioners) experiences the constraint as pure coordination gain. The analytical observer sees the same: a mechanism that solves a coordination problem (unified notation, systematic arithmetic) without asymmetric benefit. The astronomical calculator confirms: zero's integration enables reliable calculation. There is no perspectival gap because no agent experiences asymmetric extraction or suppression. This uniformity is diagnostic: the number reading is genuinely a coordination mechanism, not a hidden extraction constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents in the number reading context are beneficiaries or neutral observers. Directionality (d) is uniformly low (0.10-0.30 across institutional contexts), corresponding to f(d) ≈ 0.0 to 0.20, producing χ ≈ 0.008 to 0.016 at global scope. No agent is a victim or target of extraction. The structure is pure coordination — the constraint's existence benefits the practitioners who adopt it, costs nothing materially, and suppresses no alternatives. The parmenidean tradition can hold its rejection of zero as a number without being suppressed by this reading's success. The placeholder reading can maintain its distinction between syntactic and semantic zero without being foreclosed. The number reading does not extract its authority through suppression of rivals; it succeeds through functional superiority in enabling mathematical practice.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY: Extractiveness is 0.08, below the 0.46 threshold. The number reading exhibits no mandatrophy because the constraint's coordination function is clear and unambiguous. The arithmetic rules serve the practitioners who use them; there is no hidden extraction or suppression. The constraint is stable across all perspectives and time horizons — it remains Rope under all observed contexts. The minimal theater ratio (0.15) reflects that the arithmetic rules require no performative justification: they are transparent, verifiable, and logically coherent. There is no contradiction between the constraint's claimed function (coordination) and its structural mechanism (unified arithmetic integration).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zero_ontological_status,
    'Is zero a number, or merely a notational placeholder? Does zero''s arithmetic integration rest on treating it as genuinely numerical (with quantity=absence) or as a syntactic convention (a position-holder in place-value notation)?',
    'Examine Brahmagupta''s own justification in the Brahmasphutasiddhanta (628 CE); compare with alternative readings of zero as non-numerical placeholder in earlier Hindu, Chinese, and Babylonian systems. Assess whether the arithmetic rules (a+0=a, a×0=0) require zero to be ontologically a number or merely conventionally integrated.',
    'If zero is genuinely numerical: this reading (number_reading) holds. The constraint is rope. If zero is merely notational: the parmenidean_rejection reading (zero as non-entity) gains legitimacy. The constraint becomes ambiguous between rope and a degraded coordination mechanism (piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_ontological_status, conceptual, 'Ontological status of zero: number vs. notational convention').

omega_variable(
    arithmetic_closure_requirement,
    'Does the requirement that number systems be closed under arithmetic operations (a+b, a×b always yield a number) mandate zero''s inclusion, or is closure a modern mathematical preference that post-dates Brahmagupta?',
    'Historical analysis of when closure axioms became explicit requirements in number-system definitions. Examine whether Brahmagupta''s integration of zero was motivated by closure demands or by empirical/computational necessity (handling negative balances, placeholder notation).',
    'If closure was a later requirement: Brahmagupta''s integration is contingent coordination improvement (rope). If closure was implicit in ancient practice: the integration is a discovery of necessity (rope remains, but with stronger modal force — less contingent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arithmetic_closure_requirement, empirical, 'Whether arithmetic closure was an explicit requirement in ancient practice').

omega_variable(
    parmenidean_alternative_viability,
    'Could a coherent mathematical system exist that rejects zero as a number, treating it only as a position-holder or operational identity (a+0=a as definitional, not as zero-as-number arithmetic)? Would such a system be functionally equivalent to Brahmagupta''s, or would it lose essential capability?',
    'Formal comparison of Brahmagupta-style arithmetic (zero as number) vs. alternative systems that treat zero as non-numerical but operationally defined. Test whether symbolic algebra, calculus, or complex analysis require zero''s numerical status or merely its operational integration.',
    'If alternative is viable and functionally equivalent: the parmenidean_rejection reading remains structurally coherent (coexists_with). If alternative loses essential capability: this reading (number_reading) forecloses parmenidean_rejection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parmenidean_alternative_viability, conceptual, 'Whether a coherent non-numerical zero system could achieve mathematical closure').

omega_variable(
    reading_kernel_identity,
    'What constitutes ''the kernel'' that this reading and its siblings read differently? Is the kernel Brahmagupta''s original statement of the rules? The abstract concept of zero? The place-value notation system? Or an implicit commitment to numerical completeness?',
    'Clarify which historical or conceptual artifact serves as the stabilized commitment that all readings engage with. If siblings disagree about what the kernel is, the readings may not be genuinely sibling readings of a single kernel.',
    'If the kernel is Brahmagupta''s statement: the readings differ on interpretation (hermeneutic). If the kernel is numerical completeness: the readings differ on whether it is achievable or desirable (metaphysical). The nature of the kernel determines how strongly the reading_relations structure applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Identity of the zero-status kernel contested by all readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(number_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(numb_tr_t0, number_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(numb_tr_t50, number_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(numb_tr_t100, number_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(numb_be_t0, number_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(numb_be_t100, number_reading, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(number_reading, information_standard).
narrative_ontology:affects_constraint(number_reading, parmenidean_rejection).
narrative_ontology:affects_constraint(number_reading, placeholder_reading).
narrative_ontology:affects_constraint(number_reading, place_value_notation_system).
narrative_ontology:affects_constraint(number_reading, algebraic_closure_requirement).

% DUAL FORMULATION NOTE:
% The zero_mathematical_status kernel has three reading stories: number_reading (this one), parmenidean_rejection (zero as non-entity), and placeholder_reading (zero as syntactic convention). The three readings have ε values reflecting their respective structural relationships to mathematical practice. Number_reading has ε=0.08 (pure coordination). Parmenidean_rejection has higher ε (rejection incurs coordination costs). Placeholder_reading occupies intermediate ground (syntactic integration without numerical commitment). All three stories are linked via network.affects_constraints and share the same kernel_id in their respective cs_structure.kernel_context fields.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
