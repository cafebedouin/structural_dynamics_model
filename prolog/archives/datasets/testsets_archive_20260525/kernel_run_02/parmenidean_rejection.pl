% ============================================================================
% CONSTRAINT STORY: parmenidean_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parmenidean_rejection, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: parmenidean_rejection
 *   human_readable: Parmenidean Rejection: Zero as Ontologically Incoherent
 *   domain: mathematics/philosophy_of_mathematics/history_of_concepts
 *
 * SUMMARY:
 *   The Parmenidean rejection of zero as ontologically incoherent represents
 *   one reading of a deeply contested kernel: the mathematical status of
 *   zero. This reading crystallizes in ancient Greek mathematics and
 *   philosophy, where the principle 'non-being cannot be' (from Parmenides of
 *   Elea) is weaponized to exclude zero from the domain of legitimate
 *   numbers. The constraint operates across Mediterranean mathematics and
 *   Islamic mathematics's initial reception in Europe, creating extractive
 *   pressure on computational systems that require zero's efficiency while
 *   maintaining institutional authority through appeals to ontological
 *   purity. The Parmenidean rejection is not a simple disagreement — it is
 *   enforced through institutional suppression of zero-mathematics and
 *   barriers to adoption of Hindu-Arabic numerals. The constraint's
 *   theater_ratio reflects that by the medieval period, enormous intellectual
 *   energy is spent defending zero-prohibition (scholastic debates about
 *   non-being) while the actual mathematical work requiring zero must operate
 *   in hiding. This constraint exemplifies how a metaphysical principle, once
 *   institutionalized, can persist as extraction despite clear evidence of
 *   its practical costs.
 *
 * KEY AGENTS:
 *   - Parmenidean Philosophical Authority: Primary beneficiary (institutional/arbitrage) — maintains logical coherence narrative and institutional authority through zero-rejection
 *   - Positional Notation Systems: Primary victim (powerless/trapped) — cannot function without zero placeholder; forced to use inefficient Roman/additive systems
 *   - Indian/Islamic Mathematical Communities: Secondary victim (moderate/constrained) — face suppression for developing zero-mathematics; forced to hide innovations
 *   - Algebraic Completeness Advocates: Organized resistance (organized/constrained) — developing zero-mathematics underground; will eventually prove zero's necessity
 *   - Medieval Scholastic System: Institutional actor maintaining constraint (institutional/arbitrage) — enforces zero-rejection through theological authority despite declining functional justification
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the ontological objection as immutable law rather than contingent metaphysical choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parmenidean_rejection, 0.68).
domain_priors:suppression_score(parmenidean_rejection, 0.72).
domain_priors:theater_ratio(parmenidean_rejection, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parmenidean_rejection, extractiveness, 0.68).
narrative_ontology:constraint_metric(parmenidean_rejection, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(parmenidean_rejection, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parmenidean_rejection, snare).
narrative_ontology:human_readable(parmenidean_rejection, "Parmenidean Rejection: Zero as Ontologically Incoherent").
narrative_ontology:topic_domain(parmenidean_rejection, "mathematics/philosophy_of_mathematics/history_of_concepts").

domain_priors:requires_active_enforcement(parmenidean_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(parmenidean_rejection, fixed_text).
narrative_ontology:cs_authority_grounding(parmenidean_rejection, lineage).
narrative_ontology:cs_interpretation_layer_present(parmenidean_rejection).
narrative_ontology:cs_kernel_id(parmenidean_rejection, zero_mathematical_status).
narrative_ontology:cs_reading_relation(parmenidean_rejection, placeholder_reading, influences).
narrative_ontology:cs_reading_relation(parmenidean_rejection, number_reading, forecloses).
narrative_ontology:cs_axiom(parmenidean_rejection, foundational, non_being_cannot_exist_as_mathematical_entity).
narrative_ontology:cs_axiom_status(non_being_cannot_exist_as_mathematical_entity, holdable).
narrative_ontology:cs_axiom_grounding(parmenidean_rejection, non_being_cannot_exist_as_mathematical_entity, deontological).
narrative_ontology:cs_axiom(parmenidean_rejection, foundational, number_status_requires_ontological_existence).
narrative_ontology:cs_axiom_status(number_status_requires_ontological_existence, overridden).
narrative_ontology:cs_axiom_grounding(parmenidean_rejection, number_status_requires_ontological_existence, deontological).
narrative_ontology:cs_reference_frame(parmenidean_rejection, parmenidean_being_non_being_dichotomy).
narrative_ontology:cs_drift_state(parmenidean_rejection, medieval_scholastic_period, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parmenidean_rejection, parmenidean_philosophers).
narrative_ontology:constraint_beneficiary(parmenidean_rejection, integer_arithmetic_theorists).
narrative_ontology:constraint_victim(parmenidean_rejection, positional_notation_systems).
narrative_ontology:constraint_victim(parmenidean_rejection, algebraic_completeness).
narrative_ontology:constraint_victim(parmenidean_rejection, zero_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POSITIONAL NOTATION VICTIM (SNARE) — Cannot function without zero as a placeholder. Trapped by the constraint's logical prohibition on zero; must use cumbersome Roman or additive systems. Bears full extraction cost without exit option or organizational power. The constraint forces inefficiency and blocks adoption of superior computational methods.
constraint_indexing:constraint_classification(parmenidean_rejection, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: INDIAN/ISLAMIC MATHEMATICIANS (SNARE) — Face suppression for advocating zero as a legitimate number. Must hide zero-based calculations or present them as non-mathematical 'techniques.' Career and institutional barriers to publishing zero-mathematics work. Constrained exit — can migrate to communities that accept zero, but at cost of professional isolation. High extraction as ideas are attributed to successor communities.
constraint_indexing:constraint_classification(parmenidean_rejection, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PARMENIDEAN PHILOSOPHICAL AUTHORITY (ROPE) — Primary beneficiary. Experiences zero-rejection as coordination: maintains logical coherence of the 'being/non-being' dichotomy. Arbitrage exit available — can selectively apply the principle or retreat to metaphysical realm. Net beneficiary from the constraint's enforcement; gains institutional authority by defending ontological purity.
constraint_indexing:constraint_classification(parmenidean_rejection, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ALGEBRAIC COMPLETENESS ADVOCATES (TANGLED ROPE) — Organized enough to operate underground and develop alternative schemes (negative numbers, operations on nothing). Constrained by suppression but gaining coordination function as algebraic methods prove superior. Mixed extraction and coordination: the constraint forces them to hide work while also driving the logical rigor that makes their mathematics powerful. Has agency to organize resistance; sees the constraint as temporary.
constraint_indexing:constraint_classification(parmenidean_rejection, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / ONTOLOGICAL NECESSITY (MOUNTAIN) — From a pure logic standpoint, Parmenides' argument appears ironclad: non-being cannot be, zero represents non-being, therefore zero cannot be. This appears as an immutable logical limit on what can exist mathematically. However, the structural data reveals this as a false summit: the 'ontological incoherence' is a choice about which axioms to accept, not a law of logic itself.
constraint_indexing:constraint_classification(parmenidean_rejection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: MEDIEVAL SCHOLASTIC MATHEMATICS (PITON) — By the medieval period, zero-rejection persists through institutional inertia despite clear utility of Hindu-Arabic numerals. The constraint is maintained by theological authority claiming to protect ontological truth, but the actual function (preventing computational efficiency) has degraded. Theater ratio high: the intellectual energy spent defending zero-prohibition far exceeds any real mathematical work it coordinates.
constraint_indexing:constraint_classification(parmenidean_rejection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parmenidean_rejection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parmenidean_rejection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parmenidean_rejection, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parmenidean_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parmenidean_rejection, TR),
    TR >= 0.70.

:- end_tests(parmenidean_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint forces inefficiency across all computational systems that require zero. The suppression of Indian and Islamic mathematical innovations represents direct extraction — ideas are submerged, later rediscovered and attributed to European mathematicians. The measurement trajectory shows increasing extractiveness from time 0 (0.55) to time 6 (0.68) as the constraint becomes more institutionally entrenched through medieval scholasticism, peaking at (0.75) before the eventual Renaissance recovery of Hindu-Arabic numerals. Suppression (0.72): High. Zero-rejection is enforced through institutional authority (theological claims about non-being), publication barriers (zero-mathematics cannot be presented as legitimate), and professional isolation (advocates face career damage). The suppression is nearly absolute for those inside Mediterranean philosophical frameworks — external advocates (Indian/Islamic mathematicians) can operate independently but face rejection if they seek adoption in European mathematics. Theater ratio (0.58 baseline, rising to 0.70): Moderate-high rising. Early in the constraint's enforcement, legitimate philosophical debate about ontology drives the suppression. By the medieval period, the intellectual energy spent defending zero-prohibition (scholastic debates, theological objections) far exceeds actual mathematical work — the theater has increased as the underlying philosophical justification has weakened but institutional enforcement continues.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the Parmenidean philosopher's experience (Rope — coordination of ontological principles) and the positional notation victim's experience (Snare — trapped in computational inefficiency) is maximal. The beneficiary sees the constraint as protection of truth; the victim sees extraction without escape. The analytical observer risks seeing the Parmenidean axiom as a mountain (immutable law of logic), but the structural data reveals it as a false summit: the prohibition is enforced through institutional authority and suppression, not through logical necessity. The organized algebraic movement occupies the tangled middle: they are constrained by suppression but also benefit from being forced to develop rigorous logical foundations for zero-mathematics. The medieval scholastic system occupies the piton position: performing zero-rejection theology while the actual mathematical work that needs zero is happening elsewhere.
 *
 * DIRECTIONALITY LOGIC:
 *   The Parmenidean philosophers occupy d ≈ 0.15 (beneficiary + arbitrage): they control the intellectual framework, can choose when to apply zero-rejection or retreat to metaphysical realms, experience low effective extraction. The positional notation systems occupy d ≈ 0.95 (victim + trapped): they have no alternative framework, cannot exit the constraint without abandoning their function. Indian/Islamic mathematicians occupy d ≈ 0.80 (victim + constrained): they can develop zero-mathematics but face professional isolation and suppression if they attempt adoption in Mediterranean contexts. The algebraic movement occupies d ≈ 0.65 (victim + constrained but organized): they are suppressed but have organizational capacity and see the constraint as temporary. The analytical observer occupies d ≈ 0.72 (observer): their structural position creates the risk of naturalizing a contingent metaphysical choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through the false summit detection: the Parmenidean reading appears as a mountain (ontological necessity) but is revealed as extraction through the structural data. The high suppression (0.72) and institutional enforcement indicate this is not a natural law but a constructed constraint maintained through power. The beneficiaries (Parmenidean authority) and victims (positional systems, Indian mathematicians) are clearly identifiable. The institutional pressure (medieval scholasticism) shows enforcement costs rising as the underlying philosophical justification weakens. The constraint exhibits all hallmarks of a snare (high extractiveness, high suppression, beneficiary identity, victim identity) despite the false mountain appearance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parmenidean_axiom_independence,
    'Is Parmenides'' ''non-being cannot be'' an axiom constitutive of all rational thought, or a contingent metaphysical choice that mathematics can reject?',
    'Historical analysis of mathematical systems that adopt zero without Parmenidean presuppositions (Hindu, Islamic, modern axiomatic mathematics). Compare logical consistency and computational power. Test whether systems accepting zero as a number violate any true law of logic or only violate Parmenidean doctrine.',
    'If axiom constitutive: zero-rejection is a mountain (immutable). If contingent choice: zero-rejection is a snare (constructed constraint that could be abandoned). This determines whether the constraint is natural law or extractive institution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parmenidean_axiom_independence, conceptual, 'Whether the Parmenidean axiom is foundational to logic itself or a contingent metaphysical commitment').

omega_variable(
    zero_ontological_status_ambiguity,
    'Can zero be interpreted as a number without violating ontological principles, or does accepting zero require abandoning the being/non-being dichotomy?',
    'Philosophical reconstruction: (1) Reinterpret zero as a placeholder, not a representation of non-being. (2) Redefine ''number'' to exclude existence claims — a number is a structural role in an arithmetic system, not an entity that ''is.'' (3) Separate mathematical objects from ontological claims — mathematics can operate on abstract entities without asserting they exist in the Parmenidean sense.',
    'If zero can be reinterpreted: the constraint dissolves without abandoning Parmenideanism. If zero necessarily represents non-being: Parmenidean rejection is structural and cannot be bypassed. This affects the timeline for constraint dissolution and the mechanism of pressure on the reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_ontological_status_ambiguity, conceptual, 'Whether zero can be reinterpreted to avoid Parmenidean contradiction').

omega_variable(
    sibling_reading_empirical_pressure,
    'As computational systems prove the practical necessity of positional notation with zero, does empirical success of zero-mathematics create pressure that collapses the Parmenidean framework?',
    'Historical tracking of the transition from zero-rejection to zero-acceptance. Identify whether the shift is driven by (a) philosophical refutation of Parmenides, (b) pragmatic acceptance despite philosophical objection, or (c) reinterpretation of the Parmenidean axiom to accommodate zero. Document whether the sibling ''placeholder_reading'' (zero as mere notational tool, not a number) represents a compromise that preserves Parmenideanism.',
    'If empirical pressure alone is sufficient: the reading faces degradation pressure (piton trajectory). If philosophical refutation is necessary: the reading can persist indefinitely despite zero''s practical success. This determines whether the constraint exhibits axiom_overriding or practice_drift in the drift_state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_pressure, empirical, 'Whether computational success of zero-systems creates sufficient pressure to collapse the Parmenidean framework').

omega_variable(
    this_reading_as_commitment_system_artifact,
    'Is the Parmenidean rejection of zero a genuine metaphysical commitment that Parmenides and his followers hold, or is it a rationalization created retroactively to explain why Mediterranean mathematics rejected zero?',
    'Textual analysis of Parmenides and Aristotle. Distinguish between (a) explicit statements about zero or arithmetic completeness, (b) principles that logically entail zero-rejection, and (c) later scholastic interpretations that impose Parmenidean logic on zero questions. Historical documentation of whether zero-rejection was motivated by Parmenidean principle or by other factors (lack of exposure, computational inertia, theological preferences).',
    'If the reading represents genuine Parmenidean commitment: the constraint is authentically grounded in a philosophical tradition. If retroactive rationalization: the constraint is a cover story for institutional resistance to foreign mathematical methods. This affects the status of the foundational axioms (holdable vs overridden vs foreclosed) and the authority_grounding classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(this_reading_as_commitment_system_artifact, empirical, 'Whether zero-rejection is grounded in genuine Parmenidean commitment or retroactive rationalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parmenidean_rejection, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parm_tr_t0, parmenidean_rejection, theater_ratio, 0, 0.42).
narrative_ontology:measurement(parm_tr_t3, parmenidean_rejection, theater_ratio, 3, 0.5).
narrative_ontology:measurement(parm_tr_t6, parmenidean_rejection, theater_ratio, 6, 0.58).
narrative_ontology:measurement(parm_tr_t9, parmenidean_rejection, theater_ratio, 9, 0.7).

% Extraction over time
narrative_ontology:measurement(parm_be_t0, parmenidean_rejection, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(parm_be_t3, parmenidean_rejection, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(parm_be_t6, parmenidean_rejection, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(parm_be_t9, parmenidean_rejection, base_extractiveness, 9, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parmenidean_rejection, identity_coordination).
narrative_ontology:affects_constraint(parmenidean_rejection, placeholder_reading).
narrative_ontology:affects_constraint(parmenidean_rejection, number_reading).
narrative_ontology:affects_constraint(parmenidean_rejection, hindu_arabic_numeral_adoption).
narrative_ontology:affects_constraint(parmenidean_rejection, algebraic_completeness_construction).

% DUAL FORMULATION NOTE:
% The Parmenidean rejection is upstream of the placeholder reading (zero as notation, not number) and the number reading (zero as legitimate number). Both sibling readings emerge as responses to or against this constraint. The network also connects to institutional constraints (medieval scholasticism's enforcement, Hindu-Arabic numeral adoption barriers) that are downstream consequences of the Parmenidean framework. Each constraint in the family has its own epsilon value: the rejection itself has high extractiveness (0.68); the placeholder reading has lower extractiveness because it attempts coordination without full number status (estimated 0.35-0.45); the number reading has moderate extractiveness during its development (0.50-0.60) as it faces resistance. Decomposition was necessary because the observable (what zero IS) changes the constraint structure fundamentally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parmenidean_rejection, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
