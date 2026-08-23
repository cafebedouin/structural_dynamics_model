% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__parmenidean_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Parmenidean Rejection of Zero as a Number
 *   domain: history_of_mathematics/philosophy_of_mathematics/conceptual_history
 *
 * SUMMARY:
 *   The Parmenidean rejection of zero (c. 500 BCE – 1500 CE) is the
 *   constraint that 'nothing cannot exist, therefore zero cannot be a
 *   number.' It originates in Parmenides' poem (fr. 2, 6–8) where the goddess
 *   declares that non-being is unthinkable and unsayable. This metaphysical
 *   principle becomes the foundation for Greek mathematics' rejection of zero
 *   as a number (as opposed to a placeholder), transmitted through Plato,
 *   Aristotle, and the Neoplatonists into medieval Scholasticism. The
 *   constraint is actively enforced: Florence bans Arabic numerals in 1299;
 *   universities teach only Roman numerals and abacus; algorism texts are
 *   treated as suspect. The victims — astronomers needing sexagesimal
 *   computation, merchants needing efficient bookkeeping, reckoners needing
 *   algorithmic speed — bear the extraction. The beneficiaries —
 *   philosophical and theological establishments — gain metaphysical
 *   coherence at the cost of cognitive technology. The constraint persists
 *   long after its founding problem (defending Eleatic monism) is dead,
 *   making it a classic snare: the coordination story (metaphysical unity) is
 *   cover for extraction from practical calculation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__parmenidean_rejection, 0.75).
domain_priors:suppression_score(zero_mathematical_status__parmenidean_rejection, 0.8).
domain_priors:theater_ratio(zero_mathematical_status__parmenidean_rejection, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, extractiveness, 0.75).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(zero_mathematical_status__parmenidean_rejection, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__parmenidean_rejection, snare).
narrative_ontology:human_readable(zero_mathematical_status__parmenidean_rejection, "Parmenidean Rejection of Zero as a Number").
narrative_ontology:topic_domain(zero_mathematical_status__parmenidean_rejection, "history_of_mathematics/philosophy_of_mathematics/conceptual_history").

domain_priors:requires_active_enforcement(zero_mathematical_status__parmenidean_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__parmenidean_rejection, '277c596e-cf9f-41ed-8b9a-02d4cfa1c14a').
narrative_ontology:cs_kernel_codification('277c596e-cf9f-41ed-8b9a-02d4cfa1c14a', fixed_text).
narrative_ontology:cs_authority_grounding('277c596e-cf9f-41ed-8b9a-02d4cfa1c14a', lineage).
narrative_ontology:cs_interpretation_layer_present('277c596e-cf9f-41ed-8b9a-02d4cfa1c14a').
narrative_ontology:cs_reading_relation('277c596e-cf9f-41ed-8b9a-02d4cfa1c14a', zero_mathematical_status__number_reading, forecloses).
narrative_ontology:cs_reading_relation('277c596e-cf9f-41ed-8b9a-02d4cfa1c14a', zero_mathematical_status__placeholder_reading, influences).
narrative_ontology:cs_axiom('277c596e-cf9f-41ed-8b9a-02d4cfa1c14a', foundational, being_is_being_nonbeing_is_not).
narrative_ontology:cs_axiom_status(being_is_being_nonbeing_is_not, holdable).
narrative_ontology:cs_axiom_grounding('277c596e-cf9f-41ed-8b9a-02d4cfa1c14a', being_is_being_nonbeing_is_not, deontological).
narrative_ontology:cs_axiom('277c596e-cf9f-41ed-8b9a-02d4cfa1c14a', foundational, zero_denotes_nonbeing).
narrative_ontology:cs_axiom_status(zero_denotes_nonbeing, holdable).
narrative_ontology:cs_axiom_grounding('277c596e-cf9f-41ed-8b9a-02d4cfa1c14a', zero_denotes_nonbeing, empirically_contingent).
narrative_ontology:cs_reference_frame('277c596e-cf9f-41ed-8b9a-02d4cfa1c14a', parmenidean_ontology).
narrative_ontology:cs_drift_state('277c596e-cf9f-41ed-8b9a-02d4cfa1c14a', hindu_arabic_transmission, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('277c596e-cf9f-41ed-8b9a-02d4cfa1c14a', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, parmenidean_philosophers).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, neoplatonic_theologians).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__parmenidean_rejection, aristotelian_scholastics).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, hellenistic_astronomers).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, medieval_merchants).
narrative_ontology:constraint_victim(zero_mathematical_status__parmenidean_rejection, european_reckoners).
narrative_ontology:constraint_vindicates(zero_mathematical_status__parmenidean_rejection, being_is_being_nonbeing_is_not).
narrative_ontology:constraint_vindicates(zero_mathematical_status__parmenidean_rejection, unity_of_being_excludes_void).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and defend the metaphysical principle that 'what is not cannot be'; zero as a symbol of nothingness is therefore ontologically illegitimate. Their authority derives from founding a philosophical tradition that shapes Greek and later European thought. Exit from this framework requires abandoning the core metaphysical commitment.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, parmenidean_philosophers, agenda_setter,
    institutional, civilizational, identity_locked, universal).

% Incorporate the Parmenidean principle into Christian theology (creation ex nihilo vs. Greek eternity); the rejection of zero as a number reinforces the uniqueness of divine creation. They benefit from a metaphysics that distinguishes Creator from creation. Their professional and spiritual identity is fused with this framework.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, neoplatonic_theologians, beneficiary,
    institutional, civilizational, identity_locked, universal).

% Transmit and enforce the Aristotelian-Ptolemaic cosmology where void and nothingness are impossible; zero has no place in a physics of natural place and continuous magnitude. They control university curricula and ecclesiastical censorship. Their institutional position depends on the coherence of this system.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, aristotelian_scholastics, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__parmenidean_rejection, aristotelian_scholastics, beneficiary).

% Need efficient computation for planetary models and star catalogs but are denied zero as a placeholder and arithmetic operand; forced to use sexagesimal Babylonian systems without zero notation or Greek alphabetic numerals. Their professional credibility depends on predictive accuracy, which zero would improve.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, hellenistic_astronomers, payer,
    moderate, biographical, constrained, regional).

% Require efficient arithmetic for trade, banking, and double-entry bookkeeping; Roman numerals and abacus methods are slow and error-prone for complex calculations. They adopt Hindu-Arabic numerals clandestinely (e.g., Florentine merchants) but face legal prohibitions (1299 Florence ban). Their commercial survival pressures them toward the forbidden notation.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, medieval_merchants, payer,
    organized, biographical, constrained, regional).

% Professional calculators and teachers of arithmetic; their craft is hamstrung by the lack of zero in the dominant notation. They learn and teach both abacus methods and the new algorism, but the latter is suspect. Their livelihood depends on computational speed and accuracy, which the constraint degrades.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, european_reckoners, payer,
    moderate, biographical, constrained, regional).

% Develop and transmit a full positional decimal system with zero as a number (Brahmagupta, al-Khwarizmi, al-Kindi). Their system solves the coordination problem of efficient calculation. They are structurally excluded from the European university curriculum and ecclesiastical approval until the 12th-15th century translations.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, hindu_arabic_mathematicians, excluded,
    powerful, civilizational, mobile, global).

% Analyzes the constraint from outside its operative period; sees the full structural pattern — metaphysical commitment functioning as a barrier to cognitive technology adoption. No material stake in the outcome; exit is trivial (change research topic).
narrative_ontology:constraint_stakeholder(zero_mathematical_status__parmenidean_rejection, modern_historian, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a metaphysical framework in which being is plenary and non-being is impossible; this coordinates theological doctrine (creation ex nihilo), physical theory (horror vacui), and logical grammar (no null subject) across a civilizational tradition.
% TRANSFER_FUNCTION: Transfers computational efficiency and notational power from merchants, astronomers, and reckoners to the custodians of the metaphysical framework; the cost is paid in slower calculation, higher error rates, and delayed scientific development, while the benefit (metaphysical coherence) accrues to the philosophical-theological establishment.
% ABSENT_VOICES: The Hindu-Arabic mathematicians who had already solved the coordination problem of positional notation with zero; their voices were excluded by linguistic, geographic, and epistemic barriers, and later by active suppression (burning of algorism texts, university prohibitions).
% DISAPPEARANCE_RATIONALE: If the Parmenidean prohibition vanished overnight, European mathematics would have adopted Hindu-Arabic numerals centuries earlier; double-entry bookkeeping, algebraic notation, and the scientific revolution would accelerate; the theological framework would need to reinterpret creation ex nihilo without relying on the impossibility of zero.
% FOUNDING_PROBLEM: How to preserve the metaphysical principle that being is one, eternal, and plenary against the Heraclitean flux and the Milesian material monisms that implied generation from non-being; zero as a number would legitimate 'nothing' as a something.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (defending Parmenidean monism against pre-Socratic alternatives) is acknowledged as historically superseded by modern philosophers of ancient thought (e.g., Kirk & Raven, Graham) who note the problem was internal to 5th-century BCE metaphysics; no contemporary Parmenidean scholar claims the original problem is live. The constraint persisted 2000 years after its founding problem dissolved.
narrative_ontology:disappearance_verdict(zero_mathematical_status__parmenidean_rejection, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__parmenidean_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__parmenidean_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_mathematical_status__parmenidean_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__parmenidean_rejection, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness 0.75: the constraint denies a cognitive tool (zero) that reduces computational complexity from O(n) to O(log n) for arithmetic, imposing massive cumulative costs on every calculation-intensive activity. Suppression 0.8: active prohibitions (Florence 1299, university statutes), censorship of translations, and the theological equation of zero with heretical void. Theater 0.4: early period has genuine metaphysical debate; by 1200 CE the debate is performative — the philosophical establishment knows Hindu-Arabic numerals work but suppresses them to protect authority. Accessibility collapse 0.85: alternatives (abacus, Roman numerals, sexagesimal without zero) fail catastrophically for algebra, calculus, and large-scale commerce. Resistance 0.7: Fibonacci's Liber Abaci (1202), merchant adoption despite bans, astronomical tables using Hindu-Arabic numerals — the constraint requires increasing enforcement to hold.
 *
 * PERSPECTIVAL GAP:
 *   From the Scholastic seat: the constraint is a genuine coordination mechanism preserving the integrity of Christian metaphysics against the category error of treating nothing as something. From the merchant/astronomer seat: the same structure is an enforced barrier to the cognitive technology their work requires. The engine computes this divergence from the structural data — the agenda_setter's identity_locked exit and the payer's constrained exit produce opposite effective extraction values from the same base ε.
 *
 * DIRECTIONALITY LOGIC:
 *   Parmenidean philosophers and Scholastics are agenda_setters with identity_locked exit (their professional identity is the framework) — d ≈ 0.1 (beneficiary end, they gain authority from the constraint). Neoplatonic theologians are beneficiaries with identity_locked exit — d ≈ 0.15. Hellenistic astronomers, medieval merchants, European reckoners are payers with constrained exit (they cannot leave their profession or region easily) — d ≈ 0.8. Hindu-Arabic mathematicians are excluded with mobile exit (they operate in a different civilizational sphere) — d ≈ 0.3 but structurally outside the constraint's direct operation. Modern historian is analytical — d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defending Eleatic monism against 5th-century BCE alternatives) died by the 3rd century BCE; the constraint persisted 1800 more years. The mandatrophy is resolved: the arrangement is a zombie, maintained by institutional inertia (universities, ecclesiastical censorship) and identity fusion (theologians cannot abandon the framework without abandoning their vocation). The snare classification prevents mislabeling this as coordination — there is no collective-action problem being solved; the 'coordination' of metaphysical doctrine is a one-sided imposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'How does the parmenidean_rejection reading structurally relate to the number_reading and placeholder_reading of the zero_mathematical_status kernel?',
    'Map the logical relations between the three readings: does parmenidean_rejection foreclose number_reading (mutually exclusive ontologies)? Does it influence placeholder_reading (creating pressure against even notational use)? The engine computes foreclosure from axiom contradiction; authoring the relations here documents the committer structure.',
    'If parmenidean_rejection forecloses number_reading, they cannot coexist in a single framework — the historical transition requires framework replacement. If it only influences placeholder_reading, the notational use can persist as a pragmatic workaround (as historically occurred with ''ciphers'').',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relations between sibling readings of the zero_mathematical_status kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of zero structural (ecclesiastical bans, university statutes) or internalized (mathematicians believing zero is metaphysically suspect)?',
    'Post-adoption suppression trajectory: after Hindu-Arabic numerals are adopted in Europe (15th c.), does residual resistance persist in philosophical theology? If yes, internalized component is significant.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression intellectually even after legal barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the zero prohibition').

omega_variable(
    coordination_function_genuineness,
    'Does the Parmenidean framework genuinely coordinate a civilizational metaphysics, or is the coordination story post-hoc rationalization for a constraint whose primary function is protecting institutional authority?',
    'Counterfactual: if the metaphysical framework were genuinely coordinating, its dissolution should produce fragmentation. The Reformation and scientific revolution proceeded without Parmenidean ontology — suggesting the coordination was not load-bearing.',
    'If coordination is not genuine, the constraint is a pure snare from inception; if genuine but atrophied, it is a snare with mandatrophy (coordination function dead, extraction persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_genuineness, conceptual, 'Whether the metaphysical coordination function is load-bearing or cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__parmenidean_rejection, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_parmenidean_tr_t0, zero_mathematical_status__parmenidean_rejection, theater_ratio, 0, 0.1).
narrative_ontology:measurement(zero_parmenidean_tr_t400, zero_mathematical_status__parmenidean_rejection, theater_ratio, 400, 0.2).
narrative_ontology:measurement(zero_parmenidean_tr_t800, zero_mathematical_status__parmenidean_rejection, theater_ratio, 800, 0.3).
narrative_ontology:measurement(zero_parmenidean_tr_t1200, zero_mathematical_status__parmenidean_rejection, theater_ratio, 1200, 0.38).
narrative_ontology:measurement(zero_parmenidean_tr_t1600, zero_mathematical_status__parmenidean_rejection, theater_ratio, 1600, 0.4).
narrative_ontology:measurement(zero_parmenidean_tr_t2000, zero_mathematical_status__parmenidean_rejection, theater_ratio, 2000, 0.4).

% Extraction over time
narrative_ontology:measurement(zero_parmenidean_be_t0, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(zero_parmenidean_be_t400, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 400, 0.45).
narrative_ontology:measurement(zero_parmenidean_be_t800, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 800, 0.6).
narrative_ontology:measurement(zero_parmenidean_be_t1200, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 1200, 0.72).
narrative_ontology:measurement(zero_parmenidean_be_t1600, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 1600, 0.75).
narrative_ontology:measurement(zero_parmenidean_be_t2000, zero_mathematical_status__parmenidean_rejection, base_extractiveness, 2000, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(zero_parmenidean_su_t0, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(zero_parmenidean_su_t400, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 400, 0.55).
narrative_ontology:measurement(zero_parmenidean_su_t800, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 800, 0.7).
narrative_ontology:measurement(zero_parmenidean_su_t1200, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 1200, 0.8).
narrative_ontology:measurement(zero_parmenidean_su_t1600, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 1600, 0.8).
narrative_ontology:measurement(zero_parmenidean_su_t2000, zero_mathematical_status__parmenidean_rejection, suppression_requirement, 2000, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__parmenidean_rejection, identity_coordination).
narrative_ontology:boltzmann_floor_override(zero_mathematical_status__parmenidean_rejection, 0.08).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__parmenidean_rejection, zero_mathematical_status__placeholder_reading).

% DUAL FORMULATION NOTE:
% This constraint, zero_mathematical_status__number_reading, and zero_mathematical_status__placeholder_reading form a constraint family decomposing the colloquial label 'the status of zero.' They have different ε values (this: 0.75 extractive; number_reading: near 0; placeholder_reading: low extractive, coordination-dominant) and different victim/beneficiary structures. The parmenidean_rejection influences the placeholder_reading by creating epistemic pressure against even notational use; it forecloses the number_reading within any single metaphysical framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zero_mathematical_status__parmenidean_rejection, institutional, 0.1).
constraint_indexing:directionality_override(zero_mathematical_status__parmenidean_rejection, moderate, 0.8).
constraint_indexing:directionality_override(zero_mathematical_status__parmenidean_rejection, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
