% ============================================================================
% CONSTRAINT STORY: correct_latin__prescriptive_ideal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__prescriptive_ideal_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: correct_latin__prescriptive_ideal_reading
 *   human_readable: Ciceronian Prescriptive Ideal: Correct Latin as Frozen Classical Form
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The prescriptive Ciceronian ideal, formalized by Pietro Bembo in the
 *   early 16th century and institutionalized across European humanist
 *   culture, established 'correct Latin' as strict imitation of Cicero's
 *   texts, excluding all post-Classical vocabulary, syntax, and forms. This
 *   reading of the 'correct_latin' kernel defines Latin correctness through a
 *   prescriptive standard that froze the language at a single historical
 *   moment, rupturing both medieval practice (which had evolved Latin
 *   continuously) and the natural adaptive capacity that allowed Latin to
 *   remain a living communication medium. The constraint exhibits high
 *   extractiveness (0.68) because it requires constant policing of forms,
 *   dual-register maintenance (Ciceronian for prestige, vernacular for
 *   function), and enforcement against the grain of natural language
 *   evolution. Primary beneficiaries are the humanist elite (Bembo, Sadoleto,
 *   the printing establishment) who arbitrage access to Ciceronian education
 *   and lock in status through standardization. Primary victims are
 *   vernacular Latinists (scribes, lower clergy, notaries) whose evolved
 *   forms are declared corrupt, and the language's own adaptive capacity,
 *   which is suppressed by the prescriptive gate. The constraint's theater
 *   ratio rises from 0.35 (functional phase, 16th century) to 0.78 (ritual
 *   only, 18th century), indicating steady degradation toward pure
 *   performative enforcement. This is the snare reading of the
 *   'correct_latin' kernel — it emphasizes extraction, suppression of
 *   alternatives, and the asymmetric burden on those trapped between
 *   institutional requirement and prescriptive impossibility.
 *
 * KEY AGENTS:
 *   - Pietro Bembo and humanist elite: Primary beneficiaries (institutional/arbitrage) — establish the Ciceronian standard; their prior education already matches the standard; benefit from locking in access gates
 *   - Printing establishment (Aldus Manutius, Robert Estienne): Organized beneficiaries (organized/constrained) — standardization aids reproducible print editions; benefit from reduced textual variance; face sunset as living Latin evolves
 *   - Vernacular Latinists (scribes, notaries, lower clergy): Primary victims (powerless/trapped) — evolved Latin forms declared corrupt; cannot exit (institutional requirement) nor meet standard (educational barriers); labeled incompetent despite practical function
 *   - Medieval Latin practitioners and traditions: Structural victim (powerless/trapped) — entire tradition of post-Classical Latin practice labeled degenerate and systematically suppressed
 *   - Linguistic vitality and adaptive capacity: Abstract victim (powerless/trapped) — the language's ability to evolve in response to communicative needs is explicitly suppressed by the prescriptive standard
 *   - Universities (18th century onward): Institutional maintainer (institutional/arbitrage) — enforce Ciceronian standards through certification gates despite obvious atrophy of the constraint's functionality; maintain through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__prescriptive_ideal_reading, 0.68).
domain_priors:suppression_score(correct_latin__prescriptive_ideal_reading, 0.72).
domain_priors:theater_ratio(correct_latin__prescriptive_ideal_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__prescriptive_ideal_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(correct_latin__prescriptive_ideal_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(correct_latin__prescriptive_ideal_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__prescriptive_ideal_reading, snare).
narrative_ontology:human_readable(correct_latin__prescriptive_ideal_reading, "Ciceronian Prescriptive Ideal: Correct Latin as Frozen Classical Form").
narrative_ontology:topic_domain(correct_latin__prescriptive_ideal_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__prescriptive_ideal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__prescriptive_ideal_reading, 'c5157de2-501d-47a2-8a80-11f79715a605').
narrative_ontology:cs_kernel_codification('c5157de2-501d-47a2-8a80-11f79715a605', formalized).
narrative_ontology:cs_authority_grounding('c5157de2-501d-47a2-8a80-11f79715a605', extraction).
narrative_ontology:cs_interpretation_layer_present('c5157de2-501d-47a2-8a80-11f79715a605').
narrative_ontology:cs_reading_relation('c5157de2-501d-47a2-8a80-11f79715a605', correct_latin__living_drift_reading, forecloses).
narrative_ontology:cs_reading_relation('c5157de2-501d-47a2-8a80-11f79715a605', correct_latin__textual_recovery_reading, influences).
narrative_ontology:cs_axiom('c5157de2-501d-47a2-8a80-11f79715a605', foundational, ciceronian_supremacy_invariant).
narrative_ontology:cs_axiom_status(ciceronian_supremacy_invariant, holdable).
narrative_ontology:cs_axiom_grounding('c5157de2-501d-47a2-8a80-11f79715a605', ciceronian_supremacy_invariant, deontological).
narrative_ontology:cs_axiom('c5157de2-501d-47a2-8a80-11f79715a605', foundational, post_classical_exclusion_imperative).
narrative_ontology:cs_axiom_status(post_classical_exclusion_imperative, overridden).
narrative_ontology:cs_axiom_grounding('c5157de2-501d-47a2-8a80-11f79715a605', post_classical_exclusion_imperative, empirically_contingent).
narrative_ontology:cs_reference_frame('c5157de2-501d-47a2-8a80-11f79715a605', ciceronian_supremacy_framework).
narrative_ontology:cs_drift_state('c5157de2-501d-47a2-8a80-11f79715a605', enlightenment_era_university_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5157de2-501d-47a2-8a80-11f79715a605', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(correct_latin__prescriptive_ideal_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__prescriptive_ideal_reading, ciceronian_elite).
narrative_ontology:constraint_beneficiary(correct_latin__prescriptive_ideal_reading, court_humanists).
narrative_ontology:constraint_beneficiary(correct_latin__prescriptive_ideal_reading, printing_establishment).
narrative_ontology:constraint_victim(correct_latin__prescriptive_ideal_reading, vernacular_latinists).
narrative_ontology:constraint_victim(correct_latin__prescriptive_ideal_reading, linguistic_vitality).
narrative_ontology:constraint_victim(correct_latin__prescriptive_ideal_reading, adaptive_capacity).
narrative_ontology:constraint_victim(correct_latin__prescriptive_ideal_reading, post_classical_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VERNACULAR LATINIST (SNARE) — A scribe, notary, or lower clergy attempting to use Latin for practical communication faces immediate judgment and exclusion. The prescriptive ideal traps them: their evolved forms (incorporating vernacular syntax, medieval vocabulary, practical innovations) are declared corrupt. No exit exists; they cannot stop using Latin (institutional requirement) nor can they satisfy the Ciceronian standard (requires decades of elite education). Maximum extraction: their labor is rendered illegitimate while still required.
constraint_indexing:constraint_classification(correct_latin__prescriptive_ideal_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: EDUCATED CLERIC STRADDLING REGISTERS (TANGLED ROPE) — Clergy educated in medieval Latin who must now master Ciceronian forms for advancement coordinate practical ecclesiastical communication (genuine function) while bearing extraction: they must maintain two registers (medieval for function, Ciceronian for prestige), face ongoing judgment, and risk career damage if found using post-Classical forms. Both coordination and extraction are real; exit is costly but technically possible (specializing in one register).
constraint_indexing:constraint_classification(correct_latin__prescriptive_ideal_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HUMANIST ELITE — BEMBO, SADOLETO (ROPE) — Primary beneficiaries. For them the constraint is pure coordination: establishing Ciceronian as standard solves the problem of intra-elite communication clarity and status differentiation. They arbitrage access — their educational pedigree gave them Ciceronian fluency before the standard was formalized. The standard now locks in their advantage. Suppression and extraction run toward this group as net benefit; they experience the constraint as a coordination mechanism that proved they were right all along.
constraint_indexing:constraint_classification(correct_latin__prescriptive_ideal_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: PRINTING ESTABLISHMENT — ALDUS MANUTIUS, ESTIENNE (SCAFFOLD) — Organized actors who benefit from standardized Ciceronian norms (standardization aids print production, reduces variant editions, enables market scale) but face sunset pressure: as the 16th–17th centuries progress, the living reality of Latin continues to evolve, and prescriptive enforcement becomes increasingly theatrical. The constraint has a sunset built in: natural language evolution will eventually override prescriptive force, at which point the standard becomes merely one historical register among others. Low effective extraction because these actors see the exit path (eventual naturalization of plural Latins).
constraint_indexing:constraint_classification(correct_latin__prescriptive_ideal_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: UNIVERSITY AS INSTITUTIONAL INERTIA (PITON) — By the 18th century, universities continued enforcing Ciceronian standards despite the obvious reality that living Latin had evolved beyond recovery. The institutional function (teaching students to write correctly) had atrophied — universities now enforced a ritual form that nobody actually spoke. Theater ratio: 0.65. The university continued the Ciceronian gate because it had become the definition of a university, not because the standard served any remaining coordination function. The constraint persisted through institutional inertia and credentialing authority, not through genuine utility.
constraint_indexing:constraint_classification(correct_latin__prescriptive_ideal_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — LINGUISTICALLY INFORMED (TANGLED ROPE) — From a linguistic standpoint, the Ciceronian ideal coordinates elite communication (rope function) while extracting from speakers by freezing the language (snare function). The constraint is genuinely hybrid: it solves the problem of intra-elite clarity while it suppresses adaptive language evolution. This reading acknowledges both the coordination and extraction genuinely present in the same structure.
constraint_indexing:constraint_classification(correct_latin__prescriptive_ideal_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__prescriptive_ideal_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(correct_latin__prescriptive_ideal_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(correct_latin__prescriptive_ideal_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__prescriptive_ideal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(correct_latin__prescriptive_ideal_reading, TR),
    TR >= 0.70.

:- end_tests(correct_latin__prescriptive_ideal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The prescriptive ideal creates a dual-register burden on non-elite speakers: they must use Latin for institutional function but cannot meet the Ciceronian standard without decades of elite education. This is pure extraction — the prescriptive standard provides no coordination benefit to the powerless; it only certifies their incompetence. The beneficiaries (humanist elite, printers) capture status and control through the standard. Suppression (0.72): High. The constraint suppresses alternatives through institutional gatekeeping (university certification), discursive delegitimization (medieval Latin labeled corrupt), and the logical suppression of natural language evolution (the prescriptive ideal explicitly forbids adaptive forms). Exit options are severely constrained. Theater ratio trajectory (0.35 → 0.65 → 0.78): Rising. Initially (Bembo era), the Ciceronian standard performed a genuine function: clarifying elite communication and establishing a unified literary standard. By the 16th–17th centuries, the theater rises as living Latin continued to evolve and the prescriptive standard became increasingly unmoored from practice. By the 18th century, universities enforced Ciceronian standards despite everyone knowing the standard was historically dead. The ritual persisted through institutional inertia, not utility.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The humanist elite see rope (coordination) — establishing a clear standard solves their communication problem. The vernacular Latinist sees snare (pure extraction) — the standard traps them in a no-win situation. The organized printing establishment sees scaffold (temporary coordination with sunset) — the standard is useful now but will eventually be overtaken by living Latin evolution. The university in the 18th century sees piton (degraded ritual) — the standard persists through institutional inertia despite having lost its function. The analytical observer sees tangled rope (hybrid coordination-extraction) — the prescriptive ideal simultaneously solves an elite coordination problem and extracts from non-elite speakers. No single perspective is wrong; the constraint genuinely functions differently from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position within the constraint. Bembo and the elite start at d ≈ 0.15 (beneficiary + arbitrage exit → low d → negative f(d)) — they experience the constraint as enabling, not extracting. Vernacular Latinists are at d ≈ 0.92 (victim + trapped exit → high d → f(d) ≈ 1.38) — they experience maximum extraction. The printing establishment is at d ≈ 0.55 (moderate beneficiary benefit + constrained exit → moderate d → f(d) ≈ 0.75) — they benefit but face sunset pressure. The university at the analytical level is at d ≈ 0.72 (administrative maintainer + arbitrage → high-moderate d → f(d) ≈ 1.15) — the university arbitrages credentialing authority but the constraint itself is not extracting from them. The analytical observer derives d from the structural gap between beneficiaries and victims, yielding d ≈ 0.72 (the population-wide average of extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the prescriptive reading itself INSTANTIATES the extraction it models. The snare classification is not just an analytical conclusion — it is what the prescriptive framework DOES to non-elite speakers. The snare is the point. The constraint's high extractiveness (0.68) and suppression (0.72) are not flaws in the prescriptive model; they are the mechanism through which the prescriptive standard works. The reading that declares 'correct Latin is Ciceronian imitation' necessarily creates an extractive gate because it declares everything else corrupt. The mandatrophy resolves: the prescriptive reading is not mislabeled; it is rightly classified as snare. The extractiveness value reflects the structural cost that the reading imposes on those who cannot meet its standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ciceronian_criterion_definition,
    'What criteria determine membership in ''Ciceronian''? Is it imitation of Cicero''s specific texts, or conformity to an extractable grammar derived from Cicero?',
    'Historical analysis of prescriptive rules applied across different authors claiming Ciceronian authority (Bembo''s Prose della Volgar Lingua, Erasmus''s Dialogus). Evidence of disagreement on specific forms reveals the criterion ambiguity.',
    'If criterion is text-imitation: constraint requires constant interpretive enforcement. If criterion is extractable grammar: constraint is more stable but faces immediate counterexamples (Cicero himself violates the rules extracted from Cicero).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ciceronian_criterion_definition, empirical, 'Definition of what constitutes ''Ciceronian'' correctness').

omega_variable(
    medieval_practice_suppression_mechanism,
    'Is the suppression of medieval Latin forms structural (e.g., no institutional pathways for medieval practice) or primarily discursive (medieval forms are labeled corrupt but continue in practice)?',
    'Comparison of prescriptive rules (rules stated in grammar treatises) vs. actual practice in contemporary documents. If suppression is structural, medieval forms should disappear from formal writing. If suppression is discursive, they should persist despite being labeled corrupt.',
    'If structural suppression: snare classification confirmed (exit is physically unavailable). If discursive: tangled_rope more likely (suppression is performative and can be evaded through pragmatic accommodation). Affects the ''trapped'' status of the vernacular latinist perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_practice_suppression_mechanism, empirical, 'Whether suppression of medieval Latin is structural or discursive').

omega_variable(
    prescriptive_vs_living_reading_foreclosure,
    'Does the prescriptive ideal reading logically foreclose the living drift reading, or do both coexist as held by different parties?',
    'Philosophical analysis: can a single framework (e.g., ''correct Latin is what the best writers do'') hold both readings without contradiction? Or does one reading''s core premise directly eliminate the other?',
    'If forecloses: the kernel shows genuine logical incompatibility. If coexists: the readings are different normative commitments held by different factions without logical resolution. Shapes the ''reading_relations'' declaration in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prescriptive_vs_living_reading_foreclosure, conceptual, 'Whether prescriptive and living readings logically foreclose each other').

omega_variable(
    ciceronian_elite_beneficiary_identification,
    'Who exactly are the ''Ciceronian elite'' beneficiaries? Is benefit concentrated (e.g., only Bembo and Sadoleto) or distributed across a broader humanist class?',
    'Prosopographic analysis: track which authors explicitly adopted Ciceronianism, which benefited from its establishment (through patronage, positions, prestige), and when. Map the benefit distribution across regions and time periods.',
    'If benefit is concentrated: constraint is designed to extract for a small group (snare from victim perspective confirmed). If distributed: constraint may be a coordination mechanism that happened to benefit an elite (rope logic). Affects directionality calculations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ciceronian_elite_beneficiary_identification, empirical, 'Scope and distribution of beneficiary group').

omega_variable(
    reading_kernel_separation,
    'Is this constraint distinctly a reading of the ''correct_latin'' kernel, or does its extractiveness value (0.68) reflect a different constraint entirely (e.g., ''elite gatekeeping in Renaissance humanities'')?',
    'Remove the kernel framing and evaluate whether this constraint persists as a structural claim about Latin. If the constraint''s structure is inseparable from the kernel identity claim, the reading frame is authentic. If the constraint would exist identically under a non-kernel framing, the kernel reading may be redundant.',
    'If authentic reading: committer frame is appropriate; axioms and reading_relations are meaningful. If redundant: the constraint may be better modeled as a non-kernel snare with no kernel_context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_separation, conceptual, 'Whether the reading frame is ontologically distinct from non-kernel modeling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__prescriptive_ideal_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clpir_theater_t0_functional_phase, correct_latin__prescriptive_ideal_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(clpir_theater_t50_degradation, correct_latin__prescriptive_ideal_reading, theater_ratio, 50, 0.65).
narrative_ontology:measurement(clpir_theater_t100_ritual_only, correct_latin__prescriptive_ideal_reading, theater_ratio, 100, 0.78).

% Extraction over time
narrative_ontology:measurement(clpir_extractiveness_t0_bembo_establishment, correct_latin__prescriptive_ideal_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clpir_extractiveness_t50_peak_enforcement, correct_latin__prescriptive_ideal_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(clpir_extractiveness_t100_university_ossification, correct_latin__prescriptive_ideal_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clpir_suppression_t0_initial, correct_latin__prescriptive_ideal_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(clpir_suppression_t50_peak, correct_latin__prescriptive_ideal_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(clpir_suppression_t100_declining_efficacy, correct_latin__prescriptive_ideal_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__prescriptive_ideal_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin__prescriptive_ideal_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin__prescriptive_ideal_reading, correct_latin__living_drift_reading).
narrative_ontology:affects_constraint(correct_latin__prescriptive_ideal_reading, correct_latin__textual_recovery_reading).
narrative_ontology:affects_constraint(correct_latin__prescriptive_ideal_reading, elite_gatekeeping_renaissance_literacy).
narrative_ontology:affects_constraint(correct_latin__prescriptive_ideal_reading, dual_register_maintenance_burden).

% DUAL FORMULATION NOTE:
% The prescriptive ideal reading is part of the 'correct_latin' kernel family. The sibling reading 'living_drift_reading' has a lower ε value (estimated 0.35) because it frames correctness as emerging from use rather than prescription. The 'textual_recovery_reading' has a moderate ε value (estimated 0.52) because it balances the effort to recover ancient forms against natural language evolution. All three readings share the same kernel (what makes Latin correct?) but produce different ε values because they answer the kernel question differently, creating different structural burdens on different agents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__prescriptive_ideal_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
