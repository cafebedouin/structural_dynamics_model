% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__reoccupation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__reoccupation_reading, []).

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
 *   constraint_id: correct_latin_kernel__reoccupation_reading
 *   human_readable: Classical-Medieval Latin Discontinuity (Reoccupation Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The constraint emerges from a deep tension in historical linguistics:
 *   classical Latin and medieval Latin are conventionally treated as phases
 *   of a single language, but they differ so substantially in phonology,
 *   morphology, and syntactic structure that they can barely be understood as
 *   products of normal language change. The reoccupation reading resolves
 *   this by proposing that medieval scholars, confronted with
 *   incomprehensible classical texts, developed a new linguistic system
 *   (medieval Latin) while preserving the orthography, vocabulary, and
 *   surface forms of classical writing. Reoccupation is not repair of drift —
 *   it is the installation of a new system inside preserved symbols. This
 *   reading has structural consequences: if true, 'Latin' is not a single
 *   language but a homonym; all continuity narratives become contingent
 *   institutional facts rather than linguistic necessities; and medieval
 *   scholarship becomes the study of a distinct language rather than
 *   corrupted classical norm. The constraint operates as a tangled rope
 *   because it simultaneously enables (coordinates) medieval scholarship's
 *   autonomy and extracts (suppresses) the unified-Latin concept that had
 *   grounded centuries of pedagogy. The reoccupation reading benefits
 *   institutional medieval studies by legitimating its subject matter as a
 *   distinct rule-governed system; it victimizes the continuity narrative by
 *   rendering it empirically implausible; it imposes labor extraction on
 *   philological reconstruction schools by requiring expertise in two
 *   typologically distinct systems rather than one continuous evolution.
 *
 * KEY AGENTS:
 *   - Medieval Scholarship Tradition: Primary beneficiary (institutional/arbitrage) — gains methodological autonomy and legitimacy from reoccupation reading; can study medieval texts on their own linguistic terms
 *   - Unified Latin Hypothesis: Primary victim (powerless/trapped) — empirically undermined by reoccupation reading's typological claims; cannot exit the D1 discontinuity evidence
 *   - Continuity Reading: Secondary victim (moderate/constrained) — forced to explain away morphosyntactic ruptures through ad-hoc mechanisms; loses explanatory parsimony to reoccupation model
 *   - Philological Reconstruction Schools: Mixed (organized/constrained) — benefits from clear system boundaries but victimized by requirement to maintain parallel expertise
 *   - Classical Philology Establishment: Institutional actor (institutional/arbitrage) — maintains institutional authority but sees gatekeeping function degraded as classical standards no longer adjudicate medieval correctness
 *   - Analytical Observer: Civilizational view (analytical/analytical) — evaluates whether typological discontinuity is empirical fact or scholarly interpretive choice; risks false-summit classification if treating discontinuity as natural law rather than contingent theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__reoccupation_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__reoccupation_reading, 0.65).
domain_priors:theater_ratio(correct_latin_kernel__reoccupation_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__reoccupation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__reoccupation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(correct_latin_kernel__reoccupation_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__reoccupation_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__reoccupation_reading, "Classical-Medieval Latin Discontinuity (Reoccupation Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__reoccupation_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__reoccupation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__reoccupation_reading, '4b790274-a831-47c6-a719-85f90ce9f30e').
narrative_ontology:cs_kernel_codification('4b790274-a831-47c6-a719-85f90ce9f30e', fixed_text).
narrative_ontology:cs_authority_grounding('4b790274-a831-47c6-a719-85f90ce9f30e', lineage).
narrative_ontology:cs_interpretation_layer_present('4b790274-a831-47c6-a719-85f90ce9f30e').
narrative_ontology:cs_reading_relation('4b790274-a831-47c6-a719-85f90ce9f30e', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('4b790274-a831-47c6-a719-85f90ce9f30e', correct_latin_kernel__reconstructionist_reading, coexists_with).
narrative_ontology:cs_axiom('4b790274-a831-47c6-a719-85f90ce9f30e', foundational, phonological_collapse_d1_structural).
narrative_ontology:cs_axiom_status(phonological_collapse_d1_structural, holdable).
narrative_ontology:cs_axiom_grounding('4b790274-a831-47c6-a719-85f90ce9f30e', phonological_collapse_d1_structural, empirically_contingent).
narrative_ontology:cs_axiom('4b790274-a831-47c6-a719-85f90ce9f30e', foundational, medieval_scholars_reoccupation_not_repair).
narrative_ontology:cs_axiom_status(medieval_scholars_reoccupation_not_repair, holdable).
narrative_ontology:cs_axiom_grounding('4b790274-a831-47c6-a719-85f90ce9f30e', medieval_scholars_reoccupation_not_repair, deontological).
narrative_ontology:cs_reference_frame('4b790274-a831-47c6-a719-85f90ce9f30e', unified_latin_philological_tradition).
narrative_ontology:cs_drift_state('4b790274-a831-47c6-a719-85f90ce9f30e', contemporary_typological_linguistics, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4b790274-a831-47c6-a719-85f90ce9f30e', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__reoccupation_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__reoccupation_reading, medieval_scholarship_tradition).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__reoccupation_reading, philological_reconstruction_schools).
narrative_ontology:constraint_victim(correct_latin_kernel__reoccupation_reading, continuity_narrative).
narrative_ontology:constraint_victim(correct_latin_kernel__reoccupation_reading, unified_latin_concept).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE UNIFIED LATIN HYPOTHESIS (SNARE) — The intuitive assumption that Latin is a single continuous language from Cicero through Dante is structurally trapped by the reoccupation reading. The unified concept cannot exit the evidence showing D1 discontinuity; it bears the full cost of being empirically false while remaining institutionally dominant. Zero exit options: accepting the reoccupation claim requires abandoning the core explanatory framework.
constraint_indexing:constraint_classification(correct_latin_kernel__reoccupation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE CONTINUITY READING (TANGLED ROPE) — Acknowledges some rupture but seeks to minimize it through drift models, slow change, and substrate persistence. Constrained by the need to explain away phonological and morphological breaks without claiming two languages. Benefits from institutional continuity with traditional humanist scholarship; victimized by the reoccupation reading's parsimony. Moderate power — can mobilize historical arguments but lacks the reoccupation reading's typological clarity.
constraint_indexing:constraint_classification(correct_latin_kernel__reoccupation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDIEVAL SCHOLARSHIP TRADITION (ROPE) — Benefits from reoccupation reading's legitimation of medieval Latin as a distinct, rule-governed system rather than corruption of classical norms. Experiences the constraint as coordination: the reoccupation frame enables medieval texts to be read on their own terms without constant comparison to classical standards. Net beneficiary with institutional arbitrage — can mobilize reoccupation claims to advance methodological autonomy.
constraint_indexing:constraint_classification(correct_latin_kernel__reoccupation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHILOLOGICAL RECONSTRUCTION SCHOOLS (TANGLED ROPE) — Organized methodologically but constrained by resource requirements (manuscript access, computational analysis, expertise in both systems). Benefits from the reoccupation reading's provision of distinct system boundaries for reconstruction — enables targeted textual criticism. Victimized by the requirement to maintain parallel expertise in two typologically distinct systems rather than one continuous evolution. Significant extraction in labor burden; genuine coordination function in methodology.
constraint_indexing:constraint_classification(correct_latin_kernel__reoccupation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CLASSICAL PHILOLOGY ESTABLISHMENT (PITON) — Maintains institutional authority over 'correct Latin' standards, but the reoccupation reading degrades this authority: if medieval Latin is a distinct system, classical standards no longer adjudicate medieval correctness. The establishment's gatekeeping function persists through inertia (curricula, textbooks, canonical interpretations) rather than functional necessity. Theater ratio high — continued assertion of classical supremacy despite acknowledgment that it applies to a different language.
constraint_indexing:constraint_classification(correct_latin_kernel__reoccupation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TYPOLOGICAL NECESSITY (MOUNTAIN) — From the analytical perspective, the reoccupation reading asserts a structural linguistic fact: medieval Latin exhibits phonological, morphological, and syntactic properties that cannot be derived from classical Latin through normal language change without positing a discontinuity. The engine will evaluate this mountain classification for false-summit risk: does the 'typological necessity' frame naturalize a contingent scholarly interpretation, or does the evidence genuinely support immutable structure?
constraint_indexing:constraint_classification(correct_latin_kernel__reoccupation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__reoccupation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(correct_latin_kernel__reoccupation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(correct_latin_kernel__reoccupation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__reoccupation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(correct_latin_kernel__reoccupation_reading, TR),
    TR >= 0.70.

:- end_tests(correct_latin_kernel__reoccupation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reoccupation reading imposes significant costs on the unified-Latin narrative and the continuity reading by requiring a fundamental reclassification of what 'Latin' means. Institutional disruption is real — curricula, textbooks, and scholarly expertise structures built on the unified-Latin assumption must be reorganized. However, the extraction is not total because the reoccupation reading simultaneously offers benefits (methodological autonomy for medieval studies, typological clarity for linguistic analysis). The moderate-high value reflects that the reading consolidates scholarly authority around a new semantic framework at the cost of existing institutional arrangements. Suppression (0.65): Moderate-high. The reoccupation reading suppresses the intuitive continuity narrative through typological evidence (phonological collapse D1 is a major barrier to replicating classical-to-medieval transition without positing discontinuity). However, suppression is not total because the reconstructionist reading can still mobilize counterarguments (incomplete documentation, substrate effects, intermediate stages). Institutional suppression is significant — textbooks and curricula that treat medieval Latin as natural evolution of classical Latin implicitly suppress the reoccupation reading. Theater ratio (0.68): High. Much of the discourse around Latin continuity involves performative claims about 'correct' Latin and the 'natural' evolution from Cicero to Dante. The reoccupation reading's assertion that medieval scribes 'reoccupied' classical symbols rather than developing innovations is partly a semantic/institutional claim (about how scribes conceptualized their authority) and partly a typological claim (about actual language structure). The theater element is high because the reoccupation reading must perform the reclassification of 'Latin' as homonym while maintaining continuity in institutional/textual identity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The unified-Latin hypothesis sees a single continuous system (mountain from its perspective, but actually snare — trapped by the evidence). The medieval tradition sees autonomy and vindication (rope — coordination function in methodological independence). The philological schools see mixed benefit-extraction (tangled rope — genuine coordination of reconstruction alongside labor burden). The classical establishment sees its authority intact but degraded (piton — performative assertion of standards that no longer apply to medieval texts). The analytical observer risks naturalizing the reoccupation reading as a fact of linguistic typology (false-summit mountain) when it is actually a scholarly interpretive choice with institutional consequences. The perspectival gap reveals that 'correct Latin' is not a linguistic fact but a contested authority claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the unified-Latin hypothesis: d≈0.95 (full victim, no exit) — the hypothesis is trapped by discontinuity evidence and cannot escape reinterpretation. Directionality for medieval scholarship tradition: d≈0.15 (net beneficiary, arbitrage exit) — can mobilize reoccupation claims to advance methodological autonomy; has institutional alternative framing available. Directionality for continuity reading: d≈0.70 (moderate victim, constrained exit) — forced to explain away ruptures but retains argumentative resources; has institutional continuity with humanist scholarship tradition. Directionality for reconstruction schools: d≈0.55 (symmetric costs-benefits, constrained exit) — gains system clarity but loses unified expertise framework. The classical establishment's piton classification derives from degraded authority rather than high directionality — the theater gate indicates that classical standards persist institutionally despite losing linguistic applicability.
 *
 * MANDATROPHY ANALYSIS:
 *   The reoccupation reading resolves mandatrophy by asserting that the apparent conflict between 'Latin as one language' and 'Latin as two systems' dissolves once we acknowledge that 'Latin' is a homonym — a single name applied to two structurally distinct systems. The classical reading is empirically false; the continuity reading is empirically implausible; the reoccupation reading is the structurally correct interpretation. However, the analytical observer must assess whether the 'structural correctness' claim is itself a false summit — whether typological evidence truly demonstrates discontinuity or whether the reoccupation reading simply offers a more parsimonious interpretation of the same ambiguous data. The omega variables (discontinuity_mechanism_ambiguity, homonymy_vs_continuity_framing, reconstructionist_viability_threshold) provide the diagnostic structure for evaluating whether the reoccupation reading is empirically grounded or institutionally motivated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discontinuity_mechanism_ambiguity,
    'Is the D1 discontinuity a structural linguistic fact (two language systems) or a documentary/transmission artifact (lost continuity chain)?',
    'Phylogenetic analysis of feature inheritance; reconstruction of intermediate stages; examination of whether proposed connecting forms have attestation or are purely hypothetical. If intermediate stages can be reconstructed with independent phonological motivation, continuity is preserved. If intermediate stages are stipulated post-hoc, discontinuity is structural.',
    'If structural linguistic fact: reoccupation reading is correct; continuity and reconstructionist readings are both empirically false. If artifact: continuity reading gains support; reoccupation reading is methodologically sound but empirically weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuity_mechanism_ambiguity, empirical, 'Whether D1 discontinuity reflects language structure or documentary transmission gap').

omega_variable(
    homonymy_vs_continuity_framing,
    'Does calling classical and medieval systems by the same name ''Latin'' constitute legitimate linguistic shorthand or conceptual conflation that obscures structural typology?',
    'Pedagogical and institutional analysis: do textbooks, curricula, and scholarly practice treat the two systems as one evolving language or two distinct systems? Do students taught unified-Latin model make systematic errors when encountering medieval texts? Does reoccupation framing reduce error rates?',
    'If conflation is pedagogically harmful: the reoccupation reading''s institutional costs (maintaining parallel curricula, expertise division) are offset by cognitive clarity. If shorthand is pedagogically neutral or beneficial: reoccupation reading imposes extraction without epistemic gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homonymy_vs_continuity_framing, empirical, 'Whether homonymic naming causes systematic pedagogical errors').

omega_variable(
    reconstructionist_viability_threshold,
    'How much morphosyntactic divergence can the reconstructionist reading accommodate before the hypothesis of normal language change breaks and reoccupation becomes parsimoniuous?',
    'Typological comparison with documented language change; quantitative divergence metrics; testing whether proposed change mechanisms have contemporary parallels or require ad-hoc stipulation.',
    'If reconstructionist models remain viable under quantitative scrutiny: the continuity/reconstructionist readings retain credibility despite reoccupation''s semantic claim. If divergence exceeds normal change thresholds: reoccupation reading gains structural strength.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reconstructionist_viability_threshold, empirical, 'Threshold of morphosyntactic divergence incompatible with normal language change').

omega_variable(
    preserved_symbols_reading_necessity,
    'Is the reoccupation reading''s semantic claim — that medieval scribes re-occupied preserved classical symbols rather than developing innovations — a structural linguistic claim or a metaphor for institutional authority dynamics?',
    'Analysis of scribal awareness: do medieval texts show evidence that scribes explicitly referenced classical models as templates (reoccupation logic) or did they treat classical texts as authority for correctness claims without modeling contemporary usage on classical forms (preservation logic)? Manuscript evidence of glosses, emendations, and metalinguistic commentary.',
    'If structural linguistic claim: reoccupation reading asserts medieval innovation was constrained by classical template preservation — implies epistemic dependency. If metaphor for institutional authority: the reading''s semantic force is weaker; it describes authority relations, not linguistic structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preserved_symbols_reading_necessity, conceptual, 'Whether reoccupation is structural linguistic claim or institutional metaphor').

omega_variable(
    kernel_reading_foreclosure,
    'If the reoccupation reading is empirically correct (D1 discontinuity confirmed, homonymy unavoidable), does it logically foreclose the continuity reading, or can continuity and reoccupation coexist as frameworks for the same data?',
    'Formal assessment: can both readings claim coherence if discontinuity is acknowledged? Or does reoccupation''s core claim (two systems, not one) directly contradict continuity''s core claim (one language evolving)?',
    'If foreclosure confirmed: the three readings form a hierarchy with reoccupation at the apex; continuity and reconstructionist readings are empirically false. If coexistence possible: all three readings remain viable under different interpretive frameworks despite empirical disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether reoccupation reading logically forecloses the continuity reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__reoccupation_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(latin_reoccupation_tr_t0, correct_latin_kernel__reoccupation_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(latin_reoccupation_tr_t3, correct_latin_kernel__reoccupation_reading, theater_ratio, 3, 0.58).
narrative_ontology:measurement(latin_reoccupation_tr_t6, correct_latin_kernel__reoccupation_reading, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(latin_reoccupation_be_t0, correct_latin_kernel__reoccupation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(latin_reoccupation_be_t3, correct_latin_kernel__reoccupation_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(latin_reoccupation_be_t6, correct_latin_kernel__reoccupation_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(latin_reoccupation_su_t0, correct_latin_kernel__reoccupation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(latin_reoccupation_su_t3, correct_latin_kernel__reoccupation_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(latin_reoccupation_su_t6, correct_latin_kernel__reoccupation_reading, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__reoccupation_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin_kernel__reoccupation_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__reoccupation_reading, correct_latin_kernel__reconstructionist_reading).

% DUAL FORMULATION NOTE:
% The three readings of correct_latin_kernel are not separate constraints in the ε-invariance sense — they are alternative interpretations of the same empirical domain (the relationship between classical and medieval Latin). However, they differ structurally in their beneficiary/victim declarations, suppression mechanisms, and authority grounding. The reoccupation reading treats the relationship as homonymy (two systems); the continuity reading treats it as evolution; the reconstructionist reading treats it as underdetermined. These are three distinct constraint stories because each instantiates a different extractive/coordinative structure with different institutional consequences. Link them via network.affects_constraints to enable analysis of how challenging one reading affects the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__reoccupation_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
