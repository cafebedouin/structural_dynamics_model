% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__continuity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: correct_latin_kernel__continuity_reading
 *   human_readable: Medieval Latin as Legitimate Continuity — Kernel Reading
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint story instantiates the continuity reading of the
 *   correct_latin_kernel: the claim that Medieval Latin is Classical Latin
 *   after natural linguistic evolution, and that reconstruction of medieval
 *   practice is internal correction within a continuous tradition. The
 *   reading frames medieval innovations (neologisms, morphological leveling,
 *   syntax adaptation) as legitimate developments stemming from classical
 *   foundations rather than as departures requiring separate genealogy. The
 *   constraint operates by treating the continuity assumption as natural and
 *   structurally transparent, while rendering discontinuity and
 *   reconstruction approaches as requiring special justification and external
 *   proof. Medieval clerical scholars benefit from this reading because their
 *   linguistic choices are legitimized as natural development. Discontinuity
 *   researchers and reconstructionist philologists bear the burden of proof
 *   against an institutionally embedded default. The kernel_context
 *   identifies this as one reading of a contested kernel shared with
 *   discontinuity_reading and hybrid_reading constraints.
 *
 * KEY AGENTS:
 *   - medieval_clerical_scholars: institutional authority transmitting and extending the Latin tradition (agenda_setter, organized/generational/constrained)
 *   - humanist_reformers: powerful scholars who authored the continuity frame as a solution to the divergence problem they discovered (powerful/biographical/mobile)
 *   - discontinuity_researchers: modern philologists arguing for systematic separation of medieval and classical Latin systems (moderate/biographical/mobile)
 *   - reconstructionist_philologists: scholars treating medieval forms as legitimate data for linguistic history (moderate/biographical/mobile)
 *   - classical_studies_tradition: institutional beneficiary preserving prestige hierarchy of classical forms (institutional/generational/constrained)
 *   - manuscript_record: empirical foundation for all readings — the primary data whose interpretation is the constraint (observer, analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, 0.62).
domain_priors:suppression_score(correct_latin_kernel__continuity_reading, 0.71).
domain_priors:theater_ratio(correct_latin_kernel__continuity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__continuity_reading, "Medieval Latin as Legitimate Continuity — Kernel Reading").
narrative_ontology:topic_domain(correct_latin_kernel__continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__continuity_reading, '7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513').
narrative_ontology:cs_kernel_codification('7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513', fixed_text).
narrative_ontology:cs_authority_grounding('7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513', lineage).
narrative_ontology:cs_interpretation_layer_present('7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513').
narrative_ontology:cs_reading_relation('7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513', correct_latin_kernel__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513', foundational, medieval_linguistic_development_inherits_classical).
narrative_ontology:cs_axiom_status(medieval_linguistic_development_inherits_classical, holdable).
narrative_ontology:cs_axiom_grounding('7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513', medieval_linguistic_development_inherits_classical, deontological).
narrative_ontology:cs_axiom('7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513', foundational, reconstruction_as_internal_correction).
narrative_ontology:cs_axiom_status(reconstruction_as_internal_correction, holdable).
narrative_ontology:cs_axiom_grounding('7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513', reconstruction_as_internal_correction, conventional).
narrative_ontology:cs_reference_frame('7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513', unbroken_latin_lineage).
narrative_ontology:cs_drift_state('7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513', contemporary_historical_linguistics, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7c5b03c0-4f9d-4aa8-aba7-00e0b0cae513', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__continuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, medieval_clerical_scholars).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, continuity_narrative_tradition).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, discontinuity_researchers).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, reconstructionist_philologists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, classical_studies_tradition).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, humanist_reformers).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, linguistic_naturalism_doctrine).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, historical_organic_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Throughout the medieval period, Latin-literate clergy maintained and extended a living written tradition from antiquity. They adapted vocabulary, syntax, and morphology to express Christian doctrine, legal concepts, and administrative needs. They authorized these innovations as natural developments within a continuous Latin tradition, not as departures requiring special justification. Their authority derived from institutional (Church) position and textual lineage back to patristic sources.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, medieval_clerical_scholars, agenda_setter,
    organized, generational, constrained, continental).

% From the 14th century onward, humanist scholars insisted on a sharp break between Classical (Ciceronian, Augustan) Latin and the medieval vulgarized form. They positioned themselves as restorers of pure Classical usage and dismissed medieval innovations as errors requiring correction. They bore the analytical and rhetorical cost of maintaining the discontinuity claim against the historical evidence of continuous manuscript transmission and organic development.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, humanist_reformers, payer,
    powerful, biographical, mobile, regional).

% Modern historical linguists and philologists who treat Medieval and Classical Latin as separate linguistic systems, each with its own rules. They argue reconstruction is necessary to recover the actual medieval system from texts shaped by scribal error, hypercorrection, and shifting orthographic conventions. They bear the burden of proof for discontinuity claims and face institutional pressure from the continuity narrative embedded in university curricula and classical studies prestige.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, discontinuity_researchers, payer,
    moderate, biographical, mobile, national).

% Scholars committed to recovering the actual linguistic practices of medieval scribes and writers through systematic textual analysis. They treat medieval forms as legitimate data for linguistic history, not as corruption. They pay the cost of detailed manuscript work and challenge the prestige hierarchy that treats only Classical forms as authoritative. Their work is methodologically rigorous but institutionally marginal within classics.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, reconstructionist_philologists, payer,
    moderate, biographical, mobile, national).

% The institutional framework of classical philology that privileges Ciceronian Latin as the apex of the tradition and treats medieval Latin as a decline to be transcended. The continuity reading (as constraint) protects this hierarchy by rendering medieval innovations as continuous corrections of itself, keeping medieval material subordinate to classical standards. Universities structure classics curricula around the continuity narrative without requiring detailed engagement with the linguistic evidence.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, classical_studies_tradition, beneficiary,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__continuity_reading, classical_studies_tradition).
narrative_ontology:fixing_cost_class(correct_latin_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single evaluative framework in which medieval innovations (neologisms, syntax shifts, case erosion) are treated as internal corrections and developments within a continuous Latin tradition rather than departures requiring separate genealogy. This frames the work of medieval scribes and writers as participating in one historical language across centuries, not as inventing a new system.
% TRANSFER_FUNCTION: Transfers interpretive authority from medieval writers (whose choices are legitimized as natural development) to classical-era authors (whose forms remain the standard against which medieval work is measured). Medieval innovations are valid because they develop from classical foundations, but classical practice retains prestige and normativity. Scholars advocating discontinuity or reconstruction bear the burden of proof against the default continuity assumption.
% ABSENT_VOICES: The voices of medieval writers themselves would object if asked directly: they did not consistently claim to be 'developing classical Latin' but adapted their language to present needs and audience. Paleographers and codicologists who treat medieval manuscripts as primary linguistic sources rather than corrupted copies of classical texts are structurally excluded from setting the interpretive framework; the continuity reading subordinates their evidence to the narrative of unbroken descent.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, the field would reorganize around discontinuity and reconstruction models within one scholarly generation. Medieval Latin would be taught as a distinct historical system with its own phonological, morphological, and syntactic rules. Manuscript evidence would reframe from 'corruption to be corrected' to 'data to be analyzed.' The prestige hierarchy that elevates classical forms would decompose; university curricula would separate medieval and classical streams rather than treating medieval as derivative.
% FOUNDING_PROBLEM: From the 14th century onward, humanists faced the problem of explaining how living medieval Latin had diverged so sharply from Cicero and the classical texts they recovered. The continuity reading solved this by reframing the divergence as natural linguistic development rather than as loss or error — a move that preserved the unity of 'Latin' across centuries without requiring institutional change to medieval practice.
% FOUNDING_PROBLEM_CORROBORATION: Humanist scholars (Petrarch, Valla, later grammarians) documented their discovery of the divergence and authored the continuity framing as their solution. Modern historical linguists (Stotz, Flobert, Banniard) and paleographers document that medieval writers were consciously working with linguistic variation and that the 'problem' was humanist-constructed, not pre-existing. The founding problem is attested from BOTH sides but with opposite readings of its status: humanists treat the divergence as a solved puzzle (medieval is continuous), while modern reconstruction scholars treat the humanist solution as a retrospective imposition.
narrative_ontology:disappearance_verdict(correct_latin_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__continuity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness (0.62 at interval end) reflects the asymmetric burden of proof: the continuity reading requires no special demonstration (it is the institutional default), while discontinuity positions must marshal detailed evidence against accumulated prestige. Suppression (0.71) is high because the constraint operates partly through silence — by NOT requiring justification from continuity advocates while requiring extraordinary proof from critics. Theater (0.48) reflects that significant performative work maintains the continuity frame: reconstructionist scholarship exists and produces rigorous analyses, but these remain marginal within classical studies despite methodological rigor. The measurement series shows extractiveness rising steeply from t0 to t12 as the humanist solution becomes institutionalized in university teaching (Renaissance through early modern period), then flattening from t12 onward as the continuity assumption becomes so embedded that extraction requires less active work — the constraint becomes structural background rather than defended position. Suppression requirement rises and plateaus similarly: the work of suppressing reconstruction arguments is intense in early modern dispute (t0–t12) but then stabilizes at t12+ because the prestige hierarchy itself does the suppressive work without requiring explicit defense.
 *
 * PERSPECTIVAL GAP:
 *   From the medieval clerical seat, the constraint is experienced as transparent: they worked within one Latin tradition, adapted it for present needs, and treated their innovations as natural extension of patristic and classical foundations. From the humanist reformer seat, the constraint is experienced as a solution they authored: they identified a divergence problem and solved it by framing medieval work as continuous development rather than as loss. From the discontinuity researcher and reconstructionist seats, the constraint is experienced as coercive prestige hierarchy: the continuity assumption is unchosen default that subordinates their evidence to classical norms and requires them to bear costs of justification. The engine's per-seat computation will show this constraint as tangled_rope from beneficiary seats (coordination function + asymmetric extraction) and as snare from payer seats (extraction + suppressed alternatives). The claim/metric independence rule applies: I author this as tangled_rope (genuine coordination function — keeping Latin studies as unified field — layered with extraction), while acknowledging the metrics will compute differently from different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval clerical scholars benefit structurally: their linguistic choices are legitimized without requiring justification. Humanist reformers paid a cost (the burden of explaining divergence) but then captured benefit (authoring the solution and gaining authority over interpretive standards). Discontinuity researchers and reconstructionists pay ongoing costs (burden of proof, institutional marginality) without clear benefit. Classical studies as an institution benefits by preserving the prestige hierarchy that elevates classical forms. The constraint's beneficiaries (medieval clergy and the tradition they represent, plus the institutions that preserve classical prestige) have d near the beneficiary end. The payers (reconstructionists, discontinuity researchers, those arguing for medieval linguistic autonomy) have d near the target end. The directionality overrides are unnecessary here: the structural data (who benefits, who bears burden, what exit options each has) produces accurate d values without intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (explaining divergence between classical and medieval forms) is contested as to status: humanists treat it as solved (by the continuity frame), while modern scholars treat the frame itself as the problem requiring solution. This contest is exactly what the mandatrophy mismatch structure captures. The constraint was built to solve a real puzzle (medieval forms are different from classical — how?), but the solution (treat medieval as continuous development) now persists beyond its justificatory function. Modern linguistics and textual analysis provide alternative solutions (treat medieval as separate system with its own rules; reconstruct from manuscript evidence). The constraint persists because institutional prestige and university curricula embed the continuity frame, not because it remains the best explanation for the evidence. The theater_ratio rising through t0–t12 and plateauing thereafter shows exactly this dynamic: the constraint's functional work (solving the divergence puzzle) is front-loaded in the early modern period when the frame was novel and contested; by the modern period, the constraint operates increasingly through institutional inertia and prestige hierarchy rather than through active explanatory work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_of_medieval_writers,
    'Did medieval clerical writers consciously view their linguistic innovations as developing classical Latin, or did they treat medieval Latin as a present-day pragmatic adaptation with different rules?',
    'Systematic analysis of medieval metalinguistic commentary (prescriptive grammar texts, glosses, scholia) for explicit claims about continuity vs. discontinuity; comparison with scribal practices in textual variant handling across manuscripts.',
    'If medieval writers explicitly claimed continuity, the continuity reading gains genealogical support from its own traditional carriers. If they treated medieval as pragmatically distinct, the reading becomes a humanist retrospective imposition rather than an inherited self-understanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_medieval_writers, empirical, 'Whether medieval linguistic consciousness aligned with the continuity narrative.').

omega_variable(
    distinction_between_readings,
    'What is the structural boundary between the continuity reading (medieval as developments FROM classical) and the hybrid reading (partial continuity + layered recovery)?',
    'Clarification of what counts as ''development'' vs. ''recovery'': does a morphological change that extends classical patterns without attestation in classical texts count as development (continuity frame) or as innovation requiring recovery (hybrid frame)?',
    'If boundaries are unclear, the readings may be empirically indistinguishable and the contest may be terminological rather than substantive. If boundaries are sharp, the readings are genuinely competing hypotheses about different components of medieval Latin.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distinction_between_readings, conceptual, 'Whether the continuity and hybrid readings occupy distinct analytical territory or overlap.').

omega_variable(
    suppression_mechanism_in_scholarship,
    'Is the suppression measured in base_properties (0.71) structural (reconstructionist scholarship is actually difficult and scarce) or internalized (potential scholars avoid reconstruction because the prestige hierarchy discourages it)?',
    'Post-suppression trajectory: do reconstructionist arguments gain institutional traction when humanist-derived prestige hierarchy is weakened? Do they persist in lower-prestige venues while being actively resisted in classical studies departments?',
    'If suppression is structural, removing the continuity constraint would require building alternative institutional infrastructure for reconstruction. If internalized, the constraint could dissolve if prestige signals shifted without structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_in_scholarship, empirical, 'Whether suppression of discontinuity positions is structural or internalized in professional incentives.').

omega_variable(
    kernel_reading_identity,
    'This reading instantiates the continuity_reading axis of the correct_latin_kernel. But how is ''continuity'' defined: genealogical (medieval derives FROM classical), evaluative (medieval is good because it develops classical), or procedural (medieval and classical follow the same analytical rules)? These may entail different constraints.',
    'Specification of what the continuity reading commits to: if it is genealogical continuity, then a discontinuity reading asserts a break in descent. If evaluative, then discontinuity asserts medieval should be judged by its own standards. If procedural, then discontinuity asserts different rule-sets apply.',
    'Clarifying the reading''s commitment fixes which empirical evidence could resolve the contest and which readings truly foreclose each other vs. merely prioritizing different aspects of the same evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'What the continuity_reading''s core normative commitment is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__continuity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__continuity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t4, correct_latin_kernel__continuity_reading, theater_ratio, 4, 0.39).
narrative_ontology:measurement_basis(corr_tr_t4, observed).
narrative_ontology:measurement(corr_tr_t8, correct_latin_kernel__continuity_reading, theater_ratio, 8, 0.43).
narrative_ontology:measurement_basis(corr_tr_t8, observed).
narrative_ontology:measurement(corr_tr_t12, correct_latin_kernel__continuity_reading, theater_ratio, 12, 0.46).
narrative_ontology:measurement_basis(corr_tr_t12, observed).
narrative_ontology:measurement(corr_tr_t16, correct_latin_kernel__continuity_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement_basis(corr_tr_t16, observed).
narrative_ontology:measurement(corr_tr_t20, correct_latin_kernel__continuity_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(corr_tr_t20, observed).
narrative_ontology:measurement(corr_tr_t24, correct_latin_kernel__continuity_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(corr_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__continuity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t4, correct_latin_kernel__continuity_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(corr_be_t4, observed).
narrative_ontology:measurement(corr_be_t8, correct_latin_kernel__continuity_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement_basis(corr_be_t8, observed).
narrative_ontology:measurement(corr_be_t12, correct_latin_kernel__continuity_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement_basis(corr_be_t12, observed).
narrative_ontology:measurement(corr_be_t16, correct_latin_kernel__continuity_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(corr_be_t16, observed).
narrative_ontology:measurement(corr_be_t20, correct_latin_kernel__continuity_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(corr_be_t20, observed).
narrative_ontology:measurement(corr_be_t24, correct_latin_kernel__continuity_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(corr_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__continuity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(corr_su_t0, observed).
narrative_ontology:measurement(corr_su_t4, correct_latin_kernel__continuity_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement_basis(corr_su_t4, observed).
narrative_ontology:measurement(corr_su_t8, correct_latin_kernel__continuity_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(corr_su_t8, observed).
narrative_ontology:measurement(corr_su_t12, correct_latin_kernel__continuity_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(corr_su_t12, observed).
narrative_ontology:measurement(corr_su_t16, correct_latin_kernel__continuity_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(corr_su_t16, observed).
narrative_ontology:measurement(corr_su_t20, correct_latin_kernel__continuity_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(corr_su_t20, observed).
narrative_ontology:measurement(corr_su_t24, correct_latin_kernel__continuity_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(corr_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__continuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__continuity_reading, 0.05).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% The correct_latin_kernel decomposes into three constraint stories: continuity_reading (this story), discontinuity_reading, and hybrid_reading. Each reading instantiates different ε values and beneficiary structures because they parse the same empirical evidence (medieval manuscript divergence from classical forms) via different interpretive frameworks. The continuity_reading treats medieval innovations as legitimate developments and renders reconstruction as internal correction. The discontinuity_reading treats medieval and classical as distinct systems requiring separate analysis. The hybrid_reading treats morphology as continuous but syntax/lexicon as requiring recovery. These are not the same constraint viewed three ways; they are three constraint stories whose ε-invariance test hinges on the evaluative standard applied to medieval evidence. See network.affects_constraints: all three stories link to the others via this field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
