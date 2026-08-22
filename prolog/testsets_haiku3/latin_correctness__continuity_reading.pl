% ============================================================================
% CONSTRAINT STORY: latin_correctness__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__continuity_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: latin_correctness__continuity_reading
 *   human_readable: Medieval Latin as Organic Continuation of Classical Latin
 *   domain: historical_linguistics/intellectual_history
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'latin_correctness': the continuity reading. Medieval Latin is treated as
 *   the legitimate organic development of classical Latin, not a corruption
 *   or rupture. Users of medieval Latin (monks, clerics, scholars) are
 *   recognized as legitimate inheritors of the classical tradition, permitted
 *   to innovate phonetically, morphologically, and lexically in response to
 *   vernacular substrates and new communicative needs. The reading validates
 *   lived medieval practice without requiring reconstruction or return to
 *   classical purity. Extractiveness is low (0.18) because the reading
 *   imposes no coercive separation between medieval users and classical
 *   legitimacy; it dissolves the boundary entirely. No suppression machinery
 *   is needed—medieval practitioners are acting within a continuous language
 *   tradition, not violating an external standard. Theater is minimal (0.08):
 *   the reading aligns with actual medieval usage; there is little gap
 *   between the framing and what practitioners do. The discontinuity_reading
 *   (rupture), by contrast, would treat medieval Latin as corruption
 *   requiring reconstruction and would carry much higher extractiveness and
 *   suppression. This constraint story reports on the continuity reading
 *   alone; the rupture reading is a separate constraint in the same family.
 *
 * KEY AGENTS:
 *   - medieval_latin_users: organized monks, clerics, and scribes across the 5th–15th centuries; power=organized; benefit from legitimacy of their own usage; mobile exit (can return to vernacular, though literacy depends on Latin mastery)
 *   - christian_scholarly_tradition: institutional authority (Church, monasteries, cathedral schools); power=institutional; collects prestige and continuity through the reading; arbitrage exit (could shift to rupture reading or demand classical purity, but institutional investment favors continuity)
 *   - classical_philologists: analytical seat, external observers emerging especially post-12th century; power=institutional (later); measure medieval against classical but remain external to medieval period's own consensus
 *   - renaissance_humanists: excluded from medieval period; emerge 14th century onward to contest continuity directly; trapped exit (committed to classical recovery as a program)
 *   - vernacular_language_speakers: excluded, non-literate; their phonetic and grammatical influence shapes medieval Latin but they have no voice in the written record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__continuity_reading, 0.18).
domain_priors:suppression_score(latin_correctness__continuity_reading, 0.12).
domain_priors:theater_ratio(latin_correctness__continuity_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__continuity_reading, rope).
narrative_ontology:human_readable(latin_correctness__continuity_reading, "Medieval Latin as Organic Continuation of Classical Latin").
narrative_ontology:topic_domain(latin_correctness__continuity_reading, "historical_linguistics/intellectual_history").

domain_priors:emerges_naturally(latin_correctness__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__continuity_reading, 'fb94df72-79ea-49c5-862e-5d638657a0e1').
narrative_ontology:cs_kernel_codification('fb94df72-79ea-49c5-862e-5d638657a0e1', fixed_text).
narrative_ontology:cs_authority_grounding('fb94df72-79ea-49c5-862e-5d638657a0e1', lineage).
narrative_ontology:cs_interpretation_layer_present('fb94df72-79ea-49c5-862e-5d638657a0e1').
narrative_ontology:cs_reading_relation('fb94df72-79ea-49c5-862e-5d638657a0e1', latin_correctness__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb94df72-79ea-49c5-862e-5d638657a0e1', latin_correctness__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('fb94df72-79ea-49c5-862e-5d638657a0e1', foundational, organic_language_evolution_legitimate).
narrative_ontology:cs_axiom_status(organic_language_evolution_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('fb94df72-79ea-49c5-862e-5d638657a0e1', organic_language_evolution_legitimate, conventional).
narrative_ontology:cs_axiom('fb94df72-79ea-49c5-862e-5d638657a0e1', foundational, living_tradition_permits_vernacular_influence).
narrative_ontology:cs_axiom_status(living_tradition_permits_vernacular_influence, holdable).
narrative_ontology:cs_axiom_grounding('fb94df72-79ea-49c5-862e-5d638657a0e1', living_tradition_permits_vernacular_influence, empirically_contingent).
narrative_ontology:cs_reference_frame('fb94df72-79ea-49c5-862e-5d638657a0e1', classical_latin_as_lived_tradition).
narrative_ontology:cs_drift_state('fb94df72-79ea-49c5-862e-5d638657a0e1', late_medieval_humanist_contestation_onset, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fb94df72-79ea-49c5-862e-5d638657a0e1', '').
narrative_ontology:cs_kernel_id(latin_correctness__continuity_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_latin_users).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, christian_scholarly_tradition).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, linguistic_continuity_doctrine).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, organic_language_evolution_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monks, clerics, scribes, and scholars writing in Latin across the medieval period (5th–15th centuries). They inherit classical grammar and vocabulary but adapt pronunciation, inflection, and word-formation to their native languages and practical needs. Under the continuity reading, their linguistic innovations are legitimate developments of a living language, not corruptions. They benefit from a reading that validates their own usage without requiring them to return to Cicero.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_latin_users, beneficiary,
    organized, generational, mobile, continental).

% The Church's institutional authority over Latin literacy in the medieval West. Monasteries and cathedral schools maintain the language and train clergy. Under the continuity reading, medieval Latin is the legitimate vehicle for Christian theology, canon law, and scriptural exegesis—no reconstruction of classical purity is required. The tradition sets the standard for what counts as correct Latin; it collects prestige and institutional continuity through this framing. It can shift to the rupture reading (Renaissance pressure) or maintain the hybrid reading (technical domains).
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, christian_scholarly_tradition, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Scholars of classical Latin literature and inscriptions (emerging as a distinct discipline from the 12th century onward, intensifying in the Renaissance). They analyze medieval texts but often hold that classical norms represent a more authentic or superior form. From their analytical seat, they measure medieval against classical; under the continuity reading, such measurement presumes a fixed standard rather than recognizing organic change.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, classical_philologists, observer,
    institutional, generational, analytical, national).

% Emerge from the 14th century onward and begin to contest the continuity reading directly. They advocate for recovery and imitation of classical models, treating medieval Latin as a fallen form requiring conscious correction. Under their own framing, medieval usage is illegitimate—a reading absent from the medieval period itself but present by the 15th century. They are excluded from the medieval period's own linguistic consensus and would argue that continuity doctrine masks loss.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, renaissance_humanists, excluded,
    powerful, biographical, trapped, regional).

% Speakers of Old French, Old English, Old High German, etc., in whose languages many medieval Latin innovations originate. Latin borrowers from the vernacular, phonetic shifts driven by native-speaker substrate—these influences are treated as legitimate evolution under the continuity reading. The vernacular speakers have no formal seat at the table but their linguistic presence shapes the constraint's content; they cannot object because they are illiterate and outside the written record.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, vernacular_language_speakers, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__continuity_reading, christian_scholarly_tradition).
narrative_ontology:fixing_cost_class(latin_correctness__continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Recognizes medieval users' linguistic practices as legitimate continuations of a single language tradition rather than corruptions requiring correction. This validation solves a social coordination problem: it permits medieval scholars to use Latin as a living tool for theology, law, and administration without the cognitive burden of constant reference to classical models as the sole correct form.
% TRANSFER_FUNCTION: Flows authority and prestige from the classical tradition to medieval practitioners. Medieval users collect the right to innovate linguistically within a continuous lineage; the Church and scholarly tradition collect legitimacy by maintaining that lineage without rupture. What moves is recognition: medieval Latin is treated as *Latin*, not as a separate corrupted object.
% ABSENT_VOICES: Renaissance humanists (who emerge later and contest continuity directly) and the entire literate vernacular population (whose phonology and grammar influence medieval Latin but who leave no written record to voice objection). The constraint reflects the consensus of the Christian scholarly elite; dissenting voices from outside that elite are structurally absent from medieval written culture.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished and were replaced by the rupture reading (medieval = corruption), medieval writers' own practices would be retroactively reclassified as errors requiring correction. Institutional and intellectual authority would shift: no longer would the medieval Church's use of Latin be automatically legitimate; instead, every medieval text would carry the burden of classical vindication. The written record of the medieval period would remain the same, but its epistemic status would change—it would become a repository of failure rather than adaptation.
% FOUNDING_PROBLEM: Latin ceases to be a native spoken language by the 5th century. Across the medieval period, it functions as a scholarly and administrative language for an elite clergy and monastery-trained scribal class, whose native languages are Romance, Germanic, or Celtic variants. How is continuity with classical Latin possible when the conditions of language transmission have fundamentally changed?
% FOUNDING_PROBLEM_CORROBORATION: Medieval scholars themselves (Isidore of Seville, Bede, Alcuin, later scholastics) attest that they are speaking and writing Latin, treating grammar texts and Christian-era authors as authoritative guides, and evolving the language to serve new needs. Classical philologists and linguists (emerging post-Renaissance) attest that medieval Latin shows systematic phonetic, morphological, and lexical divergence from classical norms—the question is whether this divergence is corruption or continuation. The linguistic evidence (manuscript variation, phonetic shift patterns, loan-word stratification) exists independently of reading; the foundational problem's resolution depends on whether organic change is treated as legitimate evolution or as a failure to preserve a fixed form.
narrative_ontology:disappearance_verdict(latin_correctness__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__continuity_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__continuity_reading_tests).
:- end_tests(latin_correctness__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the continuity reading aligns with medieval practitioners' interests: it permits them to use Latin naturally, incorporating vernacular phonology and new vocabulary without guilt or correction. Under this reading, there is no gap between what medieval users do and what they are authorized to do. Compare the rupture reading: if medieval Latin were corruption, there would be high extractiveness (users would be coerced to measure their work against an external classical standard they cannot meet). Suppression (0.12) is minimal because no coercive enforcement is needed—medieval users simply continue what they are already doing. Theater (0.08) is low because the reading describes practice honestly: medieval Latin *is* used with vernacular phonology, expanded vocabulary, changed inflection, and that is what the continuity reading validates. If the reading required elaborate ritual justification for what practitioners already do, theater would be high; instead, the reading dissolves the contradiction between usage and legitimacy. The measurement series track modest increase over the interval (0.12→0.18 extractiveness) reflecting the slow rise of pressures from external standards (Renaissance philology emerging by period's end) but the constraint itself remains low-extraction throughout the medieval interval. The initial time points are observed (manuscript evidence, medieval grammarians' own statements); later points are projected because the Renaissance contestation is beyond the medieval period's own horizon.
 *
 * PERSPECTIVAL GAP:
 *   Medieval practitioners and the Church see the continuity reading as dissolving a false problem: Latin simply develops as languages do. Classical philologists and Renaissance scholars see the same linguistic evidence and measure it as divergence from a standard, treating the reading as apologetic. No seat should compute identically: medieval users compute it as low-cost legitimacy; philologists compute it as a descriptive claim about historical change that may be true but does not vindicate medieval practice by their standards; Renaissance humanists (when they emerge) compute it as an obstacle to their program. The engine should detect this seat divergence from the directionality data.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval Latin users sit at high benefit (d near 0.0): the continuity reading licenses their own practice without requiring external reference or correction. The Christian scholarly tradition sits at moderate benefit (d ~0.2): it collects institutional legitimacy and prestige by maintaining a continuous lineage with classical authority without needing to enforce classical purity. Classical philologists sit at near-symmetric (d ~0.5): they observe the constraint analytically; they neither collect from it nor pay coercively, though they may contest it intellectually. Renaissance humanists, emerging later, would pay a cost under this reading (their program of classical recovery is delegitimized); under the rupture reading, they would benefit. Vernacular users are structurally absent—they influence the constraint (their phonology shapes medieval Latin) but have no seat at the authorization table.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading does not suffer mandatrophy in its own period. The founding problem ('How do medieval practitioners maintain Latin continuity without native-speaker transmission?') remains live throughout the medieval period—medieval scholars explicitly address it through grammar, rhetoric, and theological work. The reading validates that living response. Mandatrophy would arrive later if/when Latin ceases to function as a living scholarly language (Renaissance shift toward classical imitation, then to vernacular literacy) and becomes only a reconstructed historical object. At that point, the continuity reading's founding problem dies (Latin is no longer live practice), but the reading itself might persist as a historical-linguistic claim. That is a different story (outside this constraint's interval).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organic_change_vs_corruption_framing,
    'Is the measured divergence between classical and medieval Latin phonology, morphology, and vocabulary better understood as organic language evolution (continuity reading) or as loss/corruption of a fixed standard (rupture reading)?',
    'Comparative study of language change mechanisms: if medieval Latin exhibits patterns consistent with known mechanisms of diachronic change (sound shift, leveling of inflections, semantic broadening), the continuity reading is supported; if medieval practices cannot be derived from classical rules by known change mechanisms, the rupture reading gains ground. The key evidence is whether medieval innovations follow lawlike patterns or appear arbitrary.',
    'If organic: the continuity reading is vindicated, medieval users are legitimate inheritors, extractiveness remains low. If corrupted: the rupture reading''s framing is supported, medieval practice is measured against a standard it fails, extractiveness rises (coercive correction becomes justified), and mandatrophy emerges as medieval users cannot meet a fixed classical bar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organic_change_vs_corruption_framing, empirical, 'Whether the constraint reflects genuine language evolution or deviation from a fixed norm.').

omega_variable(
    native_transmission_loss_ambiguity,
    'Does the loss of native speakers (Latin ceasing as a first language by the 5th century) mean the medieval learned use is fundamentally discontinuous with classical practice, or can a scholarly/liturgical tradition maintain genuine continuity despite the absence of native transmission?',
    'Examine the mechanism of medieval Latin transmission: if medieval grammarians and teachers worked from classical texts and explicitly trained students in classical grammar, treating variation as deviation requiring correction, the tradition is self-consciously maintaining a fixed form (rupture framing). If medieval practitioners innovated naturally from learned classical training without requiring constant external reference, the tradition is more like organic evolution (continuity framing).',
    'If transmission is textually bound and prescriptive: the reading shifts toward rupture (medieval = restoration attempt, not evolution); extractiveness rises (medieval users are measured against the external text). If transmission is learned internalization permitting variation: continuity holds; extractiveness remains low.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(native_transmission_loss_ambiguity, empirical, 'Whether medieval Latin continuity depends on conscious restoration of fixed texts or on internalized learned competence.').

omega_variable(
    vernacular_substrate_influence_reading_dependence,
    'To what degree does the continuity reading depend on accepting that vernacular phonology, grammar, and vocabulary can legitimately influence a learned language without that influence constituting corruption?',
    'Post-hoc analysis of which linguistic innovations in medieval Latin correlate with phonetic and grammatical patterns from Romance, Germanic, and Celtic substrates. A high correlation suggests the innovations are not idiosyncratic errors but systematic effects of native-language influence—supporting the continuity reading''s framing of them as legitimate adaptation.',
    'If substrates show systematic influence: vernacular influence is treated as a mechanism of legitimate change; continuity reading is vindicated; extractiveness remains low. If medieval innovations appear arbitrary and cross-linguistic boundaries without pattern: the rupture reading gains force (medieval changes are unmotivated errors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_substrate_influence_reading_dependence, empirical, 'Whether medieval Latin innovation is patterned substrate influence or random corruption.').

omega_variable(
    reading_contest_embedded_in_period,
    'Is the contest between continuity and rupture readings present within the medieval period itself (medieval scholars aware and debating which reading is correct), or is it anachronistic (imposed by Renaissance humanists and later)?',
    'Close reading of medieval grammarians'' own statements about language correction, variation, and relationship to classical models. If medieval scholars explicitly defend their own usage against classical standards and argue it is still Latin, the contest is embedded. If medieval scholars treat classical standards as external and view their own work as a pale copy, the contest is anachronistic (imposed later).',
    'If embedded: medieval users are conscious of the reading choice; extractiveness may be higher (they navigate debate). If anachronistic: medieval practitioners are unaware of the rupture reading; extractiveness remains low (they face no conscious pressure to be other than they are). Chronologically, the contest becomes embedded around the 14th century (early humanist contestation); before that, it is largely absent from medieval consciousness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_embedded_in_period, empirical, 'Whether the reading contest is intrinsic to medieval practice or imposed by later scholarship.').

omega_variable(
    authority_grounding_shift_at_reading_boundary,
    'As the rupture reading emerges and gains influence (Renaissance humanists), does the authority grounding of Latin correctness shift from practice/lineage (supporting continuity) to textual fidelity (supporting rupture)?',
    'Examine the transition in authority claims: medieval grammarians cite current usage, tradition, and utility (practice-based authority); Renaissance humanists cite classical texts, Cicero and Quintilian as models, and formal rules extracted from ancient sources (text-based authority). If this shift is real, it marks a change in what grounds the legitimacy claim—authority moves from lived tradition to recovered text.',
    'If the shift is real: the reading contest is not just epistemological but structural—a change in authority grounding that enables rupture to challenge continuity. The extractiveness measured in this story (0.18, continuity period) may reflect the pre-shift state; post-shift extractiveness would be higher (text-based authority is stricter). This omega documents a potential T17-class signal (institutional-grounding shift) at the interval boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_grounding_shift_at_reading_boundary, conceptual, 'Whether authority grounding shifts from practice to text as the rupture reading emerges.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__continuity_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(latin_cont_tr_t0, latin_correctness__continuity_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(latin_cont_tr_t0, observed).
narrative_ontology:measurement(latin_cont_tr_t3, latin_correctness__continuity_reading, theater_ratio, 3, 0.07).
narrative_ontology:measurement_basis(latin_cont_tr_t3, observed).
narrative_ontology:measurement(latin_cont_tr_t6, latin_correctness__continuity_reading, theater_ratio, 6, 0.07).
narrative_ontology:measurement_basis(latin_cont_tr_t6, observed).
narrative_ontology:measurement(latin_cont_tr_t9, latin_correctness__continuity_reading, theater_ratio, 9, 0.08).
narrative_ontology:measurement_basis(latin_cont_tr_t9, projected).
narrative_ontology:measurement(latin_cont_tr_t12, latin_correctness__continuity_reading, theater_ratio, 12, 0.08).
narrative_ontology:measurement_basis(latin_cont_tr_t12, projected).
narrative_ontology:measurement(latin_cont_tr_t15, latin_correctness__continuity_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(latin_cont_tr_t15, projected).

% Extraction over time
narrative_ontology:measurement(latin_cont_be_t0, latin_correctness__continuity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(latin_cont_be_t0, observed).
narrative_ontology:measurement(latin_cont_be_t3, latin_correctness__continuity_reading, base_extractiveness, 3, 0.14).
narrative_ontology:measurement_basis(latin_cont_be_t3, observed).
narrative_ontology:measurement(latin_cont_be_t6, latin_correctness__continuity_reading, base_extractiveness, 6, 0.15).
narrative_ontology:measurement_basis(latin_cont_be_t6, observed).
narrative_ontology:measurement(latin_cont_be_t9, latin_correctness__continuity_reading, base_extractiveness, 9, 0.17).
narrative_ontology:measurement_basis(latin_cont_be_t9, projected).
narrative_ontology:measurement(latin_cont_be_t12, latin_correctness__continuity_reading, base_extractiveness, 12, 0.18).
narrative_ontology:measurement_basis(latin_cont_be_t12, projected).
narrative_ontology:measurement(latin_cont_be_t15, latin_correctness__continuity_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement_basis(latin_cont_be_t15, projected).

% Suppression requirement over time
narrative_ontology:measurement(latin_cont_su_t0, latin_correctness__continuity_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(latin_cont_su_t0, observed).
narrative_ontology:measurement(latin_cont_su_t3, latin_correctness__continuity_reading, suppression_requirement, 3, 0.11).
narrative_ontology:measurement_basis(latin_cont_su_t3, observed).
narrative_ontology:measurement(latin_cont_su_t6, latin_correctness__continuity_reading, suppression_requirement, 6, 0.11).
narrative_ontology:measurement_basis(latin_cont_su_t6, observed).
narrative_ontology:measurement(latin_cont_su_t9, latin_correctness__continuity_reading, suppression_requirement, 9, 0.12).
narrative_ontology:measurement_basis(latin_cont_su_t9, projected).
narrative_ontology:measurement(latin_cont_su_t12, latin_correctness__continuity_reading, suppression_requirement, 12, 0.12).
narrative_ontology:measurement_basis(latin_cont_su_t12, projected).
narrative_ontology:measurement(latin_cont_su_t15, latin_correctness__continuity_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement_basis(latin_cont_su_t15, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__continuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(latin_correctness__continuity_reading, 0.08).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__rupture_reading).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% The kernel 'latin_correctness' decomposes into three structurally distinct constraints: continuity_reading (organic evolution, low extraction), rupture_reading (fixed classical standard, high extraction), and hybrid_reading (domain-dependent norms). Each reading has different ε, different beneficiary/victim structure, and different authority grounding. The readings coexist as live institutional commitments held by different parties (medieval practitioners, Renaissance humanists, later scholastics). They are linked via network.affects_constraints because Renaissance humanist contestation influences medieval practice retroactively (reclassifying medieval texts as corrupted) and prospectively (changing what new Latin writing is permitted). The continuity reading's authority is undermined by the emergence of the rupture reading; the rupture reading's plausibility is strengthened by availability of printed classical texts and humanist scholarship. Each reading is a separate constraint file with its own six_questions, stakeholders, and temporal trajectory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(latin_correctness__continuity_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
