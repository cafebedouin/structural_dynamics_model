% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__hybrid_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Correct Latin as Classical Form via Medieval Transmission with Textual Correction
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   The hybrid reading of 'correct Latin' emerged in the 14th–15th centuries
 *   as humanist scholars (Petrarch, Valla, Erasmus) confronted a medieval
 *   Latin tradition that preserved Classical grammatical structure but had
 *   diverged in orthography, vocabulary, and syntax. Rather than treat
 *   medieval Latin as a legitimate evolution (continuity reading) or as a
 *   corrupt deviation requiring total reconstruction from ancient witnesses
 *   (discontinuity reading), the hybrid reading asserts that medieval
 *   transmission carries the Classical core but requires targeted textual
 *   correction — emendation guided by the best manuscript evidence and
 *   linguistic reasoning. This constraint coordinates editorial practice,
 *   pedagogy, and scholarly communication across early modern Europe. It is a
 *   tangled rope: it genuinely coordinates (shared standard for editions and
 *   curricula) while extracting (continuity adherents must accept correction
 *   of 'their' texts; reconstructionists must accept medieval grammatical
 *   cores as legitimate baselines). Active enforcement occurs through
 *   editorial boards, academic appointments, and the institutional weight of
 *   critical editions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.38).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.22).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Correct Latin as Classical Form via Medieval Transmission with Textual Correction").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, '79fff908-14c4-4271-af45-4288f5c0262e').
narrative_ontology:cs_kernel_codification('79fff908-14c4-4271-af45-4288f5c0262e', distributed).
narrative_ontology:cs_authority_grounding('79fff908-14c4-4271-af45-4288f5c0262e', practice).
narrative_ontology:cs_interpretation_layer_present('79fff908-14c4-4271-af45-4288f5c0262e').
narrative_ontology:cs_reading_relation('79fff908-14c4-4271-af45-4288f5c0262e', correct_latin__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('79fff908-14c4-4271-af45-4288f5c0262e', correct_latin__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('79fff908-14c4-4271-af45-4288f5c0262e', foundational, medieval_transmission_carries_classical_core).
narrative_ontology:cs_axiom_status(medieval_transmission_carries_classical_core, holdable).
narrative_ontology:cs_axiom_grounding('79fff908-14c4-4271-af45-4288f5c0262e', medieval_transmission_carries_classical_core, empirically_contingent).
narrative_ontology:cs_axiom('79fff908-14c4-4271-af45-4288f5c0262e', foundational, textual_evidence_authorizes_targeted_emendation).
narrative_ontology:cs_axiom_status(textual_evidence_authorizes_targeted_emendation, holdable).
narrative_ontology:cs_axiom_grounding('79fff908-14c4-4271-af45-4288f5c0262e', textual_evidence_authorizes_targeted_emendation, empirically_contingent).
narrative_ontology:cs_axiom('79fff908-14c4-4271-af45-4288f5c0262e', secondary, reform_is_corrective_not_reconstructive).
narrative_ontology:cs_axiom_status(reform_is_corrective_not_reconstructive, holdable).
narrative_ontology:cs_axiom_grounding('79fff908-14c4-4271-af45-4288f5c0262e', reform_is_corrective_not_reconstructive, conventional).
narrative_ontology:cs_reference_frame('79fff908-14c4-4271-af45-4288f5c0262e', humanist_editorial_consensus).
narrative_ontology:cs_drift_state('79fff908-14c4-4271-af45-4288f5c0262e', early_modern_editorial_maturity, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('79fff908-14c4-4271-af45-4288f5c0262e', '').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, philologists_textual_critics).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, medieval_latin_practitioners).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, humanist_educators).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, rigid_continuity_adherents).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, radical_reconstructionists).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, textual_emendation_principle).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, partial_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish editorial standards for critical editions; their authority derives from methodological rigor in textual criticism. They define what counts as 'correctable' medieval transmission versus 'legitimate' evolution. Their professional standing and institutional positions (university chairs, academy memberships) depend on the hybrid framework's legitimacy. Exit is arbitrage-grade: they can shift between editorial projects, journals, and national traditions.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, philologists_textual_critics, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, philologists_textual_critics, beneficiary).

% Scholars, clergy, and administrators who use medieval Latin as a living working language. Their texts and practices are partially legitimated by the hybrid reading — they need not abandon medieval forms wholesale, only accept targeted corrections. Exit is mobile: they can continue medieval practice under continuity or reconstructionist frameworks, though with professional friction.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, medieval_latin_practitioners, beneficiary,
    organized, biographical, mobile, continental).

% Teachers and curriculum designers in humanist schools and universities. The hybrid reading lets them teach a 'purified' classical standard while acknowledging the medieval transmission chain — a pedagogically useful compromise. Exit is constrained: curricula and examinations are institutionally embedded; switching frameworks requires systemic reform.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, humanist_educators, beneficiary,
    institutional, generational, constrained, continental).

% Defenders of medieval Latin as autonomously legitimate evolved Classical Latin. They bear the cost of having their preferred texts and practices marked as 'requiring correction' by the hybrid standard. Their professional and confessional identity is fused with the continuity thesis; exit would mean abandoning a self-concept constituted through the relationship to the medieval tradition.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, rigid_continuity_adherents, payer,
    organized, biographical, identity_locked, regional).

% Advocates for reconstructing Classical Latin exclusively from ancient textual witnesses, treating medieval transmission as corrupt. They bear the cost of the hybrid reading legitimating medieval grammatical cores that they reject as unauthentic. Their scholarly identity is built on the discontinuity thesis; exit means surrendering the core polemical claim of their research program.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, radical_reconstructionists, payer,
    organized, biographical, identity_locked, regional).

% The body of manuscript witnesses, inscriptions, and indirect tradition that constrains all readings. It does not speak for itself but provides the empirical surface on which correction operates. Its 'exit' is analytical: it is the reference frame, not a participant.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, textual_evidence, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(correct_latin__hybrid_reading, textual_evidence).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared editorial and pedagogical standard that legitimates medieval Latin as a transmission vehicle while authorizing philological correction — allowing scholars, educators, and institutions to work with medieval texts without either fossilizing errors or discarding the tradition wholesale.
% TRANSFER_FUNCTION: Moves editorial authority and curricular legitimacy from both rigid continuity and radical reconstruction toward the hybrid center: continuity adherents cede the claim that medieval forms are beyond correction; reconstructionists cede the claim that medieval transmission is wholly corrupt. The hybrid reading collects the coordination surplus (usable texts, stable pedagogy, professional consensus).
% ABSENT_VOICES: Vernacular authors writing in the shadow of Latin authority — their linguistic choices were constrained by which Latin standard held sway, but they were not consulted in the philological settlement. Also excluded: non-European Latin traditions (e.g., Syriac, Arabic, Hebrew mediating traditions) whose transmission chains were marginalized by the European editorial consensus.
% DISAPPEARANCE_RATIONALE: If the hybrid reading vanished, the field would polarize: continuity adherents would reclaim uncorrected medieval Latin as the standard, reconstructionists would push for ancient-text-only editions, and pedagogical practice would fracture — no single framework would coordinate editing, teaching, and scholarly communication across the Latin tradition.
% FOUNDING_PROBLEM: The collapse of living Latin fluency after antiquity left medieval scribes transmitting texts they imperfectly understood, producing a tradition that was neither purely Classical nor autonomously evolved. Renaissance humanists needed a standard that could recover Classical correctness without discarding the medieval manuscript tradition that preserved the texts.
% FOUNDING_PROBLEM_CORROBORATION: Erasmus and the early humanist editors (e.g., Aldine press) explicitly framed their work as correcting medieval transmission toward Classical purity while using medieval manuscripts as their base — attested in prefaces and correspondence. Modern codicologists (e.g., Bischoff, Reynolds & Wilson) corroborate that the manuscript tradition is a mixed stream: Classical grammatical core largely preserved, orthography and vocabulary heavily medievalized. No single party's self-assertion suffices; the corroboration comes from the material evidence of the manuscripts themselves, read against the editorial record.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__hybrid_reading_tests).
:- end_tests(correct_latin__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the asymmetric cost: continuity adherents and reconstructionists both concede ground to the hybrid center, but the hybrid reading's proponents (philologists, educators) collect the coordination surplus. Suppression (0.22) is modest — alternatives are not banned, but the hybrid standard dominates prestige venues. Theater ratio (0.18) is low: the correction function is real and active, not performative. Accessibility collapse (0.35) is partial: alternatives persist in regional and confessional enclaves. Resistance (0.45) is significant: both excluded factions maintained competing editions, curricula, and polemics throughout the period. The claimed type 'tangled_rope' captures the dual coordination/extraction structure; the engine will compute per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the philologist's seat, the constraint is a rope: genuine coordination of editorial labor with minimal coercion. From the continuity adherent's seat, it is a snare: their legitimate tradition is being corrected by an external standard they reject. From the reconstructionist's seat, it is a tangled rope: they accept the coordination function (textual criticism) but reject the legitimated medieval core as extraction. The engine computes this divergence from the declared roles, power, and exit options — the claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Philologists/textual critics are agenda_setters and beneficiaries: they define the correction methodology and gain professional authority from it (d ~ 0.15). Medieval practitioners and humanist educators are beneficiaries: they gain legitimated texts and stable pedagogy (d ~ 0.3–0.4). Rigid continuity adherents and radical reconstructionists are payers: they bear the cost of having their frameworks marginalized as 'uncritical' or 'extreme,' with identity_locked exit making the cost structurally sticky (d ~ 0.75–0.85). Textual evidence is an analytical observer (d = 0.5). The derivation chain from beneficiary/victim + power + exit produces these directionalities; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (recovering Classical correctness from a medievalized transmission) remains contested: some argue the ancient textual base is now sufficient to bypass medieval transmission entirely (founding problem dead); others argue the medieval tradition is the only continuous witness and correction is hubris (founding problem live). The hybrid reading persists because it coordinates the actual working conditions of editors and teachers — who need both the medieval manuscripts and the Classical standard. Mandatrophy is not resolved: the arrangement continues to serve a coordination function even as its original polemical justification (against medieval barbarism) has faded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_identity,
    'Is this constraint one reading of the ''correct_latin'' kernel, and does its ε refer to the standing arrangement under contest (the hybrid editorial standard) rather than the reading''s endorsed ideal?',
    'Verify that the authored metrics describe the hybrid standard''s actual operation (1350–1650), not the philologist''s aspirational target. The kernel_id and reading_id are structural metadata, not authored content.',
    'If ε were authored for the reading''s ideal rather than the standing arrangement, the extraction value would be near zero and the constraint would misclassify as rope or mountain — violating ε-invariance (DP-001). The omega records the committer-frame discipline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_identity, conceptual, 'Commitment to ε-invariance: the referent is the standing arrangement under contest, assessed by this reading''s lights.').

omega_variable(
    textual_correction_boundary,
    'Where is the boundary between ''correctable medieval form'' and ''legitimate medieval evolution'' in the hybrid reading? Is it stable or does it shift with editorial fashion?',
    'Track editorial practice in critical editions across the interval: which emendations are accepted as corrections versus rejected as hypercorrections. Codicological consensus on specific loci (e.g., orthography vs. vocabulary vs. syntax) provides empirical grounding.',
    'If the boundary is unstable, the constraint''s coordination function degrades toward piton (theater rises) or snare (suppression rises to enforce a moving line). If stable, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_correction_boundary, empirical, 'Whether the hybrid reading''s correction criterion is a stable structural feature or a moving target.').

omega_variable(
    excluded_vernacular_impact,
    'Does the hybrid reading''s editorial standard structurally shape vernacular literary development by defining the Latin authority against which vernaculars position themselves?',
    'Comparative study of vernacular grammatical treatises, orthography debates, and classicizing movements in Italian, French, Spanish, German — do they reference the hybrid Latin standard implicitly or explicitly?',
    'If yes, the constraint''s extraction extends beyond the named stakeholders to a diffuse vernacular payer class not currently represented. This would increase effective extraction and potentially shift classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_vernacular_impact, empirical, 'Downstream structural influence on vernacular standardization — an excluded voice with material consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 1350, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t1350, correct_latin__hybrid_reading, theater_ratio, 1350, 0.1).
narrative_ontology:measurement(corr_tr_t1425, correct_latin__hybrid_reading, theater_ratio, 1425, 0.12).
narrative_ontology:measurement(corr_tr_t1500, correct_latin__hybrid_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement(corr_tr_t1550, correct_latin__hybrid_reading, theater_ratio, 1550, 0.17).
narrative_ontology:measurement(corr_tr_t1600, correct_latin__hybrid_reading, theater_ratio, 1600, 0.18).
narrative_ontology:measurement(corr_tr_t1650, correct_latin__hybrid_reading, theater_ratio, 1650, 0.18).

% Extraction over time
narrative_ontology:measurement(corr_be_t1350, correct_latin__hybrid_reading, base_extractiveness, 1350, 0.25).
narrative_ontology:measurement(corr_be_t1425, correct_latin__hybrid_reading, base_extractiveness, 1425, 0.3).
narrative_ontology:measurement(corr_be_t1500, correct_latin__hybrid_reading, base_extractiveness, 1500, 0.35).
narrative_ontology:measurement(corr_be_t1550, correct_latin__hybrid_reading, base_extractiveness, 1550, 0.37).
narrative_ontology:measurement(corr_be_t1600, correct_latin__hybrid_reading, base_extractiveness, 1600, 0.38).
narrative_ontology:measurement(corr_be_t1650, correct_latin__hybrid_reading, base_extractiveness, 1650, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t1350, correct_latin__hybrid_reading, suppression_requirement, 1350, 0.15).
narrative_ontology:measurement(corr_su_t1425, correct_latin__hybrid_reading, suppression_requirement, 1425, 0.18).
narrative_ontology:measurement(corr_su_t1500, correct_latin__hybrid_reading, suppression_requirement, 1500, 0.2).
narrative_ontology:measurement(corr_su_t1550, correct_latin__hybrid_reading, suppression_requirement, 1550, 0.22).
narrative_ontology:measurement(corr_su_t1600, correct_latin__hybrid_reading, suppression_requirement, 1600, 0.22).
narrative_ontology:measurement(corr_su_t1650, correct_latin__hybrid_reading, suppression_requirement, 1650, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin__hybrid_reading, 0.03).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the 'correct_latin' constraint family. The three readings (continuity, discontinuity, hybrid) instantiate distinct constraints from the same kernel, with different ε values and beneficiary/victim structures. The hybrid reading influences both siblings by establishing the dominant editorial standard; neither sibling forecloses the other, as all three remain live in contemporary philological discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
