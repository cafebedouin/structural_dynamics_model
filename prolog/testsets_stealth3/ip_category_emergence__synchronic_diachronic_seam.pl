% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: Synchronic-Diachronic Seam: The Unified Founding-Event Arrangement (M4/M5 Collapse Test)
 *   domain: legal philosophy/intellectual property/historical jurisprudence
 *
 * SUMMARY:
 *   Within legal-historical scholarship on intellectual property, the
 *   standing arrangement treats the 1710 Statute of Anne as a single unified
 *   event in which ownable expression became legally thinkable and authors
 *   simultaneously entered the legitimate claimant set. This story
 *   instantiates the synchronic_diachronic_seam reading of the kernel
 *   ip_category_emergence: the claim that the two dimensions — category
 *   emergence (diachronic thinkability) and occupancy change (synchronic
 *   first-holding) — are either formally independent variables whose
 *   co-occurrence requires demonstration, or that their apparent unity is a
 *   temporal framing artifact of describing one event diachronically and
 *   synchronically. The epsilon referent is the standing arrangement under
 *   contest — the untested unified-event framing and the disciplinary
 *   practice that maintains it — assessed by this reading's own lights. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   while the metrics are authored independently as descriptively true; the
 *   engine measures any divergence. This story is the downstream member of a
 *   three-story family: the sibling readings (thinkability_reading,
 *   first_holding_reading) author epsilon against arrangements organized
 *   around a single dimension each; this reading authors epsilon against the
 *   arrangement that binds the dimensions into one untested event.
 *
 * KEY AGENTS:
 *   - founding_moment_historians: agenda-setting custodian (institutional/identity_locked) — writes and enforces the unified founding narrative; collects citation capital and authority from it
 *   - ip_doctrinal_establishment: primary beneficiary (institutional/constrained) — cites the founding moment for doctrinal legitimacy without administering it
 *   - decompositional_scholars: primary target (moderate/constrained) — designs and would run the independence tests; pays in review framing, citations, and placements
 *   - premodern_practice_historians: secondary target, partial beneficiary (moderate/constrained) — supplies the archival material the narrative absorbs as background
 *   - non_anglophone_ip_historiographies: excluded payer (organized/constrained) — maintains rival founding moments the Anglophone frame marginalizes
 *   - law_students_and_junior_teachers: diffuse payer, incidental beneficiary (powerless/mobile) — receives the flattened periodization as settled pedagogy
 *   - legal_metahistoriographers: analytical observer (analytical/analytical) — studies how the field makes founding moments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.58).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.62).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.58).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "Synchronic-Diachronic Seam: The Unified Founding-Event Arrangement (M4/M5 Collapse Test)").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal philosophy/intellectual property/historical jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '8d1367e4-14f5-4214-8026-f78e430ba591').
narrative_ontology:cs_kernel_codification('8d1367e4-14f5-4214-8026-f78e430ba591', distributed).
narrative_ontology:cs_authority_grounding('8d1367e4-14f5-4214-8026-f78e430ba591', expertise).
narrative_ontology:cs_interpretation_layer_present('8d1367e4-14f5-4214-8026-f78e430ba591').
narrative_ontology:cs_reading_relation('8d1367e4-14f5-4214-8026-f78e430ba591', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('8d1367e4-14f5-4214-8026-f78e430ba591', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_axiom('8d1367e4-14f5-4214-8026-f78e430ba591', foundational, independence_requires_demonstration_not_assumption).
narrative_ontology:cs_axiom_status(independence_requires_demonstration_not_assumption, holdable).
narrative_ontology:cs_axiom_grounding('8d1367e4-14f5-4214-8026-f78e430ba591', independence_requires_demonstration_not_assumption, empirically_contingent).
narrative_ontology:cs_axiom('8d1367e4-14f5-4214-8026-f78e430ba591', secondary, co_occurrence_may_be_temporal_framing_artifact).
narrative_ontology:cs_axiom_status(co_occurrence_may_be_temporal_framing_artifact, holdable).
narrative_ontology:cs_axiom_grounding('8d1367e4-14f5-4214-8026-f78e430ba591', co_occurrence_may_be_temporal_framing_artifact, conventional).
narrative_ontology:cs_reference_frame('8d1367e4-14f5-4214-8026-f78e430ba591', formal_independence_open_kernel).
narrative_ontology:cs_drift_state('8d1367e4-14f5-4214-8026-f78e430ba591', contemporary_revisionist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8d1367e4-14f5-4214-8026-f78e430ba591', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, founding_moment_historians).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, ip_doctrinal_establishment).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, premodern_practice_historians).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, law_students_and_junior_teachers).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, decompositional_scholars).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, premodern_practice_historians).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, non_anglophone_ip_historiographies).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, law_students_and_junior_teachers).
narrative_ontology:constraint_vindicates(ip_category_emergence__synchronic_diachronic_seam, statute_of_anne_founding_moment_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write the survey histories, edit the field's journals, and author the casebook origin chapters that fix 1710 as the single founding event. Their standing rests on custodianship of that narrative; a decompositional re-periodization would dissolve the object their authority is attached to. Leaving would mean retraining into general book history or cultural history at senior-career cost, and their professional self-concept is constituted by the custodial role itself.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, founding_moment_historians, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__synchronic_diachronic_seam, founding_moment_historians, beneficiary).

% Courts, treatise writers, and doctrinal scholars cite the 1710 founding moment whenever intellectual property needs a coherent, traceable origin. They collect the legitimacy the narrative provides without administering the historiography that maintains it. Their alternative — grounding doctrine in the contested multi-origin prehistory — would make every legitimacy claim harder to write.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, ip_doctrinal_establishment, beneficiary,
    institutional, generational, constrained, global).

% Propose treating category emergence and occupancy change as separately datable variables and design the tests that would check whether their co-occurrence at 1710 is necessary or contingent. Their papers are reviewed as methodologically premature, cited as footnotes to the canonical narrative, and rewarded with fewer placements and grants. Moving to book history or science-studies departments is possible but abandons the audience their question is about.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, decompositional_scholars, payer,
    moderate, biographical, constrained, continental).

% Study the pre-1710 practices — stationers' registrations, printing patents, Venetian privileges, guild licensing — that the founding narrative absorbs as background. Their archival work is indispensable to the canon and simultaneously subordinated by it: the founding frame treats their period as the darkness before 1710 rather than as a rival periodization. They gain citation traffic from the canon while losing the argument about what their period shows.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, premodern_practice_historians, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__synchronic_diachronic_seam, premodern_practice_historians, beneficiary).

% Maintain rival founding narratives — the French revolutionary copyright laws of 1791 and 1793, the Venetian privilege statute of 1474, German author's-right lineages — in their own languages and institutions. The Anglophone founding-moment conversation rarely cites them except as comparative color. They bear the marginalization of their periodizations and are not in the room where the founding moment is debated.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, non_anglophone_ip_historiographies, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__synchronic_diachronic_seam, non_anglophone_ip_historiographies, excluded).

% Learn intellectual property through casebooks whose origin chapter presents 1710 as the single founding event. They receive a teachable shared frame and pay for it with a flattened periodization they are not equipped to notice, let alone contest. Their position is transient and their exit is ordinary graduation.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, law_students_and_junior_teachers, payer,
    powerless, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__synchronic_diachronic_seam, law_students_and_junior_teachers, beneficiary).

% Study how legal history is written rather than write it: they examine how a field selects founding moments, absorbs revision, and converts archival complexity into origin narratives. They collect no rents from the founding frame and bear none of its costs, which is what makes their testimony usable by every other seat.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, legal_metahistoriographers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__synchronic_diachronic_seam, founding_moment_historians).
narrative_ontology:fixing_cost_class(ip_category_emergence__synchronic_diachronic_seam, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single founding reference point — the 1710 Statute of Anne — that coordinates a dispersed legal-historical field: one shared periodization for citation, pedagogy, and doctrinal legitimacy claims, so scholars, courts, and teachers can locate the origin of intellectual property without re-litigating the prehistory each time.
% TRANSFER_FUNCTION: Moves scholarly authority and citation capital from decompositional and alternative-periodization work to the custodians of the canonical founding narrative, and moves doctrinal legitimacy from a contested multi-origin prehistory to a fixed founding moment.
% ABSENT_VOICES: Non-Anglophone IP historiographies — the French droit d'auteur tradition (1777/1791), the Venetian 1474 statute, German author's-right lineages — sit outside the Anglophone founding-moment conversation and would object that the 1710 anchor is parochial; premodern practice historians are present but absorbed as background; the decompositional methodologists are present but their test is framed as premature rather than answered.
% DISAPPEARANCE_RATIONALE: If the unified-founding arrangement vanished overnight, IP historiography would reorganize around decompositional periodization: category emergence and occupancy change dated and argued separately, possibly at different moments and in different jurisdictions; casebook narratives would lose their single anchor chapter; and doctrinal legitimacy claims would need re-grounding in the multi-origin prehistory rather than a founding moment.
% FOUNDING_PROBLEM: Late-twentieth-century IP historiography inherited a scattered prehistory — stationers' monopoly, printing patents, guild privileges, moral-rights lineages — and needed an organizing frame: a founding moment that would make the modern category traceable and teachable.
% FOUNDING_PROBLEM_CORROBORATION: Revisionist legal historians outside the canonical custodial set — the 'making of modern intellectual property law' literature and the archival work on the Statute of Anne's passage — corroborate that the organizing problem was real but attest that the single-event solution was a retrospective consolidation rather than a discovery in the archive; comparative historians of the Venetian 1474 statute and the French revolutionary copyright laws corroborate that alternative founding moments were available and passed over. No source outside the beneficiary set attests that the founding problem is settled.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the unified framing converts an open empirical question into settled background, and the conversion pays out as citation capital and authority to the custodial seat while imposing review-framing and career costs on the seats that would run the test; it is substantial but not total, because decompositional and comparative work remains publishable. Suppression 0.62 is structural rather than legal: gatekeeping operates through peer-review framing ('premature'), citation canons, and the absorption of revisionism as footnote-nuance, not through formal sanction. Theater_ratio 0.40: tercentenary commemoration and survey-chapter canon-repetition are performative maintenance, but a real revisionist literature also exists, so the performative share stays under half. Accessibility_collapse 0.35: alternatives persist — comparative periodizations and decompositional methods are publishable — the frame marginalizes rather than eliminates them. Resistance 0.55: the revisionist wave is active, organized resistance to the unified narrative. All three series run on one shared seven-point grid (t=0..36, roughly 1988-2024): extractiveness rises through the consolidation decade, peaks as the revisionist challenge makes canon-defense costly, then settles as the apparatus absorbs revisionism; theater spikes around the 2010 tercentenary (t=22-24); the suppression_requirement series is authored because enforcement-capacity change is the dynamic being traced — intensification during the canon-defense years, partial relaxation once absorption succeeded.
 *
 * PERSPECTIVAL GAP:
 *   The custodial seat and the decompositional seat should compute differently from near-identical nominal standing: both are scholars in the same field at adjacent power levels, but the custodian's identity is fused with the narrative it administers (identity_locked exit) while the tester's exit is merely constrained — the same field, different exits, different experienced arrangement. The doctrinal establishment experiences it as pure background benefit and should compute the mildest classification; the excluded non-Anglophone historiographies experience it as parochial closure; the transient student seat experiences almost nothing at all. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   founding_moment_historians sit at the beneficiary end (d near 0.0): the arrangement subsidizes their authority, and identity_locked exit means they neither can nor would leave it. ip_doctrinal_establishment collects legitimacy without administering — low d, damped effective extraction. decompositional_scholars sit near the full-target end (d near 1.0): they bear the review and career costs of the unrun test, with constrained exit amplifying effective extraction. premodern_practice_historians are genuinely dual — absorbed as background (a cost) while gaining citation traffic (a benefit) — net targets but not full-target; the per-power-atom override mechanism cannot separate them from decompositional_scholars (same atom, same nominal exit), so the differentiation is left to the structural derivation and noted here rather than forced by an override. law_students_and_junior_teachers are near-symmetric: teachable frame received, flattened periodization borne, transient position damping both. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the continental-to-global scope of the scholarly field.
 *
 * MANDATROPHY ANALYSIS:
 *   The seam reading is the kernel's own obsolescence probe. If the founding problem (organizing a scattered prehistory) is dead and the unified event is maintained only performatively, the arrangement drifts toward inertial maintenance; if the problem is live and the coordination function real, the asymmetric costs riding on it are the thing to name. The tangled_rope classification keeps both halves visible — the shared reference frame is a genuine coordination good for a dispersed field, and the unrun seam test is the asymmetric cost. Mislabeling risk runs both ways: calling the arrangement pure extraction would erase the real pedagogical coordination the founding frame provides; calling it pure coordination would launder the career costs paid by the seats that would dissolve the kernel. The R5 status is authored 'contested' rather than 'dead' because the organizing problem persists even as the single-event solution is exactly what the seam test puts in question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seam_test_resolution,
    'Does the seam test resolve toward formal independence (category emergence and occupancy change are separately variable, and the kernel''s two-dimensionality is authentic) or toward temporal framing artifact (they necessarily co-occur, and the kernel is one-dimensional)?',
    'Historical counterfactual analysis across jurisdictions: populate or empty the off-diagonal cells — ownable expression legally coherent without author-occupancy of the claimant set (e.g., assignee and publisher holding under the Statute of Anne while author-coherence remained unsettled), or author-occupancy without category coherence (pre-1710 authorial privilege claims). Any populated cell demonstrates independence.',
    'Independence preserves both sibling readings as distinct constraints and stabilizes the three-story family; artifact collapses the kernel into a single claim, one sibling absorbs the other, and this reading loses its reason to exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seam_test_resolution, empirical, 'Whether the M4/M5 collapse test resolves toward independence or temporal artifact.').

omega_variable(
    kernel_reading_provenance,
    'This constraint is one reading (synchronic_diachronic_seam) of the kernel ip_category_emergence; what would change structurally if a sibling reading (thinkability_reading or first_holding_reading) were instantiated instead?',
    'Comparative authoring: generate the sibling stories and diff their beneficiary/victim structures, epsilon values, and enforcement surfaces. This reading authors epsilon against the unified-event arrangement; the siblings author epsilon against arrangements organized around their respective single dimension.',
    'Sibling instantiations relocate the cost surface (category-suppression for thinkability, claimant-set exclusion for first-holding) and change which seats count as targets; this reading uniquely targets the untested assumption binding the two dimensions together.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_provenance, conceptual, 'Committer-frame provenance: reading-of-kernel structure and sibling structural delta.').

omega_variable(
    m4_m5_operationalization,
    'What historical evidence would count as a successful M4 or M5 run — what operational test distinguishes ''the category could exist without this occupant'' from ''this occupant could hold without the category''?',
    'Methodological work in legal historiography: specify the two variables (coherence of ownable expression as a legal category; membership of author-as-rights-holder in the legitimate claimant set), the datable indicators for each, and the counterfactual standard for independence.',
    'Without operationalization the seam test is unfalsifiable and the kernel''s structure can never be certified as authentic or spurious; with it, the discipline faces a runnable test it currently declines to run.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(m4_m5_operationalization, conceptual, 'Operational content of the M4/M5 independence tests.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of the seam test structural (gatekeeping, review framing, citation canons) or internalized (decompositional scholars pre-emptively classify their own question as premature and never submit it)?',
    'Submission and rejection records: papers submitted and rejected indicate structural suppression; the question never reaching submission indicates internalized suppression; both channels operating indicates a mixed mechanism.',
    'Internalized suppression persists after gatekeeping reform — removing editorial barriers would not restart the test; structural suppression responds to venue and review reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism of the suppression holding the seam test unrun.').

omega_variable(
    co_occurrence_date_contingency,
    'Is 1710 the only candidate point of co-occurrence, or is the apparent necessary co-occurrence an artifact of anchoring both dimensions to the Statute of Anne when either might be better dated elsewhere (Venice 1474, the Stationers'' transition 1695-1710, France 1791)?',
    'Comparative dating: estimate category-emergence and occupancy-change dates independently across jurisdictions and test whether the gap between them varies; variable gaps demonstrate contingency of the co-occurrence.',
    'If the gap varies across jurisdictions, co-occurrence at 1710 is contingent and formal independence is supported; if the gap is constant, the temporal-artifact hypothesis strengthens and the kernel''s two-dimensionality weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_occurrence_date_contingency, empirical, 'Whether the 1710 co-occurrence of the two dimensions is contingent or necessary.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel''s authority structure best framed as scholarly expertise adjudicating a historiographical thesis, or as the doctrinal legitimacy apparatus of intellectual property law itself, which benefits from kernel stability and uses historiography as its interpretive buffer?',
    'Trace citation flow: if doctrinal legitimacy claims (court opinions, treatises) drive the founding narrative''s authority more than peer-reviewed historiographical findings do, the legitimacy-apparatus framing is the better fit; if the historiography disciplines the doctrine, the expertise framing holds.',
    'Under the legitimacy-apparatus framing, authority_grounding shifts toward extraction and the arrangement''s measured extractiveness rises — the founding narrative reads as a legitimacy-rent mechanism rather than a scholarly convention, moving classification toward the snare end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framings of the kernel''s authority structure (scholarly convention vs. doctrinal legitimacy apparatus).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t0, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0, 0.24).
narrative_ontology:measurement(ip_c_tr_t6, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 6, 0.29).
narrative_ontology:measurement(ip_c_tr_t12, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ip_c_tr_t18, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 18, 0.4).
narrative_ontology:measurement(ip_c_tr_t24, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 24, 0.43).
narrative_ontology:measurement(ip_c_tr_t30, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 30, 0.41).
narrative_ontology:measurement(ip_c_tr_t36, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 36, 0.4).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t0, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(ip_c_be_t6, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 6, 0.49).
narrative_ontology:measurement(ip_c_be_t12, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(ip_c_be_t18, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(ip_c_be_t24, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(ip_c_be_t30, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(ip_c_be_t36, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 36, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t0, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ip_c_su_t6, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(ip_c_su_t12, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(ip_c_su_t18, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 18, 0.66).
narrative_ontology:measurement(ip_c_su_t24, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(ip_c_su_t30, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(ip_c_su_t36, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 36, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, information_standard).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% The kernel ip_category_emergence decomposes into three constraint stories per the epsilon-invariance principle: the label 'the 1710 emergence of IP' conflates category emergence (thinkability) with occupancy change (first-holding), and measuring the arrangement through one observable versus the other yields different epsilon values. The sibling stories author epsilon against arrangements organized around a single dimension each; this seam story authors epsilon against the arrangement that binds the two dimensions into one untested event. The siblings are upstream (their distinctness is what the seam test evaluates); this reading is downstream of both and links to each. If the seam test resolves toward temporal artifact, the family collapses to one story; if toward formal independence, the family stabilizes as three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
