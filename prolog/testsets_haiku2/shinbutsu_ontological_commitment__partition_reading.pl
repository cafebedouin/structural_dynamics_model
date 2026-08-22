% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinbutsu Ontological Partition: Separate Domains (Life-Cycle vs Afterlife)
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   The partition reading of shinbutsu (Shinto-Buddhism coexistence) frames
 *   dual religious practice in Japan as a coherent ontological arrangement,
 *   not syncretism or incoherence. Shinto governs the life-cycle and
 *   community welfare; Buddhism governs death, afterlife, and ultimate
 *   liberation. Under this reading, practitioners' simultaneous participation
 *   in both traditions is rationally organized rather than philosophically
 *   confused. The reading emerged during pre-modern synthesis periods as a
 *   defense of Shinto autonomy, was reinforced by Meiji institutional
 *   separation, and persists as a dominant hermeneutical frame in
 *   contemporary Japanese religious studies. This constraint instantiates the
 *   partition reading's legitimating structure: the arrangement it describes
 *   is the reading's own endorsed framework, not a neutral fact. The
 *   syncretic and incoherence readings (other constraints in this family)
 *   would describe different constraints with different ε values—one higher
 *   (extraction from the constraint-of-enforced-coherence), one lower (less
 *   extraction from institutionally-tolerated-contradiction). This story
 *   captures only the partition reading's internal logic.
 *
 * KEY AGENTS:
 *   - Shinto shrine practitioners: maintain life-cycle ritual authority; benefit from functional autonomy without doctrinal integration
 *   - Buddhist temple practitioners: maintain death-ritual authority; benefit from soteriological coherence without world-affirming syncretism
 *   - Household adherents: practice both traditions instrumentally; benefit from partition logic that permits dual allegiance without cognitive dissonance
 *   - Partition-school scholars: produce and maintain the hermeneutical frame; benefit from intellectual coherence and institutional legitimacy
 *   - Meiji modernization apparatus: set the institutional separation boundary; the partition reading provides post-hoc philosophical justification for state-mandated structural separation
 *   - Syncretic and incoherence reading advocates: excluded from the partition reading's authority structure because their frameworks would dissolve or deny the reading's core premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.31).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.18).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinbutsu Ontological Partition: Separate Domains (Life-Cycle vs Afterlife)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious/philosophical/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, 'bdc1506a-2f84-41dc-88be-1d762d3da296').
narrative_ontology:cs_kernel_codification('bdc1506a-2f84-41dc-88be-1d762d3da296', distributed).
narrative_ontology:cs_authority_grounding('bdc1506a-2f84-41dc-88be-1d762d3da296', lineage).
narrative_ontology:cs_interpretation_layer_present('bdc1506a-2f84-41dc-88be-1d762d3da296').
narrative_ontology:cs_reading_relation('bdc1506a-2f84-41dc-88be-1d762d3da296', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdc1506a-2f84-41dc-88be-1d762d3da296', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('bdc1506a-2f84-41dc-88be-1d762d3da296', foundational, ontological_domain_separation).
narrative_ontology:cs_axiom_status(ontological_domain_separation, holdable).
narrative_ontology:cs_axiom_grounding('bdc1506a-2f84-41dc-88be-1d762d3da296', ontological_domain_separation, deontological).
narrative_ontology:cs_axiom('bdc1506a-2f84-41dc-88be-1d762d3da296', secondary, functional_efficacy_assignment).
narrative_ontology:cs_axiom_status(functional_efficacy_assignment, holdable).
narrative_ontology:cs_axiom_grounding('bdc1506a-2f84-41dc-88be-1d762d3da296', functional_efficacy_assignment, instrumental).
narrative_ontology:cs_reference_frame('bdc1506a-2f84-41dc-88be-1d762d3da296', domain_functional_coherence).
narrative_ontology:cs_drift_state('bdc1506a-2f84-41dc-88be-1d762d3da296', contemporary_religious_studies_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bdc1506a-2f84-41dc-88be-1d762d3da296', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_shrine_practitioners).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_temple_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, household_adherents).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__partition_reading, household_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain Shinto rituals for life-cycle events (birth, marriage, coming-of-age, annual festivals) and community protection (shrine maintenance, seasonal ceremonies). Under the partition reading, Shinto's legitimate domain is the living world, immediate welfare, and local community cohesion. Benefit from interpretive autonomy: do not need to defend Shinto's philosophical status against Buddhist categories or integrate kami into Buddhist metaphysics. Shinto shrine practice is internally coherent and self-justifying within its assigned domain.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_shrine_practitioners, beneficiary,
    organized, generational, mobile, national).

% Maintain Buddhist rituals for death preparation, ancestor veneration (especially Obon and Higan observances), memorial services, and soteriological guidance. Under the partition reading, Buddhism's legitimate domain is the post-mortem realm, karmic continuity, and ultimate liberation. Benefit from doctrinal coherence: do not need to reconcile world-affirming Shinto practice with world-renouncing Buddhist philosophy. Buddhist temple practice can maintain its metaphysical consistency within its assigned domain.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_temple_practitioners, beneficiary,
    organized, generational, mobile, national).

% Individuals and households participate in both Shinto and Buddhist practice for different life purposes: Shinto rituals for birth, childhood safety, coming-of-age, marriage, and prosperity; Buddhist rituals for death preparation, funeral rites, and ongoing ancestor care. Under the partition reading, this dual participation is rationally organized, not doctrinally confused. Households benefit from coherence-framing: they can maintain both allegiances without experiencing cognitive dissonance or requiring philosophical synthesis. They pay in time and resources but receive functional efficacy-ordering (use the tradition suited to the occasion).
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, household_adherents, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, household_adherents, payer).

% Religious scholars, theologians, and institutional interpreters who author, maintain, and defend the partition reading through texts, commentaries, doctrinal elaboration, and institutional practice. Historically: Yoshida Kanetomo (founder of Yoshida Shinto) explicitly framed the partition to defend Shinto autonomy; modern scholars (Tsunoda, Sakurai, Grapard) document partition as a stable hermeneutical strategy. Set the interpretive framework that permits household dual practice without requiring philosophical integration. Benefit from intellectual authority and institutional legitimacy—the partition reading is the dominant frame in Japanese religious studies.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, partition_school_scholars, agenda_setter,
    powerful, generational, arbitrage, national).

% The Meiji modernization state (1868+) formally separated Shinto and Buddhism through shinbutsu-bunri (separation edicts), dismantling dual-temple complexes (jingū-ji), forcing communities to choose institutional affiliation, and redefining Shinto as state religion. The state apparatus sets the structural boundary (institutional separation). The partition reading provides the philosophical justification for this structural rupture—it reframes forced separation as ontologically coherent domain-partition rather than as state-imposed fragmentation. The state does not articulate the partition reading, but the reading's persistence depends on state-maintained institutional separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, meiji_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Scholars and spiritual practitioners who advocate for honji-suijaku (original essence, manifest traces) metaphysics, arguing that kami and buddhas are manifestations of one unified cosmological order under Buddhist philosophical categories. They view dual practice as integration (kami as bodhisattva manifestations) rather than partition. They are excluded from the partition reading's authority structure because accepting their framework would dissolve the reading's foundational distinction. Contemporary syncretic revival (some schools of Nichiren Buddhism, Shugendō lineages) re-integrates kami and buddhas; the partition reading must continually defend against syncretic re-integration pressure.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, syncretic_reading_advocates, excluded,
    powerful, generational, arbitrage, national).

% Religious historians and critics who argue that shinbutsu-shugo (Shinto-Buddhism coexistence) was institutionally tolerated incoherence rather than a coherent ontological commitment. They claim no stable philosophical synthesis existed—both partition and syncretic readings are post-hoc rationalizations of what was actually pragmatic tolerance of logical contradiction. They are excluded from the partition reading's legitimacy structure because accepting their framework would deny the reading's core premise of ontologically justified domain-separation. Modern religious studies critiques (some interpretations of Shinto revival movements, some postmodern readings) deny the coherence the partition reading asserts.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, incoherence_reading_advocates, excluded,
    powerful, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__partition_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the problem of dual religious allegiance without forcing doctrinal coherence: permits households and communities to maintain both Shinto and Buddhist practice by locating each tradition in its functionally appropriate domain (life-welfare vs. death-welfare, immediate vs. ultimate). Each tradition becomes functionalized for the problem it best solves, avoiding the need for philosophical synthesis or institutional choice.
% TRANSFER_FUNCTION: Transfers religious authority and efficacy-attribution: Shinto receives authority over life-cycle rituals, community protection, and immediate prosperity; Buddhism receives authority over death preparation, ancestor veneration, and ultimate liberation. The partition reading allocates each domain to the tradition practitioners experience as efficacious within it, preserving both institutions' social role and resource base without requiring either to subordinate its metaphysics to the other.
% ABSENT_VOICES: Practitioners who experienced genuine ontological anxiety about the apparent contradiction between Shinto and Buddhist metaphysics (those who could not accept the partition without losing religious coherence or who found syncretic integration more compelling) are not represented in the partition reading's authority structure. Kami and buddha entities do not speak (they are not agents in this framework). Indigenous Shinto theorists who actively resisted Buddhist intellectual frameworks during pre-Meiji synthesis periods and preferred explicit independence (not partition-framing) are historically under-documented in the modern partition reading's lineage.
% DISAPPEARANCE_RATIONALE: If the partition reading disappeared and was replaced by explicit incoherence-acknowledgment (denying both partition and syncretic coherence frameworks), Japanese religious practice would continue but its institutional and psychological organization would transform: dual practice would be framed as conscious tolerance of philosophical contradiction (requiring defensive narratives from both traditions); or if replaced by forcible syncretic re-integration, practitioners would require new metaphysical frameworks (honji-suijaku revival) to justify dual participation. The reading's disappearance does not eliminate dual practice itself, but eliminates the coherence-framing that permits simultaneous participation without cognitive dissonance.
% FOUNDING_PROBLEM: Early medieval Japan developed a syncretic Buddhism that incorporated local kami into Buddhist cosmology (honji-suijaku), but this subordinated Shinto's metaphysical independence and conflicted with indigenous Shinto practitioners' self-understanding. By the pre-modern period, tension arose between Buddhist philosophical categories claiming universal explanatory power and Shinto practitioners' experience of kami as autonomous agents warranting independent ritual and reverence. The partition reading emerged as a solution: retain both traditions' practices and philosophical systems by asserting they address non-competing domains (immediate vs. ultimate, life vs. death) rather than competing for explanatory authority over the same phenomena.
% FOUNDING_PROBLEM_CORROBORATION: Pre-modern Shinto theologians (especially Yoshida Kanetomo, founder of Yoshida Shinto in the 15th century) explicitly articulated the partition to defend Shinto autonomy—corroboration from outside the syncretic Buddhist establishment. Kamo no Mabuchi and later Edo-period Shinto scholars authored the partition as a methodological distinction. Modern religious scholars (Tsunoda Banjō, Sakurai Tokutarō, Allan Grapard) document the partition as a historically stable hermeneutical strategy distinct from full syncretism. However, contemporary scholarship contests the founding problem's legitimacy: the incoherence reading argues that the problem was never genuine incoherence but rather pragmatic tolerance of contradiction, which the partition reading merely rationalized post-hoc; the syncretic reading argues the problem was false, since Shinto and Buddhism were always integrated under Buddhist metaphysics (honji-suijaku), and partition-framing is a defensive revision of that integration. No source outside all three reading-communities corroborates the founding problem's status—each reading's account of what problem needed solving differs fundamentally.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.31 at interval end) because the partition reading does not extract from its participants—it distributes authority symmetrically across two traditions, each receiving legitimacy in its assigned domain. No single agent captures the arrangement's gains; both traditions benefit from the coherence-framing. Suppression is very low (0.18) because the reading relies on voluntary interpretive adoption, not coercion—practitioners embrace the partition because it resolves their actual experience of dual practice as rationally organized rather than confused. Theater ratio rises modestly (0.08→0.22 across the interval) as the reading requires increasing scholarly and institutional maintenance to hold against challenges from syncretic re-integration (contemporary honji-suijaku revival) and incoherence acknowledgment (modern religious studies critiques). The measurement series models the reading's stability after Meiji separation: extractiveness plateaus as the reading stabilizes; theater gradually rises as the reading requires active intellectual defense against alternative framings. Time points span 0–150 (pre-modern synthesis era to contemporary period, with future projections); basis shifts to 'projected' at t=100+ because post-contemporary dynamics remain hermeneutical (scenario-dependent).
 *
 * PERSPECTIVAL GAP:
 *   The partition reading presents no inherent perspectival divergence—it frames both traditions as equally benefited by domain separation. However, the reading diverges sharply from the syncretic and incoherence readings: a syncretic reading would compute higher d values for partition advocates (they extract by enforcing separation against preferred integration), and an incoherence reading would compute higher d for partition scholars (they extract by enforcing false coherence). These divergences are not errors but features: they measure how different readings of the same kernel produce different structural classifications. The partition reading's authored metrics (low extractiveness, low suppression) are honest to this reading's internal frame; the syncretic and incoherence readings will author higher extractiveness for the same institutional arrangement because they locate extraction differently (in enforcement of partition, or in covering-up of incoherence).
 *
 * DIRECTIONALITY LOGIC:
 *   No single agent is the target of extraction (d → high-χ) because the partition reading distributes benefits symmetrically: both Shinto and Buddhist traditions gain authority in their respective domains; households gain cognitive coherence for dual practice; the reading's scholarly maintainers gain professional legitimacy and interpretive authority. From the partition reading's internal perspective, directionality for all named seats is near-symmetric or beneficiary-favoring (low d). However, from the syncretic reading's perspective, partition advocates would appear as extractive (enforcing domain-separation against philosophical integration); from the incoherence reading's perspective, partition advocates would appear as extractive (enforcing false-coherence against acknowledged contradiction). The engine computes per-reading directionality; the structural data authored here reflects the partition reading's own frame where all parties benefit or break even.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition reading does not exhibit mandatrophy in its own frame: the founding problem (dual practice without coherence) remains live, the reading addresses it, and the institutional structure persists because the solution is effective (households do practice both traditions without reported ontological crisis). From the syncretic or incoherence readings' perspective, however, the partition reading itself might be mandatrophy: enforcing a false coherence whose founding problem was never real (syncretism was always the actual practice) or whose problem was actually incoherence (which the partition reading merely conceals rather than resolves). The three readings' truth claims diverge in testable ways: if contemporary households report genuine cognitive coherence under the partition frame, that corroborates the founding problem's reality; if households report the partition as post-hoc rationalization covering actual contradiction, that supports the incoherence reading. The measurement trajectory (plateau at high baseline rather than decay to zero) suggests the partition reading's function remains active rather than theatrically maintained—theater_ratio stays below 0.3, indicating functional purpose persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_real_integration,
    'Is the partition reading a genuine ontological commitment that practitioners hold, or a post-hoc rationalization of what was actually syncretic integration from the start?',
    'Ethnographic observation of contemporary practitioner narratives and theological justifications; historical textual analysis comparing pre-modern synthesis-era explanations (from partition-school authors) with non-partisan observers'' accounts of actual practice; examination of whether practitioners spontaneously report domain-separation logic or whether it requires scholarly prompting.',
    'If practitioners genuinely hold partition logic as their interpretive frame, the reading is a live hermeneutical reality and the ε authoring (low extractiveness, no suppression) is accurate. If the partition is a scholarly overlay on actual syncretic integration, the reading itself becomes extractive (scholars enforce false-coherence) and ε would be re-authored higher; the constraint would then be snare, not rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_real_integration, empirical, 'Whether the partition reading is a genuine lived interpretive frame or a scholarly rationalization of syncretic practice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is suppression in maintaining this reading structural (legal prohibitions on dual-temple complexes, state enforcement of separation) or internalized (practitioners have absorbed partition logic into their religious self-concept)?',
    'Post-separation ethnography: observe whether practitioners maintain partition framing after external legal enforcement is relaxed or after extended exposure to contrary framings (syncretic revival movements, incoherence critiques). If partition logic persists after legal suppression is removed, it is at least partially internalized; if it reverses, suppression is primarily structural.',
    'If suppression is primarily structural, the authored base_properties.suppression=0.18 accurately reflects the external machinery. If suppression is internalized, effective suppression is higher than the structural measure—the reading''s persistence is more robustly enforced by practitioners'' own cognitive frameworks. This would elevate the constraint''s true extraction (targets carry suppression with them post-separation, making exit harder than the base measure suggests).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the partition reading''s maintenance.').

omega_variable(
    ontological_status_of_partition,
    'Is the partition an ontological fact (kami and buddhas genuinely operate in separate domains) or a hermeneutical choice (practitioners choose to interpret them as separate, but other interpretations are equally coherent)?',
    'Comparative cosmology: analyze whether Shinto and Buddhist metaphysics, taken on their own terms, can consistently posit domain separation without forcing contradiction or requiring one tradition to subordinate the other. If both traditions can maintain their metaphysical systems under partition logic, it is hermeneutically coherent but not ontologically mandatory; if one tradition requires integration or explicitly denies partition, the partition is a choice that suppresses that tradition''s own logic.',
    'If partition is hermeneutically chosen rather than ontologically mandated, the reading''s authority depends on practitioners'' continued adoption—it is less stable than if it were a discovered ontological fact. Under this condition, the constraint''s stability derives from maintenance activity (theater_ratio may be under-measured), and withdrawal of that activity would cause reorganization. The syncretic and incoherence readings would then appear not as false competitors but as alternative equally-coherent framings, and the partition reading''s persistence would depend on institutional power rather than truth-discovery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_status_of_partition, conceptual, 'Whether partition is ontologically necessary or hermeneutically chosen.').

omega_variable(
    beneficiary_identity_fusion,
    'Do practitioners'' identities (as Shinto adherents, Buddhist adherents, or Japanese) become fused with the partition reading such that adopting a rival reading (syncretic or incoherence) would feel like identity loss?',
    'Ethnographic study of practitioners exposed to alternative readings: do they experience rival framings as intellectual challenges (resolvable via debate) or as threats to identity (requiring defensive commitment to partition logic regardless of rational arguments)? Observe whether practitioners can hold syncretic or incoherence readings as live intellectual options or whether the partition reading becomes a non-negotiable identity marker.',
    'If identity is fused with the partition reading, practitioners'' exit options are identity-locked rather than mobile or constrained. This would elevate suppression beyond the structural measure (internal identity constraints are harder to overcome than external legal prohibitions) and make the reading more resistant to intellectual challenge. The measuring of base_properties.suppression=0.18 would then underestimate the reading''s actual hold, and the constraint''s classification could shift from rope (symmetric coordination) to snare (with identity-locked targets).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_fusion, empirical, 'Degree of identity fusion with the partition reading among practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(shin_tr_t25, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement(shin_tr_t50, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 50, 0.16).
narrative_ontology:measurement(shin_tr_t75, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement(shin_tr_t125, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 125, 0.22).
narrative_ontology:measurement(shin_tr_t150, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 150, 0.22).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(shin_be_t25, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(shin_be_t50, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 50, 0.26).
narrative_ontology:measurement(shin_be_t75, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 75, 0.3).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 100, 0.31).
narrative_ontology:measurement(shin_be_t125, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 125, 0.31).
narrative_ontology:measurement(shin_be_t150, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 150, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(shin_su_t25, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 25, 0.08).
narrative_ontology:measurement(shin_su_t50, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement(shin_su_t75, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 75, 0.15).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 100, 0.18).
narrative_ontology:measurement(shin_su_t125, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 125, 0.18).
narrative_ontology:measurement(shin_su_t150, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 150, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__partition_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu (Shinto-Buddhism coexistence) kernel contests across three constraint stories. This story instantiates the PARTITION_READING: Shinto and Buddhism occupy separate ontological domains (life vs. afterlife) without integration. The SYNCRETIC_READING (sibling) posits unified cosmology under honji-suijaku metaphysics, producing higher extractiveness (enforcement of coherence against rival readings). The INCOHERENCE_READING (sibling) denies stable commitment, producing lower extractiveness (permits acknowledged contradiction). Each reading produces a different constraint with different ε, directionality, and stakeholder structures. The three are linked via network.affects_constraints: the partition reading's persistence constrains the plausibility of syncretic re-integration (influences relation); the syncretic reading's revival would undermine partition authority (coexists_with relation); the incoherence reading forecloses both partition and syncretic claims of coherence (forecloses relation). Each story's ε is stable within its reading but differs across readings: the same institutional arrangement (households practicing both traditions) yields ε=0.31 (partition), ε~0.55+ (syncretic, due to extraction from enforced integration), ε~0.15 (incoherence, permitting acknowledged contradiction). Decomposition follows ε-invariance principle: the referent is identical (Japanese religious practice), but the reading's epistemic stance produces different ε valuations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
