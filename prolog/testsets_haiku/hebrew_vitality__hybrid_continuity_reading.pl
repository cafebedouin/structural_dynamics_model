% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality_hybrid_continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hebrew Vitality: Hybrid Continuity Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   The Hebrew vitality kernel asks: what constitutes a language's 'vitality'
 *   when it has undergone diaspora, liturgical continuity, and modern
 *   revitalization? Three readings decompose this question. The
 *   hybrid_continuity_reading claims that both liturgical preservation and
 *   vernacular reconstruction were necessary — neither alone explains how
 *   Modern Hebrew emerged with both textual fidelity AND native generational
 *   transfer. This reading is not an actor's position; it is an analytical
 *   synthesis attempting to transcend the binary between pure liturgical and
 *   pure native-speech framings by showing why each captures one necessary
 *   condition but neither alone suffices.
 *
 * KEY AGENTS:
 *   - Liturgical tradition custodians: maintained textual and ritual continuity across diaspora (2000 years)
 *   - Native generation movement (late 19th–early 20th century): rebuilt Hebrew as daily vernacular through education, standardization, and community adoption
 *   - Diaspora communities: sustained liturgical Hebrew while adopting vernacular languages for daily life
 *   - Modern Hebrew speakers: inherit a language that is both liturgically rooted and reconstructively completed
 *   - Linguistic historians: document the dual process and assess competing vitality claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.15).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.08).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.12).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hebrew Vitality: Hybrid Continuity Reading").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, 'c5a991fc-1598-446f-8c3d-2b41a9f2a94a').
narrative_ontology:cs_kernel_codification('c5a991fc-1598-446f-8c3d-2b41a9f2a94a', distributed).
narrative_ontology:cs_authority_grounding('c5a991fc-1598-446f-8c3d-2b41a9f2a94a', expertise).
narrative_ontology:cs_reading_relation('c5a991fc-1598-446f-8c3d-2b41a9f2a94a', hebrew_vitality__liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5a991fc-1598-446f-8c3d-2b41a9f2a94a', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_axiom('c5a991fc-1598-446f-8c3d-2b41a9f2a94a', foundational, lithurgical_preservation_necessary_condition).
narrative_ontology:cs_axiom_status(lithurgical_preservation_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('c5a991fc-1598-446f-8c3d-2b41a9f2a94a', lithurgical_preservation_necessary_condition, empirically_contingent).
narrative_ontology:cs_axiom('c5a991fc-1598-446f-8c3d-2b41a9f2a94a', foundational, vernacular_reconstruction_necessary_condition).
narrative_ontology:cs_axiom_status(vernacular_reconstruction_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('c5a991fc-1598-446f-8c3d-2b41a9f2a94a', vernacular_reconstruction_necessary_condition, empirically_contingent).
narrative_ontology:cs_reference_frame('c5a991fc-1598-446f-8c3d-2b41a9f2a94a', dual_substrate_and_reconstruction).
narrative_ontology:cs_drift_state('c5a991fc-1598-446f-8c3d-2b41a9f2a94a', contemporary_linguistic_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c5a991fc-1598-446f-8c3d-2b41a9f2a94a', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, synthesis_frameworks).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, revitalization_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, liturgical_tradition_custodians).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, native_generation_movement).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, modern_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_vitality__hybrid_continuity_reading, diaspora_communities).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, liturgical_preservation_as_substrate).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, vernacular_reconstruction_as_completion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Guardians of Hebrew liturgical texts and ritual use across diaspora communities. Their preservation activities are recognized by this reading as the substrate that enabled later revitalization — their historical work is vindicated, not displaced. They maintain textual fidelity and liturgical transmission as continuous practice.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_tradition_custodians, beneficiary,
    institutional, civilizational, identity_locked, global).

% The community of linguists, educators, and revitalization advocates who rebuilt Hebrew as a daily vernacular language beginning in the late 19th century. This reading frames their work as reconstruction grounded in a liturgical substrate — they are not creating from scratch, but rebuilding a language with living memory embedded in ritual and text.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, native_generation_movement, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, native_generation_movement, agenda_setter).

% Jewish communities across centuries who sustained liturgical Hebrew transmission while their vernaculars shifted to local languages (Yiddish, Ladino, Arabic, etc.). They carried the substrate but did not natively speak Hebrew as daily language. This reading acknowledges both their transmission cost and the reconstruction work required to make Hebrew live again beyond liturgy.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, diaspora_communities, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, diaspora_communities, beneficiary).

% Contemporary native speakers of Hebrew whose linguistic competence depends on both the reconstructed grammatical and lexical system AND the continuous substrate of liturgical memory embedded in the language's deepest layers. They speak a language that could only exist because both traditions were maintained.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, modern_hebrew_speakers, beneficiary,
    moderate, biographical, mobile, regional).

% Scholars analyzing how Hebrew vitality actually emerged — examining texts, grammars, reconstructions, liturgical continuity, and community adoption patterns. They take empirical positions on whether liturgical preservation alone could sustain vitality or whether reconstruction was necessary.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, linguistic_historians, observer,
    analytical, biographical, analytical, global).

% Holders of the liturgical_reading (ritual is sufficient) and native_daily_reading (only native speech counts) positions. This reading's synthesis excludes the pure adequacy claims that define those readings — it brackets the question of sufficiency by treating both elements as necessary. Its excludes would object that bracketing the contest concedes territory to both sides rather than adjudicating it.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, competing_reading_advocates, excluded,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves an apparent paradox in language vitality: how Hebrew remained a functional living language when no diaspora community spoke it natively as a vernacular for nearly two millennia. The hybrid reading coordinates liturgical preservation and vernacular reconstruction as complementary rather than competing processes — the first maintains the substrate, the second rebuilds the full speech ecosystem.
% TRANSFER_FUNCTION: This constraint does not transfer resources or extract from actors; it transfers interpretive authority. The reading moves the vitality question from 'which camp was right' (liturgical custodians vs. revivalist reconstructors) to 'what conditions were necessary and what reconstructive work was required.' It distributes legitimacy across both traditions rather than concentrating it in one.
% ABSENT_VOICES: The voices excluded are the advocates of the pure liturgical_reading and pure native_daily_reading positions. They cannot be here because the hybrid reading's core move is to deny the sufficiency of either alone — including them would dissolve the reading's framing. Their objection would be that synthesis masks rather than resolves the question of whether unbroken liturgical use constitutes vitality (their claim) or whether only native generation does (counter-claim).
% DISAPPEARANCE_RATIONALE: This constraint is an analytical reading, not a structural arrangement organizing actors' lives. If this particular framing disappeared, Hebrew speakers would continue speaking Hebrew, liturgical communities would continue their practice, and historical events would remain as they occurred. What would disappear is ONE interpretation of how those events fit together — a reframing, not a material arrangement. The world rearranged itself when revitalization occurred; the question of which reading best accounts for it does not organize ongoing activity.
% FOUNDING_PROBLEM: How to understand Hebrew vitality given two seemingly contradictory empirical facts: (1) Hebrew was continuously used in liturgy across the diaspora with no native speakers, and (2) Hebrew revitalization in the modern period required intentional reconstruction and community adoption, not spontaneous eruption of native speech from liturgical seeds. The pure liturgical reading claims liturgical use alone is sufficient to call Hebrew 'vital'; the pure native reading claims only native speech creates vitality. The founding problem is how these facts coexist.
% FOUNDING_PROBLEM_CORROBORATION: Linguistic historians and revitalization scholars (outside both the liturgical custodian tradition and the native-speech movement) document both the continuous liturgical transmission and the intentional reconstructive work. Historical linguists verify that Modern Hebrew's lexicon includes systematic innovations not present in liturgical Hebrew (neologisms, borrowed terms, grammatical regularizations). Scholars of revitalization processes note that Hebrew uniquely combined continuous textual substrate with intentional re-nativization — neither pure liturgical continuity nor pure reconstruction from historical sources appears in isolation in other language revitalization cases.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).
:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because this is a reading, not a coercive arrangement. It does not extract resources or suppress alternatives — it proposes a framework for understanding two empirical processes. Suppression is minimal (0.08) because no power structure forces adherence to this framing; scholars and communities can adopt or reject it. Theater ratio is negligible (0.05) because the reading has no performed function — it is analytical. Accessibility_collapse and resistance are both low because the reading competes with other interpretations in a scholarly domain where alternatives are available and explicitly discussed. The measurement series are flat because this constraint has no lifecycle drift — it is a stable analytical synthesis without accumulation or decay.
 *
 * PERSPECTIVAL GAP:
 *   Liturgical custodians may experience this reading as vindicating their preservation work (low d, beneficiary seat). Revitalization activists may experience it as requiring their reconstruction (payer/agenda-setter seat — they had to do the work the hybrid reading describes). Linguists take an analytical position (d near 0.5, observer seat). The engine should compute this divergence from the structural data — different seats perceive the reading differently because it allocates necessary-ness to different traditions.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading names two beneficiaries (synthesis_frameworks and revitalization_practitioners) that are non-agent entities — intellectual frameworks and a professional community — not real actors extracting resources. The real actors (liturgical custodians, native speakers, diaspora communities) are beneficiaries in the sense that the reading validates their historical contributions, but the reading compels nothing. This is a low-extraction analytical constraint that distributes vindication across multiple traditions rather than concentrating legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not suffer mandatrophy because its founding problem (how to understand dual liturgical and reconstructive continuity) remains live. The readings do not attempt to substitute an obsolete justification for current operation; they offer alternative accounts of a historical/linguistic phenomenon that continues to matter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_vs_necessity_framing,
    'Does bracketing the contest between sufficiency claims (liturgical preservation alone is sufficient vs. native generation alone is sufficient) constitute a genuine resolution, or does it defer the underlying disagreement?',
    'Examining whether advocates of the pure liturgical and pure native readings accept the hybrid reading as capturing their position''s necessary contribution, or whether they still maintain their original sufficiency claims and view the hybrid reading as concessive rather than synthetic.',
    'If advocates accept the hybrid reading as capturing their truth, the three readings genuinely form a coherent set (hybrid = integration of both necessary conditions). If they reject it as concessive, the readings remain in contest and the hybrid is a third position, not a synthesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_vs_necessity_framing, conceptual, 'Whether the hybrid reading resolves or defers the vitality contest.').

omega_variable(
    liturgical_substrate_vs_contingent_preservation,
    'Did liturgical preservation provide a structurally necessary substrate for revitalization (enabling the reconstruction to succeed because the language was not lost), or was it contingent — could revitalization have succeeded with only historical texts and no living liturgical tradition?',
    'Comparative analysis of language revitalization cases with and without continuous liturgical/ritual use; linguistic analysis of how much Modern Hebrew''s coherence depends on liturgical transmission vs. how much was reconstructed from historical sources.',
    'If necessary: the hybrid reading''s claim that liturgical preservation was necessary is correct. If contingent: revitalization would have succeeded anyway, and liturgical preservation''s role is overstated in the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_substrate_vs_contingent_preservation, empirical, 'Whether the liturgical tradition was structurally necessary for revitalization or a contingent enabler.').

omega_variable(
    reconstruction_degree_and_nativity_timeline,
    'How much of Modern Hebrew required intentional reconstruction vs. spontaneous development from liturgical substrate, and at what point in revitalization did native generational transfer begin?',
    'Detailed lexical and grammatical analysis of Modern Hebrew''s innovations; historical documentation of when communities adopted Hebrew as a primary language; study of learner language vs. native development in early revitalization cohorts.',
    'High reconstruction + late nativity: the hybrid reading is correct that reconstruction was a major necessary component. Low reconstruction + early nativity: the native_daily_reading may be correct that the reading occurred earlier than the hybrid reading claims. Early nativity + high reconstruction: the readings may be describing different phases of the same process.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstruction_degree_and_nativity_timeline, empirical, 'The relative magnitude of reconstruction vs. organic development in Modern Hebrew''s emergence.').

omega_variable(
    kernel_contest_unresolved,
    'Is this reading one interpretation of a genuinely contested kernel, or does it presume a resolution the competing readings would reject?',
    'Direct engagement with advocates of liturgical_reading and native_daily_reading to determine whether they accept the hybrid reading as a legitimate synthesis or view it as a third position that misses their essential claim.',
    'If legitimate synthesis: the three readings form a coherent account. If disputed: the kernel remains unresolved and the three readings coexist as incommensurable frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_unresolved, conceptual, 'Whether the kernel contest is genuinely resolved by the hybrid reading or remains in tension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t25, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement_basis(hebr_tr_t25, observed).
narrative_ontology:measurement(hebr_tr_t50, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement_basis(hebr_tr_t50, observed).
narrative_ontology:measurement(hebr_tr_t75, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 75, 0.05).
narrative_ontology:measurement_basis(hebr_tr_t75, observed).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement_basis(hebr_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t25, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement_basis(hebr_be_t25, observed).
narrative_ontology:measurement(hebr_be_t50, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement_basis(hebr_be_t50, observed).
narrative_ontology:measurement(hebr_be_t75, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 75, 0.15).
narrative_ontology:measurement_basis(hebr_be_t75, observed).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 100, 0.15).
narrative_ontology:measurement_basis(hebr_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__hybrid_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__native_daily_reading).

% DUAL FORMULATION NOTE:
% The hebrew_vitality kernel decomposes into three constraints representing three readings. The hybrid_continuity_reading claims that both liturgical preservation (the substrate of the liturgical_reading) and vernacular reconstruction (the central fact of the native_daily_reading) were necessary. The liturgical_reading instantiates the claim that ritual use alone constitutes vitality. The native_daily_reading instantiates the claim that only native generational transmission constitutes true vitality. These three readings represent different resolutions of the same kernel: what makes Hebrew vital? The ε-invariance principle applies to the kernel itself — the three readings have different ε values because they make different structural claims about which conditions are sufficient, necessary, or both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
