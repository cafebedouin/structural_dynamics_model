% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__hybrid_reading, []).

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
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Hybrid Classical-Ecclesiastical Latin Correctness Standard
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the
 *   classical_latin_standard kernel: correct Latin requires Classical textual
 *   fidelity as the baseline AND recognition of legitimate post-Classical
 *   technical/ecclesiastical vocabulary as a bounded exception class. This is
 *   distinct from the continuity_reading (which treats all natural drift as
 *   legitimate, sibling constraint) and the reconstruction_reading (which
 *   rejects all medieval drift, sibling constraint). The hybrid reading's
 *   structural signature is a reduced but real victim set — only usages
 *   classed as 'barbarisms' (unsanctioned vernacular-influenced or regional
 *   drift) are delegitimized, while a curated set of technical/ecclesiastical
 *   coinages is grandfathered in because institutions found them useful. This
 *   produces a standard that looks moderate on every axis relative to its
 *   siblings: less suppressive than reconstruction (which would reject nearly
 *   all post-Classical forms), more suppressive than continuity (which
 *   rejects almost nothing), and moderately extractive because the
 *   accommodation is selective rather than principled.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.48).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.42).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Hybrid Classical-Ecclesiastical Latin Correctness Standard").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, '1ccbfbea-6922-464c-8253-a44d36fea4ca').
narrative_ontology:cs_kernel_codification('1ccbfbea-6922-464c-8253-a44d36fea4ca', distributed).
narrative_ontology:cs_authority_grounding('1ccbfbea-6922-464c-8253-a44d36fea4ca', lineage).
narrative_ontology:cs_interpretation_layer_present('1ccbfbea-6922-464c-8253-a44d36fea4ca').
narrative_ontology:cs_reading_relation('1ccbfbea-6922-464c-8253-a44d36fea4ca', classical_latin_standard__continuity_reading, influences).
narrative_ontology:cs_reading_relation('1ccbfbea-6922-464c-8253-a44d36fea4ca', classical_latin_standard__reconstruction_reading, influences).
narrative_ontology:cs_axiom('1ccbfbea-6922-464c-8253-a44d36fea4ca', foundational, classical_baseline_with_bounded_technical_exception).
narrative_ontology:cs_axiom_status(classical_baseline_with_bounded_technical_exception, holdable).
narrative_ontology:cs_axiom_grounding('1ccbfbea-6922-464c-8253-a44d36fea4ca', classical_baseline_with_bounded_technical_exception, conventional).
narrative_ontology:cs_axiom('1ccbfbea-6922-464c-8253-a44d36fea4ca', secondary, institutional_adjudication_of_legitimate_accretion).
narrative_ontology:cs_axiom_status(institutional_adjudication_of_legitimate_accretion, holdable).
narrative_ontology:cs_axiom_grounding('1ccbfbea-6922-464c-8253-a44d36fea4ca', institutional_adjudication_of_legitimate_accretion, instrumental).
narrative_ontology:cs_reference_frame('1ccbfbea-6922-464c-8253-a44d36fea4ca', augustan_ciceronian_baseline_with_curial_accretion).
narrative_ontology:cs_drift_state('1ccbfbea-6922-464c-8253-a44d36fea4ca', post_tridentine_codification_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1ccbfbea-6922-464c-8253-a44d36fea4ca', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, seminary_and_curial_latinists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, classical_philology_faculties).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, vatican_editorial_bodies).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, vernacular_influenced_scribes).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, regional_medieval_latin_writers).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, self_taught_ecclesiastical_composers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, self_taught_ecclesiastical_composers).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, classical_norm_as_baseline_authority).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, domain_specific_lexical_exception_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compose and adjudicate liturgical and curial Latin, applying Classical syntax and morphology while retaining an approved technical vocabulary (ecclesiastical, legal, scholastic terms). They author the style guides and editorial norms (e.g. for papal documents) that decide which post-Classical forms count as legitimate accretion versus error. Their professional standing depends on mastering this exact hybrid competence.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, seminary_and_curial_latinists, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, seminary_and_curial_latinists, beneficiary).

% Teach and certify Latin competence against a norm that privileges Classical grammar as the measuring stick even where technical vocabulary is admitted. Their curricular and credentialing authority is reinforced by a standard that treats Classical fidelity as the default and post-Classical usage as a bounded exception requiring justification.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, classical_philology_faculties, beneficiary,
    institutional, generational, analytical, global).

% Produce and revise official Latin texts (e.g. Nova Vulgata, liturgical books), exercising the discretionary authority to declare which medieval or technical forms are 'legitimate developments' and which are barbarisms. This gatekeeping function is itself a source of institutional prestige and control over textual authority.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, vatican_editorial_bodies, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, vatican_editorial_bodies, agenda_setter).

% Historically and in later pedagogical practice, writers whose Latin carries syntactic or lexical influence from their native vernacular are marked as producing incorrect Latin, even when their forms are functionally intelligible and historically attested. They cannot argue their usage into legitimacy the way an institutionally sanctioned technical term can be argued into the accepted exception list.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, vernacular_influenced_scribes, payer,
    powerless, biographical, trapped, regional).

% Authors of medieval charters, chronicles, and correspondence whose Latin reflects genuine regional linguistic development are retroactively judged by a hybrid standard that admits only the subset of medieval innovation that later institutions found useful (ecclesiastical/technical terms) while classing the rest as decline or corruption. Their actual usage had no say in which developments would be canonized.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, regional_medieval_latin_writers, payer,
    powerless, generational, trapped, regional).

% Parish-level or provincial clergy and administrators who write functional Latin for practical ecclesiastical purposes. They benefit somewhat from the hybrid standard's accommodation of technical vocabulary, but are still penalized in formal assessment when their broader syntax drifts from Classical norms in ways the standard has not pre-approved — leaving them exposed on one axis while nominally accommodated on another.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, self_taught_ecclesiastical_composers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, self_taught_ecclesiastical_composers, beneficiary).

% Philologists who hold that only rigorous return to attested Classical usage constitutes correctness reject the hybrid standard's accommodations as unprincipled concessions to institutional convenience. They participate in the same academic conversations but their reconstruction-only position is treated as a minority critique rather than incorporated into the operative hybrid norm.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, modern_classicists_reconstructionist_faction, excluded,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__hybrid_reading, vatican_editorial_bodies).
narrative_ontology:fixing_cost_class(classical_latin_standard__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single teachable, gradable norm for Latin correctness that lets institutions (seminaries, curial offices, universities) certify competence and produce interoperable technical/liturgical texts without every writer relitigating which century's usage counts.
% TRANSFER_FUNCTION: Moves prestige, credentialing authority, and interpretive legitimacy toward institutions that already possess Classical training plus curated technical vocabularies, and moves the burden of proof onto writers whose Latin reflects unsanctioned regional or vernacular-influenced development.
% ABSENT_VOICES: The reconstructionist faction argues the accommodations are arbitrary; excluded medieval and regional writers, long dead, cannot contest the retroactive judgment applied to their own attested usage — their actual historical practice is evidence for continuity readings but is filtered out here except where it matches institutionally useful technical terms.
% DISAPPEARANCE_RATIONALE: Institutional users (seminaries, philology faculties, Vatican bodies) would need to construct a replacement adjudication standard to keep certifying and editing texts, so for them the world rearranges. For most working Latinists and casual users, however, the practical difference between the hybrid standard and either sibling reading is small day-to-day, since the disputed territory is mostly boundary cases — hence contested rather than a clean verdict either way.
% FOUNDING_PROBLEM: By the early modern and Counter-Reformation periods, an unregulated mix of genuinely useful ecclesiastical/technical Latin coinages and increasingly vernacular-corrupted prose made it hard to certify a text or a writer as competently Latinate at all; the hybrid standard was built to preserve intelligibility and continuity with authoritative texts while not discarding centuries of necessary technical vocabulary.
% FOUNDING_PROBLEM_CORROBORATION: Vatican editorial bodies and seminary Latinists attest the problem remains live (liturgical and curial texts still require adjudicated correctness). Independent historical linguists outside these institutions — who study the actual corpus of medieval and regional Latin without an institutional stake in certification — largely attest that the 'barbarism' category tracks institutional convenience more than any principled linguistic criterion, i.e., the founding problem as originally framed was already partly a pretext for a jurisdiction the beneficiary institutions wanted to hold.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, contested).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__hybrid_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) and suppression (0.42) sit in the middle of the plausible range for this kernel because the hybrid reading's whole structural point is selective accommodation: it does not suppress all deviation (as reconstruction would) nor tolerate all deviation (as continuity would). The rising trajectory across the interval reflects the standard's gradual institutional hardening — as more technical vocabulary accumulates canonical status and more editorial bodies formalize the exception list, the boundary between 'legitimate development' and 'barbarism' becomes more rigorously enforced rather than less, even though the total suppression stays moderate compared to a reconstruction-only norm. Accessibility_collapse (0.4) and resistance (0.45) are moderate: unlike a genuine mountain, real institutional and philological contestation over the boundary persists (the reconstructionist faction actively argues the exceptions are unprincipled), but the disputed territory is bounded rather than total.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional Latinists (seminary/curial, philology faculties, Vatican editorial bodies) are structural beneficiaries: they both compose according to the hybrid norm AND administer the adjudication process that decides which post-Classical forms are legitimized, giving them low d. Vernacular-influenced scribes and regional medieval writers are the clearest targets: their usage is judged against a standard they had no voice in constructing, and their exit options are trapped (they are historical agents whose written record cannot be revised or defended after the fact). Self-taught ecclesiastical composers occupy a genuinely intermediate position — partially beneficiaries of the technical-vocabulary accommodation, partially payers when their broader syntax is judged non-Classical — reflected in their dual role and constrained (not trapped) exit, since they are living actors who could in principle acquire more Classical training.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading's founding problem (need to certify/edit texts amid genuine technical vocabulary growth and encroaching vernacular corruption) is genuinely contested as live vs. dead: institutional users still need adjudication machinery for liturgical/curial production, so declaring the standard purely mandatrophic would be premature. But the corroboration from outside the beneficiary institutions — independent historical linguists studying the full medieval corpus without institutional stake — suggests the 'barbarism' boundary has always tracked institutional convenience as much as linguistic principle, which is a live mandatrophy candidate for the SPECIFIC exception-list mechanism (which forms get grandfathered) even if the general coordination need persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_forms_get_grandfathered,
    'Is the boundary between ''legitimate post-Classical development'' and ''barbarism'' drawn on principled linguistic grounds, or does it simply track which forms institutional users (curial, ecclesiastical, scholastic) happened to find useful?',
    'Comparative corpus analysis of grandfathered technical terms versus excluded regional/vernacular forms of comparable linguistic novelty and period, checking whether usefulness-to-institution predicts admission better than any independent grammatical or phonological criterion.',
    'If admission tracks institutional usefulness rather than linguistic principle, the hybrid reading''s coordination story (a principled middle path) is substantially cover for the same extraction the reconstruction reading performs, just with better PR — pushing the classification toward snare-adjacent tangled_rope. If admission tracks genuine, defensible linguistic criteria independent of institutional interest, the coordination function is more real than extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_forms_get_grandfathered, empirical, 'Whether the hybrid standard''s exception list is principled or institutionally self-serving.').

omega_variable(
    hybrid_reading_kernel_framing,
    'Is the classical_latin_standard kernel genuinely three-way contested (continuity, hybrid, reconstruction), or is the hybrid reading actually just the reconstruction reading with a public-relations layer of accommodation added to make its extraction more palatable to working ecclesiastical Latinists?',
    'Trace institutional practice over centuries: if the set of ''legitimate'' post-Classical accommodations has been narrowing over time toward the reconstruction reading''s position, the hybrid reading is a transitional or camouflage state rather than a stable independent reading.',
    'If the hybrid reading is drifting toward reconstruction, the ''moderate'' ε and suppression values authored here describe a snapshot of a moving target rather than a stable equilibrium reading, and the network relation to reconstruction_reading should be understood as convergent rather than merely coexisting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_reading_kernel_framing, conceptual, 'Whether hybrid is a stable third reading or a transitional camouflage of the reconstruction reading.').

omega_variable(
    victim_set_stability,
    'Is the reduced victim set (only ''barbarisms'', not all drift) stable across the interval, or does the definition of barbarism expand as institutional editorial bodies harden their canon?',
    'Track successive editions of authoritative texts (e.g. papal Latin style guides, Vatican editorial norms) for whether the list of tolerated technical forms grows, shrinks, or stays fixed relative to the pool of attested medieval usage.',
    'An expanding barbarism category over time would corroborate the rising suppression_requirement trajectory authored in the measurements and support treating later-period instances of this reading as closer to the reconstruction reading in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_stability, empirical, 'Whether the exception class narrows over time, converging the hybrid reading toward reconstruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clas_tr_t20, classical_latin_standard__hybrid_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(clas_tr_t40, classical_latin_standard__hybrid_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(clas_tr_t60, classical_latin_standard__hybrid_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement(clas_tr_t80, classical_latin_standard__hybrid_reading, theater_ratio, 80, 0.29).
narrative_ontology:measurement(clas_tr_t100, classical_latin_standard__hybrid_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clas_be_t20, classical_latin_standard__hybrid_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(clas_be_t40, classical_latin_standard__hybrid_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(clas_be_t60, classical_latin_standard__hybrid_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement(clas_be_t80, classical_latin_standard__hybrid_reading, base_extractiveness, 80, 0.47).
narrative_ontology:measurement(clas_be_t100, classical_latin_standard__hybrid_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clas_su_t20, classical_latin_standard__hybrid_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(clas_su_t40, classical_latin_standard__hybrid_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(clas_su_t60, classical_latin_standard__hybrid_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(clas_su_t80, classical_latin_standard__hybrid_reading, suppression_requirement, 80, 0.41).
narrative_ontology:measurement(clas_su_t100, classical_latin_standard__hybrid_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).

% DUAL FORMULATION NOTE:
% Three constraints instantiate the classical_latin_standard kernel: continuity_reading (low suppression, near-rope, all drift legitimate), hybrid_reading (this story — moderate suppression and extractiveness, tangled_rope, selective legitimation), and reconstruction_reading (highest suppression and extractiveness, most snare-like, all medieval drift rejected). Each carries its own stable ε per the ε-invariance principle; they are linked here rather than merged because they instantiate structurally distinct victim sets and beneficiary structures despite sharing a natural-language label ('correct Latin').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
