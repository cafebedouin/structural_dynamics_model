% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Classical Latin Standard (Hybrid Reading)
 *   domain: historical_linguistics/commitment_systems
 *
 * SUMMARY:
 *   The Classical Latin standard, under the hybrid reading, asserts that
 *   correct Latin requires both fidelity to Classical Ciceronian and Augustan
 *   norms AND explicit recognition of legitimate technical, ecclesiastical,
 *   and scholastic post-Classical developments. This reading occupies the
 *   middle ground between strict Humanist reconstruction (which rejects all
 *   medieval drift as barbarism) and continuity theory (which treats all
 *   inherited forms as naturally legitimate). The hybrid reading's structural
 *   innovation is to legitimize SOME post-Classical forms (ecclesiastical
 *   vocabulary, medieval technical terminology, liturgical usage) while
 *   simultaneously maintaining that unauthorized departures from Classical
 *   syntax and morphology are illegitimate. This creates a bifurcated
 *   legitimacy system: a form is correct if either (a) it appears in
 *   Classical texts or (b) it has been explicitly approved by ecclesiastical
 *   or institutional authority as domain-appropriate. Unapproved medieval
 *   innovations fall into the excluded set. The constraint operates as a
 *   partial suppression of linguistic drift — not rejection of all medieval
 *   forms (which would alienate the Church and technical practitioners), but
 *   delegitimization of forms that lack either textual or institutional
 *   warrant.
 *
 * KEY AGENTS:
 *   - Institutional Classical educators: set standards via university faculties and philological societies; benefit by retaining gatekeeping authority while appearing accommodating
 *   - Ecclesiastical Latin practitioners: benefit from legitimized medieval liturgical vocabulary while remaining subordinate to Classical norms in non-domain-specific writing
 *   - Vernacular drift speakers: bear the cost of delegitimization; their forms are treated as barbarisms unless they can be defended institutionally
 *   - Reconstruction advocates (excluded): would impose stricter standards; their voice is systematically marginalized
 *   - Continuity advocates (excluded): would legitimize all inherited forms; their position is incompatible with the hybrid reading's bifurcated legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.48).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.52).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Classical Latin Standard (Hybrid Reading)").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "historical_linguistics/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, '6272735f-2175-4c89-a8f8-1ee1419c8562').
narrative_ontology:cs_kernel_codification('6272735f-2175-4c89-a8f8-1ee1419c8562', fixed_text).
narrative_ontology:cs_authority_grounding('6272735f-2175-4c89-a8f8-1ee1419c8562', lineage).
narrative_ontology:cs_interpretation_layer_present('6272735f-2175-4c89-a8f8-1ee1419c8562').
narrative_ontology:cs_reading_relation('6272735f-2175-4c89-a8f8-1ee1419c8562', classical_latin_standard__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6272735f-2175-4c89-a8f8-1ee1419c8562', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_axiom('6272735f-2175-4c89-a8f8-1ee1419c8562', foundational, classical_texts_are_authoritative_baseline).
narrative_ontology:cs_axiom_status(classical_texts_are_authoritative_baseline, holdable).
narrative_ontology:cs_axiom_grounding('6272735f-2175-4c89-a8f8-1ee1419c8562', classical_texts_are_authoritative_baseline, conventional).
narrative_ontology:cs_axiom('6272735f-2175-4c89-a8f8-1ee1419c8562', foundational, post_classical_institutional_legitimacy_is_admissible).
narrative_ontology:cs_axiom_status(post_classical_institutional_legitimacy_is_admissible, holdable).
narrative_ontology:cs_axiom_grounding('6272735f-2175-4c89-a8f8-1ee1419c8562', post_classical_institutional_legitimacy_is_admissible, conventional).
narrative_ontology:cs_reference_frame('6272735f-2175-4c89-a8f8-1ee1419c8562', classical_textual_authority_with_institutional_accommodation).
narrative_ontology:cs_drift_state('6272735f-2175-4c89-a8f8-1ee1419c8562', post_humanist_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6272735f-2175-4c89-a8f8-1ee1419c8562', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, institutional_classical_educators).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, ecclesiastical_latin_practitioners).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, vernacular_drift_speakers).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, excluded_medieval_forms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Universities, seminaries, and philological societies that set and enforce standards for what counts as correct Latin in formal writing and instruction. They maintain textual authority by reference to Classical authors (Cicero, Virgil, Livy) while granting domain-specific legitimacy to ecclesiastical and technical vocabulary. They benefit by retaining gatekeeping power over standards while appearing accommodating to practical domains. Their exit is strong — they could adopt any standard — but they choose the hybrid reading because it preserves their authority.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, institutional_classical_educators, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, institutional_classical_educators, beneficiary).

% The Catholic Church, theological institutions, and liturgical communities that need Latin for sacramental and doctrinal purposes. They receive legitimacy for medieval ecclesiastical vocabulary (Christi, gratia Dei, liturgical neologisms) within the hybrid standard while remaining subordinate to Classical norms for non-domain-specific prose. Their exit option is modest — they could adopt Classical orthography entirely or abandon Latin — but the hybrid reading lets them use their inherited liturgical forms without explicit rejection. They benefit from the accommodation but experience suppression on non-ecclesiastical forms.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, ecclesiastical_latin_practitioners, beneficiary,
    institutional, generational, mobile, continental).

% Speakers and writers of Late Latin and post-Classical regional variants that developed organically from spoken Latin and local linguistic contact. Their forms are treated as 'barbarisms' or at best provincial variations by the hybrid standard, delegitimizing their speech as incorrect even when it represents coherent linguistic evolution. They can exit by adopting Classical norms or abandoning Latin entirely, but within Latin they are subordinate. They bear the cost of delegitimization without having voice in standards-setting.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, vernacular_drift_speakers, payer,
    moderate, biographical, constrained, regional).

% Humanist and modern scholars who argue for strict recovery of Classical forms and explicit rejection of medieval drift as barbarism. They would participate in standards-setting but are marginalized by the hybrid reading's accommodation of ecclesiastical needs. Their position is systematically excluded from legitimacy decisions even though they have institutional bases (university chairs, philological societies). They view the hybrid reading as incoherent compromise that undermines the Classical standard.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, philological_reconstruction_advocates, excluded,
    institutional, generational, constrained, continental).

% Modern communities and individuals who view Latin as a living language capable of organic development and who resist the fixation on Classical texts as authoritative. They are structurally excluded from the hybrid reading's standards-setting; their voice would legitimize broader medieval and post-Classical innovation but is not heard in formal institutional discourse. They experience the constraint as suppression of natural linguistic development.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, living_language_continuity_advocates, excluded,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__hybrid_reading, institutional_classical_educators).
narrative_ontology:fixing_cost_class(classical_latin_standard__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared standard for written Latin across institutional, educational, and ecclesiastical domains: a unified textual norm that allows communication across centuries and communities without fragmenting into mutually unintelligible dialects or requiring every domain to maintain its own authority structure.
% TRANSFER_FUNCTION: Moves authority over linguistic legitimacy from organic speech communities and local traditions to centralized institutional gatekeepers (universities, ecclesiastical hierarchies, philological societies) who decide which forms are 'correct.' The hybrid reading transfers less authority than the reconstruction reading (which rejects all medieval drift) but more than the continuity reading (which treats all inherited forms as legitimate).
% ABSENT_VOICES: Vernacular speakers of Late Latin and post-Classical regional variants are structurally excluded because they cannot defend their forms via reference to Classical texts or institutional authority. Living-language continuity advocates are excluded because the constraint's entire logic is backward-looking to written Classical sources. Speakers of Romance languages that evolved from Latin are excluded by the definition itself — they cannot participate in defending Latin-standard claims.
% DISAPPEARANCE_RATIONALE: If the Classical standard constraint vanished, institutional Latin instruction would fragment into competing orthographic and morphological systems within decades. Ecclesiastical Latin would drift further from Classical forms; philosophical and scientific Latin would develop new technical vocabulary without reference to Classical precedent; texts written after the standard's collapse would be mutually difficult to parse across institutional communities.
% FOUNDING_PROBLEM: After the decline of Rome, Latin literacy was preserved through written texts and unbroken ecclesiastical and monastic practice, but regional drift and post-Classical innovation created divergence between the inherited written Ciceronian form and the living usage of medieval speakers and writers. A standard was needed to ensure that scholars across Europe could communicate through a shared normative Latin and that the authority of Classical texts (Bible commentary, philosophical authority, legal precedent) remained accessible.
% FOUNDING_PROBLEM_CORROBORATION: Modern philologists, ecclesiastical institutions, and educational bodies continue to maintain Classical standards and treat post-Classical forms as requiring special justification. Reconstruction-reading advocates (strict purists) attest that standardization is necessary and cite the fragmenting effect of unrestricted medieval drift on textual comprehension across regions. Continuity-reading advocates attest that the problem is *overstated* — living transmission never actually fragmented communication, and the founding concern was invented to justify authority centralization. The empirical consensus is mixed.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__hybrid_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.48) because the hybrid reading partially accommodates post-Classical forms rather than wholesale rejecting them. This distinguishes it structurally from the reconstruction reading, which would show higher extraction (wholesale rejection) and the continuity reading (which would show lower extraction or negative values, subsidizing all inherited forms). Suppression is moderate-high (0.52) because enforcement requires active institutional machinery to police the boundary between approved and unapproved post-Classical forms — the ambiguity about what counts as 'legitimate development' necessitates constant adjudication. Theater ratio (0.38) reflects that while the constraint does real work (maintaining textual coherence across institutional communities), a growing share of enforcement energy goes into defending the boundary against both Humanist purists (who reject the ecclesiastical accommodations as incoherent) and continuity advocates (who reject the suppression of medieval forms as artificial). The measurement series shows extractiveness and suppression rising gradually from interval start (when the hybrid reading was newly institutionalized around the high Renaissance) and plateauing by the early modern period, suggesting the constraint settled into stable institutional practice rather than continuing to tighten. The rising trajectory in early periods reflects increasing enforcement investment as institutional authorities explicitly worked out which medieval forms would be admitted.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional educator's seat, the hybrid reading is genuine coordination that preserves Classical authority while pragmatically accommodating institutional realities (Church needs, technical terminology) — the arrangement is seen as a workable compromise. From the vernacular drift speaker's seat, the same structure is enforcement of elite textual authority that delegitimizes their inherited speech while offering no real voice in standards-setting. From the reconstruction advocate's seat, the hybrid reading is incoherent accommodation that undermines the entire Classical standard by admitting medieval barbarisms. From the continuity advocate's seat, the constraint is invented suppression of natural linguistic development, justified by false authority claims. The engine computes divergent classifications at each seat from the structural data (beneficiary vs. victim assignment, power levels, exit options, scope) — the authoring claim of 'tangled rope' asserts that the parties do experience the same constraint differently, and that divergence is diagnostic of the constraint's true structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional educators occupy the beneficiary/agenda-setter seat: they collect authority rents, set the standard, and maintain the enforcement machinery. Their exit option is strong (arbitrage: they could adopt any standard they choose) and their power is institutional. They derive low directionality (~0.2) — they benefit from the constraint. Ecclesiastical practitioners occupy a mixed seat: they benefit from the accommodation of ecclesiastical vocabulary (their role is beneficiary) but remain subordinate to Classical norms, so they also experience some suppression. Their exit is mobile (they could adopt pure Classical norms or abandon Latin) — they get moderate directionality (~0.45) reflecting the hybrid position. Vernacular drift speakers occupy the payer seat: their forms are delegitimized, their voice is excluded, their exit is constrained (speaking correct Latin is institutionally demanded for participation in educated discourse). They derive high directionality (~0.75). The overrides field is empty because the structural derivation from beneficiary/victim + exit options produces accurate directionality values without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading addresses a genuine mandatrophy question at the kernel level: does the Classical standard's founding problem (need for unified literacy across post-Imperial Christendom) remain live? The institutional reading is that it does — Latin literacy would fragment without the standard. The continuity reading is that the problem never existed in the form claimed — living practice never fragmented, and the standard was imposed to centralize authority. The reconstruction reading is that the founding problem is PARTIALLY dead (Classical textual transmission is now secure via modern printing and scholarship) but the archaeological recovery function is very much live and actually deepens the problem (because medieval forms now actively contaminate correct usage). The hybrid reading sidesteps the mandatrophy dispute by admitting that the founding problem is contested but arguing that the solution is ROBUST to this contest because it preserves Classical authority (satisfying reconstructionists) while accommodating ecclesiastical reality (satisfying pragmatists). This robustness is partly genuine — the standard does enable communication across institutional domains — and partly theatrical: the claim that the solution is robust sometimes masks that it imposes suppression on marginalized speakers to maintain an unstable compromise between incompatible readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_specificity_boundary_ambiguity,
    'What linguistic forms count as legitimate ''domain-specific developments'' versus unauthorized ''barbarisms''? Where is the boundary between ecclesiastical/technical neologisms and drift that should be rejected?',
    'Examination of standards manuals and pedagogical practice: which forms do institutional educators actually treat as acceptable, and by what principle? Creation of an explicit taxonomy of approved post-Classical domains.',
    'If the boundary is principled and stable, the hybrid reading succeeds in its compromise. If the boundary is arbitrary or shifting (increasingly many forms claimed as ''technical'' to escape rejection), the hybrid reading collapses toward continuity. If the boundary is consistently enforced to exclude forms, the reading collapses toward reconstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_specificity_boundary_ambiguity, conceptual, 'The ambiguity between legitimate domain-specific development and unauthorized drift.').

omega_variable(
    ecclesiastical_cooptation_vs_genuine_accommodation,
    'Do institutional educators genuinely accommodate ecclesiastical Latin as a co-equal domain, or does the admission of ecclesiastical forms serve primarily to neutralize ecclesiastical resistance to the Classical standard?',
    'Historical analysis of how institutional decisions about ecclesiastical vocabulary changed when the Church''s institutional power was higher vs. lower. Survey of how ecclesiastical practitioners actually experience the constraint — do they feel legitimized or marginalized?',
    'If accommodation is genuine (ecclesiastical vocabulary faces the same standards as Classical), the constraint is a true compromise and extraction is shared. If accommodation is cooptive (ecclesiastical forms are admitted strategically to neutralize resistance), the constraint is fundamentally extractive and the beneficiary is solely the Classical institutional educator, not the Church.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_cooptation_vs_genuine_accommodation, empirical, 'Whether ecclesiastical accommodation is genuine compromise or strategic cooptation.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Can the hybrid reading and the reconstruction reading coexist as equally legitimate institutional practices, or does the hybrid reading''s implicit claim that Medieval forms can be ''legitimized'' foreclose the reconstruction reading''s core principle that Medieval drift is inherently barbaric?',
    'Intellectual history: can a scholarly community simultaneously hold that some medieval forms are legitimate (hybrid) while maintaining that medieval linguistic change is inherently illegitimate (reconstruction)? Or must one reading ultimately dominate?',
    'If readings can coexist, the kernel permits genuine pluralism within institutional practice. If the hybrid reading forecloses reconstruction (by delegitimizing its core claim that all medieval drift is barbaric), the constraint''s actual structure is more like reconstruction wearing a compromise mask — apparent accommodation masking fundamental suppression of one reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether the hybrid reading genuinely coexists with its siblings or forecloses them logically.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of unapproved medieval forms structural (external institutional machinery enforcing rejection) or internalized (speakers have come to view medieval forms as inherently barbaric and police their own speech)?',
    'Observation of post-institutional contexts: when institutional enforcement machinery is absent (informal writing, speech among scholars without formal stakes), do speakers spontaneously avoid medieval forms or revert to them? Interviews with institutional practitioners about whether they experience rejection as external coercion or internalized standard.',
    'If suppression is primarily structural, removal of institutional enforcement would likely shift the constraint toward continuity (reversion to inherited forms). If internalized, the constraint would persist even without institutional machinery — speakers would carry the delegitimization with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of medieval forms is externally enforced or internalized.').

omega_variable(
    kernel_reading_identity_fusion,
    'For institutional Classical educators, is commitment to the Classical standard fused with their professional identity and institutional role, such that questioning the standard would constitute questioning their own authority and expertise?',
    'Historical and ethnographic evidence: when institutional editors have been challenged to defend the Classical standard, do they appeal to independent criteria (textual authority, usage documentation) or to their own authority as experts to define correctness?',
    'If identity fusion is strong, institutional educators are locked into defending the hybrid reading not because it is structurally sound but because their professional credibility depends on it. This would increase the constraint''s extractiveness (defense of hybrid reading becomes defense of institutional power, not genuine accommodation) and would weaken the resistance to both reconstruction and continuity readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_fusion, conceptual, 'Identity lock-in of institutional educators to the Classical standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__hybrid_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(clas_tr_t0, observed).
narrative_ontology:measurement(clas_tr_t4, classical_latin_standard__hybrid_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement_basis(clas_tr_t4, observed).
narrative_ontology:measurement(clas_tr_t8, classical_latin_standard__hybrid_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(clas_tr_t8, observed).
narrative_ontology:measurement(clas_tr_t12, classical_latin_standard__hybrid_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement_basis(clas_tr_t12, observed).
narrative_ontology:measurement(clas_tr_t16, classical_latin_standard__hybrid_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(clas_tr_t16, observed).
narrative_ontology:measurement(clas_tr_t20, classical_latin_standard__hybrid_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(clas_tr_t20, observed).
narrative_ontology:measurement(clas_tr_t24, classical_latin_standard__hybrid_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(clas_tr_t24, observed).
narrative_ontology:measurement(clas_tr_t28, classical_latin_standard__hybrid_reading, theater_ratio, 28, 0.38).
narrative_ontology:measurement_basis(clas_tr_t28, observed).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__hybrid_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(clas_be_t0, observed).
narrative_ontology:measurement(clas_be_t4, classical_latin_standard__hybrid_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement_basis(clas_be_t4, observed).
narrative_ontology:measurement(clas_be_t8, classical_latin_standard__hybrid_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement_basis(clas_be_t8, observed).
narrative_ontology:measurement(clas_be_t12, classical_latin_standard__hybrid_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement_basis(clas_be_t12, observed).
narrative_ontology:measurement(clas_be_t16, classical_latin_standard__hybrid_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement_basis(clas_be_t16, observed).
narrative_ontology:measurement(clas_be_t20, classical_latin_standard__hybrid_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(clas_be_t20, observed).
narrative_ontology:measurement(clas_be_t24, classical_latin_standard__hybrid_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement_basis(clas_be_t24, observed).
narrative_ontology:measurement(clas_be_t28, classical_latin_standard__hybrid_reading, base_extractiveness, 28, 0.48).
narrative_ontology:measurement_basis(clas_be_t28, observed).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__hybrid_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(clas_su_t0, observed).
narrative_ontology:measurement(clas_su_t4, classical_latin_standard__hybrid_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement_basis(clas_su_t4, observed).
narrative_ontology:measurement(clas_su_t8, classical_latin_standard__hybrid_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement_basis(clas_su_t8, observed).
narrative_ontology:measurement(clas_su_t12, classical_latin_standard__hybrid_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement_basis(clas_su_t12, observed).
narrative_ontology:measurement(clas_su_t16, classical_latin_standard__hybrid_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement_basis(clas_su_t16, observed).
narrative_ontology:measurement(clas_su_t20, classical_latin_standard__hybrid_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(clas_su_t20, observed).
narrative_ontology:measurement(clas_su_t24, classical_latin_standard__hybrid_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(clas_su_t24, observed).
narrative_ontology:measurement(clas_su_t28, classical_latin_standard__hybrid_reading, suppression_requirement, 28, 0.52).
narrative_ontology:measurement_basis(clas_su_t28, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel decomposes into three constraints instantiating different readings of the same foundational commitment to unified Latin standards. The hybrid_reading (this story) occupies the structural middle ground: it preserves Classical textual authority (satisfying reconstruction-reading logic) while accommodating post-Classical institutional realities (satisfying continuity-reading logic). The three constraints are linked by network edges; each reading produces different ε, different victim/beneficiary sets, and different type classifications from the same kernel. See kernel_context in commentary for the full three-reading family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
